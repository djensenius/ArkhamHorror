{-# LANGUAGE ScopedTypeVariables #-}

module PendingCleanupOwnerSpec (spec) where

import Arkham.Prelude
import Control.Concurrent (forkIO, myThreadId, threadDelay)
import Control.Exception (AsyncException (ThreadKilled))
import Control.Exception qualified as E
import PendingCleanupOwner (
  CleanupReceipt,
  CleanupAttempt (..),
  PendingCleanupOwner,
  ReceiptOutcome (..),
  attemptCleanupReceipt,
  drainPendingCleanup,
  drainPendingCleanupBounded,
  hasPendingCleanup,
  newPendingCleanupOwner,
  transferPendingCleanup,
  transferPendingCleanupEphemeralOnce,
 )
import Test.Hspec
import System.Timeout qualified as Timeout

-- | 'ReceiptOutcome' has no useful 'Eq' instance (its 'ReceiptFailed'
-- constructor carries a 'SomeException', which doesn't have one), so
-- assert on it by pattern match rather than 'shouldBe'.
expectReceiptSucceeded :: ReceiptOutcome -> Expectation
expectReceiptSucceeded = \case
  ReceiptSucceeded -> pure ()
  other -> expectationFailure ("expected ReceiptSucceeded, got " <> show other)

-- | Likewise, 'drainPendingCleanup'\'s 'Left' case carries
-- 'SomeException's with no 'Eq' instance.
expectDrainSucceeded :: Either [SomeException] () -> Expectation
expectDrainSucceeded = \case
  Right () -> pure ()
  Left errs -> expectationFailure ("expected drain to succeed, got failures: " <> show (length errs))

{- | Wait for @gate@ to be filled -- signalling that this trial's own
racing injector thread has definitely already called
'Control.Exception.throwTo' -- regardless of whether the resulting
'Control.Exception.ThreadKilled' was already caught earlier in @attempt@
itself (in which case @attempt@'s own inner 'Control.Exception.try'
below silently absorbs it before ever reaching this wait) or has not yet
arrived at all (in which case this 'Control.Concurrent.MVar.takeMVar'
itself blocks until it does, and the /outer/ 'Control.Exception.try'
around this whole helper's call site is what actually catches it).

This closes an easy-to-miss synchronization bug in an earlier version of
these \"many independent trials\" tests: each trial there wrapped only
@attemptCleanupReceipt \`Control.Exception.finally\` pure ()@-shaped code
in its own 'Control.Exception.try' and then, /separately/, did
@takeMVar gate@ -- but if the racing 'Control.Exception.ThreadKilled' was
delivered (and caught) DURING @attempt@ itself, that 'Control.Exception.try'
returned immediately WITHOUT ever reaching the subsequent @takeMVar
gate@ at all, so the next trial began immediately while this trial's own
racing injector thread (a brand new 'Control.Concurrent.forkIO' per
trial) was potentially still not yet even scheduled. Under real
scheduler contention (e.g. this exact test running concurrently
alongside OTHER heavy, similarly 'Control.Concurrent.forkIO'-based test
items elsewhere in this suite -- hspec runs different top-level @it@
items on their own, genuinely concurrent green threads by default), that
dangling, still-in-flight injector could be delayed long enough to
finally fire its 'Control.Concurrent.throwTo' arbitrarily far into a
/later/, completely unrelated trial (or even into a subsequent, different
test item entirely), landing outside any 'Control.Exception.try'
expecting it and escaping uncaught -- reproduced directly on this exact
project's own development machine once enough concurrently-scheduled
trials existed elsewhere in this suite. Always waiting for @gate@ here,
unconditionally, regardless of which of the two points the exception
actually arrived at, guarantees this trial's own injector has completed
before the next trial (or the test item itself) ever proceeds -- while
ALSO, unlike that earlier version, never letting the racing exception
escape uncaught if it instead lands during the wait for @gate@ itself
(the outer 'Control.Exception.try' here catches that case; the inner one
is what lets control reach @gate@ at all when the exception instead
already landed during @attempt@).
-}
awaitInjectorThenAttempt :: MVar () -> IO a -> IO ()
awaitInjectorThenAttempt gate attempt = void $ E.try @SomeException $ do
  _ <- E.try @SomeException (void attempt)
  takeMVar gate

{- | Repeatedly call 'attemptCleanupReceipt' against @receipt@, tolerating
'ReceiptBusy' (another, concurrent caller -- e.g. this exact receipt's
own original claimant, itself still unwinding after being asynchronously
interrupted -- currently has it claimed) by waiting briefly and retrying,
up to @maxAttempts@ times, and failing the expectation outright if it is
still 'ReceiptBusy' even after that many attempts (a genuinely stuck
receipt, as opposed to merely a still-in-flight concurrent unwind, would
report 'ReceiptBusy' forever and never resolve no matter how long this
polls).

Needed because a single, immediate follow-up 'attemptCleanupReceipt'
call made from a DIFFERENT thread than the one that actually raced the
original claim (see e.g. the claim-window tests below, which
'Control.Exception.throwTo' a separate, forked attempting thread rather
than the calling thread itself) is not synchronized with that other
thread's own commit: 'Control.Exception.throwTo' only guarantees delivery
has begun on the target, not that the target's own subsequent, masked
commit-back-to-'PendingCleanupOwner.Queued' (or -'PendingCleanupOwner.Terminated')
has itself already finished running by the time it returns -- so an
immediate, single follow-up call could observe a transient
'PendingCleanupOwner.ReceiptBusy' even though the receipt is, in fact,
never actually stuck.
-}
pollUntilResolved :: PendingCleanupOwner -> CleanupReceipt -> IO ReceiptOutcome
pollUntilResolved owner receipt = go (200 :: Int)
 where
  go maxAttempts
    | maxAttempts <= 0 = expectationFailure "receipt was still ReceiptBusy after many polls -- stuck, not merely still in flight" >> pure ReceiptBusy
    | otherwise = do
        outcome <- attemptCleanupReceipt owner receipt
        case outcome of
          ReceiptBusy -> threadDelay 500 >> go (maxAttempts - 1)
          resolved -> pure resolved

{- | Direct, module-level regression tests for 'PendingCleanupOwner',
covering the two properties its own Haddock claims but which
'Arkham.Api.AwsEnvSupervisorLifecycleSpec''s higher-level,
production-wiring tests don't themselves directly probe at the
STM-transaction level: (1) that this module never holds its own single
queue lock across the execution of an arbitrary claimed capability's own
'IO' action -- so a capability that itself needs to durably transfer
further outstanding work back into the very same owner (e.g. a
still-interrupted retry) can never deadlock against its own claim, and a
second, fully independent capability can make genuine forward progress
concurrently rather than queueing behind the first one's own execution
time; and (2) that repeated asynchronous interruption (here, real
'Control.Exception.ThreadKilled' signals, not merely a thrown
'Control.Exception.SomeException' value standing in for one) delivered
while a capability is claimed and running never loses that capability --
every signal either lands before the claim (leaving it untouched and
still 'PendingCleanupOwner.Queued', per this module's own 'attemptClaimed')
or after it (silently discarded once the corresponding thread has
already exited, but the capability itself was by then already safely
committed, one way or the other).

Every test here uses a freshly constructed, independent
'PendingCleanupOwner.PendingCleanupOwner' (never the process-global
'PendingCleanupOwner.globalPendingCleanupOwner'), so these tests can
never observe, or be corrupted by, any other test module's own use of
the global owner.
-}
spec :: Spec
spec = describe "PendingCleanupOwner" do
  describe "concurrent claim / no self-deadlock, no double-run" do
    it "a capability that itself transfers further work back into the same owner while running does not deadlock, and the newly transferred entry is separately, successfully drained" do
      owner <- newPendingCleanupOwner
      nestedRanVar <- newIORef False
      let nestedAction = do
            writeIORef nestedRanVar True
            pure (Right ())
      outerReceiptVar <- newIORef Nothing
      let outerAction = do
            -- While this outer capability's own IO action is genuinely
            -- running (i.e. its own entry is already committed
            -- 'Running', outside any STM transaction), it registers a
            -- second, independent capability into the exact same owner.
            -- If 'transferPendingCleanup' or the surrounding claim
            -- machinery ever held a lock across this call's own
            -- execution, this would deadlock the test (and the test
            -- would time out / hang rather than fail cleanly).
            nestedReceipt <- transferPendingCleanup owner nestedAction
            writeIORef outerReceiptVar (Just nestedReceipt)
            pure (Right ())
      _outerReceipt <- transferPendingCleanup owner outerAction
      result <- drainPendingCleanup owner
      expectDrainSucceeded result
      -- The nested transfer must have actually happened...
      Just nestedReceipt <- readIORef outerReceiptVar
      -- ...and, since it was registered strictly after the first
      -- 'claimNextExcluding' pass had already begun running the outer
      -- action, a single top-level 'drainPendingCleanup' call's own
      -- claim-loop (which re-polls until nothing remains 'Queued') must
      -- still have reached and run it before returning.
      readIORef nestedRanVar `shouldReturn` True
      outcome <- attemptCleanupReceipt owner nestedReceipt
      expectReceiptSucceeded outcome

    it "two concurrent drains against independent, already-queued capabilities never deadlock and never both run the same entry" do
      owner <- newPendingCleanupOwner
      runCounts <- newIORef (mempty :: [Int])
      let mkAction n = do
            atomicModifyIORef' runCounts (\ns -> (n : ns, ()))
            threadDelay 2_000
            pure (Right ())
      receipts <- for [1 .. 20 :: Int] $ \n -> transferPendingCleanup owner (mkAction n)
      resultsVar <- newMVar []
      doneA <- newEmptyMVar
      doneB <- newEmptyMVar
      _ <- forkIO $ do
        r <- drainPendingCleanup owner
        modifyMVar_ resultsVar (pure . (r :))
        putMVar doneA ()
      _ <- forkIO $ do
        r <- drainPendingCleanup owner
        modifyMVar_ resultsVar (pure . (r :))
        putMVar doneB ()
      takeMVar doneA
      takeMVar doneB
      results <- readMVar resultsVar
      -- Both concurrent drains must themselves report success: nothing
      -- either of them individually claimed can have failed.
      traverse_ expectDrainSucceeded results
      finalRunCounts <- readIORef runCounts
      -- Every one of the 20 independently transferred capabilities ran
      -- EXACTLY once in total, split across the two concurrent drains
      -- (never zero, and -- critically -- never twice).
      sort finalRunCounts `shouldBe` [1 .. 20]
      -- And every receipt is now confirmed complete.
      for_ receipts $ \r -> attemptCleanupReceipt owner r >>= expectReceiptSucceeded
      hasPendingCleanup owner `shouldReturn` False

  describe "repeated asynchronous interruption never loses a capability" do
    it "registering a capability while masked, immediately before rethrowing the original asynchronous exception, never loses it: it is later still fully drainable" do
      owner <- newPendingCleanupOwner
      ranVar <- newIORef False
      let action = writeIORef ranVar True >> pure (Right ())
      receiptVar <- newIORef Nothing
      -- Mirrors the exact production shape this guards
      -- ('Api.Arkham.Lifecycle.releaseAll' catching a genuine
      -- asynchronous cancellation, masking, durably transferring its own
      -- still-outstanding capability into this owner, and only then
      -- rethrowing the /original/, already-caught exception -- never a
      -- fresh, independently-raced signal). This is deliberately
      -- deterministic (no concurrency, no timing race): 'transferPendingCleanup''s
      -- own transaction never blocks\/retries, so it cannot itself be
      -- interrupted mid-flight even while merely (non-uninterruptibly)
      -- masked -- see 'PendingCleanupOwner.transferPendingCleanup''s own
      -- Haddock.
      result <- E.try $ E.mask_ $ do
        r <- transferPendingCleanup owner action
        writeIORef receiptVar (Just r)
        E.throwIO ThreadKilled
      case result :: Either SomeException () of
        Left e -> fromException e `shouldBe` Just ThreadKilled
        Right () -> expectationFailure "expected the original ThreadKilled to propagate out unchanged"
      Just receipt <- readIORef receiptVar
      -- The capability itself must still be fully, independently
      -- drainable/attemptable: rethrowing the original exception
      -- afterwards cannot have silently discarded it.
      outcome <- attemptCleanupReceipt owner receipt
      expectReceiptSucceeded outcome
      readIORef ranVar `shouldReturn` True


    {- | Root-cause regression for the specific claim/attempt window
    itself (not merely the surrounding transfer): once a capability has
    been atomically claimed (moved 'Queued' -> 'Running'), an
    asynchronous exception landing while its own action is actually
    executing must -- per 'PendingCleanupOwner.attemptClaimed' -- commit it
    straight back to 'Queued' /before/ ever rethrowing, rather than
    leaving it stuck 'Running' forever (which would deadlock every later
    'attemptCleanupReceipt'\/'drainPendingCleanup' against it). Proven
    directly here by injecting 'Control.Exception.ThreadKilled' from a
    genuinely concurrent thread at the exact moment the claimed action's
    own body is running (synchronized via a barrier the action itself
    signals just before blocking), then confirming a fresh
    'attemptCleanupReceipt' against that same receipt afterwards still
    succeeds (i.e. was never left permanently 'Running').
    -}
    it "a ThreadKilled interrupting a capability's own claimed IO action mid-execution leaves it safely re-queued, not permanently stuck, and a later attempt still succeeds" do
      owner <- newPendingCleanupOwner
      startedGate <- newEmptyMVar
      allowFinish <- newMVar False
      ranToCompletion <- newIORef False
      let action = do
            putMVar startedGate ()
            -- Block until this test explicitly allows the action to
            -- finish -- giving the concurrent ThreadKilled injector a
            -- deterministic window in which the claimed action is
            -- genuinely still running.
            let waitForRelease = do
                  allowed <- readMVar allowFinish
                  if allowed then pure () else threadDelay 1_000 >> waitForRelease
            waitForRelease
            writeIORef ranToCompletion True
            pure (Right ())
      receipt <- transferPendingCleanup owner action
      attemptResultVar <- newEmptyMVar
      attemptTid <- forkIO $ do
        result <- E.try (attemptCleanupReceipt owner receipt) :: IO (Either SomeException ReceiptOutcome)
        putMVar attemptResultVar result
      takeMVar startedGate
      -- The claimed action is now genuinely running (blocked on
      -- 'allowFinish'); interrupt the thread actually attempting it.
      E.throwTo attemptTid ThreadKilled
      firstOutcome <- takeMVar attemptResultVar
      case firstOutcome of
        Left (e :: SomeException) ->
          fromException e `shouldBe` Just ThreadKilled
        Right _ -> expectationFailure "expected the ThreadKilled to propagate out of attemptCleanupReceipt"
      -- The capability itself must not have been discarded: unblock its
      -- action and confirm a fresh attempt against the exact same
      -- receipt still runs it to completion and reports success.
      modifyMVar_ allowFinish (const (pure True))
      secondOutcome <- attemptCleanupReceipt owner receipt
      expectReceiptSucceeded secondOutcome
      readIORef ranToCompletion `shouldReturn` True

  describe "a persistently, synchronously failing entry never starves later, independently queued work" do
    {- | Root-cause regression for a MEDIUM-severity starvation finding
    against 'drainPendingCleanup''s own structural predecessor, which
    re-claimed the single /oldest/ still-'Queued' entry on every
    iteration with no memory of what it had already attempted this pass:
    a persistently, synchronously failing entry (requeued unchanged after
    every attempt) was therefore reclaimed again, forever, and no other,
    later-queued entry was ever even reached. Entirely deterministic --
    no threading, no timing, no flood -- since the fix (tracking an
    @attempted@ set for the duration of one pass) is a pure, sequential
    property of a single 'drainPendingCleanup' call.
    -}
    it "one drain pass attempts the persistent failure exactly once, still completes every other queued entry, a newly-transferred entry from within the persistent failure's own action is also picked up in that SAME pass, and a later, separate drain call retries the persistent failure again" do
      owner <- newPendingCleanupOwner
      persistentAttempts <- newIORef (0 :: Int)
      otherRunCounts <- newIORef (mempty :: [Int])
      nestedRanVar <- newIORef False
      let persistentAction = do
            atomicModifyIORef' persistentAttempts (\n -> (n + 1, ()))
            -- Mirrors a still-outstanding capability itself registering
            -- further work back into this exact owner while it is
            -- claimed and running (e.g. a retried
            -- 'Api.Arkham.Lifecycle.ManagedReleasePlan' asynchronously
            -- interrupted again mid-retry) -- this must still be picked
            -- up within the very same drain pass, never starved by the
            -- persistent failure's own presence.
            _ <- transferPendingCleanup owner (writeIORef nestedRanVar True >> pure (Right ()))
            pure (Left (toException (userError "persistent failure")))
          mkOtherAction n = do
            atomicModifyIORef' otherRunCounts (\ns -> (n : ns, ()))
            pure (Right ())
      -- The persistent failure is transferred FIRST, so a
      -- starvation-prone implementation (always reclaiming the single
      -- oldest 'Queued' entry) would loop on it before ever reaching any
      -- of the entries transferred afterwards.
      persistentReceipt <- transferPendingCleanup owner persistentAction
      otherReceipts <- for [1 .. 10 :: Int] $ \n -> transferPendingCleanup owner (mkOtherAction n)

      firstResult <- drainPendingCleanup owner
      case firstResult of
        Right () -> expectationFailure "expected the first drain to report the persistent failure"
        Left errs -> length errs `shouldBe` 1

      -- Attempted EXACTLY once this pass -- never looped on repeatedly,
      -- and never zero (it must have been reached at all).
      readIORef persistentAttempts `shouldReturn` 1
      -- Every OTHER, later-queued entry still completed in this SAME
      -- pass -- proving it was never starved behind the persistent
      -- failure.
      finalOtherCounts <- readIORef otherRunCounts
      sort finalOtherCounts `shouldBe` [1 .. 10]
      for_ otherReceipts $ \r -> attemptCleanupReceipt owner r >>= expectReceiptSucceeded
      -- The nested transfer, registered from within the persistent
      -- failure's own action while it was running, was also reached and
      -- completed in this exact same pass.
      readIORef nestedRanVar `shouldReturn` True

      -- The persistent failure itself is left safely 'Queued' (never
      -- discarded, never left stuck): a genuinely later, separate drain
      -- call attempts it again.
      secondResult <- drainPendingCleanup owner
      case secondResult of
        Right () -> expectationFailure "expected the second drain to report the persistent failure again"
        Left errs -> length errs `shouldBe` 1
      readIORef persistentAttempts `shouldReturn` 2

      -- It never succeeds on its own, so it remains outstanding
      -- indefinitely until some caller stops retrying it -- confirmed
      -- distinct from every other, already-'Terminated' entry.
      hasPendingCleanup owner `shouldReturn` True
      outcome <- attemptCleanupReceipt owner persistentReceipt
      case outcome of
        ReceiptFailed _ -> pure ()
        other -> expectationFailure ("expected ReceiptFailed, got " <> show other)

  describe "ephemeral coalesced ownership" do
    it "atomically reuses one outstanding receipt and removes its slot/entry exactly once on success without changing retained receipt polling" do
      owner <- newPendingCleanupOwner
      slot <- newTVarIO Nothing
      shouldFail <- newIORef True
      attempts <- newIORef (0 :: Int)
      let action = do
            atomicModifyIORef' attempts (\n -> (n + 1, ()))
            failing <- readIORef shouldFail
            pure $
              if failing
                then CleanupFailed (toException $ userError "still unavailable")
                else CleanupComplete
      receipt1 <- transferPendingCleanupEphemeralOnce owner slot action
      receipt2 <- transferPendingCleanupEphemeralOnce owner slot action
      receipt2 `shouldBe` receipt1
      attemptCleanupReceipt owner receipt1 >>= \case
        ReceiptFailed _ -> pure ()
        other -> expectationFailure ("expected ReceiptFailed, got " <> show other)
      receipt3 <- transferPendingCleanupEphemeralOnce owner slot action
      receipt3 `shouldBe` receipt1
      writeIORef shouldFail False
      attemptCleanupReceipt owner receipt1 >>= expectReceiptSucceeded
      readTVarIO slot `shouldReturn` Nothing
      hasPendingCleanup owner `shouldReturn` False
      readIORef attempts `shouldReturn` 2
      -- An ephemeral receipt remains truthfully pollable as succeeded even
      -- after explicit removal, but never reruns the action.
      attemptCleanupReceipt owner receipt1 >>= expectReceiptSucceeded
      readIORef attempts `shouldReturn` 2
      receipt4 <- transferPendingCleanupEphemeralOnce owner slot (pure CleanupComplete)
      receipt4 `shouldNotBe` receipt1
      attemptCleanupReceipt owner receipt4 >>= expectReceiptSucceeded

  describe "bounded fixed-snapshot draining" do
    it "returns despite continuous registration and leaves work arriving during the snapshot for a later tick" do
      owner <- newPendingCleanupOwner
      attempts <- newIORef (0 :: Int)
      nestedReceipt <- newIORef Nothing
      let action = do
            atomicModifyIORef' attempts (\n -> (n + 1, ()))
            receipt <- transferPendingCleanup owner action
            writeIORef nestedReceipt (Just receipt)
            pure (Right ())
      _ <- transferPendingCleanup owner action
      completed <- Timeout.timeout 500_000 $ drainPendingCleanupBounded owner 1 50_000
      completed `shouldBe` Just ()
      readIORef attempts `shouldReturn` 1
      hasPendingCleanup owner `shouldReturn` True
      Just receipt <- readIORef nestedReceipt
      attemptCleanupReceipt owner receipt >>= expectReceiptSucceeded

    it "times out a blocked receipt, advances later work fairly on the next capped tick, and leaves the blocked capability retriable" do
      owner <- newPendingCleanupOwner
      release <- newEmptyMVar
      blockerRan <- newIORef (0 :: Int)
      laterRan <- newIORef False
      blockedReceipt <-
        transferPendingCleanup owner do
          atomicModifyIORef' blockerRan (\n -> (n + 1, ()))
          takeMVar release
          pure (Right ())
      laterReceipt <-
        transferPendingCleanup owner do
          writeIORef laterRan True
          pure (Right ())
      firstTick <- Timeout.timeout 500_000 $ drainPendingCleanupBounded owner 1 20_000
      firstTick `shouldBe` Just ()
      readIORef laterRan `shouldReturn` False
      secondTick <- Timeout.timeout 500_000 $ drainPendingCleanupBounded owner 1 20_000
      secondTick `shouldBe` Just ()
      readIORef laterRan `shouldReturn` True
      attemptCleanupReceipt owner laterReceipt >>= expectReceiptSucceeded
      putMVar release ()
      attemptCleanupReceipt owner blockedReceipt >>= expectReceiptSucceeded
      readIORef blockerRan `shouldReturn` 2

  describe "hasPendingCleanup" do
    it "reports False for a freshly constructed owner and True while an entry remains queued/running, then False once terminated" do
      owner <- newPendingCleanupOwner
      hasPendingCleanup owner `shouldReturn` False
      receipt <- transferPendingCleanup owner (pure (Right ()))
      hasPendingCleanup owner `shouldReturn` True
      outcome <- attemptCleanupReceipt owner receipt
      expectReceiptSucceeded outcome
      hasPendingCleanup owner `shouldReturn` False

  -- These two 'describe' blocks are deliberately run 'sequential'ly
  -- (overriding this project's default 'SpecHook.hook = parallel'):
  -- each already performs its own internal, statistically-significant
  -- concurrent trials (many independent 'Control.Concurrent.forkIO'
  -- \/ 'Control.Exception.throwTo' races per test), and running
  -- several such heavy, independently-forking tests genuinely
  -- concurrently /with each other/ (as opposed to each test's own
  -- internal trials, which remain fully concurrent) was observed to
  -- make one test's own injector thread occasionally escape uncaught
  -- into a completely different, concurrently-running test's report --
  -- a test-harness-level hazard from cross-test thread interference
  -- under heavy scheduler load, not a 'PendingCleanupOwner' defect.
  -- Forcing these specific items to not overlap with each other
  -- eliminates that interference while preserving every trial's own
  -- internal concurrency and coverage unchanged.
  sequential $ describe "commit-window asynchronous-exception safety" do
    {- | Root-cause regression for the OTHER gap in 'attemptClaimed' (distinct
    from the mid-action interruption already covered above): once the
    claimed action has itself already returned (successfully or with a
    clean, data-shaped failure), a /new/ asynchronous exception arriving
    in the narrow window between that return and the STM commit of its
    outcome must never be allowed to skip that commit -- doing so would
    leave the receipt stuck 'PendingCleanupOwner.Running' forever,
    deadlocking every later 'attemptCleanupReceipt'\/'drainPendingCleanup'
    call against it. 'attemptClaimed' masks exactly this window (running only
    the action itself, via @restore@, fully interruptible), so this test
    races many independent, freshly claimed receipts, each forking a
    thread that immediately (racing the claiming thread's own return from
    the action) delivers a real 'Control.Exception.ThreadKilled' to it,
    and confirms every single one of them is always found genuinely
    'PendingCleanupOwner.Terminated' afterwards -- never stuck --
    regardless of exactly when, relative to the commit, the signal
    happened to land.
    -}
    it "an asynchronous exception racing the boundary between a claimed action returning and its outcome being committed never leaves the receipt stuck Running, across many independent trials" do
      let trials = 500 :: Int
      results <- for [1 .. trials] $ \_ -> do
        owner <- newPendingCleanupOwner
        runCount <- newIORef (0 :: Int)
        deliveredGate <- newEmptyMVar
        let action = do
              atomicModifyIORef' runCount (\n -> (n + 1, ()))
              myTid <- myThreadId
              -- 'Control.Exception.throwTo' only returns once delivery
              -- has genuinely begun on the target thread, so this
              -- forked thread's own 'putMVar' below can only ever run
              -- afterwards -- letting the synchronization immediately
              -- following (still inside the very same 'E.try' this
              -- action itself is run under, via 'restore') confirm
              -- delivery has definitely occurred /somewhere/ in that
              -- span before this trial's own final check proceeds,
              -- without needing to know exactly where it landed.
              _ <- forkIO (E.throwTo myTid ThreadKilled >> putMVar deliveredGate ())
              pure (Right ())
        receipt <- transferPendingCleanup owner action
        -- This single racing signal can land at any interruptible point
        -- from here onward (inside 'attemptCleanupReceipt' itself, or at
        -- the 'takeMVar' immediately below) -- so both are kept inside
        -- ONE continuous synchronization via 'awaitInjectorThenAttempt'
        -- (see its own Haddock for the specific escape-uncaught bug this
        -- closes), exactly mirroring 'Arkham.Api.AwsEnvSupervisor''s own
        -- mid-cancellation probe test. Whichever way this resolves
        -- (caught during the attempt itself, or during the following
        -- wait for 'deliveredGate'), the racing injector thread is
        -- always guaranteed to have completed before this trial's own
        -- final check proceeds.
        awaitInjectorThenAttempt deliveredGate (attemptCleanupReceipt owner receipt)
        finalOutcome <- attemptCleanupReceipt owner receipt
        finalRunCount <- readIORef runCount
        pure (finalOutcome, finalRunCount)
      for_ results $ \(finalOutcome, finalRunCount) -> do
        expectReceiptSucceeded finalOutcome
        -- The action must have run at least once in total across both
        -- attempts above (it would have to run at least once for either
        -- attempt to ever report success -- the property this whole test
        -- exists to prove: the capability is never simply lost). It may
        -- occasionally run twice: 'action' here is a bare, non-idempotent
        -- counter increment with no interruptible operation of its own,
        -- and GHC may still deliver a pending asynchronous exception
        -- immediately after such an action's own side effect has
        -- already run but before 'Control.Exception.try' observes its
        -- clean return -- indistinguishable, from 'attemptClaimed''s own
        -- point of view, from the action never having completed at all,
        -- so it is safely requeued and may genuinely run again on retry.
        -- This is the same, well-known, unavoidable trade-off as any
        -- "at-least-once" cancellation-safe retry of a non-idempotent
        -- action (exactly-once would require either the action itself
        -- to be idempotent, which production release actions generally
        -- are -- closing an already-closed resource is ordinarily a
        -- no-op -- or making the entire action uninterruptible, which
        -- would defeat genuine cancellability). What must never happen,
        -- and is what this assertion actually guards, is the capability
        -- being lost entirely.
        finalRunCount `shouldSatisfy` (>= 1)

  sequential $ describe "claim-window asynchronous-exception safety" do
    {- | Root-cause regression for the specific gap a MEDIUM-severity
    finding raised against an EARLIER version of 'PendingCleanupOwner.attemptClaimed',
    which performed its own claim transaction (@Queued -> Running@) via a
    plain, unmasked 'Control.Concurrent.STM.atomically' call /before/ ever
    entering its own 'Control.Exception.mask': an asynchronous exception
    landing in the gap between that claim committing and the 'mask' call
    beginning left the claimed entry permanently stranded 'Running', with
    nothing left to ever requeue or rethrow anything, deadlocking every
    later attempt against it. The fix moved the claim itself /inside/ one
    single, continuous 'Control.Exception.mask' spanning claim, action,
    and commit (see 'PendingCleanupOwner.attemptClaimed''s own Haddock),
    closing that gap entirely by construction: 'Control.Exception.mask'
    establishes its masked state immediately upon entry (with no
    interruptible operation possible in between), so there is no longer
    any window, anywhere between a caller invoking 'attemptCleanupReceipt'\/'drainPendingCleanup'
    and this module's own claim transaction committing, in which an
    asynchronous exception could land unmasked.

    This test extends the commit-window proof immediately above
    (currently: many independent trials racing 'Control.Exception.ThreadKilled'
    against the boundary between a claimed action returning and its
    commit) to race the EXACT SAME signal as early as possible against
    both the single-receipt ('attemptCleanupReceipt') and whole-owner
    ('drainPendingCleanup') entry points, confirming across many
    independent trials of each that the receipt is always found either
    still safely 'PendingCleanupOwner.Queued' (never attempted) or
    genuinely 'PendingCleanupOwner.Terminated' afterwards -- never
    'PendingCleanupOwner.Running' with no owner left to ever finish it.

    The racing signal is targeted only once the attempting thread has
    confirmed (via a dedicated \"started\" 'Control.Concurrent.MVar.MVar')
    that it has genuinely begun running -- i.e. is already inside
    'attemptCleanupReceipt'\/'drainPendingCleanup' -- rather than being
    fired the instant 'Control.Concurrent.forkIO' returns. Racing
    'Control.Exception.throwTo' against a just-forked, not-yet-scheduled
    target is not a scenario production code ever creates (every real
    caller targets an already-running, known worker thread), and doing
    so here was observed to make an unrelated, concurrently-running
    'Control.Exception.ThreadKilled' escape uncaught under heavy
    parallel scheduler load -- a test-harness hazard, not a
    'PendingCleanupOwner' defect. Gating on \"started\" still exercises
    the meaningful race (cancelled while genuinely attempting the claim)
    without that hazard.
    -}
    it "a ThreadKilled racing the very start of attemptCleanupReceipt (before any claim can possibly have happened) never strands the receipt Running: it is always still safely retriable afterwards, across many independent trials" do
      let trials = 1000 :: Int
      results <- for [1 .. trials] $ \_ -> do
        owner <- newPendingCleanupOwner
        runCount <- newIORef (0 :: Int)
        let action = atomicModifyIORef' runCount (\n -> (n + 1, ())) >> pure (Right ())
        receipt <- transferPendingCleanup owner action
        started <- newEmptyMVar
        attemptTid <- forkIO $ void $ E.try @SomeException $ do
          putMVar started ()
          void (attemptCleanupReceipt owner receipt)
        -- Wait for the attempting thread to genuinely be running
        -- before targeting it, so 'Control.Exception.throwTo' can
        -- never land on a thread that has not yet been scheduled at
        -- all (see the Haddock above).
        takeMVar started
        E.throwTo attemptTid ThreadKilled
        -- Regardless of exactly when the signal landed, this receipt
        -- must always still be safely, repeatedly attemptable: never
        -- stuck 'PendingCleanupOwner.Running' forever. Polls rather
        -- than checking a single immediate follow-up call, tolerating a
        -- transient 'PendingCleanupOwner.ReceiptBusy' while @attemptTid@
        -- -- a genuinely separate, concurrent thread from this one --
        -- may still be completing its own masked commit (see
        -- 'pollUntilResolved''s own Haddock).
        finalOutcome <- pollUntilResolved owner receipt
        finalRunCount <- readIORef runCount
        pure (finalOutcome, finalRunCount)
      for_ results $ \(finalOutcome, finalRunCount) -> do
        expectReceiptSucceeded finalOutcome
        -- Ran at least once (to ever succeed): the capability itself is
        -- never lost. It may occasionally run twice for the same
        -- unavoidable reason documented on the commit-window test above
        -- ('action' here is a non-idempotent counter, and a pending
        -- asynchronous exception may still land immediately after its
        -- own side effect but before 'Control.Exception.try' observes a
        -- clean return) -- not itself evidence the receipt was ever
        -- stuck.
        finalRunCount `shouldSatisfy` (>= 1)

    it "the same ThreadKilled-at-the-very-start race against drainPendingCleanup (the whole-owner entry point) never strands any entry Running either, across many independent trials" do
      let trials = 1000 :: Int
      results <- for [1 .. trials] $ \_ -> do
        owner <- newPendingCleanupOwner
        runCount <- newIORef (0 :: Int)
        let action = atomicModifyIORef' runCount (\n -> (n + 1, ())) >> pure (Right ())
        receipt <- transferPendingCleanup owner action
        started <- newEmptyMVar
        drainTid <- forkIO $ void $ E.try @SomeException $ do
          putMVar started ()
          void (drainPendingCleanup owner)
        takeMVar started
        E.throwTo drainTid ThreadKilled
        finalOutcome <- pollUntilResolved owner receipt
        finalRunCount <- readIORef runCount
        pure (finalOutcome, finalRunCount)
      for_ results $ \(finalOutcome, finalRunCount) -> do
        expectReceiptSucceeded finalOutcome
        -- See the commit-window test's Haddock above: at-least-once,
        -- never-lost is the guarantee; an occasional second run of this
        -- non-idempotent counter is an accepted trade-off, not a stuck
        -- receipt.
        finalRunCount `shouldSatisfy` (>= 1)
