{-# LANGUAGE ScopedTypeVariables #-}

module PendingCleanupOwnerSpec (spec) where

import Arkham.Prelude
import Control.Concurrent (forkIO, myThreadId, threadDelay)
import Control.Exception (AsyncException (ThreadKilled))
import Control.Exception qualified as E
import PendingCleanupOwner (
  ReceiptOutcome (..),
  attemptCleanupReceipt,
  drainPendingCleanup,
  hasPendingCleanup,
  newPendingCleanupOwner,
  transferPendingCleanup,
 )
import Test.Hspec

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
still 'PendingCleanupOwner.Queued', per this module's own 'attemptOne')
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
      -- 'claimOneQueued' pass had already begun running the outer
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
    executing must -- per 'PendingCleanupOwner.attemptOne' -- commit it
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

  describe "hasPendingCleanup" do
    it "reports False for a freshly constructed owner and True while an entry remains queued/running, then False once terminated" do
      owner <- newPendingCleanupOwner
      hasPendingCleanup owner `shouldReturn` False
      receipt <- transferPendingCleanup owner (pure (Right ()))
      hasPendingCleanup owner `shouldReturn` True
      outcome <- attemptCleanupReceipt owner receipt
      expectReceiptSucceeded outcome
      hasPendingCleanup owner `shouldReturn` False

  describe "commit-window asynchronous-exception safety" do
    {- | Root-cause regression for the OTHER gap in 'attemptOne' (distinct
    from the mid-action interruption already covered above): once the
    claimed action has itself already returned (successfully or with a
    clean, data-shaped failure), a /new/ asynchronous exception arriving
    in the narrow window between that return and the STM commit of its
    outcome must never be allowed to skip that commit -- doing so would
    leave the receipt stuck 'PendingCleanupOwner.Running' forever,
    deadlocking every later 'attemptCleanupReceipt'\/'drainPendingCleanup'
    call against it. 'attemptOne' masks exactly this window (running only
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
      let trials = 8000 :: Int
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
        -- ONE continuous 'E.try' with no gap between them, exactly
        -- mirroring 'Arkham.Api.AwsEnvSupervisor''s own mid-cancellation
        -- probe test. Whichever way this resolves (caught here as
        -- 'Left', or completing normally because the 'takeMVar' itself
        -- already observed delivery), the signal is by now fully,
        -- exactly-once spent -- proven by 'deliveredGate' being taken
        -- only after the forked thread's own 'throwTo' call has already
        -- returned -- so nothing further can interrupt this trial's own
        -- separate, guaranteed race-free final check below.
        _ <- E.try @SomeException $ do
          _ <- attemptCleanupReceipt owner receipt
          takeMVar deliveredGate
        finalOutcome <- attemptCleanupReceipt owner receipt
        finalRunCount <- readIORef runCount
        pure (finalOutcome, finalRunCount)
      for_ results $ \(finalOutcome, finalRunCount) -> do
        expectReceiptSucceeded finalOutcome
        -- The action must have run exactly once in total across both
        -- attempts above: never zero (it would have to run at least once
        -- for either attempt to report success) and, just as important,
        -- never twice (which would mean the first attempt's own success
        -- was somehow NOT committed as 'Terminated', leaving the second
        -- attempt to needlessly re-claim and re-run a receipt that had
        -- already genuinely completed).
        finalRunCount `shouldBe` 1
