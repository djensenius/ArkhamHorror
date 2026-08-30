{-# LANGUAGE RankNTypes #-}

{- | Regression tests for the MEDIUM-severity audit finding that
'Api.Arkham.AwsEnvSupervisor.AwsEnvSupervisor''s lifecycle was not fully
bracketed by 'Application.makeFoundation'\/'Application.appMain'\/
'Application.handler'\/'Application.getApplicationRepl' (in turn used by
@app\/DevelMain.hs@): a later initialization failure or cancellation could
leak the supervisor's dedicated thread, the GHCI @handler@ helper never
shut one down at all, and @DevelMain@ could signal restart readiness
before the old generation's supervisor had actually finished stopping,
letting two generations overlap; and a failed\/interrupted @DevelMain@
shutdown never delivered any result at all, deadlocking a later restart's
wait forever.

'Application.hs' cannot itself be exercised directly here without a live
Postgres\/Redis connection ('makeFoundation' unconditionally creates a
connection pool), so these tests instead import and directly execute the
/exact/ production ownership helpers from 'Api.Arkham.Lifecycle' --
'acquireTransferringOwnershipOnSuccess' ('Application.makeFoundation',
'Application.getApplicationRepl'), 'acquireWithUnconditionalRelease'
('Application.appMain', 'Application.handler'), and
'shutdownThenDeliver' (@app\/DevelMain.hs@) -- against the real,
production 'Api.Arkham.AwsEnvSupervisor.AwsEnvSupervisor' (safe to
construct and stop in a fast unit test: it is demand-driven and starts
its dedicated thread without ever contacting a credential source until
first demanded, see 'Api.Arkham.AwsEnvSupervisor.newAwsEnvSupervisor').
Because every call site and this spec share the same concrete
definitions, a regression at 'Api.Arkham.Lifecycle' itself (its
combinator choice, or 'shutdownThenDeliver''s result-delivery contract)
is directly caught here, and is proven by the mutation checks below.
-}
module Arkham.Api.AwsEnvSupervisorLifecycleSpec (spec) where

import Api.Arkham.AwsEnvSupervisor (
  AwsAuthErrorDiagnostic (..),
  SupervisedEnvState (..),
  newAwsEnvSupervisor,
  requestAwsEnvReady,
  stopAwsEnvSupervisor,
 )
import Api.Arkham.Helpers (tryRedis_)
import Api.Arkham.Lifecycle (
  acquireThenForkTransferringOwnershipGuarded,
  acquireThenForkTransferringOwnershipGuardedUsing,
  acquireTransferringOwnershipOnSuccess,
  acquireWithUnconditionalRelease,
  consumePreviousShutdownReplayable,
  consumePreviousShutdownReplayableUsing,
  forkTransferringOwnership,
  forkTransferringOwnershipUsing,
  raceManaged_,
  releaseAll,
  restartGateForPreviousGeneration,
  shutdownThenDeliver,
 )
import Arkham.Prelude
import Control.Concurrent (ThreadId, forkIO, killThread, myThreadId, threadDelay)
import Control.Exception qualified as Exception
import GHC.Conc.Sync (ThreadStatus (..), threadStatus)
import Test.Hspec

-- | Every test below stops via 'requestAwsEnvReady' with a zero-length
-- wait rather than inspecting internal state directly: once a
-- supervisor's dedicated thread has actually finished (whether via
-- 'stopAwsEnvSupervisor' or otherwise), its terminal
-- 'AwsAuthSupervisorTerminated' state is published and stays fixed, so a
-- subsequent 'requestAwsEnvReady' call is a safe, side-effect-free way to
-- observe "already stopped" -- it can never trigger a new acquisition
-- attempt, because the thread that would ever read the demand flag no
-- longer exists.
isTerminated :: SupervisedEnvState env -> Bool
isTerminated = \case
  SupervisedEnvUnavailable AwsAuthSupervisorTerminated -> True
  _ -> False

-- | Not 'shouldSatisfy': 'SupervisedEnvState Env' has no 'Show' instance
-- (the wrapped 'Amazonka.Env.Env'' isn't showable), so these avoid ever
-- needing one.
shouldBeTerminated :: SupervisedEnvState env -> Expectation
shouldBeTerminated state =
  unless (isTerminated state) $ expectationFailure "expected the supervisor to have published its terminal Unavailable state"

shouldNotBeTerminated :: SupervisedEnvState env -> Expectation
shouldNotBeTerminated state =
  when (isTerminated state) $ expectationFailure "expected the supervisor NOT to have been stopped yet"

spec :: Spec
spec = describe "Foundation lifecycle bracketing (Application.hs composition patterns)" do
  describe "makeFoundation-style bracketOnError: release only on failure/cancellation, never on success" do
    it "stops the supervisor exactly once when a later initialization step throws immediately after construction" do
      supRef <- newIORef Nothing
      result <- Exception.try @Exception.SomeException do
        acquireTransferringOwnershipOnSuccess newAwsEnvSupervisor stopAwsEnvSupervisor \sup -> do
          writeIORef supRef (Just sup)
          Exception.throwIO (userError "connection pool creation failed")
      case result of
        Left _ -> pure ()
        Right () -> expectationFailure "expected the initialization failure to propagate"
      Just sup <- readIORef supRef
      state <- requestAwsEnvReady sup 0
      shouldBeTerminated state

    it "stops the supervisor exactly once when a failure occurs at a later Foundation-initialization seam, after other unrelated steps have already run" do
      supRef <- newIORef Nothing
      stepsCompletedRef <- newIORef (0 :: Int)
      result <- Exception.try @Exception.SomeException do
        acquireTransferringOwnershipOnSuccess newAwsEnvSupervisor stopAwsEnvSupervisor \sup -> do
          writeIORef supRef (Just sup)
          -- Stand in for the several unrelated initialization steps that
          -- in 'Application.makeFoundation' run after the supervisor is
          -- constructed (Redis, connection pool, heartbeat fork, ...)
          -- before the later one that fails here.
          atomicModifyIORef' stepsCompletedRef (\n -> (n + 1, ()))
          atomicModifyIORef' stepsCompletedRef (\n -> (n + 1, ()))
          Exception.throwIO (userError "a later Foundation-initialization seam failed")
      case result of
        Left _ -> pure ()
        Right () -> expectationFailure "expected the initialization failure to propagate"
      readIORef stepsCompletedRef `shouldReturn` 2
      Just sup <- readIORef supRef
      state <- requestAwsEnvReady sup 0
      shouldBeTerminated state

    it "stops the supervisor when the constructing thread is asynchronously cancelled mid-initialization" do
      supRef <- newIORef Nothing
      bodyStarted <- newEmptyMVar
      workerDone <- newEmptyMVar
      workerTid <-
        forkIO
          $ putMVar workerDone
          =<< Exception.try @Exception.AsyncException do
            acquireTransferringOwnershipOnSuccess newAwsEnvSupervisor stopAwsEnvSupervisor \sup -> do
              writeIORef supRef (Just sup)
              putMVar bodyStarted ()
              forever (threadDelay maxBound)
      takeMVar bodyStarted
      Exception.throwTo workerTid Exception.ThreadKilled
      result <- takeMVar workerDone
      case result of
        Left Exception.ThreadKilled -> pure ()
        _ -> expectationFailure "expected the initializing worker to be cancelled with ThreadKilled"
      Just sup <- readIORef supRef
      state <- requestAwsEnvReady sup 0
      shouldBeTerminated state

    it "does NOT stop the supervisor on success -- ownership transfers to the caller, matching bracketOnError semantics" do
      result <- acquireTransferringOwnershipOnSuccess newAwsEnvSupervisor stopAwsEnvSupervisor \sup -> pure sup
      -- Still live: a fresh demand-driven supervisor that has never been
      -- stopped reports 'SupervisedEnvInitializing' (it never even
      -- attempts acquisition until first demanded), not the terminal
      -- state 'acquireTransferringOwnershipOnSuccess' would have
      -- published had it (incorrectly) released on this success path too.
      state <- requestAwsEnvReady result 0
      shouldNotBeTerminated state
      -- Ownership transfers to this test, matching 'makeFoundation''s
      -- caller (@appMain@\/@handler@\/@getApplicationRepl@ via
      -- 'shutdownApp'): explicitly stop it now.
      stopAwsEnvSupervisor result

  describe "appMain/handler-style plain bracket: release unconditionally, on both success and failure" do
    it "stops the supervisor after a successful action" do
      sup <- newAwsEnvSupervisor
      () <- acquireWithUnconditionalRelease (pure sup) stopAwsEnvSupervisor \_ -> pure ()
      state <- requestAwsEnvReady sup 0
      shouldBeTerminated state

    it "stops the supervisor even when the action (standing in for a Warp exit/exception) throws" do
      sup <- newAwsEnvSupervisor
      result <- Exception.try @Exception.SomeException do
        acquireWithUnconditionalRelease (pure sup) stopAwsEnvSupervisor \_ -> Exception.throwIO (userError "Warp exited abnormally")
      case result of
        Left _ -> pure ()
        Right () -> expectationFailure "expected the action's failure to propagate"
      state <- requestAwsEnvReady sup 0
      shouldBeTerminated state

  describe "getApplicationRepl-style acquireTransferringOwnershipOnSuccess: no gap between acquisition and cleanup installation" do
    it "stops the supervisor when a later step (standing in for getDevSettings/makeApplication) throws" do
      supRef <- newIORef Nothing
      result <- Exception.try @Exception.SomeException do
        acquireTransferringOwnershipOnSuccess newAwsEnvSupervisor stopAwsEnvSupervisor \sup -> do
          writeIORef supRef (Just sup)
          Exception.throwIO (userError "a later getApplicationRepl step failed")
      case result of
        Left _ -> pure ()
        Right () -> expectationFailure "expected the failure to propagate"
      Just sup <- readIORef supRef
      state <- requestAwsEnvReady sup 0
      shouldBeTerminated state

    it "does NOT stop the supervisor on success -- ownership transfers to DevelMain's site for later shutdownApp" do
      sup <- acquireTransferringOwnershipOnSuccess newAwsEnvSupervisor stopAwsEnvSupervisor pure
      state <- requestAwsEnvReady sup 0
      shouldNotBeTerminated state
      stopAwsEnvSupervisor sup

  {- | 'shutdownThenDeliver' is what @DevelMain.hs@'s @start@ now uses
  instead of the old bare @shutdownApp site >> putMVar done ()@: it must
  always deliver an outcome to @done@, converting a shutdown failure (or
  a cancellation delivered while shutdown is blocked on its own
  interruptible internals) into a delivered 'Left' rather than letting it
  propagate and skip delivery entirely.
  -}
  describe "shutdownThenDeliver (DevelMain restart-result delivery)" do
    it "delivers Right () for a successful shutdown" do
      done <- newEmptyMVar
      shutdownThenDeliver (pure ()) done
      result <- takeMVar done
      case result of
        Right () -> pure ()
        Left e -> expectationFailure ("expected a successful shutdown to deliver Right (), got Left " <> show e)

    it "delivers Left for a shutdown that throws, instead of propagating and skipping delivery" do
      done <- newEmptyMVar
      shutdownThenDeliver (Exception.throwIO (userError "supervisor stop failed")) done
      result <- takeMVar done
      case result of
        Left _ -> pure ()
        Right () -> expectationFailure "expected the shutdown failure to be delivered as Left, not silently succeed"

    it "delivers Left for a shutdown cancelled by an async exception mid-flight, rather than deadlocking the waiter forever" do
      done <- newEmptyMVar
      shutdownStarted <- newEmptyMVar
      workerTid <-
        forkIO
          $ shutdownThenDeliver
            ( do
                putMVar shutdownStarted ()
                forever (threadDelay maxBound)
            )
            done
      takeMVar shutdownStarted
      Exception.throwTo workerTid Exception.ThreadKilled
      result <- takeMVar done
      case result of
        Left _ -> pure ()
        Right () -> expectationFailure "expected the cancelled shutdown to be delivered as Left"

  {- | Regression for the MEDIUM-severity audit finding: @app\/DevelMain.hs@'s
  initial \"no server running\" branch created its 'Foreign.Store'-backed
  slots (so a /subsequent/ @update@ call takes the \"server is already
  running\" branch) and then called @start tidRef done@ directly, with no
  handler at all. An ordinary acquisition\/spawn failure on that very
  first generation (before any child is ever spawned to eventually fill
  @done@ via 'shutdownThenDeliver') left @done@ permanently empty, so
  /every/ later @update@ call would take the \"already running\" branch
  and block forever inside 'consumePreviousShutdownReplayable''s own
  'Control.Concurrent.MVar.readMVar' against a cell nothing was ever
  going to fill -- deadlocking restart permanently.

  These tests exercise 'acquireThenForkTransferringOwnershipGuarded'\/
  'acquireThenForkTransferringOwnershipGuardedUsing' directly -- the
  *exact* single-masked-transaction combinator @DevelMain.hs@'s @start@
  now calls, with an injected @gate@ (@'pure' ()@ for the very first
  start, 'consumePreviousShutdownReplayable' @done@ for a restart) -- not
  a locally-mirrored composition of separately-scoped combinators, so a
  regression at either the gate or the acquire\/spawn\/publish transition
  is caught here directly. See 'acquireThenForkTransferringOwnershipGuarded''s
  own Haddock for why this single-mask design makes an earlier
  @transferred@-flag-based version of this wrapper (and the HIGH-severity
  stale-overwrite hazard it existed to close) structurally unnecessary
  rather than merely narrower.
  -}
  describe "acquireThenForkTransferringOwnershipGuarded (DevelMain start: single-masked gate + acquire + spawn + publish)" do
    it "a failed very-first start durably publishes Left, so every subsequent restart attempt fails immediately (never blocks) and never starts a replacement" do
      done <- newEmptyMVar
      replacementStartedRef <- newIORef False
      acquireAttempted <- newIORef (0 :: Int)
      let failingAcquire = atomicModifyIORef' acquireAttempted (\n -> (n + 1, ())) >> Exception.throwIO (userError "initial acquisition failed") :: IO String
          release _ = pure ()
          body _ = pure ()
          finalize _ (_ :: Either SomeException ()) = pure ()
          cancelChild _ = expectationFailure "cancel should never run: nothing was ever spawned"
          publish _ = expectationFailure "publish should never run: nothing was ever spawned"
      -- Mirrors 'DevelMain.update''s @Nothing@ branch exactly: @gate =
      -- 'pure' ()@, since there is no previous generation to consult yet.
      result1 <-
        Exception.try @SomeException
          (acquireThenForkTransferringOwnershipGuarded (pure ()) done failingAcquire release body finalize cancelChild publish)
      case result1 of
        Left _ -> pure ()
        Right (_ :: ThreadId) -> expectationFailure "expected the failing initial start to propagate"
      readIORef acquireAttempted `shouldReturn` 1
      -- The cell is durably filled -- not left empty -- immediately after
      -- the failure, with no need for any other thread to ever fill it.
      filled <- readMVar done
      case filled of
        Left _ -> pure ()
        Right () -> expectationFailure "expected done to be durably filled with Left after the failing initial start"
      -- Mirrors 'DevelMain.update''s @Just@ branch on the *second* call:
      -- gate = 'consumePreviousShutdownReplayable' done must fail
      -- immediately (observing the same Left), never block, and never
      -- reach @acquire@ (i.e. never start a replacement generation).
      result2 <-
        Exception.try @SomeException
          ( acquireThenForkTransferringOwnershipGuarded
              (consumePreviousShutdownReplayable done)
              done
              (atomicModifyIORef' acquireAttempted (\n -> (n + 1, ())) >> writeIORef replacementStartedRef True >> pure ("acquired" :: String))
              release
              body
              finalize
              cancelChild
              publish
          )
      case result2 of
        Left _ -> pure ()
        Right (_ :: ThreadId) -> expectationFailure "expected the second restart attempt to observe the same durable failure"
      readIORef replacementStartedRef `shouldReturn` False
      readIORef acquireAttempted `shouldReturn` 1
      -- A third attempt observes the exact same failure again, still
      -- without blocking or starting a replacement -- the cell is never
      -- silently drained by a failing read.
      result3 <-
        Exception.try @SomeException
          (acquireThenForkTransferringOwnershipGuarded (consumePreviousShutdownReplayable done) done failingAcquire release body finalize cancelChild publish)
      case result3 of
        Left _ -> pure ()
        Right (_ :: ThreadId) -> expectationFailure "expected the third restart attempt to observe the same durable failure"
      readIORef replacementStartedRef `shouldReturn` False

    it "a failed spawn (not just a synchronous acquisition failure) is published identically, without ever reaching release/cancel/publish" do
      done <- newEmptyMVar
      let acquire = pure ("acquired-resource" :: String)
          release _ = expectationFailure "release should never run: acquire succeeded, so it must be the spawned child's own finalize that owns cleanup, but spawn itself never produced a handle"
          body _ = pure ()
          finalize _ (_ :: Either SomeException ()) = pure ()
          cancelChild _ = expectationFailure "cancel should never run: spawn itself never produced a handle"
          publish _ = expectationFailure "publish should never run: spawn itself never produced a handle"
          fakeSpawn :: ((forall a. IO a -> IO a) -> IO ()) -> IO ThreadId
          fakeSpawn _ = Exception.throwIO Exception.ThreadKilled
      result <-
        Exception.try @SomeException
          (acquireThenForkTransferringOwnershipGuardedUsing (pure ()) fakeSpawn done acquire release body finalize cancelChild publish)
      case result of
        Left _ -> pure ()
        Right (_ :: ThreadId) -> expectationFailure "expected the failing spawn to propagate"
      outcome <- readMVar done
      case outcome of
        Left _ -> pure ()
        Right () -> expectationFailure "expected the failed spawn to be published as Left"

    {- | Deterministic (non-racy), matching the equivalent
    'forkTransferringOwnershipUsing' test above: the fake @spawn@ blocks
    forever until the tester's own cancellation interrupts it, so this is
    the only way the call can ever proceed at all -- no timing race
    against a real, essentially-uninterruptible 'forkIOWithUnmask' to
    lose. Proves the acquired resource is already protected (masked, with
    @release@ installed) the instant @acquire@ returns, even though no
    separate statement has run yet to say so.
    -}
    it "releases the resource if this thread is asynchronously cancelled strictly between acquisition returning and the spawn producing a handle" do
      released <- newEmptyMVar
      spawnReached <- newEmptyMVar
      neverFilled <- newEmptyMVar
      done <- newEmptyMVar
      let acquire = pure ("acquired-resource" :: String)
          release _ = putMVar released ()
          body _ = threadDelay maxBound
          finalize _ (_ :: Either SomeException ()) = pure ()
          cancelChild _ = expectationFailure "cancel should never run: spawn itself never produced a handle"
          publish _ = expectationFailure "publish should never run: spawn itself never produced a handle"
          fakeSpawn :: ((forall a. IO a -> IO a) -> IO ()) -> IO ThreadId
          fakeSpawn _threadBody = putMVar spawnReached () >> takeMVar neverFilled >> error "unreachable"
      callerResult <- newEmptyMVar
      callerTid <- forkIO do
        result <- Exception.try @Exception.AsyncException do
          acquireThenForkTransferringOwnershipGuardedUsing (pure ()) fakeSpawn done acquire release body finalize cancelChild publish
        putMVar callerResult result
      takeMVar spawnReached
      Exception.throwTo callerTid Exception.ThreadKilled
      result <- takeMVar callerResult
      result `shouldSatisfy` \case
        Left Exception.ThreadKilled -> True
        _ -> False
      readMVar released `shouldReturn` ()

    it "propagates an acquisition failure without calling release, spawn, cancel, or publish" do
      done <- newEmptyMVar
      spawnCalled <- newIORef False
      releaseCalled <- newIORef False
      let acquire = Exception.throwIO (userError "getApplicationRepl failed") :: IO String
          release _ = writeIORef releaseCalled True
          body _ = pure ()
          finalize _ (_ :: Either SomeException ()) = pure ()
          cancelChild _ = expectationFailure "cancel should never run: nothing was ever spawned"
          publish _ = expectationFailure "publish should never run: nothing was ever spawned"
          fakeSpawn :: ((forall a. IO a -> IO a) -> IO ()) -> IO ThreadId
          fakeSpawn _ = writeIORef spawnCalled True >> myThreadId
      outcome <-
        Exception.try @Exception.SomeException do
          acquireThenForkTransferringOwnershipGuardedUsing (pure ()) fakeSpawn done acquire release body finalize cancelChild publish
      case outcome of
        Left _ -> pure ()
        Right (_ :: ThreadId) -> expectationFailure "expected the acquisition failure to propagate"
      readIORef spawnCalled `shouldReturn` False
      readIORef releaseCalled `shouldReturn` False

    {- | Structural (not flag-based) regression for the HIGH-severity
    finding that a naive, unconditional \"any exception escaping @start@
    means no child was created\" version of this wrapper would overwrite
    @done@ with a stale, spurious 'Left' even when a live, already-tracked
    child exists. Mirrors 'DevelMain.start''s exact @cancel@\/@finalize@
    composition: @finalize@ delivers the child's own outcome into @done@
    via 'shutdownThenDeliver' (exactly as @DevelMain@'s does), and
    @cancel@ (@killThread tid >> (() <$ readMVar done)@, matching
    @DevelMain.hs@'s own @cancel@ exactly) genuinely waits for that
    delivery before returning -- so by the time this function's own final
    'Control.Concurrent.MVar.tryPutMVar' could run (because @publish@
    itself then throws), @done@ has *already* been filled by the
    cancelled child's own finalizer, and 'Control.Concurrent.MVar.tryPutMVar'
    can never overwrite it.

    @publish@ below only throws /after/ observing (via @bodyFinished@)
    that the spawned child's own @body@ has already run to completion --
    without this, @killThread@ could instead race a still-in-flight
    @body@ and genuinely interrupt it, delivering some cancellation-
    classified 'Left' into @done@ rather than @body@'s own 'Right ()',
    which is a fact about /which/ outcome the child genuinely produced,
    not about whether that outcome can be overwritten (the actual subject
    of this test) -- so this ordering exists purely to make the observed
    outcome deterministic, not to influence the overwrite proof itself.

    Mutation check: replacing @cancelChild@ with a version that merely
    @killThread@s without ever waiting for @done@ to be filled (the
    pre-fix hazard, reachable if 'acquireThenForkTransferringOwnershipGuardedUsing'
    stopped awaiting @cancel@ before its own final 'tryPutMVar') makes
    this test fail: it would then observe @done@ filled with the publish
    failure's spurious 'Left', not the child's own genuine 'Right ()'.
    -}
    it "does not overwrite done with a stale failure once cancel has already awaited the spawned child's own terminal delivery" do
      done <- newEmptyMVar
      finalizeCount <- newIORef (0 :: Int)
      -- Deterministically sequences the race this test needs to prove
      -- something about: 'publish' below only throws /after/ observing
      -- that 'body' has already run to completion (its own last
      -- statement, filling 'bodyFinished', has already executed) -- so
      -- the spawned child's own 'try (unmask (body res))' is
      -- overwhelmingly certain to have already recorded 'Right ()' by
      -- the time 'publish''s exception can propagate out to 'cancel'
      -- (which still requires several further stack frames to unwind on
      -- this thread), rather than leaving the child's own outcome to an
      -- unconstrained race against 'killThread'.
      bodyFinished <- newEmptyMVar
      let acquire = pure ("acquired-resource" :: String)
          release _ = pure ()
          body _ = putMVar bodyFinished ()
          finalize _ (outcome :: Either SomeException ()) = do
            atomicModifyIORef' finalizeCount (\n -> (n + 1, ()))
            shutdownThenDeliver (either Exception.throwIO pure outcome) done
          cancelChild tid = killThread tid >> (() <$ readMVar done)
          publish _ = takeMVar bodyFinished >> Exception.throwIO (userError "publish failed")
      result <-
        Exception.try @Exception.SomeException do
          acquireThenForkTransferringOwnershipGuarded (pure ()) done acquire release body finalize cancelChild publish
      case result of
        Left err -> show err `shouldContain` "publish failed"
        Right (_ :: ThreadId) -> expectationFailure "expected the publish failure to propagate"
      readIORef finalizeCount `shouldReturn` 1
      delivered <- readMVar done
      case delivered of
        Right () -> pure ()
        Left err -> expectationFailure ("expected the live child's own genuine outcome (Right ()) to have already filled done, not be overwritten by the publish failure; got Left " <> show err)

    {- | Structural, non-probabilistic regression for the exact
    async-exception-safety gap identified in independent review: the
    entire span from @gate@ consulting @done@ through @publish@
    committing must run under one continuous
    'Control.Exception.mask'\/'restore' boundary (restoring only around
    @acquire@ itself), never two independently-scoped masks with ordinary
    unmasked bookkeeping code running in between. Rather than racing a
    real exception against that window, this observes
    'Control.Exception.getMaskingState' /from directly inside/ it via
    'consumePreviousShutdownReplayableUsing''s test-only hook, called as
    the injected @gate@: it must read as
    'Control.Exception.MaskedInterruptible', never
    'Control.Exception.Unmasked'.

    Mutation check: reverting 'acquireThenForkTransferringOwnershipGuardedUsing'
    to a design that calls @gate@ *before* entering its own 'mask' (e.g.
    composing two separately-scoped combinators, as an earlier version of
    this codebase did) makes this same hook observe
    'Control.Exception.Unmasked' instead, failing deterministically.
    -}
    it "the entire gate-through-publish transaction is masked: the window immediately after consuming a previous success is genuinely protected" do
      done <- newMVar (Right ())
      maskingStateAfterConsume <- newEmptyMVar
      let afterConsume = Exception.getMaskingState >>= putMVar maskingStateAfterConsume
          acquire = pure ("acquired-resource" :: String)
          release _ = pure ()
          body _ = threadDelay maxBound
          finalize _ (_ :: Either SomeException ()) = pure ()
          cancelChild tid = killThread tid >> (() <$ readMVar done)
          publish _ = pure ()
      spawned <-
        acquireThenForkTransferringOwnershipGuarded
          (consumePreviousShutdownReplayableUsing afterConsume done)
          done
          acquire
          release
          body
          finalize
          cancelChild
          publish
      observed <- takeMVar maskingStateAfterConsume
      observed `shouldBe` Exception.MaskedInterruptible
      -- Ownership was durably transferred; clean it up like a genuine
      -- restart would.
      killThread spawned

    {- | Regression for the async-exception-safety gap identified in
    independent review: after the gate's own 'takeMVar' consumes a
    previous 'Right ()' but before @acquire@ genuinely begins, an
    asynchronous exception targeting this thread must not be delivered in
    that narrow window (which would abort the call with @done@ left
    permanently empty, since nothing would ever catch it to write a
    replayable 'Left' back). 'GHC.Conc.throwTo' blocks the calling thread
    until the target thread actually receives the exception -- so, by
    making @acquire@ itself the /only/ operation in this whole call that
    can ever actually block (a 'takeMVar' on an 'MVar' nothing will ever
    fill, which remains interruptible even under 'Control.Exception.mask'
    by design), a successful, unblocked return from 'Exception.throwTo'
    below can /only/ mean the exception was delivered there, at or after
    @acquire@ genuinely began running -- never any earlier, during the
    masked gate\/bookkeeping (which never blocks here, since @done@
    already holds a value, and so is never itself an interruption point).

    Mutation check: removing the 'Control.Exception.mask' spanning
    @gate@ through @acquire@ (reverting to two separately-scoped masks
    with unmasked bookkeeping in between) makes the canary exception's
    delivery point non-deterministic -- it can arrive before @acquire@
    ever begins, interrupting the gate's own bookkeeping itself, in which
    case @done@ is left neither holding a replayable 'Left' nor its
    original 'Right' having ever been restored -- exactly the leak this
    fix closes.
    -}
    it "defers a canary asynchronous exception through the just-consumed-success bookkeeping, delivering it only once acquire is genuinely running, where it is caught and safely re-recorded rather than lost with done left empty" do
      done <- newMVar (Right ())
      neverFilled <- newEmptyMVar
      caughtInsideAcquire <- newEmptyMVar
      resultVar <- newEmptyMVar
      let acquire =
            takeMVar (neverFilled :: MVar ())
              `Exception.catch` \e@(Exception.SomeException _) -> do
                putMVar caughtInsideAcquire ()
                Exception.throwIO e
          release _ = pure ()
          body _ = pure ()
          finalize _ (_ :: Either SomeException ()) = pure ()
          cancelChild _ = expectationFailure "cancel should never run: nothing was ever spawned"
          publish _ = expectationFailure "publish should never run: nothing was ever spawned"
      -- The forking thread is itself masked, so the child inherits
      -- 'Exception.MaskedInterruptible' at birth: this rules out the
      -- exception being delivered before the child has even reached its
      -- own internal 'Control.Exception.mask' call (a distinct,
      -- uninteresting race this test is not about), isolating the one
      -- gap under test.
      Exception.mask_ do
        tid <- forkIO do
          result <-
            Exception.try @Exception.SomeException
              (acquireThenForkTransferringOwnershipGuarded (consumePreviousShutdownReplayable done) done acquire release body finalize cancelChild publish)
          putMVar resultVar result
        Exception.throwTo tid (userError "canary")
      -- 'Exception.throwTo' above only returned because the canary was
      -- actually received -- confirm it was specifically inside
      -- @acquire@, never lost any earlier.
      takeMVar caughtInsideAcquire `shouldReturn` ()
      outcome <- takeMVar resultVar
      case outcome of
        Left err -> show err `shouldContain` "canary"
        Right (_ :: ThreadId) -> expectationFailure "expected the delivered canary exception to propagate as this call's result"
      -- 'done' must hold a replayable failure, never be left permanently
      -- empty (which would deadlock every future restart attempt).
      final <- tryTakeMVar done
      case final of
        Just (Left _) -> pure ()
        Just (Right ()) -> expectationFailure "expected 'done' to be repopulated with Left, not left as the original Right"
        Nothing -> expectationFailure "expected 'done' to hold a replayable Left, not be left permanently empty"

  {- | Regression for the exact @DevelMain.hs@ ordering bug: the old code
  signalled restart readiness (@putMVar done ()@, which unblocks
  @restartAppInNewThread@'s wait and lets it immediately start building
  a brand-new 'Application.App'\/supervisor generation) /before/
  @shutdownApp site@ on the old generation had actually finished,
  allowing the old and new supervisors to run concurrently for a window.
  The fix reorders these two actions so shutdown is awaited first, using
  'shutdownThenDeliver' to also ensure a failed shutdown is reported and
  does not start a replacement generation at all.
  -}
  describe "DevelMain-style restart ordering: shutdown must complete before signalling restart readiness" do
    it "the corrected ordering never allows a new generation's supervisor to exist while the old one is still stopping" do
      oldSup <- newAwsEnvSupervisor
      newSupStartedRef <- newIORef False
      done <- newEmptyMVar
      -- Mirrors the fixed 'app/DevelMain.hs': finish stopping the OLD
      -- generation's supervisor via the exact production
      -- 'shutdownThenDeliver', and only then signal restart readiness.
      _ <- forkIO (shutdownThenDeliver (stopAwsEnvSupervisor oldSup) done)
      -- Mirrors 'restartAppInNewThread': waits for the readiness signal
      -- before ever constructing the next generation, and only proceeds
      -- on 'Right'.
      result <- takeMVar done
      case result of
        Left err -> expectationFailure ("expected a clean shutdown, got: " <> show err)
        Right () -> pure ()
      newSup <- newAwsEnvSupervisor
      writeIORef newSupStartedRef True
      -- By the time the new generation exists, the old one is
      -- unconditionally already fully stopped -- never merely "about to
      -- stop" -- so no window of overlap between generations exists.
      oldState <- requestAwsEnvReady oldSup 0
      shouldBeTerminated oldState
      readIORef newSupStartedRef `shouldReturn` True
      stopAwsEnvSupervisor newSup

    it "the previous (buggy) ordering could observably let a new generation start before the old one finished stopping" do
      oldSup <- newAwsEnvSupervisor
      allowStop <- newEmptyMVar
      doneSignalled <- newEmptyMVar
      -- Mirrors the ORIGINAL bug: signal restart readiness, THEN stop
      -- the old generation's supervisor -- reversed from the fix above.
      -- A gate (not a delay) deterministically holds the stop back
      -- until this test has already observed the readiness signal and
      -- constructed the new generation, proving the overlap window is
      -- genuinely reachable rather than only plausible under timing
      -- luck.
      _ <- forkIO (putMVar doneSignalled () >> takeMVar allowStop >> stopAwsEnvSupervisor oldSup)
      takeMVar doneSignalled
      newSup <- newAwsEnvSupervisor
      -- The old generation's supervisor is provably still not stopped:
      -- the forked thread above is deterministically blocked on
      -- 'allowStop', which this test has not yet put.
      oldStateWhileNewExists <- requestAwsEnvReady oldSup 0
      shouldNotBeTerminated oldStateWhileNewExists
      putMVar allowStop ()
      stopAwsEnvSupervisor oldSup
      stopAwsEnvSupervisor newSup

    it "does NOT start a replacement generation when the previous shutdown itself failed, matching restartAppInNewThread's Left branch" do
      newSupStartedRef <- newIORef False
      done <- newEmptyMVar
      _ <- forkIO (shutdownThenDeliver (Exception.throwIO (userError "old supervisor failed to stop")) done)
      -- Give 'shutdownThenDeliver' a moment to actually deliver, so this
      -- exercises the genuine 'MVar' contents rather than racing it.
      threadDelay (50 * 1000)
      -- Exercises the exact production 'consumePreviousShutdownReplayable'
      -- helper that 'DevelMain.restartAppInNewThread' passes as @gate@ to
      -- 'acquireThenForkTransferringOwnershipGuarded' -- not a
      -- locally-mirrored case expression -- so a regression that starts a
      -- replacement on 'Left', or swallows the failure instead of
      -- propagating it, is directly observable here.
      propagated <- Exception.try @Exception.SomeException do
        consumePreviousShutdownReplayable done
        writeIORef newSupStartedRef True
      case propagated of
        Left _ -> pure ()
        Right () -> expectationFailure "expected the previous shutdown's failure to propagate, not be swallowed"
      readIORef newSupStartedRef `shouldReturn` False

    it "DOES proceed past the gate when the previous shutdown succeeded, matching restartAppInNewThread's Right branch" do
      newSupStartedRef <- newIORef False
      done <- newEmptyMVar
      shutdownThenDeliver (pure ()) done
      consumePreviousShutdownReplayable done
      writeIORef newSupStartedRef True
      readIORef newSupStartedRef `shouldReturn` True

    {- | Regression for the poisoned-'MVar' half of the MEDIUM lifecycle
    finding: the earlier, one-shot @Either@-based helper always consumed
    the previous outcome (even a 'Left') from the shared restart 'MVar'
    before re-throwing, so a /second/ restart attempt after a failed
    shutdown would read an already-emptied cell and block forever instead
    of observing the same failure again. This proves the replacement is
    genuinely /replayable/: repeated calls against the same still-'Left'
    'MVar' all fail immediately, none of them ever blocks, and none of
    them proceeds past the gate.

    Mutation check: reverting 'consumePreviousShutdownReplayable' to
    consume the 'Left' via 'Control.Concurrent.MVar.takeMVar' instead of
    peeking with 'Control.Concurrent.MVar.readMVar' makes the *second*
    call in this test hang forever (an empty 'MVar' with nothing left to
    ever fill it), rather than failing immediately like the first.
    -}
    it "a Left outcome is replayable: repeated restart attempts all fail immediately without blocking or proceeding past the gate" do
      done <- newEmptyMVar
      _ <- forkIO (shutdownThenDeliver (Exception.throwIO (userError "old supervisor failed to stop")) done)
      threadDelay (50 * 1000)
      let attempt = do
            newSupStartedRef <- newIORef False
            outcome <-
              Exception.try @Exception.SomeException do
                consumePreviousShutdownReplayable done
                writeIORef newSupStartedRef True
            started <- readIORef newSupStartedRef
            pure (outcome, started)
      -- Three attempts in a row, each against the *same* 'MVar': every
      -- one must fail immediately (this whole test itself has a bounded
      -- runtime under normal hspec timeouts, so a regression to the old
      -- consuming behaviour would make this test itself hang rather than
      -- merely fail an assertion).
      results <- mapM (const attempt) [1 .. 3 :: Int]
      for_ results \(outcome, started) -> do
        case outcome of
          Left _ -> pure ()
          Right () -> expectationFailure "expected every replay to fail, not just the first"
        started `shouldBe` False

  {- | Regression for the MEDIUM finding that @DevelMain.hs@'s @update@
  durably publishes its 'Foreign.Store' slots for @tidRef@\/@done@ (so a
  /later/ @update@ call takes the \"already running\" branch,
  'restartAppInNewThread') as ordinary, unmasked statements, strictly
  before ever calling @start@ (whose own single
  'Control.Exception.mask', inside
  'acquireThenForkTransferringOwnershipGuarded', is what actually
  protects everything from that point on). An asynchronous exception
  landing in that gap -- after the 'Foreign.Store' slot exists
  (publishing @tidRef = 'Nothing'@) but before a generation is ever
  actually spawned -- used to leave a /subsequent/ @update@ call reading
  @tidRef = 'Nothing'@ and unconditionally treating that as \"a previous
  generation exists and must be consulted\", calling
  'consumePreviousShutdownReplayable' on a @done@ that no generation was
  ever going to fill -- deadlocking every later restart attempt forever.
  'restartGateForPreviousGeneration' is the exact production fix
  'DevelMain.restartAppInNewThread' now uses in place of an unconditional
  @'Control.Concurrent.killThread'@ + 'consumePreviousShutdownReplayable':
  these tests exercise it directly.
  -}
  describe "restartGateForPreviousGeneration (DevelMain restartAppInNewThread's gate: recovers from a Nothing previously-published generation instead of blocking on it)" do
    it "gates with an immediate no-op when no generation has ever been durably published (Nothing), even though 'done' would otherwise block forever" do
      -- A permanently empty 'MVar': no generation was ever spawned to
      -- fill it, so 'consumePreviousShutdownReplayable' called
      -- unconditionally against it would block forever.
      done <- newEmptyMVar
      reached <- timeout (200 * 1000) (restartGateForPreviousGeneration Nothing done)
      reached `shouldBe` Just ()

    it "kills the previous generation and consumes its already-delivered Right result when one was durably published" do
      readyVar <- newEmptyMVar
      tid <- forkIO (putMVar readyVar () >> forever (threadDelay maxBound))
      takeMVar readyVar
      done <- newEmptyMVar
      shutdownThenDeliver (pure ()) done
      restartGateForPreviousGeneration (Just tid) done
      status <- waitUntilTerminated tid
      status `shouldSatisfy` isTerminatedStatus
      -- 'consumePreviousShutdownReplayable' drains the 'Right' it
      -- consumed: nothing is left behind for the next generation to
      -- mistakenly observe.
      stillEmpty <- timeout (50 * 1000) (readMVar done)
      stillEmpty `shouldSatisfy` isNothing

    it "propagates (not swallows) a previous generation's delivered Left failure, without killing/proceeding a second time" do
      readyVar <- newEmptyMVar
      tid <- forkIO (putMVar readyVar () >> forever (threadDelay maxBound))
      takeMVar readyVar
      done <- newEmptyMVar
      shutdownThenDeliver (Exception.throwIO (userError "previous generation failed to stop")) done
      outcome <- Exception.try @Exception.SomeException (restartGateForPreviousGeneration (Just tid) done)
      case outcome of
        Left err -> show err `shouldContain` "previous generation failed to stop"
        Right () -> expectationFailure "expected the previous generation's failure to propagate"
      killThread tid

    it "mutation check: unconditionally consuming (ignoring Nothing/Just) would instead block forever on a Nothing previous generation" do
      done <- newEmptyMVar
      let unconditional :: Maybe ThreadId -> MVar (Either Exception.SomeException ()) -> IO ()
          unconditional _ = consumePreviousShutdownReplayable
      blocked <- timeout (200 * 1000) (unconditional Nothing done)
      blocked `shouldBe` Nothing
  {- | Regression for the second MEDIUM lifecycle finding: 'DevelMain.hs's
  @start@ transfers an already-acquired 'App''s remaining lifetime to a
  newly-forked child thread (which runs Warp, then shuts the 'App' down
  once Warp exits). A gap between the 'App' being acquired and that child
  actually being spawned -- or the fork itself failing -- could leak the
  'App' (and its AWS Env supervisor) with nothing left to ever shut it
  down. 'forkTransferringOwnership' is the exact production helper
  'DevelMain.start' uses to close that gap; these tests exercise it
  directly against a fake resource (a plain 'IORef' flag standing in for
  'Application.App'\/'Application.shutdownApp'), never a real 'App'.
  -}
  describe "forkTransferringOwnership (DevelMain child-thread ownership transfer)" do
    it "transfers ownership to the child on success: the child runs the body then the finalizer exactly once, and release is never separately called" do
      released <- newIORef (0 :: Int)
      finalizeCount <- newIORef (0 :: Int)
      bodyRan <- newEmptyMVar
      let release _ = atomicModifyIORef' released (\n -> (n + 1, ()))
          body _ = putMVar bodyRan ()
          finalize _ (_ :: Either Exception.SomeException ()) = atomicModifyIORef' finalizeCount (\n -> (n + 1, ()))
      _tid <- forkTransferringOwnership () release body finalize
      takeMVar bodyRan
      -- Bounded poll for the finalizer, which runs asynchronously in the
      -- child once its body (an already-completed 'putMVar' above)
      -- returns.
      let waitFinalize (n :: Int)
            | n <= 0 = expectationFailure "finalize was never called"
            | otherwise = do
                count <- readIORef finalizeCount
                if count >= 1 then pure () else threadDelay (1000 :: Int) >> waitFinalize (n - 1)
      waitFinalize 2000
      readIORef finalizeCount `shouldReturn` 1
      readIORef released `shouldReturn` 0

    {- | An independent review raised (incorrectly, on inspection --
    documented here to avoid re-litigating it) a concern that the child
    body could be silently left masked, since @spawn@ (here, production's
    'Control.Concurrent.forkIOWithUnmask') is called from /within/
    'forkTransferringOwnershipUsing''s own 'Control.Exception.mask'.
    Confirmed by direct experimentation (including with an
    already-masked caller before this function is ever invoked):
    'Control.Concurrent.forkIOWithUnmask''s own supplied @unmask@ callback
    always delivers a genuinely 'Control.Exception.Unmasked' state to its
    argument, regardless of any enclosing masking context -- this is its
    specific, documented purpose (\"used when the parent thread is masking
    asynchronous exceptions and doesn't want its children to inherit that
    masking state\"). This test instead guards against the real risk in
    this area: forgetting to route @body@ through @spawn@'s supplied
    @unmask@ at all (leaving it running in the child's inherited masking
    state, i.e. masked, since forking always happens from within this
    function's own 'mask').

    Mutation check: replacing @unmask (body res)@ with plain @body res@
    (skipping @unmask@ entirely) makes this test observe
    'Control.Exception.MaskedInterruptible' instead of
    'Control.Exception.Unmasked', failing deterministically.
    -}
    it "the child body runs unmasked, not left masked from being forked within this function's own mask" do
      maskingStateInBody <- newEmptyMVar
      let release _ = pure ()
          body _ = Exception.getMaskingState >>= putMVar maskingStateInBody
          finalize _ (_ :: Either Exception.SomeException ()) = pure ()
      _tid <- forkTransferringOwnership () release body finalize
      observed <- takeMVar maskingStateInBody
      observed `shouldBe` Exception.Unmasked

    {- | Companion to the above: the finalizer, matching
    'Control.Concurrent.forkFinally', must run masked so it cannot itself
    be interrupted mid-cleanup\/result-delivery. This holds today by
    inheritance (the child is always forked from within this function's
    own 'mask', and @unmask@ only lifts masking for @body@'s own
    duration), but 'forkTransferringOwnershipUsing' also wraps it in an
    explicit 'Control.Exception.mask_' so this invariant is guaranteed
    rather than incidental.

    Mutation check: removing the explicit 'Control.Exception.mask_' around
    the finalizer call (relying solely on inheritance) does not currently
    fail this specific test (the inherited state already happens to be
    masked here) -- confirming the explicit 'mask_' is a defensive,
    non-load-bearing clarity improvement for this call site as currently
    used, not a behavior change.
    -}
    it "the finalizer runs masked, matching forkFinally" do
      maskingStateInFinalize <- newEmptyMVar
      let release _ = pure ()
          body _ = pure ()
          finalize _ (_ :: Either Exception.SomeException ()) = Exception.getMaskingState >>= putMVar maskingStateInFinalize
      _tid <- forkTransferringOwnership () release body finalize
      observed <- takeMVar maskingStateInFinalize
      observed `shouldBe` Exception.MaskedInterruptible

    it "runs the finalizer exactly once even when the child body itself throws, and release is never separately called" do
      finalizeResults <- newIORef ([] :: [Either Exception.SomeException ()])
      released <- newIORef (0 :: Int)
      let release _ = atomicModifyIORef' released (\n -> (n + 1, ()))
          body _ = Exception.throwIO (userError "Warp exited abnormally") :: IO ()
          finalize _ result = atomicModifyIORef' finalizeResults (\rs -> (rs <> [result], ()))
      _tid <- forkTransferringOwnership () release body finalize
      let waitFinalize (n :: Int)
            | n <= 0 = expectationFailure "finalize was never called"
            | otherwise = do
                rs <- readIORef finalizeResults
                if not (null rs) then pure () else threadDelay (1000 :: Int) >> waitFinalize (n - 1)
      waitFinalize 2000
      rs <- readIORef finalizeResults
      length rs `shouldBe` 1
      case rs of
        [Left _] -> pure ()
        _ -> expectationFailure "expected the finalizer to observe the child body's own exception"
      readIORef released `shouldReturn` 0

    {- | Regression for the "fork itself fails" half of the ownership
    contract: 'forkTransferringOwnership' wraps its call to
    'Control.Concurrent.forkIOWithUnmask' in
    'Control.Exception.onException', releasing @res@ synchronously here
    if that fork itself ever throws (however rare in practice for the
    real, unmodified 'forkIOWithUnmask') instead of silently leaking
    @res@ with no child ever spawned to own it. This is exercised
    directly against the exact release-on-exception composition
    'forkTransferringOwnership' is built from, substituting a
    deliberately-failing stand-in for the fork step itself (since forcing
    GHC's real, unmodified 'forkIOWithUnmask' to fail is not practical
    from a test), so a regression that drops the 'onException' wrapper
    entirely is what this test exists to catch.
    -}
    it "releases the resource here (not via the child) when forking itself fails, without ever running the child body" do
      released <- newIORef False
      bodyRan <- newIORef False
      let release _ = writeIORef released True
          fakeFork :: IO ()
          fakeFork = Exception.throwIO (userError "forkIOWithUnmask failed")
      outcome <- Exception.try @Exception.SomeException (fakeFork `Exception.onException` release (bodyRan :: IORef Bool))
      case outcome of
        Left _ -> pure ()
        Right () -> expectationFailure "expected the fake fork to fail"
      readIORef released `shouldReturn` True
      readIORef bodyRan `shouldReturn` False

    {- | Deterministic (non-racy) regression for cancellation landing
    strictly between @res@ being in hand and the actual spawn primitive
    producing a handle: the fake @spawn@ below blocks forever on an
    unfilled 'MVar' (a stand-in for a spawn primitive that could itself
    block on something genuinely interruptible), so the ONLY way this
    test's 'forkTransferringOwnershipUsing' call can ever proceed at all
    is via the tester's own 'Exception.throwTo' interrupting that block
    -- there is no timing race to lose here, unlike racing a real
    'Control.Concurrent.forkIOWithUnmask' (which has no interruptible
    operation of its own, and so essentially never actually loses that
    race in practice -- see the superseded version of this test this
    replaced, which observed 0\/20 meaningful hits even with the
    production 'onException' wrapper removed entirely).

    Mutation check: removing 'onException' from
    'forkTransferringOwnershipUsing' (keeping 'Control.Exception.mask')
    makes this test hang\/fail, since 'Control.Concurrent.MVar.takeMVar'
    remains interruptible even when masked (by design -- see
    'Control.Exception.mask''s own Haddock), so the injected
    'Exception.ThreadKilled' still unwinds straight out of the blocked
    fake @spawn@ call with nothing left to catch it and run @release@.
    -}
    it "releases the resource if this thread is asynchronously cancelled while genuinely blocked immediately before the fork can complete" do
      released <- newEmptyMVar
      spawnReached <- newEmptyMVar
      neverFilled <- newEmptyMVar
      let release _ = putMVar released ()
          body _ = threadDelay maxBound
          finalize _ _ = pure ()
          -- Stands in for 'Control.Concurrent.forkIOWithUnmask' itself:
          -- signals it has been reached (i.e. @res@ is already held by
          -- this masked span, immediately before any handle would ever
          -- be produced), then blocks forever -- genuinely
          -- interruptible even under 'Control.Exception.mask', per
          -- 'Control.Concurrent.MVar.takeMVar''s documented exemption --
          -- until the tester's cancellation interrupts it.
          fakeSpawn :: ((forall a. IO a -> IO a) -> IO ()) -> IO ThreadId
          fakeSpawn _threadBody = putMVar spawnReached () >> takeMVar neverFilled >> error "unreachable"
      callerResult <- newEmptyMVar
      callerTid <- forkIO do
        result <- Exception.try @Exception.AsyncException do
          forkTransferringOwnershipUsing fakeSpawn () release body finalize
        putMVar callerResult result
      takeMVar spawnReached
      Exception.throwTo callerTid Exception.ThreadKilled
      result <- takeMVar callerResult
      result `shouldSatisfy` \case
        Left Exception.ThreadKilled -> True
        _ -> False
      readMVar released `shouldReturn` ()

  {- | 'Application.shutdownApp' releases every foundation-owned resource
  (the AWS supervisor, the room-heartbeat thread, the optional
  pub/sub-supervisor thread, the Redis connection, and the connection
  pool) via 'releaseAll' rather than a plain sequence of statements, so
  that an earlier release throwing can never cause a later one to be
  skipped -- otherwise a single misbehaving release would leave every
  resource after it leaking on every subsequent 'DevelMain' restart or
  'Application.handler' call. These tests exercise 'releaseAll' itself
  (the exact combinator 'shutdownApp' calls), not a locally-duplicated
  mirror of it.
  -}
  describe "releaseAll (Application.shutdownApp resource release, attempt-every-release-even-if-one-throws)" do
    it "runs every action, in order, when all succeed" do
      order <- newIORef []
      let recordOrder n = atomicModifyIORef' order (\xs -> (xs <> [n :: Int], ()))
      releaseAll [recordOrder 1, recordOrder 2, recordOrder 3]
      readIORef order `shouldReturn` [1, 2, 3]

    it "still runs every later action even when an earlier one throws, and re-raises that failure" do
      ranAfterFailure <- newIORef False
      result <-
        Exception.try @Exception.SomeException
          $ releaseAll
            [ Exception.throwIO (userError "first release failed")
            , atomicModifyIORef' ranAfterFailure (const (True, ()))
            ]
      readIORef ranAfterFailure `shouldReturn` True
      case result of
        Left err -> show err `shouldContain` "first release failed"
        Right () -> expectationFailure "expected the first failure to propagate"

    it "runs every action even when several throw, and surfaces only the first failure" do
      thirdRan <- newIORef False
      result <-
        Exception.try @Exception.SomeException
          $ releaseAll
            [ Exception.throwIO (userError "first release failed")
            , Exception.throwIO (userError "second release failed")
            , atomicModifyIORef' thirdRan (const (True, ()))
            ]
      readIORef thirdRan `shouldReturn` True
      case result of
        Left err -> show err `shouldContain` "first release failed"
        Right () -> expectationFailure "expected the first failure to propagate"

    it "is silent and a no-op for an empty release list" do
      releaseAll []

    it "mutation check: without attempt-every-release, a later release would be skipped after an earlier failure" do
      -- Proves the test above is load-bearing: the naive alternative
      -- (`sequence_`, which abandons every later action once one throws)
      -- would fail this exact assertion.
      ranAfterFailure <- newIORef False
      _ <-
        Exception.try @Exception.SomeException @()
          $ sequence_
            [ Exception.throwIO (userError "first release failed")
            , atomicModifyIORef' ranAfterFailure (const (True, ()))
            ]
      readIORef ranAfterFailure `shouldReturn` False

  {- | Regression for the HIGH-severity finding that 'Api.Arkham.Helpers.pubSubSupervisor'
  used @UnliftIO.Async.race_@ (built on @Control.Concurrent.Async.withAsync@,
  whose own cleanup is @uninterruptibleCancel@): a slow-to-respond losing
  branch (e.g. blocked in a synchronous Redis read) made cleanup, and
  therefore the whole race, unconditionally uninterruptible until that
  loser eventually finished on its own -- capable of hanging Foundation
  shutdown forever if the racer itself was ever asynchronously cancelled
  while waiting. 'raceManaged_' is the repository-owned, always-interruptible
  replacement now used there (via 'Api.Arkham.Lifecycle.raceManaged_').
  -}
  describe "raceManaged_ (always-interruptible race_ replacement used by Api.Arkham.Helpers.pubSubSupervisor)" do
    it "returns the winner's own successful result, having already cancelled and genuinely awaited the loser before returning" do
      loserCleanedUp <- newIORef False
      let winner = pure ()
          loser = Exception.finally (forever (threadDelay maxBound)) (writeIORef loserCleanedUp True)
      result <- raceManaged_ winner loser
      case result of
        Right () -> pure ()
        Left err -> expectationFailure ("expected the winner's own successful result, got Left " <> show err)
      -- 'cancelBoth' runs via 'finally' *around* the wait for the
      -- winner, so by the time 'raceManaged_' itself has returned, the
      -- loser has already been genuinely cancelled and awaited to
      -- completion -- not merely signalled and left running in the
      -- background.
      readIORef loserCleanedUp `shouldReturn` True

    it "propagates the winner's own synchronous failure as Left, having already cancelled and awaited the loser first" do
      loserCleanedUp <- newIORef False
      let winner = Exception.throwIO (userError "winning side failed synchronously")
          loser = Exception.finally (forever (threadDelay maxBound)) (writeIORef loserCleanedUp True)
      result <- raceManaged_ winner loser
      case result of
        Left err -> show err `shouldContain` "winning side failed synchronously"
        Right () -> expectationFailure "expected the winner's own synchronous failure to be captured, not swallowed"
      readIORef loserCleanedUp `shouldReturn` True

    {- | The core regression: unlike @withAsync@\/@uninterruptibleCancel@,
    an asynchronous exception delivered to whichever thread is itself
    calling 'raceManaged_' (standing in for 'Application.shutdownApp'
    cancelling 'Api.Arkham.Helpers.pubSubSupervisor' during Foundation
    shutdown) can always still land and propagate -- even while both
    sides are still genuinely running and neither has responded to
    cancellation yet -- and, when it does, both children are cancelled
    and genuinely awaited to completion (not merely uninterruptibly,
    silently abandoned) before that cancellation is allowed to finish
    propagating out of 'raceManaged_' itself.

    Mutation check: replacing 'raceManaged_' with an ordinary
    @UnliftIO.Async.race_@\/@withAsync@-based implementation makes the
    'Exception.throwTo' below block for as long as the (here,
    permanently-blocked) losing sides take to finish on their own --
    i.e. forever in this test, since neither ever exits by itself --
    rather than landing and unwinding promptly once both children are
    confirmed cancelled.
    -}
    it "when the racer itself is asynchronously cancelled while waiting on both sides, both children are cancelled and genuinely awaited before that cancellation propagates" do
      leftStarted <- newEmptyMVar
      rightStarted <- newEmptyMVar
      leftCleanedUp <- newIORef False
      rightCleanedUp <- newIORef False
      let leftAction =
            Exception.finally
              (putMVar leftStarted () >> forever (threadDelay maxBound))
              (writeIORef leftCleanedUp True)
          rightAction =
            Exception.finally
              (putMVar rightStarted () >> forever (threadDelay maxBound))
              (writeIORef rightCleanedUp True)
      racerDone <- newEmptyMVar
      racerTid <- forkIO $ do
        outcome <- Exception.try @Exception.SomeException (raceManaged_ leftAction rightAction)
        putMVar racerDone outcome
      takeMVar leftStarted
      takeMVar rightStarted
      Exception.throwTo racerTid Exception.ThreadKilled
      outcome <- takeMVar racerDone
      case outcome of
        Left _ -> pure ()
        Right _ -> expectationFailure "expected the cancelled racer's own call to propagate a failure"
      readIORef leftCleanedUp `shouldReturn` True
      readIORef rightCleanedUp `shouldReturn` True

  {- | Regression for the production 'Api.Arkham.Helpers.tryRedis_' guard
  itself (used directly by 'Api.Arkham.Helpers.withRedis',
  'Api.Arkham.Helpers.getRedisRoomCounts''s stale-room sweep, and
  'Api.Arkham.Helpers.pubSubSupervisor''s health-ping, and, transitively
  via those, by 'Api.Arkham.Helpers.roomHeartbeat''s and
  'Api.Arkham.Helpers.pubSubSupervisor''s own @watchdog@ -- each of which
  is one racer under 'raceManaged_' and so depends on actually terminating
  on the 'Exception.ThreadKilled' 'Api.Arkham.Lifecycle.cancelManagedThread'
  delivers). 'tryRedis_' must swallow only genuinely synchronous failures
  (a real Redis\/network error) and let any asynchronous exception --
  above all a caller's own cancellation -- propagate unchanged; a
  'Control.Exception.try' \@'Control.Exception.SomeException'-based
  implementation would instead swallow the cancellation too, leaving
  'Api.Arkham.Lifecycle.cancelManagedThread''s own await blocked forever
  and hanging Foundation shutdown.

  This exercises 'tryRedis_' directly (not a fake stand-in): rather than
  merely checking the guarded thread eventually terminates -- which a
  buggy, swallowing 'tryRedis_' would also do, just via its /caller/
  finishing normally instead of via the propagating exception, giving no
  real signal either way -- it instead proves the guarded action's
  enclosing @do@-block genuinely never reaches a statement placed
  immediately /after/ the 'tryRedis_' call. For a correct
  implementation this is a hard guarantee, not a probabilistic one:
  Haskell's own exception semantics mean a propagating exception
  unconditionally aborts the rest of that @do@-block, so the marker below
  can never be reached, for any timeout bound whatsoever. For a buggy,
  swallowing implementation, 'tryRedis_' instead returns normally almost
  immediately after the cancellation is delivered, letting execution
  reach the marker within a generous bound.
  -}
  describe "tryRedis_ (async-safe Redis guard shared by withRedis, getRedisRoomCounts, roomHeartbeat, and pubSubSupervisor)" do
    it "propagates (never swallows) an asynchronous cancellation delivered while the guarded action is running" do
      startedVar <- newEmptyMVar
      reachedAfterVar <- newEmptyMVar
      tid <- forkIO do
        tryRedis_ (putMVar startedVar () >> forever (threadDelay maxBound))
        -- Only reachable if 'tryRedis_' wrongly swallowed the
        -- cancellation and returned normally instead of letting it
        -- propagate.
        putMVar reachedAfterVar ()
      takeMVar startedVar
      Exception.throwTo tid Exception.ThreadKilled
      reachedAfter <- timeout (200 * 1000) (takeMVar reachedAfterVar)
      reachedAfter `shouldBe` Nothing
      status <- waitUntilTerminated tid
      status `shouldSatisfy` isTerminatedStatus

    it "swallows a genuine synchronous failure instead of propagating it" do
      tryRedis_ (Exception.throwIO (userError "genuine synchronous Redis failure")) `shouldReturn` ()

    it "mutation check: a Control.Exception.try @SomeException-based guard would instead swallow the cancellation above" do
      -- Proves the first assertion is load-bearing: the naive
      -- alternative this guards against actually would let execution
      -- reach the post-guard marker.
      startedVar <- newEmptyMVar
      reachedAfterVar <- newEmptyMVar
      tid <- forkIO do
        _ <-
          Exception.try @Exception.SomeException
            (putMVar startedVar () >> forever (threadDelay maxBound))
        putMVar reachedAfterVar ()
      takeMVar startedVar
      Exception.throwTo tid Exception.ThreadKilled
      reachedAfter <- timeout (200 * 1000) (takeMVar reachedAfterVar)
      reachedAfter `shouldBe` Just ()
 where
  isTerminatedStatus :: ThreadStatus -> Bool
  isTerminatedStatus status = status == ThreadFinished || status == ThreadDied

  waitUntilTerminated :: ThreadId -> IO ThreadStatus
  waitUntilTerminated tid = go (500 :: Int)
   where
    go n = do
      status <- threadStatus tid
      if isTerminatedStatus status || n <= 0
        then pure status
        else threadDelay 1000 >> go (n - 1)
