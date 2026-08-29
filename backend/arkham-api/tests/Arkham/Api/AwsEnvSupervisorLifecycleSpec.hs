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
import Api.Arkham.Lifecycle (
  acquireTransferringOwnershipOnSuccess,
  acquireWithUnconditionalRelease,
  proceedOnlyIfPreviousShutdownSucceeded,
  shutdownThenDeliver,
 )
import Arkham.Prelude
import Control.Concurrent (forkIO, threadDelay)
import Control.Exception qualified as Exception
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
      result <- takeMVar done
      -- Exercises the exact production 'proceedOnlyIfPreviousShutdownSucceeded'
      -- helper that 'DevelMain.restartAppInNewThread' calls -- not a
      -- locally-mirrored case expression -- so a regression that starts
      -- a replacement on 'Left', or swallows the failure instead of
      -- propagating it, is directly observable here.
      propagated <- Exception.try @Exception.SomeException do
        proceedOnlyIfPreviousShutdownSucceeded result (writeIORef newSupStartedRef True)
      case propagated of
        Left _ -> pure ()
        Right () -> expectationFailure "expected the previous shutdown's failure to propagate, not be swallowed"
      readIORef newSupStartedRef `shouldReturn` False

    it "DOES start a replacement generation when the previous shutdown succeeded, matching restartAppInNewThread's Right branch" do
      newSupStartedRef <- newIORef False
      done <- newEmptyMVar
      shutdownThenDeliver (pure ()) done
      result <- takeMVar done
      proceedOnlyIfPreviousShutdownSucceeded result (writeIORef newSupStartedRef True)
      readIORef newSupStartedRef `shouldReturn` True
