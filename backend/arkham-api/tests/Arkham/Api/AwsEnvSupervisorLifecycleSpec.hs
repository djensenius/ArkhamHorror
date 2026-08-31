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
  CleanupReceipt,
  ReceiptOutcome (..),
  RestartState (..),
  RetirementOutcome (..),
  RetirementRetry (..),
  StopOutcome (..),
  acquireTransferringOwnershipOnSuccess,
  acquireWithUnconditionalRelease,
  classifyRetirementFailure,
  drainOwnedCleanup,
  forkTransferringOwnership,
  forkTransferringOwnershipUsing,
  newManagedCleanup,
  newManagedReleasePlan,
  raceManaged_,
  releaseAll,
  releaseAllRecordingReceipt,
  restartManagedGeneration,
  restartManagedGenerationUsing,
  runManagedCleanup,
  runManagedReleasePlan,
  shutdownThenDeliver,
  shutdownThenDeliverRecordingReceipt,
  stopManagedGeneration,
 )
import PendingCleanupOwner (
  attemptCleanupReceipt,
  drainPendingCleanup,
  globalPendingCleanupOwner,
  hasPendingCleanup,
  newPendingCleanupOwner,
  transferPendingCleanup,
 )
import Arkham.Prelude
import Control.Concurrent (ThreadId, forkIO, forkIOWithUnmask, killThread, myThreadId, threadDelay)
import Control.Exception qualified as Exception
import Data.List.NonEmpty qualified as NE
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
    it "delivers RetiredCleanly for a successful shutdown" do
      done <- newEmptyMVar
      shutdownThenDeliver (pure ()) done
      result <- takeMVar done
      case result of
        RetiredCleanly -> pure ()
        RetirementFailed e _ -> expectationFailure ("expected a successful shutdown to deliver RetiredCleanly, got RetirementFailed " <> show e)

    it "delivers RetirementFailed for a shutdown that throws, instead of propagating and skipping delivery" do
      done <- newEmptyMVar
      shutdownThenDeliver (Exception.throwIO (userError "supervisor stop failed")) done
      result <- takeMVar done
      case result of
        RetirementFailed _ _ -> pure ()
        RetiredCleanly -> expectationFailure "expected the shutdown failure to be delivered as RetirementFailed, not silently succeed"

    it "delivers RetirementFailed for a shutdown cancelled by an async exception mid-flight, rather than deadlocking the waiter forever" do
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
        RetirementFailed _ _ -> pure ()
        RetiredCleanly -> expectationFailure "expected the cancelled shutdown to be delivered as RetirementFailed"

  {- | Regression for the MEDIUM-severity audit finding: @app\/DevelMain.hs@'s
  initial \"no server running\" branch created its 'Foreign.Store'-backed
  slots (so a /subsequent/ @update@ call takes the \"server is already
  running\" branch) and then called @start tidRef done@ directly, with no
  handler at all. An ordinary acquisition\/spawn failure on that very
  first generation (before any child is ever spawned to eventually fill
  @done@ via 'shutdownThenDeliver') left @done@ permanently empty, so
  /every/ later @update@ call would take the \"already running\" branch
  and block forever waiting on a cell nothing was ever going to fill --
  deadlocking restart permanently.

  A /later/, independent review found the first fix for that
  (@restartGateForPreviousGeneration@\/@consumePreviousShutdownReplayable@,
  plus two separately-populated 'Foreign.Store' slots) was still
  structurally unsound: every generation shared the exact same @done@
  'Control.Concurrent.MVar.MVar' across restarts, so a first-generation
  acquisition\/spawn failure (filling that shared cell with 'Left', but
  never durably publishing a 'Control.Concurrent.ThreadId') left a
  /later/, entirely successful generation's own eventual shutdown result
  silently discarded by 'Control.Concurrent.MVar.tryPutMVar' (which can
  never overwrite an already-full cell) -- corrupting every subsequent
  restart's view of \"did the previous generation actually stop cleanly\"
  with a stale, unrelated failure from a generation that was never even
  spawned.

  These tests exercise 'restartManagedGeneration'\/'restartManagedGenerationUsing'
  directly -- the *exact* single-masked, single-lock transaction
  @DevelMain.hs@'s @update@ now calls -- so a regression at the lock's own
  state transitions, or the acquire\/spawn\/publish transition, is caught
  here directly, not merely in a locally-mirrored composition.
  -}
  describe "restartManagedGenerationUsing (DevelMain update: single-masked, single-lock RestartState transaction)" do
    it "a failed very-first start publishes StartFailed (not NotStarted, but functionally identical), so a subsequent restart attempt retries immediately -- never blocks, never replays a stale failure" do
      lock <- newMVar NotStarted
      acquireAttempted <- newIORef (0 :: Int)
      let failingAcquire = atomicModifyIORef' acquireAttempted (\n -> (n + 1, ())) >> Exception.throwIO (userError "initial acquisition failed") :: IO String
          release _ = pure ()
          body _ = pure ()
          finalize _ (_ :: Either SomeException ()) _done = pure ()
          cancelChild _ = expectationFailure "cancel should never run: nothing was ever spawned"
      result1 <-
        Exception.try @SomeException
          (restartManagedGenerationUsing lock forkIOWithUnmask cancelChild failingAcquire release body finalize)
      case result1 of
        Left _ -> pure ()
        Right () -> expectationFailure "expected the failing initial start to propagate"
      readIORef acquireAttempted `shouldReturn` 1
      afterFirstFailure <- readMVar lock
      case afterFirstFailure of
        StartFailed _ -> pure ()
        _ -> expectationFailure "expected the lock to read StartFailed after a failed very-first start"
      -- A second attempt must retry immediately (StartFailed is gated
      -- exactly like NotStarted), not replay the same failure forever.
      result2 <-
        Exception.try @SomeException
          (restartManagedGenerationUsing lock forkIOWithUnmask cancelChild failingAcquire release body finalize)
      case result2 of
        Left _ -> pure ()
        Right () -> expectationFailure "expected the second attempt to also fail (it uses the same failing acquire), but it must have genuinely retried"
      readIORef acquireAttempted `shouldReturn` 2

    it "a failed spawn (not just a synchronous acquisition failure) releases the acquired resource and publishes StartFailed" do
      lock <- newMVar NotStarted
      releaseCalled <- newIORef False
      let acquire = pure ("acquired-resource" :: String)
          release _ = writeIORef releaseCalled True
          body _ = pure ()
          finalize _ (_ :: Either SomeException ()) _done = pure ()
          cancelChild _ = expectationFailure "cancel should never run: no previous generation existed"
          fakeSpawn :: ((forall a. IO a -> IO a) -> IO ()) -> IO ThreadId
          fakeSpawn _ = Exception.throwIO Exception.ThreadKilled
      result <-
        Exception.try @SomeException
          (restartManagedGenerationUsing lock fakeSpawn cancelChild acquire release body finalize)
      case result of
        Left _ -> pure ()
        Right () -> expectationFailure "expected the failing spawn to propagate"
      readIORef releaseCalled `shouldReturn` True
      final <- readMVar lock
      case final of
        StartFailed _ -> pure ()
        _ -> expectationFailure "expected the lock to read StartFailed after a failed spawn"

    {- | Root-cause regression for the MEDIUM finding that a failed
    @spawn@ whose compensating @release@ ITSELF also fails must never be
    conflated with the (safe-to-retry-immediately) 'StartFailed' case
    above: the acquired resource's own teardown may not have completed,
    so a later caller must not be allowed to silently proceed as if
    nothing were live -- 'restartManagedGenerationUsing' itself must
    refuse to spawn a fresh generation on top of it. Mutation check:
    reverting 'StartCleanupFailed' (collapsing it back into
    'StartFailed' whenever @release@ itself throws, or discarding its
    retained retry capability) makes this fail, because
    'stopManagedGeneration' would then report 'NothingWasRunning' /
    silently succeed instead of genuinely retrying the exact retained
    @release@ action.
    -}
    it "a failed spawn whose compensating release ALSO fails publishes StartCleanupFailed carrying a retriable release, and refuses to start a new generation" do
      lock <- newMVar NotStarted
      let spawnErr = userError "spawn failed"
          cleanupErr = userError "release failed too"
          acquire = pure ("acquired-resource" :: String)
          release _ = Exception.throwIO cleanupErr
          body _ = pure ()
          finalize _ (_ :: Either SomeException ()) _done = pure ()
          cancelChild _ = expectationFailure "cancel should never run: no previous generation existed"
          fakeSpawn :: ((forall a. IO a -> IO a) -> IO ()) -> IO ThreadId
          fakeSpawn _ = Exception.throwIO spawnErr
      result <-
        Exception.try @SomeException
          (restartManagedGenerationUsing lock fakeSpawn cancelChild acquire release body finalize)
      case result of
        Left err -> show err `shouldBe` show cleanupErr
        Right () -> expectationFailure "expected the release failure to propagate"
      afterFailure <- readMVar lock
      case afterFailure of
        StartCleanupFailed observedSpawnErr observedCleanupErrs _retryRelease -> do
          show observedSpawnErr `shouldBe` show spawnErr
          show (NE.head observedCleanupErrs) `shouldBe` show cleanupErr
        _ -> expectationFailure "expected the lock to read StartCleanupFailed, not StartFailed"
      -- Unlike 'StartFailed', a subsequent attempt must refuse to
      -- proceed on top of the possibly-still-held resource: it re-raises
      -- the exact same cleanup failure rather than trying (and
      -- potentially spawning a second generation over unreleased
      -- resources).
      secondAcquireCalled <- newIORef False
      let secondAcquire = writeIORef secondAcquireCalled True >> pure ("should never be reached" :: String)
      retryResult <-
        Exception.try @SomeException
          (restartManagedGenerationUsing lock fakeSpawn cancelChild secondAcquire release body finalize)
      case retryResult of
        Left err -> show err `shouldBe` show cleanupErr
        Right () -> expectationFailure "expected the retry to re-raise the same unresolved cleanup failure"
      readIORef secondAcquireCalled `shouldReturn` False
      stillUnresolved <- readMVar lock
      case stillUnresolved of
        StartCleanupFailed _ _ _ -> pure ()
        _ -> expectationFailure "expected the lock to still read StartCleanupFailed, never reset by a mere retry attempt"

    {- | 'stopManagedGeneration''s own side of the same fix: unlike
    'StartFailed' (where nothing is live, so 'stopManagedGeneration'
    safely resets to 'NotStarted' and reports 'NothingWasRunning'),
    'StartCleanupFailed' retains an opaque, retriable release capability
    that 'stopManagedGeneration' is now the ONE place that actually
    retries -- see 'Api.Arkham.Lifecycle.stopManagedGeneration''s own
    Haddock. This covers all three outcomes deterministically: the
    retained release genuinely succeeding (lock clears to 'NotStarted',
    'StoppedCleanly' reported); genuinely failing again synchronously
    (lock keeps 'StartCleanupFailed' with the new failure prepended to
    its history, 'StopFailed' reported, the SAME capability retained for
    a further retry); and being asynchronously interrupted mid-retry
    (lock is restored to the exact prior 'StartCleanupFailed' untouched,
    and the interrupting exception is rethrown rather than mistaken for
    a retry failure). Mutation check: reverting to discarding the
    retained release (e.g. 'const (pure ())' -> always \"succeeds\"
    without truly retrying, or unconditionally reporting 'StopFailed'
    without ever clearing the lock on genuine success) makes one of
    these three cases fail.
    -}
    it "stopManagedGeneration retries the retained release: success clears the lock, sync failure keeps it (with the same capability), async interruption restores it untouched" do
      let spawnErr = userError "spawn failed"
          cleanupErr = userError "release failed too"

      -- Case 1: retained release genuinely succeeds on retry.
      succeedingRetryCalls <- newIORef (0 :: Int)
      let succeedingRetry = modifyIORef' succeedingRetryCalls (+ 1)
      succeedingRetryCap <- newManagedCleanup [succeedingRetry]
      lockSucceeds <-
        newMVar (StartCleanupFailed (Exception.toException spawnErr) (Exception.toException cleanupErr :| []) succeedingRetryCap)
      outcomeSucceeds <- stopManagedGeneration lockSucceeds (\_ -> expectationFailure "cancel should never run: nothing is live")
      case outcomeSucceeds of
        StoppedCleanly -> pure ()
        _ -> expectationFailure ("expected StoppedCleanly once the retained release genuinely succeeds, got " <> show outcomeSucceeds)
      readIORef succeedingRetryCalls `shouldReturn` 1
      afterSuccess <- readMVar lockSucceeds
      case afterSuccess of
        NotStarted -> pure ()
        _ -> expectationFailure "expected the lock to clear to NotStarted once the retained release genuinely succeeded"

      -- Case 2: retained release fails again (synchronously): the SAME
      -- capability is retained (never replaced by a no-op), and the new
      -- failure is prepended to the history.
      let retryErr = userError "release failed again on retry"
          failingRetry = Exception.throwIO retryErr
      failingRetryCap <- newManagedCleanup [failingRetry]
      lockFailsAgain <-
        newMVar (StartCleanupFailed (Exception.toException spawnErr) (Exception.toException cleanupErr :| []) failingRetryCap)
      outcomeFailsAgain <- stopManagedGeneration lockFailsAgain (\_ -> expectationFailure "cancel should never run: nothing is live")
      case outcomeFailsAgain of
        StopFailed err -> show err `shouldBe` show retryErr
        _ -> expectationFailure ("expected StopFailed carrying the new retry failure, got " <> show outcomeFailsAgain)
      afterFailsAgain <- readMVar lockFailsAgain
      case afterFailsAgain of
        StartCleanupFailed _ observedHistory retainedRetry -> do
          NE.head observedHistory `shouldSatisfy` (\e -> show e == show retryErr)
          -- the exact same capability must still be retained, callable again
          runManagedCleanup retainedRetry `shouldThrow` (\e -> show (e :: SomeException) == show retryErr)
        _ -> expectationFailure "expected the lock to still read StartCleanupFailed, retaining the exact retry capability"

      -- Case 3: the retry attempt is itself asynchronously interrupted
      -- mid-retry -- the capability must not be consumed, and the lock
      -- must be restored to the exact prior state. This is made fully
      -- deterministic (no timing race): the stopper thread only signals
      -- 'retryStarted' once it is genuinely blocked inside the retained
      -- capability, and the main thread only reads the lock after
      -- blocking on the stopper's own completion ('stopperFinished'),
      -- never after a fixed sleep.
      retryStarted <- newEmptyMVar
      stopperFinished <- newEmptyMVar
      let blockingRetry = putMVar retryStarted () >> forever (threadDelay 1000)
      blockingRetryCap <- newManagedCleanup [blockingRetry]
      lockInterrupted <-
        newMVar (StartCleanupFailed (Exception.toException spawnErr) (Exception.toException cleanupErr :| []) blockingRetryCap)
      stopperTid <- forkIO do
        outcome <-
          Exception.try @Exception.AsyncException
            (stopManagedGeneration lockInterrupted (\_ -> expectationFailure "cancel should never run: nothing is live"))
        putMVar stopperFinished outcome
      takeMVar retryStarted
      Exception.throwTo stopperTid Exception.ThreadKilled
      stopperOutcome <- takeMVar stopperFinished
      case stopperOutcome of
        Left Exception.ThreadKilled -> pure ()
        _ -> expectationFailure ("expected the interrupted retry itself to rethrow ThreadKilled, got " <> show stopperOutcome)
      afterInterrupted <- readMVar lockInterrupted
      case afterInterrupted of
        StartCleanupFailed observedSpawnErr' observedHistory' _retainedRetry -> do
          show observedSpawnErr' `shouldBe` show spawnErr
          show (NE.head observedHistory') `shouldBe` show cleanupErr
        _ -> expectationFailure "expected the lock to be restored to the exact StartCleanupFailed it held before the interrupted retry"

    {- | Deterministic (barrier-based, not timing-based) proof that
    concurrent 'stopManagedGeneration' callers against a shared
    'StartCleanupFailed' lock are genuinely serialized by the underlying
    'MVar' (never two retries running at once, never lost history):
    each of several concurrent callers barrier-waits for the others via
    'QSemN' before racing 'stopManagedGeneration' together against a
    retry that fails deterministically for the first N-1 observed
    attempts, then succeeds on the Nth. Exactly one caller must observe
    'StoppedCleanly' (the lock finally clearing to 'NotStarted'), and
    the retained release itself must only ever run once at a time
    (never re-entered), proving 'stopManagedGeneration''s own 'mask'
    genuinely excludes overlapping retries rather than merely racing
    reads of a shared 'IORef'.
    -}
    it "concurrent stopManagedGeneration callers against a StartCleanupFailed lock serialize the retained retry: never re-entered, exactly one genuine success" do
      let spawnErr = userError "spawn failed"
          cleanupErr = userError "release failed too"
          callerCount = 8 :: Int
          succeedOnAttempt = 5 :: Int
      inFlight <- newIORef (0 :: Int)
      maxObservedInFlight <- newIORef (0 :: Int)
      attemptCounter <- newIORef (0 :: Int)
      let retry = do
            n <- atomicModifyIORef' inFlight (\c -> (c + 1, c + 1))
            atomicModifyIORef' maxObservedInFlight (\m -> (max m n, ()))
            attemptNum <- atomicModifyIORef' attemptCounter (\c -> (c + 1, c + 1))
            -- Give any (incorrectly) concurrent retry a real chance to
            -- overlap before this one finishes.
            threadDelay 5000
            atomicModifyIORef' inFlight (\c -> (c - 1, ()))
            when (attemptNum < succeedOnAttempt) (Exception.throwIO (userError ("retry attempt " <> show attemptNum <> " deliberately fails")))
      lock <- do
        retryCap <- newManagedCleanup [retry]
        newMVar (StartCleanupFailed (Exception.toException spawnErr) (Exception.toException cleanupErr :| []) retryCap)
      resultsVar <- newMVar ([] :: [StopOutcome])
      let runCaller = do
            outcome <- stopManagedGeneration lock (\_ -> expectationFailure "cancel should never run: nothing is live")
            modifyMVar_ resultsVar (pure . (outcome :))
      finished <- mapM (const newEmptyMVar) [1 .. callerCount]
      forM_ finished \doneVar ->
        void (forkIO (runCaller `Exception.finally` putMVar doneVar ()))
      mapM_ takeMVar finished
      readIORef maxObservedInFlight >>= (`shouldSatisfy` (<= (1 :: Int)))
      results <- readMVar resultsVar
      let isStoppedCleanly outcome = case outcome of
            StoppedCleanly -> True
            _ -> False
      length (filter isStoppedCleanly results) `shouldBe` 1
      length results `shouldBe` callerCount
      finalState <- readMVar lock
      case finalState of
        NotStarted -> pure ()
        _ -> expectationFailure "expected the lock to have cleared to NotStarted once the retained release genuinely succeeded exactly once"

    {- | Root-cause regression for the companion (nested-acquisition)
    finding: when a nested 'acquireTransferringOwnershipOnSuccess' call
    (as production nests AWS supervisor -> Redis connection -> pub/sub
    thread -> DB pool inside 'Application.makeFoundation') has its OWN
    compensating cleanup fail (because ITS OWN body/use step failed),
    the resulting 'AcquisitionCleanupFailed' escapes as the outer
    @acquire@ action itself throwing -- never as a spawn failure. This
    must be distinguished from an ordinary \"nothing was ever created\"
    acquisition failure: 'restartManagedGenerationUsing' must map it to
    the very same 'StartCleanupFailed' (never a plain retryable
    'StartFailed'), preserving the nested exception's own original
    failure, its cleanup history, and its exact composed retry
    capability. Mutation check: collapsing this 'acquire'-side
    'AcquisitionCleanupFailed' into a plain 'AcquireFailed' (as if
    nothing were left to clean up) makes this fail, because a
    subsequent 'stopManagedGeneration' would then report
    'NothingWasRunning' instead of genuinely retrying the retained
    inner release.
    -}
    it "a nested acquireTransferringOwnershipOnSuccess whose own compensating cleanup fails, thrown from @acquire@ itself, composes into StartCleanupFailed, not StartFailed" do
      lock <- newMVar NotStarted
      let innerCleanupErr = userError "inner (Redis) release failed"
          innerBodyErr = userError "inner nested acquisition's own body failed"
          acquire :: IO String
          acquire =
            acquireTransferringOwnershipOnSuccess
              (pure ("inner-resource" :: String))
              (\_ -> Exception.throwIO innerCleanupErr)
              (\_innerRes -> Exception.throwIO innerBodyErr)
          release _ = expectationFailure "release should never run: acquire itself failed before any handle to release existed"
          body _ = pure ()
          finalize _ (_ :: Either SomeException ()) _done = pure ()
          cancelChild _ = expectationFailure "cancel should never run: no previous generation existed"
          fakeSpawn :: ((forall a. IO a -> IO a) -> IO ()) -> IO ThreadId
          fakeSpawn _ = expectationFailure "spawn should never run: acquire itself must fail first" >> myThreadId
      result <-
        Exception.try @SomeException
          (restartManagedGenerationUsing lock fakeSpawn cancelChild acquire release body finalize)
      case result of
        Left err -> show err `shouldBe` show innerCleanupErr
        Right () -> expectationFailure "expected the nested cleanup failure to propagate"
      afterFailure <- readMVar lock
      case afterFailure of
        StartCleanupFailed observedOriginal cleanupErrs _retryRelease -> do
          show observedOriginal `shouldBe` show innerBodyErr
          NE.head cleanupErrs `shouldSatisfy` (\e -> show e == show innerCleanupErr)
        _ -> expectationFailure "expected the nested cleanup failure to publish StartCleanupFailed, not StartFailed"

    {- | Deterministic (non-racy), matching the equivalent
    'forkTransferringOwnershipUsing' test: the fake @spawn@ blocks forever
    until the tester's own cancellation interrupts it, so this is the
    only way the call can ever proceed at all -- no timing race against a
    real, essentially-uninterruptible 'forkIOWithUnmask' to lose. Proves
    the acquired resource is already protected (masked, with @release@
    installed) the instant @acquire@ returns, even though no separate
    statement has run yet to say so.
    -}
    it "releases the resource if this thread is asynchronously cancelled strictly between acquisition returning and the spawn producing a handle" do
      released <- newEmptyMVar
      spawnReached <- newEmptyMVar
      neverFilled <- newEmptyMVar
      lock <- newMVar NotStarted
      let acquire = pure ("acquired-resource" :: String)
          release _ = putMVar released ()
          body _ = threadDelay maxBound
          finalize _ (_ :: Either SomeException ()) _done = pure ()
          cancelChild _ = expectationFailure "cancel should never run: no previous generation existed"
          fakeSpawn :: ((forall a. IO a -> IO a) -> IO ()) -> IO ThreadId
          fakeSpawn _threadBody = putMVar spawnReached () >> takeMVar neverFilled >> error "unreachable"
      callerResult <- newEmptyMVar
      callerTid <- forkIO do
        result <- Exception.try @Exception.AsyncException do
          restartManagedGenerationUsing lock fakeSpawn cancelChild acquire release body finalize
        putMVar callerResult result
      takeMVar spawnReached
      Exception.throwTo callerTid Exception.ThreadKilled
      result <- takeMVar callerResult
      result `shouldSatisfy` \case
        Left Exception.ThreadKilled -> True
        _ -> False
      readMVar released `shouldReturn` ()

    it "propagates an acquisition failure without calling release, spawn, or cancel" do
      lock <- newMVar NotStarted
      spawnCalled <- newIORef False
      releaseCalled <- newIORef False
      let acquire = Exception.throwIO (userError "getApplicationRepl failed") :: IO String
          release _ = writeIORef releaseCalled True
          body _ = pure ()
          finalize _ (_ :: Either SomeException ()) _done = pure ()
          cancelChild _ = expectationFailure "cancel should never run: no previous generation existed"
          fakeSpawn :: ((forall a. IO a -> IO a) -> IO ()) -> IO ThreadId
          fakeSpawn _ = writeIORef spawnCalled True >> myThreadId
      outcome <-
        Exception.try @Exception.SomeException do
          restartManagedGenerationUsing lock fakeSpawn cancelChild acquire release body finalize
      case outcome of
        Left _ -> pure ()
        Right () -> expectationFailure "expected the acquisition failure to propagate"
      readIORef spawnCalled `shouldReturn` False
      readIORef releaseCalled `shouldReturn` False

    {- | The structural fix itself, proven end-to-end: an earlier failed
    /initial/ start (which never spawned any child) must never corrupt a
    /later/, entirely successful generation's own eventual shutdown
    result. Under the old shared-@done@ design, this exact sequence would
    have left the later generation's own genuine 'Right ()' silently
    discarded by a 'Control.Concurrent.MVar.tryPutMVar' racing an
    already-full, stale cell. Here, each generation owns its own freshly
    created completion cell (see 'Running'), so there is nothing left for
    the two attempts to ever collide over.
    -}
    it "structural fix: a failed initial start never corrupts a later, successful generation's own completion cell" do
      lock <- newMVar NotStarted
      let failingAcquire = Exception.throwIO (userError "initial acquisition failed") :: IO String
          release _ = pure ()
          body _ = pure ()
          finalize _ (_ :: Either SomeException ()) _done = pure ()
          cancelChild _ = expectationFailure "cancel should never run: nothing was ever spawned"
      _ <-
        Exception.try @SomeException
          (restartManagedGenerationUsing lock forkIOWithUnmask cancelChild failingAcquire release body finalize)
          :: IO (Either SomeException ())
      -- Now start a genuinely successful generation, whose body blocks
      -- forever (so it can be observed live) until explicitly finalized.
      bodyStarted <- newEmptyMVar
      let goodAcquire = pure ("acquired-resource" :: String)
          goodBody _ = putMVar bodyStarted () >> forever (threadDelay maxBound)
          goodFinalize _ (outcome :: Either SomeException ()) newDone = shutdownThenDeliver (either Exception.throwIO pure outcome) newDone
      restartManagedGeneration lock killThread goodAcquire release goodBody goodFinalize
      takeMVar bodyStarted
      running <- readMVar lock
      case running of
        Running tid newDone -> do
          -- Cancel this genuinely live generation and confirm ITS OWN
          -- completion cell receives ITS OWN outcome (a cancellation,
          -- not the earlier, unrelated initial failure).
          killThread tid
          delivered <- readMVar newDone
          case delivered of
            RetirementFailed _ _ -> pure ()
            RetiredCleanly -> expectationFailure "expected the cancelled generation's own outcome (a RetirementFailed), not a stale value"
        _ -> expectationFailure "expected the lock to read Running after a successful start"

    {- | Structural, non-probabilistic regression for the async-exception-
    safety gap identified in independent review: the entire span from
    consulting a previous 'Running' generation through publishing the
    replacement's own handle must run under one continuous
    'Control.Exception.mask'\/'restore' boundary (restoring only around
    @acquire@ itself), never with ordinary unmasked bookkeeping code in
    between. Rather than racing a real exception against that window,
    this observes 'Control.Exception.getMaskingState' /from directly
    inside/ it, via the injected @cancel@ callback -- called exactly
    while retiring a previous 'Running' generation, still inside
    'restartManagedGenerationUsing''s own single 'mask'.
    -}
    it "the entire retire-through-publish transaction is masked: cancelling a previous generation happens under genuine protection" do
      priorDone <- newMVar RetiredCleanly
      priorTid <- forkIO (forever (threadDelay maxBound))
      lock <- newMVar (Running priorTid priorDone)
      maskingStateDuringCancel <- newEmptyMVar
      let cancelChild tid = do
            Exception.getMaskingState >>= putMVar maskingStateDuringCancel
            killThread tid
          acquire = pure ("acquired-resource" :: String)
          release _ = pure ()
          body _ = threadDelay maxBound
          finalize _ (_ :: Either SomeException ()) _done = pure ()
      restartManagedGeneration lock cancelChild acquire release body finalize
      observed <- takeMVar maskingStateDuringCancel
      observed `shouldBe` Exception.MaskedInterruptible
      -- Ownership was durably transferred; clean it up like a genuine
      -- restart would.
      Running newTid _ <- readMVar lock
      killThread newTid

    {- | Regression for the async-exception-safety gap identified in
    independent review: after retiring a previous generation (here,
    'NotStarted' -- nothing to retire) but before @acquire@ genuinely
    begins, an asynchronous exception targeting this thread must not be
    delivered in that narrow window (which would abort the call with the
    lock left in an inconsistent state). By making @acquire@ itself the
    /only/ operation in this whole call that can ever actually block (a
    'Control.Concurrent.MVar.takeMVar' on an 'Control.Concurrent.MVar.MVar'
    nothing will ever fill, which remains interruptible even under
    'Control.Exception.mask' by design), a successful, unblocked return
    from 'Control.Exception.throwTo' below can /only/ mean the exception
    was delivered there, at or after @acquire@ genuinely began running --
    never any earlier, during masked bookkeeping.
    -}
    it "defers a canary asynchronous exception through masked bookkeeping, delivering it only once acquire is genuinely running, where it is caught and safely re-recorded as StartFailed" do
      lock <- newMVar NotStarted
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
          finalize _ (_ :: Either SomeException ()) _done = pure ()
          cancelChild _ = expectationFailure "cancel should never run: nothing was ever spawned"
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
              (restartManagedGenerationUsing lock forkIOWithUnmask cancelChild acquire release body finalize)
          putMVar resultVar result
        Exception.throwTo tid (userError "canary")
      -- 'Exception.throwTo' above only returned because the canary was
      -- actually received -- confirm it was specifically inside
      -- @acquire@, never lost any earlier.
      takeMVar caughtInsideAcquire `shouldReturn` ()
      outcome <- takeMVar resultVar
      case outcome of
        Left err -> show err `shouldContain` "canary"
        Right () -> expectationFailure "expected the delivered canary exception to propagate as this call's result"
      final <- readMVar lock
      case final of
        StartFailed _ -> pure ()
        _ -> expectationFailure "expected the lock to read StartFailed, not be left describing something no longer true"

    it "no overlap/replacement after a failed stop: if retiring the previous generation itself fails, the lock is restored unchanged, and a later retry succeeds" do
      priorDone <- newEmptyMVar
      priorTid <- forkIO (forever (threadDelay maxBound))
      lock <- newMVar (Running priorTid priorDone)
      cancelAttempts <- newIORef (0 :: Int)
      let failingCancel _ = atomicModifyIORef' cancelAttempts (\n -> (n + 1, ())) >> Exception.throwIO (userError "cancel failed")
          acquire = expectationFailure "acquire should never run: retiring the previous generation must fail first" >> pure ("unreachable" :: String)
          release _ = pure ()
          body _ = pure ()
          finalize _ (_ :: Either SomeException ()) _done = pure ()
      result <-
        Exception.try @SomeException
          (restartManagedGenerationUsing lock forkIOWithUnmask failingCancel acquire release body finalize)
      case result of
        Left _ -> pure ()
        Right () -> expectationFailure "expected the failing cancel to propagate"
      readIORef cancelAttempts `shouldReturn` 1
      -- The lock is restored to describe the exact same still-live
      -- generation -- never silently marked as if it had been retired.
      unchanged <- readMVar lock
      case unchanged of
        Running tid _ | tid == priorTid -> pure ()
        _ -> expectationFailure "expected the lock to still read the exact same Running generation"
      killThread priorTid
      putMVar priorDone RetiredCleanly

    {- | Fully serialized through the shared lock: each concurrent attempt
    retires whatever the lock currently holds (cancelling and awaiting it)
    before spawning its own replacement, so no two attempts ever spawn
    concurrently-live generations, however they interleave. Every attempt
    must succeed (there is nothing for any of them to genuinely fail on),
    and the lock must end up describing exactly one live generation --
    never two, and never left in some torn intermediate state.

    @finalize@ here deliberately ignores @body@'s own exit outcome
    (matching production's own typical shape, @\\res outcome newDone ->
    shutdownThenDeliver (release' res) newDone@ -- see this function's own
    Haddock): a generation retired via an ordinary, expected cancellation
    always exits with a 'Left' 'Control.Exception.ThreadKilled' outcome,
    which is /not/ itself a teardown failure (see 'RestartState''s own
    Haddock, and the review's own \"Distinguish expected body ThreadKilled
    from teardown result correctly\"); only the actual release action
    (here, trivially @release res = pure ()@, since there is no real
    resource) can genuinely fail a retirement.
    -}
    it "concurrent restart attempts fully serialize through the same lock: every attempt succeeds and exactly one generation is left alive" do
      lock <- newMVar NotStarted
      let acquire = pure ("acquired-resource" :: String)
          release _ = pure ()
          body _ = forever (threadDelay maxBound)
          finalize res (_ :: Either SomeException ()) newDone = shutdownThenDeliver (release res) newDone
          attempt = Exception.try @SomeException (restartManagedGeneration lock killThread acquire release body finalize) :: IO (Either SomeException ())
      barrier <- newEmptyMVar
      resultVars <- mapM (const (newEmptyMVar :: IO (MVar (Either SomeException ())))) [1 .. 5 :: Int]
      for_ resultVars \resultVar ->
        void $ forkIO (takeMVar barrier >> attempt >>= putMVar resultVar)
      mapM_ (const (putMVar barrier ())) [1 .. 5 :: Int]
      results <- mapM takeMVar resultVars
      for_ results \result -> case result of
        Right () -> pure ()
        Left err -> expectationFailure ("expected every concurrent attempt to succeed, got: " <> show err)
      final <- readMVar lock
      case final of
        Running tid _ -> killThread tid
        _ -> expectationFailure "expected the lock to read Running after 5 successful concurrent attempts"

  describe "stopManagedGeneration (DevelMain shutdown: serialized against restartManagedGeneration via the same lock)" do
    it "returns NothingWasRunning and leaves the lock NotStarted when nothing has ever been started" do
      lock <- newMVar NotStarted
      stopped <- stopManagedGeneration lock killThread
      case stopped of
        NothingWasRunning -> pure ()
        _ -> expectationFailure ("expected NothingWasRunning, got " <> show stopped)
      final <- readMVar lock
      case final of
        NotStarted -> pure ()
        _ -> expectationFailure "expected the lock to still read NotStarted"

    it "returns NothingWasRunning and resets StartFailed to NotStarted" do
      lock <- newMVar (StartFailed (Exception.toException (userError "stale")))
      stopped <- stopManagedGeneration lock killThread
      case stopped of
        NothingWasRunning -> pure ()
        _ -> expectationFailure ("expected NothingWasRunning, got " <> show stopped)
      final <- readMVar lock
      case final of
        NotStarted -> pure ()
        _ -> expectationFailure "expected StartFailed to reset to NotStarted"

    it "cancels and awaits a live generation, returning StoppedCleanly and leaving the lock NotStarted" do
      readyVar <- newEmptyMVar
      tid <- forkIO (putMVar readyVar () >> forever (threadDelay maxBound))
      takeMVar readyVar
      done <- newEmptyMVar
      _ <- forkIO (shutdownThenDeliver (pure ()) done)
      lock <- newMVar (Running tid done)
      stopped <- stopManagedGeneration lock killThread
      case stopped of
        StoppedCleanly -> pure ()
        _ -> expectationFailure ("expected StoppedCleanly, got " <> show stopped)
      finalAfterStop <- readMVar lock
      case finalAfterStop of
        NotStarted -> pure ()
        _ -> expectationFailure "expected the lock to read NotStarted after a successful stop"
      status <- waitUntilTerminated tid
      status `shouldSatisfy` isTerminatedStatus

    it "on a failed stop, restores the exact same Running value rather than reporting NotStarted while the generation might still be alive" do
      done <- newEmptyMVar
      tid <- forkIO (forever (threadDelay maxBound))
      lock <- newMVar (Running tid done)
      let failingCancel _ = Exception.throwIO (userError "cancel failed")
      result <- Exception.try @SomeException (stopManagedGeneration lock failingCancel)
      case result of
        Left _ -> pure ()
        Right (outcome :: StopOutcome) -> expectationFailure ("expected the failing cancel to propagate, got " <> show outcome)
      unchanged <- readMVar lock
      case unchanged of
        Running observedTid _ | observedTid == tid -> pure ()
        _ -> expectationFailure "expected the lock to still read the exact same Running generation after a failed stop"
      killThread tid
      putMVar done RetiredCleanly

    {- | Direct regression for the MEDIUM-severity finding that both
    'restartManagedGenerationUsing' and 'stopManagedGeneration' used to
    discard the retired generation's own teardown outcome via
    @() <$ readMVar priorDone@: a previous generation's own
    'shutdownThenDeliver'\/@finalize@ call genuinely running and
    filling its completion cell with 'Left' (its own teardown failed,
    e.g. 'Application.shutdownApp' itself threw) must never be reported
    as 'StoppedCleanly', nor silently reset the lock to 'NotStarted'
    (which would let a later caller believe there is nothing left to
    release, even though this generation's resources may not actually
    have been freed).

    Mutation check: reverting 'stopManagedGeneration' to
    @() <$ readMVar priorDone@ (discarding the 'Either') makes this test
    fail: it would report 'StoppedCleanly' and reset the lock to
    'NotStarted' instead of 'StopFailed'\/'RetireFailed'.
    -}
    it "reports StopFailed (not StoppedCleanly) and sets the lock to RetireFailed when the retired generation's own teardown itself failed" do
      tid <- forkIO (forever (threadDelay maxBound))
      done <- newEmptyMVar
      let teardownErr = Exception.toException (userError "shutdownApp failed")
      -- Simulates a generation whose body has already exited and whose
      -- own 'shutdownThenDeliver'\/@finalize@ call has already, genuinely
      -- completed -- but reported its own teardown failure, exactly as a
      -- real 'Application.shutdownApp' throwing would.
      putMVar done (RetirementFailed teardownErr Nothing)
      lock <- newMVar (Running tid done)
      stopped <- stopManagedGeneration lock killThread
      case stopped of
        StopFailed err -> show err `shouldBe` show teardownErr
        _ -> expectationFailure ("expected StopFailed, got " <> show stopped)
      final <- readMVar lock
      case final of
        RetireFailed observedTid _ _ | observedTid == tid -> pure ()
        _ -> expectationFailure "expected the lock to read RetireFailed after a genuinely failed teardown"
      killThread tid

    {- | A 'RetireFailed' lock is never silently treated as \"nothing to
    stop\": both this call and a later one must keep re-reporting the
    exact same recorded failure, never resetting to 'NotStarted' on
    their own (see 'RetireFailed''s own Haddock for why an operator must
    explicitly clear it instead).
    -}
    it "re-reports the exact same StopFailed when the lock is already RetireFailed, without resetting to NotStarted" do
      tid <- forkIO (forever (threadDelay maxBound))
      let priorErr = Exception.toException (userError "already-recorded teardown failure")
      lock <- newMVar (RetireFailed tid (priorErr :| []) Nothing)
      stopped <- stopManagedGeneration lock killThread
      case stopped of
        StopFailed err -> show err `shouldBe` show priorErr
        _ -> expectationFailure ("expected StopFailed, got " <> show stopped)
      final <- readMVar lock
      case final of
        RetireFailed observedTid _ _ | observedTid == tid -> pure ()
        _ -> expectationFailure "expected the lock to still read RetireFailed, not reset to NotStarted"
      killThread tid

  describe "restartManagedGenerationUsing propagates a retired generation's own teardown outcome (never discards the Either)" do
    {- | Direct regression, on 'restartManagedGenerationUsing' itself,
    for the same MEDIUM-severity finding as the 'stopManagedGeneration'
    tests above: if the previous generation's own completion cell holds
    'Left' (its teardown failed), this must re-raise that exact failure
    and never proceed to acquire\/spawn a replacement, and must leave
    the lock 'RetireFailed', never 'Running' (a replacement) nor
    'NotStarted'.

    Mutation check: reverting to @() <$ readMVar priorDone@ makes this
    test fail: a replacement generation would be started (@acquire@
    would be called and the lock would read 'Running') instead of the
    call re-raising and leaving the lock 'RetireFailed'.
    -}
    it "re-raises the retired generation's own teardown failure and never starts a replacement" do
      tid <- forkIO (forever (threadDelay maxBound))
      done <- newEmptyMVar
      let teardownErr = Exception.toException (userError "shutdownApp failed")
      putMVar done (RetirementFailed teardownErr Nothing)
      lock <- newMVar (Running tid done)
      acquireCalledRef <- newIORef False
      result <-
        Exception.try @SomeException
          ( restartManagedGenerationUsing
              lock
              forkIOWithUnmask
              killThread
              (writeIORef acquireCalledRef True)
              (\() -> pure ())
              (\() -> forever (threadDelay maxBound))
              (\() outcome newDone -> shutdownThenDeliver (either Exception.throwIO pure outcome) newDone)
          )
      case result of
        Left err -> show err `shouldBe` show teardownErr
        Right () -> expectationFailure "expected the retired generation's own teardown failure to propagate"
      acquireCalled <- readIORef acquireCalledRef
      acquireCalled `shouldBe` False
      final <- readMVar lock
      case final of
        RetireFailed observedTid _ _ | observedTid == tid -> pure ()
        _ -> expectationFailure "expected the lock to read RetireFailed, not a fresh replacement"
      killThread tid

    it "re-raises the exact same failure again, still without starting a replacement, when the lock is already RetireFailed" do
      tid <- forkIO (forever (threadDelay maxBound))
      let priorErr = Exception.toException (userError "already-recorded teardown failure")
      lock <- newMVar (RetireFailed tid (priorErr :| []) Nothing)
      acquireCalledRef <- newIORef False
      result <-
        Exception.try @SomeException
          ( restartManagedGenerationUsing
              lock
              forkIOWithUnmask
              killThread
              (writeIORef acquireCalledRef True)
              (\() -> pure ())
              (\() -> forever (threadDelay maxBound))
              (\() outcome newDone -> shutdownThenDeliver (either Exception.throwIO pure outcome) newDone)
          )
      case result of
        Left err -> show err `shouldBe` show priorErr
        Right () -> expectationFailure "expected the already-recorded teardown failure to propagate again"
      acquireCalled <- readIORef acquireCalledRef
      acquireCalled `shouldBe` False
      final <- readMVar lock
      case final of
        RetireFailed observedTid _ _ | observedTid == tid -> pure ()
        _ -> expectationFailure "expected the lock to still read RetireFailed"
      killThread tid


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
        RetirementFailed err _ -> expectationFailure ("expected a clean shutdown, got: " <> show err)
        RetiredCleanly -> pure ()
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

    {- | Regression for the MEDIUM finding that 'releaseAll' previously used
    plain 'Control.Exception.try' (which catches /any/ exception,
    synchronous or asynchronous): if 'Application.shutdownApp' were
    cancelled (e.g. 'Exception.ThreadKilled') while blocked inside one of
    the release actions in the list, the old code recorded the
    cancellation as an ordinary release "failure" and carried on running
    every later release in the list before finally re-raising it --
    exactly the "effectively uninterruptible shutdown" the reviewer
    flagged, since a slow or hung later release could then delay the
    cancellation indefinitely. 'releaseAll' now uses 'UE.try' (the
    "safe-exceptions"-style variant already used elsewhere in this
    module), which only catches synchronous exceptions and immediately
    rethrows anything asynchronous -- so cancellation aborts the whole
    'releaseAll' call right away, running no further releases in the
    list, rather than merely being queued behind them.

    This test injects a genuine 'Exception.throwTo' from a second thread,
    synchronized via an 'MVar' gate so there is no timing race to lose:
    the first release action signals @firstReached@ the instant it is
    running and then blocks on @neverFilled@ (a stand-in for a real
    release that is itself doing interruptible work, e.g. awaiting a
    supervised thread), so the tester's cancellation can only ever land
    while genuinely inside that first release action, not by racing
    'forkIO' startup.

    Unlike an earlier (rejected) version of this test, the interrupted
    first release here is genuinely retriable rather than a dead end: it
    is a stateful action (an 'IORef' call-counter) that blocks forever
    only on its FIRST invocation and succeeds trivially on any later one
    -- exactly like a real release whose underlying resource is still
    live and can genuinely be torn down on a second attempt. This lets
    the test prove ALL THREE aspects of the fix at once: cancellation
    still propagates immediately (the second release has not run right
    after the injected 'Exception.ThreadKilled'), the propagated
    exception is the ORIGINAL, unwrapped 'Exception.ThreadKilled' itself
    -- never a synchronously-shaped 'ReleaseAllFailed' (a MEDIUM
    \"cleanup progress\" finding this round: wrapping cancellation that
    way gave a caller with no durable place of its own to retain the
    outstanding capability -- production: 'Application.handler' -- what
    looked like an ordinary, already-classified failure for a shutdown
    that was, in fact, still asynchronously in flight, with nothing left
    responsible for finishing it) -- and the exact, still-outstanding
    capability is nonetheless never abandoned: it is durably transferred
    to 'Api.Arkham.Lifecycle.drainOwnedCleanup''s own global owner
    /before/ that original exception is rethrown, and a later,
    independent 'drainOwnedCleanup' call (standing in for
    @app\/DevelMain.hs@\'s or 'Application.handler''s own opportunistic
    calls) genuinely completes what cancellation interrupted (both
    releases have run once it returns).

    Mutation check: reverting 'releaseAll' to plain 'Control.Exception.try'
    makes the cancellation-propagates assertion fail deterministically --
    the second release's @secondRan@ flag would end up 'True' before the
    retry is ever invoked (plain 'try' catches the 'Exception.ThreadKilled',
    "succeeds" in registering it as this release's failure, and moves on
    to run the second release before ultimately re-raising). Reverting
    'releaseAll' to wrap the cancellation in 'ReleaseAllFailed' again (as
    an earlier, rejected version of this fix did) makes the
    \"propagated exception is the original 'Exception.ThreadKilled'\"
    assertion fail. Reverting the async branch to simply rethrow without
    ever calling 'PendingCleanupOwner.transferPendingCleanup' first makes
    the final 'drainOwnedCleanup' recovery assertion fail: there would be
    nothing durably owned left to drain, and @firstCalls@\/@secondRan@
    would never advance.
    -}
    it "propagates the ORIGINAL asynchronous cancellation immediately, without running any later release, but durably transfers ownership so a later drainOwnedCleanup completes both" do
      firstReached <- newEmptyMVar
      neverFilled <- newEmptyMVar
      firstCalls <- newIORef (0 :: Int)
      secondRan <- newIORef False
      let firstRelease = do
            n <- atomicModifyIORef' firstCalls (\c -> (c + 1, c + 1))
            if n == 1
              then putMVar firstReached () >> takeMVar neverFilled >> error "unreachable"
              else pure ()
          secondRelease = atomicModifyIORef' secondRan (const (True, ()))
      callerResult <- newEmptyMVar
      callerTid <- forkIO do
        result <- Exception.try @Exception.SomeException (releaseAll [firstRelease, secondRelease])
        putMVar callerResult result
      takeMVar firstReached
      Exception.throwTo callerTid Exception.ThreadKilled
      result <- takeMVar callerResult
      -- Cancellation propagated immediately: the second release never ran.
      readIORef secondRan `shouldReturn` False
      case result of
        Left err -> case Exception.fromException err of
          Just Exception.ThreadKilled -> pure ()
          _ -> expectationFailure ("expected the ORIGINAL, unwrapped ThreadKilled to propagate out of releaseAll, got " <> show err)
        Right () -> expectationFailure "expected the injected ThreadKilled to propagate out of releaseAll"
      -- Never abandoned: the exact, still-outstanding capability was
      -- durably transferred away before that exception was rethrown, so
      -- an independent later caller (standing in for DevelMain/handler's
      -- own opportunistic calls) can genuinely finish it.
      drainResult <- drainOwnedCleanup
      case drainResult of
        Right () -> pure ()
        Left remaining -> expectationFailure ("expected drainOwnedCleanup to genuinely finish the transferred cleanup, but it still reports " <> show remaining)
      readIORef firstCalls `shouldReturn` 2
      readIORef secondRan `shouldReturn` True

    {- | End-to-end regression for the same MEDIUM finding, but through the
    real production path: @app\/DevelMain.hs@'s own actual
    'shutdownThenDeliverRecordingReceipt'\/'releaseAllRecordingReceipt'
    wiring (a freshly created, per-attempt @receiptSink@ shared between
    both calls -- exactly as @update@'s own @finalize@ closure builds
    it), a genuine 'Running' lock, and 'stopManagedGeneration' itself --
    rather than calling 'releaseAll' directly. Proves that when the
    retired generation's own shutdown was genuinely asynchronously
    cancelled (never a purely synchronous 'ReleaseAllFailed'),
    'classifyRetirementFailure' correctly reports NO local retry
    capability (this module cannot itself prove one safe, and one was
    never captured locally to begin with -- see 'RetireFailed''s own
    Haddock) but DOES capture the exact 'PendingCleanupOwner.CleanupReceipt'
    identifying the durably-transferred remainder, and that this is
    genuinely, observably useful later: once an independent
    'drainOwnedCleanup' call (standing in for @app\/DevelMain.hs@\'s own
    opportunistic call after @shutdown@) finishes that transferred work
    out-of-band, a SUBSEQUENT 'stopManagedGeneration' call against the
    exact same, still-'RetireFailed' lock can observe -- via
    'PendingCleanupOwner.attemptCleanupReceipt' -- that this exact
    receipt's own work already terminated, and finally clears the lock
    to 'NotStarted', rather than replaying the same 'StopFailed' forever
    with no way out short of an operator manually resetting it.

    Mutation check: reverting the async branch of 'runManagedReleasePlan'
    to skip 'PendingCleanupOwner.transferPendingCleanup' (abandoning the
    interrupted release rather than transferring it) makes the final
    'drainOwnedCleanup' assertion fail: there would be nothing durably
    owned left to drain, and @firstCalls@\/@secondRan@ would never
    advance. Reverting 'releaseAll' to wrap async cancellation back into
    a synchronously-shaped 'ReleaseAllFailed' makes the
    \"RetireFailed carries no local retry\" assertion fail (it would
    incorrectly regain one), reintroducing a caller with no durable
    place of its own (production: 'Application.handler') receiving what
    looks like an ordinary, already-classified failure for a shutdown
    still asynchronously in flight. Reverting 'classifyRetirementFailure'
    to always report 'Nothing' (discarding the receipt) makes the
    'GlobalReceipt' assertion fail, and permanently reintroduces the
    original MEDIUM-severity finding this whole test exists to close: a
    'RetireFailed' lock that can /never/ observe out-of-band completion
    and is therefore stuck forever short of a manual operator reset.
    -}
    it "stopManagedGeneration reports RetireFailed with a GlobalReceipt (no local retry) after a genuinely cancelled shutdown, and a SUBSEQUENT stopManagedGeneration call observes that receipt's own later, out-of-band completion and clears the lock" do
      firstReached <- newEmptyMVar
      neverFilled <- newEmptyMVar
      firstCalls <- newIORef (0 :: Int)
      secondRan <- newIORef False
      let firstRelease = do
            n <- atomicModifyIORef' firstCalls (\c -> (c + 1, c + 1))
            if n == 1
              then putMVar firstReached () >> takeMVar neverFilled >> error "unreachable"
              else pure ()
          secondRelease = atomicModifyIORef' secondRan (const (True, ()))
      done <- newEmptyMVar
      receiptSink <- newIORef Nothing
      genTid <- forkIOWithUnmask \unmask ->
        shutdownThenDeliverRecordingReceipt
          receiptSink
          (unmask (releaseAllRecordingReceipt receiptSink [firstRelease, secondRelease]))
          done
      takeMVar firstReached
      lock <- newMVar (Running genTid done)
      -- 'stopManagedGeneration''s OWN internal @cancel priorHandle@
      -- delivers the cancellation that genuinely interrupts the still-
      -- blocked first release (rather than this test pre-empting it
      -- manually, which would race 'stopManagedGeneration''s own
      -- cancellation); the generation's own teardown is thereby
      -- genuinely interrupted mid-release, and the second release must
      -- not have run.
      stopOutcome <- stopManagedGeneration lock killThread
      case stopOutcome of
        StopFailed _ -> pure ()
        _ -> expectationFailure ("expected the stop to report StopFailed, got " <> show stopOutcome)
      readIORef secondRan `shouldReturn` False
      afterStop <- readMVar lock
      receipt <- case afterStop of
        RetireFailed _ _ (Just (GlobalReceipt r)) -> pure r
        RetireFailed _ _ Nothing ->
          expectationFailure "expected RetireFailed to carry a GlobalReceipt: the interrupted teardown's own remainder was durably transferred and this generation's own receipt sink observed it"
            >> error "unreachable"
        _ -> expectationFailure "expected the lock to read RetireFailed after the interrupted teardown" >> error "unreachable"
      -- Never abandoned even though the LOCAL lock has nothing of its
      -- own left to retry directly: the interrupted release was
      -- durably transferred away, and completes here, via an
      -- independent caller that never even sees @receipt@ itself.
      drainResult <- drainOwnedCleanup
      case drainResult of
        Right () -> pure ()
        Left remaining -> expectationFailure ("expected drainOwnedCleanup to genuinely finish the transferred cleanup, but it still reports " <> show remaining)
      readIORef firstCalls `shouldReturn` 2
      readIORef secondRan `shouldReturn` True
      -- The actual Finding-4 regression: a SUBSEQUENT stopManagedGeneration
      -- call against the exact same lock must now be able to observe
      -- that this exact receipt's own work already terminated --
      -- out-of-band, via 'drainOwnedCleanup' above, not through this
      -- call's own retry at all -- and finally clear the lock, rather
      -- than replaying the same StopFailed forever with no way out
      -- short of an operator manually resetting it.
      secondStopOutcome <- stopManagedGeneration lock killThread
      case secondStopOutcome of
        StoppedCleanly -> pure ()
        _ -> expectationFailure ("expected the second stop to observe the already-terminated global receipt and report StoppedCleanly, got " <> show secondStopOutcome)
      finalLock <- readMVar lock
      case finalLock of
        NotStarted -> pure ()
        _ -> expectationFailure "expected the lock to finally clear to NotStarted once the global receipt's own work was confirmed terminal"
      -- Sanity: @receipt@ genuinely identifies the same, already-
      -- terminated capability -- polling it again directly must also
      -- report success, never busy\/failed\/some fresh re-run.
      polledAgain <- attemptCleanupReceipt globalPendingCleanupOwner receipt
      case polledAgain of
        ReceiptSucceeded -> pure ()
        other -> expectationFailure ("expected polling the exact same receipt again to still report ReceiptSucceeded, got " <> show other)

  {- | Root-cause regression, on 'acquireTransferringOwnershipOnSuccess'
  itself, for the second MEDIUM \"cleanup-progress\" finding: the old
  nested-retry composition (@innerRetry >> release res@) re-ran the
  INNER resource's own release every time the retained retry capability
  was invoked, even after that inner release had already genuinely
  succeeded on an earlier retry -- so if the OUTER release kept failing,
  a non-idempotent inner release (e.g. one that throws if called twice)
  would make every subsequent retry fail forever on the outer release
  alone, since the (already-doomed) inner release would always be
  re-attempted first. 'retryableCleanup' fixes this by durably, and
  permanently, dropping each step out of the retained retry action the
  instant it succeeds, so a later retry only ever re-attempts what
  genuinely still remains outstanding.

  This drives the nested failure entirely through the real production
  path -- 'restartManagedGenerationUsing' with a nested
  'acquireTransferringOwnershipOnSuccess' inside its own @acquire@ (the
  shape 'Api.Arkham.AwsEnvSupervisor' actually uses to acquire a
  dependency's own dependency) -- and 'stopManagedGeneration' to drive
  the retries, rather than calling 'acquireTransferringOwnershipOnSuccess'
  directly, so a regression at either function is caught.

  Mutation check: reverting the retry composition back to
  @innerRetry >> release res@ makes the final assertion
  (@innerReleaseCalls \`shouldReturn\` 2@ after the second retry) fail:
  the inner release would be invoked a THIRD time (once it is genuinely
  non-idempotent -- see @innerRelease@ below, which fails on any call
  after its first) and the whole second retry would report 'StopFailed'
  with the inner failure, never reaching the outer release at all, so
  the outer release would never even get its final successful attempt
  and the lock would never clear to 'NotStarted'.
  -}

  {- | Direct, unit-level regression for the MEDIUM \"sync release
  failures dropped\" finding against 'runManagedReleasePlan' itself
  (production: entirely inside 'releaseAll'): its own structural
  predecessor wrote back only @rest@ (whatever came strictly after the
  failed action) as the retained plan, silently dropping the
  synchronously-failed action itself out of the retry -- so a later
  retry against that plan could report a clean pass without ever having
  genuinely retried the one action that actually still needs releasing
  (e.g. a still-live AWS supervisor). These tests exercise
  'newManagedReleasePlan'\/'runManagedReleasePlan' directly rather than
  only through 'releaseAll''s own all-or-nothing exception, so the
  retained plan's own exact contents (not just what 'releaseAll'
  ultimately throws) are directly observable.
  -}
  describe "ManagedReleasePlan (releaseAll's own retry capability: independent, attempt-every-action, retains only what genuinely still failed)" do
    it "after a partial failure, retries only the synchronously-failed action -- an action that already succeeded is never re-attempted" do
      failingCalls <- newIORef (0 :: Int)
      succeedingCalls <- newIORef (0 :: Int)
      let retryErr = userError "still failing"
          -- Fails on its first invocation, succeeds on every later one --
          -- exactly like a real release whose underlying resource is
          -- still live and can genuinely be torn down on a retry.
          failingAction = do
            n <- atomicModifyIORef' failingCalls (\c -> (c + 1, c + 1))
            when (n == 1) (Exception.throwIO retryErr)
          succeedingAction = atomicModifyIORef' succeedingCalls (\c -> (c + 1, ()))
      plan <- newManagedReleasePlan [failingAction, succeedingAction]

      firstOutcome <- runManagedReleasePlan plan
      case firstOutcome of
        Left failures -> NE.length failures `shouldBe` 1
        Right () -> expectationFailure "expected the first pass to report the synchronous failure"
      -- Both actions were attempted this pass (the failure did not skip
      -- the later, independent action)...
      readIORef failingCalls `shouldReturn` 1
      readIORef succeedingCalls `shouldReturn` 1

      -- ...but only the FAILED action is genuinely retained: retrying
      -- the SAME plan again must never re-run the action that already
      -- succeeded.
      secondOutcome <- runManagedReleasePlan plan
      case secondOutcome of
        Right () -> pure ()
        Left failures -> expectationFailure ("expected the retry to genuinely succeed once the retained action stops failing, got " <> show failures)
      readIORef failingCalls `shouldReturn` 2
      readIORef succeedingCalls `shouldReturn` 1

    {- | Mutation check proving the test above is load-bearing: reverting
    'runManagedReleasePlan' to retain only the actions strictly after the
    failed one (production's own former @writeIORef remainingRef rest@,
    silently dropping the failed action itself) is verified by hand
    against this exact test above -- see the accompanying commit
    message's mutation-testing note; it is not repeated as an
    independent test here because doing so faithfully requires actually
    mutating 'runManagedReleasePlan' itself, not a local stand-in.
    -}

    {- | Deterministic (barrier-based, not timing-based) proof of the
    companion MEDIUM \"progress update interruptible\/ledger
    unserialized\" finding, directly against 'ManagedReleasePlan': many
    concurrent callers racing 'runManagedReleasePlan' against the exact
    same plan, where the sole retained action fails deterministically
    for the first N-1 observed attempts and succeeds on the Nth, must
    never observe two attempts of that action running at once (the
    whole pass is one masked, 'MVar'-serialized transaction), and exactly
    one caller ever observes the clean, successful pass.
    -}
    it "concurrent retries against the exact same plan serialize: the retained action is never invoked twice at once, and settles after exactly the expected number of attempts" do
      let callerCount = 8 :: Int
          succeedOnAttempt = 5 :: Int
      inFlight <- newIORef (0 :: Int)
      maxObservedInFlight <- newIORef (0 :: Int)
      attemptCounter <- newIORef (0 :: Int)
      let retryAction = do
            n <- atomicModifyIORef' inFlight (\c -> (c + 1, c + 1))
            atomicModifyIORef' maxObservedInFlight (\m -> (max m n, ()))
            attemptNum <- atomicModifyIORef' attemptCounter (\c -> (c + 1, c + 1))
            -- Give any (incorrectly) concurrent retry a genuine chance to
            -- overlap before this one finishes.
            threadDelay 5000
            atomicModifyIORef' inFlight (\c -> (c - 1, ()))
            when (attemptNum < succeedOnAttempt) (Exception.throwIO (userError ("retry attempt " <> show attemptNum <> " deliberately fails")))
      plan <- newManagedReleasePlan [retryAction]
      resultsVar <- newMVar ([] :: [Either (NonEmpty SomeException) ()])
      finished <- mapM (const newEmptyMVar) [1 .. callerCount]
      forM_ finished \doneVar ->
        void $ forkIO do
          outcome <- runManagedReleasePlan plan
          modifyMVar_ resultsVar (pure . (outcome :))
          putMVar doneVar ()
      mapM_ takeMVar finished
      -- Never re-entered: whichever single caller is genuinely attempting
      -- the retained action at any moment, it is never overlapped by
      -- another concurrent caller's own attempt.
      readIORef maxObservedInFlight >>= (`shouldSatisfy` (<= (1 :: Int)))
      -- The action is retried exactly enough times to succeed once --
      -- never fewer (every caller genuinely serializes through the same
      -- retained action) and never more (once it succeeds, it is
      -- permanently dropped from the plan, so any caller whose own pass
      -- runs afterwards sees an already-empty plan and never re-invokes
      -- it, no matter how many callers are still racing).
      readIORef attemptCounter `shouldReturn` succeedOnAttempt
      results <- readMVar resultsVar
      length results `shouldBe` callerCount
      let isClean outcome = case outcome of
            Right () -> True
            Left _ -> False
      -- Exactly the callers whose own pass genuinely attempted (and
      -- failed) the action before it finally succeeded observe a
      -- failure; every other caller -- whether it succeeded outright or
      -- raced in after the plan had already emptied -- observes a clean
      -- pass. (Once the retained action succeeds, the plan is
      -- permanently empty, so every later concurrent caller trivially
      -- observes a clean pass too -- this is not itself a bug: nothing
      -- remains for them to retry.)
      length (filter (not . isClean) results) `shouldBe` (succeedOnAttempt - 1)
      length (filter isClean results) `shouldBe` (callerCount - (succeedOnAttempt - 1))

  describe "acquireTransferringOwnershipOnSuccess nested-release retry never re-runs an already-succeeded inner release" do
    it "the outer capability's retry only re-attempts what genuinely still remains, never a resource whose release already succeeded" do
      innerReleaseCalls <- newIORef (0 :: Int)
      outerReleaseCalls <- newIORef (0 :: Int)
      let -- Inner release: fails on its first call, then succeeds
          -- exactly once -- and, being non-idempotent, THROWS if ever
          -- invoked again after that (this is what makes the mutation
          -- above detectable: the old buggy composition would call it a
          -- third time).
          innerRelease _ = do
            n <- atomicModifyIORef' innerReleaseCalls (\c -> (c + 1, c + 1))
            case n of
              1 -> Exception.throwIO (userError "inner release failed (1st attempt)")
              2 -> pure ()
              _ -> Exception.throwIO (userError "inner release called again after already succeeding!")
          -- Outer release: fails its first two calls, succeeds on the
          -- third (i.e. the second retry).
          outerRelease _ = do
            n <- atomicModifyIORef' outerReleaseCalls (\c -> (c + 1, c + 1))
            when (n < 3) (Exception.throwIO (userError ("outer release failed (attempt " <> show n <> ")")))
          -- Nested exactly the way 'Api.Arkham.AwsEnvSupervisor' acquires
          -- a dependency's own dependency: the OUTER
          -- 'acquireTransferringOwnershipOnSuccess''s own body performs a
          -- second, INNER 'acquireTransferringOwnershipOnSuccess', whose
          -- body always fails -- so the inner release runs (and fails)
          -- first, then the outer release runs (and fails) too, and the
          -- whole nested acquisition surfaces as a single
          -- 'AcquisitionCleanupFailed' with both retained.
          nestedAcquire :: IO Int
          nestedAcquire =
            acquireTransferringOwnershipOnSuccess
              (pure (100 :: Int))
              outerRelease
              ( \_ ->
                  acquireTransferringOwnershipOnSuccess
                    (pure (0 :: Int))
                    innerRelease
                    (\_ -> Exception.throwIO (userError "inner body always fails"))
              )
          fakeSpawn :: ((forall a. IO a -> IO a) -> IO ()) -> IO ThreadId
          fakeSpawn _ = Exception.throwIO Exception.ThreadKilled
      lock <- newMVar NotStarted
      firstResult <-
        Exception.try @SomeException
          ( restartManagedGenerationUsing
              lock
              fakeSpawn
              (\_ -> expectationFailure "cancel should never run: no previous generation existed")
              nestedAcquire
              (\_ -> pure ())
              (\_ -> pure ())
              (\_ _ _ -> pure ())
          )
      case firstResult of
        Left _ -> pure ()
        Right () -> expectationFailure "expected the nested acquire failure to propagate"
      readIORef innerReleaseCalls `shouldReturn` 1
      readIORef outerReleaseCalls `shouldReturn` 1
      afterAcquire <- readMVar lock
      case afterAcquire of
        StartCleanupFailed _ _ _ -> pure ()
        _ -> expectationFailure "expected the lock to read StartCleanupFailed after the nested acquire/release failure"
      -- First retry (via stopManagedGeneration): inner now succeeds
      -- (call 2), outer still fails (call 2) -> StopFailed, but the
      -- inner step must never be attempted again from here on.
      firstRetry <- stopManagedGeneration lock (\_ -> expectationFailure "cancel should never run: nothing is live")
      case firstRetry of
        StopFailed _ -> pure ()
        _ -> expectationFailure ("expected the first retry to report StopFailed (outer still failing), got " <> show firstRetry)
      readIORef innerReleaseCalls `shouldReturn` 2
      readIORef outerReleaseCalls `shouldReturn` 2
      -- Second retry: must NOT re-run the inner release (still 2); outer
      -- finally succeeds (call 3) -> StoppedCleanly, lock clears.
      secondRetry <- stopManagedGeneration lock (\_ -> expectationFailure "cancel should never run: nothing is live")
      case secondRetry of
        StoppedCleanly -> pure ()
        _ -> expectationFailure ("expected the second retry to genuinely complete and report StoppedCleanly, got " <> show secondRetry)
      readIORef innerReleaseCalls `shouldReturn` 2
      readIORef outerReleaseCalls `shouldReturn` 3
      final <- readMVar lock
      case final of
        NotStarted -> pure ()
        _ -> expectationFailure "expected the lock to clear to NotStarted once the outer release genuinely completed"


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
      loserReady <- newEmptyMVar
      let -- The winner deliberately waits for the loser's own readiness
          -- signal (put from strictly *inside* 'Exception.finally''s own
          -- protected region, once it is genuinely blocked in the
          -- interruptible 'threadDelay') before completing, so
          -- 'raceManaged_''s decision to cancel the loser can only ever
          -- happen once the loser is already safely past the narrow,
          -- unavoidable window (common to *any* freshly-unmasked thread,
          -- not specific to 'raceManaged_') between a forked thread
          -- first becoming interruptible and its own first protective
          -- construct (here, 'Exception.finally') actually having run --
          -- a bare, unsynchronized @winner = pure ()@ can race that
          -- window and let a genuinely-delivered 'Exception.ThreadKilled'
          -- land before 'Exception.finally' ever installs its own
          -- handler, skipping the write below despite 'raceManaged_'
          -- itself behaving correctly. This is a property of the
          -- /test's/ own synchronization, not of 'raceManaged_'.
          winner = takeMVar loserReady
          loser = Exception.finally (putMVar loserReady () >> forever (threadDelay maxBound)) (writeIORef loserCleanedUp True)
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
      loserReady <- newEmptyMVar
      let -- See the previous test's Haddock for exactly why the winner
          -- must wait for the loser's own readiness signal, put from
          -- inside 'Exception.finally''s protected region, rather than
          -- completing (here, throwing) unconditionally and
          -- unsynchronized.
          winner = takeMVar loserReady >> Exception.throwIO (userError "winning side failed synchronously")
          loser = Exception.finally (putMVar loserReady () >> forever (threadDelay maxBound)) (writeIORef loserCleanedUp True)
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
