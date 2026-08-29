module Arkham.Api.AwsEnvSupervisorSpec (spec) where

import Amazonka (AuthEnv (..), Env' (..), Error (..), Region (..), SerializeError (..), newEnvNoAuth)
import Amazonka.Auth (Auth (..), AuthError (..))
import Amazonka.Error (serviceError)
import Api.Arkham.AwsEnvSupervisor (
  AwsAuthErrorDiagnostic (..),
  AwsErrorCategory (..),
  AwsErrorDiagnostic (..),
  DemandDrivenSupervisor,
  SupervisedEnv,
  SupervisedEnvState (..),
  acquireRegionBeforeAuth,
  classifyAuthErrorDiagnostic,
  classifyErrorDiagnostic,
  newDemandDrivenSupervisor,
  readSupervisedEnv,
  releaseAwsEnvChild,
  requestDemandDrivenReady,
  startSupervisedEnv,
  stopDemandDrivenSupervisor,
  stopSupervisedEnv,
 )
import Arkham.Prelude
import Control.Concurrent (forkIO, killThread, myThreadId, threadDelay)
import Control.Exception qualified as Exception
import Data.Void (absurd)
import GHC.Conc.Sync (ThreadStatus (..), threadStatus)
import Network.HTTP.Client qualified as Client
import Network.HTTP.Types.Status (status403, status404, status500)
import System.IO.Unsafe (unsafePerformIO)
import Test.Hspec

-- | A minimal fake resource standing in for a real Amazonka 'Env' in the
-- generic 'startSupervisedEnv'\/'readSupervisedEnv'\/'stopSupervisedEnv'
-- lifecycle tests below, which exercise the supervisor protocol itself
-- and deliberately have no real AWS\/network dependency. The wrapped 'Int'
-- lets successive acquisition generations be told apart.
newtype TestResource = TestResource Int
  deriving stock (Eq, Show)

-- | An @awaitInvalidation@ action for supervisor tests that never expect
-- their generation to be invalidated at all (e.g. because the test stops
-- the supervisor explicitly before that would ever matter).
neverInvalidate :: env -> IO AwsAuthErrorDiagnostic
neverInvalidate _ = forever (threadDelay maxBound)

-- | A @release@ action for supervisor tests that do not exercise release
-- behaviour at all -- most of the generic-protocol tests below are about
-- state publication/acquisition/backoff, not the release contract itself.
noRelease :: env -> IO ()
noRelease _ = pure ()

{- | Poll 'readSupervisedEnv' until @predicate@ holds. This is only a
bounded hang-guard against a genuinely stuck test suite: the actual
ordering\/determinism each test proves comes from the 'MVar' gates its own
fake @acquire@\/@awaitInvalidation@\/@backoff@ actions synchronize on, not
from this loop's polling interval or bound.
-}
waitForSupervisedState :: Show env => SupervisedEnv env -> (SupervisedEnvState env -> Bool) -> IO ()
waitForSupervisedState sup predicate = go (200 :: Int)
 where
  go 0 = do
    s <- readSupervisedEnv sup
    expectationFailure $ "supervisor state never satisfied the expected predicate; last seen: " <> show s
  go n = do
    s <- readSupervisedEnv sup
    if predicate s
      then pure ()
      else threadDelay (10 * 1000) >> go (n - 1)

-- | Same as 'waitForSupervisedState', but against a 'DemandDrivenSupervisor'
-- by polling 'requestDemandDrivenReady' with a tiny per-poll bound so it
-- never itself blocks the poll loop for long. Used only where a test needs
-- to observe a demand-driven supervisor settle asynchronously to its own
-- act of first demanding it (e.g. after a separate, earlier demand call).
waitForDemandDrivenState
  :: Show env => DemandDrivenSupervisor env -> (SupervisedEnvState env -> Bool) -> IO ()
waitForDemandDrivenState sup predicate = go (200 :: Int)
 where
  go 0 = do
    s <- requestDemandDrivenReady sup 0
    expectationFailure $ "supervisor state never satisfied the expected predicate; last seen: " <> show s
  go n = do
    s <- requestDemandDrivenReady sup 0
    if predicate s
      then pure ()
      else threadDelay (10 * 1000) >> go (n - 1)

{- | Wraps a value so that the *first* time it is forced (to WHNF), it
records that fact into @ref@ before returning the value unchanged. Used to
prove that a value was forced *before* some earlier observation point,
rather than only incidentally by the test's own later inspection.

'NOINLINE' prevents GHC from floating/duplicating or eliminating the
'unsafePerformIO' as dead code; ordering against the read of @ref@ is
guaranteed by the data dependency (the read happens strictly after the
action being tested has already returned, and the write only happens when
something forces this thunk, which can only be the code under test if it
happens before that read).
-}
markForcedOnceEvaluated :: IORef Bool -> a -> a
markForcedOnceEvaluated ref x = unsafePerformIO (writeIORef ref True) `seq` x
{-# NOINLINE markForcedOnceEvaluated #-}

httpExceptionFixture :: Client.HttpException
httpExceptionFixture = Client.InvalidUrlException "https://arkham-horror-bugs.s3.amazonaws.com" "connection failure"

{- | Regression\/design-verification for the application-lifetime
supervised-'Env' architecture that replaced the earlier per-request
@discoverFrozenEnv@\/@freezeAuth@\/@runOnDisposableWorker@ worker protocol,
and its Foundation-owned, demand-driven refinement (see
'Api.Arkham.AwsEnvSupervisor' for the full rationale): exactly one
dedicated thread -- the supervisor's own, started once (in production, in
@Application.makeFoundation@, before Warp accepts any request) and never
restarted -- ever calls @acquire@ (in production, @acquireAwsEnv@, i.e.
@newEnv discover@), so it is the only thread a delayed background-refresh
'AuthError' could ever target; a Warp request thread only ever reads a
strict, typed snapshot and never itself performs, or blocks on,
acquisition.

These tests exercise the generic protocol and its demand-driven wrapper
directly against fake, fully test-controlled @acquire@\/@awaitInvalidation@\/
@backoff@ actions -- no real AWS\/network dependency is involved -- using
'MVar's\/'TVar's as synchronization gates so each assertion is
deterministic rather than timing-dependent. 'waitForSupervisedState'\/
'waitForDemandDrivenState' below are only bounded hang-guards against a
genuinely stuck suite; they never substitute for the ordering guarantees
each test's own gates establish.
-}
spec :: Spec
spec = describe "AWS Env supervisor" do
  {- | Regression for the follow-up audit: the handler used to log
  'tshow err'/'tshow authErr' directly. 'TransportError'/'RetrievalError'
  wrap an 'HttpException' whose embedded 'Request' leaves the
  @X-Amz-Security-Token@ header (used for temporary/session credentials)
  unredacted, and 'SerializeError' carries the raw response body. Even
  'ServiceError''s own symbolic error code and request id are
  server-provided free-form text, so they are excluded too.
  'classifyErrorDiagnostic'/'classifyAuthErrorDiagnostic' are the exact
  functions the handler now logs instead: these tests prove the diagnostic
  for every secret-bearing case is a bare, structurally secret-free
  category (no wrapped exception, request, body, error code, or request id
  reaches the diagnostic type at all), while a genuine service response
  still yields the sanitized status and a status-derived-only category.
  -}
  describe "classifyErrorDiagnostic" do
    it "extracts only a sanitized status and status-derived category from a genuine service error" do
      let err = ServiceError $ serviceError "S3" status404 [] (Just "NoSuchKey") (Just "computer says no") (Just "req-123")
      classifyErrorDiagnostic err `shouldBe` AwsServiceFailure 404 AwsCategoryNotFound

    it "never carries the underlying exception for a transport failure" do
      -- 'AwsTransportFailure' is nullary: this is a type-level guarantee,
      -- not just a runtime one, that no 'HttpException' (and therefore no
      -- unredacted request header) can reach the diagnostic.
      classifyErrorDiagnostic (TransportError httpExceptionFixture) `shouldBe` AwsTransportFailure

    it "drops the raw response body from a serialize error, keeping only the sanitized status/category" do
      let err = SerializeError $ SerializeError' "S3" status500 (Just "<sensitive-looking-body>") "unexpected token"
      classifyErrorDiagnostic err `shouldBe` AwsSerializeFailure 500 AwsCategoryServerError

  describe "classifyAuthErrorDiagnostic" do
    it "never carries the underlying exception for a credential retrieval failure" do
      let authErr = RetrievalError httpExceptionFixture
      classifyAuthErrorDiagnostic authErr `shouldBe` AwsAuthRetrievalFailure

    it "drops the (potentially credential-bearing) message for a missing env var" do
      classifyAuthErrorDiagnostic (MissingEnvError "AWS_CONTAINER_CREDENTIALS_RELATIVE_URI") `shouldBe` AwsAuthMissingEnv

    it "drops the message for a missing credentials file" do
      classifyAuthErrorDiagnostic (MissingFileError "/root/.aws/credentials") `shouldBe` AwsAuthMissingFile

    it "drops the message for an invalid credentials file (may echo file content)" do
      classifyAuthErrorDiagnostic (InvalidFileError "parse error near line 3") `shouldBe` AwsAuthInvalidFile

    it "drops the message for an invalid IAM/task-identity document (may echo response content)" do
      classifyAuthErrorDiagnostic (InvalidIAMError "Error parsing Task Identity Document: ...") `shouldBe` AwsAuthInvalidIAM

    it "reports credential chain exhaustion" do
      classifyAuthErrorDiagnostic CredentialChainExhausted `shouldBe` AwsAuthCredentialChainExhausted

    it "extracts only a sanitized status and status-derived category from an auth-layer service error" do
      let err = serviceError "S3" status403 [] (Just "AccessDenied") Nothing Nothing
      classifyAuthErrorDiagnostic (AuthServiceError err) `shouldBe` AwsAuthServiceFailure 403 AwsCategoryClientError

    it "never carries the underlying arbitrary exception for an uncategorized auth failure" do
      -- 'AwsAuthOtherFailure' is nullary: 'OtherAuthError' wraps an
      -- unconstrained 'SomeException', so this is the only safe diagnostic.
      classifyAuthErrorDiagnostic (OtherAuthError (toException (userError "boom"))) `shouldBe` AwsAuthOtherFailure

  {- | Regression for the HIGH-severity async-credential-refresh audit: the
  pinned Amazonka fork's background credential-refresh timer captures the
  thread that called 'Amazonka.newEnv'/'discover' as its eventual
  refresh-failure 'throwTo' target. 'releaseAwsEnvChild' kills that
  target's associated 'Ref'-embedded refresh 'ThreadId' directly, before
  it can ever reach its @throwTo@ line; see 'startSupervisedEnv' for how
  this is composed with a single dedicated supervisor thread so that
  target is always this same long-lived thread, never a per-request one.
  -}
  describe "releaseAwsEnvChild" do
    it "does nothing for already-static credentials, without starting or touching any thread" do
      releaseAwsEnvChild (Auth (AuthEnv "AKIAEXAMPLE" "secret" Nothing Nothing)) `shouldReturn` ()

    it "kills the background refresh thread before it can act" do
      let authEnv = AuthEnv "AKIAEXAMPLE" "secret" (Just "token") Nothing
      ref <- newIORef authEnv
      reachedRefresh <- newIORef False
      -- Stands in for Amazonka's real background refresh-timer thread:
      -- it sleeps briefly, then (if never killed first) would flip this
      -- flag, standing in for its real 'Exception.throwTo' call.
      refreshThreadId <- forkIO $ do
        threadDelay (50 * 1000)
        writeIORef reachedRefresh True
      releaseAwsEnvChild (Ref refreshThreadId ref)
      -- Long enough that, had the refresh thread not been killed above,
      -- it would certainly have flipped the flag by now.
      threadDelay (150 * 1000)
      readIORef reachedRefresh `shouldReturn` False

  {- | Regression for the HIGH-severity finding that
  @Amazonka.Auth.InstanceProfile.fromNamedInstanceProfile@ (pinned source,
  @b562aa3f24845e34b95748daae671860017426be@) calls
  @fetchAuthInBackground getCredentials@ -- which may fork a background
  refresh child -- /before/ the separate, fallible
  @getRegionFromIdentity@ metadata call: if region resolution fails or
  this thread is cancelled after that fork, the only handle to the child
  (inside @keys@) is discarded with it, with no way for anything in this
  module's supervisor\/release protocol to ever kill it.
  'acquireRegionBeforeAuth' is the exact ordering fix
  'safeNamedInstanceProfile' applies -- factored out here so it can be
  exercised directly against fake @getRegion@\/@getAuth@ actions, without
  a real IMDS endpoint.

  These are genuine mutation-check regressions: reverting
  'acquireRegionBeforeAuth' to @getAuth@-then-@getRegion@ (the pinned
  source's own order) makes the first test below fail, since @getAuth@
  would then run -- and 'authAttempted' would become 'True' -- before
  @getRegion@'s failure is ever observed.
  -}
  describe "acquireRegionBeforeAuth" do
    it "never attempts auth/credential acquisition (which may fork a background refresh child) if region resolution fails first" do
      envNoAuth <- newEnvNoAuth
      authAttempted <- newIORef False
      let getRegion = Exception.throwIO (RetrievalError httpExceptionFixture) :: IO Region
          getAuth = writeIORef authAttempted True >> pure (Auth (AuthEnv "AKIAEXAMPLE" "secret" Nothing Nothing))
      result <- Exception.try @AuthError (acquireRegionBeforeAuth envNoAuth getRegion getAuth)
      -- Not 'shouldSatisfy': the wrapped 'Env' has no 'Show' instance, so
      -- a 'case' avoids ever needing one for the success branch.
      case result of
        Left (RetrievalError _) -> pure ()
        _ -> expectationFailure "expected acquireRegionBeforeAuth to fail with RetrievalError before ever calling getAuth"
      readIORef authAttempted `shouldReturn` False

    it "never attempts auth/credential acquisition if this thread is asynchronously cancelled while region resolution is still in flight" do
      envNoAuth <- newEnvNoAuth
      authAttempted <- newIORef False
      regionStarted <- newEmptyMVar
      let getRegion = putMVar regionStarted () >> forever (threadDelay maxBound) :: IO Region
          getAuth = writeIORef authAttempted True >> pure (Auth (AuthEnv "AKIAEXAMPLE" "secret" Nothing Nothing))
      resultVar <- newEmptyMVar
      workerTid <-
        forkIO
          $ putMVar resultVar
          =<< Exception.try @Exception.AsyncException (acquireRegionBeforeAuth envNoAuth getRegion getAuth)
      takeMVar regionStarted
      Exception.throwTo workerTid Exception.ThreadKilled
      result <- takeMVar resultVar
      case result of
        Left Exception.ThreadKilled -> pure ()
        _ -> expectationFailure "expected acquireRegionBeforeAuth to be cancelled with ThreadKilled while getRegion was still in flight"
      readIORef authAttempted `shouldReturn` False

    it "does still attempt auth/credential acquisition once region resolution has genuinely succeeded, and the resolved region reaches the returned Env" do
      envNoAuth <- newEnvNoAuth
      authAttempted <- newIORef False
      let getRegion = pure Ireland
          getAuth = writeIORef authAttempted True >> pure (Auth (AuthEnv "AKIAEXAMPLE" "secret" Nothing Nothing))
      resultEnv <- acquireRegionBeforeAuth envNoAuth getRegion getAuth
      readIORef authAttempted `shouldReturn` True
      resultEnv.region `shouldBe` Ireland

  describe "startSupervisedEnv / readSupervisedEnv / stopSupervisedEnv" do
    it "reports Initializing before the first acquisition completes, then Ready once it does, then terminal Unavailable once stopped" do
      acquireGate <- newEmptyMVar
      sup <- startSupervisedEnv (takeMVar acquireGate >> pure (Right (TestResource 1))) noRelease neverInvalidate (pure ())
      readSupervisedEnv sup `shouldReturn` SupervisedEnvInitializing
      putMVar acquireGate ()
      waitForSupervisedState sup \case SupervisedEnvReady _ -> True; _ -> False
      readSupervisedEnv sup `shouldReturn` SupervisedEnvReady (TestResource 1)
      stopSupervisedEnv sup
      -- A reader can never observe a stale 'SupervisedEnvReady' snapshot
      -- once nothing is monitoring it any longer.
      readSupervisedEnv sup `shouldReturn` SupervisedEnvUnavailable AwsAuthSupervisorTerminated

    it "also terminates cleanly, publishing terminal Unavailable, when stopped while still Initializing (acquisition never completes)" do
      acquireStarted <- newEmptyMVar
      sup <-
        startSupervisedEnv
          (putMVar acquireStarted () >> forever (threadDelay maxBound) :: IO (Either AwsAuthErrorDiagnostic TestResource))
          noRelease
          neverInvalidate
          (pure ())
      takeMVar acquireStarted
      readSupervisedEnv sup `shouldReturn` SupervisedEnvInitializing
      stopSupervisedEnv sup
      readSupervisedEnv sup `shouldReturn` SupervisedEnvUnavailable AwsAuthSupervisorTerminated

    it "publishes Unavailable with the acquire's diagnostic on failure, and does not retry until backoff completes (no retry storm)" do
      attemptCountRef <- newIORef (0 :: Int)
      backoffGate <- newEmptyMVar
      let acquire = do
            n <- atomicModifyIORef' attemptCountRef (\k -> (k + 1, k + 1))
            pure $ if n == 1 then Left AwsAuthCredentialChainExhausted else Right (TestResource n)
      sup <- startSupervisedEnv acquire noRelease neverInvalidate (takeMVar backoffGate)
      waitForSupervisedState sup (== SupervisedEnvUnavailable AwsAuthCredentialChainExhausted)
      -- Backoff has not been released yet: no second attempt has happened.
      threadDelay (50 * 1000)
      readIORef attemptCountRef `shouldReturn` 1
      putMVar backoffGate ()
      waitForSupervisedState sup \case SupervisedEnvReady (TestResource 2) -> True; _ -> False
      stopSupervisedEnv sup

    {- | Regression for the review finding that 'SupervisedEnvInitializing'
    was only ever published once, at construction: after a failure, the
    snapshot stayed at the *previous* generation's 'SupervisedEnvUnavailable'
    for the entire duration of the next attempt, even while that attempt
    was already genuinely in flight. A caller with a bounded wait budget
    (see 'requestDemandDrivenReady') only waits when it observes
    'SupervisedEnvInitializing', so it would instead see the stale
    'SupervisedEnvUnavailable' and fail immediately -- an avoidable 502
    even when the in-flight re-acquisition was about to succeed well
    within that caller's own timeout. This blocks the *second* attempt
    mid-acquisition (via 'acquireGate') so the test can observe the
    published state while it is genuinely in flight, proving it is
    'SupervisedEnvInitializing' and not the first attempt's stale
    diagnostic.
    -}
    it "re-publishes Initializing at the start of every re-acquisition attempt, not only the very first, so a stale Unavailable from the previous generation is never observed while a new attempt is already in flight" do
      attemptCountRef <- newIORef (0 :: Int)
      acquireGate <- newEmptyMVar
      backoffGate <- newEmptyMVar
      let acquire = do
            n <- atomicModifyIORef' attemptCountRef (\k -> (k + 1, k + 1))
            if n == 1
              then pure (Left AwsAuthCredentialChainExhausted)
              else takeMVar acquireGate >> pure (Right (TestResource n))
      sup <- startSupervisedEnv acquire noRelease neverInvalidate (takeMVar backoffGate)
      waitForSupervisedState sup (== SupervisedEnvUnavailable AwsAuthCredentialChainExhausted)
      putMVar backoffGate ()
      -- The second attempt is now genuinely in flight, blocked on
      -- 'acquireGate' -- prove the published state is 'Initializing', not
      -- the first attempt's stale 'Unavailable'.
      waitForSupervisedState sup (== SupervisedEnvInitializing)
      putMVar acquireGate ()
      waitForSupervisedState sup \case SupervisedEnvReady (TestResource 2) -> True; _ -> False
      stopSupervisedEnv sup

    {- | Regression for the HIGH-severity async-credential-refresh-escape
    audit itself: a delayed invalidation (standing in for the pinned
    Amazonka fork's real background refresh-timer 'Exception.throwTo')
    can only ever land on the supervisor's own dedicated thread, never on
    a separate thread standing in for a live Warp request worker -- and
    the old generation's child resource is released (here, a flag flipped
    by its own 'finally') strictly before the next generation's 'Ready' is
    published.

    Delivery uses plain, unwrapped 'Control.Exception.throwTo' carrying a
    real 'AuthError' constructor, matching exactly how the pinned fork's
    own background timer delivers it.
    -}
    it "a delayed invalidation lands only on the supervisor thread, never an unrelated request-worker stand-in, releasing the old generation's child before the next is published" do
      supervisorTidVar <- newEmptyMVar
      childReleasedRef <- newIORef False
      generationRef <- newIORef (0 :: Int)
      backoffGate <- newEmptyMVar
      let acquire = do
            n <- atomicModifyIORef' generationRef (\k -> (k + 1, k + 1))
            pure (Right (TestResource n))
          release _ = writeIORef childReleasedRef True
          awaitInvalidation _ = do
            tid <- myThreadId
            putMVar supervisorTidVar tid
            outcome <- Exception.try @AuthError (forever (threadDelay maxBound))
            either (evaluate . classifyAuthErrorDiagnostic) absurd outcome
      -- A separate, unrelated thread standing in for a live Warp request
      -- worker: it must never be affected by the delayed invalidation
      -- below, however precisely it targets only the supervisor's thread.
      requestWorkerAffected <- newIORef False
      requestWorkerTid <-
        forkIO $ Exception.catch (forever (threadDelay maxBound)) \(_ :: AuthError) -> writeIORef requestWorkerAffected True
      sup <- startSupervisedEnv acquire release awaitInvalidation (takeMVar backoffGate)
      waitForSupervisedState sup \case SupervisedEnvReady (TestResource 1) -> True; _ -> False
      supervisorTid <- takeMVar supervisorTidVar
      supervisorTid `shouldNotBe` requestWorkerTid
      Exception.throwTo supervisorTid (RetrievalError httpExceptionFixture)
      waitForSupervisedState sup (== SupervisedEnvUnavailable AwsAuthRetrievalFailure)
      readIORef childReleasedRef `shouldReturn` True
      readIORef requestWorkerAffected `shouldReturn` False
      killThread requestWorkerTid
      stopSupervisedEnv sup

    it "forces the classified diagnostic before publishing it, not deferring it as a thunk for a later reader to force" do
      forcedRef <- newIORef False
      let acquire = pure (Left (markForcedOnceEvaluated forcedRef AwsAuthCredentialChainExhausted)) :: IO (Either AwsAuthErrorDiagnostic ())
      sup <- startSupervisedEnv acquire noRelease neverInvalidate (forever (threadDelay maxBound))
      -- This predicate inspects only the outer constructor (a wildcard
      -- binder for the wrapped diagnostic), never the diagnostic value
      -- itself, so if 'forcedRef' is already 'True' once it is satisfied,
      -- that can only be because 'startSupervisedEnv' forced the
      -- diagnostic *before* publishing it -- not because this test's own
      -- inspection forced it first.
      waitForSupervisedState sup \case SupervisedEnvUnavailable _ -> True; _ -> False
      readIORef forcedRef `shouldReturn` True
      -- Only now does the test itself inspect the diagnostic's value.
      readSupervisedEnv sup `shouldReturn` SupervisedEnvUnavailable AwsAuthCredentialChainExhausted
      stopSupervisedEnv sup

    it "stopSupervisedEnv terminates the supervisor thread and releases (waits for) the current generation's live child" do
      supervisorTidVar <- newEmptyMVar
      childKilled <- newIORef False
      let acquire = pure (Right (TestResource 1))
          release _ = writeIORef childKilled True
          awaitInvalidation _ = do
            tid <- myThreadId
            putMVar supervisorTidVar tid
            forever (threadDelay maxBound)
      sup <- startSupervisedEnv acquire release awaitInvalidation (pure ())
      waitForSupervisedState sup \case SupervisedEnvReady _ -> True; _ -> False
      supervisorTid <- takeMVar supervisorTidVar
      stopSupervisedEnv sup
      status <- threadStatus supervisorTid
      status `shouldNotBe` ThreadRunning
      readIORef childKilled `shouldReturn` True

    {- | The single-'mask' design (see 'startSupervisedEnv') spans from
    just before @acquire@ is called through the moment
    'SupervisedEnvReady' is published, and only then restores/unmasks to
    enter @awaitInvalidation@. This proves that gap is genuinely closed:
    a fake @acquire@ forks its own \"refresh child\" thread immediately
    before returning (standing in for the pinned Amazonka fork's own
    @forkIO@ inside @fetchAuthInBackground@), and an asynchronous
    exception (standing in for 'stopSupervisedEnv') is delivered to the
    supervisor thread at the exact handoff moment -- via a dedicated
    \"delivered\" gate signalled by the fake @acquire@ itself, so no sleep
    or timing guess is involved. Because the whole span is masked, that
    exception cannot possibly land until @awaitInvalidation@ is entered
    (restored), by which point 'SupervisedEnvReady' has already been
    published and the forked child is already reachable via @env@ -- so
    @release@ can, and must, kill it exactly once. If the exception could
    instead land in the gap between @acquire@ returning and
    'SupervisedEnvReady' being published, the child would leak: nothing
    would ever call this test's release action at all, since
    'startSupervisedEnv' never even entered @awaitInvalidation@ for a
    generation that was never published.
    -}
    it "closes the fork-then-orphan gap: an async exception delivered exactly at acquire's handoff cannot orphan a child forked immediately before return" do
      readyPublished <- newEmptyMVar
      releaseCountRef <- newIORef (0 :: Int)
      let acquire = do
            -- Stands in for the pinned fork's own 'forkIO' immediately
            -- before returning a successfully-acquired resource: nothing
            -- else happens between this and 'acquire' returning.
            childTid <- forkIO (forever (threadDelay maxBound))
            pure (Right childTid)
          release childTid = do
            atomicModifyIORef' releaseCountRef (\n -> (n + 1, ()))
            killThread childTid
          awaitInvalidation _ = do
            putMVar readyPublished ()
            -- Blocks here (restored/interruptible) until the external
            -- exception below arrives; @release@ (applied via 'finally'
            -- around this call, see 'startSupervisedEnv') always runs
            -- whether this returns normally or the exception propagates
            -- through.
            forever (threadDelay maxBound)
      sup <- startSupervisedEnv acquire release awaitInvalidation (pure ())
      -- Waits only for 'SupervisedEnvReady' to have been *published*,
      -- which -- under the single-mask design -- can only happen after
      -- the fork above has already completed and its 'ThreadId' is
      -- already captured in @env@, ready for 'awaitInvalidation' to
      -- release. No sleep: this synchronizes on the same gate
      -- 'awaitInvalidation' itself signals immediately upon entry.
      takeMVar readyPublished
      stopSupervisedEnv sup
      -- Released via 'finally' around @restore (awaitInvalidation env)@
      -- exactly once -- not zero times (which would mean the child
      -- leaked) and not more than once (which would mean release
      -- raced/duplicated across generations).
      readIORef releaseCountRef `shouldReturn` 1

    {- | Regression for the second half of the same audit finding: a
    /pending/ asynchronous exception queued while a generation is still
    masked (i.e. during @acquire@, or between @acquire@ returning and
    'SupervisedEnvReady' being published) is delivered the instant
    'restore' unmasks -- /before/ @awaitInvalidation@'s own argument thunk
    is ever forced, let alone entered. An internal @finally@ inside
    @awaitInvalidation@'s own body (the design this replaced) could
    therefore be skipped entirely, silently leaking the resource: exactly
    why release is now applied via 'finally' /around/ the
    @restore (awaitInvalidation env)@ call in 'startSupervisedEnv' itself
    -- a handler installed synchronously, one level further out, before
    'restore' is ever reached.

    The pending exception here is queued from /within/ @acquire@ itself
    (which always runs on the supervisor thread, still masked): a
    throwaway helper thread @throwTo@s this same thread, then this test
    spins (deliberately not 'threadDelay', which remains interruptible
    even under mask -- see the inline comment below) long enough that the
    RTS has certainly already queued the exception as pending before
    @acquire@ returns -- so by the time @runGeneration@ reaches @restore
    (awaitInvalidation env)@, delivery is guaranteed to happen at that
    exact unmask, not later. 'awaitInvalidationEntered' staying 'False'
    proves @awaitInvalidation@'s body genuinely never got a chance to run
    even its first instruction; 'releaseCountRef' reading @1@ proves
    'release' still ran despite that.

    Mutation check: reverting to the previous design -- release folded
    into @awaitInvalidation@'s own internal @finally@ instead of applied
    by 'startSupervisedEnv' around the call -- makes this test fail
    ('releaseCountRef' stays @0@), since the pending exception is
    delivered before that internal handler is ever installed.
    -}
    it "closes the restore-boundary gap: a pending exception queued during acquire still runs release exactly once, even though it fires before awaitInvalidation's body ever executes" do
      releaseCountRef <- newIORef (0 :: Int)
      awaitInvalidationEnteredRef <- newIORef False
      let acquire = do
            tid <- myThreadId
            _ <- forkIO $ Exception.throwTo tid Exception.ThreadKilled
            -- A bounded, deliberately *non-interruptible* busy spin, not
            -- 'threadDelay': 'threadDelay' is itself one of the specific
            -- operations that remains interruptible even while masked
            -- (verified directly against this exact pinned GHC), so it
            -- would let the helper's pending exception land right here,
            -- inside @acquire@, rather than staying genuinely pending
            -- until 'restore' below -- defeating the very race this test
            -- exists to reproduce. A tight recursive loop with no
            -- interruptible operation cannot be preempted while masked,
            -- no matter how long it runs, and is long enough here that
            -- the helper thread has certainly already issued its
            -- 'Exception.throwTo' (which itself blocks until delivered,
            -- so it is provably still pending, not yet delivered, the
            -- entire time this thread remains masked).
            let spin :: Int -> IO ()
                spin 0 = pure ()
                spin n = spin (n - 1)
            spin (20 * 1000 * 1000)
            pure (Right (TestResource 1))
          release _ = atomicModifyIORef' releaseCountRef (\n -> (n + 1, ()))
          awaitInvalidation _ = do
            writeIORef awaitInvalidationEnteredRef True
            forever (threadDelay maxBound)
      -- 'startSupervisedEnv' only starts its own dedicated thread and
      -- returns immediately; the pending exception queued above
      -- terminates that thread asynchronously, so wait for its terminal
      -- published state rather than anything returned here.
      sup <- startSupervisedEnv acquire release awaitInvalidation (pure ())
      waitForSupervisedState sup (== SupervisedEnvUnavailable AwsAuthSupervisorTerminated)
      readIORef awaitInvalidationEnteredRef `shouldReturn` False
      readIORef releaseCountRef `shouldReturn` 1
      readIORef awaitInvalidationEnteredRef `shouldReturn` False
      readIORef releaseCountRef `shouldReturn` 1

  {- | The demand-driven wrapper ('newDemandDrivenSupervisor'\/
  'requestDemandDrivenReady'\/'stopDemandDrivenSupervisor') that
  'AwsEnvSupervisor' is a thin, production-facing instance of -- tested
  here directly against a fake, fully test-controlled @acquire@ so that
  demand-gating behavior is proven without ever touching real AWS
  credentials\/network\/filesystem via 'acquireAwsEnv'.
  -}
  describe "newDemandDrivenSupervisor / requestDemandDrivenReady / stopDemandDrivenSupervisor" do
    it "starts its dedicated thread immediately but never calls acquire until first demanded" do
      acquireCalls <- newIORef (0 :: Int)
      let acquire = atomicModifyIORef' acquireCalls (\n -> (n + 1, ())) >> pure (Right (TestResource 1))
      sup <- newDemandDrivenSupervisor acquire noRelease neverInvalidate (pure ())
      -- Long enough that, had construction itself triggered acquisition,
      -- it would certainly have happened by now.
      threadDelay (100 * 1000)
      readIORef acquireCalls `shouldReturn` 0
      _ <- requestDemandDrivenReady sup (200 * 1000)
      waitForDemandDrivenState sup \case SupervisedEnvReady _ -> True; _ -> False
      readIORef acquireCalls `shouldReturn` 1
      stopDemandDrivenSupervisor sup

    it "a second (or later) demand call is a cheap no-op: acquire is not called again" do
      acquireCalls <- newIORef (0 :: Int)
      let acquire = atomicModifyIORef' acquireCalls (\n -> (n + 1, ())) >> pure (Right (TestResource 1))
      sup <- newDemandDrivenSupervisor acquire noRelease neverInvalidate (pure ())
      _ <- requestDemandDrivenReady sup (200 * 1000)
      waitForDemandDrivenState sup \case SupervisedEnvReady _ -> True; _ -> False
      readIORef acquireCalls `shouldReturn` 1
      _ <- requestDemandDrivenReady sup (200 * 1000)
      _ <- requestDemandDrivenReady sup (200 * 1000)
      readIORef acquireCalls `shouldReturn` 1
      stopDemandDrivenSupervisor sup

    it "concurrent first demand triggers exactly one acquisition" do
      acquireCalls <- newIORef (0 :: Int)
      let acquire = atomicModifyIORef' acquireCalls (\n -> (n + 1, ())) >> pure (Right (TestResource 1))
      sup <- newDemandDrivenSupervisor acquire noRelease neverInvalidate (pure ())
      _ <- forkIO $ () <$ requestDemandDrivenReady sup (200 * 1000)
      _ <- forkIO $ () <$ requestDemandDrivenReady sup (200 * 1000)
      _ <- requestDemandDrivenReady sup (200 * 1000)
      waitForDemandDrivenState sup \case SupervisedEnvReady _ -> True; _ -> False
      readIORef acquireCalls `shouldReturn` 1
      stopDemandDrivenSupervisor sup

    it "a request's own wait timeout does NOT cancel the gated acquisition, which completes and is later observed" do
      acquireGate <- newEmptyMVar
      sup <- newDemandDrivenSupervisor (takeMVar acquireGate >> pure (Right (TestResource 1))) noRelease neverInvalidate (pure ())
      -- A very short bound: this demand call will time out while the
      -- acquisition below is still deliberately blocked, well before
      -- 'acquireGate' is ever filled.
      timedOut <- requestDemandDrivenReady sup (10 * 1000)
      timedOut `shouldBe` SupervisedEnvInitializing
      -- Only now release the still-in-flight acquisition -- proving the
      -- earlier timeout never touched, let alone cancelled, it.
      putMVar acquireGate ()
      waitForDemandDrivenState sup \case SupervisedEnvReady (TestResource 1) -> True; _ -> False
      stopDemandDrivenSupervisor sup

    {- | End-to-end regression, at the 'requestDemandDrivenReady' level, for
    the same review finding as the generic-protocol test above: a request
    landing while a post-failure re-acquisition is already in flight must
    be given the chance to wait for it (because it observes
    'SupervisedEnvInitializing'), rather than immediately reading the
    *previous* generation's stale 'SupervisedEnvUnavailable' and failing
    even though this attempt goes on to succeed well within its own bound.
    -}
    it "a request's bounded wait succeeds on a re-acquisition already in flight after a prior failure, rather than immediately returning the previous generation's stale Unavailable" do
      attemptCountRef <- newIORef (0 :: Int)
      acquireGate <- newEmptyMVar
      backoffGate <- newEmptyMVar
      let acquire = do
            n <- atomicModifyIORef' attemptCountRef (\k -> (k + 1, k + 1))
            if n == 1
              then pure (Left AwsAuthCredentialChainExhausted)
              else takeMVar acquireGate >> pure (Right (TestResource n))
      sup <- newDemandDrivenSupervisor acquire noRelease neverInvalidate (takeMVar backoffGate)
      firstOutcome <- requestDemandDrivenReady sup (200 * 1000)
      firstOutcome `shouldBe` SupervisedEnvUnavailable AwsAuthCredentialChainExhausted
      putMVar backoffGate ()
      -- Wait for the second attempt to genuinely be in flight (blocked on
      -- 'acquireGate') before issuing the request under test, so its
      -- bounded wait really does observe an in-progress re-acquisition
      -- rather than winning a race against backoff/attempt-start.
      waitForDemandDrivenState sup (== SupervisedEnvInitializing)
      resultVar <- newEmptyMVar
      _ <- forkIO $ putMVar resultVar =<< requestDemandDrivenReady sup (2 * 1000 * 1000)
      -- Give the concurrent request a moment to actually enter its own
      -- bounded STM wait before releasing the acquisition -- proving it
      -- was genuinely waiting, not merely re-reading a settled value.
      threadDelay (20 * 1000)
      putMVar acquireGate ()
      takeMVar resultVar `shouldReturn` SupervisedEnvReady (TestResource 2)
      stopDemandDrivenSupervisor sup

    it "stop during gated pre-acquisition (never demanded) terminates the supervisor with no acquisition ever attempted" do
      acquireCalls <- newIORef (0 :: Int)
      let acquire = atomicModifyIORef' acquireCalls (\n -> (n + 1, ())) >> pure (Right (TestResource 1))
      sup <- newDemandDrivenSupervisor acquire noRelease neverInvalidate (pure ())
      stopDemandDrivenSupervisor sup
      readIORef acquireCalls `shouldReturn` 0
      requestDemandDrivenReady sup 0 `shouldReturn` SupervisedEnvUnavailable AwsAuthSupervisorTerminated

    it "stop during Ready terminates the child before returning" do
      childKilled <- newIORef False
      let acquire = pure (Right (TestResource 1))
          release _ = writeIORef childKilled True
          awaitInvalidation _ = forever (threadDelay maxBound)
      sup <- newDemandDrivenSupervisor acquire release awaitInvalidation (pure ())
      _ <- requestDemandDrivenReady sup (200 * 1000)
      waitForDemandDrivenState sup \case SupervisedEnvReady _ -> True; _ -> False
      stopDemandDrivenSupervisor sup
      readIORef childKilled `shouldReturn` True
