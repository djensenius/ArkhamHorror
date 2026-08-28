module Arkham.Api.GameBugUploadSpec (spec) where

import Amazonka (AuthEnv (..), Error (..), SerializeError (..))
import Amazonka.Auth (Auth (..), AuthError (..))
import Amazonka.Error (serviceError)
import Api.Handler.Arkham.Game.Bug (
  AwsAuthErrorDiagnostic (..),
  AwsErrorCategory (..),
  AwsErrorDiagnostic (..),
  BugUploadFailure (..),
  BugUploadOutcome (..),
  HeadObjectOutcome (..),
  classifyAuthErrorDiagnostic,
  classifyErrorDiagnostic,
  classifyHeadObjectError,
  freezeAuth,
  runBoundedOnDisposableWorker,
  runBugUploadPolicy,
  runHeadObjectAction,
  runOnDisposableWorker,
  runPutObjectAction,
 )
import Arkham.Prelude
import Control.Concurrent (forkIO, myThreadId, threadDelay)
import Control.Exception qualified as Exception
import Data.Text qualified as T
import GHC.Conc.Sync (ThreadStatus (..), threadStatus)
import Network.HTTP.Client qualified as Client
import Network.HTTP.Types.Status (Status, status403, status404, status429, status500)
import System.IO.Unsafe (unsafePerformIO)
import Test.Hspec

-- | Deliberately distinct from any exception type used elsewhere in this
-- suite (or thrown internally by the production code under test), so a
-- test can prove a caught exception is *exactly* this one -- i.e. the
-- caller's own cancellation -- and not, say, a worker's internal failure
-- coincidentally caught by too broad a handler.
data TestCancellation = TestCancellation
  deriving stock (Eq, Show)
  deriving anyclass Exception

{- | Wraps a value so that the *first* time it is forced (to WHNF), it
records that fact into @ref@ before returning the value unchanged. Used to
prove that 'runHeadObjectAction'\/'runPutObjectAction' force their
sanitized outcome *inside* the action, rather than returning an unforced
thunk that only happens to be forced later by the test's own
'shouldBe'\/'evaluate' call -- by checking @ref@'s flag *before* the test
itself ever inspects the returned value.

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

-- | A bare-bones S3 'ServiceError' carrying only the HTTP status that
-- 'classifyHeadObjectError' inspects. HeadObject responses have no body, so
-- production code never has more than this to go on either.
serviceErrorWithStatus :: Status -> Error
serviceErrorWithStatus status = ServiceError $ serviceError "S3" status [] Nothing Nothing Nothing

transportFailure :: Error
transportFailure = TransportError httpExceptionFixture

-- | A bare HTTP transport exception, used both to build 'transportFailure'
-- and directly as 'RetrievalError's payload, without ever needing to
-- pattern-match back out of an existing 'Error'/'AuthError' value (which
-- would require a partial fallback for constructors that cannot occur).
httpExceptionFixture :: Client.HttpException
httpExceptionFixture = Client.InvalidUrlException "https://arkham-horror-bugs.s3.amazonaws.com" "connection failure"

{- | Regression for #30: 'postApiV1ArkhamGameBugR' caught only 'Amazonka.Error'
around HeadObject and treated *every* service error -- including
permission-denied, throttling, and 5xx responses, plus any transport
failure -- as "object missing", unconditionally proceeding to PUT and
returning the public success URL regardless of what actually happened.

'classifyHeadObjectError' and 'runBugUploadPolicy' are the exact functions
the production handler now uses to classify HeadObject failures and to
sequence the protected PUT. These tests exercise them directly.
-}
spec :: Spec
spec = describe "bug report upload" do
  describe "classifyHeadObjectError" do
    it "classifies a genuine 404 as absent" do
      classifyHeadObjectError (serviceErrorWithStatus status404) `shouldBe` ObjectAbsent

    it "classifies a 403 permission-denied response as a failure, not absent" do
      classifyHeadObjectError (serviceErrorWithStatus status403) `shouldBe` HeadObjectFailed (AwsServiceFailure 403 AwsCategoryClientError)

    it "classifies a 429 throttling response as a failure, not absent" do
      classifyHeadObjectError (serviceErrorWithStatus status429) `shouldBe` HeadObjectFailed (AwsServiceFailure 429 AwsCategoryThrottled)

    it "classifies a 500 server error as a failure, not absent" do
      classifyHeadObjectError (serviceErrorWithStatus status500) `shouldBe` HeadObjectFailed (AwsServiceFailure 500 AwsCategoryServerError)

    it "classifies a transport-level failure (DNS/TLS/timeout/etc) as a failure, not absent" do
      classifyHeadObjectError transportFailure `shouldBe` HeadObjectFailed AwsTransportFailure

  describe "runBugUploadPolicy" do
    it "skips PUT and succeeds when the object already exists" do
      putCalls <- newIORef (0 :: Int)
      outcome <-
        runBugUploadPolicy
          (pure ObjectPresent)
          (modifyIORef' putCalls (+ 1) >> pure (Right ()))
      readIORef putCalls `shouldReturn` 0
      outcome `shouldBe` BugUploadSucceeded

    it "performs exactly one PUT and succeeds when the object is genuinely absent" do
      putCalls <- newIORef (0 :: Int)
      outcome <-
        runBugUploadPolicy
          (pure ObjectAbsent)
          (modifyIORef' putCalls (+ 1) >> pure (Right ()))
      readIORef putCalls `shouldReturn` 1
      outcome `shouldBe` BugUploadSucceeded

    it "fails without ever calling PUT when HeadObject fails for a non-404 reason" do
      putCalls <- newIORef (0 :: Int)
      outcome <-
        runBugUploadPolicy
          (pure $ classifyHeadObjectError (serviceErrorWithStatus status403))
          (modifyIORef' putCalls (+ 1) >> pure (Right ()))
      readIORef putCalls `shouldReturn` 0
      outcome `shouldBe` BugUploadFailed (HeadCheckFailed (AwsServiceFailure 403 AwsCategoryClientError))

    it "fails without ever calling PUT on a transport-level HeadObject failure" do
      putCalls <- newIORef (0 :: Int)
      outcome <-
        runBugUploadPolicy
          (pure $ classifyHeadObjectError transportFailure)
          (modifyIORef' putCalls (+ 1) >> pure (Right ()))
      readIORef putCalls `shouldReturn` 0
      outcome `shouldBe` BugUploadFailed (HeadCheckFailed AwsTransportFailure)

    it "reports failure (never a false success) when PUT itself fails" do
      outcome <- runBugUploadPolicy (pure ObjectAbsent) (pure $ Left (AwsServiceFailure 500 AwsCategoryServerError))
      outcome `shouldBe` BugUploadFailed (PutObjectFailed (AwsServiceFailure 500 AwsCategoryServerError))

    {- | Regression for the async-refresh/lazy-thunk audit: the diagnostic
    used to cross the 'runResourceT IO' -> 'Handler' boundary through a
    non-strict 'IORef' that could retain a thunk closing over the raw
    'Error' until something happened to force it later. It is now threaded
    straight through 'runBugUploadPolicy''s ordinary return value as a
    field of 'BugUploadFailure', built via 'classifyErrorDiagnostic' at the
    exact point of failure. 'evaluate' fully forces the whole outcome here
    (not just its outer constructor), and this is possible -- without ever
    touching the original exception value again -- only because nothing
    about that exception survives past classification: a distinctive
    "secret" string embedded in the raw error's fields never appears
    anywhere in the forced outcome's textual representation.
    -}
    it "never retains any part of the raw exception once forced into a BugUploadFailure, even under full strict evaluation" do
      let secret = "s3-secret-req-id-must-never-leak-6f2a1"
      let err = ServiceError $ serviceError "S3" status500 [] (Just secret) (Just secret) (Just secret)
      outcome <- runBugUploadPolicy (pure $ classifyHeadObjectError err) (pure $ Right ())
      forced <- evaluate outcome
      forced `shouldBe` BugUploadFailed (HeadCheckFailed (AwsServiceFailure 500 AwsCategoryServerError))
      T.isInfixOf secret (tshow forced) `shouldBe` False

  {- | Regression for the PUT-result-laziness audit: a bare @pure $ either
  ... / case ... of ...@ only builds a thunk, so a failing PUT's
  classified diagnostic (and, transitively, whatever raw 'Error' it was
  built from) could still be unforced when 'runResourceT' returns --
  forced only incidentally, later, by whatever eventually pattern-matches
  the outcome. 'runHeadObjectAction'\/'runPutObjectAction' instead
  'evaluate' the classified outcome themselves, from inside the action,
  before returning it. 'markForcedOnceEvaluated' proves this happens
  *inside* the action under test: its "forced" flag is asserted 'True'
  immediately after the action returns, strictly before the test itself
  ever forces the returned value (e.g. via 'shouldBe').
  -}
  describe "runHeadObjectAction" do
    it "reports the object present without inspecting/forcing anything from a successful response" do
      runHeadObjectAction (pure (Right ("response" :: String))) `shouldReturn` ObjectPresent

    it "forces the classified diagnostic to normal form before returning, not deferring it as a thunk" do
      forcedRef <- newIORef False
      let poisonedErr = markForcedOnceEvaluated forcedRef (serviceErrorWithStatus status403)
      outcome <- runHeadObjectAction (pure (Left poisonedErr) :: IO (Either Error ()))
      -- Asserted before 'shouldBe' below ever forces 'outcome' itself, so
      -- this can only be 'True' already if 'runHeadObjectAction' forced
      -- the diagnostic internally.
      readIORef forcedRef `shouldReturn` True
      outcome `shouldBe` HeadObjectFailed (AwsServiceFailure 403 AwsCategoryClientError)

  describe "runPutObjectAction" do
    it "succeeds without inspecting/forcing anything from a successful response" do
      runPutObjectAction (pure (Right ("response" :: String))) `shouldReturn` Right ()

    it "forces the classified diagnostic to normal form before returning, not deferring it as a thunk" do
      forcedRef <- newIORef False
      let poisonedErr = markForcedOnceEvaluated forcedRef (serviceErrorWithStatus status500)
      outcome <- runPutObjectAction (pure (Left poisonedErr) :: IO (Either Error ()))
      -- As above: proves the forcing happened inside the action, not
      -- merely as a side effect of this test's own inspection of
      -- 'outcome'.
      readIORef forcedRef `shouldReturn` True
      outcome `shouldBe` Left (AwsServiceFailure 500 AwsCategoryServerError)

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
      classifyErrorDiagnostic transportFailure `shouldBe` AwsTransportFailure

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
  refresh-failure 'throwTo' target, and separately, its own 'ThreadId' --
  distinct from that captured target -- is what 'Auth''s 'Ref' constructor
  carries. 'freezeAuth' kills that 'Ref'-embedded thread id directly,
  before it can ever reach its @throwTo@ line, and 'runOnDisposableWorker'
  ensures discovery itself (and therefore the captured @throwTo@ target)
  never runs on a long-lived Warp request thread. Together
  ('discoverFrozenEnv') this means no background refresh thread can ever
  outlive a single bounded credential-discovery call, and the only thread
  it could ever target is a disposable worker that is never reused for
  anything else.
  -}
  describe "freezeAuth" do
    it "returns already-static credentials unchanged, without starting or touching any thread" do
      let authEnv = AuthEnv "AKIAEXAMPLE" "secret" Nothing Nothing
      frozen <- freezeAuth (Auth authEnv)
      frozen `shouldBe` authEnv

    it "kills the background refresh thread before it can act, while still returning the current credentials" do
      let authEnv = AuthEnv "AKIAEXAMPLE" "secret" (Just "token") Nothing
      ref <- newIORef authEnv
      reachedRefresh <- newIORef False
      -- Stands in for Amazonka's real background refresh-timer thread:
      -- it sleeps briefly, then (if never killed first) would flip this
      -- flag, standing in for its real 'Exception.throwTo' call.
      refreshThreadId <- forkIO $ do
        threadDelay (50 * 1000)
        writeIORef reachedRefresh True
      frozen <- freezeAuth (Ref refreshThreadId ref)
      frozen `shouldBe` authEnv
      -- Long enough that, had the refresh thread not been killed by
      -- 'freezeAuth' above, it would certainly have flipped the flag by
      -- now.
      threadDelay (150 * 1000)
      readIORef reachedRefresh `shouldReturn` False

  describe "runOnDisposableWorker" do
    it "runs the action on a different thread than the caller" do
      callerTid <- myThreadId
      actionTid <- runOnDisposableWorker myThreadId
      actionTid `shouldNotBe` callerTid

    it "returns the action's result on success" do
      runOnDisposableWorker (pure (42 :: Int)) `shouldReturn` 42

    it "propagates an exception raised inside the action back to the caller, not swallowed" do
      result <- try @_ @IOException (runOnDisposableWorker (throwIO (userError "boom")))
      case result of
        Left ioErr -> show ioErr `shouldContain` "boom"
        Right () -> expectationFailure "expected the worker's exception to propagate"

    it "leaves no worker thread running after a normal return" do
      workerTidRef <- newIORef Nothing
      _ <- runOnDisposableWorker (myThreadId >>= writeIORef workerTidRef . Just)
      mWorkerTid <- readIORef workerTidRef
      case mWorkerTid of
        Nothing -> expectationFailure "worker never recorded its own ThreadId"
        Just workerTid -> do
          status <- threadStatus workerTid
          status `shouldNotBe` ThreadRunning

    {- | Regression for the caller-cancellation/worker-lifetime audit: a
    fire-and-forget @forkIO@ that merely discarded the worker's 'ThreadId'
    could leave the worker running indefinitely past a cancelled caller.
    'runOnDisposableWorker' now retains that 'ThreadId' and, on caller
    cancellation, kills the worker and waits for its acknowledgement before
    letting the caller's own exception propagate -- unchanged, never
    swallowed, never turned into a false success.

    Run via a "canary" thread (rather than 'throwTo'ing this test thread
    itself) so the cancellation exception can be delivered with an ordinary
    'throwTo' from outside, exactly as a real caller (e.g. Warp abandoning a
    request) would be interrupted asynchronously.

    Both the delivery ('Control.Exception.throwTo') and the observing
    catch ('Control.Exception.try', qualified here as @Exception@) are
    deliberately the plain ones from "Control.Exception", not this
    module's ambient "UnliftIO.Exception" re-exports (via 'Arkham.Prelude'
    \/ @classy-prelude@). 'UnliftIO.Exception.throwTo' marks the delivered
    exception as asynchronous so that 'UnliftIO.Exception.try' -- which
    only ever catches /synchronous/ exceptions by design -- correctly
    refuses to catch it; using that pairing here would make this test's
    own observation @try@ unable to see the very cancellation it just
    delivered, which is a property of the sync-only wrapper, not evidence
    of any bug in 'runOnDisposableWorker'. A real caller cancellation (e.g.
    Warp's own thread-kill on an abandoned request) is delivered with
    plain, unwrapped 'Control.Exception.throwTo' semantics, so using the
    same plain functions here is also the more faithful simulation.
    -}
    it "propagates the caller's own asynchronous cancellation unchanged, terminating (not orphaning) the worker" do
      workerStarted <- newEmptyMVar
      workerFinishedNormally <- newIORef False
      canaryResult <- newEmptyMVar
      canaryTid <- forkIO do
        result <-
          Exception.try @SomeException $
            runOnDisposableWorker do
              putMVar workerStarted ()
              threadDelay (2 * 1000 * 1000)
              writeIORef workerFinishedNormally True
        putMVar canaryResult result
      takeMVar workerStarted
      -- A tiny scheduler-settle delay: 'workerStarted' only proves the
      -- *worker* thread has begun running (it alone can 'putMVar' it),
      -- not that the *canary* thread calling 'runOnDisposableWorker' has
      -- itself already reached its own blocking wait. This closes that
      -- narrow scheduling gap so the cancellation below reliably lands
      -- while canary is genuinely blocked waiting on the worker, exactly
      -- as intended, rather than racing its own setup code.
      threadDelay (20 * 1000)
      Exception.throwTo canaryTid TestCancellation
      mResult <- timeout (5 * 1000 * 1000) (takeMVar canaryResult)
      case mResult of
        Nothing -> expectationFailure "runOnDisposableWorker did not return after caller cancellation"
        Just (Left ex) -> fromException ex `shouldBe` Just TestCancellation
        Just (Right ()) -> expectationFailure "expected the caller's cancellation to propagate, got a normal result instead"
      -- The worker's own action never reached its final line: it was
      -- genuinely killed, not merely abandoned while still running.
      readIORef workerFinishedNormally `shouldReturn` False

    {- | Regression for the same audit: proves 'runOnDisposableWorker'
    genuinely *waits* for the worker's cleanup to finish, rather than
    firing 'killThread' and immediately letting the caller's cancellation
    through regardless. The worker's own 'finally' cleanup (itself run
    under an implicit mask while it handles its incoming 'ThreadKilled')
    can only complete -- and only afterwards does the worker 'putMVar' its
    result -- strictly before 'runOnDisposableWorker''s cleanup handler can
    'takeMVar' that result and rethrow the caller's exception. So by the
    time this test observes the caller's cancellation having propagated,
    the flag below can only already be 'True': this is a structural
    guarantee, not a timing race.
    -}
    it "waits for the worker's own cleanup to finish before letting a cancelled caller's exception through" do
      workerStarted <- newEmptyMVar
      ackFinished <- newIORef False
      canaryResult <- newEmptyMVar
      canaryTid <- forkIO do
        result <-
          Exception.try @SomeException $
            runOnDisposableWorker $
              (putMVar workerStarted () >> threadDelay (2 * 1000 * 1000))
                `finally` writeIORef ackFinished True
        putMVar canaryResult result
      takeMVar workerStarted
      -- A tiny scheduler-settle delay: 'workerStarted' only proves the
      -- *worker* thread has begun running (it alone can 'putMVar' it),
      -- not that the *canary* thread calling 'runOnDisposableWorker' has
      -- itself already reached its own blocking wait. This closes that
      -- narrow scheduling gap so the cancellation below reliably lands
      -- while canary is genuinely blocked waiting on the worker, exactly
      -- as intended, rather than racing its own setup code.
      threadDelay (20 * 1000)
      Exception.throwTo canaryTid TestCancellation
      mResult <- timeout (5 * 1000 * 1000) (takeMVar canaryResult)
      case mResult of
        Nothing -> expectationFailure "runOnDisposableWorker did not return after caller cancellation"
        Just _ -> pure ()
      readIORef ackFinished `shouldReturn` True

    it "leaves no worker thread running after the caller is cancelled and the worker is killed" do
      workerStarted <- newEmptyMVar
      workerTidRef <- newIORef Nothing
      canaryResult <- newEmptyMVar
      canaryTid <- forkIO do
        result <-
          Exception.try @SomeException $
            runOnDisposableWorker do
              tid <- myThreadId
              writeIORef workerTidRef (Just tid)
              putMVar workerStarted ()
              threadDelay (2 * 1000 * 1000)
        putMVar canaryResult result
      takeMVar workerStarted
      -- A tiny scheduler-settle delay: 'workerStarted' only proves the
      -- *worker* thread has begun running (it alone can 'putMVar' it),
      -- not that the *canary* thread calling 'runOnDisposableWorker' has
      -- itself already reached its own blocking wait. This closes that
      -- narrow scheduling gap so the cancellation below reliably lands
      -- while canary is genuinely blocked waiting on the worker, exactly
      -- as intended, rather than racing its own setup code.
      threadDelay (20 * 1000)
      Exception.throwTo canaryTid TestCancellation
      _ <- timeout (5 * 1000 * 1000) (takeMVar canaryResult)
      mWorkerTid <- readIORef workerTidRef
      case mWorkerTid of
        Nothing -> expectationFailure "worker never recorded its own ThreadId"
        Just workerTid -> do
          status <- threadStatus workerTid
          status `shouldNotBe` ThreadRunning

  describe "runBoundedOnDisposableWorker" do
    it "returns the action's result when it completes within the bound" do
      runBoundedOnDisposableWorker (2 * 1000 * 1000) (pure (7 :: Int)) `shouldReturn` Just 7

    it "propagates an exception raised inside the action, never converting it into a Nothing timeout" do
      result <- try @_ @IOException (runBoundedOnDisposableWorker (2 * 1000 * 1000) (throwIO (userError "boom")))
      case result of
        Left ioErr -> show ioErr `shouldContain` "boom"
        Right _ -> expectationFailure "expected the action's exception to propagate, not become Nothing"

    it "returns Nothing, without ever letting the action finish, when it does not complete within the bound" do
      hangCompleted <- newIORef False
      result <-
        runBoundedOnDisposableWorker (50 * 1000) do
          threadDelay (2 * 1000 * 1000)
          writeIORef hangCompleted True
      result `shouldBe` Nothing
      -- Long enough that, had the hung action not actually been
      -- terminated, it would certainly have flipped the flag by now.
      threadDelay (300 * 1000)
      readIORef hangCompleted `shouldReturn` False

    {- | Regression for the refresh-thread-lifetime audit, composed at the
    same level 'discoverFrozenEnvWithTimeout' itself uses: even when the
    /overall/ bounded action goes on to time out doing unrelated work
    /after/ 'freezeAuth', the background refresh thread it killed is
    -- and stays -- dead. Termination of the refresh child is not
    contingent on the outer bound being respected by the rest of the
    action; 'freezeAuth' kills it immediately, synchronously, as soon as
    it runs.
    -}
    it "still lets freezeAuth kill a background refresh thread even when the overall bounded action itself later times out" do
      let authEnv = AuthEnv "AKIAEXAMPLE" "secret" (Just "token") Nothing
      ref <- newIORef authEnv
      reachedRefresh <- newIORef False
      refreshThreadId <- forkIO do
        threadDelay (400 * 1000)
        writeIORef reachedRefresh True
      result <-
        runBoundedOnDisposableWorker (50 * 1000) do
          _ <- freezeAuth (Ref refreshThreadId ref)
          threadDelay (2 * 1000 * 1000)
      result `shouldBe` Nothing
      threadDelay (500 * 1000)
      readIORef reachedRefresh `shouldReturn` False
