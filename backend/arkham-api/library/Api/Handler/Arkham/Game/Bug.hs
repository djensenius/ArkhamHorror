{-# LANGUAGE TemplateHaskell #-}

module Api.Handler.Arkham.Game.Bug (
  postApiV1ArkhamGameBugR,

  -- * Exposed for regression tests
  HeadObjectOutcome (..),
  BugUploadOutcome (..),
  BugUploadFailure (..),
  classifyHeadObjectError,
  runBugUploadPolicy,
  AwsErrorDiagnostic (..),
  AwsErrorCategory (..),
  classifyErrorDiagnostic,
  AwsAuthErrorDiagnostic (..),
  classifyAuthErrorDiagnostic,
  runHeadObjectAction,
  runPutObjectAction,

  -- * Supervised AWS 'Env' lifecycle -- exposed for regression tests
  SupervisedEnvState (..),
  SupervisedEnv,
  startSupervisedEnv,
  readSupervisedEnv,
  stopSupervisedEnv,
  releaseAwsEnvChild,
  acquireAwsEnv,
  awaitAwsEnvInvalidation,
  defaultAwsEnvDiscoveryTimeoutMicros,
) where

import Amazonka (Env, Env' (..), Error (..), SerializeError (..), ServiceError (..), ToBody (toBody), discover, newEnv, runResourceT, send)
import Amazonka.Auth (Auth (..), AuthError (..))
import Amazonka.S3
import Api.Arkham.Export
import Api.Handler.Arkham.Games.Shared (withGameAccess)
import Control.Concurrent (killThread, threadDelay)
import Control.Concurrent.Async qualified as Async
import Control.Exception (evaluate)
import Crypto.Hash.SHA256 qualified as SHA256
import Data.Aeson (encode)
import Data.ByteString.Base16 qualified as B16
import Import hiding ((==.))
import Network.HTTP.Types.Status (statusCode)
import Network.HTTP.Types.Status qualified as Status
import System.IO.Unsafe (unsafePerformIO)
import System.Timeout (timeout)
import UnliftIO.Exception (finally, try)

{- | Result of attempting a HeadObject check for the export before upload.
Deliberately excludes any AWS response payload -- only the fact of
presence/absence/failure is meaningful to callers.
-}
data HeadObjectOutcome
  = ObjectPresent
  | ObjectAbsent
  | -- | Any HeadObject failure other than a genuine 404: permission denied,
    -- throttling, server errors, or a transport-level failure (DNS, TLS,
    -- timeout, connection refused, etc, all wrapped as 'TransportError').
    -- Carries the same sanitized diagnostic that would be logged, so the
    -- failure and its diagnostic can never drift apart -- there is no
    -- separate side channel to keep in sync.
    HeadObjectFailed AwsErrorDiagnostic
  deriving stock (Eq, Show)

-- | Final result of the bug-report upload policy.
data BugUploadOutcome
  = BugUploadSucceeded
  | BugUploadFailed BugUploadFailure
  deriving stock (Eq, Show)

data BugUploadFailure
  = HeadCheckFailed AwsErrorDiagnostic
  | PutObjectFailed AwsErrorDiagnostic
  deriving stock (Eq, Show)

{- | Sequencing/policy seam for the protected upload.

An existing object never triggers a PUT. A confirmed-absent object
triggers exactly one PUT. Any HeadObject failure short-circuits before PUT
is ever attempted, and a failing PUT is reported as failure. This is kept
free of any AWS/IO dependency (both actions are supplied by the caller) so
tests can inject deterministic stub actions and assert PUT is (or is not)
invoked. The diagnostic threaded through 'HeadObjectFailed'/'Left' is
carried straight into the returned 'BugUploadFailure' as an ordinary,
strict (via this package's default 'StrictData') function argument -- not
stashed in a mutable cell for later, separate retrieval -- so by the time
this action's result exists at all, it is already a fully-evaluated,
self-contained value with no lingering reference to whatever exception it
was classified from.
-}
runBugUploadPolicy :: Monad m => m HeadObjectOutcome -> m (Either AwsErrorDiagnostic ()) -> m BugUploadOutcome
runBugUploadPolicy headAction putAction =
  headAction >>= \case
    ObjectPresent -> pure BugUploadSucceeded
    HeadObjectFailed diag -> pure $ BugUploadFailed (HeadCheckFailed diag)
    ObjectAbsent -> do
      putResult <- putAction
      pure $ case putResult of
        Right () -> BugUploadSucceeded
        Left diag -> BugUploadFailed (PutObjectFailed diag)

{- | Classify a HeadObject failure using the exact Amazonka SDK error type.

Per AWS, a HeadObject error response carries no body (it's a HEAD request),
so the HTTP status code is the only reliable signal: 404 means the object
genuinely does not exist. Everything else -- 403 permission denied, 5xx
server errors, 429 throttling, a malformed response ('SerializeError'), or
a transport-level failure ('TransportError', covering DNS, TLS, timeout,
and connection failures) -- must be treated as a real failure and must
never unlock the PUT.
-}
classifyHeadObjectError :: Error -> HeadObjectOutcome
classifyHeadObjectError (ServiceError e) | statusCode e.status == 404 = ObjectAbsent
classifyHeadObjectError err = HeadObjectFailed (classifyErrorDiagnostic err)

{- | Run the HeadObject check, forcing the sanitized outcome with
'evaluate' *before* returning it -- unlike a bare @pure $ either ...@,
which would only build an unforced thunk still closing over the raw
Amazonka 'Error' (and, transitively, whatever it wraps: an 'HttpException'
carrying request headers, or a raw response body) until something else
happens to force it later. 'evaluate' itself only forces to /weak head
normal form/ (the outermost constructor), not full normal form -- but
every type it can bottom out in here ('HeadObjectOutcome',
'AwsErrorDiagnostic', 'AwsErrorCategory', down to a plain 'Int') is
declared in this module, which enables this package's default
'StrictData' extension, so every field of every nested constructor is
itself strict. That single outer WHNF force therefore cascades through
every nested constructor all the way down to atomic values with no
further structure to hide a thunk in -- the practical result is full
normal form, even though 'evaluate' alone never guarantees that in
general. Forcing here, inside this action, guarantees that by the time it
returns -- in particular, by the time 'runResourceT' built around it
returns -- nothing reachable from the result can still retain the raw
exception. A successful response is discarded in-scope (never inspected,
never retained) since only presence\/absence\/failure is meaningful to
callers. Polymorphic over 'MonadIO' \/ the response type so it is exactly
the same code path production ('ResourceT IO', a real Amazonka response)
and tests (plain 'IO', a fixture) both exercise.
-}
runHeadObjectAction :: MonadIO m => m (Either Error a) -> m HeadObjectOutcome
runHeadObjectAction sendHeadObject =
  sendHeadObject >>= \case
    Right _response -> pure ObjectPresent
    Left err -> liftIO (evaluate (classifyHeadObjectError err))

{- | Run the PutObject upload, forcing the sanitized outcome with
'evaluate' *before* returning it. See 'runHeadObjectAction' for why this
matters -- including why a single WHNF force is enough here, given this
module's 'StrictData' default -- without it, a failing PUT's result would
be an unforced thunk over the raw 'Error' until something outside this
action's scope happened to force it.
-}
runPutObjectAction :: MonadIO m => m (Either Error a) -> m (Either AwsErrorDiagnostic ())
runPutObjectAction sendPutObject =
  sendPutObject >>= \case
    Right _response -> pure (Right ())
    Left err -> do
      diag <- liftIO (evaluate (classifyErrorDiagnostic err))
      pure (Left diag)

-- | Stable, non-secret envelope for an upload failure. Never includes AWS
-- credentials, request/response bodies, or bucket internals.
newtype BugUploadError = BugUploadError {message :: Text}
  deriving stock (Show, Generic)
  deriving anyclass ToJSON

{- | Non-secret, structured classification of a per-request 'Error', safe to
log. Deliberately extracts only the numeric HTTP status, plus a category
derived purely from that status number, rather than showing the exception
itself or any text originating in the response: 'TransportError' wraps an
'HttpException' whose embedded 'Request' includes the raw
@X-Amz-Security-Token@ header verbatim when temporary/session credentials
are in use (http-client's default 'Show' instance only redacts
@Authorization@, not that header), 'SerializeError' carries the raw,
unredacted response body, and 'ServiceError''s symbolic error code and
request id are themselves server-provided free-form text -- so neither is
included here, only the status code they were reported with.
-}
data AwsErrorDiagnostic
  = AwsTransportFailure
  | AwsSerializeFailure {awsErrorStatus :: Int, awsErrorCategory :: AwsErrorCategory}
  | AwsServiceFailure {awsErrorStatus :: Int, awsErrorCategory :: AwsErrorCategory}
  deriving stock (Eq, Show)

{- | A coarse category derived purely from the numeric HTTP status code --
never from response body, headers, message text, symbolic error code, or
request id -- so it can never echo server-provided content.
-}
data AwsErrorCategory
  = AwsCategoryNotFound
  | AwsCategoryClientError
  | AwsCategoryThrottled
  | AwsCategoryServerError
  | AwsCategoryUnknownStatus
  deriving stock (Eq, Show)

categorizeAwsStatus :: Int -> AwsErrorCategory
categorizeAwsStatus 404 = AwsCategoryNotFound
categorizeAwsStatus 429 = AwsCategoryThrottled
categorizeAwsStatus s
  | s >= 500 && s < 600 = AwsCategoryServerError
  | s >= 400 && s < 500 = AwsCategoryClientError
  | otherwise = AwsCategoryUnknownStatus

classifyErrorDiagnostic :: Error -> AwsErrorDiagnostic
classifyErrorDiagnostic = \case
  TransportError _ -> AwsTransportFailure
  -- Pattern-matched directly on the field (rather than record-dot syntax)
  -- because 'SerializeError' and 'ServiceError' share field names within
  -- Amazonka.Types, and only 'ServiceError' picks up an automatic
  -- 'HasField' instance for '.status' in this GHC/amazonka-core version.
  SerializeError SerializeError' {status = st} -> AwsSerializeFailure (statusCode st) (categorizeAwsStatus (statusCode st))
  ServiceError e -> let st = statusCode e.status in AwsServiceFailure st (categorizeAwsStatus st)

-- | Render a bug-upload failure for the application logger. The diagnostic
-- crosses from 'runResourceT IO' back into 'Handler' as an ordinary,
-- already-sanitized field of 'BugUploadFailure' (see 'runBugUploadPolicy'),
-- never via a mutable side channel, so there is nothing here to keep in
-- sync with what actually happened.
describeBugUploadFailure :: ArkhamGameId -> BugUploadFailure -> Text
describeBugUploadFailure gameId = \case
  HeadCheckFailed diag -> "HeadObject failed for game " <> toPathPiece gameId <> ": " <> tshow diag
  PutObjectFailed diag -> "PutObject failed for game " <> toPathPiece gameId <> ": " <> tshow diag

{- | Non-secret, structured classification of an 'AuthError' raised by
credential/config discovery, safe to log. Deliberately never shows the
exception itself: 'RetrievalError' wraps an 'HttpException' with the same
@X-Amz-Security-Token@/IMDS-token header risk as 'AwsErrorDiagnostic',
'InvalidIAMError' wraps a message built by appending an Aeson decode error
to text derived from the (potentially credential-bearing) container
credentials response body (which Aeson's error messages can echo
fragments of), and 'OtherAuthError' wraps an arbitrary 'SomeException'
whose 'Show' output is entirely unconstrained. Only the failure category
(and, for 'AuthServiceError', the same sanitized status/category as
'AwsErrorDiagnostic' -- never the response's own error code or request id)
is reported.
-}
data AwsAuthErrorDiagnostic
  = AwsAuthRetrievalFailure
  | AwsAuthMissingEnv
  | AwsAuthMissingFile
  | AwsAuthInvalidFile
  | AwsAuthInvalidIAM
  | AwsAuthCredentialChainExhausted
  | AwsAuthServiceFailure {awsAuthErrorStatus :: Int, awsAuthErrorCategory :: AwsErrorCategory}
  | AwsAuthOtherFailure
  | -- | Credential discovery did not finish within
    -- 'defaultAwsEnvDiscoveryTimeoutMicros' (or an injected test bound).
    -- Deliberately nullary: there is no underlying exception to classify,
    -- only the bare fact that the bound was exceeded.
    AwsAuthDiscoveryTimedOut
  | -- | The dedicated AWS 'Env' supervisor thread itself has exited --
    -- whether from an unanticipated programmer fault propagating out of
    -- 'acquireAwsEnv'\/'awaitAwsEnvInvalidation', or from an explicit
    -- 'stopSupervisedEnv' -- and so no generation is being acquired,
    -- refreshed, or monitored any longer. Published so that a stale
    -- 'SupervisedEnvReady' snapshot (pointing at an 'Env' nobody is still
    -- supervising) can never be read as if it were still valid; see
    -- 'startSupervisedEnv'.
    AwsAuthSupervisorTerminated
  deriving stock (Eq, Show)

classifyAuthErrorDiagnostic :: AuthError -> AwsAuthErrorDiagnostic
classifyAuthErrorDiagnostic = \case
  RetrievalError _ -> AwsAuthRetrievalFailure
  MissingEnvError _ -> AwsAuthMissingEnv
  MissingFileError _ -> AwsAuthMissingFile
  InvalidFileError _ -> AwsAuthInvalidFile
  InvalidIAMError _ -> AwsAuthInvalidIAM
  CredentialChainExhausted -> AwsAuthCredentialChainExhausted
  AuthServiceError e -> let st = statusCode e.status in AwsAuthServiceFailure st (categorizeAwsStatus st)
  OtherAuthError _ -> AwsAuthOtherFailure

{- | Kill whatever background credential-refresh thread an already-acquired
'Auth' value may own, without otherwise touching its credentials.

Why this exists: the pinned Amazonka fork's
@Amazonka.Auth.Background.fetchAuthInBackground@ (used internally by every
role-based credential source @discover@ can reach -- container and
instance-profile credentials both go through it) starts a background
thread as soon as temporary\/expiring credentials are obtained. Per its
own source, that thread captures /the thread that called @discover@/ as
its eventual refresh-failure @throwTo@ target, and separately, its own
'ThreadId' -- returned by 'forkIO' -- is what ends up inside the 'Ref'
constructor here. This is exactly why 'startSupervisedEnv' never calls
@discover@ from a Warp request-handling thread: it is always called from
one dedicated, long-lived supervisor thread (see 'acquireAwsEnv'), which
remains alive for as long as the generation it acquired remains current
(see 'awaitAwsEnvInvalidation') -- so that thread is always still there,
never a request worker, to either observe a genuine refresh failure or
(via this function) pre-emptively kill the thread that would otherwise
raise it. 'Auth' (no expiration, e.g. static access keys) never started
such a thread in the first place, so there is nothing to kill.
-}
releaseAwsEnvChild :: Auth -> IO ()
releaseAwsEnvChild (Auth _) = pure ()
releaseAwsEnvChild (Ref refreshThreadId _) = killThread refreshThreadId

{- | How long 'acquireAwsEnv' allows AWS credential discovery to run before
giving up and reporting the sanitized 'AwsAuthDiscoveryTimedOut' dependency
failure, instead of ever blocking the supervisor thread indefinitely. Real
discovery (environment\/file\/IMDS\/ECS metadata lookups) normally completes
in well under a second; this bound exists purely so a wedged or
unreachable credential provider can never permanently wedge the
supervisor in 'SupervisedEnvInitializing'. Because discovery always runs
on the dedicated supervisor thread (never a request-handling thread), a
hang here only ever delays this one thread reaching 'SupervisedEnvReady'
-- every request continues to observe an immediate, bounded, sanitized
snapshot (see 'readSupervisedEnv') the entire time, never blocking on
discovery itself.
-}
defaultAwsEnvDiscoveryTimeoutMicros :: Int
defaultAwsEnvDiscoveryTimeoutMicros = 8 * 1000 * 1000

{- | Acquire one fresh generation of AWS credentials\/'Env', already
classified into a sanitized diagnostic on failure (including timeout). Only
ever called from 'startSupervisedEnv''s dedicated supervisor thread -- see
'releaseAwsEnvChild' for why that placement matters. Credentials are left
exactly as @discover@\/'Amazonka.newEnv' returned them (never frozen): a
successfully acquired 'Env' keeps Amazonka's own background refresh alive
for as long as this generation remains current, so genuine temporary
credentials (IAM\/ECS\/IMDS) stay fresh for the supervisor's entire
lifetime rather than being re-discovered\/re-frozen per request.
-}
acquireAwsEnv :: IO (Either AwsAuthErrorDiagnostic Env)
acquireAwsEnv = do
  outcome <- timeout defaultAwsEnvDiscoveryTimeoutMicros (try @_ @AuthError (newEnv discover))
  case outcome of
    Nothing -> pure (Left AwsAuthDiscoveryTimedOut)
    Just (Left authErr) -> Left <$> evaluate (classifyAuthErrorDiagnostic authErr)
    Just (Right env) -> pure (Right env)

{- | Block for as long as @env@'s generation remains valid, then return the
sanitized diagnostic that invalidated it. Only ever called from
'startSupervisedEnv''s dedicated supervisor thread, immediately after that
same thread acquired @env@ via 'acquireAwsEnv' -- so it is exactly the
thread the pinned Amazonka fork's background refresh timer captured as its
@throwTo@ target (see 'releaseAwsEnvChild'), and it is still here,
deliberately blocked, ready to receive that exact exception whenever (if
ever) it arrives; never a Warp request thread that has since moved on.

Blocks forever unless interrupted: either by a genuine delayed
'AuthError' refresh failure (caught here, classified, and returned as this
generation's invalidation reason), or by an external asynchronous
exception (e.g. 'stopSupervisedEnv''s cancellation, or an unanticipated
programmer fault elsewhere) -- which is deliberately *not* caught here and
propagates unchanged, since only 'AuthError' is a recognized,
sanitizable dependency failure for this generation. Either way, this
generation's background refresh child (if it has one) is always killed
via 'releaseAwsEnvChild' -- wrapped in 'finally', so it runs whether this
function returns normally (an 'AuthError' was caught) or the exception
propagates straight through -- so no refresh child ever survives whatever
made this function stop blocking.
-}
awaitAwsEnvInvalidation :: Env -> IO AwsAuthErrorDiagnostic
awaitAwsEnvInvalidation env = do
  outcome <- try @_ @AuthError (blockForever `finally` releaseAwsEnvChild (runIdentity env.auth))
  either (evaluate . classifyAuthErrorDiagnostic) absurd outcome
 where
  blockForever :: IO Void
  blockForever = forever (threadDelay maxBound)

{- | A typed snapshot of the application's single AWS 'Env' supervisor,
observable by request handlers without ever blocking on -- or triggering
-- credential discovery\/refresh themselves. Never contains a raw
exception: 'SupervisedEnvUnavailable' only ever carries an already-forced
'AwsAuthErrorDiagnostic'.
-}
data SupervisedEnvState env
  = -- | The supervisor has not yet completed its first acquisition.
    SupervisedEnvInitializing
  | -- | @env@ is this generation's live, currently-valid resource.
    SupervisedEnvReady env
  | -- | The most recent generation failed to acquire, or was invalidated
    -- (e.g. a genuine background refresh failure), or the supervisor
    -- itself has stopped; a fresh attempt is (or was, before stopping)
    -- pending after backoff.
    SupervisedEnvUnavailable AwsAuthErrorDiagnostic
  deriving stock (Eq, Show)

-- | A running supervisor: a strict, typed state snapshot plus the handle
-- of the single dedicated thread that owns its entire acquire\/monitor\/
-- reacquire lifecycle. See 'startSupervisedEnv'.
data SupervisedEnv env = SupervisedEnv
  { supervisedEnvStateVar :: TVar (SupervisedEnvState env)
  , supervisedEnvAsync :: Async.Async ()
  }

{- | Start a dedicated, single-thread supervisor that owns the entire
acquire\/monitor\/reacquire lifecycle of one resource generation at a time,
publishing a strict typed snapshot of its current state for readers (see
'readSupervisedEnv') and never blocking a caller on acquisition.

This is the architectural fix for the async-credential-refresh-escape
audit: exactly one thread -- this supervisor's own, for the entire
lifetime of the running application -- ever calls @acquire@ (in
production, 'acquireAwsEnv', i.e. @newEnv discover@). Because the pinned
Amazonka fork's background refresh timer always targets \"the thread that
called @discover@\" for its eventual refresh-failure @throwTo@, and that
thread here is always this same dedicated supervisor thread (never a
per-request thread, and this supervisor is never restarted per request),
a delayed refresh failure can only ever land back on the one thread still
deliberately waiting for it (in @awaitInvalidation@, in production
'awaitAwsEnvInvalidation') -- never on a Warp worker that has since moved
on to, or been reused for, an unrelated request.

The lifecycle itself is built entirely from the \"async\" package's
high-level 'Async.async'\/'Async.cancel' rather than a hand-rolled
@forkIO@\/@MVar@\/@mask@ protocol: 'Async.async' already wraps its action in
a plain, unrestricted 'Control.Exception.try' internally (catching
/every/ exception, synchronous or asynchronous, so its result @MVar@\/@STM@
slot is always filled), and 'Async.cancel' both delivers an ordinary,
interruptible @throwTo@ and synchronously waits for the target thread to
actually finish before returning -- exactly the \"terminate and wait for
completion\" protocol this module previously had to construct and debug
by hand.

On every loop iteration: @acquire@ is attempted; a failure publishes
'SupervisedEnvUnavailable' and waits @backoff@ before retrying.  A success
publishes 'SupervisedEnvReady', then blocks in @awaitInvalidation@ until
that generation is invalidated, at which point its diagnostic is published
and @backoff@ is awaited before the next attempt. Whatever exception
terminates the supervisor thread itself (an external 'stopSupervisedEnv',
or an unanticipated exception escaping @acquire@\/@awaitInvalidation@ that
is deliberately *not* caught here, per the \"no broad catch\" requirement)
is guaranteed, via 'finally', to first publish 'AwsAuthSupervisorTerminated'
-- so a reader can never observe a stale 'SupervisedEnvReady' snapshot
pointing at a resource nobody is monitoring any longer.
-}
startSupervisedEnv
  :: IO (Either AwsAuthErrorDiagnostic env)
  -- ^ acquire one fresh generation.
  -> (env -> IO AwsAuthErrorDiagnostic)
  -- ^ block until @env@'s generation is invalidated (or an external
  -- exception interrupts this call), returning the classified reason.
  -> IO ()
  -- ^ backoff between a failed\/invalidated generation and the next
  -- attempt.
  -> IO (SupervisedEnv env)
startSupervisedEnv acquire awaitInvalidation backoff = do
  stateVar <- newTVarIO SupervisedEnvInitializing
  supervisorAsync <- Async.async (supervise stateVar)
  pure SupervisedEnv {supervisedEnvStateVar = stateVar, supervisedEnvAsync = supervisorAsync}
 where
  supervise stateVar =
    forever loopOnce `finally` publish stateVar AwsAuthSupervisorTerminated
   where
    loopOnce = do
      acquired <- acquire
      case acquired of
        Left diag -> publish stateVar diag >> backoff
        Right env -> do
          atomically $ writeTVar stateVar (SupervisedEnvReady env)
          diag <- awaitInvalidation env
          publish stateVar diag
          backoff
  publish stateVar diag = do
    forced <- evaluate diag
    atomically $ writeTVar stateVar (SupervisedEnvUnavailable forced)

-- | Read the supervisor's current strict, typed state snapshot. Never
-- blocks on acquisition\/refresh: a request observing 'SupervisedEnvReady'
-- may use that @env@ immediately; any other state is an immediate,
-- sanitized dependency failure.
readSupervisedEnv :: SupervisedEnv env -> IO (SupervisedEnvState env)
readSupervisedEnv = readTVarIO . supervisedEnvStateVar

{- | Explicitly stop a supervisor: terminate its dedicated thread and
/wait/ for it to actually finish (via 'Async.cancel', see 'startSupervisedEnv')
-- which, per 'awaitInvalidation''s\/'awaitAwsEnvInvalidation''s own
'finally', also releases the current generation's background resource
(e.g. kills a live Amazonka refresh thread) before this returns. Intended
for deterministic tests (each starting\/stopping its own supervisor) and,
optionally, graceful process shutdown; production request handling never
calls this on the application's single supervisor.
-}
stopSupervisedEnv :: SupervisedEnv env -> IO ()
stopSupervisedEnv = Async.cancel . supervisedEnvAsync

{- | The application's single AWS 'Env' supervisor: lazily started on first
use and, once started, never restarted for the remainder of the process.
'unsafePerformIO' plus 'NOINLINE' on a top-level CAF is this codebase's
existing pattern for exactly this "lazy, at-most-once, shared for the
whole process" idiom (see @Arkham.Metrics@'s @globalMetricsRef@); GHC's
runtime guarantees a 'NOINLINE' top-level CAF is forced at most once even
under concurrent access from multiple Warp request threads (the first
forcer runs it; any concurrent forcer blocks until that completes and then
shares the same result) -- so no additional locking is needed here to
ensure only one supervisor thread\/one @newEnv discover@ call site ever
exists for the whole application.
-}
{-# NOINLINE globalAwsEnvSupervisor #-}
globalAwsEnvSupervisor :: SupervisedEnv Env
globalAwsEnvSupervisor =
  unsafePerformIO $ startSupervisedEnv acquireAwsEnv awaitAwsEnvInvalidation awsEnvSupervisorBackoff
 where
  -- Real reacquisition backoff: deliberately not configurable/injectable
  -- in production (unlike 'defaultAwsEnvDiscoveryTimeoutMicros'), since
  -- nothing about backoff duration affects request-facing correctness --
  -- it only paces how often a persistently failing credential source is
  -- retried, avoiding a tight retry storm against it.
  awsEnvSupervisorBackoff :: IO ()
  awsEnvSupervisorBackoff = threadDelay (5 * 1000 * 1000)

postApiV1ArkhamGameBugR :: ArkhamGameId -> Handler Text
postApiV1ArkhamGameBugR gameId = do
  Entity userId user <- getRequestUser
  withGameAccess
    user.admin
    (isJust <$> runDB (getBy $ UniquePlayer userId gameId))
    notFound
    $ do
      export <- generateScenarioExport gameId

      let jsonBody = encode export
      let filename = decodeUtf8 (B16.encode $ SHA256.hashlazy jsonBody) <> ".json"

      -- Set S3 parameters
      let bucket = "arkham-horror-bugs"
          key = ObjectKey $ "exports/" <> filename

      -- Read the application's single AWS 'Env' supervisor's current
      -- snapshot (see 'globalAwsEnvSupervisor'/'startSupervisedEnv'):
      -- never blocks on credential discovery/refresh, and never runs
      -- 'newEnv'/'discover' on this (or any other) Warp request-handling
      -- thread -- only the supervisor's own dedicated thread ever does,
      -- so a delayed background refresh failure can never target a
      -- request worker. Any non-'Ready' snapshot is already a fully
      -- classified/sanitized diagnostic (or the absence of one yet).
      snapshot <- liftIO (readSupervisedEnv globalAwsEnvSupervisor)
      case snapshot of
        SupervisedEnvInitializing -> do
          $(logWarn) $ "bug report upload: AWS credential supervisor still initializing for game " <> toPathPiece gameId
          sendStatusJSON Status.status502 $ BugUploadError "Failed to upload bug report"
        SupervisedEnvUnavailable diag -> do
          $(logWarn) $ "bug report upload: AWS credentials unavailable for game " <> toPathPiece gameId <> ": " <> tshow diag
          sendStatusJSON Status.status502 $ BugUploadError "Failed to upload bug report"
        SupervisedEnvReady env -> do
          -- The HeadObject/PutObject actions run inside 'runResourceT IO',
          -- which has no application-logger access. Rather than logging
          -- from inside that IO action (which would require an ad-hoc
          -- stdout/file logger bypassing the normal Yesod logging path),
          -- any failure is classified into an already-sanitized diagnostic
          -- and *forced* ('runHeadObjectAction'/'runPutObjectAction' both
          -- use 'evaluate', not a bare lazy 'pure') before it ever leaves
          -- 'ResourceT'; the whole 'BugUploadOutcome' is forced again just
          -- before 'runResourceT' returns, so nothing reachable from
          -- 'outcome' can retain any part of a raw Amazonka 'Error'/
          -- response by the time it is logged afterwards back in
          -- 'Handler' via the normal monad-logger path.
          outcome <-
            liftIO $ runResourceT do
              result <-
                runBugUploadPolicy
                  (runHeadObjectAction (try @_ @Error (send env (newHeadObject bucket key))))
                  (runPutObjectAction (try @_ @Error (send env (newPutObject bucket key (toBody jsonBody)))))
              liftIO (evaluate result)
          case outcome of
            BugUploadSucceeded ->
              pure $ "https://arkham-horror-bugs.s3.amazonaws.com/exports/" <> filename
            BugUploadFailed failure -> do
              $(logWarn) $ "bug report upload: " <> describeBugUploadFailure gameId failure
              sendStatusJSON Status.status502 $ BugUploadError "Failed to upload bug report"
