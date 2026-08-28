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
  freezeAuth,
  runOnDisposableWorker,
  runBoundedOnDisposableWorker,
  discoverFrozenEnv,
  discoverFrozenEnvWithTimeout,
  defaultDiscoverFrozenEnvTimeoutMicros,
  runHeadObjectAction,
  runPutObjectAction,
) where

import Amazonka (AuthEnv (..), Env, Env' (..), Error (..), SerializeError (..), ServiceError (..), ToBody (toBody), discover, newEnv, runResourceT, send)
import Amazonka.Auth (Auth (..), AuthError (..))
import Amazonka.S3
import Api.Arkham.Export
import Api.Handler.Arkham.Games.Shared (withGameAccess)
import Control.Concurrent (forkIOWithUnmask, killThread)
import Control.Exception (catch, evaluate, mask, throwIO)
import Control.Exception qualified as Exception
import Crypto.Hash.SHA256 qualified as SHA256
import Data.Aeson (encode)
import Data.ByteString.Base16 qualified as B16
import Import hiding ((==.))
import Network.HTTP.Types.Status (statusCode)
import Network.HTTP.Types.Status qualified as Status
import System.Timeout (timeout)
import UnliftIO.Exception (try)

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
  | -- | Credential discovery plus freezing did not finish within
    -- 'defaultDiscoverFrozenEnvTimeoutMicros' (or an injected test bound).
    -- Deliberately nullary: there is no underlying exception to classify,
    -- only the bare fact that the bound was exceeded.
    AwsAuthDiscoveryTimedOut
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

{- | Given a freshly 'Amazonka.discover'ed 'Auth' value, synchronously stop
any background credential-refresh thread it may own and return a frozen
snapshot of the current credentials.

Why this exists: the pinned Amazonka fork's
@Amazonka.Auth.Background.fetchAuthInBackground@ (used internally by every
role-based credential source @discover@ can reach -- container and
instance-profile credentials both go through it) starts a background
thread as soon as temporary\/expiring credentials are obtained. Per its
own source, that thread captures /the thread that called @discover@/ as
@p@, and separately, its own 'ThreadId' -- returned by 'forkIO' -- is what
ends up inside the 'Ref' constructor here. On a later refresh failure it
unconditionally @throwTo@s a sanitized-shape 'AuthError' at @p@, without
ever checking whether that original caller is still doing anything
related. If @newEnv discover@ were called once per HTTP request and then
kept around (as it used to be, and as a naive per-request 'Env' still
would be), a delayed refresh failure could land on a Warp worker thread
that has since moved on to -- or been reused for -- a completely
unrelated request.

The fix is architectural, not a broader catch: 'killThread' on the 'Ref'
constructor's 'ThreadId' terminates that exact background thread
immediately, before it can ever reach its @throwTo@ line -- there is no
window in which it is merely \"caught\" after firing, it is prevented from
firing at all. 'Auth' (no expiration, e.g. static access keys) never
started such a thread in the first place, so there is nothing to stop.
The returned 'AuthEnv' is then a plain, static snapshot: safe to embed in
a frozen 'Env' that will only ever be used for one bounded
HeadObject\/PutObject sequence and discarded, never refreshed or reused
for longer than that.
-}
freezeAuth :: Auth -> IO AuthEnv
freezeAuth (Auth authEnv) = pure authEnv
freezeAuth (Ref refreshThreadId ref) = do
  killThread refreshThreadId
  readIORef ref

{- | Run @action@ to completion on a brand-new, disposable worker thread --
never the calling thread -- and return (or re-throw, on the calling
thread, as an ordinary synchronous exception) its result once finished.

This exists solely so that 'Amazonka.newEnv'\/'discover' -- and therefore
'Amazonka.Auth.Background.fetchAuthInBackground''s capture of \"the
calling thread\" as its eventual refresh-failure @throwTo@ target -- is
never run on a long-lived Warp request-handling thread. The disposable
worker is the only thread that can ever be targeted, and (via
'freezeAuth', called from within @action@) it kills any such target
before it returns, so there is nothing left to target by the time this
function's caller resumes; the worker itself is not reused for anything
else afterwards.

If the calling thread is itself asynchronously interrupted (or
cancelled -- e.g. Warp abandoning a request) while waiting for the
worker, the worker is never left running past this call: it is killed
and this function synchronously waits for its acknowledgement (the
worker always @putMVar@s exactly once, even when killed) before letting
the original interrupting exception propagate completely unchanged --
never a false 502, never swallowed. That wait is only as protected as
an ordinary 'Control.Exception.catch' handler already is: per its own
Haddock, \"the handler is inside an implicit mask\" -- not
'Control.Exception.uninterruptibleMask_'. So if the worker were ever
somehow genuinely stuck (e.g. blocked in an uninterruptible foreign
call), a further external exception could still interrupt this wait,
rather than turning the calling thread into a permanent,
uninterruptible orphan.

The worker deliberately wraps @action@ in
'Control.Exception.try' (the plain one from "Control.Exception", imported
qualified here as @Exception@) rather than the otherwise-preferred
"UnliftIO.Exception" 'try'. The latter only catches /synchronous/
exceptions by design (it is meant to keep code from accidentally
swallowing an unrelated asynchronous cancellation), which is exactly
wrong here: this worker's own @putMVar resultVar@ acknowledgement must
run even when it was the disposable worker itself that received an
asynchronous exception -- namely the very 'killThread' issued by this
function's own cancellation cleanup above. Using the sync-only 'try'
would let that 'killThread' terminate the worker before it ever reaches
@putMVar@, so the cleanup path's subsequent @takeMVar resultVar@ would
block forever waiting for an acknowledgement that can never arrive.
-}
runOnDisposableWorker :: IO a -> IO a
runOnDisposableWorker action = do
  resultVar <- newEmptyMVar
  workerThreadId <-
    forkIOWithUnmask $ \unmask ->
      unmask (Exception.try @SomeException action) >>= putMVar resultVar
  result <-
    takeMVar resultVar `catch` \(callerException :: SomeException) -> do
      killThread workerThreadId
      _ <- takeMVar resultVar
      throwIO callerException
  either throwIO pure result

{- | As 'runOnDisposableWorker', but bounded to at most @timeoutMicros@
microseconds; returns 'Nothing' if that bound is exceeded. The timeout is
applied *inside* the disposable worker (i.e. it is the worker, never the
caller, that 'System.Timeout.timeout' can interrupt), so a hung @action@
is still cleaned up exactly as 'runOnDisposableWorker' always cleans up a
cancelled worker -- nothing survives this call either way, whether it
finishes, times out, or the caller is itself cancelled while waiting.

'System.Timeout.timeout' only ever converts *its own* internal,
uniquely-tagged timeout signal into 'Nothing'; any other exception raised
by @action@ (including a genuine 'AuthError' from discovery) still
propagates through unchanged -- this is not a broad, success-shaped catch.
-}
runBoundedOnDisposableWorker :: Int -> IO a -> IO (Maybe a)
runBoundedOnDisposableWorker timeoutMicros action =
  runOnDisposableWorker (timeout timeoutMicros action)

{- | How long 'discoverFrozenEnv' allows AWS credential discovery plus
freezing to run before giving up and reporting the sanitized
'AwsAuthDiscoveryTimedOut' dependency failure, instead of ever blocking a
request indefinitely. Real discovery (environment\/file\/IMDS\/ECS
metadata lookups) normally completes in well under a second; this bound
exists purely so a wedged or unreachable credential provider can never
hang a request worker forever, and is kept comfortably below typical
request-handling deadlines.
-}
defaultDiscoverFrozenEnvTimeoutMicros :: Int
defaultDiscoverFrozenEnvTimeoutMicros = 8 * 1000 * 1000

{- | Discover fresh AWS credentials for exactly one bounded upload, never
leaving behind a background refresh thread that could survive this call
(see 'runOnDisposableWorker' and 'freezeAuth'), and never blocking past
'defaultDiscoverFrozenEnvTimeoutMicros'. The returned 'Env', if any, has
entirely static credentials from this point forward: reusing or holding
onto it for longer than the immediate HeadObject\/PutObject sequence
would silently let its temporary credentials go stale (Amazonka's own
refresh mechanism no longer exists for it), so a fresh 'discoverFrozenEnv'
is called for every request rather than caching the result -- this
preserves IAM\/ECS\/IMDS-sourced temporary-credential support and
freshness, it just never lets any single 'Env' outlive one request.
-}
discoverFrozenEnv :: IO (Either AwsAuthErrorDiagnostic Env)
discoverFrozenEnv = discoverFrozenEnvWithTimeout defaultDiscoverFrozenEnvTimeoutMicros

{- | As 'discoverFrozenEnv', but with an explicit, injectable timeout (in
microseconds) rather than the fixed production default -- this is the
seam production code and deterministic tests share, so tests can exercise
the discovery-hangs-forever case with a tiny bound instead of waiting on
(or trying to fake) the real production duration. 'discoverFrozenEnv'
itself is not directly unit-tested beyond this: real credential discovery
genuinely talks to the environment\/filesystem\/IMDS\/ECS metadata
endpoints, which is out of scope for a deterministic unit test, consistent
with this codebase's existing practice of only unit-testing the pure
classification\/sequencing seams around such calls.
-}
discoverFrozenEnvWithTimeout :: Int -> IO (Either AwsAuthErrorDiagnostic Env)
discoverFrozenEnvWithTimeout timeoutMicros = do
  eResult <- try @_ @AuthError (runBoundedOnDisposableWorker timeoutMicros discoverAndFreeze)
  pure $ case eResult of
    Left authErr -> Left (classifyAuthErrorDiagnostic authErr)
    Right Nothing -> Left AwsAuthDiscoveryTimedOut
    Right (Just env) -> Right env
 where
  -- 'mask'ed (not 'uninterruptibleMask_'ed) from the moment 'newEnv
  -- discover' returns: 'restore' only re-admits interruption for the
  -- discovery call itself (so a hung network request can still be timed
  -- out or cancelled), never for the freeze step immediately after, so
  -- there is no gap in which a background refresh thread could exist
  -- without this worker having already committed to killing it before
  -- anything else can interrupt it. If 'killThread' itself would need to
  -- block (the refresh thread already masked), that specific call
  -- remains an interruptible operation even under plain 'mask', so this
  -- can still be interrupted rather than hang uninterruptibly.
  discoverAndFreeze :: IO Env
  discoverAndFreeze = mask $ \restore -> do
    env <- restore (newEnv discover)
    frozen <- freezeAuth (runIdentity env.auth)
    pure env {auth = Identity (Auth frozen)}

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

      -- Fresh credentials are discovered for every request (see
      -- 'discoverFrozenEnv') on a disposable worker thread, never this
      -- Warp request-handling thread, and immediately frozen: no
      -- background credential-refresh thread can survive past
      -- 'discoverFrozenEnv' returning (whether it succeeds, times out,
      -- fails, or this very request is cancelled while waiting on it),
      -- so this thread can never be the target of a delayed
      -- refresh-failure 'throwTo'. The returned diagnostic (including the
      -- timeout case) is already fully classified/sanitized.
      eEnv <- liftIO discoverFrozenEnv
      case eEnv of
        Left diag -> do
          $(logWarn) $ "bug report upload: AWS credential discovery failed for game " <> toPathPiece gameId <> ": " <> tshow diag
          sendStatusJSON Status.status502 $ BugUploadError "Failed to upload bug report"
        Right env -> do
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
