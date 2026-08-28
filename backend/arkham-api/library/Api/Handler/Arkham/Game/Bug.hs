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
  discoverFrozenEnv,
) where

import Amazonka (AuthEnv (..), Env, Env' (..), Error (..), SerializeError (..), ServiceError (..), ToBody (toBody), discover, newEnv, runResourceT, send)
import Amazonka.Auth (Auth (..), AuthError (..))
import Amazonka.S3
import Api.Arkham.Export
import Api.Handler.Arkham.Games.Shared (withGameAccess)
import Control.Concurrent (forkIO, killThread)
import Control.Exception (throwIO, uninterruptibleMask_)
import Crypto.Hash.SHA256 qualified as SHA256
import Data.Aeson (encode)
import Data.ByteString.Base16 qualified as B16
import Import hiding ((==.))
import Network.HTTP.Types.Status (statusCode)
import Network.HTTP.Types.Status qualified as Status
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
-}
runOnDisposableWorker :: IO a -> IO a
runOnDisposableWorker action = do
  resultVar <- newEmptyMVar
  _workerThreadId <- forkIO (try @_ @SomeException action >>= putMVar resultVar)
  either throwIO pure =<< takeMVar resultVar

{- | Discover fresh AWS credentials for exactly one bounded upload, never
leaving behind a background refresh thread that could survive this call
(see 'runOnDisposableWorker' and 'freezeAuth'). The returned 'Env', if
any, has entirely static credentials from this point forward: reusing or
holding onto it for longer than the immediate HeadObject\/PutObject
sequence would silently let its temporary credentials go stale (Amazonka's
own refresh mechanism no longer exists for it), so a fresh
'discoverFrozenEnv' is called for every request rather than caching the
result -- this preserves IAM\/ECS\/IMDS-sourced temporary-credential
support and freshness, it just never lets any single 'Env' outlive one
request.
-}
discoverFrozenEnv :: IO (Either AuthError Env)
discoverFrozenEnv = try @_ @AuthError $ runOnDisposableWorker $ do
  env <- newEnv discover
  -- Uninterruptibly, to close the -- already vanishingly small, given
  -- AWS-issued temporary credentials always have a lifetime of at least
  -- several minutes before the earliest possible refresh attempt -- window
  -- between discovery succeeding and the background thread being killed.
  uninterruptibleMask_ do
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
      -- 'discoverFrozenEnv' returning, so this thread can never be the
      -- target of a delayed refresh-failure 'throwTo'.
      eEnv <- liftIO discoverFrozenEnv
      case eEnv of
        Left authErr -> do
          $(logWarn) $ "bug report upload: AWS credential discovery failed for game " <> toPathPiece gameId <> ": " <> tshow (classifyAuthErrorDiagnostic authErr)
          sendStatusJSON Status.status502 $ BugUploadError "Failed to upload bug report"
        Right env -> do
          -- The HeadObject/PutObject actions run inside 'runResourceT IO',
          -- which has no application-logger access. Rather than logging
          -- from inside that IO action (which would require an ad-hoc
          -- stdout/file logger bypassing the normal Yesod logging path),
          -- any failure is classified into an already-sanitized,
          -- strict-by-default (this package's 'StrictData') field of the
          -- 'BugUploadOutcome' returned by 'runBugUploadPolicy', and logged
          -- afterwards back in 'Handler' via the normal monad-logger path.
          -- There is no mutable cell crossing the 'runResourceT' boundary,
          -- so there is nothing that could retain an unforced thunk over a
          -- raw exception.
          outcome <-
            liftIO $ runResourceT do
              runBugUploadPolicy
                ( do
                    eHead <- try @_ @Error (send env (newHeadObject bucket key))
                    pure $ either classifyHeadObjectError (const ObjectPresent) eHead
                )
                ( do
                    ePut <- try @_ @Error (send env (newPutObject bucket key (toBody jsonBody)))
                    pure $ either (Left . classifyErrorDiagnostic) (const $ Right ()) ePut
                )
          case outcome of
            BugUploadSucceeded ->
              pure $ "https://arkham-horror-bugs.s3.amazonaws.com/exports/" <> filename
            BugUploadFailed failure -> do
              $(logWarn) $ "bug report upload: " <> describeBugUploadFailure gameId failure
              sendStatusJSON Status.status502 $ BugUploadError "Failed to upload bug report"
