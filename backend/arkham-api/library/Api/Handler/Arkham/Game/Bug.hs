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
  AwsEnvSupervisorOutcome (..),
  awsEnvSupervisorStep,
) where

import Amazonka (Env, Error (..), SerializeError (..), ServiceError (..), ToBody (toBody), discover, newEnv, runResourceT, send)
import Amazonka.Auth (AuthError (..))
import Amazonka.S3
import Api.Arkham.Export
import Api.Handler.Arkham.Games.Shared (withGameAccess)
import Control.Concurrent (forkIO, threadDelay)
import Crypto.Hash.SHA256 qualified as SHA256
import Data.Aeson (encode)
import Data.ByteString.Base16 qualified as B16
import Import hiding ((==.))
import Network.HTTP.Types.Status (statusCode)
import Network.HTTP.Types.Status qualified as Status
import System.IO.Unsafe (unsafePerformIO)
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

{- | Outcome of a single supervisor round, so the production driver
('awsEnvSupervisorLoop') can decide whether to back off (discovery itself
is failing -- e.g. no credential source is reachable at all) or retry
immediately (discovery previously succeeded; this only recovers a now-stale
env).
-}
data AwsEnvSupervisorOutcome
  = AwsEnvDiscoveryFailed
  | AwsEnvRefreshFailed
  | AwsEnvHoldFinished
  deriving stock (Eq, Show)

{- | Sequencing seam for the AWS 'Env' supervisor. Decoupled from any real
'newEnv'\/'discover'\/background-thread machinery so tests can inject
deterministic stub actions and assert the loop's shape -- in particular,
that a caught refresh failure from @holdAction@ is always absorbed here
and never escapes -- without spawning a real background credential-refresh
thread or depending on network\/IMDS\/ECS availability.

Why this exists: the pinned Amazonka fork's
@Amazonka.Auth.Background.fetchAuthInBackground@ (used internally by every
role-based credential source @discover@ can reach -- container and
instance-profile credentials both go through it) starts a background
thread as soon as temporary\/expiring credentials are obtained. That thread
captures the /calling/ thread's id and, on a later refresh failure,
unconditionally @throwTo@s a sanitized-shape 'AuthError' at it, without
ever checking whether the original caller is still doing anything related.
If @newEnv discover@ were called per HTTP request (as it used to be), a
delayed refresh failure could land on a Warp worker thread that has since
moved on to -- or been reused for -- a completely unrelated request,
producing a raw 500 and unstructured framework-level logging, plus a
leaked background thread.

The fix is architectural, not a broader catch: @newEnv discover@ is now
called exactly once per \"round\" of this loop, always from the same
dedicated, permanently-running supervisor thread ('awsEnvHandle', started
once via 'forkIO' and never a Warp request-handling thread), and
@holdAction@ -- which blocks for as long as this concrete env remains
current -- always runs on that same thread too. Any refresh-failure
@throwTo@ can therefore only ever target this loop, never a request
worker. Request handlers only ever read the shared, published env (or its
absence) from a 'TVar'; they never call @newEnv@\/@discover@ themselves, so
they can never be the captured target in the first place.

Per the fork's own source, a refresh failure is not retried internally: it
throws once and that background thread ends there. So a failing
@holdAction@ is treated as \"this concrete env's credentials are now stale
and will not self-heal\": it is discarded (publishing 'Nothing') and the
loop goes straight back to a fresh @discoverAction@ call, which starts an
entirely new background refresh timer pinned to this same, still-alive
supervisor thread.
-}
awsEnvSupervisorStep
  :: Monad m
  => m (Either AuthError env)
  -> (env -> m (Either AuthError ()))
  -> (Maybe env -> m ())
  -> (Maybe AwsAuthErrorDiagnostic -> m ())
  -> m AwsEnvSupervisorOutcome
awsEnvSupervisorStep discoverAction holdAction publishEnv publishDiagnostic = do
  discovered <- discoverAction
  case discovered of
    Left authErr -> do
      publishEnv Nothing
      publishDiagnostic (Just $ classifyAuthErrorDiagnostic authErr)
      pure AwsEnvDiscoveryFailed
    Right env -> do
      publishEnv (Just env)
      publishDiagnostic Nothing
      held <- holdAction env
      case held of
        Left authErr -> do
          publishEnv Nothing
          publishDiagnostic (Just $ classifyAuthErrorDiagnostic authErr)
          pure AwsEnvRefreshFailed
        Right () -> pure AwsEnvHoldFinished

-- | Capped exponential backoff between discovery attempts while credentials
-- remain entirely unavailable (e.g. no credential source is reachable at
-- all).
awsEnvSupervisorMaxBackoffSeconds :: Int
awsEnvSupervisorMaxBackoffSeconds = 60

{- | Handle to the single, process-wide, supervised Amazonka 'Env' shared by
every bug-report upload request. See 'awsEnvSupervisorStep' for why this
must be a single long-lived owner rather than a per-request 'newEnv'.
-}
data AwsEnvHandle = AwsEnvHandle
  { awsEnvCurrent :: TVar (Maybe Env)
  , awsEnvLastDiagnostic :: TVar (Maybe AwsAuthErrorDiagnostic)
  }

{- | Production driver: repeatedly runs 'awsEnvSupervisorStep' against real
'newEnv discover' \/ 'threadDelay' \/ 'TVar' publishing, forever, on
whichever thread it is started on. Must only ever be started once, via
'forkIO', from 'awsEnvHandle'.
-}
awsEnvSupervisorLoop :: AwsEnvHandle -> IO ()
awsEnvSupervisorLoop handle = go 1
 where
  go backoffSeconds = do
    outcome <-
      awsEnvSupervisorStep
        (try @_ @AuthError (newEnv discover))
        (const $ try @_ @AuthError (threadDelay maxBound))
        (atomically . writeTVar handle.awsEnvCurrent)
        (atomically . writeTVar handle.awsEnvLastDiagnostic)
    case outcome of
      AwsEnvDiscoveryFailed -> do
        threadDelay (backoffSeconds * 1000000)
        go (min awsEnvSupervisorMaxBackoffSeconds (backoffSeconds * 2))
      AwsEnvRefreshFailed -> go 1
      AwsEnvHoldFinished -> go 1

{- | The single, lazily-started, process-wide handle. Forcing this for the
first time (i.e. the first bug-report upload request) starts the
supervisor thread via 'forkIO'; every subsequent request -- and the
supervisor thread itself -- shares the exact same 'TVar's. 'NOINLINE' plus
'unsafePerformIO' is the same lazily-initialized-singleton idiom already
used for 'Arkham.Metrics.globalMetricsRef'; here it guarantees "exactly one
discovery, exactly one supervisor thread, for the life of the process"
without threading a new field through 'Foundation'\/'Application' and every
place that constructs an 'App' (including tests).
-}
{-# NOINLINE awsEnvHandle #-}
awsEnvHandle :: AwsEnvHandle
awsEnvHandle = unsafePerformIO do
  current <- newTVarIO Nothing
  lastDiagnostic <- newTVarIO Nothing
  let handle = AwsEnvHandle current lastDiagnostic
  _ <- forkIO (awsEnvSupervisorLoop handle)
  pure handle

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

      -- The shared Amazonka 'Env' is discovered and supervised exactly once
      -- for the whole process's lifetime by 'awsEnvHandle' (see
      -- 'awsEnvSupervisorStep' for why); this handler only ever reads its
      -- current published value, so it never calls 'newEnv'/'discover'
      -- itself and can never be the thread a delayed background
      -- credential-refresh failure targets.
      mEnv <- liftIO $ readTVarIO awsEnvHandle.awsEnvCurrent
      case mEnv of
        Nothing -> do
          mDiagnostic <- liftIO $ readTVarIO awsEnvHandle.awsEnvLastDiagnostic
          $(logWarn)
            $ "bug report upload: AWS credentials unavailable for game "
            <> toPathPiece gameId
            <> foldMap ((": " <>) . tshow) mDiagnostic
          sendStatusJSON Status.status502 $ BugUploadError "Failed to upload bug report"
        Just env -> do
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
