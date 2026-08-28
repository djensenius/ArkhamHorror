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
) where

import Amazonka (Error (..), SerializeError (..), ServiceError (..), ToBody (toBody), discover, newEnv, runResourceT, send)
import Amazonka.Auth (AuthError (..))
import Amazonka.S3
import Api.Arkham.Export
import Api.Handler.Arkham.Games.Shared (withGameAccess)
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
    HeadObjectFailed
  deriving stock (Eq, Show)

-- | Final result of the bug-report upload policy.
data BugUploadOutcome
  = BugUploadSucceeded
  | BugUploadFailed BugUploadFailure
  deriving stock (Eq, Show)

data BugUploadFailure
  = HeadCheckFailed
  | PutObjectFailed
  deriving stock (Eq, Show)

{- | Sequencing/policy seam for the protected upload.

An existing object never triggers a PUT. A confirmed-absent object
triggers exactly one PUT. Any HeadObject failure short-circuits before PUT
is ever attempted, and a failing PUT is reported as failure. This is kept
free of any AWS/IO dependency (both actions are supplied by the caller) so
tests can inject deterministic stub actions and assert PUT is (or is not)
invoked, without encoding exceptions as strings.
-}
runBugUploadPolicy :: Monad m => m HeadObjectOutcome -> m Bool -> m BugUploadOutcome
runBugUploadPolicy headAction putAction =
  headAction >>= \case
    ObjectPresent -> pure BugUploadSucceeded
    HeadObjectFailed -> pure $ BugUploadFailed HeadCheckFailed
    ObjectAbsent -> do
      putSucceeded <- putAction
      pure $ if putSucceeded then BugUploadSucceeded else BugUploadFailed PutObjectFailed

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
classifyHeadObjectError _ = HeadObjectFailed

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

{- | Which AWS request a captured 'AwsErrorDiagnostic' came from, so the
eventual log line can distinguish a HeadObject failure from a PutObject
failure. This (along with the diagnostic itself) is the only thing carried
across the 'runResourceT IO' boundary back into 'Handler': it is built
from 'classifyErrorDiagnostic', so it inherits the same guarantee of
containing no raw exception, header, body, or free-form server text.
-}
data BugUploadDiagnostic
  = HeadObjectDiagnostic AwsErrorDiagnostic
  | PutObjectDiagnostic AwsErrorDiagnostic
  deriving stock (Eq, Show)

-- | Render a captured diagnostic for the application logger.
describeBugUploadDiagnostic :: ArkhamGameId -> BugUploadDiagnostic -> Text
describeBugUploadDiagnostic gameId = \case
  HeadObjectDiagnostic diag -> "HeadObject failed for game " <> toPathPiece gameId <> ": " <> tshow diag
  PutObjectDiagnostic diag -> "PutObject failed for game " <> toPathPiece gameId <> ": " <> tshow diag

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

      -- Credential/config discovery is a separate exact exception type
      -- ('AuthError') from the per-request 'Error' thrown by 'send', and
      -- must be handled explicitly rather than allowed to escape as an
      -- unhandled exception.
      eEnv <- liftIO $ try @_ @AuthError (newEnv discover)
      case eEnv of
        Left authErr -> do
          $(logWarn) $ "bug report upload: AWS credential discovery failed for game " <> toPathPiece gameId <> ": " <> tshow (classifyAuthErrorDiagnostic authErr)
          sendStatusJSON Status.status502 $ BugUploadError "Failed to upload bug report"
        Right env -> do
          -- The HeadObject/PutObject actions run inside 'runResourceT IO',
          -- which has no application-logger access. Rather than logging
          -- from inside that IO action (which would require an ad-hoc
          -- stdout/file logger bypassing the normal Yesod logging path),
          -- any sanitized diagnostic is stashed here and logged afterwards,
          -- back in 'Handler', via the normal monad-logger path.
          diagnosticRef <- liftIO $ newIORef Nothing
          outcome <-
            liftIO $ runResourceT do
              runBugUploadPolicy
                ( do
                    eHead <- try @_ @Error (send env (newHeadObject bucket key))
                    case eHead of
                      Right _ -> pure ObjectPresent
                      Left err -> do
                        let classified = classifyHeadObjectError err
                        when (classified == HeadObjectFailed)
                          $ liftIO
                          $ writeIORef diagnosticRef (Just $ HeadObjectDiagnostic (classifyErrorDiagnostic err))
                        pure classified
                )
                ( do
                    ePut <- try @_ @Error (send env (newPutObject bucket key (toBody jsonBody)))
                    case ePut of
                      Right _ -> pure True
                      Left err -> do
                        liftIO $ writeIORef diagnosticRef (Just $ PutObjectDiagnostic (classifyErrorDiagnostic err))
                        pure False
                )
          mDiagnostic <- liftIO $ readIORef diagnosticRef
          for_ mDiagnostic $ \diagnostic ->
            $(logWarn) $ "bug report upload: " <> describeBugUploadDiagnostic gameId diagnostic
          case outcome of
            BugUploadSucceeded ->
              pure $ "https://arkham-horror-bugs.s3.amazonaws.com/exports/" <> filename
            BugUploadFailed _ ->
              sendStatusJSON Status.status502 $ BugUploadError "Failed to upload bug report"
