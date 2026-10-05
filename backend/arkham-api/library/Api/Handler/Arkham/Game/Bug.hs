{-# LANGUAGE TemplateHaskell #-}

module Api.Handler.Arkham.Game.Bug (
  postApiV1ArkhamGameBugR,

  -- * Exposed for regression tests
  AwsErrorCategory (..),
  AwsErrorDiagnostic (..),
  HeadObjectOutcome (..),
  BugUploadOutcome (..),
  BugUploadFailure (..),
  classifyErrorDiagnostic,
  classifyHeadObjectError,
  runBugUploadPolicy,
  runHeadObjectAction,
  runPutObjectAction,
  describeBugUploadFailure,
) where

import Amazonka (Error (..), SerializeError (..), ServiceError (..), ToBody (toBody), discover, newEnv, runResourceT, send)
import Api.Arkham.Export
import Api.Handler.Arkham.Games.Shared (withGameAccess)
import Amazonka.S3
import Control.Exception (evaluate)
import Crypto.Hash.SHA256 qualified as SHA256
import Data.Aeson (encode)
import Data.ByteString.Base16 qualified as B16
import Import hiding ((==.))
import Network.HTTP.Types.Status (statusCode)
import Network.HTTP.Types.Status qualified as Status
import UnliftIO.Exception (try)

{- | Non-secret, structured classification of a per-request AWS error, safe to
log. Deliberately extracts only the numeric HTTP status, plus a category
derived purely from that status number, rather than showing the exception
itself or any text originating in the response.
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
  SerializeError SerializeError' {status = st} -> AwsSerializeFailure (statusCode st) (categorizeAwsStatus (statusCode st))
  ServiceError e -> let st = statusCode e.status in AwsServiceFailure st (categorizeAwsStatus st)

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
'AwsErrorDiagnostic', and 'AwsErrorCategory', all declared in this
module, down to
a plain 'Int') is compiled with this package's default 'StrictData'
extension (a package-wide default, not specific to either module), so
every field of every nested constructor is itself strict. That single
outer WHNF force therefore cascades through every nested constructor
all the way down to atomic values with no further structure to hide a
thunk in -- the practical result is full
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

-- | Render a bug-upload failure for the application logger. The diagnostic
-- crosses from 'runResourceT IO' back into 'Handler' as an ordinary,
-- already-sanitized field of 'BugUploadFailure' (see 'runBugUploadPolicy'),
-- never via a mutable side channel, so there is nothing here to keep in
-- sync with what actually happened.
describeBugUploadFailure :: ArkhamGameId -> BugUploadFailure -> Text
describeBugUploadFailure gameId = \case
  HeadCheckFailed diag -> "HeadObject failed for game " <> toPathPiece gameId <> ": " <> tshow diag
  PutObjectFailed diag -> "PutObject failed for game " <> toPathPiece gameId <> ": " <> tshow diag

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

      env <- liftIO $ newEnv discover
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
