module Arkham.Api.GameBugUploadSpec (spec) where

import Amazonka (Error (..), SerializeError (..))
import Amazonka.Auth (AuthError (..))
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
  runBugUploadPolicy,
 )
import Arkham.Prelude
import Network.HTTP.Client qualified as Client
import Network.HTTP.Types.Status (Status, status403, status404, status429, status500)
import Test.Hspec

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
      classifyHeadObjectError (serviceErrorWithStatus status403) `shouldBe` HeadObjectFailed

    it "classifies a 429 throttling response as a failure, not absent" do
      classifyHeadObjectError (serviceErrorWithStatus status429) `shouldBe` HeadObjectFailed

    it "classifies a 500 server error as a failure, not absent" do
      classifyHeadObjectError (serviceErrorWithStatus status500) `shouldBe` HeadObjectFailed

    it "classifies a transport-level failure (DNS/TLS/timeout/etc) as a failure, not absent" do
      classifyHeadObjectError transportFailure `shouldBe` HeadObjectFailed

  describe "runBugUploadPolicy" do
    it "skips PUT and succeeds when the object already exists" do
      putCalls <- newIORef (0 :: Int)
      outcome <-
        runBugUploadPolicy
          (pure ObjectPresent)
          (modifyIORef' putCalls (+ 1) >> pure True)
      readIORef putCalls `shouldReturn` 0
      outcome `shouldBe` BugUploadSucceeded

    it "performs exactly one PUT and succeeds when the object is genuinely absent" do
      putCalls <- newIORef (0 :: Int)
      outcome <-
        runBugUploadPolicy
          (pure ObjectAbsent)
          (modifyIORef' putCalls (+ 1) >> pure True)
      readIORef putCalls `shouldReturn` 1
      outcome `shouldBe` BugUploadSucceeded

    it "fails without ever calling PUT when HeadObject fails for a non-404 reason" do
      putCalls <- newIORef (0 :: Int)
      outcome <-
        runBugUploadPolicy
          (pure $ classifyHeadObjectError (serviceErrorWithStatus status403))
          (modifyIORef' putCalls (+ 1) >> pure True)
      readIORef putCalls `shouldReturn` 0
      outcome `shouldBe` BugUploadFailed HeadCheckFailed

    it "fails without ever calling PUT on a transport-level HeadObject failure" do
      putCalls <- newIORef (0 :: Int)
      outcome <-
        runBugUploadPolicy
          (pure $ classifyHeadObjectError transportFailure)
          (modifyIORef' putCalls (+ 1) >> pure True)
      readIORef putCalls `shouldReturn` 0
      outcome `shouldBe` BugUploadFailed HeadCheckFailed

    it "reports failure (never a false success) when PUT itself fails" do
      outcome <- runBugUploadPolicy (pure ObjectAbsent) (pure False)
      outcome `shouldBe` BugUploadFailed PutObjectFailed

  {- | Regression for the follow-up audit: the handler used to log
  'tshow err'/'tshow authErr' directly. 'TransportError'/'RetrievalError'
  wrap an 'HttpException' whose embedded 'Request' leaves the
  @X-Amz-Security-Token@ header (used for temporary/STS session
  credentials) unredacted, and 'SerializeError' carries the raw response
  body. Even 'ServiceError''s own symbolic error code and request id are
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
