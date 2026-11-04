module Arkham.Api.ArchiveFailureLogSpec (spec) where

import Amazonka (Error (..), SerializeError (..))
import Amazonka.Auth (AuthError (..))
import Amazonka.Error (serviceError)
import Amazonka.Types (ErrorCode (..), ErrorMessage (..), RequestId (..))
import Api.Arkham.Archive (describeArchiveException)
import Arkham.Prelude
import Control.Exception (ErrorCall (..))
import Data.Text qualified as T
import Database.PostgreSQL.Simple qualified as PG
import Network.HTTP.Client qualified as Client
import Network.HTTP.Client.Internal qualified as Client (Response (..), ResponseClose (..))
import Network.HTTP.Types (Status, http11, status403, status404, status500)
import Test.Hspec

{- | What an archive, restore, sweep or hand-archive failure log says about an
exception (TASK-2.84).

Each fixture carries sentinels where a real failure carries secrets or
response text: an @X-Amz-Security-Token@ and @Authorization@ header, a signed
query string, a response body, an AWS message, an attacker-shaped error code
and request id, a file path. The baseline proves 'displayException' -- what
the log used to print -- really does show each sentinel; the rendering must
show none of them.
-}
spec :: Spec
spec = describe "archive failure log rendering" do
  for_ fixtures \(label, raw, sentinels, expected) -> describe label do
    it "starts from an exception whose text carries every sentinel" do
      for_ sentinels \s -> T.pack (displayException raw) `shouldSatisfy` T.isInfixOf s

    it "renders none of them" do
      let rendered = describeArchiveException raw
      for_ (allSentinels <> sentinels) \s -> rendered `shouldNotSatisfy` T.isInfixOf s

    it ("renders " <> show expected) do
      describeArchiveException raw `shouldBe` expected

  it "keeps the Authorization header out, which http-client already redacts" do
    -- http-client shows the Authorization header as <REDACTED>, so the
    -- baseline above cannot include it. Checked separately here: it is in the
    -- request, it is not in 'displayException', and it is not rendered.
    lookup "Authorization" (Client.requestHeaders signedRequest) `shouldBe` Just (encodeUtf8 authSentinel)
    T.pack (displayException statusCodeFailure) `shouldNotSatisfy` T.isInfixOf authSentinel
    T.pack (displayException statusCodeFailure) `shouldSatisfy` T.isInfixOf "<REDACTED>"
    for_ fixtures \(_, raw, _, _) ->
      describeArchiveException raw `shouldNotSatisfy` T.isInfixOf authSentinel

  describe "AWS error codes and request ids" do
    it "keeps a code and request id shaped like identifiers" do
      describeArchiveException (awsService status404 "NoSuchKey" (Just "4442587FB7D0A2F9.x_y-z"))
        `shouldBe` "aws service error (status 404, code NoSuchKey, request id 4442587FB7D0A2F9.x_y-z)"

    it "says none for a missing request id" do
      describeArchiveException (awsService status500 "InternalError" Nothing)
        `shouldBe` "aws service error (status 500, code InternalError, request id none)"

    it "replaces, rather than strips, anything else" do
      for_ ["", "No Such Key", "NoSuchKey;", "Code\n", "Cödé", T.replicate 65 "A"] \bad ->
        describeArchiveException (awsService status404 bad (Just bad))
          `shouldBe` "aws service error (status 404, code <filtered>, request id <filtered>)"

    it "keeps one of exactly 64 characters" do
      let longest = T.replicate 64 "A"
      describeArchiveException (awsService status404 longest Nothing)
        `shouldBe` "aws service error (status 404, code " <> longest <> ", request id none)"

-- | Label, exception, the sentinels its text carries, and its rendering.
fixtures :: [(String, SomeException, [Text], Text)]
fixtures =
  [
    ( "a transport failure with a signed request and a response body"
    , toException $ TransportError statusCodeFailure
    , [tokenSentinel, signatureSentinel, keySentinel, bodySentinel]
    , "aws transport error (StatusCodeException, status 403)"
    )
  ,
    ( "a bare http-client failure"
    , toException statusCodeFailure
    , [tokenSentinel, signatureSentinel, keySentinel, bodySentinel]
    , "http error (StatusCodeException, status 403)"
    )
  ,
    ( "a connection failure"
    , toException
        $ TransportError
        $ Client.HttpExceptionRequest signedRequest
        $ Client.ConnectionFailure
        $ toException
        $ userError
        $ T.unpack bodySentinel
    , [tokenSentinel, signatureSentinel, keySentinel, bodySentinel]
    , "aws transport error (ConnectionFailure)"
    )
  ,
    ( "an invalid URL"
    , toException
        $ TransportError
        $ Client.InvalidUrlException ("https://bucket.s3.amazonaws.com/?X-Amz-Signature=" <> T.unpack signatureSentinel) (T.unpack bodySentinel)
    , [signatureSentinel, bodySentinel]
    , "aws transport error (invalid-url)"
    )
  ,
    ( "a service error with an attacker-shaped code and request id"
    , toException
        $ ServiceError
        $ serviceError
          "S3"
          status404
          [("x-amz-request-id", encodeUtf8 requestIdSentinel), ("x-amz-security-token", encodeUtf8 tokenSentinel)]
          (Just $ ErrorCode codeSentinel)
          (Just $ ErrorMessage messageSentinel)
          (Just $ RequestId requestIdSentinel)
    , [codeSentinel, messageSentinel, requestIdSentinel, tokenSentinel]
    , "aws service error (status 404, code <filtered>, request id <filtered>)"
    )
  ,
    ( "a serialisation error"
    , toException
        $ SerializeError
        $ SerializeError' "S3" status500 (Just $ fromStrict $ encodeUtf8 bodySentinel) (T.unpack messageSentinel)
    , [bodySentinel, messageSentinel]
    , "aws serialize error (status 500)"
    )
  ,
    ( "a credentials failure"
    , toException $ RetrievalError statusCodeFailure
    , [tokenSentinel, signatureSentinel, keySentinel, bodySentinel]
    , "aws credentials error (RetrievalError)"
    )
  ,
    ( "a credentials file that is not there"
    , toException $ MissingFileError $ "/home/" <> T.unpack pathSentinel <> "/.aws/credentials"
    , [pathSentinel]
    , "aws credentials error (MissingFileError)"
    )
  ,
    ( "an I/O error on a path"
    , toException $ mkIOError doesNotExistErrorType "openBinaryTempFile" Nothing (Just $ "/tmp/" <> T.unpack pathSentinel)
    , [pathSentinel]
    , "io error (does not exist)"
    )
  ,
    ( "a SQL error"
    , toException
        $ PG.SqlError
          { PG.sqlState = "23505"
          , PG.sqlExecStatus = PG.FatalError
          , PG.sqlErrorMsg = encodeUtf8 messageSentinel
          , PG.sqlErrorDetail = encodeUtf8 bodySentinel
          , PG.sqlErrorHint = encodeUtf8 keySentinel
          }
    , [messageSentinel, bodySentinel, keySentinel]
    , "sql error (state 23505)"
    )
  ,
    ( "anything else"
    , toException $ ErrorCall $ T.unpack messageSentinel
    , [messageSentinel]
    , "exception (ErrorCall)"
    )
  ]

{- | A request as amazonka signs it with temporary credentials: the session
token and the signature header, and a presigned-style query string.
-}
signedRequest :: Client.Request
signedRequest =
  (Client.parseRequest_ $ "https://test-bucket.s3.amazonaws.com/archives/" <> T.unpack keySentinel <> ".json.gz?X-Amz-Signature=" <> T.unpack signatureSentinel)
    { Client.requestHeaders =
        [ ("X-Amz-Security-Token", encodeUtf8 tokenSentinel)
        , ("Authorization", encodeUtf8 authSentinel)
        ]
    }

statusCodeFailure :: Client.HttpException
statusCodeFailure =
  Client.HttpExceptionRequest signedRequest
    $ Client.StatusCodeException response (encodeUtf8 bodySentinel)
 where
  response =
    Client.Response
      { Client.responseStatus = status403
      , Client.responseVersion = http11
      , Client.responseHeaders = [("x-amz-request-id", encodeUtf8 requestIdSentinel)]
      , Client.responseBody = ()
      , Client.responseCookieJar = Client.createCookieJar []
      , Client.responseClose' = Client.ResponseClose (pure ())
      , Client.responseOriginalRequest = signedRequest
      , Client.responseEarlyHints = []
      }

awsService :: Status -> Text -> Maybe Text -> SomeException
awsService status code rid =
  toException
    $ ServiceError
    $ serviceError "S3" status [] (Just $ ErrorCode code) (Just $ ErrorMessage messageSentinel) (RequestId <$> rid)

tokenSentinel, authSentinel, signatureSentinel, keySentinel, bodySentinel, messageSentinel, codeSentinel, requestIdSentinel, pathSentinel :: Text
tokenSentinel = "SENTINELsessiontokenFQoGZXIvYXdz"
authSentinel = "SENTINELauthorizationAWS4HMAC"
signatureSentinel = "SENTINELsignature0123abcd"
keySentinel = "SENTINELobjectkey"
bodySentinel = "SENTINELresponsebody"
messageSentinel = "SENTINELawsmessage"
codeSentinel = "SENTINELcode with spaces"
requestIdSentinel = "SENTINELrequestid<script>"
pathSentinel = "SENTINELpath"

allSentinels :: [Text]
allSentinels =
  [ tokenSentinel
  , authSentinel
  , signatureSentinel
  , keySentinel
  , bodySentinel
  , messageSentinel
  , codeSentinel
  , requestIdSentinel
  , pathSentinel
  ]
