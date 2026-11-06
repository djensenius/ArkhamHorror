module Arkham.Api.CustomCardArtUploadSpec (spec) where

import Amazonka (Env, Error (..), SerializeError (..))
import Amazonka.Error (serviceError)
import Amazonka.Types (ErrorCode (..), ErrorMessage (..), RequestId (..))
import Api.Handler.Arkham.CustomCards (
  ArtUploadFailure (..),
  ArtUploadOutcome (..),
  PutObjectFailure (..),
  awsArtUploadWith,
  classifyPutObjectError,
  describeArtUploadError,
  describeArtUploadFailure,
  runArtUploadGuarded,
  runArtUploadWith,
 )
import Arkham.Prelude
import Control.Exception (AsyncException (ThreadKilled), ErrorCall (..))
import Control.Exception qualified as Base
import Data.Text qualified as T
import Network.HTTP.Client qualified as Client
import Network.HTTP.Types.Status (Status, status403, status404, status500)
import Test.Hspec

-- | Text a failed credential discovery carries that must never surface.
discoverySecret :: String
discoverySecret = "aws-art-discovery-secret-must-never-leak-51d0a"

-- | Credential-discovery failures other than Amazonka's 'AuthError': an IO
-- error (an unreadable credentials file) and a pure error (malformed config).
discoveryFailures :: [IO a]
discoveryFailures = [throwIO (userError discoverySecret), throwIO (ErrorCall discoverySecret)]

-- | Text a failing upload after discovery carries that must never surface.
sendSecret :: String
sendSecret = "aws-art-send-secret-must-never-leak-e27c4"

serviceErrorWithStatus :: Status -> Error
serviceErrorWithStatus status = ServiceError $ serviceError "S3" status [] Nothing Nothing Nothing

transportFailure :: Error
transportFailure =
  TransportError
    $ Client.InvalidUrlException "https://arkham-horror-assets.s3.amazonaws.com" "connection failure"

-- | Text a failed PutObject's response carries that must never be logged.
putMarker :: Text
putMarker = "SECRET-MARKER-xyz"

-- | A PutObject 'ServiceError' whose message and request id carry 'putMarker'.
putServiceError :: Status -> Text -> Error
putServiceError status code =
  ServiceError
    $ serviceError
      "S3"
      status
      [("x-amz-request-id", encodeUtf8 putMarker)]
      (Just $ ErrorCode code)
      (Just $ ErrorMessage $ "The request signature we calculated does not match " <> putMarker)
      (Just $ RequestId putMarker)

putSerializeError :: Error
putSerializeError =
  SerializeError
    $ SerializeError' "S3" status500 (Just $ fromStrict $ encodeUtf8 putMarker) (unpack putMarker)

putTransportError :: Error
putTransportError =
  TransportError
    $ Client.InvalidUrlException
      ("https://arkham-horror-assets.s3.amazonaws.com/img/custom/" <> unpack putMarker)
      (unpack putMarker)

-- | What would be logged for an outcome; empty on success.
logged :: ArtUploadOutcome -> Text
logged = \case
  ArtUploadFailedWith failure -> describeArtUploadFailure failure
  ArtStored -> ""

leaks :: String -> ArtUploadOutcome -> Bool
leaks secret outcome =
  T.isInfixOf (pack secret) (tshow outcome) || T.isInfixOf (pack secret) (logged outcome)

-- | Counters for each S3 call a stubbed upload makes.
counting :: IO (IORef Int, IORef Int)
counting = (,) <$> newIORef 0 <*> newIORef 0

spec :: Spec
spec = describe "custom card art upload" do
  it "sanitizes any synchronous credential discovery exception and never sends" do
    for_ discoveryFailures \failingDiscovery -> do
      (heads, puts) <- counting
      outcome <-
        runArtUploadWith
          failingDiscovery
          (\() -> modifyIORef' heads (+ 1))
          (\() -> modifyIORef' puts (+ 1))
      readIORef heads `shouldReturn` 0
      readIORef puts `shouldReturn` 0
      outcome `shouldBe` ArtUploadFailedWith ArtDiscoveryFailed
      logged outcome `shouldBe` "custom card art upload: AWS credential discovery failed"
      leaks discoverySecret outcome `shouldBe` False

  it "answers the production transport's failed credential discovery with the sanitized failure" do
    for_ discoveryFailures \failingDiscovery -> do
      outcome <- awsArtUploadWith failingDiscovery "img/custom/1/fixture.png" "image/png" "bytes"
      outcome `shouldBe` ArtUploadFailedWith ArtDiscoveryFailed
      leaks discoverySecret outcome `shouldBe` False

  it "answers a late, non-Amazonka exception from the production send path with the sanitized failure" do
    -- discovery hands back an Env that only throws once send forces it
    let lateEnv = Base.throw (userError sendSecret) :: Env
    outcome <- awsArtUploadWith (pure lateEnv) "img/custom/1/fixture.png" "image/png" "bytes"
    outcome `shouldBe` ArtUploadFailedWith ArtUploadFailed
    logged outcome `shouldBe` "custom card art upload: S3 upload failed"
    leaks sendSecret outcome `shouldBe` False

  it "sanitizes a non-Amazonka PutObject exception" do
    outcome <-
      runArtUploadWith
        (pure ())
        (\() -> liftIO $ Base.throwIO (serviceErrorWithStatus status404))
        (\() -> liftIO $ Base.throwIO (ErrorCall sendSecret))
    outcome `shouldBe` ArtUploadFailedWith ArtUploadFailed
    leaks sendSecret outcome `shouldBe` False

  it "sanitizes a synchronous upload exception, thrown or lazily returned, inside the guard" do
    for_ [throwIO (ErrorCall sendSecret), pure (Base.throw (userError sendSecret))] \upload -> do
      outcome <- runArtUploadGuarded upload
      outcome `shouldBe` ArtUploadFailedWith ArtUploadFailed
      leaks sendSecret outcome `shouldBe` False

  -- base's throwIO: UnliftIO's would rewrap ThreadKilled as synchronous
  it "lets an asynchronous exception during credential discovery propagate" do
    runArtUploadWith (Base.throwIO ThreadKilled) (\() -> pure ()) (\() -> pure ())
      `shouldThrow` (== ThreadKilled)

  it "lets an asynchronous exception during the upload propagate" do
    runArtUploadWith
      (pure ())
      (\() -> liftIO $ Base.throwIO (serviceErrorWithStatus status404))
      (\() -> liftIO $ Base.throwIO ThreadKilled)
      `shouldThrow` (== ThreadKilled)
    runArtUploadGuarded (Base.throwIO ThreadKilled) `shouldThrow` (== ThreadKilled)

  -- Today's semantics, pinned: an IAM role without ListBucket gets 403 for a
  -- missing key, so any Amazonka error from HeadObject means upload.
  it "uploads exactly once when HeadObject fails with any Amazonka error" do
    for_ [serviceErrorWithStatus status403, serviceErrorWithStatus status404, transportFailure] \err -> do
      (heads, puts) <- counting
      outcome <-
        runArtUploadWith
          (pure ())
          (\() -> liftIO $ modifyIORef' heads (+ 1) >> Base.throwIO err)
          (\() -> modifyIORef' puts (+ 1))
      readIORef heads `shouldReturn` 1
      readIORef puts `shouldReturn` 1
      outcome `shouldBe` ArtStored

  it "skips the upload when HeadObject finds the object" do
    (heads, puts) <- counting
    outcome <-
      runArtUploadWith (pure ()) (\() -> modifyIORef' heads (+ 1)) (\() -> modifyIORef' puts (+ 1))
    readIORef heads `shouldReturn` 1
    readIORef puts `shouldReturn` 0
    outcome `shouldBe` ArtStored

  describe "PutObject failure log" do
    it "logs a service error's status and code, and no message, request id or header" do
      let line = describeArtUploadError (putServiceError status403 "SignatureDoesNotMatch")
      line `shouldBe` "custom card art upload: PutObject failed (status 403, code SignatureDoesNotMatch)"
      T.isInfixOf "403" line `shouldBe` True
      T.isInfixOf "SignatureDoesNotMatch" line `shouldBe` True
      T.isInfixOf putMarker line `shouldBe` False

    it "replaces an error code outside [A-Za-z0-9._-]{1,64} with <filtered>" do
      for_
        [ "Signature DoesNotMatch " <> putMarker
        , "Access/" <> putMarker
        , "Access\n" <> putMarker
        , putMarker <> T.replicate 60 "A"
        ]
        \code -> do
          let line = describeArtUploadError (putServiceError status403 code)
          line `shouldBe` "custom card art upload: PutObject failed (status 403, code <filtered>)"
          T.isInfixOf putMarker line `shouldBe` False

    it "logs a serialize error's status only" do
      let line = describeArtUploadError putSerializeError
      line `shouldBe` "custom card art upload: PutObject failed (status 500, serialize error)"
      T.isInfixOf putMarker line `shouldBe` False

    it "logs a transport error's kind only" do
      let line = describeArtUploadError putTransportError
      line `shouldBe` "custom card art upload: PutObject failed (transport error)"
      T.isInfixOf putMarker line `shouldBe` False

    it "keeps no part of the raw error in the classification" do
      for_ [putServiceError status403 "SignatureDoesNotMatch", putSerializeError, putTransportError] \err ->
        T.isInfixOf putMarker (tshow $ classifyPutObjectError err) `shouldBe` False

    it "classifies an Amazonka error from the upload's PutObject, and logs its status and code" do
      (heads, puts) <- counting
      outcome <-
        runArtUploadWith
          (pure ())
          (\() -> liftIO $ modifyIORef' heads (+ 1) >> Base.throwIO (serviceErrorWithStatus status404))
          (\() -> liftIO $ modifyIORef' puts (+ 1) >> Base.throwIO (putServiceError status403 "SignatureDoesNotMatch"))
      readIORef heads `shouldReturn` 1
      readIORef puts `shouldReturn` 1
      outcome
        `shouldBe` ArtUploadFailedWith (ArtPutObjectFailed (PutObjectServiceFailed 403 "SignatureDoesNotMatch"))
      logged outcome `shouldBe` "custom card art upload: PutObject failed (status 403, code SignatureDoesNotMatch)"
      leaks (unpack putMarker) outcome `shouldBe` False

    it "still answers a HeadObject Amazonka error by uploading, not as a PutObject failure" do
      outcome <-
        runArtUploadWith
          (pure ())
          (\() -> liftIO $ Base.throwIO (putServiceError status403 "SignatureDoesNotMatch"))
          (\() -> pure ())
      outcome `shouldBe` ArtStored
