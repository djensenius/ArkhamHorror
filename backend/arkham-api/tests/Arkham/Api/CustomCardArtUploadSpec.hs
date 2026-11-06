module Arkham.Api.CustomCardArtUploadSpec (spec) where

import Amazonka (Env, Error (..))
import Amazonka.Error (serviceError)
import Api.Handler.Arkham.CustomCards (
  ArtUploadFailure (..),
  ArtUploadOutcome (..),
  awsArtUploadWith,
  describeArtUploadFailure,
  runArtUploadGuarded,
  runArtUploadWith,
 )
import Arkham.Prelude
import Control.Exception (AsyncException (ThreadKilled), ErrorCall (..))
import Control.Exception qualified as Base
import Data.Text qualified as T
import Network.HTTP.Client qualified as Client
import Network.HTTP.Types.Status (Status, status403, status404)
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
