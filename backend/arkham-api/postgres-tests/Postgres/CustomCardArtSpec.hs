{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE NoFieldSelectors #-}
{-# LANGUAGE OverloadedStrings #-}

{- | The custom card art upload handler on the production foundation, with the
S3 transport supplied: the production transport with a credential discovery that
fails or hands back an environment that throws once used, or a stub that only
records, so nothing reaches AWS or this machine's credentials. A failure is
answered with the stable 502 envelope and a constant log line, and the text of
the exception behind it reaches neither.
-}
module Postgres.CustomCardArtSpec (spec) where

import Api.Handler.Arkham.CustomCards (ArtUploadOutcome (..), ArtUploadTransport, awsArtUploadWith, storeArtWith)
import Config (makeYesodLogger)
import Control.Exception (ErrorCall (..))
import Data.Aeson (Value, decode, object, (.=))
import Data.ByteString.Builder (toLazyByteString)
import Data.Text qualified as T
import Database.Persist.Sql (toSqlKey)
import Entity.User (UserId)
import Foundation (App (..), Handler, unsafeHandler)
import Network.HTTP.Types (Status, status502)
import Postgres.Harness (Harness)
import Relude
import Settings (AppSettings (..))
import System.Directory (getTemporaryDirectory, removeFile)
import System.IO (hClose, openTempFile)
import System.IO.Error (userError)
import System.Log.FastLogger (defaultBufSize, newFileLoggerSet, rmLoggerSet)
import Test.Hspec
import UnliftIO.Exception qualified as Exception
import Yesod.Core.Types (Content (..), ErrorResponse (..), HandlerContents (..), TypedContent (..))

spec :: Harness -> App -> Spec
spec _harness app0 = describe "Custom card art upload on the production foundation" do
  it "answers any exception from credential discovery with the sanitized 502 and never logs its text" do
    for_ discoveryFailures \failure -> do
      (response, logged) <- withCapturedLog \foundation ->
        runResponse foundation $ storeArtWith (awsArtUploadWith failure) uploader "image/png" "bytes"
      T.isInfixOf discoverySecret (show response) `shouldBe` False
      T.isInfixOf discoverySecret logged `shouldBe` False
      response `shouldBe` Left (status502, Just failedUpload)
      logged `shouldSatisfy` T.isInfixOf "custom card art upload: AWS credential discovery failed"

  it "answers any non-Amazonka exception from the S3 send path with the sanitized 502 and never logs its text" do
    let sendSecret = "raw-aws-art-send-exception: AWS_SECRET_ACCESS_KEY=not-a-real-key" :: Text
        -- discovery succeeds, but the Env it returns only throws once send forces it
        lateEnv = Exception.impureThrow $ userError $ toString sendSecret
    (response, logged) <- withCapturedLog \foundation ->
      runResponse foundation $ storeArtWith (awsArtUploadWith $ pure lateEnv) uploader "image/png" "bytes"
    T.isInfixOf sendSecret (show response) `shouldBe` False
    T.isInfixOf sendSecret logged `shouldBe` False
    response `shouldBe` Left (status502, Just failedUpload)
    logged `shouldSatisfy` T.isInfixOf "custom card art upload: S3 upload failed"

  it "refuses a non-image with a 400 before anything is uploaded" do
    uploads <- newIORef (0 :: Int)
    let transport :: ArtUploadTransport
        transport _ _ _ = modifyIORef' uploads (+ 1) $> ArtStored
    result <-
      unsafeHandler app $
        (Right <$> storeArtWith transport uploader "text/plain" "bytes") `Exception.catch` \case
          HCError err -> pure $ Left err
          response -> Exception.throwIO response
    result `shouldBe` Left (InvalidArgs ["Not an image"])
    readIORef uploads `shouldReturn` 0
 where
  -- the S3 branch, whatever the compiled-in settings say
  app = app0 {appSettings = app0.appSettings {appCustomCardArtDir = Nothing}}
  uploader :: UserId
  uploader = toSqlKey 1
  failedUpload = object ["message" .= ("Failed to upload image" :: Text)]
  -- The app with its logger writing to a file, as 'Postgres.ThirdEditionSpec'
  -- captures everything the action logged.
  withCapturedLog :: (App -> IO a) -> IO (a, Text)
  withCapturedLog action = do
    directory <- lookupEnv "ARKHAM_TEST_LOG_DIR" >>= maybe getTemporaryDirectory pure
    Exception.bracket (openTempFile directory "art-upload.log") (removeFile . fst) \(file, handle) -> do
      hClose handle
      result <- Exception.bracket (newFileLoggerSet defaultBufSize file) rmLoggerSet \loggerSet -> do
        logger <- makeYesodLogger loggerSet
        action app {appLogger = logger}
      logged <- decodeUtf8 <$> readFileBS file
      pure (result, logged)

-- | 'Postgres.ThirdEditionSpec.runResponse': keeps an error response's JSON
-- body so the envelope itself is checked rather than only its status.
runResponse :: App -> Handler a -> IO (Either (Status, Maybe Value) a)
runResponse app action = unsafeHandler app $
  (Right <$> action) `Exception.catch` \case
    HCContent status (TypedContent _ (ContentBuilder body _)) -> pure $ Left (status, decode $ toLazyByteString body)
    response -> Exception.throwIO response

-- | Text a failed credential discovery carries that must reach neither the
-- client nor the log.
discoverySecret :: Text
discoverySecret = "raw-aws-art-discovery-exception: /home/someone/.aws/config"

discoveryFailures :: [IO a]
discoveryFailures =
  [ Exception.throwIO $ userError $ toString discoverySecret
  , Exception.throwIO $ ErrorCall $ toString discoverySecret
  ]
