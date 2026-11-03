{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE NoFieldSelectors #-}
{-# LANGUAGE OverloadedStrings #-}

{- | The third-edition export and file-bug handlers against the production
table store and JWT authentication. A table's export is its whole engine state,
hidden information included, so only its seated players, its host and admins may
have it; anyone else gets the missing-table 404. A filed bug goes through the
Arkham bug endpoint's hardened upload.

Only two things are supplied rather than real. The S3 transport is a stub
built from the production upload seams, so nothing reaches AWS or this
machine's credentials. The admin lookup is a constant: User's entity reads an
@admin@ column that Sqitch does not create, so the real lookup cannot run here
(see 'Main.seedUser').
-}
module Postgres.ThirdEditionSpec (spec) where

import AH3e.Types.Card (Expansion (CoreSet))
import AH3e.Types.State (GameMode (StandardMode))
import Amazonka (Error (..))
import Amazonka.Error (serviceError)
import Api.Handler.Arkham.Game.Bug (
  BugUploadOutcome (..),
  BugUploadTransport,
  runBugUploadPolicy,
  runBugUploadWithEnvDiscovery,
  runHeadObjectAction,
  uploadBugExportUsing,
 )
import Api.Handler.ThirdEdition (getApiV1ThirdEditionExportWith, postApiV1ThirdEditionBugWith)
import Data.Aeson (Value, decode, object, withObject, (.:), (.=))
import Data.Aeson.Types (parseMaybe)
import Data.ByteString.Builder (toLazyByteString)
import Data.Char (isHexDigit)
import Data.Text qualified as T
import Data.Time.Clock (getCurrentTime)
import Data.UUID (UUID)
import Data.UUID.V4 (nextRandom)
import Database.Persist.Sql (Single (..), SqlBackend, rawSql, toSqlKey)
import Database.Persist.Types (PersistValue (..))
import Entity.User (UserId)
import Foundation (App (..), Handler, unsafeHandler, userIdToToken)
import Network.HTTP.Types (Status, status403, status404, status500, status502)
import Network.Wai (defaultRequest, requestHeaders)
import Postgres.Harness
import Postgres.Production (withRequest)
import Relude
import Test.Hspec
import ThirdEdition.Store (Store (..))
import ThirdEdition.Table (Seat (..), Table (..), TableOptions (..))
import UnliftIO.Exception qualified as Exception
import Yesod.Core.Types (Content (..), HandlerContents (..), TypedContent (..))

data Fixture = Fixture
  { host :: UserId
  , seated :: UserId
  , outsider :: UserId
  , table :: UUID
  -- ^ the host in seat 1, the seated player in seat 2, seat 3 open
  , hostless :: UUID
  -- ^ the seated player alone: the host has left their seat
  }

spec :: Harness -> App -> Spec
spec harness app = describe "Third-edition export and file-bug with production SQL" do
  it "exports a table to its seated players, its host (seated or not) and an admin" $
    withFixture "3e-export" \f -> do
      for_ [(f.host, f.table), (f.seated, f.table), (f.host, f.hostless), (f.seated, f.hostless)] \(uid, tid) ->
        exportedId (export notAdmin uid tid) `shouldReturn` Right (Just tid)
      exportedId (export admin f.outsider f.table) `shouldReturn` Right (Just f.table)

  it "answers anyone else exactly as it answers a table that does not exist" $
    withFixture "3e-export-denied" \f -> do
      missing <- nextRandom
      export notAdmin f.outsider missing `shouldReturn` Left (status404, Just noSuchTable)
      for_ [f.table, f.hostless] \tid ->
        export notAdmin f.outsider tid `shouldReturn` Left (status404, Just noSuchTable)
      export notAdmin f.host missing `shouldReturn` Left (status404, Just noSuchTable)

  it "refuses a bug from anyone else before anything is uploaded" $
    withFixture "3e-bug-denied" \f -> do
      (transport, uploads) <- recording $ pure BugUploadSucceeded
      fileBug notAdmin transport f.outsider f.table `shouldReturn` Left (status404, Just noSuchTable)
      readIORef uploads `shouldReturn` []

  it "uploads a seated player's export under the upstream key and returns its URL" $
    withFixture "3e-bug-seated" \f -> do
      (transport, uploads) <- recording $ pure BugUploadSucceeded
      Right exported <- export notAdmin f.seated f.table
      Right response <- fileBug notAdmin transport f.seated f.table
      [(key, body)] <- readIORef uploads
      response `shouldBe` object ["url" .= ("https://arkham-horror-bugs.s3.amazonaws.com/" <> key)]
      contentKey "exports/ah3e-" key `shouldBe` True
      decode body `shouldBe` Just exported
      -- the Arkham endpoint's key is unchanged by sharing the upload
      (arkham, arkhamUploads) <- recording $ pure BugUploadSucceeded
      Right url <- runResponse app $ uploadBugExportUsing arkham "game fixture" "" "{}"
      [(arkhamKey, _)] <- readIORef arkhamUploads
      contentKey "exports/" arkhamKey `shouldBe` True
      T.isPrefixOf "exports/ah3e-" arkhamKey `shouldBe` False
      url `shouldBe` "https://arkham-horror-bugs.s3.amazonaws.com/" <> arkhamKey

  it "answers a failed HeadObject or credential discovery with the sanitized 502 and never PUTs" $
    withFixture "3e-bug-failed" \f -> do
      puts <- newIORef (0 :: Int)
      let recordPut = modifyIORef' puts (+ 1) $> Right ()
          headFails status = \_ _ ->
            runBugUploadWithEnvDiscovery (pure $ Right @Text ()) \() ->
              runBugUploadPolicy (runHeadObjectAction (pure $ Left @_ @() $ rawAwsError status)) recordPut
          discoveryFails = \_ _ ->
            runBugUploadWithEnvDiscovery (pure $ Left @Text @() rawDiscoveryError) \() ->
              runBugUploadPolicy (fail "HeadObject after failed discovery") recordPut
      for_ [headFails status403, headFails status500, discoveryFails] \transport -> do
        response <- fileBug notAdmin transport f.seated f.table
        response `shouldBe` Left (status502, Just $ object ["message" .= ("Failed to upload bug report" :: Text)])
      readIORef puts `shouldReturn` 0
 where
  notAdmin _ = pure False
  admin _ = pure True
  export isAdmin uid tid = runAs uid $ getApiV1ThirdEditionExportWith isAdmin tid
  fileBug isAdmin transport uid tid = runAs uid $ postApiV1ThirdEditionBugWith isAdmin transport tid
  -- the actual JWT the API hands out, on an actual request
  runAs :: UserId -> Handler a -> IO (Either (Status, Maybe Value) a)
  runAs uid action = do
    token <- unsafeHandler app $ userIdToToken uid
    runResponse app $
      withRequest defaultRequest {requestHeaders = [("Authorization", "Token " <> encodeUtf8 token)]} action
  withFixture :: Text -> (Fixture -> IO a) -> IO a
  withFixture label action = withConnection harness \connection -> do
    host <- seedUser connection (label <> "-host")
    seated <- seedUser connection (label <> "-seated")
    outsider <- seedUser connection (label <> "-outsider")
    table <- insertTable host [(1, host, "host"), (2, seated, "seated")] 3
    hostless <- insertTable host [(1, seated, "seated")] 2
    action Fixture {..}
  insertTable host occupied size = do
    tid <- nextRandom
    now <- getCurrentTime
    let seat n = case find (\(m, _, _) -> m == n) occupied of
          Just (_, uid, name) -> Seat n (Just uid) (Just name)
          Nothing -> Seat n Nothing Nothing
    app.appThirdEditionStore.insertTable
      (Table tid "Fixture table" host "host" (map seat [1 .. size]) (TableOptions [CoreSet] StandardMode Nothing) Nothing 0 now)
      []
    pure tid

-- | 'Postgres.Production.runHandlerResponse', keeping an error response's
-- JSON body so the envelope itself is checked rather than only its status.
runResponse :: App -> Handler a -> IO (Either (Status, Maybe Value) a)
runResponse app action = unsafeHandler app $
  (Right <$> action) `Exception.catch` \case
    HCContent status (TypedContent _ (ContentBuilder body _)) -> pure $ Left (status, decode $ toLazyByteString body)
    response -> Exception.throwIO response

exportedId :: IO (Either e Value) -> IO (Either e (Maybe UUID))
exportedId = fmap $ fmap $ parseMaybe $ withObject "TableExport" (.: "id")

noSuchTable :: Value
noSuchTable = object ["error" .= ("No such table" :: Text)]

-- | A transport that records each upload and answers with @outcome@.
recording :: IO BugUploadOutcome -> IO (BugUploadTransport, IORef [(Text, LByteString)])
recording outcome = do
  uploads <- newIORef []
  pure (\key body -> modifyIORef' uploads (<> [(key, body)]) *> outcome, uploads)

-- | @<prefix><sha256 hex>.json@: content addressed, as upstream names exports.
contentKey :: Text -> Text -> Bool
contentKey prefix key = case T.stripSuffix ".json" =<< T.stripPrefix prefix key of
  Just digest -> T.length digest == 64 && T.all isHexDigit digest
  Nothing -> False

-- | An S3 error carrying text that must never reach the client: a message,
-- an error code, a request id and a header.
rawAwsError :: Status -> Error
rawAwsError status =
  ServiceError $
    serviceError "S3" status [("x-amz-request-id", "raw-aws-header")]
      (Just "RawAwsCode") (Just "raw-aws-message") (Just "raw-aws-request-id")

rawDiscoveryError :: Text
rawDiscoveryError = "raw-aws-discovery: /home/someone/.aws/credentials"

-- Fixture SQL only, as 'Main.seedUser'.
seedUser :: SqlBackend -> Text -> IO UserId
seedUser connection label = transaction connection do
  rows <- rawSql
    "INSERT INTO users (username, email, password_digest) VALUES (?, ?, 'not-a-real-password') RETURNING id"
    [PersistText label, PersistText $ label <> "@postgres-test.invalid"]
  case rows of
    [Single key] -> pure $ toSqlKey key
    _ -> liftIO $ fail "Expected one fixture user"
