{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE NoFieldSelectors #-}
{-# LANGUAGE OverloadedStrings #-}

{- | A game's archive status and restore, against the production tables and
JWT authentication.

The status and unarchive endpoints answer only an admin or a seated player;
anyone else gets the missing-game 404. A failed restore stores a category and
tells clients one fixed sentence, whatever the underlying exception said.

Nothing here reaches object storage: the restore is the synchronous runner
'runRestore' with an injected failing action, and the unarchive cases are the
ones that answer without starting a download.
-}
module Postgres.ArchiveSpec (spec) where

import Amazonka (Error (..))
import Amazonka.Error (serviceError)
import Api.Arkham.Archive
import Api.Arkham.Helpers (ApiResponse (GameUnarchiveFailed), publishFromApp)
import Api.Arkham.Types.MultiplayerVariant (MultiplayerVariant (Solo))
import Api.Handler.Arkham.Archive (checkArchiveSeam, getApiV1ArkhamGameArchiveR, putApiV1ArkhamGameUnarchiveR)
import Arkham.Difficulty (Difficulty (Easy))
import Arkham.Game qualified as Game
import Control.Concurrent.MVar qualified as MVar
import Control.Exception (throwIO)
import Data.Aeson (Value (..), decode, encode, object, toJSON, (.=))
import Data.Aeson.KeyMap qualified as KM
import Data.Map.Strict qualified as Map
import Data.Text qualified as T
import Database.Persist qualified as P
import Database.Persist.Sql (Single (..), SqlBackend, rawExecute, rawSql, runSqlPool, toSqlKey)
import Database.Persist.Types (PersistValue (..))
import Entity.Arkham.Game
import Entity.Arkham.GameArchive
import Entity.Arkham.Player
import Entity.User (UserId)
import Foundation (App (..), Handler, Subscriber (..), anonymousSubscriber, newRoom, subscribeToRoom, unsafeHandler, userIdToToken)
import Network.HTTP.Types (Status, status401, status404)
import Network.Wai (defaultRequest, requestHeaders)
import Postgres.Harness
import Postgres.Production (withRequest)
import Relude
import Settings (AppSettings (..))
import System.IO.Error (userError)
import Test.Hspec
import UnliftIO.Exception qualified as Exception
import UnliftIO.STM qualified as STM
import Yesod.Core.Types (ErrorResponse (NotAuthenticated, NotFound), HandlerContents (..))

data Fixture = Fixture
  { seated :: UserId
  , outsider :: UserId
  , admin :: UserId
  , gameId :: ArkhamGameId
  }

spec :: Harness -> App -> Spec
spec harness app = describe "Game archive access and restore failures with production SQL" do
  it "answers a non-participant 404 on archive status and unarchive" $
    withFixture "archive-denied" \f -> do
      runAs app f.outsider (getApiV1ArkhamGameArchiveR f.gameId) `shouldReturn` Left status404
      runAs bucketApp f.outsider (putApiV1ArkhamGameUnarchiveR f.gameId) `shouldReturn` Left status404
      -- and without a bucket: the same answer, so the configuration is not disclosed either
      runAs app f.outsider (putApiV1ArkhamGameUnarchiveR f.gameId) `shouldReturn` Left status404
      stateOf f.gameId `shouldReturn` Just Archived

  it "answers the seated player and an admin" $
    withFixture "archive-allowed" \f -> do
      for_ [f.seated, f.admin] \uid -> do
        Right status <- runAs app uid (getApiV1ArkhamGameArchiveR f.gameId)
        field "state" status `shouldBe` Just (String "archived")
        field "error" status `shouldBe` Just Null
      -- Unarchive answers without starting a download for a restore already
      -- running or done, which is enough to show both get past the access check.
      setState f.gameId Restoring
      Right running <- runAs bucketApp f.seated (putApiV1ArkhamGameUnarchiveR f.gameId)
      field "alreadyRunning" running `shouldBe` Just (Bool True)
      setState f.gameId Restored
      Right restored <- runAs bucketApp f.admin (putApiV1ArkhamGameUnarchiveR f.gameId)
      field "alreadyRestored" restored `shouldBe` Just (Bool True)

  it "still requires sign-in" $
    withFixture "archive-anonymous" \f ->
      respond app (getApiV1ArkhamGameArchiveR f.gameId) `shouldReturn` Left status401

  for_ failures \(label, restore, summary) ->
    it ("keeps the " <> toString label <> " restore failure's detail out of the row, the responses and the broadcast") $
      withFixture ("archive-failure-" <> label) \f -> do
        room <- newRoom "archive-spec"
        MVar.modifyMVar_ app.appGameRooms (pure . Map.insert f.gameId room)
        info <- anonymousSubscriber
        (_, listener) <- subscribeToRoom info room
        flip Exception.finally (MVar.modifyMVar_ app.appGameRooms (pure . Map.delete f.gameId)) do
          now <- currentTimeMicros
          claimed <-
            maybe (fail "Expected to claim the restore") pure
              =<< db (claimForRestore now (Just f.seated) f.gameId)
          runRestore app restore (publishFromApp app f.gameId) claimed

          stored <- maybe (fail "Expected the archive row") pure =<< db (lookupArchive f.gameId)
          stored.state `shouldBe` Failed
          stored.errorMessage `shouldBe` Just summary

          Right status <- runAs app f.seated (getApiV1ArkhamGameArchiveR f.gameId)
          field "error" status `shouldBe` Just (String archiveRestoreFailedMessage)
          refusal <- maybe (fail "Expected an undo refusal") pure =<< db (checkArchiveSeam 0 f.gameId)
          field "error" refusal `shouldBe` Just (String "restoreFailed")
          (field "archive" refusal >>= field "error") `shouldBe` Just (String archiveRestoreFailedMessage)

          broadcast <- drain listener
          map (decode @Value) broadcast
            `shouldBe` [Just $ object ["tag" .= ("GameUnarchiveFailed" :: Text), "contents" .= fixtureMessage]]
          map (decode @Value) broadcast `shouldBe` [Just $ toJSON $ GameUnarchiveFailed fixtureMessage]

          for_ (encode status : encode refusal : broadcast) \body ->
            (decodeUtf8 body :: Text) `shouldNotSatisfy` T.isInfixOf sentinel
          for_ stored.errorMessage (`shouldNotSatisfy` T.isInfixOf sentinel)
 where
  bucketApp = app {appSettings = app.appSettings {appArchiveBucket = Just "test"}}
  db :: ReaderT SqlBackend IO a -> IO a
  db action = runSqlPool action app.appConnPool
  stateOf gid = fmap (.state) <$> db (lookupArchive gid)
  setState gid st = db $ P.updateWhere [ArkhamGameArchiveArkhamGameId P.==. gid] [ArkhamGameArchiveState P.=. st]

  withFixture :: Text -> (Fixture -> IO a) -> IO a
  withFixture label action = withConnection harness \connection -> do
    seated <- seedUser connection (label <> "-seated")
    outsider <- seedUser connection (label <> "-outsider")
    admin <- seedUser connection (label <> "-admin")
    now <- currentTimeMicros
    gameId <- transaction connection do
      rawExecute "UPDATE users SET admin = true WHERE id = ?" [P.toPersistValue admin]
      gameId <- P.insert $ ArkhamGame label (Game.newCampaign "01" Nothing 1 1 Easy False) 12 Solo now now False
      P.insert_ $ ArkhamPlayer seated gameId "01001"
      P.insert_
        $ ArkhamGameArchive gameId Archived "test" "archives/fixture.json.gz" 10 10 1234 now now Nothing Nothing Nothing Nothing
      pure gameId
    action Fixture {..}

-- | The restore failures to inject: a raw exception and a reader 'Left', each
-- carrying text that must never leave the server.
failures :: [(Text, ArkhamGameArchive -> IO (Either ArchiveFailure Int), Text)]
failures =
  [ ("thrown", \_ -> throwIO $ userError $ toString sentinel, "restore failed: unknown")
  , ("storage", \_ -> throwIO rawAwsError, "restore failed: storage")
  , ("integrity", \_ -> pure $ Left $ ArchiveFailure ArchiveIntegrityFailure $ "could not read archive: " <> sentinel, "restore failed: integrity")
  ]

sentinel :: Text
sentinel = "SENTINEL-raw-request-id SELECT secret"

-- | The text of @contracts/fixtures/game-unarchive-failed.json@.
fixtureMessage :: Text
fixtureMessage = "The archived steps could not be restored."

rawAwsError :: Error
rawAwsError =
  ServiceError
    $ serviceError "S3" status404 [("x-amz-request-id", encodeUtf8 sentinel)]
      (Just "NoSuchKey") (Just $ fromString $ toString sentinel) (Just $ fromString $ toString sentinel)

field :: Text -> Value -> Maybe Value
field key = \case
  Object o -> KM.lookup (fromString $ toString key) o
  _ -> Nothing

drain :: Subscriber -> IO [LByteString]
drain subscriber = STM.atomically $ go []
 where
  go acc = STM.tryReadTBQueue subscriber.subQueue >>= \case
    Nothing -> pure $ reverse acc
    Just msg -> go (msg : acc)

-- | The actual JWT the API hands out, on an actual request.
runAs :: App -> UserId -> Handler a -> IO (Either Status a)
runAs app uid action = do
  token <- unsafeHandler app $ userIdToToken uid
  respond app
    $ withRequest defaultRequest {requestHeaders = [("Authorization", "Token " <> encodeUtf8 token)]} action

-- | 'Postgres.Production.runHandlerResponse', keeping Yesod's 'notFound',
-- which is an 'HCError' rather than built content.
respond :: App -> Handler a -> IO (Either Status a)
respond app action = unsafeHandler app $
  (Right <$> action) `Exception.catch` \case
    HCError NotFound -> pure $ Left status404
    HCError NotAuthenticated -> pure $ Left status401
    response -> Exception.throwIO response

-- Fixture SQL only, as 'Main.seedUser'; @admin@ takes its schema default.
seedUser :: SqlBackend -> Text -> IO UserId
seedUser connection label = transaction connection do
  rows <- rawSql
    "INSERT INTO users (username, email, password_digest) VALUES (?, ?, 'not-a-real-password') RETURNING id"
    [PersistText label, PersistText $ label <> "@postgres-test.invalid"]
  case rows of
    [Single key] -> pure $ toSqlKey key
    _ -> liftIO $ fail "Expected one fixture user"
