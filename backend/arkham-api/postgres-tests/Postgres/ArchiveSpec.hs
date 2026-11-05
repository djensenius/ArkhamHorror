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
import Api.Arkham.Helpers (ApiResponse (GameUnarchiveFailed, GameUnarchived), publishFromApp)
import Api.Arkham.Types.MultiplayerVariant (MultiplayerVariant (Solo))
import Api.Handler.Arkham.Admin.Archives (postApiV1AdminArchiveGameR)
import Api.Handler.Arkham.Archive (checkArchiveSeam, getApiV1ArkhamGameArchiveR, putApiV1ArkhamGameUnarchiveR)
import Arkham.Difficulty (Difficulty (Easy))
import Arkham.Game qualified as Game
import Control.Concurrent.MVar qualified as MVar
import Control.Exception (throwIO)
import Data.Aeson (Value (..), decode, encode, object, toJSON, (.=))
import Data.Aeson.KeyMap qualified as KM
import Data.Map.Strict qualified as Map
import Data.Text qualified as T
import Data.Time.Clock (addUTCTime)
import Database.Persist qualified as P
import Database.Persist.Sql (Single (..), SqlBackend, rawExecute, rawSql, runSqlPool, toSqlKey)
import Database.Persist.Types (PersistValue (..))
import Entity.Arkham.Game
import Entity.Arkham.GameArchive
import Entity.Arkham.Player
import Entity.User (UserId)
import Foundation (App (..), Handler, Subscriber (..), anonymousSubscriber, newRoom, subscribeToRoom, unsafeHandler, userIdToToken)
import Network.HTTP.Types (Status, status401, status404, status500)
import Network.Wai (defaultRequest, requestHeaders)
import Postgres.Harness
import Postgres.Production (runHandlerResponseBody, withRequest)
import Relude
import Settings (AppSettings (..))
import System.Environment (setEnv, unsetEnv)
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
          logged <- newIORef []
          runRestore app (\line -> modifyIORef' logged (line :)) restore (publishFromApp app f.gameId) claimed

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

          -- The server log has the failure, but not the exception's text. The
          -- integrity case is exempt: its detail is the injected 'Left' itself,
          -- not an exception the log describes.
          lines' <- readIORef logged
          length lines' `shouldBe` 1
          when (label /= "integrity") $ for_ lines' (`shouldNotSatisfy` T.isInfixOf sentinel)
          when (label == "storage")
            $ lines' `shouldSatisfy` any (T.isInfixOf "(restore failed: storage): aws service error (status 404, code NoSuchKey, request id <filtered>)")

  {- A failure that arrives after the row is already 'Restored' -- a slow first
  attempt finishing while a restarted one fails, or an exception after the
  restore committed. The steps are back, so the row must stay 'Restored' and
  nobody may be told the restore failed. -}
  it "leaves a restored archive alone when a late restore failure arrives" $
    withFixture "archive-late-failure" \f -> do
      room <- newRoom "archive-spec"
      MVar.modifyMVar_ app.appGameRooms (pure . Map.insert f.gameId room)
      info <- anonymousSubscriber
      (_, listener) <- subscribeToRoom info room
      flip Exception.finally (MVar.modifyMVar_ app.appGameRooms (pure . Map.delete f.gameId)) do
        now <- currentTimeMicros
        claimed <-
          maybe (fail "Expected to claim the restore") pure
            =<< db (claimForRestore now (Just f.seated) f.gameId)
        restoredAt <- addUTCTime 1 <$> currentTimeMicros
        let
          restoreThenThrow _ = do
            db $ P.updateWhere
              [ArkhamGameArchiveArkhamGameId P.==. f.gameId]
              [ ArkhamGameArchiveState P.=. Restored
              , ArkhamGameArchiveRestoredAt P.=. Just restoredAt
              , ArkhamGameArchiveErrorMessage P.=. Nothing
              ]
            throwIO rawAwsError
        logged <- newIORef []
        runRestore app (\line -> modifyIORef' logged (line :)) restoreThenThrow (publishFromApp app f.gameId) claimed

        stored <- maybe (fail "Expected the archive row") pure =<< db (lookupArchive f.gameId)
        stored.state `shouldBe` Restored
        stored.errorMessage `shouldBe` Nothing
        stored.restoredAt `shouldBe` Just restoredAt
        drain listener `shouldReturn` []
        lines' <- readIORef logged
        length lines' `shouldBe` 2
        for_ lines' (`shouldNotSatisfy` T.isInfixOf sentinel)
        -- The ignored failure is still visible to an operator, with only the safe summary.
        lines'
          `shouldSatisfy` any \line ->
            "archive: late restore failure ignored for game " `T.isInfixOf` line
              && "(restore failed: storage): the archive is no longer restoring" `T.isInfixOf` line

  {- The restore worker restarted a restore that looked abandoned, and then the
  original attempt fails while the restart is still running. The restart owns
  the row: the original's failure must not mark it 'Failed' or tell anyone,
  and the restart's own outcome still lands (TASK-2.87). -}
  it "ignores a stale attempt's failure while a restarted attempt owns the row" $
    withRestartedRestore "archive-stale-failure" \f listener original restarted -> do
      logged <- newIORef []
      runRestore app (\line -> modifyIORef' logged (line :)) (\_ -> throwIO rawAwsError) (publishFromApp app f.gameId) original

      stored <- maybe (fail "Expected the archive row") pure =<< db (lookupArchive f.gameId)
      stored.state `shouldBe` Restoring
      stored.errorMessage `shouldBe` Nothing
      stored.requestedAt `shouldBe` restarted.requestedAt
      drain listener `shouldReturn` []
      lines' <- readIORef logged
      length lines' `shouldBe` 2
      for_ lines' (`shouldNotSatisfy` T.isInfixOf sentinel)
      lines'
        `shouldSatisfy` any \line ->
          "archive: late restore failure ignored for game " `T.isInfixOf` line
            && "(restore failed: storage): a newer restore attempt owns the archive" `T.isInfixOf` line

      -- The restart then succeeds, as 'restoreGame' records it.
      restoredAt <- currentTimeMicros
      let
        restore _ = do
          db $ P.updateWhere
            [ArkhamGameArchiveArkhamGameId P.==. f.gameId]
            [ArkhamGameArchiveState P.=. Restored, ArkhamGameArchiveRestoredAt P.=. Just restoredAt]
          pure $ Right 10
      runRestore app (\_ -> pure ()) restore (publishFromApp app f.gameId) restarted
      stateOf f.gameId `shouldReturn` Just Restored
      map (decode @Value) <$> drain listener `shouldReturn` [Just $ toJSON GameUnarchived]

  it "still records a restarted attempt's own failure" $
    withRestartedRestore "archive-restart-failure" \f listener _ restarted -> do
      runRestore app (\_ -> pure ()) (\_ -> throwIO rawAwsError) (publishFromApp app f.gameId) restarted
      stored <- maybe (fail "Expected the archive row") pure =<< db (lookupArchive f.gameId)
      stored.state `shouldBe` Failed
      stored.errorMessage `shouldBe` Just "restore failed: storage"
      map (decode @Value) <$> drain listener
        `shouldReturn` [Just $ toJSON $ GameUnarchiveFailed fixtureMessage]

  {- The hand-archive fails before any S3 call: credentials come from fake
  environment keys (the first link of Amazonka's discovery chain, with the
  credential files and instance metadata pointed away), and the export's temp
  file cannot be opened because TMPDIR names a directory that does not exist --
  one whose path carries the sentinel into the exception text. -}
  it "answers a failed admin hand-archive with a fixed sentence and no raw detail" $
    withFixture "archive-hand-failure" \f -> do
      old <- addUTCTime (-3 * 86400) <$> currentTimeMicros
      db $ P.update f.gameId [ArkhamGameUpdatedAt P.=. old]
      db $ P.deleteWhere [ArkhamGameArchiveArkhamGameId P.==. f.gameId]
      response <- withEnv failingArchiveEnv
        $ runHandlerResponseBody bucketApp (postApiV1AdminArchiveGameR f.gameId)
      case response of
        Right _ -> expectationFailure "Expected the hand-archive to fail"
        Left (status, body) -> do
          status `shouldBe` status500
          (decode body >>= field "error") `shouldBe` Just (String handArchiveFailedMessage)
          (decode body >>= field "archived") `shouldBe` Just (Bool False)
          (decodeUtf8 body :: Text) `shouldNotSatisfy` T.isInfixOf sentinel
      stateOf f.gameId `shouldReturn` Nothing
 where
  bucketApp = app {appSettings = app.appSettings {appArchiveBucket = Just "test"}}
  db :: ReaderT SqlBackend IO a -> IO a
  db action = runSqlPool action app.appConnPool
  stateOf gid = fmap (.state) <$> db (lookupArchive gid)
  setState gid st = db $ P.updateWhere [ArkhamGameArchiveArkhamGameId P.==. gid] [ArkhamGameArchiveState P.=. st]

  {- A restore claimed a day ago and then restarted by the worker's claim: the
  original attempt, as 'claimForRestore' handed it out, and the restarted one,
  as 'claimStaleRestores' did. The game's room is subscribed to. -}
  withRestartedRestore
    :: Text -> (Fixture -> Subscriber -> ArkhamGameArchive -> ArkhamGameArchive -> IO a) -> IO a
  withRestartedRestore label action = withFixture label \f -> do
    room <- newRoom "archive-spec"
    MVar.modifyMVar_ app.appGameRooms (pure . Map.insert f.gameId room)
    info <- anonymousSubscriber
    (_, listener) <- subscribeToRoom info room
    flip Exception.finally (MVar.modifyMVar_ app.appGameRooms (pure . Map.delete f.gameId)) do
      now <- currentTimeMicros
      original <-
        maybe (fail "Expected to claim the restore") pure
          =<< db (claimForRestore (addUTCTime (-86400) now) (Just f.seated) f.gameId)
      restarted <-
        maybe (fail "Expected the worker to claim the stale restore") pure
          . find (\a -> a.arkhamGameId == f.gameId)
          =<< db (claimStaleRestores now)
      restarted.requestedAt `shouldBe` Just now
      original.requestedAt `shouldNotBe` restarted.requestedAt
      action f listener original restarted

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

handArchiveFailedMessage :: Text
handArchiveFailedMessage = "The game could not be archived. The server log has the details."

failingArchiveEnv :: [(String, String)]
failingArchiveEnv =
  [ ("AWS_ACCESS_KEY_ID", "AKIDARCHIVESPEC")
  , ("AWS_SECRET_ACCESS_KEY", "archive-spec-not-a-secret")
  , ("AWS_REGION", "us-east-1")
  , ("AWS_SHARED_CREDENTIALS_FILE", "/nonexistent/archive-spec/credentials")
  , ("AWS_CONFIG_FILE", "/nonexistent/archive-spec/config")
  , ("AWS_EC2_METADATA_DISABLED", "true")
  , ("TMPDIR", "/nonexistent/" <> toString sentinel)
  ]

-- | Set these variables for the action, restoring each afterwards. Process
-- wide, so only for a sequential spec.
withEnv :: [(String, String)] -> IO a -> IO a
withEnv vars action = Exception.bracket save restore \_ -> traverse_ (uncurry setEnv) vars *> action
 where
  save = traverse (\(k, _) -> (k,) <$> lookupEnv k) vars
  restore = traverse_ \(k, v) -> maybe (unsetEnv k) (setEnv k) v

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
