{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NoFieldSelectors #-}
{-# LANGUAGE OverloadedStrings #-}

{- | The third-edition table handlers against the production table store and
JWT authentication. A table's export is its whole engine state, hidden
information included, so only its seated players, its host and admins may have
it; anyone else gets the missing-table 404. A filed bug goes through the Arkham
bug endpoint's hardened upload. The table itself, its socket and every action
on it answer anyone else the same way, except that an unstarted table may be
watched and joined by anyone; those routes go through production WAI dispatch.

In the export and file-bug examples the admin lookup is a constant, and the S3
transport is a stub built from the production upload seams, so nothing reaches
AWS or this machine's credentials. The routed examples use the real lookup,
against an admin seeded through the @admin@ column (Sqitch change
@add_admin_to_users@).
-}
module Postgres.ThirdEditionSpec (spec) where

import AH3e.Types.Card (Expansion (CoreSet))
import AH3e.Types.State (GameMode (StandardMode))
import Amazonka (Error (..))
import Amazonka.Error (serviceError)
import Api.Arkham.Helpers (joinRoomIn, releaseRoomIfEmpty)
import Api.Handler.Arkham.Game.Bug (
  BugUploadOutcome (..),
  BugUploadTransport,
  runBugUploadPolicy,
  runBugUploadWithEnvDiscovery,
  runHeadObjectAction,
  uploadBugExportUsing,
 )
import Api.Handler.ThirdEdition (getApiV1ThirdEditionExportWith, postApiV1ThirdEditionBugWith, tableFeedGate)
import Data.Aeson (Value, decode, object, withObject, (.:), (.=))
import Data.Aeson.Types (parseMaybe)
import Data.ByteString.Builder (toLazyByteString)
import Data.Char (isHexDigit)
import Data.Text qualified as T
import Data.Time.Clock (getCurrentTime)
import Data.Map.Strict qualified as Map
import Data.UUID (UUID)
import Data.UUID qualified as UUID
import Data.UUID.V4 (nextRandom)
import Database.Persist.Sql (Single (..), SqlBackend, rawExecute, rawSql, toPersistValue, toSqlKey)
import Database.Persist.Types (PersistValue (..))
import Entity.User (UserId)
import Foundation (App (..), Handler, Subscriber (..), anonymousSubscriber, unsafeHandler, unsubscribeFromRoom, userIdToToken)
import Network.HTTP.Types (Header, Method, Status, status200, status400, status403, status404, status500, status502)
import Network.Wai (defaultRequest, requestHeaders)
import Postgres.Harness
import Postgres.Production (authorizationFor, routeRequest, withRequest)
import Relude
import Test.Hspec
import ThirdEdition.Store (Store (..))
import ThirdEdition.Table (Seat (..), Table (..), TableOptions (..))
import UnliftIO.Exception qualified as Exception
import UnliftIO.STM (tryReadTBQueue)
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
spec harness app = do
  exportSpec harness app
  tableRouteSpec harness app

exportSpec :: Harness -> App -> Spec
exportSpec harness app = describe "Third-edition export and file-bug with production SQL" do
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
    table <- insertTable app host [(1, host, "host"), (2, seated, "seated")] 3
    hostless <- insertTable app host [(1, seated, "seated")] 2
    action Fixture {..}

data Members = Members
  { host :: UserId
  , seated :: UserId
  , outsider :: UserId
  , admin :: UserId
  -- ^ a real admin (@users.admin@), seated nowhere
  , lobby :: UUID
  -- ^ unstarted: the host in seat 1, the seated player in seat 2, seat 3 open
  , game :: UUID
  -- ^ the same two players, started by the host through its route
  }

tableRouteSpec :: Harness -> App -> Spec
tableRouteSpec harness app = describe "Third-edition table routes through WAI dispatch" do
  it "answers an outsider on a started table exactly as a missing table, and changes nothing" $
    withMembers "3e-route-outsider" \m -> do
      missing <- nextRandom
      unchanged <- snapshot m.game
      for_ (join' : actions) \(method, suffix, body) -> do
        refused <- route m.outsider method m.game suffix [] body
        (method, suffix, refused) `shouldBe` (method, suffix, (status404, Just noSuchTable))
        route m.outsider method missing suffix [] body `shouldReturn` refused
        snapshot m.game `shouldReturn` unchanged

  it "lets an outsider watch and join an unstarted table, and nothing else" $
    withMembers "3e-route-lobby" \m -> do
      unchanged <- snapshot m.lobby
      for_ (filter (\(method, _, _) -> method /= "GET") actions) \(method, suffix, body) -> do
        refused <- route m.outsider method m.lobby suffix [] body
        (method, suffix, refused) `shouldBe` (method, suffix, (status404, Just noSuchTable))
        snapshot m.lobby `shouldReturn` unchanged
      (watched, view) <- route m.outsider "GET" m.lobby "" [] ""
      (watched, view >>= tableId) `shouldBe` (status200, Just m.lobby)
      (joined, _) <- route m.outsider "POST" m.lobby "/join" [] "{}"
      joined `shouldBe` status200
      Just (t, _) <- app.appThirdEditionStore.getTable m.lobby
      t.version `shouldBe` 1
      [s.user | s <- t.seats] `shouldBe` [Just m.host, Just m.seated, Just m.outsider]

  it "refuses an outsider's socket on a started table before the upgrade, so no room is joined" $
    withMembers "3e-route-socket" \m -> do
      -- The 404 is the guard itself, answered before any upgrade.
      route m.outsider "GET" m.game "" upgrade "" `shouldReturn` (status404, Just noSuchTable)
      Map.member m.game <$> readMVar app.appThirdEditionRooms `shouldReturn` False
      -- A socket the check lets through reaches the upgrade. A WAI test session
      -- cannot carry out a raw (websocket) response and answers with Yesod's 500
      -- fallback instead: the 500 shows the gate passed, nothing more. That
      -- holds for a member, and for an outsider while the table is unstarted.
      for_ [(m.seated, m.game), (m.outsider, m.lobby)] \(uid, tid) -> do
        (accepted, _) <- route uid "GET" tid "" upgrade ""
        accepted `shouldBe` status500

  it "stops a lobby socket once the game starts unless its user turns out to be a member" $
    withMembers "3e-route-feed" \m -> do
      stranger <- withConnection harness (`seedUser` "3e-route-feed-stranger")
      ready <- insertTable app m.host [(1, m.host, "host"), (2, m.seated, "seated")] 2
      -- one socket's gate per user, made while the table is a lobby
      gates <- forM [stranger, m.outsider, m.seated, m.host, m.admin] \uid -> handlerAs uid (tableFeedGate ready)
      -- the room's queue, holding exactly what publishTable put on the feed
      (room, subId, sub) <- unsafeHandler app do
        info <- anonymousSubscriber
        joinRoomIn info appThirdEditionRooms (const "ah3e-feed-test") ready
      let published uid method suffix body = do
            (status, _) <- route uid method ready suffix [] body
            (method, suffix, status) `shouldBe` (method, suffix, status200)
            atomically (tryReadTBQueue sub.subQueue) >>= maybe (fail "Expected a published message") pure
          verdicts msg = forM gates \gate -> unsafeHandler app (gate msg)
      -- the lobby is anyone's to watch
      left <- published m.seated "POST" "/leave" "{}"
      verdicts left `shouldReturn` [True, True, True, True, True]
      joined <- published m.outsider "POST" "/join" "{}"
      verdicts joined `shouldReturn` [True, True, True, True, True]
      -- the game is not: the outsider took a seat and stays, the seated player
      -- gave theirs up and is cut off with the stranger
      begun <- published m.host "POST" "/start" "{}"
      verdicts begun `shouldReturn` [False, True, False, True, True]
      closed <- published m.host "DELETE" "" ""
      verdicts closed `shouldReturn` [True, True, True, True, True]
      unsafeHandler app do
        unsubscribeFromRoom room subId
        void $ releaseRoomIfEmpty appThirdEditionRooms ready

  it "lets the seated players, the host and an admin through, under each route's own rules" $
    withMembers "3e-route-members" \m -> do
      for_ [m.host, m.seated, m.admin] \uid -> do
        (status, view) <- route uid "GET" m.game "" [] ""
        (status, view >>= tableId) `shouldBe` (status200, Just m.game)
      -- seated players use debug actions and undo them
      (debugged, _) <- route m.seated "POST" m.game "/debug" [] debugBody
      debugged `shouldBe` status200
      (undone, _) <- route m.seated "POST" m.game "/undo" [] ""
      undone `shouldBe` status200
      Just (t, _) <- app.appThirdEditionStore.getTable m.game
      t.version `shouldBe` 3 -- start, debug, undo
      -- the rules for members are unchanged: a 400 or 403, not the 404
      let refusedWith status message = (status, Just $ object ["error" .= (message :: Text)])
      route m.seated "POST" m.game "/answer" [] "{\"player\":1,\"choice\":0}"
        `shouldReturn` refusedWith status400 "That isn't your seat"
      route m.admin "POST" m.game "/debug" [] debugBody
        `shouldReturn` refusedWith status400 "Only seated players can use debug actions"
      route m.admin "POST" m.game "/start" [] "{}"
        `shouldReturn` refusedWith status400 "Only the host can start the game"
      route m.seated "POST" m.game "/leave" [] "{}"
        `shouldReturn` refusedWith status400 "The game has already started"
      for_ [m.seated, m.admin] \uid ->
        route uid "DELETE" m.lobby "" [] "" `shouldReturn` refusedWith status403 "Only the host can close the table"
      -- a table that is not there is a 404 for members too, not a 400
      missing <- nextRandom
      route m.host "POST" missing "/leave" [] "{}" `shouldReturn` (status404, Just noSuchTable)
      -- an admin may leave (holding no seat, nothing moves but the version), and the host closes the table
      for_ [m.admin, m.seated] \uid -> do
        (left, _) <- route uid "POST" m.lobby "/leave" [] "{}"
        left `shouldBe` status200
      Just (lobby, _) <- app.appThirdEditionStore.getTable m.lobby
      (lobby.version, [s.user | s <- lobby.seats]) `shouldBe` (2, [Just m.host, Nothing, Nothing])
      (closed, _) <- route m.host "DELETE" m.lobby "" [] ""
      closed `shouldBe` status200
      app.appThirdEditionStore.getTable m.lobby `shouldReturn` Nothing
 where
  withMembers :: Text -> (Members -> IO a) -> IO a
  withMembers label action = withConnection harness \connection -> do
    host <- seedUser connection (label <> "-host")
    seated <- seedUser connection (label <> "-seated")
    outsider <- seedUser connection (label <> "-outsider")
    admin <- seedUser connection (label <> "-admin")
    transaction connection $ rawExecute "UPDATE users SET admin = true WHERE id = ?" [toPersistValue admin]
    let players = [(1, host, "host"), (2, seated, "seated")]
    lobby <- insertTable app host players 3
    game <- insertTable app host players 2
    (started, _) <- route host "POST" game "/start" [] "{}"
    started `shouldBe` status200
    action Members {..}
  snapshot tid = app.appThirdEditionStore.getTable tid
  handlerAs :: UserId -> Handler a -> IO a
  handlerAs uid action = do
    authorization <- authorizationFor app uid
    unsafeHandler app $ withRequest defaultRequest {requestHeaders = [authorization]} action
  route :: UserId -> Method -> UUID -> ByteString -> [Header] -> LByteString -> IO (Status, Maybe Value)
  route uid method tid suffix headers body = do
    authorization <- authorizationFor app uid
    (status, response) <-
      routeRequest app method ("/api/v1/3ed/tables/" <> UUID.toASCIIBytes tid <> suffix)
        (authorization : ("Content-Type", "application/json") : headers) body
    pure (status, decode response)
  tableId = parseMaybe $ withObject "Table" (.: "id")
  -- every route on a table but join and the export and file-bug routes above
  actions :: [(Method, ByteString, LByteString)]
  actions =
    [ ("GET", "", "")
    , ("POST", "/leave", "{}")
    , ("POST", "/start", "{}")
    , ("POST", "/answer", "{\"player\":1,\"choice\":0}")
    , ("POST", "/debug", debugBody)
    , ("POST", "/undo", "")
    , ("DELETE", "", "")
    ]
  join' = ("POST", "/join", "{}")
  debugBody = "{\"tag\":\"DebugSetSheetDoom\",\"contents\":1}"
  upgrade =
    [ ("Connection", "Upgrade")
    , ("Upgrade", "websocket")
    , ("Sec-WebSocket-Version", "13")
    , ("Sec-WebSocket-Key", "dGhlIHNhbXBsZSBub25jZQ==")
    ]

insertTable :: App -> UserId -> [(Int, UserId, Text)] -> Int -> IO UUID
insertTable app host occupied size = do
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
