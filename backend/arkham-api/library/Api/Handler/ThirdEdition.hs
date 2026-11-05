{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE NoFieldSelectors #-}

{- | Third edition tables: a lobby of seats that turns into a game when its host
starts it. Every change is published to the table's room, so each seated
player (and anyone watching) sees it; the websocket itself is a read-only feed
and moves arrive over REST.

Everything third-edition specific in the API lives here and under
"ThirdEdition", reached from @/api/v1/3ed@.
-}
module Api.Handler.ThirdEdition (
  getApiV1ThirdEditionCatalogR,
  getApiV1ThirdEditionTablesR,
  postApiV1ThirdEditionTablesR,
  getApiV1ThirdEditionTableR,
  deleteApiV1ThirdEditionTableR,
  postApiV1ThirdEditionJoinR,
  postApiV1ThirdEditionLeaveR,
  postApiV1ThirdEditionStartR,
  postApiV1ThirdEditionAnswerR,
  postApiV1ThirdEditionDebugR,
  postApiV1ThirdEditionUndoR,
  getApiV1ThirdEditionExportR,
  postApiV1ThirdEditionBugR,
  postApiV1ThirdEditionImportR,

  -- * Exposed for tests
  tableFeedGate,
  withTableAccess,
  getApiV1ThirdEditionExportWith,
  postApiV1ThirdEditionBugWith,
) where

import AH3e.Engine (GameOptions (..), answer, applyDebug, newGame, runEngine, withSeedFrom)
import AH3e.Game (Game)
import AH3e.Types.Card (Expansion (CoreSet))
import AH3e.Types.Ids (PlayerId (..), ScenarioCode)
import AH3e.Types.State (DebugAction, GameMode)
import AH3e.View (catalogView, gameView)
import Api.Arkham.Helpers (joinRoomIn, releaseRoomIfEmpty)
import Api.Handler.Arkham.Game.Bug (BugUploadTransport, awsBugUpload, uploadBugExportUsing)
import Api.Handler.Arkham.Games.Shared (publishOrWarn, streamRoomWhile, websocketConnectionOptions)
import Control.Monad.Random (getRandomR)
import Data.Aeson qualified as Aeson
import Data.Aeson.Types (parseMaybe)
import Data.Map.Strict qualified as Map
import Data.Time (getCurrentTime)
import Data.UUID (UUID)
import Data.UUID qualified as UUID
import Data.UUID.V4 (nextRandom)
import Database.Redis (RedisChannel)
import Import
import Network.HTTP.Types (Status, status400, status403, status404)
import ThirdEdition.Store (HistoryChange (..), Store (..), UpdateFailure (..))
import ThirdEdition.Table
import Yesod.WebSockets (webSocketsOptions)

tableChannel :: UUID -> RedisChannel
tableChannel tid = "ah3e-" <> UUID.toASCIIBytes tid

getStore :: Handler Store
getStore = getsApp appThirdEditionStore

failWith :: Status -> Text -> Handler a
failWith status msg = sendResponseStatus status (object ["error" .= msg])

getApiV1ThirdEditionCatalogR :: Handler Value
getApiV1ThirdEditionCatalogR = pure (object catalogView)

getApiV1ThirdEditionTablesR :: Handler Value
getApiV1ThirdEditionTablesR = do
  userId <- getRequestUserId
  store <- getStore
  (open, mine) <- liftIO (store.listTables userId)
  let newest = sortOn (Down . (.createdAt))
  pure $ object ["open" .= map tableSummary (newest open), "mine" .= map tableSummary (newest mine)]

data NewTable = NewTable
  { name :: Maybe Text
  , seats :: Int
  , expansions :: [Expansion]
  , mode :: GameMode
  , scenario :: Maybe ScenarioCode
  }
  deriving stock Generic
  deriving anyclass FromJSON

postApiV1ThirdEditionTablesR :: Handler Value
postApiV1ThirdEditionTablesR = do
  Entity userId user <- getRequestUser
  body <- (requireCheckJsonBody :: Handler NewTable)
  unless (body.seats >= 1 && body.seats <= 6) $ failWith status400 "A table seats one to six players"
  tid <- liftIO nextRandom
  now <- liftIO getCurrentTime
  let table =
        Table
          { id = tid
          , name = fromMaybe (userUsername user <> "'s table") (mfilter (/= "") body.name)
          , host = userId
          , hostName = userUsername user
          , seats =
              [ Seat
                  n
                  (if n == 1 then Just userId else Nothing)
                  (if n == 1 then Just (userUsername user) else Nothing)
              | n <- [1 .. body.seats]
              ]
          , -- the core set is always in play
            options = TableOptions (ordNub (CoreSet : body.expansions)) body.mode body.scenario
          , game = Nothing
          , version = 0
          , createdAt = now
          }
  store <- getStore
  liftIO (store.insertTable table [])
  pure (tableView Nothing table 0)

{- | The table as it stands, or its live feed when the request is a websocket
upgrade. A client should open the feed first and then fetch, keeping whichever
copy has the higher @version@.

Open to anyone until the game starts, and then to its members only
('openOrMemberOf'). The check comes before the upgrade, so a refused socket
never joins the room. A socket opened before the start is checked again when
the game reaches it ('tableFeedGate').
-}
getApiV1ThirdEditionTableR :: UUID -> Handler Value
getApiV1ThirdEditionTableR tid = do
  userId <- getRequestUserId
  mayWatch <- openOrMemberOf userId
  store <- getStore
  liftIO (store.getTable tid) >>= \case
    Just (t, depth) | mayWatch t -> do
      wsOptions <- websocketConnectionOptions
      joining <- anonymousSubscriber
      admit <- tableFeedGate tid
      webSocketsOptions wsOptions
        $ streamRoomWhile
          admit
          (joinRoomIn joining appThirdEditionRooms tableChannel tid)
          (void $ releaseRoomIfEmpty appThirdEditionRooms tid)
      pure (tableView Nothing t depth)
    _ -> noSuchTable

{- | Only the host closes a table. Other members are told so; anyone else gets
the missing-table 404.
-}
deleteApiV1ThirdEditionTableR :: UUID -> Handler ()
deleteApiV1ThirdEditionTableR tid = do
  userId <- getRequestUserId
  member <- memberOf userId
  store <- getStore
  liftIO (store.getTable tid) >>= \case
    Just (t, _) | member t -> do
      unless (t.host == userId) $ failWith status403 "Only the host can close the table"
      liftIO (store.deleteTable tid)
      publishTable tid (object ["tag" .= ("TableClosed" :: Text)])
    _ -> noSuchTable

newtype SeatRequest = SeatRequest {seat :: Maybe Int}
  deriving stock Generic
  deriving anyclass FromJSON

postApiV1ThirdEditionJoinR :: UUID -> Handler Value
postApiV1ThirdEditionJoinR tid = do
  Entity userId user <- getRequestUser
  req <- (requireCheckJsonBody :: Handler SeatRequest)
  mayJoin <- openOrMemberOf userId
  mutate "join" tid mayJoin \t _ -> do
    when (started t) $ Left "The game has already started"
    (,KeepHistory) <$> takeSeat userId (userUsername user) req.seat t

postApiV1ThirdEditionLeaveR :: UUID -> Handler Value
postApiV1ThirdEditionLeaveR tid = do
  userId <- getRequestUserId
  req <- (requireCheckJsonBody :: Handler SeatRequest)
  member <- memberOf userId
  mutate "leave" tid member \t _ -> do
    when (started t) $ Left "The game has already started"
    pure (leaveSeat userId req.seat t, KeepHistory)

postApiV1ThirdEditionStartR :: UUID -> Handler Value
postApiV1ThirdEditionStartR tid = do
  userId <- getRequestUserId
  seed <- liftIO (getRandomR (0, 2 ^ (30 :: Int)))
  member <- memberOf userId
  mutate "start" tid member \t _ -> do
    unless (t.host == userId) $ Left "Only the host can start the game"
    when (started t) $ Left "The game has already started"
    unless (null (freeSeats t)) $ Left "Every seat needs a player first"
    let opts =
          GameOptions
            { expansions = t.options.expansions
            , mode = t.options.mode
            , scenario = t.options.scenario
            }
    g <- newGame [PlayerId s.player | s <- t.seats] seed opts
    g' <- first tshow (runEngine g)
    pure (withGame g' t, KeepHistory)

{- | @version@ is the table the player was looking at. Choices are indexes, so
an answer to a question that has since moved on is refused, not misapplied.
-}
data AnswerRequest = AnswerRequest {player :: Int, choice :: Int, version :: Maybe Int}
  deriving stock Generic
  deriving anyclass FromJSON

-- | Only the player holding a seat answers for it.
postApiV1ThirdEditionAnswerR :: UUID -> Handler Value
postApiV1ThirdEditionAnswerR tid = do
  userId <- getRequestUserId
  req <- (requireCheckJsonBody :: Handler AnswerRequest)
  member <- memberOf userId
  mutate "answer" tid member \t _ -> do
    unless (holdsSeat userId req.player t) $ Left "That isn't your seat"
    unless (maybe True (== t.version) req.version)
      $ Left "The table changed before your answer arrived; choose again"
    step t (answer (PlayerId req.player) req.choice)

-- | Debug actions and undo are open to anyone seated.
postApiV1ThirdEditionDebugR :: UUID -> Handler Value
postApiV1ThirdEditionDebugR tid = do
  userId <- getRequestUserId
  action <- (requireCheckJsonBody :: Handler DebugAction)
  member <- memberOf userId
  mutate "debug" tid member \t _ -> do
    unless (isSeated userId t) $ Left "Only seated players can use debug actions"
    step t (applyDebug action)

postApiV1ThirdEditionUndoR :: UUID -> Handler Value
postApiV1ThirdEditionUndoR tid = do
  userId <- getRequestUserId
  member <- memberOf userId
  mutate "undo" tid member \t prev -> do
    unless (isSeated userId t) $ Left "Only seated players can undo"
    earlier <- maybe (Left "Nothing to undo") Right prev
    -- Resume from before the action, but keep the seed it reached: undoing a roll
    -- and answering again rolls different dice rather than the same ones.
    let resumed = maybe earlier (withSeedFrom earlier) t.game
    pure (withGame resumed t, PopHistory)

{- | The whole table, game state and all: what an import takes back and what a
filed bug carries. Not the game /view/ the table endpoint serves, which is what
a player may see rather than what the engine is holding.
-}
getApiV1ThirdEditionExportR :: UUID -> Handler Value
getApiV1ThirdEditionExportR = getApiV1ThirdEditionExportWith isAdminUser

-- | 'getApiV1ThirdEditionExportR' with the admin lookup supplied.
getApiV1ThirdEditionExportWith :: (UserId -> Handler Bool) -> UUID -> Handler Value
getApiV1ThirdEditionExportWith isAdmin tid = Aeson.toJSON <$> exportTable isAdmin tid

-- | File a bug: the export goes to the bug bucket and its URL into the issue.
postApiV1ThirdEditionBugR :: UUID -> Handler Value
postApiV1ThirdEditionBugR = postApiV1ThirdEditionBugWith isAdminUser awsBugUpload

{- | 'postApiV1ThirdEditionBugR' with the admin lookup and S3 transport
supplied. The upload is the Arkham bug endpoint's hardened one: a sanitized 502
rather than a raw AWS error, and no PUT unless HeadObject confirmed the object
absent.
-}
postApiV1ThirdEditionBugWith :: (UserId -> Handler Bool) -> BugUploadTransport -> UUID -> Handler Value
postApiV1ThirdEditionBugWith isAdmin transport tid = do
  table <- exportTable isAdmin tid
  url <- uploadBugExportUsing transport ("third-edition table " <> UUID.toText tid) "ah3e-" (Aeson.encode table)
  pure (object ["url" .= url])

{- | The full engine state, hidden information included, is for the table's
own players, its host and admins. Anyone else gets the same 404 as a table
that does not exist: table ids are listed publicly, so a different answer
would disclose which ones are real.
-}
exportTable :: (UserId -> Handler Bool) -> UUID -> Handler TableExport
exportTable isAdmin tid = do
  userId <- getRequestUserId
  store <- getStore
  liftIO (store.getTable tid) >>= \case
    Nothing -> noSuchTable
    Just (t, _) ->
      withTableAccess userId t (isAdmin userId) noSuchTable
        -- the earlier states ride along, so a filed bug can be rewound rather than
        -- only looked at: third edition has no replay runner
        (TableExport t <$> liftIO (store.getHistory tid))

{- | Run protected table work only for a seated player, the host (who may have
left their seat) or an admin. As with
'Api.Handler.Arkham.Games.Shared.withGameAccess', rejection and protected
work share a result type, so a denial cannot fall through to the protected
work; the admin lookup runs only for someone who is neither.
-}
withTableAccess :: Monad m => UserId -> Table -> m Bool -> m a -> m a -> m a
withTableAccess userId t lookupAdmin reject protected
  | hostOrSeated userId t = protected
  | otherwise = do
      admin <- lookupAdmin
      if admin then protected else reject

hostOrSeated :: UserId -> Table -> Bool
hostOrSeated userId t = t.host == userId || isSeated userId t

{- | Who may watch a started table and act on any table: a seated player, the
host (who may have left their seat) or an admin. Anyone else is answered with
'noSuchTable', as for a table that does not exist: table ids are listed
publicly, so a different answer would disclose which ones are real.

The admin lookup runs first, before the table is read, so a missing table and a
refused one cost the same.
-}
memberOf :: UserId -> Handler (Table -> Bool)
memberOf userId = do
  admin <- isAdminUser userId
  pure \t -> admin || hostOrSeated userId t

{- | 'memberOf', or anyone at all until the game starts: an open table is in the
public lobby already, and a shared link has to reach its Join button.
-}
openOrMemberOf :: UserId -> Handler (Table -> Bool)
openOrMemberOf userId = memberOf userId <&> \member t -> not (started t) || member t

noSuchTable :: Handler a
noSuchTable = failWith status404 "No such table"

{- | What one socket on a table's feed may be sent, for the user who opened it.

A socket can be opened on a lobby by anyone, and the room does not know who
is on it, so starting the game must not stream it to everyone who looked in.
A message any visitor may see -- an update to a table that hasn't started, or
its closing -- passes. Anything else (a started table, or a message this
cannot read) passes only once the user has been found to be a member of the
table as the store has it, and the first refusal ends the socket.

That check runs at most once per socket that is let through: after the start
nobody can take or leave a seat, and the host does not change, so a member
stays one. It also catches a player who left their seat before the start, and
lets an outsider who took one keep watching.
-}
tableFeedGate :: UUID -> Handler (LByteString -> Handler Bool)
tableFeedGate tid = do
  userId <- getRequestUserId
  confirmed <- newIORef False
  pure \msg ->
    readIORef confirmed >>= \case
      True -> pure True
      False
        | openToAnyone msg -> pure True
        | otherwise -> do
            member <- memberOf userId
            store <- getStore
            ok <- maybe False (member . fst) <$> liftIO (store.getTable tid)
            when ok $ writeIORef confirmed True
            pure ok
 where
  openToAnyone msg = fromMaybe False do
    v <- Aeson.decode msg
    flip parseMaybe v $ Aeson.withObject "TableMessage" \o ->
      o Aeson..: "tag" >>= \case
        "TableClosed" -> pure True
        "TableUpdate" -> (== Just False) <$> (o Aeson..: "contents" >>= Aeson.withObject "Table" (Aeson..:? "started"))
        (_ :: Text) -> pure False

isAdminUser :: UserId -> Handler Bool
isAdminUser userId = maybe False (\(Entity _ user) -> user.admin) <$> runDB (getEntity userId)

{- | Load an exported table as a new one. The importer hosts it and holds every
seat, so they can answer for each player on their own, as an imported Arkham
game does.
-}
postApiV1ThirdEditionImportR :: Handler Value
postApiV1ThirdEditionImportR = do
  Entity userId user <- getRequestUser
  TableExport imported history <- (requireCheckJsonBody :: Handler TableExport)
  tid <- liftIO nextRandom
  now <- liftIO getCurrentTime
  let
    table =
      imported
        { id = tid
        , host = userId
        , hostName = userUsername user
        , seats = [s {user = Just userId, username = Just (userUsername user)} | s <- imported.seats]
        , version = 0
        , createdAt = now
        }
  store <- getStore
  liftIO (store.insertTable table history)
  pure (tableView Nothing table (length history))

-- | Run the engine on the table's game, keeping the state it replaces for undo.
step :: Show e => Table -> (Game -> Either e Game) -> Either Text (Table, HistoryChange)
step t f = do
  g <- maybe (Left "The game hasn't started") Right t.game
  g' <- first tshow (f g)
  pure (withGame g' t, PushHistory g)

{- | Apply a change atomically, then send the new table to everyone in its room.
The cause lets a client tell an undo (cards going back) from a move.

@allowed@ ('memberOf' or 'openOrMemberOf') is checked under the table's row
lock, against the table the change applies to. A table it refuses is answered
as a missing one, and nothing is written or broadcast.
-}
mutate
  :: Text
  -> UUID
  -> (Table -> Bool)
  -> (Table -> Maybe Game -> Either Text (Table, HistoryChange))
  -> Handler Value
mutate cause tid allowed f = do
  store <- getStore
  let change t prev
        | allowed t = bimap Refused (first bumpVersion) (f t prev)
        | otherwise = Left NoSuchTable
  liftIO (store.updateTable tid change) >>= \case
    Left NoSuchTable -> noSuchTable
    Left (Refused e) -> failWith status400 e
    Right (t, depth) -> do
      let v = tableView (Just cause) t depth
      publishTable tid (object ["tag" .= ("TableUpdate" :: Text), "contents" .= v])
      pure v

publishTable :: UUID -> Value -> Handler ()
publishTable tid v =
  getsApp appMessageBroker >>= \case
    RedisBroker conn _ -> publishOrWarn conn (tableChannel tid) v
    WebSocketBroker -> do
      rooms <- getsApp appThirdEditionRooms
      room <- Map.lookup tid <$> readMVar rooms
      for_ room \r -> broadcastToRoom r (Aeson.encode v)

seatsJson :: Table -> Value
seatsJson t = Aeson.toJSON [object ["player" .= s.player, "username" .= s.username] | s <- t.seats]

tableSummary :: Table -> Value
tableSummary t =
  object
    [ "id" .= t.id
    , "name" .= t.name
    , "hostName" .= t.hostName
    , "seats" .= seatsJson t
    , "options" .= t.options
    , "started" .= started t
    , "createdAt" .= t.createdAt
    ]

tableView :: Maybe Text -> Table -> Int -> Value
tableView cause t depth =
  object
    [ "id" .= t.id
    , "cause" .= cause
    , "name" .= t.name
    , "hostName" .= t.hostName
    , "seats" .= seatsJson t
    , "options" .= t.options
    , "started" .= started t
    , "version" .= t.version
    , "canUndo" .= (depth > 0)
    , "view" .= fmap gameView t.game
    ]
