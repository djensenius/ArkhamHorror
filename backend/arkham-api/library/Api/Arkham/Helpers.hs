{-# LANGUAGE OverloadedRecordDot #-}

module Api.Arkham.Helpers where

import Api.Arkham.Lifecycle (drainOwnedCleanup, raceManaged_)
import Arkham.Card
import Arkham.Classes hiding (Entity (..), select)
import Arkham.Classes.HasGame
import Arkham.Classes.HasQueue
import Arkham.Debug
import Arkham.Epic.Types (EpicEnv, HasMaybeEpic (..), SharedEventState)
import Arkham.Game
import Arkham.Id
import Arkham.Message
import Arkham.Queue
import Arkham.Random
import Control.Concurrent (threadDelay)
import Control.Concurrent.MVar
import Control.Concurrent.MVar qualified as MVar
import Control.Exception (IOException, throwIO)
import Control.Lens hiding (from)
import Control.Monad.Catch (MonadCatch, MonadMask, MonadThrow)
import Control.Monad.Random (MonadRandom (..), StdGen)
import Data.Aeson qualified as Aeson
import Data.ByteString.Char8 qualified as BS8
import Data.ByteString.Lazy qualified as BSL
import Data.Map.Strict qualified as Map
import Data.Time.Clock
import Data.Time.Clock.POSIX (getPOSIXTime)
import Data.UUID qualified as UUID
import Database.Esqueleto.Experimental
import Database.Redis (
  Connection,
  PubSubController,
  RedisChannel,
  addChannels,
  hdel,
  hgetall,
  hincrby,
  hset,
  pubSubForever,
  publish,
  runRedis,
 )
import Entity.Arkham.Game
import Entity.Arkham.LogEntry
import GHC.Records
import Import hiding (appLogger, (==.), (>=.))
import PendingCleanupOwner qualified
import UnliftIO.Exception qualified as UE

newtype GameLog = GameLog {gameLogToLogEntries :: [Text]}
  deriving newtype (Monoid, Semigroup)

instance HasField "entries" GameLog [Text] where
  getField = gameLogToLogEntries

newLogEntry :: ArkhamGameId -> Int -> UTCTime -> Text -> ArkhamLogEntry
newLogEntry gameId step now body =
  ArkhamLogEntry
    { arkhamLogEntryBody = body
    , arkhamLogEntryArkhamGameId = gameId
    , arkhamLogEntryStep = step
    , arkhamLogEntryCreatedAt = now
    }

getGameLog :: ArkhamGameId -> Maybe Int -> DB GameLog
getGameLog gameId mStep = fmap (GameLog . fmap unValue) $ select $ do
  entries <- from $ table @ArkhamLogEntry
  where_ $ entries.arkhamGameId ==. val gameId
  for_ mStep \step ->
    where_ $ entries.step >=. val step
  -- Order by step (monotonic per game) so the planner can use
  -- idx_arkham_log_entry_gameid_step directly without a Sort node.
  orderBy [asc entries.step, asc entries.id]
  pure entries.body

toPublicGame :: Entity ArkhamGame -> GameLog -> PublicGame ArkhamGameId
toPublicGame (Entity gId ArkhamGame {..}) gameLog =
  PublicGame gId arkhamGameName (gameLogToLogEntries gameLog) arkhamGameCurrentData

data ApiResponse
  = GameUpdate (PublicGame ArkhamGameId)
  | GameMessage Text
  | GameError Text
  | GameUI Text
  | GameAudio Text
  | GameCard {title :: Text, card :: Aeson.Value}
  | GameCardOnly {player :: PlayerId, title :: Text, card :: Aeson.Value}
  | GameTarot Aeson.Value
  | GameShowDiscard InvestigatorId
  | GameShowUnder InvestigatorId
  | {- | Above-the-table achievement unlocked (flat achievement tag); the
    client renders the toast from its i18n catalog.
    -}
    GameAchievement Text
  | GamePlayabilityInfo {cardId :: CardId, cardCode :: Text, checks :: [(Text, Maybe Text)]}
  | -- Epic Multiplayer: the event's shared state, pushed to a group's own stream
    -- so the shared panel renders from a single source (the group websocket).
    SharedStateUpdate SharedEventState
  | -- Event membership/group roster changed. Payloads are user-specific, so
    -- clients refetch EventDetails rather than receiving a shared digest.
    EventChanged
  deriving stock Generic

instance Aeson.ToJSON ApiResponse where
  toJSON = Aeson.genericToJSON Aeson.defaultOptions
  toEncoding = Aeson.genericToEncoding Aeson.defaultOptions

newtype GameAppT a = GameAppT {unGameAppT :: ReaderT GameApp IO a}
  deriving newtype
    ( MonadReader GameApp
    , Functor
    , Applicative
    , Monad
    , MonadFail
    , MonadIO
    , MonadRandom
    , MonadMask
    , MonadCatch
    , MonadThrow
    , MonadUnliftIO
    )

data GameApp = GameApp
  { appGame :: IORef Game
  , appQueue :: Queue Message
  , appGen :: IORef StdGen
  , appLogger :: ClientMessage -> IO ()
  , appEvent :: Maybe EpicEnv
  {- ^ present only when this game is a group within an Epic Multiplayer event;
  'Nothing' (the default for every ordinary game) means zero behavior change.
  -}
  }

instance HasMaybeEpic GameApp where
  getMaybeEpicEnv = appEvent

instance HasDebugLevel GameAppT where
  getDebugLevel = liftIO getDebugLevel

instance HasGame GameAppT where
  getGame = readIORef =<< asks appGame
  getCache = GameCache \_ build -> build

instance HasStdGen GameApp where
  genL = lens appGen $ \m x -> m {appGen = x}

instance HasGameRef GameApp where
  gameRefL = lens appGame $ \m x -> m {appGame = x}

instance HasQueue Message GameAppT where
  messageQueue = asks appQueue

instance HasGameLogger GameAppT where
  getLogger = do
    logger <- asks appLogger
    pure $ \msg -> liftIO $ logger msg

gameIdToText :: ArkhamGameId -> Text
gameIdToText = UUID.toText . coerce

runGameApp :: MonadIO m => GameApp -> GameAppT a -> m a
runGameApp gameApp = liftIO . flip runReaderT gameApp . unGameAppT

gameChannel :: ArkhamGameId -> RedisChannel
gameChannel gameId = "arkham-" <> encodeUtf8 (tshow gameId)

{- | Epic Multiplayer: per-event broadcast channel + room helpers, mirroring the
per-game ones above but keyed by 'ArkhamEpicEventId' on 'appEventRooms'.
-}
eventChannel :: ArkhamEpicEventId -> RedisChannel
eventChannel eventId = "arkham-epic-" <> encodeUtf8 (tshow eventId)

{- | Join a room: look it up (creating it, and its one Redis subscription, when
this pod isn't serving the channel yet) AND register this WebSocket as a
subscriber, both in one turn of the rooms lock.

There is exactly ONE subscription per channel per pod and the room owns it;
every WebSocket on the room is then fed from that single callback. Each
connection used to open its own Redis connection and subscribe independently,
which meant every published message was delivered once per connection to
every connection (N^2 sends for an N-player table) and cost one Redis
connection per open browser tab.

Look-up and subscribe-to-room have to happen together. 'releaseRoomIfEmpty'
reads "present in the map with zero subscribers" as "nobody is here", so if a
connection could hold a room it had not yet registered on, a departing
connection could release that room out from under it -- leaving the new
arrival attached to a room whose channel is no longer subscribed, silently
receiving nothing for the life of the socket. Joining atomically is what makes
the zero-subscriber reading trustworthy.

'addChannels' only enqueues the SUBSCRIBE onto the controller's queue rather
than waiting for the ack, which keeps a Redis round-trip (or a controller
stalled mid-reconnect) from blocking every other connect and disconnect on the
pod.
-}
joinRoomIn
  :: (MonadIO m, HasApp m, Ord k)
  => (App -> MVar (Map k Room))
  -> (k -> RedisChannel)
  -> k
  -> m (Room, Int, Subscriber)
joinRoomIn roomsOf channelFor key = do
  app <- getApp
  liftIO $ modifyMVar (roomsOf app) \rooms -> do
    (rooms', r) <- ensureRoom app channelFor key rooms
    (subId, sub) <- subscribeToRoom r
    pure (rooms', (r, subId, sub))

-- | Look up or create a room. Assumes the caller holds the rooms 'MVar'.
ensureRoom
  :: Ord k
  => App -> (k -> RedisChannel) -> k -> Map k Room -> IO (Map k Room, Room)
ensureRoom app channelFor key rooms = case Map.lookup key rooms of
  Just r -> pure (rooms, r)
  Nothing -> do
    let chn = channelFor key
    r <- newRoom chn
    -- Real traffic counts as proof of life too, so a busy pod never trips
    -- the stall watchdog even if a heartbeat publish happens to fail.
    let onMessage m = do
          markPubSubAlive (appPubSubHealth app)
          broadcastToRoom r (BSL.fromStrict m)
    unsub <- case appMessageBroker app of
      WebSocketBroker -> pure (pure ())
      RedisBroker _ ctrl -> addChannels ctrl [(chn, onMessage)] []
    atomically $ writeTVar (roomUnsubscribe r) unsub
    pure (Map.insert key r rooms, r)

{- | Drop a room once its last WebSocket subscriber has left: remove it from the
map and tear down its Redis subscription, both under the rooms 'MVar'.

The emptiness check belongs under that lock too. Checking first and deleting
afterwards leaves a window in which a new connection joins the still-present
room and is then left holding one whose channel we immediately unsubscribe --
so it silently never receives another update. Returns the outcome of the
attempt -- see 'RoomCleanupOutcome' -- rather than a bare 'Bool': a failed
unsubscribe is reported (never silently swallowed), the room is
deliberately left tracked rather than erased, and the failure is durably
registered for retry (see 'registerRoomCleanupRetry').
-}
releaseRoomIfEmpty
  :: (MonadIO m, HasApp m, Ord k) => (App -> MVar (Map k Room)) -> k -> m RoomCleanupOutcome
releaseRoomIfEmpty roomsOf key = do
  app <- getApp
  let roomsVar = roomsOf app
  (outcome, attempted) <-
    liftIO $ modifyMVar roomsVar \rooms -> case Map.lookup key rooms of
      Nothing -> pure (rooms, (RoomCleanupAbsent, Nothing))
      Just r -> do
        n <- roomClientCount r
        if n > 0
          then pure (rooms, (RoomCleanupStillOccupied, Nothing))
          else do
            (rooms', out) <- attemptRoomTeardown key rooms
            pure (rooms', (out, Just r))
  -- Only ever registers a retry when THIS attempt itself failed; a
  -- no-op for every other outcome (see 'registerRoomCleanupRetryIfFailed').
  for_ attempted \r ->
    liftIO $ registerRoomCleanupRetryIfFailed (== 0) roomsVar key r outcome
  pure outcome

joinEventRoom
  :: (MonadIO m, HasApp m) => ArkhamEpicEventId -> m (Room, Int, Subscriber)
joinEventRoom = joinRoomIn appEventRooms eventChannel

releaseEventRoomIfEmpty :: (MonadIO m, HasApp m) => ArkhamEpicEventId -> m RoomCleanupOutcome
releaseEventRoomIfEmpty = releaseRoomIfEmpty appEventRooms

lookupEventRoom :: (MonadIO m, HasApp m) => ArkhamEpicEventId -> m (Maybe Room)
lookupEventRoom eid = do
  roomsVar <- getsApp appEventRooms
  Map.lookup eid <$> liftIO (MVar.readMVar roomsVar)

joinGameRoom :: (MonadIO m, HasApp m) => ArkhamGameId -> m (Room, Int, Subscriber)
joinGameRoom = joinRoomIn appGameRooms gameChannel

releaseGameRoomIfEmpty :: (MonadIO m, HasApp m) => ArkhamGameId -> m RoomCleanupOutcome
releaseGameRoomIfEmpty = releaseRoomIfEmpty appGameRooms

{- | The typed result of one attempted room teardown -- shared by the
ordinary "last subscriber left" release above ('releaseRoomIfEmpty') and
the forced "game\/event was deleted" path
('Api.Handler.Arkham.Games.Shared.forceDeleteRoom'), and by every retry
capability registered against a failure via 'registerRoomCleanupRetry'.
Reported to the caller, and (for a failure) durably retried, instead of
ever being silently swallowed.

No bespoke, room-specific scheduling infrastructure is layered on top of
this: a failed unsubscribe is instead handed to the exact same
process-global 'PendingCleanupOwner.globalPendingCleanupOwner' this
codebase already uses for asynchronously-interrupted AWS-supervisor
shutdown retries (see 'Api.Arkham.Lifecycle.drainOwnedCleanup'), and is
genuinely, periodically retried in production by 'roomHeartbeat' -- the
one background thread already alive whenever a Redis broker (the only
broker configuration under which this teardown can ever actually fail;
see 'ensureRoom') is in use -- rather than fabricating a room-specific
retry loop this fix does not need.
-}
data RoomCleanupOutcome
  = -- | No room was tracked under this key at all: already cleaned up,
    -- replaced by a newer room instance since an earlier failure was
    -- registered for retry, or this game\/event never had any live
    -- subscriber to begin with.
    RoomCleanupAbsent
  | -- | Present, but ineligible for THIS particular attempt -- only ever
    -- reported by an ordinary (never a forced) release: the room still
    -- has at least one live subscriber, whether it always did or it
    -- regained one since an earlier failure was first registered for
    -- retry. Nothing is attempted; a future disconnect (or retry, once
    -- it is genuinely empty again) will.
    RoomCleanupStillOccupied
  | -- | A room was present and eligible, and its subscription was torn
    -- down successfully; it has been removed from the rooms map.
    RoomCleanupClean
  | -- | A room was present and eligible, but tearing down its
    -- subscription failed synchronously ('UnliftIO.Exception.tryAny'
    -- only ever catches a genuine, synchronous failure -- any
    -- asynchronous exception, e.g. this thread's own cancellation,
    -- always propagates unchanged). The room is deliberately left IN the
    -- map -- dropping it would discard the only reachable handle to its
    -- still-registered callback, turning an outstanding, retryable leak
    -- into a permanently unretryable one -- and this exact failure is
    -- durably registered for retry.
    RoomCleanupUnsubscribeFailed SomeException
  deriving stock Show

{- | The exact per-room teardown decision, shared by every FIRST attempt
(an ordinary, just-emptied release, or a forced post-deletion teardown,
both of which have already separately confirmed it is eligible right
now) and, via 'retryRoomTeardown', by every later retry of a failed one.
Factored out to operate directly on a rooms 'Map' (never the 'MVar'
itself) so tests can exercise this SAME decision against a constructed
'Room' with a stubbed, deliberately-failing unsubscribe action, no live
server required.
-}
attemptRoomTeardown :: Ord k => k -> Map k Room -> IO (Map k Room, RoomCleanupOutcome)
attemptRoomTeardown key rooms = case Map.lookup key rooms of
  Nothing -> pure (rooms, RoomCleanupAbsent)
  Just r -> do
    result <- UE.tryAny $ join $ readTVarIO (roomUnsubscribe r)
    pure $ case result of
      Right () -> (Map.delete key rooms, RoomCleanupClean)
      Left e -> (rooms, RoomCleanupUnsubscribeFailed e)

{- | Re-run 'attemptRoomTeardown' later, against whatever this key's
CURRENT room is -- never blindly re-running against the room captured at
the moment of the original failure, which could by now have been fully
torn down and replaced by an unrelated, later room created under the same
key (identity is compared via 'roomUnsubscribe'\'s own 'TVar' reference
equality -- exactly the handle 'attemptRoomTeardown' itself tears down),
or -- for an ordinary release only, never a forced deletion -- have
gained a brand-new live subscriber since the original failure was first
observed.

@stillEligible@, given the CURRENT subscriber count read right now (not
the one read at the time of the original failure), decides whether this
retry should even attempt anything: @const True@ for a forced deletion
(occupancy is irrelevant -- see
'Api.Handler.Arkham.Games.Shared.forceDeleteRoom'), or @(== 0)@ for an
ordinary release, so a room that has regained a live subscriber is never
torn down out from under it; a future, fresh disconnect from that
subscriber will attempt (and, if needed, itself register) its own
teardown when it eventually leaves.
-}
retryRoomTeardown
  :: Ord k => (Int -> Bool) -> MVar (Map k Room) -> k -> Room -> IO (Either SomeException ())
retryRoomTeardown stillEligible roomsVar key expectedRoom =
  modifyMVar roomsVar \rooms -> case Map.lookup key rooms of
    Nothing -> pure (rooms, Right ())
    Just r
      | roomUnsubscribe r /= roomUnsubscribe expectedRoom -> pure (rooms, Right ())
      | otherwise -> do
          n <- roomClientCount r
          if not (stillEligible n)
            then pure (rooms, Right ())
            else do
              (rooms', out) <- attemptRoomTeardown key rooms
              pure $ case out of
                RoomCleanupUnsubscribeFailed e -> (rooms', Left e)
                _ -> (rooms', Right ())

{- | Durably hand a just-failed teardown off to the process-global
'PendingCleanupOwner.globalPendingCleanupOwner' so it is genuinely,
repeatedly retried later (see 'roomHeartbeat') instead of ever being
logged once and forgotten. Returns the opaque receipt so a caller
(currently only tests) can also poll this exact capability directly via
'PendingCleanupOwner.attemptCleanupReceipt'.
-}
registerRoomCleanupRetry
  :: Ord k
  => (Int -> Bool) -> MVar (Map k Room) -> k -> Room -> IO PendingCleanupOwner.CleanupReceipt
registerRoomCleanupRetry stillEligible roomsVar key expectedRoom =
  PendingCleanupOwner.transferPendingCleanup
    PendingCleanupOwner.globalPendingCleanupOwner
    (retryRoomTeardown stillEligible roomsVar key expectedRoom)

{- | Called unconditionally after every FIRST teardown attempt: registers
a fresh retry precisely when (and only when) that attempt's own outcome
was a failure, and is a complete no-op for every other outcome. (A retry
itself, via 'retryRoomTeardown', does NOT call this again -- a
still-failing retry is instead requeued automatically by
'PendingCleanupOwner.attemptClaimed'\/'PendingCleanupOwner.drainPendingCleanup'.)
-}
registerRoomCleanupRetryIfFailed
  :: Ord k => (Int -> Bool) -> MVar (Map k Room) -> k -> Room -> RoomCleanupOutcome -> IO ()
registerRoomCleanupRetryIfFailed stillEligible roomsVar key expectedRoom = \case
  RoomCleanupUnsubscribeFailed _ -> void $ registerRoomCleanupRetry stillEligible roomsVar key expectedRoom
  RoomCleanupAbsent -> pure ()
  RoomCleanupStillOccupied -> pure ()
  RoomCleanupClean -> pure ()

{- | A closed, non-sensitive summary of why a room teardown failed,
reported instead of the raw exception: directly 'Show'ing\/'tshow'ing a
'SomeException' (as an earlier version of
'Api.Handler.Arkham.Games.Shared.forceDeleteRoom' did) risks leaking
arbitrary connection details or other exception-message text into
application logs. Extend this with a new, still-non-sensitive case if a
genuinely useful-to-distinguish failure shape appears; never widen it
back to an open, arbitrary 'Text'\/'String'.
-}
data RoomCleanupFailureCategory
  = -- | The underlying failure was a genuine I\/O\/connection-level
    -- exception (e.g. a dropped or refused Redis connection) --
    -- generally transient, and expected to succeed on a later retry.
    RoomCleanupConnectionFailure
  | -- | Anything else: an unexpected, not-otherwise-classified
    -- synchronous failure.
    RoomCleanupOtherFailure
  deriving stock (Show, Eq)

classifyRoomCleanupFailure :: SomeException -> RoomCleanupFailureCategory
classifyRoomCleanupFailure e
  | Just (_ :: IOException) <- fromException e = RoomCleanupConnectionFailure
  | otherwise = RoomCleanupOtherFailure

{- | Like 'joinGameRoom' but never creates a Room and never subscribes. Use
from the publish path so that broadcasting to a game with no listeners
doesn't leak an empty Room into 'appGameRooms' that nothing will ever
clean up.
-}
lookupRoom :: (MonadIO m, HasApp m) => ArkhamGameId -> m (Maybe Room)
lookupRoom gid = do
  roomsVar <- getsApp appGameRooms
  Map.lookup gid <$> liftIO (MVar.readMVar roomsVar)

{- | Cross-server room registry. Two parallel Redis hashes:

  * 'roomsHashKey'      gameId -> total WebSocket subscribers
  * 'roomsSeenHashKey'  gameId -> unix epoch of last activity

Every subscribe/unsubscribe and a per-pod heartbeat refresh the seen
timestamp. On admin read, entries whose seen timestamp is older than
'roomStaleSeconds' (or missing entirely) are treated as crashed-pod
cruft, removed from both hashes, and excluded from the response.
-}
roomsHashKey :: ByteString
roomsHashKey = "arkham:rooms"

roomsSeenHashKey :: ByteString
roomsSeenHashKey = "arkham:rooms:seen"

-- A room without an updated seen timestamp for this many seconds is
-- treated as stale. Sized to be 3x the heartbeat cadence so a single
-- missed write doesn't drop a live game.
roomStaleSeconds :: Int
roomStaleSeconds = 90

-- Cadence for 'roomHeartbeat' (the per-pod "I'm still serving these
-- games" pulse).
roomHeartbeatSeconds :: Int
roomHeartbeatSeconds = 30

roomField :: ArkhamGameId -> ByteString
roomField = encodeUtf8 . gameIdToText

currentEpoch :: IO Int
currentEpoch = floor <$> getPOSIXTime

{- | Best-effort wrapper: tracking room counts is observability, not
correctness, so we never let a Redis hiccup tear down a live session.

Uses 'UE.tryAny' (not 'Control.Exception.try' \@'SomeException'):
every current call site is either directly on, or reachable from, a
thread some other caller can legitimately deliver an asynchronous
cancellation to -- most importantly 'pubSubSupervisor''s @watchdog@,
which is one racer under 'Api.Arkham.Lifecycle.raceManaged_' and so
must actually terminate on the 'Control.Exception.ThreadKilled' that
function's own 'Api.Arkham.Lifecycle.cancelManagedThread' delivers to
it; catching that here and looping again in @watchdog@\/@forever@'s
next iteration would leave 'Api.Arkham.Lifecycle.cancelManagedThread''s
'Control.Concurrent.MVar.readMVar' blocked forever, hanging
'pubSubSupervisor' itself and, transitively, Foundation shutdown. The
same reasoning applies to every other call site
('withRedis', the 'sweepStaleRooms' call inside
'getRedisRoomCounts', and 'attemptRoomTeardown'\/'retryRoomTeardown' via
their own 'UE.tryAny'): each already only ever wants to swallow a
genuine, synchronous Redis\/network failure, never a caller's own
cancellation (e.g. a WebSocket disconnect handler or admin request
being torn down mid-flight). 'UE.tryAny' only ever catches genuinely
synchronous exceptions; any asynchronous exception -- shutdown or
otherwise -- always propagates unchanged.
-}
tryRedis_ :: MonadIO m => IO a -> m ()
tryRedis_ action = void $ liftIO $ UE.tryAny action

-- Run a best-effort Redis action if a Redis broker is configured.
withRedis :: (MonadIO m, HasApp m) => (Connection -> IO a) -> m ()
withRedis action = do
  broker <- getsApp appMessageBroker
  case broker of
    WebSocketBroker -> pure ()
    RedisBroker conn _ -> tryRedis_ (action conn)

{- | Increment the cross-server client count for a game in Redis and
refresh its seen timestamp. No-op without a Redis broker.
-}
incrRoomMember :: (MonadIO m, HasApp m) => ArkhamGameId -> m ()
incrRoomMember gameId = withRedis \conn -> runRedis conn do
  void $ hincrby roomsHashKey (roomField gameId) 1
  now <- liftIO currentEpoch
  void $ hset roomsSeenHashKey ((roomField gameId, BS8.pack (show now)) :| [])

{- | Decrement the cross-server client count. When the new count is at or
below zero, drop the field from both hashes so the admin view doesn't
carry empty rooms; otherwise refresh the seen timestamp.
-}
decrRoomMember :: (MonadIO m, HasApp m) => ArkhamGameId -> m ()
decrRoomMember gameId = withRedis \conn -> runRedis conn do
  result <- hincrby roomsHashKey (roomField gameId) (-1)
  case result of
    Right n | n <= 0 -> do
      void $ hdel roomsHashKey (roomField gameId :| [])
      void $ hdel roomsSeenHashKey (roomField gameId :| [])
    _ -> do
      now <- liftIO currentEpoch
      void $ hset roomsSeenHashKey ((roomField gameId, BS8.pack (show now)) :| [])

{- | Aggregate client counts across servers from Redis, filtering out
entries whose 'seen' timestamp is older than 'roomStaleSeconds'. Stale
entries are also HDEL'd from both hashes so cruft from crashed pods
doesn't accumulate. Returns 'Nothing' when no Redis broker is
configured (callers fall back to local state).
-}
getRedisRoomCounts :: (MonadIO m, HasApp m) => m (Maybe (Map ArkhamGameId Int))
getRedisRoomCounts = do
  broker <- getsApp appMessageBroker
  case broker of
    WebSocketBroker -> pure Nothing
    RedisBroker conn _ -> do
      now <- liftIO currentEpoch
      -- 'UE.tryAny' (not 'Control.Exception.try' \@'SomeException'): this
      -- is reachable from an admin request handler's own Warp
      -- request-worker thread, which its caller can legitimately
      -- cancel (e.g. a disconnected admin client). Catching that
      -- cancellation here and falling through to the @Just Map.empty@
      -- "Redis hiccup" branch below would make a genuine cancellation
      -- masquerade as a successful, merely-empty result instead of
      -- actually terminating the request thread. 'UE.tryAny' only ever
      -- catches genuinely synchronous exceptions (a real Redis\/network
      -- failure); any asynchronous exception propagates unchanged.
      result <- liftIO $ UE.tryAny $ runRedis conn do
        countsR <- hgetall roomsHashKey
        seenR <- hgetall roomsSeenHashKey
        pure (countsR, seenR)
      case result of
        Right (Right counts, Right seen) -> do
          let countMap = Map.fromList (mapMaybe parseIntEntry counts)
              seenMap = Map.fromList (mapMaybe parseIntEntry seen)
              isFresh gid = case Map.lookup gid seenMap of
                Just t -> now - t < roomStaleSeconds
                Nothing -> False
              (fresh, stale) = Map.partitionWithKey (\gid _ -> isFresh gid) countMap
          unless (Map.null stale)
            $ tryRedis_
            $ sweepStaleRooms conn (Map.keys stale)
          pure $ Just fresh
        _ -> pure (Just Map.empty)
 where
  parseIntEntry (k, v) = do
    uuid <- UUID.fromText (decodeUtf8 k)
    n <- readMaybe (BS8.unpack v)
    pure (coerce uuid :: ArkhamGameId, n)

sweepStaleRooms :: Connection -> [ArkhamGameId] -> IO ()
sweepStaleRooms conn gameIds = for_ (nonEmpty $ map roomField gameIds) \fields ->
  void $ runRedis conn do
    void $ hdel roomsHashKey fields
    void $ hdel roomsSeenHashKey fields

{- | Background heartbeat: every 'roomHeartbeatSeconds' refresh the seen
timestamp for every game this pod still has live subscribers for, and
opportunistically retry any room teardown that previously failed to
unsubscribe (see 'registerRoomCleanupRetry'). This keeps active games out
of the staleness sweep even when nothing else (subscribe / unsubscribe)
is writing to Redis, and gives every retained, retryable room -- game or
event, since 'drainOwnedCleanup' drains the ONE process-global owner
regardless of which rooms map originally registered the failure -- a
genuine, bounded, periodic chance to finish tearing down, rather than
being retried only the next time (if ever) that exact game or event
happens to be deleted again. Run once per pod, tracked via
'Api.Arkham.Lifecycle.spawnManagedThread' from 'Application.makeFoundation'
and cancelled\/awaited by 'Application.shutdownApp'.

Deliberately takes the broker and room registry directly rather than a
whole 'App': 'Application.makeFoundation' needs to spawn this (and
durably record its 'Api.Arkham.Lifecycle.ManagedThread' handle as
'appRoomHeartbeatThread') before the 'App' value it would otherwise read
from exists, and this avoids the self-referential construction that
would otherwise require.
-}
roomHeartbeat :: MessageBroker -> MVar (Map ArkhamGameId Room) -> IO ()
roomHeartbeat broker gameRooms = case broker of
  WebSocketBroker -> pure ()
  RedisBroker conn _ -> forever do
    threadDelay (roomHeartbeatSeconds * 1000000)
    -- Retrying leftover room-cleanup capabilities never blocks (it only
    -- ever runs each entry's own already-'UnliftIO.Exception.tryAny'-
    -- guarded retry action -- see 'retryRoomTeardown') and, like every
    -- other action in this loop, only lets a genuine asynchronous
    -- exception (shutdown) propagate; a synchronous failure is reported
    -- back as 'Left' and simply requeued for the next tick, never thrown
    -- here.
    _ <- drainOwnedCleanup
    rooms <- MVar.readMVar gameRooms
    active <- catMaybes <$> traverse keepIfActive (Map.toList rooms)
    unless (null active) do
      now <- currentEpoch
      -- 'UE.tryAny' (not 'Control.Exception.try' \@'SomeException'): this
      -- loop is tracked via 'Api.Arkham.Lifecycle.spawnManagedThread' and
      -- cancelled\/awaited by 'Application.shutdownApp', so a shutdown
      -- 'Control.Exception.ThreadKilled' delivered while inside
      -- 'runRedis' must actually terminate this thread rather than being
      -- caught here and silently absorbed into another 'forever'
      -- iteration -- which would leave the managed thread's own
      -- completion cell never filled, hanging Foundation shutdown
      -- indefinitely. 'UE.tryAny' only ever catches genuinely synchronous
      -- exceptions (a real Redis\/network failure), exactly the
      -- best-effort case this is meant to tolerate; any asynchronous
      -- exception -- shutdown or otherwise -- propagates unchanged.
      void $ UE.tryAny $ runRedis conn do
        for_ active \gid ->
          void $ hset roomsSeenHashKey ((roomField gid, BS8.pack (show now)) :| [])
 where
  keepIfActive (gid, room) = do
    n <- roomClientCount room
    pure $ if n > 0 then Just gid else Nothing

{- | Channel every pod subscribes to and publishes a heartbeat on, purely to
prove the pub/sub path is alive end to end.

This is the one subscription that is never removed: it is registered as an
initial subscription on the 'PubSubController', so 'pubSubForever' restores
it on every reconnect and the controller never sits at zero channels.
-}
pubSubHealthChannel :: RedisChannel
pubSubHealthChannel = "arkham:pubsub:health"

-- How often each pod publishes a pub/sub heartbeat.
pubSubHeartbeatSeconds :: Int
pubSubHeartbeatSeconds = 20

{- | How long the subscriber socket may go without delivering anything before
we call it dead and reconnect. Three missed beats, so a slow Redis or a GC
pause can't trip it. Every pod publishes and every pod is subscribed, so a
healthy pod sees a beat every 'pubSubHeartbeatSeconds' regardless of how
quiet the games themselves are.
-}
pubSubStaleSeconds :: NominalDiffTime
pubSubStaleSeconds = 70

-- Cap on the reconnect backoff between 'pubSubForever' attempts.
pubSubMaxBackoffSeconds :: Int
pubSubMaxBackoffSeconds = 30

-- An attempt that survived this long is treated as healthy, resetting backoff.
pubSubHealthyRunSeconds :: NominalDiffTime
pubSubHealthyRunSeconds = 120

-- | Record that the subscriber socket just delivered something.
markPubSubAlive :: MonadIO m => TVar UTCTime -> m ()
markPubSubAlive healthVar = liftIO do
  now <- getCurrentTime
  atomically $ writeTVar healthVar now

data PubSubStalled = PubSubStalled
  deriving stock Show
  deriving anyclass Exception

{- | Own this pod's single pub/sub subscriber connection.

Two distinct failure modes, and 'pubSubForever' alone survives neither:

* It throws on network death and has to be re-called to resubscribe every
  channel tracked by the controller (hedis documents exactly this). It used
  to be forked bare, so a single Redis blip would have silently ended all
  cross-pod delivery for the remaining life of the pod.

* It does not notice a HALF-OPEN socket. An idle TCP connection dropped by a
  proxy leaves it blocked in @recv@ forever -- still "connected", delivering
  nothing, throwing nothing. That is what strands a quiet game: the WebSocket
  stays healthy (its own ping thread keeps it up) and log lines keep flowing,
  because those are broadcast in-process, while every GameUpdate -- which
  travels via Redis -- is silently lost.

So each pod publishes to 'pubSubHealthChannel' over the ordinary connection
pool, meaning the beat travels the exact route a GameUpdate does, and every
pod is subscribed to it. If nothing at all arrives on the subscriber socket
for 'pubSubStaleSeconds', we tear the connection down and reconnect. The beat
doubles as keepalive traffic, so in the common case the idle drop never
happens in the first place.
-}
pubSubSupervisor :: TVar UTCTime -> Connection -> PubSubController -> IO ()
pubSubSupervisor healthVar conn ctrl = go 1
 where
  go backoff = do
    markPubSubAlive healthVar
    startedAt <- getCurrentTime
    -- 'raceManaged_' (not 'UnliftIO.Async.race_'): this loop is tracked
    -- via 'Api.Arkham.Lifecycle.spawnManagedThread' and
    -- cancelled\/awaited by 'Application.shutdownApp'. 'race_' is built
    -- on 'Control.Concurrent.Async.withAsync' for both branches, whose
    -- own cleanup falls back to 'Control.Concurrent.Async.uninterruptibleCancel'
    -- if the losing branch does not respond to cancellation quickly (e.g.
    -- 'pubSubForever' blocked in a synchronous socket read) -- making a
    -- shutdown cancellation of *this* thread hang until that loser
    -- eventually finishes. 'raceManaged_' uses only ordinary,
    -- always-interruptible cancellation throughout, so a shutdown
    -- 'Control.Exception.ThreadKilled' targeting this thread can never be
    -- turned uninterruptible by racing.
    outcome <- raceManaged_ (pubSubForever conn ctrl (markPubSubAlive healthVar)) watchdog
    endedAt <- getCurrentTime
    putStrLn $ case outcome of
      Left e -> "pubsub subscriber died, reconnecting: " <> show e
      Right () -> "pubsub subscriber stalled, reconnecting"
    threadDelay (backoff * 1000000)
    go
      $ if diffUTCTime endedAt startedAt > pubSubHealthyRunSeconds
        then 1
        else min pubSubMaxBackoffSeconds (backoff * 2)

  watchdog = forever do
    threadDelay (pubSubHeartbeatSeconds * 1000000)
    tryRedis_ $ runRedis conn $ publish pubSubHealthChannel "ping"
    now <- getCurrentTime
    seen <- readTVarIO healthVar
    when (diffUTCTime now seen > pubSubStaleSeconds) $ throwIO PubSubStalled

lockGame :: ArkhamGameId -> DB ()
lockGame gameId = void $ select do
  game <- from $ table @ArkhamGame
  where_ $ game.id ==. val gameId
  locking forUpdate

{- | One round-trip in the hot path: lock the row AND fetch its data.
Replaces the previous lockGame + get404 pair, halving the DB calls
for every caller of atomicallyWithGame on the success path.
(notFound lives in MonadHandler, but DB is rank-2 over MonadIO; on the
rare missing-game path we delegate to get404 to throw the 404, which
costs one extra empty SELECT only when the game doesn't exist.)
-}
atomicallyWithGame :: ArkhamGameId -> (ArkhamGame -> DB a) -> DB a
atomicallyWithGame gameId f = do
  results <- select do
    game <- from $ table @ArkhamGame
    where_ $ game.id ==. val gameId
    locking forUpdate
    pure game
  case results of
    [] -> do
      game <- get404 gameId
      f game
    (Entity _ game : _) -> f game
