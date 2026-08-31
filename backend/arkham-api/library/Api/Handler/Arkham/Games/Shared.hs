{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE TemplateHaskell #-}

module Api.Handler.Arkham.Games.Shared where

import Api.Arkham.Epic (
  applyEpicDeltasLocked,
  canonicalEpicGameLockOrder,
  lockEpicEventRow,
  lookupGameEvent,
  mkEpicEnv,
  modifySharedStateLockedWith,
 )
import Api.Arkham.Helpers
import Api.Arkham.Types.Game
import Api.Arkham.Types.MultiplayerVariant (MultiplayerVariant (WithFriends))
import Arkham.Achievement.Types (Achievement, achievementChecklist, achievementName)
import Arkham.Asset.Types (Asset, assetController, assetOwner, assetPlacement)
import Arkham.Campaign.Types (CampaignAttrs)
import Arkham.Card.CardCode (CardCode (..), HasCardCode (toCardCode))
import Arkham.Classes.Entity (attr, overAttrs, toAttrs)
import Arkham.Classes.GameLogger
import Arkham.Classes.HasQueue
import Arkham.Effect.Types (effectTarget)
import Arkham.Entities (Entities (..), entitiesActs)
import Arkham.Epic.Types (
  EpicRole (GroupPlayer),
  GroupOrdinal (..),
  SharedEventState,
  SharedKey (
    ActAdvanceGen,
    ActContribution,
    ActSpend,
    AwaitingOrganizer,
    MainStreetEligible,
    MainStreetReady,
    SharedActProgress
  ),
  actProgressStages,
  epicEnvDeltaRef,
  epicEnvGroup,
  epicEnvSharedRef,
  groupOrdinalKey,
  setSharedCounter,
  sharedCounter,
  sharedCounters,
  sharedTotalInvestigators,
  totalInvestigatorsKey,
  updateSharedCounter,
 )
import Arkham.Event.Types (eventController)
import Arkham.Game
import Arkham.Game.Diff
import Arkham.Game.State
import Arkham.GameEnv
import Arkham.Id
import Arkham.Investigator (lookupInvestigator)
import Arkham.Investigator.Types (Investigator, investigatorPlacement, investigatorPlayerId)
import Arkham.Location.CardDefs.TheBlobThatAteEverythingELSE qualified as Locations
import Arkham.Message
import Arkham.Placement (
  Placement (AtLocation, AttachedToInvestigator, InPlayArea, InThreatArea, StillInHand),
 )
import Arkham.Queue
import Arkham.Scenario.Types (Scenario, getMetaKeyDefault)
import Arkham.ScenarioLogKey (ScenarioCountKey (EpicShared))
import Arkham.Target (Target (InvestigatorTarget))
import Arkham.Treachery.Types (treacheryPlacement)
import Conduit
import Control.Concurrent.MVar
import Control.Concurrent.STM.TBQueue (readTBQueue)
import Control.Lens (view)
import Control.Monad.Random (mkStdGen)
import Data.Aeson.Types (parse)
import Data.ByteString.Lazy qualified as BSL
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Data.String.Conversions.Monomorphic (toStrictByteString)
import Data.Text qualified as T
import Data.These
import Data.Time.Clock
import Data.Traversable (for)
import Data.UUID (nil)
import Database.Esqueleto.Experimental hiding (update, (=.))
import Database.Redis (Connection, RedisChannel, publish, runRedis)
import Entity.Answer
import Entity.Arkham.GameRaw
import Entity.Arkham.Step
import Import hiding (delete, exists, on, (==.), (>=.))
import Import qualified as P
import Json

-- ConnectionOptions, connectionCompressionOptions and defaultConnectionOptions
-- come in via the Yesod.WebSockets re-export below.
import Network.WebSockets (
  CompressionOptions (PermessageDeflateCompression),
  ConnectionException,
  PermessageDeflate (pdCompressionLevel),
  defaultPermessageDeflate,
  withPingThread,
 )
import UnliftIO.Exception hiding (Handler)
import UnliftIO.Timeout (timeout)
import Yesod.WebSockets

{- | Admit administrators without a membership query; otherwise require the
caller's @ArkhamPlayer@ row. The rejection action is 'notFound' in production
so game absence and missing membership remain indistinguishable.

'Api.Handler.Arkham.Decks.requireGameDecksAccess' delegates to this so that
all game-access predicates share a single implementation.
-}
requireGameAccess :: Monad m => Bool -> m Bool -> m () -> m ()
requireGameAccess isAdmin lookupMembership reject =
  withGameAccess isAdmin lookupMembership reject (pure ())

{- | Run protected game work only for an administrator or game member.

The rejection and protected actions share a result type, so denied access
cannot fall through to protected work even when a test rejection action
returns normally rather than aborting like Yesod's 'notFound'.
-}
withGameAccess :: Monad m => Bool -> m Bool -> m a -> m a -> m a
withGameAccess isAdmin lookupMembership reject protected
  | isAdmin = protected
  | otherwise = do
    isMember <- lookupMembership
    if isMember then protected else reject

{- | How often to ping an idle websocket. Must stay comfortably under Warp's
'settingsTimeout' (30s by default) -- see 'withKeepAlive'.
-}
keepAlivePingSeconds :: Int
keepAlivePingSeconds = 15

{- | Connection options shared by every game and event socket.

A game update is the whole 'PublicGame' -- not a delta -- which on a real
mid-campaign game measures 57-206 KB of JSON, and it went out uncompressed
until now. permessage-deflate takes a 206 KB payload to ~33 KB (6.3x).
Browsers offer the extension on every websocket handshake and negotiate it
themselves, so this needs no client change.

DO NOT set 'serverNoContextTakeover' (or 'clientNoContextTakeover') here. They
read like pure memory/ratio knobs and are not: in @websockets@ they select
between two completely different deflaters.

> makeMessageDeflater (Just pmd)
>     | serverNoContextTakeover pmd = do
>         return $ \msg -> do
>             ptr <- initDeflate pmd        -- per MESSAGE
>             deflateMessageWith (deflateBody ptr) msg
>     | otherwise = do
>         ptr <- initDeflate pmd            -- per CONNECTION
>         return $ \msg -> deflateMessageWith (deflateBody ptr) msg

'initDeflate' is @Zlib.initDeflate level (WindowBits -15)@, a fresh zlib
arena (~256 KB at level 6 / memLevel 8) malloc'd and initialised. With
takeover disabled that happens for every outbound frame, on every connection,
with no minimum-size threshold -- and 'handleMessageLog' broadcasts one frame
per 'ClientMessage', which during scenario setup is hundreds of ~100 byte log
lines. Each one paid a 256 KB zlib setup to compress 100 bytes.

That churn is also worse for memory than the thing disabling takeover was
meant to avoid. Takeover costs a deflate+inflate pair (~400 KB) pinned per
socket but reused; disabling it allocates and frees ~256 KB per message, as
foreign memory behind a tiny Haskell object, so the GC has almost no pressure
signal to run the finalisers promptly. Lower peak, far higher RSS drift -- and
the HPA scales on memory.

The ratio argument for disabling it was sound but immaterial: deflate's window
is 32 KB against messages several times that, so the previous message is
mostly evicted before the next can reference it and takeover bought only ~2%.
That is a reason not to *expect* much from takeover, not a reason to pay
per-message zlib setup to avoid it.

Nothing in negotiation re-enables these: @setParam@ only ever sets them from
the client's offered params, and browsers offer @client_max_window_bits@
without either takeover flag.

The compression level is 6 rather than the library's 8. Compression is per
connection, so a four-player table deflates the same state four times on every
action; on a 206 KB payload level 8 measured 4.0 ms and 32.6 KB against level
6's 2.4 ms and 33.3 KB.
-}
compressedConnectionOptions :: ConnectionOptions
compressedConnectionOptions =
  defaultConnectionOptions
    { connectionCompressionOptions =
        PermessageDeflateCompression
          defaultPermessageDeflate {pdCompressionLevel = 6}
    }

{- | 'ConnectionOptions' for a game or event socket, honouring the
@ARKHAM_WS_COMPRESSION@ kill switch (see 'appWebsocketCompression').

Compression is a per-message CPU and allocation cost on a path that is hard
to reproduce outside production, so it needs to be switchable there without a
rebuild: set @ARKHAM_WS_COMPRESSION=false@ and restart to compare.
-}
websocketConnectionOptions :: Handler ConnectionOptions
websocketConnectionOptions = do
  enabled <- getsYesod $ appWebsocketCompression . appSettings
  pure $ if enabled then compressedConnectionOptions else defaultConnectionOptions

{- | Warp treats a websocket as a raw response and only tickles its idle
timeout on real socket traffic, so a quiet game (nobody taking a turn) is
torn down after 'settingsTimeout' seconds and the client silently
reconnects -- a 30s churn cycle per open tab. A server-side ping well inside
that window keeps the socket alive, and does the same for any proxy in front
of it (the Vite dev proxy in development, nginx/CloudFront in production).
-}
withKeepAlive :: WebSocketsT Handler a -> WebSocketsT Handler a
withKeepAlive inner = do
  conn <- ask
  withRunInIO \run -> withPingThread conn keepAlivePingSeconds (pure ()) (run inner)

data GameStreamRole = ParticipantStream | SpectatorStream
  deriving stock (Eq, Show)

decodeGameStreamAnswer :: GameStreamRole -> ByteString -> Either String (Maybe Answer)
decodeGameStreamAnswer ParticipantStream = fmap Just . eitherDecodeStrict
decodeGameStreamAnswer SpectatorStream = const $ Right Nothing

gameStream :: ArkhamGameId -> WebSocketsT Handler ()
gameStream = gameStreamFor ParticipantStream

spectatorGameStream :: ArkhamGameId -> WebSocketsT Handler ()
spectatorGameStream = gameStreamFor SpectatorStream

gameStreamFor :: GameStreamRole -> ArkhamGameId -> WebSocketsT Handler ()
gameStreamFor role gameId = catchingConnectionException $ withKeepAlive do
  let cleanup room subId = do
        unsubscribeFromRoom room subId
        lift $ decrRoomMember gameId
        -- Drops the room AND its Redis subscription together, under the rooms
        -- lock, if this was the last subscriber. The room owns the one
        -- subscription for this channel on this pod; this connection has no
        -- subscription of its own to tear down.
        void $ lift $ releaseGameRoomIfEmpty gameId

  -- Joining registers this socket as a subscriber in the same turn of the
  -- rooms lock that looks the room up, so a concurrently departing connection
  -- can't release the room before we are counted on it.
  let acquire = do
        joined <- lift $ joinGameRoom gameId
        lift $ incrRoomMember gameId
        pure joined

  bracket acquire (\(room, subId, _) -> cleanup room subId) \(room, _subId, sub) -> do
    let broadcast = broadcastToRoom room
    let Subscriber {subQueue, subOverflow} = sub
    let sender =
          forever
            ( do
                msg <- atomically do
                  overflowed <- readTVar subOverflow
                  if overflowed
                    then throwSTM SlowSubscriber
                    else readTBQueue subQueue
                sendTextData msg
            )
            `catch` (\(_ :: SlowSubscriber) -> pure ())

    race_
      sender
      (runConduit $ sourceWS .| mapM_C (handleData role room broadcast))
 where
  handleData streamRole room broadcast dataPacket = lift do
    case decodeGameStreamAnswer streamRole dataPacket of
      Left err -> $(logWarn) $ tshow err
      Right Nothing -> pure ()
      Right (Just answer) ->
        updateGame answer gameId (Just room) `catch` \(e :: SomeException) -> do
          liftIO $ broadcast $ encode $ GameError $ tshow e

data SlowSubscriber = SlowSubscriber
  deriving stock Show
  deriving anyclass Exception

catchingConnectionException :: WebSocketsT Handler () -> WebSocketsT Handler ()
catchingConnectionException f =
  f `catch` \e -> $(logWarn) $ tshow (e :: ConnectionException)

{- | Generic read-only room subscription loop: fan room messages out to this
websocket and run @onLeave@ when it disconnects. Inbound client frames are
ignored (read-only). Used by the Epic Multiplayer event stream; 'gameStream'
has its own variant with per-game member counting and a log cache.

@joinRoom@ must register this socket as a subscriber atomically with looking
the room up (see 'joinRoomIn'); the room's single Redis subscription is
established there and torn down by 'releaseRoomIfEmpty', so nothing is
subscribed per connection here. @onLeave@ runs on every disconnect, not only
the last one: deciding whether this was in fact the last subscriber has to
happen under the rooms lock, so it can't be decided out here.
-}
streamRoom
  :: Handler (Room, Int, Subscriber) -> Handler () -> WebSocketsT Handler ()
streamRoom joinRoom onLeave = catchingConnectionException $ withKeepAlive do
  let cleanup room subId = do
        unsubscribeFromRoom room subId
        lift onLeave
  bracket (lift joinRoom) (\(room, subId, _) -> cleanup room subId) \(_room, _subId, sub) -> do
    let Subscriber {subQueue, subOverflow} = sub
    let sender =
          forever
            ( do
                msg <- atomically do
                  overflowed <- readTVar subOverflow
                  if overflowed then throwSTM SlowSubscriber else readTBQueue subQueue
                sendTextData msg
            )
            `catch` (\(_ :: SlowSubscriber) -> pure ())
    race_ sender (runConduit $ sourceWS .| mapM_C (\(_ :: ByteString) -> pure ()))

{- | A broadcast callback. Used to fan out log lines and game-state updates
to every WebSocket subscriber on a room. May be a no-op if there are no
subscribers (e.g. a direct REST PUT with no client listening), in which
case messages are silently dropped instead of buffered indefinitely.
-}
type Broadcast = BSL.ByteString -> IO ()

{- | Hard cap on a single runMessages invocation. If a game's message
processing exceeds this we kill the action and roll back the surrounding
DB transaction so the worker (and the FOR UPDATE lock on the game row)
can be released. Empirically a normal action completes in well under 1s;
30s gives plenty of headroom for slow-but-legitimate scenario setup
while still preventing one poison game from monopolising a worker.
-}
runMessagesTimeoutMicros :: Int
runMessagesTimeoutMicros = 30 * 1000000

{- | Thrown by updateGame when 'runMessages' exceeds 'runMessagesTimeoutMicros'.
The Yesod handler turns this into a 500; the important effect is that the
exception propagates out of runDB, rolls back the transaction, and frees
the worker. Search Honeycomb / logs for this to find poison games.
-}
data RunMessagesTimeout = RunMessagesTimeout ArkhamGameId Int
  deriving stock (Eq, Show)
  deriving anyclass Exception

data EpicOrganizerGateBlocked = EpicOrganizerGateBlocked
  deriving stock Show
  deriving anyclass Exception

{- | Run @action@ under a hard @micros@ time limit, throwing
'RunMessagesTimeout' (never returning a fake success) if it does not
finish in time. This is the SAME circuit breaker 'updateGame' uses around
its 'runMessages' call (with the fixed 'runMessagesTimeoutMicros' budget),
factored out so 'Api.Handler.Arkham.PendingGames.runPendingJoinSetup' can
wrap its own (potentially TWO, back-to-back) 'runMessages' calls with the
identical budget and exception, and so tests can call this directly with a
short duration instead of waiting out the real 30s production budget. The
caller must invoke this from inside the same 'runDB' transaction whose
locks it wants released on timeout: an uncaught 'RunMessagesTimeout'
propagates out of 'runDB' and rolls the whole transaction back via
'runDB'\'s ordinary all-or-nothing semantics -- there is no separate
rollback step here, exactly as every other write failure in this codebase
already relies on.
-}
runWithMessagesTimeout :: MonadIO m => ArkhamGameId -> Int -> IO a -> m a
runWithMessagesTimeout gameId micros action = do
  mResult <- liftIO $ timeout micros action
  case mResult of
    Just a -> pure a
    Nothing -> liftIO $ throwIO $ RunMessagesTimeout gameId micros

updateGame :: Answer -> ArkhamGameId -> Maybe Room -> Handler ()
updateGame response gameId mRoom = do
  let broadcast :: Broadcast
      broadcast = case mRoom of
        Nothing -> \_ -> pure ()
        Just room -> broadcastToRoom room
  let rejectOrganizerGate action =
        action `catch` \EpicOrganizerGateBlocked ->
          permissionDenied "This event is waiting for the organizer's clue allocation"
  (ArkhamGame {..}, oldLogEntries, updatedLog, mSharedUpdate, actAdvanced, newAchievements) <- rejectOrganizerGate $ runDB $ atomicallyWithGame gameId \g@ArkhamGame {..} -> do
    -- Read the prior log from the per-room cache when it's in sync with
    -- the just-locked game's step; otherwise fall back to the DB. Avoids
    -- the 217-row-avg getGameLog read on every action in the common case.
    oldLogEntries <-
      liftIO (lookupCachedLog mRoom arkhamGameStep) >>= \case
        Just entries -> pure entries
        Nothing -> gameLogToLogEntries <$> getGameLog gameId Nothing

    mLastStep <- getBy $ UniqueStep gameId arkhamGameStep
    let
      gameJson@Game {..} = arkhamGameCurrentData
      currentQueue =
        maybe [] (choiceMessages . arkhamStepChoice . entityVal) mLastStep

    activePlayer <- runReaderT getActivePlayer gameJson

    let playerId = fromMaybe activePlayer (answerPlayer response)

    logRef <- newIORef []
    reply <- handleAnswer gameJson playerId response
    case reply of
      Unhandled _ -> pure (g, oldLogEntries, [], Nothing, False, [])
      Handled answerMessages -> do
        -- Epic Multiplayer: if this game is a group within an event, build an
        -- EpicEnv so Shared* messages emitted during the action are captured as
        -- deltas. 'Nothing' (every ordinary game) means zero behavior change.
        mEpicCtx <- lookupGameEvent gameId
        -- The organizer barrier is a server-side lock, not merely a frontend
        -- overlay. This also closes direct-API and delayed-click paths that could
        -- otherwise answer the parked Continue before allocation.
        for_ mEpicCtx \(Entity _ event, _) -> do
          let
            shared = arkhamEpicEventSharedState event
            gateOpen = any (\stage -> sharedCounter (AwaitingOrganizer stage) shared > 0) (actProgressStages shared)
            continuesActAdvance = any (\case NextAdvanceActStep {} -> True; _ -> False) answerMessages
          when (gateOpen && continuesActAdvance) $ liftIO $ throwIO EpicOrganizerGateBlocked
        mEpicEnv <- traverse (uncurry mkEpicEnv) mEpicCtx

        -- Epic Multiplayer: mirror the current shared counters into this group's
        -- scenario state (as EpicShared counts) before the action runs, so the
        -- scenario/enemy read up-to-date shared values purely. Refreshed every
        -- action (pull), keyed by sharedKeyText, plus the frozen total.
        syncMsgs <- case mEpicEnv of
          Nothing -> pure []
          Just epic -> epicSyncMessages (epicEnvGroup epic) <$> liftIO (readIORef (epicEnvSharedRef epic))

        let
          messages =
            [SetActivePlayer playerId | activePlayer /= playerId]
              <> answerMessages
              <> [SetActivePlayer activePlayer | activePlayer /= playerId]
        gameRef <- newIORef gameJson
        queueRef <- newQueue ((ClearUI : syncMsgs <> messages) <> currentQueue)
        genRef <- newIORef $ mkStdGen gameSeed

        -- Circuit breaker: cap runMessages at runMessagesTimeoutMicros so a
        -- pathological game state (infinite loop / message-handler explosion)
        -- can't hold a worker hostage and pin a FOR UPDATE lock on the game
        -- row indefinitely. On timeout, throw RunMessagesTimeout -- this
        -- aborts the surrounding DB transaction (rollback releases the lock)
        -- and lets the worker return to the pool.
        -- Above-the-table achievements: collect EarnAchievement messages via
        -- the (otherwise unused) runMessages message logger; persisted below.
        achievementsRef <- newIORef []
        achievementsByRef <- newIORef []
        achievementProgressRef <- newIORef []
        achievementProgressByRef <- newIORef []
        let
          collectAchievements = \case
            EarnAchievement a -> modifyIORef' achievementsRef (a :)
            EarnAchievementBy iid a -> modifyIORef' achievementsByRef ((iid, a) :)
            AchievementProgress a items -> modifyIORef' achievementProgressRef ((a, items) :)
            AchievementProgressBy iid a items ->
              modifyIORef' achievementProgressByRef ((iid, a, items) :)
            _ -> pure ()
        runWithMessagesTimeout gameId runMessagesTimeoutMicros do
          runGameApp (GameApp gameRef queueRef genRef (handleMessageLog logRef broadcast) mEpicEnv) do
            runMessages (gameIdToText gameId) (Just collectAchievements)

        ge <- readIORef gameRef
        let diffDown = diff ge arkhamGameCurrentData
        -- Epic Multiplayer: detect an IN-GROUP act advance (the act entity is
        -- replaced on advance/loop) so we can wall off undo across it. Epic games
        -- only; cheap (acts in play is ~1).
        let actAdvanced =
              isJust mEpicCtx
                && epicActFingerprint arkhamGameCurrentData
                /= epicActFingerprint ge

        updatedQueue <- readIORef $ queueToRef queueRef
        -- handleMessageLog conses for O(1) inserts; reverse here to restore order.
        updatedLog <- reverse <$> readIORef logRef

        now <- liftIO getCurrentTime
        deleteWhere [ArkhamStepArkhamGameId P.==. gameId, ArkhamStepStep P.>. arkhamGameStep]
        let g' =
              ArkhamGame
                arkhamGameName
                ge
                (arkhamGameStep + 1)
                arkhamGameMultiplayerVariant
                arkhamGameCreatedAt
                now
        replace gameId g'
        insertMany_ $ map (newLogEntry gameId arkhamGameStep now) updatedLog
        void
          $ upsertBy
            (UniqueStep gameId (arkhamGameStep + 1))
            ( ArkhamStep
                gameId
                (Choice diffDown updatedQueue)
                (arkhamGameStep + 1)
                (ActionDiff $ view actionDiffL ge)
            )
            [ ArkhamStepChoice =. Choice diffDown updatedQueue
            , ArkhamStepActionDiff =. ActionDiff (view actionDiffL ge)
            ]

        -- Epic Multiplayer: drain any shared-counter deltas emitted this action
        -- and apply them to the authoritative event row under a short FOR UPDATE
        -- lock (taken late, only when there are deltas), within this same
        -- transaction so the game step and shared mutation commit atomically.
        mSharedUpdate <- case mEpicCtx of
          Just (eventEntity, _) -> do
            deltas <- maybe (pure []) (liftIO . readIORef . epicEnvDeltaRef) mEpicEnv
            if null deltas
              then pure Nothing
              else do
                s <-
                  applyEpicDeltasLocked
                    (entityKey eventEntity)
                    (Just gameId)
                    (Just (arkhamGameStep + 1))
                    deltas
                pure $ Just (entityKey eventEntity, s)
          Nothing -> pure Nothing

        -- Persist newly earned achievements: one row per human player per
        -- achievement, ever. insertUnique against UniqueUserAchievement makes
        -- re-earns no-ops; only genuinely new rows produce a toast.
        earned <- liftIO $ ordNub . reverse <$> readIORef achievementsRef
        -- Single-investigator earns (EarnAchievementBy): credited only to the
        -- player controlling that investigator.
        earnedBy <- liftIO $ ordNub . reverse <$> readIORef achievementsByRef
        -- Checklist progress (AchievementProgress): merge this action's items
        -- per achievement, then per user below.
        progressed <- liftIO $ reverse <$> readIORef achievementProgressRef
        progressedBy <- liftIO $ reverse <$> readIORef achievementProgressByRef
        let
          progressList =
            [ (a, ordNub $ concat [zs | (a', zs) <- progressed, a' == a])
            | a <- ordNub (map fst progressed)
            ]
        newAchievements <-
          if null earned && null earnedBy && null progressList && null progressedBy
            then pure []
            else do
              players <- P.selectList [ArkhamPlayerArkhamGameId P.==. gameId] []
              let userIds = ordNub $ map (arkhamPlayerUserId . entityVal) players
              let
                usersFor iid =
                  ordNub
                    [ arkhamPlayerUserId p
                    | p <- map entityVal players
                    , arkhamPlayerInvestigatorId p == coerce iid
                    ]
              directEarns <- fmap concat $ for earned \achievement -> do
                inserted <- for userIds \uid ->
                  P.insertUnique
                    $ ArkhamAchievement uid achievement (Just now) (Just gameId) Null
                pure [achievement | any isJust inserted]
              soloEarns <- fmap concat $ for earnedBy \(iid, achievement) -> do
                inserted <- for (usersFor iid) \uid ->
                  P.insertUnique
                    $ ArkhamAchievement uid achievement (Just now) (Just gameId) Null
                pure [achievement | any isJust inserted]
              -- Cross-playthrough checklists: accumulate items in the row's
              -- progress column and earn when every checklist item is in.
              -- Each user has their own history, so completion is per user.
              progressEarns <- fmap concat $ for progressList \(achievement, items) -> do
                completions <- for userIds \uid ->
                  applyAchievementProgress uid achievement items gameId now
                pure [achievement | or completions]
              soloProgressEarns <- fmap concat $ for progressedBy \(iid, achievement, items) -> do
                completions <- for (usersFor iid) \uid ->
                  applyAchievementProgress uid achievement items gameId now
                pure [achievement | or completions]
              pure $ ordNub $ directEarns <> soloEarns <> progressEarns <> soloProgressEarns

        pure (g', oldLogEntries, updatedLog, mSharedUpdate, actAdvanced, newAchievements)

  -- Update the per-room cache after the DB transaction has committed,
  -- so the cache is never ahead of durably-stored state.
  let publishLog = oldLogEntries <> updatedLog
  liftIO $ writeCachedLog mRoom arkhamGameStep publishLog

  -- Publish shared state before the acting game's parked question. In particular,
  -- a threshold-crossing Act 1 action has already armed AwaitingOrganizer in the
  -- same event-row write, so clients install the blocking overlay before they can
  -- see or answer the parked Continue question.
  -- Main Street's cross-game action is available only while investigators in at
  -- least two distinct groups are currently at their copies. Movement normally
  -- emits no shared delta, so derive this presence bit after every persisted
  -- action rather than relying on card messages.
  mMainStreetUpdate <- refreshMainStreetEligibility gameId
  case mMainStreetUpdate of
    Just (eid, s) -> propagateShared eid Nothing s
    Nothing -> for_ mSharedUpdate \(eid, s) -> propagateShared eid (Just gameId) s

  publishToRoom gameId
    $ GameUpdate
    $ PublicGame
      gameId
      arkhamGameName
      publishLog
      arkhamGameCurrentData

  -- Achievement unlock toasts, after the rows are durably committed.
  for_ newAchievements \achievement ->
    publishToRoom gameId $ GameAchievement (achievementName achievement)

  -- Epic Multiplayer: wall off undo across an IN-GROUP act advance. Each group
  -- advances its own act via the normal AdvanceAct flow (no cross-group injection);
  -- when this action advanced the act, set the per-game undo floor to the committed
  -- step so it can't be locally undone (the other groups follow on their own turns
  -- via 'ActAdvanceGen'). 'arkhamGameStep' here is the post-commit (new) step.
  when actAdvanced $ setGameUndoFloor gameId arkhamGameStep

{- | Merge reported checklist items into the user's progress row for a
cross-playthrough achievement (see 'achievementChecklist'); the row's
progress column holds the checked item keys as a JSON array. Returns True
when this merge completed the checklist — the row flips to earned, pointing
at the completing game. Already-earned rows are left untouched.
-}
applyAchievementProgress
  :: UserId -> Achievement -> [Text] -> ArkhamGameId -> UTCTime -> DB Bool
applyAchievementProgress uid achievement items gameId now = do
  let
    checklist = fromMaybe [] (achievementChecklist achievement)
    complete merged = not (null checklist) && all (`elem` merged) checklist
  P.getBy (UniqueUserAchievement uid achievement) >>= \case
    Just (Entity rowId row)
      | isJust (arkhamAchievementEarnedAt row) -> pure False
      | otherwise -> do
          let
            existing = case fromJSON (arkhamAchievementProgress row) of
              Success xs -> xs
              _ -> []
            merged = ordNub (existing <> items)
          if complete merged
            then do
              P.update
                rowId
                [ ArkhamAchievementProgress P.=. toJSON merged
                , ArkhamAchievementEarnedAt P.=. Just now
                , ArkhamAchievementArkhamGameId P.=. Just gameId
                ]
              pure True
            else do
              P.update rowId [ArkhamAchievementProgress P.=. toJSON merged]
              pure False
    Nothing -> do
      let merged = ordNub items
      if complete merged
        then do
          void
            $ P.insertUnique
            $ ArkhamAchievement uid achievement (Just now) (Just gameId) (toJSON merged)
          pure True
        else do
          void
            $ P.insertUnique
            $ ArkhamAchievement uid achievement Nothing Nothing (toJSON merged)
          pure False

{- | Read the cached log entries IF the cache is consistent with the locked
game's current step. Returns Nothing on a mismatch (so the caller refetches
from the DB and refreshes the cache).
-}
lookupCachedLog :: Maybe Room -> Int -> IO (Maybe [Text])
lookupCachedLog Nothing _ = pure Nothing
lookupCachedLog (Just room) currentStep = atomically do
  cachedVal <- readTVar (roomLogCache room)
  pure $ case cachedVal of
    Just c | c.cacheStep == currentStep -> Just c.cacheEntries
    _ -> Nothing

{- | Write the cache after a successful update. The step recorded is the new
post-update step; the next action will read the game at that step and find
a consistent cache.
-}
writeCachedLog :: Maybe Room -> Int -> [Text] -> IO ()
writeCachedLog Nothing _ _ = pure ()
writeCachedLog (Just room) newStep entries =
  atomically $ writeTVar (roomLogCache room) $ Just $ RoomLogCache newStep entries

newtype RawGameJsonPut = RawGameJsonPut
  { gameMessage :: Message
  }
  deriving stock (Show, Generic)
  deriving anyclass FromJSON

handleMessageLog
  :: MonadIO m => IORef [Text] -> Broadcast -> ClientMessage -> m ()
handleMessageLog logRef broadcast msg = liftIO $ do
  -- Cons in O(1); the caller reverses once when reading the IORef.
  -- The previous (logs <> [txt]) was O(n) per call -> O(n^2) per action,
  -- which mattered during scenario setup with hundreds of log lines.
  for_ (toClientText msg) $ \txt ->
    atomicModifyIORef' logRef (\logs -> (txt : logs, ()))
  broadcast (encode $ toGameMessage msg)
 where
  toGameMessage = \case
    ClientText txt -> GameMessage txt
    ClientError txt -> GameError txt
    ClientUI txt -> GameUI txt
    ClientAudio txt -> GameAudio txt
    ClientCard t v -> GameCard t v
    ClientCardOnly i t v -> GameCardOnly i t v
    ClientTarot v -> GameTarot v
    ClientShowDiscard v -> GameShowDiscard v
    ClientShowUnder v -> GameShowUnder v
    ClientPlayabilityReport cid cc chks -> GamePlayabilityInfo cid cc chks
  toClientText = \case
    ClientText txt -> Just txt
    ClientError {} -> Nothing
    ClientUI {} -> Nothing
    ClientAudio {} -> Nothing
    ClientCard {} -> Nothing
    ClientCardOnly {} -> Nothing
    ClientTarot {} -> Nothing
    ClientShowDiscard {} -> Nothing
    ClientShowUnder {} -> Nothing
    ClientPlayabilityReport {} -> Nothing

publishToRoom :: (MonadIO m, ToJSON a, HasApp m) => ArkhamGameId -> a -> m ()
publishToRoom gameId a = do
  broker <- getsApp appMessageBroker
  case broker of
    RedisBroker redisConn _ ->
      publishOrWarn redisConn (gameChannel gameId) a
    WebSocketBroker ->
      -- Don't create a Room here. If nobody is subscribed, drop the
      -- update on the floor; the next subscriber will read the latest
      -- state from the database when they connect.
      lookupRoom gameId >>= traverse_ (`broadcastToRoom` encode a)

{- | PUBLISH a payload, surfacing failures instead of swallowing them.

'runRedis' returns @Left Reply@ for a rejected command, and this was
previously @void@ed away. A GameUpdate that never leaves the pod looks
identical, from the client's side, to one that was never generated -- the
board simply stops updating -- so a dropped publish has to leave a trace.
-}
publishOrWarn :: (MonadIO m, ToJSON a) => Connection -> RedisChannel -> a -> m ()
publishOrWarn conn channel a = liftIO do
  result <-
    tryAny
      $ runRedis conn
      $ publish channel
      $ toStrictByteString
      $ encode a
  case result of
    Right (Right _) -> pure ()
    Right (Left reply) ->
      putStrLn $ "redis publish rejected on " <> show channel <> ": " <> show reply
    Left e ->
      putStrLn $ "redis publish failed on " <> show channel <> ": " <> show e

-- | Epic Multiplayer sibling of 'publishToRoom', keyed by event id.
publishToEventRoom :: (MonadIO m, ToJSON a, HasApp m) => ArkhamEpicEventId -> a -> m ()
publishToEventRoom eid a = do
  broker <- getsApp appMessageBroker
  case broker of
    RedisBroker redisConn _ ->
      publishOrWarn redisConn (eventChannel eid) a
    WebSocketBroker ->
      lookupEventRoom eid >>= traverse_ (`broadcastToRoom` encode a)

-- | The (non-null) game ids of every group in an event.
getEventGroupGameIds :: ArkhamEpicEventId -> Handler [ArkhamGameId]
getEventGroupGameIds eid = do
  rows <- runDB $ select do
    grp <- from $ table @ArkhamEpicGroup
    where_ $ grp.arkhamEpicEventId ==. val eid
    pure grp.arkhamGameId
  pure $ mapMaybe (\(Value m) -> m) rows

{- | Each group's @(ordinal, game id)@ (ordinal order), for groups that have a
game. Used to mirror the per-group ordinal into scenario state during sync and
by 'propagateShared' to fan a shared-state change out to every group.
-}
getEventGroupGroups :: ArkhamEpicEventId -> Handler [(Int, ArkhamGameId)]
getEventGroupGroups eid = do
  rows <- runDB $ select do
    grp <- from $ table @ArkhamEpicGroup
    where_ $ grp.arkhamEpicEventId ==. val eid
    orderBy [asc grp.ordinal]
    pure (grp.ordinal, grp.arkhamGameId)
  pure [(ordinal, gid) | (Value ordinal, Value (Just gid)) <- rows]

{- | Broadcast a shared-state update to the event's dashboard feed AND to every
group's own game stream, so all connected clients (organizer dashboard,
organizer bars, shared displays) reflect the new shared counters live.
-}
broadcastSharedToEvent :: ArkhamEpicEventId -> SharedEventState -> Handler ()
broadcastSharedToEvent eid s = do
  publishToEventRoom eid (SharedStateUpdate s)
  gameIds <- getEventGroupGameIds eid
  for_ gameIds \gid -> publishToRoom gid (SharedStateUpdate s)

-- Group roster payloads contain caller-specific fields (role/youAreSeated), so a
-- membership change broadcasts only an invalidation and each client refetches.
broadcastEventChanged :: ArkhamEpicEventId -> Handler ()
broadcastEventChanged eid = do
  publishToEventRoom eid EventChanged
  gameIds <- getEventGroupGameIds eid
  for_ gameIds (`publishToRoom` EventChanged)

refreshMainStreetEligibility
  :: ArkhamGameId -> Handler (Maybe (ArkhamEpicEventId, SharedEventState))
refreshMainStreetEligibility gameId = do
  mEvent <- runDB $ lookupGameEvent gameId
  case mEvent of
    Nothing -> pure Nothing
    Just (Entity eid _, _) -> do
      groups <- runDB $ P.selectList [ArkhamEpicGroupArkhamEpicEventId P.==. eid] []
      let gameIds = mapMaybe (arkhamEpicGroupArkhamGameId . entityVal) groups
      games <- runDB $ traverse P.getJust gameIds
      let
        groupAtMainStreet rawGame =
          let
            game = arkhamGameCurrentData rawGame
            mainStreets =
              Map.keysSet
                $ Map.filter
                  ((== toCardCode Locations.mainStreet) . toCardCode)
                  (entitiesLocations $ gameEntities game)
           in
            any
              ( \investigator -> case attr investigatorPlacement investigator of
                  AtLocation lid -> lid `Set.member` mainStreets
                  _ -> False
              )
              (entitiesInvestigators $ gameEntities game)
        eligible = length (filter groupAtMainStreet games) >= 2
      (shared, changed) <- runDB $ modifySharedStateLockedWith eid \s ->
        let value = if eligible then 1 else 0
         in if sharedCounter MainStreetEligible s == value
              then (s, False)
              else (setSharedCounter MainStreetEligible value s, True)
      pure $ (eid, shared) <$ guard changed

{- | The ScenarioCountSet messages that mirror the authoritative shared counters
into a group's scenario state (as EpicShared counts), keyed by sharedKeyText,
plus the frozen total and this group's own ordinal. The scenario/enemy
reconcile their local board representations (Resource tokens, Subject 8L-08
health) from these; a card can read 'groupOrdinalKey' to learn which group it is
and 'ActAdvanceGen' to learn when it is behind on advancing its act.
-}
epicSyncMessages :: GroupOrdinal -> SharedEventState -> [Message]
epicSyncMessages (GroupOrdinal ordinal) shared =
  -- total-investigators FIRST: entities that derive a value from it (e.g. Subject
  -- 8L-08's max health = 15 * total) must see it before their own counter syncs.
  ScenarioCountSet (EpicShared totalInvestigatorsKey) (sharedTotalInvestigators shared)
    : ScenarioCountSet (EpicShared groupOrdinalKey) ordinal
    : [ScenarioCountSet (EpicShared k) v | (k, v) <- Map.toList (sharedCounters shared)]

{- | Server-initiated run of @msgs@ inside one group's game, under the same
FOR UPDATE lock / GameApp machinery a normal action uses. Runs only when the
game is still actively playing AND @p@ holds for its current state; the
predicate is evaluated INSIDE the lock so concurrent callers serialize on it
(e.g. the Epic time-up forcing checks the agenda stage here so duplicate
countdown-expiry calls can't double-advance). Persists a new step whose pending
queue is the queue produced by the run (e.g. the continuation of a question the
run parked) followed by the group's previously-pending queue, then broadcasts
the GameUpdate. Games that are not active, or fail @p@, are left untouched.
appEvent = Nothing: these server-initiated runs must not themselves emit shared
deltas (they reconcile board state directly), so there is no feedback loop.
-}
runMessagesInGroupWhen :: (Game -> Bool) -> [Message] -> ArkhamGameId -> Handler ()
runMessagesInGroupWhen p msgs gid = void $ runMessagesInGroupCore p msgs gid

{- | The core of 'runMessagesInGroupWhen'. Persists a new step with an EMPTY
down-patch: every server-initiated group run (board sync, time-up forcing, the
act-advance spend/flip) is reconciled forward and is NOT independently undoable
— the act-advance spend/flip is instead walled off by the per-game undo FLOOR
('Api.Arkham.Epic.getGameUndoFloor'), and board syncs are re-derived on the
next propagate. Returns the new 'ArkhamGame' (with its new step) so callers can
read the post-run step (e.g. to set that floor).
-}
runMessagesInGroupCore
  :: (Game -> Bool) -> [Message] -> ArkhamGameId -> Handler (Maybe ArkhamGame)
runMessagesInGroupCore p msgs gid = do
  now <- liftIO getCurrentTime
  mUpdate <- runDB $ atomicallyWithGame gid \ArkhamGame {..} ->
    case gameGameState arkhamGameCurrentData of
      IsActive | p arkhamGameCurrentData -> do
        mLastStep <- getBy (UniqueStep gid arkhamGameStep)
        let currentQueue = maybe [] (choiceMessages . arkhamStepChoice . entityVal) mLastStep
        gameRef <- liftIO $ newIORef arkhamGameCurrentData
        queueRef <- liftIO $ newQueue msgs
        genRef <- liftIO $ newIORef (mkStdGen (gameSeed arkhamGameCurrentData))
        liftIO
          $ runGameApp (GameApp gameRef queueRef genRef (pure . const ()) Nothing)
          $ runMessages (gameIdToText gid) Nothing
        updatedGame <- liftIO $ readIORef gameRef
        -- The queue left after the run: empty for a pure board sync (it drains to
        -- empty), or the continuation of a question the run parked (e.g. the
        -- lead-player confirm of a forced agenda advance). Persist it AHEAD of the
        -- group's previously-pending queue so a forced advance resolves to
        -- completion while an undisturbed sync simply preserves the in-flight
        -- queue (producedQueue is [] there).
        producedQueue <- liftIO $ readIORef (queueToRef queueRef)
        let
          game' =
            ArkhamGame
              arkhamGameName
              updatedGame
              (arkhamGameStep + 1)
              arkhamGameMultiplayerVariant
              arkhamGameCreatedAt
              now
        replace gid game'
        insert_
          $ ArkhamStep
            gid
            (Choice mempty (producedQueue <> currentQueue))
            (arkhamGameStep + 1)
            (ActionDiff $ view actionDiffL updatedGame)
        pure (Just game')
      _ -> pure Nothing
  for_ mUpdate \g' ->
    publishToRoom gid
      $ GameUpdate
      $ PublicGame gid (arkhamGameName g') [] (arkhamGameCurrentData g')
  pure mUpdate

-- | 'runMessagesInGroupWhen' with no extra guard beyond the game being active.
runMessagesInGroup :: [Message] -> ArkhamGameId -> Handler ()
runMessagesInGroup = runMessagesInGroupWhen (const True)

{- | Set (upsert) a group game's undo FLOOR to @step@: undo can no longer cross it
(enforced in 'Api.Handler.Arkham.Undo'). Called for every group an act advance
settled, with that group's post-advance persistence step. Floors only ever
increase (each settlement runs at a later step), so an unconditional set is
monotonic.
-}

{- | The pure lock-vs-mapping split for a Main Street group swap: which
'ArkhamGame' rows to lock, in canonical order, versus which one is
"first"/"second" for the actual swap semantics and response. Kept as its own
type (rather than inlining a sort at the call site) so this decision is
directly unit testable without a live database -- see
"Arkham.Api.MainStreetSwapPlanSpec".
-}
data MainStreetSwapPlan = MainStreetSwapPlan
  { lockOrder :: [ArkhamGameId]
  -- ^ Every DISTINCT game id among the two involved, ascending by
  -- 'ArkhamGameId's own 'Ord' instance -- computed by the one shared
  -- 'canonicalEpicGameLockOrder' both this swap and
  -- 'Api.Handler.Arkham.Events.deleteEpicEventAggregate' delegate to, so
  -- neither can independently drift into a different order. Deliberately
  -- NOT ordered by either group's ordinal: this swap only ever resolves
  -- TWO of an event's linked games, while a full deletion locks every
  -- linked game, and an ordinal-based order is not independent of which
  -- subset of games a caller happens to have -- see
  -- 'canonicalEpicGameLockOrder's Haddock for the concrete scenario
  -- where that would let this swap and a deletion lock the same pair of
  -- games in opposite orders. A degenerate same-game-id request (see
  -- below) collapses to a single-element list here: locking it once is
  -- exactly as sufficient as locking it twice, and
  -- 'canonicalEpicGameLockOrder' treats "lock each distinct linked game
  -- once" as its own invariant, not an accident of what happens to be
  -- harmless.
  , firstGameId :: ArkhamGameId
  -- ^ Unchanged from the request: the game 'firstOrdinal' resolved to.
  , secondGameId :: ArkhamGameId
  -- ^ Unchanged from the request: the game 'secondOrdinal' resolved to.
  }
  deriving stock (Eq, Show)

{- | Build a 'MainStreetSwapPlan' from each side's resolved game id. The lock
order is always ascending by 'ArkhamGameId' itself -- regardless of which
one the caller called "first", and regardless of either game's group
ordinal -- matching the fixed order
'Api.Handler.Arkham.Events.deleteEpicEventAggregate' locks linked games in,
so the two paths can never wait on each other in opposite orders. Both game
ids are handed to 'canonicalEpicGameLockOrder' -- the SAME pure ordering
function 'Api.Handler.Arkham.Events.deleteEpicEventAggregate' uses for its
own lock plan, not an independently reimplemented sort here, and one that
takes plain 'ArkhamGameId's, never an ordinal-carrying type, so an ordinal
cannot be smuggled into the lock order by construction. The
'firstGameId'\/'secondGameId' fields are copied straight from the
arguments, UNSORTED, so the original request's first/second mapping --
which decides the actual swap semantics, not just lock order -- is always
preserved regardless of how 'lockOrder' turned out. This function itself
stays total for a degenerate same-game-id call, collapsing to a
single-element 'lockOrder' (locking that one game once is all a
same-transaction re-acquisition of its own lock would accomplish anyway);
in production such a call never actually happens, because
'planAndExecuteMainStreetSwap' already reports 'MainStreetSwapSameGame' --
before this function is ever built -- for two DIFFERENT ordinals resolving
to the same game id. That is a distinct, later check from the handler's
own ordinal-equality guard in
'Api.Handler.Arkham.Events.postApiV1ArkhamEventSwapMainStreetR', which only
rejects requesting the exact same ordinal twice.
-}
mainStreetSwapPlan :: ArkhamGameId -> ArkhamGameId -> MainStreetSwapPlan
mainStreetSwapPlan firstGameId secondGameId =
  MainStreetSwapPlan
    { lockOrder = canonicalEpicGameLockOrder [firstGameId, secondGameId]
    , firstGameId
    , secondGameId
    }

{- | Reasons the two already-locked, already-present games this swap resolved
still cannot support the swap -- checked strictly AFTER both canonical
'lockSwapGame' locks are confirmed held, and strictly BEFORE any write,
readiness spend, room publish, or undo-floor mutation (see
'swapInvestigatorState'). Distinct from a missing/unlinked relation
('MainStreetSwapMissing', decided before any lock) and from two ordinals
sharing one game ('MainStreetSwapSameGame', also decided before any lock):
every constructor here means the request was well-formed and both games were
genuinely found and locked, but the PERSISTED state under those locks is
stale, malformed, or otherwise cannot support this swap. 'swapMainStreetInvestigators'
maps every constructor here to the exact same static, non-leaking rejection
-- this type exists so each distinct cause is independently unit testable
(see "Arkham.Api.MainStreetSwapSpec"), not so the client ever sees which one
occurred.
-}
data MainStreetSwapStateFailure
  = -- | A locked game's active scenario has no recorded 'MainStreetReady'
    -- investigator: the group never actually activated Main Street, or the
    -- metadata was never populated to begin with.
    MainStreetSwapNotReady
  | -- | A locked game has no Main Street location (card @89006@) in play to
    -- swap the OTHER side's investigator onto.
    MainStreetSwapNoLocation
  | -- | Both sides' recorded ready investigators resolved to the SAME
    -- 'InvestigatorId'. Nothing in the schema forbids this: an
    -- 'InvestigatorId' is a 'CardCode', shared by any two DIFFERENT games
    -- that both happen to include that investigator card, so this is a
    -- real, checked ambiguity, not just a defensive comment. Reported
    -- before either side's game entities are ever consulted.
    MainStreetSwapDuplicateInvestigator
  | -- | A recorded ready investigator id is not present among that game's
    -- OWN investigators: the metadata is stale, malformed, or refers to an
    -- investigator that has since left this specific game (a previous
    -- swap, or a direct game mutation, independent of this request).
    MainStreetSwapInvestigatorMissing
  | -- | A ready investigator's CURRENT placement -- read fresh, after both
    -- locks were taken -- is no longer at the Main Street location this
    -- swap expects: it moved (or was moved) between the group activating
    -- Main Street and this transaction acquiring its lock.
    MainStreetSwapInvestigatorMoved
  | -- | Both sides' recorded ready investigators resolved to the SAME
    -- 'PlayerId'. Distinct from 'MainStreetSwapDuplicateInvestigator'
    -- (which compares 'InvestigatorId's, a 'CardCode' shared across
    -- games): this compares the underlying 'Entity.Arkham.Player.ArkhamPlayer'
    -- row identity itself, which nothing in the persisted 'Game' JSON
    -- otherwise prevents from colliding.
    MainStreetSwapDuplicatePlayer
  | -- | An outgoing participant is not represented EXACTLY ONCE across its
    -- own game's entities map, player order, and players list -- missing
    -- from one of them, or duplicated within one -- before this swap would
    -- remove it. Reported before either game's membership lists are
    -- mutated.
    MainStreetSwapParticipantInconsistent
  | -- | Once its own outgoing participant is accounted for, a destination
    -- game ALREADY contains the incoming investigator id, player id, or
    -- player-order entry -- e.g. a third, non-ready investigator in that
    -- game happens to share a card code with the arriving investigator.
    -- Reported before any 'Map.insert' or list append that could otherwise
    -- silently overwrite or duplicate an unrelated investigator.
    MainStreetSwapIncomingCollision
  | -- | A participant's 'Entity.Arkham.Player.ArkhamPlayer' row could not be
    -- row-locked because it no longer exists: it was deleted concurrently,
    -- independent of this swap, between the recorded ready investigator
    -- being read and this transaction locking its player row.
    MainStreetSwapPlayerMissing
  | -- | A participant's locked 'Entity.Arkham.Player.ArkhamPlayer' row is
    -- currently linked to a DIFFERENT game than the one this swap expects
    -- it to be leaving: the authorization table and the persisted 'Game'
    -- JSON have fallen out of sync (a prior partial write, or a direct
    -- mutation of one without the other, independent of this request).
    MainStreetSwapPlayerWrongGame
  | -- | A participant's locked 'Entity.Arkham.Player.ArkhamPlayer' row is
    -- recorded under a DIFFERENT investigator id than the one this swap
    -- resolved as ready: same desynchronization hazard as
    -- 'MainStreetSwapPlayerWrongGame', for the OTHER column that row
    -- carries.
    MainStreetSwapPlayerMismatch
  | -- | A recorded ready investigator id (the map KEY this swap looked it up
    -- under, in some game's 'Arkham.Entities.entitiesInvestigators') names an
    -- 'Investigator' whose OWN internal id differs from that key. Nothing in
    -- the schema forbids a map from being keyed under one id while holding a
    -- value recorded under another, but 'computeMainStreetSwap''s @addOwned@
    -- always re-inserts an arriving investigator under its OWN internal id
    -- (never the caller-supplied key it was resolved through), while every
    -- OTHER membership list this swap touches ('gamePlayerOrder',
    -- 'gamePlayers', and 'Entity.Arkham.Player.ArkhamPlayer' locking) keys
    -- consistently off the resolved KEY. A stale/malformed persisted map
    -- like this would otherwise leave the destination game's entities map
    -- keyed under the value's id while every other list still references the
    -- key -- a silent divergence, not a crash -- so 'resolveSwapInvestigator'
    -- rejects it immediately, before any membership or placement check ever
    -- runs.
    MainStreetSwapInvestigatorKeyMismatch
  | -- | Both locked participants' 'Entity.Arkham.Player.ArkhamPlayer' rows
    -- (see 'lockSwapPlayer') are recorded under the SAME
    -- 'Entity.User.UserId'. Nothing rejects this earlier: a single user
    -- account can hold seats in two different Epic groups (each seat is its
    -- own row), but actually performing such a swap would move both
    -- participants' rows to a game the OTHER already occupies, and
    -- 'Entity.Arkham.Player.UniquePlayer' (unique on @(userId, gameId)@)
    -- would let the FIRST of the two writes succeed and the second collide
    -- with it -- an untyped database exception, not a typed outcome.
    -- Checked (see 'validateSwapParticipantUsers') strictly BEFORE any
    -- write, immediately once both participant rows are locked and
    -- individually validated.
    MainStreetSwapSameUser
  | -- | The user about to occupy a destination game (via either
    -- participant's row moving there) already holds some OTHER
    -- 'Entity.Arkham.Player.ArkhamPlayer' row in that same destination game
    -- -- e.g. an unrelated membership or spectator seat, independent of this
    -- swap's own two participants. Moving the incoming participant's row
    -- there would otherwise collide with 'Entity.Arkham.Player.UniquePlayer'
    -- exactly like 'MainStreetSwapSameUser' does, for a THIRD, pre-existing
    -- row rather than the other participant's own row. Checked (see
    -- 'checkSwapDestinationOccupancy') strictly BEFORE any write, once both
    -- participants are locked, validated, and confirmed to be different
    -- users.
    MainStreetSwapDestinationOccupied
  | -- | Either locked game's 'Arkham.Entities.entitiesInvestigators' or
    -- 'Arkham.Entities.entitiesLocations' contains AT LEAST ONE entry whose
    -- map key disagrees with that entry's own internal id -- participant
    -- or not (see 'validateEntityMapIdentity'). Distinct from, and checked
    -- strictly BEFORE, 'MainStreetSwapInvestigatorKeyMismatch' (which only
    -- re-confirms this for the specific ready participant this swap
    -- resolved): a mismatch anywhere else in either map is just as
    -- dangerous, since 'computeMainStreetSwap''s @addOwned@ always
    -- re-inserts an ARRIVING investigator under its own (correct) internal
    -- id, so a stray, wrongly-keyed NONPARTICIPANT entry sharing that same
    -- internal id would otherwise let the arriving entry silently
    -- duplicate it once the swap actually mutated state, rather than being
    -- caught by the existing 'Map.member'-based collision check (which
    -- only inspects the CURRENT key set, not whether it is itself
    -- internally consistent). Checked for BOTH games, before any
    -- readiness\/placement\/membership check or transformation ever runs.
    MainStreetSwapInvalidEntityMap
  | -- | The Epic event this swap belongs to vanished concurrently between
    -- both linked games being locked (see 'lockSwapGame') and this swap's
    -- own event-row lock being taken (see 'lockSwapEvent'). Structurally
    -- unreachable in production: the only writer that can remove an Epic
    -- event, 'Api.Handler.Arkham.Events.deleteEpicEventAggregate', must
    -- itself lock every one of the event's linked games FIRST, in the
    -- identical canonical order this swap already locked its own two
    -- games in (see 'MainStreetSwapPlan'), so it cannot have already
    -- committed the event's removal while this transaction still holds
    -- those same game locks. Handled as a typed outcome rather than
    -- assumed impossible anyway, exactly like
    -- 'Api.Handler.Arkham.PendingGames.PendingJoinEventVanished'.
    MainStreetSwapEventVanished
  | -- | Once both participants' 'Entity.Arkham.Player.ArkhamPlayer' rows
    -- are locked and validated, one participant's EXISTING
    -- 'GroupPlayer' 'Entity.Arkham.Epic.ArkhamEpicMember' membership row
    -- (if any -- a legacy seat with none at all is always valid, see
    -- 'validateSwapMembershipMove') is recorded under some ordinal OTHER
    -- than the group this swap expects them to currently occupy: the
    -- authorization table and this swap's own assumptions about which
    -- group each participant currently belongs to have fallen out of
    -- sync (a prior partial write, a direct mutation of one without the
    -- other, or a genuinely concurrent reservation this swap's own game
    -- locks did not serialize against). Checked for BOTH participants,
    -- strictly BEFORE either's membership row -- or either game's\/
    -- player's row -- is actually written; see 'reconcileSwapMemberships'.
    MainStreetSwapMembershipStale
  deriving stock (Eq, Show)

{- | The result of attempting to plan and execute a Main Street group swap,
decided entirely inside one locked transaction (see
'planAndExecuteMainStreetSwap'). Mirrors
'Api.Handler.Arkham.Events.EventDeletionOutcome': the handler maps each
constructor to a distinct HTTP outcome, and only 'MainStreetSwapCompleted'
carries anything for the caller to act on once 'runDB' returns.
-}
data MainStreetSwapOutcome
  = -- | Either requested ordinal names no group for this event, or the
    -- group it names has no linked game (never had one, or one that has
    -- since been unlinked) -- both collapse to the same nondisclosing
    -- outcome as a linked game vanishing concurrently before its lock could
    -- be taken (see 'lockSwapGame'). Maps to a plain 404, matching
    -- 'Api.Handler.Arkham.Events.EventDeletionMissing''s nondisclosure
    -- policy. In the unresolved-relation case, no lock is ever taken; in
    -- the vanished-concurrently case, one or even both canonical-order
    -- locks may already have been taken (every entry in 'lockOrder' is
    -- always attempted -- see 'planAndExecuteMainStreetSwap' -- before the
    -- results are inspected together) before this outcome is reported, but
    -- no mutable game state is ever read or written and no swap is
    -- performed either way.
    MainStreetSwapMissing
  | -- | Both ordinals resolved to a game, but to the SAME game. Nothing in
    -- the schema forbids two distinct 'Entity.Arkham.Epic.ArkhamEpicGroup'
    -- rows from referencing the same game (only the ordinal is unique per
    -- event), so this is a real, checked outcome, not just a defensive
    -- comment -- reported before 'mainStreetSwapPlan' is even built, and
    -- before any lock is taken, never treated as two independent sides to
    -- swap. Maps to 'invalidArgs'.
    MainStreetSwapSameGame
  | -- | Both games resolved to distinct ids and were confirmed locked and
    -- present, but the state under those locks cannot support the swap
    -- (see 'MainStreetSwapStateFailure' for every distinct cause). No write,
    -- readiness spend, publish, or undo-floor mutation ever happens for this
    -- outcome. Maps to 'invalidArgs', same as 'MainStreetSwapSameGame', and
    -- carries the reason purely so tests can assert exactly which
    -- precondition failed -- 'swapMainStreetInvestigators' never inspects
    -- or leaks it.
    MainStreetSwapInvalidState MainStreetSwapStateFailure
  | -- | Both games resolved to distinct ids, and both were confirmed locked
    -- and present before any mutable state was read or written; the swap
    -- itself has already been performed, in the same transaction that took
    -- both locks. Carries the plan so the caller can run its post-commit
    -- steps (spend the shared "ready" token, publish, set the undo floor)
    -- against exactly the two games this transaction locked and wrote.
    MainStreetSwapCompleted MainStreetSwapPlan
  deriving stock (Eq, Show)

{- | Which plan (if any) a swap outcome authorizes post-commit action against.
Only a committed 'MainStreetSwapCompleted' has one -- 'MainStreetSwapMissing',
'MainStreetSwapSameGame', and 'MainStreetSwapInvalidState' must never trigger
a readiness spend, publish, or undo-floor mutation. This is the exact
decision 'swapMainStreetInvestigators' makes by pattern-matching on the
outcome directly (so GHC's exhaustiveness check enforces it at the call
site); this standalone, pure copy exists purely so that same decision is
directly unit testable without a live server -- mirrors
'Api.Handler.Arkham.Events.eventDeletionCleanupGameIds'.
-}
mainStreetSwapCleanupPlan :: MainStreetSwapOutcome -> Maybe MainStreetSwapPlan
mainStreetSwapCleanupPlan (MainStreetSwapCompleted plan) = Just plan
mainStreetSwapCleanupPlan _ = Nothing

{- | Abstract persistence steps needed to plan and execute a Main Street group
swap. Mirrors 'Api.Handler.Arkham.Events.MonadEpicEventDeletion': a small set
of typed, individually testable steps, threaded by
'planAndExecuteMainStreetSwap' into the exact sequencing production runs.

Do not write an instance that catches a write failure inside
'performMainStreetSwap' and converts it into an ordinary return value of
this class: as with 'Api.Handler.Arkham.Events.MonadEpicEventDeletion',
'runDB' only rolls back on an actual uncaught exception, so silently turning
one into a normal result here would defeat the rollback guarantee.
-}
class Monad m => MonadMainStreetSwap m where
  -- | Resolve one side's requested group ordinal to its linked game id. A
  -- plain, non-locking read -- never a lock, and never a write.
  -- 'Nothing' covers BOTH "no such group for this ordinal" and "the group
  -- exists but currently has no linked game": callers must not, and do
  -- not, distinguish these -- both mean this side of the swap has no game
  -- to lock, and 'planAndExecuteMainStreetSwap' maps either straight to
  -- 'MainStreetSwapMissing' without ever calling 'lockSwapGame' or
  -- 'performMainStreetSwap'.
  resolveSwapGame :: ArkhamEpicEventId -> Int -> m (Maybe ArkhamGameId)
  -- | Row-lock one game ('FOR UPDATE' in production) and return its CURRENT
  -- row if it is still present, or 'Nothing' if it vanished concurrently.
  -- Called once per distinct game id in 'mainStreetSwapPlan''s canonical
  -- 'lockOrder', in that order, strictly BEFORE 'performMainStreetSwap'.
  -- 'planAndExecuteMainStreetSwap' always attempts every game in the lock
  -- order -- via 'traverse', which runs every action in the list before any
  -- result is inspected -- so a game vanishing concurrently can never leave
  -- one lock attempt skipped because an earlier one already failed. Callers
  -- MUST use the returned row directly rather than re-reading the game
  -- afterwards (an unlocked re-read could observe a value written by
  -- another transaction between the lock and the re-read, defeating the
  -- point of locking in the first place).
  lockSwapGame :: ArkhamGameId -> m (Maybe ArkhamGame)
  -- | Row-lock one participant's 'Entity.Arkham.Player.ArkhamPlayer' row
  -- ('FOR UPDATE' in production) and return its CURRENT row if it is still
  -- present, or 'Nothing' if it vanished concurrently. Called from
  -- 'performMainStreetSwap' ONLY after BOTH games in the plan are already
  -- confirmed locked (see 'lockSwapGame') -- games lock first, participant
  -- players lock second, and this is the ONLY place in the codebase that
  -- takes a row lock on 'Entity.Arkham.Player.ArkhamPlayer' at all, so this
  -- fixed game-then-player order can never be acquired in reverse by any
  -- other transaction. Called once per distinct locked player id, in
  -- ascending 'PlayerId' order, strictly BEFORE any game or player write --
  -- exactly mirroring 'lockSwapGame''s own contract, one level down.
  lockSwapPlayer :: PlayerId -> m (Maybe ArkhamPlayer)
  -- | Whether some 'Entity.Arkham.Player.ArkhamPlayer' row OTHER than
  -- @excludedPid@ already exists for @(userId, gameId)@ -- used by
  -- 'checkSwapDestinationOccupancy' to reject a pre-existing destination
  -- occupant (see 'MainStreetSwapDestinationOccupied') before either
  -- participant's row is moved there. A plain, non-locking read: safe
  -- because the destination game's row is already locked (via
  -- 'lockSwapGame', by the time 'lockAndValidateSwapPlayers' reaches this
  -- check), so any OTHER writer that could insert a NEW
  -- 'Entity.Arkham.Player.ArkhamPlayer' row into it must itself lock the
  -- game first (see
  -- 'Api.Handler.Arkham.Game.Debug.postApiV1ArkhamGameClaimSeatR', which
  -- enforces exactly that game-before-player ordering) and therefore
  -- cannot race this read. Called once per distinct destination game, in
  -- ascending 'ArkhamGameId' order -- the same canonical order both games
  -- were already locked in -- so two concurrent swaps sharing a
  -- destination game can never probe it in opposite orders.
  lookupSwapDestinationOccupant :: ArkhamGameId -> UserId -> ArkhamPlayerId -> m Bool
  -- | Row-lock the Epic event this swap belongs to ('FOR UPDATE' in
  -- production, via 'Api.Arkham.Epic.lockEpicEventRow') and report whether
  -- it is still present -- mirroring
  -- 'Api.Handler.Arkham.Events.MonadEpicEventDeletion.lockEpicEvent''s own
  -- plain-'Bool' contract, since neither caller ever needs the locked
  -- row's CONTENT, only that it is still there. Called from
  -- 'performMainStreetSwap' AFTER both games in the plan are already
  -- locked (see 'lockSwapGame') but BEFORE either participant's player
  -- row is locked (see 'lockSwapPlayer') -- the same game(s)->event->player
  -- order 'Api.Handler.Arkham.Game.Debug.planAndExecuteClaimSeat' and
  -- 'Api.Handler.Arkham.PendingGames.planAndExecutePendingJoin' share for
  -- any writer that touches both a game and the event. Serializes this
  -- swap's own membership reconciliation ('lookupSwapMembership',
  -- 'reconcileSwapMembership') against every other writer that creates or
  -- mutates this event's 'Entity.Arkham.Epic.ArkhamEpicMember' rows.
  -- 'False' covers the event vanishing concurrently -- structurally
  -- unreachable once both linked games are already locked (see
  -- 'MainStreetSwapEventVanished'), but handled as a typed outcome rather
  -- than assumed impossible.
  lockSwapEvent :: ArkhamEpicEventId -> m Bool
  -- | Look up (a plain, non-locking read -- safe once 'lockSwapEvent'
  -- already holds this event's row exclusively, mirroring
  -- 'Api.Arkham.Epic.reserveEpicGroupMembership''s own precondition) one
  -- user's CURRENT 'GroupPlayer' 'Entity.Arkham.Epic.ArkhamEpicMember' row
  -- for this event, if any. 'Nothing' covers a legacy seat with no
  -- membership row at all (see
  -- 'Api.Arkham.Epic.selectUserEpicSeatOrdinals'); at most one row can
  -- ever exist per @(event, user, role)@, by
  -- 'Entity.Arkham.Epic.UniqueEpicMember's own unique key, so "duplicate"
  -- is unreachable by construction here, not merely unchecked. Called
  -- once per participant, in 'swapMembershipMoves''s deterministic
  -- ascending 'UserId' order, both attempted before either result is
  -- inspected (see 'reconcileSwapMemberships').
  lookupSwapMembership :: ArkhamEpicEventId -> UserId -> m (Maybe (Entity ArkhamEpicMember))
  -- | Idempotently set one user's 'GroupPlayer' membership ordinal for
  -- this event to @ordinal@: updates the existing row if
  -- 'lookupSwapMembership' found one, or inserts a fresh one for a legacy
  -- seat with none. Called ONLY from 'reconcileSwapMemberships', and only
  -- after 'validateSwapMembershipMove' has already confirmed BOTH
  -- participants' existing rows (if any) carry their expected SOURCE
  -- ordinal -- so this never overwrites a membership row this swap did
  -- not itself just validate, and never runs for just one participant
  -- while the other fails.
  reconcileSwapMembership :: ArkhamEpicEventId -> UserId -> Int -> m ()
  -- | Perform the actual investigator swap using the two ALREADY-LOCKED
  -- game rows 'planAndExecuteMainStreetSwap' passes in (never re-read from
  -- storage): compute each side's new state, lock the Epic event (see
  -- 'lockSwapEvent'), lock and validate both participants'
  -- 'Entity.Arkham.Player.ArkhamPlayer' rows (see 'lockSwapPlayer',
  -- 'validateSwapPlayer'), reconcile both participants' Epic 'GroupPlayer'
  -- membership ordinals to their destination groups (see
  -- 'reconcileSwapMemberships'), and persist all rows if, and only if,
  -- every precondition on the locked game, player, AND membership state
  -- actually supports the swap. Returns 'Left' (and performs no write,
  -- spend, or side effect whatsoever) if it does not -- see
  -- 'MainStreetSwapStateFailure' for every distinct cause -- so a caller
  -- can never observe a crash from stale or malformed persisted state,
  -- only ever a typed result. Called from exactly ONE place: after both
  -- games in the plan are confirmed locked and present. Never called for
  -- 'MainStreetSwapMissing' or 'MainStreetSwapSameGame'. The two
  -- 'ArkhamGame' arguments correspond to the plan's 'firstGameId' and
  -- 'secondGameId' respectively, NOT to canonical lock order; the
  -- 'ArkhamEpicEventId' and the two 'Int' ordinals are the SAME event id
  -- and requested ordinals 'planAndExecuteMainStreetSwap' itself already
  -- resolved 'firstGameId'\/'secondGameId' from.
  performMainStreetSwap
    :: ArkhamEpicEventId
    -> Int
    -> Int
    -> MainStreetSwapPlan
    -> ArkhamGame
    -> ArkhamGame
    -> m (Either MainStreetSwapStateFailure ())

{- | The Main Street swap decision:

1. Resolve both requested ordinals to their linked game id. Either side
   resolving to 'Nothing' (unknown group, or a group with no linked game)
   short-circuits to 'MainStreetSwapMissing' -- before the two ordinals are
   even compared to each other, and before any lock or write.
2. If both resolve, but to the SAME game, report 'MainStreetSwapSameGame'
   -- before 'mainStreetSwapPlan' is built, and before a single lock is
   taken. This is a distinct check from the ordinal-equality guard
   'Api.Handler.Arkham.Events.postApiV1ArkhamEventSwapMainStreetR' already
   applies one level up (which only rejects requesting the SAME group
   twice): two DIFFERENT groups can, as far as the schema is concerned,
   reference the same game row.
3. Otherwise, build the canonical 'MainStreetSwapPlan' and lock BOTH
   involved games, in that canonical order, before reading either's
   mutable state. Both lock attempts are always made before their results
   are inspected together (see 'lockSwapGame'): if EITHER game vanished
   concurrently -- a linked game can be deleted directly by any of its own
   players, independent of this event, see
   'Api.Handler.Arkham.Games.deleteApiV1ArkhamGameR' -- this reports
   'MainStreetSwapMissing', and, critically, 'performMainStreetSwap' is
   never called: no read or write happens for either game.
4. If both games are locked and present, 'performMainStreetSwap' runs
   using those exact locked rows. If the state under those locks cannot
   support the swap (stale/malformed readiness metadata, a moved
   investigator, and so on), this reports 'MainStreetSwapInvalidState'
   with no write ever attempted. Only a genuine success reports
   'MainStreetSwapCompleted'.

This is the single seam production and tests both exercise directly,
mirroring 'Api.Handler.Arkham.Events.deleteEpicEventAggregate'.
-}
planAndExecuteMainStreetSwap
  :: MonadMainStreetSwap m
  => ArkhamEpicEventId -> Int -> Int -> m MainStreetSwapOutcome
planAndExecuteMainStreetSwap eventId firstOrdinal secondOrdinal = do
  mFirstGameId <- resolveSwapGame eventId firstOrdinal
  mSecondGameId <- resolveSwapGame eventId secondOrdinal
  case (mFirstGameId, mSecondGameId) of
    (Just firstGameId, Just secondGameId)
      | firstGameId == secondGameId -> pure MainStreetSwapSameGame
      | otherwise -> do
          let plan = mainStreetSwapPlan firstGameId secondGameId
          lockResults <- traverse lockSwapGame plan.lockOrder
          case sequence lockResults of
            Nothing -> pure MainStreetSwapMissing
            Just lockedGames -> do
              let lockedById = Map.fromList (zip plan.lockOrder lockedGames)
              case (Map.lookup firstGameId lockedById, Map.lookup secondGameId lockedById) of
                (Just firstRaw, Just secondRaw) -> do
                  result <- performMainStreetSwap eventId firstOrdinal secondOrdinal plan firstRaw secondRaw
                  pure $ either MainStreetSwapInvalidState (const (MainStreetSwapCompleted plan)) result
                -- Unreachable by construction: 'plan.lockOrder' is built
                -- from exactly [firstGameId, secondGameId] (deduplicated),
                -- so both keys are always present in 'lockedById' once
                -- every lock in 'lockResults' has succeeded. Handled
                -- safely (never partially) regardless.
                _ -> pure MainStreetSwapMissing
    _ -> pure MainStreetSwapMissing

{- | The player-lock-and-validate sequencing 'performMainStreetSwap' performs
once 'swapInvestigatorState' has already validated a 'MainStreetSwapTransform'
from the two locked games' state: lock BOTH participants'
'Entity.Arkham.Player.ArkhamPlayer' rows (via 'lockSwapPlayer', games having
already locked first -- see that method's Haddoc for why this order can
never be reversed), in ascending 'PlayerId' order, attempting both before
either result is inspected (exactly mirroring 'lockSwapGame''s own
game-level contract), then validate each locked row against its expected
source game and investigator (see 'validateSwapPlayer') before returning
either both validated rows or the first failure encountered. Performs no
write of any kind: this is purely the read-lock-and-validate half of
'performMainStreetSwap', factored out as its own 'MonadMainStreetSwap'-
polymorphic function so it is the exact same code path production runs and
tests can exercise directly (see "Arkham.Api.MainStreetSwapSpec").
-}
lockAndValidateSwapPlayers
  :: MonadMainStreetSwap m
  => ArkhamGameId
  -> ArkhamGameId
  -> MainStreetSwapTransform
  -> m (Either MainStreetSwapStateFailure (ArkhamPlayer, ArkhamPlayer))
lockAndValidateSwapPlayers firstGameId secondGameId transform = do
  let lockPlayerOrder = sort [transform.firstPid, transform.secondPid]
  playerLockResults <- traverse lockSwapPlayer lockPlayerOrder
  case sequence playerLockResults of
    Nothing -> pure (Left MainStreetSwapPlayerMissing)
    Just lockedPlayers -> do
      let
        lockedByPid = Map.fromList (zip lockPlayerOrder lockedPlayers)
        validated = do
          firstPlayer <-
            validateSwapPlayer firstGameId transform.firstIid (Map.lookup transform.firstPid lockedByPid)
          secondPlayer <-
            validateSwapPlayer secondGameId transform.secondIid (Map.lookup transform.secondPid lockedByPid)
          validateSwapParticipantUsers firstPlayer secondPlayer
          pure (firstPlayer, secondPlayer)
      case validated of
        Left failure -> pure (Left failure)
        Right (firstPlayer, secondPlayer) ->
          checkSwapDestinationOccupancy
            firstGameId
            transform.firstPid
            firstPlayer
            secondGameId
            transform.secondPid
            secondPlayer
            >>= \case
              Left failure -> pure (Left failure)
              Right () -> pure (Right (firstPlayer, secondPlayer))

-- | Reject the two locked participants' 'Entity.Arkham.Player.ArkhamPlayer'
-- rows sharing the SAME underlying 'Entity.User.UserId' -- see
-- 'MainStreetSwapSameUser' for why this would otherwise let the FIRST of
-- the two final 'Entity.Arkham.Player.ArkhamPlayer' writes succeed and the
-- SECOND collide with it under 'Entity.Arkham.Player.UniquePlayer'.
validateSwapParticipantUsers :: ArkhamPlayer -> ArkhamPlayer -> Either MainStreetSwapStateFailure ()
validateSwapParticipantUsers firstPlayer secondPlayer =
  when (arkhamPlayerUserId firstPlayer == arkhamPlayerUserId secondPlayer) $ Left MainStreetSwapSameUser

{- | For EACH side, probe (via 'lookupSwapDestinationOccupant') whether the
INCOMING user already holds some OTHER 'Entity.Arkham.Player.ArkhamPlayer'
row in the destination game -- in ascending destination-'ArkhamGameId'
order, the same canonical order both games were already locked in (see
'lockSwapGame'), so two concurrent swaps sharing a destination game can
never probe it in opposite orders. Each side's own already-locked row id is
passed as the excluded id purely defensively: once
'validateSwapParticipantUsers' has already confirmed two DISTINCT users,
that row could never itself be the hit (it is recorded under the OTHER
user), but excluding it by id keeps the invariant explicit rather than
implicit. Both probes are always attempted (mirroring 'lockSwapGame'\/
'lockSwapPlayer') before either result is inspected, reporting
'MainStreetSwapDestinationOccupied' if either found a conflict.
-}
checkSwapDestinationOccupancy
  :: MonadMainStreetSwap m
  => ArkhamGameId
  -> PlayerId
  -> ArkhamPlayer
  -> ArkhamGameId
  -> PlayerId
  -> ArkhamPlayer
  -> m (Either MainStreetSwapStateFailure ())
checkSwapDestinationOccupancy firstGameId firstPid firstPlayer secondGameId secondPid secondPlayer = do
  let
    -- (destination game, incoming user, own row id to exclude)
    firstMovesIn = (secondGameId, arkhamPlayerUserId firstPlayer, coerce firstPid)
    secondMovesIn = (firstGameId, arkhamPlayerUserId secondPlayer, coerce secondPid)
    ordered = sortOn (\(destGameId, _, _) -> destGameId) [firstMovesIn, secondMovesIn]
  occupied <- for ordered \(destGameId, incomingUserId, excludedPid) ->
    lookupSwapDestinationOccupant destGameId incomingUserId excludedPid
  pure $ if or occupied then Left MainStreetSwapDestinationOccupied else Right ()

{- | One participant's Epic 'GroupPlayer' membership move: their ordinal
authorization needs to follow them from the group they occupied BEFORE the
swap (@sourceOrdinal@) to the group they occupy AFTER it
(@destinationOrdinal@), mirroring the 'Entity.Arkham.Player.ArkhamPlayer'
row move 'performMainStreetSwap' itself performs. Carries the plain
'UserId' rather than a locked 'ArkhamPlayer' row: once
'validateSwapParticipantUsers' has run, that is the only identity this
move needs.
-}
data MainStreetSwapMembershipMove = MainStreetSwapMembershipMove
  { userId :: UserId
  , sourceOrdinal :: Int
  , destinationOrdinal :: Int
  }
  deriving stock (Eq, Show)

{- | Build the two membership moves a completed swap implies: the first
participant moves from @firstOrdinal@ to @secondOrdinal@, the second moves
the other way. Sorted ascending by 'UserId' -- defense in depth alongside
the exclusive event lock 'lockSwapEvent' already holds -- so two
concurrent swaps could never reconcile the same pair of users' memberships
in opposite orders even if they somehow raced past that lock.
-}
swapMembershipMoves :: Int -> Int -> ArkhamPlayer -> ArkhamPlayer -> [MainStreetSwapMembershipMove]
swapMembershipMoves firstOrdinal secondOrdinal firstPlayer secondPlayer =
  sortOn (.userId)
    [ MainStreetSwapMembershipMove (arkhamPlayerUserId firstPlayer) firstOrdinal secondOrdinal
    , MainStreetSwapMembershipMove (arkhamPlayerUserId secondPlayer) secondOrdinal firstOrdinal
    ]

{- | A legacy seat with no existing 'GroupPlayer' membership row
(@Nothing@) always validates -- there is nothing stale to detect. An
EXISTING row must carry exactly @move.sourceOrdinal@: any other ordinal
means the authorization table and this swap's own assumption about which
group @move.userId@ currently occupies have fallen out of sync (see
'MainStreetSwapMembershipStale'), and this move -- and, by
'reconcileSwapMemberships', the WHOLE swap -- must be rejected before
either participant's row is written.
-}
validateSwapMembershipMove
  :: MainStreetSwapMembershipMove -> Maybe (Entity ArkhamEpicMember) -> Either MainStreetSwapStateFailure ()
validateSwapMembershipMove _ Nothing = Right ()
validateSwapMembershipMove move (Just (Entity _ member)) =
  when (arkhamEpicMemberGroupOrdinal member /= Just move.sourceOrdinal) $
    Left MainStreetSwapMembershipStale

{- | Look up BOTH participants' current 'GroupPlayer' membership rows (via
'lookupSwapMembership', safe once 'lockSwapEvent' already holds this
event's row exclusively) and validate BOTH (via 'validateSwapMembershipMove')
before writing EITHER -- mirroring 'lockAndValidateSwapPlayers''s own
"attempt both, then inspect together" shape. Only once both validate does
'reconcileSwapMembership' actually move each participant's membership
ordinal to their destination group; a single invalid move rejects the
whole swap and performs no write for either participant.
-}
reconcileSwapMemberships
  :: MonadMainStreetSwap m
  => ArkhamEpicEventId -> [MainStreetSwapMembershipMove] -> m (Either MainStreetSwapStateFailure ())
reconcileSwapMemberships eventId moves = do
  existing <- for moves \move -> lookupSwapMembership eventId move.userId
  case traverse (uncurry validateSwapMembershipMove) (zip moves existing) of
    Left failure -> pure (Left failure)
    Right _ -> do
      for_ moves \move -> reconcileSwapMembership eventId move.userId move.destinationOrdinal
      pure (Right ())

instance MonadMainStreetSwap (SqlPersistT Handler) where
  resolveSwapGame eid ordinal = do
    mGroup <-
      P.selectFirst
        [ ArkhamEpicGroupArkhamEpicEventId P.==. eid
        , ArkhamEpicGroupOrdinal P.==. ordinal
        ]
        []
    pure $ mGroup >>= arkhamEpicGroupArkhamGameId . entityVal
  lockSwapGame gid = do
    -- Project the whole row: unlike 'Api.Handler.Arkham.Events.lockLinkedGame'
    -- (which only needs presence, since deletion never reads a game's
    -- content), 'performMainStreetSwap' needs the CURRENT locked state
    -- itself, so re-reading it later -- outside this same lock -- can never
    -- happen. 'FOR UPDATE' locks the whole row regardless of which columns
    -- are selected either way.
    locked <- select do
      game <- from $ table @ArkhamGame
      where_ $ game.id ==. val gid
      locking forUpdate
      pure game
    pure $ entityVal <$> listToMaybe locked
  lockSwapPlayer pid = do
    locked <- select do
      player <- from $ table @ArkhamPlayer
      where_ $ player.id ==. val (coerce pid)
      locking forUpdate
      pure player
    pure $ entityVal <$> listToMaybe locked
  lookupSwapDestinationOccupant gid userId excludedPid =
    isJust
      <$> P.selectFirst
        [ ArkhamPlayerArkhamGameId P.==. gid
        , ArkhamPlayerUserId P.==. userId
        , ArkhamPlayerId P.!=. excludedPid
        ]
        []
  lockSwapEvent = fmap isJust . lockEpicEventRow
  lookupSwapMembership eid userId = P.getBy (UniqueEpicMember eid userId GroupPlayer)
  reconcileSwapMembership eid userId ordinal = do
    existing <- P.getBy (UniqueEpicMember eid userId GroupPlayer)
    case existing of
      Just (Entity mid _) -> P.update mid [ArkhamEpicMemberGroupOrdinal P.=. Just ordinal]
      Nothing -> void $ P.insert (ArkhamEpicMember eid userId GroupPlayer (Just ordinal))
  performMainStreetSwap eventId firstOrdinal secondOrdinal plan firstRaw secondRaw =
    case swapInvestigatorState (arkhamGameCurrentData firstRaw) (arkhamGameCurrentData secondRaw) of
      Left failure -> pure (Left failure)
      Right transform -> do
        -- Game locks (via 'lockSwapGame', already held by the time
        -- 'performMainStreetSwap' is called -- see 'planAndExecuteMainStreetSwap')
        -- come first; the Epic event lock (via 'lockSwapEvent') comes
        -- SECOND -- the same game(s)->event order claim-seat and
        -- pending-join share; participant player-row locks come THIRD
        -- (see 'lockAndValidateSwapPlayers').
        eventStillPresent <- lockSwapEvent eventId
        if not eventStillPresent
          then pure (Left MainStreetSwapEventVanished)
          else do
            playerLockResult <- lockAndValidateSwapPlayers plan.firstGameId plan.secondGameId transform
            case playerLockResult of
              Left failure -> pure (Left failure)
              Right (firstPlayer, secondPlayer) -> do
                let moves = swapMembershipMoves firstOrdinal secondOrdinal firstPlayer secondPlayer
                membershipResult <- reconcileSwapMemberships eventId moves
                case membershipResult of
                  Left failure -> pure (Left failure)
                  Right () -> do
                    let
                      firstStep = arkhamGameStep firstRaw + 1
                      secondStep = arkhamGameStep secondRaw + 1
                    -- 'updateGet' throws if its key does not exist, rather than
                    -- silently affecting zero rows: defense in depth on top of
                    -- the fact that every key updated here was already
                    -- confirmed present and locked, in this same transaction,
                    -- moments ago (games via 'lockSwapGame', players via
                    -- 'lockSwapPlayer'). No key here is ever re-derived from
                    -- anywhere but that same lock.
                    _ <-
                      P.updateGet
                        plan.firstGameId
                        [ArkhamGameCurrentData P.=. transform.firstGame', ArkhamGameStep P.=. firstStep]
                    _ <-
                      P.updateGet
                        plan.secondGameId
                        [ArkhamGameCurrentData P.=. transform.secondGame', ArkhamGameStep P.=. secondStep]
                    _ <- P.updateGet (coerce transform.firstPid) [ArkhamPlayerArkhamGameId P.=. plan.secondGameId]
                    _ <- P.updateGet (coerce transform.secondPid) [ArkhamPlayerArkhamGameId P.=. plan.firstGameId]
                    pure (Right ())

{- | Resolve the ELSE! Main Street group swap (see 'computeMainStreetSwap' for
exactly what state moves between the two games).

This writes both games' persisted state and revision in one transaction
(via 'planAndExecuteMainStreetSwap'/'performMainStreetSwap'), so that
persisted state is never half-visible and can never be crossed by ordinary
undo. Readiness spending, the per-game undo floor, and the websocket
publish that follow (in 'swapMainStreetInvestigators', after 'runDB'
returns) are separate, post-commit steps -- mirroring the same
commit-then-cleanup shape 'Api.Handler.Arkham.Events.deleteApiV1ArkhamEventR'
uses for room cleanup -- not additional writes inside the swap's own
transaction.

The entire plan/lock/execute decision (unknown or unlinked group relations,
two ordinals resolving to one game, both games' 'FOR UPDATE' locks -- taken
in the canonical order 'mainStreetSwapPlan' computes, strictly before either
game's mutable state is read -- and every precondition on the locked state
itself, see 'MainStreetSwapStateFailure') happens inside
'planAndExecuteMainStreetSwap', in one transaction -- see that function's
Haddoc for the full decision sequence, and 'MonadMainStreetSwap' for why
each step is its own testable typeclass method. A caller may request the
swap with either ordinal named first (@firstOrdinal@ larger or smaller than
@secondOrdinal@); sorting only the LOCK ACQUISITION plan, never the
first/second mapping used for the actual swap semantics and response,
closes off a cross-path deadlock: without it, a reversed-order swap request
(say, ordinals 2 then 1) would acquire its first update's implicit row lock
on whichever game ordinal 2 resolves to before whichever game ordinal 1
resolves to. 'mainStreetSwapPlan's 'lockOrder' is ascending by
'ArkhamGameId' ALONE -- never by ordinal, and never dependent on which two
of an event's linked games this swap happened to resolve -- and
'Api.Handler.Arkham.Events.deleteEpicEventAggregate' locks every linked
game in that identical id-ascending order; both writers delegate to the
one shared 'Api.Arkham.Epic.canonicalEpicGameLockOrder', so they can never
disagree about which of any two games they have in common should lock
first, regardless of which subset of an event's games each one resolved
(see that function's Haddock for why an earlier, ordinal-based version of
this order was NOT actually safe here). Acquiring both locks up front, in
the one fixed order every writer uses, means at most one of these
transactions is ever waiting at a time.

Every lookup this swap performs against the two locked games' persisted
state -- the recorded ready investigator, the Main Street location, that
investigator's own entity row, its current placement, cross-game membership
consistency, and each participant's locked
'Entity.Arkham.Player.ArkhamPlayer' row -- is total (see
'swapInvestigatorState', 'validateSwapPlayer'): stale or malformed persisted
state can only ever produce a typed 'MainStreetSwapInvalidState', mapped
below to the same static, non-leaking rejection as 'MainStreetSwapSameGame',
never a crash. Game locks always precede participant player-row locks (see
'MonadMainStreetSwap'), an order this swap shares with no other lock kind in
the codebase, so it can never be acquired in reverse.
-}
swapMainStreetInvestigators :: ArkhamEpicEventId -> Int -> Int -> Handler ()
swapMainStreetInvestigators eventId firstOrdinal secondOrdinal = do
  outcome <- runDB $ planAndExecuteMainStreetSwap eventId firstOrdinal secondOrdinal
  plan <- case outcome of
    MainStreetSwapMissing -> notFound
    MainStreetSwapSameGame -> invalidArgs ["Investigators must be in different groups"]
    -- Deliberately discards the specific 'MainStreetSwapStateFailure':
    -- callers only ever see that the swap could not be performed, matching
    -- 'MainStreetSwapSameGame''s nondisclosure. See
    -- "Arkham.Api.MainStreetSwapSpec" for tests distinguishing each cause.
    MainStreetSwapInvalidState _failure -> invalidArgs ["This Main Street swap can no longer be performed"]
    MainStreetSwapCompleted plan -> pure plan

  runMessagesInGroupWhen
    (const True)
    [SpendShared (MainStreetReady $ GroupOrdinal firstOrdinal) 1]
    plan.firstGameId
  runMessagesInGroupWhen
    (const True)
    [SpendShared (MainStreetReady $ GroupOrdinal secondOrdinal) 1]
    plan.secondGameId
  for_ [plan.firstGameId, plan.secondGameId] \gameId -> do
    raw <- runDB $ P.get404 gameId
    setGameUndoFloor gameId (arkhamGameStep raw)
    publishToRoom gameId
      $ GameUpdate
      $ PublicGame gameId (arkhamGameName raw) [] (arkhamGameCurrentData raw)
  broadcastEventChanged eventId

-- | This game's recorded Main Street ready investigator, if any: 'Nothing'
-- if the group has never activated Main Street (or the metadata was never
-- populated). Depends on the whole 'Game' only for its active
-- scenario\/campaign -- everything else this swap validates is a function
-- of 'Entities' alone (see 'mainStreetLocation', 'resolveSwapInvestigator').
readyInvestigator :: Game -> Maybe InvestigatorId
readyInvestigator game = case gameMode game of
  That scenario -> scenarioReady scenario
  These _ scenario -> scenarioReady scenario
  This _ -> Nothing
 where
  scenarioReady :: Scenario -> Maybe InvestigatorId
  scenarioReady scenario = getMetaKeyDefault "mainStreetReady" Nothing (toAttrs scenario)

{- | The Main Street location (card @89006@) in these entities, if any.

This is never looked up by a caller-supplied key at all -- it is found
purely by searching 'Arkham.Entities.entitiesLocations' for a matching card
code (via 'toList', i.e. 'Data.Foldable.toList'\/'Map.elems', which discards
key identity), so a mismatched key\/id pair on this SPECIFIC entry cannot
make this function itself return the wrong id -- it always returns the
matched VALUE's own internal id. 'computeMainStreetSwap' also never re-keys
or moves any location entry (only investigators and their owned
assets\/treacheries\/events\/effects move between games -- see
'ownedEntities'\/'removeOwned'\/'addOwned', none of which touch
'Arkham.Entities.entitiesLocations' at all), so no analogous transform-side
bug is possible for locations either. That narrow safety argument is about
THIS function only, though: it says nothing about whether some OTHER,
unrelated location or investigator entry elsewhere in either map is itself
mismatched -- 'validateEntityMapIdentity' checks that, globally, for both
maps in both games, before 'swapInvestigatorState' ever calls this.
-}
mainStreetLocation :: Entities -> Maybe LocationId
mainStreetLocation entities =
  (.id) <$> find ((== CardCode "89006") . toCardCode) (toList $ entitiesLocations entities)

{- | Whether every entry in an 'Arkham.Entities.EntityMap' is internally
consistent: the map KEY exactly equals that entry's own VALUE's internal
id. Checked via key\/value PAIRS ('Map.toList'), never via
'Data.Foldable.toList'\/'Map.elems' alone (which discards the key entirely,
as 'mainStreetLocation' above does deliberately for its own narrower
purpose) -- so this genuinely proves the map-wide invariant
'MainStreetSwapInvestigatorKeyMismatch' historically only checked for one
specific (participant) entry.
-}
entityMapKeysMatchIds :: Eq key => (value -> key) -> Map key value -> Bool
entityMapKeysMatchIds keyOf = all (\(k, v) -> k == keyOf v) . Map.toList

{- | Reject either game's 'Arkham.Entities.entitiesInvestigators' or
'Arkham.Entities.entitiesLocations' containing ANY entry -- participant or
not -- whose map key disagrees with that entry's own internal id (see
'MainStreetSwapInvalidEntityMap'). Without this, a NONPARTICIPANT
investigator or location stored under a stale, unrelated key could let an
arriving investigator sharing that VALUE's internal id silently duplicate
it once 'computeMainStreetSwap''s @addOwned@ re-inserts the arriving
investigator under its OWN (correct) id: the existing collision check in
'validateSwapSide' only inspects 'Map.member' against the CURRENT (possibly
already-inconsistent) key set, so it cannot detect this on its own.
Checked, for BOTH games, strictly BEFORE 'resolveSwapParticipants' or any
other readiness\/placement\/membership check ever runs, so
'swapInvestigatorState' never even attempts to resolve a participant out
of an already-inconsistent map.
-}
validateEntityMapIdentity :: Game -> Either MainStreetSwapStateFailure ()
validateEntityMapIdentity game = do
  let entities = gameEntities game
  unless (entityMapKeysMatchIds (.id) (entitiesInvestigators entities)) $ Left MainStreetSwapInvalidEntityMap
  unless (entityMapKeysMatchIds (.id) (entitiesLocations entities)) $ Left MainStreetSwapInvalidEntityMap

{- | Resolve both sides' recorded ready investigator ids, rejecting either
side being absent ('MainStreetSwapNotReady') or both sides naming the SAME
investigator id across two different games ('MainStreetSwapDuplicateInvestigator'
-- possible because 'InvestigatorId' is a 'CardCode', shared by any two
games that both happen to include that investigator card). Pure and total,
independent of 'Game': directly unit testable with synthetic 'Maybe'
'InvestigatorId' values -- see "Arkham.Api.MainStreetSwapSpec".
-}
resolveSwapParticipants
  :: Maybe InvestigatorId -> Maybe InvestigatorId -> Either MainStreetSwapStateFailure (InvestigatorId, InvestigatorId)
resolveSwapParticipants mFirstIid mSecondIid = do
  firstIid <- maybe (Left MainStreetSwapNotReady) Right mFirstIid
  secondIid <- maybe (Left MainStreetSwapNotReady) Right mSecondIid
  when (firstIid == secondIid) $ Left MainStreetSwapDuplicateInvestigator
  pure (firstIid, secondIid)

{- | Resolve one ready investigator's OWN entity row out of its game's
'Entities', rejecting an id that is not present there
('MainStreetSwapInvestigatorMissing') -- stale, malformed, or an
investigator that has since left this specific game -- and, immediately
after, rejecting a resolved 'Investigator' whose OWN internal id disagrees
with the map key ('iid') it was looked up under
('MainStreetSwapInvestigatorKeyMismatch'). This second check matters
because 'computeMainStreetSwap''s @addOwned@ re-inserts an arriving
investigator under its OWN internal id, never under the key it was
resolved through -- so a mismatched map entry would otherwise leave the
destination game's entities map keyed under a DIFFERENT id than every
other membership list ('gamePlayerOrder', 'gamePlayers',
'Entity.Arkham.Player.ArkhamPlayer' locking) this swap keys off of,
silently diverging rather than crashing. Checked here, before either side
is even compared for placement or membership, so neither
'swapInvestigatorState' nor 'computeMainStreetSwap' ever operates on such a
value.
-}
resolveSwapInvestigator :: InvestigatorId -> Entities -> Either MainStreetSwapStateFailure Investigator
resolveSwapInvestigator iid entities = do
  investigator <- maybe (Left MainStreetSwapInvestigatorMissing) Right (Map.lookup iid (entitiesInvestigators entities))
  when (investigator.id /= iid) $ Left MainStreetSwapInvestigatorKeyMismatch
  pure investigator

{- | Confirm a resolved investigator's CURRENT placement -- re-checked after
both locks were taken -- is still the Main Street destination this swap
expects, rejecting a stale/moved investigator with
'MainStreetSwapInvestigatorMoved' otherwise.
-}
validateSwapPlacement :: LocationId -> Investigator -> Either MainStreetSwapStateFailure ()
validateSwapPlacement destination investigator =
  when (attr investigatorPlacement investigator /= AtLocation destination) $ Left MainStreetSwapInvestigatorMoved

-- | Reject the two participants' player ids being the same. Nothing in the
-- persisted 'Game' JSON structurally prevents this the way
-- 'Entity.Arkham.Player.UniquePlayer' prevents it at the database level --
-- checked here, defensively, before either game's membership is touched.
rejectDuplicatePlayers :: PlayerId -> PlayerId -> Either MainStreetSwapStateFailure ()
rejectDuplicatePlayers firstPid secondPid =
  when (firstPid == secondPid) $ Left MainStreetSwapDuplicatePlayer

-- | Whether @x@ appears in @xs@ EXACTLY once: neither missing nor
-- duplicated. Used by 'validateSwapSide' to require a swap's outgoing
-- participant be represented singularly in a game's own membership lists.
exactlyOnce :: Eq a => a -> [a] -> Bool
exactlyOnce x xs = length (filter (== x) xs) == 1

{- | Confirm ONE side of a swap is safe to apply to ONE game: the outgoing
participant (about to leave THIS game) is genuinely, singularly present in
every membership list this game keeps -- its own entities map, player
order, and players list -- never missing, never duplicated ('MainStreetSwapParticipantInconsistent');
and, once that outgoing participant is accounted for, the INCOMING
participant (about to arrive from the other side) is not already present in
any of them ('MainStreetSwapIncomingCollision' -- e.g. a third, non-ready
investigator already in this game happens to share a card code with the
one arriving). Checked against the game's CURRENT, pre-transform state, so
'computeMainStreetSwap''s own 'Map.insert'\/list-append can never silently
overwrite or duplicate an unrelated investigator.
-}
validateSwapSide
  :: InvestigatorId -> PlayerId -> InvestigatorId -> PlayerId -> Game -> Either MainStreetSwapStateFailure ()
validateSwapSide outgoingIid outgoingPid incomingIid incomingPid game = do
  let investigators = entitiesInvestigators (gameEntities game)
  unless (Map.member outgoingIid investigators) $ Left MainStreetSwapParticipantInconsistent
  unless (exactlyOnce outgoingIid (gamePlayerOrder game)) $ Left MainStreetSwapParticipantInconsistent
  unless (exactlyOnce outgoingPid (gamePlayers game)) $ Left MainStreetSwapParticipantInconsistent
  let
    investigatorsAfterRemoval = Map.delete outgoingIid investigators
    orderAfterRemoval = filter (/= outgoingIid) (gamePlayerOrder game)
    playersAfterRemoval = filter (/= outgoingPid) (gamePlayers game)
  when (Map.member incomingIid investigatorsAfterRemoval) $ Left MainStreetSwapIncomingCollision
  when (incomingIid `elem` orderAfterRemoval) $ Left MainStreetSwapIncomingCollision
  when (incomingPid `elem` playersAfterRemoval) $ Left MainStreetSwapIncomingCollision

-- | Apply 'validateSwapSide' to BOTH games this swap touches: each game is
-- the destination for the OTHER side's incoming participant, and the
-- source its own outgoing participant is leaving.
validateSwapMembership
  :: InvestigatorId
  -> PlayerId
  -> Game
  -> InvestigatorId
  -> PlayerId
  -> Game
  -> Either MainStreetSwapStateFailure ()
validateSwapMembership firstIid firstPid firstGame secondIid secondPid secondGame = do
  validateSwapSide firstIid firstPid secondIid secondPid firstGame
  validateSwapSide secondIid secondPid firstIid firstPid secondGame

{- | Confirm one participant's LOCKED 'Entity.Arkham.Player.ArkhamPlayer' row
(see 'lockSwapPlayer' -- never a separate, unlocked re-read) is genuinely
usable for this swap: present (rejecting 'Nothing', whether the row vanished
concurrently or never existed), currently linked to the EXPECTED source
game ('MainStreetSwapPlayerWrongGame' otherwise), and recorded under the
EXPECTED investigator id ('MainStreetSwapPlayerMismatch' otherwise) -- the
authorization table and the persisted 'Game' JSON this swap's investigator
data comes from are two independent sources of truth, and this is the one
place they are cross-checked against each other before either is trusted
for a write.
-}
validateSwapPlayer
  :: ArkhamGameId -> InvestigatorId -> Maybe ArkhamPlayer -> Either MainStreetSwapStateFailure ArkhamPlayer
validateSwapPlayer expectedGameId expectedIid mPlayer = do
  player <- maybe (Left MainStreetSwapPlayerMissing) Right mPlayer
  unless (arkhamPlayerArkhamGameId player == expectedGameId) $ Left MainStreetSwapPlayerWrongGame
  unless (arkhamPlayerInvestigatorId player == coerce expectedIid) $ Left MainStreetSwapPlayerMismatch
  pure player

{- | Everything 'performMainStreetSwap' needs to actually persist a validated
swap: both games' new state, and each side's resolved participant identity
(needed to then lock and validate its 'Entity.Arkham.Player.ArkhamPlayer'
row -- see 'validateSwapPlayer' -- before any write). Produced only once
EVERY precondition on the two locked games' CURRENT state -- readiness,
location, investigator resolution, placement, player-id distinctness, and
cross-game membership consistency -- has already checked out.
-}
data MainStreetSwapTransform = MainStreetSwapTransform
  { firstGame' :: Game
  , secondGame' :: Game
  , firstIid :: InvestigatorId
  , firstPid :: PlayerId
  , secondIid :: InvestigatorId
  , secondPid :: PlayerId
  }
  deriving stock (Eq, Show)

{- | Validate and compute a Main Street swap's new state from both locked
games' CURRENT (freshly locked, never re-read) data. Total: every lookup
that used to be a partial 'error'\/'fromMaybe (error ...)' call (an absent
ready investigator, a missing Main Street location, an unresolvable
investigator entity, two ordinals naming the same investigator, or a
since-moved investigator) now returns a typed 'MainStreetSwapStateFailure'
instead, checked in this fixed order, so a caller can never observe a crash
from stale or malformed persisted state -- only ever a typed 'Left', mapped
by 'swapMainStreetInvestigators' to a static, non-leaking rejection. This
covers every precondition expressible from the two 'Game' payloads alone;
the participant 'Entity.Arkham.Player.ArkhamPlayer' rows themselves are
locked and cross-checked separately, by 'performMainStreetSwap', against
the 'MainStreetSwapTransform' this returns.

The VERY FIRST checks ('validateEntityMapIdentity', on BOTH games) reject
any investigator or location map already keyed inconsistently with its own
entries' internal ids -- participant or not -- before even attempting to
resolve which investigator is "ready": a narrower, participant-only version
of this check ('MainStreetSwapInvestigatorKeyMismatch', inside
'resolveSwapInvestigator') still runs later purely for defense in depth,
but by that point a global inconsistency has already been rejected.
-}
swapInvestigatorState :: Game -> Game -> Either MainStreetSwapStateFailure MainStreetSwapTransform
swapInvestigatorState firstGame secondGame = do
  validateEntityMapIdentity firstGame
  validateEntityMapIdentity secondGame
  (firstIid, secondIid) <- resolveSwapParticipants (readyInvestigator firstGame) (readyInvestigator secondGame)
  firstDestination <- maybe (Left MainStreetSwapNoLocation) Right (mainStreetLocation (gameEntities secondGame))
  secondDestination <- maybe (Left MainStreetSwapNoLocation) Right (mainStreetLocation (gameEntities firstGame))
  firstInvestigator <- resolveSwapInvestigator firstIid (gameEntities firstGame)
  secondInvestigator <- resolveSwapInvestigator secondIid (gameEntities secondGame)
  validateSwapPlacement secondDestination firstInvestigator
  validateSwapPlacement firstDestination secondInvestigator
  let
    firstPid = attr investigatorPlayerId firstInvestigator
    secondPid = attr investigatorPlayerId secondInvestigator
  rejectDuplicatePlayers firstPid secondPid
  validateSwapMembership firstIid firstPid firstGame secondIid secondPid secondGame
  let
    (firstGame'', secondGame'', _, _) =
      computeMainStreetSwap
        firstIid
        firstDestination
        firstInvestigator
        firstGame
        secondIid
        secondDestination
        secondInvestigator
        secondGame
  pure
    MainStreetSwapTransform
      { firstGame' = firstGame''
      , secondGame' = secondGame''
      , firstIid
      , firstPid
      , secondIid
      , secondPid
      }

{- | The actual swap, given both sides' already-validated participant and
destination. Total by construction: every value this consumes was already
confirmed present and consistent by 'swapInvestigatorState', so this never
needs to fail. InvestigatorAttrs contains the investigator's deck, hand,
discard, resources, damage, trauma, logs, and other personal state. We
additionally move every controlled/play-area asset, threat-area treachery,
controlled event, per-investigator entity cache, history, question, and
player authorization row. Enemy cards stay behind because the printed
ability disengages them before publishing readiness.
-}
computeMainStreetSwap
  :: InvestigatorId
  -> LocationId
  -> Investigator
  -> Game
  -> InvestigatorId
  -> LocationId
  -> Investigator
  -> Game
  -> (Game, Game, PlayerId, PlayerId)
computeMainStreetSwap firstIid firstDestination firstInvestigator firstGame secondIid secondDestination secondInvestigator secondGame =
  ( install secondIid secondMoved secondOwned secondPid (remove firstIid firstPid firstGame)
  , install firstIid firstMoved firstOwned firstPid (remove secondIid secondPid secondGame)
  , firstPid
  , secondPid
  )
 where
  firstPid = attr investigatorPlayerId firstInvestigator
  secondPid = attr investigatorPlayerId secondInvestigator
  -- Each arriving investigator lands at the destination game's OWN Main
  -- Street location: 'firstInvestigator' arrives in 'secondGame', so it is
  -- placed at 'firstDestination' (== mainStreetLocation secondGame, per
  -- 'swapInvestigatorState'); symmetrically for 'secondInvestigator'.
  firstMoved = overAttrs (\a -> a {investigatorPlacement = AtLocation firstDestination}) firstInvestigator
  secondMoved = overAttrs (\a -> a {investigatorPlacement = AtLocation secondDestination}) secondInvestigator
  firstOwned = ownedEntities firstIid (gameEntities firstGame)
  secondOwned = ownedEntities secondIid (gameEntities secondGame)

  remove iid pid game =
    game
      { gameEntities = removeOwned iid (gameEntities game)
      , gamePlayers = filter (/= pid) (gamePlayers game)
      , gamePlayerOrder = filter (/= iid) (gamePlayerOrder game)
      , gameInHandEntities = Map.delete iid (gameInHandEntities game)
      , gameInDiscardEntities = Map.delete iid (gameInDiscardEntities game)
      , gamePhaseHistory = Map.delete iid (gamePhaseHistory game)
      , gameTurnHistory = Map.delete iid (gameTurnHistory game)
      , gameRoundHistory = Map.delete iid (gameRoundHistory game)
      , gameQuestion = Map.delete pid (gameQuestion game)
      , gameModifiers = Map.delete (InvestigatorTarget iid) (gameModifiers game)
      , gameCardUses = Map.map (filter (/= iid)) (gameCardUses game)
      }

  install iid investigator owned pid game =
    game
      { gameEntities = addOwned investigator owned (gameEntities game)
      , gamePlayers = gamePlayers game <> [pid]
      , gamePlayerOrder = gamePlayerOrder game <> [iid]
      , gameInHandEntities =
          copyMapEntry
            iid
            (if iid == firstIid then gameInHandEntities firstGame else gameInHandEntities secondGame)
            (gameInHandEntities game)
      , gameInDiscardEntities =
          copyMapEntry
            iid
            (if iid == firstIid then gameInDiscardEntities firstGame else gameInDiscardEntities secondGame)
            (gameInDiscardEntities game)
      , gamePhaseHistory =
          copyMapEntry
            iid
            (if iid == firstIid then gamePhaseHistory firstGame else gamePhaseHistory secondGame)
            (gamePhaseHistory game)
      , gameTurnHistory =
          copyMapEntry
            iid
            (if iid == firstIid then gameTurnHistory firstGame else gameTurnHistory secondGame)
            (gameTurnHistory game)
      , gameRoundHistory =
          copyMapEntry
            iid
            (if iid == firstIid then gameRoundHistory firstGame else gameRoundHistory secondGame)
            (gameRoundHistory game)
      , gameQuestion =
          copyMapEntry
            pid
            (if pid == firstPid then gameQuestion firstGame else gameQuestion secondGame)
            (gameQuestion game)
      , gameModifiers =
          copyMapEntry
            (InvestigatorTarget iid)
            (if iid == firstIid then gameModifiers firstGame else gameModifiers secondGame)
            (gameModifiers game)
      , gameCardUses =
          transferCardUses
            iid
            (if iid == firstIid then gameCardUses firstGame else gameCardUses secondGame)
            (gameCardUses game)
      , gameActiveInvestigatorId = replaceId firstIid secondIid iid (gameActiveInvestigatorId game)
      , gameTurnPlayerInvestigatorId =
          replaceId firstIid secondIid iid <$> gameTurnPlayerInvestigatorId game
      , gameLeadInvestigatorId = replaceId firstIid secondIid iid (gameLeadInvestigatorId game)
      , gameActivePlayerId =
          if gameActivePlayerId game `elem` [firstPid, secondPid] then pid else gameActivePlayerId game
      }

  replaceId removedA removedB inserted current
    | current == removedA || current == removedB = inserted
    | otherwise = current

transferCardUses
  :: InvestigatorId
  -> Map CardCode [InvestigatorId]
  -> Map CardCode [InvestigatorId]
  -> Map CardCode [InvestigatorId]
transferCardUses iid source destination =
  Map.unionWith
    (<>)
    (Map.map (const [iid]) $ Map.filter (elem iid) source)
    (Map.map (filter (/= iid)) destination)

copyMapEntry :: Ord key => key -> Map key value -> Map key value -> Map key value
copyMapEntry key source destination = maybe destination (\value -> Map.insert key value destination) (Map.lookup key source)

ownedEntities :: InvestigatorId -> Entities -> Entities
ownedEntities iid entities =
  mempty
    { entitiesAssets = Map.filter (assetBelongsTo iid) (entitiesAssets entities)
    , entitiesTreacheries =
        Map.filter
          ((`elem` [InThreatArea iid, AttachedToInvestigator iid]) . attr treacheryPlacement)
          (entitiesTreacheries entities)
    , entitiesEvents = Map.filter ((== iid) . attr eventController) (entitiesEvents entities)
    , entitiesEffects =
        Map.filter ((== InvestigatorTarget iid) . attr effectTarget) (entitiesEffects entities)
    }

removeOwned :: InvestigatorId -> Entities -> Entities
removeOwned iid entities =
  entities
    { entitiesInvestigators = Map.delete iid (entitiesInvestigators entities)
    , entitiesAssets = Map.filter (not . assetBelongsTo iid) (entitiesAssets entities)
    , entitiesTreacheries =
        Map.filter
          (not . (`elem` [InThreatArea iid, AttachedToInvestigator iid]) . attr treacheryPlacement)
          (entitiesTreacheries entities)
    , entitiesEvents = Map.filter ((/= iid) . attr eventController) (entitiesEvents entities)
    , entitiesEffects =
        Map.filter ((/= InvestigatorTarget iid) . attr effectTarget) (entitiesEffects entities)
    }

addOwned :: Investigator -> Entities -> Entities -> Entities
addOwned investigator owned entities =
  entities
    { entitiesInvestigators = Map.insert investigator.id investigator (entitiesInvestigators entities)
    , entitiesAssets = entitiesAssets owned <> entitiesAssets entities
    , entitiesTreacheries = entitiesTreacheries owned <> entitiesTreacheries entities
    , entitiesEvents = entitiesEvents owned <> entitiesEvents entities
    , entitiesEffects = entitiesEffects owned <> entitiesEffects entities
    }

assetBelongsTo :: InvestigatorId -> Asset -> Bool
assetBelongsTo iid asset =
  attr assetController asset
    == Just iid
    || attr assetOwner asset
    == Just iid
    || attr assetPlacement asset
    `elem` [InPlayArea iid, InThreatArea iid, StillInHand iid, AttachedToInvestigator iid]

setGameUndoFloor :: ArkhamGameId -> Int -> Handler ()
setGameUndoFloor gid step =
  runDB
    $ void
    $ P.upsertBy
      (UniqueGameUndoFloor gid)
      (ArkhamGameUndoFloor gid step)
      [ArkhamGameUndoFloorFloorStep P.=. step]

{- | Server-initiated sync of one (other) group's game state to the current
shared counters, so its BOARD (countermeasures, blob health) reflects the
change live without that group having to take an action. Runs only the sync
messages (the group's own pending queue/question is preserved), persists a new
step, and broadcasts the resulting GameUpdate. Skips games that aren't active.
-}
syncOneGroup :: GroupOrdinal -> SharedEventState -> ArkhamGameId -> Handler ()
syncOneGroup ordinal shared = runMessagesInGroup (epicSyncMessages ordinal shared)

{- | Propagate a shared-state change across an event: update every client's
shared store (organizer dashboard/bars) AND sync each group's game-state board
to it. @mOrigin@ (the acting group) is skipped — its own action already
reflected the change locally.
-}
propagateShared :: ArkhamEpicEventId -> Maybe ArkhamGameId -> SharedEventState -> Handler ()
propagateShared eid mOrigin shared = do
  broadcastSharedToEvent eid shared
  groups <- getEventGroupGroups eid
  for_ groups \(ordinal, gid) ->
    when (Just gid /= mOrigin)
      $ syncOneGroup (GroupOrdinal ordinal) shared gid
      `catch` \(e :: SomeException) ->
        $(logWarn) $ "Epic syncOneGroup failed for " <> tshow gid <> ": " <> tshow e

{- | Identity fingerprint of the act(s) in play, used to detect an IN-GROUP act
advance (the act entity is replaced on advance/loop) so 'updateGame' can set the
per-game undo floor. Acts in play is normally a singleton.
-}
epicActFingerprint :: Game -> [ActId]
epicActFingerprint game = sort [attr (.id) act | act <- toList (entitiesActs (gameEntities game))]

{- | Floor undo for EVERY group in the event at its CURRENT persistence step,
making a consuming act advance a global checkpoint: no group can undo across it (so
no contributor can rewind a now-consumed pool placement), while every group's
actions AFTER it stay undoable. Floors are monotonic (always set at a later step
than any prior floor). Called only after the organizer consumes the pool in
'settleOrganizerAdvance'.
-}
floorAllGroupsAtCurrentStep :: ArkhamEpicEventId -> Handler ()
floorAllGroupsAtCurrentStep eid = do
  gameIds <- getEventGroupGameIds eid
  for_ gameIds \gid -> do
    mStep <- runDB $ selectOne do
      g <- from $ table @ArkhamGame
      where_ $ g.id ==. val gid
      pure g.step
    for_ mStep \(Value step) -> setGameUndoFloor gid step

{- | Each group's @(ordinal, contribution)@ toward a stage-@stage@ advance, read
from the authoritative shared 'ActContribution' counters (mirrored from the
contributing acts). Shaped for the organizer endpoint to cap each group's spend.
-}
getEventGroupContributions :: ArkhamEpicEventId -> Int -> Handler [(Int, Int)]
getEventGroupContributions eid stage = do
  mEvent <- runDB $ selectOne do
    e <- from $ table @ArkhamEpicEvent
    where_ $ e.id ==. val eid
    pure e
  case mEvent of
    Nothing -> pure []
    Just (Entity _ event) -> do
      let shared = arkhamEpicEventSharedState event
      groups <- getEventGroupGroups eid
      pure
        [ (ordinal, sharedCounter (ActContribution stage (GroupOrdinal ordinal)) shared)
        | (ordinal, _gid) <- groups
        ]

{- | Apply an organizer's act-advance allocation for a stage. Atomic + idempotent:
under the event @FOR UPDATE@ lock, ONLY if @AwaitingOrganizer stage == 1@ (so a
double-submit no-ops), it writes each group's 'ActSpend', resets the pool, bumps
'ActAdvanceGen', and clears 'AwaitingOrganizer'.

CRITICAL ORDERING when it applied: mirror the new shared state into EVERY group's
replica FIRST (so the parked group's advance handler can read its 'ActSpend' from
its replica), THEN floor every group at the consumption checkpoint, and only THEN
broadcast — the broadcast clears 'AwaitingOrganizer' on the event store, which lifts
the organizer overlay and lets the parked group proceed. No gameplay message is ever
injected into any group; the seam moves shared counters only.
-}
settleOrganizerAdvance :: ArkhamEpicEventId -> Int -> Map Int Int -> Handler ()
settleOrganizerAdvance eid stage spendByOrdinal = do
  (newState, applied) <- runDB $ modifySharedStateLockedWith eid \st ->
    if sharedCounter (AwaitingOrganizer stage) st /= 1
      then (st, False)
      else
        let
          withSpends =
            foldl'
              (\acc (ordinal, spend) -> setSharedCounter (ActSpend stage (GroupOrdinal ordinal)) spend acc)
              st
              (Map.toList spendByOrdinal)
          st' =
            setSharedCounter (AwaitingOrganizer stage) 0
              . updateSharedCounter (+ 1) (ActAdvanceGen stage)
              . setSharedCounter (SharedActProgress stage) 0
              $ withSpends
         in
          (st', True)
  when applied do
    -- (1) mirror into every group's replica BEFORE lifting the overlay
    groups <- getEventGroupGroups eid
    for_ groups \(ordinal, gid) ->
      syncOneGroup (GroupOrdinal ordinal) newState gid
        `catch` \(e :: SomeException) ->
          $(logWarn) $ "Epic settle mirror failed for " <> tshow gid <> ": " <> tshow e
    -- (2) global undo checkpoint: no group can rewind across the consumption
    floorAllGroupsAtCurrentStep eid
    -- (3) broadcast LAST: clears AwaitingOrganizer -> lifts the overlay
    broadcastSharedToEvent eid newState

toGameDetailsEntry :: Entity ArkhamGameRaw -> Int -> GameDetailsEntry
toGameDetailsEntry (Entity gameId game) playerCount =
  case fromJSON @Game (arkhamGameRawCurrentData game) of
    Success a ->
      let
        investigators =
          map (\(i :: Investigator) -> InvestigatorDetails i.id i.classSymbol)
            $ toList a.gameEntities.investigators
        variant = arkhamGameRawMultiplayerVariant game
       in
        SuccessGameDetails
          $ GameDetails
            { id = coerce gameId
            , scenario = case a.gameMode of
                This _ -> Nothing
                That s ->
                  Just
                    $ ScenarioDetails
                      s.id
                      s.difficulty
                      s.name
                      (getMetaKeyDefault "variant" Nothing $ toAttrs s)
                These _ s ->
                  Just
                    $ ScenarioDetails
                      s.id
                      s.difficulty
                      s.name
                      (getMetaKeyDefault "variant" Nothing $ toAttrs s)
            , campaign = case a.gameMode of
                This c -> Just $ CampaignDetails c.id c.difficulty c.currentCampaignMode
                That _ -> Nothing
                These c _ -> Just $ CampaignDetails c.id c.difficulty c.currentCampaignMode
            , gameState = a.gameGameState
            , name = arkhamGameRawName game
            , investigators
            , otherInvestigators =
                let
                  ins = case a.gameMode of
                    This c -> campaignOtherInvestigators (toJSON c.meta)
                    That _ -> mempty
                    These c _ -> campaignOtherInvestigators (toJSON c.meta)
                 in
                  map (\i -> InvestigatorDetails i.id i.classSymbol) ins
            , multiplayerVariant = variant
            , hasOpenSeats = variant == WithFriends && playerCount < length investigators
            }
    Error e -> FailedGameDetails ("Failed to load " <> tshow gameId <> ": " <> T.pack e)
 where
  campaignOtherInvestigators j = case parse (withObject "" (.: "otherCampaignAttrs")) j of
    Error _ -> mempty
    Success (attrs :: CampaignAttrs) -> map (`lookupInvestigator` PlayerId nil) $ Map.keys attrs.decks

{- | Force-drop a room when the underlying game is deleted, regardless of who is
still connected. Unlike 'releaseGameRoomIfEmpty' this ignores the subscriber
count, but it must still tear the Redis subscription down or the controller
keeps a callback for a channel nothing will ever publish to again.

Runs strictly AFTER 'Api.Handler.Arkham.Events.deleteApiV1ArkhamEventR' has
already committed the deletion via 'runDB': a teardown failure here can
never roll back, and must never fake, that already-committed success. See
'RoomCleanupOutcome' for what this reports, and 'attemptRoomTeardown' for
the exact per-room decision, factored out so tests can exercise it
directly against a stubbed, deliberately-failing unsubscribe action,
without a live server.
-}
deleteRoom :: ArkhamGameId -> Handler RoomCleanupOutcome
deleteRoom = forceDeleteRoom "game" appGameRooms

deleteEventRoom :: ArkhamEpicEventId -> Handler RoomCleanupOutcome
deleteEventRoom = forceDeleteRoom "event" appEventRooms

{- | The typed result of one 'forceDeleteRoom' attempt, reported to the caller
instead of being silently swallowed. No bounded-retry loop is layered on
top of this: the codebase's one existing retry abstraction
('Api.Arkham.Lifecycle''s 'ManagedCleanup'\/'ManagedReleasePlan') exists to
compose ordered release steps across acquired AWS-credential-supervisor
resources, an unrelated domain with its own acquisition ordering
invariants -- forcing that machinery onto a single best-effort Redis
unsubscribe would be overengineering, not reuse. Instead,
'RoomCleanupUnsubscribeFailed' deliberately leaves the room reachable (see
'attemptRoomTeardown') so a future manual or background cleanup pass has
something to retry against, rather than fabricating new scheduling
infrastructure this fix does not need.
-}
data RoomCleanupOutcome
  = -- | No room was tracked under this key at all (already cleaned up, or
    -- this game\/event never had any live subscriber to begin with).
    RoomCleanupAbsent
  | -- | A room was present and its subscription was torn down
    -- successfully; it has been removed from the rooms map.
    RoomCleanupClean
  | -- | A room was present but tearing down its subscription failed
    -- synchronously (see 'attemptRoomTeardown' for why only synchronous
    -- failures are ever caught here). The room is deliberately left IN
    -- the map rather than dropped.
    RoomCleanupUnsubscribeFailed SomeException
  deriving stock Show

forceDeleteRoom :: (Ord k, Show k) => Text -> (App -> MVar (Map k Room)) -> k -> Handler RoomCleanupOutcome
forceDeleteRoom kind roomsOf key = do
  roomsVar <- getsYesod roomsOf
  outcome <- liftIO $ modifyMVar roomsVar (attemptRoomTeardown key)
  case outcome of
    RoomCleanupUnsubscribeFailed e ->
      $(logWarn)
        $ "Epic post-deletion room cleanup failed for "
        <> kind
        <> " "
        <> tshow key
        <> ": "
        <> tshow e
    RoomCleanupAbsent -> pure ()
    RoomCleanupClean -> pure ()
  pure outcome

{- | The exact per-room teardown 'forceDeleteRoom' performs, factored out to
operate directly on the rooms map (never on the 'MVar' or 'App' itself) so
tests can exercise this SAME decision against a constructed 'Room' with a
stubbed, deliberately-failing unsubscribe action -- no live server, 'MVar',
or 'App' required.

'UE.tryAny' (via the unqualified 'tryAny' already imported here) only
ever catches a genuine, synchronous teardown failure -- exactly like
'tryRedis_' \/ 'releaseRoomIfEmpty' -- so a caller's own cancellation (e.g.
the admin request driving this deletion itself being torn down mid-flight)
always propagates unchanged rather than being mistaken for a completed
cleanup.

The room is removed from the returned map ONLY when teardown actually
succeeded, or there was nothing tracked to begin with. On a synchronous
failure it is deliberately left IN the map: dropping it would discard the
only reachable handle to its still-registered callback, turning an
outstanding, retryable leak into a permanently unretryable one.
-}
attemptRoomTeardown :: Ord k => k -> Map k Room -> IO (Map k Room, RoomCleanupOutcome)
attemptRoomTeardown key rooms = case Map.lookup key rooms of
  Nothing -> pure (rooms, RoomCleanupAbsent)
  Just r -> do
    result <- tryAny $ join $ readTVarIO (roomUnsubscribe r)
    pure $ case result of
      Right () -> (Map.delete key rooms, RoomCleanupClean)
      Left e -> (rooms, RoomCleanupUnsubscribeFailed e)
