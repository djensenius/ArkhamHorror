{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE NoFieldSelectors #-}

{- | Epic Multiplayer HTTP + websocket handlers.

An "event" owns N group games (each an ordinary 'ArkhamGame', reached through
the existing @/games/:id@ endpoints) plus the shared state. Milestone 1 wires
creation, a read-only dashboard payload, the per-event websocket feed, and a
shared counters, organizer coordination, and live group/event synchronization.
-}
module Api.Handler.Arkham.Events (
  getApiV1ArkhamEventsR,
  postApiV1ArkhamEventsR,
  getApiV1ArkhamEventR,
  deleteApiV1ArkhamEventR,
  postApiV1ArkhamEventCounterR,
  postApiV1ArkhamEventTimeUpR,
  postApiV1ArkhamEventReadyR,
  postApiV1ArkhamEventResolveAdvanceR,
  postApiV1ArkhamEventReplicateR,
  postApiV1ArkhamEventSwapMainStreetR,
  deduplicateEventMemberships,
  withEventMember,
  PendingGroupGame (..),
  buildGroupGame,
  preparePendingGroupGames,
  MonadEpicPersistence (..),
  createEpicEventAggregate,
  gameScenarioMetaDefault,
) where

import Api.Arkham.Epic (applyEpicDeltasLocked, modifySharedStateLocked)
import Api.Arkham.Helpers
import Api.Arkham.Types.MultiplayerVariant (MultiplayerVariant (WithFriends))
import Api.Handler.Arkham.Games.Shared (
  broadcastSharedToEvent,
  deleteEventRoom,
  deleteRoom,
  getEventGroupContributions,
  getEventGroupGameIds,
  propagateShared,
  runMessagesInGroupWhen,
  settleOrganizerAdvance,
  streamRoom,
  swapMainStreetInvestigators,
  websocketConnectionOptions,
 )
import Arkham.Agenda.CardDefs.TheBlobThatAteEverything qualified as Agendas
import Arkham.Agenda.Sequence qualified as Agenda
import Arkham.Agenda.Types (agendaSequence)
import Arkham.Card.CardCode (CardCode (..), HasCardCode (toCardCode))
import Arkham.Classes.Entity (attr, toAttrs)
import Arkham.Difficulty (Difficulty)
import Arkham.Entities (Entities (..), entitiesAgendas)
import Arkham.Epic.Types
import Arkham.Game (
  Game,
  gameEntities,
  gameGameState,
  gameMode,
  newScenario,
  setInitialScenarioMeta,
 )
import Arkham.Game.State (GameState)
import Arkham.Game.Utils (gameInvestigators)
import Arkham.Id (InvestigatorId, PlayerId (..), ScenarioId)
import Arkham.Investigator.Types (Investigator, investigatorPlayerId)
import Arkham.Message (Message (AdvanceToAgenda, ScenarioSpecific))
import Arkham.Scenario.Types (Scenario, getMetaKeyDefault)
import Arkham.Source (Source (GameSource))
import Arkham.Target (Target (..))
import Control.Monad.Random.Class (getRandom)
import Data.Aeson.Key qualified as AesonKey
import Data.Bits (shiftL, (.|.))
import Data.Map.Strict qualified as Map
import Data.Text qualified as T
import Data.These (These (..))
import Data.Time.Clock (UTCTime, getCurrentTime)
import Data.Time.Clock.POSIX (utcTimeToPOSIXSeconds)
import Data.Traversable (for)
import Data.UUID qualified as UUID
import Data.UUID.V4 (nextRandom)
import Database.Esqueleto.Experimental hiding (isNothing, update, (=.))
import Database.Persist qualified as P
import Entity.Arkham.Step (ActionDiff (..), ArkhamStep (..), Choice (..))
import Import hiding (on, (==.))
import UnliftIO.Exception (catch)
import Yesod.WebSockets (WebSocketsT, webSocketsOptions)

-- Request bodies --------------------------------------------------------------

data CreateEventGroupPost = CreateEventGroupPost
  { name :: Text
  , playerCount :: Int
  }
  deriving stock (Show, Generic)
  deriving anyclass FromJSON

data CreateEventPost = CreateEventPost
  { name :: Text
  , scenarioId :: ScenarioId
  , difficulty :: Difficulty
  , includeTarotReadings :: Bool
  , playWithBlobElse :: Maybe Bool
  , timeLimitMinutes :: Maybe Int
  {- ^ optional Epic time limit (default 180); when elapsed, still-playing groups
  are forced to agenda 3b.
  -}
  , groups :: [CreateEventGroupPost]
  }
  deriving stock (Show, Generic)
  deriving anyclass FromJSON

data CounterPost = CounterPost
  { key :: Text
  , amount :: Int
  }
  deriving stock (Show, Generic)
  deriving anyclass FromJSON

-- | One group's organizer-allocated spend toward a stage advance.
data AllocationEntry = AllocationEntry
  { ordinal :: Int
  , spend :: Int
  }
  deriving stock (Show, Generic)
  deriving anyclass FromJSON

{- | Body of @POST events/{id}/resolve-advance@: the organizer's per-group spend
allocation for a stage awaiting resolution.
-}
data ResolveAdvancePost = ResolveAdvancePost
  { stage :: Int
  , allocation :: [AllocationEntry]
  }
  deriving stock (Show, Generic)
  deriving anyclass FromJSON

{- | An organizer-triggered Replicate opportunity. The organizer chooses one of
the nine physical cards and the entity/location spotted in a group; the
group's engine presents the printed countermeasure cancellation prompt.
-}
data MainStreetSwapPost = MainStreetSwapPost
  { firstGroupOrdinal :: Int
  , secondGroupOrdinal :: Int
  }
  deriving stock (Show, Generic)
  deriving anyclass FromJSON

data ReplicatePost = ReplicatePost
  { groupOrdinal :: Int
  , cardCode :: CardCode
  , target :: Target
  }
  deriving stock (Show, Generic)
  deriving anyclass FromJSON

-- Response payloads -----------------------------------------------------------

data GroupPlayerInfo = GroupPlayerInfo
  { username :: Text
  , investigatorId :: Maybe InvestigatorId
  -- ^ Nothing until the player has chosen an investigator.
  }
  deriving stock (Show, Generic)
  deriving anyclass ToJSON

data ReplicateTarget = ReplicateTarget
  { target :: Target
  , cardCode :: CardCode
  , kind :: Text
  }
  deriving stock (Show, Generic)
  deriving anyclass ToJSON

data GroupDigest = GroupDigest
  { ordinal :: Int
  , name :: Text
  , gameId :: Maybe ArkhamGameId
  , gameState :: Maybe GameState
  , investigatorCount :: Int
  -- ^ players currently seated in this group
  , seatCount :: Int
  -- ^ total seats; investigatorCount < seatCount means the lobby has open seats
  , youAreSeated :: Bool
  {- ^ whether the requesting user holds a seat in this group (so an organizer who
  also plays can drop into it).
  -}
  , players :: [GroupPlayerInfo]
  -- ^ seated players (username + chosen investigator) for the dashboard.
  , replicateTargets :: [ReplicateTarget]
  -- ^ in-play locations, investigators, and enemies the organizer can nominate.
  }
  deriving stock (Show, Generic)
  deriving anyclass ToJSON

data EventDetails = EventDetails
  { id :: ArkhamEpicEventId
  , name :: Text
  , organizerUserId :: UserId
  , role :: Maybe EpicRole
  , sharedState :: SharedEventState
  , totalInvestigators :: Int
  , createdAt :: UTCTime
  -- ^ event start; the time-limit countdown runs from here.
  , playWithBlobElse :: Bool
  , groups :: [GroupDigest]
  }
  deriving stock (Show, Generic)
  deriving anyclass ToJSON

data EventListEntry = EventListEntry
  { id :: ArkhamEpicEventId
  , name :: Text
  , role :: EpicRole
  }
  deriving stock (Show, Generic)
  deriving anyclass ToJSON

deduplicateEventMemberships
  :: Ord eventId => [(eventId, name, EpicRole)] -> [(eventId, name, EpicRole)]
deduplicateEventMemberships rows =
  let (reverseOrder, byId) = foldl' recordMembership ([], Map.empty) rows
   in mapMaybe (`Map.lookup` byId) (reverse reverseOrder)
 where
  recordMembership (order, byId) row@(rowId, _, rowRole) =
    case Map.lookup rowId byId of
      Nothing -> (rowId : order, Map.insert rowId row byId)
      Just (existingId, existingName, existingRole) ->
        ( order
        , Map.insert
            rowId
            (existingId, existingName, preferredRole existingRole rowRole)
            byId
        )

  preferredRole Organizer _ = Organizer
  preferredRole _ Organizer = Organizer
  preferredRole GroupPlayer GroupPlayer = GroupPlayer

-- AuthZ -----------------------------------------------------------------------

{- | Gate a protected resource behind an authorization check.

@authCheck@ runs first; if it throws (e.g. Yesod 'permissionDenied'),
@onGranted@ is never invoked.  The polymorphic signature lets tests exercise
the same sequencing guarantee in plain 'IO' without standing up the full
application stack.
-}
withEventMember :: Monad m => m EpicRole -> (EpicRole -> m a) -> m a
withEventMember authCheck onGranted = authCheck >>= onGranted

requireEventMember :: UserId -> ArkhamEpicEventId -> Handler EpicRole
requireEventMember userId eid = do
  mMember <-
    runDB
      $ P.selectFirst
        [ArkhamEpicMemberArkhamEpicEventId P.==. eid, ArkhamEpicMemberUserId P.==. userId]
        []
  case mMember of
    Just (Entity _ m) -> pure (arkhamEpicMemberRole m)
    Nothing -> permissionDenied "You are not a member of this event"

{- | A user may hold both Organizer and GroupPlayer rows, so check for an
Organizer row directly rather than trusting the first membership found.
-}
requireOrganizer :: UserId -> ArkhamEpicEventId -> Handler ()
requireOrganizer userId eid = do
  isOrganizer <-
    runDB
      $ P.exists
        [ ArkhamEpicMemberArkhamEpicEventId P.==. eid
        , ArkhamEpicMemberUserId P.==. userId
        , ArkhamEpicMemberRole P.==. Organizer
        ]
  unless isOrganizer $ permissionDenied "Only the event organizer may perform this action"

-- Handlers --------------------------------------------------------------------

-- | List the events the current user is a member of.
getApiV1ArkhamEventsR :: Handler [EventListEntry]
getApiV1ArkhamEventsR = do
  userId <- getRequestUserId
  rows <- runDB $ select do
    (member :& event) <-
      from
        $ table @ArkhamEpicMember
        `innerJoin` table @ArkhamEpicEvent
          `on` (\(member :& event) -> member.arkhamEpicEventId ==. event.id)
    where_ $ member.userId ==. val userId
    orderBy [desc event.updatedAt]
    pure (event.id, event.name, member.role)
  pure
    [ EventListEntry {id = eid, name = nm, role = r}
    | (eid, nm, r) <-
        deduplicateEventMemberships
          [(eid, nm, r) | (Value eid, Value nm, Value r) <- rows]
    ]

{- | Create an event: build N group games (each a normal scenario game) and the
event aggregate, seeding shared countermeasures = ceil(totalInvestigators/2).
The creator becomes the organizer.
-}
postApiV1ArkhamEventsR :: Handler EventDetails
postApiV1ArkhamEventsR = do
  userId <- getRequestUserId
  CreateEventPost {..} <- requireCheckJsonBody
  now <- liftIO getCurrentTime
  -- A single random per-event seed; groups derive the shared Act 3b story-card
  -- pick deterministically from it (so all groups agree with no cross-group race).
  storySeed <- (`mod` 1000000) <$> liftIO getRandom

  let
    totalInvestigators = sum (map (.playerCount) groups)
    seeded =
      setSharedCounter TimeLimitMinutes (fromMaybe 180 timeLimitMinutes)
        $ setSharedCounter BlobStorySeed storySeed
        $ foldr
          (\(k, v) -> setSharedCounter k v)
          (emptySharedEventState totalInvestigators)
          (epicScenarioSeeds scenarioId totalInvestigators)
    eventRecord =
      ArkhamEpicEvent
        name
        userId
        (Just (tshow scenarioId))
        Nothing
        (tshow difficulty)
        seeded
        totalInvestigators
        0
        now
        now

  -- Build every group's pending game up front, in ordinal order, each with its
  -- own independently drawn seed -- not derived from, or reused between,
  -- the shared event 'storySeed' or another group's seed. Because each draw
  -- is independent, two groups' returned seeds may legitimately coincide;
  -- this is not treated as an error. This is pure aside from drawing the
  -- seed itself, so no database access happens here.
  pendingGroups <-
    preparePendingGroupGames
      (const (liftIO getRandom))
      scenarioId
      difficulty
      includeTarotReadings
      (fromMaybe False playWithBlobElse)
      now
      [(grp.name, grp.playerCount) | grp <- groups]

  -- One transaction: every group's game + initial step (consecutively, per
  -- group), then the event, organizer membership, and every group link. A
  -- failure at any step throws, so 'runDB' rolls back the entire aggregate --
  -- no orphan games or rows.
  eid <- runDB $ createEpicEventAggregate eventRecord userId pendingGroups

  buildEventDetails userId eid

-- | Dashboard payload. Upgrades to the event websocket feed when requested.
getApiV1ArkhamEventR :: ArkhamEpicEventId -> Handler EventDetails
getApiV1ArkhamEventR eid = do
  userId <- getRequestUserId
  withEventMember (requireEventMember userId eid) \_ -> do
    wsOptions <- websocketConnectionOptions
    webSocketsOptions wsOptions $ eventStream eid
    buildEventDetails userId eid

{- | Delete an event and all of its group games (organizer only). Deleting each
group's 'ArkhamGame' cascades its players/steps/logs and the epic-group row;
deleting the event cascades members and shared-state steps.
-}
deleteApiV1ArkhamEventR :: ArkhamEpicEventId -> Handler ()
deleteApiV1ArkhamEventR eid = do
  userId <- getRequestUserId
  requireOrganizer userId eid
  gameValues <- runDB $ select do
    grp <- from $ table @ArkhamEpicGroup
    where_ $ grp.arkhamEpicEventId ==. val eid
    pure grp.arkhamGameId
  let gameIds = [gid | Value (Just gid) <- gameValues]
  runDB do
    for_ gameIds P.delete
    P.delete eid
  for_ gameIds deleteRoom
  deleteEventRoom eid

{- | Organizer correction for the one user-adjustable Blob pool. Internal keys
(health, act gates/generations, timer state) are never writable through the
public endpoint; they are owned by engine/coordinator transitions.
-}
postApiV1ArkhamEventCounterR :: ArkhamEpicEventId -> Handler ()
postApiV1ArkhamEventCounterR eid = do
  userId <- getRequestUserId
  requireOrganizer userId eid
  CounterPost {..} <- requireCheckJsonBody
  case sharedKeyFromText key of
    Just Countermeasures -> do
      did <- UUID.toText <$> liftIO nextRandom
      let delta = SharedDelta {sharedDeltaId = did, sharedDeltaKey = Countermeasures, sharedDeltaAmount = amount}
      newState <- runDB $ applyEpicDeltasLocked eid Nothing Nothing [delta]
      propagateShared eid Nothing newState
    _ -> invalidArgs ["Only the countermeasures pool may be adjusted manually"]

{- | The Epic time limit has elapsed: force every still-playing group to agenda
3b ("face the consequences"). The frontend posts here when its (createdAt +
TimeLimitMinutes) countdown hits 0; because that deadline is identical for every
client, several may fire near-simultaneously, so this must be idempotent.

For each group game we drive the Blob's stage-3 agenda (theAnomalyConsumes) to
its B side via 'AdvanceToAgenda' — the deck-id (1) form, NOT the stage; the agenda
card def carries the stage. Advancing to side B replaces the current agenda with
stage 3, flips it (AgendaAdvancedWithOther), and the card's
@AdvanceAgenda (isSide B)@ handler pushes R1, the lose-by-time resolution. The
flip parks a lead-player confirmation; 'runMessagesInGroupWhen' persists that
continuation so the resolution completes when confirmed.

Idempotency: skip any group that is not 'IsActive' or already at/past agenda
stage 3. The stage check runs INSIDE each group's FOR UPDATE lock (the @p@
predicate of 'runMessagesInGroupWhen'), so concurrent expiry calls serialize and
never double-advance — once one call advances a group to stage 3, every other
call sees stage 3 and skips it. Per-group failures are logged and isolated so one
bad group can't block forcing the rest.
-}
postApiV1ArkhamEventTimeUpR :: ArkhamEpicEventId -> Handler ()
postApiV1ArkhamEventTimeUpR eid = do
  userId <- getRequestUserId
  void $ requireEventMember userId eid
  event <- runDB (P.get eid) >>= maybe notFound pure
  elapsed <- eventTimeElapsed event
  unless elapsed $ invalidArgs ["The event time limit has not elapsed"]
  forceEventTimeUp eid

{- | Start-of-game barrier: mark the caller's group ready (idempotent, by group
ordinal bit). When EVERY group is ready, the time-limit timer starts (records the
epoch). The frontend calls this once its group reaches the first investigation
phase, and gates play until 'TimerStartedAt' is set. No-op for a caller without a
seated group (e.g. an organizer who isn't playing).
-}
postApiV1ArkhamEventReadyR :: ArkhamEpicEventId -> Handler ()
postApiV1ArkhamEventReadyR eid = do
  userId <- getRequestUserId
  void $ requireEventMember userId eid
  mOrdinal <- runDB do
    mMember <-
      P.selectFirst
        [ ArkhamEpicMemberArkhamEpicEventId P.==. eid
        , ArkhamEpicMemberUserId P.==. userId
        , ArkhamEpicMemberRole P.==. GroupPlayer
        ]
        []
    pure $ mMember >>= (arkhamEpicMemberGroupOrdinal . entityVal)
  for_ mOrdinal \ordinal -> do
    numGroups <- runDB $ P.count [ArkhamEpicGroupArkhamEpicEventId P.==. eid]
    now <- liftIO getCurrentTime
    let
      nowEpoch = floor (utcTimeToPOSIXSeconds now) :: Int
      fullMask = (1 `shiftL` numGroups) - 1
    newState <- runDB $ modifySharedStateLocked eid \s ->
      let
        mask' = sharedCounter GroupsReadyMask s .|. (1 `shiftL` ordinal)
        s' = setSharedCounter GroupsReadyMask mask' s
       in
        if mask' == fullMask && sharedCounter TimerStartedAt s == 0
          then setSharedCounter TimerStartedAt nowEpoch s'
          else s'
    broadcastSharedToEvent eid newState

{- | Organizer-mediated excess-clue distribution on a shared act advance. The
coordinator has gated the stage with @AwaitingOrganizer stage == 1@; the organizer
allocates how many of each group's contributed clues are spent toward the
threshold. 200 with empty body — the result is pushed over the websocket.

Validation is server-side from the current shared state: every @spend@ in
@[0, that group's contribution]@ and @sum spend == 2 * sharedTotalInvestigators@.
The authoritative consume (write per-group 'ActSpend', reset the pool, bump the
generation, clear the gate) + the replica mirror + the global undo floor + the
overlay-lifting broadcast all happen in 'settleOrganizerAdvance', which is atomic
and idempotent against a double-submit. NO gameplay message is injected into any
group; the parked act reads its own 'ActSpend' from its mirrored replica.
-}
postApiV1ArkhamEventResolveAdvanceR :: ArkhamEpicEventId -> Handler ()
postApiV1ArkhamEventResolveAdvanceR eid = do
  userId <- getRequestUserId
  requireOrganizer userId eid
  ResolveAdvancePost {..} <- requireCheckJsonBody
  mEvent <- runDB $ P.get eid
  case mEvent of
    Nothing -> notFound
    Just event -> do
      let
        shared0 = arkhamEpicEventSharedState event
        threshold = 2 * sharedTotalInvestigators shared0
      when (sharedCounter (AwaitingOrganizer stage) shared0 /= 1)
        $ invalidArgs ["No advance awaiting organizer for this stage"]
      contributions <- getEventGroupContributions eid stage
      let
        contribMap = Map.fromList contributions
        -- Aggregate by ordinal so duplicate entries can't defeat a per-group cap.
        spendByOrdinal = Map.fromListWith (+) [(entry.ordinal, entry.spend) | entry <- allocation]
        totalSpend = sum (Map.elems spendByOrdinal)
        invalidGroup (ordinal, spend) = spend < 0 || spend > Map.findWithDefault 0 ordinal contribMap
      when (totalSpend /= threshold)
        $ invalidArgs ["Allocation must spend exactly " <> tshow threshold <> " clues"]
      when (any invalidGroup (Map.toList spendByOrdinal))
        $ invalidArgs ["A group's spend is negative or exceeds its contribution"]
      settleOrganizerAdvance eid stage spendByOrdinal

{- | Offer a Replicating Aberration spawn to one group. This deliberately runs
through that game's message queue instead of directly changing JSON: the
investigators receive a persisted choice to spend a shared countermeasure,
all resulting shared deltas use the normal Epic transaction seam, and the
group's websocket receives the resulting question/board state.
-}
postApiV1ArkhamEventReplicateR :: ArkhamEpicEventId -> Handler ()
postApiV1ArkhamEventReplicateR eid = do
  userId <- getRequestUserId
  requireOrganizer userId eid
  ReplicatePost {..} <- requireCheckJsonBody
  unless (unCardCode cardCode `elem` ["89010" <> T.singleton suffix | suffix <- ['a' .. 'i']])
    $ invalidArgs ["Only Replicating Aberration cards may be spawned"]
  mGameId <- runDB do
    mGroup <-
      P.selectFirst
        [ ArkhamEpicGroupArkhamEpicEventId P.==. eid
        , ArkhamEpicGroupOrdinal P.==. groupOrdinal
        ]
        []
    pure $ mGroup >>= arkhamEpicGroupArkhamGameId . entityVal
  gameId <- maybe (invalidArgs ["Unknown event group"]) pure mGameId
  rawGame <- runDB $ P.getJust gameId
  unless (gameUsesBlobElse $ arkhamGameCurrentData rawGame)
    $ permissionDenied "Replicating Aberrations require The Blob That Ate Everything ELSE!"
  runMessagesInGroupWhen
    (const True)
    [ScenarioSpecific "blobRequestAberration" (toJSON (cardCode, target))]
    gameId

postApiV1ArkhamEventSwapMainStreetR :: ArkhamEpicEventId -> Handler ()
postApiV1ArkhamEventSwapMainStreetR eid = do
  userId <- getRequestUserId
  requireOrganizer userId eid
  MainStreetSwapPost {..} <- requireCheckJsonBody
  when (firstGroupOrdinal == secondGroupOrdinal)
    $ invalidArgs ["Investigators must be in different groups"]
  swapMainStreetInvestigators eid firstGroupOrdinal secondGroupOrdinal

{- | Whether any agenda currently in play in the group's game is at or past
@stage@. Used as the in-lock idempotency guard for the time-up forcing: a group
already at agenda stage 3 (forced previously, or advanced there in normal play)
is left untouched.
-}
eventTimeElapsed :: ArkhamEpicEvent -> Handler Bool
eventTimeElapsed event = do
  nowEpoch <- floor . utcTimeToPOSIXSeconds <$> liftIO getCurrentTime
  let
    shared = arkhamEpicEventSharedState event
    limitSeconds = sharedCounter TimeLimitMinutes shared * 60
    startedAt = sharedCounter TimerStartedAt shared
  pure $ limitSeconds > 0 && startedAt > 0 && nowEpoch >= startedAt + limitSeconds

forceEventTimeUp :: ArkhamEpicEventId -> Handler ()
forceEventTimeUp eid = do
  gameIds <- getEventGroupGameIds eid
  for_ gameIds \gid ->
    runMessagesInGroupWhen
      (not . agendaAtOrPastStage 3)
      [AdvanceToAgenda 1 Agendas.theAnomalyConsumes Agenda.B GameSource]
      gid
      `catch` \(e :: SomeException) ->
        $(logWarn) $ "Epic time-up advance failed for " <> tshow gid <> ": " <> tshow e

agendaAtOrPastStage :: Int -> Game -> Bool
agendaAtOrPastStage stage game =
  any
    (\ag -> Agenda.agendaSequenceStep (attr agendaSequence ag) >= stage)
    (toList (entitiesAgendas (gameEntities game)))

-- Helpers ---------------------------------------------------------------------

buildEventDetails :: UserId -> ArkhamEpicEventId -> Handler EventDetails
buildEventDetails userId eid = do
  mEvent <- runDB $ P.get eid
  case mEvent of
    Nothing -> notFound
    Just event -> do
      whenM (eventTimeElapsed event) $ forceEventTimeUp eid
      groupRows <- runDB $ select do
        grp <- from $ table @ArkhamEpicGroup
        where_ $ grp.arkhamEpicEventId ==. val eid
        orderBy [asc grp.ordinal]
        pure grp
      -- A user may hold both Organizer and GroupPlayer rows (an organizer who
      -- also took a seat). Report Organizer in that case so organizer-only UI
      -- (e.g. the delete control) stays available.
      isOrganizer <-
        runDB
          $ P.exists
            [ ArkhamEpicMemberArkhamEpicEventId P.==. eid
            , ArkhamEpicMemberUserId P.==. userId
            , ArkhamEpicMemberRole P.==. Organizer
            ]
      mRole <-
        runDB
          $ P.selectFirst
            [ArkhamEpicMemberArkhamEpicEventId P.==. eid, ArkhamEpicMemberUserId P.==. userId]
            []
      let resolvedRole =
            if isOrganizer
              then Just Organizer
              else arkhamEpicMemberRole . entityVal <$> mRole
      digests <- traverse (mkGroupDigest userId) groupRows
      playWithBlobElse <- or <$> traverse groupUsesBlobElse groupRows
      pure
        EventDetails
          { id = eid
          , name = arkhamEpicEventName event
          , organizerUserId = arkhamEpicEventOrganizerUserId event
          , role = resolvedRole
          , sharedState = arkhamEpicEventSharedState event
          , totalInvestigators = arkhamEpicEventTotalInvestigators event
          , createdAt = arkhamEpicEventCreatedAt event
          , playWithBlobElse = playWithBlobElse
          , groups = digests
          }

scenarioUsesBlobElse :: Scenario -> Bool
scenarioUsesBlobElse = getMetaKeyDefault "blobThatAteEverythingElse" False . toAttrs

{- | Read a scenario meta key from a built 'Game', defaulting when the game has
no scenario attached (campaign-only mode). Exposed so
"Arkham.Api.Events.EventCreationSpec" can inspect 'buildGroupGame' output (its
epic/blob/variant meta) without adding 'these' as a test dependency.
-}
gameScenarioMetaDefault :: FromJSON a => AesonKey.Key -> a -> Game -> a
gameScenarioMetaDefault k def game = case gameMode game of
  That scenario -> getMetaKeyDefault k def (toAttrs scenario)
  These _ scenario -> getMetaKeyDefault k def (toAttrs scenario)
  This _ -> def

gameUsesBlobElse :: Game -> Bool
gameUsesBlobElse game = case gameMode game of
  That scenario -> scenarioUsesBlobElse scenario
  These _ scenario -> scenarioUsesBlobElse scenario
  This _ -> False

groupUsesBlobElse :: Entity ArkhamEpicGroup -> Handler Bool
groupUsesBlobElse (Entity _ grp) = case arkhamEpicGroupArkhamGameId grp of
  Nothing -> pure False
  Just gid -> do
    mGame <- runDB $ P.get gid
    pure $ maybe False (gameUsesBlobElse . arkhamGameCurrentData) mGame

mkGroupDigest :: UserId -> Entity ArkhamEpicGroup -> Handler GroupDigest
mkGroupDigest userId (Entity _ grp) = case arkhamEpicGroupArkhamGameId grp of
  Nothing ->
    pure
      GroupDigest
        { ordinal = ordx
        , name = nm
        , gameId = Nothing
        , gameState = Nothing
        , investigatorCount = 0
        , seatCount = seats
        , youAreSeated = False
        , players = []
        , replicateTargets = []
        }
  Just gid -> do
    mGame <- runDB $ P.get gid
    seated <- runDB $ P.exists [ArkhamPlayerArkhamGameId P.==. gid, ArkhamPlayerUserId P.==. userId]
    playerRows <- runDB $ select do
      (p :& u) <-
        from
          $ table @ArkhamPlayer
          `innerJoin` table @User
            `on` (\(p :& u) -> p.userId ==. u.id)
      where_ $ p.arkhamGameId ==. val gid
      pure (p.id, u.username)
    let
      investigators :: [Investigator]
      investigators = maybe [] (toList . gameInvestigators . arkhamGameCurrentData) mGame
      invByPlayer :: Map PlayerId InvestigatorId
      invByPlayer = Map.fromList [(attr investigatorPlayerId i, i.id) | i <- investigators]
      players =
        [ GroupPlayerInfo {username = un, investigatorId = Map.lookup (PlayerId (coerce pid)) invByPlayer}
        | (Value pid, Value un) <- playerRows
        ]
      replicateTargets = case mGame of
        Just rawGame
          | gameUsesBlobElse (arkhamGameCurrentData rawGame) ->
              let game = arkhamGameCurrentData rawGame
                  entities = gameEntities game
               in [ ReplicateTarget (LocationTarget lid) (toCardCode l) "location"
                  | (lid, l) <- Map.toList $ entitiesLocations entities
                  ]
                    <> [ ReplicateTarget (InvestigatorTarget iid) (toCardCode i) "investigator"
                       | (iid, i) <- Map.toList $ entitiesInvestigators entities
                       ]
                    <> [ ReplicateTarget (EnemyTarget eid) (toCardCode e) "enemy"
                       | (eid, e) <- Map.toList $ entitiesEnemies entities
                       ]
        _ -> []
    pure
      GroupDigest
        { ordinal = ordx
        , name = nm
        , gameId = Just gid
        , gameState = gameGameState . arkhamGameCurrentData <$> mGame
        , investigatorCount = length players
        , seatCount = seats
        , youAreSeated = seated
        , players = players
        , replicateTargets = replicateTargets
        }
 where
  ordx = arkhamEpicGroupOrdinal grp
  nm = arkhamEpicGroupName grp
  seats = arkhamEpicGroupSeatCount grp

{- | One group's fully-built game, computed with its own independently drawn
seed before the persistence transaction runs. Constructing this value performs
no IO or database access, so it is safe to build outside (or, harmlessly,
inside) the single transaction that persists it.
-}
data PendingGroupGame = PendingGroupGame
  { ordinal :: Int
  , name :: Text
  , seatCount :: Int
  -- ^ the group's requested @playerCount@, unclamped -- this is what gets
  -- written to 'ArkhamEpicGroup''s @seatCount@ column, matching prior
  -- behavior. The game's own seat count ('gamePlayerCount', via 'buildGroupGame')
  -- is separately clamped to a minimum of 1.
  , game :: ArkhamGame
  }
  deriving stock (Show, Generic)

{- | Pure construction of one group's initial 'ArkhamGame': an OPEN
multiplayer lobby -- a WithFriends game with @playerCount@ seats (minimum 1)
and no players yet. It stays in 'IsPending' until players join asynchronously
via @PUT /games/:id/join@, then activates and runs setup once its seats fill —
exactly the normal multiplayer flow, one lobby per group. The organizer is NOT
auto-seated (they may join a group like anyone else).

The scenario is flagged Epic Multiplayer in its meta so it picks its epic
setup branch at Setup time (see 'Api.Handler.Arkham.Events.buildGroupGame' --
the join/setup path has no event context to consult otherwise).
-}
buildGroupGame
  :: Text
  -> ScenarioId
  -> Difficulty
  -> Bool
  -> Bool
  -> Int
  -> Int
  -> UTCTime
  -> ArkhamGame
buildGroupGame gameName scenarioId difficulty includeTarotReadings playWithBlobElse playerCount newGameSeed now =
  let
    seats = max 1 playerCount
    game =
      (if playWithBlobElse then setInitialScenarioMeta "variant" ("else" :: Text) else id)
        $ setInitialScenarioMeta "blobThatAteEverythingElse" playWithBlobElse
        $ setInitialScenarioMeta "epicMultiplayer" True
        $ newScenario scenarioId newGameSeed seats difficulty includeTarotReadings
   in ArkhamGame gameName game 0 WithFriends now now

{- | Build every group's 'PendingGroupGame' up front, in ordinal order, via the
supplied ordinal-aware seed action. Production calls this with
@const (liftIO getRandom)@: each group's seed is drawn independently, not
derived from the shared event story seed or from another group's seed.
Because each draw is independent, two groups' returned seeds may legitimately
coincide -- callers must not assume, or rely on, mutual distinctness. Aside
from the supplied action itself, this performs no database access.
-}
preparePendingGroupGames
  :: Monad m
  => (Int -> m Int)
  -- ^ given a group's ordinal, independently draw that group's game seed.
  -> ScenarioId
  -> Difficulty
  -> Bool
  -> Bool
  -> UTCTime
  -> [(Text, Int)]
  -- ^ each group's (name, playerCount), in ordinal order
  -> m [PendingGroupGame]
preparePendingGroupGames drawSeed scenarioId difficulty includeTarotReadings playWithBlobElse now groupSpecs =
  for (zip [0 :: Int ..] groupSpecs) \(ordx, (groupName, playerCount)) -> do
    groupSeed <- drawSeed ordx
    pure
      PendingGroupGame
        { ordinal = ordx
        , name = groupName
        , seatCount = playerCount
        , game =
            buildGroupGame
              groupName
              scenarioId
              difficulty
              includeTarotReadings
              playWithBlobElse
              playerCount
              groupSeed
              now
        }

{- | Abstract persistence steps needed to create one Epic event aggregate: N
group games, each immediately followed by its own initial step, then the
event row, the creator's organizer membership, and every group link.

Production runs 'createEpicEventAggregate' inside a single 'runDB' call over
'SqlPersistT Handler' (see the instance below), so an exception thrown by any
'P.insert'/'P.insert_' call rolls back every earlier insert in the same
database transaction. Tests exercise the exact same sequencing against a pure,
in-memory instance (see "Arkham.Api.Events.EventCreationSpec") to prove each
group's game and initial step are always inserted (consecutively, in ordinal
order) before the event/member/link rows, each group receives its own
supplied seed in ordinal order, and a failure injected at any step --
including a group's initial step, or the late event/member/link steps --
short-circuits with no successful result.

Do not write an instance that catches an insertion exception and converts it
into an ordinary return value of this class: 'runDB' only rolls back on an
actual uncaught exception, so silently turning one into a normal result here
would defeat the rollback guarantee.
-}
class Monad m => MonadEpicPersistence m where
  insertGroupGame :: PendingGroupGame -> m ArkhamGameId
  insertInitialStep :: ArkhamGameId -> m ()
  insertEvent :: ArkhamEpicEvent -> m ArkhamEpicEventId
  insertOrganizerMember :: ArkhamEpicEventId -> UserId -> m ()
  insertGroupLink :: ArkhamEpicEventId -> PendingGroupGame -> ArkhamGameId -> m ()

{- | The sequencing plan itself: every group's game immediately followed by
its own initial step (in ordinal order), then the event, the organizer's
membership, and every group link. This is the single seam production and
tests both exercise directly.
-}
createEpicEventAggregate
  :: MonadEpicPersistence m
  => ArkhamEpicEvent -> UserId -> [PendingGroupGame] -> m ArkhamEpicEventId
createEpicEventAggregate eventRecord organizerId pendingGroups = do
  groupGameIds <- for pendingGroups \pg -> do
    gid <- insertGroupGame pg
    insertInitialStep gid
    pure (pg, gid)
  eid <- insertEvent eventRecord
  insertOrganizerMember eid organizerId
  for_ groupGameIds \(pg, gid) -> insertGroupLink eid pg gid
  pure eid

instance MonadEpicPersistence (SqlPersistT Handler) where
  insertGroupGame pg = P.insert pg.game
  insertInitialStep gid = P.insert_ $ ArkhamStep gid (Choice mempty []) 0 (ActionDiff [])
  insertEvent = P.insert
  insertOrganizerMember eid uid = P.insert_ $ ArkhamEpicMember eid uid Organizer Nothing
  insertGroupLink eid pg gid =
    P.insert_ $ ArkhamEpicGroup eid pg.ordinal (Just gid) pg.name pg.seatCount

{- | Initial shared counters for an event, by scenario. Frozen at event start
(scales by the total investigator count across all groups).
-}
epicScenarioSeeds :: ScenarioId -> Int -> [(SharedKey, Int)]
epicScenarioSeeds scenarioId total
  | scenarioId == "85001" =
      -- The Blob That Ate Everything: countermeasures = ceil(total/2); Subject
      -- 8L-08 (epic, card 85037) global health = 15 x total. Epic Act 1's shared
      -- clue progress is seeded at 0 so the UI can show its threshold immediately.
      [ (Countermeasures, (total + 1) `div` 2)
      , (SharedEnemyHealth (CardCode "85037"), 15 * total)
      , (SharedActProgress 1, 0)
      ]
  | otherwise = []

-- | The per-event websocket: a read-only feed of shared-state updates.
eventStream :: ArkhamEpicEventId -> WebSocketsT Handler ()
eventStream eid =
  -- Releases the room and its Redis subscription together, but only once the
  -- last subscriber has actually gone; see 'releaseRoomIfEmpty'.
  streamRoom (joinEventRoom eid) (void $ releaseEventRoomIfEmpty eid)
