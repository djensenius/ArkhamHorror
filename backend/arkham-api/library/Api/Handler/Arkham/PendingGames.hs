{- | A pending (lobby) game's FIRST player join, run entirely inside one
'Api.Arkham.Helpers.atomicallyWithGame' transaction that locks the TARGET
game row first -- establishing the SAME game-before-event-before-membership
order 'Api.Handler.Arkham.Game.Debug.MonadClaimSeat' and
'Api.Handler.Arkham.Events.MonadEpicEventDeletion' already use, and, when
this game is Epic-linked, RESERVING (not merely reading) this user's
'Arkham.Epic.Types.GroupPlayer' membership through the SAME shared
'Api.Arkham.Epic.reserveEpicGroupMembershipReconciling' primitive
claim-seat uses -- including reconciliation against a LEGACY seat (an
'Entity.Arkham.Player.ArkhamPlayer' row in some OTHER game linked to this
event with no membership row at all, predating this reservation
machinery) -- so the "one event group per user" invariant cannot
independently drift between the two entry points that can create a user's
FIRST membership row for an event.

/Cross-path lock-order audit (why this module cannot deadlock against
event deletion, claim-seat, or Main Street swap):/ 'putApiV1ArkhamPendingGameR'
only ever locks ONE game -- the target game -- via 'atomicallyWithGame',
strictly before 'planPendingJoinMembership' ever locks the event (see that
function's own lock order below). A singleton game set is trivially a
valid ascending subset of 'Api.Arkham.Epic.canonicalEpicGameLockOrder', so
this can never invert relative to
'Api.Handler.Arkham.Events.deleteEpicEventAggregate' (which locks every
game linked to an event, in ascending id order, before the event) or
'Api.Handler.Arkham.Games.Shared.mainStreetSwapPlan' (which locks exactly
two games, in that same ascending order, before either player). Later,
once setup has run and produced any shared deltas, this module calls
'Api.Arkham.Epic.applyEpicDeltasLocked' -- audited to confirm it takes NO
lock of its own on any OTHER game: its only database write is against the
single event row (already locked earlier in this same transaction; see
'Api.Arkham.Epic.canonicalEpicGameLockOrder's Haddoc on why re-locking a
row a transaction already holds is a documented no-op), and every
'Entity.Arkham.Epic.ArkhamEpicGroup' row links to AT MOST one game (see
that entity's @arkhamGameId ArkhamGameId Maybe@ field), so there is no
"all of this event's games" lock for it to acquire even in principle. The
full path this module ever exercises is therefore exactly
game(target)->event->membership->player, never game->event->ANOTHER game,
so two concurrent pending joins into different games of the same event,
a pending join racing a concurrent claim-seat into a different group, and
a pending join racing event deletion, can only ever contend on shared
locks in that one fixed order -- never form a cycle.
-}
module Api.Handler.Arkham.PendingGames (
  getApiV1ArkhamPendingGameR,
  putApiV1ArkhamPendingGameR,
  PendingJoinPlan (..),
  PendingJoinRejection (..),
  MonadPendingJoin (..),
  planPendingJoinMembership,
) where

import Import hiding (on, (==.))

import Api.Arkham.Epic (
  EpicGroupReservation (..),
  applyEpicDeltasLocked,
  lockEpicEventRow,
  lookupGameEvent,
  mkEpicEnv,
  reserveEpicGroupMembershipReconciling,
 )
import Api.Arkham.Helpers
import Api.Handler.Arkham.Games.Shared (
  broadcastEventChanged,
  epicSyncMessages,
  propagateShared,
  publishToRoom,
  runMessagesTimeoutMicros,
  runWithMessagesTimeout,
 )
import Arkham.Classes.HasQueue
import Arkham.Epic.Types (
  GroupOrdinal (..),
  SharedEventState,
  epicEnvDeltaRef,
  epicEnvGroup,
  epicEnvSharedRef,
 )
import Arkham.Game
import Arkham.Game.State
import Arkham.Id
import Arkham.Queue
import Control.Lens (view)
import Control.Monad.Random (mkStdGen)
import Data.Time.Clock
import Database.Persist ((==.))
import Database.Persist.Sql (SqlBackend)
import Entity.Arkham.Step

getApiV1ArkhamPendingGameR :: ArkhamGameId -> Handler (PublicGame ArkhamGameId)
getApiV1ArkhamPendingGameR gameId = do
  _ <- getRequestUserId
  ge <- runDB $ get404 gameId
  pure $ toPublicGame (Entity gameId ge) mempty

{- | The pending-join membership decision, threaded by
'planPendingJoinMembership' into the exact sequencing production runs.
Carries only plain ids\/ordinals (never a full 'Entity'), so it can be
compared\/logged directly by tests without depending on any entity's
derived instances.
-}
data PendingJoinPlan
  = -- | This user already holds an 'Entity.Arkham.Player.ArkhamPlayer' row
    -- in THIS (already-locked) game -- a duplicate\/idempotent join
    -- attempt (e.g. a doubled click). For a non-Epic game, no event
    -- lookup, lock, or reservation is even attempted. For an Epic-linked
    -- game, this is decided ONLY AFTER event-membership reconciliation has
    -- already run (see 'planPendingJoinMembership') -- never before it --
    -- so an otherwise-idempotent re-join still repairs a legacy seat's
    -- missing membership row.
    PendingJoinAlreadyMember
  | -- | This game is not linked to any Epic event at all. No lock\/
    -- reservation is attempted.
    PendingJoinNoEvent
  | -- | The membership was freshly reserved (or already existed under the
    -- SAME ordinal) for the given event\/ordinal -- see
    -- 'reservePendingJoinMembership'. The caller should proceed to insert
    -- the player and seed 'Api.Arkham.Epic.mkEpicEnv' from this event.
    PendingJoinReserved ArkhamEpicEventId GroupOrdinal
  | -- | This user already holds a seat -- WITH a membership row under a
    -- DIFFERENT ordinal, or a LEGACY seat (no membership row at all) in
    -- some OTHER game of this event -- that conflicts with the requested
    -- ordinal. No player row is inserted or modified.
    PendingJoinConflict
  | -- | The event vanished concurrently between discovering this game's
    -- link and locking it. Structurally unreachable in practice (see
    -- 'lockPendingJoinEvent'), but handled as a typed outcome rather than
    -- assumed impossible.
    PendingJoinEventVanished
  deriving stock (Eq, Show)

-- | The two ways a pending join can be rejected, decided entirely inside
-- one transaction before any player\/game\/step\/shared-state write.
data PendingJoinRejection
  = PendingJoinMembershipConflict
  | PendingJoinEventGone
  deriving stock (Eq, Show)

{- | Abstract persistence steps needed to decide whether a pending-join
attempt may proceed to insert its FIRST player row for this game, and --
when the game is Epic-linked -- to serialize the "one event group per
user" reservation against every OTHER entry point that can create a
user's FIRST 'Entity.Arkham.Epic.ArkhamEpicMember' row for the same event
(see 'Api.Handler.Arkham.Game.Debug.MonadClaimSeat', which reserves the
SAME way for its own claim-seat flow). Mirrors 'Api.Handler.Arkham.Game.Debug.MonadClaimSeat'
and 'Api.Handler.Arkham.Events.MonadEpicEventDeletion': a small set of
typed, individually testable steps, threaded by
'planPendingJoinMembership' into the exact sequencing production runs.

The TARGET GAME is already locked by the time this class's methods are
ever consulted: 'putApiV1ArkhamPendingGameR' only calls
'planPendingJoinMembership' from inside
'Api.Arkham.Helpers.atomicallyWithGame', which locks the game row FIRST.
-}
class Monad m => MonadPendingJoin m where
  -- | Whether this user already holds a player row in this
  -- (already-locked) game -- a duplicate\/idempotent join attempt. For a
  -- non-Epic game, checked FIRST, before any event lookup, lock, or
  -- reservation is even attempted. For an Epic-linked game, checked ONLY
  -- AFTER 'reservePendingJoinMembership' has already run (see
  -- 'planPendingJoinMembership'), never before it, exactly mirroring
  -- 'Api.Handler.Arkham.Game.Debug.isClaimSeatAlreadyJoined''s own
  -- ordering relative to
  -- 'Api.Handler.Arkham.Game.Debug.reserveClaimSeatMembership'.
  hasExistingPendingPlayer :: ArkhamGameId -> UserId -> m Bool

  -- | Row-lock the Epic event ('FOR UPDATE' in production; see
  -- 'Api.Arkham.Epic.lockEpicEventRow') and report whether it is still
  -- present. Called from exactly ONE place, strictly BEFORE
  -- 'reservePendingJoinMembership' -- preserving the SAME
  -- game(s)-before-event order 'Api.Handler.Arkham.Game.Debug.lockClaimSeatEvent'
  -- and 'Api.Handler.Arkham.Events.MonadEpicEventDeletion' already
  -- establish. 'False' would mean the event vanished concurrently --
  -- structurally unreachable here (deleting it first requires locking,
  -- and deleting, THIS already-locked game as one of its linked games),
  -- but handled as a typed outcome rather than assumed impossible.
  lockPendingJoinEvent :: ArkhamEpicEventId -> m Bool

  -- | Attempt to reserve this user's 'GroupPlayer' membership for the
  -- ALREADY-LOCKED event under the requested ordinal -- see
  -- 'Api.Arkham.Epic.reserveEpicGroupMembershipReconciling', the EXACT
  -- SAME legacy-aware primitive
  -- 'Api.Handler.Arkham.Game.Debug.reserveClaimSeatMembership' delegates
  -- to for claim-seat's own flow, so the "one event group per user"
  -- invariant (including reconciliation against a bare, unreconciled
  -- LEGACY seat in some other game of this event, with no membership row
  -- at all) cannot independently drift between the two entry points that
  -- can create a user's FIRST membership row for an event. Called
  -- REGARDLESS of whether this user already holds a player row in THIS
  -- (target) game -- i.e. strictly BEFORE 'hasExistingPendingPlayer' is
  -- even consulted (see 'planPendingJoinMembership') -- so an
  -- otherwise-idempotent re-join still repairs a legacy seat's missing
  -- membership row.
  reservePendingJoinMembership :: ArkhamEpicEventId -> UserId -> Int -> m EpicGroupReservation

{- | The pending-join membership decision:

1. A game not linked to any Epic event at all reports 'PendingJoinAlreadyMember'
   (if this user already holds a player row here) or 'PendingJoinNoEvent'
   otherwise; nothing else is ever attempted for a non-Epic game.
2. Otherwise, lock the event row ('lockPendingJoinEvent') and, if it is
   somehow gone (structurally unreachable; see that method's Haddoc),
   report 'PendingJoinEventVanished'.
3. Otherwise, actually RESERVE this user's 'GroupPlayer' membership under
   THIS game's own ordinal, WITH legacy-seat reconciliation
   ('reservePendingJoinMembership') -- a genuine cross-game reservation
   through 'Entity.Arkham.Epic.UniqueEpicMember's own unique key PLUS a
   scan of every OTHER game linked to this event for a bare, unreconciled
   seat (see 'Api.Arkham.Epic.reserveEpicGroupMembershipReconciling'),
   run REGARDLESS of whether this user already holds a player row in THIS
   game -- i.e. BEFORE 'hasExistingPendingPlayer' is even consulted --
   specifically so an otherwise-idempotent re-join still repairs a legacy
   seat's missing membership row: a user with no prior seat OR membership
   for this event can no longer be the FIRST joiner of two different
   groups of the same event, sequentially or concurrently, because the
   event row lock serializes every such reservation attempt against the
   SAME primitive 'Api.Handler.Arkham.Game.Debug.claimSeatEventMembershipConflict'
   also uses. A conflicting seat under a DIFFERENT ordinal (whether via an
   existing membership row or a bare legacy seat) reports 'PendingJoinConflict',
   with no player row inserted.
4. Only once reconciliation reports no conflict is 'hasExistingPendingPlayer'
   consulted: this user already holding a player row in THIS (target) game
   reports 'PendingJoinAlreadyMember'.
5. Otherwise reports 'PendingJoinReserved', carrying the event id and
   ordinal the caller should seed 'Api.Arkham.Epic.mkEpicEnv' with.

This is the single seam production and tests both exercise directly,
mirroring 'Api.Handler.Arkham.Game.Debug.planAndExecuteClaimSeat' and
'Api.Handler.Arkham.Events.deleteEpicEventAggregate'. The Epic
event\/ordinal link ('mEvent') is resolved ONCE, by the caller, before
this function ever runs (see 'planAndExecutePendingJoin'), so it is
supplied here rather than re-derived, avoiding a second redundant lookup.
-}
planPendingJoinMembership
  :: MonadPendingJoin m
  => ArkhamGameId
  -> UserId
  -> Maybe (ArkhamEpicEventId, Int)
  -> m PendingJoinPlan
planPendingJoinMembership gameId userId mEvent = case mEvent of
  Nothing -> do
    already <- hasExistingPendingPlayer gameId userId
    pure $ if already then PendingJoinAlreadyMember else PendingJoinNoEvent
  Just (eventId, ordinal) -> do
    eventStillPresent <- lockPendingJoinEvent eventId
    if not eventStillPresent
      then pure PendingJoinEventVanished
      else do
        reservation <- reservePendingJoinMembership eventId userId ordinal
        case reservation of
          EpicGroupReservationConflict -> pure PendingJoinConflict
          EpicGroupReserved -> do
            already <- hasExistingPendingPlayer gameId userId
            pure
              $ if already
                then PendingJoinAlreadyMember
                else PendingJoinReserved eventId (GroupOrdinal ordinal)

instance MonadIO m => MonadPendingJoin (ReaderT SqlBackend m) where
  hasExistingPendingPlayer gid uid =
    exists [ArkhamPlayerArkhamGameId ==. gid, ArkhamPlayerUserId ==. uid]
  lockPendingJoinEvent eid = isJust <$> lockEpicEventRow eid
  reservePendingJoinMembership = reserveEpicGroupMembershipReconciling

putApiV1ArkhamPendingGameR :: ArkhamGameId -> Handler (PublicGame ArkhamGameId)
putApiV1ArkhamPendingGameR gameId = do
  userId <- getRequestUserId
  now <- liftIO getCurrentTime
  outcome <- runDB $ atomicallyWithGame gameId (planAndExecutePendingJoin gameId userId now)
  case outcome of
    Left PendingJoinMembershipConflict ->
      permissionDenied "You already occupy a seat in another group in this event"
    Left PendingJoinEventGone -> notFound
    Right (game@ArkhamGame {..}, mShared, mEventId) -> do
      for_ mShared \(eid, s) -> propagateShared eid (Just gameId) s
      for_ mEventId broadcastEventChanged
      publishToRoom gameId
        $ GameUpdate
        $ PublicGame gameId arkhamGameName [] arkhamGameCurrentData
      pure $ toPublicGame (Entity gameId game) mempty

{- | Resolve this game's Epic link (once, non-locking; safe because a
group's game link is fixed at game\/group creation and never changes), then
decide and execute the join, entirely inside the ONE transaction
'atomicallyWithGame' already has this game locked for. The returned event
id is threaded through regardless of branch (matching the previous
behavior of broadcasting an event-changed notification whenever this game
is Epic-linked at all, even for a no-op re-join). For an Epic-linked game,
the membership decision itself ('planPendingJoinMembership') ALWAYS locks
the event and attempts reservation\/reconciliation, even for a branch that
turns out to be an already-a-member no-op re-join -- see that function's
own Haddoc for why the repair must not be skipped for that branch; only a
non-Epic game skips the event lock\/reservation entirely.
-}
planAndExecutePendingJoin
  :: ArkhamGameId
  -> UserId
  -> UTCTime
  -> ArkhamGame
  -> DB (Either PendingJoinRejection (ArkhamGame, Maybe (ArkhamEpicEventId, SharedEventState), Maybe ArkhamEpicEventId))
planAndExecutePendingJoin gameId userId now original@ArkhamGame {..} = do
  mEvent <- fmap (\(e, GroupOrdinal o) -> (entityKey e, o)) <$> lookupGameEvent gameId
  let mEventId = fst <$> mEvent
  case gameGameState arkhamGameCurrentData of
    IsPending _ -> do
      plan <- planPendingJoinMembership gameId userId mEvent
      case plan of
        PendingJoinAlreadyMember -> pure $ Right (original, Nothing, mEventId)
        PendingJoinNoEvent -> Right <$> runPendingJoinSetup gameId userId now original Nothing
        PendingJoinReserved eventId ordinal ->
          Right <$> runPendingJoinSetup gameId userId now original (Just (eventId, ordinal))
        PendingJoinConflict -> pure $ Left PendingJoinMembershipConflict
        PendingJoinEventVanished -> pure $ Left PendingJoinEventGone
    _ -> pure $ Right (original, Nothing, mEventId)

{- | Run the actual game-engine setup for this game's FIRST player: adds the
player, runs setup messages (pausing at deck selection for a lobby), and --
for an Epic-linked game whose membership 'planPendingJoinMembership' has
ALREADY reserved -- seeds\/commits any shared-state deltas the setup run
emitted. Called from exactly ONE place, after every membership check above
has passed.

Both 'runMessages' calls this can make (initial setup, and the
deck-selection-complete shared-pool sync) run inside a SINGLE
'runWithMessagesTimeout' wrap around the whole engine block, using the
SAME budget\/exception 'Api.Handler.Arkham.Games.Shared.updateGame' uses --
a pathological setup can only ever pin this transaction's game (and,
when Epic-linked, event) lock for that one bounded window before the
timeout exception aborts the whole 'runDB' transaction and releases both.
-}
runPendingJoinSetup
  :: ArkhamGameId
  -> UserId
  -> UTCTime
  -> ArkhamGame
  -> Maybe (ArkhamEpicEventId, GroupOrdinal)
  -> DB (ArkhamGame, Maybe (ArkhamEpicEventId, SharedEventState), Maybe ArkhamEpicEventId)
runPendingJoinSetup gameId userId now ArkhamGame {..} mEventCtx = do
  mLastStep <- getBy (UniqueStep gameId arkhamGameStep)
  let currentQueue = maybe [] (choiceMessages . arkhamStepChoice . entityVal) mLastStep

  gameRef <- liftIO $ newIORef arkhamGameCurrentData
  queueRef <- liftIO $ newQueue currentQueue
  genRef <- liftIO $ newIORef (mkStdGen (gameSeed arkhamGameCurrentData))

  pid <- insert $ ArkhamPlayer userId gameId "00000"

  -- Epic Multiplayer: seed the run loop's shared-state view from the event
  -- row this game's membership was already reserved against above. Setup
  -- must run event-aware so the group's shared values (countermeasures,
  -- Subject 8L-08 health) start from the CURRENT pool rather than 0/seed --
  -- otherwise a group set up after others have acted would not reflect
  -- their spends/damage.
  mEpicEnv <- case mEventCtx of
    Nothing -> pure Nothing
    Just (eventId, ordinal) -> do
      -- Already locked by 'lockPendingJoinEvent' just above (see
      -- 'planPendingJoinMembership'); re-locking the SAME row a
      -- transaction already holds is a documented no-op (see
      -- 'Api.Arkham.Epic.canonicalEpicGameLockOrder'), so this just
      -- recovers the entity's CURRENT fields (sharedState etc.) needed to
      -- seed 'mkEpicEnv', not a second real lock acquisition. A 'Nothing'
      -- here would mean the row we are still holding FOR UPDATE vanished
      -- out from under us, which is structurally impossible; rather than
      -- assume that, we degrade to skipping Epic wiring for this one
      -- action instead of crashing -- accepting that the 'ArkhamPlayer'
      -- row inserted just above would, in this unreachable branch, be
      -- rolled back only via 'runDB'\'s normal all-or-nothing semantics
      -- if some LATER step in this same transaction then fails, exactly
      -- as any other write earlier in this function already relies on.
      mEntity <- lockEpicEventRow eventId
      traverse (`mkEpicEnv` ordinal) mEntity

  -- Circuit breaker: cap BOTH runMessages calls below (setup, then the
  -- deck-selection-complete sync pull) at the SAME budget 'updateGame'
  -- uses, via the SAME 'runWithMessagesTimeout' helper, so a pathological
  -- setup can't pin this game row's FOR UPDATE lock (and, for an
  -- Epic-linked game, the event row already locked above) indefinitely.
  -- An uncaught 'RunMessagesTimeout' escapes this 'runDB' transaction and
  -- rolls back the player insert, any membership reservation, and any
  -- shared-delta commit below -- nothing partial is left committed.
  runWithMessagesTimeout gameId runMessagesTimeoutMicros $ runGameApp (GameApp gameRef queueRef genRef (pure . const ()) mEpicEnv) $ do
    addPlayer (PlayerId $ coerce pid)
    -- Run setup. For a multiplayer lobby this PAUSES at ChooseDeck
    -- (IsChooseDecks) until players pick decks, so we must NOT run any
    -- further messages here or we'd blast past deck selection and start
    -- the scenario with zero investigators ("No lead found").
    runMessages (gameIdToText gameId) Nothing
    -- Only when setup actually completed in this request (e.g. a fully
    -- pre-decked/AI group) do we reconcile the board to the shared pool.
    -- Otherwise the reconcile happens after deck selection via the
    -- action pull (StartScenario preserves EpicShared counts) + push.
    setupState <- gameGameState <$> liftIO (readIORef gameRef)
    when (setupState == IsActive) $ for_ mEpicEnv \epic -> do
      shared <- liftIO $ readIORef (epicEnvSharedRef epic)
      pushAll (epicSyncMessages (epicEnvGroup epic) shared)
      runMessages (gameIdToText gameId) Nothing

  updatedGame <- liftIO $ readIORef gameRef
  updatedQueue <- liftIO $ readIORef (queueToRef queueRef)

  -- Commit any shared deltas emitted during setup (same transaction).
  mShared <- case (mEventCtx, mEpicEnv) of
    (Just (eventId, _), Just epic) -> do
      deltas <- liftIO $ readIORef (epicEnvDeltaRef epic)
      if null deltas
        then pure Nothing
        else do
          s <- applyEpicDeltasLocked eventId (Just gameId) (Just (arkhamGameStep + 1)) deltas
          pure $ Just (eventId, s)
    _ -> pure Nothing

  let
    game' =
      ArkhamGame
        arkhamGameName
        updatedGame
        (arkhamGameStep + 1)
        arkhamGameMultiplayerVariant
        arkhamGameCreatedAt
        now

  replace gameId game'
  insert_
    $ ArkhamStep
      gameId
      (Choice mempty updatedQueue)
      (arkhamGameStep + 1)
      (ActionDiff $ view actionDiffL updatedGame)

  pure (game', mShared, fst <$> mEventCtx)
