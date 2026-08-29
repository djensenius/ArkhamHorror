{-# LANGUAGE OverloadedRecordDot #-}

{- | Server-side orchestration for Epic Multiplayer shared state.

The authoritative shared state lives in a single @arkham_epic_events@ row.
A group's engine emits invertible 'SharedDelta's (captured during its action,
see 'Arkham.Game.captureSharedDelta'); this module applies them to the locked
event row and records each as an 'ArkhamEpicStep' so the mutation can be
reverted on undo.
-}
module Api.Arkham.Epic where

import Arkham.Card.CardCode (CardCode (..))
import Arkham.Epic.Types
import Data.List.Extra (nubOrd)
import Data.Set qualified as Set
import Data.Time.Clock (getCurrentTime)
import Database.Esqueleto.Experimental hiding (update, (=.))
import Entity.Arkham.Epic
import Import hiding (on, (==.))
import Import qualified as P

{- | Find the event and this game's group ordinal, if the game is part of an
event. Cheap indexed lookup on the @arkham_epic_groups@ join table, so the
central @arkham_games@ table needs no event columns.
-}
lookupGameEvent
  :: MonadIO m
  => ArkhamGameId
  -> ReaderT SqlBackend m (Maybe (Entity ArkhamEpicEvent, GroupOrdinal))
lookupGameEvent gameId = do
  rows <- select do
    (grp :& evt) <-
      from
        $ table @ArkhamEpicGroup
        `innerJoin` table @ArkhamEpicEvent
          `on` (\(grp :& evt) -> grp.arkhamEpicEventId ==. evt.id)
    where_ $ grp.arkhamGameId ==. just (val gameId)
    pure (evt, grp.ordinal)
  pure $ case rows of
    (evt, Value ordinal) : _ -> Just (evt, GroupOrdinal ordinal)
    [] -> Nothing

{- | The result of attempting to reserve a user's 'Arkham.Epic.Types.GroupPlayer'
membership for an Epic event under a specific group ordinal, via
'Entity.Arkham.Epic.UniqueEpicMember's own unique key (event, user, role).
This is the ONE typed answer to "is this user\/event\/ordinal combination
allowed to proceed", shared by every entry point that can create the FIRST
'ArkhamEpicMember' row for a user in an event --
'Api.Handler.Arkham.Game.Debug.postApiV1ArkhamGameClaimSeatR' delegates to it
via 'reserveEpicGroupMembership' -- so the "one event group per user"
invariant and its conflict semantics cannot independently drift between
entry points that share it.
-}
data EpicGroupReservation
  = -- | Either a freshly-inserted row, or a pre-existing row already under
    -- the SAME ordinal (e.g. re-claiming a seat in a group this user
    -- previously left, if this game's own player row was since removed).
    -- Either way, the caller may proceed.
    EpicGroupReserved
  | -- | A pre-existing row under a DIFFERENT ordinal. No row was inserted or
    -- modified.
    EpicGroupReservationConflict
  deriving stock (Eq, Show)

{- | Row-lock an Epic event ('FOR UPDATE' in production) and report its
CURRENT row if still present, or 'Nothing' if it never existed or vanished
concurrently. Reused by every writer that must serialize against the event
row -- 'Api.Handler.Arkham.Events.deleteEpicEventAggregate' (after every
linked game is already locked; see that module's
'Api.Handler.Arkham.Events.MonadEpicEventDeletion' production instance) and
'Api.Handler.Arkham.Game.Debug.planAndExecuteClaimSeat' (after its own
single game lock; see 'Api.Handler.Arkham.Game.Debug.MonadClaimSeat') -- so
this lock is always taken from the game(s)-then-event side of the one
shared cross-path lock-order invariant, never the reverse.
-}
lockEpicEventRow
  :: MonadIO m
  => ArkhamEpicEventId
  -> ReaderT SqlBackend m (Maybe (Entity ArkhamEpicEvent))
lockEpicEventRow eid = do
  locked <- select do
    e <- from $ table @ArkhamEpicEvent
    where_ $ e.id ==. val eid
    locking forUpdate
    pure e
  pure $ listToMaybe locked

{- | Attempt to reserve this user's 'Arkham.Epic.Types.GroupPlayer' membership
for the given event under the requested ordinal, via
'Entity.Arkham.Epic.UniqueEpicMember's unique key.

The caller MUST already hold the event row's 'FOR UPDATE' lock (see
'lockEpicEventRow') before calling this: only then is the underlying
check-then-insert race-free. 'Database.Persist.insertBy' itself only
pre-checks the unique key with a plain 'SELECT' before inserting -- two
concurrent callers reserving for DIFFERENT games of the SAME event never
share a game lock, so without the event row locked first, both could pass
that pre-check before either commits (a lost-conflict false negative), or
one could lose an actual concurrent 'INSERT' race and surface a raw
unique-constraint violation as an untyped exception instead of a typed
result. With the event already locked exclusively, at most one caller can
ever be inside this function for a given event at a time, so the ordinary
check-then-insert behavior is sufficient by construction and cannot race.
-}
reserveEpicGroupMembership
  :: MonadIO m
  => ArkhamEpicEventId
  -> UserId
  -> Int
  -> ReaderT SqlBackend m EpicGroupReservation
reserveEpicGroupMembership eid userId ordinal = do
  result <- P.insertBy $ ArkhamEpicMember eid userId GroupPlayer (Just ordinal)
  pure $ case result of
    Right _ -> EpicGroupReserved
    Left (Entity _ existing)
      | arkhamEpicMemberGroupOrdinal existing == Just ordinal -> EpicGroupReserved
      | otherwise -> EpicGroupReservationConflict

{- | /Residual, pre-existing, deliberately out-of-scope risk:/
'Api.Handler.Arkham.PendingGames' independently performs its own
@insertBy ArkhamEpicMember ...@ reservation (for a group's very first
player, at game-creation time) followed by a partial 'Prelude.error' on
the losing branch. That code path is untouched by this change (it does
not call 'reserveEpicGroupMembership' or 'lockEpicEventRow') and is not
proven race-free against a concurrent claim-seat reservation for the
same event\/user: nothing there locks the event row first, so two
concurrent \"first join\" attempts into different groups of the same
event could still both pass 'Database.Persist.insertBy'\'s pre-check
before either commits. Restructuring it was judged too broad and risky
for this PR -- it sits deep inside an unrelated, delicate
game-setup\/deck-selection\/message-running transaction
('Api.Arkham.Helpers.atomicallyWithGame') where the 'DB' type alias
already forbids calling 'Yesod.Core.notFound'\/'Yesod.Core.permissionDenied'-style
handler functions, so any real fix would require restructuring that
callback's control flow, not just swapping in a typed result. This is
called out explicitly, rather than silently left, so it is tracked as a
known follow-up rather than mistaken for an already-safe path.
-}

{- | The one canonical lock order for a set of Epic-linked games: ascending by
'ArkhamGameId' itself -- Haskell's own 'Ord' instance for the id, NEVER a
database @ORDER BY@/collation, and, critically, NEVER a group's ordinal --
with duplicates collapsed to a single occurrence each.

This must be independent of which SUBSET of an event's linked games a
particular caller happens to have resolved, and independent of each game's
group ordinal entirely. Two different production writers only ever see
different, overlapping subsets of one event's linked games:
'Api.Handler.Arkham.Events.deleteEpicEventAggregate' locks every game
linked to an event, while
'Api.Handler.Arkham.Games.Shared.mainStreetSwapPlan' locks only the two
games a single swap request resolved. An earlier version of this function
ordered by @(group ordinal, game id)@, with game id only a tie-break -- that
is NOT subset-independent: consider one event with three groups, ordinal 0
and ordinal 2 both linked to game A, and ordinal 1 linked to a different
game B. Deletion, handed all three refs, would sort by ordinal first and
lock @[A, B]@ (A's lowest ordinal, 0, sorts before B's ordinal 1). A swap
naming ordinals 2 and 1 -- a perfectly ordinary request, since it never
sees ordinal 0 at all -- would sort ITS two refs by ordinal and lock
@[B, A]@: B (ordinal 1) before A (ordinal 2). Same underlying pair of
games, opposite lock order, a real cross-path deadlock. Ordering by the
game id's OWN 'Ord' instance instead has no such dependency on which
ordinals happen to be present in a given caller's subset: for any set of
games, or any subset of them, the relative order of any two IDs that both
appear is fixed by the ids alone, and can never disagree between callers
that see different subsets of the same underlying games.

Ordinals remain meaningful elsewhere -- deciding WHICH group/game a request
refers to, and (for a swap) which one is semantically "first" vs "second"
for the actual swap and its response -- but never for deciding lock
ACQUISITION order. See 'Api.Handler.Arkham.Events.selectLinkedGameIds' and
'Api.Handler.Arkham.Games.Shared.mainStreetSwapPlan', both of which now
extract and pass plain 'ArkhamGameId's to this function rather than a
richer ordinal-carrying type, so a caller cannot accidentally reintroduce
ordinal into the ordering by construction.

'nubOrd' (an 'Ord'-based set, not a pairwise/adjacency-dependent scan) keeps
only the first surviving occurrence of each distinct id and drops every
later one, regardless of how far apart the duplicates land in the input --
'deleteEpicEventAggregate' can be handed ids for arbitrarily many linked
games in one event, and nothing forbids two different groups (however far
apart their ordinals are) from referencing the SAME game. A redundant
re-lock of an already-held row is harmless under PostgreSQL's semantics
(re-locking a row a transaction already holds is a no-op), but every
distinct game should still appear exactly once here, as an explicit
invariant of this function, not an accident of what happens to be
harmless.
-}
canonicalEpicGameLockOrder :: [ArkhamGameId] -> [ArkhamGameId]
canonicalEpicGameLockOrder = nubOrd . sort

{- | Build a per-action 'EpicEnv': the current shared state in an 'IORef' plus an
empty delta buffer that the run loop appends to.
-}
mkEpicEnv
  :: MonadIO m => Entity ArkhamEpicEvent -> GroupOrdinal -> ReaderT SqlBackend m EpicEnv
mkEpicEnv (Entity eid e) ordinal = do
  sharedRef <- liftIO $ newIORef (arkhamEpicEventSharedState e)
  deltaRef <- liftIO $ newIORef []
  pure
    EpicEnv
      { epicEnvId = coerce eid
      , epicEnvGroup = ordinal
      , epicEnvSharedRef = sharedRef
      , epicEnvDeltaRef = deltaRef
      }

{- | Bound counters whose physical representation cannot go below zero (and the
Blob's health, which also cannot exceed its printed global maximum). Return the
effective delta so undo reverses exactly what was applied, including overkill
or concurrent spends that were clamped at the event row.
-}
applyBoundedDelta :: SharedEventState -> SharedDelta -> (SharedEventState, SharedDelta)
applyBoundedDelta s d =
  let
    current = sharedCounter d.sharedDeltaKey s
    proposed = current + d.sharedDeltaAmount
    bounded = case d.sharedDeltaKey of
      Countermeasures -> max 0 proposed
      SharedEnemyHealth (CardCode "85037") -> max 0 $ min (15 * sharedTotalInvestigators s) proposed
      SharedEnemyHealth _ -> max 0 proposed
      SharedActProgress _ -> max 0 proposed
      AdvanceRequested _ -> max 0 proposed
      ActAdvanceGen _ -> max 0 proposed
      ActContribution _ _ -> max 0 proposed
      ActSpend _ _ -> max 0 proposed
      _ -> proposed
    effective = d {sharedDeltaAmount = bounded - current}
   in
    (applyDelta effective s, effective)

applyBoundedDeltas :: SharedEventState -> [SharedDelta] -> (SharedEventState, [SharedDelta])
applyBoundedDeltas = go []
 where
  go acc s [] = (s, reverse acc)
  go acc s (d : ds)
    | d.sharedDeltaId `Set.member` sharedAppliedDeltas s = go acc s ds
    | otherwise =
        let (s', effective) = applyBoundedDelta s d
         in go (effective : acc) s' ds

-- Arm a shared Act 1 organizer gate in the same locked write that applies the
-- threshold-crossing action. This prevents the parked Continue question from
-- ever being published while AwaitingOrganizer is still false.
armActAdvanceGates :: SharedEventState -> SharedEventState
armActAdvanceGates s = foldl' arm s (actProgressStages s)
 where
  arm st stage
    | sharedCounter (AdvanceRequested stage) st <= 0 = st
    | otherwise =
        let
          pool = sharedCounter (SharedActProgress stage) st
          threshold = 2 * sharedTotalInvestigators st
          withGate =
            if threshold > 0 && pool >= threshold
              then setSharedCounter (AwaitingOrganizer stage) 1 st
              else st
         in
          setSharedCounter (AdvanceRequested stage) 0 withGate

-- sharedVersion is a monotonic state revision used to reject stale websocket
-- delivery. Existing rows begin at 1; every actual authoritative mutation bumps
-- it once, including direct barrier/organizer bookkeeping.
bumpSharedRevision :: SharedEventState -> SharedEventState -> SharedEventState
bumpSharedRevision old new
  | old == new = old
  | otherwise = new {sharedVersion = sharedVersion old + 1}

{- | Apply a batch of deltas under a @FOR UPDATE@ lock on the event row, persist
the new shared state, and record one 'ArkhamEpicStep' per effective delta.
Returns the new shared state. Lock order is always game-then-event (callers
already hold the game lock), and the lock is held only briefly.
-}
applyEpicDeltasLocked
  :: MonadIO m
  => ArkhamEpicEventId
  -> Maybe ArkhamGameId
  -> Maybe Int
  -> [SharedDelta]
  -> ReaderT SqlBackend m SharedEventState
applyEpicDeltasLocked eid mGameId mGameStep deltas = do
  locked <- select do
    e <- from $ table @ArkhamEpicEvent
    where_ $ e.id ==. val eid
    locking forUpdate
    pure e
  case locked of
    [] -> error "applyEpicDeltasLocked: epic event row vanished mid-transaction"
    (Entity _ e : _) -> do
      now <- liftIO getCurrentTime
      let s0 = arkhamEpicEventSharedState e
          baseStep = arkhamEpicEventStep e
          (sApplied, effectiveDeltas) = applyBoundedDeltas s0 deltas
          s1 = bumpSharedRevision s0 (armActAdvanceGates sApplied)
      P.update
        eid
        [ ArkhamEpicEventSharedState P.=. s1
        , ArkhamEpicEventStep P.=. baseStep + length effectiveDeltas
        , ArkhamEpicEventUpdatedAt P.=. now
        ]
      for_ (zip [1 ..] effectiveDeltas) \(i, d) ->
        insert_ $ ArkhamEpicStep eid (baseStep + i) mGameId mGameStep d now
      pure s1

{- | Apply a pure update to the event's shared state under a @FOR UPDATE@ lock and
persist it, returning the new state. For barrier/timer BOOKKEEPING
(groups-ready bitmask, timer-start) that is set directly rather than as an
undoable additive delta — so it records no 'ArkhamEpicStep'. The update runs
inside the lock, so concurrent callers see each other's writes (e.g. two groups
marking ready at once can't lose a bit).
-}
modifySharedStateLocked
  :: MonadIO m
  => ArkhamEpicEventId
  -> (SharedEventState -> SharedEventState)
  -> ReaderT SqlBackend m SharedEventState
modifySharedStateLocked eid f = fst <$> modifySharedStateLockedWith eid (\s -> (f s, ()))

{- | 'modifySharedStateLocked' that also returns an extra value computed from the
locked pre-update state — e.g. the act-advance coordinator decides INSIDE the
lock whether this call actually consumed the pool (crossed the threshold) and
returns that flag, so a concurrent second resolver (which sees the pool already
reset) can be told it did NOT consume.
-}
modifySharedStateLockedWith
  :: MonadIO m
  => ArkhamEpicEventId
  -> (SharedEventState -> (SharedEventState, a))
  -> ReaderT SqlBackend m (SharedEventState, a)
modifySharedStateLockedWith eid f = do
  locked <- select do
    e <- from $ table @ArkhamEpicEvent
    where_ $ e.id ==. val eid
    locking forUpdate
    pure e
  case locked of
    [] -> error "modifySharedStateLockedWith: epic event row vanished mid-transaction"
    (Entity _ e : _) -> do
      now <- liftIO getCurrentTime
      let
        s0 = arkhamEpicEventSharedState e
        (sChanged, a) = f s0
        s1 = bumpSharedRevision s0 sChanged
      P.update
        eid
        [ ArkhamEpicEventSharedState P.=. s1
        , ArkhamEpicEventUpdatedAt P.=. now
        ]
      pure (s1, a)

{- | Revert deltas that were recorded for a particular game step (used by undo).
Subtracts each delta's amount from the *current* value under lock; additive
deltas commute, so this is correct even if other groups moved the counter in
between. The corresponding 'ArkhamEpicStep' rows are deleted.
-}
revertEpicDeltasForGameStep
  :: MonadIO m
  => ArkhamEpicEventId
  -> ArkhamGameId
  -> Int
  -> ReaderT SqlBackend m (Maybe SharedEventState)
revertEpicDeltasForGameStep eid gameId gameStep = do
  stepRows <- select do
    s <- from $ table @ArkhamEpicStep
    where_ $ s.arkhamEpicEventId ==. val eid
    where_ $ s.arkhamGameId ==. just (val gameId)
    where_ $ s.gameStep ==. just (val gameStep)
    pure s
  let deltas = map (arkhamEpicStepDelta . entityVal) stepRows
  if null deltas
    then pure Nothing
    else do
      locked <- select do
        e <- from $ table @ArkhamEpicEvent
        where_ $ e.id ==. val eid
        locking forUpdate
        pure e
      case locked of
        [] -> pure Nothing
        (Entity _ e : _) -> do
          now <- liftIO getCurrentTime
          let
            s0 = arkhamEpicEventSharedState e
            s1 = bumpSharedRevision s0 $ foldl' (flip revertDelta) s0 deltas
          P.update
            eid
            [ ArkhamEpicEventSharedState P.=. s1
            , ArkhamEpicEventUpdatedAt P.=. now
            ]
          for_ stepRows \(Entity sid _) -> P.delete sid
          pure (Just s1)

{- | The per-game undo FLOOR: the persistence step at/below which undo is walled
off, because crossing it would rewind an epic act-clue advance whose shared
effect (pool reset + generation bump that the OTHER groups then follow) cannot be
locally undone. Set in 'Api.Handler.Arkham.Games.Shared.updateGame' when a group
advances its act IN-GROUP; enforced by 'Api.Handler.Arkham.Undo.stepBack' /
'stepBackToScenarioStep'. 0 (no row) means no floor — ordinary undo.
-}
getGameUndoFloor :: MonadIO m => ArkhamGameId -> ReaderT SqlBackend m Int
getGameUndoFloor gameId = do
  mRow <- P.getBy (UniqueGameUndoFloor gameId)
  pure $ maybe 0 (arkhamGameUndoFloorFloorStep . entityVal) mRow
