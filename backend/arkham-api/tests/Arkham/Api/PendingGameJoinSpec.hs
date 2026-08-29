{- | Proves the game-locked-first pending-game (lobby) join sequencing
described in 'Api.Handler.Arkham.PendingGames.planAndExecutePendingJoin' \/
'Api.Handler.Arkham.PendingGames.planPendingJoinMembership' without a live
database, and that its Epic membership reservation is the SAME shared
primitive\/invariant 'Api.Handler.Arkham.Game.Debug.planAndExecuteClaimSeat'
uses for its own, independent, seat-claim flow.

A pure, in-memory 'MonadPendingJoin' instance ('TestDB') runs the exact same
production decision function ('planPendingJoinMembership') and records every
step it performs. This lets us assert:

* a user who already holds a player row in THIS (already-locked), NON-Epic
  game reports 'PendingJoinAlreadyMember' with no event lookup, lock, or
  reservation even attempted (there is no event to reconcile against) --
  the idempotent, doubled-click re-join case; for an EPIC-LINKED game, the
  SAME idempotent re-join instead reports 'PendingJoinAlreadyMember' only
  AFTER event-membership reconciliation has ALREADY locked the event and
  reserved\/repaired this user's own group membership as a side effect --
  see the dedicated legacy-seat reconciliation tests below;
* a game with no linked Epic event at all reports 'PendingJoinNoEvent'
  and never locks an event or attempts a reservation;
* a game linked to an Epic event locks that event row ('lockPendingJoinEvent')
  and only THEN actually RESERVES this user's 'GroupPlayer' membership
  ('reservePendingJoinMembership') -- a genuine mutation through
  'Entity.Arkham.Epic.UniqueEpicMember's own unique key, not a mere read --
  reporting 'PendingJoinConflict' for a pre-existing reservation under a
  DIFFERENT ordinal, but treating a pre-existing reservation under the SAME
  ordinal (or no prior reservation at all) as 'PendingJoinReserved';
* an event that vanished concurrently between the (locked) game and the
  event lock reports 'PendingJoinEventVanished', structurally unreachable in
  production (see 'lockPendingJoinEvent'\'s Haddoc) but handled as a typed
  outcome rather than assumed impossible;
* two complete, SEQUENTIAL production-seam joins by the same user into two
  DIFFERENT groups of the SAME event demonstrate the actual bug this round
  fixes: the first reserves the membership and succeeds; the second,
  against that same (mutated) membership state, is rejected with
  'PendingJoinConflict';
* the reservation decision itself is order-independent set-membership
  logic, exactly mirroring "Arkham.Api.ClaimSeatSpec"'s equivalent proof:
  whichever reservation attempt runs first against an empty map "wins", and
  any later attempt deterministically either agrees (same ordinal) or loses
  (different ordinal) once it observes the row the winner already
  committed -- the closest a live-database-free seam can get to proving the
  "unique-key loser" side of a genuine concurrent race;
* a pending join and a claim-seat claim, run against the SAME threaded
  membership map via 'Api.Handler.Arkham.Game.Debug.planAndExecuteClaimSeat',
  genuinely observe and mutate ONE shared invariant: a user who joins one
  group of an event via the pending-lobby path is then rejected claiming a
  DIFFERENT group of the SAME event via claim-seat, and vice versa -- proving
  the two entry points cannot independently drift, because both ultimately
  delegate to the identical 'Api.Arkham.Epic.reserveEpicGroupMembership'
  primitive (mirrored here by the identical pure reservation semantics in
  both instances below);
* legacy-seat reconciliation -- a bare 'Entity.Arkham.Player.ArkhamPlayer'
  row in some OTHER game of this event with NO membership row at all,
  predating the reservation machinery entirely -- is modelled directly by
  'reservePendingJoinMembership'\'s own pure formula (mirroring
  'Api.Arkham.Epic.reserveEpicGroupMembershipReconciling' exactly): a seat
  in a DIFFERENT ordinal than requested conflicts with zero writes; a seat
  ONLY in the requested ordinal is idempotently repaired (a membership row
  is created even though no player row previously recorded one); seats
  spanning two different legacy ordinals always conflict, whichever is
  requested; and this reconciliation genuinely runs even for the
  already-a-member branch (a legacy seat elsewhere is still caught for an
  otherwise idempotent re-join);
* a failure injected at any single step -- the already-member check, the
  event lock, or the membership reservation -- can never produce a
  success-shaped ('PendingJoinReserved' or 'PendingJoinNoEvent') result, and
  no step after the injected failure is attempted.

As with "Arkham.Api.ClaimSeatSpec", "Arkham.Api.Events.EventDeletionSpec",
and "Arkham.Api.MainStreetSwapSpec", this pure interpreter's step log (and
its mutable membership map) is evidence of the deterministic order
operations were attempted in, where a sequence short-circuited, and what
state a successful reservation actually recorded -- it is NOT a simulation
of transactional rollback. A failure injected AFTER a reservation was
recorded still leaves that reservation "committed" in this pure model's
'TestState' (see the dedicated test below): actual rollback of a
partially-applied write on a real injected failure (including any later
game\/player\/step\/shared-state write 'Api.Handler.Arkham.PendingGames.runPendingJoinSetup'
performs, entirely outside this class's testable surface) is a property of
'runDB' (an uncaught exception aborts the whole transaction), never of this
fake interpreter's log or map. The deeply embedded game-engine setup itself
('runGameApp'\/'addPlayer'\/'runMessages'\/'applyEpicDeltasLocked') is, as in
the original PR this module extends, not independently unit-testable and is
therefore not exercised here; this module proves only the membership
reservation decision that GATES whether that setup ever runs, exactly the
seam production actually branches on (see 'planAndExecutePendingJoin').
-}
module Arkham.Api.PendingGameJoinSpec (spec) where

import Api.Arkham.Epic (EpicGroupReservation (..))
import Api.Handler.Arkham.Game.Debug (
  ClaimSeatFailure (..),
  ClaimSeatOutcome (..),
  MonadClaimSeat (..),
  planAndExecuteClaimSeat,
 )
import Api.Handler.Arkham.PendingGames
import Api.Arkham.Types.MultiplayerVariant
import Arkham.Card.CardCode
import Arkham.Difficulty (Difficulty (Standard))
import Arkham.Epic.Types (GroupOrdinal (..))
import Arkham.Game
import Arkham.Id
import Arkham.Prelude
import Control.Monad.Except (ExceptT, MonadError, runExceptT, throwError)
import Control.Monad.State.Strict (MonadState, State, gets, modify, runState)
import Data.Either (isLeft)
import Data.Map.Strict qualified as Map
import Data.UUID qualified as UUID
import Database.Persist.Sql (toSqlKey)
import Entity.Arkham.Epic qualified as Epic
import Entity.Arkham.Game qualified as GameEntity
import Entity.User qualified as User
import Test.Hspec

-- Fixtures ------------------------------------------------------------------

fixtureGameId :: GameEntity.ArkhamGameId
fixtureGameId = GameEntity.ArkhamGameKey $ UUID.fromWords 0 0 0 1

fixtureOtherGameId :: GameEntity.ArkhamGameId
fixtureOtherGameId = GameEntity.ArkhamGameKey $ UUID.fromWords 0 0 0 2

fixtureEventId :: Epic.ArkhamEpicEventId
fixtureEventId = Epic.ArkhamEpicEventKey $ UUID.fromWords 0 0 0 999

fixtureUserId :: Int -> User.UserId
fixtureUserId = toSqlKey . fromIntegral

-- Pure, in-memory persistence backend for 'MonadPendingJoin' -----------------

-- | One recorded persistence step, in the order it happened.
data Step
  = CheckedAlreadyMember Bool
  | -- | the event was locked ('FOR UPDATE'), and whether it was still present
    LockedEvent Bool
  | -- | the actual reservation attempt against 'UniqueEpicMember', and its
    -- typed result
    ReservedMembership EpicGroupReservation
  deriving stock (Eq, Show)

-- | Which single step (if any) should fail, for a given test run.
data FailAt
  = FailNever
  | FailAtAlreadyMember
  | FailAtLockEvent
  | FailAtReserve
  deriving stock (Eq, Show)

{- | Mutable state threaded through 'TestDB'. 'membership' is a genuine,
mutable analogue of the @arkham_epic_members@ table's 'UniqueEpicMember'
unique key (event, user) -> ordinal -- 'reservePendingJoinMembership'
actually reads AND writes it, exactly like the production
'Api.Arkham.Epic.reserveEpicGroupMembership' it delegates to, and exactly
like "Arkham.Api.ClaimSeatSpec"'s own 'reserveClaimSeatMembership'
analogue. 'legacySeats' is an INDEPENDENT analogue of every OTHER game's
linked 'Entity.Arkham.Player.ArkhamPlayer' row for this (event, user) that
predates the reservation machinery entirely -- a bare seat with no
membership row at all -- exactly what
'Api.Arkham.Epic.reserveEpicGroupMembershipReconciling' additionally
queries ('Api.Arkham.Epic.selectUserEpicSeatOrdinals') before ever
consulting or writing 'membership'.
-}
data TestState = TestState
  { steps :: [Step]
  , alreadyMember :: Bool
  , eventPresent :: Bool
  , membership :: Map (Epic.ArkhamEpicEventId, User.UserId) Int
  , legacySeats :: Map (Epic.ArkhamEpicEventId, User.UserId) [Int]
  }

-- | The common case: no pre-existing player row in this game, the event
-- (when supplied) still present, no pre-existing membership/legacy-seat
-- conflict, zero steps recorded yet.
fixtureTestState :: TestState
fixtureTestState =
  TestState
    { steps = []
    , alreadyMember = False
    , eventPresent = True
    , membership = Map.empty
    , legacySeats = Map.empty
    }

{- | A pure interpreter of 'MonadPendingJoin': records every step it is asked
to perform (mutating 'membership' exactly as a real reservation would) and
short-circuits (via 'ExceptT') at the configured 'FailAt' step, exactly the
way an uncaught exception aborts a real 'runDB' transaction.
-}
newtype TestDB a = TestDB
  {unTestDB :: ReaderT FailAt (ExceptT String (State TestState)) a}
  deriving newtype
    ( Functor
    , Applicative
    , Monad
    , MonadReader FailAt
    , MonadError String
    , MonadState TestState
    )

runTestDB :: FailAt -> TestState -> TestDB a -> (Either String a, [Step])
runTestDB failAt initial action =
  let (result, finalState) = runTestDBWithState failAt initial action
   in (result, finalState.steps)

-- | Run a join, returning the result and the FULL final state -- needed to
-- chain a second, sequential join against the first join's committed
-- membership map (see the cross-group reservation tests below).
runTestDBWithState :: FailAt -> TestState -> TestDB a -> (Either String a, TestState)
runTestDBWithState failAt initial action =
  runState (runExceptT (runReaderT (unTestDB action) failAt)) initial

failIfConfigured :: FailAt -> TestDB ()
failIfConfigured this = do
  configured <- ask
  when (configured == this) $ throwError (show this)

recordStep :: Step -> TestDB ()
recordStep step = modify \s -> s {steps = s.steps ++ [step]}

instance MonadPendingJoin TestDB where
  hasExistingPendingPlayer _gid _uid = do
    failIfConfigured FailAtAlreadyMember
    already <- gets (.alreadyMember)
    recordStep (CheckedAlreadyMember already)
    pure already

  lockPendingJoinEvent _eid = do
    failIfConfigured FailAtLockEvent
    present <- gets (.eventPresent)
    recordStep (LockedEvent present)
    pure present

  -- Mirrors 'Api.Arkham.Epic.reserveEpicGroupMembershipReconciling' exactly:
  -- FIRST, any seat (via 'legacySeats') under a DIFFERENT ordinal than
  -- requested conflicts outright, with 'membership' left untouched (the
  -- legacy-seat reconciliation this round adds); OTHERWISE falls through
  -- to the ordinary 'Api.Arkham.Epic.reserveEpicGroupMembership' formula:
  -- a fresh key inserts and reserves; an existing key under the SAME
  -- ordinal is idempotently reserved (no map change); an existing key
  -- under a DIFFERENT ordinal conflicts and the map is left untouched.
  reservePendingJoinMembership eid userId ordinal = do
    failIfConfigured FailAtReserve
    seated <- gets (Map.findWithDefault [] (eid, userId) . (.legacySeats))
    if any (/= ordinal) seated
      then do
        recordStep (ReservedMembership EpicGroupReservationConflict)
        pure EpicGroupReservationConflict
      else do
        existing <- gets (Map.lookup (eid, userId) . (.membership))
        let reservation = case existing of
              Nothing -> EpicGroupReserved
              Just existingOrdinal
                | existingOrdinal == ordinal -> EpicGroupReserved
                | otherwise -> EpicGroupReservationConflict
        when (reservation == EpicGroupReserved)
          $ modify \s -> s {membership = Map.insert (eid, userId) ordinal s.membership}
        recordStep (ReservedMembership reservation)
        pure reservation

-- Pure, in-memory persistence backend for 'MonadClaimSeat' (cross-module
-- shared-invariant proof only) ------------------------------------------------

{- | A second, independent 'TestDB'-style interpreter, this time for
"Arkham.Api.Game.Debug"'s 'MonadClaimSeat', threaded against the SAME
underlying membership 'Map' a 'TestDB' \/ 'MonadPendingJoin' run already
mutated. This is how this module proves the two entry points share ONE
invariant without needing either module to expose its own private test
harness: both interpreters are thin, and both delegate their reservation
step to the identical pure semantics 'Api.Arkham.Epic.reserveEpicGroupMembership'
itself implements, so threading the SAME 'Map' through both proves the
cross-module claim.
-}
newtype ClaimTestDB a = ClaimTestDB
  {unClaimTestDB :: State (Map (Epic.ArkhamEpicEventId, User.UserId) Int) a}
  deriving newtype (Functor, Applicative, Monad, MonadState (Map (Epic.ArkhamEpicEventId, User.UserId) Int))

runClaimTestDB
  :: Map (Epic.ArkhamEpicEventId, User.UserId) Int
  -> ClaimTestDB a
  -> (a, Map (Epic.ArkhamEpicEventId, User.UserId) Int)
runClaimTestDB initial action = runState (unClaimTestDB action) initial

-- | A minimal, fully-forced 'WithFriends' game whose player order contains
-- exactly one fixture investigator, and which never has an already-taken or
-- already-joined seat -- only the Epic-membership branch is under test here.
instance MonadClaimSeat ClaimTestDB where
  lockClaimSeatGame _gid =
    pure
      $ Just
        GameEntity.ArkhamGame
          { GameEntity.arkhamGameName = "fixture"
          , GameEntity.arkhamGameCurrentData =
              (newCampaign "06" Nothing 0 1 Standard False) {gamePlayerOrder = [rolandId]}
          , GameEntity.arkhamGameStep = 0
          , GameEntity.arkhamGameMultiplayerVariant = WithFriends
          , GameEntity.arkhamGameCreatedAt = fixtureTime
          , GameEntity.arkhamGameUpdatedAt = fixtureTime
          }
   where
    fixtureTime = UTCTime (ModifiedJulianDay 0) 0
    rolandId = InvestigatorId (CardCode "01001")
  lookupClaimSeatEvent _gid = pure (Just (fixtureEventId, claimGroupOrdinal))
   where
    claimGroupOrdinal = 3
  lookupClaimSeatOccupants _gid _investigatorId = pure []
  isClaimSeatAlreadyJoined _uid _gid = pure False
  lockClaimSeatEvent _eid = pure True
  reserveClaimSeatMembership eid userId ordinal = do
    existing <- gets (Map.lookup (eid, userId))
    let reservation = case existing of
          Nothing -> EpicGroupReserved
          Just existingOrdinal
            | existingOrdinal == ordinal -> EpicGroupReserved
            | otherwise -> EpicGroupReservationConflict
    when (reservation == EpicGroupReserved) $ modify (Map.insert (eid, userId) ordinal)
    pure reservation
  insertClaimSeatPlayer _uid _gid _investigatorId = pure ()

-- Specs -----------------------------------------------------------------------

spec :: Spec
spec = describe "planPendingJoinMembership (game-locked-first pending-join membership decision sequencing)" do
  let run failAt state mEvent =
        runTestDB failAt state (planPendingJoinMembership fixtureGameId (fixtureUserId 1) mEvent)

  it "a user who already holds a player row in this NON-Epic game reports AlreadyMember, with no event lookup/lock/reservation even attempted (no event to reconcile against)" do
    let (result, log_) = run FailNever fixtureTestState {alreadyMember = True} Nothing
    result `shouldBe` Right PendingJoinAlreadyMember
    log_ `shouldBe` [CheckedAlreadyMember True]

  it "a user who already holds a player row in an EPIC-linked game still reports AlreadyMember, but only AFTER event-membership reconciliation has ALREADY run and repaired this user's own group membership as a side effect" do
    let (result, log_) = run FailNever fixtureTestState {alreadyMember = True} (Just (fixtureEventId, 0))
    result `shouldBe` Right PendingJoinAlreadyMember
    log_ `shouldBe` [LockedEvent True, ReservedMembership EpicGroupReserved, CheckedAlreadyMember True]

  it "a game with no linked Epic event at all reports NoEvent and never locks an event or attempts a reservation" do
    let (result, log_) = run FailNever fixtureTestState Nothing
    result `shouldBe` Right PendingJoinNoEvent
    log_ `shouldBe` [CheckedAlreadyMember False]

  it "a fresh reservation into a linked event's own group succeeds: the event is locked and the membership actually reserved BEFORE the already-member check is even consulted" do
    let (result, log_) = run FailNever fixtureTestState (Just (fixtureEventId, 0))
    result `shouldBe` Right (PendingJoinReserved fixtureEventId (GroupOrdinal 0))
    log_
      `shouldBe` [LockedEvent True, ReservedMembership EpicGroupReserved, CheckedAlreadyMember False]

  it "a pre-existing GroupPlayer reservation under the SAME ordinal (re-joining this very group) is correctly NOT a conflict" do
    let seeded = Map.singleton (fixtureEventId, fixtureUserId 1) 2
        (result, log_) = run FailNever fixtureTestState {membership = seeded} (Just (fixtureEventId, 2))
    result `shouldBe` Right (PendingJoinReserved fixtureEventId (GroupOrdinal 2))
    log_
      `shouldBe` [LockedEvent True, ReservedMembership EpicGroupReserved, CheckedAlreadyMember False]

  it "a pre-existing GroupPlayer reservation under a DIFFERENT ordinal reports Conflict and writes nothing -- the sequential cross-group bug this round fixes -- and the already-member check is never even reached" do
    let seeded = Map.singleton (fixtureEventId, fixtureUserId 1) 1
        (result, log_) = run FailNever fixtureTestState {membership = seeded} (Just (fixtureEventId, 0))
    result `shouldBe` Right PendingJoinConflict
    log_
      `shouldBe` [LockedEvent True, ReservedMembership EpicGroupReservationConflict]

  it "an event that vanished concurrently between the game lock and the event lock reports EventVanished, with the already-member check never even reached" do
    let (result, log_) = run FailNever fixtureTestState {eventPresent = False} (Just (fixtureEventId, 0))
    result `shouldBe` Right PendingJoinEventVanished
    log_ `shouldBe` [LockedEvent False]

  it "two complete SEQUENTIAL joins by the same user into two DIFFERENT groups of the SAME event: the first reserves and succeeds, the second is rejected with the membership map untouched" do
    let firstGroupOrdinal = 0
        secondGroupOrdinal = 1
        (firstResult, afterFirst) =
          runTestDBWithState
            FailNever
            fixtureTestState
            (planPendingJoinMembership fixtureGameId (fixtureUserId 1) (Just (fixtureEventId, firstGroupOrdinal)))
    firstResult `shouldBe` Right (PendingJoinReserved fixtureEventId (GroupOrdinal firstGroupOrdinal))
    Map.lookup (fixtureEventId, fixtureUserId 1) afterFirst.membership `shouldBe` Just firstGroupOrdinal
    -- Simulate a SECOND, distinct game/group of the SAME event: reset the
    -- step log to isolate this call's trace, but thread the SAME
    -- (committed) membership map through -- exactly as one shared
    -- @arkham_epic_members@ table would carry the first join's row into a
    -- second, later transaction against a different game.
    let secondCallState = afterFirst {steps = []}
        (secondResult, afterSecond) =
          runTestDBWithState
            FailNever
            secondCallState
            (planPendingJoinMembership fixtureOtherGameId (fixtureUserId 1) (Just (fixtureEventId, secondGroupOrdinal)))
    secondResult `shouldBe` Right PendingJoinConflict
    -- the FIRST reservation is untouched -- no overwrite, no second row
    Map.lookup (fixtureEventId, fixtureUserId 1) afterSecond.membership `shouldBe` Just firstGroupOrdinal

  describe "reservePendingJoinMembership (production-used Epic membership reservation seam, exercised directly)" do
    let reserve eid uid ordinal state =
          runTestDBWithState FailNever state (reservePendingJoinMembership eid uid ordinal)

    it "a fresh reservation against an empty map is an ordinary insert: Reserved, and the map is updated" do
      let (result, finalState) = reserve fixtureEventId (fixtureUserId 1) 0 fixtureTestState
      result `shouldBe` Right EpicGroupReserved
      Map.lookup (fixtureEventId, fixtureUserId 1) finalState.membership `shouldBe` Just 0

    it "the 'unique-key loser': a second reservation attempt for the SAME (event, user) under a DIFFERENT ordinal conflicts, and the map is left untouched by the loser" do
      let seeded = Map.singleton (fixtureEventId, fixtureUserId 1) 0
          (result, finalState) = reserve fixtureEventId (fixtureUserId 1) 1 fixtureTestState {membership = seeded}
      result `shouldBe` Right EpicGroupReservationConflict
      Map.lookup (fixtureEventId, fixtureUserId 1) finalState.membership `shouldBe` Just 0

  it "for a NON-Epic game, a failure checking whether this user is already a member cannot produce a success-shaped result, and no other step is ever attempted" do
    let (result, log_) = run FailAtAlreadyMember fixtureTestState Nothing
    result `shouldSatisfy` isLeft
    log_ `shouldBe` []

  it "for an EPIC-linked game, a failure checking whether this user is already a member cannot produce a success-shaped result, proving the event was genuinely locked AND reconciled/reserved first" do
    let (result, log_) = run FailAtAlreadyMember fixtureTestState (Just (fixtureEventId, 0))
    result `shouldSatisfy` isLeft
    log_ `shouldBe` [LockedEvent True, ReservedMembership EpicGroupReserved]

  it "a failure locking the event cannot produce a success-shaped result, and no other step is ever attempted (the event lock is the FIRST action for an Epic-linked game)" do
    let (result, log_) = run FailAtLockEvent fixtureTestState (Just (fixtureEventId, 0))
    result `shouldSatisfy` isLeft
    log_ `shouldBe` []

  it "a failure reserving Epic membership cannot produce a success-shaped result, proving the event was genuinely locked first" do
    let (result, log_) = run FailAtReserve fixtureTestState (Just (fixtureEventId, 0))
    result `shouldSatisfy` isLeft
    log_ `shouldBe` [LockedEvent True]

  it "a failure reserving membership AFTER the event was genuinely locked still cannot produce a success-shaped result -- and the pure model's step log, unlike real runDB, does NOT roll back (documented, not a rollback claim)" do
    let (result, finalState) =
          runTestDBWithState
            FailAtReserve
            fixtureTestState
            (planPendingJoinMembership fixtureGameId (fixtureUserId 1) (Just (fixtureEventId, 0)))
    result `shouldSatisfy` isLeft
    finalState.steps `shouldBe` [LockedEvent True]
    -- No reservation was attempted this time (the injected failure is
    -- BEFORE the reservation call itself), so the membership map is
    -- untouched -- distinguishing "failed before reserving" from
    -- ClaimSeatSpec's analogous "failed after reserving" case.
    Map.lookup (fixtureEventId, fixtureUserId 1) finalState.membership `shouldBe` Nothing

  describe "cross-module shared invariant: pending-join and claim-seat reservations observe and mutate ONE membership map" do
    it "a pending join into group 0 of an event, then a claim-seat claim into a DIFFERENT group of the SAME event/user, is rejected -- proving claim-seat sees the pending join's reservation" do
      let (pendingResult, afterPending) =
            runTestDBWithState
              FailNever
              fixtureTestState
              (planPendingJoinMembership fixtureGameId (fixtureUserId 1) (Just (fixtureEventId, 0)))
      pendingResult `shouldBe` Right (PendingJoinReserved fixtureEventId (GroupOrdinal 0))
      let (claimResult, afterClaim) =
            runClaimTestDB afterPending.membership
              $ planAndExecuteClaimSeat fixtureGameId (fixtureUserId 1) "c01001"
      -- ClaimTestDB's fixture game is linked to event group ordinal 3 (a
      -- DIFFERENT group than the pending join's ordinal 0), so the shared
      -- membership map correctly rejects it.
      claimResult `shouldBe` ClaimSeatRejected ClaimSeatEventMembershipConflict
      Map.lookup (fixtureEventId, fixtureUserId 1) afterClaim `shouldBe` Just 0

    it "a claim-seat claim into group 3, then a pending join into a DIFFERENT group of the SAME event/user, is rejected -- proving pending-join sees claim-seat's reservation" do
      let (claimResult, afterClaim) =
            runClaimTestDB Map.empty $ planAndExecuteClaimSeat fixtureGameId (fixtureUserId 1) "c01001"
      claimResult `shouldBe` ClaimSeatClaimed
      Map.lookup (fixtureEventId, fixtureUserId 1) afterClaim `shouldBe` Just 3
      let (pendingResult, _afterPending) =
            runTestDBWithState
              FailNever
              fixtureTestState {membership = afterClaim}
              (planPendingJoinMembership fixtureOtherGameId (fixtureUserId 1) (Just (fixtureEventId, 0)))
      pendingResult `shouldBe` Right PendingJoinConflict

    it "a claim-seat claim, then a pending join into the SAME group ordinal, is correctly NOT a conflict -- both entry points agree on same-group idempotence" do
      let (claimResult, afterClaim) =
            runClaimTestDB Map.empty $ planAndExecuteClaimSeat fixtureGameId (fixtureUserId 1) "c01001"
      claimResult `shouldBe` ClaimSeatClaimed
      let (pendingResult, _afterPending) =
            runTestDBWithState
              FailNever
              fixtureTestState {membership = afterClaim}
              (planPendingJoinMembership fixtureOtherGameId (fixtureUserId 1) (Just (fixtureEventId, 3)))
      pendingResult `shouldBe` Right (PendingJoinReserved fixtureEventId (GroupOrdinal 3))

  describe "legacy-seat reconciliation (a pre-existing ArkhamPlayer row in ANOTHER game of this event, with NO membership row at all, predating the reservation machinery)" do
    it "a legacy seat in a DIFFERENT ordinal than requested is a typed conflict, with zero membership writes and the already-member check never even reached" do
      let seededLegacy = Map.singleton (fixtureEventId, fixtureUserId 1) [2]
          (result, finalState) =
            runTestDBWithState
              FailNever
              fixtureTestState {legacySeats = seededLegacy}
              (planPendingJoinMembership fixtureGameId (fixtureUserId 1) (Just (fixtureEventId, 0)))
      result `shouldBe` Right PendingJoinConflict
      finalState.steps `shouldBe` [LockedEvent True, ReservedMembership EpicGroupReservationConflict]
      Map.lookup (fixtureEventId, fixtureUserId 1) finalState.membership `shouldBe` Nothing

    it "a legacy seat ONLY in the requested ordinal (this very group) is idempotently repaired: a membership row is created even though no player row previously recorded one" do
      let seededLegacy = Map.singleton (fixtureEventId, fixtureUserId 1) [0]
          (result, finalState) =
            runTestDBWithState
              FailNever
              fixtureTestState {legacySeats = seededLegacy}
              (planPendingJoinMembership fixtureGameId (fixtureUserId 1) (Just (fixtureEventId, 0)))
      result `shouldBe` Right (PendingJoinReserved fixtureEventId (GroupOrdinal 0))
      Map.lookup (fixtureEventId, fixtureUserId 1) finalState.membership `shouldBe` Just 0

    it "seats recorded across TWO different legacy ordinals reject regardless of which ordinal is requested -- an inconsistent legacy state is never silently resolved" do
      let seededLegacy = Map.singleton (fixtureEventId, fixtureUserId 1) [1, 2]
          (result, finalState) =
            runTestDBWithState
              FailNever
              fixtureTestState {legacySeats = seededLegacy}
              (planPendingJoinMembership fixtureGameId (fixtureUserId 1) (Just (fixtureEventId, 1)))
      result `shouldBe` Right PendingJoinConflict
      Map.lookup (fixtureEventId, fixtureUserId 1) finalState.membership `shouldBe` Nothing

    it "the already-a-member branch genuinely invokes reconciliation: a legacy seat in a DIFFERENT game's group is caught even for a user re-joining their OWN existing seat" do
      let seededLegacy = Map.singleton (fixtureEventId, fixtureUserId 1) [7]
          (result, finalState) =
            runTestDBWithState
              FailNever
              fixtureTestState {alreadyMember = True, legacySeats = seededLegacy}
              (planPendingJoinMembership fixtureGameId (fixtureUserId 1) (Just (fixtureEventId, 0)))
      -- Reconciliation runs and conflicts BEFORE 'CheckedAlreadyMember' is
      -- ever consulted -- the conflict outcome, not 'PendingJoinAlreadyMember',
      -- is reported, and 'CheckedAlreadyMember' never appears in the log.
      result `shouldBe` Right PendingJoinConflict
      CheckedAlreadyMember True `shouldSatisfy` (`notElem` finalState.steps)
      CheckedAlreadyMember False `shouldSatisfy` (`notElem` finalState.steps)

    it "a user with NO legacy seat at all and no prior membership reserves normally -- the common, unaffected case" do
      let (result, finalState) =
            runTestDBWithState
              FailNever
              fixtureTestState
              (planPendingJoinMembership fixtureGameId (fixtureUserId 1) (Just (fixtureEventId, 0)))
      result `shouldBe` Right (PendingJoinReserved fixtureEventId (GroupOrdinal 0))
      Map.lookup (fixtureEventId, fixtureUserId 1) finalState.membership `shouldBe` Just 0

    it "reservation itself (exercised directly): a legacy seat in a different ordinal conflicts even against an otherwise-empty membership map" do
      let seededLegacy = Map.singleton (fixtureEventId, fixtureUserId 1) [5]
          (result, finalState) =
            runTestDBWithState
              FailNever
              fixtureTestState {legacySeats = seededLegacy}
              (reservePendingJoinMembership fixtureEventId (fixtureUserId 1) 9)
      result `shouldBe` Right EpicGroupReservationConflict
      Map.lookup (fixtureEventId, fixtureUserId 1) finalState.membership `shouldBe` Nothing

    it "sequential reservations: a first user's legacy-seat repair does not affect a second, unrelated user's fresh reservation into the same event" do
      let seededLegacy = Map.singleton (fixtureEventId, fixtureUserId 1) [4]
          (firstResult, afterFirst) =
            runTestDBWithState
              FailNever
              fixtureTestState {legacySeats = seededLegacy}
              (reservePendingJoinMembership fixtureEventId (fixtureUserId 1) 4)
      firstResult `shouldBe` Right EpicGroupReserved
      let (secondResult, afterSecond) =
            runTestDBWithState FailNever afterFirst (reservePendingJoinMembership fixtureEventId (fixtureUserId 2) 0)
      secondResult `shouldBe` Right EpicGroupReserved
      Map.lookup (fixtureEventId, fixtureUserId 1) afterSecond.membership `shouldBe` Just 4
      Map.lookup (fixtureEventId, fixtureUserId 2) afterSecond.membership `shouldBe` Just 0

    it "the 'unique-key loser' side of a genuine concurrent race, now with a legacy seat in the mix: the WINNER's repair commits first, and a LOSER attempt for the SAME user under a DIFFERENT ordinal still conflicts against the winner's committed row" do
      let seededLegacy = Map.singleton (fixtureEventId, fixtureUserId 1) [0]
          (winnerResult, afterWinner) =
            runTestDBWithState
              FailNever
              fixtureTestState {legacySeats = seededLegacy}
              (reservePendingJoinMembership fixtureEventId (fixtureUserId 1) 0)
      winnerResult `shouldBe` Right EpicGroupReserved
      let (loserResult, afterLoser) =
            runTestDBWithState FailNever afterWinner (reservePendingJoinMembership fixtureEventId (fixtureUserId 1) 1)
      loserResult `shouldBe` Right EpicGroupReservationConflict
      Map.lookup (fixtureEventId, fixtureUserId 1) afterLoser.membership `shouldBe` Just 0

    it "an Organizer-role membership row is a structurally SEPARATE key ('UniqueEpicMember (eventId, userId, Organizer)' vs '(eventId, userId, GroupPlayer)') that this reconciliation neither reads nor writes: an organizer with no ArkhamPlayer seat anywhere reserves a GroupPlayer seat exactly as any other user would" do
      let (result, finalState) =
            runTestDBWithState
              FailNever
              -- 'selectUserEpicSeatOrdinals' joins 'ArkhamEpicGroup' to
              -- 'ArkhamPlayer' alone, which has no role column at all, so an
              -- Organizer-role row (which need not ever have a
              -- corresponding player row) can never appear in
              -- 'legacySeats' or be conflated with a GroupPlayer seat here.
              fixtureTestState
              (planPendingJoinMembership fixtureGameId (fixtureUserId 1) (Just (fixtureEventId, 0)))
      result `shouldBe` Right (PendingJoinReserved fixtureEventId (GroupOrdinal 0))
      Map.lookup (fixtureEventId, fixtureUserId 1) finalState.membership `shouldBe` Just 0
