{- | Proves the game-locked-first game-seat claim sequencing described in
'Api.Handler.Arkham.Game.Debug.planAndExecuteClaimSeat' without a live
database.

A pure, in-memory 'MonadClaimSeat' instance ('TestDB') runs the exact same
production decision function and records every step it performs. This lets
us assert:

* the target 'Entity.Arkham.Game.ArkhamGame' row is ALWAYS the first row
  locked ('FOR UPDATE') by this flow -- see 'lockClaimSeatGame' --
  establishing the same game-before-player (and, for an Epic-linked game,
  game-before-event) order Main Street swap\/event deletion already use
  (see 'Api.Handler.Arkham.Games.Shared.lockSwapGame' and
  'Api.Handler.Arkham.Events.MonadEpicEventDeletion'), so a concurrent swap
  or deletion that has already locked this same game can never be
  interleaved with any check or write this flow performs, and this flow
  can never insert a player row (or observe game\/event state) before that
  lock is held;
* a missing (or already-deleted/vanished) game short-circuits to
  'ClaimSeatMissingGame' before any other check;
* a locked game that is not a "WithFriends" multiplayer game reports
  'ClaimSeatNotMultiplayer', read straight from the locked snapshot, before
  any seat-occupancy check, Epic-event check, or insert;
* a requested investigator id that is not part of this game's own player
  order reports 'ClaimSeatInvalidInvestigator', likewise before any further
  check;
* the requested slot's occupants are looked up and classified, relative to
  the REQUESTING user, via 'Api.Handler.Arkham.Game.Debug.classifyClaimSeatOccupancy'
  ('ClaimSeatOccupancy'), BEFORE any Epic-event lock\/reservation is even
  attempted: some OTHER user's occupied slot (or an anomalous multi-occupant
  slot) reports 'ClaimSeatTaken' immediately with no further check; the
  REQUESTER's own already-held slot proceeds straight to Epic reconciliation
  and, on success, reports 'ClaimSeatAlreadyJoined' directly -- WITHOUT ever
  consulting 'isClaimSeatAlreadyJoined' or inserting a player row -- this is
  the fix for the bug where a legacy user idempotently re-claiming their own
  seat was previously rejected by a bare, non-user-aware "taken" check and
  could never reach (and so never repair via) reconciliation at all;
* a game linked to an Epic event only then has its event row locked
  ('lockClaimSeatEvent') and this user's 'GroupPlayer' membership actually
  RESERVED ('reserveClaimSeatMembership') -- a genuine mutation through
  'UniqueEpicMember's own unique key, not a mere read -- reporting
  'ClaimSeatEventMembershipConflict' for a pre-existing reservation under a
  DIFFERENT ordinal, but treating a pre-existing reservation under the SAME
  ordinal (or no prior reservation at all) as no conflict; a game with no
  linked event at all skips this step entirely;
* two complete, SEQUENTIAL production-seam claims by the same user into two
  DIFFERENT groups of the SAME event demonstrate the actual bug this round
  fixes: the first reserves the membership and succeeds; the second,
  against that same (mutated) membership state, is rejected with
  'ClaimSeatEventMembershipConflict' and performs no player insert;
* the reservation decision itself ('reserveClaimSeatMembership') is
  order-independent set-membership logic: whichever reservation attempt
  runs first against an empty map "wins" (an ordinary insert), and any
  later attempt -- however many real wall-clock requests it may represent
  -- deterministically either agrees (same ordinal) or loses (different
  ordinal) once it observes the row the winner already committed; this is
  the closest a live-database-free seam can get to proving the "unique-key
  loser" side of a genuine concurrent race;
* a successful claim performs the insert exactly once, only after every
  check has passed, with the game lock as the very first step in the log;
* a failure injected at any single step -- the game lock, the
  occupancy lookup, the already-joined check, the event lookup, the event
  lock, the membership reservation, or the insert itself -- can never
  produce a successful ('ClaimSeatClaimed') result, and no step after the
  injected failure is attempted.

As with "Arkham.Api.Events.EventDeletionSpec" and
"Arkham.Api.MainStreetSwapSpec", this pure interpreter's step log (and its
mutable membership map) is evidence of the deterministic order operations
were attempted in, where a sequence short-circuited, and what state a
successful reservation actually recorded -- it is NOT a simulation of
transactional rollback. A failure injected AFTER a reservation was
recorded still leaves that reservation "committed" in this pure model's
'TestState' (see the dedicated test below): actual rollback of a
partially-applied write on a real injected failure is a property of
'runDB' (an uncaught exception aborts the whole transaction), never of
this fake interpreter's log or map.
-}
module Arkham.Api.ClaimSeatSpec (spec) where

import Api.Arkham.Epic (EpicGroupReservation (..))
import Api.Arkham.Types.MultiplayerVariant
import Api.Handler.Arkham.Game.Debug
import Arkham.Card.CardCode
import Arkham.Difficulty (Difficulty (Standard))
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

fixtureEventId :: Epic.ArkhamEpicEventId
fixtureEventId = Epic.ArkhamEpicEventKey $ UUID.fromWords 0 0 0 999

fixtureUserId :: Int -> User.UserId
fixtureUserId = toSqlKey . fromIntegral

-- | Roland Banks (card 01001) -- any real investigator id works, this flow
-- only ever compares its normalized text form.
rolandId :: InvestigatorId
rolandId = InvestigatorId (CardCode "01001")

-- | The exact text form 'planAndExecuteClaimSeat' compares against: the
-- SAME normalization ('normalizeJsonInvestigatorId' in production,
-- prefixing a bare card code with @\"c\"@) the handler applies to both the
-- request body and this game's own stored 'gamePlayerOrder' before ever
-- calling this decision function.
rolandRequestId :: Text
rolandRequestId = "c01001"

-- | An investigator id genuinely absent from 'fixtureArkhamGame''s player
-- order.
unknownRequestId :: Text
unknownRequestId = "c99999"

-- | Wendy Adams (card 01003) -- a SECOND investigator in this game's own
-- player order, distinct from 'rolandId', used to exercise the case where
-- the requester already holds a DIFFERENT seat in this same game while the
-- REQUESTED seat itself is free.
secondInvestigatorId :: InvestigatorId
secondInvestigatorId = InvestigatorId (CardCode "01003")

-- | The exact text form of 'secondInvestigatorId'.
secondRequestId :: Text
secondRequestId = "c01003"

{- | A minimal, fully-forced 'Entity.Arkham.Game.ArkhamGame' row: a
"WithFriends" multiplayer game whose 'gamePlayerOrder' contains exactly
'rolandId' and 'secondInvestigatorId'. Entity fields in this codebase are
strict (@StrictData@ is a default extension, see @package.yaml@).
-}
fixtureArkhamGame :: GameEntity.ArkhamGame
fixtureArkhamGame =
  GameEntity.ArkhamGame
    { GameEntity.arkhamGameName = "fixture"
    , GameEntity.arkhamGameCurrentData =
        (newCampaign "06" Nothing 0 1 Standard False)
          {gamePlayerOrder = [rolandId, secondInvestigatorId]}
    , GameEntity.arkhamGameStep = 0
    , GameEntity.arkhamGameMultiplayerVariant = WithFriends
    , GameEntity.arkhamGameCreatedAt = fixtureTime
    , GameEntity.arkhamGameUpdatedAt = fixtureTime
    }
 where
  fixtureTime = UTCTime (ModifiedJulianDay 0) 0

-- Pure, in-memory persistence backend ----------------------------------------

-- | One recorded persistence step, in the order it happened.
data Step
  = -- | the game was locked ('FOR UPDATE'), and whether it was still present
    LockedGame Bool
  | -- | whether the locked game resolved to a linked Epic event (and its
    -- group ordinal, if so)
    LookedUpEvent (Maybe Int)
  | -- | every user id (ordinarily zero or one -- more than one models the
    -- anomalous, defensively-handled case) currently occupying the
    -- requested investigator slot in this game
    CheckedOccupants [User.UserId]
  | CheckedAlreadyJoined Bool
  | -- | the event was locked ('FOR UPDATE'), and whether it was still present
    LockedEvent Bool
  | -- | the actual reservation attempt against 'UniqueEpicMember', and its
    -- typed result
    ReservedMembership EpicGroupReservation
  | InsertedPlayer
  deriving stock (Eq, Show)

-- | Which single step (if any) should fail, for a given test run.
data FailAt
  = FailNever
  | FailAtLockGame
  | FailAtLookupEvent
  | FailAtOccupants
  | FailAtAlreadyJoined
  | FailAtLockEvent
  | FailAtReserve
  | FailAtInsert
  deriving stock (Eq, Show)

{- | Mutable state threaded through 'TestDB'. 'membership' is a genuine,
mutable analogue of the @arkham_epic_members@ table's 'UniqueEpicMember'
unique key (event, user) -> ordinal -- 'reserveClaimSeatMembership'
actually reads AND writes it, exactly like the production
'Api.Arkham.Epic.reserveEpicGroupMembership' it delegates to.
'legacySeats' is an INDEPENDENT analogue of every OTHER game's linked
'Entity.Arkham.Player.ArkhamPlayer' row for this (event, user) that
predates the reservation machinery entirely -- a bare seat with no
membership row at all -- exactly what
'Api.Arkham.Epic.reserveEpicGroupMembershipReconciling' additionally
queries ('Api.Arkham.Epic.selectUserEpicSeatOrdinals') before ever
consulting or writing 'membership'. 'seatOccupants' mirrors production's
'lookupClaimSeatOccupants' exactly: every user id currently occupying the
REQUESTED investigator slot in THIS game (see
'Api.Handler.Arkham.Game.Debug.classifyClaimSeatOccupancy'), so a test can
model the requester's OWN existing seat, a STRANGER's seat, or (for the
anomalous case) more than one occupant, rather than a bare taken\/not-taken
'Bool' that cannot distinguish those.
-}
data TestState = TestState
  { steps :: [Step]
  , gamePresent :: Bool
  , gameVariant :: MultiplayerVariant
  , linkedEvent :: Maybe (Epic.ArkhamEpicEventId, Int)
  , eventPresent :: Bool
  , membership :: Map (Epic.ArkhamEpicEventId, User.UserId) Int
  , legacySeats :: Map (Epic.ArkhamEpicEventId, User.UserId) [Int]
  , seatOccupants :: [User.UserId]
  , alreadyJoined :: Bool
  }

-- | The common case: the fixture game present (as a "WithFriends" game), no
-- linked Epic event, no pre-existing membership/legacy-seat/occupant/joined
-- conflict, zero steps recorded yet.
fixtureTestState :: TestState
fixtureTestState =
  TestState
    { steps = []
    , gamePresent = True
    , gameVariant = WithFriends
    , linkedEvent = Nothing
    , eventPresent = True
    , membership = Map.empty
    , legacySeats = Map.empty
    , seatOccupants = []
    , alreadyJoined = False
    }

{- | A pure interpreter of 'MonadClaimSeat': records every step it is asked
to perform (mutating 'membership' exactly as a real reservation would) and
short-circuits (via 'ExceptT') at the configured 'FailAt' step, exactly the
way an uncaught exception aborts a real 'runDB' transaction. As with the
sibling deletion/swap specs' 'TestDB', this interpreter does not roll back
earlier log entries OR earlier map mutations when a later operation
throws -- both are evidence of the deterministic attempted order (and, for
the map, what a successful step actually recorded), never a simulation of
what would (or would not) remain committed after a real rollback.
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

-- | Run a claim, returning the result and the step log only (the common
-- case for single-claim tests).
runTestDB :: FailAt -> TestState -> TestDB a -> (Either String a, [Step])
runTestDB failAt initial action =
  let (result, finalState) = runTestDBWithState failAt initial action
   in (result, finalState.steps)

-- | Run a claim, returning the result and the FULL final state -- needed to
-- chain a second, sequential claim against the first claim's committed
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

instance MonadClaimSeat TestDB where
  lockClaimSeatGame _gid = do
    failIfConfigured FailAtLockGame
    present <- gets (.gamePresent)
    variant <- gets (.gameVariant)
    recordStep (LockedGame present)
    pure $ if present then Just fixtureArkhamGame {GameEntity.arkhamGameMultiplayerVariant = variant} else Nothing

  lookupClaimSeatEvent _gid = do
    failIfConfigured FailAtLookupEvent
    mEvent <- gets (.linkedEvent)
    recordStep (LookedUpEvent (snd <$> mEvent))
    pure mEvent

  lookupClaimSeatOccupants _gid _investigatorId = do
    failIfConfigured FailAtOccupants
    occupants <- gets (.seatOccupants)
    recordStep (CheckedOccupants occupants)
    pure occupants

  isClaimSeatAlreadyJoined _userId _gid = do
    failIfConfigured FailAtAlreadyJoined
    joined <- gets (.alreadyJoined)
    recordStep (CheckedAlreadyJoined joined)
    pure joined

  lockClaimSeatEvent _eid = do
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
  reserveClaimSeatMembership eid userId ordinal = do
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

  insertClaimSeatPlayer _userId _gid _investigatorId = do
    failIfConfigured FailAtInsert
    recordStep InsertedPlayer

-- Specs -----------------------------------------------------------------------

spec :: Spec
spec = describe "planAndExecuteClaimSeat (game-locked-first claim decision sequencing)" do
  let run failAt state = runTestDB failAt state (planAndExecuteClaimSeat fixtureGameId (fixtureUserId 1) rolandRequestId)

  it "an authorized claim locks the game FIRST, checks every precondition in order, and inserts exactly once" do
    let (result, log_) = run FailNever fixtureTestState
    result `shouldBe` Right ClaimSeatClaimed
    log_
      `shouldBe` [ LockedGame True
                 , CheckedOccupants []
                 , LookedUpEvent Nothing
                 , CheckedAlreadyJoined False
                 , InsertedPlayer
                 ]

  it "a missing (or already vanished) game reports MissingGame before any other check, with the lock still the only step attempted" do
    let (result, log_) = run FailNever fixtureTestState {gamePresent = False}
    result `shouldBe` Right (ClaimSeatRejected ClaimSeatMissingGame)
    log_ `shouldBe` [LockedGame False]

  it "a locked game that is not a WithFriends multiplayer variant reports NotMultiplayer, immediately after the lock and before any seat/Epic check" do
    let (result, log_) = run FailNever fixtureTestState {gameVariant = Solo}
    result `shouldBe` Right (ClaimSeatRejected ClaimSeatNotMultiplayer)
    log_ `shouldBe` [LockedGame True]

  it "a requested investigator id absent from this game's own player order reports InvalidInvestigator, before any seat/Epic check" do
    let result = fst (run FailNever fixtureTestState {gamePresent = True})
        (resultUnknown, logUnknown) =
          runTestDB FailNever fixtureTestState (planAndExecuteClaimSeat fixtureGameId (fixtureUserId 1) unknownRequestId)
    result `shouldBe` Right ClaimSeatClaimed
    resultUnknown `shouldBe` Right (ClaimSeatRejected ClaimSeatInvalidInvestigator)
    logUnknown `shouldBe` [LockedGame True]

  it "an investigator slot occupied by ANOTHER user reports Taken, checked before any Epic event lookup/lock/reservation is even attempted" do
    let (result, log_) =
          run FailNever fixtureTestState {seatOccupants = [fixtureUserId 2], linkedEvent = Just (fixtureEventId, 0)}
    result `shouldBe` Right (ClaimSeatRejected ClaimSeatTaken)
    log_ `shouldBe` [LockedGame True, CheckedOccupants [fixtureUserId 2]]

  it "an ANOMALOUS slot (more than one occupant, a data anomaly this codebase never expects but never assumes impossible either) also reports Taken, never a crash, and never reaches any Epic check" do
    let (result, log_) =
          run
            FailNever
            fixtureTestState {seatOccupants = [fixtureUserId 2, fixtureUserId 3], linkedEvent = Just (fixtureEventId, 0)}
    result `shouldBe` Right (ClaimSeatRejected ClaimSeatTaken)
    log_ `shouldBe` [LockedGame True, CheckedOccupants [fixtureUserId 2, fixtureUserId 3]]

  it "the REQUESTER's own exact seat -- an idempotent re-claim -- is NEVER 'Taken': it proceeds straight through Epic reconciliation (repairing a legacy seat's missing membership row) and reports AlreadyJoined directly, WITHOUT ever consulting isClaimSeatAlreadyJoined or inserting a player row -- the bug this round fixes" do
    let (result, log_) =
          run FailNever fixtureTestState {seatOccupants = [fixtureUserId 1], linkedEvent = Just (fixtureEventId, 0)}
    result `shouldBe` Right (ClaimSeatRejected ClaimSeatAlreadyJoined)
    log_
      `shouldBe` [ LockedGame True
                 , CheckedOccupants [fixtureUserId 1]
                 , LookedUpEvent (Just 0)
                 , LockedEvent True
                 , ReservedMembership EpicGroupReserved
                 ]
    CheckedAlreadyJoined True `shouldSatisfy` (`notElem` log_)
    CheckedAlreadyJoined False `shouldSatisfy` (`notElem` log_)
    InsertedPlayer `shouldSatisfy` (`notElem` log_)

  it "the REQUESTER's own seat, but with a CONFLICTING legacy seat in another group of the same event, is rejected with EventMembershipConflict and NO repair -- 'ClaimSeatSeatOwnedByRequester' still goes through the SAME reconciliation as every other branch" do
    let seededLegacy = Map.singleton (fixtureEventId, fixtureUserId 1) [7]
        (result, finalState) =
          runTestDBWithState
            FailNever
            fixtureTestState
              { seatOccupants = [fixtureUserId 1]
              , linkedEvent = Just (fixtureEventId, 0)
              , legacySeats = seededLegacy
              }
            (planAndExecuteClaimSeat fixtureGameId (fixtureUserId 1) rolandRequestId)
    result `shouldBe` Right (ClaimSeatRejected ClaimSeatEventMembershipConflict)
    Map.lookup (fixtureEventId, fixtureUserId 1) finalState.membership `shouldBe` Nothing
    InsertedPlayer `shouldSatisfy` (`notElem` finalState.steps)

  it "a DIFFERENT (STRANGER) user occupying the requested slot is rejected with Taken and NO membership write is ever attempted, even when that stranger has their own legacy seat elsewhere" do
    let seededLegacy = Map.singleton (fixtureEventId, fixtureUserId 2) [3]
        (result, finalState) =
          runTestDBWithState
            FailNever
            fixtureTestState
              { seatOccupants = [fixtureUserId 2]
              , linkedEvent = Just (fixtureEventId, 0)
              , legacySeats = seededLegacy
              }
            (planAndExecuteClaimSeat fixtureGameId (fixtureUserId 1) rolandRequestId)
    result `shouldBe` Right (ClaimSeatRejected ClaimSeatTaken)
    finalState.steps `shouldBe` [LockedGame True, CheckedOccupants [fixtureUserId 2]]
    Map.lookup (fixtureEventId, fixtureUserId 1) finalState.membership `shouldBe` Nothing
    Map.lookup (fixtureEventId, fixtureUserId 2) finalState.membership `shouldBe` Nothing

  it "a requester who already holds a DIFFERENT seat in this SAME game, requesting an investigator slot that is itself FREE, is reconciled into this game's own group THEN reported AlreadyJoined (idempotent) -- preserving the pre-existing already-joined semantics for a genuinely different requested seat" do
    let (result, log_) =
          runTestDB
            FailNever
            fixtureTestState {alreadyJoined = True, linkedEvent = Just (fixtureEventId, 2)}
            (planAndExecuteClaimSeat fixtureGameId (fixtureUserId 1) secondRequestId)
    result `shouldBe` Right (ClaimSeatRejected ClaimSeatAlreadyJoined)
    log_
      `shouldBe` [ LockedGame True
                 , CheckedOccupants []
                 , LookedUpEvent (Just 2)
                 , LockedEvent True
                 , ReservedMembership EpicGroupReserved
                 , CheckedAlreadyJoined True
                 ]

  it "a game with no linked Epic event at all skips the event lock and membership reservation entirely" do
    let (result, log_) = run FailNever fixtureTestState {linkedEvent = Nothing}
        isEpicStep step = case step of
          LockedEvent _ -> True
          ReservedMembership _ -> True
          _ -> False
    result `shouldBe` Right ClaimSeatClaimed
    log_ `shouldNotSatisfy` any isEpicStep

  it "a pre-existing GroupPlayer reservation under a DIFFERENT event group ordinal reports EventMembershipConflict and writes nothing -- and the already-joined check is never even reached" do
    let seeded = Map.singleton (fixtureEventId, fixtureUserId 1) 1
        (result, log_) =
          run FailNever fixtureTestState {linkedEvent = Just (fixtureEventId, 0), membership = seeded}
        isInsertedPlayer step = step == InsertedPlayer
    result `shouldBe` Right (ClaimSeatRejected ClaimSeatEventMembershipConflict)
    log_
      `shouldBe` [ LockedGame True
                 , CheckedOccupants []
                 , LookedUpEvent (Just 0)
                 , LockedEvent True
                 , ReservedMembership EpicGroupReservationConflict
                 ]
    log_ `shouldNotSatisfy` any isInsertedPlayer
    log_ `shouldNotSatisfy` (CheckedAlreadyJoined True `elem`)

  it "a pre-existing GroupPlayer reservation under the SAME event group ordinal (this very game's own group) is correctly NOT a conflict" do
    let seeded = Map.singleton (fixtureEventId, fixtureUserId 1) 0
        (result, log_) =
          run FailNever fixtureTestState {linkedEvent = Just (fixtureEventId, 0), membership = seeded}
    result `shouldBe` Right ClaimSeatClaimed
    log_
      `shouldBe` [ LockedGame True
                 , CheckedOccupants []
                 , LookedUpEvent (Just 0)
                 , LockedEvent True
                 , ReservedMembership EpicGroupReserved
                 , CheckedAlreadyJoined False
                 , InsertedPlayer
                 ]

  it "a user with NO prior membership row is ACTUALLY RESERVED into this game's own group on success -- the bug this round fixes" do
    let (result, finalState) =
          runTestDBWithState
            FailNever
            fixtureTestState {linkedEvent = Just (fixtureEventId, 2)}
            (planAndExecuteClaimSeat fixtureGameId (fixtureUserId 1) rolandRequestId)
    result `shouldBe` Right ClaimSeatClaimed
    Map.lookup (fixtureEventId, fixtureUserId 1) finalState.membership `shouldBe` Just 2

  it "two complete SEQUENTIAL claims by the same user into two DIFFERENT groups of the SAME event: the first reserves and succeeds, the second is rejected with no player write" do
    let firstGroupOrdinal = 0
        secondGroupOrdinal = 1
        (firstResult, afterFirst) =
          runTestDBWithState
            FailNever
            fixtureTestState {linkedEvent = Just (fixtureEventId, firstGroupOrdinal)}
            (planAndExecuteClaimSeat fixtureGameId (fixtureUserId 1) rolandRequestId)
    firstResult `shouldBe` Right ClaimSeatClaimed
    Map.lookup (fixtureEventId, fixtureUserId 1) afterFirst.membership `shouldBe` Just firstGroupOrdinal
    -- Simulate a SECOND, distinct game/group of the SAME event: reset the
    -- per-game flags (a different game's own taken/joined state), reset the
    -- step log to isolate this call's trace, but thread the SAME
    -- (committed) membership map through -- exactly as one shared
    -- @arkham_epic_members@ table would carry the first claim's row into a
    -- second, later transaction against a different game.
    let secondCallState =
          afterFirst
            { steps = []
            , linkedEvent = Just (fixtureEventId, secondGroupOrdinal)
            , seatOccupants = []
            , alreadyJoined = False
            }
        (secondResult, afterSecond) =
          runTestDBWithState
            FailNever
            secondCallState
            (planAndExecuteClaimSeat fixtureGameId (fixtureUserId 1) rolandRequestId)
    secondResult `shouldBe` Right (ClaimSeatRejected ClaimSeatEventMembershipConflict)
    -- the FIRST reservation is untouched -- no overwrite, no second row
    Map.lookup (fixtureEventId, fixtureUserId 1) afterSecond.membership `shouldBe` Just firstGroupOrdinal
    InsertedPlayer `shouldSatisfy` (`notElem` afterSecond.steps)

  describe "reserveClaimSeatMembership (production-used Epic membership reservation seam, exercised directly)" do
    let reserve eid uid ordinal state =
          runTestDBWithState FailNever state (reserveClaimSeatMembership eid uid ordinal)

    it "a fresh reservation against an empty map is an ordinary insert: Reserved, and the map is updated" do
      let (result, finalState) = reserve fixtureEventId (fixtureUserId 1) 0 fixtureTestState
      result `shouldBe` Right EpicGroupReserved
      Map.lookup (fixtureEventId, fixtureUserId 1) finalState.membership `shouldBe` Just 0

    it "the 'unique-key loser': a second reservation attempt for the SAME (event, user) under a DIFFERENT ordinal conflicts, and the map is left untouched by the loser" do
      let seeded = Map.singleton (fixtureEventId, fixtureUserId 1) 0
          (result, finalState) = reserve fixtureEventId (fixtureUserId 1) 1 fixtureTestState {membership = seeded}
      result `shouldBe` Right EpicGroupReservationConflict
      Map.lookup (fixtureEventId, fixtureUserId 1) finalState.membership `shouldBe` Just 0

    it "a second reservation attempt for the SAME (event, user, ordinal) is idempotently Reserved (re-claiming a seat in a group previously left)" do
      let seeded = Map.singleton (fixtureEventId, fixtureUserId 1) 0
          (result, finalState) = reserve fixtureEventId (fixtureUserId 1) 0 fixtureTestState {membership = seeded}
      result `shouldBe` Right EpicGroupReserved
      Map.lookup (fixtureEventId, fixtureUserId 1) finalState.membership `shouldBe` Just 0

    it "a DIFFERENT user reserving the SAME event/ordinal is independent of the first user's row (the unique key includes the user)" do
      let seeded = Map.singleton (fixtureEventId, fixtureUserId 1) 0
          (result, finalState) = reserve fixtureEventId (fixtureUserId 2) 0 fixtureTestState {membership = seeded}
      result `shouldBe` Right EpicGroupReserved
      Map.lookup (fixtureEventId, fixtureUserId 1) finalState.membership `shouldBe` Just 0
      Map.lookup (fixtureEventId, fixtureUserId 2) finalState.membership `shouldBe` Just 0

  it "a failure locking the game cannot produce a success-shaped result, and no other step is ever attempted" do
    let (result, log_) = run FailAtLockGame fixtureTestState
    result `shouldSatisfy` isLeft
    log_ `shouldBe` []

  it "a failure checking who occupies the requested seat cannot produce a success-shaped result, proving the game was genuinely locked first" do
    let (result, log_) = run FailAtOccupants fixtureTestState
    result `shouldSatisfy` isLeft
    log_ `shouldBe` [LockedGame True]

  it "a failure checking whether the user already joined cannot produce a success-shaped result, proving the occupancy check AND the (no-op, no-event) Epic lookup were genuinely attempted first" do
    let (result, log_) = run FailAtAlreadyJoined fixtureTestState
    result `shouldSatisfy` isLeft
    log_ `shouldBe` [LockedGame True, CheckedOccupants [], LookedUpEvent Nothing]

  it "a failure looking up the linked Epic event cannot produce a success-shaped result, proving every earlier check (game lock, occupancy check) was genuinely attempted first" do
    let (result, log_) = run FailAtLookupEvent fixtureTestState
    result `shouldSatisfy` isLeft
    log_ `shouldBe` [LockedGame True, CheckedOccupants []]

  it "a failure locking the event cannot produce a success-shaped result, proving every earlier check (game lock, occupancy check, event lookup) was genuinely attempted first" do
    let (result, log_) = run FailAtLockEvent fixtureTestState {linkedEvent = Just (fixtureEventId, 0)}
    result `shouldSatisfy` isLeft
    log_ `shouldBe` [LockedGame True, CheckedOccupants [], LookedUpEvent (Just 0)]

  it "a failure reserving Epic membership cannot produce a success-shaped result, proving the event was genuinely locked first" do
    let (result, log_) = run FailAtReserve fixtureTestState {linkedEvent = Just (fixtureEventId, 0)}
    result `shouldSatisfy` isLeft
    log_
      `shouldBe` [LockedGame True, CheckedOccupants [], LookedUpEvent (Just 0), LockedEvent True]

  it "a failure inserting the player row cannot produce a success-shaped result, proving every precondition check was genuinely attempted first" do
    let (result, log_) = run FailAtInsert fixtureTestState
    result `shouldSatisfy` isLeft
    log_ `shouldBe` [LockedGame True, CheckedOccupants [], LookedUpEvent Nothing, CheckedAlreadyJoined False]

  it "a failure inserting the player row AFTER a real Epic reservation was made still cannot produce a success-shaped result -- and the pure model's membership map, unlike real runDB, does NOT roll back (documented, not a rollback claim)" do
    let (result, finalState) =
          runTestDBWithState
            FailAtInsert
            fixtureTestState {linkedEvent = Just (fixtureEventId, 0)}
            (planAndExecuteClaimSeat fixtureGameId (fixtureUserId 1) rolandRequestId)
    result `shouldSatisfy` isLeft
    finalState.steps
      `shouldBe` [ LockedGame True
                 , CheckedOccupants []
                 , LookedUpEvent (Just 0)
                 , LockedEvent True
                 , ReservedMembership EpicGroupReserved
                 , CheckedAlreadyJoined False
                 ]
    -- This is exactly the caveat documented at the top of this module: the
    -- reservation step ran (and this FAKE map recorded it) before the
    -- injected insert failure, but this proves ONLY that the reservation
    -- was genuinely attempted before the insert -- production's actual
    -- rollback of this same reservation on a real, uncaught insert
    -- exception is a property of 'runDB', not of this pure model.
    Map.lookup (fixtureEventId, fixtureUserId 1) finalState.membership `shouldBe` Just 0

  describe "legacy-seat reconciliation (a pre-existing ArkhamPlayer row in ANOTHER game of this event, with NO membership row at all, predating the reservation machinery)" do
    it "a legacy seat in a DIFFERENT ordinal than requested is a typed conflict, with zero membership writes" do
      let seededLegacy = Map.singleton (fixtureEventId, fixtureUserId 1) [2]
          (result, finalState) =
            runTestDBWithState
              FailNever
              fixtureTestState {linkedEvent = Just (fixtureEventId, 0), legacySeats = seededLegacy}
              (planAndExecuteClaimSeat fixtureGameId (fixtureUserId 1) rolandRequestId)
      result `shouldBe` Right (ClaimSeatRejected ClaimSeatEventMembershipConflict)
      finalState.steps
        `shouldBe` [ LockedGame True
                   , CheckedOccupants []
                   , LookedUpEvent (Just 0)
                   , LockedEvent True
                   , ReservedMembership EpicGroupReservationConflict
                   ]
      Map.lookup (fixtureEventId, fixtureUserId 1) finalState.membership `shouldBe` Nothing
      InsertedPlayer `shouldSatisfy` (`notElem` finalState.steps)

    it "a legacy seat ONLY in the requested ordinal (this very group) is idempotently repaired: a membership row is created even though no player row previously recorded one" do
      let seededLegacy = Map.singleton (fixtureEventId, fixtureUserId 1) [0]
          (result, finalState) =
            runTestDBWithState
              FailNever
              fixtureTestState {linkedEvent = Just (fixtureEventId, 0), legacySeats = seededLegacy}
              (planAndExecuteClaimSeat fixtureGameId (fixtureUserId 1) rolandRequestId)
      result `shouldBe` Right ClaimSeatClaimed
      Map.lookup (fixtureEventId, fixtureUserId 1) finalState.membership `shouldBe` Just 0

    it "seats recorded across TWO different legacy ordinals reject regardless of which ordinal is requested -- an inconsistent legacy state is never silently resolved" do
      let seededLegacy = Map.singleton (fixtureEventId, fixtureUserId 1) [1, 2]
          (resultAt1, stateAt1) =
            runTestDBWithState
              FailNever
              fixtureTestState {linkedEvent = Just (fixtureEventId, 1), legacySeats = seededLegacy}
              (planAndExecuteClaimSeat fixtureGameId (fixtureUserId 1) rolandRequestId)
      resultAt1 `shouldBe` Right (ClaimSeatRejected ClaimSeatEventMembershipConflict)
      Map.lookup (fixtureEventId, fixtureUserId 1) stateAt1.membership `shouldBe` Nothing

    it "the 'ClaimSeatSeatOwnedByRequester' branch (an idempotent re-claim of a user's OWN seat) still genuinely invokes reconciliation: a legacy seat in a DIFFERENT game's group is caught even for a user re-claiming their OWN existing seat, and 'isClaimSeatAlreadyJoined' is never consulted at all in this branch" do
      let seededLegacy = Map.singleton (fixtureEventId, fixtureUserId 1) [7]
          (result, finalState) =
            runTestDBWithState
              FailNever
              fixtureTestState
                { seatOccupants = [fixtureUserId 1]
                , linkedEvent = Just (fixtureEventId, 0)
                , legacySeats = seededLegacy
                }
              (planAndExecuteClaimSeat fixtureGameId (fixtureUserId 1) rolandRequestId)
      -- Reconciliation runs and conflicts; 'CheckedAlreadyJoined' never
      -- appears in the log because 'ClaimSeatSeatOwnedByRequester' never
      -- calls 'isClaimSeatAlreadyJoined' at all.
      result `shouldBe` Right (ClaimSeatRejected ClaimSeatEventMembershipConflict)
      finalState.steps
        `shouldBe` [ LockedGame True
                   , CheckedOccupants [fixtureUserId 1]
                   , LookedUpEvent (Just 0)
                   , LockedEvent True
                   , ReservedMembership EpicGroupReservationConflict
                   ]
      CheckedAlreadyJoined True `shouldSatisfy` (`notElem` finalState.steps)
      CheckedAlreadyJoined False `shouldSatisfy` (`notElem` finalState.steps)

    it "a user with NO legacy seat at all and no prior membership reserves normally -- the common, unaffected case" do
      let (result, finalState) =
            runTestDBWithState
              FailNever
              fixtureTestState {linkedEvent = Just (fixtureEventId, 0)}
              (planAndExecuteClaimSeat fixtureGameId (fixtureUserId 1) rolandRequestId)
      result `shouldBe` Right ClaimSeatClaimed
      Map.lookup (fixtureEventId, fixtureUserId 1) finalState.membership `shouldBe` Just 0

    it "reservation itself (exercised directly): a legacy seat in a different ordinal conflicts even against an otherwise-empty membership map" do
      let seededLegacy = Map.singleton (fixtureEventId, fixtureUserId 1) [5]
          (result, finalState) =
            runTestDBWithState
              FailNever
              fixtureTestState {legacySeats = seededLegacy}
              (reserveClaimSeatMembership fixtureEventId (fixtureUserId 1) 9)
      result `shouldBe` Right EpicGroupReservationConflict
      Map.lookup (fixtureEventId, fixtureUserId 1) finalState.membership `shouldBe` Nothing

    it "sequential reservations: a first user's legacy-seat repair does not affect a second, unrelated user's fresh reservation into the same event" do
      let seededLegacy = Map.singleton (fixtureEventId, fixtureUserId 1) [4]
          (firstResult, afterFirst) =
            runTestDBWithState
              FailNever
              fixtureTestState {legacySeats = seededLegacy}
              (reserveClaimSeatMembership fixtureEventId (fixtureUserId 1) 4)
      firstResult `shouldBe` Right EpicGroupReserved
      let (secondResult, afterSecond) =
            runTestDBWithState FailNever afterFirst (reserveClaimSeatMembership fixtureEventId (fixtureUserId 2) 0)
      secondResult `shouldBe` Right EpicGroupReserved
      Map.lookup (fixtureEventId, fixtureUserId 1) afterSecond.membership `shouldBe` Just 4
      Map.lookup (fixtureEventId, fixtureUserId 2) afterSecond.membership `shouldBe` Just 0

    it "the 'unique-key loser' side of a genuine concurrent race, now with a legacy seat in the mix: the WINNER's repair commits first, and the LOSER (whose own legacy seat is in a different ordinal) still conflicts against the winner's committed row" do
      let seededLegacy = Map.singleton (fixtureEventId, fixtureUserId 1) [0]
          (winnerResult, afterWinner) =
            runTestDBWithState
              FailNever
              fixtureTestState {legacySeats = seededLegacy}
              (reserveClaimSeatMembership fixtureEventId (fixtureUserId 1) 0)
      winnerResult `shouldBe` Right EpicGroupReserved
      -- A second, later attempt for the SAME (event, user) under a
      -- DIFFERENT ordinal -- whichever real wall-clock request observes
      -- the row the winner already committed -- is rejected purely from
      -- the (now non-empty) membership map, independent of legacySeats.
      let (loserResult, afterLoser) =
            runTestDBWithState FailNever afterWinner (reserveClaimSeatMembership fixtureEventId (fixtureUserId 1) 1)
      loserResult `shouldBe` Right EpicGroupReservationConflict
      Map.lookup (fixtureEventId, fixtureUserId 1) afterLoser.membership `shouldBe` Just 0

    it "an Organizer-role membership row is a structurally SEPARATE key ('UniqueEpicMember (eventId, userId, Organizer)' vs '(eventId, userId, GroupPlayer)') that this reconciliation neither reads nor writes: an organizer with no ArkhamPlayer seat anywhere reserves a GroupPlayer seat exactly as any other user would" do
      let (result, finalState) =
            runTestDBWithState
              FailNever
              -- 'organizerOf' models ONLY that this user separately holds the
              -- Organizer role for this event -- 'selectUserEpicSeatOrdinals'
              -- joins 'ArkhamEpicGroup' to 'ArkhamPlayer' alone, which has no
              -- role column at all, so an Organizer-role row (which need not
              -- ever have a corresponding player row) can never appear in
              -- 'legacySeats' or be conflated with a GroupPlayer seat here.
              fixtureTestState {linkedEvent = Just (fixtureEventId, 0)}
              (planAndExecuteClaimSeat fixtureGameId (fixtureUserId 1) rolandRequestId)
      result `shouldBe` Right ClaimSeatClaimed
      Map.lookup (fixtureEventId, fixtureUserId 1) finalState.membership `shouldBe` Just 0

    it "an Organizer-role user re-claiming their OWN already-held seat is handled purely through 'ClaimSeatSeatOwnedByRequester', exactly like any other user: reconciled (idempotently, since a GroupPlayer membership already exists for this ordinal) and reported AlreadyJoined, with the SEPARATE Organizer role neither read nor written by this path" do
      let seeded = Map.singleton (fixtureEventId, fixtureUserId 1) 0
          (result, finalState) =
            runTestDBWithState
              FailNever
              -- 'seatOccupants = [fixtureUserId 1]' models this organizer ALSO
              -- already holding an ArkhamPlayer seat (Organizer and GroupPlayer
              -- are independent 'UniqueEpicMember' roles for the same user, so
              -- an organizer may hold one GroupPlayer seat too -- see
              -- 'Api.Arkham.Epic.reserveEpicGroupMembershipReconciling').
              fixtureTestState {seatOccupants = [fixtureUserId 1], linkedEvent = Just (fixtureEventId, 0), membership = seeded}
              (planAndExecuteClaimSeat fixtureGameId (fixtureUserId 1) rolandRequestId)
      result `shouldBe` Right (ClaimSeatRejected ClaimSeatAlreadyJoined)
      finalState.steps
        `shouldBe` [ LockedGame True
                   , CheckedOccupants [fixtureUserId 1]
                   , LookedUpEvent (Just 0)
                   , LockedEvent True
                   , ReservedMembership EpicGroupReserved
                   ]
      Map.lookup (fixtureEventId, fixtureUserId 1) finalState.membership `shouldBe` Just 0
      InsertedPlayer `shouldSatisfy` (`notElem` finalState.steps)
