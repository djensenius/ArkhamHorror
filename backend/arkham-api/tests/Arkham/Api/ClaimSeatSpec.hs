{- | Proves the game-locked-first game-seat claim sequencing described in
'Api.Handler.Arkham.Game.Debug.planAndExecuteClaimSeat' without a live
database.

A pure, in-memory 'MonadClaimSeat' instance ('TestDB') runs the exact same
production decision function and records every step it performs. This lets
us assert:

* the target 'Entity.Arkham.Game.ArkhamGame' row is ALWAYS the first (and
  only) row-locked ('FOR UPDATE') by this flow -- see 'lockClaimSeatGame' --
  establishing the same game-before-player order Main Street swap and Epic
  event deletion already use (see
  'Api.Handler.Arkham.Games.Shared.lockSwapGame' and
  'Api.Handler.Arkham.Events.MonadEpicEventDeletion'), so a concurrent swap
  that has already locked this same game can never be interleaved with any
  check or write this flow performs, and this flow can never insert a
  player row (or observe game state) before that lock is held;
* a missing (or already-deleted/vanished) game short-circuits to
  'ClaimSeatMissingGame' before any other check;
* a locked game that is not a "WithFriends" multiplayer game reports
  'ClaimSeatNotMultiplayer', read straight from the locked snapshot, before
  any Epic-event membership check, seat-taken check, or insert;
* a requested investigator id that is not part of this game's own player
  order reports 'ClaimSeatInvalidInvestigator', likewise before any further
  check;
* a game that IS linked to an Epic event, where the requesting user already
  holds a 'GroupPlayer' membership row for that SAME event under a
  DIFFERENT group ordinal, reports 'ClaimSeatEventMembershipConflict' --
  checked only once the game is already locked, so this decision can never
  be raced by a concurrent claim into (or swap out of) the same game -- but
  a membership row under the SAME ordinal (i.e. this very game's own group)
  is correctly NOT a conflict, and a game with no linked event at all skips
  this check entirely;
* an already-taken investigator slot ('ClaimSeatTaken') and an
  already-held seat for this user ('ClaimSeatAlreadyJoined') are each
  checked only after every earlier check has passed, and each reported
  distinctly;
* a successful claim performs the insert exactly once, only after every
  check has passed, with the game lock as the very first step in the log;
* a failure injected at any single step -- the game lock, the event lookup,
  the membership lookup, the seat-taken check, the already-joined check, or
  the insert itself -- can never produce a successful ('ClaimSeatClaimed')
  result, and no step after the injected failure is attempted.

As with "Arkham.Api.Events.EventDeletionSpec" and
"Arkham.Api.MainStreetSwapSpec", this pure interpreter's step log is
evidence of the deterministic order operations were attempted in and where
a sequence short-circuited -- it is not a simulation of transactional
rollback. Actual rollback of a partially-applied write on a real injected
failure is a property of 'runDB' (an uncaught exception aborts the whole
transaction), not of this test.
-}
module Arkham.Api.ClaimSeatSpec (spec) where

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

{- | A minimal, fully-forced 'Entity.Arkham.Game.ArkhamGame' row: a
"WithFriends" multiplayer game whose 'gamePlayerOrder' contains exactly
'rolandId'. Entity fields in this codebase are strict (@StrictData@ is a
default extension, see @package.yaml@).
-}
fixtureArkhamGame :: GameEntity.ArkhamGame
fixtureArkhamGame =
  GameEntity.ArkhamGame
    { GameEntity.arkhamGameName = "fixture"
    , GameEntity.arkhamGameCurrentData = (newCampaign "06" Nothing 0 1 Standard False) {gamePlayerOrder = [rolandId]}
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
  | -- | the user's existing 'GroupPlayer' membership ordinal for that event,
    -- if any
    CheckedMembership (Maybe Int)
  | CheckedTaken Bool
  | CheckedAlreadyJoined Bool
  | InsertedPlayer
  deriving stock (Eq, Show)

-- | Which single step (if any) should fail, for a given test run.
data FailAt
  = FailNever
  | FailAtLockGame
  | FailAtLookupEvent
  | FailAtMembership
  | FailAtTaken
  | FailAtAlreadyJoined
  | FailAtInsert
  deriving stock (Eq, Show)

-- | Mutable state threaded through 'TestDB'.
data TestState = TestState
  { steps :: [Step]
  , gamePresent :: Bool
  , gameVariant :: MultiplayerVariant
  , linkedEvent :: Maybe (Epic.ArkhamEpicEventId, Int)
  , existingMembershipOrdinal :: Maybe Int
  , seatTaken :: Bool
  , alreadyJoined :: Bool
  }

-- | The common case: the fixture game present (as a "WithFriends" game), no
-- linked Epic event, no pre-existing membership/taken/joined conflict, zero
-- steps recorded yet.
fixtureTestState :: TestState
fixtureTestState =
  TestState
    { steps = []
    , gamePresent = True
    , gameVariant = WithFriends
    , linkedEvent = Nothing
    , existingMembershipOrdinal = Nothing
    , seatTaken = False
    , alreadyJoined = False
    }

{- | A pure interpreter of 'MonadClaimSeat': records every step it is asked
to perform and short-circuits (via 'ExceptT') at the configured 'FailAt'
step, exactly the way an uncaught exception aborts a real 'runDB'
transaction. As with the sibling deletion/swap specs' 'TestDB', this
interpreter does not roll back earlier log entries when a later operation
throws -- the log is evidence of the deterministic attempted order, not a
simulation of what would (or would not) remain committed.
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
  let (result, finalState) = runState (runExceptT (runReaderT (unTestDB action) failAt)) initial
   in (result, finalState.steps)

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

  lookupClaimSeatEventMembership _eid _userId = do
    failIfConfigured FailAtMembership
    mOrdinal <- gets (.existingMembershipOrdinal)
    recordStep (CheckedMembership mOrdinal)
    pure mOrdinal

  isClaimSeatTaken _gid _investigatorId = do
    failIfConfigured FailAtTaken
    taken <- gets (.seatTaken)
    recordStep (CheckedTaken taken)
    pure taken

  isClaimSeatAlreadyJoined _userId _gid = do
    failIfConfigured FailAtAlreadyJoined
    joined <- gets (.alreadyJoined)
    recordStep (CheckedAlreadyJoined joined)
    pure joined

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
                 , LookedUpEvent Nothing
                 , CheckedTaken False
                 , CheckedAlreadyJoined False
                 , InsertedPlayer
                 ]

  it "a missing (or already vanished) game reports MissingGame before any other check, with the lock still the only step attempted" do
    let (result, log_) = run FailNever fixtureTestState {gamePresent = False}
    result `shouldBe` Right (ClaimSeatRejected ClaimSeatMissingGame)
    log_ `shouldBe` [LockedGame False]

  it "a locked game that is not a WithFriends multiplayer variant reports NotMultiplayer, immediately after the lock and before any Epic/seat check" do
    let (result, log_) = run FailNever fixtureTestState {gameVariant = Solo}
    result `shouldBe` Right (ClaimSeatRejected ClaimSeatNotMultiplayer)
    log_ `shouldBe` [LockedGame True]

  it "a requested investigator id absent from this game's own player order reports InvalidInvestigator, before any Epic/seat check" do
    let result = fst (run FailNever fixtureTestState {gamePresent = True})
        (resultUnknown, logUnknown) =
          runTestDB FailNever fixtureTestState (planAndExecuteClaimSeat fixtureGameId (fixtureUserId 1) unknownRequestId)
    result `shouldBe` Right ClaimSeatClaimed
    resultUnknown `shouldBe` Right (ClaimSeatRejected ClaimSeatInvalidInvestigator)
    logUnknown `shouldBe` [LockedGame True]

  it "a pre-existing GroupPlayer membership under a DIFFERENT event group ordinal reports EventMembershipConflict, checked only once the game is already locked" do
    let (result, log_) =
          run
            FailNever
            fixtureTestState {linkedEvent = Just (fixtureEventId, 0), existingMembershipOrdinal = Just 1}
    result `shouldBe` Right (ClaimSeatRejected ClaimSeatEventMembershipConflict)
    log_ `shouldBe` [LockedGame True, LookedUpEvent (Just 0), CheckedMembership (Just 1)]

  it "a pre-existing GroupPlayer membership under the SAME event group ordinal (this very game's own group) is correctly NOT a conflict" do
    let (result, log_) =
          run
            FailNever
            fixtureTestState {linkedEvent = Just (fixtureEventId, 0), existingMembershipOrdinal = Just 0}
    result `shouldBe` Right ClaimSeatClaimed
    log_
      `shouldBe` [ LockedGame True
                 , LookedUpEvent (Just 0)
                 , CheckedMembership (Just 0)
                 , CheckedTaken False
                 , CheckedAlreadyJoined False
                 , InsertedPlayer
                 ]

  it "a game with no linked Epic event at all skips the membership check entirely" do
    let (result, log_) = run FailNever fixtureTestState {linkedEvent = Nothing}
        isCheckedMembership step = case step of
          CheckedMembership _ -> True
          _ -> False
    result `shouldBe` Right ClaimSeatClaimed
    log_ `shouldNotSatisfy` any isCheckedMembership

  it "an already-taken investigator slot reports Taken, checked only after every earlier check has passed" do
    let (result, log_) = run FailNever fixtureTestState {seatTaken = True}
    result `shouldBe` Right (ClaimSeatRejected ClaimSeatTaken)
    log_ `shouldBe` [LockedGame True, LookedUpEvent Nothing, CheckedTaken True]

  it "a user who already holds ANY seat in this game reports AlreadyJoined, checked only after the seat-taken check has passed" do
    let (result, log_) = run FailNever fixtureTestState {alreadyJoined = True}
    result `shouldBe` Right (ClaimSeatRejected ClaimSeatAlreadyJoined)
    log_ `shouldBe` [LockedGame True, LookedUpEvent Nothing, CheckedTaken False, CheckedAlreadyJoined True]

  it "a failure locking the game cannot produce a success-shaped result, and no other step is ever attempted" do
    let (result, log_) = run FailAtLockGame fixtureTestState
    result `shouldSatisfy` isLeft
    log_ `shouldBe` []

  it "a failure looking up the linked Epic event cannot produce a success-shaped result, proving the game was genuinely locked first" do
    let (result, log_) = run FailAtLookupEvent fixtureTestState
    result `shouldSatisfy` isLeft
    log_ `shouldBe` [LockedGame True]

  it "a failure checking existing event membership cannot produce a success-shaped result, proving the event lookup was genuinely attempted first" do
    let (result, log_) = run FailAtMembership fixtureTestState {linkedEvent = Just (fixtureEventId, 0)}
    result `shouldSatisfy` isLeft
    log_ `shouldBe` [LockedGame True, LookedUpEvent (Just 0)]

  it "a failure checking whether the seat is already taken cannot produce a success-shaped result" do
    let (result, log_) = run FailAtTaken fixtureTestState
    result `shouldSatisfy` isLeft
    log_ `shouldBe` [LockedGame True, LookedUpEvent Nothing]

  it "a failure checking whether the user already joined cannot produce a success-shaped result, proving the seat-taken check was genuinely attempted first" do
    let (result, log_) = run FailAtAlreadyJoined fixtureTestState
    result `shouldSatisfy` isLeft
    log_ `shouldBe` [LockedGame True, LookedUpEvent Nothing, CheckedTaken False]

  it "a failure inserting the player row cannot produce a success-shaped result, proving every precondition check was genuinely attempted first" do
    let (result, log_) = run FailAtInsert fixtureTestState
    result `shouldSatisfy` isLeft
    log_ `shouldBe` [LockedGame True, LookedUpEvent Nothing, CheckedTaken False, CheckedAlreadyJoined False]
