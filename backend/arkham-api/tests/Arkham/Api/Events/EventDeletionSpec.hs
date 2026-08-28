{- | Proves the atomic Epic-event deletion sequencing described in
'Api.Handler.Arkham.Events.deleteEpicEventAggregate' without a live database.

A pure, in-memory 'MonadEpicEventDeletion' instance ('TestDB') runs the exact
same production sequencing function and records every step it performs. This
lets us assert:

* a missing (or already-deleted) event short-circuits to 'EventDeletionMissing'
  before any organizer lookup, group selection, or delete is attempted;
* an existing event whose caller holds no Organizer membership row
  short-circuits to 'EventDeletionForbidden' before any group is selected or
  deleted;
* an authorized deletion selects every linked group game id (in deterministic
  ordinal order), deletes all of them, then the event row, in that exact
  order, and returns those same ids (in order) in 'EventDeletionDeleted';
* running the exact same production decision function twice in a row -- the
  second time against the state left behind by the first, committed deletion
  -- reproduces the "sequential repeat" contract end to end: the first call
  deletes, the second sees the event gone and reports 'EventDeletionMissing'
  (not a misleading 'EventDeletionForbidden');
* a failure injected at any single step -- locking the event, checking
  organizer membership, selecting linked games, deleting those games, or
  deleting the event row itself -- can never produce a successful
  ('EventDeletionDeleted', or any other) result, and no step after the
  injected failure is attempted;
* the tiny, pure 'eventDeletionCleanupGameIds' helper -- the one seam the
  production handler actually calls to decide whether to run any room
  cleanup at all -- returns games to clean up only for 'EventDeletionDeleted',
  never for 'EventDeletionMissing' or 'EventDeletionForbidden'.

As with "Arkham.Api.Events.EventCreationSpec", this pure interpreter's step
log is evidence of the deterministic order operations were attempted in and
where a sequence short-circuited -- it is not a simulation of transactional
rollback. Actual rollback of partially-applied writes on a real injected
failure is a property of 'runDB' (an uncaught exception aborts the whole
transaction), not of this test.
-}
module Arkham.Api.Events.EventDeletionSpec (spec) where

import Api.Handler.Arkham.Events
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

-- Fixtures ----------------------------------------------------------------------

fixtureEventId :: Epic.ArkhamEpicEventId
fixtureEventId = Epic.ArkhamEpicEventKey $ UUID.fromWords 0 0 0 999

fixtureOrganizerId :: User.UserId
fixtureOrganizerId = toSqlKey 7

-- | A game id distinguished only by its ordinal -- tests compare the returned
-- lists against these directly, so ordering bugs (e.g. an unordered select)
-- show up as an ordinary list-equality failure.
fixtureGameId :: Int -> GameEntity.ArkhamGameId
fixtureGameId ordx = GameEntity.ArkhamGameKey $ UUID.fromWords 0 0 0 (fromIntegral ordx)

fixtureLinkedGameIds :: [GameEntity.ArkhamGameId]
fixtureLinkedGameIds = [fixtureGameId 0, fixtureGameId 1, fixtureGameId 2]

-- Pure, in-memory persistence backend --------------------------------------------

-- | One recorded persistence step, in the order it happened.
data Step
  = -- | whether the locked row was found to exist
    LockedEvent Bool
  | -- | whether the caller held an Organizer membership row
    CheckedOrganizer Bool
  | SelectedGameIds [GameEntity.ArkhamGameId]
  | DeletedGames [GameEntity.ArkhamGameId]
  | DeletedEventRow
  deriving stock (Eq, Show)

-- | Which step (if any) should fail, for a given test run.
data FailAt
  = FailNever
  | FailAtLock
  | FailAtOrganizerCheck
  | FailAtSelect
  | FailAtDeleteGames
  | FailAtDeleteEvent
  deriving stock (Eq, Show)

-- | Mutable state threaded through 'TestDB': the step log; whether the event
-- row still "exists" (flipped to 'False' once the event row is deleted, so a
-- second run against the same threaded state models a sequential repeat
-- deletion); whether the caller holds an Organizer row; and the linked group
-- game ids a successful selection would return, in deterministic ordinal
-- order.
data TestState = TestState
  { steps :: [Step]
  , eventExists :: Bool
  , isOrganizerRow :: Bool
  , linkedGameIds :: [GameEntity.ArkhamGameId]
  }

fixtureTestState :: Bool -> Bool -> TestState
fixtureTestState eventExists isOrganizerRow =
  TestState
    { steps = []
    , eventExists
    , isOrganizerRow
    , linkedGameIds = fixtureLinkedGameIds
    }

{- | A pure interpreter of 'MonadEpicEventDeletion': records every step it is
asked to perform and short-circuits (via 'ExceptT') at the configured
'FailAt' step, exactly the way an uncaught exception aborts a real 'runDB'
transaction (see the rollback caveat documented on 'MonadEpicEventDeletion').
Note that, unlike a real 'runDB' transaction, this interpreter does not roll
back earlier entries already appended to its step log when a later operation
throws -- the log is evidence of the deterministic order operations were
attempted in and where the sequence short-circuited, not a simulation of what
would (or would not) remain committed.
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

instance MonadEpicEventDeletion TestDB where
  lockEpicEvent _eid = do
    failIfConfigured FailAtLock
    exists <- gets (.eventExists)
    recordStep (LockedEvent exists)
    pure exists

  isEventOrganizer _eid _uid = do
    failIfConfigured FailAtOrganizerCheck
    isOrganizer <- gets (.isOrganizerRow)
    recordStep (CheckedOrganizer isOrganizer)
    pure isOrganizer

  selectLinkedGameIds _eid = do
    failIfConfigured FailAtSelect
    gameIds <- gets (.linkedGameIds)
    recordStep (SelectedGameIds gameIds)
    pure gameIds

  deleteLinkedGames gameIds = do
    failIfConfigured FailAtDeleteGames
    recordStep (DeletedGames gameIds)

  deleteEpicEventRow _eid = do
    failIfConfigured FailAtDeleteEvent
    recordStep DeletedEventRow
    -- Models the event row now being gone, exactly as a real deleted row
    -- would be for any later call against the same database: a subsequent
    -- 'lockEpicEvent' in the same threaded state must see 'False'.
    modify \s -> s {eventExists = False}

-- Specs ---------------------------------------------------------------------------

spec :: Spec
spec = do
  describe "eventDeletionCleanupGameIds (room-cleanup decision seam)" do
    it "returns no games to clean up for a missing event" do
      eventDeletionCleanupGameIds EventDeletionMissing `shouldBe` []

    it "returns no games to clean up for a forbidden (non-organizer) deletion attempt" do
      eventDeletionCleanupGameIds EventDeletionForbidden `shouldBe` []

    it "returns exactly the committed game ids, in order, only for a deleted event" do
      eventDeletionCleanupGameIds (EventDeletionDeleted fixtureLinkedGameIds)
        `shouldBe` fixtureLinkedGameIds
      eventDeletionCleanupGameIds (EventDeletionDeleted []) `shouldBe` []

  describe "deleteEpicEventAggregate (atomic sequencing)" do
    let run failAt initial =
          runTestDB failAt initial (deleteEpicEventAggregate fixtureEventId fixtureOrganizerId)

    it "an organizer deleting an existing event locks, checks membership, selects every linked game (in ordinal order), deletes the games, then the event row, and returns the committed ids in order" do
      let (result, log_) = run FailNever (fixtureTestState True True)
      result `shouldBe` Right (EventDeletionDeleted fixtureLinkedGameIds)
      log_
        `shouldBe` [ LockedEvent True
                   , CheckedOrganizer True
                   , SelectedGameIds fixtureLinkedGameIds
                   , DeletedGames fixtureLinkedGameIds
                   , DeletedEventRow
                   ]

    it "supports zero linked games, matching a single-group-only or already-empty event" do
      let noGames = (fixtureTestState True True) {linkedGameIds = []}
          (result, log_) =
            runTestDB
              FailNever
              noGames
              (deleteEpicEventAggregate fixtureEventId fixtureOrganizerId)
      result `shouldBe` Right (EventDeletionDeleted [])
      log_
        `shouldBe` [LockedEvent True, CheckedOrganizer True, SelectedGameIds [], DeletedGames [], DeletedEventRow]

    it "a missing event short-circuits to EventDeletionMissing before any organizer lookup, group selection, or delete" do
      -- isOrganizerRow is deliberately True here: even a caller who WOULD be
      -- the organizer must not have their membership checked (or anything
      -- selected/deleted) once the locked row is found absent.
      let (result, log_) = run FailNever (fixtureTestState False True)
      result `shouldBe` Right EventDeletionMissing
      log_ `shouldBe` [LockedEvent False]

    it "an existing event's non-organizer caller short-circuits to EventDeletionForbidden before any group is selected or deleted" do
      let (result, log_) = run FailNever (fixtureTestState True False)
      result `shouldBe` Right EventDeletionForbidden
      log_ `shouldBe` [LockedEvent True, CheckedOrganizer False]

    it "sequential repeat: a second deletion attempt against the state a committed deletion left behind reports Missing, not Forbidden" do
      -- Threads one 'TestState' through two consecutive production-sequencing
      -- calls, exactly mirroring two sequential HTTP DELETE requests against
      -- the same event: the first commits (flips 'eventExists' to 'False'
      -- inside 'deleteEpicEventRow'); the second sees the row already gone.
      let both = do
            firstOutcome <- deleteEpicEventAggregate fixtureEventId fixtureOrganizerId
            secondOutcome <- deleteEpicEventAggregate fixtureEventId fixtureOrganizerId
            pure (firstOutcome, secondOutcome)
          (result, log_) = runTestDB FailNever (fixtureTestState True True) both
      case result of
        Left e -> expectationFailure ("unexpected failure in fixture setup: " <> e)
        Right (firstOutcome, secondOutcome) -> do
          firstOutcome `shouldBe` EventDeletionDeleted fixtureLinkedGameIds
          secondOutcome `shouldBe` EventDeletionMissing
      log_
        `shouldBe` [ LockedEvent True
                   , CheckedOrganizer True
                   , SelectedGameIds fixtureLinkedGameIds
                   , DeletedGames fixtureLinkedGameIds
                   , DeletedEventRow
                   , -- the second call's entire contribution: it never
                     -- reaches organizer checks, selection, or deletes
                     LockedEvent False
                   ]

    it "a failure locking the event cannot produce a success-shaped result, and nothing else is attempted" do
      let (result, log_) = run FailAtLock (fixtureTestState True True)
      result `shouldSatisfy` isLeft
      log_ `shouldBe` []

    it "a failure checking organizer membership cannot produce a success-shaped result, and no group is selected or deleted" do
      let (result, log_) = run FailAtOrganizerCheck (fixtureTestState True True)
      result `shouldSatisfy` isLeft
      log_ `shouldBe` [LockedEvent True]

    it "a failure selecting linked games cannot produce a success-shaped result, and nothing is deleted" do
      let (result, log_) = run FailAtSelect (fixtureTestState True True)
      result `shouldSatisfy` isLeft
      log_ `shouldBe` [LockedEvent True, CheckedOrganizer True]

    it "a failure deleting the linked games cannot produce a success-shaped result, and the event row is never deleted" do
      let (result, log_) = run FailAtDeleteGames (fixtureTestState True True)
      result `shouldSatisfy` isLeft
      log_
        `shouldBe` [LockedEvent True, CheckedOrganizer True, SelectedGameIds fixtureLinkedGameIds]

    it "a failure deleting the event row cannot produce a success-shaped result, even though every linked game was already deleted" do
      let (result, log_) = run FailAtDeleteEvent (fixtureTestState True True)
      result `shouldSatisfy` isLeft
      log_
        `shouldBe` [ LockedEvent True
                   , CheckedOrganizer True
                   , SelectedGameIds fixtureLinkedGameIds
                   , DeletedGames fixtureLinkedGameIds
                   ]
