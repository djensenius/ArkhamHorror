{- | Proves the atomic Epic-event deletion sequencing described in
'Api.Handler.Arkham.Events.deleteEpicEventAggregate' without a live database.

A pure, in-memory 'MonadEpicEventDeletion' instance ('TestDB') runs the exact
same production sequencing function and records every step it performs. This
lets us assert:

* a missing (or already-deleted) event short-circuits to 'EventDeletionMissing'
  before any organizer lookup, lock, or delete is attempted;
* an existing event whose caller holds no Organizer membership row
  short-circuits to 'EventDeletionForbidden' -- but only once confirmed
  against a ground-truth 'lockEpicEvent' check taken immediately after the
  unauthorized-looking read, before any game is selected or locked --
  driven by actual @(event, user, role)@ membership rows, not a bare boolean,
  so a 'GroupPlayer'-only membership is correctly rejected while a caller who
  ALSO holds an 'Organizer' row for that same event is correctly authorized,
  and a membership row for the wrong event id or wrong user id never grants
  access;
* an authorized deletion selects every linked group game id (in deterministic
  ordinal order), locks each one individually -- BEFORE the event, never
  after -- deletes each still-present game individually, then locks and
  deletes the event row, and returns exactly the present game ids (in order)
  in 'EventDeletionDeleted';
* a linked game that has vanished concurrently (e.g. a player deleting their
  own game directly, see 'Api.Handler.Arkham.Games.deleteApiV1ArkhamGameR') is
  simply excluded from what gets deleted and from the returned cleanup ids --
  never a partial-function crash, never treated as the whole event vanishing;
* an event that vanishes between the initial non-locking probe and the
  authoritative post-game-lock check (a concurrent deletion winning the race)
  is reported as 'EventDeletionMissing', never 'EventDeletionForbidden' and
  never a success, and no delete is attempted;
* an event that vanishes between the initial probe and the initial
  (unauthorized-looking) organizer check -- whose membership rows cascade
  away with the event itself, so even a genuine organizer reads back as
  unauthorized there -- is likewise reported as 'EventDeletionMissing', never
  'EventDeletionForbidden', and without ever selecting or locking a game;
* organizer membership is revalidated a second time, at the safe point after
  every lock is already held, and a caller who loses authorization between
  the two checks is rejected there too, before any delete;
* running the exact same production decision function twice in a row -- the
  second time against the state left behind by the first, committed deletion
  -- reproduces the "sequential repeat" contract end to end: the first call
  deletes, the second sees the event gone and reports 'EventDeletionMissing'
  (not a misleading 'EventDeletionForbidden');
* a failure injected at any single step -- the existence probe, either
  organizer check, selecting linked games, locking any single game, locking
  the event, or deleting any single game or the event row itself -- can never
  produce a successful ('EventDeletionDeleted', or any other) result, and no
  step after the injected failure is attempted. In particular, a failure
  injected at the SECOND (or later) per-game delete proves the earlier
  delete(s) were genuinely attempted individually, and that no later game
  delete, event delete, or successful result follows;
* the tiny, pure 'eventDeletionCleanupGameIds' helper -- the one seam the
  production handler actually calls to decide whether to run any room
  cleanup at all -- returns games to clean up only for 'EventDeletionDeleted',
  never for 'EventDeletionMissing' or 'EventDeletionForbidden', and reflects
  exactly the committed deletion plan (excluding any game that had already
  vanished).

Lock-order analysis (why games are always locked before the event): live
gameplay ('Api.Arkham.Helpers.atomicallyWithGame' + 'Api.Arkham.Epic.applyEpicDeltasLocked')
always locks a game first and the event second, inside the same transaction.
This module's production instance (see 'Api.Handler.Arkham.Events') follows
the identical order, so the two paths can never form a lock cycle: whichever
one gets to a shared game lock first simply makes the other wait on that same
game lock, never on the event lock. Two concurrent full deletions of the same
event are likewise safe: both select the same linked game ids in the same
ordinal order (a plain, unlocked read), so both attempt to lock games in the
identical order and cannot deadlock against each other either; whichever
commits first, the other observes the now-vanished game(s) and/or event via
the same "vanished concurrently" and "event vanished after game locks" cases
already covered above and below -- a real concurrent interleaving is a
composition of those two individually-tested outcomes, not a new code path.
None of this is (or can be) exercised by literally blocking two Haskell
threads against each other in a pure, single-threaded interpreter; it is
guaranteed by PostgreSQL's row-lock semantics for @SELECT ... FOR UPDATE@ and
by both code paths sharing one fixed lock order, as documented on
'Api.Handler.Arkham.Events.MonadEpicEventDeletion'.

As with "Arkham.Api.Events.EventCreationSpec", this pure interpreter's step
log is evidence of the deterministic order operations were attempted in and
where a sequence short-circuited -- it is not a simulation of transactional
rollback. Actual rollback of partially-applied writes on a real injected
failure is a property of 'runDB' (an uncaught exception aborts the whole
transaction), not of this test.
-}
module Arkham.Api.Events.EventDeletionSpec (spec) where

import Api.Handler.Arkham.Events
import Arkham.Epic.Types (EpicRole (..))
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

-- Fixtures ----------------------------------------------------------------------

fixtureEventId :: Epic.ArkhamEpicEventId
fixtureEventId = Epic.ArkhamEpicEventKey $ UUID.fromWords 0 0 0 999

-- | A distinct event id used only to prove a membership row for the wrong
-- event never grants access (see the "ids are honored" test).
otherEventId :: Epic.ArkhamEpicEventId
otherEventId = Epic.ArkhamEpicEventKey $ UUID.fromWords 0 0 0 998

fixtureOrganizerId :: User.UserId
fixtureOrganizerId = toSqlKey 7

-- | A distinct user id used only to prove a membership row for the wrong
-- user never grants access (see the "ids are honored" test).
otherUserId :: User.UserId
otherUserId = toSqlKey 8

-- | A game id distinguished only by its ordinal -- tests compare the returned
-- lists against these directly, so ordering bugs (e.g. an unordered select)
-- show up as an ordinary list-equality failure.
fixtureGameId :: Int -> GameEntity.ArkhamGameId
fixtureGameId ordx = GameEntity.ArkhamGameKey $ UUID.fromWords 0 0 0 (fromIntegral ordx)

fixtureLinkedGameIds :: [GameEntity.ArkhamGameId]
fixtureLinkedGameIds = [fixtureGameId 0, fixtureGameId 1, fixtureGameId 2]

-- | One real @arkham_epic_members@-shaped row: which event, which user, which
-- role. 'isEventOrganizer' below filters these exactly the way the production
-- 'P.exists' query filters on all three columns, so a row for the wrong event
-- or the wrong user never grants access, and a 'GroupPlayer' row never
-- substitutes for an 'Organizer' row.
type MembershipRow = (Epic.ArkhamEpicEventId, User.UserId, EpicRole)

organizerRow :: Epic.ArkhamEpicEventId -> User.UserId -> MembershipRow
organizerRow eid uid = (eid, uid, Organizer)

groupPlayerRow :: Epic.ArkhamEpicEventId -> User.UserId -> MembershipRow
groupPlayerRow eid uid = (eid, uid, GroupPlayer)

-- Pure, in-memory persistence backend --------------------------------------------

-- | One recorded persistence step, in the order it happened.
data Step
  = -- | the non-locking existence probe, and whether it found the row
    ProbedEventExists Bool
  | -- | whether the caller held an Organizer membership row (recorded both
    -- for the initial check and the post-lock revalidation -- their position
    -- in the log distinguishes which is which)
    CheckedOrganizer Bool
  | SelectedGameIds [GameEntity.ArkhamGameId]
  | -- | one game was locked ('FOR UPDATE'), and whether it was still present
    LockedGame GameEntity.ArkhamGameId Bool
  | -- | the event was locked ('FOR UPDATE'), after every present game; whether
    -- it was still present
    LockedEvent Bool
  | DeletedGame GameEntity.ArkhamGameId
  | DeletedEventRow
  deriving stock (Eq, Show)

{- | Which step (if any) should fail, for a given test run. The 'Int' on
'FailAtOrganizerCheck', 'FailAtLockGame', and 'FailAtDeleteGame' selects WHICH
occurrence of that repeated step should fail (1-based for the two organizer
checks; 0-based, in ordinal order, for the per-game steps), so a test can
target e.g. specifically the second game's delete without also matching the
first.
-}
data FailAt
  = FailNever
  | FailAtEventExistsProbe
  | FailAtOrganizerCheck Int
  | FailAtSelect
  | FailAtLockGame Int
  | FailAtLockEvent
  | FailAtDeleteGame Int
  | FailAtDeleteEvent
  deriving stock (Eq, Show)

{- | Mutable state threaded through 'TestDB'.

'eventExistsProbe' and 'eventExistsAtLock' are deliberately separate fields
(both usually equal) so a test can model an event that is present for the
cheap initial probe but has vanished by the time the authoritative
post-game-lock check runs -- exactly what a concurrent deletion winning the
race looks like from this request's point of view. 'deleteEpicEventRow'
flips both to 'False', so a second call threaded through the same state (a
sequential repeat) sees the row gone from its very first probe.

'gamePresence' models the same idea per linked game: a game absent from this
map (or mapped to 'False') is treated as already vanished when locked,
regardless of still being listed by 'linkedGameIds' -- exactly what a
concurrently, independently deleted game (see
'Api.Handler.Arkham.Games.deleteApiV1ArkhamGameR') looks like.
-}
data TestState = TestState
  { steps :: [Step]
  , eventExistsProbe :: Bool
  , eventExistsAtLock :: Bool
  , membershipRows :: [MembershipRow]
  , organizerCheckCount :: Int
  , linkedGameIds :: [GameEntity.ArkhamGameId]
  , gamePresence :: Map GameEntity.ArkhamGameId Bool
  , lockGameCallCount :: Int
  , deleteGameCallCount :: Int
  }

-- | The common case: event present (for both the probe and the lock), every
-- linked game present, given membership rows, zero steps recorded yet.
fixtureTestState :: Bool -> [MembershipRow] -> TestState
fixtureTestState eventPresent membership =
  TestState
    { steps = []
    , eventExistsProbe = eventPresent
    , eventExistsAtLock = eventPresent
    , membershipRows = membership
    , organizerCheckCount = 0
    , linkedGameIds = fixtureLinkedGameIds
    , gamePresence = Map.fromList [(gid, True) | gid <- fixtureLinkedGameIds]
    , lockGameCallCount = 0
    , deleteGameCallCount = 0
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
  eventExists _eid = do
    failIfConfigured FailAtEventExistsProbe
    exists <- gets (.eventExistsProbe)
    recordStep (ProbedEventExists exists)
    pure exists

  isEventOrganizer eid uid = do
    occurrence <- gets ((+ 1) . (.organizerCheckCount))
    failIfConfigured (FailAtOrganizerCheck occurrence)
    modify \s -> s {organizerCheckCount = occurrence}
    rows <- gets (.membershipRows)
    let isOrganizer = (eid, uid, Organizer) `elem` rows
    recordStep (CheckedOrganizer isOrganizer)
    pure isOrganizer

  selectLinkedGameIds _eid = do
    failIfConfigured FailAtSelect
    gameIds <- gets (.linkedGameIds)
    recordStep (SelectedGameIds gameIds)
    pure gameIds

  lockLinkedGame gid = do
    occurrence <- gets (.lockGameCallCount)
    failIfConfigured (FailAtLockGame occurrence)
    modify \s -> s {lockGameCallCount = occurrence + 1}
    present <- gets (Map.findWithDefault False gid . (.gamePresence))
    recordStep (LockedGame gid present)
    pure present

  lockEpicEvent _eid = do
    failIfConfigured FailAtLockEvent
    present <- gets (.eventExistsAtLock)
    recordStep (LockedEvent present)
    pure present

  deleteLinkedGame gid = do
    occurrence <- gets (.deleteGameCallCount)
    failIfConfigured (FailAtDeleteGame occurrence)
    modify \s -> s {deleteGameCallCount = occurrence + 1}
    recordStep (DeletedGame gid)

  deleteEpicEventRow _eid = do
    failIfConfigured FailAtDeleteEvent
    recordStep DeletedEventRow
    -- Models the event row now being gone, exactly as a real deleted row
    -- would be for any later call against the same database: a subsequent
    -- 'eventExists'/'lockEpicEvent' in the same threaded state must see
    -- 'False'.
    modify \s -> s {eventExistsProbe = False, eventExistsAtLock = False}

-- Specs ---------------------------------------------------------------------------

organizerMembership :: [MembershipRow]
organizerMembership = [organizerRow fixtureEventId fixtureOrganizerId]

-- | Both roles at once -- proves an Organizer who ALSO plays in a group is
-- still authorized (the two rows coexist under the per-role uniqueness
-- enforced by 'UniqueEpicMember', they are not mutually exclusive).
organizerAndGroupPlayerMembership :: [MembershipRow]
organizerAndGroupPlayerMembership =
  [organizerRow fixtureEventId fixtureOrganizerId, groupPlayerRow fixtureEventId fixtureOrganizerId]

groupPlayerOnlyMembership :: [MembershipRow]
groupPlayerOnlyMembership = [groupPlayerRow fixtureEventId fixtureOrganizerId]

fullLog :: [Step]
fullLog =
  [ ProbedEventExists True
  , CheckedOrganizer True
  , SelectedGameIds fixtureLinkedGameIds
  , LockedGame (fixtureGameId 0) True
  , LockedGame (fixtureGameId 1) True
  , LockedGame (fixtureGameId 2) True
  , LockedEvent True
  , CheckedOrganizer True
  , DeletedGame (fixtureGameId 0)
  , DeletedGame (fixtureGameId 1)
  , DeletedGame (fixtureGameId 2)
  , DeletedEventRow
  ]

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

    it "reflects the committed plan, excluding a game that had already vanished" do
      -- A vanished game is never in the 'EventDeletionDeleted' payload to
      -- begin with (see the aggregate-level test below); this just pins the
      -- helper's behavior on that already-filtered outcome directly.
      eventDeletionCleanupGameIds (EventDeletionDeleted [fixtureGameId 0, fixtureGameId 2])
        `shouldBe` [fixtureGameId 0, fixtureGameId 2]

  describe "deleteEpicEventAggregate (atomic sequencing)" do
    let run failAt state_ =
          runTestDB failAt state_ (deleteEpicEventAggregate fixtureEventId fixtureOrganizerId)

    it "an organizer deleting an existing event probes existence, checks membership, selects every linked game (ordinal order), locks each game then the event, revalidates membership, deletes each game then the event row, and returns the committed ids in order" do
      let (result, log_) = run FailNever (fixtureTestState True organizerMembership)
      result `shouldBe` Right (EventDeletionDeleted fixtureLinkedGameIds)
      log_ `shouldBe` fullLog

    it "authorizes a caller who holds both a GroupPlayer and an Organizer row for the event" do
      let (result, _log) = run FailNever (fixtureTestState True organizerAndGroupPlayerMembership)
      result `shouldBe` Right (EventDeletionDeleted fixtureLinkedGameIds)

    it "forbids a caller who holds only a GroupPlayer row for the event" do
      let (result, log_) = run FailNever (fixtureTestState True groupPlayerOnlyMembership)
      result `shouldBe` Right EventDeletionForbidden
      -- The event is still present, so the ground-truth 'lockEpicEvent' check
      -- taken after the unauthorized-looking read confirms Forbidden (never
      -- Missing) -- see the "vanishes ... looks forbidden" test below for the
      -- opposite case.
      log_ `shouldBe` [ProbedEventExists True, CheckedOrganizer False, LockedEvent True]

    it "honors both the event id and the user id: an Organizer row for the wrong event, or for the wrong user, never authorizes this deletion" do
      let wrongEvent = run FailNever (fixtureTestState True [organizerRow otherEventId fixtureOrganizerId])
          wrongUser = run FailNever (fixtureTestState True [organizerRow fixtureEventId otherUserId])
      fst wrongEvent `shouldBe` Right EventDeletionForbidden
      fst wrongUser `shouldBe` Right EventDeletionForbidden

    it "supports zero linked games, matching a single-group-only or already-empty event" do
      let noGames = (fixtureTestState True organizerMembership) {linkedGameIds = [], gamePresence = Map.empty}
          (result, log_) = runTestDB FailNever noGames (deleteEpicEventAggregate fixtureEventId fixtureOrganizerId)
      result `shouldBe` Right (EventDeletionDeleted [])
      log_
        `shouldBe` [ ProbedEventExists True
                   , CheckedOrganizer True
                   , SelectedGameIds []
                   , LockedEvent True
                   , CheckedOrganizer True
                   , DeletedEventRow
                   ]

    it "excludes a linked game that has vanished concurrently, without a partial-function crash, deleting only the still-present games and the event" do
      let vanished =
            (fixtureTestState True organizerMembership)
              {gamePresence = Map.insert (fixtureGameId 1) False (Map.fromList [(gid, True) | gid <- fixtureLinkedGameIds])}
          (result, log_) = run FailNever vanished
      result `shouldBe` Right (EventDeletionDeleted [fixtureGameId 0, fixtureGameId 2])
      log_
        `shouldBe` [ ProbedEventExists True
                   , CheckedOrganizer True
                   , SelectedGameIds fixtureLinkedGameIds
                   , LockedGame (fixtureGameId 0) True
                   , LockedGame (fixtureGameId 1) False
                   , LockedGame (fixtureGameId 2) True
                   , LockedEvent True
                   , CheckedOrganizer True
                   , DeletedGame (fixtureGameId 0)
                   , DeletedGame (fixtureGameId 2)
                   , DeletedEventRow
                   ]

    it "a missing event short-circuits to EventDeletionMissing before any organizer lookup, lock, or delete" do
      -- Membership is deliberately an Organizer row: even a caller who WOULD
      -- be authorized must not have their membership checked (or anything
      -- locked/selected/deleted) once the initial probe finds the row absent.
      let (result, log_) = run FailNever (fixtureTestState False organizerMembership)
      result `shouldBe` Right EventDeletionMissing
      log_ `shouldBe` [ProbedEventExists False]

    it "an event that vanishes between the initial probe and the post-game-lock check reports Missing, never Forbidden or a success, and never deletes anything" do
      -- Simulates a concurrent deletion committing after our cheap initial
      -- probe (which still saw the row) but before we finish locking every
      -- linked game and re-lock the event ourselves.
      let racedAway = (fixtureTestState True organizerMembership) {eventExistsAtLock = False}
          (result, log_) = run FailNever racedAway
      result `shouldBe` Right EventDeletionMissing
      log_
        `shouldBe` [ ProbedEventExists True
                   , CheckedOrganizer True
                   , SelectedGameIds fixtureLinkedGameIds
                   , LockedGame (fixtureGameId 0) True
                   , LockedGame (fixtureGameId 1) True
                   , LockedGame (fixtureGameId 2) True
                   , LockedEvent False
                   ]

    it "an event that vanishes between the initial probe and the initial (unauthorized-looking) organizer check reports Missing, never Forbidden -- even for a caller with no membership row at all" do
      -- The event's own membership rows cascade-delete with it, so a
      -- concurrently-deleted event makes ANY caller -- including a genuine
      -- organizer -- read back as unauthorized on the initial, non-locking
      -- 'isEventOrganizer' check. Reporting Forbidden there would wrongly
      -- disclose that the event still exists; the ground-truth 'lockEpicEvent'
      -- check this branch takes must correct that to Missing instead, and
      -- must do so without ever selecting or locking a game.
      let racedAway = (fixtureTestState True []) {eventExistsAtLock = False}
          (result, log_) = run FailNever racedAway
      result `shouldBe` Right EventDeletionMissing
      log_
        `shouldBe` [ ProbedEventExists True
                   , CheckedOrganizer False
                   , LockedEvent False
                   ]

    it "revalidates organizer membership at the safe point after every lock is held, rejecting a caller who lost authorization in between, before any delete" do
      -- Membership is removed (by mutating state directly) the moment the
      -- event lock succeeds -- i.e. between the two 'isEventOrganizer' calls
      -- -- to prove the SECOND check is a real, independent re-read, not a
      -- cached result of the first.
      let revoke s = case s.steps of
            steps' | LockedEvent True `elem` steps' -> s {membershipRows = []}
            _ -> s
          instrumented =
            runTestDB FailNever (fixtureTestState True organizerMembership) do
              found <- eventExists fixtureEventId
              organizer1 <- isEventOrganizer fixtureEventId fixtureOrganizerId
              gids <- selectLinkedGameIds fixtureEventId
              present <- mapM lockLinkedGame gids
              stillThere <- lockEpicEvent fixtureEventId
              modify revoke
              organizer2 <- isEventOrganizer fixtureEventId fixtureOrganizerId
              pure (found, organizer1, present, stillThere, organizer2)
          (result, _log) = instrumented
      case result of
        Left e -> expectationFailure ("unexpected failure in fixture setup: " <> e)
        Right (found, organizer1, present, stillThere, organizer2) -> do
          found `shouldBe` True
          organizer1 `shouldBe` True
          present `shouldBe` [True, True, True]
          stillThere `shouldBe` True
          -- The revalidation call, run after membership was revoked, must
          -- see the caller as no longer authorized.
          organizer2 `shouldBe` False

    it "sequential repeat: a second deletion attempt against the state a committed deletion left behind reports Missing, not Forbidden" do
      -- Threads one 'TestState' through two consecutive production-sequencing
      -- calls, exactly mirroring two sequential HTTP DELETE requests against
      -- the same event: the first commits (flips both existence flags to
      -- 'False' inside 'deleteEpicEventRow'); the second sees the row already
      -- gone at its very first, cheap probe.
      let both = do
            firstOutcome <- deleteEpicEventAggregate fixtureEventId fixtureOrganizerId
            secondOutcome <- deleteEpicEventAggregate fixtureEventId fixtureOrganizerId
            pure (firstOutcome, secondOutcome)
          (result, log_) = runTestDB FailNever (fixtureTestState True organizerMembership) both
      case result of
        Left e -> expectationFailure ("unexpected failure in fixture setup: " <> e)
        Right (firstOutcome, secondOutcome) -> do
          firstOutcome `shouldBe` EventDeletionDeleted fixtureLinkedGameIds
          secondOutcome `shouldBe` EventDeletionMissing
      log_ `shouldBe` fullLog <> [ProbedEventExists False]

    it "a failure at the initial existence probe cannot produce a success-shaped result, and nothing else is attempted" do
      let (result, log_) = run FailAtEventExistsProbe (fixtureTestState True organizerMembership)
      result `shouldSatisfy` isLeft
      log_ `shouldBe` []

    it "a failure at the initial organizer check cannot produce a success-shaped result, and no lock or delete is attempted" do
      let (result, log_) = run (FailAtOrganizerCheck 1) (fixtureTestState True organizerMembership)
      result `shouldSatisfy` isLeft
      log_ `shouldBe` [ProbedEventExists True]

    it "a failure locking the event in the unauthorized-looking ground-truth check cannot produce a success-shaped result, and no game is ever selected or locked" do
      let (result, log_) = run FailAtLockEvent (fixtureTestState True groupPlayerOnlyMembership)
      result `shouldSatisfy` isLeft
      log_ `shouldBe` [ProbedEventExists True, CheckedOrganizer False]

    it "a failure selecting linked games cannot produce a success-shaped result, and no lock or delete is attempted" do
      let (result, log_) = run FailAtSelect (fixtureTestState True organizerMembership)
      result `shouldSatisfy` isLeft
      log_ `shouldBe` [ProbedEventExists True, CheckedOrganizer True]

    it "a failure locking the first linked game cannot produce a success-shaped result, and no later game or the event is locked or deleted" do
      let (result, log_) = run (FailAtLockGame 0) (fixtureTestState True organizerMembership)
      result `shouldSatisfy` isLeft
      log_ `shouldBe` [ProbedEventExists True, CheckedOrganizer True, SelectedGameIds fixtureLinkedGameIds]

    it "a failure locking the SECOND linked game proves the first was genuinely locked first, and nothing further is locked or deleted" do
      let (result, log_) = run (FailAtLockGame 1) (fixtureTestState True organizerMembership)
      result `shouldSatisfy` isLeft
      log_
        `shouldBe` [ ProbedEventExists True
                   , CheckedOrganizer True
                   , SelectedGameIds fixtureLinkedGameIds
                   , LockedGame (fixtureGameId 0) True
                   ]

    it "a failure locking the event -- after every game is already locked -- cannot produce a success-shaped result, and nothing is deleted" do
      let (result, log_) = run FailAtLockEvent (fixtureTestState True organizerMembership)
      result `shouldSatisfy` isLeft
      log_
        `shouldBe` [ ProbedEventExists True
                   , CheckedOrganizer True
                   , SelectedGameIds fixtureLinkedGameIds
                   , LockedGame (fixtureGameId 0) True
                   , LockedGame (fixtureGameId 1) True
                   , LockedGame (fixtureGameId 2) True
                   ]

    it "a failure at the revalidation organizer check (after the event is locked) cannot produce a success-shaped result, and nothing is deleted" do
      let (result, log_) = run (FailAtOrganizerCheck 2) (fixtureTestState True organizerMembership)
      result `shouldSatisfy` isLeft
      log_
        `shouldBe` [ ProbedEventExists True
                   , CheckedOrganizer True
                   , SelectedGameIds fixtureLinkedGameIds
                   , LockedGame (fixtureGameId 0) True
                   , LockedGame (fixtureGameId 1) True
                   , LockedGame (fixtureGameId 2) True
                   , LockedEvent True
                   ]

    it "a failure deleting the FIRST game cannot produce a success-shaped result, and no game or the event row is deleted" do
      let (result, log_) = run (FailAtDeleteGame 0) (fixtureTestState True organizerMembership)
      result `shouldSatisfy` isLeft
      log_
        `shouldBe` [ ProbedEventExists True
                   , CheckedOrganizer True
                   , SelectedGameIds fixtureLinkedGameIds
                   , LockedGame (fixtureGameId 0) True
                   , LockedGame (fixtureGameId 1) True
                   , LockedGame (fixtureGameId 2) True
                   , LockedEvent True
                   , CheckedOrganizer True
                   ]

    it "a failure deleting the SECOND (or later) game proves the first delete was genuinely attempted, and that no later game delete, event delete, or successful result follows" do
      let (result, log_) = run (FailAtDeleteGame 1) (fixtureTestState True organizerMembership)
      result `shouldSatisfy` isLeft
      log_
        `shouldBe` [ ProbedEventExists True
                   , CheckedOrganizer True
                   , SelectedGameIds fixtureLinkedGameIds
                   , LockedGame (fixtureGameId 0) True
                   , LockedGame (fixtureGameId 1) True
                   , LockedGame (fixtureGameId 2) True
                   , LockedEvent True
                   , CheckedOrganizer True
                   , DeletedGame (fixtureGameId 0)
                   ]

    it "a failure deleting the event row cannot produce a success-shaped result, even though every linked game was already deleted" do
      let (result, log_) = run FailAtDeleteEvent (fixtureTestState True organizerMembership)
      result `shouldSatisfy` isLeft
      log_
        `shouldBe` [ ProbedEventExists True
                   , CheckedOrganizer True
                   , SelectedGameIds fixtureLinkedGameIds
                   , LockedGame (fixtureGameId 0) True
                   , LockedGame (fixtureGameId 1) True
                   , LockedGame (fixtureGameId 2) True
                   , LockedEvent True
                   , CheckedOrganizer True
                   , DeletedGame (fixtureGameId 0)
                   , DeletedGame (fixtureGameId 1)
                   , DeletedGame (fixtureGameId 2)
                   ]
