{- | Proves the Main Street group swap decision sequencing described in
'Api.Handler.Arkham.Games.Shared.planAndExecuteMainStreetSwap' without a live
database, mirroring the approach in "Arkham.Api.Events.EventDeletionSpec" for
'Api.Handler.Arkham.Events.deleteEpicEventAggregate'.

A pure, in-memory 'MonadMainStreetSwap' instance ('TestDB') runs the exact
same production decision function and records every step it performs. This
lets us assert:

* an unknown group for either requested ordinal (no 'ArkhamEpicGroup' row at
  all) resolves to 'MainStreetSwapMissing', with no lock ever taken and
  'performMainStreetSwap' never called;
* a group that exists but has no linked game (a null @arkhamGameId@
  relation) resolves identically to an unknown group -- both collapse to the
  same nondisclosing outcome, exactly like production's
  @mGroup >>= arkhamEpicGroupArkhamGameId . entityVal@;
* two DISTINCT requested ordinals that resolve to the SAME game id report
  'MainStreetSwapSameGame' -- before a single lock is taken, and before the
  two ordinals are ever treated as two independent sides to swap;
* a game vanishing concurrently before its own lock could be taken --
  whichever side of the request it was -- reports 'MainStreetSwapMissing',
  with 'performMainStreetSwap' never called and no write ever attempted;
* BOTH locks in the canonical order are always attempted before either
  result is inspected: a scenario where the FIRST canonically-ordered lock
  succeeds but the SECOND is absent (and the mirror, first absent, second
  present) both still show every lock in the canonical order was attempted,
  proving no lock is ever skipped just because an earlier one already
  failed;
* an authorized, fully-present swap request -- including one naming its
  ordinals in descending order -- locks its two games in ascending
  'ArkhamGameId' order alone (see "Arkham.Api.EpicGameLockOrderSpec" and
  "Arkham.Api.MainStreetSwapPlanSpec" for the pure ordering function
  itself, and why ordinal plays no part in it), then calls
  'performMainStreetSwap' exactly once, and reports
  'MainStreetSwapCompleted' carrying a plan whose
  'firstGameId'\/'secondGameId' are unchanged from the request regardless
  of lock order;
* a failure injected at either 'resolveSwapGame' call, either 'lockSwapGame'
  call, or 'performMainStreetSwap' itself can never produce a
  success-shaped ('MainStreetSwapCompleted', or any other) result, and no
  step after the injected failure is attempted;
* 'performMainStreetSwap' reporting stale/invalid LOCKED state (both games
  present, but not swap-able) is a TYPED, ordinary outcome
  ('MainStreetSwapInvalidState'), not a crash: both locks are still taken,
  the swap is still attempted exactly once, and every distinct
  'MainStreetSwapStateFailure' reason survives unchanged through
  'planAndExecuteMainStreetSwap'.

'mainStreetSwapCleanupPlan' (mirroring
'Api.Handler.Arkham.Events.eventDeletionCleanupGameIds') is proven directly:
only 'MainStreetSwapCompleted' authorizes any post-commit action -- every
other outcome, including every 'MainStreetSwapInvalidState' reason, does
not.

Separately, 'resolveSwapParticipants', 'resolveSwapInvestigator',
'validateSwapPlacement', and 'mainStreetLocation' -- the production-used
pure precondition helpers 'swapInvestigatorState' composes, formerly a chain
of 'error'\/'fromMaybe (error ...)' calls -- are exercised directly with
REAL 'Investigator'\/'Entities' values (built via the same
'lookupInvestigator'\/'lookupLocation' production itself uses), covering
every distinct failure cause: an absent ready investigator on either side,
both sides naming the same investigator, an investigator id missing from
its own game's entities, no Main Street location in play, and an
investigator whose current placement no longer matches the expected
destination.

As with "Arkham.Api.Events.EventDeletionSpec", this pure interpreter's step
log is evidence of the deterministic order operations were attempted in and
where a sequence short-circuited -- it is not a simulation of transactional
rollback. Actual rollback of a partially-applied write on a real injected
failure is a property of 'runDB' (an uncaught exception aborts the whole
transaction), not of this test.
-}
module Arkham.Api.MainStreetSwapSpec (spec) where

import Api.Arkham.Types.MultiplayerVariant (MultiplayerVariant (WithFriends))
import Api.Handler.Arkham.Games.Shared
import Arkham.Card.CardCode (CardCode (..))
import Arkham.Card.Id (nullCardId)
import Arkham.Classes.Entity (overAttrs)
import Arkham.Difficulty (Difficulty (Standard))
import Arkham.Entities (Entities (..))
import Arkham.Game (newCampaign)
import Arkham.Id (InvestigatorId (..), LocationId (..), PlayerId (..))
import Arkham.Investigator (lookupInvestigator)
import Arkham.Investigator.Types (Investigator, investigatorPlacement)
import Arkham.Location (lookupLocation)
import Arkham.Placement (Placement (AtLocation))
import Arkham.Prelude
import Control.Monad.Except (ExceptT, MonadError, runExceptT, throwError)
import Control.Monad.State.Strict (MonadState, State, gets, modify, runState)
import Data.Either (isLeft)
import Data.Map.Strict qualified as Map
import Data.UUID qualified as UUID
import Entity.Arkham.Epic qualified as Epic
import Entity.Arkham.Game qualified as GameEntity
import Test.Hspec

-- Fixtures --------------------------------------------------------------------

fixtureEventId :: Epic.ArkhamEpicEventId
fixtureEventId = Epic.ArkhamEpicEventKey $ UUID.fromWords 0 0 0 999

-- | A game id distinguished only by its tag, exactly as the sibling deletion
-- and lock-order specs fixture their own game ids.
fixtureGameId :: Int -> GameEntity.ArkhamGameId
fixtureGameId tag = GameEntity.ArkhamGameKey $ UUID.fromWords 0 0 0 (fromIntegral tag)

-- Fixtures for the pure precondition helpers (production-used, see
-- 'resolveSwapParticipants', 'resolveSwapInvestigator', 'validateSwapPlacement',
-- 'mainStreetLocation') -----------------------------------------------------

-- | Roland Banks (card 01001) -- any real investigator id works, since
-- these helpers never consult card-specific behavior, only identity and
-- placement.
rolandId :: InvestigatorId
rolandId = InvestigatorId (CardCode "01001")

-- | Daisy Walker (card 01002) -- a SECOND, distinct real investigator id.
daisyId :: InvestigatorId
daisyId = InvestigatorId (CardCode "01002")

fixturePlayerId :: Int -> PlayerId
fixturePlayerId tag = PlayerId $ UUID.fromWords 0 0 0 (fromIntegral tag)

fixtureLocationId :: Int -> LocationId
fixtureLocationId tag = LocationId $ UUID.fromWords 0 0 0 (fromIntegral tag)

-- | A real 'Investigator' entity (via the same 'lookupInvestigator' production
-- uses) with its placement forced to a specific location, exactly the shape
-- 'resolveSwapInvestigator'\/'validateSwapPlacement' consume.
fixtureInvestigator :: InvestigatorId -> PlayerId -> LocationId -> Investigator
fixtureInvestigator iid pid location =
  overAttrs (\a -> a {investigatorPlacement = AtLocation location}) (lookupInvestigator iid pid)

-- | 'Entities' containing exactly one investigator, keyed by its id.
fixtureEntitiesWithInvestigator :: InvestigatorId -> Investigator -> Entities
fixtureEntitiesWithInvestigator iid investigator = mempty {entitiesInvestigators = Map.singleton iid investigator}

-- | 'Entities' containing exactly one Main Street location (card 89006), via
-- the same 'lookupLocation' production uses.
fixtureEntitiesWithMainStreet :: LocationId -> Entities
fixtureEntitiesWithMainStreet lid =
  mempty {entitiesLocations = Map.singleton lid (lookupLocation (CardCode "89006") lid nullCardId)}

{- | A minimal 'GameEntity.ArkhamGame' row for 'lockSwapGame' to return when a
game is present. 'arkhamGameCurrentData' is a real, fully-built 'Game' (via
the same 'newCampaign' helper "Arkham.Game.PendingGameOptionsSpec" already
uses for fixtures) -- entity fields in this codebase are strict
(@StrictData@ is a default extension, see @package.yaml@), so an unforced
@error@ thunk here would blow up merely by being placed in the record, not
by being genuinely read. This pure interpreter's 'performMainStreetSwap'
still never calls the real 'swapInvestigatorState' transform on it (that
transform's own total behavior, using REAL 'Investigator'\/'Entities'
values, is exercised directly by the \"pure precondition helpers\" spec
block below) -- it only ever records that a swap was attempted and returns
the configured 'FailAt' outcome.
-}
fixtureArkhamGame :: GameEntity.ArkhamGame
fixtureArkhamGame =
  GameEntity.ArkhamGame
    { GameEntity.arkhamGameName = "fixture"
    , GameEntity.arkhamGameCurrentData = newCampaign "06" Nothing 0 1 Standard False
    , GameEntity.arkhamGameStep = 0
    , GameEntity.arkhamGameMultiplayerVariant = WithFriends
    , GameEntity.arkhamGameCreatedAt = fixtureTime
    , GameEntity.arkhamGameUpdatedAt = fixtureTime
    }
 where
  fixtureTime = UTCTime (ModifiedJulianDay 0) 0

-- Pure, in-memory persistence backend ------------------------------------------

-- | One recorded step, in the order it happened.
data Step
  = -- | one requested ordinal was resolved, and to what (if anything)
    ResolvedGame Int (Maybe GameEntity.ArkhamGameId)
  | -- | one game was locked ('FOR UPDATE'), and whether it was still present
    LockedGame GameEntity.ArkhamGameId Bool
  | -- | the swap itself was performed, for this plan
    PerformedSwap MainStreetSwapPlan
  deriving stock (Eq, Show)

{- | Which step (if any) should fail. The 'Int' on 'FailAtResolve' and
'FailAtLock' selects WHICH occurrence (1-based for resolves, matching the
two 'resolveSwapGame' calls; 0-based, in canonical lock order, for
'FailAtLock') should fail, so a test can target specifically the second
resolve or specifically the second canonical lock without also matching the
first. 'InvalidStateAtPerform' is NOT a hard failure (it never
'throwError's): it models 'performMainStreetSwap' itself returning a typed
'Left' -- exactly what stale\/malformed persisted state now produces in
production -- so a test can assert 'planAndExecuteMainStreetSwap' reports
the corresponding 'MainStreetSwapInvalidState' as an ordinary, successful
'Right' result, distinct from 'FailAtPerform' (a genuine aborting
exception).
-}
data FailAt
  = FailNever
  | FailAtResolve Int
  | FailAtLock Int
  | FailAtPerform
  | InvalidStateAtPerform MainStreetSwapStateFailure
  deriving stock (Eq, Show)

{- | Mutable state threaded through 'TestDB'.

'relations' models the @arkham_epic_groups@ table's @(ordinal, game id)@
projection directly: a missing key is "no such group for this ordinal"
(unknown group) and a present key mapped to 'Nothing' is "the group exists
but its @arkhamGameId@ column is null" -- both collapse to the same
'Nothing' once looked up with 'Map.findWithDefault Nothing', exactly
mirroring production's @mGroup >>= arkhamEpicGroupArkhamGameId . entityVal@,
which never distinguishes them either.

'gamePresence' models a linked game vanishing concurrently, independent of
this swap, the same way "Arkham.Api.Events.EventDeletionSpec" does: a game
absent from this map (or mapped to 'False') is treated as gone when its lock
is attempted, regardless of what 'relations' resolved it to.
-}
data TestState = TestState
  { steps :: [Step]
  , relations :: Map Int (Maybe GameEntity.ArkhamGameId)
  , gamePresence :: Map GameEntity.ArkhamGameId Bool
  , resolveCallCount :: Int
  , lockCallCount :: Int
  }

-- | The common case: both requested ordinals resolve to distinct, present
-- games, zero steps recorded yet.
fixtureTestState :: Map Int (Maybe GameEntity.ArkhamGameId) -> Map GameEntity.ArkhamGameId Bool -> TestState
fixtureTestState relations gamePresence =
  TestState
    { steps = []
    , relations
    , gamePresence
    , resolveCallCount = 0
    , lockCallCount = 0
    }

{- | A pure interpreter of 'MonadMainStreetSwap': records every step it is
asked to perform and short-circuits (via 'ExceptT') at the configured
'FailAt' step, exactly the way an uncaught exception aborts a real 'runDB'
transaction. As with 'Arkham.Api.Events.EventDeletionSpec' \'s 'TestDB', this
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

instance MonadMainStreetSwap TestDB where
  resolveSwapGame _eid ordinal = do
    occurrence <- gets ((+ 1) . (.resolveCallCount))
    failIfConfigured (FailAtResolve occurrence)
    modify \s -> s {resolveCallCount = occurrence}
    result <- gets (Map.findWithDefault Nothing ordinal . (.relations))
    recordStep (ResolvedGame ordinal result)
    pure result

  lockSwapGame gid = do
    occurrence <- gets (.lockCallCount)
    failIfConfigured (FailAtLock occurrence)
    modify \s -> s {lockCallCount = occurrence + 1}
    present <- gets (Map.findWithDefault False gid . (.gamePresence))
    recordStep (LockedGame gid present)
    pure $ if present then Just fixtureArkhamGame else Nothing

  performMainStreetSwap plan _firstRaw _secondRaw = do
    failIfConfigured FailAtPerform
    recordStep (PerformedSwap plan)
    configured <- ask
    pure $ case configured of
      InvalidStateAtPerform failure -> Left failure
      _ -> Right ()

-- Specs -------------------------------------------------------------------------

spec :: Spec
spec = do
  mainStreetSwapSequencingSpec
  mainStreetSwapCleanupPlanSpec
  swapPreconditionHelpersSpec

mainStreetSwapSequencingSpec :: Spec
mainStreetSwapSequencingSpec = describe "planAndExecuteMainStreetSwap (Main Street swap decision sequencing)" do
  let run failAt state_ ordinal1 ordinal2 =
        runTestDB failAt state_ (planAndExecuteMainStreetSwap fixtureEventId ordinal1 ordinal2)

  it "an unknown first ordinal (no group row at all) reports Missing, with no lock and no swap performed" do
    let state_ = fixtureTestState (Map.fromList [(2, Just (fixtureGameId 2))]) (Map.fromList [(fixtureGameId 2, True)])
        (result, log_) = run FailNever state_ 1 2
    result `shouldBe` Right MainStreetSwapMissing
    log_ `shouldBe` [ResolvedGame 1 Nothing, ResolvedGame 2 (Just (fixtureGameId 2))]

  it "an unknown second ordinal reports Missing, with no lock and no swap performed" do
    let state_ = fixtureTestState (Map.fromList [(1, Just (fixtureGameId 1))]) (Map.fromList [(fixtureGameId 1, True)])
        (result, log_) = run FailNever state_ 1 2
    result `shouldBe` Right MainStreetSwapMissing
    log_ `shouldBe` [ResolvedGame 1 (Just (fixtureGameId 1)), ResolvedGame 2 Nothing]

  it "a group that exists but has a null linked game (Just Nothing) reports Missing, exactly like an unknown group" do
    let state_ =
          fixtureTestState
            (Map.fromList [(1, Nothing), (2, Just (fixtureGameId 2))])
            (Map.fromList [(fixtureGameId 2, True)])
        (result, log_) = run FailNever state_ 1 2
    result `shouldBe` Right MainStreetSwapMissing
    log_ `shouldBe` [ResolvedGame 1 Nothing, ResolvedGame 2 (Just (fixtureGameId 2))]

  it "two distinct requested ordinals resolving to the SAME game report SameGame, before any lock is taken" do
    let state_ =
          fixtureTestState
            (Map.fromList [(1, Just (fixtureGameId 5)), (2, Just (fixtureGameId 5))])
            (Map.fromList [(fixtureGameId 5, True)])
        (result, log_) = run FailNever state_ 1 2
    result `shouldBe` Right MainStreetSwapSameGame
    log_ `shouldBe` [ResolvedGame 1 (Just (fixtureGameId 5)), ResolvedGame 2 (Just (fixtureGameId 5))]

  it "the FIRST requested ordinal's game vanishing before its lock reports Missing, with no swap performed" do
    let state_ =
          fixtureTestState
            (Map.fromList [(1, Just (fixtureGameId 1)), (2, Just (fixtureGameId 2))])
            (Map.fromList [(fixtureGameId 1, False), (fixtureGameId 2, True)])
        (result, log_) = run FailNever state_ 1 2
    result `shouldBe` Right MainStreetSwapMissing
    -- Canonical lock order is ascending by 'ArkhamGameId' (fixture ids 1
    -- and 2 here, which happen to equal their ordinals), so game 1's
    -- absent lock is attempted first, but game 2's is STILL attempted
    -- (see the "both locks always attempted" tests below): no
    -- 'PerformedSwap' step follows either way.
    log_
      `shouldBe` [ ResolvedGame 1 (Just (fixtureGameId 1))
                 , ResolvedGame 2 (Just (fixtureGameId 2))
                 , LockedGame (fixtureGameId 1) False
                 , LockedGame (fixtureGameId 2) True
                 ]

  it "the SECOND requested ordinal's game vanishing before its lock reports Missing, with no swap performed" do
    let state_ =
          fixtureTestState
            (Map.fromList [(1, Just (fixtureGameId 1)), (2, Just (fixtureGameId 2))])
            (Map.fromList [(fixtureGameId 1, True), (fixtureGameId 2, False)])
        (result, log_) = run FailNever state_ 1 2
    result `shouldBe` Right MainStreetSwapMissing
    log_
      `shouldBe` [ ResolvedGame 1 (Just (fixtureGameId 1))
                 , ResolvedGame 2 (Just (fixtureGameId 2))
                 , LockedGame (fixtureGameId 1) True
                 , LockedGame (fixtureGameId 2) False
                 ]

  it "when the canonically-FIRST lock succeeds but the canonically-SECOND is absent, both locks are still attempted, and no swap is performed" do
    -- Fixture game id 0 < game id 1, so canonical order locks game 0
    -- first (ordinal happens to equal id here, but id alone decides).
    -- Game 0
    -- is present, game 1 is absent: 'and lockResults' must still be
    -- checked only after BOTH are attempted, not short-circuited after the
    -- first succeeds.
    let state_ =
          fixtureTestState
            (Map.fromList [(0, Just (fixtureGameId 0)), (1, Just (fixtureGameId 1))])
            (Map.fromList [(fixtureGameId 0, True), (fixtureGameId 1, False)])
        (result, log_) = run FailNever state_ 0 1
    result `shouldBe` Right MainStreetSwapMissing
    log_
      `shouldBe` [ ResolvedGame 0 (Just (fixtureGameId 0))
                 , ResolvedGame 1 (Just (fixtureGameId 1))
                 , LockedGame (fixtureGameId 0) True
                 , LockedGame (fixtureGameId 1) False
                 ]

  it "when the canonically-FIRST lock is absent, the canonically-SECOND is still attempted even though the outcome is already determined, and no swap is performed" do
    let state_ =
          fixtureTestState
            (Map.fromList [(0, Just (fixtureGameId 0)), (1, Just (fixtureGameId 1))])
            (Map.fromList [(fixtureGameId 0, False), (fixtureGameId 1, True)])
        (result, log_) = run FailNever state_ 0 1
    result `shouldBe` Right MainStreetSwapMissing
    log_
      `shouldBe` [ ResolvedGame 0 (Just (fixtureGameId 0))
                 , ResolvedGame 1 (Just (fixtureGameId 1))
                 , LockedGame (fixtureGameId 0) False
                 , LockedGame (fixtureGameId 1) True
                 ]

  it "an authorized, fully-present swap requested in ASCENDING ordinal order locks in canonical order and performs the swap exactly once" do
    let state_ =
          fixtureTestState
            (Map.fromList [(1, Just (fixtureGameId 1)), (2, Just (fixtureGameId 2))])
            (Map.fromList [(fixtureGameId 1, True), (fixtureGameId 2, True)])
        (result, log_) = run FailNever state_ 1 2
        expectedPlan = mainStreetSwapPlan (fixtureGameId 1) (fixtureGameId 2)
    result `shouldBe` Right (MainStreetSwapCompleted expectedPlan)
    log_
      `shouldBe` [ ResolvedGame 1 (Just (fixtureGameId 1))
                 , ResolvedGame 2 (Just (fixtureGameId 2))
                 , LockedGame (fixtureGameId 1) True
                 , LockedGame (fixtureGameId 2) True
                 , PerformedSwap expectedPlan
                 ]

  it "an authorized, fully-present swap requested in DESCENDING ordinal order (2 then 1) still locks ascending (game 1, then game 2), while the plan's firstGameId/secondGameId preserve the request's own mapping" do
    let state_ =
          fixtureTestState
            (Map.fromList [(1, Just (fixtureGameId 1)), (2, Just (fixtureGameId 2))])
            (Map.fromList [(fixtureGameId 1, True), (fixtureGameId 2, True)])
        (result, log_) = run FailNever state_ 2 1
        expectedPlan = mainStreetSwapPlan (fixtureGameId 2) (fixtureGameId 1)
    result `shouldBe` Right (MainStreetSwapCompleted expectedPlan)
    expectedPlan.lockOrder `shouldBe` [fixtureGameId 1, fixtureGameId 2]
    expectedPlan.firstGameId `shouldBe` fixtureGameId 2
    expectedPlan.secondGameId `shouldBe` fixtureGameId 1
    log_
      `shouldBe` [ ResolvedGame 2 (Just (fixtureGameId 2))
                 , ResolvedGame 1 (Just (fixtureGameId 1))
                 , LockedGame (fixtureGameId 1) True
                 , LockedGame (fixtureGameId 2) True
                 , PerformedSwap expectedPlan
                 ]

  it "this matches deleteEpicEventAggregate's own lock order: both paths lock the same two games in the identical ascending order regardless of request/selection order" do
    -- Cross-checks against the same shared 'canonicalEpicGameLockOrder' /
    -- 'mainStreetSwapPlan' seam "Arkham.Api.Events.EventDeletionSpec" and
    -- "Arkham.Api.EpicGameLockOrderSpec" exercise directly.
    let state_ =
          fixtureTestState
            (Map.fromList [(1, Just (fixtureGameId 1)), (2, Just (fixtureGameId 2))])
            (Map.fromList [(fixtureGameId 1, True), (fixtureGameId 2, True)])
        (_, logDescending) = run FailNever state_ 2 1
        (_, logAscending) = run FailNever state_ 1 2
        lockSteps steps' = [s | s@(LockedGame _ _) <- steps']
    lockSteps logDescending `shouldBe` lockSteps logAscending

  it "a failure at the first resolveSwapGame call cannot produce a success-shaped result, and nothing else is attempted" do
    let state_ =
          fixtureTestState
            (Map.fromList [(1, Just (fixtureGameId 1)), (2, Just (fixtureGameId 2))])
            (Map.fromList [(fixtureGameId 1, True), (fixtureGameId 2, True)])
        (result, log_) = run (FailAtResolve 1) state_ 1 2
    result `shouldSatisfy` isLeft
    log_ `shouldBe` []

  it "a failure at the second resolveSwapGame call cannot produce a success-shaped result, and no lock or swap is attempted" do
    let state_ =
          fixtureTestState
            (Map.fromList [(1, Just (fixtureGameId 1)), (2, Just (fixtureGameId 2))])
            (Map.fromList [(fixtureGameId 1, True), (fixtureGameId 2, True)])
        (result, log_) = run (FailAtResolve 2) state_ 1 2
    result `shouldSatisfy` isLeft
    log_ `shouldBe` [ResolvedGame 1 (Just (fixtureGameId 1))]

  it "a failure locking the FIRST canonically-ordered game cannot produce a success-shaped result, and no swap is performed" do
    let state_ =
          fixtureTestState
            (Map.fromList [(1, Just (fixtureGameId 1)), (2, Just (fixtureGameId 2))])
            (Map.fromList [(fixtureGameId 1, True), (fixtureGameId 2, True)])
        (result, log_) = run (FailAtLock 0) state_ 1 2
    result `shouldSatisfy` isLeft
    log_ `shouldBe` [ResolvedGame 1 (Just (fixtureGameId 1)), ResolvedGame 2 (Just (fixtureGameId 2))]

  it "a failure locking the SECOND canonically-ordered game proves the first was genuinely locked first, and no swap is performed" do
    let state_ =
          fixtureTestState
            (Map.fromList [(1, Just (fixtureGameId 1)), (2, Just (fixtureGameId 2))])
            (Map.fromList [(fixtureGameId 1, True), (fixtureGameId 2, True)])
        (result, log_) = run (FailAtLock 1) state_ 1 2
    result `shouldSatisfy` isLeft
    log_
      `shouldBe` [ ResolvedGame 1 (Just (fixtureGameId 1))
                 , ResolvedGame 2 (Just (fixtureGameId 2))
                 , LockedGame (fixtureGameId 1) True
                 ]

  it "a failure inside performMainStreetSwap cannot produce a success-shaped result, even though both games were already locked" do
    let state_ =
          fixtureTestState
            (Map.fromList [(1, Just (fixtureGameId 1)), (2, Just (fixtureGameId 2))])
            (Map.fromList [(fixtureGameId 1, True), (fixtureGameId 2, True)])
        (result, log_) = run FailAtPerform state_ 1 2
    result `shouldSatisfy` isLeft
    log_
      `shouldBe` [ ResolvedGame 1 (Just (fixtureGameId 1))
                 , ResolvedGame 2 (Just (fixtureGameId 2))
                 , LockedGame (fixtureGameId 1) True
                 , LockedGame (fixtureGameId 2) True
                 ]

  it "performMainStreetSwap reporting invalid/stale locked state is a TYPED outcome (not an exception): both locks are still taken, the swap is attempted exactly once, and the result is a successful Right carrying MainStreetSwapInvalidState -- never a success-shaped MainStreetSwapCompleted" do
    let state_ =
          fixtureTestState
            (Map.fromList [(1, Just (fixtureGameId 1)), (2, Just (fixtureGameId 2))])
            (Map.fromList [(fixtureGameId 1, True), (fixtureGameId 2, True)])
        (result, log_) = run (InvalidStateAtPerform MainStreetSwapInvestigatorMoved) state_ 1 2
        expectedPlan = mainStreetSwapPlan (fixtureGameId 1) (fixtureGameId 2)
    result `shouldBe` Right (MainStreetSwapInvalidState MainStreetSwapInvestigatorMoved)
    log_
      `shouldBe` [ ResolvedGame 1 (Just (fixtureGameId 1))
                 , ResolvedGame 2 (Just (fixtureGameId 2))
                 , LockedGame (fixtureGameId 1) True
                 , LockedGame (fixtureGameId 2) True
                 , PerformedSwap expectedPlan
                 ]

  it "every distinct MainStreetSwapStateFailure reason survives unchanged through planAndExecuteMainStreetSwap, proving the outcome is not collapsed to a single generic failure" do
    let state_ =
          fixtureTestState
            (Map.fromList [(1, Just (fixtureGameId 1)), (2, Just (fixtureGameId 2))])
            (Map.fromList [(fixtureGameId 1, True), (fixtureGameId 2, True)])
        reasons =
          [ MainStreetSwapNotReady
          , MainStreetSwapNoLocation
          , MainStreetSwapDuplicateInvestigator
          , MainStreetSwapInvestigatorMissing
          , MainStreetSwapInvestigatorMoved
          ]
    for_ reasons \reason -> do
      let (result, _) = run (InvalidStateAtPerform reason) state_ 1 2
      result `shouldBe` Right (MainStreetSwapInvalidState reason)

-- | Mirrors "Arkham.Api.Events.EventDeletionSpec" \'s equivalent block for
-- 'eventDeletionCleanupGameIds': proves the room-cleanup\/readiness-spend
-- gating decision directly, without a live server, for EVERY outcome
-- constructor -- including the new 'MainStreetSwapInvalidState'.
mainStreetSwapCleanupPlanSpec :: Spec
mainStreetSwapCleanupPlanSpec = describe "mainStreetSwapCleanupPlan (post-commit action decision seam)" do
  it "MainStreetSwapMissing authorizes no post-commit action" do
    mainStreetSwapCleanupPlan MainStreetSwapMissing `shouldBe` Nothing

  it "MainStreetSwapSameGame authorizes no post-commit action" do
    mainStreetSwapCleanupPlan MainStreetSwapSameGame `shouldBe` Nothing

  it "MainStreetSwapInvalidState authorizes no post-commit action, for every distinct reason" do
    for_
      [ MainStreetSwapNotReady
      , MainStreetSwapNoLocation
      , MainStreetSwapDuplicateInvestigator
      , MainStreetSwapInvestigatorMissing
      , MainStreetSwapInvestigatorMoved
      ]
      \reason -> mainStreetSwapCleanupPlan (MainStreetSwapInvalidState reason) `shouldBe` Nothing

  it "MainStreetSwapCompleted authorizes post-commit action against exactly its own plan" do
    let plan = mainStreetSwapPlan (fixtureGameId 1) (fixtureGameId 2)
    mainStreetSwapCleanupPlan (MainStreetSwapCompleted plan) `shouldBe` Just plan

{- | Directly exercises the production-used pure precondition helpers
'resolveSwapParticipants', 'resolveSwapInvestigator', 'validateSwapPlacement',
and 'mainStreetLocation' -- the exact functions 'swapInvestigatorState'
composes, using REAL 'Investigator'\/'Entities' values (built with the same
'lookupInvestigator'\/'lookupLocation' production itself uses), covering
every distinct 'MainStreetSwapStateFailure' cause: an absent ready
investigator on either side, both sides naming the same investigator, an
investigator id missing from its game's own entities, no Main Street
location in play, and an investigator whose current placement no longer
matches the expected destination.
-}
swapPreconditionHelpersSpec :: Spec
swapPreconditionHelpersSpec = describe "swap precondition helpers (production-used, no partial functions)" do
  describe "resolveSwapParticipants" do
    it "both sides ready and resolving to distinct investigators succeeds" do
      resolveSwapParticipants (Just rolandId) (Just daisyId) `shouldBe` Right (rolandId, daisyId)

    it "an absent FIRST side (group never activated Main Street, or its metadata is missing) reports NotReady" do
      resolveSwapParticipants Nothing (Just daisyId) `shouldBe` Left MainStreetSwapNotReady

    it "an absent SECOND side reports NotReady" do
      resolveSwapParticipants (Just rolandId) Nothing `shouldBe` Left MainStreetSwapNotReady

    it "both sides absent reports NotReady" do
      resolveSwapParticipants Nothing Nothing `shouldBe` Left MainStreetSwapNotReady

    it "both sides resolving to the SAME investigator id -- across two different games, which the schema permits -- reports DuplicateInvestigator, not a successful pair" do
      resolveSwapParticipants (Just rolandId) (Just rolandId) `shouldBe` Left MainStreetSwapDuplicateInvestigator

  describe "resolveSwapInvestigator" do
    it "an investigator id present in its game's entities resolves to that investigator" do
      let investigator = fixtureInvestigator rolandId (fixturePlayerId 1) (fixtureLocationId 1)
          entities = fixtureEntitiesWithInvestigator rolandId investigator
      resolveSwapInvestigator rolandId entities `shouldBe` Right investigator

    it "an investigator id absent from its game's entities (stale or malformed readiness metadata) reports InvestigatorMissing" do
      resolveSwapInvestigator rolandId mempty `shouldBe` Left MainStreetSwapInvestigatorMissing

    it "an investigator id present only under a DIFFERENT id reports InvestigatorMissing, never the other investigator" do
      let investigator = fixtureInvestigator daisyId (fixturePlayerId 1) (fixtureLocationId 1)
          entities = fixtureEntitiesWithInvestigator daisyId investigator
      resolveSwapInvestigator rolandId entities `shouldBe` Left MainStreetSwapInvestigatorMissing

  describe "validateSwapPlacement" do
    it "an investigator still at the expected destination validates successfully" do
      let destination = fixtureLocationId 1
          investigator = fixtureInvestigator rolandId (fixturePlayerId 1) destination
      validateSwapPlacement destination investigator `shouldBe` Right ()

    it "an investigator no longer at the expected destination (moved, or a stale race) reports InvestigatorMoved" do
      let destination = fixtureLocationId 1
          investigator = fixtureInvestigator rolandId (fixturePlayerId 1) (fixtureLocationId 2)
      validateSwapPlacement destination investigator `shouldBe` Left MainStreetSwapInvestigatorMoved

  describe "mainStreetLocation" do
    it "resolves the Main Street location (card 89006) when one is in play" do
      let lid = fixtureLocationId 1
      mainStreetLocation (fixtureEntitiesWithMainStreet lid) `shouldBe` Just lid

    it "reports Nothing when no Main Street location is in play" do
      mainStreetLocation mempty `shouldBe` Nothing
