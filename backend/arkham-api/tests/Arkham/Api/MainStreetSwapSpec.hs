{- | Proves the Main Street group swap decision sequencing described in
'Api.Handler.Arkham.Games.Shared.planAndExecuteMainStreetSwap' without a live
database, mirroring "Arkham.Api.Events.EventDeletionSpec" 's approach for
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
  canonical @(ordinal, game id)@ order (see
  "Arkham.Api.EpicGameLockOrderSpec" and "Arkham.Api.MainStreetSwapPlanSpec"
  for the pure ordering function itself), then calls 'performMainStreetSwap'
  exactly once, and reports 'MainStreetSwapCompleted' carrying a plan whose
  'firstGameId'\/'secondGameId' are unchanged from the request regardless of
  lock order;
* a failure injected at either 'resolveSwapGame' call, either 'lockSwapGame'
  call, or 'performMainStreetSwap' itself can never produce a
  success-shaped ('MainStreetSwapCompleted', or any other) result, and no
  step after the injected failure is attempted.

As with "Arkham.Api.Events.EventDeletionSpec", this pure interpreter's step
log is evidence of the deterministic order operations were attempted in and
where a sequence short-circuited -- it is not a simulation of transactional
rollback. Actual rollback of a partially-applied write on a real injected
failure is a property of 'runDB' (an uncaught exception aborts the whole
transaction), not of this test.
-}
module Arkham.Api.MainStreetSwapSpec (spec) where

import Api.Handler.Arkham.Games.Shared
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
first.
-}
data FailAt
  = FailNever
  | FailAtResolve Int
  | FailAtLock Int
  | FailAtPerform
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
    pure present

  performMainStreetSwap plan = do
    failIfConfigured FailAtPerform
    recordStep (PerformedSwap plan)

-- Specs -------------------------------------------------------------------------

spec :: Spec
spec = describe "planAndExecuteMainStreetSwap (Main Street swap decision sequencing)" do
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
    -- Canonical lock order is ascending by ordinal here (1 then 2), so game
    -- 1's absent lock is attempted first, but game 2's is STILL attempted
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
    -- Ordinal 0 < ordinal 1, so canonical order locks game 0 first. Game 0
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
        expectedPlan = mainStreetSwapPlan (1, fixtureGameId 1) (2, fixtureGameId 2)
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
        expectedPlan = mainStreetSwapPlan (2, fixtureGameId 2) (1, fixtureGameId 1)
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
