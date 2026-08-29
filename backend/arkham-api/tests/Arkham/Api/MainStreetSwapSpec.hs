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
import Arkham.Classes.Entity (attr, overAttrs)
import Arkham.Difficulty (Difficulty (Standard))
import Arkham.Entities (Entities (..))
import Arkham.Game (Game (..), newCampaign, newScenario, setInitialScenarioMeta)
import Arkham.Id (InvestigatorId (..), LocationId (..), PlayerId (..))
import Arkham.Investigator (lookupInvestigator)
import Arkham.Investigator.Types (Investigator, investigatorPlacement, investigatorPlayerId)
import Arkham.Location (lookupLocation)
import Arkham.Placement (Placement (AtLocation))
import Arkham.Prelude
import Control.Monad.Except (ExceptT, MonadError, runExceptT, throwError)
import Control.Monad.State.Strict (MonadState, State, gets, modify, runState)
import Data.Either (isLeft)
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Data.UUID qualified as UUID
import Database.Persist.Sql (toSqlKey)
import Entity.Arkham.Epic qualified as Epic
import Entity.Arkham.Game qualified as GameEntity
import Entity.Arkham.Player (ArkhamPlayer (..))
import Entity.User qualified as User
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

{- | Insert one EXTRA 'Investigator' entry into a 'Game''s
'Arkham.Entities.entitiesInvestigators', under the given key -- used ONLY to
build a deliberately\/artificially INCONSISTENT map (see
'validateEntityMapIdentity'\/'MainStreetSwapInvalidEntityMap' below), by
passing a @key@ that does NOT equal @investigator.id@. Every other fixture
helper in this module (e.g. 'fixtureEntitiesWithInvestigator', 'buildGame')
always keys an investigator under its own id, so this is the one place a
mismatch can be constructed at all.
-}
withInvestigatorEntry :: InvestigatorId -> Investigator -> Game -> Game
withInvestigatorEntry key investigator game =
  game
    { gameEntities =
        (gameEntities game)
          { entitiesInvestigators = Map.insert key investigator (entitiesInvestigators (gameEntities game))
          }
    }

{- | Insert one EXTRA location entry (via the same 'lookupLocation' production
uses) into a 'Game''s 'Arkham.Entities.entitiesLocations', under the given
key -- the location analogue of 'withInvestigatorEntry' above, used ONLY to
build a deliberately inconsistent map.
-}
withLocationEntry :: LocationId -> LocationId -> Game -> Game
withLocationEntry key locationOwnId game =
  game
    { gameEntities =
        (gameEntities game)
          { entitiesLocations =
              Map.insert key (lookupLocation (CardCode "89006") locationOwnId nullCardId) (entitiesLocations (gameEntities game))
          }
    }

{- | Populate the given base 'Game' (already in whichever 'gameMode' the
caller needs) with exactly the given Main Street location and investigators,
with 'gamePlayerOrder'\/'gamePlayers' populated consistently -- unlike
'fixtureArkhamGame' below (whose content is irrelevant to the sequencing
spec that uses it), this is REAL state 'swapInvestigatorState'\/
'computeMainStreetSwap' -- the actual production transform, not a
stand-in -- can genuinely operate on end to end.
-}
buildGame :: Game -> LocationId -> [(InvestigatorId, Investigator)] -> Game
buildGame base mainStreetId investigators =
  base
    { gameEntities =
        mconcat
          $ fixtureEntitiesWithMainStreet mainStreetId
          : [fixtureEntitiesWithInvestigator iid investigator | (iid, investigator) <- investigators]
    , gamePlayerOrder = map fst investigators
    , gamePlayers = map (attr investigatorPlayerId . snd) investigators
    }

-- | A campaign-mode (via 'newCampaign') 'Game' -- sufficient for the pure
-- membership\/collision helpers ('validateSwapSide', 'validateSwapMembership')
-- below, which never consult 'gameMode' at all.
fixtureGameWith :: LocationId -> [(InvestigatorId, Investigator)] -> Game
fixtureGameWith mainStreetId investigators =
  buildGame (newCampaign "06" Nothing 0 (length investigators) Standard False) mainStreetId investigators

-- | The common case of 'fixtureGameWith': exactly one investigator.
fixtureGame :: LocationId -> InvestigatorId -> Investigator -> Game
fixtureGame mainStreetId iid investigator = fixtureGameWith mainStreetId [(iid, investigator)]

{- | A SCENARIO-mode (via 'newScenario') 'Game' with the given investigator
recorded as the "mainStreetReady" scenario meta key (see
'setInitialScenarioMeta') -- needed for 'readyInvestigator' to resolve at
all: campaign-only ('This') mode, which 'fixtureGameWith' above produces,
always reports 'Nothing' there (see 'readyInvestigator'). Used by
"swapInvestigatorState / computeMainStreetSwap (production transform, end
to end)" below, the only spec block that exercises 'readyInvestigator'
through a full 'swapInvestigatorState' call.
-}
fixtureReadyGameWith :: LocationId -> InvestigatorId -> [(InvestigatorId, Investigator)] -> Game
fixtureReadyGameWith mainStreetId readyIid investigators =
  setInitialScenarioMeta "mainStreetReady" readyIid
    $ buildGame (newScenario "01104" 0 (length investigators) Standard False) mainStreetId investigators

-- | The common case of 'fixtureReadyGameWith': exactly one (ready) investigator.
fixtureReadyGame :: LocationId -> InvestigatorId -> Investigator -> Game
fixtureReadyGame mainStreetId iid investigator = fixtureReadyGameWith mainStreetId iid [(iid, investigator)]

-- | Force full evaluation of a 'Game' (via its derived 'Show' instance,
-- which must recursively evaluate every field to render it) so an
-- end-to-end assertion genuinely proves the production transform produced
-- usable, non-bottom state -- not merely that some lazy thunk type-checked.
forceGame :: Game -> Expectation
forceGame g = length (show g) `shouldSatisfy` (> 0)

fixtureUserId :: Int -> User.UserId
fixtureUserId = toSqlKey . fromIntegral

{- | A synthetic 'Entity.Arkham.Player.ArkhamPlayer' row for one swap
participant -- shaped exactly like what 'lockSwapPlayer' returns in
production (see 'validateSwapPlayer'). Takes an explicit 'User.UserId' (see
'MainStreetSwapSameUser'\/'MainStreetSwapDestinationOccupied': a swap's two
participants MUST be distinct users for a "valid" fixture to be realistic
-- callers use DISTINCT ids for the two sides of an authorized swap, and
only deliberately share one to build the SAME-user rejection fixture).
-}
fixtureArkhamPlayer :: User.UserId -> GameEntity.ArkhamGameId -> InvestigatorId -> ArkhamPlayer
fixtureArkhamPlayer userId gid iid =
  ArkhamPlayer
    { arkhamPlayerUserId = userId
    , arkhamPlayerArkhamGameId = gid
    , arkhamPlayerInvestigatorId = coerce iid
    }

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
  | -- | one participant's 'Entity.Arkham.Player.ArkhamPlayer' row was locked
    -- ('FOR UPDATE'), and whether it was still present
    LockedPlayer PlayerId Bool
  | -- | one destination game was probed for a pre-existing occupant of the
    -- given user, and whether one was found
    CheckedDestinationOccupant GameEntity.ArkhamGameId User.UserId Bool
  | -- | the swap itself was performed, for this plan
    PerformedSwap MainStreetSwapPlan
  deriving stock (Eq, Show)

{- | Which step (if any) should fail. The 'Int' on 'FailAtResolve', 'FailAtLock',
and 'FailAtLockPlayer' selects WHICH occurrence (1-based for resolves,
matching the two 'resolveSwapGame' calls; 0-based, in canonical lock order,
for 'FailAtLock'\/'FailAtLockPlayer') should fail, so a test can target
specifically the second resolve or specifically the second canonical lock
without also matching the first. 'InvalidStateAtPerform' is NOT a hard
failure (it never 'throwError's): it models 'performMainStreetSwap' itself
returning a typed 'Left' -- exactly what stale\/malformed persisted state
now produces in production -- so a test can assert
'planAndExecuteMainStreetSwap' reports the corresponding
'MainStreetSwapInvalidState' as an ordinary, successful 'Right' result,
distinct from 'FailAtPerform' (a genuine aborting exception).
-}
data FailAt
  = FailNever
  | FailAtResolve Int
  | FailAtLock Int
  | FailAtLockPlayer Int
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

'playerRows' models a participant's 'Entity.Arkham.Player.ArkhamPlayer'
row -- keyed by 'PlayerId', mirroring how 'lockSwapPlayer' looks one up in
production -- an absent key (or one mapped to 'Nothing') is "no such row, or
it vanished concurrently."

'destinationOccupants' models a PRE-EXISTING, unrelated
'Entity.Arkham.Player.ArkhamPlayer' row occupying @(gameId, userId)@ --
keyed exactly like 'lookupSwapDestinationOccupant' looks one up in
production -- a present @(gameId, userId)@ key mapped to 'True' is "yes,
some OTHER row already occupies this destination for this user."
-}
data TestState = TestState
  { steps :: [Step]
  , relations :: Map Int (Maybe GameEntity.ArkhamGameId)
  , gamePresence :: Map GameEntity.ArkhamGameId Bool
  , playerRows :: Map PlayerId (Maybe ArkhamPlayer)
  , destinationOccupants :: Map (GameEntity.ArkhamGameId, User.UserId) Bool
  , resolveCallCount :: Int
  , lockCallCount :: Int
  , playerLockCallCount :: Int
  }

-- | The common case: both requested ordinals resolve to distinct, present
-- games, zero steps recorded yet, no player rows configured (only needed by
-- the 'lockAndValidateSwapPlayers' persistence-seam tests below).
fixtureTestState :: Map Int (Maybe GameEntity.ArkhamGameId) -> Map GameEntity.ArkhamGameId Bool -> TestState
fixtureTestState relations gamePresence =
  TestState
    { steps = []
    , relations
    , gamePresence
    , playerRows = mempty
    , destinationOccupants = mempty
    , resolveCallCount = 0
    , lockCallCount = 0
    , playerLockCallCount = 0
    }


-- | State for the 'lockAndValidateSwapPlayers' persistence-seam tests below:
-- only 'playerRows' and 'destinationOccupants' are relevant, since that
-- function never calls 'resolveSwapGame', 'lockSwapGame', or
-- 'performMainStreetSwap'.
fixturePlayerTestState
  :: Map PlayerId (Maybe ArkhamPlayer) -> Map (GameEntity.ArkhamGameId, User.UserId) Bool -> TestState
fixturePlayerTestState playerRows destinationOccupants =
  TestState
    { steps = []
    , relations = mempty
    , gamePresence = mempty
    , playerRows
    , destinationOccupants
    , resolveCallCount = 0
    , lockCallCount = 0
    , playerLockCallCount = 0
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

  lockSwapPlayer pid = do
    occurrence <- gets (.playerLockCallCount)
    failIfConfigured (FailAtLockPlayer occurrence)
    modify \s -> s {playerLockCallCount = occurrence + 1}
    mPlayer <- gets (Map.findWithDefault Nothing pid . (.playerRows))
    recordStep (LockedPlayer pid (isJust mPlayer))
    pure mPlayer

  lookupSwapDestinationOccupant gid userId _excludedPid = do
    occupied <- gets (Map.findWithDefault False (gid, userId) . (.destinationOccupants))
    recordStep (CheckedDestinationOccupant gid userId occupied)
    pure occupied

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
  swapMembershipHelpersSpec
  validateSwapPlayerSpec
  validateEntityMapIdentitySpec
  swapInvestigatorStateEndToEndSpec
  lockAndValidateSwapPlayersSpec

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

    it "a map key that does not match the resolved investigator's own internal id (a stale/corrupt keying, however it could arise) reports InvestigatorKeyMismatch, not a successful resolution under the wrong identity" do
      -- Genuinely construct an investigator whose OWN internal id is daisyId,
      -- then store it under the UNRELATED key rolandId: the map lookup by
      -- rolandId succeeds, but the resolved investigator's own id disagrees.
      let mismatched = fixtureInvestigator daisyId (fixturePlayerId 1) (fixtureLocationId 1)
          entities = fixtureEntitiesWithInvestigator rolandId mismatched
      resolveSwapInvestigator rolandId entities `shouldBe` Left MainStreetSwapInvestigatorKeyMismatch

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

{- | Unit tests for the pure membership\/collision-safety helpers
'rejectDuplicatePlayers' and 'validateSwapSide'\/'validateSwapMembership' --
the checks that guard 'computeMainStreetSwap''s internal
'Map.insert'\/list-append from silently overwriting or duplicating an
unrelated investigator. See "swap precondition helpers" above for the
OLDER preconditions (readiness, location, entity resolution, placement)
these compose alongside.
-}
swapMembershipHelpersSpec :: Spec
swapMembershipHelpersSpec = describe "swap membership/collision-safety helpers (production-used)" do
  describe "rejectDuplicatePlayers" do
    it "two distinct player ids succeeds" do
      rejectDuplicatePlayers (fixturePlayerId 1) (fixturePlayerId 2) `shouldBe` Right ()

    it "the SAME player id on both sides reports DuplicatePlayer" do
      rejectDuplicatePlayers (fixturePlayerId 1) (fixturePlayerId 1) `shouldBe` Left MainStreetSwapDuplicatePlayer

  describe "validateSwapSide" do
    let locA = fixtureLocationId 1
        pid1 = fixturePlayerId 1
        pid2 = fixturePlayerId 2
        rolandAtA = fixtureInvestigator rolandId pid1 locA
        gameWithRoland = fixtureGame locA rolandId rolandAtA

    it "an outgoing participant genuinely, singularly present, with no incoming collision, validates successfully" do
      validateSwapSide rolandId pid1 daisyId pid2 gameWithRoland `shouldBe` Right ()

    it "an outgoing participant missing from the game's own entities reports ParticipantInconsistent" do
      let broken = gameWithRoland {gameEntities = fixtureEntitiesWithMainStreet locA}
      validateSwapSide rolandId pid1 daisyId pid2 broken `shouldBe` Left MainStreetSwapParticipantInconsistent

    it "an outgoing participant missing from gamePlayerOrder (present in entities) reports ParticipantInconsistent" do
      let broken = gameWithRoland {gamePlayerOrder = []}
      validateSwapSide rolandId pid1 daisyId pid2 broken `shouldBe` Left MainStreetSwapParticipantInconsistent

    it "an outgoing participant missing from gamePlayers (present in entities/order) reports ParticipantInconsistent" do
      let broken = gameWithRoland {gamePlayers = []}
      validateSwapSide rolandId pid1 daisyId pid2 broken `shouldBe` Left MainStreetSwapParticipantInconsistent

    it "an incoming investigator id already present at the destination (after accounting for the outgoing participant) reports IncomingCollision" do
      -- Roland (outgoing) shares no id with the incoming investigator here,
      -- so the collision must come from a THIRD investigator already
      -- resident in this game under the SAME id as the incoming one.
      let thirdAsIncoming =
            fixtureGameWith locA [(rolandId, rolandAtA), (daisyId, fixtureInvestigator daisyId (fixturePlayerId 3) locA)]
      validateSwapSide rolandId pid1 daisyId pid2 thirdAsIncoming `shouldBe` Left MainStreetSwapIncomingCollision

    it "an incoming player id already present at the destination reports IncomingCollision even when the incoming investigator id itself is absent" do
      let skidsId = InvestigatorId (CardCode "01003")
          thirdPlayer = fixtureGameWith locA [(rolandId, rolandAtA), (skidsId, fixtureInvestigator skidsId pid2 locA)]
      validateSwapSide rolandId pid1 daisyId pid2 thirdPlayer `shouldBe` Left MainStreetSwapIncomingCollision

  describe "validateSwapMembership" do
    let locA = fixtureLocationId 1
        locB = fixtureLocationId 2
        pid1 = fixturePlayerId 1
        pid2 = fixturePlayerId 2
        game1 = fixtureGame locA rolandId (fixtureInvestigator rolandId pid1 locA)
        game2 = fixtureGame locB daisyId (fixtureInvestigator daisyId pid2 locB)

    it "two consistent, non-colliding games validate successfully" do
      validateSwapMembership rolandId pid1 game1 daisyId pid2 game2 `shouldBe` Right ()

    it "a collision on the SECOND game is reported, not silently ignored because the first side is fine" do
      let collidingGame2 =
            fixtureGameWith
              locB
              [ (daisyId, fixtureInvestigator daisyId pid2 locB)
              , (rolandId, fixtureInvestigator rolandId (fixturePlayerId 3) (fixtureLocationId 99))
              ]
      validateSwapMembership rolandId pid1 game1 daisyId pid2 collidingGame2 `shouldBe` Left MainStreetSwapIncomingCollision

    it "a collision on the FIRST game is reported symmetrically" do
      let collidingGame1 =
            fixtureGameWith
              locA
              [ (rolandId, fixtureInvestigator rolandId pid1 locA)
              , (daisyId, fixtureInvestigator daisyId (fixturePlayerId 3) (fixtureLocationId 99))
              ]
      validateSwapMembership rolandId pid1 collidingGame1 daisyId pid2 game2 `shouldBe` Left MainStreetSwapIncomingCollision

{- | Unit tests for 'validateSwapPlayer' -- the cross-check between a
LOCKED 'Entity.Arkham.Player.ArkhamPlayer' row and the 'MainStreetSwapTransform'
this swap already validated purely from the two games' own state.
-}
validateSwapPlayerSpec :: Spec
validateSwapPlayerSpec = describe "validateSwapPlayer (locked ArkhamPlayer row cross-check, production-used)" do
  let gid = fixtureGameId 1
      otherGid = fixtureGameId 2

  it "a present row matching both the expected source game and investigator validates successfully" do
    let player = fixtureArkhamPlayer (fixtureUserId 1) gid rolandId
    case validateSwapPlayer gid rolandId (Just player) of
      Right validated -> do
        arkhamPlayerArkhamGameId validated `shouldBe` gid
        arkhamPlayerInvestigatorId validated `shouldBe` coerce rolandId
      Left failure -> expectationFailure $ "expected success, got: " <> show failure

  it "an absent locked row (vanished concurrently, or never existed) reports PlayerMissing" do
    case validateSwapPlayer gid rolandId Nothing of
      Left MainStreetSwapPlayerMissing -> pure ()
      other -> expectationFailure $ "expected PlayerMissing, got: " <> show other

  it "a present row linked to a DIFFERENT game than expected reports PlayerWrongGame" do
    let player = fixtureArkhamPlayer (fixtureUserId 1) otherGid rolandId
    case validateSwapPlayer gid rolandId (Just player) of
      Left MainStreetSwapPlayerWrongGame -> pure ()
      other -> expectationFailure $ "expected PlayerWrongGame, got: " <> show other

  it "a present row in the right game but recorded under a DIFFERENT investigator reports PlayerMismatch" do
    let player = fixtureArkhamPlayer (fixtureUserId 1) gid daisyId
    case validateSwapPlayer gid rolandId (Just player) of
      Left MainStreetSwapPlayerMismatch -> pure ()
      other -> expectationFailure $ "expected PlayerMismatch, got: " <> show other

{- | Executes the ACTUAL production 'swapInvestigatorState'\/'computeMainStreetSwap'
transform end to end against REAL 'Game' fixtures (built via
'fixtureReadyGame'\/'fixtureReadyGameWith', with genuinely populated
entities, player order, players lists, and scenario "mainStreetReady" meta
-- not the placeholder 'fixtureArkhamGame' the sequencing spec above uses,
whose content is irrelevant there). This is the exact function
'performMainStreetSwap' calls in production before ever locking a player
row: fully forcing both resulting games (see 'forceGame') proves the
transform genuinely produces usable, non-bottom state, not merely that
'swapInvestigatorState' type-checks. A mutation check (temporarily
swapping 'firstDestination'\/'secondDestination' back to their prior,
incorrect assignment in 'computeMainStreetSwap') was performed manually
while writing this spec block: it reproducibly failed the "authorized
swap" test below with the arriving investigator landing at the WRONG
game's Main Street location, which is exactly how this end-to-end test
caught and drove the fix of a genuine pre-existing placement bug (the
arriving investigator was placed back at their OWN former game's Main
Street location instead of the OTHER game's); the corrected definition is
restored and verified in this committed code.
-}
{- | Directly exercises the production-used 'entityMapKeysMatchIds' and
'validateEntityMapIdentity' -- the GLOBAL map-key\/internal-id identity
check 'swapInvestigatorState' runs, on BOTH games' 'entitiesInvestigators'
AND 'entitiesLocations', strictly BEFORE any participant resolution,
readiness, placement, or membership check (see 'swapInvestigatorStateEndToEndSpec'
below for that end-to-end ordering proof). This block proves the pure
helpers themselves are total and correctly discriminate consistent from
inconsistent maps, including for entries that are NOT swap participants at
all -- the exact gap the historical, participant-only
'MainStreetSwapInvestigatorKeyMismatch' check could not close.
-}
validateEntityMapIdentitySpec :: Spec
validateEntityMapIdentitySpec = describe "validateEntityMapIdentity / entityMapKeysMatchIds (production-used global map-key/internal-id identity check)" do
  describe "entityMapKeysMatchIds" do
    it "an empty map is trivially consistent" do
      entityMapKeysMatchIds (.id) (Map.empty :: Map InvestigatorId Investigator) `shouldBe` True

    it "a map whose every key matches its value's own internal id is consistent" do
      let investigator = fixtureInvestigator rolandId (fixturePlayerId 1) (fixtureLocationId 1)
      entityMapKeysMatchIds (.id) (Map.singleton rolandId investigator) `shouldBe` True

    it "a map with even ONE entry whose key disagrees with its value's own internal id is inconsistent" do
      let mismatched = fixtureInvestigator daisyId (fixturePlayerId 1) (fixtureLocationId 1) -- internal id daisyId
      entityMapKeysMatchIds (.id) (Map.singleton rolandId mismatched) `shouldBe` False

  describe "validateEntityMapIdentity" do
    it "a fully consistent game (investigators and locations both keyed by their own internal ids) passes" do
      let locA = fixtureLocationId 1
          game = fixtureReadyGame locA rolandId (fixtureInvestigator rolandId (fixturePlayerId 1) locA)
      validateEntityMapIdentity game `shouldBe` Right ()

    it "an investigator map with a mismatched key -- even for a NONPARTICIPANT entry unrelated to any ready investigator -- reports MainStreetSwapInvalidEntityMap" do
      let locA = fixtureLocationId 1
          skidsId = InvestigatorId (CardCode "01003")
          -- skidsId is stored as the KEY, but the stored value's own
          -- internal id is actually daisyId: a stale/corrupt nonparticipant
          -- entry, unrelated to Roland (the ready investigator here).
          nonparticipant = fixtureInvestigator daisyId (fixturePlayerId 2) locA
          game =
            withInvestigatorEntry skidsId nonparticipant
              $ fixtureReadyGame locA rolandId (fixtureInvestigator rolandId (fixturePlayerId 1) locA)
      validateEntityMapIdentity game `shouldBe` Left MainStreetSwapInvalidEntityMap

    it "a location map with a mismatched key -- for a location no participant is even standing at -- reports MainStreetSwapInvalidEntityMap" do
      let locA = fixtureLocationId 1
          bystanderKey = fixtureLocationId 98
          bystanderOwnId = fixtureLocationId 99
          game =
            withLocationEntry bystanderKey bystanderOwnId
              $ fixtureReadyGame locA rolandId (fixtureInvestigator rolandId (fixturePlayerId 1) locA)
      validateEntityMapIdentity game `shouldBe` Left MainStreetSwapInvalidEntityMap

swapInvestigatorStateEndToEndSpec :: Spec
swapInvestigatorStateEndToEndSpec =
  describe "swapInvestigatorState / computeMainStreetSwap (production transform, end to end)" do
    let locA = fixtureLocationId 1
        locB = fixtureLocationId 2
        pid1 = fixturePlayerId 1
        pid2 = fixturePlayerId 2
        rolandAtA = fixtureInvestigator rolandId pid1 locA
        daisyAtB = fixtureInvestigator daisyId pid2 locB
        game1 = fixtureReadyGame locA rolandId rolandAtA
        game2 = fixtureReadyGame locB daisyId daisyAtB

    it "an authorized swap transfers both investigators, updates player order/players, and moves each to the OTHER game's Main Street location" do
      case swapInvestigatorState game1 game2 of
        Left failure -> expectationFailure $ "expected a successful transform, got: " <> show failure
        Right transform -> do
          transform.firstIid `shouldBe` rolandId
          transform.secondIid `shouldBe` daisyId
          transform.firstPid `shouldBe` pid1
          transform.secondPid `shouldBe` pid2
          forceGame transform.firstGame'
          forceGame transform.secondGame'
          -- Roland leaves game1's investigators/order/players and arrives in game2's.
          Map.member rolandId (entitiesInvestigators (gameEntities transform.firstGame')) `shouldBe` False
          Map.member rolandId (entitiesInvestigators (gameEntities transform.secondGame')) `shouldBe` True
          gamePlayerOrder transform.firstGame' `shouldBe` [daisyId]
          gamePlayerOrder transform.secondGame' `shouldBe` [rolandId]
          gamePlayers transform.firstGame' `shouldBe` [pid2]
          gamePlayers transform.secondGame' `shouldBe` [pid1]
          -- Each arriving investigator is placed at the OTHER game's own
          -- Main Street location.
          let arrivedRoland = Map.lookup rolandId (entitiesInvestigators (gameEntities transform.secondGame'))
              arrivedDaisy = Map.lookup daisyId (entitiesInvestigators (gameEntities transform.firstGame'))
          (attr investigatorPlacement <$> arrivedRoland) `shouldBe` Just (AtLocation locB)
          (attr investigatorPlacement <$> arrivedDaisy) `shouldBe` Just (AtLocation locA)
          -- Every remaining investigator's map key exactly matches its own
          -- internal id, and gamePlayerOrder contains exactly those same
          -- ids -- proving the transform never re-keys under a caller-
          -- supplied id (see 'MainStreetSwapInvestigatorKeyMismatch').
          let keysMatchOwnIds entities =
                all (\(k, v) -> v.id == k) (Map.toList (entitiesInvestigators entities))
          keysMatchOwnIds (gameEntities transform.firstGame') `shouldBe` True
          keysMatchOwnIds (gameEntities transform.secondGame') `shouldBe` True
          Map.keysSet (entitiesInvestigators (gameEntities transform.firstGame'))
            `shouldBe` Set.fromList (gamePlayerOrder transform.firstGame')
          Map.keysSet (entitiesInvestigators (gameEntities transform.secondGame'))
            `shouldBe` Set.fromList (gamePlayerOrder transform.secondGame')

    it "the reported transform's game/player-id fields are exactly computeMainStreetSwap's own (Game, Game, PlayerId, PlayerId), embedded unchanged" do
      case swapInvestigatorState game1 game2 of
        Left failure -> expectationFailure $ "expected success, got: " <> show failure
        Right transform ->
          (transform.firstGame', transform.secondGame', transform.firstPid, transform.secondPid)
            `shouldBe` computeMainStreetSwap rolandId locB rolandAtA game1 daisyId locA daisyAtB game2

    it "duplicate player ids across both sides (a data anomaly) short-circuits BEFORE any membership/collision check, reporting DuplicatePlayer" do
      let daisyAtBSharedPid = fixtureInvestigator daisyId pid1 locB -- same pid as Roland's
          collidingGame2 = fixtureReadyGame locB daisyId daisyAtBSharedPid
      swapInvestigatorState game1 collidingGame2 `shouldBe` Left MainStreetSwapDuplicatePlayer

    it "an incoming collision in the destination game (a third, unrelated investigator sharing the incoming investigator's id) reports IncomingCollision, with no partial swap" do
      let collidingGame2 =
            fixtureReadyGameWith
              locB
              daisyId
              [(daisyId, daisyAtB), (rolandId, fixtureInvestigator rolandId (fixturePlayerId 3) (fixtureLocationId 99))]
      swapInvestigatorState game1 collidingGame2 `shouldBe` Left MainStreetSwapIncomingCollision

    it "a source-game membership inconsistency (investigator present in entities, but absent from gamePlayerOrder) reports ParticipantInconsistent, before any transform" do
      let brokenGame1 = game1 {gamePlayerOrder = []}
      swapInvestigatorState brokenGame1 game2 `shouldBe` Left MainStreetSwapParticipantInconsistent

    it "a stale/corrupt \"mainStreetReady\" key resolving to an investigator recorded under a DIFFERENT internal id is caught by the GLOBAL validateEntityMapIdentity check (reporting MainStreetSwapInvalidEntityMap), before resolveSwapInvestigator's own narrower, participant-only key check ever runs -- even when the destination independently already contains that other internal id, with no partial swap" do
      -- readyIid is rolandId, but the entities map stores it under that key
      -- holding an investigator whose OWN internal id is actually daisyId --
      -- a stale/corrupt keying, however it could arise in persisted state.
      -- This is exactly the shape 'entityMapKeysMatchIds' checks for EVERY
      -- entry, so it is now impossible for such a map to reach
      -- 'resolveSwapInvestigator''s own (still-present, defense-in-depth)
      -- 'MainStreetSwapInvestigatorKeyMismatch' check at all.
      let mismatched = fixtureInvestigator daisyId pid1 locA
          brokenGame1 = fixtureReadyGameWith locA rolandId [(rolandId, mismatched)]
      -- game2 (daisyAtB, above) already independently contains daisyId as
      -- its OWN distinct, unrelated ready investigator.
      swapInvestigatorState brokenGame1 game2 `shouldBe` Left MainStreetSwapInvalidEntityMap

    it "the same stale/corrupt key/id mismatch is caught by MainStreetSwapInvalidEntityMap even when the destination does NOT otherwise contain that internal id" do
      let mismatched = fixtureInvestigator daisyId pid1 locA
          brokenGame1 = fixtureReadyGameWith locA rolandId [(rolandId, mismatched)]
          skidsId = InvestigatorId (CardCode "01003")
          destWithoutDaisy = fixtureReadyGame locB skidsId (fixtureInvestigator skidsId pid2 locB)
      swapInvestigatorState brokenGame1 destWithoutDaisy `shouldBe` Left MainStreetSwapInvalidEntityMap

    it "a NONPARTICIPANT investigator entry in the SOURCE game, stored under a mismatched key, reports MainStreetSwapInvalidEntityMap before any participant is even resolved -- both sides are otherwise entirely valid and would swap successfully" do
      let skidsId = InvestigatorId (CardCode "01003")
          -- Stored under skidsId, but its OWN internal id is daisyId --
          -- daisyId is also game2's genuine ready participant, so this is
          -- exactly the "nonparticipant value with internal id A under key
          -- X, where A collides with something relevant elsewhere"
          -- shape the global check exists to catch.
          bystander = fixtureInvestigator daisyId (fixturePlayerId 3) (fixtureLocationId 98)
          brokenGame1 = withInvestigatorEntry skidsId bystander game1
      swapInvestigatorState brokenGame1 game2 `shouldBe` Left MainStreetSwapInvalidEntityMap

    it "a NONPARTICIPANT investigator entry in the DESTINATION game, stored under a key that disagrees with its own internal id which equals the INCOMING investigator's id, reports MainStreetSwapInvalidEntityMap -- catching a collision 'validateSwapSide's Map.member check (keyed lookup only) cannot see" do
      let skidsId = InvestigatorId (CardCode "01003")
          -- Stored under skidsId in game2, but its OWN internal id is
          -- rolandId -- the very investigator ABOUT to arrive in game2.
          -- Before the global identity check existed, 'validateSwapSide's
          -- 'Map.member rolandId' probe would look up key rolandId, find
          -- nothing (this bystander sits under skidsId), and wrongly permit
          -- 'addOwned' to insert the arriving Roland, silently duplicating
          -- his internal id under two distinct keys.
          bystander = fixtureInvestigator rolandId (fixturePlayerId 4) (fixtureLocationId 98)
          brokenGame2 = withInvestigatorEntry skidsId bystander game2
      swapInvestigatorState game1 brokenGame2 `shouldBe` Left MainStreetSwapInvalidEntityMap

    it "a mismatched LOCATION map key in either game reports MainStreetSwapInvalidEntityMap, even for a location no participant is standing at" do
      let bystanderKey = fixtureLocationId 97
          bystanderOwnId = fixtureLocationId 96
          brokenGame1 = withLocationEntry bystanderKey bystanderOwnId game1
          brokenGame2 = withLocationEntry bystanderKey bystanderOwnId game2
      swapInvestigatorState brokenGame1 game2 `shouldBe` Left MainStreetSwapInvalidEntityMap
      swapInvestigatorState game1 brokenGame2 `shouldBe` Left MainStreetSwapInvalidEntityMap

    it "a valid EXTRA, correctly-keyed nonparticipant investigator/location in either game does not itself block a successful swap (the check rejects inconsistency, not mere extra entries)" do
      let extraId = InvestigatorId (CardCode "01003")
          extra = fixtureInvestigator extraId (fixturePlayerId 5) locA
          extraLocId = fixtureLocationId 95
          gameWithExtras =
            withLocationEntry extraLocId extraLocId $ withInvestigatorEntry extraId extra game1
      case swapInvestigatorState gameWithExtras game2 of
        Left failure -> expectationFailure $ "expected a successful transform, got: " <> show failure
        Right transform -> transform.firstIid `shouldBe` rolandId


fixtureTransform :: InvestigatorId -> PlayerId -> InvestigatorId -> PlayerId -> MainStreetSwapTransform
fixtureTransform firstIid firstPid secondIid secondPid =
  MainStreetSwapTransform
    { firstGame' = newCampaign "06" Nothing 0 1 Standard False
    , secondGame' = newCampaign "06" Nothing 0 1 Standard False
    , firstIid
    , firstPid
    , secondIid
    , secondPid
    }

{- | Exercises the ACTUAL production 'lockAndValidateSwapPlayers' -- the exact
function 'performMainStreetSwap' calls, after both game locks, before any
write -- through the same 'TestDB' pure interpreter used by
'mainStreetSwapSequencingSpec' above (which also implements 'lockSwapPlayer',
see 'fixturePlayerTestState'). Proves: canonical ascending-'PlayerId' lock
order regardless of the transform's own first\/second mapping; BOTH locks
always attempted before either result is inspected (mirroring
'lockSwapGame''s own contract); zero validation performed if either lock is
absent; and each of 'validateSwapPlayer''s three failure reasons is
reachable through this exact seam, not merely as an isolated pure unit.
-}
lockAndValidateSwapPlayersSpec :: Spec
lockAndValidateSwapPlayersSpec =
  describe "lockAndValidateSwapPlayers (game-locks-then-player-locks sequencing, production-used)" do
    let gid1 = fixtureGameId 1
        gid2 = fixtureGameId 2
        pid1 = fixturePlayerId 10
        pid2 = fixturePlayerId 20
        user1 = fixtureUserId 1
        user2 = fixtureUserId 2
        transform = fixtureTransform rolandId pid1 daisyId pid2
        run failAt rows = runTestDB failAt (fixturePlayerTestState rows mempty) (lockAndValidateSwapPlayers gid1 gid2 transform)
        runWithOccupants failAt rows occupants =
          runTestDB failAt (fixturePlayerTestState rows occupants) (lockAndValidateSwapPlayers gid1 gid2 transform)

    it "both participant rows present and valid, DISTINCT users, locks in ascending PlayerId order and validates successfully" do
      let rows =
            Map.fromList
              [(pid1, Just (fixtureArkhamPlayer user1 gid1 rolandId)), (pid2, Just (fixtureArkhamPlayer user2 gid2 daisyId))]
          (result, log_) = run FailNever rows
      case result of
        Right (Right (p1, p2)) -> do
          arkhamPlayerArkhamGameId p1 `shouldBe` gid1
          arkhamPlayerInvestigatorId p1 `shouldBe` coerce rolandId
          arkhamPlayerArkhamGameId p2 `shouldBe` gid2
          arkhamPlayerInvestigatorId p2 `shouldBe` coerce daisyId
        other -> expectationFailure $ "expected a successful validated pair, got: " <> show other
      log_ `shouldBe` [LockedPlayer pid1 True, LockedPlayer pid2 True, CheckedDestinationOccupant gid1 user2 False, CheckedDestinationOccupant gid2 user1 False]

    it "requested pids out of ascending order still lock in canonical ascending PlayerId order" do
      let descendingTransform = fixtureTransform daisyId pid2 rolandId pid1
          rows =
            Map.fromList
              [(pid1, Just (fixtureArkhamPlayer user1 gid1 rolandId)), (pid2, Just (fixtureArkhamPlayer user2 gid2 daisyId))]
          (result, log_) =
            runTestDB FailNever (fixturePlayerTestState rows mempty) (lockAndValidateSwapPlayers gid2 gid1 descendingTransform)
      case result of
        Right (Right _) -> pure ()
        other -> expectationFailure $ "expected a successful validated pair, got: " <> show other
      log_
        `shouldBe` [ LockedPlayer pid1 True
                   , LockedPlayer pid2 True
                   , CheckedDestinationOccupant gid1 user2 False
                   , CheckedDestinationOccupant gid2 user1 False
                   ]

    it "the FIRST canonically-ordered player row being absent reports PlayerMissing, but BOTH locks are still attempted" do
      let rows = Map.fromList [(pid2, Just (fixtureArkhamPlayer user2 gid2 daisyId))]
          (result, log_) = run FailNever rows
      case result of
        Right (Left MainStreetSwapPlayerMissing) -> pure ()
        other -> expectationFailure $ "expected PlayerMissing, got: " <> show other
      log_ `shouldBe` [LockedPlayer pid1 False, LockedPlayer pid2 True]

    it "the SECOND canonically-ordered player row being absent reports PlayerMissing, proving the first was genuinely locked first" do
      let rows = Map.fromList [(pid1, Just (fixtureArkhamPlayer user1 gid1 rolandId))]
          (result, log_) = run FailNever rows
      case result of
        Right (Left MainStreetSwapPlayerMissing) -> pure ()
        other -> expectationFailure $ "expected PlayerMissing, got: " <> show other
      log_ `shouldBe` [LockedPlayer pid1 True, LockedPlayer pid2 False]

    it "a failure locking the FIRST canonically-ordered player row cannot produce a success-shaped result, and the second is never attempted" do
      let rows =
            Map.fromList
              [(pid1, Just (fixtureArkhamPlayer user1 gid1 rolandId)), (pid2, Just (fixtureArkhamPlayer user2 gid2 daisyId))]
          (result, log_) = run (FailAtLockPlayer 0) rows
      result `shouldSatisfy` isLeft
      log_ `shouldBe` []

    it "a failure locking the SECOND canonically-ordered player row proves the first was genuinely locked first" do
      let rows =
            Map.fromList
              [(pid1, Just (fixtureArkhamPlayer user1 gid1 rolandId)), (pid2, Just (fixtureArkhamPlayer user2 gid2 daisyId))]
          (result, log_) = run (FailAtLockPlayer 1) rows
      result `shouldSatisfy` isLeft
      log_ `shouldBe` [LockedPlayer pid1 True]

    it "a locked row present but linked to the WRONG game reports PlayerWrongGame, with both locks already attempted" do
      let rows =
            Map.fromList
              [(pid1, Just (fixtureArkhamPlayer user1 gid2 rolandId)), (pid2, Just (fixtureArkhamPlayer user2 gid2 daisyId))]
          (result, log_) = run FailNever rows
      case result of
        Right (Left MainStreetSwapPlayerWrongGame) -> pure ()
        other -> expectationFailure $ "expected PlayerWrongGame, got: " <> show other
      log_ `shouldBe` [LockedPlayer pid1 True, LockedPlayer pid2 True]

    it "a locked row present in the right game but recorded under the WRONG investigator reports PlayerMismatch" do
      let rows =
            Map.fromList
              [(pid1, Just (fixtureArkhamPlayer user1 gid1 daisyId)), (pid2, Just (fixtureArkhamPlayer user2 gid2 daisyId))]
          (result, log_) = run FailNever rows
      case result of
        Right (Left MainStreetSwapPlayerMismatch) -> pure ()
        other -> expectationFailure $ "expected PlayerMismatch, got: " <> show other
      log_ `shouldBe` [LockedPlayer pid1 True, LockedPlayer pid2 True]

    it "both participants sharing the SAME user reports SameUser, before any destination-occupancy probe or write" do
      let rows =
            Map.fromList
              [(pid1, Just (fixtureArkhamPlayer user1 gid1 rolandId)), (pid2, Just (fixtureArkhamPlayer user1 gid2 daisyId))]
          (result, log_) = run FailNever rows
      case result of
        Right (Left MainStreetSwapSameUser) -> pure ()
        other -> expectationFailure $ "expected SameUser, got: " <> show other
      log_ `shouldBe` [LockedPlayer pid1 True, LockedPlayer pid2 True]

    it "the incoming user for gid1's destination already holding an unrelated seat there (e.g. a pre-existing member/spectator row) reports DestinationOccupied, with BOTH destinations still probed" do
      let rows =
            Map.fromList
              [(pid1, Just (fixtureArkhamPlayer user1 gid1 rolandId)), (pid2, Just (fixtureArkhamPlayer user2 gid2 daisyId))]
          -- user2 is arriving in gid1 (pid2's owner swaps into gid1); some OTHER row already occupies it.
          occupants = Map.fromList [((gid1, user2), True)]
          (result, log_) = runWithOccupants FailNever rows occupants
      case result of
        Right (Left MainStreetSwapDestinationOccupied) -> pure ()
        other -> expectationFailure $ "expected DestinationOccupied, got: " <> show other
      log_
        `shouldBe`
          [ LockedPlayer pid1 True
          , LockedPlayer pid2 True
          , CheckedDestinationOccupant gid1 user2 True
          , CheckedDestinationOccupant gid2 user1 False
          ]

    it "the incoming user for gid2's destination already holding an unrelated seat there reports DestinationOccupied, proving BOTH probes are still attempted in canonical (ascending destination game id) order" do
      let rows =
            Map.fromList
              [(pid1, Just (fixtureArkhamPlayer user1 gid1 rolandId)), (pid2, Just (fixtureArkhamPlayer user2 gid2 daisyId))]
          -- user1 is arriving in gid2 (pid1's owner swaps into gid2); some OTHER row already occupies it.
          occupants = Map.fromList [((gid2, user1), True)]
          (result, log_) = runWithOccupants FailNever rows occupants
      case result of
        Right (Left MainStreetSwapDestinationOccupied) -> pure ()
        other -> expectationFailure $ "expected DestinationOccupied, got: " <> show other
      log_
        `shouldBe`
          -- gid1 (ascending) is still probed FIRST (and reports no conflict there), before the
          -- gid2 probe that actually finds one.
          [LockedPlayer pid1 True, LockedPlayer pid2 True, CheckedDestinationOccupant gid1 user2 False, CheckedDestinationOccupant gid2 user1 True]
