{- | Proves the atomic Epic-event creation sequencing described in
'Api.Handler.Arkham.Events.createEpicEventAggregate' without a live database.

A pure, in-memory 'MonadEpicPersistence' instance ('TestDB') runs the exact
same production sequencing function and records every step it performs. This
lets us assert:

* every group's game, immediately followed by its own initial step, is
  inserted (in ordinal order) before the event, member, and group-link rows;
* each group is built from its own independently supplied seed, in ordinal
  order, with correct pure metadata -- the seed is not derived from, or
  reused between, groups or the shared event story seed, though independently
  drawn values may legitimately coincide;
* a failure injected at any step -- a group's game, a group's initial step, or
  the late event/member/link steps -- short-circuits the whole sequence with
  no successful (@Right@) result;
* a successful run returns every group link in ordinal order;
* zero groups, and a zero @playerCount@, behave the same as they do today.

'buildGroupGame' and 'preparePendingGroupGames' are also unit-tested directly
to pin down the pure per-group metadata (seats, 'WithFriends', step 0,
scenario meta, seed) and the seed-drawing contract (one call per group, in
ordinal order, paired with that group in order).
-}
module Arkham.Api.Events.EventCreationSpec (spec) where

import Api.Arkham.Types.MultiplayerVariant (MultiplayerVariant (WithFriends))
import Api.Handler.Arkham.Events
import Arkham.Difficulty (Difficulty (Easy))
import Arkham.Epic.Types (emptySharedEventState)
import Arkham.Game (gamePlayerCount, gameSeed)
import Arkham.Id (ScenarioId)
import Arkham.Prelude
import Control.Monad.Except (ExceptT, MonadError, runExceptT, throwError)
import Control.Monad.State.Strict (MonadState, State, gets, modify, runState)
import Data.Either (isLeft)
import Data.Time.Clock (secondsToDiffTime)
import Data.UUID qualified as UUID
import Database.Persist.Sql (toSqlKey)
import Entity.Arkham.Epic qualified as Epic
import Entity.Arkham.Game qualified as GameEntity
import Entity.User qualified as User
import Test.Hspec

-- Fixtures ----------------------------------------------------------------------

fixtureNow :: UTCTime
fixtureNow = UTCTime (fromGregorian 2026 2 3) (secondsToDiffTime 14706)

fixtureScenarioId :: ScenarioId
fixtureScenarioId = "85001"

fixtureOrganizerId :: User.UserId
fixtureOrganizerId = toSqlKey 7

fixtureEventId :: Epic.ArkhamEpicEventId
fixtureEventId = Epic.ArkhamEpicEventKey $ UUID.fromWords 0 0 0 999

fixtureGameId :: Int -> GameEntity.ArkhamGameId
fixtureGameId ordx = GameEntity.ArkhamGameKey $ UUID.fromWords 0 0 0 (fromIntegral ordx)

fixtureEventRecord :: Epic.ArkhamEpicEvent
fixtureEventRecord =
  Epic.ArkhamEpicEvent
    "Fixture Event"
    fixtureOrganizerId
    (Just (tshow fixtureScenarioId))
    Nothing
    (tshow Easy)
    (emptySharedEventState 0)
    0
    0
    fixtureNow
    fixtureNow

-- | Build one group's pending game the same way 'postApiV1ArkhamEventsR' does:
-- via the pure 'buildGroupGame' helper, with an explicit ordinal and seed.
mkPending :: Int -> Text -> Int -> Int -> PendingGroupGame
mkPending ordx groupName playerCount seed =
  PendingGroupGame
    { ordinal = ordx
    , name = groupName
    , seatCount = playerCount
    , game =
        buildGroupGame groupName fixtureScenarioId Easy False False playerCount seed fixtureNow
    }

-- Pure, in-memory persistence backend --------------------------------------------

-- | One recorded persistence step, in the order it happened.
data Step
  = InsertedGame Int Int
  -- ^ ordinal, the seed that group's game was built with
  | InsertedStep Int
  -- ^ ordinal of the group whose initial 'ArkhamStep' was inserted
  | InsertedEvent
  | InsertedMember
  | InsertedLink Int
  -- ^ ordinal
  deriving stock (Eq, Show)

-- | Which step (if any) should fail, for a given test run.
data FailAt
  = FailNever
  | FailAtGame Int
  | FailAtInitialStep Int
  | FailAtEvent
  | FailAtMember
  | FailAtLink Int
  deriving stock (Eq, Show)

-- | Mutable state threaded through 'TestDB': the step log, plus a lookup from
-- an inserted game's id back to its ordinal (so 'insertInitialStep', which
-- only receives the game id, can still be checked against 'FailAtInitialStep'
-- and logged with the right ordinal).
data TestState = TestState
  { steps :: [Step]
  , gameOrdinals :: [(GameEntity.ArkhamGameId, Int)]
  }

emptyTestState :: TestState
emptyTestState = TestState {steps = [], gameOrdinals = []}

{- | A pure interpreter of 'MonadEpicPersistence': records every step it is
asked to perform and short-circuits (via 'ExceptT') at the configured 'FailAt'
step, exactly the way an uncaught exception aborts a real 'runDB' transaction
(see the rollback caveat documented on 'MonadEpicPersistence').
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

runTestDB :: FailAt -> TestDB a -> (Either String a, [Step])
runTestDB failAt action =
  let (result, finalState) = runState (runExceptT (runReaderT (unTestDB action) failAt)) emptyTestState
   in (result, finalState.steps)

failIfConfigured :: FailAt -> TestDB ()
failIfConfigured this = do
  configured <- ask
  when (configured == this) $ throwError (show this)

recordStep :: Step -> TestDB ()
recordStep step = modify \s -> s {steps = s.steps ++ [step]}

instance MonadEpicPersistence TestDB where
  insertGroupGame pg = do
    failIfConfigured (FailAtGame pg.ordinal)
    recordStep (InsertedGame pg.ordinal (gameSeed pg.game.currentData))
    let gid = fixtureGameId pg.ordinal
    modify \s -> s {gameOrdinals = (gid, pg.ordinal) : s.gameOrdinals}
    pure gid

  insertInitialStep gid = do
    ordinals <- gets (.gameOrdinals)
    let ordx = fromMaybe (error "insertInitialStep: unknown game id") (lookup gid ordinals)
    failIfConfigured (FailAtInitialStep ordx)
    recordStep (InsertedStep ordx)

  insertEvent _event = do
    failIfConfigured FailAtEvent
    recordStep InsertedEvent
    pure fixtureEventId

  insertOrganizerMember _eid _uid = do
    failIfConfigured FailAtMember
    recordStep InsertedMember

  insertGroupLink _eid pg _gid = do
    failIfConfigured (FailAtLink pg.ordinal)
    recordStep (InsertedLink pg.ordinal)

-- Specs ---------------------------------------------------------------------------

spec :: Spec
spec = do
  describe "buildGroupGame" do
    it "seats max 1 playerCount, including the zero-player edge case" do
      gamePlayerCount (mkPending 0 "Solo" 0 111).game.currentData `shouldBe` 1
      gamePlayerCount (mkPending 0 "Solo" 1 111).game.currentData `shouldBe` 1
      gamePlayerCount (mkPending 0 "Trio" 3 111).game.currentData `shouldBe` 3

    it "keeps the group link's seatCount unclamped, distinct from the game's clamped seat count" do
      -- Matches prior behavior: 'ArkhamEpicGroup's seatCount column stores the
      -- raw requested playerCount (even 0), while the game itself always seats
      -- at least 1.
      let pg = mkPending 0 "Solo" 0 111
      pg.seatCount `shouldBe` 0
      gamePlayerCount pg.game.currentData `shouldBe` 1

    it "uses WithFriends, step 0, and the supplied timestamp" do
      let ag = (mkPending 0 "G" 2 111).game
      ag.multiplayerVariant `shouldBe` WithFriends
      ag.step `shouldBe` (0 :: Int)
      ag.createdAt `shouldBe` fixtureNow
      ag.updatedAt `shouldBe` fixtureNow

    it "flags epicMultiplayer, and blobThatAteEverythingElse with no variant key when false" do
      let game = (mkPending 0 "G" 2 111).game.currentData
      gameScenarioMetaDefault "epicMultiplayer" False game `shouldBe` True
      gameScenarioMetaDefault "blobThatAteEverythingElse" False game `shouldBe` False
      gameScenarioMetaDefault "variant" ("" :: Text) game `shouldBe` ""

    it "sets variant = \"else\" only when playing with Blob That Ate Everything Else" do
      let game = buildGroupGame "G" fixtureScenarioId Easy False True 2 111 fixtureNow
          gameState = game.currentData
      gameScenarioMetaDefault "blobThatAteEverythingElse" False gameState `shouldBe` True
      gameScenarioMetaDefault "variant" ("" :: Text) gameState `shouldBe` "else"

    it "uses whatever seed it is given, independent of any other value -- coincidental equality is not an error" do
      -- 'buildGroupGame' itself does not draw or compare seeds; it just plants
      -- whatever it's handed. A caller supplying the same seed twice (however
      -- unlikely from an independent random draw) is not something this pure
      -- function can or should reject.
      let storySeed = 111 :: Int
          pendingGroups = [mkPending 0 "A" 1 111, mkPending 1 "B" 2 111, mkPending 2 "C" 3 333]
          gotSeeds = [gameSeed pg.game.currentData | pg <- pendingGroups]
      gotSeeds `shouldBe` [111, 111, 333]
      case gotSeeds of
        (firstSeed : _) -> storySeed `shouldBe` firstSeed -- coincidentally equal; still accepted
        [] -> expectationFailure "expected at least one seed"

  describe "preparePendingGroupGames (seed-drawing contract)" do
    let groupSpecs = [("A", 1), ("B", 2), ("C", 3)] :: [(Text, Int)]

    it "draws exactly one seed per group, in ordinal order, and pairs it with that group in order" do
      -- Records every ordinal it was asked to draw a seed for, and returns a
      -- distinguishable (but not necessarily distinct) value for each. Uses a
      -- State-based recorder to keep this test pure.
      let drawSeed :: Int -> State [Int] Int
          drawSeed ordx = do
            modify (++ [ordx])
            pure (100 + ordx)
          (pendingGroups, requestedOrdinals) =
            runState
              ( preparePendingGroupGames
                  drawSeed
                  fixtureScenarioId
                  Easy
                  False
                  False
                  fixtureNow
                  groupSpecs
              )
              []
      requestedOrdinals `shouldBe` [0, 1, 2]
      [(pg.ordinal, pg.name, gameSeed pg.game.currentData) | pg <- pendingGroups]
        `shouldBe` [(0, "A", 100), (1, "B", 101), (2, "C", 102)]

    it "preserves/accepts equal independently returned seeds rather than requiring distinctness" do
      -- An action that (perhaps coincidentally) returns the same seed for
      -- every group must not be rejected or de-duplicated.
      let constantSeed :: Int -> Identity Int
          constantSeed _ordx = pure 777
          pendingGroups = runIdentity (preparePendingGroupGames constantSeed fixtureScenarioId Easy False False fixtureNow groupSpecs)
      [gameSeed pg.game.currentData | pg <- pendingGroups] `shouldBe` [777, 777, 777]

    it "does not pass the group's playerCount or name into the seed action -- only its ordinal" do
      let recordedOrdinal :: Int -> Identity Int
          recordedOrdinal ordx = pure ordx -- echoes the ordinal back as the "seed"
          pendingGroups = runIdentity (preparePendingGroupGames recordedOrdinal fixtureScenarioId Easy False False fixtureNow groupSpecs)
      [gameSeed pg.game.currentData | pg <- pendingGroups] `shouldBe` [0, 1, 2]

  describe "createEpicEventAggregate (atomic sequencing)" do
    let pendingGroups = [mkPending 0 "A" 1 111, mkPending 1 "B" 2 222, mkPending 2 "C" 3 333]
        run failAt = runTestDB failAt (createEpicEventAggregate fixtureEventRecord fixtureOrganizerId pendingGroups)

    it "inserts every group's game, immediately followed by its own initial step, before the event/member/link rows, in ordinal order" do
      let (result, log_) = run FailNever
      result `shouldBe` Right fixtureEventId
      log_
        `shouldBe` [ InsertedGame 0 111
                   , InsertedStep 0
                   , InsertedGame 1 222
                   , InsertedStep 1
                   , InsertedGame 2 333
                   , InsertedStep 2
                   , InsertedEvent
                   , InsertedMember
                   , InsertedLink 0
                   , InsertedLink 1
                   , InsertedLink 2
                   ]

    it "produces group links in ordinal order on success" do
      let (result, log_) = run FailNever
      result `shouldBe` Right fixtureEventId
      [ordx | InsertedLink ordx <- log_] `shouldBe` [0, 1, 2]

    it "a failure inserting a group's game cannot produce a success-shaped result" do
      let (result, log_) = run (FailAtGame 1)
      result `shouldSatisfy` isLeft
      -- ordinal 0's game and initial step were already inserted before the
      -- failing group's game insert
      log_ `shouldBe` [InsertedGame 0 111, InsertedStep 0]

    it "a failure inserting a group's initial step cannot produce a success-shaped result, and no event/member/link steps occur" do
      let (result, log_) = run (FailAtInitialStep 1)
      result `shouldSatisfy` isLeft
      -- ordinal 0's game+step completed; ordinal 1's game was inserted, but its
      -- initial step failed -- so the event/member/link rows never happen
      log_ `shouldBe` [InsertedGame 0 111, InsertedStep 0, InsertedGame 1 222]

    it "a failure inserting the event cannot produce a success-shaped result, even though all games/steps were built" do
      let (result, log_) = run FailAtEvent
      result `shouldSatisfy` isLeft
      log_
        `shouldBe` [ InsertedGame 0 111
                   , InsertedStep 0
                   , InsertedGame 1 222
                   , InsertedStep 1
                   , InsertedGame 2 333
                   , InsertedStep 2
                   ]

    it "a failure inserting the organizer membership cannot produce a success-shaped result" do
      let (result, log_) = run FailAtMember
      result `shouldSatisfy` isLeft
      log_
        `shouldBe` [ InsertedGame 0 111
                   , InsertedStep 0
                   , InsertedGame 1 222
                   , InsertedStep 1
                   , InsertedGame 2 333
                   , InsertedStep 2
                   , InsertedEvent
                   ]

    it "a failure inserting a late group link cannot produce a success-shaped result" do
      let (result, log_) = run (FailAtLink 2)
      result `shouldSatisfy` isLeft
      log_
        `shouldBe` [ InsertedGame 0 111
                   , InsertedStep 0
                   , InsertedGame 1 222
                   , InsertedStep 1
                   , InsertedGame 2 333
                   , InsertedStep 2
                   , InsertedEvent
                   , InsertedMember
                   , InsertedLink 0
                   , InsertedLink 1
                   ]

    it "supports zero groups, matching today's unvalidated (accepted) request behavior" do
      let (result, log_) = runTestDB FailNever (createEpicEventAggregate fixtureEventRecord fixtureOrganizerId [])
      result `shouldBe` Right fixtureEventId
      log_ `shouldBe` [InsertedEvent, InsertedMember]
