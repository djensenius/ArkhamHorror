{- | Proves the atomic Epic-event creation sequencing described in
'Api.Handler.Arkham.Events.createEpicEventAggregate' without a live database.

A pure, in-memory 'MonadEpicPersistence' instance ('TestDB') runs the exact
same production sequencing function and records every step it performs. This
lets us assert:

* every group's game is inserted, in ordinal order, before the event, member,
  and group-link rows;
* each group is built with its own distinct supplied seed and correct pure
  metadata (never the shared event seed, never another group's seed);
* a failure injected at any step -- including the late event/member/link steps
  -- short-circuits the whole sequence with no successful (@Right@) result;
* a successful run returns every group link in ordinal order;
* zero groups, and a zero @playerCount@, behave the same as they do today.

'buildGroupGame' itself is also unit-tested directly to pin down the pure
per-group metadata (seats, 'WithFriends', step 0, scenario meta, seed).
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
import Control.Monad.State.Strict (MonadState, State, modify, runState)
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
  | InsertedEvent
  | InsertedMember
  | InsertedLink Int
  -- ^ ordinal
  deriving stock (Eq, Show)

-- | Which step (if any) should fail, for a given test run.
data FailAt
  = FailNever
  | FailAtGame Int
  | FailAtEvent
  | FailAtMember
  | FailAtLink Int
  deriving stock (Eq, Show)

{- | A pure interpreter of 'MonadEpicPersistence': records every step it is
asked to perform and short-circuits (via 'ExceptT') at the configured 'FailAt'
step, exactly the way an uncaught exception aborts a real 'runDB' transaction
(see the rollback caveat documented on 'MonadEpicPersistence').
-}
newtype TestDB a = TestDB
  {unTestDB :: ReaderT FailAt (ExceptT String (State [Step])) a}
  deriving newtype
    ( Functor
    , Applicative
    , Monad
    , MonadReader FailAt
    , MonadError String
    , MonadState [Step]
    )

runTestDB :: FailAt -> TestDB a -> (Either String a, [Step])
runTestDB failAt action =
  runState (runExceptT (runReaderT (unTestDB action) failAt)) []

failIfConfigured :: FailAt -> TestDB ()
failIfConfigured this = do
  configured <- ask
  when (configured == this) $ throwError (show this)

recordStep :: Step -> TestDB ()
recordStep step = modify (++ [step])

instance MonadEpicPersistence TestDB where
  insertGroupGame pg = do
    failIfConfigured (FailAtGame pg.ordinal)
    recordStep (InsertedGame pg.ordinal (gameSeed pg.game.currentData))
    pure (fixtureGameId pg.ordinal)

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

    it "gives each group its own independent seed, distinct from the shared event story seed" do
      let storySeed = 999999 :: Int
          pendingGroups = [mkPending 0 "A" 1 111, mkPending 1 "B" 2 222, mkPending 2 "C" 3 333]
          gotSeeds = [gameSeed pg.game.currentData | pg <- pendingGroups]
      gotSeeds `shouldBe` [111, 222, 333]
      nub gotSeeds `shouldBe` gotSeeds -- all distinct from one another
      storySeed `notElem` gotSeeds `shouldBe` True

  describe "createEpicEventAggregate (atomic sequencing)" do
    let pendingGroups = [mkPending 0 "A" 1 111, mkPending 1 "B" 2 222, mkPending 2 "C" 3 333]
        run failAt = runTestDB failAt (createEpicEventAggregate fixtureEventRecord fixtureOrganizerId pendingGroups)

    it "inserts every group's game before the event/member/link rows, in ordinal order" do
      let (result, log_) = run FailNever
      result `shouldBe` Right fixtureEventId
      log_
        `shouldBe` [ InsertedGame 0 111
                   , InsertedGame 1 222
                   , InsertedGame 2 333
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
      -- ordinal 0 was already (attemptedly) inserted before the failing group
      log_ `shouldBe` [InsertedGame 0 111]

    it "a failure inserting the event cannot produce a success-shaped result, even though all games were built" do
      let (result, log_) = run FailAtEvent
      result `shouldSatisfy` isLeft
      log_ `shouldBe` [InsertedGame 0 111, InsertedGame 1 222, InsertedGame 2 333]

    it "a failure inserting the organizer membership cannot produce a success-shaped result" do
      let (result, log_) = run FailAtMember
      result `shouldSatisfy` isLeft
      log_ `shouldBe` [InsertedGame 0 111, InsertedGame 1 222, InsertedGame 2 333, InsertedEvent]

    it "a failure inserting a late group link cannot produce a success-shaped result" do
      let (result, log_) = run (FailAtLink 2)
      result `shouldSatisfy` isLeft
      log_
        `shouldBe` [ InsertedGame 0 111
                   , InsertedGame 1 222
                   , InsertedGame 2 333
                   , InsertedEvent
                   , InsertedMember
                   , InsertedLink 0
                   , InsertedLink 1
                   ]

    it "supports zero groups, matching today's unvalidated (accepted) request behavior" do
      let (result, log_) = runTestDB FailNever (createEpicEventAggregate fixtureEventRecord fixtureOrganizerId [])
      result `shouldBe` Right fixtureEventId
      log_ `shouldBe` [InsertedEvent, InsertedMember]
