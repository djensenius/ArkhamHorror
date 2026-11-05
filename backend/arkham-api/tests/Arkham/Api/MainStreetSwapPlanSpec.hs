module Arkham.Api.MainStreetSwapPlanSpec (spec) where

import Api.Handler.Arkham.Events (mainStreetSwapErrorStatus)
import Api.Handler.Arkham.Games.Shared (
  MainStreetSwapError (..),
  MainStreetSwapPlan (..),
  MainStreetSwapSide (..),
  clearMainStreetReady,
  mainStreetSwapSide,
  planMainStreetSwap,
  requireMainStreetReady,
  resolveSwapGames,
  validateSwapOrdinals,
 )
import Arkham.Classes.Entity (attr, overAttrs)
import Arkham.Difficulty (Difficulty (Easy))
import Arkham.Epic.Types (
  GroupOrdinal (..),
  SharedKey (MainStreetReady),
  emptySharedEventState,
  setSharedCounter,
 )
import Arkham.Game (newScenario, setInitialScenarioMeta)
import Arkham.Id (InvestigatorId, LocationId (..), PlayerId (..))
import Arkham.Investigator (lookupInvestigator)
import Arkham.Investigator.Types (Investigator, investigatorPlacement)
import Arkham.Placement (Placement (AtLocation, Unplaced))
import Arkham.Prelude
import Data.Map.Strict qualified as Map
import Data.UUID qualified as UUID
import Data.Word (Word32)
import Entity.Arkham.Game qualified as Game
import Network.HTTP.Types.Status qualified as Status
import Test.Hspec

gameA, gameB :: Game.ArkhamGameId
gameA = Game.ArkhamGameKey $ UUID.fromWords 0 0 0 1
gameB = Game.ArkhamGameKey $ UUID.fromWords 0 0 0 2

mainStreetA, mainStreetB, elsewhere :: LocationId
mainStreetA = LocationId $ UUID.fromWords 0 0 1 1
mainStreetB = LocationId $ UUID.fromWords 0 0 1 2
elsewhere = LocationId $ UUID.fromWords 0 0 1 3

roland, daisy :: InvestigatorId
roland = "01001"
daisy = "01002"

investigatorAt :: InvestigatorId -> Word32 -> Placement -> Investigator
investigatorAt iid player placement =
  overAttrs (\a -> a {investigatorPlacement = placement})
    $ lookupInvestigator iid (PlayerId $ UUID.fromWords 0 0 2 player)

-- | A group that readied @iid@, standing at its own Main Street.
readySide :: Int -> InvestigatorId -> LocationId -> MainStreetSwapSide
readySide ordinal iid mainStreet =
  MainStreetSwapSide
    { mainStreetSideOrdinal = GroupOrdinal ordinal
    , mainStreetSideReady = Just iid
    , mainStreetSideLocation = Just mainStreet
    , mainStreetSideInvestigators =
        Map.singleton iid (investigatorAt iid (fromIntegral ordinal) (AtLocation mainStreet))
    }

firstSide, secondSide :: MainStreetSwapSide
firstSide = readySide 0 roland mainStreetA
secondSide = readySide 1 daisy mainStreetB

spec :: Spec
spec = do
  describe "validateSwapOrdinals" do
    it "accepts two distinct, non-negative groups" do
      validateSwapOrdinals 1 0 `shouldBe` Right ()

    it "refuses the same group twice, or a negative ordinal, from the request alone" do
      validateSwapOrdinals 1 1 `shouldBe` Left SwapSameGroup
      validateSwapOrdinals (-1) 0 `shouldBe` Left (SwapBadOrdinal (-1))
      validateSwapOrdinals 0 (-2) `shouldBe` Left (SwapBadOrdinal (-2))

  describe "resolveSwapGames" do
    it "resolves each requested group's game, in request order" do
      resolveSwapGames 1 0 [(0, Just gameA), (1, Just gameB)] `shouldBe` Right (gameB, gameA)

    it "refuses the same group twice" do
      resolveSwapGames 1 1 [(1, Just gameA)] `shouldBe` Left SwapSameGroup

    it "refuses a negative ordinal" do
      resolveSwapGames (-1) 0 [(0, Just gameA)] `shouldBe` Left (SwapBadOrdinal (-1))
      resolveSwapGames 0 (-2) [(0, Just gameA)] `shouldBe` Left (SwapBadOrdinal (-2))

    it "refuses a group the event does not have" do
      resolveSwapGames 0 2 [(0, Just gameA)] `shouldBe` Left (SwapGroupMissing (GroupOrdinal 2))

    it "refuses a group without a game" do
      resolveSwapGames 0 1 [(0, Just gameA), (1, Nothing)]
        `shouldBe` Left (SwapGroupHasNoGame (GroupOrdinal 1))

  describe "planMainStreetSwap" do
    it "sends each readied investigator to the other group's Main Street" do
      let plan = planMainStreetSwap firstSide secondSide
      plan
        `shouldBe` Right
          MainStreetSwapPlan
            { mainStreetFirstInvestigator = investigatorAt roland 0 (AtLocation mainStreetA)
            , mainStreetFirstDestination = mainStreetB
            , mainStreetSecondInvestigator = investigatorAt daisy 1 (AtLocation mainStreetB)
            , mainStreetSecondDestination = mainStreetA
            }
      -- The planned investigators are the ones read from each game, unmoved.
      (attr investigatorPlacement . mainStreetFirstInvestigator <$> plan)
        `shouldBe` Right (AtLocation mainStreetA)
      (attr investigatorPlacement . mainStreetSecondInvestigator <$> plan)
        `shouldBe` Right (AtLocation mainStreetB)

    it "refuses a group that has not readied an investigator" do
      planMainStreetSwap firstSide {mainStreetSideReady = Nothing} secondSide
        `shouldBe` Left (SwapNotReady (GroupOrdinal 0))
      planMainStreetSwap firstSide secondSide {mainStreetSideReady = Nothing}
        `shouldBe` Left (SwapNotReady (GroupOrdinal 1))

    it "refuses a group whose game has no Main Street" do
      planMainStreetSwap firstSide {mainStreetSideLocation = Nothing} secondSide
        `shouldBe` Left (SwapNoMainStreet (GroupOrdinal 0))
      planMainStreetSwap firstSide secondSide {mainStreetSideLocation = Nothing}
        `shouldBe` Left (SwapNoMainStreet (GroupOrdinal 1))

    it "refuses a readied investigator who is no longer in the game" do
      planMainStreetSwap firstSide {mainStreetSideInvestigators = mempty} secondSide
        `shouldBe` Left (SwapInvestigatorMissing (GroupOrdinal 0))

    it "refuses a readied investigator who has left Main Street" do
      let movedTo placement =
            secondSide
              { mainStreetSideInvestigators = Map.singleton daisy (investigatorAt daisy 1 placement)
              }
      planMainStreetSwap firstSide (movedTo (AtLocation elsewhere))
        `shouldBe` Left (SwapInvestigatorMoved (GroupOrdinal 1))
      planMainStreetSwap firstSide (movedTo Unplaced)
        `shouldBe` Left (SwapInvestigatorMoved (GroupOrdinal 1))

  describe "requireMainStreetReady" do
    let ready ordinals =
          foldr
            (\o -> setSharedCounter (MainStreetReady (GroupOrdinal o)) 1)
            (emptySharedEventState 2)
            ordinals
    it "accepts two groups whose readiness is still unspent" do
      requireMainStreetReady (ready [0, 1]) (GroupOrdinal 0) (GroupOrdinal 1) `shouldBe` Right ()

    it "refuses a group whose readiness is spent or was never raised" do
      requireMainStreetReady (ready [1]) (GroupOrdinal 0) (GroupOrdinal 1)
        `shouldBe` Left (SwapNotReady (GroupOrdinal 0))
      requireMainStreetReady (ready [0]) (GroupOrdinal 0) (GroupOrdinal 1)
        `shouldBe` Left (SwapNotReady (GroupOrdinal 1))
      requireMainStreetReady (ready []) (GroupOrdinal 1) (GroupOrdinal 0)
        `shouldBe` Left (SwapNotReady (GroupOrdinal 1))

  describe "clearMainStreetReady" do
    it "forgets the investigator the scenario readied at Main Street" do
      let readied =
            setInitialScenarioMeta "mainStreetReady" roland $ newScenario "85001" 1 2 Easy False
      mainStreetSideReady (mainStreetSwapSide (GroupOrdinal 0) readied) `shouldBe` Just roland
      mainStreetSideReady (mainStreetSwapSide (GroupOrdinal 0) (clearMainStreetReady readied))
        `shouldBe` Nothing

  describe "mainStreetSwapErrorStatus" do
    it "maps every refusal to a stable 4xx status and code" do
      map
        mainStreetSwapErrorStatus
        [ SwapSameGroup
        , SwapBadOrdinal (-1)
        , SwapEventMissing
        , SwapGroupMissing (GroupOrdinal 2)
        , SwapGroupHasNoGame (GroupOrdinal 1)
        , SwapGameMissing gameA
        , SwapNotReady (GroupOrdinal 0)
        , SwapNoMainStreet (GroupOrdinal 0)
        , SwapInvestigatorMissing (GroupOrdinal 0)
        , SwapInvestigatorMoved (GroupOrdinal 1)
        ]
        `shouldBe` [ (Status.status400, "main-street-same-group")
                   , (Status.status400, "main-street-bad-group")
                   , (Status.status404, "main-street-event-missing")
                   , (Status.status404, "main-street-group-missing")
                   , (Status.status404, "main-street-game-missing")
                   , (Status.status404, "main-street-game-missing")
                   , (Status.status409, "main-street-not-ready")
                   , (Status.status409, "main-street-stale")
                   , (Status.status409, "main-street-stale")
                   , (Status.status409, "main-street-stale")
                   ]
