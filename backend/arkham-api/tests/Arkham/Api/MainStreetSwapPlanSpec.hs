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
  swapInvestigatorState,
  validateSwapOrdinals,
 )
import Arkham.Card.Id (unsafeMakeCardId)
import Arkham.Classes.Entity (attr, overAttrs)
import Arkham.Difficulty (Difficulty (Easy))
import Arkham.Entities (Entities (..))
import Arkham.Epic.Types (
  GroupOrdinal (..),
  SharedKey (MainStreetReady),
  emptySharedEventState,
  setSharedCounter,
 )
import Arkham.Game (Game, newScenario, setInitialScenarioMeta)
import Arkham.Game qualified as Game
import Arkham.Game.State (GameState (IsActive))
import Arkham.Id (InvestigatorId, LocationId (..), PlayerId (..))
import Arkham.Investigator (lookupInvestigator)
import Arkham.Investigator.Types (Investigator, investigatorPlacement)
import Arkham.Location (lookupLocation)
import Arkham.Phase (Phase (..))
import Arkham.Placement (Placement (AtLocation, Unplaced))
import Arkham.Question (Question (ChooseOne))
import Arkham.Prelude
import Data.Map.Strict qualified as Map
import Data.UUID qualified as UUID
import Entity.Arkham.Game qualified as GameRow
import Network.HTTP.Types.Status qualified as Status
import Test.Hspec

gameA, gameB :: GameRow.ArkhamGameId
gameA = GameRow.ArkhamGameKey $ UUID.fromWords 0 0 0 1
gameB = GameRow.ArkhamGameKey $ UUID.fromWords 0 0 0 2

mainStreetA, mainStreetB, elsewhere :: LocationId
mainStreetA = LocationId $ UUID.fromWords 0 0 1 1
mainStreetB = LocationId $ UUID.fromWords 0 0 1 2
elsewhere = LocationId $ UUID.fromWords 0 0 1 3

roland, rolandAlternate, daisy :: InvestigatorId
roland = "01001"
rolandAlternate = "01501"
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
    , mainStreetSideResumable = True
    }

{- | A group's game, playing its investigation phase: its Main Street, and
@iid@ (seated by @player@) readied there by the scenario.
-}
readyGame :: InvestigatorId -> Word32 -> LocationId -> Game
readyGame iid player mainStreet =
  initial
    { Game.gameGameState = IsActive
    , Game.gamePhase = InvestigationPhase
    , Game.gamePlayers = [PlayerId $ UUID.fromWords 0 0 2 player]
    , Game.gamePlayerOrder = [iid]
    , Game.gameEntities =
        (Game.gameEntities initial)
          { entitiesInvestigators =
              Map.singleton iid (investigatorAt iid player (AtLocation mainStreet))
          , entitiesLocations =
              Map.singleton mainStreet
                $ lookupLocation "89006" mainStreet (unsafeMakeCardId $ UUID.fromWords 0 0 3 player)
          }
    }
 where
  initial = setInitialScenarioMeta "mainStreetReady" iid $ newScenario "85001" 1 2 Easy False

-- | Plan and run the swap of two groups' games.
swapGames :: Game -> Game -> Either MainStreetSwapError (Game, Game)
swapGames firstGame secondGame = do
  plan <-
    planMainStreetSwap
      (mainStreetSwapSide (GroupOrdinal 0) firstGame)
      (mainStreetSwapSide (GroupOrdinal 1) secondGame)
  let (firstGame', secondGame', _, _) = swapInvestigatorState plan firstGame secondGame
  pure (firstGame', secondGame')

placementIn :: InvestigatorId -> Game -> Maybe Placement
placementIn iid game =
  attr investigatorPlacement <$> Map.lookup iid (entitiesInvestigators $ Game.gameEntities game)

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

    it "refuses a group whose game is not playing its investigation phase" do
      planMainStreetSwap firstSide {mainStreetSideResumable = False} secondSide
        `shouldBe` Left (SwapNotReady (GroupOrdinal 0))
      planMainStreetSwap firstSide secondSide {mainStreetSideResumable = False}
        `shouldBe` Left (SwapNotReady (GroupOrdinal 1))

    it "refuses a group whose game holds another printing of the investigator it would receive" do
      -- The first group sends Daisy but keeps a Roland (01501); it would receive Roland (01001).
      let keepsAlternate =
            (readySide 0 daisy mainStreetA)
              { mainStreetSideInvestigators =
                  Map.fromList
                    [ (daisy, investigatorAt daisy 0 (AtLocation mainStreetA))
                    , (rolandAlternate, investigatorAt rolandAlternate 2 (AtLocation elsewhere))
                    ]
              }
      planMainStreetSwap keepsAlternate (readySide 1 roland mainStreetB)
        `shouldBe` Left (SwapInvestigatorConflict (GroupOrdinal 0))
      planMainStreetSwap (readySide 1 roland mainStreetB) keepsAlternate
        `shouldBe` Left (SwapInvestigatorConflict (GroupOrdinal 0))

    it "accepts another printing of the investigator a group sends away" do
      -- Each game gives up its Roland and receives the other printing.
      let plan = planMainStreetSwap (readySide 0 rolandAlternate mainStreetA) (readySide 1 roland mainStreetB)
      (mainStreetFirstDestination <$> plan) `shouldBe` Right mainStreetB

    it "refuses a group whose game already holds the investigator it would receive" do
      let alsoHolds iid player side =
            side
              { mainStreetSideInvestigators =
                  Map.insert iid (investigatorAt iid player (AtLocation elsewhere))
                    $ mainStreetSideInvestigators side
              }
      -- The first group's game also seats a Daisy, who stayed behind.
      planMainStreetSwap (alsoHolds daisy 2 firstSide) secondSide
        `shouldBe` Left (SwapInvestigatorConflict (GroupOrdinal 0))
      planMainStreetSwap firstSide (alsoHolds roland 3 secondSide)
        `shouldBe` Left (SwapInvestigatorConflict (GroupOrdinal 1))

    it "accepts two groups that readied the same investigator" do
      (mainStreetFirstDestination <$> planMainStreetSwap firstSide (readySide 1 roland mainStreetB))
        `shouldBe` Right mainStreetB

  describe "mainStreetSwapSide" do
    it "resumes only an active game in its investigation phase" do
      let
        playing = readyGame roland 0 mainStreetA
        resumable = mainStreetSideResumable . mainStreetSwapSide (GroupOrdinal 0)
      resumable playing `shouldBe` True
      -- Mythos (e.g. at an encounter draw), Enemy and Upkeep: an empty queue
      -- does not continue the phase.
      [resumable playing {Game.gamePhase = phase} | phase <- [MythosPhase, EnemyPhase, UpkeepPhase]]
        `shouldBe` [False, False, False]
      resumable playing {Game.gameRunWindows = False} `shouldBe` False
      resumable (newScenario "85001" 1 2 Easy False) `shouldBe` False

  describe "swapInvestigatorState" do
    it "places each moved investigator at the Main Street of the game it moved into" do
      let swapped = swapGames (readyGame roland 0 mainStreetA) (readyGame daisy 1 mainStreetB)
      -- Daisy moved into the first game, Roland into the second.
      (placementIn daisy . fst <$> swapped) `shouldBe` Right (Just (AtLocation mainStreetA))
      (placementIn roland . snd <$> swapped) `shouldBe` Right (Just (AtLocation mainStreetB))

    it "does not carry a player's pending question into the game it joins" do
      let
        rolandPlayer = PlayerId $ UUID.fromWords 0 0 2 0
        asked =
          (readyGame roland 0 mainStreetA)
            { Game.gameQuestion = Map.singleton rolandPlayer (ChooseOne [])
            }
        swapped = swapGames asked (readyGame daisy 1 mainStreetB)
      -- Roland's seat now plays in the second game, with nothing asked yet.
      (elem rolandPlayer . Game.gamePlayers . snd <$> swapped) `shouldBe` Right True
      (Map.member rolandPlayer . Game.gameQuestion . snd <$> swapped)
        `shouldBe` Right False

    it "swaps two copies of the same investigator, each with its own state" do
      let
        -- Each group's Roland has used a different card.
        withUses code game = game {Game.gameCardUses = Map.singleton code [roland]}
        firstGame = withUses "01020" $ readyGame roland 0 mainStreetA
        secondGame = withUses "01030" $ readyGame roland 1 mainStreetB
        swapped = swapGames firstGame secondGame
        seats = Game.gamePlayers
        uses = Map.filter (not . null) . Game.gameCardUses
      -- Each game now seats the other group's player, whose Roland brought its
      -- own card uses and stands at this game's Main Street.
      (bimap seats seats <$> swapped) `shouldBe` Right (seats secondGame, seats firstGame)
      (bimap uses uses <$> swapped)
        `shouldBe` Right (Map.singleton "01030" [roland], Map.singleton "01020" [roland])
      (bimap (placementIn roland) (placementIn roland) <$> swapped)
        `shouldBe` Right (Just (AtLocation mainStreetA), Just (AtLocation mainStreetB))
      (bimap Game.gamePlayerOrder Game.gamePlayerOrder <$> swapped) `shouldBe` Right ([roland], [roland])

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
        , SwapInvestigatorConflict (GroupOrdinal 0)
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
                   , (Status.status409, "main-street-conflict")
                   ]
