module Arkham.Api.JsonContractsSpec (spec) where

import Api.Arkham.Helpers (ApiResponse (..))
import Api.Arkham.Types.GameStep (GameStepJson (..))
import Arkham.Epic.Types (SharedEventState (..))
import Data.Aeson qualified as Aeson
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Data.UUID qualified as UUID
import Entity.Answer (Answer (..))
import System.IO.Error qualified as IOError
import TestImport

loadFixture :: FilePath -> IO Aeson.Value
loadFixture fileName = findFixture fixturePaths
 where
  fixturePaths =
    [ "contracts/fixtures/" <> fileName
    , "../contracts/fixtures/" <> fileName
    , "../../contracts/fixtures/" <> fileName
    ]

  findFixture = \case
    [] ->
      fail
        $ "Could not find contract fixture "
        <> fileName
        <> "; searched: "
        <> show fixturePaths
    path : remainingPaths -> IOError.tryIOError (Aeson.eitherDecodeFileStrict' path) >>= \case
      Left err
        | IOError.isDoesNotExistError err -> findFixture remainingPaths
        | otherwise -> IOError.ioError err
      Right (Left err) ->
        fail $ "Could not decode contract fixture " <> fileName <> " at " <> path <> ": " <> err
      Right (Right fixture) -> pure fixture

serverMessageFixtures :: [(FilePath, ApiResponse)]
serverMessageFixtures =
  [ ("game-message.json", GameMessage "A contract fixture message.")
  , ("game-error.json", GameError "The question changed before this answer arrived.")
  , ("game-ui.json", GameUI "contract:ui")
  , ("game-audio.json", GameAudio "contract.ogg")
  , ("game-card.json", GameCard "Contract card" fixtureCard)
  , ( "game-card-only.json"
    , GameCardOnly
        (PlayerId $ UUID.fromWords 0 0 0 1)
        "Private contract card"
        fixtureCard
    )
  , ("game-tarot.json", GameTarot $ Aeson.object ["spread" .= ("fixture" :: Text)])
  , ("game-show-discard.json", GameShowDiscard "01001")
  , ("game-show-under.json", GameShowUnder "02002")
  , ("game-achievement.json", GameAchievement "fixture-achievement")
  , ( "game-playability-info.json"
    , GamePlayabilityInfo
        nullCardId
        "fixture-card"
        [("play", Nothing), ("fast", Just "Needs an action window")]
    )
  , ( "shared-state-update.json"
    , SharedStateUpdate
        $ SharedEventState
          7
          (Map.fromList [("act-progress:1", 2), ("countermeasures", 4)])
          4
          (Set.fromList ["delta-a", "delta-b"])
    )
  , ("event-changed.json", EventChanged)
  ]
 where
  fixtureCard = Aeson.object ["code" .= ("fixture-card" :: Text)]

clientAnswerFixtures :: [(FilePath, Text)]
clientAnswerFixtures =
  [ ("answer-question.json", "Answer")
  , ("answer-raw.json", "Raw")
  , ("answer-payment-amounts.json", "PaymentAmountsAnswer")
  , ("answer-amounts.json", "AmountsAnswer")
  , ("answer-standalone-settings.json", "StandaloneSettingsAnswer")
  , ("answer-campaign-settings.json", "CampaignSettingsAnswer")
  , ("answer-deck.json", "DeckAnswer")
  , ("answer-deck-list.json", "DeckListAnswer")
  , ("answer-pick-destiny.json", "PickDestinyAnswer")
  , ("answer-campaign-specific.json", "CampaignSpecificAnswer")
  , ("answer-scenario-specific.json", "ScenarioSpecificAnswer")
  , ("answer-exchange-amounts.json", "ExchangeAmountsAnswer")
  , ("answer-campaign-step.json", "CampaignStepAnswer")
  ]

answerConstructor :: Answer -> Text
answerConstructor = \case
  Answer {} -> "Answer"
  Raw {} -> "Raw"
  PaymentAmountsAnswer {} -> "PaymentAmountsAnswer"
  AmountsAnswer {} -> "AmountsAnswer"
  StandaloneSettingsAnswer {} -> "StandaloneSettingsAnswer"
  CampaignSettingsAnswer {} -> "CampaignSettingsAnswer"
  DeckAnswer {} -> "DeckAnswer"
  DeckListAnswer {} -> "DeckListAnswer"
  PickDestinyAnswer {} -> "PickDestinyAnswer"
  CampaignSpecificAnswer {} -> "CampaignSpecificAnswer"
  ScenarioSpecificAnswer {} -> "ScenarioSpecificAnswer"
  ExchangeAmountsAnswer {} -> "ExchangeAmountsAnswer"
  CampaignStepAnswer {} -> "CampaignStepAnswer"

spec :: Spec
spec = describe "Native client contract fixtures" do
  it "matches the real game-step encoder" do
    fixture <- loadFixture "game-step.json"

    Aeson.toJSON (GameStepJson 42) `shouldBe` fixture

  for_ serverMessageFixtures \(fileName, response) ->
    it ("matches the real server encoder for " <> fileName) do
      fixture <- loadFixture fileName

      Aeson.toJSON response `shouldBe` fixture

  for_ clientAnswerFixtures \(fileName, expectedConstructor) ->
    it ("decodes the real client answer for " <> fileName) do
      fixture <- loadFixture fileName

      case Aeson.fromJSON fixture of
        Aeson.Error err -> expectationFailure $ "Could not decode " <> fileName <> ": " <> err
        Aeson.Success answer -> answerConstructor answer `shouldBe` expectedConstructor
