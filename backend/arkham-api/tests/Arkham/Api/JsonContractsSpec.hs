module Arkham.Api.JsonContractsSpec (spec) where

import Api.Arkham.Deck (deckFromCreateRequest)
import Api.Arkham.Helpers (ApiResponse (..))
import Api.Arkham.Types.Achievement
import Api.Arkham.Types.Deck
import Api.Arkham.Types.Game
import Api.Arkham.Types.GameStep (GameStepJson (..))
import Api.Arkham.Types.MultiplayerVariant (MultiplayerVariant (Solo, WithFriends))
import Arkham.Achievement.Types
  ( Achievement (NightOfTheZealotAchievement, TheDunwichLegacyAchievement)
  , NightOfTheZealotAchievement (TheZealotsRevenge)
  , TheDunwichLegacyAchievement (TheGangsAllHere)
  )
import Arkham.Asset.Cards qualified as AssetCards
import Arkham.Campaigns.TheDreamEaters.Meta (CampaignPart (TheDreamQuest))
import Arkham.ClassSymbol (ClassSymbol (Guardian, Rogue, Seeker))
import Arkham.Difficulty (Difficulty (Easy))
import Arkham.Decklist (ArkhamDBDecklist (..))
import Arkham.Epic.Types (SharedEventState (..))
import Arkham.Game.State (GameState (IsActive, IsChooseDecks, IsOver, IsPending))
import Arkham.Homebrew.DarkMatter.CardDefs.Enemies qualified as DarkMatterCards
import Arkham.Investigator.Cards qualified as InvestigatorCards
import Arkham.Name (mkName)
import Base.Api.Types.Account
import Data.Aeson qualified as Aeson
import Data.Aeson.Key qualified as AesonKey
import Data.Aeson.KeyMap qualified as AesonKeyMap
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Data.Text qualified as Text
import Data.Time (secondsToDiffTime)
import Data.UUID qualified as UUID
import Database.Persist qualified as Persist
import Database.Persist.Sql (toSqlKey)
import Entity.Answer (Answer (..))
import Entity.Arkham.Achievement qualified as AchievementEntity
import Entity.Arkham.Deck qualified as DeckEntity
import Entity.Arkham.Game qualified as ArkhamGame
import Entity.Notification (Notification (..))
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

loadFixtureField :: Aeson.FromJSON a => FilePath -> Text -> IO a
loadFixtureField fileName fieldName = do
  fixture <- loadFixture fileName
  fieldValue <- case fixture of
    Aeson.Object fields ->
      maybe
        (fail $ "Missing field " <> Text.unpack fieldName <> " in " <> fileName)
        pure
        (AesonKeyMap.lookup (AesonKey.fromText fieldName) fields)
    _ -> fail $ "Expected an object fixture in " <> fileName
  case Aeson.fromJSON fieldValue of
    Aeson.Error err ->
      fail
        $ "Could not decode "
        <> Text.unpack fieldName
        <> " from "
        <> fileName
        <> ": "
        <> err
    Aeson.Success value -> pure value

fixtureGameId :: ArkhamGame.ArkhamGameId
fixtureGameId = ArkhamGame.ArkhamGameKey $ UUID.fromWords 0 0 0 3

fixtureCampaignGameId :: ArkhamGame.ArkhamGameId
fixtureCampaignGameId = ArkhamGame.ArkhamGameKey $ UUID.fromWords 0 0 0 4

fixtureActiveGameId :: ArkhamGame.ArkhamGameId
fixtureActiveGameId = ArkhamGame.ArkhamGameKey $ UUID.fromWords 0 0 0 5

fixtureCompletedGameId :: ArkhamGame.ArkhamGameId
fixtureCompletedGameId = ArkhamGame.ArkhamGameKey $ UUID.fromWords 0 0 0 6

fixtureAchievements :: [Persist.Entity AchievementEntity.ArkhamAchievement]
fixtureAchievements =
  [ Persist.Entity
      (AchievementEntity.ArkhamAchievementKey $ UUID.fromWords 0 0 0 21)
      ( AchievementEntity.ArkhamAchievement
          (toSqlKey 7)
          (NightOfTheZealotAchievement TheZealotsRevenge)
          (Just $ UTCTime (fromGregorian 2026 2 3) (secondsToDiffTime 14706))
          (Just fixtureGameId)
          (Aeson.object ["count" .= (3 :: Int)])
      )
  , Persist.Entity
      (AchievementEntity.ArkhamAchievementKey $ UUID.fromWords 0 0 0 22)
      ( AchievementEntity.ArkhamAchievement
          (toSqlKey 7)
          (TheDunwichLegacyAchievement TheGangsAllHere)
          Nothing
          Nothing
          (Aeson.object ["DrHenryArmitage" .= True])
      )
  ]

fixtureDeckList :: ArkhamDBDecklist
fixtureDeckList =
  ArkhamDBDecklist
    { slots = Map.fromList [("01016", 2), ("01018", 1)]
    , sideSlots = mempty
    , investigator_code = "01001"
    , investigator_name = "Roland Banks"
    , meta = Just "{\"alternate_front\":\"c90001\"}"
    , taboo_id = Nothing
    , url = Just "https://arkhamdb.com/decklist/view/4242"
    , decklist_id = Just "4242.0"
    , decklist_name = Just "Contract deck"
    }

fixtureDeck :: Persist.Entity DeckEntity.ArkhamDeck
fixtureDeck =
  Persist.Entity
    (DeckEntity.ArkhamDeckKey $ UUID.fromWords 0 0 0 23)
    (deckFromCreateRequest (toSqlKey 7) fixtureCreateDeckRequest)

fixtureCreateDeckRequest :: CreateDeckRequest
fixtureCreateDeckRequest =
  CreateDeckRequest
    "external-4242"
    "Contract deck"
    (Just "https://arkhamdb.com/decklist/view/4242")
    fixtureDeckList

fixturePlayerId :: PlayerId
fixturePlayerId = PlayerId $ UUID.fromWords 0 0 0 1

fixtureGame :: Game
fixtureGame =
  (newScenario "01104" 1729 1 Easy False)
    { gameGitRevision = "contract-fixture"
    }

fixturePublicGame :: PublicGame ArkhamGame.ArkhamGameId
fixturePublicGame =
  PublicGame
    fixtureGameId
    "Contract fixture game"
    ["Contract fixture log entry."]
    fixtureGame

serverMessageFixtures :: [(FilePath, ApiResponse)]
serverMessageFixtures =
  [ ( "game-update.json"
    , GameUpdate fixturePublicGame
    )
  , ("game-message.json", GameMessage "A contract fixture message.")
  , ("game-error.json", GameError "The question changed before this answer arrived.")
  , ("game-ui.json", GameUI "contract:ui")
  , ("game-audio.json", GameAudio "contract.ogg")
  , ("game-card.json", GameCard "Contract card" fixtureCard)
  , ( "game-card-only.json"
    , GameCardOnly fixturePlayerId "Private contract card" fixtureCard
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

fixtureGetGame :: GetGameJson
fixtureGetGame =
  GetGameJson
    (Just fixturePlayerId)
    Solo
    fixturePublicGame
    Nothing

fixtureGameList :: [GameDetailsEntry]
fixtureGameList =
  [ SuccessGameDetails
      $ GameDetails
        fixtureGameId
        (Just $ ScenarioDetails "01104" Easy (mkName "The Gathering") Nothing)
        Nothing
        (IsPending [])
        "Contract fixture game"
        []
        []
        Solo
        False
  , SuccessGameDetails
      $ GameDetails
        fixtureCampaignGameId
        Nothing
        (Just $ CampaignDetails "06" Easy (Just TheDreamQuest))
        (IsChooseDecks [fixturePlayerId])
        "Campaign contract fixture"
        [InvestigatorDetails "06001" Guardian, InvestigatorDetails "06002" Seeker]
        [InvestigatorDetails "06003" Rogue]
        WithFriends
        True
  , SuccessGameDetails
      $ GameDetails
        fixtureActiveGameId
        (Just $ ScenarioDetails "01104" Easy (mkName "The Gathering") Nothing)
        Nothing
        IsActive
        "Active contract fixture"
        []
        []
        Solo
        False
  , SuccessGameDetails
      $ GameDetails
        fixtureCompletedGameId
        (Just $ ScenarioDetails "01104" Easy (mkName "The Gathering") Nothing)
        Nothing
        IsOver
        "Completed contract fixture"
        []
        []
        Solo
        False
  , FailedGameDetails "Contract fixture failed to load."
  ]

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
  Answer _ -> "Answer"
  Raw _ -> "Raw"
  PaymentAmountsAnswer _ -> "PaymentAmountsAnswer"
  AmountsAnswer _ -> "AmountsAnswer"
  StandaloneSettingsAnswer _ -> "StandaloneSettingsAnswer"
  CampaignSettingsAnswer _ -> "CampaignSettingsAnswer"
  DeckAnswer _ _ -> "DeckAnswer"
  DeckListAnswer _ _ -> "DeckListAnswer"
  PickDestinyAnswer _ -> "PickDestinyAnswer"
  CampaignSpecificAnswer _ _ -> "CampaignSpecificAnswer"
  ScenarioSpecificAnswer _ _ -> "ScenarioSpecificAnswer"
  ExchangeAmountsAnswer _ _ _ _ _ -> "ExchangeAmountsAnswer"
  CampaignStepAnswer _ -> "CampaignStepAnswer"

spec :: Spec
spec = describe "Native client contract fixtures" do
  it "matches the real game-step encoder" do
    fixture <- loadFixture "game-step.json"

    Aeson.toJSON (GameStepJson 42) `shouldBe` fixture

  for_ serverMessageFixtures \(fileName, response) ->
    it ("matches the real server encoder for " <> fileName) do
      fixture <- loadFixture fileName

      Aeson.toJSON response `shouldBe` fixture

  it "matches the real game-list encoder" do
    fixture <- loadFixture "game-list.json"

    Aeson.toJSON fixtureGameList `shouldBe` fixture

  it "matches the real get-game encoder" do
    fixture <- loadFixture "get-game.json"

    Aeson.toJSON fixtureGetGame `shouldBe` fixture

  it "decodes the real password-reset request" do
    request <-
      loadFixtureField "account.json" "passwordResetRequest"
        :: IO PasswordResetRequest

    request.resetEmail `shouldBe` "investigator@example.com"

  it "decodes the real password-reset update" do
    request <-
      loadFixtureField "account.json" "passwordResetUpdate"
        :: IO PasswordResetUpdate

    request.resetPassword `shouldBe` "new passphrase"

  it "decodes the real settings update" do
    settings <-
      loadFixtureField "account.json" "userSettings"
        :: IO UserSettings

    settings.betaSetting `shouldBe` True

  it "matches the real settings response encoder" do
    fixture <- loadFixtureField "account.json" "settingsUser"

    Aeson.toJSON (SettingsUser "Investigator" "investigator@example.com" True)
      `shouldBe` fixture

  it "matches the real notification-list encoder" do
    fixture <- loadFixtureField "account.json" "notifications"
    let
      notification =
        Persist.Entity
          (toSqlKey 17)
          ( Notification
              "Contract fixture announcement."
              (UTCTime (fromGregorian 2026 1 2) (secondsToDiffTime 11045))
          )

    Aeson.toJSON [notification] `shouldBe` fixture

  it "matches the real card-list encoder" do
    fixture <- loadFixtureField "catalog.json" "cards"

    Aeson.toJSON [AssetCards.machete] `shouldBe` fixture

  it "matches the real homebrew-card-list encoder" do
    fixture <- loadFixtureField "catalog.json" "homebrewCards"

    Aeson.toJSON [DarkMatterCards.rats] `shouldBe` fixture

  it "matches the real card-detail encoder" do
    fixture <- loadFixtureField "catalog.json" "card"

    Aeson.toJSON InvestigatorCards.rolandBanks `shouldBe` fixture

  it "matches the real investigator-artwork encoder" do
    fixture <- loadFixtureField "catalog.json" "investigators"
    let
      artwork =
        map cdArt [InvestigatorCards.rolandBanks, InvestigatorCards.daisyWalker]

    Aeson.toJSON artwork `shouldBe` fixture

  it "matches the real achievement-list encoder" do
    fixture <- loadFixtureField "achievements.json" "achievements"

    Aeson.toJSON fixtureAchievements `shouldBe` fixture

  it "normalizes the real imported-deck decoder through its encoder" do
    importedDeckList <-
      loadFixtureField "decks.json" "validateDeckList"
        :: IO ArkhamDBDecklist
    normalizedDeckList <- loadFixtureField "decks.json" "normalizedDeckList"

    importedDeckList `shouldBe` fixtureDeckList
    Aeson.toJSON importedDeckList `shouldBe` normalizedDeckList

  it "decodes the real create-deck request" do
    request <-
      loadFixtureField "decks.json" "createDeck"
        :: IO CreateDeckRequest

    request `shouldBe` fixtureCreateDeckRequest

  it "decodes the real fetch-deck request" do
    request <-
      loadFixtureField "decks.json" "fetchDeck"
        :: IO FetchDeckRequest

    request `shouldBe` FetchDeckRequest "https://arkhamdb.com/decklist/view/4242"

  it "matches the real saved-deck encoder" do
    fixture <- loadFixtureField "decks.json" "deck"

    Aeson.toJSON fixtureDeck `shouldBe` fixture

  it "matches the real deck-validation error encoder" do
    fixture <- loadFixtureField "decks.json" "validationErrors"
    let errors = [UnimplementedCard "99999"]

    Aeson.toJSON errors `shouldBe` fixture

  it "matches the real deck-validation success encoder" do
    fixture <- loadFixtureField "decks.json" "validationSuccess"

    Aeson.toJSON () `shouldBe` fixture

  it "matches the real deck-operation error encoder" do
    fixture <- loadFixtureField "decks.json" "operationError"

    Aeson.toJSON (DeckOperationError "Could not sync deck") `shouldBe` fixture

  for_
    ( [ ("clearAll", ClearAll)
      , ("clearCampaign", ClearCampaign "51")
      , ("clearAchievement", ClearAchievement $ NightOfTheZealotAchievement TheZealotsRevenge)
      ]
        :: [(Text, ClearAchievements)]
    )
    \(fieldName, expectedRequest) ->
      it ("decodes the real achievement clear request for " <> Text.unpack fieldName) do
        request <-
          loadFixtureField "achievements.json" fieldName
            :: IO ClearAchievements

        request `shouldBe` expectedRequest

  for_ clientAnswerFixtures \(fileName, expectedConstructor) ->
    it ("decodes the real client answer for " <> fileName) do
      fixture <- loadFixture fileName

      case Aeson.fromJSON fixture of
        Aeson.Error err -> expectationFailure $ "Could not decode " <> fileName <> ": " <> err
        Aeson.Success answer -> answerConstructor answer `shouldBe` expectedConstructor
