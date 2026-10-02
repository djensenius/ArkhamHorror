{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Arkham.Coverage.NightOfTheZealotSpec (spec) where

import Api.Arkham.Helpers (GameApp (..), runGameApp)
import Arkham.Campaign.Types (campaignResolutions, campaignStep)
import Arkham.CampaignStep qualified as CS
import Arkham.Card.CardCode (CardCode (..), unCardCode)
import Arkham.Classes.Entity (toAttrs)
import Arkham.Classes.HasQueue (newQueue)
import Arkham.Decklist.Type qualified as Decklist
import Arkham.Difficulty (Difficulty (Easy))
import Arkham.Game (Game (..), newCampaign, runMessages)
import Arkham.Game qualified as Game
import Arkham.Game.State (GameState (IsOver))
import Arkham.Game.Utils (modeCampaign, modeScenario)
import Arkham.Id (CampaignId (..), InvestigatorId (..), PlayerId (..), ScenarioId (..), unInvestigatorId, unScenarioId)
import Arkham.Message (Message (ClearUI, DoneChoosingDecks, LoadDecklist, SetActivePlayer))
import Arkham.PlayerCard (allPlayerCards)
import Arkham.Prelude
import Arkham.Question
import Arkham.Question.Presentation qualified as QuestionPresentation
import Arkham.Queue (queueToRef)
import Arkham.Replay.Checkpoint (prependReplayAnswerMessages)
import Arkham.Scenario.Types (scenarioId)
import Arkham.Source (Source (GameSource))
import Arkham.Token (Token (Resource))
import Control.Exception qualified as Exception
import Control.Monad.Random (mkStdGen)
import Data.Aeson (Result (..), Value (..), encode, fromJSON, object, toJSON, (.=))
import Data.Aeson.Key qualified as Key
import Data.Aeson.KeyMap qualified as KeyMap
import Data.Aeson.Types (Pair)
import Data.ByteString.Lazy.Char8 qualified as BL8
import Data.Map.Strict qualified as Map
import Data.Text qualified as T
import Data.UUID qualified as UUID
import Entity.Answer
import System.Directory (createDirectoryIfMissing)
import System.Environment (lookupEnv)
import System.Timeout qualified as Timeout
import Test.Hspec

spec :: Spec
spec = describe "Night of the Zealot coverage generator" do
  describe "coverage answer encoding" do
    for_ answerEncodingExamples \(label, answer) ->
      it ("round-trips " <> label <> " through the server Answer parser") do
        assertAnswerRoundTrip answer (answerToJSON answer) `shouldBe` Right ()

  describe "core starter decks" do
    it "uses legal 30-card ordinary decks plus required cards and one random basic weakness" do
      traverse_ assertLegalStarterDeck coreInvestigators

  describe "coverage answer replay" do
    it "uses the replay helper to prepend answers before parked continuation messages" do
      let answerMessage = LoadDecklist samplePlayerId (starterDeck sampleInvestigator)
      prependReplayAnswerMessages [ClearUI, answerMessage] [DoneChoosingDecks]
        `shouldBe` [ClearUI, answerMessage, DoneChoosingDecks]

  outputDir <- runIO $ lookupEnv "ARKHAM_NOTZ_COVERAGE_DIR"
  case outputDir of
    Nothing ->
      it "runs only when ARKHAM_NOTZ_COVERAGE_DIR is set" do
        pendingWith "set ARKHAM_NOTZ_COVERAGE_DIR to generate coverage JSONL fixtures"
    Just dir ->
      it "records a deterministic solo campaign bot run for each core investigator" do
        results <- traverse (runInvestigatorCoverage dir) coreInvestigators
        writeSummary dir results
        for_ results \result -> do
          result.crStop.stopReason `shouldBe` "campaign finished"
          result.crReachedDevourerBelowEnd `shouldBe` True

coreInvestigators :: [InvestigatorSpec]
coreInvestigators =
  [ investigator "01001" "Roland Banks" 12001 rolandStarterDeck
  , investigator "01002" "Daisy Walker" 12002 daisyStarterDeck
  , investigator "01003" "Skids O'Toole" 12003 skidsStarterDeck
  , investigator "01004" "Agnes Baker" 12004 agnesStarterDeck
  , investigator "01005" "Wendy Adams" 12005 wendyStarterDeck
  ]
 where
  investigator iid name seed deck = InvestigatorSpec iid name seed deck (basicWeaknessFor seed)

data InvestigatorSpec = InvestigatorSpec
  { isInvestigatorId :: InvestigatorId
  , isInvestigatorName :: Text
  , isSeed :: Int
  , isDeckSlots :: Map CardCode Int
  , isBasicWeakness :: CardCode
  }

starterDeck :: InvestigatorSpec -> Decklist.ArkhamDBDecklist
starterDeck InvestigatorSpec {..} =
  Decklist.ArkhamDBDecklist
    { Decklist.slots = Map.insertWith (+) isBasicWeakness 1 isDeckSlots
    , Decklist.sideSlots = mempty
    , Decklist.investigator_code = isInvestigatorId
    , Decklist.investigator_name = isInvestigatorName
    , Decklist.meta = Nothing
    , Decklist.taboo_id = Nothing
    , Decklist.url = Nothing
    , Decklist.decklist_id = Just $ "core-starter-" <> unCardCode (unInvestigatorId isInvestigatorId)
    , Decklist.decklist_name = Just $ isInvestigatorName <> " core starter"
    }

oneEach :: [CardCode] -> Map CardCode Int
oneEach = Map.fromList . map (,1)

addSecondCopies :: [CardCode] -> Map CardCode Int -> Map CardCode Int
addSecondCopies secondCopies deck = foldl' (\acc cardCode -> Map.insertWith (+) cardCode 1 acc) deck secondCopies

coreStarterDeck :: [CardCode] -> [CardCode] -> [CardCode] -> Map CardCode Int
coreStarterDeck requiredCards ordinaryCards secondCopies =
  addSecondCopies secondCopies $ oneEach $ requiredCards <> ordinaryCards

{- | Legal NOTZ coverage decks, not the printed core starter lists. Each uses two
complete level-0 class sets, the neutral core cards, signature/personal weakness,
and two investigator-specific extra copies:
Roland: Physical Training, Machete; Daisy: Old Book of Lore, Dr. Milan Christopher;
Skids: .41 Derringer, Leo De Luca; Agnes: Holy Rosary, Shrivelling;
Wendy: Leo De Luca, Hard Knocks.
-}
neutralCore :: [CardCode]
neutralCore = ["01086", "01087", "01088", "01089", "01090", "01091", "01092", "01093"]

guardian0, seeker0, rogue0, mystic0, survivor0 :: [CardCode]
guardian0 = ["01016", "01017", "01018", "01019", "01020", "01021", "01022", "01023", "01024", "01025"]
seeker0 = ["01030", "01031", "01032", "01033", "01034", "01035", "01036", "01037", "01038", "01039"]
rogue0 = ["01044", "01045", "01046", "01047", "01048", "01049", "01050", "01051", "01052", "01053"]
mystic0 = ["01058", "01059", "01060", "01061", "01062", "01063", "01064", "01065", "01066", "01067"]
survivor0 = ["01072", "01073", "01074", "01075", "01076", "01077", "01078", "01079", "01080", "01081"]

coreBasicWeaknesses :: [CardCode]
coreBasicWeaknesses = ["01096", "01097", "01098", "01099", "01100", "01101", "01102", "01103"]

basicWeaknessFor :: Int -> CardCode
basicWeaknessFor seed = fromMaybe "01096" $ coreBasicWeaknesses !!? (seed `mod` length coreBasicWeaknesses)

rolandStarterDeck, daisyStarterDeck, skidsStarterDeck, agnesStarterDeck, wendyStarterDeck :: Map CardCode Int
rolandStarterDeck = coreStarterDeck ["01006", "01007"] (guardian0 <> seeker0 <> neutralCore) ["01017", "01020"]
daisyStarterDeck = coreStarterDeck ["01008", "01009"] (seeker0 <> mystic0 <> neutralCore) ["01031", "01033"]
skidsStarterDeck = coreStarterDeck ["01010", "01011"] (rogue0 <> guardian0 <> neutralCore) ["01047", "01048"]
agnesStarterDeck = coreStarterDeck ["01012", "01013"] (mystic0 <> survivor0 <> neutralCore) ["01059", "01060"]
wendyStarterDeck = coreStarterDeck ["01014", "01015"] (survivor0 <> rogue0 <> neutralCore) ["01048", "01049"]

requiredStarterCards :: InvestigatorSpec -> (CardCode, CardCode)
requiredStarterCards InvestigatorSpec {..} = case unInvestigatorId isInvestigatorId of
  "01001" -> ("01006", "01007")
  "01002" -> ("01008", "01009")
  "01003" -> ("01010", "01011")
  "01004" -> ("01012", "01013")
  "01005" -> ("01014", "01015")
  other -> error $ "unknown core starter investigator: " <> T.unpack (unCardCode other)

assertLegalStarterDeck :: InvestigatorSpec -> Expectation
assertLegalStarterDeck spec'@InvestigatorSpec {..} = do
  let (signatureCard, personalWeakness) = requiredStarterCards spec'
      ordinaryCount = sum [n | (cardCode, n) <- Map.toList isDeckSlots, cardCode /= signatureCard, cardCode /= personalWeakness]
      fullSlots = Decklist.slots $ starterDeck spec'
      unimplementedCards = filter (`Map.notMember` allPlayerCards) $ Map.keys fullSlots
  ordinaryCount `shouldBe` 30
  Map.lookup signatureCard isDeckSlots `shouldBe` Just 1
  Map.lookup personalWeakness isDeckSlots `shouldBe` Just 1
  Map.lookup isBasicWeakness fullSlots `shouldBe` Just 1
  sum (Map.elems fullSlots) `shouldBe` 33
  unimplementedCards `shouldBe` []

data CoverageResult = CoverageResult
  { crInvestigator :: InvestigatorSpec
  , crRecords :: [CoverageRecord]
  , crStop :: StopReport
  , crOutcomes :: Map Text ScenarioOutcome
  , crReachedDevourerBelowEnd :: Bool
  , crJsonlBytes :: Integer
  }

data ScenarioOutcome = ScenarioOutcome
  { soScenario :: Text
  , soStatus :: Text
  , soResolution :: Maybe Value
  }

instance ToJSON ScenarioOutcome where
  toJSON ScenarioOutcome {..} =
    object
      [ "scenario" .= soScenario
      , "status" .= soStatus
      , "resolution" .= soResolution
      ]

data CoverageRecord = CoverageRecord
  { recordScenario :: Value
  , recordScenarioKey :: Text
  , recordInvestigator :: InvestigatorId
  , recordStepIndex :: Int
  , recordQuestionVersion :: Int
  , recordPlayerId :: PlayerId
  , recordRawQuestion :: Value
  , recordQuestionPresentation :: Value
  , recordChosenAnswer :: Value
  , recordChoiceNote :: Text
  , recordChosenChoiceKind :: Maybe Text
  }

data StopReport = StopReport
  { stopReason :: Text
  , stopScenario :: Value
  , stopScenarioKey :: Text
  , stopLastQuestion :: Maybe Value
  , stopCampaignFinished :: Bool
  }

instance ToJSON CoverageRecord where
  toJSON CoverageRecord {..} =
    object
      [ "choiceNote" .= recordChoiceNote
      , "chosenAnswer" .= recordChosenAnswer
      , "chosenChoiceKind" .= recordChosenChoiceKind
      , "investigator" .= recordInvestigator
      , "playerId" .= recordPlayerId
      , "questionPresentation" .= recordQuestionPresentation
      , "questionVersion" .= recordQuestionVersion
      , "rawQuestion" .= recordRawQuestion
      , "scenario" .= recordScenario
      , "scenarioKey" .= recordScenarioKey
      , "stepIndex" .= recordStepIndex
      ]

instance ToJSON StopReport where
  toJSON StopReport {..} =
    object
      [ "campaignFinished" .= stopCampaignFinished
      , "lastQuestion" .= stopLastQuestion
      , "reason" .= stopReason
      , "scenario" .= stopScenario
      , "scenarioKey" .= stopScenarioKey
      ]

investigatorMetadata :: InvestigatorSpec -> Value
investigatorMetadata InvestigatorSpec {..} =
  object
    [ "basicWeakness" .= isBasicWeakness
    , "id" .= isInvestigatorId
    , "name" .= isInvestigatorName
    , "seed" .= isSeed
    ]

runInvestigatorCoverage :: FilePath -> InvestigatorSpec -> IO CoverageResult
runInvestigatorCoverage dir spec' = do
  createDirectoryIfMissing True dir
  result <- doRun `catchAny` \err -> pure $ emptyCoverageResult spec' ("exception before run: " <> T.pack (show err))
  bytes <- writeRecords dir result
  pure result {crJsonlBytes = bytes}
 where
  doRun = do
    let playerId = PlayerId $ UUID.fromWords 0 0 0 (fromIntegral spec'.isSeed)
        game0 = newCampaign (CampaignId "01") Nothing spec'.isSeed 1 Easy False
    gameRef <- newIORef game0
    queueRef <- newQueue []
    genRef <- newIORef $ mkStdGen spec'.isSeed
    let app = GameApp gameRef queueRef genRef (pure . const ()) Nothing
    setupResult <- tryAny do
      runGameApp app $ Game.addPlayer playerId
      drainMessages app
    case setupResult of
      Left err -> do
        game <- readIORef app.appGame
        finishWith spec' game [] ("setup failed: " <> T.pack (show err)) Nothing
      Right (Left err) -> do
        game <- readIORef app.appGame
        finishWith spec' game [] ("setup failed: " <> err) Nothing
      Right (Right ()) -> botLoop app spec' playerId 0 mempty mempty [] Nothing

botLoop
  :: GameApp
  -> InvestigatorSpec
  -> PlayerId
  -> Int
  -> Map Text Int
  -> Map Text Int
  -> [CoverageRecord]
  -> Maybe Value
  -> IO CoverageResult
botLoop app spec' playerId step seen scenarioCounts records lastQuestion = do
  result <- tryAny do
    game <- readIORef app.appGame
    let (scenarioKey, scenarioValue) = scenarioLabel game
        scenarioStepCount = Map.findWithDefault 0 scenarioKey scenarioCounts
    if scenarioStepCount >= maxStepsPerScenario
      then finishWith spec' game (reverse records) ("step cap reached for scenario " <> scenarioKey) lastQuestion
      else case Map.lookup playerId game.gameQuestion of
        Nothing -> finishWith spec' game (reverse records) (noQuestionReason game) lastQuestion
        Just question -> do
          let qVersion = game.gameScenarioSteps
              presentation = QuestionPresentation.questionPresentation qVersion question
              presentationValue = canonicalValue $ toJSON presentation
              rawQuestion = canonicalValue $ toJSON question
              lastQuestion' = Just $ smallQuestionSummary scenarioKey scenarioValue qVersion presentationValue rawQuestion
              seenKey = scenarioKey <> ":" <> compactText rawQuestion <> ":" <> compactText (withoutQuestionVersion presentationValue)
              repeatCount = Map.findWithDefault 0 seenKey seen
          case selectAnswer spec' playerId game question presentationValue repeatCount of
            Left reason -> finishWith spec' game (reverse records) reason lastQuestion'
            Right selected -> do
              let answerJson = canonicalValue $ answerToJSON selected.answerValue
              case assertAnswerRoundTrip selected.answerValue answerJson of
                Left reason -> finishWith spec' game (reverse records) reason lastQuestion'
                Right () -> do
                  let record =
                        CoverageRecord
                          { recordScenario = scenarioValue
                          , recordScenarioKey = scenarioKey
                          , recordInvestigator = spec'.isInvestigatorId
                          , recordStepIndex = step
                          , recordQuestionVersion = qVersion
                          , recordPlayerId = playerId
                          , recordRawQuestion = rawQuestion
                          , recordQuestionPresentation = presentationValue
                          , recordChosenAnswer = answerJson
                          , recordChoiceNote = selected.note
                          , recordChosenChoiceKind = selected.chosenChoiceKind
                          }
                      records' = record : records
                      scenarioCounts' = Map.insertWith (+) scenarioKey 1 scenarioCounts
                  applySelectedAnswer app playerId game selected >>= \case
                    Left reason -> finishWith spec' game (reverse records') reason lastQuestion'
                    Right () -> botLoop app spec' playerId (step + 1) (Map.insert seenKey (repeatCount + 1) seen) scenarioCounts' records' lastQuestion'
  case result of
    Left err -> do
      game <- readIORef app.appGame
      finishWith spec' game (reverse records) ("exception during run: " <> T.pack (show err)) lastQuestion
    Right coverage -> pure coverage
 where
  maxStepsPerScenario = 2500

smallQuestionSummary :: Text -> Value -> Int -> Value -> Value -> Value
smallQuestionSummary scenarioKey scenarioValue questionVersion presentation rawQuestion =
  object
    [ "questionKind" .= questionKind presentation
    , "questionVersion" .= questionVersion
    , "rawQuestionTag" .= rawQuestionTag rawQuestion
    , "scenario" .= scenarioValue
    , "scenarioKey" .= scenarioKey
    ]

noQuestionReason :: Game -> Text
noQuestionReason game
  | campaignFinished game = "campaign finished"
  | otherwise = "stuck: no pending question for investigator"

finishWith :: InvestigatorSpec -> Game -> [CoverageRecord] -> Text -> Maybe Value -> IO CoverageResult
finishWith spec' game records reason lastQuestion = do
  let outcomes = scenarioOutcomes game
      reachedEnd = maybe False (isJust . soResolution) (Map.lookup "01142" outcomes)
      (_, scenarioValue) = scenarioLabel game
      (scenarioKey, _) = scenarioLabel game
  pure
    CoverageResult
      { crInvestigator = spec'
      , crRecords = records
      , crStop = StopReport reason scenarioValue scenarioKey lastQuestion (campaignFinished game)
      , crOutcomes = outcomes
      , crReachedDevourerBelowEnd = reachedEnd
      , crJsonlBytes = 0
      }

emptyCoverageResult :: InvestigatorSpec -> Text -> CoverageResult
emptyCoverageResult spec' reason =
  CoverageResult
    { crInvestigator = spec'
    , crRecords = []
    , crStop = StopReport reason (object ["kind" .= ("not-started" :: Text)]) "not-started" Nothing False
    , crOutcomes = mempty
    , crReachedDevourerBelowEnd = False
    , crJsonlBytes = 0
    }

data SelectedAnswer = SelectedAnswer
  { answerValue :: Answer
  , answerMessages :: Maybe [Message]
  , note :: Text
  , chosenChoiceKind :: Maybe Text
  }

applySelectedAnswer :: GameApp -> PlayerId -> Game -> SelectedAnswer -> IO (Either Text ())
applySelectedAnswer app defaultPlayer game SelectedAnswer {..} = do
  result <- tryAny do
    messages <- case answerMessages of
      Just msgs -> pure msgs
      Nothing -> do
        let answerPid = fromMaybe defaultPlayer (answerPlayer answerValue)
        handleAnswerPure game answerPid answerValue >>= \case
          Unhandled reason -> Exception.throwIO $ userError $ T.unpack reason
          Handled msgs -> pure msgs
    let answerPid = fromMaybe defaultPlayer (answerPlayer answerValue)
        activePid = game.gameActivePlayerId
        bracketed =
          [SetActivePlayer answerPid | activePid /= answerPid]
            <> messages
            <> [SetActivePlayer activePid | activePid /= answerPid]
    atomicModifyIORef' (queueToRef app.appQueue) \q ->
      (prependReplayAnswerMessages (ClearUI : bracketed) q, ())
    drainMessages app
  pure case result of
    Left err -> Left $ "answer failed: " <> T.pack (show err)
    Right (Left err) -> Left err
    Right (Right ()) -> Right ()


-- Keep each drain bounded so an engine loop records a stop instead of hanging
-- the nightly/manual coverage job indefinitely.
drainMessages :: GameApp -> IO (Either Text ())
drainMessages app = do
  result <- tryAny $ Timeout.timeout (30 * 1000 * 1000) $ runGameApp app (runMessages "notz-coverage" Nothing)
  pure case result of
    Left err -> Left $ "message processing threw: " <> T.pack (show err)
    Right Nothing -> Left "message processing timed out after 30 seconds"
    Right (Just ()) -> Right ()

selectAnswer :: InvestigatorSpec -> PlayerId -> Game -> Question Message -> Value -> Int -> Either Text SelectedAnswer
selectAnswer spec' playerId game question presentation repeatCount = case stripQuestion question of
  ChooseDeck -> Right $ deckListAnswer "starter deck"
  ChooseUpgradeDeck -> Right $ deckListAnswer "continue without upgrading"
  ChooseJoinDeck {} -> Right $ deckListAnswer "join with starter deck"
  PickScenarioSettings -> Left "stuck: scenario settings prompt is not part of Night of the Zealot coverage"
  PickCampaignSettings -> Left "stuck: campaign settings prompt is not part of Night of the Zealot coverage"
  PickCampaignSpecific key value -> Right $ answerOnly (CampaignSpecificAnswer key value) "echo campaign-specific value"
  PickScenarioSpecific key value -> Right $ answerOnly (ScenarioSpecificAnswer key value) "echo scenario-specific value"
  ChooseAmounts _ target choices _ ->
    Right $ answerOnly (AmountsAnswer $ AmountsResponse (minimumAmounts target choices) (Just game.gameScenarioSteps) (Just playerId)) "minimum legal amounts"
  ChoosePaymentAmounts _ target choices ->
    Right $ answerOnly (PaymentAmountsAnswer $ PaymentAmountsResponse (minimumPaymentAmounts target choices) (Just game.gameScenarioSteps) (Just playerId)) "minimum legal payment amounts"
  ChooseExchangeAmounts source iid1 _ iid2 _ token ->
    Right $ answerOnly (ExchangeAmountsAnswer source iid1 iid2 token 0) "exchange 0"
  ContinueCampaign ->
    Right $ answerOnly (CampaignStepAnswer $ nextCampaignAnswer game) "continue with current server campaign step"
  PickDestiny drawings -> Right $ answerOnly (PickDestinyAnswer drawings) "keep destiny drawing order"
  _ -> choiceAnswer
 where
  deck = starterDeck spec'
  deckListAnswer note' =
    SelectedAnswer
      { answerValue = DeckListAnswer deck playerId
      , answerMessages = Just $ deckChosen game playerId deck
      , note = note'
      , chosenChoiceKind = Nothing
      }
  answerOnly answer note' = SelectedAnswer answer Nothing note' Nothing
  choiceAnswer = case selectableIndexes presentation of
    [] -> Left $ "stuck: no selectable choices for " <> fromMaybe "unknown" (questionKind presentation)
    indexes@(firstChoice : _) ->
      let choice = fromMaybe firstChoice $ indexes !!? (repeatCount `mod` length indexes)
       in Right
            SelectedAnswer
              { answerValue = Answer $ QuestionResponse choice (Just playerId) (Just game.gameScenarioSteps)
              , answerMessages = Nothing
              , note = "selectable choice " <> tshow choice
              , chosenChoiceKind = choiceKindAt choice presentation
              }

assertAnswerRoundTrip :: Answer -> Value -> Either Text ()
assertAnswerRoundTrip answer answerJson = case fromJSON @Answer answerJson of
  Error err -> Left $ "chosenAnswer did not parse as server Answer: " <> T.pack err
  Success decoded
    | show decoded == show answer -> Right ()
    | otherwise -> Left "chosenAnswer parsed to a different Answer than the bot applied"

stripQuestion :: Question msg -> Question msg
stripQuestion = \case
  QuestionLabel _ _ q -> stripQuestion q
  PayCostQuestion _ q -> stripQuestion q
  QuestionWithSource _ _ q -> stripQuestion q
  q -> q

selectableIndexes :: Value -> [Int]
selectableIndexes = \case
  Object o -> case KeyMap.lookup "choices" o of
    Just (Array choices) ->
      [ i
      | (i, Object choice) <- zip [0 ..] (toList choices)
      , KeyMap.lookup "selectable" choice == Just (Bool True)
      ]
    _ -> []
  _ -> []

questionKind :: Value -> Maybe Text
questionKind = \case
  Object o -> case KeyMap.lookup "questionKind" o of
    Just (String kind) -> Just kind
    _ -> Nothing
  _ -> Nothing

rawQuestionTag :: Value -> Maybe Text
rawQuestionTag = \case
  Object o -> case KeyMap.lookup "tag" o of
    Just (String tag) -> Just tag
    _ -> Nothing
  _ -> Nothing

choiceKinds :: Value -> [Text]
choiceKinds = \case
  Object o -> case KeyMap.lookup "choices" o of
    Just (Array choices) -> nub $ sort [kind | Object choice <- toList choices, Just (String kind) <- [KeyMap.lookup "kind" choice]]
    _ -> []
  _ -> []

choiceKindAt :: Int -> Value -> Maybe Text
choiceKindAt choiceIndex = \case
  Object o -> do
    Array choices <- KeyMap.lookup "choices" o
    Object choice <- toList choices !!? choiceIndex
    case KeyMap.lookup "kind" choice of
      Just (String kind) -> Just kind
      _ -> Nothing
  _ -> Nothing

withoutQuestionVersion :: Value -> Value
withoutQuestionVersion = \case
  Object o -> Object $ KeyMap.delete "questionVersion" o
  other -> other

minimumAmounts :: AmountTarget -> [AmountChoice] -> Map UUID.UUID Int
minimumAmounts target choices = allocateTo (targetAmount target choices) choices

minimumPaymentAmounts :: Maybe AmountTarget -> [PaymentAmountChoice Message] -> Map UUID.UUID Int
minimumPaymentAmounts target choices = allocatePaymentTo (maybe 0 (`paymentTargetAmount` choices) target) choices

targetAmount :: AmountTarget -> [AmountChoice] -> Int
targetAmount target choices = case target of
  MinAmountTarget n -> max n minTotal
  MaxAmountTarget _ -> minTotal
  TotalAmountTarget n -> n
  AmountOneOf ns -> fromMaybe minTotal $ find (>= minTotal) (sort ns)
 where
  minTotal = sum $ map (.minBound) choices

paymentTargetAmount :: AmountTarget -> [PaymentAmountChoice Message] -> Int
paymentTargetAmount target choices = case target of
  MinAmountTarget n -> max n minTotal
  MaxAmountTarget _ -> minTotal
  TotalAmountTarget n -> n
  AmountOneOf ns -> fromMaybe minTotal $ find (>= minTotal) (sort ns)
 where
  minTotal = sum $ map (.minBound) choices

allocateTo :: Int -> [AmountChoice] -> Map UUID.UUID Int
allocateTo target choices = Map.fromList $ go (max 0 $ target - sum (map (.minBound) choices)) choices
 where
  go _ [] = []
  go remaining (choice : rest) =
    let extra = min remaining (choice.maxBound - choice.minBound)
        amount = choice.minBound + extra
     in (choice.choiceId, amount) : go (remaining - extra) rest

allocatePaymentTo :: Int -> [PaymentAmountChoice Message] -> Map UUID.UUID Int
allocatePaymentTo target choices = Map.fromList $ go (max 0 $ target - sum (map (.minBound) choices)) choices
 where
  go _ [] = []
  go remaining (choice : rest) =
    let extra = min remaining (choice.maxBound - choice.minBound)
        amount = choice.minBound + extra
     in (choice.choiceId, amount) : go (remaining - extra) rest

samplePlayerId :: PlayerId
samplePlayerId = PlayerId $ UUID.fromWords 0 0 0 1

sampleInvestigator :: InvestigatorSpec
sampleInvestigator = fromMaybe (error "coreInvestigators is unexpectedly empty") $ headMay coreInvestigators

answerEncodingExamples :: [(String, Answer)]
answerEncodingExamples =
  [ ("Answer", Answer $ QuestionResponse 0 (Just samplePlayerId) (Just 7))
  , ("AmountsAnswer", AmountsAnswer $ AmountsResponse mempty (Just 7) (Just samplePlayerId))
  , ("PaymentAmountsAnswer", PaymentAmountsAnswer $ PaymentAmountsResponse mempty (Just 7) (Just samplePlayerId))
  , ("DeckListAnswer", DeckListAnswer (starterDeck sampleInvestigator) samplePlayerId)
  , ("CampaignSpecificAnswer", CampaignSpecificAnswer "fixture" Null)
  , ("ScenarioSpecificAnswer", ScenarioSpecificAnswer "fixture" Null)
  , ("ExchangeAmountsAnswer", ExchangeAmountsAnswer GameSource "01001" "01002" Resource 0)
  , ("CampaignStepAnswer", CampaignStepAnswer $ CS.ScenarioStep "01104")
  , ("PickDestinyAnswer", PickDestinyAnswer [])
  ]

answerToJSON :: Answer -> Value
answerToJSON = \case
  Answer QuestionResponse {..} ->
    taggedContents
      "Answer"
      [ "choice" .= qrChoice
      , "playerId" .= qrPlayerId
      , "questionVersion" .= qrQuestionVersion
      ]
  PaymentAmountsAnswer PaymentAmountsResponse {..} ->
    taggedContents
      "PaymentAmountsAnswer"
      [ "amounts" .= parAmounts
      , "playerId" .= parPlayerId
      , "questionVersion" .= parQuestionVersion
      ]
  AmountsAnswer AmountsResponse {..} ->
    taggedContents
      "AmountsAnswer"
      [ "amounts" .= arAmounts
      , "playerId" .= arPlayerId
      , "questionVersion" .= arQuestionVersion
      ]
  DeckListAnswer deckList playerId ->
    object
      [ "deckList" .= deckList
      , "playerId" .= playerId
      , "tag" .= ("DeckListAnswer" :: Text)
      ]
  CampaignSpecificAnswer key value -> taggedValue "CampaignSpecificAnswer" [toJSON key, value]
  ScenarioSpecificAnswer key value -> taggedValue "ScenarioSpecificAnswer" [toJSON key, value]
  ExchangeAmountsAnswer source fromInvestigator toInvestigator token amount ->
    object
      [ "amount" .= amount
      , "fromInvestigator" .= fromInvestigator
      , "source" .= source
      , "tag" .= ("ExchangeAmountsAnswer" :: Text)
      , "toInvestigator" .= toInvestigator
      , "token" .= token
      ]
  CampaignStepAnswer step -> taggedValue "CampaignStepAnswer" step
  PickDestinyAnswer drawings -> taggedValue "PickDestinyAnswer" drawings
  other -> error $ "coverage bot cannot encode unsupported answer: " <> show other
 where
  taggedValue :: ToJSON a => Text -> a -> Value
  taggedValue tag contents = object ["contents" .= contents, "tag" .= tag]

  taggedContents :: Text -> [Pair] -> Value
  taggedContents tag fields = object ["contents" .= object fields, "tag" .= tag]

nextCampaignAnswer :: Game -> CS.CampaignStep
nextCampaignAnswer game =
  fromMaybe CS.PrologueStep do
    step <- asum [scenarioStep =<< modeScenario game.gameMode, campaignStep . toAttrs <$> modeCampaign game.gameMode]
    pure $ fromMaybe step (CS.defaultNextStep step)
 where
  scenarioStep scenario = (toAttrs scenario).step

scenarioLabel :: Game -> (Text, Value)
scenarioLabel game = case modeScenario game.gameMode of
  Just scenario ->
    let code = unCardCode $ unScenarioId $ scenarioId $ toAttrs scenario
     in (code, object ["code" .= code, "kind" .= ("scenario" :: Text)])
  Nothing -> case modeCampaign game.gameMode of
    Just campaign -> campaignStepLabel $ campaignStep $ toAttrs campaign
    Nothing -> ("no-scenario", object ["kind" .= ("no-scenario" :: Text)])

campaignStepLabel :: CS.CampaignStep -> (Text, Value)
campaignStepLabel step =
  ( key
  , object
      [ "kind" .= ("campaignStep" :: Text)
      , "step" .= canonicalValue (toJSON step)
      ]
  )
 where
  key = case step of
    CS.PrologueStep -> "prologue"
    CS.ContinueCampaignStep continuation -> "continue:" <> campaignStepKey continuation.nextStep
    _ -> "campaign:" <> campaignStepKey step

campaignStepKey :: CS.CampaignStep -> Text
campaignStepKey = \case
  CS.ScenarioStep sid -> unCardCode $ unScenarioId sid
  CS.ScenarioStepWithOptions sid _ -> unCardCode $ unScenarioId sid
  CS.StandaloneScenarioStep sid _ -> unCardCode $ unScenarioId sid
  CS.StandaloneScenarioStepWithOptions sid _ _ -> unCardCode $ unScenarioId sid
  CS.PrologueStep -> "prologue"
  other -> compactText $ canonicalValue $ toJSON other

scenarioOutcomes :: Game -> Map Text ScenarioOutcome
scenarioOutcomes game = case modeCampaign game.gameMode of
  Nothing -> mempty
  Just campaign ->
    let resolutions = campaignResolutions $ toAttrs campaign
     in Map.fromList
          [ (code, scenarioOutcome code (Map.lookup (ScenarioId $ CardCode code) resolutions))
          | code <- ["01104", "01120", "01142"]
          ]
 where
  scenarioOutcome code = \case
    Nothing -> ScenarioOutcome code "not-recorded" Nothing
    Just resolution -> ScenarioOutcome code "recorded" (Just $ canonicalValue $ toJSON resolution)

campaignFinished :: Game -> Bool
campaignFinished game =
  game.gameGameState == IsOver
    || ( isNothing (modeScenario game.gameMode)
          && maybe False (isJust . soResolution) (Map.lookup "01142" $ scenarioOutcomes game)
       )

writeRecords :: FilePath -> CoverageResult -> IO Integer
writeRecords dir CoverageResult {..} = do
  createDirectoryIfMissing True dir
  let fileName = T.unpack (unCardCode $ unInvestigatorId crInvestigator.isInvestigatorId) <> ".jsonl"
      bytes = BL8.unlines $ map (encodeCanonical . toJSON) crRecords
      path = dir </> fileName
  BL8.writeFile path bytes
  pure $ fromIntegral $ BL8.length bytes

writeSummary :: FilePath -> [CoverageResult] -> IO ()
writeSummary dir results = do
  createDirectoryIfMissing True dir
  BL8.writeFile (dir </> "summary.json") $ BL8.pack $ prettyCanonical $ summaryValue results

summaryValue :: [CoverageResult] -> Value
summaryValue results =
  object
    [ "artifactName" .= ("night-of-the-zealot-coverage-jsonl" :: Text)
    , "botPolicy" .= ("first selectable presentation choice; rotate selectable choices on repeated question shape; minimum legal amounts; exchange 0; continue with server-provided step" :: Text)
    , "campaign" .= object ["id" .= ("01" :: Text), "name" .= ("Night of the Zealot" :: Text)]
    , "difficulty" .= ("Easy" :: Text)
    , "investigators" .= map resultSummary results
    , "schemaVersion" .= (1 :: Int)
    ]
 where
  resultSummary CoverageResult {..} =
    object
      [ "distinctChoiceKinds" .= distinctChoiceKinds crRecords
      , "distinctChosenChoiceKinds" .= distinctChosenChoiceKinds crRecords
      , "distinctQuestionKinds" .= distinctQuestionKinds crRecords
      , "file" .= (T.unpack (unCardCode $ unInvestigatorId crInvestigator.isInvestigatorId) <> ".jsonl")
      , "investigator" .= investigatorMetadata crInvestigator
      , "jsonlBytes" .= crJsonlBytes
      , "outcomes" .= crOutcomes
      , "recordCount" .= length crRecords
      , "reachedEndOf01142" .= crReachedDevourerBelowEnd
      , "stepsByScenario" .= stepsByScenario crRecords
      , "stop" .= crStop
      ]

stepsByScenario :: [CoverageRecord] -> Map Text Int
stepsByScenario = foldl' (\m r -> Map.insertWith (+) r.recordScenarioKey 1 m) mempty

distinctQuestionKinds :: [CoverageRecord] -> [Text]
distinctQuestionKinds = nub . sort . mapMaybe (questionKind . recordQuestionPresentation)

distinctChoiceKinds :: [CoverageRecord] -> [Text]
distinctChoiceKinds = nub . sort . concatMap (choiceKinds . recordQuestionPresentation)

distinctChosenChoiceKinds :: [CoverageRecord] -> [Text]
distinctChosenChoiceKinds = nub . sort . mapMaybe recordChosenChoiceKind

canonicalValue :: Value -> Value
canonicalValue = \case
  Object o -> Object $ KeyMap.fromList [(k, canonicalValue v) | (k, v) <- sortOn (Key.toText . fst) (KeyMap.toList o)]
  Array values -> Array $ fmap canonicalValue values
  other -> other

encodeCanonical :: Value -> BL8.ByteString
encodeCanonical = encode . canonicalValue

compactText :: Value -> Text
compactText = decodeUtf8 . BL8.toStrict . encodeCanonical

prettyCanonical :: Value -> String
prettyCanonical value = pretty 0 (canonicalValue value) <> "\n"
 where
  pretty indent = \case
    Object o
      | KeyMap.null o -> "{}"
      | otherwise ->
          "{\n"
            <> intercalate
              ",\n"
              [ spaces (indent + 2) <> showJsonString (Key.toText key) <> ": " <> pretty (indent + 2) value'
              | (key, value') <- KeyMap.toList o
              ]
            <> "\n"
            <> spaces indent
            <> "}"
    Array values
      | null values -> "[]"
      | otherwise ->
          "[\n"
            <> intercalate ",\n" [spaces (indent + 2) <> pretty (indent + 2) value' | value' <- toList values]
            <> "\n"
            <> spaces indent
            <> "]"
    scalar -> BL8.unpack $ encode scalar
  spaces n = replicate n ' '
  showJsonString = BL8.unpack . encode . String

