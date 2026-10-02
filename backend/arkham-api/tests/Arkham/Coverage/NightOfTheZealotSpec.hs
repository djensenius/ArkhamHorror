{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Arkham.Coverage.NightOfTheZealotSpec (spec) where

import Api.Arkham.Helpers (GameApp (..), runGameApp)
import Arkham.Campaign.Types (campaignStep)
import Arkham.CampaignStep qualified as CS
import Arkham.Card.CardCode (CardCode, unCardCode)
import Arkham.Classes.Entity (toAttrs)
import Arkham.Classes.HasQueue (newQueue, pushAll)
import Arkham.Decklist.Type qualified as Decklist
import Arkham.Difficulty (Difficulty (Easy))
import Arkham.Game (Game (..), newCampaign, runMessages)
import Arkham.Game qualified as Game
import Arkham.Game.Utils (modeCampaign, modeScenario)
import Arkham.Id (CampaignId (..), InvestigatorId (..), PlayerId (..), ScenarioId (..), unInvestigatorId, unScenarioId)
import Arkham.Message (Message (ClearUI, SetActivePlayer))
import Arkham.Prelude
import Arkham.Question
import Arkham.Question.Presentation qualified as QuestionPresentation
import Arkham.Scenario.Types (scenarioId)
import Control.Monad.Random (mkStdGen)
import Data.Aeson.Types (Pair)
import Data.ByteString.Lazy qualified as LBS
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
  outputDir <- runIO $ lookupEnv "ARKHAM_NOTZ_COVERAGE_DIR"
  case outputDir of
    Nothing ->
      it "runs only when ARKHAM_NOTZ_COVERAGE_DIR is set" do
        pendingWith "set ARKHAM_NOTZ_COVERAGE_DIR to generate coverage fixtures"
    Just dir ->
      it "records a deterministic solo campaign bot run for each core investigator" do
        results <- traverse (runInvestigatorCoverage dir) coreInvestigators
        writeSummary dir results
        unless (all (not . null . crRecords) results) $
          expectationFailure "expected every investigator to record at least one prompt"

coreInvestigators :: [InvestigatorSpec]
coreInvestigators =
  [ InvestigatorSpec "01001" "Roland Banks" 12001 rolandStarterDeck
  , InvestigatorSpec "01002" "Daisy Walker" 12002 daisyStarterDeck
  , InvestigatorSpec "01003" "Skids O'Toole" 12003 skidsStarterDeck
  , InvestigatorSpec "01004" "Agnes Baker" 12004 agnesStarterDeck
  , InvestigatorSpec "01005" "Wendy Adams" 12005 wendyStarterDeck
  ]

data InvestigatorSpec = InvestigatorSpec
  { isInvestigatorId :: InvestigatorId
  , isInvestigatorName :: Text
  , isSeed :: Int
  , isDeckSlots :: Map CardCode Int
  }

starterDeck :: InvestigatorId -> Text -> Map CardCode Int -> Decklist.ArkhamDBDecklist
starterDeck investigatorId investigatorName slots =
  Decklist.ArkhamDBDecklist
    { Decklist.slots = slots
    , Decklist.sideSlots = mempty
    , Decklist.investigator_code = investigatorId
    , Decklist.investigator_name = investigatorName
    , Decklist.meta = Nothing
    , Decklist.taboo_id = Nothing
    , Decklist.url = Nothing
    , Decklist.decklist_id = Just $ "core-starter-" <> unCardCode (unInvestigatorId investigatorId)
    , Decklist.decklist_name = Just $ investigatorName <> " core starter"
    }

oneEach :: [CardCode] -> Map CardCode Int
oneEach = Map.fromList . map (,1)

neutralCore :: [CardCode]
neutralCore = ["01086", "01087", "01088", "01089", "01090", "01091", "01092", "01093"]

guardian0, seeker0, rogue0, mystic0, survivor0 :: [CardCode]
guardian0 = ["01016", "01017", "01018", "01019", "01020", "01021", "01022", "01023", "01024", "01025"]
seeker0 = ["01030", "01031", "01032", "01033", "01034", "01035", "01036", "01037", "01038", "01039"]
rogue0 = ["01044", "01045", "01046", "01047", "01048", "01049", "01050", "01051", "01052", "01053"]
mystic0 = ["01058", "01059", "01060", "01061", "01062", "01063", "01064", "01065", "01066", "01067"]
survivor0 = ["01072", "01073", "01074", "01075", "01076", "01077", "01078", "01079", "01080", "01081"]

rolandStarterDeck, daisyStarterDeck, skidsStarterDeck, agnesStarterDeck, wendyStarterDeck :: Map CardCode Int
rolandStarterDeck = oneEach $ ["01006", "01007"] <> guardian0 <> seeker0 <> neutralCore
daisyStarterDeck = oneEach $ ["01008", "01009"] <> seeker0 <> mystic0 <> neutralCore
skidsStarterDeck = oneEach $ ["01010", "01011"] <> rogue0 <> guardian0 <> neutralCore
agnesStarterDeck = oneEach $ ["01012", "01013"] <> mystic0 <> survivor0 <> neutralCore
wendyStarterDeck = oneEach $ ["01014", "01015"] <> survivor0 <> rogue0 <> neutralCore

data CoverageResult = CoverageResult
  { crInvestigator :: InvestigatorSpec
  , crRecords :: [CoverageRecord]
  , crStop :: StopReport
  }

data CoverageRecord = CoverageRecord
  { recordScenario :: Text
  , recordInvestigator :: InvestigatorId
  , recordStepIndex :: Int
  , recordQuestionVersion :: Int
  , recordPlayerId :: PlayerId
  , recordRawQuestion :: Value
  , recordQuestionPresentation :: Value
  , recordChosenAnswer :: Value
  , recordChoiceNote :: Text
  }

data StopReport = StopReport
  { stopReason :: Text
  , stopScenario :: Text
  , stopStepsByScenario :: Map Text Int
  }

instance ToJSON CoverageRecord where
  toJSON CoverageRecord {..} =
    object
      [ "scenario" .= recordScenario
      , "investigator" .= recordInvestigator
      , "stepIndex" .= recordStepIndex
      , "questionVersion" .= recordQuestionVersion
      , "playerId" .= recordPlayerId
      , "rawQuestion" .= recordRawQuestion
      , "questionPresentation" .= recordQuestionPresentation
      , "chosenAnswer" .= recordChosenAnswer
      , "choiceNote" .= recordChoiceNote
      ]

instance ToJSON StopReport where
  toJSON StopReport {..} =
    object
      [ "reason" .= stopReason
      , "scenario" .= stopScenario
      , "stepsByScenario" .= stopStepsByScenario
      ]

coverageValue :: CoverageResult -> Value
coverageValue CoverageResult {..} =
  object
    [ "schemaVersion" .= (1 :: Int)
    , "campaign" .= object ["id" .= ("01" :: Text), "name" .= ("Night of the Zealot" :: Text)]
    , "difficulty" .= ("Easy" :: Text)
    , "botPolicy" .= ("prefer end/skip/done/progress choices; otherwise first legal choice; minimum legal amounts; exchange 0; continue with server next step" :: Text)
    , "investigator" .= investigatorMetadata crInvestigator
    , "deck" .= starterDeck crInvestigator.isInvestigatorId crInvestigator.isInvestigatorName crInvestigator.isDeckSlots
    , "stop" .= crStop
    , "records" .= crRecords
    ]

investigatorMetadata :: InvestigatorSpec -> Value
investigatorMetadata InvestigatorSpec {..} =
  object
    [ "id" .= isInvestigatorId
    , "name" .= isInvestigatorName
    , "seed" .= isSeed
    ]

runInvestigatorCoverage :: FilePath -> InvestigatorSpec -> IO CoverageResult
runInvestigatorCoverage dir spec' = do
  createDirectoryIfMissing True dir
  let playerId = PlayerId $ UUID.fromWords 0 0 0 (fromIntegral spec'.isSeed)
      game0 = newCampaign (CampaignId "01") Nothing spec'.isSeed 1 Easy False
  gameRef <- newIORef game0
  queueRef <- newQueue []
  genRef <- newIORef $ mkStdGen spec'.isSeed
  let app = GameApp gameRef queueRef genRef (pure . const ()) Nothing
      drain = drainMessages app
  runGameApp app $ do
    Game.addPlayer playerId
  drain
  result <- botLoop app spec' playerId 0 mempty []
  let fileName = T.unpack (unCardCode $ unInvestigatorId spec'.isInvestigatorId) <> ".json"
  LBS.writeFile (dir </> fileName) (encode $ coverageValue result)
  pure result

-- Keep each drain bounded so an engine loop reports as a stop instead of hanging
-- the nightly/manual coverage job indefinitely.
drainMessages :: GameApp -> IO ()
drainMessages app = do
  result <- Timeout.timeout (30 * 1000 * 1000) $ runGameApp app (runMessages "notz-coverage" Nothing)
  when (isNothing result) $ expectationFailure "message processing timed out after 30 seconds"

botLoop
  :: GameApp
  -> InvestigatorSpec
  -> PlayerId
  -> Int
  -> Map Text Int
  -> [CoverageRecord]
  -> IO CoverageResult
botLoop app spec' playerId step seen records
  | step >= maxSteps = finish "step cap reached"
  | otherwise = do
      game <- readIORef app.appGame
      case Map.lookup playerId game.gameQuestion of
        Nothing -> finish "no pending question for investigator"
        Just question -> do
          let scenario = scenarioLabel game
              qVersion = game.gameScenarioSteps
              presentation = QuestionPresentation.questionPresentation qVersion question
              rawQuestion = toJSON question
              seenKey = scenario <> ":" <> tshow qVersion <> ":" <> T.pack (show rawQuestion)
              repeatCount = Map.findWithDefault 0 seenKey seen
              selected = selectAnswer spec' playerId game question repeatCount
              record =
                CoverageRecord
                  { recordScenario = scenario
                  , recordInvestigator = spec'.isInvestigatorId
                  , recordStepIndex = step
                  , recordQuestionVersion = qVersion
                  , recordPlayerId = playerId
                  , recordRawQuestion = rawQuestion
                  , recordQuestionPresentation = toJSON presentation
                  , recordChosenAnswer = selected.answerJson
                  , recordChoiceNote = selected.note
                  }
          applySelectedAnswer app playerId game selected
          botLoop app spec' playerId (step + 1) (Map.insert seenKey (repeatCount + 1) seen) (record : records)
 where
  maxSteps = 2500
  finish reason = do
    game <- readIORef app.appGame
    pure
      CoverageResult
        { crInvestigator = spec'
        , crRecords = reverse records
        , crStop = StopReport reason (scenarioLabel game) (stepsByScenario $ reverse records)
        }

stepsByScenario :: [CoverageRecord] -> Map Text Int
stepsByScenario = foldl' (\m r -> Map.insertWith (+) r.recordScenario 1 m) mempty

data SelectedAnswer = SelectedAnswer
  { answerValue :: Answer
  , answerJson :: Value
  , answerMessages :: Maybe [Message]
  , note :: Text
  }

applySelectedAnswer :: GameApp -> PlayerId -> Game -> SelectedAnswer -> IO ()
applySelectedAnswer app defaultPlayer game SelectedAnswer {..} = do
  messages <- case answerMessages of
    Just msgs -> pure msgs
    Nothing -> do
      let answerPid = fromMaybe defaultPlayer (answerPlayer answerValue)
      handleAnswerPure game answerPid answerValue >>= \case
        Unhandled reason -> expectationFailure ("bot answer was not handled: " <> T.unpack reason) >> pure []
        Handled msgs -> pure msgs
  let answerPid = fromMaybe defaultPlayer (answerPlayer answerValue)
      activePid = game.gameActivePlayerId
      bracketed =
        [SetActivePlayer answerPid | activePid /= answerPid]
          <> messages
          <> [SetActivePlayer activePid | activePid /= answerPid]
  runGameApp app $ pushAll (ClearUI : bracketed)
  drainMessages app

selectAnswer :: InvestigatorSpec -> PlayerId -> Game -> Question Message -> Int -> SelectedAnswer
selectAnswer spec' playerId game question repeatCount = case stripQuestion question of
  ChooseDeck -> deckListAnswer "starter deck"
  ChooseUpgradeDeck -> deckListAnswer "continue without upgrading"
  ChooseJoinDeck {} -> deckListAnswer "join with starter deck"
  PickScenarioSettings -> answerOnly (StandaloneSettingsAnswer []) standaloneSettingsJson "empty standalone settings"
  PickCampaignSettings -> answerOnly (CampaignSettingsAnswer emptyCampaignSettings) campaignSettingsJson "empty campaign settings"
  PickCampaignSpecific key value -> answerOnly (CampaignSpecificAnswer key value) (taggedContents "CampaignSpecificAnswer" ["key" .= key, "value" .= value]) "echo campaign-specific value"
  PickScenarioSpecific key value -> answerOnly (ScenarioSpecificAnswer key value) (taggedContents "ScenarioSpecificAnswer" ["key" .= key, "value" .= value]) "echo scenario-specific value"
  ChooseAmounts _ target choices _ ->
    let amounts = minimumAmounts target choices
     in answerOnly
          (AmountsAnswer $ AmountsResponse amounts (Just game.gameScenarioSteps) (Just playerId))
          (taggedContents "AmountsAnswer" ["amounts" .= amounts, "questionVersion" .= game.gameScenarioSteps, "playerId" .= playerId])
          "minimum legal amounts"
  ChoosePaymentAmounts _ target choices ->
    let amounts = minimumPaymentAmounts target choices
     in answerOnly
          (PaymentAmountsAnswer $ PaymentAmountsResponse amounts (Just game.gameScenarioSteps) (Just playerId))
          (taggedContents "PaymentAmountsAnswer" ["amounts" .= amounts, "questionVersion" .= game.gameScenarioSteps, "playerId" .= playerId])
          "minimum legal payment amounts"
  ChooseExchangeAmounts source iid1 _ iid2 _ token ->
    answerOnly
      (ExchangeAmountsAnswer source iid1 iid2 token 0)
      ( object
          [ "tag" .= ("ExchangeAmountsAnswer" :: Text)
          , "source" .= source
          , "fromInvestigator" .= iid1
          , "toInvestigator" .= iid2
          , "token" .= token
          , "amount" .= (0 :: Int)
          ]
      )
      "exchange 0"
  ContinueCampaign ->
    let next = nextCampaignAnswer game
     in answerOnly
          (CampaignStepAnswer next)
          (object ["tag" .= ("CampaignStepAnswer" :: Text), "contents" .= next])
          "continue with current server campaign step"
  PickDestiny drawings ->
    answerOnly
      (PickDestinyAnswer drawings)
      (object ["tag" .= ("PickDestinyAnswer" :: Text), "contents" .= drawings])
      "keep destiny drawing order"
  q -> choiceAnswer q
 where
  deck = starterDeck spec'.isInvestigatorId spec'.isInvestigatorName spec'.isDeckSlots
  deckListAnswer note' =
    SelectedAnswer
      { answerValue = DeckListAnswer deck playerId
      , answerJson = object ["tag" .= ("DeckListAnswer" :: Text), "deckList" .= deck, "playerId" .= playerId]
      , answerMessages = Just $ deckChosen game playerId deck
      , note = note'
      }
  answerOnly answer json note' = SelectedAnswer answer json Nothing note'
  choiceAnswer q =
    let choice = chooseIndex q repeatCount
     in SelectedAnswer
          { answerValue = Answer $ QuestionResponse choice (Just playerId) (Just game.gameScenarioSteps)
          , answerJson = taggedContents "Answer" ["choice" .= choice, "playerId" .= playerId, "questionVersion" .= game.gameScenarioSteps]
          , answerMessages = Nothing
          , note = "choice " <> tshow choice
          }

stripQuestion :: Question msg -> Question msg
stripQuestion = \case
  QuestionLabel _ _ q -> stripQuestion q
  PayCostQuestion _ q -> stripQuestion q
  QuestionWithSource _ _ q -> stripQuestion q
  q -> q

chooseIndex :: Question Message -> Int -> Int
chooseIndex q repeatCount = case q of
  QuestionLabel _ _ q' -> chooseIndex q' repeatCount
  PayCostQuestion _ q' -> chooseIndex q' repeatCount
  QuestionWithSource _ _ q' -> chooseIndex q' repeatCount
  ChooseOne choices -> preferredChoice choices repeatCount False
  PlayerWindowChooseOne choices -> preferredChoice choices repeatCount True
  WindowChooseOne choices -> preferredChoice choices repeatCount True
  ChooseOneFromEach groups -> preferredChoice (concat groups) repeatCount False
  ChooseN _ choices -> preferredChoice choices repeatCount False
  ChooseSome choices -> preferredChoice choices repeatCount True
  ChooseSome1 _ choices -> preferredChoice choices repeatCount True
  ChooseUpToN _ choices -> preferredChoice choices repeatCount True
  ChooseOneAtATime choices -> preferredChoice choices repeatCount False
  ChooseOneAtATimeWithAuto _ choices -> if length choices > 1 then 0 else preferredChoice choices repeatCount False
  Read _ readChoices _ -> case readChoices of
    BasicReadChoices choices -> preferredChoice choices repeatCount False
    BasicReadChoicesN _ choices -> preferredChoice choices repeatCount False
    BasicReadChoicesUpToN _ choices -> preferredChoice choices repeatCount True
    LeadInvestigatorMustDecide choices -> preferredChoice choices repeatCount False
  ChooseOneWizard _ choices _ _ -> min repeatCount (max 0 $ length choices - 1)
  PickSupplies _ _ choices _ -> preferredChoice choices repeatCount False
  DropDown options -> if null options then 0 else repeatCount `mod` length options
  _ -> 0

preferredChoice :: [UI Message] -> Int -> Bool -> Int
preferredChoice choices repeatCount allowDone = case ranked of
  [] -> 0
  xs -> fromMaybe 0 $ xs !!? (repeatCount `mod` length xs)
 where
  indexed = zip [0 ..] choices
  ranked =
    map fst (filter (isEndTurn . snd) indexed)
      <> map fst (filter (isApplyResults . snd) indexed)
      <> [i | (i, c) <- indexed, allowDone, isDoneLike c]
      <> map fst (filter (isStartSkillTest . snd) indexed)
      <> map fst (filter (isProgressLabel . snd) indexed)
      <> [i | (i, c) <- indexed, not (isInvalid c), not (allowDone && isDoneLike c)]
  isInvalid = \case
    InvalidLabel {} -> True
    Info {} -> True
    _ -> False
  isEndTurn = \case
    EndTurnButton {} -> True
    _ -> False
  isApplyResults = \case
    SkillTestApplyResultsButton -> True
    _ -> False
  isStartSkillTest = \case
    StartSkillTestButton {} -> True
    _ -> False
  isDoneLike = \case
    Done {} -> True
    SkipTriggersButton {} -> True
    Label label _ -> "done" `T.isInfixOf` T.toLower label || "skip" `T.isInfixOf` T.toLower label || "no" == T.toLower label
    _ -> False
  isProgressLabel = \case
    Label label _ ->
      let lower = T.toLower label
       in any (`T.isInfixOf` lower) ["continue", "resign", "advance", "resolution", "proceed"]
    ScenarioLabel label _ _ -> "continue" `T.isInfixOf` T.toLower label
    _ -> False

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

emptyCampaignSettings :: CampaignSettings
emptyCampaignSettings = CampaignSettings [] mempty mempty []

standaloneSettingsJson :: Value
standaloneSettingsJson = object ["tag" .= ("StandaloneSettingsAnswer" :: Text), "contents" .= ([] :: [Value])]

campaignSettingsJson :: Value
campaignSettingsJson =
  taggedContents
    "CampaignSettingsAnswer"
    [ "keys" .= ([] :: [Value])
    , "counts" .= object ([] :: [Pair])
    , "sets" .= object ([] :: [Pair])
    , "options" .= ([] :: [Value])
    ]

taggedContents :: Text -> [Pair] -> Value
taggedContents tag fields = object ["tag" .= tag, "contents" .= object fields]

nextCampaignAnswer :: Game -> CS.CampaignStep
nextCampaignAnswer game =
  fromMaybe CS.PrologueStep do
    step <- asum [scenarioStep =<< modeScenario game.gameMode, campaignStep . toAttrs <$> modeCampaign game.gameMode]
    pure $ fromMaybe step (CS.defaultNextStep step)
 where
  scenarioStep scenario = (toAttrs scenario).step

scenarioLabel :: Game -> Text
scenarioLabel game = case modeScenario game.gameMode of
  Just scenario -> unCardCode $ unScenarioId $ scenarioId $ toAttrs scenario
  Nothing -> case modeCampaign game.gameMode of
    Just campaign -> "campaign:" <> tshow (campaignStep $ toAttrs campaign)
    Nothing -> "no-scenario"

writeSummary :: FilePath -> [CoverageResult] -> IO ()
writeSummary dir results = do
  createDirectoryIfMissing True dir
  LBS.writeFile (dir </> "summary.json") $ encode $ object
    [ "schemaVersion" .= (1 :: Int)
    , "campaign" .= ("Night of the Zealot" :: Text)
    , "investigators" .= map resultSummary results
    ]
 where
  resultSummary CoverageResult {..} =
    object
      [ "investigator" .= investigatorMetadata crInvestigator
      , "stop" .= crStop
      , "steps" .= length crRecords
      , "stepsByScenario" .= stepsByScenario crRecords
      , "file" .= (T.unpack (unCardCode $ unInvestigatorId crInvestigator.isInvestigatorId) <> ".json")
      ]
