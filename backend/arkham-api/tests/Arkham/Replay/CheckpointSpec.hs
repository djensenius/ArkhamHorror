module Arkham.Replay.CheckpointSpec (spec) where

import Api.Arkham.Export
import Api.Arkham.Types.MultiplayerVariant (MultiplayerVariant (Solo))
import Arkham.CampaignStep qualified as CS
import Arkham.Classes.HasGame (getGame)
import Arkham.Git (GitSha (..))
import Arkham.Replay.Checkpoint
import Arkham.Token (Token (Resource))
import Data.Aeson qualified as Aeson
import Data.Aeson.KeyMap qualified as KeyMap
import Data.ByteString.Lazy qualified as BSL
import Data.Either (isLeft, isRight)
import Data.Map.Strict qualified as Map
import Data.Text qualified as T
import Data.UUID qualified as UUID
import Entity.Answer
import Entity.Arkham.Game qualified as GameEntity
import Entity.Arkham.Step (ActionDiff (..), ArkhamStep (..), Choice (..))
import TestImport.New hiding (ArkhamExport, ArkhamGameExportData)

spec :: Spec
spec = describe "deterministic replay checkpoint harness" do
  it "binds exact prompts and versioned answers" . gameTest $ \_ -> do
    game <- checkpointGame
    checkpoint <- onlyCheckpoint game
    checkQuestionCheckpoint game checkpoint `shouldBe` CheckpointReached
    checkQuestionCheckpoint
      (game {gameQuestion = Map.singleton checkpoint.checkpointPlayerId (ChooseOne [Label "different" [Noop]])})
      checkpoint
      `shouldSatisfy` isMismatch
    checkQuestionCheckpoint (game {gameScenarioSteps = 8}) checkpoint `shouldSatisfy` isMismatch
    let response version player =
          ReplayAnswerStep checkpoint
            $ Answer QuestionResponse {qrChoice = 0, qrPlayerId = player, qrQuestionVersion = version}
    validateReplayAnswer game (response (Just 7) $ Just checkpoint.checkpointPlayerId)
      `shouldBe` Right checkpoint.checkpointPlayerId
    validateReplayAnswer game (response Nothing $ Just checkpoint.checkpointPlayerId)
      `shouldSatisfy` isLeft
    validateReplayAnswer game (response (Just 7) Nothing) `shouldSatisfy` isLeft

  it "fails closed on prompt and answer constructor mismatches" . gameTest $ \_ -> do
    game <- checkpointGame
    let player = game.gameActivePlayerId
        firstInvestigator = "01001" :: InvestigatorId
        secondInvestigator = "01002" :: InvestigatorId
        choiceAnswer =
          Answer
            QuestionResponse
              { qrChoice = 0
              , qrPlayerId = Just player
              , qrQuestionVersion = Just game.gameScenarioSteps
              }
        amountsAnswer =
          AmountsAnswer
            AmountsResponse
              { arAmounts = mempty
              , arQuestionVersion = Just game.gameScenarioSteps
              , arPlayerId = Just player
              }
        paymentAnswer =
          PaymentAmountsAnswer
            PaymentAmountsResponse
              { parAmounts = mempty
              , parQuestionVersion = Just game.gameScenarioSteps
              , parPlayerId = Just player
              }
        destinyPrompt = PickDestiny []
        destinyAnswer = PickDestinyAnswer []
        exchangePrompt =
          ChooseExchangeAmounts GameSource firstInvestigator 0 secondInvestigator 0 Resource
        exchangeAnswer =
          ExchangeAmountsAnswer GameSource firstInvestigator secondInvestigator Resource 1
        validCases =
          [ (ChooseOne [Label "continue" [Noop]], choiceAnswer)
          ,
            ( QuestionWithSource GameSource Nothing
                $ PayCostQuestion Free
                $ QuestionLabel "wrapped" Nothing
                $ ChooseOne [Label "continue" [Noop]]
            , choiceAnswer
            )
          , (ChooseAmounts "amounts" (TotalAmountTarget 0) [] GameTarget, amountsAnswer)
          ,
            ( QuestionLabel
                "amounts"
                Nothing
                (ChooseAmounts "amounts" (TotalAmountTarget 0) [] GameTarget)
            , amountsAnswer
            )
          , (ChoosePaymentAmounts "payment" Nothing [], paymentAnswer)
          , (PayCostQuestion Free $ ChoosePaymentAmounts "payment" Nothing [], paymentAnswer)
          , (PickScenarioSettings, StandaloneSettingsAnswer [])
          , (PickCampaignSettings, CampaignSettingsAnswer $ CampaignSettings [] mempty mempty [])
          , (destinyPrompt, destinyAnswer)
          , (QuestionLabel "destiny" Nothing destinyPrompt, destinyAnswer)
          , (PickCampaignSpecific "campaign" Aeson.Null, CampaignSpecificAnswer "choice" Aeson.Null)
          , (PickScenarioSpecific "scenario" Aeson.Null, ScenarioSpecificAnswer "choice" Aeson.Null)
          , (exchangePrompt, exchangeAnswer)
          , (ContinueCampaign, CampaignStepAnswer CS.PrologueStep)
          ]
    for_ validCases \(prompt, answer) ->
      validateAtPrompt game prompt answer `shouldBe` Right player
    for_
      [ destinyAnswer
      , StandaloneSettingsAnswer []
      , CampaignSettingsAnswer $ CampaignSettings [] mempty mempty []
      , CampaignSpecificAnswer "choice" Aeson.Null
      , ScenarioSpecificAnswer "choice" Aeson.Null
      , exchangeAnswer
      , CampaignStepAnswer CS.PrologueStep
      ]
      \answer ->
        validateAtPrompt game (ChooseOne [Label "continue" [Noop]]) answer
          `shouldSatisfy` isLeft
    validateAtPrompt game destinyPrompt choiceAnswer `shouldSatisfy` isLeft
    validateAtPrompt game (ChooseOne [Label "continue" [Noop]]) (Raw Noop)
      `shouldSatisfy` isLeft
    validateAtPrompt
      game
      exchangePrompt
      (ExchangeAmountsAnswer GameSource firstInvestigator firstInvestigator Resource 1)
      `shouldSatisfy` isLeft

  it "rejects malformed/history plans and stale, dirty, or unattested builds" . gameTest $ \_ -> do
    game <- checkpointGame
    checkpoint <- onlyCheckpoint game
    let exportHash = T.replicate 64 "a"
        source =
          ReplaySource
            exportHash
            ReplayOrdinaryExport
            (gameGitRevision game)
            fixtureBuild
            replayContractSchemaRevision
        plan = ReplayPlan 1 ReplayAnswers source [] checkpoint
    decodeReplayPlan (planBytes 1 "answers" True) `shouldSatisfy` isRight
    traverse_
      (`shouldSatisfy` isLeft)
      [ decodeReplayPlan $ planBytes 2 "answers" True
      , decodeReplayPlan $ planBytes 1 "answers" False
      , decodeReplayPlan $ planBytes 1 "history" False
      ]
    validateReplaySource fixtureBuild exportHash ReplayOrdinaryExport game plan `shouldBe` Right ()
    traverse_
      (`shouldSatisfy` isLeft)
      [ validateReplaySource fixtureBuild (T.replicate 64 "b") ReplayOrdinaryExport game plan
      , validateReplaySource fixtureBuild exportHash ReplayCheckpoint game plan
      , validateReplaySource staleBuild exportHash ReplayOrdinaryExport game plan
      , validateReplaySource
          fixtureBuild
          exportHash
          ReplayOrdinaryExport
          game
          plan {replayPlanSource = source {replaySourceSchemaRevision = "0.0.0"}}
      , validateReplayBuildIdentity
          fixtureBuild {replayBuildSourceClean = False, replayBuildAttestation = ReplayBuildGitClean}
      , validateReplayBuildIdentity
          fixtureBuild {replayBuildSourceClean = False, replayBuildAttestation = ReplayBuildUnattested}
      ]

  it "requires the exact retained current step, including zero, and preserves its queue" do
    let step0 = fixtureStep 0 [Noop]
        step1 = fixtureStep 1 []
    traverse_
      (`shouldSatisfy` isLeft)
      [ validateRetainedSteps 0 []
      , validateRetainedSteps 0 [step1]
      , validateRetainedSteps 1 [step1, step1]
      , void $ retainedQueueAt 0 []
      , void $ retainedQueueAt 0 [step1]
      ]
    validateRetainedSteps 0 [step0] `shouldBe` Right ()
    validateRetainedSteps 1 [step0, step1] `shouldBe` Right ()
    case retainedQueueAt 0 [step0] of
      Left err -> expectationFailure err
      Right queue -> Aeson.toJSON queue `shouldBe` Aeson.toJSON [Noop]
    Aeson.toJSON (prependReplayAnswerMessages [ClearUI] [Noop])
      `shouldBe` Aeson.toJSON [ClearUI, Noop]
    pure () :: IO ()

  it "distinguishes ordinary exports and verifies every checkpoint envelope authority" . gameTest $ \_ -> do
    game <- checkpointGame
    checkpoint <- onlyCheckpoint game
    let queue = [Noop]
        source = ordinaryExport game queue
        ordinaryBytes = encodeStrict source
        checkpointExport = makeCheckpointExport source game queue
        provenance = checkpointProvenance game checkpoint queue
        encodedValue = checkpointExportValue checkpointExport provenance
        encoded = encodeStrict encodedValue
    case decodeReplayInput fixtureBuild ordinaryBytes of
      Right (_, ReplayOrdinaryExport, Nothing) -> pure ()
      other -> expectationFailure $ "unexpected ordinary export result: " <> showKind other
    case decodeReplayInput fixtureBuild encoded of
      Left err -> expectationFailure err
      Right (decoded, ReplayCheckpoint, Just decodedProvenance) -> do
        let decodedData = decoded.aeCampaignData
        decodedData.agedStep `shouldBe` 0
        decodedData.agedLog `shouldSatisfy` null
        case decodedData.agedSteps of
          [ArkhamStep gameId (Choice _ decodedQueue) 0 (ActionDiff [])] -> do
            gameId `shouldBe` GameEntity.ArkhamGameKey UUID.nil
            Aeson.toJSON decodedQueue `shouldBe` Aeson.toJSON queue
          other -> expectationFailure $ "unexpected queue carrier: " <> show other
        Aeson.toJSON decodedData.agedCurrentData `shouldBe` Aeson.toJSON game
        decodedProvenance `shouldBe` provenance
      other -> expectationFailure $ "unexpected checkpoint result: " <> showKind other
    traverse_
      shouldReject
      [ mapRoot (KeyMap.delete "replayCheckpoint") encodedValue
      , mapEnvelope (KeyMap.delete "provenance") encodedValue
      , mapEnvelope
          (adjustKey "provenance" $ const $ Aeson.object ["answersApplied" Aeson..= (99 :: Int)])
          encodedValue
      , mapRoot (KeyMap.insert "campaignPlayers" $ Aeson.toJSON ["tampered" :: Text]) encodedValue
      , checkpointExportValue checkpointExport
          $ provenance {provenanceContractSchemaRevision = "0.0.0"}
      ]

checkpointGame :: TestAppT Game
checkpointGame = do
  game <- getGame
  pure
    game
      { gameScenarioSteps = 7
      , gameQuestion = Map.singleton game.gameActivePlayerId (ChooseOne [Label "continue" [Noop]])
      }

onlyCheckpoint :: Game -> TestAppT QuestionCheckpoint
onlyCheckpoint game = case questionCheckpoints game of
  Right [checkpoint] -> pure checkpoint {checkpointName = "target"}
  other -> error $ "expected one checkpoint, got " <> show other

validateAtPrompt :: Game -> Question Message -> Answer -> Either String PlayerId
validateAtPrompt game prompt answer =
  let prompted = game {gameQuestion = Map.singleton game.gameActivePlayerId prompt}
   in case questionCheckpoints prompted of
        Right [checkpoint] -> validateReplayAnswer prompted $ ReplayAnswerStep checkpoint answer
        other -> Left $ "expected one checkpoint, got " <> show other

ordinaryExport :: Game -> [Message] -> ArkhamExport
ordinaryExport game queue =
  ArkhamExport
    ["c01001"]
    ArkhamGameExportData
      { agedName = "fixture"
      , agedCurrentData = game
      , agedStep = 0
      , agedSteps = [fixtureStep 0 queue]
      , agedLog = []
      , agedMultiplayerVariant = Solo
      }

fixtureStep :: Int -> [Message] -> ArkhamStep
fixtureStep step queue =
  ArkhamStep
    (GameEntity.ArkhamGameKey $ fromJust $ UUID.fromString "00000000-0000-0000-0000-000000000001")
    (Choice mempty queue)
    step
    (ActionDiff [])

checkpointProvenance :: Game -> QuestionCheckpoint -> [Message] -> ReplayProvenance
checkpointProvenance game checkpoint queue =
  ReplayProvenance
    1
    replayContractSchemaRevision
    (T.replicate 64 "a")
    (T.replicate 64 "b")
    ReplayOrdinaryExport
    (gameGitRevision game)
    fixtureBuild
    ReplayAnswers
    0
    0
    checkpoint
    (sha256Lazy $ Aeson.encode game)
    (sha256Lazy $ Aeson.encode queue)

fixtureBuild :: ReplayBuildIdentity
fixtureBuild =
  ReplayBuildIdentity
    (GitSha $ T.replicate 40 "c")
    (GitSha $ T.replicate 40 "d")
    (T.replicate 64 "e")
    True
    ReplayBuildGitClean

staleBuild :: ReplayBuildIdentity
staleBuild = fixtureBuild {replayBuildSourceSha256 = T.replicate 64 "f"}

planBytes :: Int -> Text -> Bool -> ByteString
planBytes schema mode includeAnswers =
  encodeStrict
    $ Aeson.object
    $ [ "schemaVersion" Aeson..= schema
      , "mode" Aeson..= mode
      , "source"
          Aeson..= ReplaySource
            (T.replicate 64 "a")
            ReplayOrdinaryExport
            (GitSha $ T.replicate 40 "b")
            fixtureBuild
            replayContractSchemaRevision
      , "stopAt"
          Aeson..= Aeson.object
            [ "type" Aeson..= ("question" :: Text)
            , "name" Aeson..= ("target" :: Text)
            , "questionVersion" Aeson..= (1 :: Int)
            , "playerId" Aeson..= ("00000000-0000-0000-0000-000000000001" :: Text)
            , "promptTag" Aeson..= ("ChooseOne" :: Text)
            , "promptSha256" Aeson..= T.replicate 64 "d"
            ]
      ]
      <> ["answers" Aeson..= ([] :: [Aeson.Value]) | includeAnswers]

mapRoot :: (Aeson.Object -> Aeson.Object) -> Aeson.Value -> Aeson.Value
mapRoot f = \case
  Aeson.Object value -> Aeson.Object $ f value
  value -> value

mapEnvelope :: (Aeson.Object -> Aeson.Object) -> Aeson.Value -> Aeson.Value
mapEnvelope f = mapRoot $ adjustKey "replayCheckpoint" (mapRoot f)

adjustKey :: Aeson.Key -> (a -> a) -> KeyMap.KeyMap a -> KeyMap.KeyMap a
adjustKey key f values =
  maybe values (\value -> KeyMap.insert key (f value) values) $ KeyMap.lookup key values

shouldReject :: MonadIO m => Aeson.Value -> m ()
shouldReject value =
  when (isRight $ decodeReplayInput fixtureBuild $ encodeStrict value) $
    liftIO $ expectationFailure "expected replay input rejection"

encodeStrict :: Aeson.ToJSON a => a -> ByteString
encodeStrict = BSL.toStrict . Aeson.encode

isMismatch :: CheckpointStatus -> Bool
isMismatch = \case
  CheckpointMismatch _ -> True
  _ -> False

showKind :: Either String (ArkhamExport, ReplayInputKind, Maybe ReplayProvenance) -> String
showKind = \case
  Left err -> err
  Right (_, kind, provenance) -> show kind <> "/" <> show (isJust provenance)
