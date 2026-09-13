module Arkham.Replay.CheckpointSpec (spec) where

import Api.Handler.Arkham.Game.Debug (checkpointInvestigatorPlayerId)
import Api.Arkham.Export
import Api.Arkham.Types.MultiplayerVariant (MultiplayerVariant (Solo))
import Arkham.CampaignStep qualified as CS
import Arkham.Classes.HasGame (getGame)
import Arkham.Git (GitSha (..))
import Arkham.Replay.Checkpoint
import Arkham.Replay.ImportAuthority
import Arkham.Replay.ServerBuildIdentity (serverBuildIdentity)
import Arkham.Tarot (
  TarotCard (..),
  TarotCardArcana (TheFool0, TheHighPriestessII, TheMagicianI),
  TarotCardFacing (Reversed, Upright),
 )
import Arkham.Token (Token (Resource))
import Base.Api.Handler.Capabilities (capabilitiesResponseHeaders)
import Data.Aeson qualified as Aeson
import Data.Aeson.KeyMap qualified as KeyMap
import Data.ByteString.Lazy qualified as BSL
import Data.Either (isLeft, isRight)
import Data.Map.Strict qualified as Map
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import Data.UUID qualified as UUID
import Entity.Answer
import Entity.Arkham.Game qualified as GameEntity
import Entity.Arkham.Step (ActionDiff (..), ArkhamStep (..), Choice (..))
import TestImport.New hiding (ArkhamExport, ArkhamGameExportData)

canonicalJsonHashSpec :: IO ()
canonicalJsonHashSpec = do
  let firstValue :: Aeson.Value
      firstValue =
        Aeson.object
          [ "z" Aeson..= [Aeson.object ["d" Aeson..= (4 :: Int), "c" Aeson..= (3 :: Int)]]
          , "a" Aeson..= Aeson.object ["b" Aeson..= (2 :: Int), "a" Aeson..= (1 :: Int)]
          ]
      secondValue :: Aeson.Value
      secondValue =
        Aeson.object
          [ "a" Aeson..= Aeson.object ["a" Aeson..= (1 :: Int), "b" Aeson..= (2 :: Int)]
          , "z" Aeson..= [Aeson.object ["c" Aeson..= (3 :: Int), "d" Aeson..= (4 :: Int)]]
          ]
      expected :: Text
      expected = "3733063eae4764a370f17cd1c3152dbc98f253d583b6437c2d54310550437799"
  canonicalJsonSha256 firstValue `shouldBe` expected
  canonicalJsonSha256 secondValue `shouldBe` expected

spec :: Spec
spec = describe "deterministic replay checkpoint harness" do
  it "sorts every object level in canonical JSON hashes" canonicalJsonHashSpec

  it "binds exact prompts and versioned answers" . gameTest $ \_ -> do
    game <- checkpointGame
    checkpoint <- onlyCheckpoint game
    checkQuestionCheckpoint game checkpoint `shouldBe` CheckpointReached
    checkpoint.checkpointPromptSha256
      `shouldBe` "0918cd501919fbabf16815e00e091c56ad2f9a7668a53da58b20159ea293db47"
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
    validateReplayAnswer
      game
      ( ReplayAnswerStep checkpoint
          $ Answer
          $ QuestionResponse
            { qrChoice = 1
            , qrPlayerId = Just checkpoint.checkpointPlayerId
            , qrQuestionVersion = Just 7
            }
      )
      `shouldSatisfy` isLeft
    validateReplayAnswer
      game
      ( ReplayAnswerStep checkpoint
          $ Answer
          $ QuestionResponse
            { qrChoice = -1
            , qrPlayerId = Just checkpoint.checkpointPlayerId
            , qrQuestionVersion = Just 7
            }
      )
      `shouldSatisfy` isLeft

  it "derives checkpoint player binding from decoded game state" . gameTest $ \self -> do
    game <- getGame
    checkpointInvestigatorPlayerId game ("c" <> unCardCode (unInvestigatorId $ toId self))
      `shouldBe` Right self.player
    checkpointInvestigatorPlayerId game "c99999" `shouldSatisfy` isLeft

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
          ChooseExchangeAmounts GameSource firstInvestigator 2 secondInvestigator 3 Resource
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

  it "accepts only the prompt destiny drawings with the required reversed count" . gameTest $ \_ -> do
    game <- checkpointGame
    let firstDrawing = DestinyDrawing "first" $ TarotCard Upright TheFool0
        secondDrawing = DestinyDrawing "second" $ TarotCard Upright TheMagicianI
        thirdDrawing = DestinyDrawing "third" $ TarotCard Upright TheHighPriestessII
        prompt = PickDestiny [firstDrawing, secondDrawing, thirdDrawing]
        answer drawings = PickDestinyAnswer drawings
        reversed scope arcana = DestinyDrawing scope $ TarotCard Reversed arcana
        upright scope arcana = DestinyDrawing scope $ TarotCard Upright arcana
    validateAtPrompt
      game
      prompt
      (answer [reversed "first" TheFool0, reversed "second" TheMagicianI, upright "third" TheHighPriestessII])
      `shouldBe` Right game.gameActivePlayerId
    traverse_
      (`shouldSatisfy` isLeft)
      [ validateAtPrompt
          game
          prompt
          (answer [reversed "first" TheFool0, upright "second" TheMagicianI, upright "third" TheHighPriestessII])
      , validateAtPrompt
          game
          prompt
          (answer [reversed "first" TheFool0, reversed "second" TheMagicianI])
      , validateAtPrompt
          game
          prompt
          ( answer
              [ reversed "first" TheFool0
              , reversed "second" TheMagicianI
              , upright "third" TheHighPriestessII
              , upright "extra" TheHighPriestessII
              ]
          )
      , validateAtPrompt
          game
          prompt
          ( answer
              [ reversed "first" TheFool0
              , reversed "second" TheMagicianI
              , upright "third" TheMagicianI
              ]
          )
      , validateAtPrompt
          game
          prompt
          ( answer
              [ reversed "first" TheFool0
              , reversed "first" TheMagicianI
              , upright "third" TheHighPriestessII
              ]
          )
      , validateAtPrompt
          game
          prompt
          ( answer
              [ reversed "second" TheMagicianI
              , reversed "first" TheFool0
              , upright "third" TheHighPriestessII
              ]
          )
      ]

  it "bounds signed token exchanges by the corresponding initial balances" . gameTest $ \_ -> do
    game <- checkpointGame
    let firstInvestigator = "01001" :: InvestigatorId
        secondInvestigator = "01002" :: InvestigatorId
        prompt =
          ChooseExchangeAmounts GameSource firstInvestigator 2 secondInvestigator 3 Resource
        answer fromInvestigator destinationInvestigator amount =
          ExchangeAmountsAnswer
            GameSource
            fromInvestigator
            destinationInvestigator
            Resource
            amount
    traverse_
      (`shouldBe` Right game.gameActivePlayerId)
      [ validateAtPrompt game prompt $ answer firstInvestigator secondInvestigator 2
      , validateAtPrompt game prompt $ answer firstInvestigator secondInvestigator (-3)
      , validateAtPrompt game prompt $ answer secondInvestigator firstInvestigator 3
      , validateAtPrompt game prompt $ answer secondInvestigator firstInvestigator (-2)
      ]
    traverse_
      (`shouldSatisfy` isLeft)
      [ validateAtPrompt game prompt $ answer firstInvestigator secondInvestigator 3
      , validateAtPrompt game prompt $ answer firstInvestigator secondInvestigator (-4)
      , validateAtPrompt game prompt $ answer secondInvestigator firstInvestigator 4
      , validateAtPrompt game prompt $ answer secondInvestigator firstInvestigator (-3)
      , validateAtPrompt game prompt $ answer firstInvestigator secondInvestigator minBound
      ]

  it "rejects unknown, out-of-range, and target-violating replay amounts" . gameTest $ \_ -> do
    game <- checkpointGame
    let player = game.gameActivePlayerId
        investigator = "01001" :: InvestigatorId
        firstChoice = UUID.fromWords 0 0 0 1
        secondChoice = UUID.fromWords 0 0 0 2
        unknownChoice = UUID.fromWords 0 0 0 3
        amountPrompt =
          ChooseAmounts
            "amounts"
            (TotalAmountTarget 3)
            [ AmountChoice firstChoice "first" 1 2
            , AmountChoice secondChoice "second" 0 2
            ]
            GameTarget
        paymentPrompt =
          PayCostQuestion Free
            $ ChoosePaymentAmounts
              "payment"
              (Just $ AmountOneOf [2, 3])
              [ PaymentAmountChoice firstChoice investigator 1 2 "first" Noop
              , PaymentAmountChoice secondChoice investigator 0 2 "second" Noop
              ]
        maxPaymentPrompt =
          ChoosePaymentAmounts
            "payment"
            (Just $ MaxAmountTarget 1)
            [PaymentAmountChoice firstChoice investigator 0 2 "first" Noop]
        minPaymentPrompt =
          ChoosePaymentAmounts
            "payment"
            (Just $ MinAmountTarget 2)
            [PaymentAmountChoice firstChoice investigator 0 2 "first" Noop]
        duplicateAmountPrompt =
          ChooseAmounts
            "amounts"
            (TotalAmountTarget 1)
            [ AmountChoice firstChoice "first" 0 1
            , AmountChoice firstChoice "duplicate" 0 1
            ]
            GameTarget
        duplicatePaymentPrompt =
          ChoosePaymentAmounts
            "payment"
            (Just $ TotalAmountTarget 1)
            [ PaymentAmountChoice firstChoice investigator 0 1 "first" Noop
            , PaymentAmountChoice firstChoice investigator 0 1 "duplicate" Noop
            ]
        amountsAnswer values =
          AmountsAnswer
            AmountsResponse
              { arAmounts = Map.fromList values
              , arQuestionVersion = Just game.gameScenarioSteps
              , arPlayerId = Just player
              }
        paymentAnswer values =
          PaymentAmountsAnswer
            PaymentAmountsResponse
              { parAmounts = Map.fromList values
              , parQuestionVersion = Just game.gameScenarioSteps
              , parPlayerId = Just player
              }
    validateAtPrompt game amountPrompt (amountsAnswer [(firstChoice, 1), (secondChoice, 2)])
      `shouldBe` Right player
    validateAtPrompt game paymentPrompt (paymentAnswer [(firstChoice, 2)])
      `shouldBe` Right player
    traverse_
      (`shouldSatisfy` isLeft)
      [ validateAtPrompt game amountPrompt
          $ amountsAnswer [(firstChoice, 1), (secondChoice, 2), (unknownChoice, 0)]
      , validateAtPrompt game amountPrompt
          $ amountsAnswer [(firstChoice, 3), (secondChoice, 0)]
      , validateAtPrompt game amountPrompt
          $ amountsAnswer [(firstChoice, 1), (secondChoice, 1)]
      , validateAtPrompt game amountPrompt
          $ amountsAnswer [(secondChoice, 2)]
      , validateAtPrompt game paymentPrompt
          $ paymentAnswer [(firstChoice, 1), (unknownChoice, 1)]
      , validateAtPrompt game paymentPrompt
          $ paymentAnswer [(firstChoice, 0), (secondChoice, 2)]
      , validateAtPrompt game paymentPrompt
          $ paymentAnswer [(firstChoice, 2), (secondChoice, 2)]
      , validateAtPrompt game maxPaymentPrompt
          $ paymentAnswer [(firstChoice, 2)]
      , validateAtPrompt game minPaymentPrompt
          $ paymentAnswer [(firstChoice, 1)]
      , validateAtPrompt game duplicateAmountPrompt
          $ amountsAnswer [(firstChoice, 1)]
      , validateAtPrompt game duplicatePaymentPrompt
          $ paymentAnswer [(firstChoice, 1)]
      ]

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
          fixtureBuild {replayBuildSourceClean = True, replayBuildAttestation = ReplayBuildSourceSha256}
      , validateReplayBuildIdentity
          fixtureBuild {replayBuildSourceClean = False, replayBuildAttestation = ReplayBuildUnattested}
      ]

  it "rejects unknown fields throughout exact replay plans" do
    let checkpoint = planCheckpointValue
        answer =
          Aeson.object
            [ "tag" Aeson..= ("Answer" :: Text)
            , "contents"
                Aeson..= Aeson.object
                  [ "choice" Aeson..= (0 :: Int)
                  , "playerId" Aeson..= ("00000000-0000-0000-0000-000000000001" :: Text)
                  , "questionVersion" Aeson..= (1 :: Int)
                  ]
            ]
        step expected value =
          Aeson.object
            [ "expect" Aeson..= expected
            , "answer" Aeson..= value
            ]
        withAnswers values =
          mapRoot
            (KeyMap.insert "answers" $ Aeson.toJSON values)
            (planValue 1 "answers" True)
        validPlan = withAnswers [step checkpoint answer]
        addUnknown = mapRoot $ KeyMap.insert "ignoredTamper" Aeson.Null
        sourceUnknown =
          mapRoot
            (adjustKey "source" addUnknown)
            validPlan
        buildUnknown =
          mapRoot
            ( adjustKey "source"
                $ mapRoot
                $ adjustKey "replayBuild" addUnknown
            )
            validPlan
        stopUnknown =
          mapRoot
            (adjustKey "stopAt" addUnknown)
            validPlan
        stepUnknown =
          withAnswers [addUnknown $ step checkpoint answer]
        expectedUnknown =
          withAnswers [step (addUnknown checkpoint) answer]
        answerUnknown =
          withAnswers [step checkpoint $ addUnknown answer]
        answerContentsUnknown =
          withAnswers
            [ step checkpoint
                $ mapRoot
                  (adjustKey "contents" addUnknown)
                  answer
            ]
        standaloneSetting =
          Aeson.object
            [ "type" Aeson..= ("ToggleRecords" :: Text)
            , "key" Aeson..= ("DrivenInsaneInvestigators" :: Text)
            , "recordable" Aeson..= ("RecordableCardCode" :: Text)
            , "content"
                Aeson..=
                  [ Aeson.object
                      [ "label" Aeson..= ("investigator" :: Text)
                      , "key" Aeson..= ("01001" :: Text)
                      , "content" Aeson..= True
                      ]
                  ]
            ]
        standaloneAnswer setting =
          Aeson.object
            [ "tag" Aeson..= ("StandaloneSettingsAnswer" :: Text)
            , "contents" Aeson..= [setting]
            ]
        partnerSetting =
          Aeson.object
            [ "type" Aeson..= ("SetPartnerDetails" :: Text)
            , "key" Aeson..= ("PartnerStatus" :: Text)
            , "value" Aeson..= ("08119" :: Text)
            , "maxDamage" Aeson..= (3 :: Int)
            , "maxHorror" Aeson..= (2 :: Int)
            , "content"
                Aeson..= Aeson.object
                  [ "damage" Aeson..= (1 :: Int)
                  , "horror" Aeson..= (0 :: Int)
                  , "status" Aeson..= ("Resolute" :: Text)
                  ]
            , "ifRecorded" Aeson..= ([] :: [Aeson.Value])
            ]
        standaloneSettingUnknown =
          withAnswers [step checkpoint $ standaloneAnswer $ addUnknown standaloneSetting]
        standaloneEntryUnknown =
          withAnswers
            [ step checkpoint
                $ standaloneAnswer
                $ mapRoot
                  ( adjustKey "content"
                      $ \case
                        Aeson.Array entries ->
                          Aeson.toJSON $ addUnknown <$> toList entries
                        other -> other
                  )
                  standaloneSetting
            ]
        deckList =
          Aeson.object
            [ "slots" Aeson..= Aeson.object []
            , "sideSlots" Aeson..= Aeson.object []
            , "investigator_code" Aeson..= ("01001" :: Text)
            , "investigator_name" Aeson..= ("Roland Banks" :: Text)
            , "meta" Aeson..= Aeson.Null
            , "taboo_id" Aeson..= Aeson.Null
            , "url" Aeson..= Aeson.Null
            , "id" Aeson..= Aeson.Null
            , "name" Aeson..= Aeson.Null
            ]
        deckListAnswer value =
          Aeson.object
            [ "tag" Aeson..= ("DeckListAnswer" :: Text)
            , "deckList" Aeson..= value
            , "playerId" Aeson..= ("00000000-0000-0000-0000-000000000001" :: Text)
            ]
        destinyDrawing =
          Aeson.object
            [ "scenario" Aeson..= ("first" :: Text)
            , "tarot"
                Aeson..= Aeson.object
                  [ "facing" Aeson..= ("Reversed" :: Text)
                  , "arcana" Aeson..= ("TheFool0" :: Text)
                  ]
            ]
        destinyAnswer drawings =
          Aeson.object
            [ "tag" Aeson..= ("PickDestinyAnswer" :: Text)
            , "contents" Aeson..= drawings
            ]
        campaignEntry =
          Aeson.object
            [ "tag" Aeson..= ("Recorded" :: Text)
            , "value"
                Aeson..= Aeson.object
                  ["intentionallyArbitrary" Aeson..= True]
            ]
        campaignRecorded entry =
          Aeson.object
            [ "recordable" Aeson..= ("RecordableGeneric" :: Text)
            , "entries" Aeson..= [entry]
            ]
        campaignAnswer recorded =
          Aeson.object
            [ "tag" Aeson..= ("CampaignSettingsAnswer" :: Text)
            , "contents"
                Aeson..= Aeson.object
                  [ "keys" Aeson..= ([] :: [Aeson.Value])
                  , "counts" Aeson..= ([] :: [Aeson.Value])
                  , "sets"
                      Aeson..=
                        [ [ Aeson.String "DrivenInsaneInvestigators"
                          , recorded
                          ]
                        ]
                  , "options" Aeson..= ([] :: [Aeson.Value])
                  ]
            ]
        validStandalonePlan =
          withAnswers [step checkpoint $ standaloneAnswer standaloneSetting]
        validPartnerPlan =
          withAnswers [step checkpoint $ standaloneAnswer partnerSetting]
        validDeckListPlan =
          withAnswers [step checkpoint $ deckListAnswer deckList]
        validDestinyPlan =
          withAnswers [step checkpoint $ destinyAnswer [destinyDrawing]]
        validCampaignPlan =
          withAnswers [step checkpoint $ campaignAnswer $ campaignRecorded campaignEntry]
        deckListUnknown =
          withAnswers [step checkpoint $ deckListAnswer $ addUnknown deckList]
        destinyDrawingUnknown =
          withAnswers [step checkpoint $ destinyAnswer [addUnknown destinyDrawing]]
        destinyTarotUnknown =
          withAnswers
            [ step checkpoint
                $ destinyAnswer
                  [ mapRoot
                      (adjustKey "tarot" addUnknown)
                      destinyDrawing
                  ]
            ]
        campaignRecordedUnknown =
          withAnswers
            [ step checkpoint
                $ campaignAnswer
                $ addUnknown
                $ campaignRecorded campaignEntry
            ]
        campaignEntryUnknown =
          withAnswers
            [ step checkpoint
                $ campaignAnswer
                $ campaignRecorded
                $ addUnknown campaignEntry
            ]
    traverse_
      (`shouldSatisfy` isRight)
      [ decodeReplayPlan $ encodeStrict validPlan
      , decodeReplayPlan $ encodeStrict validStandalonePlan
      , decodeReplayPlan $ encodeStrict validPartnerPlan
      , decodeReplayPlan $ encodeStrict validDeckListPlan
      , decodeReplayPlan $ encodeStrict validDestinyPlan
      , decodeReplayPlan $ encodeStrict validCampaignPlan
      ]
    traverse_
      (`shouldSatisfy` isLeft)
      [ decodeReplayPlan $ encodeStrict $ addUnknown validPlan
      , decodeReplayPlan $ encodeStrict sourceUnknown
      , decodeReplayPlan $ encodeStrict buildUnknown
      , decodeReplayPlan $ encodeStrict stopUnknown
      , decodeReplayPlan $ encodeStrict stepUnknown
      , decodeReplayPlan $ encodeStrict expectedUnknown
      , decodeReplayPlan $ encodeStrict answerUnknown
      , decodeReplayPlan $ encodeStrict answerContentsUnknown
      , decodeReplayPlan $ encodeStrict standaloneSettingUnknown
      , decodeReplayPlan $ encodeStrict standaloneEntryUnknown
      , decodeReplayPlan $ encodeStrict deckListUnknown
      , decodeReplayPlan $ encodeStrict destinyDrawingUnknown
      , decodeReplayPlan $ encodeStrict destinyTarotUnknown
      , decodeReplayPlan $ encodeStrict campaignRecordedUnknown
      , decodeReplayPlan $ encodeStrict campaignEntryUnknown
      ]
    pure () :: IO ()

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

  it "distinguishes ordinary exports and exposes only server-validated checkpoint authority" . gameTest $ \_ -> do
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
    envelope <- case decodeReplayInputEnvelope fixtureBuild encoded of
      Right (_, ReplayCheckpoint, Just value) -> pure value
      Left err -> expectationFailure err >> fail err
      _ -> expectationFailure "validated checkpoint did not retain its envelope" >> fail "missing envelope"
    case decodeReplayImport fixtureBuild ordinaryBytes of
      Right (_, Nothing) -> pure ()
      Left err -> expectationFailure err
      Right _ -> expectationFailure "ordinary import unexpectedly produced replay authority"
    authority <- case decodeReplayImport fixtureBuild encoded of
      Right (_, Just value) -> pure value
      Left err -> expectationFailure err >> fail err
      _ -> expectationFailure "checkpoint import did not produce authority" >> fail "missing authority"
    authority.replayImportCheckpointSha256 `shouldBe` sha256Strict encoded
    authority.replayImportCanonicalEnvelopeSha256
      `shouldBe` canonicalReplayCheckpointEnvelopeSha256 checkpointExport provenance
    authority.replayImportCanonicalEnvelopeSha256
      `shouldBe` envelope.replayCheckpointEnvelopeSha256
    authority.replayImportGameGitRevision `shouldBe` provenance.provenanceSourceGameGitRevision
    authority.replayImportBackendBuild `shouldBe` fixtureBuild
    let expectedValidatedCheckpoint =
          Aeson.object
            [ "schemaVersion" Aeson..= (1 :: Int)
            , "contractSchemaRevision"
                Aeson..= provenance.provenanceContractSchemaRevision
            , "prompt"
                Aeson..= Aeson.object
                  [ "questionVersion"
                      Aeson..= checkpoint.checkpointQuestionVersion
                  , "playerId" Aeson..= checkpoint.checkpointPlayerId
                  , "promptTag" Aeson..= checkpoint.checkpointPromptTag
                  , "promptSha256"
                      Aeson..= checkpoint.checkpointPromptSha256
                  ]
            , "checkpointGameSha256"
                Aeson..= provenance.provenanceCheckpointGameSha256
            , "checkpointQueueSha256"
                Aeson..= provenance.provenanceCheckpointQueueSha256
            ]
    Aeson.toJSON authority.replayImportValidatedCheckpoint
      `shouldBe` expectedValidatedCheckpoint
    authority.replayImportValidatedCheckpoint
      `shouldBe`
        ReplayValidatedCheckpoint
          { replayValidatedCheckpointSchemaVersion = 1
          , replayValidatedCheckpointContractSchemaRevision =
              provenance.provenanceContractSchemaRevision
          , replayValidatedCheckpointPrompt =
              ReplayValidatedPrompt
                { replayValidatedPromptQuestionVersion =
                    checkpoint.checkpointQuestionVersion
                , replayValidatedPromptPlayerId =
                    checkpoint.checkpointPlayerId
                , replayValidatedPromptPromptTag =
                    checkpoint.checkpointPromptTag
                , replayValidatedPromptPromptSha256 =
                    checkpoint.checkpointPromptSha256
                }
          , replayValidatedCheckpointGameSha256 =
              provenance.provenanceCheckpointGameSha256
          , replayValidatedCheckpointQueueSha256 =
              provenance.provenanceCheckpointQueueSha256
          }
    let forgedMetadata =
          provenance
            { provenancePlanSha256 = T.replicate 64 "1"
            , provenanceSourceExportSha256 = T.replicate 64 "2"
            , provenanceSourceInputKind = ReplayCheckpoint
            , provenanceUndoSteps = 99
            , provenanceAnswersApplied = 42
            , provenanceCheckpoint =
                checkpoint {checkpointName = "forged-generation-label"}
            }
        forgedBytes =
          encodeStrict $ checkpointExportValue checkpointExport forgedMetadata
    forgedAuthority <- case decodeReplayImport fixtureBuild forgedBytes of
      Right (_, Just value) -> pure value
      Left err -> expectationFailure err >> fail err
      _ -> expectationFailure "forged metadata checkpoint did not produce authority" >> fail "missing authority"
    forgedAuthority.replayImportValidatedCheckpoint
      `shouldBe` authority.replayImportValidatedCheckpoint
    forgedAuthority.replayImportGameGitRevision
      `shouldBe` authority.replayImportGameGitRevision
    forgedAuthority.replayImportBackendBuild
      `shouldBe` authority.replayImportBackendBuild
    case decodeReplayImport fixtureBuild (encoded <> "\n") of
      Left _ -> pure ()
      Right _ ->
        expectationFailure
          "checkpoint import accepted bytes outside the deterministic canonical encoding"
    let gameId = "00000000-0000-0000-0000-000000000010" :: Text
        importedPlayerId = "00000000-0000-0000-0000-000000000020"
        playerRemapping =
          ReplayPlayerRemapping
            "c01001"
            checkpoint.checkpointPlayerId
            importedPlayerId
            importedPlayerId
            True
        receipt =
          makeReplayImportReceipt gameId [playerRemapping] authority
    receipt.replayImportReceiptGameId `shouldBe` gameId
    receipt.replayImportReceiptGameGitRevision `shouldBe` provenance.provenanceSourceGameGitRevision
    receipt.replayImportReceiptBackendBuild `shouldBe` fixtureBuild
    validateReplayImportReceipt receipt `shouldBe` Right ()
    Aeson.toJSON receipt
      `shouldBe` Aeson.object
        [ "schemaVersion" Aeson..= (1 :: Int)
        , "gameId" Aeson..= gameId
        , "gameGitRevision" Aeson..= provenance.provenanceSourceGameGitRevision
        , "backendBuild" Aeson..= fixtureBuild
        , "checkpointSha256" Aeson..= authority.replayImportCheckpointSha256
        , "canonicalEnvelopeSha256"
            Aeson..= authority.replayImportCanonicalEnvelopeSha256
        , "validatedCheckpoint" Aeson..= expectedValidatedCheckpoint
        , "playerRemappings" Aeson..= [playerRemapping]
        , "receiptSha256" Aeson..= receipt.replayImportReceiptSha256
        ]
    case Aeson.toJSON receipt of
      Aeson.Object objectValue -> do
        KeyMap.member "checkpointProvenance" objectValue `shouldBe` False
        KeyMap.member "validatedCheckpoint" objectValue `shouldBe` True
      _ -> expectationFailure "replay receipt did not encode as an object"
    Aeson.eitherDecodeStrict' @ReplayBuildIdentity
      (TE.encodeUtf8 $ backendBuildIdentityHeaderValue fixtureBuild)
      `shouldBe` Right fixtureBuild
    Aeson.eitherDecodeStrict' @ReplayImportReceipt
      (TE.encodeUtf8 $ replayImportReceiptHeaderValue receipt)
      `shouldBe` Right receipt
    Aeson.eitherDecodeStrict' @ReplayImportReceipt
      (encodeStrict $ mapRoot (KeyMap.insert "ignoredTamper" Aeson.Null) $ Aeson.toJSON receipt)
      `shouldSatisfy` isLeft
    length capabilitiesResponseHeaders `shouldBe` 1
    case lookup backendBuildIdentityHeaderName capabilitiesResponseHeaders of
      Nothing -> expectationFailure "capabilities omitted the server build identity header"
      Just value ->
        Aeson.eitherDecodeStrict' @ReplayBuildIdentity (TE.encodeUtf8 value)
          `shouldBe` Right serverBuildIdentity
    replayImportResponseHeaders fixtureBuild (Just receipt)
      `shouldBe`
        [ (backendBuildIdentityHeaderName, backendBuildIdentityHeaderValue fixtureBuild)
        , (replayImportReceiptHeaderName, replayImportReceiptHeaderValue receipt)
        ]
    replayImportResponseHeaders fixtureBuild Nothing
      `shouldBe`
        [(backendBuildIdentityHeaderName, backendBuildIdentityHeaderValue fixtureBuild)]
    Aeson.eitherDecodeStrict' @ReplayImportReceipt
      ( TE.encodeUtf8
          $ replayImportReceiptHeaderValue
            receipt
              { replayImportReceiptBackendBuild =
                  fixtureBuild {replayBuildAttestation = ReplayBuildUnattested}
              }
      )
      `shouldSatisfy` isLeft
    attestation <- case
      makeReplayAttestation
        fixtureBuild
        gameId
        provenance.provenanceSourceGameGitRevision
        receipt of
      Left err -> expectationFailure err >> fail err
      Right value -> pure value
    attestation.replayAttestationGameId `shouldBe` gameId
    attestation.replayAttestationCheckpointSha256
      `shouldBe` authority.replayImportCheckpointSha256
    attestation.replayAttestationCanonicalEnvelopeSha256
      `shouldBe` authority.replayImportCanonicalEnvelopeSha256
    attestation.replayAttestationValidatedCheckpoint
      `shouldBe` authority.replayImportValidatedCheckpoint
    attestation.replayAttestationRunningServerBuild `shouldBe` fixtureBuild
    attestation.replayAttestationImportReceipt `shouldBe` receipt
    Aeson.toJSON attestation
      `shouldBe` Aeson.object
        [ "schemaVersion" Aeson..= (1 :: Int)
        , "gameId" Aeson..= gameId
        , "gameGitRevision" Aeson..= provenance.provenanceSourceGameGitRevision
        , "checkpointSha256" Aeson..= authority.replayImportCheckpointSha256
        , "canonicalEnvelopeSha256"
            Aeson..= authority.replayImportCanonicalEnvelopeSha256
        , "validatedCheckpoint" Aeson..= expectedValidatedCheckpoint
        , "runningServerBuild" Aeson..= fixtureBuild
        , "importReceipt" Aeson..= receipt
        ]
    makeReplayAttestation fixtureBuild "00000000-0000-0000-0000-000000000011"
      provenance.provenanceSourceGameGitRevision receipt
      `shouldSatisfy` isLeft
    makeReplayAttestation fixtureBuild gameId (GitSha $ T.replicate 40 "0") receipt
      `shouldSatisfy` isLeft
    makeReplayAttestation staleBuild gameId provenance.provenanceSourceGameGitRevision receipt
      `shouldSatisfy` isLeft
    validateReplayImportReceipt
      receipt
        { replayImportReceiptPlayerRemappings =
            [playerRemapping {replayPlayerLivePlayerId = checkpoint.checkpointPlayerId & unPlayerId & UUID.toText}]
        }
      `shouldSatisfy` isLeft
    validateReplayImportReceipt
      receipt {replayImportReceiptPlayerRemappings = []}
      `shouldSatisfy` isLeft
    validateReplayImportReceipt
      ( makeReplayImportReceipt
          gameId
          [ playerRemapping
              { replayPlayerCheckpointPlayerId =
                  PlayerId $ UUID.fromWords 0 0 0 99
              }
          ]
          authority
      )
      `shouldSatisfy` isLeft
    let duplicateCheckpointPlayerRemapping =
          ReplayPlayerRemapping
            "c01002"
            checkpoint.checkpointPlayerId
            "00000000-0000-0000-0000-000000000021"
            "00000000-0000-0000-0000-000000000021"
            True
    validateReplayImportReceipt
      ( makeReplayImportReceipt
          gameId
          [playerRemapping, duplicateCheckpointPlayerRemapping]
          authority
      )
      `shouldSatisfy` isLeft
    case decodeReplayImport staleBuild encoded of
      Left _ -> pure ()
      Right _ -> expectationFailure "checkpoint import accepted a different server build"
    let sourceAttestedBuild =
          fixtureBuild
            { replayBuildSourceSha256 = T.replicate 64 "9"
            , replayBuildSourceClean = False
            , replayBuildAttestation = ReplayBuildSourceSha256
            }
        sourceAttestedProvenance =
          provenance {provenanceReplayBuild = sourceAttestedBuild}
        sourceAttestedEncoded =
          encodeStrict $ checkpointExportValue checkpointExport sourceAttestedProvenance
    case decodeReplayInput sourceAttestedBuild sourceAttestedEncoded of
      Left err -> expectationFailure err
      Right _ -> pure ()
    case decodeReplayImport sourceAttestedBuild sourceAttestedEncoded of
      Left _ -> pure ()
      Right _ ->
        expectationFailure
          "replay import accepted a source-attested but non-clean backend build"
    makeReplayAttestation
      sourceAttestedBuild
      gameId
      provenance.provenanceSourceGameGitRevision
      receipt
      `shouldSatisfy` isLeft
    traverse_
      shouldReject
      [ mapRoot (KeyMap.delete "replayCheckpoint") encodedValue
      , mapRoot (KeyMap.insert "ignoredTamper" Aeson.Null) encodedValue
      , mapEnvelope (KeyMap.delete "provenance") encodedValue
      , mapEnvelope
          ( adjustKey "provenance"
              $ mapRoot
              $ KeyMap.insert "ignoredTamper" Aeson.Null
          )
          encodedValue
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
planBytes schema mode includeAnswers = encodeStrict $ planValue schema mode includeAnswers

planValue :: Int -> Text -> Bool -> Aeson.Value
planValue schema mode includeAnswers =
  Aeson.object
    $ [ "schemaVersion" Aeson..= schema
      , "mode" Aeson..= mode
      , "source"
          Aeson..= ReplaySource
            (T.replicate 64 "a")
            ReplayOrdinaryExport
            (GitSha $ T.replicate 40 "b")
            fixtureBuild
            replayContractSchemaRevision
      , "stopAt" Aeson..= planCheckpointValue
      ]
      <> ["answers" Aeson..= ([] :: [Aeson.Value]) | includeAnswers]

planCheckpointValue :: Aeson.Value
planCheckpointValue =
  Aeson.object
    [ "type" Aeson..= ("question" :: Text)
    , "name" Aeson..= ("target" :: Text)
    , "questionVersion" Aeson..= (1 :: Int)
    , "playerId" Aeson..= ("00000000-0000-0000-0000-000000000001" :: Text)
    , "promptTag" Aeson..= ("ChooseOne" :: Text)
    , "promptSha256" Aeson..= T.replicate 64 "d"
    ]

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
shouldReject value = do
  let bytes = encodeStrict value
  when (isRight $ decodeReplayInput fixtureBuild bytes) $
    liftIO $ expectationFailure "expected replay input rejection"
  when (isRight $ decodeReplayImport fixtureBuild bytes) $
    liftIO $ expectationFailure "expected replay import rejection"

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
