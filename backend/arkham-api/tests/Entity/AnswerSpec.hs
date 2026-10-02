module Entity.AnswerSpec (spec) where

import Arkham.Campaign (lookupCampaign)
import Arkham.Campaign.Types (campaignStep)
import Arkham.CampaignStep qualified as CS
import Arkham.Classes.HasGame (getGame)
import Arkham.Difficulty
import Arkham.Token (Token (Clue, Resource))
import Data.UUID (fromWords64)
import Entity.Answer
import TestImport.New

{- | A second seat at the table. 'handleAnswerPure' only looks players up in
@gameQuestion@, so it does not need a matching investigator.
-}
otherPlayer :: PlayerId
otherPlayer = PlayerId (fromWords64 0 2)

afterSkillTestSeat :: [UI Message] -> Question Message
afterSkillTestSeat = QuestionLabel "$label.chooseAfterSkillTestEffect" Nothing . ChooseOneAtATime

answerFirstChoice :: PlayerId -> Answer
answerFirstChoice pid =
  Answer QuestionResponse {qrChoice = 0, qrPlayerId = Just pid, qrQuestionVersion = Nothing}

reparked :: [Message] -> Map PlayerId (Question Message)
reparked msgs = mconcat [m | Retain (AskMap m) <- msgs]

spec :: Spec
spec = do
  describe "CampaignStepAnswer" do
    it "rejects a stale campaign answer after a side scenario has started" . gameTest $ \self -> do
      pid <- getPlayer (toId self)
      let
        nextStep = CS.ScenarioStep "51025"
        continuation =
          CS.ContinueCampaignStep
            $ CS.Continuation nextStep True False Nothing False
        sideScenario = CS.StandaloneScenarioStep "81001" continuation
        campaign =
          overAttrs
            (\a -> a {campaignStep = sideScenario})
            (lookupCampaign "51" Easy)

      overTest $ \g ->
        g
          { gameMode =
              These
                campaign
                (fromJustNote "test harness always has a scenario" $ modeScenario g.gameMode)
          , gameQuestion =
              singletonMap pid
                $ QuestionLabel "$chooseLeadInvestigator" Nothing (ChooseOne [])
          }

      game <- getGame
      liftIO (handleAnswerPure game pid (CampaignStepAnswer sideScenario)) >>= \case
        Unhandled _ -> pure ()
        Handled messages ->
          expectationFailure
            $ "stale campaign answer was accepted: "
            <> show messages

  describe "question wrappers" do
    it "preserves nested source, label, and payment wrappers when re-asking" . gameTest $ \self -> do
      pid <- getPlayer (toId self)
      let
        question =
          QuestionWithSource GameSource Nothing
            $ QuestionLabel "wrapped" Nothing
            $ PayCostQuestion Free
            $ ChooseOne [Label "choice" [ClearUI]]
        invalidAnswer =
          Answer
            QuestionResponse
              { qrChoice = 1
              , qrPlayerId = Just pid
              , qrQuestionVersion = Nothing
              }
      overTest $ \g -> g {gameQuestion = singletonMap pid question}

      game <- getGame
      liftIO (handleAnswerPure game pid invalidAnswer) >>= \case
        Unhandled reason -> expectationFailure $ "answer rejected: " <> show reason
        Handled messages -> messages `shouldBe` [Ask pid question]

  describe "ChooseOneWizard" do
    it "runs only the finally confirmed choice" . gameTest $ \self -> do
      pid <- getPlayer (toId self)
      let question =
            ChooseOneWizard
              mempty
              [ WizardChoice "First" mempty [ClearUI]
              , WizardChoice "Second" mempty [GameOver]
              ]
              "Confirm"
              "Back"
      overTest $ \g -> g {gameQuestion = singletonMap pid question}

      game <- getGame
      liftIO (handleAnswerPure game pid (answerFirstChoice pid)) >>= \case
        Unhandled reason -> expectationFailure $ "answer rejected: " <> show reason
        Handled messages -> messages `shouldBe` [Run [ClearUI]]

  -- #4787: an after-skill-test AskMap is built from messages already popped off
  -- the queue, so nothing regenerates the seats it publishes. Before Retain, one
  -- player answering discarded every other player's option -- Unrelenting (1)'s
  -- three UnsealChaosToken messages among them, permanently shrinking the bag.
  describe "retained questions" do
    it "keeps the other seats parked when one seat answers" . gameTest $ \self -> do
      pid <- getPlayer (toId self)
      let theirs = afterSkillTestSeat [Label "Unrelenting (1)" [ClearUI]]
      overTest $ \g ->
        g
          { gameQuestion =
              mapFromList
                [ (pid, afterSkillTestSeat [Label "Quick Thinking" [ClearUI]])
                , (otherPlayer, theirs)
                ]
          , gameRetainedQuestion = True
          }

      game <- getGame
      liftIO (handleAnswerPure game pid (answerFirstChoice pid)) >>= \case
        Unhandled reason -> expectationFailure $ "answer rejected: " <> show reason
        Handled msgs -> do
          Run [ClearUI] `elem` msgs `shouldBe` True
          lookup otherPlayer (reparked msgs) `shouldBe` Just theirs

    it "folds the answering seat's remaining options into the same map" . gameTest $ \self -> do
      pid <- getPlayer (toId self)
      let
        mine = [Label "Quick Thinking" [ClearUI], Label "Nimble" [ClearUI]]
        theirs = afterSkillTestSeat [Label "Unrelenting (1)" [ClearUI]]
      overTest $ \g ->
        g
          { gameQuestion = mapFromList [(pid, afterSkillTestSeat mine), (otherPlayer, theirs)]
          , gameRetainedQuestion = True
          }

      game <- getGame
      liftIO (handleAnswerPure game pid (answerFirstChoice pid)) >>= \case
        Unhandled reason -> expectationFailure $ "answer rejected: " <> show reason
        Handled msgs -> do
          -- one AskMap holding both seats, not an Ask parked ahead of an AskMap:
          -- the table must stay free to resolve these in any order
          any (\case Ask {} -> True; _ -> False) msgs `shouldBe` False
          let question' = reparked msgs
          lookup otherPlayer question' `shouldBe` Just theirs
          lookup pid question' `shouldBe` Just (afterSkillTestSeat [Label "Nimble" [ClearUI]])

    it "still drops the other seats when the question is not retained" . gameTest $ \self -> do
      pid <- getPlayer (toId self)
      overTest $ \g ->
        g
          { gameQuestion =
              mapFromList
                [ (pid, ChooseOne [Label "mine" [ClearUI]])
                , (otherPlayer, ChooseOne [Label "theirs" [ClearUI]])
                ]
          , gameRetainedQuestion = False
          }

      game <- getGame
      liftIO (handleAnswerPure game pid (answerFirstChoice pid)) >>= \case
        Unhandled reason -> expectationFailure $ "answer rejected: " <> show reason
        Handled msgs -> do
          reparked msgs `shouldBe` mempty
          [m | AskMap m <- msgs] `shouldBe` []

  describe "amount, payment, and exchange answers" do
    let
      expectUnhandled expected game pid candidate =
        liftIO (handleAnswerPure game pid candidate) >>= \case
          Unhandled reason -> reason `shouldBe` expected
          Handled messages -> expectationFailure $ "illegal answer emitted messages: " <> show messages

      expectHandled expected game pid candidate =
        liftIO (handleAnswerPure game pid candidate) >>= \case
          Unhandled reason -> expectationFailure $ "legal answer rejected: " <> show reason
          Handled messages -> messages `shouldBe` expected

    it "rejects illegal AmountsAnswer values before emitting messages" . gameTest $ \self -> do
      pid <- getPlayer (toId self)
      baseGame <- getGame
      let
        firstChoice = fromWords64 0 10
        secondChoice = fromWords64 0 11
        unknownChoice = fromWords64 0 12
        bareQuestion =
          ChooseAmounts
            "$amount"
            (TotalAmountTarget 3)
            [ AmountChoice firstChoice "first" 1 2
            , AmountChoice secondChoice "second" 0 2
            ]
            GameTarget
        wrappedQuestion = QuestionLabel "amounts" Nothing bareQuestion
        gameFor question = baseGame {gameQuestion = singletonMap pid question, gameScenarioSteps = 12}
        answer game values =
          AmountsAnswer
            AmountsResponse
              { arAmounts = mapFromList values
              , arQuestionVersion = Just game.gameScenarioSteps
              , arPlayerId = Just pid
              }
        legalMessages =
          [ ResolveAmounts
              (toId self)
              [(NamedUUID "first" firstChoice, 1), (NamedUUID "second" secondChoice, 2)]
              GameTarget
          ]
        bareGame = gameFor bareQuestion
        wrappedGame = gameFor wrappedQuestion
      expectHandled legalMessages bareGame pid (answer bareGame [(firstChoice, 1), (secondChoice, 2)])
      expectHandled legalMessages wrappedGame pid (answer wrappedGame [(firstChoice, 1), (secondChoice, 2)])
      expectUnhandled "Wrong choice id" wrappedGame pid $
        answer wrappedGame [(firstChoice, 1), (secondChoice, 2), (unknownChoice, 0)]
      traverse_
        (expectUnhandled "Illegal amount allocation" wrappedGame pid)
        [ answer wrappedGame [(firstChoice, 1)]
        , answer wrappedGame [(firstChoice, 3), (secondChoice, 0)]
        , answer wrappedGame [(firstChoice, 1), (secondChoice, 1)]
        ]

    it "rejects illegal PaymentAmountsAnswer values before emitting messages" . gameTest $ \self -> do
      pid <- getPlayer (toId self)
      baseGame <- getGame
      let
        investigator = toId self
        firstChoice = fromWords64 0 20
        secondChoice = fromWords64 0 21
        unknownChoice = fromWords64 0 22
        bareQuestion =
          ChoosePaymentAmounts
            "$payment"
            (Just $ AmountOneOf [2, 3])
            [ PaymentAmountChoice firstChoice investigator 1 2 "first" ClearUI
            , PaymentAmountChoice secondChoice investigator 0 2 "second" GameOver
            ]
        wrappedQuestion = PayCostQuestion Free bareQuestion
        gameFor question = baseGame {gameQuestion = singletonMap pid question, gameScenarioSteps = 13}
        answer game values =
          PaymentAmountsAnswer
            PaymentAmountsResponse
              { parAmounts = mapFromList values
              , parQuestionVersion = Just game.gameScenarioSteps
              , parPlayerId = Just pid
              }
        bareGame = gameFor bareQuestion
        wrappedGame = gameFor wrappedQuestion
        legalMessages = [ClearUI, ClearUI]
      expectHandled legalMessages bareGame pid (answer bareGame [(firstChoice, 2), (secondChoice, 0)])
      expectHandled legalMessages wrappedGame pid (answer wrappedGame [(firstChoice, 2)])
      traverse_
        (expectUnhandled "Illegal amount allocation" wrappedGame pid)
        [ answer wrappedGame [(firstChoice, 2), (secondChoice, 0), (unknownChoice, 0)]
        , answer wrappedGame [(firstChoice, 0), (secondChoice, 2)]
        , answer wrappedGame [(firstChoice, 2), (secondChoice, 2)]
        ]

    it "rejects ExchangeAmountsAnswer values outside the prompted balances before emitting messages" . gameTest $ \self -> do
      pid <- getPlayer (toId self)
      baseGame <- getGame
      let
        firstInvestigator = toId self
        secondInvestigator = "01002" :: InvestigatorId
        question = ChooseExchangeAmounts GameSource firstInvestigator 2 secondInvestigator 3 Resource
        promptedGame = baseGame {gameQuestion = singletonMap pid question, gameScenarioSteps = 14}
        noPromptGame = baseGame {gameQuestion = mempty, gameScenarioSteps = 14}
        answer amount = ExchangeAmountsAnswer GameSource firstInvestigator secondInvestigator Resource amount
        reverseAnswer amount = ExchangeAmountsAnswer GameSource secondInvestigator firstInvestigator Resource amount
      expectHandled
        [MoveTokens GameSource (toSource firstInvestigator) (toTarget secondInvestigator) Resource 2]
        promptedGame
        pid
        (answer 2)
      expectHandled
        [MoveTokens GameSource (toSource secondInvestigator) (toTarget firstInvestigator) Resource 3]
        promptedGame
        pid
        (answer (-3))
      expectHandled
        [MoveTokens GameSource (toSource secondInvestigator) (toTarget firstInvestigator) Resource 3]
        promptedGame
        pid
        (reverseAnswer 3)
      traverse_
        (expectUnhandled "Illegal exchange amount" promptedGame pid)
        [ answer 3
        , answer (-4)
        , ExchangeAmountsAnswer GameSource firstInvestigator firstInvestigator Resource 1
        , ExchangeAmountsAnswer ScenarioSource firstInvestigator secondInvestigator Resource 1
        , ExchangeAmountsAnswer GameSource firstInvestigator secondInvestigator Clue 1
        , answer minBound
        ]
      expectUnhandled "Wrong question type" noPromptGame pid (answer 1)
