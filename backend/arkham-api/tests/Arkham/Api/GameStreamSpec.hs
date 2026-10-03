module Arkham.Api.GameStreamSpec (spec) where

import Api.Arkham.Helpers (ApiResponse (GameError))
import Api.Handler.Arkham.Games.Shared
import Arkham.Classes.HasGame (getGame)
import Control.Concurrent.STM qualified as STM
import Data.Aeson qualified as Aeson
import Data.ByteString.Lazy qualified as BSL
import Data.UUID (fromWords64)
import Entity.Answer (Answer (..), AmountsResponse (..), QuestionResponse (..), Reply (..), handleAnswerPure)
import Foundation (Subscriber (..))
import TestImport

validAnswer :: ByteString
validAnswer =
  "{\"tag\":\"Answer\",\"contents\":{\"choice\":2,\"playerId\":\"00000000-0000-0000-0000-000000000001\",\"questionVersion\":42}}"

newTestSubscriber :: IO Subscriber
newTestSubscriber = STM.atomically do
  subQueue <- newTBQueue 8
  subOverflow <- STM.newTVar False
  pure Subscriber {..}

drainSubscriber :: Subscriber -> IO [BSL.ByteString]
drainSubscriber Subscriber {subQueue} = STM.atomically $ go []
 where
  go acc = do
    tryReadTBQueue subQueue >>= \case
      Nothing -> pure $ reverse acc
      Just msg -> go (msg : acc)

notifyRejectedAnswer :: Subscriber -> Reply -> IO ()
notifyRejectedAnswer subscriber = traverse_ (sendAnswerRejection subscriber) . answerRejectionReason

spec :: Spec
spec = do
  describe "Game WebSocket frame handling" do
    it "decodes participant answer frames" do
      case decodeGameStreamAnswer ParticipantStream validAnswer of
        Right (Just (Answer _)) -> pure ()
        result -> expectationFailure $ "Expected a participant answer, got: " <> show result

    it "rejects malformed participant frames" do
      decodeGameStreamAnswer ParticipantStream "not json"
        `shouldSatisfy` isRejected

    it "ignores valid spectator answer frames" do
      decodeGameStreamAnswer SpectatorStream validAnswer
        `shouldSatisfy` isIgnored

    it "ignores malformed spectator frames" do
      decodeGameStreamAnswer SpectatorStream "not json"
        `shouldSatisfy` isIgnored

  describe "answer rejection feedback" do
    it "sends a rejected amount answer GameError only to the answering subscriber and leaves the prompt unchanged" . gameTest $ \self -> do
      pid <- getPlayer (toId self)
      baseGame <- getGame
      sender <- liftIO newTestSubscriber
      otherParticipant <- liftIO newTestSubscriber
      spectator <- liftIO newTestSubscriber
      let
        firstChoice = fromWords64 0 10
        secondChoice = fromWords64 0 11
        question =
          ChooseAmounts
            "$amount"
            (TotalAmountTarget 3)
            [ AmountChoice firstChoice "first" 1 2
            , AmountChoice secondChoice "second" 0 2
            ]
            GameTarget
        game = baseGame {gameQuestion = singletonMap pid question, gameScenarioSteps = 12}
        rejectedAnswer =
          AmountsAnswer
            AmountsResponse
              { arAmounts = mapFromList [(firstChoice, 3), (secondChoice, 0)]
              , arQuestionVersion = Just game.gameScenarioSteps
              , arPlayerId = Just pid
              }

      reply <- liftIO $ handleAnswerPure game pid rejectedAnswer
      liftIO $ case reply of
        Unhandled reason -> reason `shouldBe` "Illegal amount allocation"
        Handled messages -> expectationFailure $ "illegal amount answer emitted messages: " <> show messages
      liftIO $ gameQuestion game `shouldBe` singletonMap pid question
      liftIO $ gameScenarioSteps game `shouldBe` 12
      liftIO $ notifyRejectedAnswer sender reply

      liftIO $ drainSubscriber sender `shouldReturn` [Aeson.encode $ GameError "Illegal amount allocation"]
      liftIO $ drainSubscriber otherParticipant `shouldReturn` []
      liftIO $ drainSubscriber spectator `shouldReturn` []

    it "sends a stale-question GameError only to the answering subscriber and leaves the prompt unchanged" . gameTest $ \self -> do
      pid <- getPlayer (toId self)
      baseGame <- getGame
      sender <- liftIO newTestSubscriber
      otherParticipant <- liftIO newTestSubscriber
      spectator <- liftIO newTestSubscriber
      let
        question = ChooseOne [Label "Continue" [ClearUI]]
        game = baseGame {gameQuestion = singletonMap pid question, gameScenarioSteps = 9}
        staleAnswer = Answer QuestionResponse {qrChoice = 0, qrPlayerId = Just pid, qrQuestionVersion = Just 8}

      reply <- liftIO $ handleAnswerPure game pid staleAnswer
      liftIO $ case reply of
        Unhandled reason -> reason `shouldBe` "Stale question"
        Handled messages -> expectationFailure $ "stale answer emitted messages: " <> show messages
      liftIO $ gameQuestion game `shouldBe` singletonMap pid question
      liftIO $ gameScenarioSteps game `shouldBe` 9
      liftIO $ notifyRejectedAnswer sender reply

      liftIO $ drainSubscriber sender `shouldReturn` [Aeson.encode $ GameError "Stale question"]
      liftIO $ drainSubscriber otherParticipant `shouldReturn` []
      liftIO $ drainSubscriber spectator `shouldReturn` []

    it "does not send GameError for an accepted answer" . gameTest $ \self -> do
      pid <- getPlayer (toId self)
      baseGame <- getGame
      sender <- liftIO newTestSubscriber
      let
        question = ChooseOne [Label "Continue" [ClearUI]]
        game = baseGame {gameQuestion = singletonMap pid question, gameScenarioSteps = 9}
        acceptedAnswer = Answer QuestionResponse {qrChoice = 0, qrPlayerId = Just pid, qrQuestionVersion = Just 9}

      reply <- liftIO $ handleAnswerPure game pid acceptedAnswer
      liftIO $ case reply of
        Handled messages -> messages `shouldBe` [Run [ClearUI]]
        Unhandled reason -> expectationFailure $ "accepted answer rejected: " <> show reason
      liftIO $ notifyRejectedAnswer sender reply

      liftIO $ drainSubscriber sender `shouldReturn` []
 where
  isIgnored = \case
    Right Nothing -> True
    _ -> False
  isRejected = \case
    Left _ -> True
    _ -> False
