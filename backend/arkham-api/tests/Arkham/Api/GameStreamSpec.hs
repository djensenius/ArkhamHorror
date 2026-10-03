module Arkham.Api.GameStreamSpec (spec) where

import Api.Arkham.Helpers (ApiResponse (..))
import Api.Handler.Arkham.Games.Shared
import Arkham.Classes.HasGame (getGame)
import Control.Concurrent.STM qualified as STM
import Data.Aeson qualified as Aeson
import Data.Aeson.Key qualified as AesonKey
import Data.ByteString.Lazy qualified as BSL
import Data.UUID (fromWords64)
import Data.UUID qualified as UUID
import Entity.Answer (Answer (..), Reply (..), handleAnswerPure)
import Entity.Arkham.Game qualified as ArkhamGame
import Foundation (Room, Subscriber (..), broadcastToRoom, newRoom, subscribeToRoom)
import TestImport

validAnswer :: ByteString
validAnswer =
  "{\"tag\":\"Answer\",\"contents\":{\"choice\":2,\"playerId\":\"00000000-0000-0000-0000-000000000001\",\"questionVersion\":42}}"

testGameId :: ArkhamGame.ArkhamGameId
testGameId = ArkhamGame.ArkhamGameKey $ fromWords64 0 21

drainSubscriber :: Subscriber -> IO [BSL.ByteString]
drainSubscriber Subscriber {subQueue} = STM.atomically $ go []
 where
  go acc = do
    tryReadTBQueue subQueue >>= \case
      Nothing -> pure $ reverse acc
      Just msg -> go (msg : acc)

withRoomSubscribers :: (Room -> Subscriber -> Subscriber -> Subscriber -> IO ()) -> IO ()
withRoomSubscribers body = do
  room <- newRoom "game-stream-test"
  (_, sender) <- subscribeToRoom room
  (_, otherParticipant) <- subscribeToRoom room
  (_, spectator) <- subscribeToRoom room
  body room sender otherParticipant spectator

questionAnswerFrame :: Int -> PlayerId -> Int -> ByteString
questionAnswerFrame choice playerId questionVersion =
  BSL.toStrict
    $ Aeson.encode
    $ Aeson.object
      [ "tag" .= ("Answer" :: Text)
      , "contents"
          .= Aeson.object
            [ "choice" .= choice
            , "playerId" .= playerId
            , "questionVersion" .= questionVersion
            ]
      ]

amountsAnswerFrame :: UUID.UUID -> UUID.UUID -> PlayerId -> Int -> ByteString
amountsAnswerFrame firstChoice secondChoice playerId questionVersion =
  BSL.toStrict
    $ Aeson.encode
    $ Aeson.object
      [ "tag" .= ("AmountsAnswer" :: Text)
      , "contents"
          .= Aeson.object
            [ "amounts"
                .= Aeson.object
                  [ AesonKey.fromText (UUID.toText firstChoice) .= (3 :: Int)
                  , AesonKey.fromText (UUID.toText secondChoice) .= (0 :: Int)
                  ]
            , "playerId" .= playerId
            , "questionVersion" .= questionVersion
            ]
      ]

answerRejectedMessage :: Text -> Maybe Int -> BSL.ByteString
answerRejectedMessage reason questionVersion = Aeson.encode $ AnswerRejected reason questionVersion

unexpectedDecode :: String -> IO ()
unexpectedDecode err = expectationFailure $ "unexpected decode error: " <> err

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

    it "does not call the answer updater for spectator frames" do
      called <- newIORef False
      withRoomSubscribers \room sender otherParticipant spectator -> do
        let update _ = writeIORef called True >> pure (Just "must not run")
        handleGameStreamFrame SpectatorStream unexpectedDecode update sender (broadcastToRoom room) validAnswer
        readIORef called `shouldReturn` False
        drainSubscriber sender `shouldReturn` []
        drainSubscriber otherParticipant `shouldReturn` []
        drainSubscriber spectator `shouldReturn` []

  describe "answer rejection feedback" do
    it "sends a rejected amount answer AnswerRejected only to the answering subscriber and leaves the prompt unchanged" . gameTest $ \self -> do
      pid <- getPlayer (toId self)
      baseGame <- getGame
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
        gameUpdate = Aeson.encode $ GameUpdate $ PublicGame testGameId "Test game" [] game
        rejection = answerRejectedMessage "Illegal amount allocation" (Just 12)

      liftIO $ withRoomSubscribers \room sender otherParticipant spectator -> do
        let update answer = do
              reply <- handleAnswerPure game pid answer
              case reply of
                Unhandled reason -> do
                  gameQuestion game `shouldBe` singletonMap pid question
                  gameScenarioSteps game `shouldBe` 12
                  broadcastToRoom room gameUpdate
                  pure $ Just reason
                Handled messages -> expectationFailure ("illegal amount answer emitted messages: " <> show messages) >> pure Nothing
        handleGameStreamFrame ParticipantStream unexpectedDecode update sender (broadcastToRoom room) (amountsAnswerFrame firstChoice secondChoice pid 12)

        drainSubscriber sender `shouldReturn` [gameUpdate, rejection]
        drainSubscriber otherParticipant `shouldReturn` [gameUpdate]
        drainSubscriber spectator `shouldReturn` [gameUpdate]

    it "sends a stale-question AnswerRejected only to the answering subscriber and leaves the prompt unchanged" . gameTest $ \self -> do
      pid <- getPlayer (toId self)
      baseGame <- getGame
      let
        question = ChooseOne [Label "Continue" [ClearUI]]
        game = baseGame {gameQuestion = singletonMap pid question, gameScenarioSteps = 9}
        gameUpdate = Aeson.encode $ GameUpdate $ PublicGame testGameId "Test game" [] game
        rejection = answerRejectedMessage "Stale question" (Just 8)

      liftIO $ do
        room <- newRoom "game-stream-test-stale"
        (_, sender) <- subscribeToRoom room
        (_, otherParticipant) <- subscribeToRoom room
        (_, spectator) <- subscribeToRoom room
        let update answer = do
              reply <- handleAnswerPure game pid answer
              case reply of
                Unhandled reason -> do
                  gameQuestion game `shouldBe` singletonMap pid question
                  gameScenarioSteps game `shouldBe` 9
                  broadcastToRoom room gameUpdate
                  pure $ Just reason
                Handled messages -> expectationFailure ("stale answer emitted messages: " <> show messages) >> pure Nothing
        handleGameStreamFrame ParticipantStream unexpectedDecode update sender (broadcastToRoom room) (questionAnswerFrame 0 pid 8)

        drainSubscriber sender `shouldReturn` [gameUpdate, rejection]
        drainSubscriber otherParticipant `shouldReturn` [gameUpdate]
        drainSubscriber spectator `shouldReturn` [gameUpdate]

    it "does not send AnswerRejected for an accepted answer" . gameTest $ \self -> do
      pid <- getPlayer (toId self)
      baseGame <- getGame
      let
        question = ChooseOne [Label "Continue" [ClearUI]]
        game = baseGame {gameQuestion = singletonMap pid question, gameScenarioSteps = 9}
        gameUpdate = Aeson.encode $ GameUpdate $ PublicGame testGameId "Test game" [] game

      liftIO $ do
        room <- newRoom "game-stream-test-accepted"
        (_, sender) <- subscribeToRoom room
        (_, otherParticipant) <- subscribeToRoom room
        (_, spectator) <- subscribeToRoom room
        let update answer = do
              reply <- handleAnswerPure game pid answer
              case reply of
                Handled messages -> do
                  messages `shouldBe` [Run [ClearUI]]
                  broadcastToRoom room gameUpdate
                  pure Nothing
                Unhandled reason -> expectationFailure ("accepted answer rejected: " <> show reason) >> pure Nothing
        handleGameStreamFrame ParticipantStream unexpectedDecode update sender (broadcastToRoom room) (questionAnswerFrame 0 pid 9)

        drainSubscriber sender `shouldReturn` [gameUpdate]
        drainSubscriber otherParticipant `shouldReturn` [gameUpdate]
        drainSubscriber spectator `shouldReturn` [gameUpdate]
 where
  isIgnored = \case
    Right Nothing -> True
    _ -> False
  isRejected = \case
    Left _ -> True
    _ -> False
