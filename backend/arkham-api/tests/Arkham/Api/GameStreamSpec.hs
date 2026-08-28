module Arkham.Api.GameStreamSpec (spec) where

import Api.Handler.Arkham.Games.Shared
import Entity.Answer (Answer (Answer))
import TestImport

validAnswer :: ByteString
validAnswer =
  "{\"tag\":\"Answer\",\"contents\":{\"choice\":2,\"playerId\":\"00000000-0000-0000-0000-000000000001\",\"questionVersion\":42}}"

spec :: Spec
spec = describe "Game WebSocket frame handling" do
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
 where
  isIgnored = \case
    Right Nothing -> True
    _ -> False
  isRejected = \case
    Left _ -> True
    _ -> False
