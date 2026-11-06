module Arkham.Api.ViewerSpec (spec) where

import Api.Arkham.Redact (Viewer (..), redactFor)
import Api.Arkham.Viewer (GameView (..), Redacted (..), gameViewFor, redactGameField)
import Arkham.Id (PlayerId (..))
import Arkham.Prelude
import Data.Aeson qualified as Aeson
import Data.Aeson.KeyMap qualified as KeyMap
import Data.Set qualified as Set
import Data.UUID qualified as UUID
import Database.Persist.Sql (toSqlKey)
import Entity.User (UserId)
import Helpers.Contracts (loadContractJson)
import Test.Hspec

alice, bob :: UserId
alice = toSqlKey 1
bob = toSqlKey 2

seat :: Word32 -> PlayerId
seat = PlayerId . UUID.fromWords 0x2221 0 0

-- | A response with a game whose two investigators each hold a hand.
response :: Value
response =
  object
    [ "playerId" .= seat 1
    , "game"
        .= object
          [ "investigators"
              .= object
                [ "01001" .= object ["playerId" .= seat 1, "hand" .= [1 :: Int, 2]]
                , "01002" .= object ["playerId" .= seat 2, "hand" .= [3 :: Int]]
                ]
          ]
    ]

spec :: Spec
spec = describe "Api.Arkham.Viewer" do
  describe "gameViewFor" do
    it "gives the full view to a caller holding every seat row" do
      gameViewFor alice [(alice, seat 1), (alice, seat 2)] `shouldBe` FullView
      gameViewFor alice [(alice, seat 1)] `shouldBe` FullView

    it "gives the caller's seats when others hold the rest" do
      gameViewFor alice [(alice, seat 1), (bob, seat 2)]
        `shouldBe` RedactedView (ViewerSeats (Set.singleton (seat 1)))
      gameViewFor bob [(alice, seat 1), (bob, seat 2), (bob, seat 3)]
        `shouldBe` RedactedView (ViewerSeats (Set.fromList [seat 2, seat 3]))

    it "gives a non-member, or a game with no seat rows, the spectator view" do
      gameViewFor bob [(alice, seat 1), (alice, seat 2)] `shouldBe` RedactedView ViewerSpectator
      gameViewFor alice [] `shouldBe` RedactedView ViewerSpectator

  describe "Redacted" do
    it "encodes FullView exactly as the wrapped value" do
      toJSON (Redacted FullView response) `shouldBe` response
      encode (Redacted FullView response) `shouldBe` encode response

    it "redacts only the value under game, the same way through toJSON and toEncoding" do
      let viewer = ViewerSeats (Set.singleton (seat 1))
          expected =
            object
              [ "playerId" .= seat 1
              , "game" .= redactFor viewer (fromMaybe Aeson.Null (lookupGame response))
              ]
      toJSON (Redacted (RedactedView viewer) response) `shouldBe` expected
      Aeson.decode (encode (Redacted (RedactedView viewer) response)) `shouldBe` Just expected
      redactGameField viewer response `shouldBe` expected
      -- The other seat's hand really is gone, and the caller's is intact.
      expected
        `shouldBe` object
          [ "playerId" .= seat 1
          , "game"
              .= object
                [ "investigators"
                    .= object
                      [ "01001" .= object ["playerId" .= seat 1, "hand" .= [1 :: Int, 2]]
                      , "01002" .= object ["playerId" .= seat 2, "hand" .= ([] :: [Int])]
                      ]
                ]
          ]

    it "keeps the governed get-game fixture unchanged for every view" do
      -- The fixture's only investigator (seat ...0001) holds no hidden cards,
      -- so redaction must neither add a field (no handCount yet) nor change
      -- the shape the get-game schema and fixture checks pin.
      fixture <- loadContractJson "contracts/fixtures/get-game.json" :: IO Value
      let fixtureSeat = PlayerId (UUID.fromWords 0 0 0 1)
      for_
        [ViewerSpectator, ViewerSeats (Set.singleton fixtureSeat), ViewerSeats (Set.singleton (seat 9))]
        \viewer -> do
          toJSON (Redacted (RedactedView viewer) fixture) `shouldBe` fixture
          Aeson.decode (encode (Redacted (RedactedView viewer) fixture)) `shouldBe` Just fixture

    it "leaves a response with no game untouched" do
      let other = object ["totalSteps" .= (3 :: Int)]
      redactGameField ViewerSpectator other `shouldBe` other
 where
  lookupGame = \case
    Aeson.Object o -> KeyMap.lookup "game" o
    _ -> Nothing
