{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE NoFieldSelectors #-}
{-# LANGUAGE OverloadedStrings #-}

-- Per-viewer redaction on the REST game reads (TASK-2.22.1): GET game,
-- spectate, replay, the pending game GET and PUT join, create and import,
-- through the production WAI application, so the caller's seats come from the
-- real arkham_players rows and the body is the real wire bytes.
module Postgres.RedactRestSpec (spec) where

import Api.Arkham.Export (ArkhamExport (..), ArkhamGameExportData (..))
import Api.Arkham.Types.MultiplayerVariant (MultiplayerVariant (Solo, WithFriends))
import Arkham.Asset.Cards.NightOfTheZealot qualified as Assets
import Arkham.Card (Card (PlayerCard))
import Arkham.Card.Id (unsafeMakeCardId)
import Arkham.Card.PlayerCard (lookupPlayerCard)
import Arkham.Classes.Entity (overAttrs)
import Arkham.Difficulty (Difficulty (Easy))
import Arkham.Entities (entitiesInvestigators)
import Arkham.Game qualified as Game
import Arkham.Helpers (Deck (..))
import Arkham.Id (PlayerId (..))
import Arkham.Investigator (lookupInvestigator)
import Arkham.Investigator.Types (InvestigatorAttrs (..))
import Data.Aeson (Value (..), decode, encode, object, toJSON, (.=))
import Data.Aeson.KeyMap qualified as KeyMap
import Data.Map.Strict qualified as Map
import Data.UUID (UUID)
import Data.UUID qualified as UUID
import Database.Persist (toPersistValue)
import Database.Persist qualified as P
import Database.Persist.Sql (SqlBackend, rawExecute)
import Entity.Arkham.Game
import Entity.Arkham.Player
import Entity.User (UserId)
import Foundation (App)
import Network.HTTP.Types (Status, hContentType, status200, status404)
import Postgres.Harness
import Postgres.Production (authorizationFor, routeRequest)
import Relude
import Test.Hspec
import Yesod.Core (fromPathPiece, toPathPiece)

type SeedUser = SqlBackend -> Text -> IO UserId

-- | (hand size, deck size) of each investigator, keyed by its playerId.
type Zones = Map Text (Int, Int)

seatUuid :: Word32 -> UUID
seatUuid = UUID.fromWords 0x2221 0 0

seatText :: Word32 -> Text
seatText = UUID.toText . seatUuid

-- Seat 1 (Roland) holds 2 hand and 3 deck cards; seat 2 (Daisy) 1 and 2.
seededZones :: Zones
seededZones = Map.fromList [(seatText 1, (2, 3)), (seatText 2, (1, 2))]

spec :: Harness -> App -> SeedUser -> Spec
spec harness app seedUser = describe "REST game reads are redacted per viewer" do
  it "shows each seat its own hand and deck, not the other seat's (GET game)" $
    withConnection harness \connection -> do
      gameId <- seedGame connection "redact-get" WithFriends
      alice <- seedUser connection "redact-get-alice"
      bob <- seedUser connection "redact-get-bob"
      seat connection gameId alice 1
      seat connection gameId bob 2
      zonesOf <$> request alice (gamePath gameId "")
        `shouldReturn` (status200, Map.fromList [(seatText 1, (2, 3)), (seatText 2, (0, 0))])
      zonesOf <$> request bob (gamePath gameId "")
        `shouldReturn` (status200, Map.fromList [(seatText 1, (0, 0)), (seatText 2, (1, 2))])

  it "still refuses GET game to a non-member" $
    withConnection harness \connection -> do
      gameId <- seedGame connection "redact-get-stranger" WithFriends
      alice <- seedUser connection "redact-get-stranger-alice"
      carol <- seedUser connection "redact-get-stranger-carol"
      seat connection gameId alice 1
      fst <$> request carol (gamePath gameId "") `shouldReturn` status404

  it "gives a caller holding every seat row the full game, even when the row ids don't match" $
    withConnection harness \connection -> do
      -- An imported Solo game keeps the exporter's investigator player ids,
      -- so the importer's rows (fresh ids) match neither investigator.
      gameId <- seedGame connection "redact-all-seats" Solo
      dave <- seedUser connection "redact-all-seats-dave"
      transaction connection do
        P.insert_ $ ArkhamPlayer dave gameId "c01001"
        P.insert_ $ ArkhamPlayer dave gameId "c01002"
      zonesOf <$> request dave (gamePath gameId "") `shouldReturn` (status200, seededZones)

  it "always shows spectate as a spectator, even to a seated caller" $
    withConnection harness \connection -> do
      gameId <- seedGame connection "redact-spectate" WithFriends
      alice <- seedUser connection "redact-spectate-alice"
      carol <- seedUser connection "redact-spectate-carol"
      seat connection gameId alice 1
      let hidden = Map.fromList [(seatText 1, (0, 0)), (seatText 2, (0, 0))]
      for_ [alice, carol] \caller ->
        zonesOf <$> request caller (gamePath gameId "/spectate") `shouldReturn` (status200, hidden)

  it "redacts a replay step for a seated player, and shows a seatless admin the spectator view" $
    withConnection harness \connection -> do
      gameId <- seedGame connection "redact-replay" WithFriends
      alice <- seedUser connection "redact-replay-alice"
      bob <- seedUser connection "redact-replay-bob"
      admin <- seedUser connection "redact-replay-admin"
      carol <- seedUser connection "redact-replay-carol"
      seat connection gameId alice 1
      seat connection gameId bob 2
      transaction connection $
        rawExecute "UPDATE users SET admin = TRUE WHERE id = ?" [toPersistValue admin]
      zonesOf <$> request alice (gamePath gameId "/replay/0")
        `shouldReturn` (status200, Map.fromList [(seatText 1, (2, 3)), (seatText 2, (0, 0))])
      zonesOf <$> request admin (gamePath gameId "/replay/0")
        `shouldReturn` (status200, Map.fromList [(seatText 1, (0, 0)), (seatText 2, (0, 0))])
      fst <$> request carol (gamePath gameId "/replay/0") `shouldReturn` status404

  it "redacts the pending game for each seat, and shows a non-member the spectator view" $
    withConnection harness \connection -> do
      gameId <- seedGame connection "redact-pending" WithFriends
      alice <- seedUser connection "redact-pending-alice"
      bob <- seedUser connection "redact-pending-bob"
      carol <- seedUser connection "redact-pending-carol"
      seat connection gameId alice 1
      seat connection gameId bob 2
      gameZonesOf <$> request alice (gamePath gameId "/join")
        `shouldReturn` (status200, Map.fromList [(seatText 1, (2, 3)), (seatText 2, (0, 0))])
      gameZonesOf <$> request bob (gamePath gameId "/join")
        `shouldReturn` (status200, Map.fromList [(seatText 1, (0, 0)), (seatText 2, (1, 2))])
      gameZonesOf <$> request carol (gamePath gameId "/join")
        `shouldReturn` (status200, Map.fromList [(seatText 1, (0, 0)), (seatText 2, (0, 0))])

  it "redacts the join response for the caller's seats as they stand after the join" $
    withConnection harness \connection -> do
      -- A first joiner holds every seat row once the join commits, so a
      -- lookup that missed the new row would wrongly answer the spectator
      -- view here.
      lobby <- seedGame connection "redact-join-first" WithFriends
      dave <- seedUser connection "redact-join-first-dave"
      gameZonesOf <$> joinAs dave lobby `shouldReturn` (status200, seededZones)
      seatOwners connection lobby `shouldReturn` [dave]

      gameId <- seedGame connection "redact-join" WithFriends
      alice <- seedUser connection "redact-join-alice"
      bob <- seedUser connection "redact-join-bob"
      seat connection gameId alice 1
      -- Bob's new seat has no investigator yet: Alice's hand and deck stay
      -- hidden from him.
      gameZonesOf <$> joinAs bob gameId
        `shouldReturn` (status200, Map.fromList [(seatText 1, (0, 0)), (seatText 2, (0, 0))])
      seatOwners connection gameId `shouldReturn` sort [alice, bob]
      -- Joining again leaves the seats alone and shows Alice her own zones only.
      gameZonesOf <$> joinAs alice gameId
        `shouldReturn` (status200, Map.fromList [(seatText 1, (2, 3)), (seatText 2, (0, 0))])

  it "answers a create with the creator's view of the new game" $
    withConnection harness \connection -> do
      dave <- seedUser connection "redact-create-dave"
      authorization <- authorizationFor app dave
      (status, body) <-
        routeRequest app "POST" "/api/v1/arkham/games"
          [authorization, (hContentType, "application/json")]
          (encode createBody)
      status `shouldBe` status200
      let mGameId = do
            Object response <- decode body
            String gid <- KeyMap.lookup "id" response
            fromPathPiece gid
      gameId <- maybe (expectationFailure "create returned no game id" >> error "no id") pure mGameId
      seatOwners connection gameId `shouldReturn` [dave, dave]
      -- The creator holds every seat, so the body is the stored game unredacted.
      stored <- transaction connection $ P.get gameId
      decode body `shouldBe` fmap (\g -> toJSON (Game.PublicGame gameId g.name [] g.currentData)) stored

  it "answers a Solo import with the full game for the importer" $
    withConnection harness \connection -> do
      source <- seedGame connection "redact-import-source" Solo
      dave <- seedUser connection "redact-import-dave"
      exported <- transaction connection $ P.get source
      game <- maybe (expectationFailure "no source game" >> error "no source game") pure exported
      authorization <- authorizationFor app dave
      let export =
            ArkhamExport
              { aeCampaignPlayers = []
              , aeCampaignData =
                  ArkhamGameExportData
                    { agedName = "redact-import"
                    , agedCurrentData = game.currentData
                    , agedStep = 1
                    , agedSteps = []
                    , agedLog = []
                    , agedMultiplayerVariant = Solo
                    }
              }
          boundary = "redact-import-boundary"
          multipart =
            mconcat
              [ "--" <> boundary <> "\r\n"
              , "Content-Disposition: form-data; name=\"debugFile\"; filename=\"export.json\"\r\n"
              , "Content-Type: application/json\r\n\r\n"
              , encode export
              , "\r\n--" <> boundary <> "--\r\n"
              ]
      response <-
        routeRequest app "POST" "/api/v1/arkham/games/import"
          [authorization, (hContentType, "multipart/form-data; boundary=" <> toStrict boundary)]
          multipart
      -- The import keeps the exporter's player ids, and its seat rows (all
      -- the importer's) match neither: only the full view shows the zones.
      gameZonesOf response `shouldBe` (status200, seededZones)
 where
  request caller path = do
    authorization <- authorizationFor app caller
    routeRequest app "GET" path [authorization] ""
  joinAs caller gameId = do
    authorization <- authorizationFor app caller
    routeRequest app "PUT" (gamePath gameId "/join") [authorization] ""
  createBody =
    object
      [ "deckIds" .= ([] :: [Value])
      , "playerCount" .= (2 :: Int)
      , "campaignId" .= ("01" :: Text)
      , "difficulty" .= ("Easy" :: Text)
      , "campaignName" .= ("redact-create" :: Text)
      , "multiplayerVariant" .= ("Solo" :: Text)
      , "includeTarotReadings" .= False
      , "options" .= ([] :: [Value])
      ]

-- | The users holding the game's seat rows, sorted.
seatOwners :: SqlBackend -> ArkhamGameId -> IO [UserId]
seatOwners connection gameId =
  sort . map (arkhamPlayerUserId . P.entityVal)
    <$> transaction connection (P.selectList [ArkhamPlayerArkhamGameId P.==. gameId] [])

gamePath :: ArkhamGameId -> Text -> ByteString
gamePath gameId suffix = encodeUtf8 $ "/api/v1/arkham/games/" <> toPathPiece gameId <> suffix

-- | Take a seat whose arkham_players id is the investigator's playerId.
seat :: SqlBackend -> ArkhamGameId -> UserId -> Word32 -> IO ()
seat connection gameId uid n =
  transaction connection
    $ P.insertKey (ArkhamPlayerKey $ seatUuid n)
    $ ArkhamPlayer uid gameId (if n == 1 then "c01001" else "c01002")

-- | A two-investigator game whose hands and decks hold real cards.
seedGame :: SqlBackend -> Text -> MultiplayerVariant -> IO ArkhamGameId
seedGame connection label variant = do
  now <- currentTimeMicros
  let initial = Game.newCampaign "01" Nothing 1 2 Easy False
      card n = lookupPlayerCard Assets.flashlight (unsafeMakeCardId $ UUID.fromWords 0x2221 1 0 n)
      investigator code n handIds deckIds =
        overAttrs
          (\attrs -> attrs
            { investigatorHand = map (PlayerCard . card) handIds
            , investigatorDeck = Deck (map card deckIds)
            })
          (lookupInvestigator code (PlayerId $ seatUuid n))
      game = initial
        { Game.gamePlayerOrder = ["01001", "01002"]
        , Game.gameEntities = initial.gameEntities
            { entitiesInvestigators = Map.fromList
                [ ("01001", investigator "01001" 1 [1, 2] [3, 4, 5])
                , ("01002", investigator "01002" 2 [6] [7, 8])
                ]
            }
        }
  transaction connection $ P.insert $ ArkhamGame label game 1 variant now now False

-- | The status, and each investigator's zone sizes in the response's game.
zonesOf :: (Status, LByteString) -> (Status, Zones)
zonesOf (status, body) = (status, fromMaybe mempty zones)
 where
  zones = do
    Object response <- decode body
    Object game <- KeyMap.lookup "game" response
    investigatorZones game

-- | 'zonesOf' for a response that is itself the game (pending, create, import).
gameZonesOf :: (Status, LByteString) -> (Status, Zones)
gameZonesOf (status, body) = (status, fromMaybe mempty zones)
 where
  zones = do
    Object game <- decode body
    investigatorZones game

-- | Each investigator's (hand, deck) sizes in an encoded game, by playerId.
investigatorZones :: KeyMap.KeyMap Value -> Maybe Zones
investigatorZones game = do
  Object investigators <- KeyMap.lookup "investigators" game
  pure $ Map.fromList
    [ (playerId, (size "hand" investigator, size "deck" investigator))
    | Object investigator <- KeyMap.elems investigators
    , Just (String playerId) <- [KeyMap.lookup "playerId" investigator]
    ]
 where
  size key investigator = case KeyMap.lookup key investigator of
    Just (Array cards) -> length cards
    _ -> -1
