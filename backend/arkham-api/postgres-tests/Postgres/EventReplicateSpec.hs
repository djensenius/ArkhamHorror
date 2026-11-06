{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE NoFieldSelectors #-}
{-# LANGUAGE OverloadedStrings #-}

-- The actual JWT-authenticated Replicating Aberration handler against the
-- disposable database: the countermeasure question it parks in an existing
-- Blob ELSE group game, and a 404 that changes nothing when the group's game
-- row is gone, whether it vanished before the handler's game read or while the
-- request waited on its lock.
module Postgres.EventReplicateSpec (spec) where

import Api.Arkham.Types.MultiplayerVariant (MultiplayerVariant (WithFriends))
import Arkham.Card.CardCode (CardCode)
import Arkham.Card.Id (unsafeMakeCardId)
import Arkham.Classes.Entity (overAttrs)
import Arkham.Difficulty (Difficulty (Easy))
import Arkham.Entities (Entities (..))
import Arkham.Epic.Types (EpicRole (..), SharedEventState, emptySharedEventState)
import Arkham.Game qualified as Game
import Arkham.Game.State (GameState (IsActive))
import Arkham.Id (InvestigatorId, LocationId (..), PlayerId (..))
import Arkham.Investigator (lookupInvestigator)
import Arkham.Investigator.Types (investigatorPlacement)
import Arkham.Location (lookupLocation)
import Arkham.Message (Message (BeginRound))
import Arkham.Placement (Placement (AtLocation))
import Arkham.Scenario.Types (ScenarioAttrs (..))
import Arkham.Target (Target (LocationTarget))
import Arkham.Token qualified as Token
import Data.Aeson (Value, encode, object, toJSON, (.=))
import Data.Map.Strict qualified as Map
import Data.UUID.V4 (nextRandom)
import Database.Persist (Entity (..), SelectOpt (..))
import Database.Persist qualified as P
import Database.Persist.Sql (SqlBackend, rawExecute)
import Entity.Arkham.Epic
import Entity.Arkham.Game
import Entity.Arkham.Step
import Entity.User (UserId)
import Foundation (App)
import Network.HTTP.Types (statusCode)
import Postgres.Harness
import Postgres.Production (authorizationFor, routeRequest)
import Relude
import Test.Hspec
import UnliftIO.Exception (finally)
import Yesod.Core (toPathPiece)

type SeedUser = SqlBackend -> Text -> IO UserId

-- What the route answered; any other status (a 500) is 'Failed'.
data Outcome = Succeeded | Missing | Failed Int Text
  deriving stock (Eq, Show)

-- An event whose group 0 plays @target@ and group 1 plays @other@, both
-- active Blob ELSE games at step 1 with Main Street in play.
data Fixture = Fixture
  { eventId :: ArkhamEpicEventId
  , organizer :: UserId
  , target :: ArkhamGameId
  , other :: ArkhamGameId
  , mainStreet :: LocationId
  }

-- Every row a replicate writes or depends on, so a test can show a refusal
-- changed nothing.
data Snapshot = Snapshot
  { event :: Maybe (SharedEventState, Int)
  , groups :: [(Int, Maybe ArkhamGameId)]
  , epicSteps :: Int
  , games :: [(ArkhamGameId, Int, Value)]
  , steps :: [(ArkhamGameId, Int)]
  }
  deriving stock (Eq, Show)

spec :: Harness -> App -> SeedUser -> Spec
spec harness app seedUser = describe "Epic event replicate" do
  it "asks an existing group game whether to spend a countermeasure" $
    withConnection harness \connection -> do
      f <- seedReplicateFixture connection seedUser "replicate-ok"
      original <- snapshot connection f
      requestReplicate app f.organizer f `shouldReturn` Succeeded
      updated <- snapshot connection f
      -- Only the target game moved on: one new step that persists the lead's
      -- countermeasure question; nothing has spawned yet.
      map (\(g, s, _) -> (g, s)) updated.games
        `shouldBe` sort [(f.target, 2), (f.other, 1)]
      updated.steps `shouldBe` sort [(f.target, 1), (f.target, 2), (f.other, 1)]
      game <- maybe (fail "game missing") (pure . arkhamGameCurrentData)
        =<< transaction connection (P.get f.target)
      Map.size (Game.gameQuestion game) `shouldBe` 1
      Map.size (entitiesEnemies $ Game.gameEntities game) `shouldBe` 0
      updated {games = [], steps = []} `shouldBe` original {games = [], steps = []}
      filter (\(g, _, _) -> g == f.other) updated.games
        `shouldBe` filter (\(g, _, _) -> g == f.other) original.games

  it "answers 404 and changes nothing when the group's game row is missing" $
    withConnection harness \connection -> do
      f <- seedReplicateFixture connection seedUser "replicate-orphan"
      -- A committed orphan: replica mode skips the foreign-key triggers, so
      -- the cascade does not fire and group 0 still names the deleted game.
      -- This is the state the handler sees when the game is deleted between
      -- its group read and its game read.
      -- Deleting the event cascades to the orphan group, so no later test
      -- meets it, even when an assertion here fails.
      (`finally` transaction connection (P.delete f.eventId)) do
        transaction connection do
          rawExecute "SET LOCAL session_replication_role = replica" []
          P.deleteWhere [ArkhamStepArkhamGameId P.==. f.target]
          P.delete f.target
        original <- snapshot connection f
        original.groups `shouldBe` [(0, Just f.target), (1, Just f.other)]
        map (\(g, _, _) -> g) original.games `shouldBe` [f.other]
        requestReplicate app f.organizer f `shouldReturn` Missing
        snapshot connection f `shouldReturn` original

  -- Guards atomicallyWithGame's get404 fallback, not the handler's game read.
  it "answers 404 and changes nothing when the game is deleted while the request waits on it" $
    withConnection harness \holding -> withConnection harness \observer -> do
      f <- seedReplicateFixture observer seedUser "replicate-deleted"
      original <- snapshot observer f
      -- The uncommitted delete holds the game's row lock. The request's plain
      -- reads still see the committed game, so it passes the handler's game
      -- read and waits in atomicallyWithGame's FOR UPDATE until the delete
      -- commits, then finds no row.
      behindHeldLock holding observer (P.delete f.target) (requestReplicate app f.organizer f)
        `shouldReturn` Missing
      -- The delete cascaded to group 0 and the game's steps; nothing else moved.
      snapshot observer f
        `shouldReturn` original
          { groups = filter ((/= 0) . fst) original.groups
          , games = filter (\(g, _, _) -> g /= f.target) original.games
          , steps = filter ((/= f.target) . fst) original.steps
          }

-- A Blob ELSE group game, actively playing at step 1, whose one investigator
-- stands at its Main Street. It holds one countermeasure, so a replicate parks
-- the printed spend-or-spawn question rather than spawning straight away.
seedReplicateGame :: SqlBackend -> Text -> InvestigatorId -> LocationId -> IO ArkhamGameId
seedReplicateGame connection label investigator mainStreet = do
  now <- currentTimeMicros
  playerId <- PlayerId <$> nextRandom
  cardId <- unsafeMakeCardId <$> nextRandom
  let
    initial =
      Game.setInitialScenarioMeta "blobThatAteEverythingElse" True
        $ Game.newScenario "85001" 1 1 Easy False
    game =
      initial
        { Game.gameGameState = IsActive
        , Game.gameMode =
            second
              (overAttrs \a -> a {scenarioTokens = Token.setTokens Token.Resource 1 (scenarioTokens a)})
              (Game.gameMode initial)
        , Game.gamePlayers = [playerId]
        , Game.gamePlayerOrder = [investigator]
        , Game.gameEntities =
            (Game.gameEntities initial)
              { entitiesInvestigators =
                  Map.singleton investigator
                    $ overAttrs (\a -> a {investigatorPlacement = AtLocation mainStreet})
                    $ lookupInvestigator investigator playerId
              , entitiesLocations =
                  Map.singleton mainStreet $ lookupLocation "89006" mainStreet cardId
              }
        }
  transaction connection do
    gameId <- P.insert $ ArkhamGame label game 1 WithFriends now now False
    P.insert_ $ ArkhamStep gameId (Choice mempty [BeginRound]) 1 (ActionDiff [])
    pure gameId

seedReplicateFixture :: SqlBackend -> SeedUser -> Text -> IO Fixture
seedReplicateFixture connection seedUser label = do
  now <- currentTimeMicros
  organizer <- seedUser connection (label <> "-organizer")
  mainStreet <- LocationId <$> nextRandom
  otherStreet <- LocationId <$> nextRandom
  target <- seedReplicateGame connection (label <> "-group-0") "01001" mainStreet
  other <- seedReplicateGame connection (label <> "-group-1") "01002" otherStreet
  transaction connection do
    eventId <-
      P.insert
        $ ArkhamEpicEvent label organizer (Just "85001") Nothing "Easy" (emptySharedEventState 2) 2 0 now now
    P.insert_ $ ArkhamEpicMember eventId organizer Organizer Nothing
    for_ [(0 :: Int, target), (1, other)] \(ordinal, gameId) ->
      P.insert_ $ ArkhamEpicGroup eventId ordinal (Just gameId) ("Group " <> show ordinal) 1
    pure Fixture {..}

snapshot :: SqlBackend -> Fixture -> IO Snapshot
snapshot connection f = transaction connection do
  let gameIds = [f.target, f.other]
  event <-
    fmap (\e -> (arkhamEpicEventSharedState e, arkhamEpicEventStep e)) <$> P.get f.eventId
  groups <-
    map ((\g -> (arkhamEpicGroupOrdinal g, arkhamEpicGroupArkhamGameId g)) . entityVal)
      <$> P.selectList [ArkhamEpicGroupArkhamEpicEventId P.==. f.eventId] [Asc ArkhamEpicGroupOrdinal]
  epicSteps <- P.count [ArkhamEpicStepArkhamEpicEventId P.==. f.eventId]
  games <-
    sort
      . map (\(Entity k g) -> (k, arkhamGameStep g, toJSON $ arkhamGameCurrentData g))
      <$> P.selectList [ArkhamGameId P.<-. gameIds] []
  steps <-
    sort . map ((\s -> (arkhamStepArkhamGameId s, arkhamStepStep s)) . entityVal)
      <$> P.selectList [ArkhamStepArkhamGameId P.<-. gameIds] []
  pure Snapshot {..}

-- Spawn a Replicating Aberration at group 0's Main Street.
requestReplicate :: App -> UserId -> Fixture -> IO Outcome
requestReplicate app uid f = do
  authorization <- authorizationFor app uid
  (status, response) <-
    routeRequest
      app
      "POST"
      (encodeUtf8 $ "/api/v1/arkham/events/" <> toPathPiece f.eventId <> "/replicate")
      [authorization, ("Content-Type", "application/json")]
      $ encode
      $ object
        [ "groupOrdinal" .= (0 :: Int)
        , "cardCode" .= ("89010b" :: CardCode)
        , "target" .= LocationTarget f.mainStreet
        ]
  pure case statusCode status of
    200 -> Succeeded
    404 -> Missing
    code -> Failed code (decodeUtf8 response)
