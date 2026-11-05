{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE NoFieldSelectors #-}
{-# LANGUAGE OverloadedStrings #-}

-- The actual JWT-authenticated Main Street swap handler against the
-- disposable database: one committed unit (both games and their step rows,
-- seats, memberships, the spent readiness and the undo floors), refusals that
-- change no row, and the canonical lock order against a double submit, a
-- reversed swap, gameplay and a concurrent event delete.
module Postgres.MainStreetSwapSpec (spec) where

import Api.Arkham.Epic (modifySharedStateLocked)
import Api.Arkham.Types.MultiplayerVariant (MultiplayerVariant (WithFriends))
import Api.Handler.Arkham.Games.Shared (MainStreetSwapSide (..), mainStreetSwapSide)
import Arkham.Card.Id (unsafeMakeCardId)
import Arkham.Classes.Entity (attr, overAttrs)
import Arkham.Difficulty (Difficulty (Easy))
import Arkham.Entities (Entities (..))
import Arkham.Epic.Types (
  EpicRole (..),
  GroupOrdinal (..),
  SharedDelta (..),
  SharedEventState,
  SharedKey (MainStreetReady),
  emptySharedEventState,
  setSharedCounter,
  sharedCounter,
 )
import Arkham.Game qualified as Game
import Arkham.Id (InvestigatorId, LocationId (..), PlayerId (..))
import Arkham.Investigator (lookupInvestigator)
import Arkham.Investigator.Types (investigatorPlacement)
import Arkham.Location (lookupLocation)
import Arkham.Message (Message (BeginRound))
import Arkham.Placement (Placement (AtLocation, Unplaced))
import Control.Concurrent (threadDelay)
import Data.Aeson (Value, decode, encode, object, toJSON, (.=))
import Data.Map.Strict qualified as Map
import Data.UUID.V4 (nextRandom)
import Database.Persist (Entity (..), SelectOpt (..), toPersistValue)
import Database.Persist qualified as P
import Database.Persist.Sql (Single (..), SqlBackend, rawSql)
import Database.Persist.Types (PersistValue (..))
import Entity.Arkham.Epic
import Entity.Arkham.Game
import Entity.Arkham.Player
import Entity.Arkham.Step
import Entity.User (UserId)
import Foundation (App)
import Network.HTTP.Types (Method, statusCode)
import Postgres.Harness
import Postgres.Production (authorizationFor, routeRequest)
import Relude hiding (first, second)
import Test.Hspec
import UnliftIO.Async (link, wait, withAsync)
import Yesod.Core (toPathPiece)

type SeedUser = SqlBackend -> Text -> IO UserId

-- What the route answered: 200, 404, 403, a refusal's status and stable
-- error code, or any other status (a 500), which fails the test.
data Outcome = Succeeded | Refused Int (Maybe Text) | Missing | Denied | Failed Int Text
  deriving stock (Eq, Ord, Show)

-- One group: its game, and the seat whose investigator the scenario readied
-- at the game's Main Street.
data Side = Side
  { gameId :: ArkhamGameId
  , user :: UserId
  , investigator :: InvestigatorId
  , mainStreet :: LocationId
  }

data Fixture = Fixture
  { eventId :: ArkhamEpicEventId
  , organizer :: UserId
  , side0 :: Side
  , side1 :: Side
  }

-- Every row a swap writes, so a test can show a refusal changed nothing.
data Rows = Rows
  { games :: [Maybe (Int, Value)]
  , steps :: [(ArkhamGameId, Int)]
  , floors :: [(ArkhamGameId, Int)]
  , seats :: [(UserId, ArkhamGameId)]
  , memberships :: [(UserId, EpicRole, Maybe Int)]
  , event :: Maybe (SharedEventState, Int)
  , epicSteps :: Int
  }
  deriving stock (Eq, Show)

notReady, stale :: Outcome
notReady = Refused 409 (Just "main-street-not-ready")
stale = Refused 409 (Just "main-street-stale")

spec :: Harness -> App -> SeedUser -> Spec
spec harness app seedUser = describe "Main Street swap" do
  it "commits both games, seats, memberships, the spent readiness and the undo floors" $
    withConnection harness \connection -> do
      f <- seedFixture connection seedUser "swap-commit"
      requestSwap app f.organizer f.eventId 0 1 `shouldReturn` Succeeded
      assertSwapped connection f
      -- Each game has a step row at its new step whose queue is empty: the
      -- pending queue ([BeginRound]) is dropped, as the next run read none
      -- before the swap was atomic.
      queues <- transaction connection $ forM [f.side0, f.side1] \side ->
        fmap (toJSON . choiceMessages . arkhamStepChoice . entityVal)
          <$> P.getBy (UniqueStep side.gameId 2)
      queues `shouldBe` replicate 2 (Just $ toJSON ([] :: [Message]))
      -- Each spend is recorded against its game's swap step.
      spends <- transaction connection $
        P.selectList [ArkhamEpicStepArkhamEpicEventId P.==. f.eventId] [Asc ArkhamEpicStepStep]
      map (spendRow . entityVal) spends
        `shouldBe` [ (1, Just f.side0.gameId, Just 2, MainStreetReady (GroupOrdinal 0), -1)
                   , (2, Just f.side1.gameId, Just 2, MainStreetReady (GroupOrdinal 1), -1)
                   ]
      -- A repeat finds both groups not ready and changes nothing.
      original <- rows connection f
      requestSwap app f.organizer f.eventId 0 1 `shouldReturn` notReady
      requestSwap app f.organizer f.eventId 1 0 `shouldReturn` notReady
      rows connection f `shouldReturn` original

  it "refuses a spent readiness with 409 and changes no row" $
    withConnection harness \connection -> do
      f <- seedFixture connection seedUser "swap-spent"
      transaction connection $ void $ modifySharedStateLocked f.eventId $
        setSharedCounter (MainStreetReady (GroupOrdinal 1)) 0
      original <- rows connection f
      requestSwap app f.organizer f.eventId 0 1 `shouldReturn` notReady
      rows connection f `shouldReturn` original

  it "refuses an investigator who left Main Street with 409 and changes no row" $
    withConnection harness \connection -> do
      f <- seedFixture connection seedUser "swap-stale"
      relocate connection f.side1 Unplaced
      original <- rows connection f
      requestSwap app f.organizer f.eventId 0 1 `shouldReturn` stale
      rows connection f `shouldReturn` original

  it "refuses invalid and unauthorized requests and changes no row" $
    withConnection harness \connection -> do
      f <- seedFixture connection seedUser "swap-invalid"
      original <- rows connection f
      requestSwap app f.organizer f.eventId 1 1
        `shouldReturn` Refused 400 (Just "main-street-same-group")
      requestSwap app f.organizer f.eventId 0 (-1)
        `shouldReturn` Refused 400 (Just "main-street-bad-group")
      requestSwap app f.organizer f.eventId 0 2 `shouldReturn` Missing
      requestSwap app f.side0.user f.eventId 0 1 `shouldReturn` Denied
      rows connection f `shouldReturn` original

  it "refuses an invalid request without waiting on the games or the event" $
    withConnection harness \holding -> withConnection harness \observer -> do
      f <- seedFixture observer seedUser "swap-invalid-unlocked"
      held <- newBarrier
      withAsync
        ( transaction holding do
            lockGameRow (lowerGame f)
            lockEventRow f.eventId
            liftIO $ pauseAt held
        )
        \holder -> do
          link holder
          awaitBarrier held
          -- Each answers while the locks are held; behind them it would time out.
          requestSwap app f.organizer f.eventId 1 1
            `shouldReturn` Refused 400 (Just "main-street-same-group")
          requestSwap app f.organizer f.eventId 0 (-1)
            `shouldReturn` Refused 400 (Just "main-street-bad-group")
          releaseBarrier held
          within "lock holder commit" $ wait holder

  it "lets one of two concurrent identical swaps win; the other is 409 with one spend" $
    withConnection harness \holding -> withConnection harness \observer -> do
      f <- seedFixture observer seedUser "swap-double"
      outcomes <-
        whileHeld
          holding
          observer
          (lockGameRow $ lowerGame f)
          [requestSwap app f.organizer f.eventId 0 1, requestSwap app f.organizer f.eventId 0 1]
      sort outcomes `shouldBe` [Succeeded, notReady]
      assertSwapped observer f

  it "serializes reversed swaps without a deadlock; one wins, the other is 409" $
    withConnection harness \holding -> withConnection harness \observer -> do
      f <- seedFixture observer seedUser "swap-reversed"
      -- Both wait on the event row while holding their games, in id order.
      outcomes <-
        whileHeld
          holding
          observer
          (lockEventRow f.eventId)
          [requestSwap app f.organizer f.eventId 0 1, requestSwap app f.organizer f.eventId 1 0]
      sort outcomes `shouldBe` [Succeeded, notReady]
      assertSwapped observer f

  it "waits behind a gameplay transaction's game lock without taking the event lock" $
    withConnection harness \holding -> withConnection harness \observer -> do
      f <- seedFixture observer seedUser "swap-gameplay"
      held <- newBarrier
      holderPid <- backendPid holding
      -- Gameplay locks its game, then the event. The swap must not hold the
      -- event row while it waits for a game, or this would deadlock.
      withAsync (transaction holding do
        lockGameRow (higherGame f)
        liftIO $ pauseAt held
        void $ modifySharedStateLocked f.eventId id) \holder -> do
          link holder
          awaitBarrier held
          withAsync (requestSwap app f.organizer f.eventId 0 1) \swapping -> do
            awaitBlockedSessions observer holderPid 1
            releaseBarrier held
            within "gameplay transaction commit" $ wait holder
            within "swap" (wait swapping) `shouldReturn` Succeeded
            assertSwapped observer f

  it "answers 404 to a swap queued behind a concurrent event delete" $
    withConnection harness \holding -> withConnection harness \observer -> do
      f <- seedFixture observer seedUser "swap-deleted"
      outcomes <- whileHeld holding observer (lockGameRow $ lowerGame f)
        [deleteEvent app f.organizer f.eventId, requestSwap app f.organizer f.eventId 0 1]
      outcomes `shouldBe` [Succeeded, Missing]
      remaining observer f `shouldReturn` (0, 0, 0)

  it "commits a swap queued ahead of a concurrent event delete, which then deletes it all" $
    withConnection harness \holding -> withConnection harness \observer -> do
      f <- seedFixture observer seedUser "swap-before-delete"
      outcomes <- whileHeld holding observer (lockGameRow $ lowerGame f)
        [requestSwap app f.organizer f.eventId 0 1, deleteEvent app f.organizer f.eventId]
      outcomes `shouldBe` [Succeeded, Succeeded]
      remaining observer f `shouldReturn` (0, 0, 0)

-- The committed swap: each game one step on and floored there, each seat and
-- membership moved with its investigator, both readinesses spent exactly once.
assertSwapped :: SqlBackend -> Fixture -> IO ()
assertSwapped connection f = do
  r <- rows connection f
  map (fmap fst) r.games `shouldBe` [Just 2, Just 2]
  r.steps
    `shouldBe` sort
      [ (f.side0.gameId, 1)
      , (f.side0.gameId, 2)
      , (f.side1.gameId, 1)
      , (f.side1.gameId, 2)
      ]
  r.floors `shouldBe` sort [(f.side0.gameId, 2), (f.side1.gameId, 2)]
  r.seats `shouldBe` sort [(f.side0.user, f.side1.gameId), (f.side1.user, f.side0.gameId)]
  r.memberships
    `shouldBe` sort
      [ (f.organizer, Organizer, Nothing)
      , (f.side0.user, GroupPlayer, Just 1)
      , (f.side1.user, GroupPlayer, Just 0)
      ]
  r.epicSteps `shouldBe` 2
  shared <- maybe (fail "event missing") (pure . fst) r.event
  map (\o -> sharedCounter (MainStreetReady (GroupOrdinal o)) shared) [0, 1] `shouldBe` [0, 0]
  -- Each game holds the other group's investigator, and neither is ready again.
  for_ [(0, f.side0, f.side1), (1, f.side1, f.side0)] \(ordinal, side, other) -> do
    game <- gameData connection side.gameId
    Map.keys (entitiesInvestigators $ Game.gameEntities game) `shouldBe` [other.investigator]
    (mainStreetSwapSide (GroupOrdinal ordinal) game).mainStreetSideReady `shouldBe` Nothing

spendRow :: ArkhamEpicStep -> (Int, Maybe ArkhamGameId, Maybe Int, SharedKey, Int)
spendRow s =
  ( arkhamEpicStepStep s
  , arkhamEpicStepArkhamGameId s
  , arkhamEpicStepGameStep s
  , sharedDeltaKey (arkhamEpicStepDelta s)
  , sharedDeltaAmount (arkhamEpicStepDelta s)
  )

-- Hold a lock in @holding@, start each action once every earlier one is
-- queued behind it, then release it. Results are in start order.
whileHeld :: SqlBackend -> SqlBackend -> ReaderT SqlBackend IO () -> [IO Outcome] -> IO [Outcome]
whileHeld holding observer lock actions = do
  held <- newBarrier
  holderPid <- backendPid holding
  withAsync (transaction holding (lock >> liftIO (pauseAt held))) \holder -> do
    link holder
    awaitBarrier held
    let go _ [] = do
          -- lock_timeout is 3s: release as soon as every action is queued.
          releaseBarrier held
          within "lock holder commit" $ wait holder
          pure []
        go queued (action : rest) = withAsync action \running -> do
          awaitBlockedSessions observer holderPid (queued + 1)
          outcomes <- go (queued + 1) rest
          outcome <- within "queued request" $ wait running
          pure (outcome : outcomes)
    go 0 actions

lowerGame, higherGame :: Fixture -> ArkhamGameId
lowerGame f = min f.side0.gameId f.side1.gameId
higherGame f = max f.side0.gameId f.side1.gameId

lockGameRow :: ArkhamGameId -> ReaderT SqlBackend IO ()
lockGameRow gameId =
  void
    ( rawSql "SELECT 1::bigint FROM arkham_games WHERE id = ? FOR UPDATE" [toPersistValue gameId]
        :: ReaderT SqlBackend IO [Single Int64]
    )

lockEventRow :: ArkhamEpicEventId -> ReaderT SqlBackend IO ()
lockEventRow eventId =
  void
    ( rawSql
        "SELECT 1::bigint FROM arkham_epic_events WHERE id = ? FOR UPDATE"
        [toPersistValue eventId]
        :: ReaderT SqlBackend IO [Single Int64]
    )

-- A Blob group game at step 1 with a pending queue, whose one investigator
-- stands at its Main Street, readied there by the scenario. @uid@ holds the seat.
seedSide :: SqlBackend -> Text -> UserId -> InvestigatorId -> IO Side
seedSide connection label user investigator = do
  now <- currentTimeMicros
  playerUuid <- nextRandom
  mainStreet <- LocationId <$> nextRandom
  cardId <- unsafeMakeCardId <$> nextRandom
  let
    playerId = PlayerId playerUuid
    initial =
      Game.setInitialScenarioMeta "mainStreetReady" investigator
        $ Game.newScenario "85001" 1 2 Easy False
    game =
      initial
        { Game.gamePlayers = [playerId]
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
    P.insertKey (ArkhamPlayerKey playerUuid) $ ArkhamPlayer user gameId "00000"
    P.insert_ $ ArkhamStep gameId (Choice mempty [BeginRound]) 1 (ActionDiff [])
    pure Side {..}

seedFixture :: SqlBackend -> SeedUser -> Text -> IO Fixture
seedFixture connection seedUser label = do
  now <- currentTimeMicros
  organizer <- seedUser connection (label <> "-organizer")
  alice <- seedUser connection (label <> "-alice")
  bob <- seedUser connection (label <> "-bob")
  side0 <- seedSide connection (label <> "-group-0") alice "01001"
  side1 <- seedSide connection (label <> "-group-1") bob "01002"
  let shared =
        setSharedCounter (MainStreetReady (GroupOrdinal 0)) 1
          $ setSharedCounter (MainStreetReady (GroupOrdinal 1)) 1
          $ emptySharedEventState 2
  transaction connection do
    eventId <-
      P.insert
        $ ArkhamEpicEvent label organizer (Just "85001") Nothing "Easy" shared 2 0 now now
    P.insert_ $ ArkhamEpicMember eventId organizer Organizer Nothing
    P.insert_ $ ArkhamEpicMember eventId alice GroupPlayer (Just 0)
    P.insert_ $ ArkhamEpicMember eventId bob GroupPlayer (Just 1)
    for_ [(0 :: Int, side0), (1, side1)] \(ordinal, side) ->
      P.insert_ $ ArkhamEpicGroup eventId ordinal (Just side.gameId) ("Group " <> show ordinal) 1
    pure Fixture {..}

-- The investigator walked away from Main Street after readying there.
relocate :: SqlBackend -> Side -> Placement -> IO ()
relocate connection side placement = do
  game <- gameData connection side.gameId
  let moved =
        game
          { Game.gameEntities =
              (Game.gameEntities game)
                { entitiesInvestigators =
                    Map.adjust
                      (overAttrs \a -> a {investigatorPlacement = placement})
                      side.investigator
                      (entitiesInvestigators $ Game.gameEntities game)
                }
          }
  transaction connection $ P.update side.gameId [ArkhamGameCurrentData P.=. moved]
  ( attr investigatorPlacement
      <$> Map.lookup side.investigator (entitiesInvestigators $ Game.gameEntities moved)
    )
    `shouldBe` Just placement

gameData :: SqlBackend -> ArkhamGameId -> IO Game.Game
gameData connection gameId =
  maybe (fail "game missing") (pure . arkhamGameCurrentData)
    =<< transaction connection (P.get gameId)

rows :: SqlBackend -> Fixture -> IO Rows
rows connection f = transaction connection do
  let gameIds = [f.side0.gameId, f.side1.gameId]
  games <- forM gameIds \gameId ->
    fmap (\g -> (arkhamGameStep g, toJSON $ arkhamGameCurrentData g)) <$> P.get gameId
  steps <- sort . map ((\s -> (arkhamStepArkhamGameId s, arkhamStepStep s)) . entityVal)
    <$> P.selectList [ArkhamStepArkhamGameId P.<-. gameIds] []
  floors <-
    sort
      . map
        ( (\u -> (arkhamGameUndoFloorArkhamGameId u, arkhamGameUndoFloorFloorStep u))
            . entityVal
        )
      <$> P.selectList [ArkhamGameUndoFloorArkhamGameId P.<-. gameIds] []
  seats <- sort . map ((\p -> (arkhamPlayerUserId p, arkhamPlayerArkhamGameId p)) . entityVal)
    <$> P.selectList [ArkhamPlayerArkhamGameId P.<-. gameIds] []
  memberships <-
    sort
      . map
        ( ( \m ->
              (arkhamEpicMemberUserId m, arkhamEpicMemberRole m, arkhamEpicMemberGroupOrdinal m)
          )
            . entityVal
        )
      <$> P.selectList [ArkhamEpicMemberArkhamEpicEventId P.==. f.eventId] []
  event <-
    fmap (\e -> (arkhamEpicEventSharedState e, arkhamEpicEventStep e)) <$> P.get f.eventId
  epicSteps <- P.count [ArkhamEpicStepArkhamEpicEventId P.==. f.eventId]
  pure Rows {..}

-- (events, games, seats) left of the fixture's aggregate.
remaining :: SqlBackend -> Fixture -> IO (Int, Int, Int)
remaining connection f = transaction connection do
  let gameIds = [f.side0.gameId, f.side1.gameId]
  (,,)
    <$> P.count [ArkhamEpicEventId P.==. f.eventId]
    <*> P.count [ArkhamGameId P.<-. gameIds]
    <*> P.count [ArkhamPlayerArkhamGameId P.<-. gameIds]

requestSwap :: App -> UserId -> ArkhamEpicEventId -> Int -> Int -> IO Outcome
requestSwap app uid eventId first second =
  route app uid "POST" (eventPath eventId "/swap-main-street")
    $ encode
    $ object ["firstGroupOrdinal" .= first, "secondGroupOrdinal" .= second]

deleteEvent :: App -> UserId -> ArkhamEpicEventId -> IO Outcome
deleteEvent app uid eventId = route app uid "DELETE" (eventPath eventId "") ""

eventPath :: ArkhamEpicEventId -> Text -> ByteString
eventPath eventId suffix = encodeUtf8 $ "/api/v1/arkham/events/" <> toPathPiece eventId <> suffix

-- One request through the production WAI application with the user's JWT, so
-- every status is the one a client sees (a 500 is 'Failed').
route :: App -> UserId -> Method -> ByteString -> LByteString -> IO Outcome
route app uid method path body = do
  authorization <- authorizationFor app uid
  (status, response) <-
    routeRequest app method path [authorization, ("Content-Type", "application/json")] body
  pure case statusCode status of
    200 -> Succeeded
    403 -> Denied
    404 -> Missing
    code
      | code `elem` [400, 409] ->
          Refused code $ Map.lookup ("error" :: Text) =<< decode @(Map Text Text) response
      | otherwise -> Failed code (decodeUtf8 response)

-- Pool sessions have no PID the test can name up front, so wait until the
-- expected number of sessions are blocked, directly or behind each other, on
-- the holder's lock.
awaitBlockedSessions :: SqlBackend -> Int64 -> Int64 -> IO ()
awaitBlockedSessions observer holder expected = within "requests queued behind the held lock" loop
 where
  loop = do
    rows' <- transaction observer $
      rawSql
        "SELECT count(*)::bigint FROM pg_stat_activity \
        \WHERE datname = current_database() AND pid <> ?::integer \
        \AND cardinality(pg_blocking_pids(pid)) > 0"
        [PersistInt64 holder]
    case rows' of
      [Single n] | n >= expected -> pure ()
      [Single _] -> threadDelay 10000 >> loop
      _ -> fail "Expected one PostgreSQL lock observation"
