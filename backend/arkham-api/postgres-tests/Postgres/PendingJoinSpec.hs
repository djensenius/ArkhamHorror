{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

-- The actual JWT-authenticated pending-game join against the disposable
-- database: the one-group-per-event Epic membership is checked and reserved
-- under the game lock, concurrent joins never answer 500, and the event roster
-- is announced only when a join seats the player.
module Postgres.PendingJoinSpec (spec) where

import Api.Arkham.Helpers (ApiResponse (EventChanged), joinEventRoom)
import Api.Arkham.Types.MultiplayerVariant (MultiplayerVariant (WithFriends))
import Api.Handler.Arkham.PendingGames (putApiV1ArkhamPendingGameR)
import Arkham.Difficulty (Difficulty (Easy))
import Arkham.Epic.Types (EpicRole (GroupPlayer), emptySharedEventState)
import Arkham.Game qualified as Game
import Control.Concurrent (threadDelay)
import Data.Aeson (encode)
import Database.Persist (Entity (..), toPersistValue)
import Database.Persist qualified as P
import Database.Persist.Sql (Single (..), SqlBackend, rawSql)
import Database.Persist.Types (PersistValue (..))
import Entity.Arkham.Epic
import Entity.Arkham.Game
import Entity.Arkham.Player
import Entity.User (UserId)
import Foundation (App, Handler, Subscriber (..), unsafeHandler, userIdToToken)
import Network.Wai (defaultRequest, requestHeaders, requestMethod)
import Postgres.Harness
import Postgres.Production (withRequest)
import Relude hiding (first, second)
import Test.Hspec
import UnliftIO.Async (link, wait, withAsync)
import UnliftIO.Exception qualified as Exception
import UnliftIO.STM (flushTBQueue)
import Yesod.Core.Types (ErrorResponse (..), HandlerContents (..))

type SeedUser = SqlBackend -> Text -> IO UserId

-- What the route answered. A permission failure is the HCError Yesod renders
-- as 403; anything else, a 500 included, fails the test.
data Outcome = Succeeded | Forbidden Text
  deriving stock (Eq, Ord, Show)

data EpicFixture = EpicFixture
  { eventId :: ArkhamEpicEventId
  , group0 :: ArkhamGameId
  , group1 :: ArkhamGameId
  }

anotherGroup :: Text
anotherGroup = "You already occupy a seat in another group in this event"

spec :: Harness -> App -> SeedUser -> Spec
spec harness app seedUser = describe "Pending game join" do
  it "lets one of two concurrent first joins to different groups of an event seat, and refuses the other" $
    withConnection harness \holding -> withConnection harness \observer -> do
      organizer <- seedUser observer "join-groups-organizer"
      player <- seedUser observer "join-groups-player"
      f <- seedEvent observer "join-groups" organizer
      announcements <- watchEvent app f
      -- Hold both group games, so both joins have started (and, before the
      -- membership check moved under the lock, passed it) when they go on.
      (firstOutcome, secondOutcome) <-
        joinsBehindLock holding observer [f.group0, f.group1]
          (pendingJoin app player f.group0)
          (pendingJoin app player f.group1)
      sort [firstOutcome, secondOutcome] `shouldBe` [Succeeded, Forbidden anotherGroup]
      let (winner, winnerOrdinal, loser) =
            if firstOutcome == Succeeded then (f.group0, 0, f.group1) else (f.group1, 1, f.group0)
      seatRows observer winner `shouldReturn` [player]
      seatRows observer loser `shouldReturn` []
      membership observer f player `shouldReturn` [(GroupPlayer, Just winnerOrdinal)]
      announcements `shouldReturn` 1

  it "answers two concurrent joins to the same game with one seat" $
    withConnection harness \holding -> withConnection harness \observer -> do
      organizer <- seedUser observer "join-same-organizer"
      player <- seedUser observer "join-same-player"
      f <- seedEvent observer "join-same" organizer
      announcements <- watchEvent app f
      outcomes <-
        joinsBehindLock holding observer [f.group0]
          (pendingJoin app player f.group0)
          (pendingJoin app player f.group0)
      outcomes `shouldBe` (Succeeded, Succeeded)
      seatRows observer f.group0 `shouldReturn` [player]
      membership observer f player `shouldReturn` [(GroupPlayer, Just 0)]
      -- Only the join that seated the player changed the roster.
      announcements `shouldReturn` 1

  it "refuses a groupless or other-group membership before taking a seat" $
    withConnection harness \connection -> do
      organizer <- seedUser connection "join-refused-organizer"
      groupless <- seedUser connection "join-refused-groupless"
      elsewhere <- seedUser connection "join-refused-elsewhere"
      f <- seedEvent connection "join-refused" organizer
      transaction connection do
        P.insert_ $ ArkhamEpicMember f.eventId groupless GroupPlayer Nothing
        P.insert_ $ ArkhamEpicMember f.eventId elsewhere GroupPlayer (Just 1)
      announcements <- watchEvent app f
      pendingJoin app groupless f.group0 `shouldReturn` Forbidden anotherGroup
      pendingJoin app elsewhere f.group0 `shouldReturn` Forbidden anotherGroup
      seatRows connection f.group0 `shouldReturn` []
      gameStep connection f.group0 `shouldReturn` 1
      membership connection f groupless `shouldReturn` [(GroupPlayer, Nothing)]
      membership connection f elsewhere `shouldReturn` [(GroupPlayer, Just 1)]
      announcements `shouldReturn` 0
      -- The player's own group still seats them, through the existing row.
      pendingJoin app elsewhere f.group1 `shouldReturn` Succeeded
      seatRows connection f.group1 `shouldReturn` [elsewhere]
      membership connection f elsewhere `shouldReturn` [(GroupPlayer, Just 1)]
      announcements `shouldReturn` 1

  it "announces nothing for a join that does not seat, and keeps its 403" $
    withConnection harness \connection -> do
      organizer <- seedUser connection "join-seated-organizer"
      player <- seedUser connection "join-seated-player"
      stray <- seedUser connection "join-seated-stray"
      f <- seedEvent connection "join-seated" organizer
      pendingJoin app player f.group0 `shouldReturn` Succeeded
      -- A seat whose membership names another group, as only old data has.
      transaction connection do
        P.insert_ $ ArkhamPlayer stray f.group0 "00000"
        P.insert_ $ ArkhamEpicMember f.eventId stray GroupPlayer (Just 1)
      announcements <- watchEvent app f
      pendingJoin app player f.group0 `shouldReturn` Succeeded
      pendingJoin app stray f.group0 `shouldReturn` Forbidden anotherGroup
      sort <$> seatRows connection f.group0 `shouldReturn` sort [player, stray]
      announcements `shouldReturn` 0

-- A WithFriends campaign lobby with two seats, so a first join leaves it
-- pending and runs no setup.
seedGame :: SqlBackend -> Text -> IO ArkhamGameId
seedGame connection label = do
  now <- currentTimeMicros
  let game = Game.newCampaign "01" Nothing 1 2 Easy False
  transaction connection $ P.insert $ ArkhamGame label game 1 WithFriends now now False

seedEvent :: SqlBackend -> Text -> UserId -> IO EpicFixture
seedEvent connection label organizer = do
  now <- currentTimeMicros
  group0 <- seedGame connection (label <> "-group-0")
  group1 <- seedGame connection (label <> "-group-1")
  transaction connection do
    eventId <- P.insert $ ArkhamEpicEvent label organizer Nothing Nothing "Easy" (emptySharedEventState 4) 4 0 now now
    for_ [(0 :: Int, group0), (1, group1)] \(ordinal, gameId) ->
      P.insert_ $ ArkhamEpicGroup eventId ordinal (Just gameId) ("Group " <> show ordinal) 2
    pure EpicFixture {..}

-- Run two joins while another session holds the games FOR UPDATE, and
-- release the games only once both joins are queued behind that lock.
joinsBehindLock :: SqlBackend -> SqlBackend -> [ArkhamGameId] -> IO Outcome -> IO Outcome -> IO (Outcome, Outcome)
joinsBehindLock holding observer gameIds joinA joinB = do
  held <- newBarrier
  holderPid <- backendPid holding
  withAsync (transaction holding do
    for_ gameIds \gameId ->
      rawSql "SELECT 1::bigint FROM arkham_games WHERE id = ? FOR UPDATE"
        [toPersistValue gameId] :: ReaderT SqlBackend IO [Single Int64]
    liftIO $ pauseAt held) \holder -> do
      link holder
      awaitBarrier held
      withAsync joinA \first -> withAsync joinB \second -> do
        awaitBlockedSessions observer holderPid 2
        -- lock_timeout is 3s: release as soon as both joins are queued.
        releaseBarrier held
        within "lock holder commit" $ wait holder
        (,) <$> within "first join" (wait first) <*> within "second join" (wait second)

seatRows :: SqlBackend -> ArkhamGameId -> IO [UserId]
seatRows connection gameId = transaction connection do
  players <- P.selectList [ArkhamPlayerArkhamGameId P.==. gameId] []
  pure $ map (arkhamPlayerUserId . entityVal) players

gameStep :: SqlBackend -> ArkhamGameId -> IO Int
gameStep connection gameId = transaction connection do
  game <- P.getJust gameId
  pure $ arkhamGameStep game

membership :: SqlBackend -> EpicFixture -> UserId -> IO [(EpicRole, Maybe Int)]
membership connection f uid = transaction connection do
  rows <- P.selectList [ArkhamEpicMemberArkhamEpicEventId P.==. f.eventId, ArkhamEpicMemberUserId P.==. uid] []
  pure $ map ((\m -> (arkhamEpicMemberRole m, arkhamEpicMemberGroupOrdinal m)) . entityVal) rows

-- Subscribe to the event's room, as an event page would, and count the
-- EventChanged announcements it has received so far.
watchEvent :: App -> EpicFixture -> IO (IO Int)
watchEvent app f = do
  (_, _, Subscriber {subQueue}) <- unsafeHandler app $ joinEventRoom f.eventId
  seen <- newIORef (0 :: Int)
  pure do
    queued <- atomically $ flushTBQueue subQueue
    atomicModifyIORef' seen \n ->
      let n' = n + length (filter (== encode EventChanged) queued) in (n', n')

pendingJoin :: App -> UserId -> ArkhamGameId -> IO Outcome
pendingJoin app uid gameId = route app uid $ void (putApiV1ArkhamPendingGameR gameId)

-- The HCError is caught only after runDB has rolled back.
route :: App -> UserId -> Handler () -> IO Outcome
route app uid action = do
  token <- unsafeHandler app $ userIdToToken uid
  let request = defaultRequest
        { requestMethod = "PUT"
        , requestHeaders = [("Authorization", "Token " <> encodeUtf8 token)]
        }
  unsafeHandler app $
    (Succeeded <$ withRequest request action) `Exception.catch` \case
      HCError (PermissionDenied message) -> pure $ Forbidden message
      other -> Exception.throwIO other

-- Pool sessions have no PID the test can name up front, so wait until the
-- expected number of sessions are blocked, directly or behind each other, on
-- the holder's lock.
awaitBlockedSessions :: SqlBackend -> Int64 -> Int64 -> IO ()
awaitBlockedSessions observer holder expected = within "joins queued behind the game lock" loop
 where
  loop = do
    rows <- transaction observer $
      rawSql
        "SELECT count(*)::bigint FROM pg_stat_activity \
        \WHERE datname = current_database() AND pid <> ?::integer \
        \AND cardinality(pg_blocking_pids(pid)) > 0"
        [PersistInt64 holder]
    case rows of
      [Single n] | n >= expected -> pure ()
      [Single _] -> threadDelay 10000 >> loop
      _ -> fail "Expected one PostgreSQL lock observation"
