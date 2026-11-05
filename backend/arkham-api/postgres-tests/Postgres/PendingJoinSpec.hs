{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

-- The actual JWT-authenticated pending-game join against the disposable
-- database: the one-group-per-event Epic membership is checked and reserved
-- under the game lock, concurrent joins never answer 500, and the event roster
-- is announced only when a join seats the player.
module Postgres.PendingJoinSpec (spec) where

import Api.Arkham.Helpers (ApiResponse (EventChanged), joinEventRoom)
import Api.Arkham.Types.MultiplayerVariant (MultiplayerVariant (WithFriends))
import Api.Handler.Arkham.Events (deleteApiV1ArkhamEventR)
import Api.Handler.Arkham.PendingGames (putApiV1ArkhamPendingGameR)
import Arkham.Campaign (lookupCampaign)
import Arkham.Campaign.Types (campaignStep)
import Arkham.CampaignStep (CampaignStep (ContinueCampaignStep, ScenarioStep), Continuation (..))
import Arkham.Classes.Entity (overAttrs)
import Arkham.Difficulty (Difficulty (Easy))
import Arkham.Epic.Types (EpicRole (GroupPlayer, Organizer), SharedKey (Countermeasures), emptySharedEventState, sharedCounter)
import Arkham.Game qualified as Game
import Arkham.Game.State (GameState (IsActive))
import Arkham.Message (Message (RaiseShared))
import Data.Aeson (encode)
import Data.Aeson.Diff (Patch (..))
import Arkham.Prelude (These (This))
import Database.Persist (Entity (..), toPersistValue)
import Database.Persist qualified as P
import Database.Persist.Sql (Single (..), SqlBackend, rawSql)
import Entity.Arkham.Epic
import Entity.Arkham.Game
import Entity.Arkham.Player
import Entity.Arkham.Step
import Entity.User (UserId)
import Foundation (App, Handler, Subscriber (..), unsafeHandler, userIdToToken)
import Network.HTTP.Types (Method)
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
-- as 403, and Missing the one it renders as 404; anything else, a 500
-- included, fails the test.
data Outcome = Succeeded | Forbidden Text | Missing
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

  it "reserves the membership on a campaign-continuation join and refuses another group afterwards" $
    withConnection harness \connection -> do
      organizer <- seedUser connection "join-continuation-organizer"
      player <- seedUser connection "join-continuation-player"
      continuing <- seedContinuationGame connection "join-continuation-group-0"
      lobby <- seedGame connection "join-continuation-group-1"
      f <- seedEventWith connection "join-continuation" organizer continuing lobby
      announcements <- watchEvent app f
      -- The game sits between scenarios, so the join takes the continuation
      -- path: the seat is added to the live game.
      pendingJoin app player f.group0 `shouldReturn` Succeeded
      seatRows connection f.group0 `shouldReturn` [player]
      gameStep connection f.group0 `shouldReturn` 2
      membership connection f player `shouldReturn` [(GroupPlayer, Just 0)]
      announcements `shouldReturn` 1
      -- That membership now keeps the player out of the event's other group.
      pendingJoin app player f.group1 `shouldReturn` Forbidden anotherGroup
      seatRows connection f.group1 `shouldReturn` []
      gameStep connection f.group1 `shouldReturn` 1
      membership connection f player `shouldReturn` [(GroupPlayer, Just 0)]
      announcements `shouldReturn` 1

  it "commits two concurrent first joins to different groups whose setup changes the shared state" $
    withConnection harness \holding -> withConnection harness \observer -> do
      organizer <- seedUser observer "join-setup-organizer"
      player0 <- seedUser observer "join-setup-player-0"
      player1 <- seedUser observer "join-setup-player-1"
      game0 <- seedSetupGame observer "join-setup-group-0"
      game1 <- seedSetupGame observer "join-setup-group-1"
      f <- seedEventWith observer "join-setup" organizer game0 game1
      initial <- countermeasures observer f
      -- Share the event row, as a member insert's KEY SHARE does, so both
      -- joins have started when they go on. Before the event was locked ahead
      -- of the member insert, each join held KEY SHARE and then deadlocked
      -- waiting for FOR UPDATE to commit its setup's shared delta.
      -- Once released, the second join waits on the event lock for the whole
      -- of the first join's setup, so this relies on the launcher's
      -- lock_timeout (3s) outlasting that setup. Here it is tiny: setup
      -- pauses at ChooseDeck.
      outcomes <-
        joinsBehind holding observer
          (eventRowLock "FOR SHARE" f)
          (pendingJoin app player0 game0)
          (pendingJoin app player1 game1)
      outcomes `shouldBe` (Succeeded, Succeeded)
      seatRows observer game0 `shouldReturn` [player0]
      seatRows observer game1 `shouldReturn` [player1]
      membership observer f player0 `shouldReturn` [(GroupPlayer, Just 0)]
      membership observer f player1 `shouldReturn` [(GroupPlayer, Just 1)]
      countermeasures observer f `shouldReturn` initial + 2
      epicStepCount observer f `shouldReturn` 2
      eventStep observer f `shouldReturn` 2
      gameStep observer game0 `shouldReturn` 2
      gameStep observer game1 `shouldReturn` 2

  it "answers 404, not 500, when a delete outside the canonical lock order removes the event under a waiting join" $
    withConnection harness \holding -> withConnection harness \observer -> do
      organizer <- seedUser observer "join-gone-organizer"
      player <- seedUser observer "join-gone-player"
      f <- seedEvent observer "join-gone" organizer
      joinAfterRawEventDelete holding observer f (pendingJoin app player f.group0)
        `shouldReturn` Missing
      seatRows observer f.group0 `shouldReturn` []
      gameStep observer f.group0 `shouldReturn` 1

  it "answers 404, not 500, when a delete outside the canonical lock order removes the event under a waiting continuation join" $
    withConnection harness \holding -> withConnection harness \observer -> do
      organizer <- seedUser observer "join-gone-continuation-organizer"
      player <- seedUser observer "join-gone-continuation-player"
      continuing <- seedContinuationGame observer "join-gone-continuation-group-0"
      lobby <- seedGame observer "join-gone-continuation-group-1"
      f <- seedEventWith observer "join-gone-continuation" organizer continuing lobby
      original <- transaction observer $ P.getJust continuing
      joinAfterRawEventDelete holding observer f (pendingJoin app player continuing)
        `shouldReturn` Missing
      seatRows observer continuing `shouldReturn` []
      afterwards <- transaction observer $ P.getJust continuing
      arkhamGameStep afterwards `shouldBe` arkhamGameStep original
      encode (arkhamGameCurrentData afterwards) `shouldBe` encode (arkhamGameCurrentData original)

  it "clears stale later step rows before a pending join inserts its step" $
    withConnection harness \connection -> do
      player <- seedUser connection "join-stale-pending-player"
      gameId <- seedGame connection "join-stale-pending"
      seedStaleSteps connection gameId
      pendingJoin app player gameId `shouldReturn` Succeeded
      seatRows connection gameId `shouldReturn` [player]
      gameStep connection gameId `shouldReturn` 2
      -- Only the join's own step is left: the stale step 2 was replaced and
      -- the stale step 3 is gone.
      stepRows connection gameId `shouldReturn` [(2, False)]

  it "clears stale later step rows before a continuation join inserts its step" $
    withConnection harness \connection -> do
      player <- seedUser connection "join-stale-continuation-player"
      gameId <- seedContinuationGame connection "join-stale-continuation"
      seedStaleSteps connection gameId
      pendingJoin app player gameId `shouldReturn` Succeeded
      seatRows connection gameId `shouldReturn` [player]
      gameStep connection gameId `shouldReturn` 2
      stepRows connection gameId `shouldReturn` [(2, False)]

  it "answers 404, not 500, to a join queued behind an event delete on the lowest-id group game" $
    withConnection harness \holding -> withConnection harness \observer -> do
      (f, organizer, player) <- seedDeleteRace seedUser observer "join-delete-first"
      let target = min f.group0 f.group1
      outcomes <-
        inOrderBehindGameLock holding observer target
          (deleteEvent app organizer f.eventId)
          (pendingJoin app player target)
      outcomes `shouldBe` (Succeeded, Missing)
      remaining observer f player `shouldReturn` (0, 0, 0, 0)

  it "lets an event delete queued behind a join on the lowest-id group game remove the new seat with the event" $
    withConnection harness \holding -> withConnection harness \observer -> do
      (f, organizer, player) <- seedDeleteRace seedUser observer "join-delete-second"
      let target = min f.group0 f.group1
      announcements <- watchEvent app f
      outcomes <-
        inOrderBehindGameLock holding observer target
          (pendingJoin app player target)
          (deleteEvent app organizer f.eventId)
      outcomes `shouldBe` (Succeeded, Succeeded)
      -- The join committed its seat and membership first.
      announcements `shouldReturn` 1
      remaining observer f player `shouldReturn` (0, 0, 0, 0)

-- The event delete handler locks the group games first
-- ('canonicalEpicGameLockOrder'), so a join never sees its event vanish. This
-- holder deletes the event row directly instead, bypassing that order, to
-- model a raw-SQL or future delete path: the join, already past its event
-- lookup, waits on the event lock and finds the row gone once the delete
-- commits.
joinAfterRawEventDelete :: SqlBackend -> SqlBackend -> EpicFixture -> IO Outcome -> IO Outcome
joinAfterRawEventDelete holding observer f = behindHeldLock holding observer (P.delete f.eventId)

-- A WithFriends campaign lobby with two seats, so a first join leaves it
-- pending and runs no setup.
seedGame :: SqlBackend -> Text -> IO ArkhamGameId
seedGame connection label = do
  now <- currentTimeMicros
  let game = Game.newCampaign "01" Nothing 1 2 Easy False
  transaction connection $ P.insert $ ArkhamGame label game 1 WithFriends now now False

-- A live campaign sitting on its between-scenarios continuation screen, the
-- only active state a pending join seats a player in.
seedContinuationGame :: SqlBackend -> Text -> IO ArkhamGameId
seedContinuationGame connection label = do
  now <- currentTimeMicros
  let continuation = ContinueCampaignStep $ Continuation (ScenarioStep "01104") True False Nothing True
      game = (Game.newCampaign "01" Nothing 1 1 Easy False)
        { Game.gameGameState = IsActive
        , Game.gameMode = This $ overAttrs (\attrs -> attrs {campaignStep = continuation}) $ lookupCampaign "01" Easy
        }
  transaction connection $ P.insert $ ArkhamGame label game 1 WithFriends now now False

-- A one-seat WithFriends lobby whose persisted queue raises the shared
-- countermeasures, so its first join runs setup that commits a shared delta.
seedSetupGame :: SqlBackend -> Text -> IO ArkhamGameId
seedSetupGame connection label = do
  now <- currentTimeMicros
  let game = Game.newCampaign "01" Nothing 1 1 Easy False
  transaction connection do
    gameId <- P.insert $ ArkhamGame label game 1 WithFriends now now False
    P.insert_ $ ArkhamStep gameId (Choice (Patch []) [RaiseShared Countermeasures 1]) 1 (ActionDiff [])
    pure gameId

-- Step rows above the game's step (1), as only old data, a debug import or an
-- edit made outside the app leave: the join's own step 2 collides with one.
seedStaleSteps :: SqlBackend -> ArkhamGameId -> IO ()
seedStaleSteps connection gameId = transaction connection $
  for_ [2, 3] \step -> P.insert_ $ ArkhamStep gameId (Choice (Patch []) staleQueue) step (ActionDiff [])

staleQueue :: [Message]
staleQueue = [RaiseShared Countermeasures 7]

-- Each step row of the game, in order, and whether it still carries the
-- stale queue.
stepRows :: SqlBackend -> ArkhamGameId -> IO [(Int, Bool)]
stepRows connection gameId = transaction connection do
  steps <- P.selectList [ArkhamStepArkhamGameId P.==. gameId] [P.Asc ArkhamStepStep]
  pure $ map ((\s -> (arkhamStepStep s, encode (choiceMessages $ arkhamStepChoice s) == encode staleQueue)) . entityVal) steps

-- An event the organizer may delete, and a player with no membership yet.
seedDeleteRace :: SeedUser -> SqlBackend -> Text -> IO (EpicFixture, UserId, UserId)
seedDeleteRace seedUser connection label = do
  organizer <- seedUser connection (label <> "-organizer")
  player <- seedUser connection (label <> "-player")
  f <- seedEvent connection label organizer
  transaction connection $ P.insert_ $ ArkhamEpicMember f.eventId organizer Organizer Nothing
  pure (f, organizer, player)

seedEvent :: SqlBackend -> Text -> UserId -> IO EpicFixture
seedEvent connection label organizer = do
  group0 <- seedGame connection (label <> "-group-0")
  group1 <- seedGame connection (label <> "-group-1")
  seedEventWith connection label organizer group0 group1

seedEventWith :: SqlBackend -> Text -> UserId -> ArkhamGameId -> ArkhamGameId -> IO EpicFixture
seedEventWith connection label organizer group0 group1 = do
  now <- currentTimeMicros
  transaction connection do
    eventId <- P.insert $ ArkhamEpicEvent label organizer Nothing Nothing "Easy" (emptySharedEventState 4) 4 0 now now
    for_ [(0 :: Int, group0), (1, group1)] \(ordinal, gameId) ->
      P.insert_ $ ArkhamEpicGroup eventId ordinal (Just gameId) ("Group " <> show ordinal) 2
    pure EpicFixture {..}

-- Run two joins while another session holds the games FOR UPDATE, and
-- release the games only once both joins are queued behind that lock.
joinsBehindLock :: SqlBackend -> SqlBackend -> [ArkhamGameId] -> IO Outcome -> IO Outcome -> IO (Outcome, Outcome)
joinsBehindLock holding observer gameIds =
  joinsBehind holding observer $ for_ gameIds \gameId ->
    void (rawSql "SELECT 1::bigint FROM arkham_games WHERE id = ? FOR UPDATE"
      [toPersistValue gameId] :: ReaderT SqlBackend IO [Single Int64])

-- Run two joins while another session holds the locks the given statements
-- take, and release them only once both joins are queued behind them.
joinsBehind :: SqlBackend -> SqlBackend -> ReaderT SqlBackend IO () -> IO Outcome -> IO Outcome -> IO (Outcome, Outcome)
joinsBehind holding observer hold joinA joinB = do
  held <- newBarrier
  holderPid <- backendPid holding
  withAsync (transaction holding do
    hold
    liftIO $ pauseAt held) \holder -> do
      link holder
      awaitBarrier held
      withAsync joinA \first -> withAsync joinB \second -> do
        awaitBlockedSessions observer holderPid 2
        -- lock_timeout is 3s: release as soon as both joins are queued.
        releaseBarrier held
        within "lock holder commit" $ wait holder
        (,) <$> within "first join" (wait first) <*> within "second join" (wait second)

-- Hold the game row, queue @first@ and then @second@ behind it, in that order,
-- and release it once both wait. PostgreSQL grants a row to its waiters in
-- queue order, so @first@ takes the game and commits before @second@ gets it.
inOrderBehindGameLock :: SqlBackend -> SqlBackend -> ArkhamGameId -> IO a -> IO b -> IO (a, b)
inOrderBehindGameLock holding observer gameId first second = do
  held <- newBarrier
  holderPid <- backendPid holding
  withAsync (transaction holding do
    void (rawSql "SELECT 1::bigint FROM arkham_games WHERE id = ? FOR UPDATE"
      [toPersistValue gameId] :: ReaderT SqlBackend IO [Single Int64])
    liftIO $ pauseAt held) \holder -> do
      link holder
      awaitBarrier held
      withAsync first \a -> do
        awaitBlockedSessions observer holderPid 1
        withAsync second \b -> do
          awaitBlockedSessions observer holderPid 2
          -- lock_timeout is 3s: release as soon as both are queued.
          releaseBarrier held
          within "lock holder commit" $ wait holder
          (,) <$> within "first queued" (wait a) <*> within "second queued" (wait b)

-- What is left of the event: its row, its group games, and the player's seats
-- and memberships.
remaining :: SqlBackend -> EpicFixture -> UserId -> IO (Int, Int, Int, Int)
remaining connection f uid = transaction connection do
  events <- P.count [ArkhamEpicEventId P.==. f.eventId]
  games <- P.count [ArkhamGameId P.<-. [f.group0, f.group1]]
  seats <- P.count [ArkhamPlayerUserId P.==. uid]
  members <- P.count [ArkhamEpicMemberUserId P.==. uid]
  pure (events, games, seats, members)

eventRowLock :: Text -> EpicFixture -> ReaderT SqlBackend IO ()
eventRowLock mode f =
  void (rawSql ("SELECT 1::bigint FROM arkham_epic_events WHERE id = ? " <> mode)
    [toPersistValue f.eventId] :: ReaderT SqlBackend IO [Single Int64])

countermeasures :: SqlBackend -> EpicFixture -> IO Int
countermeasures connection f = transaction connection do
  event <- P.getJust f.eventId
  pure $ sharedCounter Countermeasures (arkhamEpicEventSharedState event)

eventStep :: SqlBackend -> EpicFixture -> IO Int
eventStep connection f = transaction connection do
  event <- P.getJust f.eventId
  pure $ arkhamEpicEventStep event

epicStepCount :: SqlBackend -> EpicFixture -> IO Int
epicStepCount connection f = transaction connection $
  P.count [ArkhamEpicStepArkhamEpicEventId P.==. f.eventId]

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
pendingJoin app uid gameId = route app uid "PUT" $ void (putApiV1ArkhamPendingGameR gameId)

deleteEvent :: App -> UserId -> ArkhamEpicEventId -> IO Outcome
deleteEvent app uid eid = route app uid "DELETE" (deleteApiV1ArkhamEventR eid)

-- The HCError is caught only after runDB has rolled back.
route :: App -> UserId -> Method -> Handler () -> IO Outcome
route app uid method action = do
  token <- unsafeHandler app $ userIdToToken uid
  let request = defaultRequest
        { requestMethod = method
        , requestHeaders = [("Authorization", "Token " <> encodeUtf8 token)]
        }
  unsafeHandler app $
    (Succeeded <$ withRequest request action) `Exception.catch` \case
      HCError (PermissionDenied message) -> pure $ Forbidden message
      HCError NotFound -> pure Missing
      other -> Exception.throwIO other
