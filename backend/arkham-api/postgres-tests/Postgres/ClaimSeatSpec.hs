{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE NoFieldSelectors #-}
{-# LANGUAGE OverloadedStrings #-}

-- The actual JWT-authenticated claim-seat handler against the disposable
-- database: the game lock that serializes competing claims, the seat check
-- for both stored seat formats, and the one-group-per-event Epic membership.
-- Open seats is checked here too, since it must agree with claim seat on
-- which seats are taken.
module Postgres.ClaimSeatSpec (spec) where

import Api.Arkham.Types.MultiplayerVariant (MultiplayerVariant (WithFriends))
import Api.Handler.Arkham.Game.Debug (getApiV1ArkhamGameOpenSeatsR, postApiV1ArkhamGameClaimSeatR)
import Api.Handler.Arkham.PendingGames (putApiV1ArkhamPendingGameR)
import Arkham.Difficulty (Difficulty (Easy))
import Arkham.Entities (entitiesInvestigators)
import Arkham.Epic.Types (EpicRole (GroupPlayer), emptySharedEventState)
import Arkham.Game qualified as Game
import Arkham.Id (PlayerId (..))
import Arkham.Investigator (lookupInvestigator)
import Data.Aeson (encode, object, (.=))
import Data.Map.Strict qualified as Map
import Data.UUID qualified as UUID
import Database.Persist (Entity (..), toPersistValue)
import Database.Persist qualified as P
import Database.Persist.Sql (Single (..), SqlBackend, rawSql)
import Database.Persist.Types (PersistValue (..))
import Entity.Arkham.Epic
import Entity.Arkham.Game
import Entity.Arkham.Player
import Entity.User (UserId)
import Foundation (App, Handler, unsafeHandler, userIdToToken)
import Network.HTTP.Types (Method)
import Network.Wai (defaultRequest, requestHeaders, requestMethod, setRequestBodyChunks)
import Postgres.Harness
import Postgres.Production (withRequest)
import Relude hiding (first, second)
import Test.Hspec
import UnliftIO.Async (link, wait, withAsync)
import UnliftIO.Exception qualified as Exception
import Yesod.Core.Types (ErrorResponse (..), HandlerContents (..))

type SeedUser = SqlBackend -> Text -> IO UserId

-- What the route answered. Permission, argument and not-found failures are
-- the HCError responses Yesod renders as 403, 400 and 404; anything else
-- fails the test.
data Outcome = Succeeded | Forbidden Text | BadRequest [Text] | Missing
  deriving stock (Eq, Ord, Show)

data EpicFixture = EpicFixture
  { eventId :: ArkhamEpicEventId
  , group0 :: ArkhamGameId
  , group1 :: ArkhamGameId
  }

seatTaken, anotherGroup :: Text
seatTaken = "This seat is already taken"
anotherGroup = "You already occupy a seat in another group in this event"

spec :: Harness -> App -> SeedUser -> Spec
spec harness app seedUser = do
  claimSeatSpec harness app seedUser
  openSeatsSpec harness app seedUser

claimSeatSpec :: Harness -> App -> SeedUser -> Spec
claimSeatSpec harness app seedUser = describe "Claim seat" do
  it "serializes two claims for one seat behind the game lock" $
    withConnection harness \holding -> withConnection harness \observer -> do
      gameId <- seedGame observer "claim-race"
      alice <- seedUser observer "claim-race-alice"
      bob <- seedUser observer "claim-race-bob"
      held <- newBarrier
      holderPid <- backendPid holding
      -- Hold the game row the way a concurrent answer/claim would, so both
      -- claims are already waiting when the row is released.
      withAsync (transaction holding do
        _ <- rawSql "SELECT 1::bigint FROM arkham_games WHERE id = ? FOR UPDATE"
          [toPersistValue gameId] :: ReaderT SqlBackend IO [Single Int64]
        liftIO $ pauseAt held) \holder -> do
          link holder
          awaitBarrier held
          withAsync (claim app alice gameId "01001") \first ->
            withAsync (claim app bob gameId "01001") \second -> do
              awaitBlockedSessions observer holderPid 2
              -- lock_timeout is 3s: release as soon as both are queued.
              releaseBarrier held
              within "lock holder commit" $ wait holder
              aliceOutcome <- within "first claim" $ wait first
              bobOutcome <- within "second claim" $ wait second
              sort [aliceOutcome, bobOutcome] `shouldBe` [Succeeded, Forbidden seatTaken]
              let winner = if aliceOutcome == Succeeded then alice else bob
              players <- transaction observer $ P.selectList [ArkhamPlayerArkhamGameId P.==. gameId] []
              map ((\p -> (arkhamPlayerUserId p, arkhamPlayerInvestigatorId p)) . entityVal) players
                `shouldBe` [(winner, "c01001")]
              -- The winner's remap ran under the same lock.
              winnerKey <- case players of
                [Entity key _] -> pure key
                _ -> fail "Expected exactly one claimed seat"
              seatPlayerId observer gameId "c01001"
                `shouldReturn` Just (UUID.toText $ unArkhamPlayerKey winnerKey)

  it "counts a deck-selected bare-coded seat as taken" $
    withConnection harness \connection -> do
      gameId <- seedGame connection "claim-bare-seat"
      chooser <- seedUser connection "claim-bare-chooser"
      claimer <- seedUser connection "claim-bare-claimer"
      -- Deck selection stores the bare ArkhamDB code.
      transaction connection $ P.insert_ $ ArkhamPlayer chooser gameId "01001"
      claim app claimer gameId "01001" `shouldReturn` Forbidden seatTaken
      claim app claimer gameId "c01001" `shouldReturn` Forbidden seatTaken
      seatRows connection gameId `shouldReturn` [(chooser, "01001")]
      -- The other seat is still open, and invalid seats are still 400.
      claim app claimer gameId "01003" `shouldReturn` BadRequest ["Invalid investigator for this game"]
      claim app claimer gameId "01002" `shouldReturn` Succeeded
      sort <$> seatRows connection gameId `shouldReturn` sort [(chooser, "01001"), (claimer, "c01002")]

  it "rejects a code with more than one leading c as an invalid seat" $
    withConnection harness \connection -> do
      gameId <- seedGame connection "claim-double-c"
      claimer <- seedUser connection "claim-double-c-claimer"
      claim app claimer gameId "cc01001" `shouldReturn` BadRequest ["Invalid investigator for this game"]
      seatRows connection gameId `shouldReturn` []
      claim app claimer gameId "c01001" `shouldReturn` Succeeded
      seatRows connection gameId `shouldReturn` [(claimer, "c01001")]

  it "reserves one Epic group per event and refuses a second group" $
    withConnection harness \connection -> do
      organizer <- seedUser connection "claim-epic-organizer"
      player <- seedUser connection "claim-epic-player"
      f <- seedEvent connection "claim-epic" organizer
      claim app player f.group0 "01001" `shouldReturn` Succeeded
      membership connection f player `shouldReturn` [(GroupPlayer, Just 0)]
      seatRows connection f.group0 `shouldReturn` [(player, "c01001")]
      claim app player f.group1 "01001" `shouldReturn` Forbidden anotherGroup
      seatRows connection f.group1 `shouldReturn` []
      membership connection f player `shouldReturn` [(GroupPlayer, Just 0)]
      -- A pending join to the other group is refused with the same 403.
      pendingJoin app player f.group1 `shouldReturn` Forbidden anotherGroup
      seatRows connection f.group1 `shouldReturn` []
      membership connection f player `shouldReturn` [(GroupPlayer, Just 0)]

  it "claims a seat in the group the player already belongs to" $
    withConnection harness \connection -> do
      organizer <- seedUser connection "claim-member-organizer"
      player <- seedUser connection "claim-member-player"
      f <- seedEvent connection "claim-member" organizer
      transaction connection $ P.insert_ $ ArkhamEpicMember f.eventId player GroupPlayer (Just 1)
      claim app player f.group1 "01002" `shouldReturn` Succeeded
      seatRows connection f.group1 `shouldReturn` [(player, "c01002")]
      membership connection f player `shouldReturn` [(GroupPlayer, Just 1)]

  it "refuses a claim when the membership has no group, as pending join does" $
    withConnection harness \connection -> do
      organizer <- seedUser connection "claim-groupless-organizer"
      player <- seedUser connection "claim-groupless-player"
      f <- seedEvent connection "claim-groupless" organizer
      transaction connection $ P.insert_ $ ArkhamEpicMember f.eventId player GroupPlayer Nothing
      claim app player f.group0 "01001" `shouldReturn` Forbidden anotherGroup
      pendingJoin app player f.group0 `shouldReturn` Forbidden anotherGroup
      seatRows connection f.group0 `shouldReturn` []
      membership connection f player `shouldReturn` [(GroupPlayer, Nothing)]

  it "serializes a pending join and a claim for one player's first membership in different groups on the event lock" $
    withConnection harness \holding -> withConnection harness \observer -> do
      organizer <- seedUser observer "claim-join-organizer"
      player <- seedUser observer "claim-join-player"
      f <- seedEvent observer "claim-join" organizer
      held <- newBarrier
      holderPid <- backendPid holding
      -- Hold the event row FOR UPDATE, queue the join's event lock behind it,
      -- then queue the claim behind both. Before claim seat locked the event,
      -- the claim inserted its membership row and waited on that insert's
      -- foreign-key KEY SHARE behind the join's FOR UPDATE, while the join's
      -- own membership insert waited on the claim's uncommitted row: 40P01.
      withAsync (transaction holding do
        _ <- rawSql "SELECT 1::bigint FROM arkham_epic_events WHERE id = ? FOR UPDATE"
          [toPersistValue f.eventId] :: ReaderT SqlBackend IO [Single Int64]
        liftIO $ pauseAt held) \holder -> do
          link holder
          awaitBarrier held
          withAsync (pendingJoin app player f.group0) \joining -> do
            awaitBlockedSessions observer holderPid 1
            withAsync (claim app player f.group1 "01001") \claiming -> do
              awaitBlockedSessions observer holderPid 2
              -- lock_timeout is 3s: release as soon as both are queued.
              releaseBarrier held
              within "lock holder commit" $ wait holder
              within "join" (wait joining) `shouldReturn` Succeeded
              within "claim" (wait claiming) `shouldReturn` Forbidden anotherGroup
      membership observer f player `shouldReturn` [(GroupPlayer, Just 0)]
      map fst <$> seatRows observer f.group0 `shouldReturn` [player]
      seatRows observer f.group1 `shouldReturn` []

  it "answers 404, not 500, when a delete outside the canonical lock order removes the event under a waiting claim" $
    withConnection harness \holding -> withConnection harness \observer -> do
      organizer <- seedUser observer "claim-gone-organizer"
      player <- seedUser observer "claim-gone-player"
      f <- seedEvent observer "claim-gone" organizer
      -- The event delete handler locks the group games first, so a claim
      -- never sees its event vanish. Deleting the row directly bypasses that
      -- order: the claim, already past its event lookup, waits on the event
      -- lock and finds the row gone once the delete commits.
      behindHeldLock holding observer (P.delete f.eventId) (claim app player f.group0 "01001")
        `shouldReturn` Missing
      seatRows observer f.group0 `shouldReturn` []
      membership observer f player `shouldReturn` []
      -- The seat's placeholder player id was not remapped either.
      seatPlayerId observer f.group0 "c01001"
        `shouldReturn` Just (UUID.toText $ UUID.fromWords 0x2580 0 0 1)

openSeatsSpec :: Harness -> App -> SeedUser -> Spec
openSeatsSpec harness app seedUser = describe "Open seats" do
  it "omits a deck-selected bare-coded seat" $
    withConnection harness \connection -> do
      gameId <- seedGame connection "open-bare-seat"
      chooser <- seedUser connection "open-bare-chooser"
      claimer <- seedUser connection "open-bare-claimer"
      openSeats app claimer gameId `shouldReturn` ["c01001", "c01002"]
      -- Deck selection stores the bare ArkhamDB code.
      transaction connection $ P.insert_ $ ArkhamPlayer chooser gameId "01001"
      openSeats app claimer gameId `shouldReturn` ["c01002"]
      claim app claimer gameId "01002" `shouldReturn` Succeeded
      openSeats app claimer gameId `shouldReturn` []

  it "omits a c-prefixed claimed seat" $
    withConnection harness \connection -> do
      gameId <- seedGame connection "open-prefixed-seat"
      claimer <- seedUser connection "open-prefixed-claimer"
      viewer <- seedUser connection "open-prefixed-viewer"
      claim app claimer gameId "c01002" `shouldReturn` Succeeded
      seatRows connection gameId `shouldReturn` [(claimer, "c01002")]
      openSeats app viewer gameId `shouldReturn` ["c01001"]

  it "does not let a pending-join placeholder hide a seat" $
    withConnection harness \connection -> do
      gameId <- seedGame connection "open-placeholder"
      joiner <- seedUser connection "open-placeholder-joiner"
      -- Pending join stores this placeholder until the player picks a deck.
      transaction connection $ P.insert_ $ ArkhamPlayer joiner gameId "00000"
      openSeats app joiner gameId `shouldReturn` ["c01001", "c01002"]

-- A WithFriends game with two seats, each investigator carrying a placeholder
-- player id that claim seat swaps for the new player's.
seedGame :: SqlBackend -> Text -> IO ArkhamGameId
seedGame connection label = do
  now <- currentTimeMicros
  let initial = Game.newCampaign "01" Nothing 1 2 Easy False
      investigator code n = lookupInvestigator code (PlayerId $ UUID.fromWords 0x2580 0 0 n)
      game = initial
        { Game.gamePlayerOrder = ["01001", "01002"]
        , Game.gameEntities = initial.gameEntities
            { entitiesInvestigators = Map.fromList
                [("01001", investigator "01001" 1), ("01002", investigator "01002" 2)]
            }
        }
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

seatRows :: SqlBackend -> ArkhamGameId -> IO [(UserId, Text)]
seatRows connection gameId = transaction connection do
  players <- P.selectList [ArkhamPlayerArkhamGameId P.==. gameId] []
  pure $ map ((\p -> (arkhamPlayerUserId p, arkhamPlayerInvestigatorId p)) . entityVal) players

membership :: SqlBackend -> EpicFixture -> UserId -> IO [(EpicRole, Maybe Int)]
membership connection f uid = transaction connection do
  rows <- P.selectList [ArkhamEpicMemberArkhamEpicEventId P.==. f.eventId, ArkhamEpicMemberUserId P.==. uid] []
  pure $ map ((\m -> (arkhamEpicMemberRole m, arkhamEpicMemberGroupOrdinal m)) . entityVal) rows

seatPlayerId :: SqlBackend -> ArkhamGameId -> Text -> IO (Maybe Text)
seatPlayerId connection gameId seat = transaction connection do
  rows <- rawSql
    "SELECT current_data->'gameEntities'->'investigators'->?->>'playerId' FROM arkham_games WHERE id = ?"
    [PersistText seat, toPersistValue gameId]
  pure $ case rows of
    [Single playerId] -> playerId
    _ -> Nothing

claim :: App -> UserId -> ArkhamGameId -> Text -> IO Outcome
claim app uid gameId seat =
  route app uid "POST" (Just $ toStrict $ encode $ object ["investigatorId" .= seat])
    $ void (postApiV1ArkhamGameClaimSeatR gameId)

openSeats :: App -> UserId -> ArkhamGameId -> IO [Text]
openSeats app uid gameId = unsafeHandler app $ asUser uid "GET" Nothing (getApiV1ArkhamGameOpenSeatsR gameId)

pendingJoin :: App -> UserId -> ArkhamGameId -> IO Outcome
pendingJoin app uid gameId = route app uid "PUT" Nothing $ void (putApiV1ArkhamPendingGameR gameId)

-- The HCError is caught only after runDB has rolled back.
route :: App -> UserId -> Method -> Maybe ByteString -> Handler () -> IO Outcome
route app uid method body action =
  unsafeHandler app $
    (Succeeded <$ asUser uid method body action) `Exception.catch` \case
      HCError (PermissionDenied message) -> pure $ Forbidden message
      HCError (InvalidArgs messages) -> pure $ BadRequest messages
      HCError NotFound -> pure Missing
      other -> Exception.throwIO other

-- Run the handler as the user's JWT-authenticated request. Each call builds a
-- fresh request whose body yields the bytes once, as WAI does.
asUser :: UserId -> Method -> Maybe ByteString -> Handler a -> Handler a
asUser uid method body action = do
  token <- userIdToToken uid
  chunks <- newIORef $ maybeToList body
  let nextChunk = atomicModifyIORef' chunks \case
        chunk : rest -> (rest, chunk)
        [] -> ([], "")
      request = setRequestBodyChunks nextChunk defaultRequest
        { requestMethod = method
        , requestHeaders = ("Authorization", "Token " <> encodeUtf8 token)
            : [("Content-Type", "application/json") | isJust body]
        }
  withRequest request action
