{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE NoFieldSelectors #-}
{-# LANGUAGE OverloadedStrings #-}

-- The actual JWT-authenticated event delete handler against the disposable
-- database: 404 for a missing event, 403 without mutation for a
-- non-organizer, concurrent deletes, and the canonical lock order against a
-- gameplay transaction that holds a group game's lock.
module Postgres.EventDeleteSpec (spec) where

import Api.Arkham.Epic (modifySharedStateLocked)
import Api.Arkham.Types.MultiplayerVariant (MultiplayerVariant (WithFriends))
import Api.Handler.Arkham.Events (deleteApiV1ArkhamEventR)
import Arkham.Difficulty (Difficulty (Easy))
import Arkham.Epic.Types (EpicRole (..), SharedEventState, emptySharedEventState)
import Arkham.Game qualified as Game
import Data.Time.Clock (UTCTime)
import Data.UUID qualified as UUID
import Database.Persist (toPersistValue)
import Database.Persist qualified as P
import Database.Persist.Sql (Single (..), SqlBackend, rawSql)
import Entity.Arkham.Epic
import Entity.Arkham.Game
import Entity.User (UserId)
import Foundation (App, Handler, unsafeHandler, userIdToToken)
import Network.Wai (defaultRequest, requestHeaders, requestMethod)
import Postgres.Harness
import Postgres.Production (withRequest)
import Relude hiding (first, second)
import Test.Hspec
import UnliftIO.Async (link, wait, withAsync)
import UnliftIO.Exception qualified as Exception
import Yesod.Core.Types (ErrorResponse (..), HandlerContents (..))

type SeedUser = SqlBackend -> Text -> IO UserId

-- Not found and permission failures are the HCError responses Yesod renders
-- as 404 and 403; anything else (a 500) fails the test.
data Outcome = Succeeded | Denied | Missing
  deriving stock (Eq, Ord, Show)

data EpicFixture = EpicFixture
  { eventId :: ArkhamEpicEventId
  , organizer :: UserId
  , player :: UserId
  , group0 :: ArkhamGameId
  , group1 :: ArkhamGameId
  }

-- Every row the aggregate owns, so a test can show nothing changed (or all of
-- it went).
data Aggregate = Aggregate
  { events :: Int
  , members :: Int
  , groups :: Int
  , games :: Int
  }
  deriving stock (Eq, Show)

spec :: Harness -> App -> SeedUser -> Spec
spec harness app seedUser = describe "Epic event delete" do
  it "deletes the aggregate, then answers 404 to a repeat delete" $
    withConnection harness \connection -> do
      f <- seedEvent connection seedUser "delete-repeat"
      aggregate connection f `shouldReturn` Aggregate 1 2 2 2
      deleteEvent app f.organizer f.eventId `shouldReturn` Succeeded
      aggregate connection f `shouldReturn` Aggregate 0 0 0 0
      deleteEvent app f.organizer f.eventId `shouldReturn` Missing
      -- A missing event is 404 for anyone, without disclosing the organizer.
      deleteEvent app f.player f.eventId `shouldReturn` Missing

  it "answers 404 for an event id that never existed" $
    withConnection harness \connection -> do
      stranger <- seedUser connection "delete-never-stranger"
      deleteEvent app stranger (ArkhamEpicEventKey $ UUID.fromWords 0x2600 0 0 1) `shouldReturn` Missing

  it "refuses a non-organizer of an existing event with 403 and no mutation" $
    withConnection harness \connection -> do
      f <- seedEvent connection seedUser "delete-forbidden"
      stranger <- seedUser connection "delete-forbidden-stranger"
      original <- snapshot connection f
      deleteEvent app f.player f.eventId `shouldReturn` Denied
      deleteEvent app stranger f.eventId `shouldReturn` Denied
      snapshot connection f `shouldReturn` original
      aggregate connection f `shouldReturn` Aggregate 1 2 2 2

  it "lets exactly one of two concurrent deletes win; the other is 404" $
    withConnection harness \holding -> withConnection harness \observer -> do
      f <- seedEvent observer seedUser "delete-race"
      held <- newBarrier
      holderPid <- backendPid holding
      -- Hold a group game the way gameplay does, so both deletes have passed
      -- their probes and organizer checks and wait in the canonical order.
      withAsync (transaction holding do
        lockGameRow f.group0
        liftIO $ pauseAt held) \holder -> do
          link holder
          awaitBarrier held
          withAsync (deleteEvent app f.organizer f.eventId) \first ->
            withAsync (deleteEvent app f.organizer f.eventId) \second -> do
              awaitBlockedSessions observer holderPid 2
              releaseBarrier held
              within "lock holder commit" $ wait holder
              a <- within "first delete" $ wait first
              b <- within "second delete" $ wait second
              sort [a, b] `shouldBe` [Succeeded, Missing]
              aggregate observer f `shouldReturn` Aggregate 0 0 0 0

  it "waits behind a gameplay transaction's game lock without taking the event lock" $
    withConnection harness \holding -> withConnection harness \observer -> do
      f <- seedEvent observer seedUser "delete-gameplay"
      held <- newBarrier
      holderPid <- backendPid holding
      -- Gameplay locks its game, then the event. Deletion must not hold the
      -- event row while it waits for the game, or this would deadlock.
      withAsync (transaction holding do
        lockGameRow f.group1
        liftIO $ pauseAt held
        void $ modifySharedStateLocked f.eventId id) \holder -> do
          link holder
          awaitBarrier held
          withAsync (deleteEvent app f.organizer f.eventId) \deleting -> do
            awaitBlockedSessions observer holderPid 1
            releaseBarrier held
            within "gameplay transaction commit" $ wait holder
            within "delete" (wait deleting) `shouldReturn` Succeeded
            aggregate observer f `shouldReturn` Aggregate 0 0 0 0

lockGameRow :: ArkhamGameId -> ReaderT SqlBackend IO ()
lockGameRow gameId = void
  (rawSql "SELECT 1::bigint FROM arkham_games WHERE id = ? FOR UPDATE" [toPersistValue gameId]
    :: ReaderT SqlBackend IO [Single Int64])

seedGame :: SqlBackend -> Text -> IO ArkhamGameId
seedGame connection label = do
  now <- currentTimeMicros
  let game = Game.newCampaign "01" Nothing 1 2 Easy False
  transaction connection $ P.insert $ ArkhamGame label game 1 WithFriends now now False

seedEvent :: SqlBackend -> SeedUser -> Text -> IO EpicFixture
seedEvent connection seedUser label = do
  now <- currentTimeMicros
  organizer <- seedUser connection (label <> "-organizer")
  player <- seedUser connection (label <> "-player")
  group0 <- seedGame connection (label <> "-group-0")
  group1 <- seedGame connection (label <> "-group-1")
  transaction connection do
    eventId <- P.insert $ ArkhamEpicEvent label organizer Nothing Nothing "Easy" (emptySharedEventState 4) 4 0 now now
    P.insert_ $ ArkhamEpicMember eventId organizer Organizer Nothing
    P.insert_ $ ArkhamEpicMember eventId player GroupPlayer (Just 0)
    for_ [(0 :: Int, group0), (1, group1)] \(ordinal, gameId) ->
      P.insert_ $ ArkhamEpicGroup eventId ordinal (Just gameId) ("Group " <> show ordinal) 2
    pure EpicFixture {..}

aggregate :: SqlBackend -> EpicFixture -> IO Aggregate
aggregate connection f = transaction connection do
  events <- P.count [ArkhamEpicEventId P.==. f.eventId]
  members <- P.count [ArkhamEpicMemberArkhamEpicEventId P.==. f.eventId]
  groups <- P.count [ArkhamEpicGroupArkhamEpicEventId P.==. f.eventId]
  games <- P.count [ArkhamGameId P.<-. [f.group0, f.group1]]
  pure Aggregate {..}

-- What a delete or a stray write would change: the event's shared state and
-- step, and each game's step and update time.
snapshot :: SqlBackend -> EpicFixture -> IO (Maybe (SharedEventState, Int, UTCTime), [Maybe (Int, UTCTime)])
snapshot connection f = transaction connection do
  event <- P.get f.eventId
  games <- traverse P.get [f.group0, f.group1]
  pure
    ( (\e -> (arkhamEpicEventSharedState e, arkhamEpicEventStep e, arkhamEpicEventUpdatedAt e)) <$> event
    , fmap (\g -> (arkhamGameStep g, arkhamGameUpdatedAt g)) <$> games
    )

deleteEvent :: App -> UserId -> ArkhamEpicEventId -> IO Outcome
deleteEvent app uid eid = route app uid (deleteApiV1ArkhamEventR eid)

-- The HCError is caught only after runDB has rolled back.
route :: App -> UserId -> Handler () -> IO Outcome
route app uid action = do
  token <- unsafeHandler app $ userIdToToken uid
  let request = defaultRequest
        { requestMethod = "DELETE"
        , requestHeaders = [("Authorization", "Token " <> encodeUtf8 token)]
        }
  unsafeHandler app $
    (Succeeded <$ withRequest request action) `Exception.catch` \case
      HCError NotFound -> pure Missing
      HCError (PermissionDenied _) -> pure Denied
      other -> Exception.throwIO other
