{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

-- Standalone integration suite, intentionally not hspec-discovered by unit
-- tests. Invocation and stage2 package wiring are documented in the launcher.
module Main where

import Arkham.ArkhamDB
import Api.Arkham.ArkhamDBCleanup (cleanupBatchSize, purgeExpiredLinks)
import Arkham.ArkhamDB.Service
import Arkham.Decklist (ArkhamDBDecklist (..))
import Control.Exception (throwIO)
import Data.Time.Clock (UTCTime, addUTCTime, getCurrentTime)
import Database.Persist (Entity (..), entityKey, toPersistValue)
import Database.Persist qualified as P
import Database.Persist.Sql (Single (..), SqlBackend, rawExecute, rawSql, runSqlConn, toSqlKey)
import Database.Persist.Types (PersistValue (..))
import Entity.Arkham.ArkhamDBAccount
import Entity.Arkham.Deck
import Entity.User (UserId)
import Foundation (App, unsafeHandler, userIdToToken)
import Postgres.Harness
import Postgres.HarnessSpec qualified as Harness
import Postgres.Production
import Relude hiding (first, second)
import Settings (ArkhamDBSettings (..))
import System.Timeout (timeout)
import Test.Hspec
import UnliftIO.Async (link, wait, withAsync)

data SmokeRollback = SmokeRollback deriving stock (Show)
instance Exception SmokeRollback

main :: IO ()
main = withHarness \harness -> withProductionApp harness \app ->
  hspec do
    Harness.spec harness
    spec harness app

spec :: Harness -> App -> Spec
spec harness app = describe "ArkhamDB production PostgreSQL stores" do
  it "claims and finalizes a nonce exactly once using migrated tables" $
    withConnection harness \connection -> do
      now <- getCurrentTime
      uid <- seedUser connection "finalize"
      runStore app connection (\store -> store.saveLinkState $ linkState now uid "finalize")
      claimed <- runStore app connection (\store -> store.claimLinkState now "finalize")
      claimed `shouldSatisfy` isJust
      runStore app connection (\store -> isNothing <$> store.claimLinkState now "finalize") `shouldReturn` True
      runStore app connection (\store -> store.stagePendingLink now "finalize" "digest" "encrypted-pending") `shouldReturn` True
      runStore app connection (\store -> store.finalizeLink now uid "digest" $ account now uid) `shouldReturn` True
      runStore app connection (\store -> store.finalizeLink now uid "digest" $ account now uid) `shouldReturn` False
      stored <- runStore app connection (\store -> store.findAccount uid)
      fmap arkhamDBAccountEncryptedAccessToken stored `shouldBe` Just "encrypted-access"

  it "serializes competing claims on separate sessions without timing sleeps" $
    withConnection harness \first -> withConnection harness \second -> withConnection harness \observer -> do
      firstPid <- backendPid first
      secondPid <- backendPid second
      firstPid `shouldNotBe` secondPid
      now <- getCurrentTime
      uid <- seedUser first "race"
      runStore app first (\store -> store.saveLinkState $ linkState now uid "race")
      barrier <- newBarrier
      withAsync
        (runStore app first \store -> do
          claimed <- store.claimLinkState now "race"
          liftIO $ pauseAt barrier
          pure $ isJust claimed)
        \winner -> do
          link winner
          awaitBarrier barrier
          withAsync (runStore app second \store -> isJust <$> store.claimLinkState now "race") \loser -> do
            link loser
            awaitBlockedBy observer firstPid secondPid
            releaseBarrier barrier
            within "winning claim" (wait winner) `shouldReturn` True
            within "losing claim" (wait loser) `shouldReturn` False

  it "rolls back nonce consumption and account creation together after an exception" $
    withConnection harness \connection -> do
      now <- getCurrentTime
      uid <- seedUser connection "rollback"
      runStore app connection (\store -> store.saveLinkState $ linkState now uid "rollback")
      let abort = runStore app connection \store -> do
            claimed <- store.claimLinkState now "rollback"
            liftIO $ claimed `shouldSatisfy` isJust
            staged <- store.stagePendingLink now "rollback" "digest" "encrypted-pending"
            liftIO $ staged `shouldBe` True
            finalized <- store.finalizeLink now uid "digest" $ account now uid
            liftIO $ finalized `shouldBe` True
            liftIO $ throwIO SmokeRollback
      -- fakeHandler reports uncaught Handler exceptions as an InternalError;
      -- require our exact injected failure, not just any exception.
      abort `shouldThrow` errorCall "runFakeHandler issue: InternalError \"SmokeRollback\""
      stored <- runStore app connection (\store -> store.findAccount uid)
      isNothing stored `shouldBe` True
      rows <- transaction connection $
        rawSql "SELECT count(*) FROM arkhamdb_link_states WHERE user_id = ?" [toPersistValue uid]
      rows `shouldBe` [Single (1 :: Int64)]
      reclaimed <- runStore app connection (\store -> store.claimLinkState now "rollback")
      reclaimed `shouldSatisfy` isJust

  it "compares the old access credential before persisting a refresh" $
    withConnection harness \connection -> do
      now <- getCurrentTime
      uid <- seedUser connection "compare-refresh"
      let previous = account now uid
          refreshed = previous {arkhamDBAccountEncryptedAccessToken = "encrypted-new-access"}
      void $ seedAccount app connection now uid "compare-refresh" previous
      runStore app connection (\store -> store.updateAccount previous refreshed) `shouldReturn` True
      runStore app connection (\store -> store.updateAccount previous previous) `shouldReturn` False
      stored <- runStore app connection (\store -> store.findAccount uid)
      fmap arkhamDBAccountEncryptedAccessToken stored `shouldBe` Just "encrypted-new-access"

  it "holds the production refresh lock through remote refresh and concurrent unlink" $
    withConnection harness \refreshing -> withConnection harness \unlinking -> withConnection harness \observer -> do
      now <- getCurrentTime
      uid <- seedUser refreshing "unlink-refresh"
      previous <- encryptedAccount settings now uid $
        (tokens now) {tokenExpiresAt = Just $ addUTCTime (-60) now}
      void $ seedAccount app refreshing now uid "unlink-refresh" previous
      barrier <- newBarrier
      let client = hoistArkhamDBClient liftIO $ transport
            { refreshTokens = \credential -> do
                credential `shouldBe` "fixture-refresh"
                pauseAt barrier
                pure $ Right $ (tokens now) {accessToken = "refreshed-access"}
            }
      refreshingPid <- backendPid refreshing
      unlinkingPid <- backendPid unlinking
      withAsync
        (runStoreActions app refreshing \store -> runExceptT $ refreshAccount store client settings now previous)
        \refresh -> do
          link refresh
          awaitBarrier barrier
          withAsync (runStore app unlinking \store -> store.unlinkAccount uid) \unlink -> do
            link unlink
            awaitBlockedBy observer refreshingPid unlinkingPid
            releaseBarrier barrier
            within "refresh commits before unlink" (wait refresh) `shouldReturn` Right "refreshed-access"
            within "unlink after refresh" (wait unlink)
      stored <- runStore app observer (\store -> store.findAccount uid)
      isNothing stored `shouldBe` True
      runStore app observer (\store -> store.updateAccount previous previous) `shouldReturn` False

  it "does not stage callback credentials when unlink commits during OAuth exchange" $
    withConnection harness \callback -> withConnection harness \unlinking -> do
      now <- getCurrentTime
      uid <- seedUser callback "unlink-exchange"
      runStoreActions app callback (\store -> store.saveLinkState $ linkState now uid "unlink-exchange")
      barrier <- newBarrier
      let client = hoistArkhamDBClient liftIO $ transport
            { exchangeCode = \_ _ -> do
                pauseAt barrier
                pure $ Right $ tokens now
            }
      withAsync
        (runStoreActions app callback \store -> runExceptT $
          completeLink store client settings now "unlink-exchange" "authorization-code" "completion-secret")
        \exchange -> do
          link exchange
          awaitBarrier barrier
          -- Claim has committed before the transport is called, just as in runDB.
          within "unlink during OAuth exchange" $
            runStoreActions app unlinking (\store -> store.unlinkAccount uid)
          releaseBarrier barrier
          result <- within "callback after unlink" $ wait exchange
          fmap arkhamDBErrorCode (leftToMaybe result) `shouldBe` Just "oauthCallbackInvalidState"
      pendingLink <- runStoreActions app callback (\store -> store.findPendingLink now uid $ completionHash "completion-secret")
      isNothing pendingLink `shouldBe` True
      stored <- runStoreActions app callback (\store -> store.findAccount uid)
      isNothing stored `shouldBe` True

  it "refuses callback finalization queued behind a committed unlink" $
    withConnection harness \unlinking -> withConnection harness \finalizing -> withConnection harness \observer -> do
      now <- getCurrentTime
      uid <- seedUser unlinking "unlink-finalize"
      runStore app unlinking \store -> do
        store.saveLinkState $ linkState now uid "unlink-finalize"
        void $ store.claimLinkState now "unlink-finalize"
        void $ store.stagePendingLink now "unlink-finalize" "digest" "encrypted-pending"
      barrier <- newBarrier
      unlinkingPid <- backendPid unlinking
      finalizingPid <- backendPid finalizing
      withAsync
        (runStore app unlinking \store -> store.unlinkAccount uid >> liftIO (pauseAt barrier))
        \unlink -> do
          link unlink
          awaitBarrier barrier
          withAsync
            (runStore app finalizing \store -> store.finalizeLink now uid "digest" $ account now uid)
            \finalize -> do
              link finalize
              awaitBlockedBy observer unlinkingPid finalizingPid
              releaseBarrier barrier
              within "unlink commits before callback" (wait unlink)
              within "stale callback finalization" (wait finalize) `shouldReturn` False
      stored <- runStore app observer (\store -> store.findAccount uid)
      isNothing stored `shouldBe` True

  it "does not resurrect an import loaded before unlink and relink to a new generation" $
    withConnection harness \importing -> withConnection harness \unlinking -> do
      now <- getCurrentTime
      uid <- seedUser importing "unlink-import"
      previous <- encryptedAccount settings now uid $ tokens now
      oldGeneration <- seedAccount app importing now uid "unlink-import" previous
      barrier <- newBarrier
      let client = hoistArkhamDBClient liftIO $ transport
            { loadDeck = \credential remoteId -> do
                credential `shouldBe` "fixture-access"
                remoteId `shouldBe` "123"
                pauseAt barrier
                pure $ Right remote
            }
      withAsync
        (runStoreActions app importing \store -> runExceptT $
          importRemoteDeck store client settings now uid "123" forbiddenPublic)
        \imported -> do
          link imported
          awaitBarrier barrier
          runStore app unlinking (\store -> store.unlinkAccount uid)
          generation <- seedAccount app unlinking now uid "relink-import" previous
          generation `shouldNotBe` oldGeneration
          releaseBarrier barrier
          result <- within "stale import completion" $ wait imported
          fmap (const ()) result `shouldBe` Left linkChanged
      counts <- deckCounts importing uid
      counts `shouldBe` (0, 0)

  it "serializes competing imports into one local deck and user/remote mapping" $
    withConnection harness \first -> withConnection harness \second -> withConnection harness \observer -> do
      now <- getCurrentTime
      uid <- seedUser first "duplicate-import"
      previous <- encryptedAccount settings now uid $ tokens now
      void $ seedAccount app first now uid "duplicate-import" previous
      barrier <- newBarrier
      let client = hoistArkhamDBClient liftIO $ transport {loadDeck = \_ _ -> pure $ Right remote}
          importing store = runExceptT $ importRemoteDeck store client settings now uid "123" forbiddenPublic
      firstPid <- backendPid first
      secondPid <- backendPid second
      withAsync
        (runStore app first \store -> do
          imported <- importing store
          liftIO $ pauseAt barrier
          pure imported)
        \winner -> do
          link winner
          awaitBarrier barrier
          withAsync (runStoreActions app second importing) \follower -> do
            link follower
            awaitBlockedBy observer firstPid secondPid
            releaseBarrier barrier
            firstResult <- within "first import" $ wait winner
            secondResult <- within "serialized import" $ wait follower
            firstResult `shouldSatisfy` isRight
            fmap entityKey firstResult `shouldBe` fmap entityKey secondResult
      deckCounts observer uid `shouldReturn` (1, 1)

  it "purges expired nonces and staged credentials in bounded batches without changing the account" $
    withConnection harness \connection -> do
      now <- getCurrentTime
      uid <- seedUser connection "expiry-batches"
      generation <- seedAccount app connection now uid "expiry-batches" $ account now uid
      let expired n = (linkState now uid $ "expired-" <> show n)
            { arkhamDBLinkStateExpiresAt = if n == (0 :: Int) then now else addUTCTime (-1) now
            , arkhamDBLinkStateClaimedAt = if even n then Just now else Nothing
            , arkhamDBLinkStateEncryptedPendingTokens = if even n then Just "encrypted-pending-secret" else Nothing
            }
          active = linkState now uid "active-nonce"
          pendingLink = (linkState now uid "active-pending")
            { arkhamDBLinkStateClaimedAt = Just now
            , arkhamDBLinkStateEncryptedPendingTokens = Just "encrypted-active-secret"
            }
      transaction connection $
        P.insertMany_ $ map expired [0 .. 2 * cleanupBatchSize + 4] <> [active, pendingLink]
      -- Starting another user's link must not synchronously delete a global
      -- backlog in that request's transaction. Only the scheduled batches do.
      otherUser <- seedUser connection "expiry-unrelated-start"
      runStoreActions app connection \store -> store.saveLinkState $ linkState now otherUser "unrelated-active"
      transaction connection (purgeExpiredLinks now) `shouldReturn` cleanupBatchSize
      transaction connection (purgeExpiredLinks now) `shouldReturn` cleanupBatchSize
      transaction connection (purgeExpiredLinks now) `shouldReturn` 5
      transaction connection (purgeExpiredLinks now) `shouldReturn` 0
      remaining <- transaction connection $ P.selectList [ArkhamDBLinkStateUserId P.==. uid] []
      sort (map (arkhamDBLinkStateNonce . P.entityVal) remaining) `shouldBe` ["active-nonce", "active-pending"]
      runStoreActions app connection (\store -> store.findAccountGeneration uid) `shouldReturn` Just generation
      limits <- transaction connection $ rawSql
        "SELECT current_setting('statement_timeout'), current_setting('lock_timeout')" []
      limits `shouldBe` [(Single ("8s" :: Text), Single ("3s" :: Text))]

  it "makes deadline-bounded expiry progress amid 200000 active nonces and staged credentials" $
    withConnection harness \connection -> do
      now <- getCurrentTime
      let activeCount = 200000 :: Int
          expiredCount = 3 * cleanupBatchSize + 5
          -- One authorization per real migrated user, half already staged.
          -- Bounded fixture inserts retain the harness's normal SQL deadlines.
          seed start count expires = transaction connection $ rawExecute
            "WITH owners AS (INSERT INTO users (username, email, password_digest) SELECT 'expiry-large-' || n::text, 'expiry-large-' || n::text || '@postgres-test.invalid', 'not-a-real-password' FROM generate_series(?::integer, ?::integer) AS n RETURNING id) INSERT INTO arkhamdb_link_states (nonce, user_id, expires_at, created_at, claimed_at, completion_hash, encrypted_pending_tokens) SELECT 'expiry-large-' || id::text, id, ?, ?, CASE WHEN id % 2 = 0 THEN ?::timestamptz END, CASE WHEN id % 2 = 0 THEN 'completion-' || id::text END, CASE WHEN id % 2 = 0 THEN repeat('encrypted-pending-', 32) END FROM owners"
            [ PersistInt64 $ fromIntegral start, PersistInt64 $ fromIntegral (start + count - 1)
            , PersistUTCTime expires, PersistUTCTime $ addUTCTime (-60) now
            , PersistUTCTime $ addUTCTime (-30) now
            ]
          activeRows = transaction connection $ rawSql
            "SELECT count(*)::bigint, count(encrypted_pending_tokens)::bigint FROM arkhamdb_link_states AS states JOIN users ON users.id = states.user_id WHERE users.username LIKE 'expiry-large-%' AND states.expires_at > ?"
            [PersistUTCTime now]
          sweep = timeout 5000000 (transaction connection $ purgeExpiredLinks now)
            >>= maybe (fail "Production cleanup exceeded its five-second budget") pure
      for_ [0, 10000 .. activeCount - 1] \start -> seed start 10000 $ addUTCTime 600 now
      seed activeCount expiredCount $ addUTCTime (-1) now
      transaction connection $ rawExecute "ANALYZE arkhamdb_link_states" []
      activeBefore <- activeRows
      activeBefore `shouldBe` [(Single (200000 :: Int64), Single (100000 :: Int64))]
      replicateM 3 sweep `shouldReturn` replicate 3 cleanupBatchSize
      sweep `shouldReturn` 5
      sweep `shouldReturn` 0
      activeRows `shouldReturn` activeBefore

  it "skips a locked expired authorization and makes progress after its transaction finishes" $
    withConnection harness \writer -> withConnection harness \cleaner -> do
      now <- getCurrentTime
      uid <- seedUser writer "expiry-locked"
      key <- transaction writer $ P.insert $
        (linkState now uid "expiry-locked") {arkhamDBLinkStateExpiresAt = addUTCTime (-1) now}
      barrier <- newBarrier
      withAsync
        (transaction writer do
          P.update key [ArkhamDBLinkStateEncryptedPendingTokens P.=. Just "encrypted-pending-secret"]
          liftIO $ pauseAt barrier)
        \writing -> do
          link writing
          awaitBarrier barrier
          transaction cleaner (purgeExpiredLinks now) `shouldReturn` 0
          releaseBarrier barrier
          within "expired authorization writer" $ wait writing
      transaction cleaner (purgeExpiredLinks now) `shouldReturn` 1
      transaction cleaner (P.get key) >>= (`shouldSatisfy` isNothing)

-- Fixture SQL only; all lifecycle mutations above invoke persistentStoreWith.
-- User's production entity currently includes an admin field not in Sqitch;
-- do not hide that unrelated schema drift by inventing a test-only column.
seedUser :: SqlBackend -> Text -> IO UserId
seedUser connection label = transaction connection do
  rows <- rawSql
    "INSERT INTO users (username, email, password_digest) VALUES (?, ?, 'not-a-real-password') RETURNING id"
    [PersistText label, PersistText $ label <> "@postgres-test.invalid"]
  case rows of
    [Single key] -> pure $ toSqlKey key
    _ -> liftIO $ fail "Expected one fixture user"

linkState :: UTCTime -> UserId -> Text -> ArkhamDBLinkState
linkState now uid nonce = ArkhamDBLinkState nonce uid (addUTCTime 600 now) now Nothing Nothing Nothing

account :: UTCTime -> UserId -> ArkhamDBAccount
account now uid = ArkhamDBAccount uid (Just "remote-user") (Just "remote-name")
  "encrypted-access" (Just "encrypted-refresh") (Just $ addUTCTime 3600 now) now now Nothing 0

seedAccount :: App -> SqlBackend -> UTCTime -> UserId -> Text -> ArkhamDBAccount -> IO ArkhamDBAccountId
seedAccount app connection now uid nonce value = do
  runStore app connection \store -> do
    store.saveLinkState $ linkState now uid nonce
    claimed <- store.claimLinkState now nonce
    liftIO $ claimed `shouldSatisfy` isJust
    staged <- store.stagePendingLink now nonce "digest" "encrypted-pending"
    liftIO $ staged `shouldBe` True
    finalized <- store.finalizeLink now uid "digest" value
    liftIO $ finalized `shouldBe` True
  runStore app connection (\store -> store.findAccountGeneration uid)
    >>= maybe (fail "Expected linked account generation") pure

deckCounts :: SqlBackend -> UserId -> IO (Int64, Int64)
deckCounts connection uid = transaction connection do
  rows <- rawSql
    "SELECT (SELECT count(*) FROM arkham_decks WHERE user_id = ?), (SELECT count(*) FROM arkhamdb_deck_links WHERE user_id = ?)"
    [toPersistValue uid, toPersistValue uid]
  case rows of
    [(Single decks, Single mappings)] -> pure (decks, mappings)
    _ -> liftIO $ fail "Expected deck and mapping counts"

settings :: ArkhamDBSettings
settings = ArkhamDBSettings "fixture-client" "fixture-secret"
  "MDEyMzQ1Njc4OWFiY2RlZjAxMjM0NTY3ODlhYmNkZWY="
  "https://arkhamdb.invalid" "https://server.invalid"

tokens :: UTCTime -> ArkhamDBOAuthTokens
tokens now = ArkhamDBOAuthTokens "fixture-access" (Just "fixture-refresh")
  (Just $ addUTCTime 3600 now) (Just "remote-user") (Just "remote-name")

remote :: ArkhamDBRemoteDeck
remote = ArkhamDBRemoteDeck
  (ArkhamDBDecklist mempty mempty "01001" "Roland Banks" Nothing Nothing Nothing (Just "123") (Just "SQL fixture"))
  "123" (Just "2026-10-28T20:00:00Z") (Just "1.0") Nothing True "" "" mempty Nothing

-- Only the remote transport is substituted; persistence always uses the
-- production store. Unexpected network operations fail rather than reach URLs.
transport :: ArkhamDBClient IO
transport = ArkhamDBClient
  { exchangeCode = \_ _ -> fail "Unexpected OAuth exchange"
  , refreshTokens = \_ -> fail "Unexpected token refresh"
  , listDecks = \_ -> fail "Unexpected remote listing"
  , loadDeck = \_ _ -> fail "Unexpected remote load"
  , upgradeDeck = \_ _ _ _ -> fail "Unexpected remote upgrade"
  , createDeck = \_ _ -> fail "Unexpected remote create"
  , saveDeck = \_ _ _ -> fail "Unexpected remote save"
  }

forbiddenPublic :: MonadIO m => ArkhamDeckId -> m (Entity ArkhamDeck)
forbiddenPublic _ = liftIO $ fail "Unexpected public fallback"
