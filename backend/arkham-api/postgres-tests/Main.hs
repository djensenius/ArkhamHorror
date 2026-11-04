{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

-- Standalone integration suite, intentionally not hspec-discovered by unit
-- tests. Invocation and stage2 package wiring are documented in the launcher.
module Main where

import Arkham.ArkhamDB
import Api.Arkham.ArkhamDBCleanup (cleanupBatchSize, purgeExpiredLinks)
import Api.Arkham.ArkhamDBStore (persistentStoreWith)
import Api.Arkham.ArkhamDBWriteBack (writeBackStore)
import Api.Arkham.Maintenance (Maintenance (..))
import Api.Handler.Arkham.ArkhamDB (runArkhamDB)
import Api.Handler.Arkham.Decks
  ( saveDeckOverlay, savePublicSyncedDeck
  , validatePlayableDecklist )
import Arkham.ArkhamDB.WriteBack
import Arkham.ArkhamDB.Service
import Arkham.Custom.Overlay (DeckOverlay (..))
import Arkham.Decklist (ArkhamDBDecklist (..))
import Arkham.Decklist qualified as Decklist
import Control.Concurrent.MVar qualified as MVar
import Control.Exception (throwIO)
import Data.Aeson (Value (Bool), decode, encode, object, toJSON, (.=))
import Data.Map.Strict qualified as Map
import Data.Text qualified as T
import Data.Time.Clock (UTCTime, addUTCTime)
import Data.UUID qualified as UUID
import Database.Persist (Entity (..), entityKey, toPersistValue)
import Database.Persist qualified as P
import Database.Persist.Sql (Single (..), SqlBackend, rawExecute, rawSql, runSqlConn, toSqlKey)
import Database.Persist.Types (PersistValue (..))
import Entity.Arkham.ArkhamDBAccount
import Entity.Arkham.Deck
import Entity.Arkham.Game
import Entity.User (UserId)
import Foundation (App (appMaintenance), Handler, unsafeHandler)
import GHC.Clock (getMonotonicTime)
import Network.HTTP.Types (Status, status200, status400, status401, status404, status405, status409, status502, status503)
import Network.Wai (rawPathInfo, requestMethod, responseLBS)
import Network.Wai.Handler.Warp qualified as Warp
import Postgres.Harness
import Postgres.HarnessSpec qualified as Harness
import Postgres.DispatchSpec qualified as DispatchSQL
import Postgres.PayloadSpec qualified as PayloadSQL
import Postgres.ConcurrencySpec qualified as ConcurrencySQL
import Postgres.DeleteSpec qualified as DeleteSQL
import Postgres.EndToEndSpec qualified as EndToEndSQL
import Postgres.ThirdEditionSpec qualified as ThirdEditionSQL
import Postgres.Production
import Postgres.WriteBackSpec qualified as WriteBackSQL
import Relude hiding (first, second)
import Settings (ArkhamDBSettings (..))
import System.Environment qualified as Environment
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
    WriteBackSQL.spec harness app remote (writeBackSeed app)
      (\connection uid label -> do
        now <- currentTimeMicros
        encrypted <- encryptedAccount settings now uid $ tokens now
        void $ seedAccount app connection now uid label encrypted
        let client = hoistArkhamDBClient liftIO $ transport {loadDeck = \_ _ -> pure $ Right remote}
        fmap entityKey $ either (fail . show) pure =<< runStoreActions app connection
          (\store -> runExceptT $ importRemoteDeck store client settings now uid "123" forbiddenPublic))
    DispatchSQL.spec harness app settings transport remote (writeBackSeed app)
    PayloadSQL.spec harness app settings transport remote (writeBackSeed app)
    ConcurrencySQL.spec harness app settings transport remote (writeBackSeed app)
    DeleteSQL.spec harness app settings transport remote (writeBackSeed app)
    EndToEndSQL.spec harness app settings transport remote (writeBackSeed app)
    ThirdEditionSQL.spec harness app

writeBackSeed :: App -> WriteBackSQL.Seed
writeBackSeed app connection label = do
  now <- currentTimeMicros
  uid <- seedUser connection label
  encrypted <- encryptedAccount settings now uid $ tokens now
  generation <- seedAccount app connection now uid label encrypted
  let client = hoistArkhamDBClient liftIO $ transport {loadDeck = \_ _ -> pure $ Right remote}
  Entity deckId _ <- either (fail . show) pure =<< runStoreActions app connection
    (\store -> runExceptT $ importRemoteDeck store client settings now uid "123" forbiddenPublic)
  pure (uid, generation, deckId)

spec :: Harness -> App -> Spec
spec harness app = describe "ArkhamDB production PostgreSQL stores" do
  it "claims and finalizes a nonce exactly once using migrated tables" $
    withConnection harness \connection -> do
      now <- currentTimeMicros
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
      now <- currentTimeMicros
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
      now <- currentTimeMicros
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
      now <- currentTimeMicros
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
      now <- currentTimeMicros
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

  it "turns production refresh lock contention into remoteUnavailable at the lock deadline" $
    withConnection harness \holding -> withConnection harness \refreshing -> withConnection harness \observer -> do
      now <- currentTimeMicros
      uid <- seedUser holding "refresh-deadline"
      previous <- encryptedAccount settings now uid (tokens now)
      generation <- seedAccount app holding now uid "refresh-deadline" previous
      -- Import a deck while the token is valid, then expire it: the snapshot
      -- below must hold a real mapping for the timeout path to preserve.
      let importer = hoistArkhamDBClient liftIO $ transport {loadDeck = \_ _ -> pure $ Right remote}
      void $ either (fail . show) pure =<< runStoreActions app holding
        (\store -> runExceptT $ importRemoteDeck store importer settings now uid "123" forbiddenPublic)
      valid <- runStore app observer (\store -> store.findAccount uid) >>= maybe (fail "Expected linked account") pure
      runStore app holding (\store -> store.updateAccount valid valid {arkhamDBAccountExpiresAt = Just $ addUTCTime (-60) now})
        `shouldReturn` True
      stored <- runStore app observer (\store -> store.findAccount uid) >>= maybe (fail "Expected linked account") pure
      arkhamDBAccountExpiresAt stored `shouldBe` Just (addUTCTime (-60) now)
      persisted <- linkedRows observer uid
      bimap length length persisted `shouldBe` (1, 1)
      transaction observer (rawSql "SELECT remote_deck_id FROM arkhamdb_deck_links WHERE user_id = ?" [toPersistValue uid])
        `shouldReturn` [Single ("123" :: Text)]
      barrier <- newBarrier
      calls <- newIORef (0 :: Int)
      let client = hoistArkhamDBClient liftIO $ transport
            { refreshTokens = \_ -> do
                modifyIORef' calls (+ 1)
                fail "Refresh acquired the held refresh lock instead of timing out" }
      holdingPid <- backendPid holding
      refreshingPid <- backendPid refreshing
      -- `holding` is one libpq connection from withPostgresqlConn (not a pool),
      -- so these reads and the holder's transaction share one session; the PID
      -- check after commit confirms it.
      sessionTimeout <- lockTimeout holding
      sessionTimeout `shouldNotBe` "5s"
      withAsync (runStore app holding \store -> withRefreshLock store uid \_ -> liftIO do
          -- Inside the lock's transaction: the deadline is exactly 5s.
          runReaderT currentLockTimeout holding `shouldReturn` "5s"
          pauseAt barrier) \holder -> do
        link holder
        awaitBarrier barrier
        started <- getMonotonicTime
        withAsync
          (within "refresh at lock deadline" do
            result <- refreshResponse app refreshing client now stored
            finished <- getMonotonicTime
            pure (result, finished - started))
          \refresh -> do
            link refresh
            awaitBlockedBy observer holdingPid refreshingPid
            (result, elapsed) <- wait refresh
            expectSanitizedError status502 "remoteUnavailable" "ArkhamDB request failed" result
            -- The 3s server default would fail here: the 5s SET LOCAL deadline fired.
            elapsed `shouldSatisfy` (>= 4.5)
        releaseBarrier barrier
        within "refresh lock holder commits" $ wait holder
      -- SET LOCAL: the 5s deadline ended with the holder's transaction.
      backendPid holding `shouldReturn` holdingPid
      lockTimeout holding `shouldReturn` sessionTimeout
      readIORef calls `shouldReturn` 0
      linkedRows observer uid `shouldReturn` persisted
      runStore app observer (\store -> store.findAccountGeneration uid) `shouldReturn` Just generation

  it "keeps credentials, generation and mappings after a failed remote refresh" $
    refreshFailureCase harness app "refresh-remote-error" (Just "fixture-refresh")
      (Just $ ArkhamDBRemoteError $ leakMarker <> " ArkhamDB is down") status502 "remoteUnavailable" "ArkhamDB request failed"

  it "keeps credentials, generation and mappings after a rejected remote refresh" $
    refreshFailureCase harness app "refresh-rejected" (Just "fixture-refresh")
      (Just ArkhamDBUnauthorized) status409 "tokenExpired" "ArkhamDB rejected the token; relink required"

  it "requires relinking without calling ArkhamDB when no refresh token is stored" $
    refreshFailureCase harness app "refresh-missing-token" Nothing Nothing status409 "tokenExpired"
      "ArkhamDB token expired; relink required"

  it "keeps the mapping and skips public fallback when a linked sync's token refresh fails remotely" $
    syncRefreshFailureCase harness app "sync-refresh-remote-error" (Just "fixture-refresh")
      (Just $ ArkhamDBRemoteError $ leakMarker <> " ArkhamDB is down") status502 "remoteUnavailable" "ArkhamDB request failed"

  it "keeps the mapping and skips public fallback when a linked sync's token refresh is rejected" $
    syncRefreshFailureCase harness app "sync-refresh-rejected" (Just "fixture-refresh")
      (Just ArkhamDBUnauthorized) status409 "tokenExpired" "ArkhamDB rejected the token; relink required"

  it "keeps the mapping and skips public fallback when a linked sync has no refresh token" $
    syncRefreshFailureCase harness app "sync-refresh-missing-token" Nothing Nothing status409 "tokenExpired"
      "ArkhamDB token expired; relink required"

  it "does not stage callback credentials when unlink commits during OAuth exchange" $
    withConnection harness \callback -> withConnection harness \unlinking -> do
      now <- currentTimeMicros
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
      now <- currentTimeMicros
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
      now <- currentTimeMicros
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
      now <- currentTimeMicros
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

  for_ [False, True] \markers ->
    it ("removes an unchanged 404 mapping and falls back exactly once with " <> if markers then "non-null markers" else "null markers") $
      withConnection harness \connection -> do
        now <- currentTimeMicros
        let label = if markers then "404-current-markers" else "404-current-null"
            original = if markers then remote else remote {dateUpdate = Nothing, deckVersion = Nothing}
            initialClient = hoistArkhamDBClient liftIO $ transport {loadDeck = \_ _ -> pure $ Right original}
            missingClient = hoistArkhamDBClient liftIO $ transport {loadDeck = \_ _ -> pure $ Left ArkhamDBNotFound}
        uid <- seedUser connection label
        encrypted <- encryptedAccount settings now uid $ tokens now
        void $ seedAccount app connection now uid label encrypted
        Entity deckId _ <- either (fail . show) pure =<< runStoreActions app connection
          (\store -> runExceptT $ importRemoteDeck store initialClient settings now uid "123" forbiddenPublic)
        calls <- MVar.newMVar (0 :: Int)
        let public key = do
              liftIO $ MVar.modifyMVar_ calls $ pure . (+ 1)
              result <- runSqlConn (savePublicSyncedDeck uid key remote.deck) connection
              either (liftIO . fail . show) pure result
        result <- runStoreActions app connection \store ->
          runExceptT $ syncRemoteDeck store missingClient settings now uid deckId public
        fmap entityKey result `shouldBe` Right deckId
        MVar.readMVar calls `shouldReturn` 1
        transaction connection (isNothing <$> P.getBy (UniqueArkhamDBDeckLinkDeck deckId)) `shouldReturn` True

  for_ [False, True] \missing ->
    for_ ["retarget", "markers", "identity"] \change ->
      it ("fences a delayed " <> (if missing then "404" else "sync") <> " after mapping " <> toString change) $
        withConnection harness \loading -> withConnection harness \writing -> do
          now <- currentTimeMicros
          let label = "stale-" <> change <> if missing then "-404" else "-sync"
          uid <- seedUser loading label
          encrypted <- encryptedAccount settings now uid $ tokens now
          generation <- seedAccount app loading now uid label encrypted
          let initialClient = hoistArkhamDBClient liftIO $ transport {loadDeck = \_ _ -> pure $ Right remote}
          Entity deckId _ <- either (fail . show) pure =<< runStoreActions app loading
            (\store -> runExceptT $ importRemoteDeck store initialClient settings now uid "123" forbiddenPublic)
          mapping <- transaction writing (P.getBy $ UniqueArkhamDBDeckLinkDeck deckId)
            >>= maybe (fail "Expected imported mapping") pure
          barrier <- newBarrier
          let delayedClient = hoistArkhamDBClient liftIO $ transport
                { loadDeck = \_ _ -> do
                    pauseAt barrier
                    pure $ if missing then Left ArkhamDBNotFound else Right remote
                }
          withAsync
            (runStoreActions app loading \store -> runExceptT $
              syncRemoteDeck store delayedClient settings now uid deckId forbiddenPublic)
            \syncing -> do
              link syncing
              awaitBarrier barrier
              if change == "identity" then transaction writing do
                -- Fixture-only identity replacement with identical values:
                -- specifically isolate the mapping-id fence from generation,
                -- remote-id and marker checks.
                P.delete $ entityKey mapping
                P.insert_ $ P.entityVal mapping
              else do
                gameId <- WriteBackSQL.seedCampaignGame writing label now
                let campaign = UUID.toText (coerce gameId) <> "/01001"
                transaction writing do
                  P.update (entityKey mapping) [ArkhamDBDeckLinkWriteBack P.=. True]
                  P.insert_ $ ArkhamDBCampaignDeck campaign deckId uid 0
                  P.insert_ $ ArkhamDBPush deckId uid generation (label <> "/1/01001") campaign
                    (encodePushDeck remote.deck) 5 "pending" "ready" Nothing Nothing
                    remote.dateUpdate remote.deckVersion Nothing Nothing Nothing "normal" now now "scenarioEnd"
                claimed <- unsafeHandler app (writeBackStore.claimPush now False uid deckId)
                  >>= maybe (fail "Expected production claim") pure
                let committed = remote
                      { remoteDeckId = if change == "retarget" then "456" else "123"
                      , dateUpdate = Just "2026-10-28T22:00:00Z", deckVersion = Just "2.0"
                      }
                unsafeHandler app (writeBackStore.finishPush claimed committed) `shouldReturn` Just deckId
              expected <- transaction writing $ P.getBy $ UniqueArkhamDBDeckLinkDeck deckId
              releaseBarrier barrier
              result <- within "stale remote completion" $ wait syncing
              fmap (const ()) result `shouldBe` Left linkChanged
              (fmap (show :: Entity ArkhamDBDeckLink -> String) <$> transaction writing (P.getBy $ UniqueArkhamDBDeckLinkDeck deckId))
                `shouldReturn` fmap show expected

  it "fences a delayed 404 after a remote-ID-only mapping retarget" $
    withConnection harness \loading -> withConnection harness \writing -> do
      now <- currentTimeMicros
      let label = "stale-remote-id-only-404"
      uid <- seedUser loading label
      encrypted <- encryptedAccount settings now uid $ tokens now
      void $ seedAccount app loading now uid label encrypted
      let initialClient = hoistArkhamDBClient liftIO $ transport {loadDeck = \_ _ -> pure $ Right remote}
      Entity deckId _ <- either (fail . show) pure =<< runStoreActions app loading
        (\store -> runExceptT $ importRemoteDeck store initialClient settings now uid "123" forbiddenPublic)
      mapping <- transaction writing (P.getBy $ UniqueArkhamDBDeckLinkDeck deckId)
        >>= maybe (fail "Expected imported mapping") pure
      barrier <- newBarrier
      let delayedClient = hoistArkhamDBClient liftIO $ transport
            { loadDeck = \_ remoteId -> do
                remoteId `shouldBe` "123"
                pauseAt barrier
                pure $ Left ArkhamDBNotFound
            }
          -- Only the remote deck ID changes: mapping id, user and sync
          -- markers stay identical, isolating the remote-ID drop fence.
          expected = Entity (entityKey mapping) (entityVal mapping) {arkhamDBDeckLinkRemoteDeckId = "456"}
      withAsync
        (runStoreActions app loading \store -> runExceptT $
          syncRemoteDeck store delayedClient settings now uid deckId forbiddenPublic)
        \syncing -> do
          link syncing
          awaitBarrier barrier
          transaction writing $ P.update (entityKey mapping) [ArkhamDBDeckLinkRemoteDeckId P.=. "456"]
          releaseBarrier barrier
          result <- within "stale 404 completion" $ wait syncing
          fmap (const ()) result `shouldBe` Left linkChanged
      (fmap (show :: Entity ArkhamDBDeckLink -> String) <$> transaction writing (P.getBy $ UniqueArkhamDBDeckLinkDeck deckId))
        `shouldReturn` Just (show expected)

  for_ [False, True] \public ->
    for_ [False, True] \syncFirst ->
      for_ ["add", "replace", "remove", "invalid"] \change ->
        it ("serializes " <> (if public then "public" else "linked") <> " sync "
          <> (if syncFirst then "before" else "after") <> " overlay " <> toString change) $
          withConnection harness \first -> withConnection harness \second -> withConnection harness \observer -> do
            now <- currentTimeMicros
            let label = "overlay-" <> (if public then "public" else "linked")
                  <> (if syncFirst then "-first-" else "-last-") <> change
                old = DeckOverlay Nothing mempty (Map.singleton "01007" 1) mempty
                initial = if change `elem` ["add", "invalid"] then Nothing else Just old
                -- A real private identity accompanies every seeded attachment.
                seeded = UUID.fromWords 0 0 0 2 <$ initial
                edited = case change of
                  "remove" -> Nothing
                  "invalid" -> Just $ DeckOverlay Nothing (Map.singleton "01016" "unimplemented-card") mempty mempty
                  _ -> Just $ DeckOverlay Nothing mempty (Map.singleton "01008" 1) mempty
                incoming = remote {deck = remote.deck {slots = Map.singleton "01016" 1}}
                initialClient = hoistArkhamDBClient liftIO $ transport {loadDeck = \_ _ -> pure $ Right remote}
                syncingClient = hoistArkhamDBClient liftIO $ transport {loadDeck = \_ _ -> pure $ Right incoming}
            uid <- seedUser first label
            encrypted <- encryptedAccount settings now uid $ tokens now
            void $ seedAccount app first now uid label encrypted
            Entity deckId _ <- either (fail . show) pure =<< runStoreActionsWith validatePlayableDecklist app first
              (\store -> runExceptT $ importRemoteDeck store initialClient settings now uid "123" forbiddenPublic)
            transaction first $ P.update deckId
              [ArkhamDeckOverlay P.=. initial, ArkhamDeckOverlayAttachmentId P.=. seeded]
            let writer connection hold sync
                  | not sync = runHandlerResponse app $ runSqlConn
                      (do
                        saved <- saveDeckOverlay uid deckId edited
                        liftIO hold
                        pure saved)
                      connection
                  | public = runHandlerResponse app $ runSqlConn
                      (do
                        result <- savePublicSyncedDeck uid deckId incoming.deck
                        saved <- either (liftIO . fail . show) pure result
                        liftIO hold
                        pure saved)
                      connection
                  | otherwise = runHandlerResponse app $ storeTransaction validatePlayableDecklist connection \store -> do
                      result <- runExceptT $ syncRemoteDeck store syncingClient settings now uid deckId forbiddenPublic
                      saved <- either (liftIO . fail . show) pure result
                      liftIO hold
                      pure saved
            barrier <- newBarrier
            firstPid <- backendPid first
            secondPid <- backendPid second
            withAsync (writer first (pauseAt barrier) syncFirst) \leading -> do
              link leading
              awaitBarrier barrier
              withAsync (writer second (pure ()) (not syncFirst)) \following -> do
                link following
                awaitBlockedBy observer firstPid secondPid
                releaseBarrier barrier
                firstResult <- within "first overlay/sync writer" $ wait leading
                firstSaved <- either (fail . show) pure firstResult
                secondResult <- within "serialized overlay/sync writer" $ wait following
                persisted <- transaction observer (P.getJust deckId)
                -- ToJSON omits the private attachment id; compare it directly.
                let attachmentOf = arkhamDeckOverlayAttachmentId . P.entityVal
                    freshlyAssigned fresh = do
                      fresh `shouldSatisfy` isJust
                      fresh `shouldNotBe` seeded
                if change == "invalid" then do
                  -- The swap is dormant on the old empty base, but invalid on
                  -- the fetched base. Whichever writes second must revalidate
                  -- the actual locked composition and leave the first intact.
                  either Just (const Nothing) secondResult `shouldBe` Just status400
                  toJSON persisted `shouldBe` toJSON (P.entityVal firstSaved)
                  arkhamDeckOverlayAttachmentId persisted `shouldBe` attachmentOf firstSaved
                  if syncFirst
                    then attachmentOf firstSaved `shouldBe` seeded
                    else freshlyAssigned $ attachmentOf firstSaved
                else do
                  saved <- either (fail . show) pure secondResult
                  toJSON persisted `shouldBe` toJSON (P.entityVal saved)
                  arkhamDeckOverlay persisted `shouldBe` edited
                  toJSON (arkhamDeckList persisted) `shouldBe`
                    toJSON (if public then incoming.deck else remoteDecklist settings incoming)
                  let (overlaySaved, syncSaved) = if syncFirst then (saved, firstSaved) else (firstSaved, saved)
                      assigned = attachmentOf overlaySaved
                  -- The overlay write owns the final identity: a fresh id for
                  -- an attachment, none for removal. A provenance-free sync
                  -- keeps whichever identity it found under the deck lock.
                  arkhamDeckOverlayAttachmentId persisted `shouldBe` assigned
                  attachmentOf syncSaved `shouldBe` if syncFirst then seeded else assigned
                  if isNothing edited then assigned `shouldBe` Nothing else freshlyAssigned assigned

  it "assigns a fresh private attachment id on every actual overlay PUT, even byte-identical, and clears it on DELETE" $
    withConnection harness \connection -> do
      uid <- seedUser connection "overlay-attachment-identity"
      let overlay = DeckOverlay Nothing mempty (Map.singleton "01007" 1) mempty
          body = toStrict $ encode overlay
          deck = ArkhamDeck uid Nothing "Roland Banks" "Overlay attachment identity" remote.deck Nothing Nothing Nothing
          persistedAttachment deckId = do
            persisted <- transaction connection (P.getJust deckId)
            arkhamDeckOverlay persisted `shouldBe` Just overlay
            maybe (fail "Expected a persisted overlay attachment id") (pure . (,) persisted) $
              arkhamDeckOverlayAttachmentId persisted
      deckId <- transaction connection $ P.insert deck
      -- The response omits the private attachment id; it must equal the row.
      void $ expectSaved connection deckId =<< overlayRoute app uid deckId (Just body)
      (firstPersisted, firstId) <- persistedAttachment deckId
      -- A second PUT of the same bytes is a NEW attachment, not a no-op.
      void $ expectSaved connection deckId =<< overlayRoute app uid deckId (Just body)
      (secondPersisted, secondId) <- persistedAttachment deckId
      secondId `shouldNotBe` firstId
      toJSON secondPersisted `shouldBe` toJSON firstPersisted
      persisted <- expectSaved connection deckId =<< overlayRoute app uid deckId Nothing
      arkhamDeckOverlay persisted `shouldBe` Nothing
      arkhamDeckOverlayAttachmentId persisted `shouldBe` Nothing
      toJSON persisted `shouldBe` toJSON deck

  it "dispatches overlay PUT and DELETE through WAI routing, JWT authentication, ownership and maintenance" $
    withConnection harness \connection -> do
      owner <- seedUser connection "overlay-route-owner"
      stranger <- seedUser connection "overlay-route-stranger"
      let overlay = DeckOverlay Nothing mempty (Map.singleton "01007" 1) mempty
          replacement = DeckOverlay Nothing mempty (Map.singleton "01008" 1) mempty
          body = encode replacement
          seeded = UUID.fromWords 0 0 0 7
          deck = ArkhamDeck owner Nothing "Roland Banks" "Overlay routes" remote.deck (Just overlay) (Just seeded) Nothing
          json = [("Content-Type", "application/json")]
      deckId <- transaction connection $ P.insert deck
      let route method path headers = routeRequest app method (deckPath deckId path) headers
          snapshot = transaction connection (P.getJust deckId) <&> \d -> (toJSON d, arkhamDeckOverlayAttachmentId d)
      original <- snapshot
      snd original `shouldBe` Just seeded
      ownerToken <- authorizationFor app owner
      strangerToken <- authorizationFor app stranger
      for_ [("PUT", body), ("DELETE", "")] \(method, payload) -> do
        let refused expected path headers = do
              (status, _) <- route method path (headers <> json) payload
              (method, path, status) `shouldBe` (method, path, expected)
              snapshot `shouldReturn` original
        refused status401 "/overlay" []
        refused status401 "/overlay" [("Authorization", "Token not-a-jwt")]
        refused status404 "/overlays" [ownerToken]
        (status, response) <- route method "/overlay" (strangerToken : json) payload
        (status, decode @Value response) `shouldBe`
          (status400, Just $ object ["errorMsg" .= ("Deck does not belong to this user" :: Text)])
        snapshot `shouldReturn` original
        missing <- routeRequest app method (deckPath (ArkhamDeckKey $ UUID.fromWords 0 0 0 99) "/overlay") (ownerToken : json) payload
        fst missing `shouldBe` status404
        snapshot `shouldReturn` original
      (patched, _) <- route "PATCH" "/overlay" (ownerToken : json) body
      patched `shouldBe` status405
      snapshot `shouldReturn` original
      -- A foundation copy with its own maintenance flag; the shared app is untouched.
      since <- currentTimeMicros
      maintenance <- newTVarIO $ Just $ Maintenance since Nothing
      for_ [("PUT", body), ("DELETE", "")] \(method, payload) -> do
        (status, response) <- routeRequest app {appMaintenance = maintenance} method (deckPath deckId "/overlay") (ownerToken : json) payload
        (status, decode @(Map.Map Text Value) response >>= Map.lookup "maintenance") `shouldBe` (status503, Just (Bool True))
        snapshot `shouldReturn` original
      -- The same owner request is accepted once routed normally.
      saved <- expectSaved connection deckId =<< route "PUT" "/overlay" (ownerToken : json) body
      arkhamDeckOverlay saved `shouldBe` Just replacement
      arkhamDeckOverlayAttachmentId saved `shouldSatisfy` maybe False (/= seeded)

  for_ [False, True] \public ->
    it ("keeps an actual overlay PUT's attachment id through a later provenance-free "
      <> (if public then "public" else "linked") <> " sync") $
      withConnection harness \connection -> do
        now <- currentTimeMicros
        let label = if public then "public-attachment-kept" else "linked-attachment-kept"
            overlay = DeckOverlay Nothing mempty (Map.singleton "01008" 1) mempty
            incoming = remote {deck = remote.deck {slots = Map.singleton "01016" 1}}
            initialClient = hoistArkhamDBClient liftIO $ transport {loadDeck = \_ _ -> pure $ Right remote}
            syncingClient = hoistArkhamDBClient liftIO $ transport {loadDeck = \_ _ -> pure $ Right incoming}
        uid <- seedUser connection label
        encrypted <- encryptedAccount settings now uid $ tokens now
        void $ seedAccount app connection now uid label encrypted
        Entity deckId _ <- either (fail . show) pure =<< runStoreActionsWith validatePlayableDecklist app connection
          (\store -> runExceptT $ importRemoteDeck store initialClient settings now uid "123" forbiddenPublic)
        attached <- expectSaved connection deckId =<< overlayRoute app uid deckId (Just $ toStrict $ encode overlay)
        attachment <- maybe (fail "Expected the PUT to assign an attachment id") pure $
          arkhamDeckOverlayAttachmentId attached
        (expected, synced) <- if public
          then publicSyncRoute app connection uid deckId incoming.deck <&> \(deckUrl, saved) ->
            (toJSON incoming.deck {Decklist.url = Just deckUrl}, saved)
          else do
            result <- runStoreActionsWith validatePlayableDecklist app connection \store ->
              runExceptT $ syncRemoteDeck store syncingClient settings now uid deckId forbiddenPublic
            (toJSON $ remoteDecklist settings incoming,) . P.entityVal <$> either (fail . show) pure result
        persisted <- transaction connection (P.getJust deckId)
        toJSON (arkhamDeckList persisted) `shouldBe` expected
        arkhamDeckOverlay persisted `shouldBe` Just overlay
        arkhamDeckOverlayAttachmentId persisted `shouldBe` Just attachment
        arkhamDeckOverlayAttachmentId synced `shouldBe` Just attachment

  for_ [False, True] \public ->
    it ("consumes incorporated overlay provenance through actual " <> (if public then "public" else "linked") <> " sync") $
      withConnection harness \connection -> do
        now <- currentTimeMicros
        let label = if public then "public-provenance" else "linked-provenance"
            overlay = DeckOverlay Nothing mempty (Map.singleton "01007" 1) mempty
            attachment = UUID.fromWords 0 0 0 1
            incoming = remote {deck = withExportedAttachment (Just attachment) (Just overlay) $
              remote.deck {slots = Map.singleton "01007" 1, meta = Just "{\"ahn_push\":\"fixture-push\"}"}}
            initialClient = hoistArkhamDBClient liftIO $ transport {loadDeck = \_ _ -> pure $ Right remote}
            syncingClient = hoistArkhamDBClient liftIO $ transport {loadDeck = \_ _ -> pure $ Right incoming}
        uid <- seedUser connection label
        encrypted <- encryptedAccount settings now uid $ tokens now
        void $ seedAccount app connection now uid label encrypted
        Entity deckId _ <- either (fail . show) pure =<< runStoreActionsWith validatePlayableDecklist app connection
          (\store -> runExceptT $ importRemoteDeck store initialClient settings now uid "123" forbiddenPublic)
        transaction connection $ P.update deckId
          [ArkhamDeckOverlay P.=. Just overlay, ArkhamDeckOverlayAttachmentId P.=. Just attachment]
        saved <- if public
          then snd <$> publicSyncRoute app connection uid deckId incoming.deck
          else do
            result <- runStoreActionsWith validatePlayableDecklist app connection \store ->
              runExceptT $ syncRemoteDeck store syncingClient settings now uid deckId forbiddenPublic
            P.entityVal <$> either (fail . show) pure result
        persisted <- transaction connection (P.getJust deckId)
        arkhamDeckOverlay persisted `shouldBe` Nothing
        arkhamDeckOverlayAttachmentId persisted `shouldBe` Nothing
        slots (arkhamDeckPlayList persisted) `shouldBe` Map.singleton "01007" 1
        toJSON persisted `shouldBe` toJSON saved

  it "refuses public deck sync through WAI without a token, for another user, without a url, or off-route" $
    withConnection harness \connection -> do
      owner <- seedUser connection "public-sync-route-owner"
      stranger <- seedUser connection "public-sync-route-stranger"
      let deck = ArkhamDeck owner (Just "http://127.0.0.1:9/decklist.json") "Roland Banks" "Public sync routes"
            remote.deck Nothing Nothing Nothing
          snapshot deckId = transaction connection (P.getJust deckId) <&> \d -> (toJSON d, arkhamDeckOverlayAttachmentId d)
          errorBody message = Just $ object ["errorMsg" .= (message :: Text)]
      deckId <- transaction connection $ P.insert deck
      unlinked <- transaction connection $ P.insert deck {arkhamDeckUrl = Nothing}
      ownerToken <- authorizationFor app owner
      strangerToken <- authorizationFor app stranger
      original <- snapshot deckId
      originalUnlinked <- snapshot unlinked
      -- Every refusal happens before any fetch: the url's discard port is never dialled.
      for_ [([], deckId, "/sync", status401), ([("Authorization", "Token not-a-jwt")], deckId, "/sync", status401), ([ownerToken], deckId, "/syncs", status404)]
        \(headers, target, path, expected) -> do
          (status, _) <- routeRequest app "POST" (deckPath target path) headers ""
          (path, status) `shouldBe` (path, expected)
      (getStatus, _) <- routeRequest app "GET" (deckPath deckId "/sync") [ownerToken] ""
      getStatus `shouldBe` status405
      (foreignStatus, foreignBody) <- routeRequest app "POST" (deckPath deckId "/sync") [strangerToken] ""
      (foreignStatus, decode @Value foreignBody) `shouldBe` (status400, errorBody "Deck does not belong to this user")
      (urlStatus, urlBody) <- routeRequest app "POST" (deckPath unlinked "/sync") [ownerToken] ""
      (urlStatus, decode @Value urlBody) `shouldBe` (status400, errorBody
        "This deck was not imported from a url, so there is nothing to sync from. Point it at one first.")
      snapshot deckId `shouldReturn` original
      snapshot unlinked `shouldReturn` originalUnlinked

  it "rejects overlay removal that would expose unsupported base cards without changing the saved deck" $
    withConnection harness \connection -> do
      uid <- seedUser connection "invalid-overlay-removal"
      let hidden = DeckOverlay Nothing mempty mempty $ Map.singleton "unimplemented-card" 1
          deck = ArkhamDeck uid Nothing "Roland Banks" "Hidden unsupported card"
            (remote.deck {slots = Map.singleton "unimplemented-card" 1}) (Just hidden) Nothing Nothing
      deckId <- transaction connection $ P.insert deck
      result <- runHandlerResponse app $ runSqlConn (saveDeckOverlay uid deckId Nothing) connection
      either Just (const Nothing) result `shouldBe` Just status400
      persisted <- transaction connection (P.getJust deckId)
      toJSON persisted `shouldBe` toJSON deck

  for_ [False, True] \unlink ->
    for_ ["unstarted", "uncertain-ready", "creating", "created", "acknowledged"] \checkpoint ->
      it ("retains production " <> toString checkpoint <> " accounting across "
        <> if unlink then "unlink/relink" else "callback generation replacement") $
        withConnection harness \connection -> do
          now <- currentTimeMicros
          let label = "accounting-" <> checkpoint <> if unlink then "-unlink" else "-replace"
              client = hoistArkhamDBClient liftIO $ transport {loadDeck = \_ _ -> pure $ Right remote}
          uid <- seedUser connection label
          encrypted <- encryptedAccount settings now uid $ tokens now
          generation <- seedAccount app connection now uid label encrypted
          Entity deckId _ <- either (fail . show) pure =<< runStoreActions app connection
            (\store -> runExceptT $ importRemoteDeck store client settings now uid "123" forbiddenPublic)
          unsafeHandler app (writeBackStore.toggleWriteBack now uid deckId True) `shouldReturn` True
          gameKey <- WriteBackSQL.seedCampaignGame connection label now
          let gameId = UUID.toText $ coerce gameKey
              campaign = gameId <> "/01001"
              event = gameId <> "/5/01001/" <> gameId
          pushId <- transaction connection do
            P.insert_ $ ArkhamDBCampaignDeck campaign deckId uid 0
            P.insert $ ArkhamDBPush deckId uid generation event campaign (encodePushDeck remote.deck) 5
              "pending" "ready" Nothing Nothing remote.dateUpdate remote.deckVersion
              Nothing Nothing Nothing "normal" now now "scenarioEnd"
          previous <- if checkpoint == "unstarted" then transaction connection $ P.getJustEntity pushId else do
            claimed <- unsafeHandler app (writeBackStore.claimPush now False uid deckId)
              >>= maybe (fail "Expected real write-back claim") pure
            let advance old stage target status = do
                  let updated = (P.entityVal old)
                        { arkhamDBPushStage = stage, arkhamDBPushTargetId = target
                        , arkhamDBPushStatus = status
                        , arkhamDBPushError = if status == "failed" then Just "remoteUnavailable" else Nothing
                        }
                  unsafeHandler app (writeBackStore.checkpointPush old updated) `shouldReturn` True
                  pure $ Entity pushId updated
            case checkpoint of
              "uncertain-ready" -> pure claimed
              "creating" -> advance claimed "creating" Nothing "failed"
              "created" -> do
                creating <- advance claimed "creating" Nothing "running"
                advance creating "created" (Just "456") "failed"
              "acknowledged" -> do
                unsafeHandler app (writeBackStore.finishPush claimed remote) `shouldReturn` Just deckId
                transaction connection $ P.getJustEntity pushId
              _ -> fail "Unknown checkpoint fixture"
          when unlink $ runStoreActions app connection \store ->
            either (liftIO . fail . show) pure =<< runExceptT (unlinkLinkedAccount store uid)
          let later = addUTCTime 120 now
          fresh <- encryptedAccount settings later uid $ (tokens later) {accessToken = "fresh-generation-access"}
          newGeneration <- seedAccount app connection later uid (label <> "-new") fresh
          newGeneration `shouldNotBe` generation
          transaction connection (isNothing <$> P.get generation) `shouldReturn` True
          transaction connection (P.count [ArkhamDBLinkStateUserId P.==. uid]) `shouldReturn` 0
          let fenced = checkpoint `elem` ["uncertain-ready", "creating", "created"]
              expectedStatus = if fenced then "conflict" else if checkpoint == "unstarted" then "cancelled" else "succeeded"
              baseline = if checkpoint == "acknowledged" then 5 else 0
              assertAccounting = do
                retained <- transaction connection $ P.getJust pushId
                arkhamDBPushStatus retained `shouldBe` expectedStatus
                arkhamDBPushError retained `shouldBe` if fenced then Just "accountChanged" else Nothing
                arkhamDBPushGeneration retained `shouldBe` generation
                arkhamDBPushTargetId retained `shouldBe` arkhamDBPushTargetId (P.entityVal previous)
                unsafeHandler app (writeBackStore.campaignXpBaseline campaign) `shouldReturn` baseline
          assertAccounting
          when fenced do
            -- A fresh generation's older-timestamped work in another campaign
            -- must not hide the durable uncertainty or become runnable.
            transaction connection $ P.insert_ $ ArkhamDBPush deckId uid newGeneration
              (event <> "-other") (campaign <> "-other") (encodePushDeck remote.deck) 5
              "pending" "ready" Nothing Nothing remote.dateUpdate remote.deckVersion
              Nothing Nothing Nothing "normal" (addUTCTime (-1) now) later "scenarioEnd"
            for_ [False, True] \retry ->
              unsafeHandler app (isNothing <$> writeBackStore.claimPush later retry uid deckId) `shouldReturn` True
            unsafeHandler app (writeBackStore.requestPushRetry later uid deckId)
            for_ ["keep", "overwrite", "new"] \mode ->
              unsafeHandler app (writeBackStore.resolvePush later uid deckId mode) `shouldReturn` False
            unsafeHandler app (writeBackStore.keepRemoteDeck previous remote) `shouldReturn` False
            unsafeHandler app (writeBackStore.finishPush previous remote) `shouldReturn` Nothing
            unsafeHandler app (writeBackStore.checkpointPush previous $ P.entityVal previous) `shouldReturn` False
            for_ [False, True] \enabled -> do
              unsafeHandler app (writeBackStore.toggleWriteBack later uid deckId enabled) `shouldReturn` not unlink
              assertAccounting
            for_ [4, 0] \restoredStep -> do
              transaction connection $ WriteBackSQL.undoCampaignWriteBack gameKey restoredStep
              assertAccounting
            unless unlink do
              statuses <- unsafeHandler app $ writeBackStore.deckWriteBackStatus uid
              statuses `shouldSatisfy` (not . null)
              -- Exact production encoder, not a separately modelled status.
              show (toJSON statuses) `shouldContain` "accountChanged"

  it "purges expired nonces and staged credentials in bounded batches without changing the account" $
    withConnection harness \connection -> do
      now <- currentTimeMicros
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
      now <- currentTimeMicros
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
      now <- currentTimeMicros
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

-- The production handler boundary: each store action commits like runDB, and
-- runArkhamDB maps failures (including the 55P03 lock timeout) to a JSON error.
-- An error is returned as its status, decoded JSON body and raw body text.
refreshResponse :: App -> SqlBackend -> ArkhamDBClient Handler -> UTCTime -> ArkhamDBAccount -> IO (Either (Status, Maybe Value, Text) Text)
refreshResponse app connection client now previous = do
  result <- runHandlerResponseBody app $ runArkhamDB $
    refreshAccount (persistentStoreWith (const $ pure ()) (`runSqlConn` connection)) client settings now previous
  pure $ either (\(status, body) -> Left (status, decode body, decodeUtf8 body)) Right result

-- A recognizable secret carried by the fake remote error.
leakMarker :: Text
leakMarker = "LEAK-MARKER-55"

-- The complete sanitized envelope (ToJSON ArkhamDBError: exactly code and
-- error, with the fixed production message), and no upstream error text,
-- fixture credential or SQL detail anywhere in the raw body.
expectSanitizedError :: Status -> Text -> Text -> Either (Status, Maybe Value, Text) Text -> IO ()
expectSanitizedError status code message result = case result of
  Right value -> expectationFailure $ "Expected an error response, got " <> show value
  Left (actual, body, raw) -> do
    (actual, body) `shouldBe` (status, Just $ object ["code" .= code, "error" .= message])
    for_ [leakMarker, "fixture-refresh", "fixture-access", "lock timeout", "55P03"] \secret ->
      (secret, secret `T.isInfixOf` raw) `shouldBe` (secret, False)

-- The session's effective lock_timeout, read in its own transaction.
lockTimeout :: SqlBackend -> IO Text
lockTimeout connection = transaction connection currentLockTimeout

currentLockTimeout :: ReaderT SqlBackend IO Text
currentLockTimeout = rawSql "SELECT current_setting('lock_timeout')" [] >>= \case
  [Single value] -> pure value
  _ -> liftIO $ fail "Expected one lock_timeout setting"

-- Exact persisted rows: credentials and generation id, and every deck mapping.
linkedRows :: SqlBackend -> UserId -> IO ([Text], [Text])
linkedRows connection uid = transaction connection do
  accounts <- rawSql "SELECT row_to_json(a)::text FROM arkhamdb_accounts AS a WHERE a.user_id = ?" [toPersistValue uid]
  mappings <- rawSql "SELECT row_to_json(l)::text FROM arkhamdb_deck_links AS l WHERE l.user_id = ? ORDER BY l.id" [toPersistValue uid]
  pure (map unSingle accounts, map unSingle mappings)

-- A locked refresh that fails after taking the lock (remote error, remote
-- rejection, or no stored refresh token) must leave every persisted row intact.
refreshFailureCase :: Harness -> App -> Text -> Maybe Text -> Maybe ArkhamDBClientError -> Status -> Text -> Text -> IO ()
refreshFailureCase harness app label credential outcome status code message =
  withConnection harness \connection -> do
    now <- currentTimeMicros
    uid <- seedUser connection label
    previous <- encryptedAccount settings now uid (tokens now) {refreshToken = credential}
    generation <- seedAccount app connection now uid label previous
    let importer = hoistArkhamDBClient liftIO $ transport {loadDeck = \_ _ -> pure $ Right remote}
    void $ either (fail . show) pure =<< runStoreActions app connection
      (\store -> runExceptT $ importRemoteDeck store importer settings now uid "123" forbiddenPublic)
    stored <- runStore app connection (\store -> store.findAccount uid) >>= maybe (fail "Expected linked account") pure
    persisted <- linkedRows connection uid
    bimap length length persisted `shouldBe` (1, 1)
    calls <- newIORef (0 :: Int)
    let client = hoistArkhamDBClient liftIO $ transport
          { refreshTokens = \sent -> do
              sent `shouldBe` "fixture-refresh"
              modifyIORef' calls (+ 1)
              maybe (fail "A missing refresh token reached ArkhamDB") (pure . Left) outcome
          }
    within "failed refresh" (refreshResponse app connection client now stored) >>= expectSanitizedError status code message
    readIORef calls `shouldReturn` (if isJust outcome then 1 else 0)
    linkedRows connection uid `shouldReturn` persisted
    runStore app connection (\store -> store.findAccountGeneration uid) `shouldReturn` Just generation

-- A linked sync whose expired token cannot be refreshed must surface the
-- sanitized refresh error through runArkhamDB: never the notFound path, so the
-- mapping is kept and the public fallback is not used. Store actions commit
-- one at a time (as runDB does), so nothing is hidden by an outer rollback.
syncRefreshFailureCase :: Harness -> App -> Text -> Maybe Text -> Maybe ArkhamDBClientError -> Status -> Text -> Text -> IO ()
syncRefreshFailureCase harness app label credential outcome status code message =
  withConnection harness \connection -> withConnection harness \observer -> do
    now <- currentTimeMicros
    uid <- seedUser connection label
    previous <- encryptedAccount settings now uid (tokens now) {refreshToken = credential}
    generation <- seedAccount app connection now uid label previous
    let importer = hoistArkhamDBClient liftIO $ transport {loadDeck = \_ _ -> pure $ Right remote}
    imported@(Entity deckId _) <- either (fail . show) pure =<< runStoreActions app connection
      (\store -> runExceptT $ importRemoteDeck store importer settings now uid "123" forbiddenPublic)
    valid <- runStore app observer (\store -> store.findAccount uid) >>= maybe (fail "Expected linked account") pure
    runStore app connection (\store -> store.updateAccount valid valid {arkhamDBAccountExpiresAt = Just $ addUTCTime (-60) now})
      `shouldReturn` True
    stored <- runStore app observer (\store -> store.findAccount uid) >>= maybe (fail "Expected linked account") pure
    arkhamDBAccountExpiresAt stored `shouldBe` Just (addUTCTime (-60) now)
    isJust (arkhamDBAccountEncryptedRefreshToken stored) `shouldBe` isJust credential
    let deckRow = transaction observer
          (rawSql "SELECT row_to_json(d)::text FROM arkham_decks AS d WHERE d.user_id = ? ORDER BY d.id" [toPersistValue uid])
            <&> map unSingle :: IO [Text]
        linkStates = transaction observer
          (rawSql "SELECT count(*) FROM arkhamdb_link_states WHERE user_id = ?" [toPersistValue uid])
            <&> map unSingle :: IO [Int64]
    persisted <- linkedRows observer uid
    bimap length length persisted `shouldBe` (1, 1)
    decks <- deckRow
    length decks `shouldBe` 1
    linkStates `shouldReturn` [0]
    counts <- deckCounts observer uid
    counts `shouldBe` (1, 1)
    refreshes <- newIORef (0 :: Int)
    loads <- newIORef (0 :: Int)
    fallbacks <- MVar.newMVar (0 :: Int)
    let client = hoistArkhamDBClient liftIO $ transport
          { refreshTokens = \sent -> do
              sent `shouldBe` "fixture-refresh"
              modifyIORef' refreshes (+ 1)
              -- Never ArkhamDBNotFound: that is the legitimate drop-and-fallback path.
              maybe (fail "A missing refresh token reached ArkhamDB") (pure . Left) outcome
          , loadDeck = \_ _ -> do
              modifyIORef' loads (+ 1)
              pure $ Left $ ArkhamDBRemoteError "Unexpected remote load after a failed refresh"
          }
        -- Counts and returns the deck so a fallback regression reaches the
        -- row and count assertions below instead of failing inside the handler.
        public _ = do
          liftIO $ MVar.modifyMVar_ fallbacks $ pure . (+ 1)
          pure imported
    result <- within "linked sync after a failed refresh" $ runHandlerResponseBody app $ runArkhamDB $
      syncRemoteDeck (persistentStoreWith (const $ pure ()) (`runSqlConn` connection)) client settings now uid deckId public
    readIORef refreshes `shouldReturn` (if isJust outcome then 1 else 0)
    readIORef loads `shouldReturn` 0
    linkedRows observer uid `shouldReturn` persisted
    deckRow `shouldReturn` decks
    runStore app observer (\store -> store.findAccountGeneration uid) `shouldReturn` Just generation
    linkStates `shouldReturn` [0]
    deckCounts observer uid `shouldReturn` counts
    MVar.readMVar fallbacks `shouldReturn` 0
    expectSanitizedError status code message $
      either (\(actual, body) -> Left (actual, decode body, decodeUtf8 body)) (Right . show . entityKey) result
    -- The refresh lock's transaction ended: another session takes it at once.
    -- runSqlConn normally ends it; this guards against a leaked open transaction or held user-row lock from the sync path.
    locked <- transaction observer
      (rawSql "SELECT id FROM users WHERE id = ? FOR NO KEY UPDATE NOWAIT" [toPersistValue uid] :: ReaderT SqlBackend IO [Single Int64])
    length locked `shouldBe` 1

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

-- The JWT-authenticated overlay PUT (with a JSON body) or DELETE request,
-- dispatched by the production WAI application rather than calling a handler.
overlayRoute :: App -> UserId -> ArkhamDeckId -> Maybe ByteString -> IO (Status, LByteString)
overlayRoute app uid deckId body = do
  authorization <- authorizationFor app uid
  routeRequest app (maybe "DELETE" (const "PUT") body) (deckPath deckId "/overlay")
    (authorization : [("Content-Type", "application/json") | isJust body]) (maybe "" toLazy body)

-- The JWT-authenticated public deck sync (POST /decks/<id>/sync) through the
-- production WAI application. The deck's stored url is pointed at a loopback
-- HTTP fixture serving the decklist, so the handler's real fetch (getDeckList
-- -> simpleHttp for a non-arkham.build url) reads it; exactly one GET must
-- arrive. Returns the fetch url and the committed row.
publicSyncRoute :: App -> SqlBackend -> UserId -> ArkhamDeckId -> ArkhamDBDecklist -> IO (Text, ArkhamDeck)
publicSyncRoute app connection uid deckId served = do
  -- http-client would send a plain-http fetch through an ambient proxy.
  proxies <- traverse (\name -> (name,) <$> Environment.lookupEnv name) ["http_proxy", "HTTP_PROXY"]
  filter (isJust . snd) proxies `shouldBe` []
  fetches <- newIORef []
  let fixture request respond = do
        atomicModifyIORef' fetches \seen -> (seen <> [(requestMethod request, rawPathInfo request)], ())
        respond $ responseLBS status200 [("Content-Type", "application/json")] $ encode served
  Warp.testWithApplication (pure fixture) \port -> do
    let deckUrl = "http://127.0.0.1:" <> show port <> "/decklist.json"
    transaction connection $ P.update deckId [ArkhamDeckUrl P.=. Just deckUrl]
    authorization <- authorizationFor app uid
    saved <- expectSaved connection deckId =<< routeRequest app "POST" (deckPath deckId "/sync") [authorization] ""
    readIORef fetches `shouldReturn` [("GET", "/decklist.json")]
    arkhamDeckUrl saved `shouldBe` Just deckUrl
    pure (deckUrl, saved)

-- A 200 response whose JSON is exactly the committed row (ToJSON omits the
-- private attachment id, so callers read that from the returned row).
expectSaved :: SqlBackend -> ArkhamDeckId -> (Status, LByteString) -> IO ArkhamDeck
expectSaved connection deckId (status, body) = do
  persisted <- transaction connection (P.getJust deckId)
  (status, decode @Value body) `shouldBe` (status200, Just $ toJSON $ Entity deckId persisted)
  pure persisted
