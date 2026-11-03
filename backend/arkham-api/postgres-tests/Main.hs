{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

-- Standalone integration suite, intentionally not hspec-discovered by unit
-- tests. Invocation and stage2 package wiring are documented in the launcher.
module Main where

import Arkham.ArkhamDB
import Api.Arkham.ArkhamDBCleanup (cleanupBatchSize, purgeExpiredLinks)
import Api.Arkham.ArkhamDBWriteBack (writeBackStore)
import Api.Handler.Arkham.Decks
  ( deleteApiV1ArkhamDeckOverlayR, putApiV1ArkhamDeckOverlayR, saveDeckOverlay, savePublicSyncedDeck
  , validatePlayableDecklist )
import Arkham.ArkhamDB.WriteBack
import Arkham.ArkhamDB.Service
import Arkham.Custom.Overlay (DeckOverlay (..))
import Arkham.Decklist (ArkhamDBDecklist (..))
import Control.Concurrent.MVar qualified as MVar
import Control.Exception (throwIO)
import Data.Aeson (encode, toJSON)
import Data.Map.Strict qualified as Map
import Data.Time.Clock (UTCTime, addUTCTime, getCurrentTime)
import Data.UUID qualified as UUID
import Database.Persist (Entity (..), entityKey, toPersistValue)
import Database.Persist qualified as P
import Database.Persist.Sql (Single (..), SqlBackend, rawExecute, rawSql, runSqlConn, toSqlKey)
import Database.Persist.Types (PersistValue (..))
import Entity.Arkham.ArkhamDBAccount
import Entity.Arkham.Deck
import Entity.Arkham.Game
import Entity.User (UserId)
import Foundation (App, unsafeHandler, userIdToToken)
import Network.HTTP.Types (Status, status400)
import Network.Wai (defaultRequest, requestHeaders, requestMethod, setRequestBodyChunks)
import Postgres.Harness
import Postgres.HarnessSpec qualified as Harness
import Postgres.DispatchSpec qualified as DispatchSQL
import Postgres.PayloadSpec qualified as PayloadSQL
import Postgres.ConcurrencySpec qualified as ConcurrencySQL
import Postgres.DeleteSpec qualified as DeleteSQL
import Postgres.ThirdEditionSpec qualified as ThirdEditionSQL
import Postgres.Production
import Postgres.WriteBackSpec qualified as WriteBackSQL
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
    WriteBackSQL.spec harness app remote (writeBackSeed app)
      (\connection uid label -> do
        now <- getCurrentTime
        encrypted <- encryptedAccount settings now uid $ tokens now
        void $ seedAccount app connection now uid label encrypted
        let client = hoistArkhamDBClient liftIO $ transport {loadDeck = \_ _ -> pure $ Right remote}
        fmap entityKey $ either (fail . show) pure =<< runStoreActions app connection
          (\store -> runExceptT $ importRemoteDeck store client settings now uid "123" forbiddenPublic))
    DispatchSQL.spec harness app settings transport remote (writeBackSeed app)
    PayloadSQL.spec harness app settings transport remote (writeBackSeed app)
    ConcurrencySQL.spec harness app settings transport remote (writeBackSeed app)
    DeleteSQL.spec harness app settings transport remote (writeBackSeed app)
    ThirdEditionSQL.spec harness app

writeBackSeed :: App -> WriteBackSQL.Seed
writeBackSeed app connection label = do
  now <- getCurrentTime
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

  for_ [False, True] \markers ->
    it ("removes an unchanged 404 mapping and falls back exactly once with " <> if markers then "non-null markers" else "null markers") $
      withConnection harness \connection -> do
        now <- getCurrentTime
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
          now <- getCurrentTime
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

  for_ [False, True] \public ->
    for_ [False, True] \syncFirst ->
      for_ ["add", "replace", "remove", "invalid"] \change ->
        it ("serializes " <> (if public then "public" else "linked") <> " sync "
          <> (if syncFirst then "before" else "after") <> " overlay " <> toString change) $
          withConnection harness \first -> withConnection harness \second -> withConnection harness \observer -> do
            now <- getCurrentTime
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
      firstSaved <- either (fail . show) pure =<< overlayRoute app uid deckId (Just body)
      (firstPersisted, firstId) <- persistedAttachment deckId
      arkhamDeckOverlayAttachmentId (P.entityVal firstSaved) `shouldBe` Just firstId
      -- A second PUT of the same bytes is a NEW attachment, not a no-op.
      secondSaved <- either (fail . show) pure =<< overlayRoute app uid deckId (Just body)
      (secondPersisted, secondId) <- persistedAttachment deckId
      secondId `shouldNotBe` firstId
      arkhamDeckOverlayAttachmentId (P.entityVal secondSaved) `shouldBe` Just secondId
      toJSON secondPersisted `shouldBe` toJSON firstPersisted
      removed <- either (fail . show) pure =<< overlayRoute app uid deckId Nothing
      persisted <- transaction connection (P.getJust deckId)
      arkhamDeckOverlay persisted `shouldBe` Nothing
      arkhamDeckOverlayAttachmentId persisted `shouldBe` Nothing
      arkhamDeckOverlayAttachmentId (P.entityVal removed) `shouldBe` Nothing
      toJSON persisted `shouldBe` toJSON deck

  for_ [False, True] \public ->
    it ("keeps an actual overlay PUT's attachment id through a later provenance-free "
      <> (if public then "public" else "linked") <> " sync") $
      withConnection harness \connection -> do
        now <- getCurrentTime
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
        attached <- either (fail . show) pure =<< overlayRoute app uid deckId (Just $ toStrict $ encode overlay)
        attachment <- maybe (fail "Expected the PUT to assign an attachment id") pure $
          arkhamDeckOverlayAttachmentId (P.entityVal attached)
        result <- if public
          then unsafeHandler app $ runSqlConn (savePublicSyncedDeck uid deckId incoming.deck) connection
          else runStoreActionsWith validatePlayableDecklist app connection \store ->
            runExceptT $ syncRemoteDeck store syncingClient settings now uid deckId forbiddenPublic
        synced <- either (fail . show) pure result
        persisted <- transaction connection (P.getJust deckId)
        toJSON (arkhamDeckList persisted) `shouldBe`
          toJSON (if public then incoming.deck else remoteDecklist settings incoming)
        arkhamDeckOverlay persisted `shouldBe` Just overlay
        arkhamDeckOverlayAttachmentId persisted `shouldBe` Just attachment
        arkhamDeckOverlayAttachmentId (P.entityVal synced) `shouldBe` Just attachment

  for_ [False, True] \public ->
    it ("consumes incorporated overlay provenance through actual " <> (if public then "public" else "linked") <> " sync") $
      withConnection harness \connection -> do
        now <- getCurrentTime
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
        result <- if public
          then unsafeHandler app $ runSqlConn (savePublicSyncedDeck uid deckId incoming.deck) connection
          else runStoreActionsWith validatePlayableDecklist app connection \store ->
            runExceptT $ syncRemoteDeck store syncingClient settings now uid deckId forbiddenPublic
        saved <- either (fail . show) pure result
        persisted <- transaction connection (P.getJust deckId)
        arkhamDeckOverlay persisted `shouldBe` Nothing
        arkhamDeckOverlayAttachmentId persisted `shouldBe` Nothing
        slots (arkhamDeckPlayList persisted) `shouldBe` Map.singleton "01007" 1
        toJSON persisted `shouldBe` toJSON (P.entityVal saved)

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
          now <- getCurrentTime
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

-- The actual JWT-authenticated overlay PUT (with a JSON body) or DELETE handler,
-- not saveDeckOverlay alone. Every call builds a fresh request whose body
-- yields exactly the given bytes once, as WAI does for a real client request.
overlayRoute :: App -> UserId -> ArkhamDeckId -> Maybe ByteString -> IO (Either Status (Entity ArkhamDeck))
overlayRoute app uid deckId body = do
  token <- unsafeHandler app $ userIdToToken uid
  chunks <- newIORef $ maybeToList body
  let nextChunk = atomicModifyIORef' chunks \case
        chunk : rest -> (rest, chunk)
        [] -> ([], "")
      request = setRequestBodyChunks nextChunk defaultRequest
        { requestMethod = maybe "DELETE" (const "PUT") body
        , requestHeaders = ("Authorization", "Token " <> encodeUtf8 token)
            : [("Content-Type", "application/json") | isJust body]
        }
  runHandlerResponse app $ withRequest request $
    maybe deleteApiV1ArkhamDeckOverlayR (const putApiV1ArkhamDeckOverlayR) body deckId
