{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE NoFieldSelectors #-}
{-# LANGUAGE OverloadedStrings #-}

module Postgres.ConcurrencySpec (spec) where

import Api.Arkham.ArkhamDBStore (persistentStore, persistentStoreWith)
import Api.Arkham.ArkhamDBWriteBack (writeBackStore, writeBackStoreWith)
import Api.Handler.Arkham.Games.Shared (updateGameWithAnswerRejection)
import Arkham.ArkhamDB
import Arkham.ArkhamDB.Service
import Arkham.ArkhamDB.WriteBack
import Control.Concurrent.MVar qualified as MVar
import Data.Text qualified as Text
import Data.Time.Clock (addUTCTime, diffUTCTime)
import Database.Persist (toPersistValue)
import Database.Persist qualified as P
import Database.Persist.Sql (Single, SqlBackend, rawSql, runSqlConn)
import Entity.Arkham.ArkhamDBAccount
import Entity.Arkham.Game
import Foundation (App, unsafeHandler)
import Postgres.DispatchSpec (prepareAnswer, answer)
import Postgres.Harness
import Postgres.Production
import Postgres.Remote
import Postgres.WriteBackSpec (Fixture (..), Seed, fixture, queue, runWrites)
import Relude hiding (first, second)
import Settings (ArkhamDBSettings)
import Test.Hspec
import UnliftIO.Async (link, wait, withAsync)

spec :: Harness -> App -> ArkhamDBSettings -> ArkhamDBClient IO -> ArkhamDBRemoteDeck -> Seed -> Spec
spec harness app settings unexpected source seed = describe "Actual answer versus remote push/resolve/unlink" do
  for_ [False, True] \unlink ->
    it (if unlink then "retires a prepared in-flight creation under the real user lock without undoing an answer"
        else "rejects a competing push/resolve while the real answer commits independently") $
      withConnection harness \first -> withConnection harness \second -> withConnection harness \observer -> do
        f <- fixture seed app first $ if unlink then "push-unlink-answer" else "push-resolve-answer"
        transaction first $ P.update f.gameId [ArkhamGameCurrentData P.=. prepareAnswer f]
        queue f first 1
        P.Entity key _ <- storedJob observer f
        remote <- newRemote unexpected source
        network <- newBarrier
        adoption <- newBarrier
        let client = remote.client
              { upgradeDeck = \token target earned metadata -> do
                  result <- remote.client.upgradeDeck token target earned metadata
                  -- This callback still owns the production refresh/request
                  -- user lock on 'first', not an unrelated fixture lock.
                  pauseAt network
                  pure result
              }
            store = persistentStoreWith (const $ pure ()) (`runSqlConn` first)
            writes = writeBackStore
              { checkpointPush = \previous proposed -> do
                  -- Unlink must commit before the already-returned remote id's
                  -- adoption CAS, proving its durable accountChanged fence.
                  when (unlink && arkhamDBPushStage proposed == "created") $ liftIO $ pauseAt adoption
                  writeBackStore.checkpointPush previous proposed
              }
        firstPid <- backendPid first
        secondPid <- backendPid second
        withAsync (unsafeHandler app $ runWriteBack store writes (hoistArkhamDBClient liftIO client)
          settings f.now False f.uid f.deckId) \pushing -> do
            link pushing
            awaitBarrier network
            unsafeHandler app (isNothing <$> writeBackStore.claimPush (addUTCTime 120 f.now) True f.uid f.deckId) `shouldReturn` True
            within "answer is independent of held remote user lock" $
              unsafeHandler app (updateGameWithAnswerRejection mempty answer f.gameId Nothing) `shouldReturn` Nothing
            committed <- transaction observer $ P.getJust f.gameId
            arkhamGameStep committed `shouldBe` 3
            transaction observer (P.count [ArkhamDBPushDeckId P.==. f.deckId]) `shouldReturn` 2
            let competing = if unlink
                  then runStoreActions app second (\locked -> locked.unlinkAccount f.uid) >> pure False
                  else unsafeHandler app $
                    (writeBackStoreWith (`runSqlConn` second)).resolvePush f.now f.uid f.deckId "overwrite"
            withAsync competing \changing -> do
              link changing
              awaitBlockedBy observer firstPid secondPid
              releaseBarrier network
              when unlink $ awaitBarrier adoption
              within "serialized lifecycle operation" (wait changing) `shouldReturn` False
              when unlink $ releaseBarrier adoption
              within "push respects lifecycle result" $ wait pushing
        job <- transaction observer $ P.getJust key
        arkhamDBPushStatus job `shouldBe` if unlink then "conflict" else "succeeded"
        when unlink do
          arkhamDBPushError job `shouldBe` Just "accountChanged"
          decodePushDeckPayload (arkhamDBPushPayload job) `shouldSatisfy` either (const False) fst
          for_ ["keep", "new", "overwrite"] \mode ->
            unsafeHandler app (writeBackStore.resolvePush f.now f.uid f.deckId mode) `shouldReturn` False
          push app settings remote.client f True 240
          current <- transaction observer $ P.getJust key
          arkhamDBPushPayload current `shouldBe` arkhamDBPushPayload job
          arkhamDBPushError current `shouldBe` Just "accountChanged"
          unsafeHandler app (writeBackStore.campaignXpBaseline f.campaign) `shouldReturn` 0
        filter (Text.isPrefixOf "upgrade/") <$> MVar.readMVar remote.calls `shouldReturn` ["upgrade/123/5"]
        arkhamGameStep <$> transaction observer (P.getJust f.gameId) `shouldReturn` 3

  it "records a real answer's push while a linked sync holds its uncommitted link UPDATE" $
    withConnection harness \syncing -> withConnection harness \observer -> do
      f <- fixture seed app observer "linked-sync-answer"
      transaction observer $ P.update f.gameId [ArkhamGameCurrentData P.=. prepareAnswer f]
      preSync <- transaction observer (P.getBy $ UniqueArkhamDBDeckLinkDeck f.deckId)
        >>= maybe (fail "Expected linked deck") (pure . arkhamDBDeckLinkLastSyncedDateUpdate . P.entityVal)
      preSync `shouldNotBe` Just "synced"
      remote <- newRemote unexpected source
      editRemote remote source.remoteDeckId \old -> old {dateUpdate = Just "synced"}
      now <- currentTimeMicros
      barrier <- newBarrier
      let pendingPushes = transaction observer $
            P.count [ArkhamDBPushDeckId P.==. f.deckId, ArkhamDBPushStatus P.==. "pending"]
      -- Pause after saveSyncedDeck's deck and link UPDATEs, before commit.
      withAsync (unsafeHandler app $ storeTransaction (const $ pure ()) syncing \store -> do
        result <- runExceptT $ syncRemoteDeck store (hoistArkhamDBClient liftIO remote.client) settings now f.uid f.deckId
          (\_ -> liftIO $ fail "Unexpected public fallback")
        liftIO $ pauseAt barrier
        pure result) \synchronizing -> do
          link synchronizing
          awaitBarrier barrier
          -- The uncommitted link UPDATE is held: a FOR SHARE reader would skip it.
          transaction observer (rawSql "SELECT id FROM arkhamdb_deck_links WHERE deck_id = ? FOR SHARE SKIP LOCKED"
            [toPersistValue f.deckId] :: ReaderT SqlBackend IO [Single ArkhamDBDeckLinkId]) `shouldReturn` []
          within "answer beside paused linked sync" $
            unsafeHandler app (updateGameWithAnswerRejection mempty answer f.gameId Nothing) `shouldReturn` Nothing
          pendingPushes `shouldReturn` 1
          releaseBarrier barrier
          result <- within "linked sync commit" $ wait synchronizing
          void result `shouldBe` Right ()
      pendingPushes `shouldReturn` 1
      synced <- transaction observer (P.getBy $ UniqueArkhamDBDeckLinkDeck f.deckId)
        >>= maybe (fail "Expected linked deck") (pure . P.entityVal)
      arkhamDBDeckLinkLastSyncedDateUpdate synced `shouldBe` Just "synced"
      P.Entity _ job <- storedJob observer f
      arkhamDBPushExpectedDateUpdate job `shouldBe` preSync

  it "records the owner's real answer push while another user's toggle names the deck" $
    withConnection harness \toggling -> withConnection harness \observer -> do
      f <- fixture seed app observer "foreign-toggle-answer"
      (stranger, _, _) <- seed observer "foreign-toggle-stranger"
      transaction observer $ P.update f.gameId [ArkhamGameCurrentData P.=. prepareAnswer f]
      barrier <- newBarrier
      let pendingPushes = transaction observer $
            P.count [ArkhamDBPushDeckId P.==. f.deckId, ArkhamDBPushStatus P.==. "pending"]
      -- The deck id comes from the URL; a non-owner's toggle changes nothing
      -- and must lock nothing on the owner's link while it stays open.
      withAsync (runWrites app toggling \store -> do
        changed <- store.toggleWriteBack f.now stranger f.deckId False
        liftIO $ pauseAt barrier
        pure changed) \strangerToggle -> do
          link strangerToggle
          awaitBarrier barrier
          within "answer beside a non-owner toggle" $
            unsafeHandler app (updateGameWithAnswerRejection mempty answer f.gameId Nothing) `shouldReturn` Nothing
          pendingPushes `shouldReturn` 1
          releaseBarrier barrier
          within "non-owner toggle commit" (wait strangerToggle) `shouldReturn` False
      pendingPushes `shouldReturn` 1
      transaction observer (fmap (arkhamDBDeckLinkWriteBack . P.entityVal) <$> P.getBy (UniqueArkhamDBDeckLinkDeck f.deckId))
        `shouldReturn` Just True

  it "renews a healthy running push's lease past 60 seconds so another dispatcher cannot steal it" $
    withConnection harness \observer -> do
      f <- fixture seed app observer "healthy-lease-renewal"
      queue f observer 1
      P.Entity key _ <- storedJob observer f
      remote <- newRemote unexpected source
      transported <- newIORef False
      renewed <- newIORef False
      paused <- newIORef False
      outside <- newBarrier
      let beforeTransport = addUTCTime 10 f.now
          -- The production clock is injected: the remote read took 80 s, so
          -- the claim (f.now) and its last pre-request write are both stale.
          renewedLease = addUTCTime 90 f.now
          competing = addUTCTime 120 f.now
          client = remote.client
            { loadDeck = \token remoteId -> do
                result <- remote.client.loadDeck token remoteId
                writeIORef transported True
                pure result
            }
          clock = liftIO do
            afterTransport <- readIORef transported
            if not afterTransport then pure beforeTransport else do
              -- The first post-transport read is the request's own renewal
              -- under the user lock. Pause on the next one: the following
              -- checkpoint, outside every transaction.
              wasRenewed <- atomicModifyIORef' renewed \old -> (True, old)
              when wasRenewed do
                wasPaused <- atomicModifyIORef' paused \old -> (True, old)
                unless wasPaused $ pauseAt outside
              pure renewedLease
          userAvailable = transaction observer
            (rawSql "SELECT id FROM users WHERE id = ? FOR NO KEY UPDATE SKIP LOCKED" [toPersistValue f.uid]
              :: ReaderT SqlBackend IO [Single Int64])
      withAsync (unsafeHandler app $ runWriteBackWithClock clock (persistentStore $ const $ pure ()) writeBackStore
        (hoistArkhamDBClient liftIO client) settings f.now False f.uid f.deckId) \pushing -> do
          link pushing
          awaitBarrier outside
          -- No user lock gates a competitor here; only the lease can.
          length <$> userAvailable `shouldReturn` 1
          running <- transaction observer $ P.getJust key
          calls <- MVar.readMVar remote.calls
          calls `shouldBe` ["load/123"]
          -- 120 s after the claim and 110 s after its last pre-request write:
          -- stale without the post-request renewal, still leased with it.
          for_ [False, True] \retry ->
            unsafeHandler app (fmap P.entityKey <$> writeBackStore.claimPush competing retry f.uid f.deckId)
              `shouldReturn` Nothing
          for_ [False, True] \retry -> push app settings remote.client f retry 120
          current <- transaction observer $ P.getJust key
          show current `shouldBe` (show running :: String)
          MVar.readMVar remote.calls `shouldReturn` calls
          arkhamDBPushStatus running `shouldBe` "running"
          arkhamDBPushUpdatedAt running `shouldSatisfy` \at -> abs (diffUTCTime at renewedLease) < 0.001
          releaseBarrier outside
          within "healthy renewed push" $ wait pushing
      job <- transaction observer $ P.getJust key
      arkhamDBPushStatus job `shouldBe` "succeeded"
      filter (Text.isPrefixOf "upgrade/") <$> MVar.readMVar remote.calls `shouldReturn` ["upgrade/123/5"]
      unsafeHandler app (writeBackStore.campaignXpBaseline f.campaign) `shouldReturn` 5
