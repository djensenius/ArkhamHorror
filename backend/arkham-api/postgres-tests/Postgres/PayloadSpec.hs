{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE NoFieldSelectors #-}
{-# LANGUAGE OverloadedStrings #-}

module Postgres.PayloadSpec (spec) where

import Api.Arkham.ArkhamDBStore (persistentStore)
import Api.Arkham.ArkhamDBWriteBack (queueCampaignWriteBack, writeBackStore)
import Arkham.ArkhamDB
import Arkham.ArkhamDB qualified as ArkhamDB
import Arkham.ArkhamDB.Service
import Arkham.ArkhamDB.WriteBack
import Arkham.Decklist qualified as Deck
import Arkham.Campaign.Types (campaignDecks)
import Arkham.Classes.Entity (overAttrs)
import Arkham.Game qualified as Game
import Arkham.Helpers (Deck (..))
import Arkham.Prelude (These (This))
import Control.Concurrent.MVar qualified as MVar
import Control.Exception (throwIO)
import Data.Aeson (Value (..), encode)
import Data.Aeson.KeyMap qualified as KM
import Data.Map.Strict qualified as Map
import Data.Text qualified as Text
import Data.Time.Clock (addUTCTime)
import Database.Persist qualified as P
import Entity.Arkham.ArkhamDBAccount
import Foundation (App, unsafeHandler)
import Postgres.Harness
import Postgres.Remote
import Postgres.WriteBackSpec (Fixture (..), Seed, fixture, queue, undoCampaignWriteBack)
import Relude
import Settings (ArkhamDBSettings)
import Test.Hspec hiding (before, after)
import UnliftIO.Exception qualified as Exception

data TransportInterrupted = TransportInterrupted deriving stock (Eq, Show)
instance Exception TransportInterrupted

sync :: App -> ArkhamDBSettings -> ArkhamDBClient IO -> Fixture -> IO ()
sync app settings client f = do
  result <- unsafeHandler app $ runExceptT $ syncRemoteDeck
    (persistentStore $ const $ pure ()) (hoistArkhamDBClient liftIO client)
    settings f.now f.uid f.deckId (\_ -> liftIO $ fail "Unexpected public fallback")
  result `shouldSatisfy` isRight

preparedDeck :: Text -> IO Deck.ArkhamDBDecklist
preparedDeck bytes = case decodePushDeckPayload bytes of
  Right (True, deck) -> pure deck
  _ -> expectationFailure "Expected durable prepared envelope" >> fail "Missing preparation"

spec :: Harness -> App -> ArkhamDBSettings -> ArkhamDBClient IO -> ArkhamDBRemoteDeck -> Seed -> Spec
spec harness app settings unexpected source seed = describe "Production SQL prepared payload and restart" do
  for_ ["normal", "overwrite", "new"] \mode ->
    it ("commits creating and selected-source payload atomically before transport: " <> toString mode) $
      withConnection harness \connection -> withConnection harness \observer -> do
        f <- fixture seed app connection $ "prepared-" <> mode
        queue f connection 1
        let current = source {ArkhamDB.dateUpdate = Just "edited", deck = source.deck
              { Deck.sideSlots = Map.singleton "01009" 2
              , Deck.meta = Just "{\"extra_deck\":\"01008\",\"remote_only\":\"not-exported\"}"
              }}
        remote <- newRemote unexpected current
        let verify = do
              P.Entity _ job <- storedJob observer f
              arkhamDBPushStage job `shouldBe` "creating"
              frozen <- preparedDeck $ arkhamDBPushPayload job
              frozen.sideSlots `shouldBe` Map.singleton "01009" 2
              (KM.lookup "extra_deck" <$> pushDeckMetadata frozen) `shouldBe` Right (Just $ String "01008")
              (KM.lookup "remote_only" <$> pushDeckMetadata frozen) `shouldBe` Right Nothing
              frozen.slots `shouldBe` Map.fromList [("01006", 1), ("01100", 1), ("01007", 1)]
              frozen.taboo_id `shouldBe` Just 10
              arkhamDBPushTotalXp job `shouldBe` 5
            client = remote.client
              { upgradeDeck = \token key earned metadata -> verify >> remote.client.upgradeDeck token key earned metadata
              , createDeck = \token deck -> verify >> remote.client.createDeck token deck
              }
        if mode == "normal" then sync app settings client f else do
          push app settings client f False 0
          unsafeHandler app (writeBackStore.resolvePush (addUTCTime 121 f.now) f.uid f.deckId mode) `shouldReturn` True
        push app settings client f False 240
        P.Entity _ job <- storedJob observer f
        arkhamDBPushStatus job `shouldBe` "succeeded"
        frozen <- preparedDeck $ arkhamDBPushPayload job
        final <- readRemote remote "124"
        samePushedDeck frozen final `shouldBe` True
        when (mode == "new") $
          filter (\call -> call == "create" || Text.isPrefixOf "save/" call) <$> MVar.readMVar remote.calls
            `shouldReturn` ["create", "save/124"]
        unsafeHandler app (writeBackStore.campaignXpBaseline f.campaign) `shouldReturn` 5

  it "reclaims an interrupted running creation from committed SQL without a second XP creation" $
    withConnection harness \connection -> do
      f <- fixture seed app connection "interrupted-creation"
      queue f connection 1
      P.Entity key queued <- storedJob connection f
      (_, captured) <- either fail pure $ decodePushDeckPayload $ arkhamDBPushPayload queued
      remote <- newRemote unexpected source {deck = captured}
      let interrupted = remote.client
            {upgradeDeck = \token target earned metadata -> do
              void $ remote.client.upgradeDeck token target earned metadata
              throwIO TransportInterrupted}
      outcome <- unsafeHandler app $ Exception.try $ runWriteBack
        (persistentStore $ const $ pure ()) writeBackStore (hoistArkhamDBClient liftIO interrupted)
        settings f.now False f.uid f.deckId
      (outcome :: Either TransportInterrupted ()) `shouldBe` Left TransportInterrupted
      running <- transaction connection $ P.getJust key
      arkhamDBPushStatus running `shouldBe` "running"
      arkhamDBPushStage running `shouldBe` "creating"
      void $ preparedDeck $ arkhamDBPushPayload running
      -- No explicit retry/fixture status update: a new production invocation
      -- must reclaim the abandoned >60s lease and read its durable payload.
      push app settings remote.client f False 240
      completed <- transaction connection $ P.getJust key
      arkhamDBPushStatus completed `shouldBe` "succeeded"
      arkhamDBPushPayload completed `shouldBe` arkhamDBPushPayload running
      filter (Text.isPrefixOf "upgrade/") <$> MVar.readMVar remote.calls `shouldReturn` ["upgrade/123/5"]
      unsafeHandler app (writeBackStore.campaignXpBaseline f.campaign) `shouldReturn` 5

  for_ ["lost-create", "failed-save", "lost-save"] \failure ->
    it ("restarts from actual SQL with byte-identical frozen payload and one XP creation: " <> toString failure) $
      withConnection harness \connection -> withConnection harness \observer -> do
        f <- fixture seed app connection failure
        queue f connection 1
        -- A lost upgrade reply can already have materialized the desired main
        -- deck; failed/lost save cases deliberately require the save transport.
        let current = source {deck = source.deck
              { Deck.slots = if failure == "lost-create"
                  then Map.fromList [("01006", 1), ("01100", 1), ("01007", 1)] else mempty
              , Deck.taboo_id = Just 10, Deck.sideSlots = Map.singleton "01009" 2
              , Deck.meta = Just "{\"extra_deck\":\"01008\"}"
              }}
        remote <- newRemote unexpected current
        let broken = remote.client
              { upgradeDeck = \token key earned metadata -> do
                  result <- remote.client.upgradeDeck token key earned metadata
                  pure $ if failure == "lost-create" then Left $ ArkhamDBRemoteError "lost create reply" else result
              , saveDeck = \token key value ->
                  if failure == "failed-save" then pure $ Left $ ArkhamDBRemoteError "save unavailable"
                  else remote.client.saveDeck token key value >> pure (Left $ ArkhamDBRemoteError "lost save reply")
              }
        push app settings broken f False 0
        P.Entity key before <- storedJob observer f
        arkhamDBPushStatus before `shouldBe` "failed"
        arkhamDBPushStage before `shouldBe` if failure == "lost-create" then "creating" else "saving"
        arkhamDBPushSourceId before `shouldBe` Just "123"
        arkhamDBPushTargetId before `shouldBe` if failure == "lost-create" then Nothing else Just "124"
        frozen <- preparedDeck $ arkhamDBPushPayload before
        editRemote remote "123" \old -> old {deck = old.deck
          {Deck.sideSlots = Map.singleton "01100" 9, Deck.meta = Just "{\"extra_deck\":\"01100\"}"}}
        -- Fresh invocation/store selection, not an in-memory resumed entity.
        push app settings remote.client f True 240
        after <- transaction observer $ P.getJust key
        arkhamDBPushStatus after `shouldBe` "succeeded"
        arkhamDBPushPayload after `shouldBe` arkhamDBPushPayload before
        final <- readRemote remote "124"
        samePushedDeck frozen final `shouldBe` True
        filter (Text.isPrefixOf "upgrade/") <$> MVar.readMVar remote.calls `shouldReturn` ["upgrade/123/5"]
        unsafeHandler app (writeBackStore.campaignXpBaseline f.campaign) `shouldReturn` 5

  it "reuses the acknowledged scenario-end version for a later upgrade with current source fields" $
    withConnection harness \connection -> do
      f <- fixture seed app connection "same-version"
      remote <- newRemote unexpected source
      queue f connection 1
      push app settings remote.client f False 0
      editRemote remote "124" \old -> old {ArkhamDB.dateUpdate = Just "later",
        deck = old.deck {Deck.sideSlots = Map.singleton "01009" 3, Deck.meta =
          either (const Nothing) (Just . decodeUtf8 . toStrict . encode . Object . KM.insert "extra_deck" (String "01100")) $ pushDeckMetadata old.deck}}
      sync app settings remote.client f
      -- Snapshot another legitimate boundary with changed campaign cards.
      let changed = f.game {Game.gameMode = case f.game.gameMode of
            This campaign -> This $ overAttrs (\attrs -> attrs
              {campaignDecks = Map.map (\(Deck cards) -> Deck $ drop 1 cards) $ campaignDecks attrs}) campaign
            _ -> error "Expected campaign fixture"}
      transaction connection $ queueCampaignWriteBack f.gameId 2 (addUTCTime 180 f.now) "upgrade" changed
      push app settings remote.client f False 240
      P.Entity _ job <- storedJob connection f
      arkhamDBPushStatus job `shouldBe` "succeeded"
      frozen <- preparedDeck $ arkhamDBPushPayload job
      frozen.sideSlots `shouldBe` Map.singleton "01009" 3
      filter (Text.isPrefixOf "upgrade/") <$> MVar.readMVar remote.calls `shouldReturn` ["upgrade/123/5"]

  for_ [False, True] \undo ->
    it (if undo then "retains opaque prepared creation accounting after undo" else "retains opaque prepared creation accounting during opt-out") $
      withConnection harness \connection -> do
        f <- fixture seed app connection $ if undo then "prepared-undo" else "prepared-disable"
        remote <- newRemote unexpected source
        queue f connection 1
        push app settings remote.client {saveDeck = \_ _ _ -> pure $ Left $ ArkhamDBRemoteError "unsaved"} f False 0
        P.Entity key before <- storedJob connection f
        void $ preparedDeck $ arkhamDBPushPayload before
        if undo then transaction connection $ undoCampaignWriteBack f.gameId 0
        else unsafeHandler app (writeBackStore.toggleWriteBack f.now f.uid f.deckId False) `shouldReturn` True
        calls <- MVar.readMVar remote.calls
        push app settings remote.client f True 240
        after <- transaction connection $ P.getJust key
        arkhamDBPushPayload after `shouldBe` arkhamDBPushPayload before
        arkhamDBPushTargetId after `shouldBe` Just "124"
        arkhamDBPushError after `shouldBe` Just (if undo then "boundaryUndone" else "writeBackDisabled")
        MVar.readMVar remote.calls `shouldReturn` calls
        unsafeHandler app (writeBackStore.campaignXpBaseline f.campaign) `shouldReturn` 0

  it "rejects a stale real creating checkpoint without publishing its payload or issuing creation" $
    withConnection harness \connection -> do
      f <- fixture seed app connection "checkpoint-cas"
      queue f connection 1
      remote <- newRemote unexpected source
      let writes = writeBackStore
            { checkpointPush = \previous proposed -> do
                -- Opt-out is a real competing production operation, not a
                -- fake False checkpoint. The unchanged CAS must now refuse.
                when (arkhamDBPushStage proposed == "creating") $
                  void $ writeBackStore.toggleWriteBack f.now f.uid f.deckId False
                writeBackStore.checkpointPush previous proposed
            }
      unsafeHandler app $ runWriteBack (persistentStore $ const $ pure ()) writes
        (hoistArkhamDBClient liftIO remote.client) settings f.now False f.uid f.deckId
      P.Entity _ job <- storedJob connection f
      arkhamDBPushStage job `shouldBe` "ready"
      decodePushDeckPayload (arkhamDBPushPayload job) `shouldSatisfy` either (const False) (not . fst)
      filter (\call -> call == "create" || Text.isPrefixOf "upgrade/" call) <$> MVar.readMVar remote.calls `shouldReturn` []

  it "preserves prepared bytes when proven-unsent recovery returns ready then a synced source changes" $
    withConnection harness \connection -> do
      f <- fixture seed app connection "proven-unsent"
      remote <- newRemote unexpected source
      queue f connection 1
      push app settings remote.client {upgradeDeck = \_ _ _ _ -> pure $ Left $ ArkhamDBRemoteError "unsent"} f False 0
      P.Entity key before <- storedJob connection f
      void $ preparedDeck $ arkhamDBPushPayload before
      push app settings remote.client f True 240
      ready <- transaction connection $ P.getJust key
      arkhamDBPushStage ready `shouldBe` "ready"
      editRemote remote "123" \old -> old {ArkhamDB.dateUpdate = Just "changed", deck = old.deck {Deck.sideSlots = Map.singleton "01009" 2}}
      sync app settings remote.client f
      push app settings remote.client f True 480
      after <- transaction connection $ P.getJust key
      arkhamDBPushStatus after `shouldBe` "conflict"
      arkhamDBPushError after `shouldBe` Just "remoteChanged"
      arkhamDBPushPayload after `shouldBe` arkhamDBPushPayload before
      filter (Text.isPrefixOf "upgrade/") <$> MVar.readMVar remote.calls `shouldReturn` []

  for_ ["creating", "created", "saving"] \stage ->
    it ("refuses legacy persisted " <> toString stage <> " without transport or XP acknowledgment") $
      withConnection harness \connection -> do
        f <- fixture seed app connection $ "legacy-" <> stage
        queue f connection 1
        P.Entity key _ <- storedJob connection f
        -- An old-version persisted row is necessarily fixture input, not a
        -- new service transition; it deliberately has no prepared envelope.
        transaction connection $ P.update key
          [ ArkhamDBPushStatus P.=. "failed", ArkhamDBPushStage P.=. stage
          , ArkhamDBPushSourceId P.=. Just "123"
          , ArkhamDBPushTargetId P.=. if stage == "creating" then Nothing else Just "124"
          ]
        remote <- newRemote unexpected source
        push app settings remote.client f True 240
        job <- transaction connection $ P.getJust key
        arkhamDBPushError job `shouldBe` Just "pushUncertain"
        MVar.readMVar remote.calls `shouldReturn` []
        unsafeHandler app (writeBackStore.campaignXpBaseline f.campaign) `shouldReturn` 0
