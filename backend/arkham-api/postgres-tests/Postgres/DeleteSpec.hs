{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE NoFieldSelectors #-}
{-# LANGUAGE OverloadedStrings #-}

module Postgres.DeleteSpec (spec) where

import Api.Arkham.Helpers (ApiResponse (EventChanged, RoomClosed), eventChannel, gameChannel)
import Api.Handler.Arkham.Decks (deleteApiV1ArkhamDeckR)
import Api.Handler.Arkham.Games (deleteApiV1ArkhamGameR)
import Api.Arkham.ArkhamDBWriteBack (writeBackStore)
import Arkham.ArkhamDB (ArkhamDBClient, ArkhamDBRemoteDeck)
import Arkham.ArkhamDB.Service
import Arkham.ArkhamDB.WriteBack
import Arkham.Epic.Types (emptySharedEventState)
import Control.Concurrent.MVar qualified as MVar
import Data.Aeson qualified as Aeson
import Data.Map.Strict qualified as Map
import Database.Persist ((==.))
import Database.Persist qualified as P
import Entity.Arkham.Epic
import Entity.Arkham.Game (ArkhamGameId)
import Entity.Arkham.Player
import Foundation (App (..), Subscriber (..), anonymousSubscriber, newRoom, subscribeToRoom, userIdToToken, unsafeHandler)
import Network.HTTP.Types (status401, status409)
import Network.Wai (defaultRequest, requestHeaders, requestMethod)
import Postgres.Harness
import Postgres.Production
import Postgres.Remote
import Postgres.WriteBackSpec (Fixture (..), Seed, fixture, queue, seedCampaignGame)
import Relude
import Settings (ArkhamDBSettings)
import Test.Hspec
import UnliftIO.Exception (finally)
import UnliftIO.STM (flushTBQueue, isEmptyTBQueue)

spec :: Harness -> App -> ArkhamDBSettings -> ArkhamDBClient IO -> ArkhamDBRemoteDeck -> Seed -> Spec
spec harness app settings unexpected source seed = describe "Actual JWT-authenticated DELETE Handler with production SQL" do
  for_ [False, True] \unlink ->
    it (if unlink then "returns 409 for accounted owned decks after unlink" else "returns 409 for accounted owned decks") $
      withConnection harness \connection -> do
        f <- fixture seed app connection $ if unlink then "jwt-delete-unlinked" else "jwt-delete-accounted"
        queue f connection 1
        remote <- newRemote unexpected source
        push app settings remote.client f False 0
        unsafeHandler app (writeBackStore.campaignXpBaseline f.campaign) `shouldReturn` 5
        when unlink $ runStoreActions app connection \store -> store.unlinkAccount f.uid
        token <- unsafeHandler app $ userIdToToken f.uid
        let request = defaultRequest {requestMethod = "DELETE", requestHeaders = [("Authorization", "Token " <> encodeUtf8 token)]}
        runHandlerResponse app (withRequest request $ deleteApiV1ArkhamDeckR f.deckId) `shouldReturn` Left status409
        transaction connection (isJust <$> P.get f.deckId) `shouldReturn` True

  it "authenticates the actual owner, allows unused deletion and does not disclose foreign/missing ids" $
    withConnection harness \connection -> do
      (uid, _, deckId) <- seed connection "jwt-delete-unused"
      (foreignUid, _, _) <- seed connection "jwt-delete-foreign"
      foreignToken <- unsafeHandler app $ userIdToToken foreignUid
      ownerToken <- unsafeHandler app $ userIdToToken uid
      let request token = defaultRequest {requestMethod = "DELETE", requestHeaders = [("Authorization", "Token " <> encodeUtf8 token)]}
      runHandlerResponse app (withRequest (request foreignToken) $ deleteApiV1ArkhamDeckR deckId) `shouldReturn` Right ()
      transaction connection (isJust <$> P.get deckId) `shouldReturn` True
      runHandlerResponse app (withRequest (request ownerToken) $ deleteApiV1ArkhamDeckR deckId) `shouldReturn` Right ()
      transaction connection (isNothing <$> P.get deckId) `shouldReturn` True
      runHandlerResponse app (withRequest (request ownerToken) $ deleteApiV1ArkhamDeckR deckId) `shouldReturn` Right ()

  it "rejects a request lacking the app JWT before any deletion" $
    withConnection harness \connection -> do
      (_, _, deckId) <- seed connection "jwt-delete-denied"
      runHandlerResponse app (withRequest defaultRequest $ deleteApiV1ArkhamDeckR deckId) `shouldReturn` Left status401
      transaction connection (isJust <$> P.get deckId) `shouldReturn` True

  describe "game DELETE closes the room only for a game it removed" do
    let request token = defaultRequest {requestMethod = "DELETE", requestHeaders = [("Authorization", "Token " <> encodeUtf8 token)]}
        -- A player seated in a fresh game, with a socket open on its room.
        seatedGame connection label = do
          (uid, _, _) <- seed connection label
          now <- currentTimeMicros
          gameId <- seedCampaignGame connection label now
          void $ transaction connection $ P.insert $ ArkhamPlayer uid gameId "01001"
          token <- unsafeHandler app $ userIdToToken uid
          pure (gameId, token)
        -- The rooms map is shared by every test on this app, so the room is
        -- dropped again when the test ends, pass or fail.
        withRoom gameId action = do
          room <- newRoom $ gameChannel gameId
          info <- anonymousSubscriber
          (_, sub) <- subscribeToRoom info room
          let rooms = appGameRooms app
          (MVar.modifyMVar_ rooms (pure . Map.insert gameId room) >> action sub)
            `finally` MVar.modifyMVar_ rooms (pure . Map.delete gameId)
        -- The event room is shared the same way, and dropped the same way.
        withEventRoom eventId action = do
          room <- newRoom $ eventChannel eventId
          info <- anonymousSubscriber
          (_, sub) <- subscribeToRoom info room
          let rooms = appEventRooms app
          (MVar.modifyMVar_ rooms (pure . Map.insert eventId room) >> action sub)
            `finally` MVar.modifyMVar_ rooms (pure . Map.delete eventId)
        {- An Epic event whose first group is the given game (or an unassigned
        group) and whose second is a sibling game, with sockets open on the
        event room and the sibling's room. -}
        withEpic connection label mGameId action = do
          (organizer, _, _) <- seed connection (label <> "-organizer")
          now <- currentTimeMicros
          sibling <- seedCampaignGame connection (label <> "-sibling") now
          eventId <- transaction connection do
            eventId <- P.insert $ ArkhamEpicEvent label organizer Nothing Nothing "Easy" (emptySharedEventState 2) 2 0 now now
            P.insert_ $ ArkhamEpicGroup eventId 0 mGameId "First" 1
            P.insert_ $ ArkhamEpicGroup eventId 1 (Just sibling) "Second" 1
            pure eventId
          withEventRoom eventId \eventSub -> withRoom sibling \siblingSub -> action eventId eventSub siblingSub
        quiet sub = atomically (isEmptyTBQueue sub.subQueue) `shouldReturn` True
        roomOpen gameId = Map.member gameId <$> MVar.readMVar (appGameRooms app)
        untouched gameId sub = do
          roomOpen gameId `shouldReturn` True
          readTVarIO sub.subClosed `shouldReturn` False
          atomically (isEmptyTBQueue sub.subQueue) `shouldReturn` True
        gameExists connection gameId = transaction connection (isJust <$> P.get gameId)
        players connection (gameId :: ArkhamGameId) = transaction connection $ P.count [ArkhamPlayerArkhamGameId ==. gameId]

    it "removes a player's game and closes its room, then repeats as a no-op" $
      withConnection harness \connection -> do
        (gameId, token) <- seatedGame connection "jwt-delete-game-player"
        withRoom gameId \sub -> do
          runHandlerResponse app (withRequest (request token) $ deleteApiV1ArkhamGameR gameId) `shouldReturn` Right ()
          gameExists connection gameId `shouldReturn` False
          players connection gameId `shouldReturn` 0
          roomOpen gameId `shouldReturn` False
          readTVarIO sub.subClosed `shouldReturn` True
          -- Exactly one notice, the deletion close, and nothing after it.
          atomically (flushTBQueue sub.subQueue) `shouldReturn` [Aeson.encode $ RoomClosed "deleted"]
          runHandlerResponse app (withRequest (request token) $ deleteApiV1ArkhamGameR gameId) `shouldReturn` Right ()
          gameExists connection gameId `shouldReturn` False
          atomically (isEmptyTBQueue sub.subQueue) `shouldReturn` True

    it "tells the event and the remaining groups when it removes an Epic group game, once" $
      withConnection harness \connection -> do
        (gameId, token) <- seatedGame connection "jwt-delete-epic-player"
        withEpic connection "jwt-delete-epic-player" (Just gameId) \eventId eventSub siblingSub -> withRoom gameId \sub -> do
          runHandlerResponse app (withRequest (request token) $ deleteApiV1ArkhamGameR gameId) `shouldReturn` Right ()
          gameExists connection gameId `shouldReturn` False
          transaction connection (P.count [ArkhamEpicGroupArkhamGameId ==. Just gameId]) `shouldReturn` 0
          transaction connection (P.count [ArkhamEpicGroupArkhamEpicEventId ==. eventId]) `shouldReturn` 1
          transaction connection (isJust <$> P.get eventId) `shouldReturn` True
          -- One invalidation each, so watchers refetch the roster without the group.
          atomically (flushTBQueue eventSub.subQueue) `shouldReturn` [Aeson.encode EventChanged]
          atomically (flushTBQueue siblingSub.subQueue) `shouldReturn` [Aeson.encode EventChanged]
          atomically (flushTBQueue sub.subQueue) `shouldReturn` [Aeson.encode $ RoomClosed "deleted"]
          runHandlerResponse app (withRequest (request token) $ deleteApiV1ArkhamGameR gameId) `shouldReturn` Right ()
          quiet eventSub
          quiet siblingSub
          quiet sub

    it "keeps an Epic group game and tells nobody when the caller has no seat" $
      withConnection harness \connection -> do
        (gameId, _) <- seatedGame connection "jwt-delete-epic-owner"
        (outsider, _, _) <- seed connection "jwt-delete-epic-outsider"
        outsiderToken <- unsafeHandler app $ userIdToToken outsider
        withEpic connection "jwt-delete-epic-owner" (Just gameId) \eventId eventSub siblingSub -> withRoom gameId \sub -> do
          runHandlerResponse app (withRequest (request outsiderToken) $ deleteApiV1ArkhamGameR gameId) `shouldReturn` Right ()
          gameExists connection gameId `shouldReturn` True
          transaction connection (P.count [ArkhamEpicGroupArkhamEpicEventId ==. eventId]) `shouldReturn` 2
          quiet eventSub
          quiet siblingSub
          untouched gameId sub

    it "tells nobody about a missing game next to an Epic event" $
      withConnection harness \connection -> do
        (gameId, token) <- seatedGame connection "jwt-delete-epic-missing"
        transaction connection $ P.delete gameId
        withEpic connection "jwt-delete-epic-missing" Nothing \_ eventSub siblingSub -> withRoom gameId \sub -> do
          runHandlerResponse app (withRequest (request token) $ deleteApiV1ArkhamGameR gameId) `shouldReturn` Right ()
          quiet eventSub
          quiet siblingSub
          untouched gameId sub

    it "keeps a game and its room when the caller has no seat" $
      withConnection harness \connection -> do
        (gameId, _) <- seatedGame connection "jwt-delete-game-owner"
        (outsider, _, _) <- seed connection "jwt-delete-game-outsider"
        outsiderToken <- unsafeHandler app $ userIdToToken outsider
        withRoom gameId \sub -> do
          runHandlerResponse app (withRequest (request outsiderToken) $ deleteApiV1ArkhamGameR gameId) `shouldReturn` Right ()
          gameExists connection gameId `shouldReturn` True
          players connection gameId `shouldReturn` 1
          untouched gameId sub

    it "answers a missing game with the same no-op and leaves a room under its id alone" $
      withConnection harness \connection -> do
        (gameId, token) <- seatedGame connection "jwt-delete-game-missing"
        transaction connection $ P.delete gameId
        withRoom gameId \sub -> do
          runHandlerResponse app (withRequest (request token) $ deleteApiV1ArkhamGameR gameId) `shouldReturn` Right ()
          untouched gameId sub

    it "rejects a request lacking the app JWT before any deletion" $
      withConnection harness \connection -> do
        (gameId, _) <- seatedGame connection "jwt-delete-game-denied"
        withRoom gameId \sub -> do
          runHandlerResponse app (withRequest defaultRequest $ deleteApiV1ArkhamGameR gameId) `shouldReturn` Left status401
          gameExists connection gameId `shouldReturn` True
          untouched gameId sub
