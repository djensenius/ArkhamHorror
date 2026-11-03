{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE NoFieldSelectors #-}
{-# LANGUAGE OverloadedStrings #-}

module Postgres.DeleteSpec (spec) where

import Api.Handler.Arkham.Decks (deleteApiV1ArkhamDeckR)
import Api.Arkham.ArkhamDBWriteBack (writeBackStore)
import Arkham.ArkhamDB (ArkhamDBClient, ArkhamDBRemoteDeck)
import Arkham.ArkhamDB.Service
import Arkham.ArkhamDB.WriteBack
import Database.Persist qualified as P
import Foundation (App, userIdToToken, unsafeHandler)
import Network.HTTP.Types (status401, status409)
import Network.Wai (defaultRequest, requestHeaders, requestMethod)
import Postgres.Harness
import Postgres.Production
import Postgres.Remote
import Postgres.WriteBackSpec (Fixture (..), Seed, fixture, queue)
import Relude
import Settings (ArkhamDBSettings)
import Test.Hspec

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
