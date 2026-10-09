{-# LANGUAGE OverloadedStrings #-}

module Arkham.Api.CampaignCatalogSpec (spec) where

import Api.Handler.Arkham.CampaignCatalog (campaignCatalogValue)
import Data.Aeson qualified as Aeson
import Data.Aeson.KeyMap qualified as KeyMap
import Helpers.Contracts (loadContractJson)
import TestImport

spec :: Spec
spec = describe "campaign catalog endpoint payload" do
  it "serves the governed campaign-catalog fixture" do
    fixture <- loadContractJson "contracts/fixtures/campaign-catalog.json"
    campaignCatalogValue `shouldBe` fixture

  it "advertises its public endpoint and revision in the embedded payload" do
    case campaignCatalogValue of
      Aeson.Object payload -> do
        KeyMap.lookup "endpoint" payload `shouldBe` Just (Aeson.String "/api/v1/arkham/campaign-catalog")
        KeyMap.lookup "schemaVersion" payload `shouldBe` Just (Aeson.String "1.0.0")
      _ -> expectationFailure "campaign catalog fixture must be an object"
