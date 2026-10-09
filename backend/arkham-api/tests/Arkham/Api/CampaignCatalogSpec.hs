{-# LANGUAGE OverloadedStrings #-}

module Arkham.Api.CampaignCatalogSpec (spec) where

import Base.Api.Types.CampaignCatalog (
  CampaignCatalog (..),
  campaignCatalogBytes,
  campaignCatalogCapability,
  campaignCatalogETag,
  campaignCatalogMetadata,
  campaignCatalogResponseHeaders,
  campaignCatalogValue,
 )
import Base.Api.Types.Capabilities (ServerCapabilities (..), serverCapabilities)
import Data.Aeson qualified as Aeson
import Data.Aeson.KeyMap qualified as KeyMap
import Data.ByteString qualified as BS
import Data.List qualified as List
import TestImport

spec :: Spec
spec = describe "campaign catalog endpoint payload" do
  it "serves the generated backend catalog artifact bytes" do
    artifact <- BS.readFile "data/campaign-catalog.json"
    campaignCatalogBytes `shouldBe` artifact
    Aeson.decodeStrict artifact `shouldBe` Just campaignCatalogValue

  it "advertises its public endpoint and revision in the embedded payload" do
    case campaignCatalogValue of
      Aeson.Object payload -> do
        KeyMap.lookup "endpoint" payload `shouldBe` Just (Aeson.String "/api/v1/arkham/campaign-catalog")
        KeyMap.lookup "schemaVersion" payload `shouldBe` Just (Aeson.String "1.0.0")
        KeyMap.lookup "catalogRevision" payload `shouldBe` Just (Aeson.String campaignCatalogMetadata.catalogRevision)
      _ -> expectationFailure "campaign catalog artifact must be an object"

  it "publishes cache headers derived from the catalog revision" do
    campaignCatalogResponseHeaders
      `shouldContain` [("ETag", "\"" <> campaignCatalogMetadata.catalogRevision <> "\"")]
    campaignCatalogResponseHeaders
      `shouldContain` [("Cache-Control", "public, max-age=300, must-revalidate")]
    campaignCatalogETag `shouldBe` "\"" <> campaignCatalogMetadata.catalogRevision <> "\""

  it "advertises the catalog capability and revision in GET /api/v1/capabilities" do
    let capabilities = serverCapabilities Nothing
    capabilities.capabilities `shouldSatisfy` List.elem campaignCatalogCapability
    capabilities.campaignCatalog `shouldBe` campaignCatalogMetadata
