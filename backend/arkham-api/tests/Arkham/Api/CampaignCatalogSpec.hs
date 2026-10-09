{-# LANGUAGE OverloadedStrings #-}

module Arkham.Api.CampaignCatalogSpec (spec) where

import Api.Handler.Arkham.CampaignCatalog (
  CampaignCatalogResponse (..),
  campaignCatalogResponse,
  etagMatches,
 )
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
import Data.ByteString.Char8 qualified as BS8
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

  describe "If-None-Match entity tag matching" do
    let current = encodeUtf8 campaignCatalogETag
    it "matches the exact quoted entity tag" do
      etagMatches (Just current) `shouldBe` True

    it "matches a weak entity tag with the current revision" do
      etagMatches (Just $ "W/" <> current) `shouldBe` True

    it "matches the current revision inside a comma-delimited list" do
      etagMatches (Just $ "\"old\", " <> current <> ", \"new\"") `shouldBe` True

    it "matches the wildcard validator" do
      etagMatches (Just "*") `shouldBe` True

    it "does not match a different entity tag" do
      etagMatches (Just "\"not-the-current-revision\"") `shouldBe` False

    it "does not match when the header is missing" do
      etagMatches Nothing `shouldBe` False

    it "ignores malformed unquoted entity tags" do
      etagMatches (Just $ BS8.pack "W/not-quoted") `shouldBe` False

  describe "campaign catalog response decision" do
    it "returns 304 metadata when If-None-Match matches" do
      campaignCatalogResponse (Just $ encodeUtf8 campaignCatalogETag)
        `shouldBe` CampaignCatalogNotModified campaignCatalogResponseHeaders

    it "returns 200 payload metadata when If-None-Match does not match" do
      campaignCatalogResponse (Just "\"stale\"")
        `shouldBe` CampaignCatalogOk campaignCatalogResponseHeaders campaignCatalogBytes

  it "advertises the catalog capability and revision in GET /api/v1/capabilities" do
    let capabilities = serverCapabilities Nothing
    capabilities.capabilities `shouldSatisfy` List.elem campaignCatalogCapability
    capabilities.campaignCatalog `shouldBe` campaignCatalogMetadata
