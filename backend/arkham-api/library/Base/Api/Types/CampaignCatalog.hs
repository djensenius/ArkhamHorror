{-# LANGUAGE TemplateHaskell #-}

module Base.Api.Types.CampaignCatalog (
  CampaignCatalog (..),
  campaignCatalogBytes,
  campaignCatalogCapability,
  campaignCatalogETag,
  campaignCatalogMetadata,
  campaignCatalogResponseHeaders,
  campaignCatalogValue,
) where

import Data.Aeson (FromJSON (..), ToJSON (..), Value (..), defaultOptions, genericToEncoding, genericToJSON, withObject, (.:))
import Data.Aeson qualified as Aeson
import Data.FileEmbed (embedFile)
import Relude

-- | Capability identifier for the static campaign/scenario catalog endpoint.
campaignCatalogCapability :: Text
campaignCatalogCapability = "arkham.campaign-catalog.v1"

-- | The generated catalog bytes served at GET /api/v1/arkham/campaign-catalog.
campaignCatalogBytes :: ByteString
campaignCatalogBytes = $(embedFile "data/campaign-catalog.json")

data CampaignCatalog = CampaignCatalog
  { endpoint :: Text
  , catalogRevision :: Text
  , schemaVersion :: Text
  , digestAlgorithm :: Text
  }
  deriving stock (Eq, Show, Generic)

instance ToJSON CampaignCatalog where
  toJSON = genericToJSON defaultOptions
  toEncoding = genericToEncoding defaultOptions

instance FromJSON CampaignCatalog where
  parseJSON = withObject "campaign catalog metadata" \o ->
    CampaignCatalog
      <$> o .: "endpoint"
      <*> o .: "catalogRevision"
      <*> o .: "schemaVersion"
      <*> o .: "digestAlgorithm"

campaignCatalogValue :: Value
campaignCatalogValue =
  case Aeson.eitherDecodeStrict campaignCatalogBytes of
    Left err -> error $ "invalid embedded campaign catalog: " <> toText err
    Right value -> value

campaignCatalogMetadata :: CampaignCatalog
campaignCatalogMetadata =
  case Aeson.fromJSON campaignCatalogValue of
    Aeson.Error err -> error $ "invalid embedded campaign catalog metadata: " <> toText err
    Aeson.Success metadata -> metadata

campaignCatalogETag :: Text
campaignCatalogETag = "\"" <> campaignCatalogMetadata.catalogRevision <> "\""

campaignCatalogResponseHeaders :: [(Text, Text)]
campaignCatalogResponseHeaders =
  [ ("ETag", campaignCatalogETag)
  , ("Cache-Control", "public, max-age=300, must-revalidate")
  , ("Vary", "Origin")
  ]
