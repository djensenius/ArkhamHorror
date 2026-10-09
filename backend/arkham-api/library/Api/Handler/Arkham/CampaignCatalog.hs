{-# LANGUAGE TemplateHaskell #-}

module Api.Handler.Arkham.CampaignCatalog (
  campaignCatalogValue,
  getApiV1ArkhamCampaignCatalogR,
) where

import Import

import Data.Aeson qualified as Aeson
import Data.FileEmbed (embedFile)

campaignCatalogBytes :: ByteString
campaignCatalogBytes = $(embedFile "../../contracts/fixtures/campaign-catalog.json")

campaignCatalogValue :: Aeson.Value
campaignCatalogValue =
  case Aeson.eitherDecodeStrict campaignCatalogBytes of
    Left err -> error $ "invalid embedded campaign catalog: " <> toText err
    Right value -> value

getApiV1ArkhamCampaignCatalogR :: Handler Aeson.Value
getApiV1ArkhamCampaignCatalogR = pure campaignCatalogValue
