module Api.Handler.Arkham.CampaignCatalog (
  getApiV1ArkhamCampaignCatalogR,
) where

import Import

import Base.Api.Types.CampaignCatalog (
  campaignCatalogBytes,
  campaignCatalogETag,
  campaignCatalogResponseHeaders,
 )
import Network.HTTP.Types.Status (status304)

getApiV1ArkhamCampaignCatalogR :: Handler TypedContent
getApiV1ArkhamCampaignCatalogR = do
  traverse_ (uncurry addHeader) campaignCatalogResponseHeaders
  ifNoneMatch <- lookupHeader "If-None-Match"
  when (ifNoneMatch == Just (encodeUtf8 campaignCatalogETag)) do
    sendResponseStatus status304 ("" :: Text)
  pure $ TypedContent typeJson $ toContent campaignCatalogBytes
