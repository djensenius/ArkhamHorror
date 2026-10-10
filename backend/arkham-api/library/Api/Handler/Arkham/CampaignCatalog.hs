module Api.Handler.Arkham.CampaignCatalog (
  CampaignCatalogResponse (..),
  campaignCatalogResponse,
  etagMatches,
  getApiV1ArkhamCampaignCatalogR,
) where

import Import

import Base.Api.Types.CampaignCatalog (
  campaignCatalogBytes,
  campaignCatalogETag,
  campaignCatalogResponseHeaders,
 )
import Data.ByteString.Char8 qualified as BS8
import Network.HTTP.Types.Status (status304)

data CampaignCatalogResponse
  = CampaignCatalogNotModified [(Text, Text)]
  | CampaignCatalogOk [(Text, Text)] ByteString
  deriving stock (Eq, Show)

etagMatches :: Maybe ByteString -> Bool
etagMatches Nothing = False
etagMatches (Just raw) = any matchesToken $ BS8.split ',' raw
 where
  expected = stripWeak $ encodeUtf8 campaignCatalogETag
  matchesToken token
    | trimmed == "*" = True
    | otherwise = stripWeak trimmed == expected
   where
    trimmed = trimOWS token
  stripWeak token =
    let entityTag = if "W/" `BS8.isPrefixOf` token then BS8.drop 2 token else token
     in if isQuoted entityTag then entityTag else ""
  isQuoted token = BS8.length token >= 2 && BS8.head token == '"' && BS8.last token == '"'
  trimOWS = BS8.dropWhileEnd isOWS . BS8.dropWhile isOWS
  isOWS c = c == ' ' || c == '\t'

campaignCatalogResponse :: Maybe ByteString -> CampaignCatalogResponse
campaignCatalogResponse ifNoneMatch
  | etagMatches ifNoneMatch = CampaignCatalogNotModified campaignCatalogResponseHeaders
  | otherwise = CampaignCatalogOk campaignCatalogResponseHeaders campaignCatalogBytes

getApiV1ArkhamCampaignCatalogR :: Handler TypedContent
getApiV1ArkhamCampaignCatalogR = do
  ifNoneMatch <- lookupHeader "If-None-Match"
  case campaignCatalogResponse ifNoneMatch of
    CampaignCatalogNotModified headers -> do
      traverse_ (uncurry addHeader) headers
      sendResponseStatus status304 ("" :: Text)
    CampaignCatalogOk headers bytes -> do
      traverse_ (uncurry addHeader) headers
      pure $ TypedContent typeJson $ toContent bytes
