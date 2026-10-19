{-# LANGUAGE NoFieldSelectors #-}

module Arkham.ArkhamDB (
  ArkhamDBClient (..),
  ArkhamDBClientError (..),
  ArkhamDBDeckSummary (..),
  ArkhamDBImportRequest (..),
  ArkhamDBLinkStartResponse (..),
  ArkhamDBLinkStatusResponse (..),
  ArkhamDBOAuthTokens (..),
  ArkhamDBRemoteDeck (..),
  ArkhamDBSyncResult (..),
  TokenCiphertext (..),
  arkhamDBAccountLinkingCapability,
  arkhamDBAuthorizeUrl,
  decryptToken,
  encryptToken,
  httpArkhamDBClient,
  isTokenExpired,
  validateLinkState,
) where

import Arkham.Decklist (ArkhamDBDecklist)
import Crypto.Cipher.AES (AES256)
import Crypto.Cipher.Types (AEADMode (AEAD_GCM), AuthTag, aeadInit, aeadSimpleDecrypt, aeadSimpleEncrypt, cipherInit)
import Crypto.Error (CryptoFailable (..))
import Crypto.Hash.SHA256 qualified as SHA256
import Crypto.Random (getRandomBytes)
import Data.Aeson
import Data.Aeson.Casing (aesonPrefix, snakeCase)
import Data.Aeson.Types (Parser, parseEither, typeMismatch)
import Data.ByteArray qualified as BA
import Data.ByteString.Base64.URL qualified as B64Url
import Data.ByteString.Char8 qualified as BS8
import Data.ByteString.Lazy qualified as BSL
import Data.Text qualified as T
import Data.Time.Clock (UTCTime, addUTCTime, diffUTCTime)
import Data.Time.Clock qualified as Clock
import Database.Persist (Entity (..))
import Entity.Arkham.ArkhamDBAccount
import Entity.Arkham.Deck
import Entity.User
import Network.HTTP.Client
import Network.HTTP.Types (hAccept, hAuthorization, status401)
import Network.HTTP.Types.URI (renderQuery)
import Relude

arkhamDBAccountLinkingCapability :: Text
arkhamDBAccountLinkingCapability = "arkhamdb.account-linking.v1"

data ArkhamDBClientError
  = ArkhamDBUnauthorized
  | ArkhamDBRemoteError Text
  | ArkhamDBDecodeError Text
  deriving stock (Eq, Show)

data ArkhamDBOAuthTokens = ArkhamDBOAuthTokens
  { accessToken :: Text
  , refreshToken :: Maybe Text
  , expiresAt :: Maybe UTCTime
  , remoteUserId :: Text
  , remoteUserName :: Maybe Text
  }
  deriving stock (Eq, Show, Generic)

data ArkhamDBDeckSummary = ArkhamDBDeckSummary
  { id :: Text
  , name :: Text
  , url :: Maybe Text
  , investigatorName :: Maybe Text
  , lastUpdatedAt :: Maybe Text
  , version :: Maybe Int
  , linkedLocalDeckId :: Maybe ArkhamDeckId
  }
  deriving stock (Eq, Show, Generic)

instance ToJSON ArkhamDBDeckSummary where
  toJSON = genericToJSON defaultOptions
  toEncoding = genericToEncoding defaultOptions

instance FromJSON ArkhamDBDeckSummary where
  parseJSON = withObject "ArkhamDBDeckSummary" \o -> do
    id <- parseTextId =<< o .: "id"
    name <- o .: "name"
    url <- o .:? "url"
    investigatorName <- o .:? "investigatorName" <|> o .:? "investigator_name"
    lastUpdatedAt <- o .:? "lastUpdatedAt" <|> o .:? "date_update"
    version <- o .:? "version"
    pure ArkhamDBDeckSummary {linkedLocalDeckId = Nothing, ..}

data ArkhamDBRemoteDeck = ArkhamDBRemoteDeck
  { deck :: ArkhamDBDecklist
  , remoteDeckId :: Text
  , dateUpdate :: Maybe Text
  , version :: Maybe Int
  }
  deriving stock (Eq, Show, Generic)

instance FromJSON ArkhamDBRemoteDeck where
  parseJSON value = do
    deck <- parseJSON value
    withObject "ArkhamDBRemoteDeck" (\o -> ArkhamDBRemoteDeck deck <$> (parseTextId =<< o .: "id") <*> o .:? "date_update" <*> o .:? "version") value

newtype ArkhamDBImportRequest = ArkhamDBImportRequest {arkhamdbDeckId :: Text}
  deriving stock (Eq, Show, Generic)

instance FromJSON ArkhamDBImportRequest where
  parseJSON = genericParseJSON $ aesonPrefix snakeCase

-- | Returned by POST /link/start. The nonce stays server-side in the URL state;
-- clients only need the URL and expiry.
data ArkhamDBLinkStartResponse = ArkhamDBLinkStartResponse
  { authorizeUrl :: Text
  , expiresAt :: UTCTime
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON)

data ArkhamDBLinkStatusResponse = ArkhamDBLinkStatusResponse
  { linked :: Bool
  , remoteAccountId :: Maybe Text
  , remoteAccountName :: Maybe Text
  , tokenExpiry :: Maybe UTCTime
  , linkedAt :: Maybe UTCTime
  , writeBackEnabledDeckCount :: Int
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON)

data ArkhamDBSyncResult = ArkhamDBSyncResult
  { localDeckId :: ArkhamDeckId
  , remoteDeckId :: Text
  , syncedAt :: UTCTime
  , remoteVersion :: Maybe Int
  , remoteDateUpdate :: Maybe Text
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON)

data ArkhamDBClient m = ArkhamDBClient
  { exchangeCode :: Text -> Text -> m (Either ArkhamDBClientError ArkhamDBOAuthTokens)
  , refreshTokens :: Text -> m (Either ArkhamDBClientError ArkhamDBOAuthTokens)
  , listDecks :: Text -> m (Either ArkhamDBClientError [ArkhamDBDeckSummary])
  , loadDeck :: Text -> Text -> m (Either ArkhamDBClientError ArkhamDBRemoteDeck)
  }

newtype TokenCiphertext = TokenCiphertext {unTokenCiphertext :: Text}
  deriving stock (Eq, Show)

keyBytes :: Text -> ByteString
keyBytes = SHA256.hash . encodeUtf8

encryptToken :: MonadIO m => Text -> Text -> m TokenCiphertext
encryptToken key plaintext = liftIO do
  nonce <- getRandomBytes 12
  let Right cipher = initAES key
      aead = aeadInit AEAD_GCM cipher nonce
      (tag, ciphertext) = aeadSimpleEncrypt aead mempty (encodeUtf8 plaintext) 16
      render = decodeUtf8 . B64Url.encodeUnpadded
  pure $ TokenCiphertext $ T.intercalate ":" ["v1", render nonce, render (BA.convert tag), render ciphertext]

decryptToken :: Text -> TokenCiphertext -> Either Text Text
decryptToken key (TokenCiphertext encoded) = case T.splitOn ":" encoded of
  ["v1", nonceText, tagText, ciphertextText] -> do
    nonce <- decodePart "nonce" nonceText
    tagBytes <- decodePart "tag" tagText
    ciphertext <- decodePart "ciphertext" ciphertextText
    tag <- maybeToRight "invalid token tag" $ BA.convert tagBytes
    cipher <- first toText $ initAES key
    let aead = aeadInit AEAD_GCM cipher nonce
    bytes <- maybeToRight "token authentication failed" $ aeadSimpleDecrypt aead mempty ciphertext tag
    first toText $ decodeUtf8' bytes
  _ -> Left "invalid token ciphertext"
 where
  decodePart label = first (const $ "invalid token " <> label) . B64Url.decodeUnpadded . encodeUtf8

initAES :: Text -> Either String AES256
initAES key = case cipherInit (keyBytes key) of
  CryptoPassed cipher -> Right cipher
  CryptoFailed err -> Left $ show err

isTokenExpired :: UTCTime -> Maybe UTCTime -> Bool
isTokenExpired _ Nothing = False
isTokenExpired now (Just expiresAt) = diffUTCTime expiresAt now <= 60

validateLinkState :: UTCTime -> Text -> Maybe (Entity ArkhamDBLinkState) -> Either Text UserId
validateLinkState now _ Nothing = Left "Invalid ArkhamDB link state"
validateLinkState now nonce (Just (Entity _ state))
  | arkhamDBLinkStateNonce state /= nonce = Left "Invalid ArkhamDB link state"
  | arkhamDBLinkStateExpiresAt state <= now = Left "ArkhamDB link state expired"
  | otherwise = Right $ arkhamDBLinkStateUserId state

arkhamDBAuthorizeUrl :: Text -> Text -> Text -> Text -> Text
arkhamDBAuthorizeUrl baseUrl clientId redirectUri nonce =
  normalizeBase baseUrl
    <> "/oauth/v2/auth?"
    <> decodeUtf8 (BSL.toStrict $ renderQuery False params)
 where
  params =
    [ ("response_type", Just "code")
    , ("client_id", Just $ encodeUtf8 clientId)
    , ("redirect_uri", Just $ encodeUtf8 redirectUri)
    , ("state", Just $ encodeUtf8 nonce)
    ]

normalizeBase :: Text -> Text
normalizeBase = T.dropWhileEnd (== '/')

httpArkhamDBClient :: MonadIO m => Manager -> Text -> Text -> Text -> ArkhamDBClient m
httpArkhamDBClient manager baseUrl clientId clientSecret = ArkhamDBClient {..}
 where
  exchangeCode code redirectUri = tokenRequest [("grant_type", "authorization_code"), ("code", code), ("redirect_uri", redirectUri)]
  refreshTokens refreshToken = tokenRequest [("grant_type", "refresh_token"), ("refresh_token", refreshToken)]

  listDecks accessToken = authedGet accessToken "/api/oauth2/decks"
  loadDeck accessToken deckId = authedGet accessToken $ "/api/oauth2/deck/load/" <> deckId

  tokenRequest fields = liftIO do
    req0 <- parseRequest $ T.unpack $ normalizeBase baseUrl <> "/oauth/v2/token"
    now <- Clock.getCurrentTime
    let form = [(encodeUtf8 k, encodeUtf8 v) | (k, v) <- fields <> [("client_id", clientId), ("client_secret", clientSecret)]]
        req = urlEncodedBody form req0 {method = "POST", requestHeaders = [(hAccept, "application/json")]}
    response <- httpLbs req manager
    if responseStatus response == status401
      then pure $ Left ArkhamDBUnauthorized
      else pure $ parseTokenResponse now (responseBody response)

  authedGet accessToken path = liftIO do
    req0 <- parseRequest $ T.unpack $ normalizeBase baseUrl <> path
    let req = req0 {requestHeaders = [(hAuthorization, "Bearer " <> encodeUtf8 accessToken), (hAccept, "application/json")]}
    response <- httpLbs req manager
    if responseStatus response == status401
      then pure $ Left ArkhamDBUnauthorized
      else pure $ first (ArkhamDBDecodeError . toText) $ eitherDecode (responseBody response)

parseTokenResponse :: UTCTime -> BSL.ByteString -> Either ArkhamDBClientError ArkhamDBOAuthTokens
parseTokenResponse now body = first (ArkhamDBDecodeError . toText) do
  value <- eitherDecode body
  flip parseEither value $ withObject "ArkhamDB token response" \o -> do
    accessToken <- o .: "access_token"
    refreshToken <- o .:? "refresh_token"
    expiresIn <- o .:? "expires_in"
    remoteUserId <- (o .: "user_id" <|> o .: "id" <|> pure "unknown") >>= parseRemoteId
    remoteUserName <- o .:? "user_name" <|> o .:? "username" <|> o .:? "name"
    let expiresAt = addUTCTime . fromInteger <$> expiresIn <*> pure now
    pure ArkhamDBOAuthTokens {..}

parseTextId :: Value -> Parser Text
parseTextId = \case
  String t -> pure t
  Number n -> pure $ tshow n
  other -> typeMismatch "text id" other

parseRemoteId :: Value -> Parser Text
parseRemoteId = \case
  String t -> pure t
  Number n -> pure $ tshow n
  other -> typeMismatch "remote user id" other
