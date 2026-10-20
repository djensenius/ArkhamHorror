{-# LANGUAGE NoFieldSelectors #-}

module Arkham.ArkhamDB (
  ArkhamDBClient (..),
  ArkhamDBClientError (..),
  ArkhamDBDeckSummary (..),
  ArkhamDBImportRequest (..),
  ArkhamDBLinkCompleteRequest (..),
  completionHash,
  callbackLocation,
  ArkhamDBLinkStartResponse (..),
  ArkhamDBLinkStatusResponse (..),
  ArkhamDBOAuthTokens (..),
  ArkhamDBRemoteDeck (..),
  TokenCiphertext (..),
  arkhamDBAccountLinkingCapability,
  arkhamDBAuthorizeUrl,
  decryptToken,
  encryptToken,
  httpArkhamDBClient,
  hoistArkhamDBClient,
  isTokenExpired,
  parseTokenResponse,
  validateLinkState,
) where

import Arkham.Decklist (ArkhamDBDecklist)
import Arkham.Decklist qualified as Decklist
import Control.Exception qualified as E
import Crypto.Cipher.AES (AES256)
import Crypto.Cipher.Types (AEADMode (AEAD_GCM), AuthTag (..), aeadInit, aeadSimpleDecrypt, aeadSimpleEncrypt, cipherInit)
import Crypto.Error (CryptoFailable (..))
import Crypto.Hash.SHA256 qualified as SHA256
import Crypto.Random (getRandomBytes)
import Data.Aeson
import Data.Aeson.Types (Parser, parseEither, typeMismatch)
import Data.ByteArray qualified as BA
import Data.ByteString.Base64 qualified as B64
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
import Network.HTTP.Types (hAccept, hAuthorization, status400, status401, status403, status404, statusCode)
import Network.HTTP.Types.URI (renderQuery)
import Relude hiding (id, state)
import System.Timeout (timeout)

arkhamDBAccountLinkingCapability :: Text
arkhamDBAccountLinkingCapability = "arkhamdb.account-linking.v1"

data ArkhamDBClientError
  = ArkhamDBUnauthorized
  | ArkhamDBNotFound
  | ArkhamDBRemoteError Text
  | ArkhamDBDecodeError Text
  deriving stock (Eq, Show)

data ArkhamDBOAuthTokens = ArkhamDBOAuthTokens
  { accessToken :: Text
  , refreshToken :: Maybe Text
  , tokenExpiresAt :: Maybe UTCTime
  , remoteUserId :: Maybe Text
  , remoteUserName :: Maybe Text
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

data ArkhamDBDeckSummary = ArkhamDBDeckSummary
  { id :: Text
  , name :: Text
  , url :: Maybe Text
  , investigatorName :: Maybe Text
  , lastUpdatedAt :: Maybe Text
  , version :: Maybe Text
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
    investigatorName <- ((<|>) <$> o .:? "investigatorName" <*> o .:? "investigator_name")
    lastUpdatedAt <- ((<|>) <$> o .:? "lastUpdatedAt" <*> o .:? "date_update")
    version <- o .:? "version" >>= traverse parseTextValue
    pure ArkhamDBDeckSummary {linkedLocalDeckId = Nothing, ..}

data ArkhamDBRemoteDeck = ArkhamDBRemoteDeck
  { deck :: ArkhamDBDecklist
  , remoteDeckId :: Text
  , dateUpdate :: Maybe Text
  , deckVersion :: Maybe Text
  }
  deriving stock (Eq, Show, Generic)

instance FromJSON ArkhamDBRemoteDeck where
  parseJSON value = do
    parsedDeck <- parseJSON value
    withObject "ArkhamDBRemoteDeck" (\o -> do
      remoteDeckId <- parseTextId =<< o .: "id"
      dateUpdate <- o .:? "date_update"
      deckVersion <- o .:? "version" >>= traverse parseTextValue
      let deck = parsedDeck {Decklist.decklist_id = Just remoteDeckId}
      pure ArkhamDBRemoteDeck {..}) value

newtype ArkhamDBImportRequest = ArkhamDBImportRequest {arkhamdbDeckId :: Text}
  deriving stock (Eq, Show, Generic)

instance FromJSON ArkhamDBImportRequest where
  parseJSON = withObject "ArkhamDBImportRequest" \o -> ArkhamDBImportRequest <$> (parseTextId =<< o .: "deckId")

newtype ArkhamDBLinkCompleteRequest = ArkhamDBLinkCompleteRequest {completion :: Text}
  deriving stock (Eq, Show, Generic)
  deriving anyclass (FromJSON, ToJSON)

-- Only a digest is persisted; the bearer secret is returned to the fixed app scheme.
completionHash :: Text -> Text
completionHash = decodeUtf8 . B64Url.encodeUnpadded . SHA256.hash . encodeUtf8

callbackLocation :: Either Text Text -> Text
callbackLocation result = "arkhamhorror://arkhamdb-link?" <> decodeUtf8 (renderQuery False $ case result of
  Right secret -> [("result", Just "pending"), ("completion", Just $ encodeUtf8 secret)]
  Left reason -> [("result", Just "error"), ("reason", Just $ encodeUtf8 $ if reason == "access_denied" then reason else "link_failed")])

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

data ArkhamDBClient m = ArkhamDBClient
  { exchangeCode :: Text -> Text -> m (Either ArkhamDBClientError ArkhamDBOAuthTokens)
  , refreshTokens :: Text -> m (Either ArkhamDBClientError ArkhamDBOAuthTokens)
  , listDecks :: Text -> m (Either ArkhamDBClientError [ArkhamDBDeckSummary])
  , loadDeck :: Text -> Text -> m (Either ArkhamDBClientError ArkhamDBRemoteDeck)
  }

hoistArkhamDBClient :: (forall a. m a -> n a) -> ArkhamDBClient m -> ArkhamDBClient n
hoistArkhamDBClient transform client = ArkhamDBClient
  { exchangeCode = \code uri -> transform $ client.exchangeCode code uri
  , refreshTokens = transform . client.refreshTokens
  , listDecks = transform . client.listDecks
  , loadDeck = \token remoteId -> transform $ client.loadDeck token remoteId
  }

newtype TokenCiphertext = TokenCiphertext {unTokenCiphertext :: Text}
  deriving stock (Eq, Show)

keyBytes :: Text -> Either String ByteString
keyBytes key = do
  bytes <- B64.decode $ encodeUtf8 key
  if BS8.length bytes == 32 then Right bytes else Left "Invalid ArkhamDB token key"

encryptToken :: MonadIO m => Text -> Text -> m TokenCiphertext
encryptToken key plaintext = liftIO do
  nonce <- getRandomBytes 12
  let Right cipher = initAES key
      CryptoPassed aead = aeadInit AEAD_GCM cipher nonce
      (tag, ciphertext) = aeadSimpleEncrypt aead (mempty :: ByteString) (encodeUtf8 plaintext) 16
      render = decodeUtf8 . B64Url.encodeUnpadded
  pure $ TokenCiphertext $ T.intercalate ":" ["v1", render nonce, render (BA.convert tag), render ciphertext]

decryptToken :: Text -> TokenCiphertext -> Either Text Text
decryptToken key (TokenCiphertext encoded) = case T.splitOn ":" encoded of
  ["v1", nonceText, tagText, ciphertextText] -> do
    nonce <- decodePart "nonce" nonceText
    unless (BS8.length nonce == 12) $ Left "invalid token nonce"
    tagBytes <- decodePart "tag" tagText
    ciphertext <- decodePart "ciphertext" ciphertextText
    unless (BS8.length tagBytes == 16) $ Left "invalid token tag"
    let tag = AuthTag (BA.convert tagBytes)
    cipher <- first toText $ initAES key
    aead <- case aeadInit AEAD_GCM cipher nonce of
      CryptoPassed value -> Right value
      CryptoFailed err -> Left $ T.pack $ show err
    bytes <- maybeToRight "token authentication failed" $ aeadSimpleDecrypt aead (mempty :: ByteString) ciphertext tag
    first (T.pack . show) $ decodeUtf8' bytes
  _ -> Left "invalid token ciphertext"
 where
  decodePart label = first (const $ "invalid token " <> label) . B64Url.decodeUnpadded . encodeUtf8

initAES :: Text -> Either String AES256
initAES key = do
  bytes <- keyBytes key
  case cipherInit bytes of
    CryptoPassed cipher -> Right cipher
    CryptoFailed err -> Left $ show err

isTokenExpired :: UTCTime -> Maybe UTCTime -> Bool
isTokenExpired _ Nothing = False
isTokenExpired now (Just expiresAt) = diffUTCTime expiresAt now <= 60

validateLinkState :: UTCTime -> Text -> Maybe (Entity ArkhamDBLinkState) -> Either Text UserId
validateLinkState _ _ Nothing = Left "Invalid ArkhamDB link state"
validateLinkState now nonce (Just (Entity _ state))
  | arkhamDBLinkStateNonce state /= nonce = Left "Invalid ArkhamDB link state"
  | arkhamDBLinkStateExpiresAt state <= now = Left "ArkhamDB link state expired"
  | otherwise = Right $ arkhamDBLinkStateUserId state

arkhamDBAuthorizeUrl :: Text -> Text -> Text -> Text -> Text
arkhamDBAuthorizeUrl baseUrl clientId redirectUri nonce =
  normalizeBase baseUrl
    <> "/oauth/v2/auth?"
    <> decodeUtf8 (renderQuery False params)
 where
  params =
    [ ("response_type", Just "code")
    , ("client_id", Just $ encodeUtf8 clientId)
    , ("redirect_uri", Just $ encodeUtf8 redirectUri)
    , ("state", Just $ encodeUtf8 nonce)
    ]

normalizeBase :: Text -> Text
normalizeBase = T.dropWhileEnd (== '/')

httpArkhamDBClient :: forall m. MonadIO m => Manager -> Text -> Text -> Text -> ArkhamDBClient m
httpArkhamDBClient manager baseUrl clientId clientSecret = ArkhamDBClient {..}
 where
  exchangeCode code redirectUri = tokenRequest [("grant_type", "authorization_code"), ("code", code), ("redirect_uri", redirectUri)]
  refreshTokens refreshToken = tokenRequest [("grant_type", "refresh_token"), ("refresh_token", refreshToken)]

  listDecks accessToken = authedGet accessToken "/api/oauth2/decks"
  loadDeck accessToken deckId = authedGet accessToken $ "/api/oauth2/deck/load/" <> deckId

  tokenRequest :: [(Text, Text)] -> m (Either ArkhamDBClientError ArkhamDBOAuthTokens)
  tokenRequest fields = safeRequest do
    req0 <- parseRequest $ T.unpack $ normalizeBase baseUrl <> "/oauth/v2/token"
    now <- Clock.getCurrentTime
    let form = [(encodeUtf8 k, encodeUtf8 v) | (k, v) <- fields <> [("client_id", clientId), ("client_secret", clientSecret)]]
        req = urlEncodedBody form req0 {method = "POST", requestHeaders = [(hAccept, "application/json")], redirectCount = 0, checkResponse = \_ _ -> pure ()}
    response <- httpLbs req manager
    let invalidGrant = do
          value <- eitherDecode (responseBody response)
          parseEither (withObject "OAuth error" (.: "error")) value :: Either String Text
    pure $ if responseStatus response == status401 || (responseStatus response == status400 && invalidGrant == Right "invalid_grant")
      then Left ArkhamDBUnauthorized
      else if statusCode (responseStatus response) < 200 || statusCode (responseStatus response) >= 300
        then Left $ ArkhamDBRemoteError "Unexpected HTTP status"
        else parseTokenResponse now $ responseBody response

  authedGet :: FromJSON a => Text -> Text -> m (Either ArkhamDBClientError a)
  authedGet accessToken path = safeRequest do
    req0 <- parseRequest $ T.unpack $ normalizeBase baseUrl <> path
    let req = req0 {requestHeaders = [(hAuthorization, "Bearer " <> encodeUtf8 accessToken), (hAccept, "application/json")], redirectCount = 0, checkResponse = \_ _ -> pure ()}
    response <- httpLbs req manager
    pure $ responseResult response >>= first (const $ ArkhamDBDecodeError "Invalid response") . eitherDecode

  -- Do not include HTTP exception bodies/requests in public errors: they may
  -- contain the client secret, authorization code, or bearer token.
  safeRequest :: IO (Either ArkhamDBClientError a) -> m (Either ArkhamDBClientError a)
  safeRequest action = liftIO $ timeout 10000000 (E.try @HttpException action) >>= \case
    Nothing -> pure $ Left $ ArkhamDBRemoteError "Transport timed out"
    Just (Left _) -> pure $ Left $ ArkhamDBRemoteError "Transport failed"
    Just (Right result) -> pure result

  responseResult :: Response BSL.ByteString -> Either ArkhamDBClientError BSL.ByteString
  responseResult response
    | responseStatus response == status401 = Left ArkhamDBUnauthorized
    | responseStatus response `elem` [status403, status404] = Left ArkhamDBNotFound
    | statusCode (responseStatus response) < 200 || statusCode (responseStatus response) >= 300 = Left $ ArkhamDBRemoteError "Unexpected HTTP status"
    | otherwise = Right $ responseBody response

parseTokenResponse :: UTCTime -> BSL.ByteString -> Either ArkhamDBClientError ArkhamDBOAuthTokens
parseTokenResponse now body = first (const $ ArkhamDBDecodeError "Invalid token response") do
  value <- eitherDecode body
  flip parseEither value $ withObject "ArkhamDB token response" \o -> do
    accessToken <- o .: "access_token"
    when (T.null accessToken) $ fail "Empty access token"
    refreshToken <- o .:? "refresh_token"
    expiresIn <- o .:? "expires_in"
    rawUserId <- ((<|>) <$> o .:? "user_id" <*> o .:? "id")
    remoteUserId <- traverse parseTextValue rawUserId
    remoteUserName <- ((<|>) <$> o .:? "user_name" <*> ((<|>) <$> o .:? "username" <*> o .:? "name"))
    let tokenExpiresAt = addUTCTime . fromInteger <$> expiresIn <*> pure now
    pure ArkhamDBOAuthTokens {..}

parseTextId :: Value -> Parser Text
parseTextId value = case value of
  String t | not (T.null t) && T.all (\c -> c >= '0' && c <= '9') t -> pure t
  Number _ -> do
    integer <- parseJSON value :: Parser Integer
    if integer >= 0 then pure $ T.pack $ show integer else fail "negative deck id"
  other -> typeMismatch "numeric deck/account id" other

parseTextValue :: Value -> Parser Text
parseTextValue = \case
  String t -> pure t
  Number n -> pure $ decodeUtf8 $ BSL.toStrict $ encode n
  other -> typeMismatch "string or number" other
