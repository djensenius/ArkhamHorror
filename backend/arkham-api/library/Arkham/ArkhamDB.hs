{-# LANGUAGE NoFieldSelectors #-}

module Arkham.ArkhamDB (
  ArkhamDBClient (..),
  arkhamDBWriteBackCapability,
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
  runArkhamDBRequest,
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

arkhamDBWriteBackCapability :: Text
arkhamDBWriteBackCapability = "arkhamdb.write-back.v1"

data ArkhamDBClientError
  = ArkhamDBUnauthorized
  | ArkhamDBNotFound
  | ArkhamDBRemoteError Text
  | ArkhamDBDecodeError Text
  deriving stock (Eq, Show, Generic)
  deriving anyclass (NFData)

data ArkhamDBOAuthTokens = ArkhamDBOAuthTokens
  { accessToken :: Text
  , refreshToken :: Maybe Text
  , tokenExpiresAt :: Maybe UTCTime
  , remoteUserId :: Maybe Text
  , remoteUserName :: Maybe Text
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON, NFData)

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

-- Force every decoded field without requiring NFData for persistent keys.
instance NFData ArkhamDBDeckSummary where
  rnf = rnf . toJSON

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
  , nextDeckId :: Maybe Text
  }
  deriving stock (Eq, Show, Generic)

instance NFData ArkhamDBRemoteDeck where
  rnf ArkhamDBRemoteDeck {..} = rnf (toJSON deck, remoteDeckId, dateUpdate, deckVersion, nextDeckId)

instance FromJSON ArkhamDBRemoteDeck where
  parseJSON value = do
    parsedDeck <- parseJSON value
    withObject "ArkhamDBRemoteDeck" (\o -> do
      remoteDeckId <- parseTextId =<< o .: "id"
      dateUpdate <- o .:? "date_update"
      deckVersion <- o .:? "version" >>= traverse parseTextValue
      nextDeckId <- ((<|>) <$> o .:? "next_deck" <*> o .:? "next_deck_id") >>= traverse parseTextId
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
  , upgradeDeck :: Text -> Text -> Int -> Maybe Text -> m (Either ArkhamDBClientError Text)
  , createDeck :: Text -> ArkhamDBDecklist -> m (Either ArkhamDBClientError Text)
  , saveDeck :: Text -> Text -> ArkhamDBDecklist -> m (Either ArkhamDBClientError Text)
  }

hoistArkhamDBClient :: (forall a. m a -> n a) -> ArkhamDBClient m -> ArkhamDBClient n
hoistArkhamDBClient transform client = ArkhamDBClient
  { exchangeCode = \code uri -> transform $ client.exchangeCode code uri
  , refreshTokens = transform . client.refreshTokens
  , listDecks = transform . client.listDecks
  , loadDeck = \token remoteId -> transform $ client.loadDeck token remoteId
  , upgradeDeck = \token remoteId xp metadata -> transform $ client.upgradeDeck token remoteId xp metadata
  , createDeck = \token decklist -> transform $ client.createDeck token decklist
  , saveDeck = \token remoteId decklist -> transform $ client.saveDeck token remoteId decklist
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

  upgradeDeck token remoteId xp metadata = authedWrite token "PUT" ("/api/oauth2/deck/upgrade/" <> remoteId) $
    [("xp", showText xp)] <> optional "meta" metadata
  createDeck token decklist = authedWrite token "POST" "/api/oauth2/deck/new" $
    [("investigator", showJSON decklist.investigator_code)] <> deckFields decklist
  saveDeck token remoteId decklist = authedWrite token "PUT" ("/api/oauth2/deck/save/" <> remoteId) $
    [("slots", jsonText decklist.slots), ("side", jsonText decklist.sideSlots), ("problem", "")] <> deckFields decklist

  deckFields decklist = optional "name" decklist.decklist_name
    <> optional "meta" decklist.meta <> [("taboo", maybe "0" showText decklist.taboo_id)]
  optional key = maybe [] (\value -> [(key, value)])
  showText :: Show a => a -> Text
  showText = T.pack . show
  jsonText :: ToJSON a => a -> Text
  jsonText = decodeUtf8 . BSL.toStrict . encode
  showJSON value = case toJSON value of
    String text -> text
    _ -> jsonText value

  authedWrite token verb path fields = liftIO $ runArkhamDBRequest 10000000 do
    req0 <- parseRequest $ T.unpack $ normalizeBase baseUrl <> path
    let req = (urlEncodedBody [(encodeUtf8 k, encodeUtf8 v) | (k, v) <- fields] req0
          { requestHeaders = [(hAuthorization, "Bearer " <> encodeUtf8 token), (hAccept, "application/json")]
          , redirectCount = 0
          , checkResponse = \_ _ -> pure ()
          }) {method = verb}
    withBoundedResponse req manager \response -> pure do
      body <- responseResult response
      value <- first (const $ ArkhamDBDecodeError "Invalid response") $ eitherDecode body
      first (const $ ArkhamDBRemoteError "Write rejected") $ flip parseEither value $ withObject "ArkhamDB write" \o -> do
        success <- o .: "success"
        unless success $ fail "Write rejected"
        parseTextId =<< o .: "msg"

  tokenRequest :: [(Text, Text)] -> m (Either ArkhamDBClientError ArkhamDBOAuthTokens)
  tokenRequest fields = liftIO $ runArkhamDBRequest 10000000 do
    req0 <- parseRequest $ T.unpack $ normalizeBase baseUrl <> "/oauth/v2/token"
    now <- Clock.getCurrentTime
    let form = [(encodeUtf8 k, encodeUtf8 v) | (k, v) <- fields <> [("client_id", clientId), ("client_secret", clientSecret)]]
        req = urlEncodedBody form req0 {method = "POST", requestHeaders = [(hAccept, "application/json")], redirectCount = 0, checkResponse = \_ _ -> pure ()}
    withBoundedResponse req manager \response -> do
      let invalidGrant = do
            value <- eitherDecode (responseBody response)
            parseEither (withObject "OAuth error" (.: "error")) value :: Either String Text
      pure $ if responseStatus response == status401 || (responseStatus response == status400 && invalidGrant == Right "invalid_grant")
        then Left ArkhamDBUnauthorized
        else if statusCode (responseStatus response) < 200 || statusCode (responseStatus response) >= 300
          then Left $ ArkhamDBRemoteError "Unexpected HTTP status"
          else parseTokenResponse now $ responseBody response

  authedGet :: (FromJSON a, NFData a) => Text -> Text -> m (Either ArkhamDBClientError a)
  authedGet accessToken path = liftIO $ runArkhamDBRequest 10000000 do
    req0 <- parseRequest $ T.unpack $ normalizeBase baseUrl <> path
    let req = req0 {requestHeaders = [(hAuthorization, "Bearer " <> encodeUtf8 accessToken), (hAccept, "application/json")], redirectCount = 0, checkResponse = \_ _ -> pure ()}
    withBoundedResponse req manager \response ->
      pure $ responseResult response >>= first (const $ ArkhamDBDecodeError "Invalid response") . eitherDecode

  responseResult :: Response BSL.ByteString -> Either ArkhamDBClientError BSL.ByteString
  responseResult response
    | responseStatus response == status401 = Left ArkhamDBUnauthorized
    | responseStatus response `elem` [status403, status404] = Left ArkhamDBNotFound
    | statusCode (responseStatus response) < 200 || statusCode (responseStatus response) >= 300 = Left $ ArkhamDBRemoteError "Unexpected HTTP status"
    | otherwise = Right $ responseBody response

-- http-client's BodyReader yields decompressed bytes. Count these, not wire
-- Content-Length, so gzip and chunked responses have the same 1 MiB bound.
withBoundedResponse :: Request -> Manager -> (Response BSL.ByteString -> IO (Either ArkhamDBClientError a)) -> IO (Either ArkhamDBClientError a)
withBoundedResponse request manager action = withResponse request manager \response -> do
  let readBody remaining chunks = do
        chunk <- brReadSome (responseBody response) (min 32768 $ remaining + 1)
        let size = fromIntegral $ BSL.length chunk
        if size > remaining
          then pure $ Left $ ArkhamDBRemoteError "Response too large"
          else if BSL.null chunk
            then action response {responseBody = BSL.concat $ reverse chunks}
            else readBody (remaining - size) (chunk : chunks)
  readBody (1024 * 1024 :: Int) []

-- Include classification, JSON decoding and all decoded fields in the deadline,
-- not just transport IO. Do not expose exception requests/bodies: they can hold
-- client secrets, authorization codes or bearer tokens.
runArkhamDBRequest :: NFData a => Int -> IO (Either ArkhamDBClientError a) -> IO (Either ArkhamDBClientError a)
runArkhamDBRequest timeoutMicros action = timeout timeoutMicros (E.try @HttpException $ action >>= E.evaluate . force) >>= \case
  Nothing -> pure $ Left $ ArkhamDBRemoteError "Transport timed out"
  Just (Left _) -> pure $ Left $ ArkhamDBRemoteError "Transport failed"
  Just (Right result) -> pure result

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
