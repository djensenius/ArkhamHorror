module Arkham.ArkhamDB.Service where

import Arkham.ArkhamDB
import Arkham.Decklist (ArkhamDBDecklist)
import Arkham.Decklist qualified as Decklist
import Control.Monad.Except
import Data.Aeson (ToJSON (..), object, (.=))
import Data.Time.Clock (UTCTime, addUTCTime)
import Database.Persist (Entity (..))
import Entity.Arkham.ArkhamDBAccount
import Entity.Arkham.Deck
import Entity.User
import Data.Text qualified as T
import Relude hiding (state)
import Settings (ArkhamDBSettings (..))

-- | Persistence operations are supplied by the handler. Claiming a nonce,
-- finalizing its account/link changes, and paired deck/link writes must each
-- be atomic. Account refresh compares the old ciphertext and never inserts.
data ArkhamDBStore m = ArkhamDBStore
  { saveLinkState :: ArkhamDBLinkState -> m ()
  , claimLinkState :: UTCTime -> Text -> m (Maybe (Entity ArkhamDBLinkState))
  , findAccount :: UserId -> m (Maybe ArkhamDBAccount)
  , finalizeLink :: Text -> ArkhamDBAccount -> m Bool
  , updateAccount :: ArkhamDBAccount -> ArkhamDBAccount -> m Bool
  , unlinkAccount :: UserId -> m ()
  , findDeckLinks :: UserId -> m [(ArkhamDeckId, ArkhamDBDeckLink)]
  , findDeck :: ArkhamDeckId -> m (Maybe (ArkhamDeck, Maybe ArkhamDBDeckLink))
  , saveImportedDeck :: ArkhamDeck -> ArkhamDBRemoteDeck -> UTCTime -> m (Entity ArkhamDeck)
  , saveSyncedDeck :: ArkhamDeckId -> ArkhamDeck -> ArkhamDBRemoteDeck -> UTCTime -> m ()
  , validateDeck :: ArkhamDBDecklist -> m ()
  }

data ArkhamDBError = ArkhamDBError
  { arkhamDBErrorCode :: Text
  , arkhamDBErrorMessage :: Text
  }
  deriving stock (Eq, Show)

instance ToJSON ArkhamDBError where
  toJSON err = object ["code" .= arkhamDBErrorCode err, "error" .= arkhamDBErrorMessage err]

type ArkhamDBAction m = ExceptT ArkhamDBError m

configuredSettings :: Monad m => Maybe ArkhamDBSettings -> ArkhamDBAction m ArkhamDBSettings
configuredSettings = maybe (throwError $ ArkhamDBError "disabled" "ArkhamDB account linking is not configured") pure

redirectUri :: ArkhamDBSettings -> Text
redirectUri settings = T.dropWhileEnd (== '/') settings.arkhamDBPublicServerBase <> "/api/v1/arkham/arkhamdb/callback"

startLink :: Monad m => ArkhamDBStore m -> ArkhamDBSettings -> UTCTime -> UserId -> Text -> ArkhamDBAction m ArkhamDBLinkStartResponse
startLink store settings now userId nonce = do
  let expiresAt = addUTCTime 600 now
      authorizeUrl = arkhamDBAuthorizeUrl settings.arkhamDBBaseUrl settings.arkhamDBClientId (redirectUri settings) nonce
  lift $ store.saveLinkState $ ArkhamDBLinkState nonce userId expiresAt now Nothing
  pure ArkhamDBLinkStartResponse {..}

completeLink :: MonadIO m => ArkhamDBStore m -> ArkhamDBClient m -> ArkhamDBSettings -> UTCTime -> Text -> Text -> ArkhamDBAction m ()
completeLink store client settings now nonce code = do
  state <- lift $ store.claimLinkState now nonce
  userId <- either (throwError . ArkhamDBError "oauthCallbackInvalidState") pure $ validateLinkState now nonce state
  tokens <- remoteResult =<< lift (client.exchangeCode code $ redirectUri settings)
  existing <- lift $ store.findAccount userId
  account <- encryptedAccount settings now userId tokens
  -- Relinking invalidates old deck associations: they belong to the previous
  -- remote account, not necessarily to the account just authorized.
  saved <- lift $ store.finalizeLink nonce account {arkhamDBAccountCreatedAt = maybe now arkhamDBAccountCreatedAt existing}
  unless saved $ throwError $ ArkhamDBError "oauthCallbackInvalidState" "Link authorization is no longer active"

encryptedAccount :: MonadIO m => ArkhamDBSettings -> UTCTime -> UserId -> ArkhamDBOAuthTokens -> m ArkhamDBAccount
encryptedAccount settings now userId tokens = do
  access <- (.unTokenCiphertext) <$> encryptToken settings.arkhamDBTokenKey tokens.accessToken
  refresh <- traverse (fmap (.unTokenCiphertext) . encryptToken settings.arkhamDBTokenKey) tokens.refreshToken
  pure $ ArkhamDBAccount userId tokens.remoteUserId tokens.remoteUserName access refresh tokens.tokenExpiresAt now now

accessTokenFor :: MonadIO m => ArkhamDBStore m -> ArkhamDBClient m -> ArkhamDBSettings -> UTCTime -> UserId -> ArkhamDBAction m Text
accessTokenFor store client settings now userId = do
  account <- linkedAccount store userId
  if isTokenExpired now (arkhamDBAccountExpiresAt account)
    then refreshAccount store client settings now account
    else decryptStored settings (arkhamDBAccountEncryptedAccessToken account)

linkedAccount :: Monad m => ArkhamDBStore m -> UserId -> ArkhamDBAction m ArkhamDBAccount
linkedAccount store userId = lift (store.findAccount userId) >>= maybe (throwError $ ArkhamDBError "notLinked" "ArkhamDB account is not linked") pure

decryptStored :: Monad m => ArkhamDBSettings -> Text -> ArkhamDBAction m Text
decryptStored settings = either (const $ throwError $ ArkhamDBError "storageFailure" "Stored ArkhamDB credentials could not be decrypted") pure . decryptToken settings.arkhamDBTokenKey . TokenCiphertext

refreshAccount :: MonadIO m => ArkhamDBStore m -> ArkhamDBClient m -> ArkhamDBSettings -> UTCTime -> ArkhamDBAccount -> ArkhamDBAction m Text
refreshAccount store client settings now account = do
  encrypted <- maybe (throwError $ ArkhamDBError "tokenExpired" "ArkhamDB token expired; relink required") pure (arkhamDBAccountEncryptedRefreshToken account)
  refresh <- decryptStored settings encrypted
  tokens <- remoteResult =<< lift (client.refreshTokens refresh)
  updated <- encryptedAccount settings now (arkhamDBAccountUserId account) tokens {refreshToken = tokens.refreshToken <|> Just refresh}
  saved <- lift $ store.updateAccount account updated
    { arkhamDBAccountRemoteUserId = arkhamDBAccountRemoteUserId account
    , arkhamDBAccountRemoteUserName = arkhamDBAccountRemoteUserName account
    , arkhamDBAccountCreatedAt = arkhamDBAccountCreatedAt account
    }
  unless saved $ throwError $ ArkhamDBError "notLinked" "ArkhamDB account link changed during refresh"
  pure tokens.accessToken

withAccessToken :: MonadIO m => ArkhamDBStore m -> ArkhamDBClient m -> ArkhamDBSettings -> UTCTime -> UserId -> (Text -> m (Either ArkhamDBClientError a)) -> ArkhamDBAction m a
withAccessToken store client settings now userId action = do
  token <- accessTokenFor store client settings now userId
  lift (action token) >>= \case
    Right result -> pure result
    Left ArkhamDBUnauthorized -> do
      account <- linkedAccount store userId
      fresh <- refreshAccount store client settings now account
      remoteResult =<< lift (action fresh)
    Left err -> remoteResult $ Left err

remoteResult :: Monad m => Either ArkhamDBClientError a -> ArkhamDBAction m a
remoteResult = either (throwError . \case
  ArkhamDBNotFound -> ArkhamDBError "notFound" "ArkhamDB deck not found"
  ArkhamDBUnauthorized -> ArkhamDBError "tokenExpired" "ArkhamDB rejected the token; relink required"
  _ -> ArkhamDBError "remoteUnavailable" "ArkhamDB request failed") pure

listRemoteDecks :: MonadIO m => ArkhamDBStore m -> ArkhamDBClient m -> ArkhamDBSettings -> UTCTime -> UserId -> ArkhamDBAction m [ArkhamDBDeckSummary]
listRemoteDecks store client settings now userId = do
  summaries <- withAccessToken store client settings now userId client.listDecks
  links <- lift $ store.findDeckLinks userId
  pure [summary {linkedLocalDeckId = fst <$> find ((== summary.id) . arkhamDBDeckLinkRemoteDeckId . snd) links} | summary <- summaries]

importRemoteDeck :: MonadIO m => ArkhamDBStore m -> ArkhamDBClient m -> ArkhamDBSettings -> UTCTime -> UserId -> Text -> ArkhamDBAction m (Entity ArkhamDeck)
importRemoteDeck store client settings now userId remoteId = do
  remote <- withAccessToken store client settings now userId (\token -> client.loadDeck token remoteId)
  lift $ store.validateDeck remote.deck
  let decklist = remote.deck {Decklist.url = remote.deck.url <|> Just (settings.arkhamDBBaseUrl <> "/deck/view/" <> remoteId)}
      deck = ArkhamDeck userId decklist.url (fromMaybe decklist.investigator_name decklist.decklist_name) decklist.investigator_name decklist Nothing Nothing
  lift $ store.saveImportedDeck deck remote now

-- | A missing mapping keeps the pre-existing public URL sync path intact.
-- Both paths return the same local deck response and never expose tokens.
syncRemoteDeck :: MonadIO m => ArkhamDBStore m -> ArkhamDBClient m -> ArkhamDBSettings -> UTCTime -> UserId -> ArkhamDeckId -> (ArkhamDeckId -> m (Entity ArkhamDeck)) -> ArkhamDBAction m (Entity ArkhamDeck)
syncRemoteDeck store client settings now userId deckId publicSync = do
  (deck, link) <- lift (store.findDeck deckId) >>= maybe (throwError $ ArkhamDBError "notFound" "Deck not found") pure
  unless (arkhamDeckUserId deck == userId) $ throwError $ ArkhamDBError "notFound" "Deck not found"
  case link of
    Nothing -> lift $ publicSync deckId
    Just mapping -> do
      remote <- withAccessToken store client settings now userId (\token -> client.loadDeck token (arkhamDBDeckLinkRemoteDeckId mapping))
      lift $ store.validateDeck remote.deck
      let updated = deck
            { arkhamDeckList = remote.deck
            , arkhamDeckUrl = remote.deck.url <|> arkhamDeckUrl deck
            , arkhamDeckName = fromMaybe (arkhamDeckName deck) remote.deck.decklist_name
            , arkhamDeckInvestigatorName = remote.deck.investigator_name
            }
      lift $ store.saveSyncedDeck deckId updated remote now
      pure $ Entity deckId updated

linkStatus :: Monad m => ArkhamDBStore m -> UserId -> ArkhamDBAction m ArkhamDBLinkStatusResponse
linkStatus store userId = do
  account <- lift $ store.findAccount userId
  links <- lift $ store.findDeckLinks userId
  pure $ case account of
    Nothing -> ArkhamDBLinkStatusResponse False Nothing Nothing Nothing Nothing 0
    Just value -> ArkhamDBLinkStatusResponse True
      (arkhamDBAccountRemoteUserId value) (arkhamDBAccountRemoteUserName value)
      (arkhamDBAccountExpiresAt value) (Just $ arkhamDBAccountCreatedAt value)
      (length $ filter (arkhamDBDeckLinkWriteBack . snd) links)

unlinkLinkedAccount :: Monad m => ArkhamDBStore m -> UserId -> ArkhamDBAction m ()
unlinkLinkedAccount store userId = lift $ store.unlinkAccount userId
