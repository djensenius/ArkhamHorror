module Arkham.ArkhamDB.Service where

import Arkham.ArkhamDB
import Arkham.Decklist (ArkhamDBDecklist)
import Arkham.Decklist qualified as Decklist
import Control.Monad.Except
import Data.Aeson (ToJSON (..), Value (..), decodeStrict', eitherDecodeStrict', encode, object, (.=))
import Data.Aeson.KeyMap qualified as KM
import Data.Text qualified as T
import Data.Time.Clock (UTCTime, addUTCTime)
import Database.Persist (Entity (..), Update, (=.))
import Entity.Arkham.ArkhamDBAccount
import Entity.Arkham.Deck
import Entity.User
import Network.HTTP.Types.Status qualified as Status
import Relude hiding (state)
import Settings (ArkhamDBSettings (..))

-- | Persistence operations are supplied by the handler. Claiming a nonce,
-- finalizing its account/link changes, and paired deck/link writes must each
-- be atomic. Refresh holds a per-user transaction lock from the re-read through
-- remote refresh and persistence; the callback must use the supplied store so
-- all its database operations share that transaction/connection.
data ArkhamDBStore m = ArkhamDBStore
  { withRefreshLock :: forall a. UserId -> (ArkhamDBStore m -> m a) -> m a
  , ownsPushClaim :: Entity ArkhamDBPush -> m Bool
  , saveLinkState :: ArkhamDBLinkState -> m ()
  , claimLinkState :: UTCTime -> Text -> m (Maybe (Entity ArkhamDBLinkState))
  , findAccountGeneration :: UserId -> m (Maybe ArkhamDBAccountId)
  , findAccount :: UserId -> m (Maybe ArkhamDBAccount)
  , stagePendingLink :: UTCTime -> Text -> Text -> Text -> m Bool
  , findPendingLink :: UTCTime -> UserId -> Text -> m (Maybe ArkhamDBLinkState)
  , finalizeLink :: UTCTime -> UserId -> Text -> ArkhamDBAccount -> m Bool
  , updateAccount :: ArkhamDBAccount -> ArkhamDBAccount -> m Bool
  , unlinkAccount :: UserId -> m ()
  , findDeckLinks :: UserId -> m [(ArkhamDeckId, ArkhamDBDeckLink)]
  , findDeck :: ArkhamDeckId -> m (Maybe (ArkhamDeck, Maybe ArkhamDBDeckLink))
  , saveImportedDeck :: ArkhamDBAccountId -> ArkhamDeck -> ArkhamDBRemoteDeck -> UTCTime -> m (Maybe (Entity ArkhamDeck))
  , saveSyncedDeck :: ArkhamDBAccountId -> ArkhamDeckId -> ArkhamDeck -> ArkhamDBRemoteDeck -> UTCTime -> m Bool
  , dropDeckLink :: ArkhamDBAccountId -> UserId -> ArkhamDeckId -> m Bool
  , validateDeck :: ArkhamDBDecklist -> m ()
  }

data ArkhamDBError = ArkhamDBError
  { arkhamDBErrorCode :: Text
  , arkhamDBErrorMessage :: Text
  }
  deriving stock (Eq, Show)

instance ToJSON ArkhamDBError where
  toJSON err = object ["code" .= arkhamDBErrorCode err, "error" .= arkhamDBErrorMessage err]

-- ArkhamDB authorization is separate from the app session: never return 401
-- for an expired or absent remote account link.
arkhamDBErrorStatus :: ArkhamDBError -> Status.Status
arkhamDBErrorStatus err = case arkhamDBErrorCode err of
  "disabled" -> Status.status501
  "oauthCallbackInvalidState" -> Status.status400
  "oauthCompletionInvalid" -> Status.status403
  "notLinked" -> Status.status409
  "tokenExpired" -> Status.status409
  "notFound" -> Status.status404
  "storageFailure" -> Status.status500
  _ -> Status.status502

type ArkhamDBAction m = ExceptT ArkhamDBError m

configuredSettings :: Monad m => Maybe ArkhamDBSettings -> ArkhamDBAction m ArkhamDBSettings
configuredSettings = maybe (throwError $ ArkhamDBError "disabled" "ArkhamDB account linking is not configured") pure

redirectUri :: ArkhamDBSettings -> Text
redirectUri settings = T.dropWhileEnd (== '/') settings.arkhamDBPublicServerBase <> "/api/v1/arkham/arkhamdb/callback"

startLink :: Monad m => ArkhamDBStore m -> ArkhamDBSettings -> UTCTime -> UserId -> Text -> ArkhamDBAction m ArkhamDBLinkStartResponse
startLink store settings now userId nonce = do
  let expiresAt = addUTCTime 600 now
      authorizeUrl = arkhamDBAuthorizeUrl settings.arkhamDBBaseUrl settings.arkhamDBClientId (redirectUri settings) nonce
  lift $ store.saveLinkState $ ArkhamDBLinkState nonce userId expiresAt now Nothing Nothing Nothing
  pure ArkhamDBLinkStartResponse {..}

-- Callback claims state and stages encrypted credentials, but cannot link an account.
completeLink :: MonadIO m => ArkhamDBStore m -> ArkhamDBClient m -> ArkhamDBSettings -> UTCTime -> Text -> Text -> Text -> ArkhamDBAction m ()
completeLink store client settings now nonce code secret = do
  state <- lift $ store.claimLinkState now nonce
  _ <- either (throwError . ArkhamDBError "oauthCallbackInvalidState") pure $ validateLinkState now nonce state
  tokens <- remoteResult =<< lift (client.exchangeCode code $ redirectUri settings)
  encrypted <- encryptToken settings.arkhamDBTokenKey $ decodeUtf8 $ toStrict $ encode tokens
  saved <- lift $ store.stagePendingLink now nonce (completionHash secret) encrypted.unTokenCiphertext
  unless saved $ throwError $ ArkhamDBError "oauthCallbackInvalidState" "Link authorization is no longer active"

-- Completion must be authenticated as the initiating local user, independently
-- of the browser callback. The store rechecks expiry and consumes atomically.
finalizePendingLink :: MonadIO m => ArkhamDBStore m -> ArkhamDBSettings -> UTCTime -> UserId -> Text -> ArkhamDBAction m ()
finalizePendingLink store settings now userId secret = do
  let digest = completionHash secret
      invalid = ArkhamDBError "oauthCompletionInvalid" "Link completion is invalid or expired"
  pending <- lift (store.findPendingLink now userId digest) >>= maybe (throwError invalid) pure
  encrypted <- maybe (throwError invalid) pure $ arkhamDBLinkStateEncryptedPendingTokens pending
  plaintext <- decryptStored settings encrypted
  tokens <- either (const $ throwError $ ArkhamDBError "storageFailure" "Pending credentials could not be decrypted") pure $ eitherDecodeStrict' $ encodeUtf8 plaintext
  account <- encryptedAccount settings now userId tokens
  existing <- lift $ store.findAccount userId
  saved <- lift $ store.finalizeLink now userId digest account {arkhamDBAccountCreatedAt = maybe now arkhamDBAccountCreatedAt existing}
  unless saved $ throwError invalid

encryptedAccount :: MonadIO m => ArkhamDBSettings -> UTCTime -> UserId -> ArkhamDBOAuthTokens -> m ArkhamDBAccount
encryptedAccount settings now userId tokens = do
  access <- (.unTokenCiphertext) <$> encryptToken settings.arkhamDBTokenKey tokens.accessToken
  refresh <- traverse (fmap (.unTokenCiphertext) . encryptToken settings.arkhamDBTokenKey) tokens.refreshToken
  pure $ ArkhamDBAccount userId tokens.remoteUserId tokens.remoteUserName access refresh tokens.tokenExpiresAt now now Nothing 0

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
refreshAccount store client settings now previous = ExceptT $
  withRefreshLock store (arkhamDBAccountUserId previous) \locked -> runExceptT do
    current <- linkedAccount locked (arkhamDBAccountUserId previous)
    if arkhamDBAccountEncryptedAccessToken current /= arkhamDBAccountEncryptedAccessToken previous
      && not (isTokenExpired now $ arkhamDBAccountExpiresAt current)
      then decryptStored settings $ arkhamDBAccountEncryptedAccessToken current
      else refreshLockedAccount locked client settings now current

refreshLockedAccount :: MonadIO m => ArkhamDBStore m -> ArkhamDBClient m -> ArkhamDBSettings -> UTCTime -> ArkhamDBAccount -> ArkhamDBAction m Text
refreshLockedAccount store client settings now account = do
  generation <- accountGeneration store (arkhamDBAccountUserId account)
  let useConcurrentRefresh failure = do
        currentGeneration <- lift $ store.findAccountGeneration (arkhamDBAccountUserId account)
        current <- lift $ store.findAccount (arkhamDBAccountUserId account)
        case current of
          Just value | currentGeneration == Just generation
            && arkhamDBAccountEncryptedAccessToken value /= arkhamDBAccountEncryptedAccessToken account
            && not (isTokenExpired now $ arkhamDBAccountExpiresAt value) -> decryptStored settings $ arkhamDBAccountEncryptedAccessToken value
          _ -> throwError failure
      refreshCurrent = do
        encrypted <- maybe (throwError $ ArkhamDBError "tokenExpired" "ArkhamDB token expired; relink required") pure (arkhamDBAccountEncryptedRefreshToken account)
        refresh <- decryptStored settings encrypted
        tokens <- remoteResult =<< lift (client.refreshTokens refresh)
        updated <- encryptedAccount settings now (arkhamDBAccountUserId account) tokens {refreshToken = tokens.refreshToken <|> Just refresh}
        saved <- lift $ store.updateAccount account updated
          { arkhamDBAccountRemoteUserId = arkhamDBAccountRemoteUserId account
          , arkhamDBAccountRemoteUserName = arkhamDBAccountRemoteUserName account
          , arkhamDBAccountCreatedAt = arkhamDBAccountCreatedAt account
          }
        if saved then pure tokens.accessToken else useConcurrentRefresh linkChanged
  refreshCurrent `catchError` \err -> if arkhamDBErrorCode err == "tokenExpired" then useConcurrentRefresh err else throwError err

withAccessToken :: MonadIO m => ArkhamDBStore m -> ArkhamDBClient m -> ArkhamDBSettings -> UTCTime -> UserId -> (Text -> m (Either ArkhamDBClientError a)) -> ArkhamDBAction m a
withAccessToken store client settings now userId action = do
  token <- accessTokenFor store client settings now userId
  lift (action token) >>= \case
    Right result -> pure result
    Left ArkhamDBUnauthorized -> do
      account <- linkedAccount store userId
      current <- decryptStored settings $ arkhamDBAccountEncryptedAccessToken account
      fresh <- if current /= token && not (isTokenExpired now $ arkhamDBAccountExpiresAt account)
        then pure current
        else refreshAccount store client settings now account
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

-- The account row id is a generation: relink replaces that row, refresh does not.
-- Persistence checks it under the same transaction lock used by unlink/relink.
accountGeneration :: Monad m => ArkhamDBStore m -> UserId -> ArkhamDBAction m ArkhamDBAccountId
accountGeneration store userId = lift (store.findAccountGeneration userId) >>= maybe (throwError linkChanged) pure

linkChanged :: ArkhamDBError
linkChanged = ArkhamDBError "notLinked" "ArkhamDB account link changed; retry after linking"

publicDeckUrl :: ArkhamDBSettings -> Text -> Text
publicDeckUrl settings remoteId = T.dropWhileEnd (== '/') settings.arkhamDBBaseUrl <> "/api/public/deck/" <> remoteId

remoteDecklist :: ArkhamDBSettings -> ArkhamDBRemoteDeck -> ArkhamDBDecklist
remoteDecklist settings remote = remote.deck {Decklist.url = Just $ publicDeckUrl settings remote.remoteDeckId}

importRemoteDeck :: MonadIO m => ArkhamDBStore m -> ArkhamDBClient m -> ArkhamDBSettings -> UTCTime -> UserId -> Text -> (ArkhamDeckId -> m (Entity ArkhamDeck)) -> ArkhamDBAction m (Entity ArkhamDeck)
importRemoteDeck store client settings now userId remoteId publicSync = do
  links <- lift $ store.findDeckLinks userId
  case find ((== remoteId) . arkhamDBDeckLinkRemoteDeckId . snd) links of
    Just (deckId, _) -> syncRemoteDeck store client settings now userId deckId publicSync
    Nothing -> do
      generation <- accountGeneration store userId
      remote <- withAccessToken store client settings now userId (\token -> client.loadDeck token remoteId)
      let decklist = remoteDecklist settings remote
          deck = ArkhamDeck userId decklist.url (fromMaybe decklist.investigator_name decklist.decklist_name) decklist.investigator_name decklist Nothing Nothing
      lift $ store.validateDeck $ arkhamDeckPlayList deck
      lift (store.saveImportedDeck generation deck remote now) >>= maybe (throwError linkChanged) pure

-- | Missing or inaccessible mappings use the existing public URL sync path.
-- Both paths return the same local deck response and never expose tokens.
syncRemoteDeck :: MonadIO m => ArkhamDBStore m -> ArkhamDBClient m -> ArkhamDBSettings -> UTCTime -> UserId -> ArkhamDeckId -> (ArkhamDeckId -> m (Entity ArkhamDeck)) -> ArkhamDBAction m (Entity ArkhamDeck)
syncRemoteDeck store client settings now userId deckId publicSync = do
  (deck, link) <- lift (store.findDeck deckId) >>= maybe (throwError $ ArkhamDBError "notFound" "Deck not found") pure
  unless (arkhamDeckUserId deck == userId) $ throwError $ ArkhamDBError "notFound" "Deck not found"
  case link of
    Nothing -> lift $ publicSync deckId
    Just mapping -> do
      generation <- accountGeneration store userId
      loaded <- (Right <$> withAccessToken store client settings now userId (\token -> client.loadDeck token (arkhamDBDeckLinkRemoteDeckId mapping))) `catchError` (pure . Left)
      case loaded of
        Left err | arkhamDBErrorCode err == "notFound" -> do
          dropped <- lift $ store.dropDeckLink generation userId deckId
          unless dropped $ throwError linkChanged
          lift $ publicSync deckId
        Left err -> throwError err
        Right remote -> do
          let decklist = remoteDecklist settings remote
              updated = deck
                { arkhamDeckList = decklist
                , arkhamDeckUrl = decklist.url
                , arkhamDeckName = fromMaybe (arkhamDeckName deck) remote.deck.decklist_name
                , arkhamDeckInvestigatorName = remote.deck.investigator_name
                -- Write-back snapshots already contain the campaign overlay.
                , arkhamDeckOverlay = if materializedCampaignDeck remote.deck then Nothing else arkhamDeckOverlay deck
                }
          lift $ store.validateDeck $ arkhamDeckPlayList updated
          saved <- lift $ store.saveSyncedDeck generation deckId updated remote now
          unless saved $ throwError linkChanged
          pure $ Entity deckId updated

materializedCampaignDeck :: ArkhamDBDecklist -> Bool
materializedCampaignDeck deck = case deck.meta >>= decodeStrict' . encodeUtf8 of
  Just (Object metadata) -> case KM.lookup "ahn_push" metadata of
    Just (String marker) -> not $ T.null marker
    _ -> False
  _ -> False

-- The same adopted overlay must be returned and persisted by linked sync.
syncedDeckUpdates :: ArkhamDeck -> [Update ArkhamDeck]
syncedDeckUpdates deck =
  [ ArkhamDeckList =. arkhamDeckList deck
  , ArkhamDeckUrl =. arkhamDeckUrl deck
  , ArkhamDeckName =. arkhamDeckName deck
  , ArkhamDeckInvestigatorName =. arkhamDeckInvestigatorName deck
  , ArkhamDeckOverlay =. arkhamDeckOverlay deck
  ]

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
