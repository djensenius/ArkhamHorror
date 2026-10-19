{-# LANGUAGE NoFieldSelectors #-}

module Api.Handler.Arkham.ArkhamDB (
  getApiV1ArkhamArkhamDBCallbackR,
  getApiV1ArkhamArkhamDBDecksR,
  getApiV1ArkhamArkhamDBStatusR,
  postApiV1ArkhamArkhamDBDeckImportR,
  postApiV1ArkhamArkhamDBDeckSyncR,
  postApiV1ArkhamArkhamDBLinkStartR,
  deleteApiV1ArkhamArkhamDBLinkR,
) where

import Api.Handler.Arkham.CustomCards (registerUserCustomCards)
import Api.Handler.Arkham.Decks (postApiV1ArkhamSyncDeckR, toDeckErrors)
import Arkham.ArkhamDB
import Arkham.Decklist (ArkhamDBDecklist (..))
import Crypto.Random (getRandomBytes)
import Data.Aeson (Value, toJSON)
import Data.ByteString.Base64.URL qualified as B64Url
import Data.Map.Strict qualified as Map
import Data.Text qualified as T
import Data.Time.Clock (UTCTime, addUTCTime, getCurrentTime)
import Database.Esqueleto.Experimental hiding (Value, delete, from, get, isNothing, on, update, (=.), (==.))
import Database.Persist qualified as P
import Database.Persist.Sql (SqlBackend)
import Import hiding (delete, on, update, (=.), (==.))
import Network.HTTP.Types.Status qualified as Status

newtype JSONError = JSONError {errorMsg :: Text}
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)

requireConfigured :: Handler ArkhamDBSettings
requireConfigured = getsYesod (appArkhamDB . appSettings) >>= \case
  Just settings -> pure settings
  Nothing -> sendStatusJSON Status.status501 (JSONError "ArkhamDB account linking is not configured")

arkhamDBRedirectUri :: ArkhamDBSettings -> Text
arkhamDBRedirectUri settings = T.dropWhileEnd (== '/') settings.arkhamDBPublicServerBase <> "/api/v1/arkham/arkhamdb/callback"

configuredClient :: ArkhamDBSettings -> Handler (ArkhamDBClient Handler)
configuredClient settings = do
  manager <- getsYesod appHttpManager
  pure $ httpArkhamDBClient manager settings.arkhamDBBaseUrl settings.arkhamDBClientId settings.arkhamDBClientSecret

randomNonce :: MonadIO m => m Text
randomNonce = decodeUtf8 . B64Url.encodeUnpadded <$> liftIO (getRandomBytes 32)

postApiV1ArkhamArkhamDBLinkStartR :: Handler ArkhamDBLinkStartResponse
postApiV1ArkhamArkhamDBLinkStartR = do
  settings <- requireConfigured
  userId <- getRequestUserId
  now <- liftIO getCurrentTime
  nonce <- randomNonce
  let expiresAt = addUTCTime (10 * 60) now
      redirectUri = arkhamDBRedirectUri settings
      authorizeUrl = arkhamDBAuthorizeUrl settings.arkhamDBBaseUrl settings.arkhamDBClientId redirectUri nonce
  runDB $ insert_ $ ArkhamDBLinkState nonce userId expiresAt now
  pure ArkhamDBLinkStartResponse {..}

getApiV1ArkhamArkhamDBCallbackR :: Handler Html
getApiV1ArkhamArkhamDBCallbackR = do
  settings <- getsYesod (appArkhamDB . appSettings) >>= maybe (sendStatus Status.status404 ("ArkhamDB account linking is not configured" :: Text)) pure
  state <- requireGetParam "state"
  code <- requireGetParam "code"
  appRedirect <- lookupGetParam "appRedirect"
  now <- liftIO getCurrentTime
  mState <- runDB $ getBy $ UniqueArkhamDBLinkStateNonce state
  userId <- case validateLinkState now state mState of
    Left err -> renderCallbackError appRedirect err
    Right uid -> do
      runDB $ deleteBy $ UniqueArkhamDBLinkStateNonce state
      pure uid
  client <- configuredClient settings
  tokens <- exchangeCode client code (arkhamDBRedirectUri settings) >>= either (renderCallbackError appRedirect . clientErrorMessage) pure
  storeTokens settings now userId tokens
  renderCallbackSuccess appRedirect

getApiV1ArkhamArkhamDBStatusR :: Handler ArkhamDBLinkStatusResponse
getApiV1ArkhamArkhamDBStatusR = do
  _ <- requireConfigured
  userId <- getRequestUserId
  mAccount <- runDB $ getBy $ UniqueArkhamDBAccountUser userId
  case mAccount of
    Nothing -> pure $ ArkhamDBLinkStatusResponse False Nothing Nothing Nothing Nothing 0
    Just (Entity _ account) -> do
      count <- runDB $ countWriteBackDecks userId
      pure $ ArkhamDBLinkStatusResponse True (Just account.remoteUserId) account.remoteUserName account.expiresAt (Just account.createdAt) count

deleteApiV1ArkhamArkhamDBLinkR :: Handler ()
deleteApiV1ArkhamArkhamDBLinkR = do
  _ <- requireConfigured
  userId <- getRequestUserId
  runDB do
    deckIds <- selectKeysList [ArkhamDeckUserId P.==. userId] []
    unless (null deckIds) $ deleteWhere [ArkhamDBDeckLinkDeckId P.<-. deckIds]
    deleteBy $ UniqueArkhamDBAccountUser userId

getApiV1ArkhamArkhamDBDecksR :: Handler [ArkhamDBDeckSummary]
getApiV1ArkhamArkhamDBDecksR = do
  settings <- requireConfigured
  userId <- getRequestUserId
  client <- configuredClient settings
  accessToken <- requireAccessToken settings client userId
  summaries <- listDecksWithRefresh settings client userId accessToken
  deckIds <- runDB $ selectKeysList [ArkhamDeckUserId P.==. userId] []
  links <- runDB $ if null deckIds then pure [] else selectList [ArkhamDBDeckLinkDeckId P.<-. deckIds] []
  let byRemote = Map.fromList [(arkhamDBDeckLinkRemoteDeckId link, deckId) | Entity _ link <- links, let deckId = arkhamDBDeckLinkDeckId link]
      attachLocal summary = summary {linkedLocalDeckId = Map.lookup summary.id byRemote}
  pure $ map attachLocal summaries

postApiV1ArkhamArkhamDBDeckImportR :: Handler (Entity ArkhamDeck)
postApiV1ArkhamArkhamDBDeckImportR = do
  settings <- requireConfigured
  userId <- getRequestUserId
  ArkhamDBImportRequest remoteDeckId <- requireCheckJsonBody
  registerUserCustomCards userId
  client <- configuredClient settings
  accessToken <- requireAccessToken settings client userId
  remote <- loadDeckWithRefresh settings client userId accessToken remoteDeckId
  now <- liftIO getCurrentTime
  let decklist = remote.deck {url = remote.deck.url <|> Just (settings.arkhamDBBaseUrl <> "/deck/view/" <> remoteDeckId)}
      deck = ArkhamDeck userId decklist.url (fromMaybe (tshow decklist.investigator_name) decklist.decklist_name) (tshow decklist.investigator_name) decklist Nothing Nothing
  case toDeckErrors decklist of
    [] -> runDB do
      deckEntity@(Entity deckId _) <- insertEntity deck
      insert_ $ ArkhamDBDeckLink deckId remote.remoteDeckId remote.dateUpdate remote.version False Nothing now now
      pure deckEntity
    err -> sendStatusJSON Status.status400 err

postApiV1ArkhamArkhamDBDeckSyncR :: ArkhamDeckId -> Handler Value
postApiV1ArkhamArkhamDBDeckSyncR deckId = do
  settings <- requireConfigured
  userId <- getRequestUserId
  deck <- runDB $ get404 deckId
  unless (arkhamDeckUserId deck == userId) $ sendStatusJSON Status.status400 (JSONError "Deck does not belong to this user")
  runDB (getBy $ UniqueArkhamDBDeckLinkDeck deckId) >>= \case
    Nothing -> toJSON <$> postApiV1ArkhamSyncDeckR deckId
    Just (Entity linkId link) -> do
      client <- configuredClient settings
      accessToken <- requireAccessToken settings client userId
      remote <- loadDeckWithRefresh settings client userId accessToken link.remoteDeckId
      registerUserCustomCards userId
      case toDeckErrors remote.deck of
        [] -> do
          now <- liftIO getCurrentTime
          let updated = deck {arkhamDeckList = remote.deck, arkhamDeckUrl = remote.deck.url <|> arkhamDeckUrl deck}
          runDB do
            P.update deckId [ArkhamDeckList P.=. remote.deck, ArkhamDeckUrl P.=. arkhamDeckUrl updated]
            P.update linkId
              [ ArkhamDBDeckLinkLastSyncedDateUpdate P.=. remote.dateUpdate
              , ArkhamDBDeckLinkLastSyncedVersion P.=. remote.version
              , ArkhamDBDeckLinkLastError P.=. Nothing
              , ArkhamDBDeckLinkUpdatedAt P.=. now
              ]
          pure $ toJSON $ ArkhamDBSyncResult deckId remote.remoteDeckId now remote.version remote.dateUpdate
        err -> sendStatusJSON Status.status400 err

storeTokens :: ArkhamDBSettings -> UTCTime -> UserId -> ArkhamDBOAuthTokens -> Handler ()
storeTokens settings now userId tokens = do
  encryptedAccessToken <- unTokenCiphertext <$> encryptToken settings.arkhamDBTokenKey tokens.accessToken
  encryptedRefreshToken <- traverse (fmap unTokenCiphertext . encryptToken settings.arkhamDBTokenKey) tokens.refreshToken
  runDB do
    deleteBy $ UniqueArkhamDBAccountUser userId
    insert_ $ ArkhamDBAccount userId tokens.remoteUserId tokens.remoteUserName encryptedAccessToken encryptedRefreshToken tokens.expiresAt now now

requireAccessToken :: ArkhamDBSettings -> ArkhamDBClient Handler -> UserId -> Handler Text
requireAccessToken settings client userId = do
  now <- liftIO getCurrentTime
  account <- runDB (getBy $ UniqueArkhamDBAccountUser userId) >>= maybe (sendStatusJSON Status.status401 $ JSONError "ArkhamDB account is not linked") pure
  let accountVal = entityVal account
  if isTokenExpired now accountVal.expiresAt
    then refreshAccount settings client (entityKey account) accountVal
    else decryptStoredToken settings.arkhamDBTokenKey accountVal.encryptedAccessToken

refreshAccount :: ArkhamDBSettings -> ArkhamDBClient Handler -> ArkhamDBAccountId -> ArkhamDBAccount -> Handler Text
refreshAccount settings client accountId account = case account.encryptedRefreshToken of
  Nothing -> sendStatusJSON Status.status401 (JSONError "ArkhamDB access token expired; relink required")
  Just encrypted -> do
    refreshToken <- decryptStoredToken settings.arkhamDBTokenKey encrypted
    refreshTokens client refreshToken >>= \case
      Left err -> sendClientError err
      Right tokens -> do
        now <- liftIO getCurrentTime
        encryptedAccessToken <- unTokenCiphertext <$> encryptToken settings.arkhamDBTokenKey tokens.accessToken
        encryptedRefreshToken <- traverse (fmap unTokenCiphertext . encryptToken settings.arkhamDBTokenKey) (tokens.refreshToken <|> Just refreshToken)
        runDB $ P.update accountId
          [ ArkhamDBAccountEncryptedAccessToken P.=. encryptedAccessToken
          , ArkhamDBAccountEncryptedRefreshToken P.=. encryptedRefreshToken
          , ArkhamDBAccountExpiresAt P.=. tokens.expiresAt
          , ArkhamDBAccountRemoteUserId P.=. tokens.remoteUserId
          , ArkhamDBAccountRemoteUserName P.=. tokens.remoteUserName
          , ArkhamDBAccountUpdatedAt P.=. now
          ]
        pure tokens.accessToken

listDecksWithRefresh :: ArkhamDBSettings -> ArkhamDBClient Handler -> UserId -> Text -> Handler [ArkhamDBDeckSummary]
listDecksWithRefresh settings client userId accessToken =
  listDecks client accessToken >>= \case
    Right decks -> pure decks
    Left ArkhamDBUnauthorized -> do
      account <- runDB (getBy $ UniqueArkhamDBAccountUser userId) >>= maybe (sendStatusJSON Status.status401 $ JSONError "ArkhamDB account is not linked") pure
      fresh <- refreshAccount settings client (entityKey account) (entityVal account)
      listDecks client fresh >>= either sendClientError pure
    Left err -> sendClientError err

loadDeckWithRefresh :: ArkhamDBSettings -> ArkhamDBClient Handler -> UserId -> Text -> Text -> Handler ArkhamDBRemoteDeck
loadDeckWithRefresh settings client userId accessToken remoteDeckId =
  loadDeck client accessToken remoteDeckId >>= \case
    Right deck -> pure deck
    Left ArkhamDBUnauthorized -> do
      account <- runDB (getBy $ UniqueArkhamDBAccountUser userId) >>= maybe (sendStatusJSON Status.status401 $ JSONError "ArkhamDB account is not linked") pure
      fresh <- refreshAccount settings client (entityKey account) (entityVal account)
      loadDeck client fresh remoteDeckId >>= either sendClientError pure
    Left err -> sendClientError err

sendClientError :: ArkhamDBClientError -> Handler a
sendClientError = sendStatusJSON Status.status502 . JSONError . clientErrorMessage

clientErrorMessage :: ArkhamDBClientError -> Text
clientErrorMessage = \case
  ArkhamDBUnauthorized -> "ArkhamDB rejected the stored token; relink required"
  ArkhamDBRemoteError message -> "ArkhamDB request failed: " <> message
  ArkhamDBDecodeError message -> "ArkhamDB returned an unexpected response: " <> message

decryptStoredToken :: Text -> Text -> Handler Text
decryptStoredToken key token = either (sendStatusJSON Status.status500 . JSONError) pure $ decryptToken key (TokenCiphertext token)

countWriteBackDecks :: UserId -> ReaderT SqlBackend Handler Int
countWriteBackDecks userId = do
  deckIds <- selectKeysList [ArkhamDeckUserId P.==. userId] []
  if null deckIds then pure 0 else count [ArkhamDBDeckLinkDeckId P.<-. deckIds, ArkhamDBDeckLinkWriteBack P.==. True]

requireGetParam :: Text -> Handler Text
requireGetParam name = lookupGetParam name >>= maybe (sendStatus Status.status400 ("Missing required query parameter" :: Text)) pure

renderCallbackSuccess :: Maybe Text -> Handler Html
renderCallbackSuccess appRedirect = case appRedirect >>= whitelistedAppRedirect of
  Just target -> redirect target
  Nothing -> sendResponseStatus Status.status200 (toHtml ("<html><body>ArkhamDB account linked. You may close this window.</body></html>" :: Text))

renderCallbackError :: Maybe Text -> Text -> Handler a
renderCallbackError appRedirect message = case appRedirect >>= whitelistedAppRedirect of
  Just target -> redirect $ target <> "?error=" <> message
  Nothing -> sendResponseStatus Status.status400 (toHtml $ "<html><body>ArkhamDB linking failed: " <> message <> "</body></html>")

whitelistedAppRedirect :: Text -> Maybe Text
whitelistedAppRedirect target = guard ("arkhamhorror://arkhamdb-linked" `T.isPrefixOf` target) $> target
