module Api.Handler.Arkham.ArkhamDB (
  getApiV1ArkhamArkhamDBCallbackR,
  arkhamDBCallback,
  runArkhamDB,
  getApiV1ArkhamArkhamDBDecksR,
  getApiV1ArkhamArkhamDBStatusR,
  postApiV1ArkhamArkhamDBDeckImportR,
  postApiV1ArkhamArkhamDBDeckSyncR,
  postApiV1ArkhamArkhamDBLinkStartR,
  postApiV1ArkhamArkhamDBLinkCompleteR,
  deleteApiV1ArkhamArkhamDBLinkR,
  putApiV1ArkhamArkhamDBWriteBackR,
  postApiV1ArkhamArkhamDBWriteBackRetryR,
  postApiV1ArkhamArkhamDBResolveConflictR,
) where

import Api.Arkham.ArkhamDBStore qualified as Store
import Api.Arkham.ArkhamDBWriteBack (writeBackStore, dispatchCampaignWriteBack)
import Arkham.ArkhamDB.WriteBack
import Api.Handler.Arkham.CustomCards (registerUserCustomCards)
import Api.Handler.Arkham.Decks (postApiV1ArkhamSyncDeckR, toDeckErrors)
import Arkham.ArkhamDB
import Arkham.ArkhamDB.Service
import Crypto.Random (getRandomBytes)
import Data.ByteString.Base64.URL qualified as B64Url
import Data.Time.Clock (getCurrentTime)
import Database.PostgreSQL.Simple (sqlState)
import Import hiding (state)
import Network.HTTP.Types.Status qualified as Status
import UnliftIO.Exception (catch, throwIO)

runArkhamDB :: (MonadHandler m, MonadUnliftIO m) => ArkhamDBAction m a -> m a
runArkhamDB action = (runExceptT action >>= either sendArkhamDBError pure) `catch` \err ->
  if sqlState err == "55P03"
    then sendArkhamDBError $ ArkhamDBError "remoteUnavailable" "ArkhamDB request failed"
    else throwIO err

sendArkhamDBError :: MonadHandler m => ArkhamDBError -> m a
sendArkhamDBError err = sendStatusJSON (arkhamDBErrorStatus err) err

requireConfigured :: Handler ArkhamDBSettings
requireConfigured = runArkhamDB . configuredSettings =<< getsYesod (appArkhamDB . appSettings)

configuredClient :: Handler (ArkhamDBClient Handler)
configuredClient = getsYesod appArkhamDBClient >>= maybe
  (sendArkhamDBError $ ArkhamDBError "disabled" "ArkhamDB account linking is not configured")
  (pure . hoistArkhamDBClient liftIO)

persistentStore :: ArkhamDBStore Handler
persistentStore = Store.persistentStore \decklist -> case toDeckErrors decklist of
  [] -> pure ()
  errors -> sendStatusJSON Status.status400 errors

postApiV1ArkhamArkhamDBLinkStartR :: Handler ArkhamDBLinkStartResponse
postApiV1ArkhamArkhamDBLinkStartR = do
  settings <- requireConfigured
  userId <- getRequestUserId
  now <- liftIO getCurrentTime
  nonce <- randomSecret
  runArkhamDB $ startLink persistentStore settings now userId nonce

getApiV1ArkhamArkhamDBCallbackR :: Handler Html
getApiV1ArkhamArkhamDBCallbackR = do
  settings <- requireConfigured
  arkhamDBCallback persistentStore configuredClient settings

-- | The production callback, with persistence and transport supplied explicitly
-- so its HTTP response can be tested without a PostgreSQL connection.
arkhamDBCallback :: MonadHandler m => ArkhamDBStore m -> m (ArkhamDBClient m) -> ArkhamDBSettings -> m Html
arkhamDBCallback store getClient settings = do
  nonce <- lookupGetParam "state"
  code <- lookupGetParam "code"
  oauthError <- lookupGetParam "error"
  result <- case (oauthError, nonce, code) of
    (Just "access_denied", _, _) -> pure $ Left "access_denied"
    (Nothing, Just state, Just authorizationCode) -> do
      client <- getClient
      now <- liftIO getCurrentTime
      secret <- randomSecret
      runExceptT (completeLink store client settings now state authorizationCode secret) <&> \case
        Left _ -> Left "link_failed"
        Right () -> Right secret
    _ -> pure $ Left "link_failed"
  redirectWith Status.status302 $ callbackLocation result

postApiV1ArkhamArkhamDBLinkCompleteR :: Handler ()
postApiV1ArkhamArkhamDBLinkCompleteR = do
  settings <- requireConfigured
  userId <- getRequestUserId
  ArkhamDBLinkCompleteRequest secret <- requireCheckJsonBody
  now <- liftIO getCurrentTime
  runArkhamDB $ finalizePendingLink persistentStore settings now userId secret

randomSecret :: MonadIO m => m Text
randomSecret = decodeUtf8 . B64Url.encodeUnpadded <$> liftIO (getRandomBytes 32)

getApiV1ArkhamArkhamDBStatusR :: Handler ArkhamDBStatusResponse
getApiV1ArkhamArkhamDBStatusR = do
  _ <- requireConfigured
  userId <- getRequestUserId
  account <- runArkhamDB $ linkStatus persistentStore userId
  ArkhamDBStatusResponse account <$> writeBackStore.deckWriteBackStatus userId

deleteApiV1ArkhamArkhamDBLinkR :: Handler ()
deleteApiV1ArkhamArkhamDBLinkR = do
  _ <- requireConfigured
  userId <- getRequestUserId
  runArkhamDB $ unlinkLinkedAccount persistentStore userId

getApiV1ArkhamArkhamDBDecksR :: Handler [ArkhamDBDeckSummary]
getApiV1ArkhamArkhamDBDecksR = do
  settings <- requireConfigured
  userId <- getRequestUserId
  client <- configuredClient
  now <- liftIO getCurrentTime
  runArkhamDB $ listRemoteDecks persistentStore client settings now userId

postApiV1ArkhamArkhamDBDeckImportR :: Handler (Entity ArkhamDeck)
postApiV1ArkhamArkhamDBDeckImportR = do
  settings <- requireConfigured
  userId <- getRequestUserId
  ArkhamDBImportRequest remoteId <- requireCheckJsonBody
  registerUserCustomCards userId
  client <- configuredClient
  now <- liftIO getCurrentTime
  runArkhamDB $ importRemoteDeck persistentStore client settings now userId remoteId postApiV1ArkhamSyncDeckR

postApiV1ArkhamArkhamDBDeckSyncR :: ArkhamDeckId -> Handler (Entity ArkhamDeck)
postApiV1ArkhamArkhamDBDeckSyncR deckId = do
  settings <- requireConfigured
  userId <- getRequestUserId
  registerUserCustomCards userId
  client <- configuredClient
  now <- liftIO getCurrentTime
  runArkhamDB $ syncRemoteDeck persistentStore client settings now userId deckId postApiV1ArkhamSyncDeckR

-- These routes manipulate durable work only; remote calls happen in a worker.
putApiV1ArkhamArkhamDBWriteBackR :: ArkhamDeckId -> Handler [ArkhamDBDeckWriteBackStatus]
putApiV1ArkhamArkhamDBWriteBackR deckId = do
  _ <- requireConfigured
  userId <- getRequestUserId
  ArkhamDBWriteBackToggleRequest enabled <- requireCheckJsonBody
  now <- liftIO getCurrentTime
  saved <- writeBackStore.toggleWriteBack now userId deckId enabled
  unless saved notFound
  writeBackStore.deckWriteBackStatus userId

postApiV1ArkhamArkhamDBWriteBackRetryR :: ArkhamDeckId -> Handler [ArkhamDBDeckWriteBackStatus]
postApiV1ArkhamArkhamDBWriteBackRetryR deckId = do
  _ <- requireConfigured
  userId <- getRequestUserId
  now <- liftIO getCurrentTime
  ownedWriteBackDeck userId deckId
  writeBackStore.requestPushRetry now userId deckId
  dispatchCampaignWriteBack
  writeBackStore.deckWriteBackStatus userId

postApiV1ArkhamArkhamDBResolveConflictR :: ArkhamDeckId -> Handler [ArkhamDBDeckWriteBackStatus]
postApiV1ArkhamArkhamDBResolveConflictR deckId = do
  _ <- requireConfigured
  userId <- getRequestUserId
  ArkhamDBResolveRequest resolution _ <- requireCheckJsonBody
  now <- liftIO getCurrentTime
  ownedWriteBackDeck userId deckId
  let mode = case resolution of
        "keepRemote" -> "keep"
        "overwrite" -> "overwrite"
        _ -> "new"
  saved <- writeBackStore.resolvePush now userId deckId mode
  unless saved $ sendStatusJSON Status.status409 $ ArkhamDBError "conflict" "No resolvable push; retry uncertain work before overwriting"
  dispatchCampaignWriteBack
  writeBackStore.deckWriteBackStatus userId

ownedWriteBackDeck :: UserId -> ArkhamDeckId -> Handler ()
ownedWriteBackDeck userId deckId = do
  found <- persistentStore.findDeck deckId
  case found of
    Just (deck, Just link) | arkhamDeckUserId deck == userId ->
      unless (arkhamDBDeckLinkWriteBack link) $ sendStatusJSON Status.status409 $ ArkhamDBError "writeBackDisabled" "Deck write-back is disabled"
    _ -> notFound
