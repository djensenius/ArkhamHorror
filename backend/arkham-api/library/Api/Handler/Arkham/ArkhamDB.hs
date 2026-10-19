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
import Arkham.ArkhamDB.Service
import Crypto.Random (getRandomBytes)
import Data.ByteString.Base64.URL qualified as B64Url
import Data.Time.Clock (getCurrentTime)
import Database.Persist qualified as P
import Database.Persist.Sql (rawSql, updateWhereCount)
import Import hiding (state)
import Network.HTTP.Types.Status qualified as Status

runArkhamDB :: ArkhamDBAction Handler a -> Handler a
runArkhamDB action = runExceptT action >>= either sendArkhamDBError pure

sendArkhamDBError :: ArkhamDBError -> Handler a
sendArkhamDBError err = sendStatusJSON status err
 where
  status = case arkhamDBErrorCode err of
    "disabled" -> Status.status501
    "oauthCallbackInvalidState" -> Status.status400
    "notLinked" -> Status.status401
    "tokenExpired" -> Status.status401
    "notFound" -> Status.status404
    "storageFailure" -> Status.status500
    _ -> Status.status502

requireConfigured :: Handler ArkhamDBSettings
requireConfigured = runArkhamDB . configuredSettings =<< getsYesod (appArkhamDB . appSettings)

configuredClient :: Handler (ArkhamDBClient Handler)
configuredClient = getsYesod appArkhamDBClient >>= maybe
  (sendArkhamDBError $ ArkhamDBError "disabled" "ArkhamDB account linking is not configured")
  (pure . hoistArkhamDBClient liftIO)

-- | Each runDB action is a transaction, including nonce consumption and the
-- paired local deck/link writes. Claiming prevents concurrent callbacks from
-- exchanging a state twice; finalizing refuses a claim removed by unlink.
persistentStore :: ArkhamDBStore Handler
persistentStore = ArkhamDBStore
  { saveLinkState = \state -> runDB do
      deleteWhere [ArkhamDBLinkStateExpiresAt P.<=. arkhamDBLinkStateCreatedAt state]
      deleteWhere [ArkhamDBLinkStateUserId P.==. arkhamDBLinkStateUserId state]
      insert_ state
  , claimLinkState = \now nonce -> runDB do
      rows <- rawSql "UPDATE arkhamdb_link_states SET claimed_at = ? WHERE nonce = ? AND claimed_at IS NULL AND expires_at > ? RETURNING ??"
        [PersistUTCTime now, PersistText nonce, PersistUTCTime now]
      pure $ viaNonEmpty head rows
  , findAccount = \userId -> fmap entityVal <$> runDB (getBy $ UniqueArkhamDBAccountUser userId)
  , finalizeLink = \nonce account -> runDB do
      claimed <- rawSql "DELETE FROM arkhamdb_link_states WHERE nonce = ? AND user_id = ? AND claimed_at IS NOT NULL RETURNING ??"
        [PersistText nonce, toPersistValue $ arkhamDBAccountUserId account]
      if null (claimed :: [Entity ArkhamDBLinkState])
        then pure False
        else do
          let userId = arkhamDBAccountUserId account
          deckIds <- selectKeysList [ArkhamDeckUserId P.==. userId] []
          unless (null deckIds) $ deleteWhere [ArkhamDBDeckLinkDeckId P.<-. deckIds]
          deleteWhere [ArkhamDBLinkStateUserId P.==. userId]
          void $ upsert account (accountUpdates account)
          pure True
  , updateAccount = \old updated -> runDB do
      affected <- updateWhereCount
        [ ArkhamDBAccountUserId P.==. arkhamDBAccountUserId old
        , ArkhamDBAccountEncryptedAccessToken P.==. arkhamDBAccountEncryptedAccessToken old
        ] (accountUpdates updated)
      pure $ affected == 1
  , unlinkAccount = \userId -> runDB do
      deckIds <- selectKeysList [ArkhamDeckUserId P.==. userId] []
      unless (null deckIds) $ deleteWhere [ArkhamDBDeckLinkDeckId P.<-. deckIds]
      deleteWhere [ArkhamDBLinkStateUserId P.==. userId]
      deleteBy $ UniqueArkhamDBAccountUser userId
  , findDeckLinks = \userId -> runDB do
      deckIds <- selectKeysList [ArkhamDeckUserId P.==. userId] []
      links <- if null deckIds then pure [] else selectList [ArkhamDBDeckLinkDeckId P.<-. deckIds] []
      pure [(arkhamDBDeckLinkDeckId link, link) | Entity _ link <- links]
  , findDeck = \deckId -> runDB do
      P.get deckId >>= \case
        Just deck -> do
          link <- fmap entityVal <$> getBy (UniqueArkhamDBDeckLinkDeck deckId)
          pure $ Just (deck, link)
        _ -> pure Nothing
  , saveImportedDeck = \deck remote now -> runDB do
      entity@(Entity deckId _) <- insertEntity deck
      insert_ $ ArkhamDBDeckLink deckId remote.remoteDeckId remote.dateUpdate remote.deckVersion False Nothing now now
      pure entity
  , saveSyncedDeck = \deckId deck remote now -> runDB do
      P.update deckId
        [ ArkhamDeckList P.=. arkhamDeckList deck
        , ArkhamDeckUrl P.=. arkhamDeckUrl deck
        , ArkhamDeckName P.=. arkhamDeckName deck
        , ArkhamDeckInvestigatorName P.=. arkhamDeckInvestigatorName deck
        ]
      updateWhere [ArkhamDBDeckLinkDeckId P.==. deckId]
        [ ArkhamDBDeckLinkLastSyncedDateUpdate P.=. remote.dateUpdate
        , ArkhamDBDeckLinkLastSyncedVersion P.=. remote.deckVersion
        , ArkhamDBDeckLinkLastError P.=. Nothing
        , ArkhamDBDeckLinkUpdatedAt P.=. now
        ]
  , validateDeck = \decklist -> case toDeckErrors decklist of
      [] -> pure ()
      errors -> sendStatusJSON Status.status400 errors
  }

accountUpdates :: ArkhamDBAccount -> [Update ArkhamDBAccount]
accountUpdates account =
  [ ArkhamDBAccountRemoteUserId P.=. arkhamDBAccountRemoteUserId account
  , ArkhamDBAccountRemoteUserName P.=. arkhamDBAccountRemoteUserName account
  , ArkhamDBAccountEncryptedAccessToken P.=. arkhamDBAccountEncryptedAccessToken account
  , ArkhamDBAccountEncryptedRefreshToken P.=. arkhamDBAccountEncryptedRefreshToken account
  , ArkhamDBAccountExpiresAt P.=. arkhamDBAccountExpiresAt account
  , ArkhamDBAccountUpdatedAt P.=. arkhamDBAccountUpdatedAt account
  ]

postApiV1ArkhamArkhamDBLinkStartR :: Handler ArkhamDBLinkStartResponse
postApiV1ArkhamArkhamDBLinkStartR = do
  settings <- requireConfigured
  userId <- getRequestUserId
  now <- liftIO getCurrentTime
  nonce <- decodeUtf8 . B64Url.encodeUnpadded <$> liftIO (getRandomBytes 32)
  runArkhamDB $ startLink persistentStore settings now userId nonce

getApiV1ArkhamArkhamDBCallbackR :: Handler Html
getApiV1ArkhamArkhamDBCallbackR = do
  settings <- requireConfigured
  nonce <- requireGetParam "state"
  code <- requireGetParam "code"
  client <- configuredClient
  now <- liftIO getCurrentTime
  runExceptT (completeLink persistentStore client settings now nonce code) >>= \case
    Left _ -> sendResponseStatus Status.status400 $ toHtml ("ArkhamDB linking failed. Please start again in the app." :: Text)
    Right () -> pure $ toHtml ("ArkhamDB account linked. You may close this window." :: Text)

getApiV1ArkhamArkhamDBStatusR :: Handler ArkhamDBLinkStatusResponse
getApiV1ArkhamArkhamDBStatusR = do
  _ <- requireConfigured
  userId <- getRequestUserId
  runArkhamDB $ linkStatus persistentStore userId

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
  runArkhamDB $ importRemoteDeck persistentStore client settings now userId remoteId

postApiV1ArkhamArkhamDBDeckSyncR :: ArkhamDeckId -> Handler (Entity ArkhamDeck)
postApiV1ArkhamArkhamDBDeckSyncR deckId = do
  settings <- requireConfigured
  userId <- getRequestUserId
  registerUserCustomCards userId
  client <- configuredClient
  now <- liftIO getCurrentTime
  runArkhamDB $ syncRemoteDeck persistentStore client settings now userId deckId postApiV1ArkhamSyncDeckR

requireGetParam :: Text -> Handler Text
requireGetParam name = lookupGetParam name >>= maybe (invalidArgs ["Missing query parameter: " <> name]) pure
