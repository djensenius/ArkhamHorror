module Api.Handler.Arkham.ArkhamDB (
  getApiV1ArkhamArkhamDBCallbackR,
  getApiV1ArkhamArkhamDBDecksR,
  getApiV1ArkhamArkhamDBStatusR,
  postApiV1ArkhamArkhamDBDeckImportR,
  postApiV1ArkhamArkhamDBDeckSyncR,
  postApiV1ArkhamArkhamDBLinkStartR,
  postApiV1ArkhamArkhamDBLinkCompleteR,
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
import Database.Persist.Sql (Single, SqlBackend, rawSql, updateWhereCount)
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
    "oauthCompletionInvalid" -> Status.status403
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
      lockArkhamDBUser $ arkhamDBLinkStateUserId state
      deleteWhere [ArkhamDBLinkStateExpiresAt P.<=. arkhamDBLinkStateCreatedAt state]
      deleteWhere [ArkhamDBLinkStateUserId P.==. arkhamDBLinkStateUserId state]
      insert_ state
  , claimLinkState = \now nonce -> runDB do
      rows <- rawSql "UPDATE arkhamdb_link_states SET claimed_at = ? WHERE nonce = ? AND claimed_at IS NULL AND expires_at > ? RETURNING ??"
        [PersistUTCTime now, PersistText nonce, PersistUTCTime now]
      pure $ viaNonEmpty head rows
  , findAccountGeneration = \userId -> fmap entityKey <$> runDB (getBy $ UniqueArkhamDBAccountUser userId)
  , findAccount = \userId -> fmap entityVal <$> runDB (getBy $ UniqueArkhamDBAccountUser userId)
  , stagePendingLink = \now nonce digest encrypted -> runDB do
      affected <- updateWhereCount
        [ArkhamDBLinkStateNonce P.==. nonce, ArkhamDBLinkStateClaimedAt P.!=. Nothing, ArkhamDBLinkStateExpiresAt P.>. now]
        [ArkhamDBLinkStateCompletionHash P.=. Just digest, ArkhamDBLinkStateEncryptedPendingTokens P.=. Just encrypted]
      pure $ affected == 1
  , findPendingLink = \now userId digest -> fmap entityVal <$> runDB (selectFirst
      [ArkhamDBLinkStateUserId P.==. userId, ArkhamDBLinkStateCompletionHash P.==. Just digest, ArkhamDBLinkStateExpiresAt P.>. now] [])
  , finalizeLink = \now userId digest account -> runDB do
      lockArkhamDBUser userId
      claimed <- rawSql "DELETE FROM arkhamdb_link_states WHERE user_id = ? AND completion_hash = ? AND encrypted_pending_tokens IS NOT NULL AND claimed_at IS NOT NULL AND expires_at > ? RETURNING ??"
        [toPersistValue userId, PersistText digest, PersistUTCTime now]
      if null (claimed :: [Entity ArkhamDBLinkState])
        then pure False
        else do
          deleteWhere [ArkhamDBLinkStateUserId P.==. userId]
          deleteBy $ UniqueArkhamDBAccountUser userId
          insert_ account
          pure True
  , updateAccount = \old updated -> runDB do
      lockArkhamDBUser $ arkhamDBAccountUserId old
      affected <- updateWhereCount
        [ ArkhamDBAccountUserId P.==. arkhamDBAccountUserId old
        , ArkhamDBAccountEncryptedAccessToken P.==. arkhamDBAccountEncryptedAccessToken old
        ] (accountUpdates updated)
      pure $ affected == 1
  , unlinkAccount = \userId -> runDB do
      lockArkhamDBUser userId
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
  , saveImportedDeck = \generation deck remote now -> runDB do
      active <- sameAccountGeneration (arkhamDeckUserId deck) generation
      if not active then pure Nothing else do
        existing <- getBy $ UniqueArkhamDBDeckLinkUserRemote (arkhamDeckUserId deck) remote.remoteDeckId
        case existing of
          Just (Entity _ link) -> fmap (Entity $ arkhamDBDeckLinkDeckId link) <$> P.get (arkhamDBDeckLinkDeckId link)
          Nothing -> do
            entity@(Entity deckId _) <- insertEntity deck
            insert_ $ ArkhamDBDeckLink deckId (arkhamDeckUserId deck) remote.remoteDeckId remote.dateUpdate remote.deckVersion False Nothing now now
            pure $ Just entity
  , saveSyncedDeck = \generation deckId deck remote now -> runDB do
      active <- sameAccountGeneration (arkhamDeckUserId deck) generation
      mapping <- getBy $ UniqueArkhamDBDeckLinkDeck deckId
      if not active || isNothing mapping then pure False else do
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
        pure True
  , dropDeckLink = \generation userId deckId -> runDB do
      active <- sameAccountGeneration userId generation
      when active $ deleteBy $ UniqueArkhamDBDeckLinkDeck deckId
      pure active
  , validateDeck = \decklist -> case toDeckErrors decklist of
      [] -> pure ()
      errors -> sendStatusJSON Status.status400 errors
  }

-- All account lifecycle and deck writes take the same user lock, including
-- when no account exists. A relink inserts a fresh account id (generation).
lockArkhamDBUser :: UserId -> ReaderT SqlBackend Handler ()
lockArkhamDBUser userId = void (rawSql "SELECT id FROM users WHERE id = ? FOR UPDATE" [toPersistValue userId] :: ReaderT SqlBackend Handler [Single Int64])

sameAccountGeneration :: UserId -> ArkhamDBAccountId -> ReaderT SqlBackend Handler Bool
sameAccountGeneration userId generation = do
  lockArkhamDBUser userId
  current <- getBy $ UniqueArkhamDBAccountUser userId
  pure $ fmap entityKey current == Just generation

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
  nonce <- randomSecret
  runArkhamDB $ startLink persistentStore settings now userId nonce

getApiV1ArkhamArkhamDBCallbackR :: Handler Html
getApiV1ArkhamArkhamDBCallbackR = do
  settings <- requireConfigured
  nonce <- lookupGetParam "state"
  code <- lookupGetParam "code"
  oauthError <- lookupGetParam "error"
  web <- (== Just "1") <$> lookupGetParam "web"
  result <- case (oauthError, nonce, code) of
    (Just "access_denied", _, _) -> pure $ Left "access_denied"
    (Nothing, Just state, Just authorizationCode) -> do
      client <- configuredClient
      now <- liftIO getCurrentTime
      secret <- randomSecret
      runExceptT (completeLink persistentStore client settings now state authorizationCode secret) <&> \case
        Left _ -> Left "link_failed"
        Right () -> Right secret
    _ -> pure $ Left "link_failed"
  if web
    then pure $ toHtml ("Return to the Arkham Horror app to continue linking." :: Text)
    else redirectWith Status.status302 $ callbackLocation result

postApiV1ArkhamArkhamDBLinkCompleteR :: Handler ()
postApiV1ArkhamArkhamDBLinkCompleteR = do
  settings <- requireConfigured
  userId <- getRequestUserId
  ArkhamDBLinkCompleteRequest secret <- requireCheckJsonBody
  now <- liftIO getCurrentTime
  runArkhamDB $ finalizePendingLink persistentStore settings now userId secret

randomSecret :: Handler Text
randomSecret = decodeUtf8 . B64Url.encodeUnpadded <$> liftIO (getRandomBytes 32)

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
  runArkhamDB $ importRemoteDeck persistentStore client settings now userId remoteId postApiV1ArkhamSyncDeckR

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
