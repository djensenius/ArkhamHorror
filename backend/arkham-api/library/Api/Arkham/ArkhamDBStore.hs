module Api.Arkham.ArkhamDBStore where

import Arkham.ArkhamDB
import Arkham.ArkhamDB.Service
import Arkham.Decklist (ArkhamDBDecklist)
import Database.Persist qualified as P
import Database.Persist.Sql (Single, SqlBackend, rawExecute, rawSql, updateWhereCount)
import Import hiding (state)

-- | Each runDB action is a transaction, including nonce consumption and the
-- paired local deck/link writes. Claiming prevents concurrent callbacks from
-- exchanging a state twice; finalizing refuses a claim removed by unlink.
persistentStore :: (ArkhamDBDecklist -> Handler ()) -> ArkhamDBStore Handler
persistentStore validate = persistentStoreWith validate runDB

-- The locked callback reuses this connection, never borrowing another pool
-- connection (which could deadlock behind waiting refresh transactions).
persistentStoreWith :: (ArkhamDBDecklist -> Handler ()) -> (forall a. ReaderT SqlBackend Handler a -> Handler a) -> ArkhamDBStore Handler
persistentStoreWith validate database = ArkhamDBStore
  { withRefreshLock = \userId action -> database do
      -- Bound cross-process contention before waiting on the user row. This
      -- setting is transaction-local and is reset before pool reuse.
      rawExecute "SET LOCAL lock_timeout = '5s'" []
      lockArkhamDBUser userId
      connection <- ask
      lift $ action $ persistentStoreWith validate (`runReaderT` connection)
  , saveLinkState = \state -> database do
      lockArkhamDBUser $ arkhamDBLinkStateUserId state
      deleteWhere [ArkhamDBLinkStateExpiresAt P.<=. arkhamDBLinkStateCreatedAt state]
      deleteWhere [ArkhamDBLinkStateUserId P.==. arkhamDBLinkStateUserId state]
      insert_ state
  , claimLinkState = \now nonce -> database do
      rows <- rawSql "UPDATE arkhamdb_link_states SET claimed_at = ? WHERE nonce = ? AND claimed_at IS NULL AND expires_at > ? RETURNING ??"
        [PersistUTCTime now, PersistText nonce, PersistUTCTime now]
      pure $ viaNonEmpty head rows
  , findAccountGeneration = \userId -> fmap entityKey <$> database (getBy $ UniqueArkhamDBAccountUser userId)
  , findAccount = \userId -> fmap entityVal <$> database (getBy $ UniqueArkhamDBAccountUser userId)
  , stagePendingLink = \now nonce digest encrypted -> database do
      affected <- updateWhereCount
        [ArkhamDBLinkStateNonce P.==. nonce, ArkhamDBLinkStateClaimedAt P.!=. Nothing, ArkhamDBLinkStateExpiresAt P.>. now]
        [ArkhamDBLinkStateCompletionHash P.=. Just digest, ArkhamDBLinkStateEncryptedPendingTokens P.=. Just encrypted]
      pure $ affected == 1
  , findPendingLink = \now userId digest -> fmap entityVal <$> database (selectFirst
      [ArkhamDBLinkStateUserId P.==. userId, ArkhamDBLinkStateCompletionHash P.==. Just digest, ArkhamDBLinkStateExpiresAt P.>. now] [])
  , finalizeLink = \now userId digest account -> database do
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
  , updateAccount = \old updated -> database do
      lockArkhamDBUser $ arkhamDBAccountUserId old
      affected <- updateWhereCount
        [ ArkhamDBAccountUserId P.==. arkhamDBAccountUserId old
        , ArkhamDBAccountEncryptedAccessToken P.==. arkhamDBAccountEncryptedAccessToken old
        ] (accountUpdates updated)
      pure $ affected == 1
  , unlinkAccount = \userId -> database do
      lockArkhamDBUser userId
      deckIds <- selectKeysList [ArkhamDeckUserId P.==. userId] []
      unless (null deckIds) $ deleteWhere [ArkhamDBDeckLinkDeckId P.<-. deckIds]
      deleteWhere [ArkhamDBLinkStateUserId P.==. userId]
      deleteBy $ UniqueArkhamDBAccountUser userId
  , findDeckLinks = \userId -> database do
      deckIds <- selectKeysList [ArkhamDeckUserId P.==. userId] []
      links <- if null deckIds then pure [] else selectList [ArkhamDBDeckLinkDeckId P.<-. deckIds] []
      pure [(arkhamDBDeckLinkDeckId link, link) | Entity _ link <- links]
  , findDeck = \deckId -> database do
      P.get deckId >>= \case
        Just deck -> do
          link <- fmap entityVal <$> getBy (UniqueArkhamDBDeckLinkDeck deckId)
          pure $ Just (deck, link)
        _ -> pure Nothing
  , saveImportedDeck = \generation deck remote now -> database do
      active <- sameAccountGeneration (arkhamDeckUserId deck) generation
      if not active then pure Nothing else do
        existing <- getBy $ UniqueArkhamDBDeckLinkUserRemote (arkhamDeckUserId deck) remote.remoteDeckId
        case existing of
          Just (Entity _ link) -> fmap (Entity $ arkhamDBDeckLinkDeckId link) <$> P.get (arkhamDBDeckLinkDeckId link)
          Nothing -> do
            entity@(Entity deckId _) <- insertEntity deck
            insert_ $ ArkhamDBDeckLink deckId (arkhamDeckUserId deck) remote.remoteDeckId remote.dateUpdate remote.deckVersion False Nothing Nothing now now
            pure $ Just entity
  , saveSyncedDeck = \generation deckId deck remote now -> database do
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
  , dropDeckLink = \generation userId deckId -> database do
      active <- sameAccountGeneration userId generation
      when active $ deleteBy $ UniqueArkhamDBDeckLinkDeck deckId
      pure active
  , validateDeck = validate
  }

-- All account lifecycle and deck writes take the same user lock, including
-- when no account exists. NO KEY UPDATE allows unrelated foreign-key inserts.
-- A relink inserts a fresh account id (generation).
lockArkhamDBUser :: forall m. MonadIO m => UserId -> ReaderT SqlBackend m ()
lockArkhamDBUser userId = void (rawSql "SELECT id FROM users WHERE id = ? FOR NO KEY UPDATE" [toPersistValue userId] :: ReaderT SqlBackend m [Single Int64])

sameAccountGeneration :: MonadIO m => UserId -> ArkhamDBAccountId -> ReaderT SqlBackend m Bool
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
