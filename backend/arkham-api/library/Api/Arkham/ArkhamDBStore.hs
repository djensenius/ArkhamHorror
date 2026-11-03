module Api.Arkham.ArkhamDBStore where

import Arkham.ArkhamDB
import Arkham.ArkhamDB.Service
import Arkham.ArkhamDB.Campaign
import Arkham.Decklist (ArkhamDBDecklist)
import Data.Aeson (decodeStrict')
import Data.Map.Strict qualified as Map
import Data.Text qualified as T
import Database.Persist qualified as P
import Database.Persist.Sql (Single (..), SqlBackend, rawExecute, rawSql, updateWhereCount)
import Import hiding (state)

-- | Each runDB action is a transaction, including nonce consumption and the
-- paired local deck/link writes. Claiming prevents concurrent callbacks from
-- exchanging a state twice; finalizing refuses a claim removed by unlink.
persistentStore :: (ArkhamDBDecklist -> ArkhamDBAction Handler ()) -> ArkhamDBStore Handler
persistentStore validate = persistentStoreWith validate runDB

-- The locked callback reuses this connection, never borrowing another pool
-- connection (which could deadlock behind waiting refresh transactions).
persistentStoreWith :: (ArkhamDBDecklist -> ArkhamDBAction Handler ()) -> (forall a. ReaderT SqlBackend Handler a -> Handler a) -> ArkhamDBStore Handler
persistentStoreWith validate database = ArkhamDBStore
  { withRefreshLock = \userId action -> database do
      -- Bound cross-process contention before waiting on the user row. This
      -- setting is transaction-local and is reset before pool reuse.
      rawExecute "SET LOCAL lock_timeout = '5s'" []
      lockArkhamDBUser userId
      connection <- ask
      lift $ action $ persistentStoreWith validate (`runReaderT` connection)
  , ownsPushClaim = \(Entity key job) -> database do
      matching <- P.count
        [ ArkhamDBPushId P.==. key, ArkhamDBPushStatus P.==. "running"
        , ArkhamDBPushStage P.==. arkhamDBPushStage job, ArkhamDBPushUpdatedAt P.==. arkhamDBPushUpdatedAt job
        , ArkhamDBPushCampaignKey P.==. arkhamDBPushCampaignKey job
        , ArkhamDBPushDeckId P.==. arkhamDBPushDeckId job, ArkhamDBPushUserId P.==. arkhamDBPushUserId job
        ]
      pure $ arkhamDBPushStatus job == "running" && matching == 1
  , renewPushClaim = \time (Entity key job) -> database do
      let renewedAt = max time $ arkhamDBPushUpdatedAt job
      -- Identity was admitted with this claim. Check its immutable key in the
      -- CAS, not every historical game's multi-MB JSON on every HTTP renewal.
      affected <- if arkhamDBPushStatus job /= "running" then pure 0 else updateWhereCount
        [ ArkhamDBPushId P.==. key, ArkhamDBPushStatus P.==. "running"
        , ArkhamDBPushStage P.==. arkhamDBPushStage job, ArkhamDBPushUpdatedAt P.==. arkhamDBPushUpdatedAt job
        , ArkhamDBPushGeneration P.==. arkhamDBPushGeneration job
        , ArkhamDBPushCampaignKey P.==. arkhamDBPushCampaignKey job
        , ArkhamDBPushDeckId P.==. arkhamDBPushDeckId job, ArkhamDBPushUserId P.==. arkhamDBPushUserId job
        ]
        [ArkhamDBPushUpdatedAt P.=. renewedAt]
      pure $ if affected == 1 then Just $ Entity key job {arkhamDBPushUpdatedAt = renewedAt} else Nothing
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
  , saveSyncedDeck = \generation deckId expected remote now adopt -> database do
      let userId = arkhamDBDeckLinkUserId expected
      active <- sameAccountGeneration userId generation
      -- Overlay PUT/DELETE only lock this row, not the account. No pre-I/O
      -- overlay or adoption decision may cross this transaction boundary.
      rows <- rawSql "SELECT ?? FROM arkham_decks WHERE id = ? FOR UPDATE"
        [toPersistValue deckId] :: ReaderT SqlBackend Handler [Entity ArkhamDeck]
      mapping <- getBy $ UniqueArkhamDBDeckLinkDeck deckId
      case (active, viaNonEmpty head rows, mapping) of
        (True, Just (Entity _ current), Just (Entity _ link))
          | arkhamDeckUserId current == userId
          , arkhamDBDeckLinkUserId link == userId
          , sameSyncedDeckLink expected link
          , arkhamDBDeckLinkRemoteDeckId link == remote.remoteDeckId -> do
              prepared <- lift $ adopt current
              case prepared of
                Left err -> pure $ Left err
                Right updated -> do
                  P.update deckId $ syncedDeckUpdates updated
                  updateWhere [ArkhamDBDeckLinkDeckId P.==. deckId]
                    [ ArkhamDBDeckLinkLastSyncedDateUpdate P.=. remote.dateUpdate
                    , ArkhamDBDeckLinkLastSyncedVersion P.=. remote.deckVersion
                    , ArkhamDBDeckLinkLastError P.=. Nothing
                    , ArkhamDBDeckLinkUpdatedAt P.=. now
                    ]
                  -- Return the row read back under the same lock, not the
                  -- callback's projection (which may contain unwritten fields).
                  Right . Entity deckId <$> getJust deckId
        _ -> pure $ Left linkChanged
  , dropDeckLink = \generation userId deckId -> database do
      active <- sameAccountGeneration userId generation
      when active $ deleteBy $ UniqueArkhamDBDeckLinkDeck deckId
      pure active
  , validateDeck = validate
  }

-- One batched JSON projection for legacy candidate games only. PostgreSQL may
-- still detoast current_data; never do this on per-request/checkpoint renewal.
-- These has sibling This/That keys, like This-only campaign intermissions.
loadCampaignIdentities
  :: forall m. MonadIO m
  => Map ArkhamGameId CampaignGameIdentity -> [Text] -> ReaderT SqlBackend m (Map ArkhamGameId CampaignGameIdentity)
loadCampaignIdentities = loadCampaignIdentitiesWith loadGames
 where
  loadGames gameIds = do
    rows <- rawSql
      ("SELECT id, json_build_object('id', current_data #> '{gameMode,This,id}', 'meta', " <>
       "json_build_object('campaignMode', current_data #> '{gameMode,This,meta,campaignMode}', " <>
       "'currentCampaignMode', current_data #> '{gameMode,This,meta,currentCampaignMode}'))::text " <>
       "FROM arkham_games WHERE id IN (" <> T.intercalate "," (replicate (length gameIds) "?") <> ")")
      (map toPersistValue gameIds) :: ReaderT SqlBackend m [(Single ArkhamGameId, Single Text)]
    pure $ Map.fromList [(gid, maybe MalformedCampaignGame campaignGameIdentityFromJSON $ decodeStrict' $ encodeUtf8 encoded)
      | (Single gid, Single encoded) <- rows]

-- Admission alone checks related history: an existing ambiguous full-DE
-- association may own this remote lineage's XP even if the live URL/link moved.
-- Deleted-game leftovers are inert in campaignAdmissionAllowed. Also inspect
-- the exact legacy seat key before a part-qualified job starts a fresh baseline.
writeBackCampaignIdentities
  :: forall m. MonadIO m
  => Map ArkhamGameId CampaignGameIdentity -> ArkhamDeckId -> Text
  -> ReaderT SqlBackend m (Map ArkhamGameId CampaignGameIdentity, [Text])
writeBackCampaignIdentities known deckId own = do
  let legacy = T.intercalate "/" $ take 2 $ T.splitOn "/" own
  rows <- rawSql
    ("SELECT campaign_key FROM arkhamdb_campaign_decks WHERE deck_id = ? OR campaign_key = ? " <>
     "UNION SELECT campaign_key FROM arkhamdb_deck_links WHERE (deck_id = ? OR campaign_key = ?) AND campaign_key IS NOT NULL " <>
     "UNION SELECT campaign_key FROM arkhamdb_pushes WHERE (deck_id = ? OR campaign_key = ?) AND status NOT IN ('succeeded', 'cancelled')")
    (concat $ replicate 3 [toPersistValue deckId, toPersistValue legacy]) :: ReaderT SqlBackend m [Single Text]
  let related = [key | Single key <- rows]
  games <- loadCampaignIdentities known $ own : related
  pure (games, related)

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
