{-# LANGUAGE NoFieldSelectors #-}
{-# LANGUAGE DuplicateRecordFields #-}

module Arkham.ArkhamDB.WriteBack where

import Arkham.ArkhamDB
import Arkham.ArkhamDB.Service
import Arkham.Decklist (ArkhamDBDecklist)
import Arkham.Decklist qualified as Decklist
import Control.Concurrent (ThreadId, myThreadId, modifyMVar, modifyMVar_, killThread)
import System.Timeout (timeout)
import Control.Monad.Except
import Data.Aeson
import Data.Aeson.KeyMap qualified as KM
import Data.Time.Clock (UTCTime, addUTCTime, getCurrentTime)
import Data.Text qualified as T
import Data.UUID qualified as UUID
import Database.Persist (Entity (..), Update, (=.))
import Entity.Arkham.ArkhamDBAccount
import Entity.Arkham.Deck
import Entity.User
import Relude
import Settings (ArkhamDBSettings)

-- One worker per application/process, with a single coalescing wake-up slot.
-- Neither gameplay boundaries nor retries allocate a worker while it is running.
data ArkhamDBExecutor = ArkhamDBExecutor
  { executorStarted :: MVar Bool
  , executorWake :: MVar ()
  , executorThread :: MVar (Maybe ThreadId)
  }

newArkhamDBExecutor :: IO ArkhamDBExecutor
newArkhamDBExecutor = ArkhamDBExecutor <$> newMVar False <*> newEmptyMVar <*> newMVar Nothing

wakeArkhamDBExecutor :: ArkhamDBExecutor -> IO Bool
wakeArkhamDBExecutor executor = modifyMVar executor.executorStarted \started -> do
  void $ tryPutMVar executor.executorWake ()
  pure (True, not started)

runArkhamDBExecutor :: MonadIO m => ArkhamDBExecutor -> m () -> m ()
runArkhamDBExecutor executor sweep = do
  tid <- liftIO myThreadId
  liftIO $ modifyMVar_ executor.executorThread $ const $ pure $ Just tid
  forever do
    -- Poll durable pending work after restart and throttled claims, even without
    -- another gameplay boundary. Coalesced signals make active work wake sooner.
    _ <- liftIO $ timeout 30000000 $ takeMVar executor.executorWake
    sweep

stopArkhamDBExecutor :: ArkhamDBExecutor -> IO ()
stopArkhamDBExecutor executor = readMVar executor.executorThread >>= traverse_ killThread

resetArkhamDBExecutor :: ArkhamDBExecutor -> IO ()
resetArkhamDBExecutor executor = do
  modifyMVar_ executor.executorThread $ const $ pure Nothing
  modifyMVar_ executor.executorStarted $ const $ pure False

writeBackRetryDelay :: Int -> Int
writeBackRetryDelay failures = min 300 $ 30 * (2 ^ min 4 (max 0 failures))

writeBackAdmissionReady :: UTCTime -> ArkhamDBAccount -> Bool
writeBackAdmissionReady now account = maybe True
  (<= addUTCTime (negate $ fromIntegral $ writeBackRetryDelay $ arkhamDBAccountWriteBackFailures account) now)
  (arkhamDBAccountLastWriteBackAttemptAt account)

-- Claim serializes a deck's oldest outstanding push. Each checkpoint commits
-- independently, BEFORE its remote side effect. A crashed creation is recovered,
-- never repeated. Every write checks account generation and the claim's version.
data ArkhamDBWriteBackStore m = ArkhamDBWriteBackStore
  { claimPush :: UTCTime -> Bool -> UserId -> ArkhamDeckId -> m (Maybe (Entity ArkhamDBPush))
  , requestPushRetry :: UTCTime -> UserId -> ArkhamDeckId -> m ()
  , checkpointPush :: Entity ArkhamDBPush -> ArkhamDBPush -> m Bool
  , finishPush :: Entity ArkhamDBPush -> ArkhamDBRemoteDeck -> m (Maybe ArkhamDeckId)
  , keepRemoteDeck :: Entity ArkhamDBPush -> ArkhamDBRemoteDeck -> m Bool
  , resolvePush :: UTCTime -> UserId -> ArkhamDeckId -> Text -> m Bool
  , toggleWriteBack :: UTCTime -> UserId -> ArkhamDeckId -> Bool -> m Bool
  , deckWriteBackStatus :: UserId -> m [ArkhamDBDeckWriteBackStatus]
  , campaignOwnsTarget :: Text -> ArkhamDBRemoteDeck -> m Bool
  , campaignXpBaseline :: Text -> m Int
  }

data ArkhamDBConflict = ArkhamDBConflict
  { reason :: Text
  , lastSyncedDateUpdate :: Maybe Text
  , lastSyncedVersion :: Maybe Text
  , remoteDateUpdate :: Maybe Text
  , remoteVersion :: Maybe Text
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON)

data ArkhamDBDeckWriteBackStatus = ArkhamDBDeckWriteBackStatus
  { localDeckId :: ArkhamDeckId
  , remoteDeckId :: Text
  , enabled :: Bool
  , spentXpSynced :: Bool
  , state :: Text
  , pushId :: Maybe ArkhamDBPushId
  , campaignKey :: Maybe Text
  , error :: Maybe Text
  , conflict :: Maybe ArkhamDBConflict
  , resultingLocalDeckId :: Maybe ArkhamDeckId
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON)

data ArkhamDBStatusResponse = ArkhamDBStatusResponse ArkhamDBLinkStatusResponse [ArkhamDBDeckWriteBackStatus]

instance ToJSON ArkhamDBStatusResponse where
  toJSON (ArkhamDBStatusResponse account decks) = case toJSON account of
    Object value -> Object $ KM.insert "deckLinks" (toJSON decks) value
    value -> value

newtype ArkhamDBWriteBackToggleRequest = ArkhamDBWriteBackToggleRequest {enabled :: Bool}
  deriving stock (Generic)
  deriving anyclass (FromJSON, ToJSON)

data ArkhamDBResolveRequest = ArkhamDBResolveRequest {resolution :: Text, confirmed :: Bool}
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON)

instance FromJSON ArkhamDBResolveRequest where
  parseJSON = withObject "ArkhamDBResolveRequest" \o -> do
    resolution <- o .: "resolution"
    unless (resolution `elem` ["keepRemote", "overwrite", "saveAsNew"]) $ fail "Unknown conflict resolution"
    confirmed <- o .:? "confirmed" .!= False
    when (resolution /= "keepRemote" && not confirmed) $ fail "Explicit confirmation required"
    pure ArkhamDBResolveRequest {..}

pushStatus :: ArkhamDeckId -> ArkhamDBDeckLink -> Maybe (Entity ArkhamDBPush) -> Maybe ArkhamDeckId -> ArkhamDBDeckWriteBackStatus
pushStatus deckId link pending resulting = ArkhamDBDeckWriteBackStatus
  { localDeckId = deckId
  , remoteDeckId = arkhamDBDeckLinkRemoteDeckId link
  , enabled = arkhamDBDeckLinkWriteBack link
  , spentXpSynced = False
  , state = maybe "idle" (arkhamDBPushStatus . entityVal) pending
  , pushId = entityKey <$> pending
  , campaignKey = arkhamDBPushCampaignKey . entityVal <$> pending
  , error = pending >>= arkhamDBPushError . entityVal
  , conflict = do
      Entity _ job <- pending
      guard $ arkhamDBPushStatus job == "conflict"
      pure $ ArkhamDBConflict (fromMaybe "remoteChanged" $ arkhamDBPushError job)
        (arkhamDBPushExpectedDateUpdate job) (arkhamDBPushExpectedVersion job)
        (arkhamDBPushRemoteDateUpdate job) (arkhamDBPushRemoteVersion job)
  , resultingLocalDeckId = resulting
  }

pushCanRun :: UTCTime -> Bool -> ArkhamDBPush -> Bool
pushCanRun now retry job = arkhamDBPushStatus job == "pending" ||
  (retry && arkhamDBPushStatus job == "failed") ||
  (arkhamDBPushStatus job == "running" && arkhamDBPushUpdatedAt job < addUTCTime (-60) now)

ownsCampaignDeck :: UserId -> ArkhamDeck -> ArkhamDBDeckLink -> Bool
ownsCampaignDeck uid deck link = arkhamDeckUserId deck == uid && arkhamDBDeckLinkUserId link == uid && arkhamDBDeckLinkWriteBack link

canResolvePush :: Text -> ArkhamDBPush -> Bool
canResolvePush mode job = arkhamDBPushStatus job `elem` ["conflict", "failed"] && mode `elem` ["overwrite", "new", "keep"] &&
  (mode /= "overwrite" || (arkhamDBPushStage job /= "creating" && isNothing (arkhamDBPushTargetId job) && arkhamDBPushError job /= Just "campaignChanged"))

-- The accepted remote list is materialized, including any campaign overlay.
keepRemoteDeckUpdates :: ArkhamDBRemoteDeck -> [Update ArkhamDeck]
keepRemoteDeckUpdates remote =
  [ ArkhamDeckList =. remote.deck
  , ArkhamDeckUrl =. remote.deck.url
  , ArkhamDeckName =. fromMaybe remote.deck.investigator_name remote.deck.decklist_name
  , ArkhamDeckInvestigatorName =. remote.deck.investigator_name
  , ArkhamDeckOverlay =. Nothing
  ]

-- Insert with a fresh primary key, but retain this boundary's position ahead
-- of snapshots queued after it. The event key identifies the new creation.
saveAsNewPush :: UTCTime -> ArkhamDBPush -> ArkhamDBPush
saveAsNewPush now job = job
  { arkhamDBPushEventKey = arkhamDBPushEventKey job <> "/new/" <> T.pack (show now)
  , arkhamDBPushStatus = "pending", arkhamDBPushStage = "ready", arkhamDBPushMode = "new"
  , arkhamDBPushSourceId = Nothing, arkhamDBPushTargetId = Nothing, arkhamDBPushError = Nothing
  , arkhamDBPushUpdatedAt = now
  }

campaignCopyDeck :: ArkhamDeck -> ArkhamDBRemoteDeck -> ArkhamDeck
campaignCopyDeck deck remote = deck
  { arkhamDeckName = arkhamDeckName deck <> " (campaign copy)"
  , arkhamDeckList = remote.deck
  , arkhamDeckUrl = remote.deck.url
  -- The campaign snapshot already includes the overlay; never apply it twice.
  , arkhamDeckOverlay = Nothing
  }

needsCampaignCopy :: ArkhamDBDeckLink -> ArkhamDBPush -> Bool
needsCampaignCopy link job = arkhamDBPushMode job == "new" &&
  maybe False (/= arkhamDBPushCampaignKey job) (arkhamDBDeckLinkCampaignKey link)

encodePushDeck :: ArkhamDBDecklist -> Text
encodePushDeck = decodeUtf8 . toStrict . encode

pushMarker :: ArkhamDBPushId -> Text
pushMarker = UUID.toText . coerce

-- Preserve every existing metadata key. A non-object meta is not silently lost.
markedDeck :: ArkhamDBPushId -> ArkhamDBDecklist -> Either ArkhamDBError ArkhamDBDecklist
markedDeck key deck = do
  metadata <- case Decklist.meta deck of
    Nothing -> Right KM.empty
    Just text -> case eitherDecodeStrict' $ encodeUtf8 text of
      Right (Object value) -> Right value
      _ -> Left $ ArkhamDBError "validationFailed" "Deck metadata must be a JSON object"
  pure deck {Decklist.meta = Just $ encodePushDeckMeta $ Object $ KM.insert "ahn_push" (String $ pushMarker key) metadata}
 where
  encodePushDeckMeta = decodeUtf8 . toStrict . encode

hasPushMarker :: ArkhamDBPushId -> ArkhamDBRemoteDeck -> Bool
hasPushMarker key remote = do
  let metadata = Decklist.meta remote.deck >>= decodeStrict' . encodeUtf8
  case metadata of
    Just (Object value) -> KM.lookup "ahn_push" value == Just (String $ pushMarker key)
    _ -> False

sameMarkers :: Maybe Text -> Maybe Text -> ArkhamDBRemoteDeck -> Bool
sameMarkers date version remote = date == remote.dateUpdate && version == remote.deckVersion

unmarkedDeck :: ArkhamDBDecklist -> ArkhamDBDecklist
unmarkedDeck deck = deck {Decklist.meta = case deck.meta >>= decodeStrict' . encodeUtf8 of
  Just (Object value) -> let clean = KM.delete "ahn_push" value
    in if KM.null clean then Nothing else Just $ decodeUtf8 $ toStrict $ encode $ Object clean
  _ -> deck.meta}

samePushedDeck :: ArkhamDBDecklist -> ArkhamDBRemoteDeck -> Bool
samePushedDeck desired remote =
  desired.slots == remote.deck.slots && desired.sideSlots == remote.deck.sideSlots
    && desired.taboo_id == remote.deck.taboo_id && desired.investigator_code == remote.deck.investigator_code
    && (decodeStrict' . encodeUtf8 =<< desired.meta :: Maybe Value) == (decodeStrict' . encodeUtf8 =<< remote.deck.meta)
    && maybe True (\name -> Just name == remote.deck.decklist_name) desired.decklist_name

-- A single durable job, with no network work in the game transaction. Errors
-- become sanitized per-deck state; the caller never needs to fail a game answer.
runWriteBack :: forall m. MonadIO m => ArkhamDBStore m -> ArkhamDBWriteBackStore m -> ArkhamDBClient m -> ArkhamDBSettings -> UTCTime -> Bool -> UserId -> ArkhamDeckId -> m ()
runWriteBack store writes client settings now retry userId deckId = do
  claimed <- writes.claimPush now retry userId deckId
  for_ claimed \initial -> do
    latest <- newIORef initial
    let checkpoint entity proposed = do
          checkpointTime <- liftIO getCurrentTime
          let updated = proposed {arkhamDBPushUpdatedAt = checkpointTime}
          saved <- lift $ writes.checkpointPush entity updated
          unless saved $ throwError linkChanged
          let next = Entity (entityKey entity) updated
          writeIORef latest next
          pure next
        ensureGenerationUsing currentStore job = do
          generation <- accountGeneration currentStore userId
          unless (generation == arkhamDBPushGeneration job) $ throwError linkChanged
          (deck, mapping) <- lift (currentStore.findDeck deckId) >>= maybe (throwError linkChanged) pure
          unless (arkhamDeckUserId deck == userId) $ throwError linkChanged
          link <- maybe (throwError linkChanged) pure mapping
          unless (arkhamDBDeckLinkWriteBack link) $ throwError $ ArkhamDBError "writeBackDisabled" "Deck write-back is disabled"
          pure link
        ensureGeneration = ensureGenerationUsing store
        request :: forall a. ArkhamDBPush -> (Text -> m (Either ArkhamDBClientError a)) -> ArkhamDBAction m a
        request job action = ExceptT $ withRefreshLock store userId \locked -> runExceptT do
          -- Bind token selection and transport to this generation. Otherwise a
          -- relink between the preflight and token read could write to a NEW
          -- account using an OLD deck intent. Checkpoints still commit outside
          -- this short per-request transaction so lost replies are recoverable.
          current <- lift $ locked.ownsPushClaim $ Entity (entityKey initial) job
          unless current $ throwError linkChanged
          _ <- ensureGenerationUsing locked job
          let requestStore = locked {withRefreshLock = \_ run -> run requestStore}
          withAccessToken requestStore client settings now userId action
        load job remoteId = request job (\token -> client.loadDeck token remoteId)
        conflict entity remote why = void $ checkpoint entity (entityVal entity)
          { arkhamDBPushStatus = "conflict"
          , arkhamDBPushError = Just why
          , arkhamDBPushRemoteDateUpdate = remote.dateUpdate
          , arkhamDBPushRemoteVersion = remote.deckVersion
          }
        latestVersion job seen remote = case remote.nextDeckId of
          Nothing -> pure remote
          Just next | next `elem` seen || length seen >= 100 -> throwError $ ArkhamDBError "remoteUnavailable" "ArkhamDB version chain is invalid"
          Just next -> load job next >>= latestVersion job (next : seen)
        recover entity desired = do
          original <- traverse (load $ entityVal entity) $ arkhamDBPushSourceId $ entityVal entity
          case original of
            Just source | isNothing source.nextDeckId && not source.nextDeckKnownAbsent ->
              conflict entity source "pushUncertain"
            _ -> recoverVersion entity desired original
        recoverVersion entity desired originalSource = do
          let job = entityVal entity
              key = entityKey entity
          chainCandidate <- case originalSource of
            Nothing -> pure Nothing
            Just original -> do
              case original.nextDeckId of
                Nothing | isJust original.dateUpdate || isJust original.deckVersion
                  , sameMarkers (arkhamDBPushExpectedDateUpdate job) (arkhamDBPushExpectedVersion job) original -> do
                    _ <- checkpoint entity job {arkhamDBPushStage = "ready", arkhamDBPushSourceId = Nothing, arkhamDBPushTargetId = Nothing}
                    throwError $ ArkhamDBError "remoteUnavailable" "Previous upgrade did not create a version; retry is safe"
                Nothing -> pure Nothing
                Just next -> do
                  candidate <- load job next
                  pure $ if hasPushMarker key candidate then Just candidate else Nothing
          recovered <- case chainCandidate of
            Just value -> pure $ Just value
            Nothing -> do
              summaries <- request job client.listDecks
              -- Recovery is deliberately bounded. If the result is ambiguous or
              -- not discoverable, keep an uncertain failure rather than create twice.
              candidates <- forM (take 100 summaries) \summary -> load job summary.id
              pure $ case filter (hasPushMarker key) candidates of
                [value] -> Just value
                _ -> Nothing
          case recovered of
            Nothing -> throwError $ ArkhamDBError "pushUncertain" "ArkhamDB creation outcome is uncertain; no creation was retried"
            Just remote -> do
              next <- checkpoint entity job {arkhamDBPushStage = "created", arkhamDBPushTargetId = Just remote.remoteDeckId, arkhamDBPushExpectedDateUpdate = Nothing, arkhamDBPushExpectedVersion = Nothing}
              saveVersion False next desired
        saveVersion establishBaseline entity desired = do
          let job = entityVal entity
          target <- maybe (throwError $ ArkhamDBError "storageFailure" "Missing push target") pure $ arkhamDBPushTargetId job
          remote <- load job target
          campaignTarget <- lift $ writes.campaignOwnsTarget (arkhamDBPushCampaignKey job) remote
          if (not (hasPushMarker (entityKey entity) remote) && not campaignTarget) || isJust remote.nextDeckId
            then conflict entity remote "remoteChanged"
            else if samePushedDeck desired remote
              then finish entity remote
              else if not establishBaseline &&
                ((isNothing (arkhamDBPushExpectedDateUpdate job) && isNothing (arkhamDBPushExpectedVersion job)) ||
                  not (sameMarkers (arkhamDBPushExpectedDateUpdate job) (arkhamDBPushExpectedVersion job) remote))
                then conflict entity remote "remoteChanged"
                else do
                  next <- checkpoint entity job
                    { arkhamDBPushStage = "saving", arkhamDBPushExpectedDateUpdate = remote.dateUpdate, arkhamDBPushExpectedVersion = remote.deckVersion }
                  _ <- request (entityVal next) (\token -> client.saveDeck token target remote {deck = desired})
                  verified <- load (entityVal next) target
                  if samePushedDeck desired verified && isNothing verified.nextDeckId
                    then finish next verified
                    else conflict next verified "remoteChanged"
        finish entity remote = do
          _ <- ensureGeneration $ entityVal entity
          saved <- lift $ writes.finishPush entity remote {deck = remoteDecklist settings remote}
          when (isNothing saved) $ throwError linkChanged
        process entity = do
          let job = entityVal entity
          link <- ensureGeneration job
          if arkhamDBPushMode job == "keep" then do
            remote <- load job (arkhamDBDeckLinkRemoteDeckId link) >>= latestVersion job [arkhamDBDeckLinkRemoteDeckId link]
            saved <- lift $ writes.keepRemoteDeck entity remote {deck = remoteDecklist settings remote}
            unless saved $ throwError linkChanged
          else processDeck entity link
        processDeck entity link = do
          let job = entityVal entity
          unmarked <- either (const $ throwError $ ArkhamDBError "storageFailure" "Invalid pending deck") pure $ eitherDecodeStrict' $ encodeUtf8 $ arkhamDBPushPayload job
          desired <- either throwError pure $ markedDeck (entityKey entity) unmarked
          case arkhamDBPushStage job of
            "creating" -> recover entity desired
            "created" -> saveVersion False entity desired
            "saving" -> saveVersion False entity desired
            _ -> do
              ready <- checkpoint entity job
                { arkhamDBPushExpectedDateUpdate = arkhamDBDeckLinkLastSyncedDateUpdate link
                , arkhamDBPushExpectedVersion = arkhamDBDeckLinkLastSyncedVersion link
                }
              baseline <- lift $ writes.campaignXpBaseline (arkhamDBPushCampaignKey job)
              original <- load (entityVal ready) $ arkhamDBDeckLinkRemoteDeckId link
              campaignTarget <- lift $ writes.campaignOwnsTarget (arkhamDBPushCampaignKey job) original
              let otherCampaign = maybe False (/= arkhamDBPushCampaignKey job) $ arkhamDBDeckLinkCampaignKey link
              if otherCampaign && arkhamDBPushMode job /= "new"
                then conflict ready original "campaignChanged"
                else if arkhamDBPushMode job == "normal" &&
                  ((isNothing (arkhamDBDeckLinkLastSyncedDateUpdate link) && isNothing (arkhamDBDeckLinkLastSyncedVersion link)) || not (sameMarkers (arkhamDBDeckLinkLastSyncedDateUpdate link) (arkhamDBDeckLinkLastSyncedVersion link) original) || isJust original.nextDeckId)
                  then conflict ready original "remoteChanged"
                  else if arkhamDBPushMode job == "normal" && arkhamDBPushTotalXp job == baseline &&
                    samePushedDeck (unmarkedDeck desired) original {deck = unmarkedDeck original.deck}
                    then finish ready original
                  else if arkhamDBPushMode job == "normal" && arkhamDBPushBoundary job == "upgrade" &&
                    arkhamDBPushTotalXp job <= baseline && campaignTarget
                    then do
                      target <- checkpoint ready (entityVal ready)
                        { arkhamDBPushStage = "created", arkhamDBPushTargetId = Just original.remoteDeckId
                        , arkhamDBPushExpectedDateUpdate = original.dateUpdate, arkhamDBPushExpectedVersion = original.deckVersion }
                      saveVersion False target desired
                  else do
                    source <- if arkhamDBPushMode job == "overwrite" then latestVersion (entityVal ready) [original.remoteDeckId] original else pure original
                    next <- checkpoint ready (entityVal ready)
                      { arkhamDBPushStage = "creating"
                      , arkhamDBPushSourceId = if arkhamDBPushMode job == "new" then Nothing else Just source.remoteDeckId
                      , arkhamDBPushExpectedDateUpdate = source.dateUpdate
                      , arkhamDBPushExpectedVersion = source.deckVersion
                      }
                    target <- request (entityVal next) \token ->
                      if arkhamDBPushMode job == "new" then client.createDeck token desired
                      else client.upgradeDeck token source.remoteDeckId (max 0 $ arkhamDBPushTotalXp job - baseline) desired.meta
                    created <- checkpoint next (entityVal next) {arkhamDBPushStage = "created", arkhamDBPushTargetId = Just target, arkhamDBPushExpectedDateUpdate = Nothing, arkhamDBPushExpectedVersion = Nothing}
                    saveVersion True created desired
    result <- runExceptT $ process initial
    case result of
      Left err -> do
        current@(Entity _ job) <- readIORef latest
        void $ writes.checkpointPush current job {arkhamDBPushStatus = "failed", arkhamDBPushError = Just $ arkhamDBErrorCode err}
      Right () -> runWriteBack store writes client settings now False userId deckId
