module Api.Arkham.ArkhamDBWriteBack where

import Api.Arkham.ArkhamDBStore
import Arkham.ArkhamDB
import Arkham.Card.CardCode (unCardCode)
import Arkham.Id (InvestigatorId (..), PlayerId (..))
import Arkham.ArkhamDB.Campaign (campaignDeckSnapshot)
import Arkham.ArkhamDB.WriteBack
import Arkham.Campaign.Types (CampaignAttrs (..))
import Arkham.Classes.Entity (toAttrs)
import Arkham.Helpers (Deck (..))
import Arkham.Decklist qualified as Decklist
import Arkham.Game (Game (..))
import Arkham.Game.Utils (gameInvestigators, modeCampaign)
import Arkham.Investigator.Types (InvestigatorAttrs (..))
import Data.Map.Strict qualified as Map
import Data.Text qualified as T
import Data.Time.Clock (UTCTime, getCurrentTime)
import Data.UUID qualified as UUID
import Database.Persist qualified as P
import Database.Persist.Sql (Single (..), rawSql, rawExecute, updateWhereCount)
import Import hiding (state)
import Control.Concurrent (forkIO)
import Control.Monad.Logger (logWarnN)
import UnliftIO.Exception (finally, try)

writeBackStore :: ArkhamDBWriteBackStore Handler
writeBackStore = ArkhamDBWriteBackStore
  { claimPush = \now retry uid deckId -> runDB do
      -- A busy user's remote request must not attract pool-sized lock waiters.
      rawExecute "SET LOCAL lock_timeout = '5s'" []
      available <- rawSql "SELECT id FROM users WHERE id = ? FOR NO KEY UPDATE SKIP LOCKED" [toPersistValue uid] :: DB [Single Int64]
      if null available then pure Nothing else do
        mapping <- getBy $ UniqueArkhamDBDeckLinkDeck deckId
        account <- getBy $ UniqueArkhamDBAccountUser uid
        case (mapping, account) of
          (Just (Entity _ link), Just (Entity generation accountValue))
            | arkhamDBDeckLinkUserId link == uid && arkhamDBDeckLinkWriteBack link && writeBackAdmissionReady now accountValue -> do
              pending <- oldestPush deckId
              case pending of
                Just (Entity key job) | pushCanRun now retry job && generation == arkhamDBPushGeneration job -> do
                  let claimed = job {arkhamDBPushStatus = "running", arkhamDBPushError = Nothing, arkhamDBPushUpdatedAt = now}
                  replace key claimed
                  P.update generation [ArkhamDBAccountLastWriteBackAttemptAt P.=. Just now]
                  pure $ Just $ Entity key claimed
                _ -> pure Nothing
          _ -> pure Nothing
  , requestPushRetry = \now uid deckId -> runDB do
      rawExecute "SET LOCAL lock_timeout = '5s'" []
      available <- rawSql "SELECT id FROM users WHERE id = ? FOR NO KEY UPDATE SKIP LOCKED" [toPersistValue uid] :: DB [Single Int64]
      unless (null available) do
        account <- getBy $ UniqueArkhamDBAccountUser uid
        pending <- oldestPush deckId
        case (account, pending) of
          (Just (Entity generation value), Just (Entity key job))
            | arkhamDBPushUserId job == uid && arkhamDBPushGeneration job == generation
            , writeBackAdmissionReady now value, pushCanRun now True job ->
                P.update key [ArkhamDBPushStatus P.=. "pending"]
          _ -> pure ()
  , checkpointPush = \previous updated -> runDB do
      active <- activePush previous
      if not active then pure False else do
        affected <- updateWhereCount (pushClaim previous) $ pushUpdates updated
        when (affected == 1 && arkhamDBPushStatus updated == "failed") $
          P.update (arkhamDBPushGeneration updated) [ArkhamDBAccountWriteBackFailures P.+=. 1]
        pure $ affected == 1
  , finishPush = \previous remote -> runDB do
      active <- activePush previous
      if not active then pure Nothing else do
        let Entity key job = previous
            deckId = arkhamDBPushDeckId job
        mapping <- getBy $ UniqueArkhamDBDeckLinkDeck deckId
        association <- getBy $ UniqueArkhamDBCampaignDeck (arkhamDBPushCampaignKey job)
        case (mapping, association) of
          (Just (Entity linkId link), Just (Entity associationId campaign)) -> do
            let crossCampaign = needsCampaignCopy link job
            result <- if crossCampaign then do
              deck <- get404 deckId
              -- A second campaign must not steal the first campaign's lineage.
              copyId <- insert $ campaignCopyDeck deck remote
              insert_ link
                { arkhamDBDeckLinkDeckId = copyId
                , arkhamDBDeckLinkRemoteDeckId = remote.remoteDeckId
                , arkhamDBDeckLinkLastSyncedDateUpdate = remote.dateUpdate
                , arkhamDBDeckLinkLastSyncedVersion = remote.deckVersion
                , arkhamDBDeckLinkCampaignKey = Just $ arkhamDBPushCampaignKey job
                , arkhamDBDeckLinkLastError = Nothing
                }
              P.update associationId [ArkhamDBCampaignDeckDeckId P.=. copyId, ArkhamDBCampaignDeckLastPushedXp P.=. arkhamDBPushTotalXp job]
              -- Move only this campaign's queued work to its new local link.
              updateWhere [ArkhamDBPushDeckId P.==. deckId, ArkhamDBPushCampaignKey P.==. arkhamDBPushCampaignKey job, ArkhamDBPushStatus P.==. "pending"]
                [ArkhamDBPushDeckId P.=. copyId]
              pure copyId
            else do
              replace linkId link
                { arkhamDBDeckLinkRemoteDeckId = remote.remoteDeckId
                , arkhamDBDeckLinkLastSyncedDateUpdate = remote.dateUpdate
                , arkhamDBDeckLinkLastSyncedVersion = remote.deckVersion
                , arkhamDBDeckLinkLastError = Nothing
                , arkhamDBDeckLinkCampaignKey = Just $ arkhamDBPushCampaignKey job
                , arkhamDBDeckLinkUpdatedAt = arkhamDBPushUpdatedAt job
                }
              replace associationId campaign {arkhamDBCampaignDeckLastPushedXp = arkhamDBPushTotalXp job}
              pure deckId
            P.update key [ArkhamDBPushStatus P.=. "succeeded", ArkhamDBPushError P.=. Nothing]
            P.update (arkhamDBPushGeneration job) [ArkhamDBAccountWriteBackFailures P.=. 0]
            pruneCompletedPushes deckId
            pure $ Just result
          _ -> pure Nothing
  , keepRemoteDeck = \previous remote -> runDB do
      active <- activePush previous
      if not active then pure False else do
        let job = entityVal previous
            deckId = arkhamDBPushDeckId job
        P.update deckId $ keepRemoteDeckUpdates remote
        updateWhere [ArkhamDBDeckLinkDeckId P.==. deckId]
          [ ArkhamDBDeckLinkRemoteDeckId P.=. remote.remoteDeckId
          , ArkhamDBDeckLinkLastSyncedDateUpdate P.=. remote.dateUpdate
          , ArkhamDBDeckLinkLastSyncedVersion P.=. remote.deckVersion
          , ArkhamDBDeckLinkLastError P.=. Nothing
          ]
        updateWhere [ArkhamDBPushDeckId P.==. deckId, ArkhamDBPushStatus P.<-. ["pending", "failed", "conflict", "running"]]
          [ArkhamDBPushStatus P.=. "cancelled", ArkhamDBPushError P.=. Nothing]
        updateWhere [ArkhamDBCampaignDeckCampaignKey P.==. arkhamDBPushCampaignKey job]
          [ArkhamDBCampaignDeckLastPushedXp P.=. arkhamDBPushTotalXp job]
        pruneCompletedPushes deckId
        pure True
  , resolvePush = \now uid deckId mode -> runDB do
      lockArkhamDBUser uid
      mapping <- getBy $ UniqueArkhamDBDeckLinkDeck deckId
      pending <- oldestPush deckId
      case (mapping, pending) of
        (Just (Entity _ link), Just (Entity key job))
          | arkhamDBDeckLinkUserId link == uid && arkhamDBDeckLinkWriteBack link
          , canResolvePush mode job -> do
              active <- sameAccountGeneration uid $ arkhamDBPushGeneration job
              if not active then pure False else do
                -- Never discard an uncertain creation intent and create again.
                -- Explicit save-as-new starts a fresh, independently identified push.
                if mode == "new" && arkhamDBPushStage job /= "ready" then do
                  P.update key [ArkhamDBPushStatus P.=. "cancelled"]
                  void $ insert $ saveAsNewPush now job
                else P.update key
                  [ ArkhamDBPushStatus P.=. "pending", ArkhamDBPushStage P.=. "ready", ArkhamDBPushMode P.=. mode
                  , ArkhamDBPushSourceId P.=. Nothing, ArkhamDBPushTargetId P.=. Nothing
                  , ArkhamDBPushError P.=. Nothing, ArkhamDBPushUpdatedAt P.=. now
                  ]
                pruneCompletedPushes deckId
                pure True
        _ -> pure False
  , toggleWriteBack = \now uid deckId enabled -> runDB do
      lockArkhamDBUser uid
      affected <- updateWhereCount [ArkhamDBDeckLinkDeckId P.==. deckId, ArkhamDBDeckLinkUserId P.==. uid]
        [ArkhamDBDeckLinkWriteBack P.=. enabled, ArkhamDBDeckLinkUpdatedAt P.=. now]
      unless enabled $ updateWhere [ArkhamDBPushDeckId P.==. deckId, ArkhamDBPushUserId P.==. uid, ArkhamDBPushStatus P.!=. "succeeded"]
        [ArkhamDBPushStatus P.=. "cancelled", ArkhamDBPushError P.=. Nothing]
      pruneCompletedPushes deckId
      pure $ affected == 1
  , deckWriteBackStatus = \uid -> runDB do
      links <- selectList [ArkhamDBDeckLinkUserId P.==. uid] [Asc ArkhamDBDeckLinkDeckId]
      forM links \(Entity _ link) -> do
        let deckId = arkhamDBDeckLinkDeckId link
        outstanding <- oldestPush deckId
        recent <- case outstanding of
          Just job -> pure $ Just job
          Nothing -> selectFirst [ArkhamDBPushDeckId P.==. deckId] [Desc ArkhamDBPushCreatedAt, Desc ArkhamDBPushId]
        resulting <- case recent of
          Nothing -> pure Nothing
          Just (Entity _ job) -> fmap (arkhamDBCampaignDeckDeckId . entityVal) <$> getBy (UniqueArkhamDBCampaignDeck $ arkhamDBPushCampaignKey job)
        pure $ pushStatus deckId link recent resulting
  , campaignOwnsTarget = \campaign remote -> runDB do
      previous <- selectFirst
        [ArkhamDBPushCampaignKey P.==. campaign, ArkhamDBPushStatus P.==. "succeeded", ArkhamDBPushTargetId P.==. Just remote.remoteDeckId]
        [Desc ArkhamDBPushCreatedAt, Desc ArkhamDBPushId]
      pure $ maybe False (\(Entity key _) -> hasPushMarker key remote) previous
  , campaignXpBaseline = \key -> runDB $ maybe 0 (arkhamDBCampaignDeckLastPushedXp . entityVal) <$> getBy (UniqueArkhamDBCampaignDeck key)
  }

-- Retain enough terminal history for status/recovery, not full campaign payloads
-- forever. Outstanding work is never pruned.
pruneCompletedPushes :: ArkhamDeckId -> DB ()
pruneCompletedPushes deckId = rawExecute
  "DELETE FROM arkhamdb_pushes WHERE id IN (SELECT id FROM arkhamdb_pushes WHERE deck_id = ? AND status IN ('succeeded', 'cancelled') ORDER BY created_at DESC, id DESC OFFSET 20)"
  [toPersistValue deckId]

oldestPush :: ArkhamDeckId -> DB (Maybe (Entity ArkhamDBPush))
oldestPush deckId = selectFirst [ArkhamDBPushDeckId P.==. deckId, ArkhamDBPushStatus P./<-. ["succeeded", "cancelled"]]
  [Asc ArkhamDBPushCreatedAt, Asc ArkhamDBPushId]

pushClaim :: Entity ArkhamDBPush -> [Filter ArkhamDBPush]
pushClaim (Entity key job) =
  [ ArkhamDBPushId P.==. key, ArkhamDBPushStatus P.==. arkhamDBPushStatus job
  , ArkhamDBPushStage P.==. arkhamDBPushStage job, ArkhamDBPushUpdatedAt P.==. arkhamDBPushUpdatedAt job
  ]

activePush :: Entity ArkhamDBPush -> DB Bool
activePush previous@(Entity _ job) = do
  active <- sameAccountGeneration (arkhamDBPushUserId job) (arkhamDBPushGeneration job)
  matching <- P.count $ pushClaim previous
  link <- getBy $ UniqueArkhamDBDeckLinkDeck $ arkhamDBPushDeckId job
  pure $ active && matching == 1 && maybe False (arkhamDBDeckLinkWriteBack . entityVal) link

pushUpdates :: ArkhamDBPush -> [Update ArkhamDBPush]
pushUpdates job =
  [ ArkhamDBPushStatus P.=. arkhamDBPushStatus job, ArkhamDBPushStage P.=. arkhamDBPushStage job
  , ArkhamDBPushSourceId P.=. arkhamDBPushSourceId job, ArkhamDBPushTargetId P.=. arkhamDBPushTargetId job
  , ArkhamDBPushExpectedDateUpdate P.=. arkhamDBPushExpectedDateUpdate job, ArkhamDBPushExpectedVersion P.=. arkhamDBPushExpectedVersion job
  , ArkhamDBPushRemoteDateUpdate P.=. arkhamDBPushRemoteDateUpdate job, ArkhamDBPushRemoteVersion P.=. arkhamDBPushRemoteVersion job
  , ArkhamDBPushError P.=. arkhamDBPushError job, ArkhamDBPushUpdatedAt P.=. arkhamDBPushUpdatedAt job
  ]

-- Snapshot the durable campaign decks, not the investigator's shuffled draw pile.
-- The association is resolved once by owner + URL and survives later remote syncs.
queueCampaignWriteBack :: ArkhamGameId -> Int -> UTCTime -> Text -> Game -> DB ()
queueCampaignWriteBack gameId step now boundary game = for_ (modeCampaign $ gameMode game) \campaign -> do
  players <- selectList [ArkhamPlayerArkhamGameId P.==. gameId] []
  let owner playerId = arkhamPlayerUserId . entityVal <$> find ((== playerId) . PlayerId . coerce . entityKey) players
      attrs = toAttrs campaign
  for_ (Map.toList $ gameInvestigators game) \(iid, investigator) -> do
    let ia = toAttrs investigator
        key = UUID.toText (coerce gameId) <> "/" <> unCardCode (unInvestigatorId iid)
    for_ (owner $ investigatorPlayerId ia) \uid -> do
      association <- getBy $ UniqueArkhamDBCampaignDeck key
      saved <- case association of
        Just (Entity _ value) -> P.get (arkhamDBCampaignDeckDeckId value) <&> fmap (Entity $ arkhamDBCampaignDeckDeckId value)
        Nothing -> case investigatorDeckUrl ia of
          Nothing -> pure Nothing
          Just url -> do
            links <- selectList [ArkhamDBDeckLinkUserId P.==. uid, ArkhamDBDeckLinkWriteBack P.==. True] []
            selectFirst [ArkhamDeckUserId P.==. uid, ArkhamDeckUrl P.==. Just url, ArkhamDeckId P.<-. map (arkhamDBDeckLinkDeckId . entityVal) links] [Asc ArkhamDeckId]
      for_ saved \(Entity deckId deck) -> do
        mapping <- getBy $ UniqueArkhamDBDeckLinkDeck deckId
        -- Lock only the generation's key, not the user/refresh lock. An unlink
        -- either wins first (skip obsolete work) or waits until this INSERT's
        -- transaction commits; it cannot turn gameplay into a FK violation.
        generations <- rawSql "SELECT ?? FROM arkhamdb_accounts WHERE user_id = ? FOR KEY SHARE SKIP LOCKED" [toPersistValue uid]
        let generation = viaNonEmpty head (generations :: [Entity ArkhamDBAccount])
        case (mapping, generation, Map.lookup iid $ campaignDecks attrs) of
          (Just (Entity _ link), Just (Entity generationId _), Just (Deck cards)) | ownsCampaignDeck uid deck link -> do
            when (isNothing association) $ insert_ $ ArkhamDBCampaignDeck key deckId uid 0
            let story = Map.findWithDefault [] iid $ campaignStoryCards attrs
                list = campaignDeckSnapshot (arkhamDeckList deck) cards story (investigatorSideDeck ia) (investigatorTaboo ia)
                event = UUID.toText (coerce gameId) <> "/" <> T.pack (show step) <> "/" <> unCardCode (unInvestigatorId iid)
            void $ insertUnique $ ArkhamDBPush deckId uid generationId event key (encodePushDeck list) (investigatorXp ia)
              "pending" "ready" Nothing Nothing (arkhamDBDeckLinkLastSyncedDateUpdate link) (arkhamDBDeckLinkLastSyncedVersion link)
              Nothing Nothing Nothing "normal" now now boundary
          _ -> pure ()

-- One bounded executor, no game lock, no per-boundary/per-retry forks. Only
-- a page of identifiers is read; full payloads are fetched after admission.
dispatchCampaignWriteBack :: Handler ()
dispatchCampaignWriteBack = do
  executor <- getsYesod appArkhamDBWriteBackExecutor
  liftIO $ void $ wakeArkhamDBExecutor executor

-- Allocate the worker from the foundation, never from a request's resources.
-- The initial wake resumes durable pending work immediately after a restart.
startCampaignWriteBack :: App -> IO ()
startCampaignWriteBack foundation =
  for_ ((,) <$> appArkhamDB (appSettings foundation) <*> appArkhamDBClient foundation) \(settings, transport) -> do
    let executor = appArkhamDBWriteBackExecutor foundation
    start <- wakeArkhamDBExecutor executor
    when start $ void $ forkIO $
      runArkhamDBExecutor executor (unsafeHandler foundation do
        result <- try @_ @SomeException $ sweep settings transport Nothing
        when (isLeft result) $ logWarnN "ArkhamDB write-back sweep failed; pending work will be retried")
        `finally` resetArkhamDBExecutor executor
 where
  sweep settings transport cursor = do
    rows <- runDB $ rawSql
      ("SELECT user_id, deck_id, id FROM arkhamdb_pushes WHERE status = 'pending' " <>
        maybe "" (const "AND id > ? ") cursor <> "ORDER BY id LIMIT 32")
      (maybe [] (pure . toPersistValue) cursor) :: Handler [(Single UserId, Single ArkhamDeckId, Single ArkhamDBPushId)]
    for_ rows \(Single uid, Single deckId, _) -> do
      now <- liftIO getCurrentTime
      void $ try @_ @SomeException $ runWriteBack (persistentStore $ const $ pure ()) writeBackStore
        (hoistArkhamDBClient liftIO transport) settings now False uid deckId
    case viaNonEmpty last rows of
      Just (_, _, Single key) | length rows == 32 -> sweep settings transport $ Just key
      _ -> pure ()
