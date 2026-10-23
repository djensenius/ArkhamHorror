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
import Data.List (nubBy)
import Data.Map.Strict qualified as Map
import Data.Text qualified as T
import Data.Time.Clock (UTCTime, getCurrentTime)
import Data.UUID qualified as UUID
import Database.Persist qualified as P
import Database.Persist.Sql (updateWhereCount)
import Import hiding (state)
import UnliftIO.Exception (try)

writeBackStore :: ArkhamDBWriteBackStore Handler
writeBackStore = ArkhamDBWriteBackStore
  { claimPush = \now retry uid deckId -> runDB do
      lockArkhamDBUser uid
      mapping <- getBy $ UniqueArkhamDBDeckLinkDeck deckId
      case mapping of
        Just (Entity _ link) | arkhamDBDeckLinkUserId link == uid && arkhamDBDeckLinkWriteBack link -> do
          pending <- oldestPush deckId
          case pending of
            Nothing -> pure Nothing
            Just (Entity key job) -> do
              let runnable = pushCanRun now retry job
              active <- sameAccountGeneration uid $ arkhamDBPushGeneration job
              if runnable && active then do
                let claimed = job {arkhamDBPushStatus = "running", arkhamDBPushError = Nothing, arkhamDBPushUpdatedAt = now}
                replace key claimed
                pure $ Just $ Entity key claimed
              else pure Nothing
        _ -> pure Nothing
  , checkpointPush = \previous updated -> runDB do
      active <- activePush previous
      if not active then pure False else do
        affected <- updateWhereCount (pushClaim previous) $ pushUpdates updated
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
            pure $ Just result
          _ -> pure Nothing
  , keepRemoteDeck = \previous remote -> runDB do
      active <- activePush previous
      if not active then pure False else do
        let job = entityVal previous
            deckId = arkhamDBPushDeckId job
            list = remoteDecklistFromRemote remote
        P.update deckId
          [ ArkhamDeckList P.=. list
          , ArkhamDeckName P.=. fromMaybe list.investigator_name list.decklist_name
          , ArkhamDeckInvestigatorName P.=. list.investigator_name
          ]
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
        pure True
  , resolvePush = \now uid deckId mode -> runDB do
      lockArkhamDBUser uid
      mapping <- getBy $ UniqueArkhamDBDeckLinkDeck deckId
      pending <- oldestPush deckId
      case (mapping, pending) of
        (Just (Entity _ link), Just (Entity key job))
          | arkhamDBDeckLinkUserId link == uid && arkhamDBDeckLinkWriteBack link
          , arkhamDBPushStatus job == "conflict" || arkhamDBPushStatus job == "failed"
          , mode `elem` ["overwrite", "new", "keep"]
          , mode /= "overwrite" || arkhamDBPushStage job /= "creating" -> do
              active <- sameAccountGeneration uid $ arkhamDBPushGeneration job
              if not active then pure False else do
                -- Never discard an uncertain creation intent and create again.
                -- Explicit save-as-new starts a fresh, independently identified push.
                if mode == "new" && arkhamDBPushStage job /= "ready" then do
                  P.update key [ArkhamDBPushStatus P.=. "cancelled"]
                  void $ insert job
                    { arkhamDBPushEventKey = arkhamDBPushEventKey job <> "/new/" <> T.pack (show now)
                    , arkhamDBPushStatus = "pending", arkhamDBPushStage = "ready", arkhamDBPushMode = "new"
                    , arkhamDBPushSourceId = Nothing, arkhamDBPushTargetId = Nothing, arkhamDBPushError = Nothing
                    , arkhamDBPushCreatedAt = now, arkhamDBPushUpdatedAt = now
                    }
                else P.update key
                  [ ArkhamDBPushStatus P.=. "pending", ArkhamDBPushStage P.=. "ready", ArkhamDBPushMode P.=. mode
                  , ArkhamDBPushSourceId P.=. Nothing, ArkhamDBPushTargetId P.=. Nothing
                  , ArkhamDBPushError P.=. Nothing, ArkhamDBPushUpdatedAt P.=. now
                  ]
                pure True
        _ -> pure False
  , toggleWriteBack = \now uid deckId enabled -> runDB do
      lockArkhamDBUser uid
      affected <- updateWhereCount [ArkhamDBDeckLinkDeckId P.==. deckId, ArkhamDBDeckLinkUserId P.==. uid]
        [ArkhamDBDeckLinkWriteBack P.=. enabled, ArkhamDBDeckLinkUpdatedAt P.=. now]
      unless enabled $ updateWhere [ArkhamDBPushDeckId P.==. deckId, ArkhamDBPushUserId P.==. uid, ArkhamDBPushStatus P.!=. "succeeded"]
        [ArkhamDBPushStatus P.=. "cancelled", ArkhamDBPushError P.=. Nothing]
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
  , campaignXpBaseline = \key -> runDB $ maybe 0 (arkhamDBCampaignDeckLastPushedXp . entityVal) <$> getBy (UniqueArkhamDBCampaignDeck key)
  }
 where
  -- URLs used to match live investigators remain stable across remote versions.
  remoteDecklistFromRemote remote = remote.deck

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
queueCampaignWriteBack :: ArkhamGameId -> Int -> UTCTime -> Game -> DB ()
queueCampaignWriteBack gameId step now game = for_ (modeCampaign $ gameMode game) \campaign -> do
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
        generation <- getBy $ UniqueArkhamDBAccountUser uid
        case (mapping, generation, Map.lookup iid $ campaignDecks attrs) of
          (Just (Entity _ link), Just (Entity generationId _), Just (Deck cards)) | arkhamDBDeckLinkWriteBack link -> do
            when (isNothing association) $ insert_ $ ArkhamDBCampaignDeck key deckId uid 0
            let story = Map.findWithDefault [] iid $ campaignStoryCards attrs
                list = campaignDeckSnapshot (arkhamDeckList deck) cards story (investigatorSideDeck ia) (investigatorTaboo ia)
                event = UUID.toText (coerce gameId) <> "/" <> T.pack (show step) <> "/" <> unCardCode (unInvestigatorId iid)
            void $ insertUnique $ ArkhamDBPush deckId uid generationId event key (encodePushDeck list) (investigatorXp ia)
              "pending" "ready" Nothing Nothing (arkhamDBDeckLinkLastSyncedDateUpdate link) (arkhamDBDeckLinkLastSyncedVersion link)
              Nothing Nothing Nothing "normal" now now
          _ -> pure ()

-- A worker has no game lock and cannot affect a committed answer. Pending jobs
-- survive a process exit; retry resumes the oldest job and its durable stage.
dispatchCampaignWriteBack :: Handler ()
dispatchCampaignWriteBack = do
  configured <- getsYesod $ appArkhamDB . appSettings
  client <- getsYesod appArkhamDBClient
  for_ ((,) <$> configured <*> client) \(settings, transport) ->
    forkHandler (const $ pure ()) do
      pending <- runDB $ selectList [ArkhamDBPushStatus P.==. "pending"] [Asc ArkhamDBPushCreatedAt, Asc ArkhamDBPushId]
      for_ (nubBy (\a b -> arkhamDBPushDeckId (entityVal a) == arkhamDBPushDeckId (entityVal b)) pending) \(Entity _ job) -> do
        now <- liftIO getCurrentTime
        void $ try @_ @SomeException $ runWriteBack (persistentStore $ const $ pure ()) writeBackStore
          (hoistArkhamDBClient liftIO transport) settings now False (arkhamDBPushUserId job) (arkhamDBPushDeckId job)
