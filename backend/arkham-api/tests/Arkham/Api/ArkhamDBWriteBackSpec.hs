{-# LANGUAGE NoFieldSelectors #-}

module Arkham.Api.ArkhamDBWriteBackSpec where

import Api.Arkham.Helpers (GameApp (..), runGameApp)
import Arkham.Campaign (lookupCampaign)
import Arkham.Campaign.Types (campaignStep, campaignDecks, campaignStoryCards, campaignStore)
import Arkham.Classes.Entity (overAttrs, toAttrs)
import Arkham.Prelude (These (This))
import Arkham.Game.Utils (gameInvestigators, modeCampaign)
import Arkham.Entities (entitiesInvestigators)
import Arkham.Investigator (lookupInvestigator)
import Arkham.Investigator.Types (investigatorXp)
import Arkham.Helpers (Deck (..))
import Control.Exception (try, throwIO, IOException, finally)
import System.IO.Error (userError)
import Arkham.Game qualified as Game
import Arkham.Game.State (GameState (IsActive))
import Arkham.Difficulty (Difficulty (Easy))
import Arkham.Id (PlayerId (..), InvestigatorId (..))
import Arkham.Classes.HasQueue (newQueue)
import Arkham.Queue (queueToRef)
import Arkham.Question (Question (ChooseUpgradeDeck, ContinueCampaign))
import Control.Monad.Random (mkStdGen)
import Arkham.Api.ArkhamDBSpec qualified as Linking
import Arkham.ArkhamDB
import Arkham.ArkhamDB qualified as ArkhamDB
import Arkham.ArkhamDB.Campaign
import Arkham.ArkhamDB.Overlay (campaignDeckIdentityKey, moveCampaignDeckIdentity)
import Arkham.ArkhamDB.Service
import Arkham.ArkhamDB.WriteBack
import Arkham.CampaignStep (CampaignStep (..))
import Arkham.Custom.Overlay (DeckOverlay (..))
import Arkham.Card (Card (..), toCardCode)
import Arkham.Card.CardCode (unCardCode)
import Arkham.Card.EncounterCard (EncounterCard (..))
import Arkham.Card.Id (unsafeMakeCardId)
import Arkham.Card.PlayerCard (PlayerCard (..))
import Arkham.Decklist qualified as Decklist
import Arkham.Message (Message (..), InitDeckAttrs (..), ShuffleIn (..))
import Arkham.Taboo.Types (TabooList (TabooList25))
import Arkham.Target (Target (GameTarget))
import Arkham.Source (Source (GameSource))
import Data.Aeson
import Data.Aeson.Key qualified as Key
import Data.Aeson.KeyMap qualified as KM
import Data.Aeson.Types (Pair)
import Data.Map.Strict qualified as Map
import Data.Text qualified as T
import Data.Time.Clock (addUTCTime, getCurrentTime)
import Control.Concurrent (forkIO, killThread)
import Control.Concurrent.STM.TBQueue

import Data.UUID qualified as UUID
import Database.Persist (Entity (..), Update (..), PersistUpdate (Assign), fieldLens)
import Database.Persist.Sql (toSqlKey)
import Entity.Arkham.ArkhamDBAccount
import Entity.Arkham.Deck
import Entity.Arkham.Game
import Entity.Arkham.Step
import Entity.Answer (deckChosenWithOverlay)
import Relude hiding (state)
import Test.Hspec
import System.Timeout (timeout)

-- The production service and HTTP adapter with transaction-shaped memory stores,
-- as in ArkhamDBSpec. SQL locking/rollback coverage belongs to task-2.46.
data Harness = Harness
  { run :: Bool -> IO ()
  , store :: ArkhamDBStore IO
  , writes :: ArkhamDBWriteBackStore IO
  , client :: ArkhamDBClient IO
  , job :: IORef ArkhamDBPush
  , remotes :: IORef (Map Text ArkhamDBRemoteDeck)
  , calls :: IORef [Text]
  , linked :: IORef Linking.Stored
  , baseline :: IORef Int
  }

pushKey :: ArkhamDBPushId
pushKey = ArkhamDBPushKey $ UUID.fromWords 0 0 0 245

currentDeck :: Decklist.ArkhamDBDecklist
currentDeck = Linking.remote.deck
  { Decklist.slots = Map.fromList [("01006", 1), ("01100", 1), ("01007", 1)]
  , Decklist.sideSlots = Map.singleton "01008" 1
  , Decklist.meta = Just "{\"alternate_front\":\"01001\"}"
  , Decklist.taboo_id = Just 10
  }

newHarness :: IO Harness
newHarness = do
  (baseStore, linked) <- Linking.newStore
  let store = baseStore
  Linking.linkAccount store
  imported <- runExceptT $ importRemoteDeck store Linking.stubClient Linking.settings Linking.now Linking.userId "123" Linking.forbiddenPublic
  imported `shouldSatisfy` isRight
  modifyIORef' linked \s -> s {Linking.storedLinks = Map.adjust (\l -> l {arkhamDBDeckLinkWriteBack = True}) Linking.deckId s.storedLinks}
  generation <- store.findAccountGeneration Linking.userId >>= maybe (fail "Missing account") pure
  job <- newIORef $ ArkhamDBPush Linking.deckId Linking.userId generation "game/1/iid" "game/iid"
    (encodePushDeck $ withExportedOverlay Nothing currentDeck) 5 "pending" "ready" Nothing Nothing Linking.remote.dateUpdate Linking.remote.deckVersion
    Nothing Nothing Nothing "normal" Linking.now Linking.now "scenarioEnd"
  remotes <- newIORef $ Map.singleton "123" Linking.remote
  calls <- newIORef []
  baseline <- newIORef 0
  completed <- newIORef Nothing
  let claimStore = store {withRefreshLock = \uid action -> withRefreshLock store uid \locked -> action locked {ownsPushClaim = claimStore.ownsPushClaim}, ownsPushClaim = \(Entity key previous) -> do
        current <- readIORef job
        pure $ key == pushKey && arkhamDBPushStatus current == "running" && arkhamDBPushStatus previous == "running" &&
          arkhamDBPushStage current == arkhamDBPushStage previous && arkhamDBPushUpdatedAt current == arkhamDBPushUpdatedAt previous}
      record call = modifyIORef' calls (<> [call])
      mapping = do
        state <- readIORef linked
        maybe (fail "Missing link") pure $ Map.lookup Linking.deckId state.storedLinks
      valid value = do
        generationNow <- store.findAccountGeneration Linking.userId
        link <- mapping
        pure $ generationNow == Just (arkhamDBPushGeneration value) && arkhamDBDeckLinkWriteBack link
      targetAvailable value remote = do
        state <- readIORef linked
        let existing = find (\link -> arkhamDBDeckLinkUserId link == arkhamDBPushUserId value && arkhamDBDeckLinkRemoteDeckId link == remote.remoteDeckId) $ Map.elems state.storedLinks
        case targetLinkConflict value remote existing of
          Nothing -> pure True
          Just conflicted -> writeIORef job conflicted >> pure False
      writes = ArkhamDBWriteBackStore
        { claimPush = \time retry uid deckId -> do
            value <- readIORef job
            active <- valid value
            if active && uid == Linking.userId && deckId == Linking.deckId &&
              pushCanRun time retry value then do
                let next = value {arkhamDBPushStatus = "running", arkhamDBPushError = Nothing, arkhamDBPushUpdatedAt = time}
                writeIORef job next
                pure $ Just $ Entity pushKey next
            else pure Nothing
        , requestPushRetry = \_ _ _ -> pure ()
        , checkpointPush = \(Entity _ previous) next -> do
            active <- valid previous
            current <- readIORef job
            if active && arkhamDBPushStatus current == arkhamDBPushStatus previous && arkhamDBPushStage current == arkhamDBPushStage previous && arkhamDBPushUpdatedAt current == arkhamDBPushUpdatedAt previous then writeIORef job next >> pure True
            else pure False
        , finishPush = \(Entity _ value) remote -> do
            active <- valid value
            available <- if active then targetAvailable value remote else pure False
            if not available then pure Nothing else do
              writeIORef completed $ Just (arkhamDBPushCampaignKey value, remote.remoteDeckId)
              modifyIORef' baseline $ acknowledgedCampaignXp value
              link <- mapping
              let copy = needsCampaignCopy link value
                  result = if copy then ArkhamDeckKey (UUID.fromWords 0 0 0 246) else Linking.deckId
                  updated = link
                    { arkhamDBDeckLinkDeckId = result
                    , arkhamDBDeckLinkRemoteDeckId = remote.remoteDeckId
                    , arkhamDBDeckLinkLastSyncedDateUpdate = remote.dateUpdate
                    , arkhamDBDeckLinkLastSyncedVersion = remote.deckVersion
                    , arkhamDBDeckLinkCampaignKey = Just $ arkhamDBPushCampaignKey value
                    }
              modifyIORef' linked \s -> s
                { Linking.storedLinks = Map.insert result updated s.storedLinks
                , Linking.storedDecks = if copy then maybe s.storedDecks (\deck -> Map.insert result (campaignCopyDeck deck remote) s.storedDecks) (Map.lookup Linking.deckId s.storedDecks) else s.storedDecks
                }
              writeIORef job value {arkhamDBPushStatus = "succeeded"}
              pure $ Just result
        , keepRemoteDeck = \(Entity _ value) remote -> do
            available <- targetAvailable value remote
            if not available then pure False else do
              modifyIORef' linked \s -> s
                { Linking.storedDecks = Map.adjust (applyDeckUpdates $ keepRemoteDeckUpdates remote) Linking.deckId s.storedDecks
                , Linking.storedLinks = Map.adjust (\link -> link
                    { arkhamDBDeckLinkRemoteDeckId = remote.remoteDeckId
                    , arkhamDBDeckLinkLastSyncedDateUpdate = remote.dateUpdate
                    , arkhamDBDeckLinkLastSyncedVersion = remote.deckVersion
                    }) Linking.deckId s.storedLinks
                }
              modifyIORef' baseline $ acknowledgedCampaignXp value
              writeIORef job value {arkhamDBPushStatus = "cancelled"}
              pure True
        , resolvePush = \time _ _ mode -> do
            value <- readIORef job
            if not (canResolvePush mode value) then pure False else do
              writeIORef job $ if mode == "new" && arkhamDBPushStage value /= "ready"
                then saveAsNewPush time value else resolvedPush time mode value
              pure True
        , toggleWriteBack = \_ _ _ enabled -> do
            modifyIORef' linked (\s -> s {Linking.storedLinks = Map.adjust (\link -> link {arkhamDBDeckLinkWriteBack = enabled}) Linking.deckId s.storedLinks})
            unless enabled $ modifyIORef' job \value ->
              if arkhamDBPushStatus value `elem` ["succeeded", "cancelled"] then value else disabledPush value
            pure True
        , deckWriteBackStatus = \_ -> do
            value <- readIORef job
            link <- mapping
            state <- readIORef linked
            let resulting = arkhamDBDeckLinkDeckId <$> find ((== Just (arkhamDBPushCampaignKey value)) . arkhamDBDeckLinkCampaignKey) (Map.elems state.storedLinks)
            pure [pushStatus Linking.deckId link (Just $ Entity pushKey value) (resulting <|> Just Linking.deckId)]
        , campaignOwnsTarget = \campaign remote -> do
            previous <- readIORef completed
            pure $ previous == Just (campaign, remote.remoteDeckId) && hasPushMarker pushKey remote
        , campaignXpBaseline = const $ readIORef baseline
        }
      client = Linking.stubClient
        { loadDeck = \_ remoteId -> do
            record $ "load/" <> remoteId
            maybe (Left ArkhamDBNotFound) Right . Map.lookup remoteId <$> readIORef remotes
        , listDecks = \_ -> do
            record "list"
            values <- Map.elems <$> readIORef remotes
            pure $ Right [ArkhamDBDeckSummary value.remoteDeckId "Deck" Nothing Nothing value.dateUpdate value.deckVersion Nothing | value <- values]
        , upgradeDeck = \_ source xp metadata -> do
            record $ "upgrade/" <> source <> "/" <> showText xp
            modifyIORef' remotes \values ->
              let original = fromMaybe Linking.remote $ Map.lookup source values
                  target = maybe "124" (showText . (+1)) (readMaybe (toString source) :: Maybe Int)
                  version = original {deck = original.deck {Decklist.meta = metadata}, remoteDeckId = target, dateUpdate = Just "created", deckVersion = Just "2.0", nextDeckId = Nothing}
              in Map.insert target version $ Map.adjust (\value -> value {nextDeckId = Just target}) source values
            pure $ Right $ maybe "124" (showText . (+1)) (readMaybe (toString source) :: Maybe Int)
        , createDeck = \_ deck -> do
            record "new"
            modifyIORef' remotes $ Map.insert "125" $ Linking.remote
              {deck = deck {Decklist.slots = mempty, Decklist.sideSlots = mempty}, remoteDeckId = "125", dateUpdate = Just "created", nextDeckId = Nothing}
            pure $ Right "125"
        , saveDeck = \_ target deck -> do
            record $ "save/" <> target
            modifyIORef' remotes $ Map.adjust (\value -> value {deck = deck.deck, dateUpdate = Just "saved"}) target
            pure $ Right target
        }
      run retry = runWriteBack claimStore writes client Linking.settings Linking.now retry Linking.userId Linking.deckId
  pure Harness {store = claimStore, ..}
 where
  showText = fromString . show

runWith :: Harness -> ArkhamDBClient IO -> Bool -> IO ()
runWith h client retry = runWriteBack h.store h.writes client Linking.settings Linking.now retry Linking.userId Linking.deckId

-- Unlike the single-intent harness above, retain every durable predecessor and
-- allocate a fresh identity for each independent replacement. Mirror SQL's
-- oldest-row/descendant selection and retire ancestors only on adoption.
newQueuedHarness :: IO (Harness, IORef (Map ArkhamDBPushId ArkhamDBPush))
newQueuedHarness = do
  base <- newHarness
  initial <- readIORef base.job
  rows <- newIORef $ Map.singleton pushKey initial
  let outstanding value = arkhamDBPushStatus value `notElem` ["succeeded", "cancelled"]
      next = do
        candidates <- map (uncurry Entity) . Map.toList . Map.filter outstanding <$> readIORef rows
        pure $ do
          original <- viaNonEmpty head $ sortOn (\(Entity key value) -> (arkhamDBPushCreatedAt value, key)) candidates
          pure $ preferReplacement original candidates
      matches (Entity key previous) = do
        current <- Map.lookup key <$> readIORef rows
        pure $ maybe False (\value ->
          arkhamDBPushStatus value == arkhamDBPushStatus previous &&
          arkhamDBPushStage value == arkhamDBPushStage previous &&
          arkhamDBPushUpdatedAt value == arkhamDBPushUpdatedAt previous) current
      owns previous = (&& arkhamDBPushStatus (entityVal previous) == "running") <$> matches previous
      store = base.store
        { ownsPushClaim = owns
        , withRefreshLock = \uid action -> withRefreshLock base.store uid \locked -> action locked {ownsPushClaim = owns}
        }
      writes = base.writes
        { claimPush = \time retry uid deckId -> next >>= \case
            Nothing -> pure Nothing
            Just (Entity key value) -> do
              writeIORef base.job value
              claimed <- base.writes.claimPush time retry uid deckId
              forM claimed \(Entity _ running) -> do
                modifyIORef' rows $ Map.insert key running
                pure $ Entity key running
        , checkpointPush = \previous@(Entity key _) value -> do
            active <- matches previous
            if not active then pure False else do
              saved <- base.writes.checkpointPush previous value
              when saved $ modifyIORef' rows $ Map.insert key value
              pure saved
        , finishPush = \previous@(Entity key value) remote -> do
            result <- base.writes.finishPush previous remote
            when (isJust result) $ modifyIORef' rows $
              Map.insert key value {arkhamDBPushStatus = "succeeded", arkhamDBPushError = Nothing} .
              Map.map (\ancestor -> if value `supersedesPush` ancestor
                then ancestor {arkhamDBPushStatus = "cancelled", arkhamDBPushError = Nothing} else ancestor)
            pure result
        , keepRemoteDeck = \previous remote -> do
            saved <- base.writes.keepRemoteDeck previous remote
            when saved $ modifyIORef' rows $ Map.map (\value -> if outstanding value
              then value {arkhamDBPushStatus = "cancelled", arkhamDBPushError = Nothing} else value)
            pure saved
        , resolvePush = \time _ _ mode -> next >>= \case
            Nothing -> pure False
            Just (Entity key value)
              | canResolvePush mode value -> do
                  modifyIORef' rows \values ->
                    if mode == "new" && arkhamDBPushStage value /= "ready"
                    then Map.insert (ArkhamDBPushKey $ UUID.fromWords 0 0 0 $ fromIntegral $ 245 + Map.size values)
                      (saveAsNewPush time value) $ Map.insert key (saveAsNewPredecessor value) values
                    else Map.insert key (resolvedPush time mode value) values
                  pure True
              | otherwise -> pure False
        , toggleWriteBack = \time uid deckId enabled -> do
            result <- base.writes.toggleWriteBack time uid deckId enabled
            unless enabled $ modifyIORef' rows $ Map.map (\value -> if outstanding value then disabledPush value else value)
            pure result
        }
      run retry = runWriteBack store writes base.client Linking.settings Linking.now retry Linking.userId Linking.deckId
  pure (base {store = store, writes = writes, run = run}, rows)

assertState :: Harness -> Text -> Expectation
assertState h expected = arkhamDBPushStatus <$> readIORef h.job >>= (`shouldBe` expected)

assertNoPush :: Harness -> Expectation
assertNoPush h = do
  calls <- readIORef h.calls
  filter (\call -> "upgrade/" `T.isPrefixOf` call || "save/" `T.isPrefixOf` call || call == "new") calls `shouldBe` []

-- Exercise the production cancellation traversal and the same per-user lock as
-- request admission. The harness replaces SQL writes, not the undo predicates.
cancelHarnessUndo :: Harness -> Int -> IO ()
cancelHarnessUndo h = cancelHarnessUndoWith h (pure ())

cancelHarnessUndoWith :: Harness -> IO () -> Int -> IO ()
cancelHarnessUndoWith h beforeLock restoredStep = do
  job <- readIORef h.job
  cancelUndoneCampaignPushes [(pushKey, arkhamDBPushEventKey job, arkhamDBPushUserId job)]
    (\users action -> beforeLock >> withLocks users action)
    (\_ -> modifyIORef' h.job \current ->
      if arkhamDBPushStatus current `elem` ["succeeded", "cancelled"] then current
      else undonePush current)
    restoredStep
 where
  withLocks [] action = action
  withLocks (uid : users) action = withRefreshLock h.store uid $ const $ withLocks users action

assertUndoNotCompleted :: Harness -> Expectation
assertUndoNotCompleted h = do
  assertState h "cancelled"
  readIORef h.baseline `shouldReturn` 0
  state <- readIORef h.linked
  arkhamDBDeckLinkRemoteDeckId <$> Map.lookup Linking.deckId state.storedLinks `shouldBe` Just "123"
  arkhamDBDeckLinkCampaignKey <$> Map.lookup Linking.deckId state.storedLinks `shouldBe` Just Nothing

boundaryGame :: CampaignStep -> Game.Game
boundaryGame step = (Game.newCampaign "01" Nothing 1 1 Easy False)
  { Game.gameGameState = IsActive
  , Game.gameMode = This $ overAttrs (\attrs -> attrs {campaignStep = step}) $ lookupCampaign "01" Easy
  }

-- Run the real deck seating/replacement queues, including Game.Runner's
-- deck-list loading and Campaign.Runner's materialization, without SQL.
newCampaignMessages :: IO (IORef Game.Game, PlayerId, [Message] -> IO ())
newCampaignMessages = do
  let pid = PlayerId $ UUID.fromWords 0 0 0 101
  gameRef <- newIORef $ (boundaryGame $ ChooseDecksStep PrologueStep)
    {Game.gamePlayers = [pid]}
  gen <- newIORef $ mkStdGen 1
  let drain messages = do
        modifyIORef' gameRef \game -> game {Game.gameQuestion = mempty}
        queue <- newQueue $ messages <> [AskMap $ Map.singleton pid ContinueCampaign]
        runGameApp (GameApp gameRef queue gen (pure . const ()) Nothing) $
          Game.runMessages "write-back-overlay-materialization" Game.noRunObservers
  pure (gameRef, pid, drain)

applyDeckUpdates :: [Update ArkhamDeck] -> ArkhamDeck -> ArkhamDeck
applyDeckUpdates updates deck = entityVal $ foldl' apply (Entity Linking.deckId deck) updates
 where
  apply entity (Update field value Assign) = runIdentity $ fieldLens field (const $ Identity value) entity
  apply _ _ = error "Only field assignments are used by keep-remote"

spec :: Spec
spec = describe "ArkhamDB campaign write-back" do
  it "keeps one association and XP baseline through actual Homunculus transformation, boundary and reset" do
    h <- newHarness
    (gameRef, pid, drain) <- newCampaignMessages
    baselines <- newIORef Map.empty
    let overlay = DeckOverlay Nothing mempty (Map.singleton "01007" 1) mempty
        base = Linking.remote.deck {Decklist.slots = Map.fromList [("01006", 1), ("11068a", 1)], Decklist.meta = Nothing}
        play = arkhamDeckPlayList $ ArkhamDeck Linking.userId Nothing "Deck" "Roland" base (Just overlay) Nothing
        story code n = PlayerCard $ MkPlayerCard (unsafeMakeCardId $ UUID.fromWords 0 0 0 n) Nothing code code mempty Nothing Nothing Nothing Nothing Nothing
        writes = h.writes
          { campaignXpBaseline = \key -> Map.findWithDefault 0 key <$> readIORef baselines
          , finishPush = \entity remote -> do
              result <- h.writes.finishPush entity remote
              when (isJust result) $ modifyIORef' baselines $
                Map.alter (Just . acknowledgedCampaignXp (entityVal entity) . fromMaybe 0) (arkhamDBPushCampaignKey $ entityVal entity)
              pure result
          }
        submit game liveId = do
          (deckId, stableId) <- maybe (fail "Missing stable write-back identity") pure $ campaignWriteBackSeatIdentity game liveId
          stableId `shouldBe` "01001"
          let attrs = maybe (error "Missing campaign") toAttrs $ modeCampaign game.gameMode
              cards = maybe [] unDeck $ Map.lookup deckId $ campaignDecks attrs
              storyCards = Map.findWithDefault [] deckId $ campaignStoryCards attrs
              snapshot = campaignWriteBackSnapshot (campaignStore attrs) deckId base cards storyCards [] Nothing Nothing
              investigator = fromMaybe (error "Missing live investigator") $ Map.lookup liveId $ gameInvestigators game
              key = "game/" <> unCardCode (unInvestigatorId stableId)
          exportedOverlay snapshot `shouldBe` Just (toJSON overlay)
          modifyIORef' h.job \job -> job
            { arkhamDBPushCampaignKey = key, arkhamDBPushPayload = encodePushDeck snapshot
            , arkhamDBPushTotalXp = investigatorXp $ toAttrs investigator
            , arkhamDBPushStatus = "pending", arkhamDBPushStage = "ready", arkhamDBPushMode = "normal"
            , arkhamDBPushSourceId = Nothing, arkhamDBPushTargetId = Nothing, arkhamDBPushError = Nothing
            }
          runWriteBack h.store writes h.client Linking.settings Linking.now False Linking.userId Linking.deckId
          assertState h "succeeded"
          pure snapshot
        boundary = do
          modifyIORef' gameRef \game -> game {Game.gameQuestion = mempty}
          captured <- withCampaignWriteBackBoundary gameRef \observers -> do
            queue <- newQueue [ForTarget GameTarget $ EndOfScenario Nothing, AskMap $ Map.singleton pid ContinueCampaign]
            gen <- newIORef $ mkStdGen 1
            runGameApp (GameApp gameRef queue gen (pure . const ()) Nothing) $
              Game.runMessages "homunculus-write-back-boundary" observers
          maybe (fail "Missing actual boundary") (pure . snd) captured
    initial <- readIORef gameRef
    drain $ deckChosenWithOverlay initial pid (Just overlay) play
    drain [ReloadDecks, GainXP "01001" GameSource 5]
    seated <- readIORef gameRef
    void $ submit seated "01001"
    drain [AddCampaignCardToDeck "01001" DoNotShuffleIn $ story "01008" 801, BecomeHomunculus "01001"]
    transformed <- readIORef gameRef
    Map.keys (gameInvestigators transformed) `shouldBe` ["01001"]
    campaignWriteBackSeatIdentity transformed "01001" `shouldBe` Just ("11068b", "01001")
    first <- boundary >>= \game -> submit game "01001"
    Map.lookup "01008" first.slots `shouldBe` Just 1
    drain [ResetGame, ForTarget GameTarget ResetGame]
    promoted <- readIORef gameRef
    Map.keys (gameInvestigators promoted) `shouldBe` ["11068b"]
    campaignWriteBackSeatIdentity promoted "11068b" `shouldBe` Just ("11068b", "01001")
    drain [GainXP "11068b" GameSource 2, AddCampaignCardToDeck "11068b" DoNotShuffleIn $ story "01009" 802]
    later <- boundary >>= \game -> submit game "11068b"
    Map.lookup "01009" later.slots `shouldBe` Just 1
    readIORef baselines `shouldReturn` Map.singleton "game/01001" 7
    filter (T.isPrefixOf "upgrade/") <$> readIORef h.calls `shouldReturn`
      ["upgrade/123/5", "upgrade/124/0", "upgrade/125/2"]
    state <- readIORef h.linked
    fmap arkhamDBDeckLinkCampaignKey (Map.lookup Linking.deckId state.storedLinks) `shouldBe` Just (Just "game/01001")
    Map.size state.storedLinks `shouldBe` 1

  it "refuses unknown or colliding transformed write-back identities without guessing a fresh lineage" do
    campaignWriteBackIdentity mempty ["11068b"] "11068b" True `shouldBe` Nothing
    let known = Map.singleton (campaignDeckIdentityKey "11068b") $ toJSON ("01001" :: InvestigatorId)
        collision = moveCampaignDeckIdentity "01001" "11068b" ["01001", "11068b"] mempty
    campaignWriteBackIdentity known ["01001", "11068b"] "11068b" True `shouldBe` Nothing
    campaignWriteBackIdentity known ["01001", "11068b"] "01001" False `shouldBe` Nothing
    Map.lookup (campaignDeckIdentityKey "11068b") collision `shouldBe` Just Null
    campaignWriteBackIdentity collision ["11068b"] "01001" True `shouldBe` Nothing

  for_ [False, True] \uncertain ->
    for_ [False, True] \undo ->
      it ("retains save-as-new predecessor accounting across " <> show (uncertain, undo)) do
        (h, rows) <- newQueuedHarness
        let failing = if uncertain
              then h.client {upgradeDeck = \token source xp metadata ->
                h.client.upgradeDeck token source xp metadata >> pure (Left $ ArkhamDBRemoteError "lost upgrade reply")}
              else h.client {saveDeck = \_ _ _ -> pure $ Left $ ArkhamDBRemoteError "save failed"}
        runWith h failing False
        h.writes.resolvePush Linking.now Linking.userId Linking.deckId "new" `shouldReturn` True
        persisted <- readIORef rows
        Map.size persisted `shouldBe` 2
        fmap arkhamDBPushStatus (Map.lookup pushKey persisted) `shouldBe` Just "failed"
        let active value = arkhamDBPushStatus value `notElem` ["succeeded", "cancelled"]
        when undo $ cancelUndoneCampaignPushes
          [(key, arkhamDBPushEventKey value, arkhamDBPushUserId value) | (key, value) <- Map.toList persisted, active value]
          (\_ action -> action)
          (\key -> modifyIORef' rows $ Map.adjust (\value -> if active value then undonePush value else value) key)
          0
        h.writes.toggleWriteBack Linking.now Linking.userId Linking.deckId False `shouldReturn` True
        h.writes.toggleWriteBack Linking.now Linking.userId Linking.deckId True `shouldReturn` True
        -- Re-answering queues a fresh boundary; the original persisted
        -- accounting fence, not the cancelled replacement, must stay ahead.
        original <- maybe (fail "Missing predecessor") pure $ Map.lookup pushKey persisted
        let fresh = original
              { arkhamDBPushEventKey = "game/2/iid", arkhamDBPushStatus = "pending", arkhamDBPushStage = "ready"
              , arkhamDBPushSourceId = Nothing, arkhamDBPushTargetId = Nothing, arkhamDBPushError = Nothing
              , arkhamDBPushCreatedAt = addUTCTime 60 Linking.now
              }
        modifyIORef' rows $ Map.insert (ArkhamDBPushKey $ UUID.fromWords 0 0 0 250) fresh
        before <- readIORef h.calls
        h.run False
        h.writes.resolvePush Linking.now Linking.userId Linking.deckId "overwrite" `shouldReturn` False
        readIORef h.calls `shouldReturn` before
        h.run True
        if undo then do
          -- Neither restart-style fresh selection nor explicit retry may save
          -- the undone snapshot. Toggling must preserve boundaryUndone.
          readIORef h.calls `shouldReturn` before
          latest <- readIORef rows
          fmap arkhamDBPushError (Map.lookup pushKey latest) `shouldBe` Just (Just "boundaryUndone")
          for_ ["keep", "new"] \mode ->
            h.writes.resolvePush Linking.now Linking.userId Linking.deckId mode `shouldReturn` False
        else do
          when uncertain do
            -- Recovery cannot infer a missing target baseline. It must remain
            -- fenced until explicit read-only keep-remote reconciliation.
            h.writes.resolvePush Linking.now Linking.userId Linking.deckId "overwrite" `shouldReturn` False
            h.writes.resolvePush Linking.now Linking.userId Linking.deckId "keep" `shouldReturn` True
            h.run False
          readIORef h.baseline `shouldReturn` 5
          h.run False
        calls <- readIORef h.calls
        filter (T.isPrefixOf "upgrade/123/") calls `shouldBe` ["upgrade/123/5"]
        filter (== "new") calls `shouldBe` []

  for_ [False, True] \undo ->
    it ("retains nested creation fences when the next independent replacement is retired: " <> show undo) do
      (h, rows) <- newQueuedHarness
      let failing = h.client {saveDeck = \_ _ _ -> pure $ Left $ ArkhamDBRemoteError "save failed"}
      runWith h failing False
      h.writes.resolvePush Linking.now Linking.userId Linking.deckId "new" `shouldReturn` True
      runWith h failing False
      h.writes.resolvePush (addUTCTime 1 Linking.now) Linking.userId Linking.deckId "new" `shouldReturn` True
      when undo $ modifyIORef' rows $ Map.map undonePush
      h.writes.toggleWriteBack Linking.now Linking.userId Linking.deckId False `shouldReturn` True
      h.writes.toggleWriteBack Linking.now Linking.userId Linking.deckId True `shouldReturn` True
      requests <- readIORef h.calls
      h.run False
      h.writes.resolvePush Linking.now Linking.userId Linking.deckId "overwrite" `shouldReturn` False
      readIORef h.calls `shouldReturn` requests
      h.run True
      if undo then readIORef h.calls `shouldReturn` requests
      else do
        readIORef h.baseline `shouldReturn` 5
        state <- readIORef h.linked
        fmap arkhamDBDeckLinkRemoteDeckId (Map.lookup Linking.deckId state.storedLinks) `shouldBe` Just "125"
      filter (T.isPrefixOf "upgrade/123/") <$> readIORef h.calls `shouldReturn` ["upgrade/123/5"]
      filter (== "new") <$> readIORef h.calls `shouldReturn` ["new"]

  it "does not redirect a failed targetless independent replacement onto its predecessor's XP lineage" do
    (h, rows) <- newQueuedHarness
    runWith h h.client {saveDeck = \_ _ _ -> pure $ Left $ ArkhamDBRemoteError "save failed"} False
    h.writes.resolvePush Linking.now Linking.userId Linking.deckId "new" `shouldReturn` True
    -- Losing the mapping after claim, before preflight, leaves a failed ready
    -- child. Restoring that mapping must not make overwrite safe.
    let missingLink = h.store {findDeck = \key ->
          fmap (fmap (\(deck, _) -> (deck, Nothing))) $ h.store.findDeck key}
    runWriteBack missingLink h.writes h.client Linking.settings Linking.now False Linking.userId Linking.deckId
    values <- Map.elems <$> readIORef rows
    values `shouldSatisfy` any (\value -> "/new/" `T.isInfixOf` arkhamDBPushEventKey value &&
      arkhamDBPushStage value == "ready" && arkhamDBPushStatus value == "failed")
    h.writes.resolvePush Linking.now Linking.userId Linking.deckId "overwrite" `shouldReturn` False
    h.run True
    filter (T.isPrefixOf "upgrade/123/") <$> readIORef h.calls `shouldReturn` ["upgrade/123/5"]
    filter (== "new") <$> readIORef h.calls `shouldReturn` ["new"]

  it "retires all retained predecessors only after a nested independent target is adopted" do
    (h, rows) <- newQueuedHarness
    let failing = h.client {saveDeck = \_ _ _ -> pure $ Left $ ArkhamDBRemoteError "save failed"}
    runWith h failing False
    h.writes.resolvePush Linking.now Linking.userId Linking.deckId "new" `shouldReturn` True
    runWith h failing False
    h.writes.resolvePush (addUTCTime 1 Linking.now) Linking.userId Linking.deckId "new" `shouldReturn` True
    before <- readIORef rows
    length (filter ((== "failed") . arkhamDBPushStatus) $ Map.elems before) `shouldBe` 2
    -- Give the next independent creation its own server identifier.
    let client = h.client {createDeck = \token deck -> do
          result <- h.client.createDeck token deck
          modifyIORef' h.remotes \values -> maybe values (\remote -> Map.insert "126" remote {ArkhamDB.remoteDeckId = "126"} $ Map.delete "125" values) $ Map.lookup "125" values
          pure $ "126" <$ result}
    runWith h client False
    after <- readIORef rows
    length (filter ((== "cancelled") . arkhamDBPushStatus) $ Map.elems after) `shouldBe` 2
    length (filter ((== "succeeded") . arkhamDBPushStatus) $ Map.elems after) `shouldBe` 1
    readIORef h.baseline `shouldReturn` 5
    state <- readIORef h.linked
    fmap arkhamDBDeckLinkRemoteDeckId (Map.lookup Linking.deckId state.storedLinks) `shouldBe` Just "126"

  for_ [False, True] \changedOverlay ->
    it ("adopts a pushed list through actual upgrade persistence without intervening sync: " <> show changedOverlay) do
      h <- newHarness
      (gameRef, pid, drain) <- newCampaignMessages
      let exported = DeckOverlay Nothing mempty (Map.singleton "01007" 1) mempty
          local = if changedOverlay then exported {overlayAdd = Map.singleton "01008" 1} else exported
          base = Linking.remote.deck {Decklist.slots = Map.singleton "01006" 1, Decklist.meta = Nothing}
          snapshot = withExportedOverlay (Just exported) base {Decklist.slots = Map.fromList [("01006", 1), ("01007", 1)]}
      modifyIORef' h.linked \state -> state
        {Linking.storedDecks = Map.adjust (\deck -> deck {arkhamDeckList = base, arkhamDeckOverlay = Just local}) Linking.deckId state.storedDecks}
      modifyIORef' h.job \job -> job {arkhamDBPushPayload = encodePushDeck snapshot}
      h.run False
      remote <- Map.lookup "124" <$> readIORef h.remotes >>= maybe (fail "Missing pushed deck") pure
      saved <- Map.lookup Linking.deckId . (.storedDecks) <$> readIORef h.linked >>= maybe (fail "Missing saved deck") pure
      initial <- readIORef gameRef
      drain $ deckChosenWithOverlay initial pid Nothing base
      drain [UpgradeDecklist "01001" remote.deck]
      -- This exact builder is used by the UPGRADE endpoint before any game or
      -- saved-deck write, not a test-only overlay edit or intervening sync.
      updates <- either (fail . show) pure $ savedDeckUpgradeUpdates Linking.now saved remote.deck
      let adopted = applyDeckUpdates updates saved
          expected = Map.fromList $ [("01006", 1), ("01007", 1)] <> [("01008", 1) | changedOverlay]
      Decklist.slots (arkhamDeckPlayList adopted) `shouldBe` expected
      arkhamDeckOverlay adopted `shouldBe` if changedOverlay then Just local else Nothing
      -- Display and reuse of the saved play list must agree, including after
      -- sync sees the already-copied push marker.
      modifyIORef' h.linked \state -> state {Linking.storedDecks = Map.insert Linking.deckId adopted state.storedDecks}
      replicateM_ 2 do
        result <- runExceptT $ syncRemoteDeck h.store h.client Linking.settings Linking.now Linking.userId Linking.deckId Linking.forbiddenPublic
        synced <- either (fail . show) (pure . entityVal) result
        Decklist.slots (arkhamDeckPlayList synced) `shouldBe` expected
      drain [LoadDecklist pid $ arkhamDeckPlayList adopted]
      reused <- readIORef gameRef
      let cards = modeCampaign reused.gameMode >>= Map.lookup "01001" . campaignDecks . toAttrs
      fmap (Map.fromListWith (+) . map (\card -> (toCardCode card, 1 :: Int)) . unDeck) cards `shouldBe` Just expected

  it "refuses ambiguous upgrade persistence before copying an unknown-provenance push marker" do
    h <- newHarness
    saved <- Map.lookup Linking.deckId . (.storedDecks) <$> readIORef h.linked >>= maybe (fail "Missing saved deck") pure
    let overlay = DeckOverlay Nothing mempty (Map.singleton "01007" 1) mempty
        incoming = currentDeck {Decklist.meta = Just "{\"ahn_push\":\"legacy\"}"}
    case savedDeckUpgradeUpdates Linking.now saved {arkhamDeckOverlay = Just overlay} incoming of
      Left err -> arkhamDBErrorCode err `shouldBe` "validationFailed"
      Right _ -> expectationFailure "Ambiguous upgrade must not persist"

  for_ [("single-step", 5), ("multi-step", 2)] \(kind, restoredStep) -> do
    it ("skips all write-back queries for unconfigured " <> kind <> " undo") do
      calls <- newIORef ([] :: [Text])
      let participates = modifyIORef' calls (<> ["exists"]) >> pure True
          cancel _ = modifyIORef' calls (<> ["cancel"])
      cancelCampaignWriteBackIfNeeded False participates cancel restoredStep
      readIORef calls `shouldReturn` []

    it ("only checks indexed participation for ordinary " <> kind <> " undo") do
      calls <- newIORef ([] :: [Text])
      let participates = modifyIORef' calls (<> ["exists"]) >> pure False
          cancel _ = modifyIORef' calls (<> ["cancel"])
      cancelCampaignWriteBackIfNeeded True participates cancel restoredStep
      readIORef calls `shouldReturn` ["exists"]

  it "retires only boundaries strictly above the restored history step" do
    let events = ["game/4/iid", "game/5/iid/row", "game/6/iid/row"]
    map (boundaryWasUndone 5) events `shouldBe` [False, False, True]
    h <- newHarness
    cancelled <- newIORef ([] :: [ArkhamDBPushId])
    let keys = [ArkhamDBPushKey $ UUID.fromWords 0 0 0 n | n <- [4,5,6]]
    cancelUndoneCampaignPushes (zipWith (\key event -> (key, event, Linking.userId)) keys events)
      (\_ action -> action) (\key -> modifyIORef' cancelled (<> [key])) 5
    readIORef cancelled `shouldReturn` [ArkhamDBPushKey $ UUID.fromWords 0 0 0 6]
    -- A boundary exactly at the restored cursor is still valid and must run.
    modifyIORef' h.job \job -> job {arkhamDBPushEventKey = "game/5/iid/row"}
    h.run False
    assertState h "succeeded"

  it "fences an already-dispatched worker cancelled before request admission" do
    h <- newHarness
    reached <- newEmptyMVar
    release <- newEmptyMVar
    done <- newEmptyMVar
    let pausedWrites = h.writes {checkpointPush = \previous next -> do
          saved <- h.writes.checkpointPush previous next
          when (saved && arkhamDBPushStage next == "creating") $ putMVar reached () >> takeMVar release
          pure saved}
    tid <- forkIO $ runWriteBack h.store pausedWrites h.client Linking.settings Linking.now False Linking.userId Linking.deckId
      `finally` putMVar done ()
    flip finally (killThread tid) do
      timeout 2000000 (takeMVar reached) `shouldReturn` Just ()
      cancelHarnessUndo h 0
      assertState h "conflict"
      before <- readIORef h.calls
      before `shouldBe` ["load/123"]
      putMVar release ()
      timeout 2000000 (takeMVar done) `shouldReturn` Just ()
      readIORef h.calls `shouldReturn` before
      readIORef h.baseline `shouldReturn` 0

  it "waits for an admitted request before cancelling and fences subsequent work" do
    h <- newHarness
    admitted <- newEmptyMVar
    release <- newEmptyMVar
    cancelling <- newEmptyMVar
    cancelled <- newEmptyMVar
    done <- newEmptyMVar
    let pausedClient = h.client {loadDeck = \token remoteId -> do
          response <- h.client.loadDeck token remoteId
          putMVar admitted ()
          takeMVar release
          pure response}
        -- Let cancellation commit before the worker's next admission. No sleep
        -- decides which contender acquires the per-user request lock next.
        pausedWrites = h.writes {checkpointPush = \previous next -> do
          when (arkhamDBPushStage next == "creating") $ readMVar cancelled
          h.writes.checkpointPush previous next}
    worker <- forkIO $ runWriteBack h.store pausedWrites pausedClient Linking.settings Linking.now False Linking.userId Linking.deckId
      `finally` putMVar done ()
    flip finally (killThread worker) do
      timeout 2000000 (takeMVar admitted) `shouldReturn` Just ()
      undo <- forkIO $ cancelHarnessUndoWith h (putMVar cancelling ()) 0 `finally` putMVar cancelled ()
      flip finally (killThread undo) do
        timeout 2000000 (takeMVar cancelling) `shouldReturn` Just ()
        timeout 100000 (readMVar cancelled) `shouldReturn` Nothing
        assertState h "running"
        readIORef h.calls `shouldReturn` ["load/123"]
        putMVar release ()
        timeout 2000000 (readMVar cancelled) `shouldReturn` Just ()
        timeout 2000000 (takeMVar done) `shouldReturn` Just ()
        readIORef h.calls `shouldReturn` ["load/123"]
        assertUndoNotCompleted h

  it "persists copy-then-upgrade to the campaign association rather than the live original URL" do
    h <- newHarness
    modifyIORef' h.linked \state -> state
      { Linking.storedLinks = Map.adjust (\link -> link {arkhamDBDeckLinkCampaignKey = Just "other/iid"}) Linking.deckId state.storedLinks }
    modifyIORef' h.job \job -> job {arkhamDBPushStatus = "conflict"}
    h.writes.resolvePush Linking.now Linking.userId Linking.deckId "new" `shouldReturn` True
    h.run False
    assertState h "succeeded"
    state <- readIORef h.linked
    let copyId = ArkhamDeckKey $ UUID.fromWords 0 0 0 246
        association = ArkhamDBCampaignDeck "game/iid" copyId Linking.userId 5
        getDeck key = pure $ Map.lookup key state.storedDecks
        byUrl url = pure [Entity key deck | (key, deck) <- Map.toList state.storedDecks, arkhamDeckUrl deck == Just url]
        oldUrl = Just $ publicDeckUrl Linking.settings "123"
        upgraded = currentDeck {Decklist.slots = Map.singleton "01007" 2}
    targets <- savedDeckUpgradeTargets getDeck byUrl Linking.userId (Just association) oldUrl
    targets `shouldBe` [copyId]
    let persisted = foldl' (\decks key -> Map.adjust (\deck -> deck {arkhamDeckList = upgraded}) key decks) state.storedDecks targets
    arkhamDeckList <$> Map.lookup Linking.deckId persisted `shouldBe` arkhamDeckList <$> Map.lookup Linking.deckId state.storedDecks
    arkhamDeckList <$> Map.lookup copyId persisted `shouldBe` Just upgraded
    -- Both association ownership and saved-row ownership are required. A
    -- stale association must not fall through to the original URL's deck.
    let foreignUser = toSqlKey 999
    savedDeckUpgradeTargets getDeck byUrl Linking.userId (Just association {arkhamDBCampaignDeckUserId = foreignUser}) oldUrl `shouldReturn` []
    savedDeckUpgradeTargets (\key -> fmap (\deck -> deck {arkhamDeckUserId = foreignUser}) <$> getDeck key)
      byUrl Linking.userId (Just association) oldUrl `shouldReturn` []
    savedDeckUpgradeTargets getDeck byUrl Linking.userId Nothing oldUrl `shouldReturn` [Linking.deckId]

  for_ ["pending", "running", "failed", "conflict"] \status ->
    it ("fences an undone " <> toString status <> " boundary before its next remote request") do
      h <- newHarness
      modifyIORef' h.job \job -> job {arkhamDBPushStatus = status}
      job <- readIORef h.job
      let claim = Entity pushKey job
      when (status == "running") $ h.store.ownsPushClaim claim `shouldReturn` True
      -- The production undo store selects this game's outstanding event keys
      -- and shares the request's user lock before cancelling their claims.
      withRefreshLock h.store Linking.userId \_ ->
        when (boundaryWasUndone 0 $ arkhamDBPushEventKey job) $
          modifyIORef' h.job \current -> current {arkhamDBPushStatus = "cancelled"}
      assertState h "cancelled"
      withRefreshLock h.store Linking.userId (\locked -> locked.ownsPushClaim claim) `shouldReturn` False
      h.run True
      assertNoPush h
      boundaryWasUndone 2 (arkhamDBPushEventKey job) `shouldBe` False
      boundaryWasUndone 0 (arkhamDBPushEventKey job) `shouldBe` True

  it "enqueues a fresh corrected snapshot when undo re-answers a reused boundary step" do
    h <- newHarness
    let gameId = ArkhamGameKey $ UUID.fromWords 0 0 0 1
        firstHistory = ArkhamStepKey $ UUID.fromWords 0 0 0 2
        rewrittenHistory = ArkhamStepKey $ UUID.fromWords 0 0 0 3
        firstEvent = boundaryEventKey gameId 5 "01001" firstHistory
        nextEvent = boundaryEventKey gameId 5 "01001" rewrittenHistory
        corrected = currentDeck {Decklist.slots = Map.singleton "01007" 2}
    firstEvent `shouldNotBe` nextEvent
    job <- readIORef h.job
    let outbox = Map.singleton firstEvent job {arkhamDBPushEventKey = firstEvent, arkhamDBPushStatus = "cancelled"}
        replacement = job {arkhamDBPushEventKey = nextEvent, arkhamDBPushPayload = encodePushDeck corrected}
        enqueued = Map.insertWith (\_ old -> old) nextEvent replacement outbox
    Map.size enqueued `shouldBe` 2
    writeIORef h.job $ fromMaybe (error "Missing rewritten boundary") $ Map.lookup nextEvent enqueued
    h.run False
    assertState h "succeeded"
    pushed <- Map.lookup "124" <$> readIORef h.remotes >>= maybe (fail "Missing pushed deck") pure
    pushed.deck.slots `shouldBe` corrected.slots

  for_ [False, True] \lostResponse ->
    it ("durably fences undone creation before re-answer and overwrite: " <> show lostResponse) do
      h <- newHarness
      let failing = if lostResponse
            then h.client {upgradeDeck = \token source xp metadata ->
              h.client.upgradeDeck token source xp metadata >> pure (Left $ ArkhamDBRemoteError "lost response")}
            else h.client {saveDeck = \_ _ _ -> pure $ Left $ ArkhamDBRemoteError "save failed"}
      runWith h failing False
      assertState h "failed"
      beforeUndo <- readIORef h.calls
      cancelHarnessUndo h 0
      fence <- readIORef h.job
      arkhamDBPushStatus fence `shouldBe` "conflict"
      for_ ["overwrite", "new", "keep"] \mode ->
        h.writes.resolvePush Linking.now Linking.userId Linking.deckId mode `shouldReturn` False
      -- A fresh boundary remains behind the durable oldest job, just as in
      -- the SQL store. Reconstruct the persisted rows after restart.
      let replacement = fence
            { arkhamDBPushEventKey = "game/1/iid/reanswered"
            , arkhamDBPushPayload = encodePushDeck currentDeck {Decklist.slots = Map.singleton "01007" 2}
            , arkhamDBPushStatus = "pending", arkhamDBPushStage = "ready"
            , arkhamDBPushTargetId = Nothing, arkhamDBPushSourceId = Nothing
            , arkhamDBPushError = Nothing
            }
          persisted = [disabledPush fence, replacement]
          oldest = find (\job -> arkhamDBPushStatus job `notElem` ["succeeded", "cancelled"]) persisted
      fmap arkhamDBPushEventKey oldest `shouldBe` Just (arkhamDBPushEventKey fence)
      for_ oldest \job -> pushCanRun (addUTCTime 3600 Linking.now) True job `shouldBe` False
      writeIORef h.job $ disabledPush fence
      h.run True
      h.run False
      calls <- readIORef h.calls
      calls `shouldBe` beforeUndo
      filter ("upgrade/" `T.isPrefixOf`) calls `shouldBe` ["upgrade/123/5"]
      filter ("save/" `T.isPrefixOf`) calls `shouldBe` []
      remote <- Map.lookup "124" <$> readIORef h.remotes >>= maybe (fail "Missing credited version") pure
      remote.deck.slots `shouldNotBe` currentDeck.slots

  it "coalesces saturated dispatch admission and leaves an unrelated small-pool connection available" do
    executor <- newArkhamDBExecutor
    pool <- newTBQueueIO 2
    atomically $ writeTBQueue pool () >> writeTBQueue pool ()
    started <- newEmptyMVar
    release <- newEmptyMVar
    workers <- newIORef (0 :: Int)
    let sweep = do
          atomically $ readTBQueue pool
          modifyIORef' workers (+1)
          void $ tryPutMVar started ()
          takeMVar release
          atomically $ writeTBQueue pool ()
        signal = do
          launch <- wakeArkhamDBExecutor executor
          when launch $ void $ forkIO $ runArkhamDBExecutor executor sweep
    signal
    timeout 2000000 (takeMVar started) `shouldReturn` Just ()
    replicateM_ 10000 signal
    readIORef workers `shouldReturn` 1
    timeout 100000 (atomically $ readTBQueue pool) `shouldReturn` Just ()
    tid <- readMVar executor.executorThread
    traverse_ killThread tid

  it "throttles per-user write-back attempts and backs off repeated failures" do
    (store, _) <- Linking.newStore
    Linking.linkAccount store
    account <- store.findAccount Linking.userId >>= maybe (fail "Missing account") pure
    let attempted = account {arkhamDBAccountLastWriteBackAttemptAt = Just Linking.now}
    writeBackAdmissionReady (addUTCTime 29 Linking.now) attempted `shouldBe` False
    writeBackAdmissionReady (addUTCTime 30 Linking.now) attempted `shouldBe` True
    writeBackAdmissionReady (addUTCTime 59 Linking.now) attempted {arkhamDBAccountWriteBackFailures = 1} `shouldBe` False
    map writeBackRetryDelay [0,1,2,3,4,10] `shouldBe` [30,60,120,240,300,300]

  it "automatically recovers an expired running claim after a crash before processing" do
    h <- newHarness
    claimed <- h.writes.claimPush Linking.now False Linking.userId Linking.deckId
    claimed `shouldSatisfy` isJust
    -- The process died after committing the claim. Polling before lease expiry
    -- cannot steal it, but the next sweep after expiry must make progress.
    runWriteBack h.store h.writes h.client Linking.settings (addUTCTime 60 Linking.now) False Linking.userId Linking.deckId
    assertState h "running"
    readIORef h.calls `shouldReturn` []
    runWriteBack h.store h.writes h.client Linking.settings (addUTCTime 61 Linking.now) False Linking.userId Linking.deckId
    assertState h "succeeded"
    filter (T.isPrefixOf "upgrade/") <$> readIORef h.calls `shouldReturn` ["upgrade/123/5"]

  it "automatically resumes a staged running claim without creating a second version" do
    h <- newHarness
    runWith h h.client {saveDeck = \_ _ _ -> pure $ Left $ ArkhamDBRemoteError "unavailable"} False
    claimTime <- getCurrentTime
    claimed <- h.writes.claimPush claimTime True Linking.userId Linking.deckId
    claimed `shouldSatisfy` isJust
    -- Crash after reclaiming the persisted saving stage.
    runWriteBack h.store h.writes h.client Linking.settings (addUTCTime 61 claimTime) False Linking.userId Linking.deckId
    assertState h "succeeded"
    filter (T.isPrefixOf "upgrade/") <$> readIORef h.calls `shouldReturn` ["upgrade/123/5"]
    filter (T.isPrefixOf "save/") <$> readIORef h.calls `shouldReturn` ["save/124"]

  it "does not automatically retry failed or conflicted jobs" do
    h <- newHarness
    for_ ["failed", "conflict"] \status -> do
      modifyIORef' h.job \job -> job {arkhamDBPushStatus = status}
      runWriteBack h.store h.writes h.client Linking.settings (addUTCTime 1000 Linking.now) False Linking.userId Linking.deckId
      assertState h status
    readIORef h.calls `shouldReturn` []

  it "does not queue another seat owner's associated deck" do
    h <- newHarness
    state <- readIORef h.linked
    deck <- maybe (fail "Missing deck") pure $ Map.lookup Linking.deckId state.storedDecks
    link <- maybe (fail "Missing link") pure $ Map.lookup Linking.deckId state.storedLinks
    ownsCampaignDeck Linking.userId deck link `shouldBe` True
    ownsCampaignDeck Linking.userId deck {arkhamDeckUserId = toSqlKey 999} link `shouldBe` False
    ownsCampaignDeck Linking.userId deck link {arkhamDBDeckLinkUserId = toSqlKey 999} `shouldBe` False

  it "snapshots campaign story cards and weaknesses with ReloadDecks reconciliation, not the investigator draw pile" do
    let card n code = MkPlayerCard (unsafeMakeCardId $ UUID.fromWords 0 0 0 n) Nothing code code mempty Nothing Nothing Nothing Nothing Nothing
        first = card 1 "01006"
        duplicate = card 2 "01006"
        weakness = card 3 "01100"
        story = card 4 "01007"
        side = card 5 "01008"
        snapshot = campaignDeckSnapshot currentDeck [first, duplicate, weakness, story] [PlayerCard story] [] (Just [side]) (Just TabooList25)
    snapshot.slots `shouldBe` Map.fromList [("01006", 2), ("01100", 1), ("01007", 1)]
    snapshot.sideSlots `shouldBe` Map.singleton "01008" 1
    snapshot.taboo_id `shouldBe` Just 10

  it "reconciles a submitted weakness with its original campaign story printing" do
    let card n code = MkPlayerCard (unsafeMakeCardId $ UUID.fromWords 0 0 0 n) Nothing code code mempty Nothing Nothing Nothing Nothing Nothing
        submitted = [card 1 "01006", card 2 "01006", card 3 "01103", card 4 "01099"]
        story = [PlayerCard $ card 5 "01603", PlayerCard $ card 6 "01099"]
        snapshot = campaignDeckSnapshot currentDeck submitted story [] Nothing Nothing
    snapshot.slots `shouldBe` Map.fromList [("01006", 2), ("01603", 1), ("01099", 1)]

  it "excludes campaign-invalid saved cards before counting export slots" do
    let card n code = MkPlayerCard (unsafeMakeCardId $ UUID.fromWords 0 0 0 n) Nothing code code mempty Nothing Nothing Nothing Nothing Nothing
        snapshot = campaignDeckSnapshot currentDeck [card 1 "01006", card 2 "01006", card 3 "01103"] [] ["01103"] Nothing Nothing
    snapshot.slots `shouldBe` Map.singleton "01006" 2

  it "preserves Joe Diamond sideSlots and extra_deck instead of exporting his hunch deck" do
    let card = MkPlayerCard (unsafeMakeCardId $ UUID.fromWords 0 0 0 20) Nothing "05010" "05010" mempty Nothing Nothing Nothing Nothing Nothing
        joe = currentDeck {Decklist.investigator_code = "05002", Decklist.meta = Just "{\"extra_deck\":{\"05010\":1}}"}
        snapshot = campaignDeckSnapshot joe [] [] [] (Just [card]) Nothing
    snapshot.sideSlots `shouldBe` joe.sideSlots
    snapshot.meta `shouldBe` joe.meta

  it "does not export encounter story cards as player deck slots" do
    let encounter = MkEncounterCard (unsafeMakeCardId $ UUID.fromWords 0 0 0 21) "01116" "01116" Nothing Nothing False Nothing
    (campaignDeckSnapshot currentDeck [] [EncounterCard encounter] [] Nothing Nothing).slots `shouldBe` mempty

  it "retains an original unknown remote taboo rather than clearing it" do
    let original = currentDeck {Decklist.taboo_id = Just 99}
    (campaignDeckSnapshot original [] [] [] Nothing Nothing).taboo_id `shouldBe` Just 99

  it "writes at scenario-end and completed-upgrade boundaries, not temporary per-card mutations" do
    isWriteBackBoundary (ForTarget GameTarget $ EndOfScenario Nothing) `shouldBe` True
    isWriteBackBoundary FinishedUpgradingDecks `shouldBe` True
    isWriteBackBoundary (GainXP "01001" GameSource 1) `shouldBe` False

  it "skips the campaign-start ChooseDecksStep with no XP or remote version" do
    campaignWriteBackBoundary (Just $ ChooseDecksStep PrologueStep) FinishedUpgradingDecks `shouldBe` Nothing
    campaignWriteBackBoundary (Just $ UpgradeDeckStep PrologueStep) FinishedUpgradingDecks `shouldBe` Just "upgrade"

  it "queues only after the upgrade drain reaches completion, not a remaining seat's AskMap" do
    let pid = PlayerId $ UUID.fromWords 0 0 0 101
        game = boundaryGame $ UpgradeDeckStep PrologueStep
    gameRef <- newIORef game
    queue <- newQueue
      [ AskMap $ Map.singleton pid ChooseUpgradeDeck
      , FinishedUpgradingDecks
      , AskMap $ Map.singleton pid ContinueCampaign
      ]
    gen <- newIORef $ mkStdGen 1
    let drain observers = runGameApp (GameApp gameRef queue gen (pure . const ()) Nothing) $
          Game.runMessages "write-back-upgrade" observers
    withCampaignWriteBackBoundary gameRef drain `shouldReturn` Nothing
    remaining <- readIORef $ queueToRef queue
    remaining `shouldSatisfy` elem FinishedUpgradingDecks
    fmap fst <$> withCampaignWriteBackBoundary gameRef drain `shouldReturn` Just "upgrade"

  it "does not enqueue the initial deck-choice completion observed by the drain" do
    let pid = PlayerId $ UUID.fromWords 0 0 0 101
        game = boundaryGame $ ChooseDecksStep PrologueStep
    gameRef <- newIORef game
    queue <- newQueue [FinishedUpgradingDecks, AskMap $ Map.singleton pid ContinueCampaign]
    gen <- newIORef $ mkStdGen 1
    withCampaignWriteBackBoundary gameRef (\observers -> runGameApp (GameApp gameRef queue gen (pure . const ()) Nothing) $
      Game.runMessages "write-back-initial-choice" observers) `shouldReturn` Nothing

  for_ [("scenarioEnd", ForTarget GameTarget $ EndOfScenario Nothing), ("upgrade", FinishedUpgradingDecks)] \(label, message) ->
    it ("captures " <> toString label <> " campaign and investigator state before a roster-swapping continuation") do
      let iid = "01001"
          pid = PlayerId $ UUID.fromWords 0 0 0 101
          card = MkPlayerCard (unsafeMakeCardId $ UUID.fromWords 0 0 0 555) Nothing "01006" "01006" mempty Nothing Nothing Nothing Nothing Nothing
          investigator = overAttrs (\attrs -> attrs {investigatorXp = 5}) $ lookupInvestigator iid pid
          initial = boundaryGame $ UpgradeDeckStep PrologueStep
          captured = initial
            { Game.gameEntities = initial.gameEntities {entitiesInvestigators = Map.singleton iid investigator}
            , Game.gameMode = This $ overAttrs (\attrs -> attrs
                {campaignStep = UpgradeDeckStep PrologueStep, campaignDecks = Map.singleton iid $ Deck [card], campaignStoryCards = Map.singleton iid [PlayerCard card]}) $ lookupCampaign "01" Easy
            }
          continuation = initial
            { Game.gameEntities = initial.gameEntities {entitiesInvestigators = mempty}
            , Game.gameMode = This $ lookupCampaign "06" Easy
            }
      gameRef <- newIORef captured
      -- Synthetic equivalent of Dream-Eaters' Waking Nightmare continuation:
      -- the boundary is popped, then campaign attributes/roster are swapped.
      observed <- withCampaignWriteBackBoundary gameRef \observers -> do
        for_ observers.observeMessage ($ message)
        writeIORef gameRef continuation
      fmap fst observed `shouldBe` Just label
      let snapshot = snd <$> observed
      fmap (Map.keys . gameInvestigators) snapshot `shouldBe` Just [iid]
      fmap (fmap (campaignDecks . toAttrs) . modeCampaign . Game.gameMode) snapshot
        `shouldBe` Just (Just $ Map.singleton iid $ Deck [card])
      fmap (fmap (campaignStoryCards . toAttrs) . modeCampaign . Game.gameMode) snapshot
        `shouldBe` Just (Just $ Map.singleton iid [PlayerCard card])
      fmap (map (investigatorXp . toAttrs) . Map.elems . gameInvestigators) snapshot `shouldBe` Just [5]
      Map.null . gameInvestigators <$> readIORef gameRef `shouldReturn` True

  it "does not enqueue a captured boundary when the later drain fails" do
    gameRef <- newIORef $ boundaryGame $ UpgradeDeckStep PrologueStep
    queued <- newIORef (0 :: Int)
    result <- try @IOException do
      snapshot <- withCampaignWriteBackBoundary gameRef \observers -> do
        for_ observers.observeMessage ($ FinishedUpgradingDecks)
        throwIO $ userError "later continuation failed"
      for_ snapshot $ const $ modifyIORef' queued (+1)
    result `shouldSatisfy` isLeft
    readIORef queued `shouldReturn` 0

  it "matches encoder-generated write-back contract fixtures" do
    generated <- writeBackFixture
    fixture <- Linking.loadNamedFixture "arkhamdb-write-back.json"
    generated `shouldBe` fixture

  it "encodes an actual undone creation as the governed boundaryUndone conflict" do
    h <- newHarness
    runWith h h.client {saveDeck = \_ _ _ -> pure $ Left $ ArkhamDBRemoteError "save failed"} False
    cancelHarnessUndo h 0
    statuses <- h.writes.deckWriteBackStatus Linking.userId
    case statuses of
      [status] -> do
        status.state `shouldBe` "conflict"
        fmap (.reason) status.conflict `shouldBe` Just "boundaryUndone"
        fixture <- Linking.loadNamedFixture "arkhamdb-write-back.json"
        case fixture of
          Object value -> case KM.lookup "states" value of
            Just (Array states) -> Just (toJSON status) `shouldBe` viaNonEmpty last (toList states)
            _ -> expectationFailure "Missing fixture states"
          _ -> expectationFailure "Invalid write-back fixture"
      _ -> expectationFailure "Expected one actual undone status"

  it "upgrades then saves the current campaign slots, story cards, weaknesses, sideboard, meta and taboo" do
    h <- newHarness
    h.run False
    assertState h "succeeded"
    readIORef h.calls `shouldReturn` ["load/123", "upgrade/123/5", "load/124", "save/124", "load/124"]
    pushed <- Map.lookup "124" <$> readIORef h.remotes
    let expected = either (const currentDeck) id $ markedDeck pushKey $ withExportedOverlay Nothing currentDeck
    fmap (samePushedDeck expected) pushed `shouldBe` Just True
    readIORef h.baseline `shouldReturn` 5

  for_ ["date", "version"] \marker ->
    it ("records a " <> toString marker <> " conflict and performs no remote mutation") do
      h <- newHarness
      modifyIORef' h.job \job -> job {arkhamDBPushExpectedDateUpdate = Just "older-queued-baseline"}
      modifyIORef' h.remotes $ Map.adjust (\remote -> if marker == "date" then remote {dateUpdate = Just "edited"} else remote {deckVersion = Just "9.0"}) "123"
      h.run False
      assertState h "conflict"
      assertNoPush h
      statuses <- h.writes.deckWriteBackStatus Linking.userId
      map (.conflict) statuses `shouldSatisfy` all isJust
      map (fmap (.lastSyncedDateUpdate) . (.conflict)) statuses `shouldBe` [Just Linking.remote.dateUpdate]

  it "refreshes an expired token under the generation lock without reentrant lock contention" do
    h <- newHarness
    modifyIORef' h.linked \state -> state {Linking.storedAccounts = Map.adjust (\account -> account {arkhamDBAccountExpiresAt = Just Linking.now}) Linking.userId state.storedAccounts}
    timeout 2000000 (h.run False) `shouldReturn` Just ()
    assertState h "succeeded"

  for_ ["write-back", "retry", "save-as-new"] \mode ->
    it ("accepts ArkhamDB's remote empty-string metadata through " <> toString mode) do
      h <- newHarness
      remote <- case fromJSON $ object
        [ "id" .= (123 :: Int), "slots" .= currentDeck.slots
        , "investigator_code" .= ("01001" :: Text), "investigator_name" .= ("Roland Banks" :: Text)
        , "meta" .= ("" :: Text), "next_deck" .= Null
        , "date_update" .= Linking.remote.dateUpdate, "version" .= Linking.remote.deckVersion
        ] of
          Error err -> fail err
          Success value -> pure (value :: ArkhamDBRemoteDeck)
      remote.deck.meta `shouldBe` Just ""
      writeIORef h.remotes $ Map.singleton "123" remote
      -- The boundary snapshot preserves the remote metadata representation.
      let snapshot = campaignDeckSnapshot remote.deck [] [] [] Nothing Nothing
      snapshot.meta `shouldBe` Just ""
      modifyIORef' h.job \job -> job {arkhamDBPushPayload = encodePushDeck snapshot}
      if mode == "retry" then do
        runWith h h.client {saveDeck = \_ _ _ -> pure $ Left $ ArkhamDBRemoteError "unavailable"} False
        assertState h "failed"
        arkhamDBPushError <$> readIORef h.job `shouldReturn` Just "remoteUnavailable"
        h.run True
      else do
        when (mode == "save-as-new") do
          writeIORef h.baseline 9
          modifyIORef' h.job \job -> job {arkhamDBPushStatus = "conflict"}
          h.writes.resolvePush Linking.now Linking.userId Linking.deckId "new" `shouldReturn` True
        h.run False
      assertState h "succeeded"
      let target = if mode == "save-as-new" then "125" else "124"
      pushed <- Map.lookup target <$> readIORef h.remotes >>= maybe (fail "Missing target") pure
      hasPushMarker pushKey pushed `shouldBe` True
      (decodeStrict' . encodeUtf8 =<< pushed.deck.meta :: Maybe Value) `shouldBe`
        Just (object ["ahn_push" .= pushMarker pushKey])
      readIORef h.baseline `shouldReturn` 5
      filter (T.isPrefixOf "upgrade/") <$> readIORef h.calls `shouldReturn`
        if mode == "save-as-new" then [] else ["upgrade/123/5"]

  for_ ["[]", "null", "not JSON", "\"\"", " "] \metadata ->
    it ("refuses nonempty invalid or non-object metadata without remote requests: " <> toString metadata) do
      h <- newHarness
      modifyIORef' h.job \job -> job {arkhamDBPushPayload = encodePushDeck currentDeck {Decklist.meta = Just metadata}}
      h.run False
      assertState h "failed"
      arkhamDBPushError <$> readIORef h.job `shouldReturn` Just "validationFailed"
      readIORef h.calls `shouldReturn` []

  it "does not push opted-out decks" do
    h <- newHarness
    _ <- h.writes.toggleWriteBack Linking.now Linking.userId Linking.deckId False
    -- Isolate the service preflight from the store's additional opt-out guard.
    let permissive = h.writes {claimPush = \_ _ _ _ -> do
          value <- readIORef h.job
          let claimed = value {arkhamDBPushStatus = "running"}
          writeIORef h.job claimed
          pure $ Just $ Entity pushKey claimed
          , checkpointPush = \_ next -> writeIORef h.job next >> pure True}
    runWriteBack h.store permissive h.client Linking.settings Linking.now False Linking.userId Linking.deckId
    assertNoPush h
    readIORef h.calls `shouldReturn` []

  it "fails without affecting the caller when ArkhamDB is down; explicit retry works" do
    h <- newHarness
    let down = h.client {loadDeck = \_ _ -> pure $ Left $ ArkhamDBRemoteError "private-token-detail"}
    runWith h down False
    assertState h "failed"
    arkhamDBPushError <$> readIORef h.job `shouldReturn` Just "remoteUnavailable"
    h.run True
    assertState h "succeeded"

  it "does not upgrade twice when a successful job is retried" do
    h <- newHarness
    h.run False
    before <- readIORef h.calls
    h.run True
    readIORef h.calls `shouldReturn` before

  it "conflicts on recovered creation without a stored target baseline, without upgrading again" do
    h <- newHarness
    let lost = h.client {upgradeDeck = \token source xp metadata -> do
          _ <- h.client.upgradeDeck token source xp metadata
          pure $ Left $ ArkhamDBRemoteError "lost response"}
    runWith h lost False
    assertState h "failed"
    h.run True
    assertState h "conflict"
    filter (T.isPrefixOf "upgrade/") <$> readIORef h.calls `shouldReturn` ["upgrade/123/5"]

  it "recovers a lost creation via its namespaced marker when no version chain is exposed" do
    h <- newHarness
    let lost = h.client {upgradeDeck = \token source xp metadata -> do
          _ <- h.client.upgradeDeck token source xp metadata
          modifyIORef' h.remotes $ Map.adjust (\remote -> remote {nextDeckId = Nothing, dateUpdate = Just "changed-source"}) source
          pure $ Left $ ArkhamDBRemoteError "lost response"}
    runWith h lost False
    h.run True
    assertState h "conflict"
    filter (T.isPrefixOf "upgrade/") <$> readIORef h.calls `shouldReturn` ["upgrade/123/5"]

  it "does not repeat creation when an uncertain outcome cannot be proven" do
    h <- newHarness
    let lost = h.client {upgradeDeck = \_ _ _ _ -> do
          modifyIORef' h.calls (<> ["upgrade/uncertain"])
          modifyIORef' h.remotes $ Map.adjust (\remote -> remote {dateUpdate = Just "changed-source"}) "123"
          pure (Left $ ArkhamDBRemoteError "lost")}
    runWith h lost False
    h.run True
    assertState h "failed"
    arkhamDBPushError <$> readIORef h.job `shouldReturn` Just "pushUncertain"
    filter (T.isPrefixOf "upgrade/") <$> readIORef h.calls `shouldReturn` ["upgrade/uncertain"]

  it "does not adopt another user's marked or unmarked next version after an uncertain upgrade" do
    h <- newHarness
    let lost = h.client {upgradeDeck = \token source xp _ -> do
          _ <- h.client.upgradeDeck token source xp (Just "{\"ahn_push\":\"someone-else\"}")
          pure $ Left $ ArkhamDBRemoteError "lost"}
    runWith h lost False
    h.run True
    assertState h "failed"
    filter (T.isPrefixOf "save/") <$> readIORef h.calls `shouldReturn` []

  it "recognizes a save whose response was lost without another upgrade or save" do
    h <- newHarness
    let lost = h.client {saveDeck = \token target deck -> do
          _ <- h.client.saveDeck token target deck
          pure $ Left $ ArkhamDBRemoteError "lost response"}
    runWith h lost False
    h.run True
    assertState h "succeeded"
    filter (\call -> "upgrade/" `T.isPrefixOf` call || "save/" `T.isPrefixOf` call) <$> readIORef h.calls
      `shouldReturn` ["upgrade/123/5", "save/124"]

  it "keeps the remote version by re-syncing only the saved local deck, with no push" do
    h <- newHarness
    modifyIORef' h.remotes $ Map.adjust (\remote -> remote {dateUpdate = Just "edited"}) "123"
    h.run False
    _ <- h.writes.resolvePush Linking.now Linking.userId Linking.deckId "keep"
    h.run False
    assertState h "cancelled"
    assertNoPush h
    state <- readIORef h.linked
    fmap arkhamDeckList (Map.lookup Linking.deckId state.storedDecks) `shouldBe` Just (remoteDecklist Linking.settings Linking.remote)

  it "keeps a materialized remote snapshot without applying its overlay twice" do
    h <- newHarness
    let overlay = DeckOverlay Nothing mempty (Map.singleton "01006" 1) mempty
    modifyIORef' h.linked \state -> state
      { Linking.storedDecks = Map.adjust (\deck -> deck {arkhamDeckOverlay = Just overlay}) Linking.deckId state.storedDecks }
    h.run False
    modifyIORef' h.job \job -> job {arkhamDBPushStatus = "conflict"}
    _ <- h.writes.resolvePush Linking.now Linking.userId Linking.deckId "keep"
    h.run False
    saved <- Map.lookup Linking.deckId . (.storedDecks) <$> readIORef h.linked
    fmap (Decklist.slots . arkhamDeckPlayList) saved `shouldBe` Just currentDeck.slots
    fmap arkhamDeckOverlay saved `shouldBe` Just Nothing

  for_ [False, True] \viaImport ->
    for_ ["upgrade", "reload", "replace" :: Text] \replacement ->
      it ("tracks actual seating then " <> toString replacement <> " before export and " <> if viaImport then "re-import" else "sync") do
        h <- newHarness
        (gameRef, pid, drain) <- newCampaignMessages
        let overlay = DeckOverlay Nothing mempty (Map.singleton "01007" 1) mempty
            base = Linking.remote.deck {Decklist.slots = Map.singleton "01006" 1, Decklist.meta = Nothing}
            upgraded = base {Decklist.investigator_code = if replacement == "replace" then "01002" else "01001"}
            nextIid = Decklist.decklistInvestigatorId upgraded
        modifyIORef' h.linked \state -> state
          { Linking.storedDecks = Map.adjust (\deck -> deck {arkhamDeckList = base, arkhamDeckOverlay = Just overlay}) Linking.deckId state.storedDecks }
        saved <- Map.lookup Linking.deckId . (.storedDecks) <$> readIORef h.linked >>= maybe (fail "Missing saved deck") pure
        initial <- readIORef gameRef
        drain $ deckChosenWithOverlay initial pid (arkhamDeckOverlay saved) $ arkhamDeckPlayList saved
        seated <- readIORef gameRef
        let seatedCampaign = maybe (error "Missing seated campaign") toAttrs $ modeCampaign seated.gameMode
        Map.lookup (campaignDeckOverlayKey "01001") (campaignStore seatedCampaign) `shouldBe` Just (toJSON overlay)
        map toCardCode (maybe [] unDeck $ Map.lookup "01001" $ campaignDecks seatedCampaign) `shouldContain` ["01007"]
        -- Actual server replacement path, NOT a hand-built campaign snapshot.
        -- The upgrade handler replaces this saved list but preserves its overlay.
        drain [case replacement of
          "upgrade" -> UpgradeDecklist "01001" upgraded
          "reload" -> LoadDecklist pid upgraded
          _ -> ReplaceInvestigator "01001" upgraded]
        modifyIORef' h.linked \state -> state
          { Linking.storedDecks = Map.adjust (\deck -> deck {arkhamDeckList = upgraded}) Linking.deckId state.storedDecks }
        replaced <- readIORef gameRef
        let attrs = maybe (error "Missing replaced campaign") toAttrs $ modeCampaign replaced.gameMode
            cards = maybe [] unDeck $ Map.lookup nextIid $ campaignDecks attrs
            snapshot = campaignWriteBackSnapshot (campaignStore attrs) nextIid upgraded cards [] [] Nothing Nothing
        map toCardCode cards `shouldBe` ["01006"]
        exportedOverlay snapshot `shouldBe` Just Null
        when (replacement == "replace") $
          Map.lookup (campaignDeckOverlayKey "01001") (campaignStore attrs) `shouldBe` Nothing
        modifyIORef' h.job \job -> job {arkhamDBPushPayload = encodePushDeck snapshot}
        h.run False
        assertState h "succeeded"
        replicateM_ 2 do
          result <- runExceptT (if viaImport
            then importRemoteDeck h.store h.client Linking.settings Linking.now Linking.userId "124" Linking.forbiddenPublic
            else syncRemoteDeck h.store h.client Linking.settings Linking.now Linking.userId Linking.deckId Linking.forbiddenPublic)
            >>= either (fail . show) pure
          arkhamDeckOverlay (entityVal result) `shouldBe` Just overlay
          Decklist.slots (arkhamDeckPlayList $ entityVal result) `shouldBe` Map.fromList [("01006", 1), ("01007", 1)]

  for_ [False, True] \viaImport ->
    it ("records actual alternate-front seating identity before export and " <> if viaImport then "re-import" else "sync") do
      h <- newHarness
      (gameRef, pid, drain) <- newCampaignMessages
      let overlay = DeckOverlay Nothing mempty (Map.singleton "01007" 1) mempty
          base = Linking.remote.deck
            {Decklist.slots = Map.singleton "01006" 1, Decklist.meta = Just "{\"alternate_front\":\"01002\"}"}
      modifyIORef' h.linked \state -> state
        { Linking.storedDecks = Map.adjust (\deck -> deck {arkhamDeckList = base, arkhamDeckOverlay = Just overlay}) Linking.deckId state.storedDecks }
      saved <- Map.lookup Linking.deckId . (.storedDecks) <$> readIORef h.linked >>= maybe (fail "Missing saved deck") pure
      initial <- readIORef gameRef
      drain $ deckChosenWithOverlay initial pid (arkhamDeckOverlay saved) $ arkhamDeckPlayList saved
      seated <- readIORef gameRef
      Map.keys (gameInvestigators seated) `shouldBe` ["01002"]
      let attrs = maybe (error "Missing seated campaign") toAttrs $ modeCampaign seated.gameMode
          cards = maybe [] unDeck $ Map.lookup "01002" $ campaignDecks attrs
          snapshot = campaignWriteBackSnapshot (campaignStore attrs) "01002" base cards [] [] Nothing Nothing
      Map.lookup (campaignDeckOverlayKey "01001") (campaignStore attrs) `shouldBe` Nothing
      Map.lookup (campaignDeckOverlayKey "01002") (campaignStore attrs) `shouldBe` Just (toJSON overlay)
      exportedOverlay snapshot `shouldBe` Just (toJSON overlay)
      modifyIORef' h.job \job -> job {arkhamDBPushPayload = encodePushDeck snapshot}
      h.run False
      assertState h "succeeded"
      replicateM_ 2 do
        result <- runExceptT (if viaImport
          then importRemoteDeck h.store h.client Linking.settings Linking.now Linking.userId "124" Linking.forbiddenPublic
          else syncRemoteDeck h.store h.client Linking.settings Linking.now Linking.userId Linking.deckId Linking.forbiddenPublic)
          >>= either (fail . show) pure
        arkhamDeckOverlay (entityVal result) `shouldBe` Nothing
        Decklist.slots (arkhamDeckPlayList $ entityVal result) `shouldBe` Map.fromList [("01006", 1), ("01007", 1)]

  for_ [False, True] \initialize ->
    it ("invalidates seated proof on an unproven raw " <> if initialize then "initialization" else "upgrade") do
      (gameRef, pid, drain) <- newCampaignMessages
      initial <- readIORef gameRef
      let overlay = DeckOverlay Nothing mempty (Map.singleton "01007" 1) mempty
          base = Linking.remote.deck {Decklist.slots = Map.singleton "01006" 1, Decklist.meta = Nothing}
      drain $ deckChosenWithOverlay initial pid (Just overlay) $ arkhamDeckPlayList $
        ArkhamDeck Linking.userId Nothing "Deck" "Roland" base (Just overlay) Nothing
      seated <- readIORef gameRef
      let attrs = maybe (error "Missing campaign") toAttrs $ modeCampaign seated.gameMode
          deck = fromMaybe (error "Missing seated deck") $ Map.lookup "01001" $ campaignDecks attrs
      drain [if initialize
        then InitDeck $ InitDeckAttrs "01001" Nothing Nothing deck
        else UpgradeDeck "01001" Nothing deck]
      replaced <- readIORef gameRef
      let store = campaignStore . toAttrs <$> modeCampaign replaced.gameMode
      (store >>= Map.lookup (campaignDeckOverlayKey "01001")) `shouldBe` Nothing

  for_ [False, True] \viaImport ->
    for_ [False, True] \hadOverlay ->
      it ("uses seated overlay provenance when the saved overlay changes before enqueue: " <> show (viaImport, hadOverlay)) do
        h <- newHarness
        let seated = DeckOverlay Nothing mempty (Map.singleton "01007" 1) mempty
            replacement = DeckOverlay Nothing mempty (Map.singleton "01008" 1) mempty
            provenance = recordCampaignDeckOverlay "01001" $ if hadOverlay then Just seated else Nothing
            store = case provenance of
              SetGlobal _ key value -> Map.singleton (Key.toText key) value
              _ -> error "Expected campaign provenance"
            card n code = MkPlayerCard (unsafeMakeCardId $ UUID.fromWords 0 0 0 n) Nothing code code mempty Nothing Nothing Nothing Nothing Nothing
            campaignCards = card 1 "01006" : [card 2 "01007" | hadOverlay]
        -- The saved row changes AFTER deck seating, but BEFORE the production
        -- snapshot builder runs. Its current overlay was never materialized.
        modifyIORef' h.linked \state -> state
          { Linking.storedDecks = Map.adjust (\deck -> deck {arkhamDeckOverlay = Just replacement}) Linking.deckId state.storedDecks }
        saved <- Map.lookup Linking.deckId . (.storedDecks) <$> readIORef h.linked >>= maybe (fail "Missing saved deck") pure
        let snapshot = campaignWriteBackSnapshot store "01001" (arkhamDeckList saved) campaignCards [] [] Nothing Nothing
        exportedOverlay snapshot `shouldBe` Just (if hadOverlay then toJSON seated else Null)
        modifyIORef' h.job \job -> job {arkhamDBPushPayload = encodePushDeck snapshot}
        h.run False
        assertState h "succeeded"
        replicateM_ 2 do
          result <- runExceptT (if viaImport
            then importRemoteDeck h.store h.client Linking.settings Linking.now Linking.userId "124" Linking.forbiddenPublic
            else syncRemoteDeck h.store h.client Linking.settings Linking.now Linking.userId Linking.deckId Linking.forbiddenPublic)
            >>= either (fail . show) pure
          arkhamDeckOverlay (entityVal result) `shouldBe` Just replacement
          Decklist.slots (arkhamDeckPlayList $ entityVal result) `shouldBe`
            Map.fromList ([("01006", 1), ("01008", 1)] <> [("01007", 1) | hadOverlay])

  for_ [False, True] \viaImport ->
    it ("refuses ambiguous legacy overlay adoption without clearing or applying it twice: " <> show viaImport) do
      h <- newHarness
      let overlay = DeckOverlay Nothing mempty (Map.singleton "01006" 1) mempty
          card n = MkPlayerCard (unsafeMakeCardId $ UUID.fromWords 0 0 0 n) Nothing "01006" "01006" mempty Nothing Nothing Nothing Nothing Nothing
          -- A legacy campaign has no seating provenance. Even an inherited
          -- metadata claim on the current saved list is not trusted.
          snapshot = campaignWriteBackSnapshot mempty "01001" (withExportedOverlay (Just overlay) currentDeck) [card 1, card 2] [] [] Nothing Nothing
      exportedOverlay snapshot `shouldBe` Nothing
      modifyIORef' h.linked \state -> state
        { Linking.storedDecks = Map.adjust (\deck -> deck {arkhamDeckOverlay = Just overlay}) Linking.deckId state.storedDecks }
      beforeSync <- Map.lookup Linking.deckId . (.storedDecks) <$> readIORef h.linked
      modifyIORef' h.job \job -> job {arkhamDBPushPayload = encodePushDeck snapshot}
      h.run False
      assertState h "succeeded"
      result <- runExceptT (if viaImport
        then importRemoteDeck h.store h.client Linking.settings Linking.now Linking.userId "124" Linking.forbiddenPublic
        else syncRemoteDeck h.store h.client Linking.settings Linking.now Linking.userId Linking.deckId Linking.forbiddenPublic)
      fmap arkhamDBErrorCode (either Just (const Nothing) result) `shouldBe` Just "validationFailed"
      afterSync <- Map.lookup Linking.deckId . (.storedDecks) <$> readIORef h.linked
      fmap (toJSON . arkhamDeckPlayList) afterSync `shouldBe` fmap (toJSON . arkhamDeckPlayList) beforeSync
      fmap arkhamDeckOverlay afterSync `shouldBe` Just (Just overlay)

  for_ [False, True] \viaImport ->
    for_ [False, True] \replacement ->
      it ("preserves unexported overlays before first adoption: " <> show (viaImport, replacement)) do
        h <- newHarness
        let exported = DeckOverlay Nothing mempty (Map.singleton "01006" 1) mempty
            current = DeckOverlay Nothing mempty (Map.singleton "01007" 2) mempty
        when replacement $ modifyIORef' h.job \job -> job
          {arkhamDBPushPayload = encodePushDeck $ withExportedOverlay (Just exported) currentDeck}
        h.run False
        modifyIORef' h.linked \state -> state
          { Linking.storedDecks = Map.adjust (\deck -> deck {arkhamDeckOverlay = Just current}) Linking.deckId state.storedDecks }
        replicateM_ 2 do
          result <- runExceptT (if viaImport
            then importRemoteDeck h.store h.client Linking.settings Linking.now Linking.userId "124" Linking.forbiddenPublic
            else syncRemoteDeck h.store h.client Linking.settings Linking.now Linking.userId Linking.deckId Linking.forbiddenPublic)
            >>= either (fail . show) pure
          arkhamDeckOverlay (entityVal result) `shouldBe` Just current

  for_ ["normal", "overwrite"] \mode ->
    it ("rejects a directly imported descendant owned by another campaign: " <> toString mode) do
      h <- newHarness
      let otherId = ArkhamDeckKey $ UUID.fromWords 0 0 0 999
      modifyIORef' h.linked \state -> state
        { Linking.storedLinks = case Map.lookup Linking.deckId state.storedLinks of
            Nothing -> mempty
            Just link -> Map.insert otherId link
              { arkhamDBDeckLinkDeckId = otherId, arkhamDBDeckLinkRemoteDeckId = "124"
              , arkhamDBDeckLinkCampaignKey = Just "campaign-a/iid" } $
              Map.singleton Linking.deckId link {arkhamDBDeckLinkRemoteDeckId = "125"}
        , Linking.storedDecks = maybe state.storedDecks (\deck -> Map.insert otherId deck state.storedDecks) $
            Map.lookup Linking.deckId state.storedDecks
        }
      modifyIORef' h.remotes $ Map.insert "124" Linking.remote
        {ArkhamDB.remoteDeckId = "124", nextDeckId = Just "125"} .
        Map.insert "125" Linking.remote {ArkhamDB.remoteDeckId = "125"}
      modifyIORef' h.job \job -> job {arkhamDBPushMode = mode}
      h.run False
      assertState h "conflict"
      arkhamDBPushError <$> readIORef h.job `shouldReturn` Just "campaignChanged"
      assertNoPush h
      h.writes.resolvePush Linking.now Linking.userId Linking.deckId "new" `shouldReturn` True
      h.run False
      assertState h "succeeded"
      state <- readIORef h.linked
      arkhamDBDeckLinkRemoteDeckId <$> Map.lookup otherId state.storedLinks `shouldBe` Just "124"

  for_ ["missing", "cycle", "incomplete", "budget"] \ambiguity ->
    it ("fails closed on ambiguous other-campaign lineage: " <> toString ambiguity) do
      h <- newHarness
      let otherId = ArkhamDeckKey $ UUID.fromWords 0 0 0 999
      modifyIORef' h.linked \state -> state
        { Linking.storedLinks = case Map.lookup Linking.deckId state.storedLinks of
            Nothing -> mempty
            Just link -> Map.insert otherId link
              { arkhamDBDeckLinkDeckId = otherId, arkhamDBDeckLinkRemoteDeckId = "200"
              , arkhamDBDeckLinkCampaignKey = Just "campaign-a/iid" } state.storedLinks
        , Linking.storedDecks = maybe state.storedDecks (\deck -> Map.insert otherId deck state.storedDecks) $
            Map.lookup Linking.deckId state.storedDecks
        }
      case ambiguity of
        "cycle" -> modifyIORef' h.remotes $ Map.insert "200" Linking.remote
          {ArkhamDB.remoteDeckId = "200", nextDeckId = Just "200"}
        "incomplete" -> modifyIORef' h.remotes $ Map.insert "200" Linking.remote
          {ArkhamDB.remoteDeckId = "200", nextDeckKnownAbsent = False}
        "budget" -> modifyIORef' h.remotes \remotes -> foldl' (\values n ->
          Map.insert (T.pack $ show n) Linking.remote
            {ArkhamDB.remoteDeckId = T.pack $ show n, nextDeckId = Just $ T.pack $ show $ n + 1} values) remotes [200 .. 301 :: Int]
        _ -> pure ()
      h.run False
      assertState h "conflict"
      arkhamDBPushError <$> readIORef h.job `shouldReturn` Just "campaignChanged"
      assertNoPush h
      calls <- readIORef h.calls
      length calls `shouldSatisfy` (<= 101)

  for_ [False, True] \viaImport ->
    it ("preserves overlays attached after adopting a push through " <> if viaImport then "repeat-import sync" else "sync") do
      h <- newHarness
      h.run False
      assertState h "succeeded"
      let sync = runExceptT (if viaImport
            then importRemoteDeck h.store h.client Linking.settings Linking.now Linking.userId "124" Linking.forbiddenPublic
            else syncRemoteDeck h.store h.client Linking.settings Linking.now Linking.userId Linking.deckId Linking.forbiddenPublic)
            >>= either (fail . show) pure
          overlay = DeckOverlay Nothing mempty (Map.singleton "01006" 1) mempty
      _ <- sync
      modifyIORef' h.linked \state -> state
        { Linking.storedDecks = Map.adjust (\deck -> deck {arkhamDeckOverlay = Just overlay}) Linking.deckId state.storedDecks }
      replicateM_ 2 do
        result <- sync
        arkhamDeckOverlay (entityVal result) `shouldBe` Just overlay
        Map.lookup "01006" (Decklist.slots $ arkhamDeckPlayList $ entityVal result) `shouldBe` Just 2
        saved <- Map.lookup Linking.deckId . (.storedDecks) <$> readIORef h.linked >>= maybe (fail "Missing saved deck") pure
        arkhamDeckOverlay saved `shouldBe` Just overlay

  it "rejects ancestor re-import overwrite into another campaign's resolved source lineage" do
    h <- newHarness
    -- A remains linked at 124, but the lineage continues to 125. Leave the harness's next local id available
    -- so the real repeat-ancestor import creates B's independently unbound row.
    let otherId = ArkhamDeckKey $ UUID.fromWords 0 0 0 999
    modifyIORef' h.linked \state -> state
      { Linking.storedLinks = case Map.lookup Linking.deckId state.storedLinks of
          Nothing -> mempty
          Just link -> Map.singleton otherId link
            { arkhamDBDeckLinkDeckId = otherId, arkhamDBDeckLinkRemoteDeckId = "124"
            , arkhamDBDeckLinkCampaignKey = Just "campaign-a/iid" }
      , Linking.storedDecks = maybe mempty (Map.singleton otherId) $ Map.lookup Linking.deckId state.storedDecks
      , Linking.storedNextDeck = 23 }
    modifyIORef' h.remotes $ Map.insert "124" Linking.remote
      {ArkhamDB.remoteDeckId = "124", dateUpdate = Just "campaign-a", nextDeckId = Just "125"} .
      Map.insert "125" Linking.remote {ArkhamDB.remoteDeckId = "125", dateUpdate = Just "later-version"} .
      Map.adjust (\remote -> remote {nextDeckId = Just "124"}) "123"
    imported <- runExceptT $ importRemoteDeck h.store h.client Linking.settings Linking.now Linking.userId "123" Linking.forbiddenPublic
    fmap entityKey imported `shouldBe` Right Linking.deckId
    modifyIORef' h.linked \state -> state
      { Linking.storedLinks = Map.adjust (\link -> link {arkhamDBDeckLinkWriteBack = True}) Linking.deckId state.storedLinks }
    h.run False
    assertState h "conflict"
    h.writes.resolvePush Linking.now Linking.userId Linking.deckId "overwrite" `shouldReturn` True
    h.run False
    assertState h "conflict"
    arkhamDBPushError <$> readIORef h.job `shouldReturn` Just "campaignChanged"
    h.writes.resolvePush Linking.now Linking.userId Linking.deckId "overwrite" `shouldReturn` False
    assertNoPush h
    readIORef h.baseline `shouldReturn` 0

  for_ [False, True] \viaImport ->
    for_ [False, True] \materialized ->
      it ((if materialized then "adopts a materialized campaign result" else "retains the overlay for an unmaterialized remote deck") <>
          " through ordinary linked " <> if viaImport then "repeat-import sync" else "sync") do
        h <- newHarness
        let overlay = DeckOverlay Nothing mempty (Map.singleton "01006" 1) mempty
        modifyIORef' h.linked \state -> state
          { Linking.storedDecks = Map.adjust (\deck -> deck {arkhamDeckOverlay = Just overlay}) Linking.deckId state.storedDecks }
        -- The normal export includes the overlay in its campaign slots, and
        -- leaves the saved local list/overlay untouched until linked sync.
        when materialized do
          let provenance = recordCampaignDeckOverlay "01001" $ Just overlay
              store = case provenance of
                SetGlobal _ key value -> Map.singleton (Key.toText key) value
                _ -> error "Expected campaign provenance"
              card n code = MkPlayerCard (unsafeMakeCardId $ UUID.fromWords 0 0 0 n) Nothing code code mempty Nothing Nothing Nothing Nothing Nothing
              cards = [card 1 "01006", card 2 "01100", card 3 "01007"]
              snapshot = campaignWriteBackSnapshot store "01001" currentDeck cards [] [] Nothing Nothing
          modifyIORef' h.job \job -> job
            {arkhamDBPushPayload = encodePushDeck snapshot}
          h.run False
          assertState h "succeeded"
        let target = if materialized then "124" else "123"
        result <- runExceptT (if viaImport
          then importRemoteDeck h.store h.client Linking.settings Linking.now Linking.userId target Linking.forbiddenPublic
          else syncRemoteDeck h.store h.client Linking.settings Linking.now Linking.userId Linking.deckId Linking.forbiddenPublic)
          >>= either (fail . show) pure
        let expectedSlots = if materialized then currentDeck.slots else Map.singleton "01006" 1
            expectedOverlay = if materialized then Nothing else Just overlay
        -- The response and SQL assignment list must agree, not just the list.
        Decklist.slots (arkhamDeckPlayList $ entityVal result) `shouldBe` expectedSlots
        arkhamDeckOverlay (entityVal result) `shouldBe` expectedOverlay
        saved <- Map.lookup Linking.deckId . (.storedDecks) <$> readIORef h.linked >>= maybe (fail "Missing saved deck") pure
        Decklist.slots (arkhamDeckPlayList saved) `shouldBe` expectedSlots
        arkhamDeckOverlay saved `shouldBe` expectedOverlay
        again <- runExceptT $ syncRemoteDeck h.store h.client Linking.settings Linking.now Linking.userId Linking.deckId Linking.forbiddenPublic
        fmap (arkhamDeckOverlay . entityVal) again `shouldBe` Right expectedOverlay

  for_ ["keep", "finish", "copy"] \completion ->
    it ("records a target-link conflict atomically on " <> toString completion <> " completion") do
      h <- newHarness
      let otherId = ArkhamDeckKey $ UUID.fromWords 0 0 0 999
          claimTarget target = modifyIORef' h.linked \state -> state
            { Linking.storedLinks = case Map.lookup Linking.deckId state.storedLinks of
                Nothing -> state.storedLinks
                Just link -> Map.insert otherId link
                  {arkhamDBDeckLinkDeckId = otherId, arkhamDBDeckLinkRemoteDeckId = target} state.storedLinks }
      if completion == "keep" then do
        -- Another saved deck already imported the successor of this lineage.
        modifyIORef' h.remotes $ Map.insert "124" Linking.remote {ArkhamDB.remoteDeckId = "124"} .
          Map.adjust (\remote -> remote {nextDeckId = Just "124"}) "123"
        claimTarget "124"
        h.run False
        h.writes.resolvePush Linking.now Linking.userId Linking.deckId "keep" `shouldReturn` True
        h.run False
      else do
        when (completion == "copy") do
          modifyIORef' h.linked \state -> state
            { Linking.storedLinks = Map.adjust (\link -> link {arkhamDBDeckLinkCampaignKey = Just "other/iid"}) Linking.deckId state.storedLinks }
          modifyIORef' h.job \job -> job {arkhamDBPushStatus = "conflict"}
          h.writes.resolvePush Linking.now Linking.userId Linking.deckId "new" `shouldReturn` True
        -- Simulate import claiming the target after remote save, before local
        -- completion. The SQL store serializes the lookup/write with imports.
        let raced = h.client {saveDeck = \token target deck -> do
              result <- h.client.saveDeck token target deck
              claimTarget target
              pure result}
        runWith h raced False
      assertState h "conflict"
      arkhamDBPushError <$> readIORef h.job `shouldReturn` Just "targetAlreadyLinked"
      readIORef h.baseline `shouldReturn` 0
      state <- readIORef h.linked
      Map.size state.storedLinks `shouldBe` 2
      fmap arkhamDBDeckLinkRemoteDeckId (Map.lookup Linking.deckId state.storedLinks) `shouldBe` Just "123"
      fmap arkhamDeckList (Map.lookup Linking.deckId state.storedDecks) `shouldBe` Just (remoteDecklist Linking.settings Linking.remote)
      before <- readIORef h.calls
      runWriteBack h.store h.writes h.client Linking.settings (addUTCTime 1000 Linking.now) False Linking.userId Linking.deckId
      readIORef h.calls `shouldReturn` before
      h.writes.resolvePush Linking.now Linking.userId Linking.deckId "new" `shouldReturn` True

  for_ [False, True] \uncertain ->
    it ("preserves " <> (if uncertain then "uncertain" else "created") <> " XP accounting when keep-remote loading fails") do
      h <- newHarness
      let failed = if uncertain then h.client {upgradeDeck = \token source xp metadata -> do
            _ <- h.client.upgradeDeck token source xp metadata
            pure $ Left $ ArkhamDBRemoteError "lost reply"}
            else h.client {saveDeck = \_ _ _ -> pure $ Left $ ArkhamDBRemoteError "unavailable"}
      runWith h failed False
      intent <- readIORef h.job
      h.writes.resolvePush Linking.now Linking.userId Linking.deckId "keep" `shouldReturn` True
      runWith h h.client {loadDeck = \_ _ -> pure $ Left $ ArkhamDBRemoteError "unavailable"} False
      assertState h "failed"
      retained <- readIORef h.job
      arkhamDBPushStage retained `shouldBe` arkhamDBPushStage intent
      arkhamDBPushSourceId retained `shouldBe` Just "123"
      arkhamDBPushTargetId retained `shouldBe` arkhamDBPushTargetId intent
      h.writes.resolvePush Linking.now Linking.userId Linking.deckId "overwrite" `shouldReturn` False
      h.run True
      assertState h "cancelled"
      readIORef h.baseline `shouldReturn` 5
      filter (T.isPrefixOf "upgrade/") <$> readIORef h.calls `shouldReturn` ["upgrade/123/5"]

  for_ [False, True] \uncertain ->
    it ("reconciles " <> (if uncertain then "uncertain" else "created") <> " +5 XP across disable and re-enable before the next boundary") do
      h <- newHarness
      let failed = if uncertain then h.client {upgradeDeck = \token source xp metadata -> do
            _ <- h.client.upgradeDeck token source xp metadata
            pure $ Left $ ArkhamDBRemoteError "lost reply"}
            else h.client {saveDeck = \_ _ _ -> pure $ Left $ ArkhamDBRemoteError "unavailable"}
      runWith h failed False
      intent <- readIORef h.job
      h.writes.toggleWriteBack Linking.now Linking.userId Linking.deckId False `shouldReturn` True
      before <- readIORef h.calls
      h.run True
      readIORef h.calls `shouldReturn` before
      h.writes.toggleWriteBack Linking.now Linking.userId Linking.deckId True `shouldReturn` True
      -- The old intent still heads the queue; re-enable cannot auto-retry it
      -- or let a later boundary replace it and submit its XP again.
      h.run False
      assertState h "failed"
      arkhamDBPushStage <$> readIORef h.job `shouldReturn` arkhamDBPushStage intent
      h.writes.resolvePush Linking.now Linking.userId Linking.deckId "overwrite" `shouldReturn` False
      h.writes.resolvePush Linking.now Linking.userId Linking.deckId "keep" `shouldReturn` True
      h.run False
      readIORef h.baseline `shouldReturn` 5
      -- Submit the next observed boundary, retaining cumulative XP = 5.
      modifyIORef' h.job \job -> job
        { arkhamDBPushStatus = "pending", arkhamDBPushStage = "ready", arkhamDBPushMode = "normal"
        , arkhamDBPushSourceId = Nothing, arkhamDBPushTargetId = Nothing, arkhamDBPushBoundary = "upgrade"
        , arkhamDBPushEventKey = "game/2/iid" }
      h.run False
      assertState h "succeeded"
      filter (\call -> "upgrade/" `T.isPrefixOf` call && "/5" `T.isSuffixOf` call) <$> readIORef h.calls `shouldReturn` ["upgrade/123/5"]

  it "explicit overwrite creates a version despite the recorded remote conflict" do
    h <- newHarness
    modifyIORef' h.remotes $ Map.adjust (\remote -> remote {dateUpdate = Just "edited"}) "123"
    h.run False
    _ <- h.writes.resolvePush Linking.now Linking.userId Linking.deckId "overwrite"
    h.run False
    assertState h "succeeded"
    filter (T.isPrefixOf "upgrade/") <$> readIORef h.calls `shouldReturn` ["upgrade/123/5"]

  it "save-as-new creates and saves an independent deck and reuses the single-campaign local link" do
    h <- newHarness
    modifyIORef' h.job \job -> job {arkhamDBPushStatus = "conflict"}
    _ <- h.writes.resolvePush Linking.now Linking.userId Linking.deckId "new"
    h.run False
    assertState h "succeeded"
    filter (\call -> "upgrade/" `T.isPrefixOf` call || call == "new" || "save/" `T.isPrefixOf` call) <$> readIORef h.calls
      `shouldReturn` ["new", "save/125"]
    state <- readIORef h.linked
    Map.size state.storedLinks `shouldBe` 1
    fmap arkhamDBDeckLinkRemoteDeckId (Map.lookup Linking.deckId state.storedLinks) `shouldBe` Just "125"

  for_ [("deleted", ArkhamDBNotFound), ("revoked", ArkhamDBUnauthorized)] \(label, sourceError) ->
    it ("saves as new when the old source is " <> label) do
      h <- newHarness
      modifyIORef' h.job \job -> job {arkhamDBPushStatus = "conflict"}
      _ <- h.writes.resolvePush Linking.now Linking.userId Linking.deckId "new"
      let unavailable = h.client {loadDeck = \token remoteId ->
            if remoteId == "123" then pure $ Left sourceError else h.client.loadDeck token remoteId}
      runWith h unavailable False
      assertState h "succeeded"
      readIORef h.calls `shouldReturn` ["new", "load/125", "save/125", "load/125"]
      arkhamDBPushSourceId <$> readIORef h.job `shouldReturn` Nothing

  it "keeps save-as-new ahead of later queued snapshots with a fresh push identity" do
    h <- newHarness
    original <- readIORef h.job
    let failed = original {arkhamDBPushStatus = "failed", arkhamDBPushStage = "created", arkhamDBPushTargetId = Just "124"}
        later = original {arkhamDBPushCreatedAt = addUTCTime 30 Linking.now, arkhamDBPushEventKey = "game/2/iid"}
        replacement = saveAsNewPush (addUTCTime 60 Linking.now) failed
        freshKey = ArkhamDBPushKey $ UUID.fromWords 0 0 0 999
        ordered = sortOn (\(Entity key job) -> (arkhamDBPushCreatedAt job, key))
          [Entity pushKey later, Entity freshKey replacement]
    map entityKey ordered `shouldBe` [freshKey, pushKey]
    arkhamDBPushCreatedAt replacement `shouldBe` arkhamDBPushCreatedAt original
    arkhamDBPushUpdatedAt replacement `shouldBe` addUTCTime 60 Linking.now
    arkhamDBPushEventKey replacement `shouldNotBe` arkhamDBPushEventKey original
    arkhamDBPushStage replacement `shouldBe` "ready"
    arkhamDBPushTargetId replacement `shouldBe` Nothing

  it "blocks a second campaign and save-as-new clones only its local association" do
    h <- newHarness
    let overlay = DeckOverlay Nothing mempty (Map.singleton "01006" 1) mempty
    modifyIORef' h.linked \state -> state
      { Linking.storedLinks = Map.adjust (\link -> link {arkhamDBDeckLinkCampaignKey = Just "first-campaign/iid"}) Linking.deckId state.storedLinks
      , Linking.storedDecks = Map.adjust (\deck -> deck {arkhamDeckOverlay = Just overlay}) Linking.deckId state.storedDecks
      }
    h.run False
    assertState h "conflict"
    arkhamDBPushError <$> readIORef h.job `shouldReturn` Just "campaignChanged"
    h.writes.resolvePush Linking.now Linking.userId Linking.deckId "overwrite" `shouldReturn` False
    assertNoPush h
    _ <- h.writes.resolvePush Linking.now Linking.userId Linking.deckId "new"
    h.run False
    state <- readIORef h.linked
    Map.size state.storedLinks `shouldBe` 2
    fmap arkhamDBDeckLinkRemoteDeckId (Map.lookup Linking.deckId state.storedLinks) `shouldBe` Just "123"
    fmap (Decklist.slots . arkhamDeckList) (Map.lookup (ArkhamDeckKey $ UUID.fromWords 0 0 0 246) state.storedDecks)
      `shouldBe` Just currentDeck.slots
    let copyId = ArkhamDeckKey $ UUID.fromWords 0 0 0 246
        copy = Map.lookup copyId state.storedDecks
        canonicalUrl = Just $ publicDeckUrl Linking.settings "125"
    fmap (Decklist.slots . arkhamDeckPlayList) copy `shouldBe` Just currentDeck.slots
    fmap arkhamDeckOverlay copy `shouldBe` Just Nothing
    fmap arkhamDeckOverlay (Map.lookup Linking.deckId state.storedDecks) `shouldBe` Just (Just overlay)
    fmap arkhamDeckUrl copy `shouldBe` Just canonicalUrl
    fmap (Decklist.url . arkhamDeckList) copy `shouldBe` Just canonicalUrl

  it "rejects overwrite of an already created target instead of sending earned XP twice" do
    h <- newHarness
    runWith h h.client {saveDeck = \_ _ _ -> pure $ Left $ ArkhamDBRemoteError "unavailable"} False
    h.writes.resolvePush Linking.now Linking.userId Linking.deckId "overwrite" `shouldReturn` False
    h.run True
    assertState h "succeeded"
    filter (T.isPrefixOf "upgrade/") <$> readIORef h.calls `shouldReturn` ["upgrade/123/5"]

  it "does not overwrite edits after the first created-target load failed" do
    h <- newHarness
    let unavailable = h.client {loadDeck = \token remoteId -> if remoteId == "124" then pure $ Left $ ArkhamDBRemoteError "unavailable" else h.client.loadDeck token remoteId}
    runWith h unavailable False
    arkhamDBPushStage <$> readIORef h.job `shouldReturn` "created"
    modifyIORef' h.remotes $ Map.adjust (\remote -> remote {dateUpdate = Just "external-edit", deck = remote.deck {Decklist.slots = Map.singleton "01009" 2}}) "124"
    h.run True
    assertState h "conflict"
    filter (T.isPrefixOf "save/") <$> readIORef h.calls `shouldReturn` []

  it "returns an unsent upgrade to ready only with explicit next_deck null on the unchanged source" do
    h <- newHarness
    source <- parseRecoverySource ["next_deck" .= Null]
    modifyIORef' h.remotes $ Map.insert "123" source
    let unsent = h.client {upgradeDeck = \_ _ _ _ -> pure $ Left $ ArkhamDBRemoteError "failed before transmission"}
    runWith h unsent False
    h.run True
    arkhamDBPushStage <$> readIORef h.job `shouldReturn` "ready"
    arkhamDBPushError <$> readIORef h.job `shouldReturn` Just "remoteUnavailable"
    assertNoPush h
    h.run True
    assertState h "succeeded"

  for_ [[], ["next_deck_id" .= Null]] \successorFields ->
    it ("conflicts on an uncertain upgrade when next_deck is missing: " <> show successorFields) do
      h <- newHarness
      source <- parseRecoverySource successorFields
      modifyIORef' h.remotes $ Map.insert "123" source
      let unsent = h.client {upgradeDeck = \_ _ _ _ -> pure $ Left $ ArkhamDBRemoteError "lost reply"}
      runWith h unsent False
      h.run True
      arkhamDBPushStage <$> readIORef h.job `shouldReturn` "creating"
      assertState h "conflict"
      arkhamDBPushError <$> readIORef h.job `shouldReturn` Just "pushUncertain"
      h.run True
      assertNoPush h

  it "does not overwrite a known target edited after a failed save" do
    h <- newHarness
    let lost = h.client {saveDeck = \_ _ _ -> pure $ Left $ ArkhamDBRemoteError "unavailable"}
    runWith h lost False
    modifyIORef' h.remotes $ Map.adjust (\remote -> remote {dateUpdate = Just "external-edit", deck = remote.deck {Decklist.slots = Map.singleton "01009" 2}}) "124"
    h.run True
    assertState h "conflict"
    filter (T.isPrefixOf "save/") <$> readIORef h.calls `shouldReturn` []

  it "saves the subsequent upgrade into this campaign's unchanged scenario-end version" do
    h <- newHarness
    h.run False
    modifyIORef' h.job \job -> job
      { arkhamDBPushStatus = "pending", arkhamDBPushStage = "ready", arkhamDBPushTargetId = Nothing
      , arkhamDBPushBoundary = "upgrade", arkhamDBPushPayload = encodePushDeck currentDeck {Decklist.slots = Map.singleton "01009" 2}}
    h.run False
    assertState h "succeeded"
    filter (T.isPrefixOf "upgrade/") <$> readIORef h.calls `shouldReturn` ["upgrade/123/5"]
    filter (T.isPrefixOf "save/") <$> readIORef h.calls `shouldReturn` ["save/124", "save/124"]
    fmap (.deck.slots) . Map.lookup "124" <$> readIORef h.remotes `shouldReturn` Just (Map.singleton "01009" 2)

  it "reuses the scenario-end version when side-story costs lower XP below the baseline" do
    h <- newHarness
    h.run False
    modifyIORef' h.job \job -> job
      { arkhamDBPushStatus = "pending", arkhamDBPushStage = "ready", arkhamDBPushTargetId = Nothing
      , arkhamDBPushBoundary = "upgrade", arkhamDBPushTotalXp = 4
      , arkhamDBPushPayload = encodePushDeck currentDeck {Decklist.slots = Map.singleton "01009" 2}}
    h.run False
    assertState h "succeeded"
    filter (T.isPrefixOf "upgrade/") <$> readIORef h.calls `shouldReturn` ["upgrade/123/5"]
    filter (T.isPrefixOf "save/") <$> readIORef h.calls `shouldReturn` ["save/124", "save/124"]
    fmap (.deck.slots) . Map.lookup "124" <$> readIORef h.remotes `shouldReturn` Just (Map.singleton "01009" 2)
    -- Inspect the next grant as well as the lower-XP save: 5 -> 4 -> 7
    -- must credit 5 then 2, never 5 then 3.
    modifyIORef' h.job \job -> job
      { arkhamDBPushStatus = "pending", arkhamDBPushStage = "ready", arkhamDBPushTargetId = Nothing
      , arkhamDBPushBoundary = "scenarioEnd", arkhamDBPushTotalXp = 7 }
    h.run False
    assertState h "succeeded"
    filter (T.isPrefixOf "upgrade/") <$> readIORef h.calls `shouldReturn` ["upgrade/123/5", "upgrade/124/2"]
    readIORef h.baseline `shouldReturn` 7

  it "creates a further version rather than dropping XP granted during the upgrade" do
    h <- newHarness
    h.run False
    modifyIORef' h.job \job -> job
      { arkhamDBPushStatus = "pending", arkhamDBPushStage = "ready", arkhamDBPushTargetId = Nothing
      , arkhamDBPushBoundary = "upgrade", arkhamDBPushTotalXp = 6 }
    h.run False
    assertState h "succeeded"
    filter (T.isPrefixOf "upgrade/") <$> readIORef h.calls `shouldReturn` ["upgrade/123/5", "upgrade/124/1"]
    -- Upgrade already copied the unchanged slots; verification can finish
    -- read-only instead of issuing a redundant save into the new target.
    state <- readIORef h.linked
    fmap arkhamDBDeckLinkRemoteDeckId (Map.lookup Linking.deckId state.storedLinks) `shouldBe` Just "125"
    readIORef h.baseline `shouldReturn` 6

  it "uses only the newly earned cumulative XP delta, not investigatorSpentXp" do
    h <- newHarness
    writeIORef h.baseline 3
    h.run False
    filter (T.isPrefixOf "upgrade/") <$> readIORef h.calls `shouldReturn` ["upgrade/123/2"]

  it "fences a paused save after another worker reclaims and completes the exact job" do
    h <- newHarness
    reached <- newEmptyMVar
    release <- newEmptyMVar
    done <- newEmptyMVar
    let pausedWrites = h.writes {checkpointPush = \previous next -> do
          saved <- h.writes.checkpointPush previous next
          when (saved && arkhamDBPushStage next == "saving") $ putMVar reached () >> takeMVar release
          pure saved}
    _ <- forkIO $ runWriteBack h.store pausedWrites h.client Linking.settings Linking.now False Linking.userId Linking.deckId >> putMVar done ()
    timeout 2000000 (takeMVar reached) `shouldReturn` Just ()
    later <- addUTCTime 61 <$> getCurrentTime
    runWriteBack h.store h.writes h.client Linking.settings later True Linking.userId Linking.deckId
    assertState h "succeeded"
    modifyIORef' h.remotes $ Map.adjust (\remote -> remote {dateUpdate = Just "external-edit"}) "124"
    before <- readIORef h.calls
    putMVar release ()
    timeout 2000000 (takeMVar done) `shouldReturn` Just ()
    readIORef h.calls `shouldReturn` before
    fmap (.dateUpdate) . Map.lookup "124" <$> readIORef h.remotes `shouldReturn` Just (Just "external-edit")

  it "refuses to mutate with a replaced account generation" do
    h <- newHarness
    let changed = h.client {loadDeck = \token remoteId -> do
          value <- h.client.loadDeck token remoteId
          modifyIORef' h.linked \state -> state {Linking.storedGenerations = Map.insert Linking.userId (ArkhamDBAccountKey $ UUID.fromWords 0 0 0 999) state.storedGenerations}
          pure value}
    -- Isolate generation preflight from the store's second generation check.
    let permissive = h.writes {checkpointPush = \_ next -> writeIORef h.job next >> pure True}
    runWriteBack h.store permissive changed Linking.settings Linking.now False Linking.userId Linking.deckId
    assertNoPush h

  it "requires explicit confirmation for overwrite and save-as-new" do
    for_ ["overwrite", "saveAsNew"] \resolution ->
      (fromJSON $ object ["resolution" .= (resolution :: Text), "confirmed" .= False] :: Result ArkhamDBResolveRequest) `shouldSatisfy` \case Error _ -> True; _ -> False

-- Exercise presence/null through the production parser, not a hand-set flag.
parseRecoverySource :: [Pair] -> IO ArkhamDBRemoteDeck
parseRecoverySource successorFields = case fromJSON $ object $
  [ "id" .= (123 :: Int), "investigator_code" .= ("01001" :: Text), "slots" .= object []
  , "date_update" .= Linking.remote.dateUpdate, "version" .= Linking.remote.deckVersion
  ] <> successorFields of
    Error err -> fail err
    Success remote -> pure (remote :: ArkhamDBRemoteDeck)

-- Run under stack exec ghci to regenerate, using the production encoders.
writeBackFixture :: IO Value
writeBackFixture = do
  success <- newHarness
  success.run False
  conflict <- newHarness
  modifyIORef' conflict.remotes $ Map.adjust (\remote -> remote {dateUpdate = Just "edited"}) "123"
  conflict.run False
  failure <- newHarness
  runWith failure failure.client {loadDeck = \_ _ -> pure $ Left $ ArkhamDBRemoteError "unavailable"} False
  disabled <- newHarness
  _ <- disabled.writes.toggleWriteBack Linking.now Linking.userId Linking.deckId False
  uncertain <- newHarness
  source <- parseRecoverySource []
  modifyIORef' uncertain.remotes $ Map.insert "123" source
  runWith uncertain uncertain.client {upgradeDeck = \_ _ _ _ -> pure $ Left $ ArkhamDBRemoteError "lost reply"} False
  uncertain.run True
  collision <- newHarness
  collisionJob <- readIORef collision.job
  let otherId = ArkhamDeckKey $ UUID.fromWords 0 0 0 999
  state <- readIORef collision.linked
  link <- maybe (fail "Missing link") pure $ Map.lookup Linking.deckId state.storedLinks
  case targetLinkConflict collisionJob Linking.remote (Just link {arkhamDBDeckLinkDeckId = otherId}) of
    Just conflicted -> writeIORef collision.job conflicted
    Nothing -> fail "Missing target conflict"
  undone <- newHarness
  runWith undone undone.client {saveDeck = \_ _ _ -> pure $ Left $ ArkhamDBRemoteError "save failed"} False
  cancelHarnessUndo undone 0
  let statuses h = h.writes.deckWriteBackStatus Linking.userId
  states <- concat <$> traverse statuses [success, conflict, failure, disabled, uncertain, collision, undone]
  pure $ object
    [ "states" .= states
    , "toggleRequest" .= ArkhamDBWriteBackToggleRequest True
    , "overwriteRequest" .= ArkhamDBResolveRequest "overwrite" True
    , "keepRemoteRequest" .= ArkhamDBResolveRequest "keepRemote" False
    , "saveAsNewRequest" .= ArkhamDBResolveRequest "saveAsNew" True
    ]
