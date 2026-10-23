{-# LANGUAGE NoFieldSelectors #-}

module Arkham.Api.ArkhamDBWriteBackSpec where

import Arkham.Api.ArkhamDBSpec qualified as Linking
import Arkham.ArkhamDB
import Arkham.ArkhamDB.Campaign
import Arkham.ArkhamDB.Service
import Arkham.ArkhamDB.WriteBack
import Arkham.CampaignStep (CampaignStep (..))
import Arkham.Custom.Overlay (DeckOverlay (..))
import Arkham.Card (Card (..))
import Arkham.Card.EncounterCard (EncounterCard (..))
import Arkham.Card.Id (unsafeMakeCardId)
import Arkham.Card.PlayerCard (PlayerCard (..))
import Arkham.Decklist qualified as Decklist
import Arkham.Message (Message (..))
import Arkham.Taboo.Types (TabooList (TabooList25))
import Arkham.Target (Target (GameTarget))
import Arkham.Source (Source (GameSource))
import Data.Aeson
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
    (encodePushDeck currentDeck) 5 "pending" "ready" Nothing Nothing Linking.remote.dateUpdate Linking.remote.deckVersion
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
            if not active then pure Nothing else do
              writeIORef completed $ Just (arkhamDBPushCampaignKey value, remote.remoteDeckId)
              writeIORef baseline $ arkhamDBPushTotalXp value
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
            modifyIORef' linked \s -> s
              { Linking.storedDecks = Map.adjust (applyDeckUpdates $ keepRemoteDeckUpdates remote) Linking.deckId s.storedDecks
              , Linking.storedLinks = Map.adjust (\link -> link
                  { arkhamDBDeckLinkRemoteDeckId = remote.remoteDeckId
                  , arkhamDBDeckLinkLastSyncedDateUpdate = remote.dateUpdate
                  , arkhamDBDeckLinkLastSyncedVersion = remote.deckVersion
                  }) Linking.deckId s.storedLinks
              }
            writeIORef job value {arkhamDBPushStatus = "cancelled"}
            pure True
        , resolvePush = \_ _ _ mode -> do
            value <- readIORef job
            if not (canResolvePush mode value) then pure False else do
              writeIORef job value {arkhamDBPushStatus = "pending", arkhamDBPushStage = "ready", arkhamDBPushMode = mode, arkhamDBPushTargetId = Nothing}
              pure True
        , toggleWriteBack = \_ _ _ enabled -> do
            modifyIORef' linked (\s -> s {Linking.storedLinks = Map.adjust (\link -> link {arkhamDBDeckLinkWriteBack = enabled}) Linking.deckId s.storedLinks})
            unless enabled $ modifyIORef' job (\value -> value {arkhamDBPushStatus = "cancelled", arkhamDBPushError = Nothing})
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

assertState :: Harness -> Text -> Expectation
assertState h expected = arkhamDBPushStatus <$> readIORef h.job >>= (`shouldBe` expected)

assertNoPush :: Harness -> Expectation
assertNoPush h = do
  calls <- readIORef h.calls
  filter (\call -> "upgrade/" `T.isPrefixOf` call || "save/" `T.isPrefixOf` call || call == "new") calls `shouldBe` []

applyDeckUpdates :: [Update ArkhamDeck] -> ArkhamDeck -> ArkhamDeck
applyDeckUpdates updates deck = entityVal $ foldl' apply (Entity Linking.deckId deck) updates
 where
  apply entity (Update field value Assign) = runIdentity $ fieldLens field (const $ Identity value) entity
  apply _ _ = error "Only field assignments are used by keep-remote"

spec :: Spec
spec = describe "ArkhamDB campaign write-back" do
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

  it "does not queue another seat owner's associated deck" do
    h <- newHarness
    state <- readIORef h.linked
    deck <- maybe (fail "Missing deck") pure $ Map.lookup Linking.deckId state.storedDecks
    link <- maybe (fail "Missing link") pure $ Map.lookup Linking.deckId state.storedLinks
    ownsCampaignDeck Linking.userId deck link `shouldBe` True
    ownsCampaignDeck Linking.userId deck {arkhamDeckUserId = toSqlKey 999} link `shouldBe` False
    ownsCampaignDeck Linking.userId deck link {arkhamDBDeckLinkUserId = toSqlKey 999} `shouldBe` False

  it "snapshots campaign story cards and weaknesses by identity, not the investigator draw pile" do
    let card n code = MkPlayerCard (unsafeMakeCardId $ UUID.fromWords 0 0 0 n) Nothing code code mempty Nothing Nothing Nothing Nothing Nothing
        first = card 1 "01006"
        duplicate = card 2 "01006"
        weakness = card 3 "01100"
        story = card 4 "01007"
        side = card 5 "01008"
        snapshot = campaignDeckSnapshot currentDeck [first, duplicate, weakness, story] [PlayerCard story] (Just [side]) (Just TabooList25)
    snapshot.slots `shouldBe` Map.fromList [("01006", 2), ("01100", 1), ("01007", 1)]
    snapshot.sideSlots `shouldBe` Map.singleton "01008" 1
    snapshot.taboo_id `shouldBe` Just 10

  it "preserves Joe Diamond sideSlots and extra_deck instead of exporting his hunch deck" do
    let card = MkPlayerCard (unsafeMakeCardId $ UUID.fromWords 0 0 0 20) Nothing "05010" "05010" mempty Nothing Nothing Nothing Nothing Nothing
        joe = currentDeck {Decklist.investigator_code = "05002", Decklist.meta = Just "{\"extra_deck\":{\"05010\":1}}"}
        snapshot = campaignDeckSnapshot joe [] [] (Just [card]) Nothing
    snapshot.sideSlots `shouldBe` joe.sideSlots
    snapshot.meta `shouldBe` joe.meta

  it "does not export encounter story cards as player deck slots" do
    let encounter = MkEncounterCard (unsafeMakeCardId $ UUID.fromWords 0 0 0 21) "01116" "01116" Nothing Nothing False Nothing
    (campaignDeckSnapshot currentDeck [] [EncounterCard encounter] Nothing Nothing).slots `shouldBe` mempty

  it "retains an original unknown remote taboo rather than clearing it" do
    let original = currentDeck {Decklist.taboo_id = Just 99}
    (campaignDeckSnapshot original [] [] Nothing Nothing).taboo_id `shouldBe` Just 99

  it "writes at scenario-end and completed-upgrade boundaries, not temporary per-card mutations" do
    isWriteBackBoundary (ForTarget GameTarget $ EndOfScenario Nothing) `shouldBe` True
    isWriteBackBoundary FinishedUpgradingDecks `shouldBe` True
    isWriteBackBoundary (GainXP "01001" GameSource 1) `shouldBe` False

  it "skips the campaign-start ChooseDecksStep with no XP or remote version" do
    campaignWriteBackBoundary (Just $ ChooseDecksStep PrologueStep) FinishedUpgradingDecks `shouldBe` Nothing
    campaignWriteBackBoundary (Just $ UpgradeDeckStep PrologueStep) FinishedUpgradingDecks `shouldBe` Just "upgrade"

  it "matches encoder-generated write-back contract fixtures" do
    generated <- writeBackFixture
    fixture <- Linking.loadNamedFixture "arkhamdb-write-back.json"
    generated `shouldBe` fixture

  it "upgrades then saves the current campaign slots, story cards, weaknesses, sideboard, meta and taboo" do
    h <- newHarness
    h.run False
    assertState h "succeeded"
    readIORef h.calls `shouldReturn` ["load/123", "upgrade/123/5", "load/124", "save/124", "load/124"]
    pushed <- Map.lookup "124" <$> readIORef h.remotes
    let expected = either (const currentDeck) id $ markedDeck pushKey currentDeck
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

  it "refuses invalid metadata without making any remote request" do
    h <- newHarness
    modifyIORef' h.job \job -> job {arkhamDBPushPayload = encodePushDeck currentDeck {Decklist.meta = Just "[]"}}
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
    readIORef h.baseline `shouldReturn` 4

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
  let statuses h = h.writes.deckWriteBackStatus Linking.userId
  states <- concat <$> traverse statuses [success, conflict, failure, disabled]
  pure $ object
    [ "states" .= states
    , "toggleRequest" .= ArkhamDBWriteBackToggleRequest True
    , "overwriteRequest" .= ArkhamDBResolveRequest "overwrite" True
    , "keepRemoteRequest" .= ArkhamDBResolveRequest "keepRemote" False
    , "saveAsNewRequest" .= ArkhamDBResolveRequest "saveAsNew" True
    ]
