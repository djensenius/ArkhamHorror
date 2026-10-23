{-# LANGUAGE NoFieldSelectors #-}

module Arkham.Api.ArkhamDBWriteBackSpec where

import Arkham.Api.ArkhamDBSpec qualified as Linking
import Arkham.ArkhamDB
import Arkham.ArkhamDB.Campaign
import Arkham.ArkhamDB.Service
import Arkham.ArkhamDB.WriteBack
import Arkham.Card (Card (..))
import Arkham.Card.Id (unsafeMakeCardId)
import Arkham.Card.PlayerCard (PlayerCard (..))
import Arkham.Decklist qualified as Decklist
import Arkham.Message (Message (..))
import Arkham.Taboo.Types (TabooList (TabooList25))
import Arkham.Target (Target (GameTarget))
import Arkham.Source (Source (GameSource))
import Data.Aeson
import Data.Map.Strict qualified as Map
import Data.Text qualified as T
import Data.UUID qualified as UUID
import Database.Persist (Entity (..))
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
  (store, linked) <- Linking.newStore
  Linking.linkAccount store
  imported <- runExceptT $ importRemoteDeck store Linking.stubClient Linking.settings Linking.now Linking.userId "123" Linking.forbiddenPublic
  imported `shouldSatisfy` isRight
  modifyIORef' linked \s -> s {Linking.storedLinks = Map.adjust (\l -> l {arkhamDBDeckLinkWriteBack = True}) Linking.deckId s.storedLinks}
  generation <- store.findAccountGeneration Linking.userId >>= maybe (fail "Missing account") pure
  job <- newIORef $ ArkhamDBPush Linking.deckId Linking.userId generation "game/1/iid" "game/iid"
    (encodePushDeck currentDeck) 5 "pending" "ready" Nothing Nothing Linking.remote.dateUpdate Linking.remote.deckVersion
    Nothing Nothing Nothing "normal" Linking.now Linking.now
  remotes <- newIORef $ Map.singleton "123" Linking.remote
  calls <- newIORef []
  baseline <- newIORef 0
  let record call = modifyIORef' calls (<> [call])
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
        , checkpointPush = \(Entity _ previous) next -> do
            active <- valid previous
            current <- readIORef job
            if active && arkhamDBPushStatus current == arkhamDBPushStatus previous && arkhamDBPushStage current == arkhamDBPushStage previous then writeIORef job next >> pure True
            else pure False
        , finishPush = \(Entity _ value) remote -> do
            active <- valid value
            if not active then pure Nothing else do
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
              { Linking.storedDecks = Map.adjust (\deck -> deck {arkhamDeckList = remote.deck}) Linking.deckId s.storedDecks
              , Linking.storedLinks = Map.adjust (\link -> link
                  { arkhamDBDeckLinkRemoteDeckId = remote.remoteDeckId
                  , arkhamDBDeckLinkLastSyncedDateUpdate = remote.dateUpdate
                  , arkhamDBDeckLinkLastSyncedVersion = remote.deckVersion
                  }) Linking.deckId s.storedLinks
              }
            writeIORef job value {arkhamDBPushStatus = "cancelled"}
            pure True
        , resolvePush = \_ _ _ mode -> modifyIORef' job (\value -> value {arkhamDBPushStatus = "pending", arkhamDBPushStage = "ready", arkhamDBPushMode = mode}) >> pure True
        , toggleWriteBack = \_ _ _ enabled -> modifyIORef' linked (\s -> s
            {Linking.storedLinks = Map.adjust (\link -> link {arkhamDBDeckLinkWriteBack = enabled}) Linking.deckId s.storedLinks}) >> pure True
        , deckWriteBackStatus = \_ -> do
            value <- readIORef job
            link <- mapping
            state <- readIORef linked
            let resulting = arkhamDBDeckLinkDeckId <$> find ((== Just (arkhamDBPushCampaignKey value)) . arkhamDBDeckLinkCampaignKey) (Map.elems state.storedLinks)
            pure [pushStatus Linking.deckId link (Just $ Entity pushKey value) (resulting <|> Just Linking.deckId)]
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
                  version = original {deck = original.deck {Decklist.meta = metadata}, remoteDeckId = "124", dateUpdate = Just "created", deckVersion = Just "2.0", nextDeckId = Nothing}
              in Map.insert "124" version $ Map.adjust (\value -> value {nextDeckId = Just "124"}) source values
            pure $ Right "124"
        , createDeck = \_ deck -> do
            record "new"
            modifyIORef' remotes $ Map.insert "125" $ Linking.remote
              {deck = deck {Decklist.slots = mempty, Decklist.sideSlots = mempty}, remoteDeckId = "125", dateUpdate = Just "created", nextDeckId = Nothing}
            pure $ Right "125"
        , saveDeck = \_ target deck -> do
            record $ "save/" <> target
            modifyIORef' remotes $ Map.adjust (\value -> value {deck = deck, dateUpdate = Just "saved"}) target
            pure $ Right target
        }
      run retry = runWriteBack store writes client Linking.settings Linking.now retry Linking.userId Linking.deckId
  pure Harness {..}
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

spec :: Spec
spec = describe "ArkhamDB campaign write-back" do
  it "snapshots campaign story cards and weaknesses by identity, not the investigator draw pile" do
    let card n code = MkPlayerCard (unsafeMakeCardId $ UUID.fromWords 0 0 0 n) Nothing code code mempty Nothing Nothing Nothing Nothing Nothing
        first = card 1 "01006"
        duplicate = card 2 "01006"
        weakness = card 3 "01100"
        story = card 4 "01007"
        side = card 5 "01008"
        snapshot = campaignDeckSnapshot Linking.remote.deck [first, duplicate, weakness, story] [PlayerCard story] (Just [side]) (Just TabooList25)
    snapshot.slots `shouldBe` Map.fromList [("01006", 2), ("01100", 1), ("01007", 1)]
    snapshot.sideSlots `shouldBe` Map.singleton "01008" 1
    snapshot.taboo_id `shouldBe` Just 10

  it "writes at scenario-end and completed-upgrade boundaries, not temporary per-card mutations" do
    isWriteBackBoundary (ForTarget GameTarget $ EndOfScenario Nothing) `shouldBe` True
    isWriteBackBoundary FinishedUpgradingDecks `shouldBe` True
    isWriteBackBoundary (GainXP "01001" GameSource 1) `shouldBe` False

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
      modifyIORef' h.remotes $ Map.adjust (\remote -> if marker == "date" then remote {dateUpdate = Just "edited"} else remote {deckVersion = Just "9.0"}) "123"
      h.run False
      assertState h "conflict"
      assertNoPush h
      statuses <- h.writes.deckWriteBackStatus Linking.userId
      map (.conflict) statuses `shouldSatisfy` all isJust

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
          pure $ Just $ Entity pushKey claimed}
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

  it "resumes an uncertain upgrade from its marked next_deck without upgrading again" do
    h <- newHarness
    let lost = h.client {upgradeDeck = \token source xp metadata -> do
          _ <- h.client.upgradeDeck token source xp metadata
          pure $ Left $ ArkhamDBRemoteError "lost response"}
    runWith h lost False
    assertState h "failed"
    h.run True
    assertState h "succeeded"
    filter (T.isPrefixOf "upgrade/") <$> readIORef h.calls `shouldReturn` ["upgrade/123/5"]

  it "recovers a lost creation via its namespaced marker when no version chain is exposed" do
    h <- newHarness
    let lost = h.client {upgradeDeck = \token source xp metadata -> do
          _ <- h.client.upgradeDeck token source xp metadata
          modifyIORef' h.remotes $ Map.adjust (\remote -> remote {nextDeckId = Nothing}) source
          pure $ Left $ ArkhamDBRemoteError "lost response"}
    runWith h lost False
    h.run True
    assertState h "succeeded"
    filter (T.isPrefixOf "upgrade/") <$> readIORef h.calls `shouldReturn` ["upgrade/123/5"]

  it "does not repeat creation when an uncertain outcome cannot be proven" do
    h <- newHarness
    let lost = h.client {upgradeDeck = \_ _ _ _ -> modifyIORef' h.calls (<> ["upgrade/uncertain"]) >> pure (Left $ ArkhamDBRemoteError "lost")}
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
    fmap arkhamDeckList (Map.lookup Linking.deckId state.storedDecks) `shouldBe` Just Linking.remote.deck

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
    _ <- h.writes.resolvePush Linking.now Linking.userId Linking.deckId "new"
    h.run False
    assertState h "succeeded"
    filter (\call -> "upgrade/" `T.isPrefixOf` call || call == "new" || "save/" `T.isPrefixOf` call) <$> readIORef h.calls
      `shouldReturn` ["new", "save/125"]
    state <- readIORef h.linked
    Map.size state.storedLinks `shouldBe` 1
    fmap arkhamDBDeckLinkRemoteDeckId (Map.lookup Linking.deckId state.storedLinks) `shouldBe` Just "125"

  it "blocks a second campaign and save-as-new clones only its local association" do
    h <- newHarness
    modifyIORef' h.linked \state -> state {Linking.storedLinks = Map.adjust (\link -> link {arkhamDBDeckLinkCampaignKey = Just "first-campaign/iid"}) Linking.deckId state.storedLinks}
    h.run False
    assertState h "conflict"
    arkhamDBPushError <$> readIORef h.job `shouldReturn` Just "campaignChanged"
    assertNoPush h
    _ <- h.writes.resolvePush Linking.now Linking.userId Linking.deckId "new"
    h.run False
    state <- readIORef h.linked
    Map.size state.storedLinks `shouldBe` 2
    fmap arkhamDBDeckLinkRemoteDeckId (Map.lookup Linking.deckId state.storedLinks) `shouldBe` Just "123"
    fmap (Decklist.slots . arkhamDeckList) (Map.lookup (ArkhamDeckKey $ UUID.fromWords 0 0 0 246) state.storedDecks)
      `shouldBe` Just currentDeck.slots

  it "does not overwrite a known target edited after a failed save" do
    h <- newHarness
    let lost = h.client {saveDeck = \_ _ _ -> pure $ Left $ ArkhamDBRemoteError "unavailable"}
    runWith h lost False
    modifyIORef' h.remotes $ Map.adjust (\remote -> remote {dateUpdate = Just "external-edit", deck = remote.deck {Decklist.slots = Map.singleton "01009" 2}}) "124"
    h.run True
    assertState h "conflict"
    filter (T.isPrefixOf "save/") <$> readIORef h.calls `shouldReturn` []

  it "uses only the newly earned cumulative XP delta, not investigatorSpentXp" do
    h <- newHarness
    writeIORef h.baseline 3
    h.run False
    filter (T.isPrefixOf "upgrade/") <$> readIORef h.calls `shouldReturn` ["upgrade/123/2"]

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
