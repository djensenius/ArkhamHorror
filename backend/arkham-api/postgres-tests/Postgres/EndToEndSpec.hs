{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE NoFieldSelectors #-}
{-# LANGUAGE OverloadedStrings #-}

-- Whole-flow write-back coverage: real Games.Shared answers and saved-upgrade
-- PUTs, real enqueue and real runWriteBack against PostgreSQL. Only ArkhamDB
-- HTTP is simulated.
module Postgres.EndToEndSpec (spec) where

import Api.Arkham.ArkhamDBStore (persistentStore)
import Api.Arkham.ArkhamDBWriteBack (validateWriteBackDeckFor, writeBackStore)
import Api.Handler.Arkham.CustomCards (userCustomCards)
import Api.Handler.Arkham.Decks (putApiV1ArkhamGameDecksR)
import Api.Handler.Arkham.Games.Shared (updateGameWithAnswerRejectionWith)
import Arkham.ArkhamDB
import Arkham.ArkhamDB.Service (importRemoteDeck)
import Arkham.ArkhamDB.WriteBack (ArkhamDBWriteBackStore (..), decodePushDeckPayload, newArkhamDBExecutor, runWriteBack, stopArkhamDBExecutor)
import Arkham.Campaign (lookupCampaign)
import Arkham.Campaign.Types (campaignDecks, campaignMeta, campaignStep, campaignStoryCards)
import Arkham.Campaigns.TheDreamEaters.Meta qualified as Dream
import Arkham.Card.CardCode (CardCode (..))
import Arkham.Card.CardDef (CardDef (..))
import Arkham.Card.CardType (CardType (AssetType))
import Arkham.Card.CustomCard (lookupCustomCard, missingCustomCardDef)
import Arkham.Classes.Entity (overAttrs, toAttrs)
import Arkham.Custom.Overlay (DeckOverlay (..))
import Arkham.Decklist qualified as Deck
import Arkham.Difficulty (Difficulty (Easy))
import Arkham.Entities (entitiesInvestigators)
import Arkham.Game qualified as Game
import Arkham.Game.State (GameState (IsChooseDecks))
import Arkham.Investigator.Types (investigatorDeckUrl, investigatorXp)
import Arkham.Message (Message (..))
import Arkham.Prelude (These (This))
import Arkham.Question (Question (ChooseUpgradeDeck))
import Control.Concurrent.MVar qualified as MVar
import Data.Aeson (encode, object, toJSON, (.=))
import Data.Aeson.Diff (Patch (..))
import Data.Map.Strict qualified as Map
import Data.Text qualified as T
import Data.Time.Clock (addUTCTime)
import Data.UUID qualified as UUID
import Database.Persist (Entity (..))
import Database.Persist qualified as P
import Database.Persist.Sql (SqlBackend, runSqlConn)
import Entity.Answer (Answer (Raw))
import Entity.Arkham.ArkhamDBAccount
import Entity.Arkham.CustomCard
import Entity.Arkham.CustomCardSet
import Entity.Arkham.Deck
import Entity.Arkham.Game
import Entity.Arkham.Step
import Entity.User (UserId)
import Foundation (App (..), Handler, unsafeHandler, userIdToToken)
import Network.HTTP.Types (Status)
import Network.Wai (defaultRequest, requestHeaders, requestMethod, setRequestBodyChunks)
import Postgres.DispatchSpec (prepareAnswer)
import Postgres.Harness
import Postgres.Production (runHandlerResponseBody, runStoreActions, withProductionApp, withRequest)
import Postgres.Remote (push)
import Postgres.WriteBackSpec (Fixture (..), Seed, fixture)
import Relude
import Settings (ArkhamDBSettings)
import Test.Hspec

-- Two linked remote decks owned by one user. Loads return the imported
-- revision unchanged (no conflict); each upgrade creates one distinct id.
data Remotes = Remotes
  { client :: ArkhamDBClient IO
  , calls :: MVar [Text]
  , decks :: MVar (Map Text ArkhamDBRemoteDeck)
  }

newRemotes :: ArkhamDBClient IO -> ArkhamDBRemoteDeck -> IO Remotes
newRemotes unexpected source = do
  decks <- MVar.newMVar $ Map.fromList [(key, source {remoteDeckId = key}) | key <- ["123", "456"]]
  calls <- MVar.newMVar []
  let successors = Map.fromList [("123", "124"), ("456", "457")] :: Map Text Text
      record name = MVar.modifyMVar_ calls $ pure . (<> [name])
      load key = Map.lookup key <$> MVar.readMVar decks
        >>= maybe (fail "Unexpected remote id") pure
      client = unexpected
        { loadDeck = \_ key -> record ("load/" <> key) >> Right <$> load key
        , upgradeDeck = \_ key xp metadata -> do
            record $ "upgrade/" <> key <> "/" <> show xp
            original <- load key
            next <- maybe (fail "Unexpected upgrade source") pure $ Map.lookup key successors
            MVar.modifyMVar_ decks \allDecks -> do
              when (Map.member next allDecks) $ fail "Duplicate remote creation"
              let target = original
                    { remoteDeckId = next, dateUpdate = Just "created", deckVersion = Just "2.0"
                    , nextDeckId = Nothing, deck = original.deck {Deck.meta = metadata}
                    }
              pure $ Map.insert next target $ Map.adjust (\old -> old {nextDeckId = Just next}) key allDecks
            pure $ Right next
        , saveDeck = \_ key proposed -> do
            record $ "save/" <> key
            MVar.modifyMVar_ decks $ pure . Map.insert key proposed {dateUpdate = Just "saved"}
            pure $ Right key
        }
  pure Remotes {..}

forbiddenPublic :: MonadIO m => ArkhamDeckId -> m (Entity ArkhamDeck)
forbiddenPublic _ = liftIO $ fail "Unexpected public fallback"

-- A full-mode Dream-Eaters game on the given part. Both teams' investigator
-- keeps deck A's URL: the stale URL left by a campaign copy.
dreamGame :: Fixture -> Text -> Dream.CampaignPart -> Int -> Game.Game
dreamGame f staleUrl part xp = prepared
  { Game.gameEntities = prepared.gameEntities
      { entitiesInvestigators = Map.adjust
          (overAttrs \attrs -> attrs {investigatorXp = xp, investigatorDeckUrl = Just staleUrl})
          "01001" $ entitiesInvestigators prepared.gameEntities
      }
  , Game.gameMode = This $ overAttrs (\attrs -> attrs
      { campaignStep = campaignStep original
      , campaignDecks = campaignDecks original
      , campaignStoryCards = campaignStoryCards original
      , campaignMeta = toJSON $ Dream.Metadata Dream.FullMode (Just part) Nothing mempty mempty
      }) $ lookupCampaign "06" Easy
  }
 where
  prepared = prepareAnswer f
  original = case prepared.gameMode of
    This campaign -> toAttrs campaign
    _ -> error "Expected campaign fixture"

xpAnswer :: Int -> Answer
xpAnswer earned = Raw $ Run [SetXP "01001" earned, FinishedUpgradingDecks]

pushes :: SqlBackend -> ArkhamDeckId -> IO [ArkhamDBPush]
pushes connection deckId = transaction connection $
  map entityVal <$> P.selectList [ArkhamDBPushDeckId P.==. deckId] [P.Asc ArkhamDBPushCreatedAt, P.Asc ArkhamDBPushId]

payloadUrl :: ArkhamDBPush -> Maybe Text
payloadUrl job = either (const Nothing) (\(_, deck) -> deck.url) $ decodePushDeckPayload $ arkhamDBPushPayload job

payloadSlots :: ArkhamDBPush -> Maybe (Map CardCode Int)
payloadSlots job = either (const Nothing) (\(_, deck) -> Just deck.slots) $ decodePushDeckPayload $ arkhamDBPushPayload job

-- Deck B ("456") for the fixture's user, linked with write-back on.
importDeckB :: App -> SqlBackend -> ArkhamDBSettings -> ArkhamDBClient IO -> ArkhamDBRemoteDeck -> Fixture -> IO ArkhamDeckId
importDeckB app connection settings unexpected source f = do
  let importer = hoistArkhamDBClient liftIO $ unexpected
        {loadDeck = \_ key -> pure $ Right source {remoteDeckId = key}}
  Entity deckB _ <- either (fail . show) pure =<< runStoreActions app connection
    (\store -> runExceptT $ importRemoteDeck store importer settings f.now f.uid "456" forbiddenPublic)
  unsafeHandler app (writeBackStore.toggleWriteBack f.now f.uid deckB True) `shouldReturn` True
  pure deckB

-- The state 'chooseUpgradeDecks' parks at an UpgradeDeckStep: the seat's deck
-- question is open and the stored queue continues with the window's close and
-- the "upgrade" write-back boundary.
openUpgradeWindow :: SqlBackend -> Fixture -> Game.Game -> IO ()
openUpgradeWindow connection f game = transaction connection do
  let pid = game.gameActivePlayerId
  P.update f.gameId [ArkhamGameCurrentData P.=. game
    {Game.gameGameState = IsChooseDecks [pid], Game.gameQuestion = Map.singleton pid ChooseUpgradeDeck}]
  P.updateWhere [ArkhamStepArkhamGameId P.==. f.gameId, ArkhamStepStep P.==. 2]
    [ArkhamStepChoice P.=. Choice (Patch []) [DoneUpgradingDecks, FinishedUpgradingDecks]]

-- The JWT-authenticated saved-upgrade PUT with a JSON body, called on the
-- real handler like Main.hs overlayRoute (route-level, not WAI routing). The
-- handler's own runDB, engine run, enqueue and executor wake are unchanged.
upgradeRoute :: App -> UserId -> ArkhamGameId -> Deck.ArkhamDBDecklist -> IO (Either (Status, LByteString) ())
upgradeRoute app uid gameId decklist = do
  token <- unsafeHandler app $ userIdToToken uid
  chunks <- newIORef [toStrict $ encode $ object ["investigatorId" .= ("01001" :: Text), "deckList" .= decklist]]
  let nextChunk = atomicModifyIORef' chunks \case
        chunk : rest -> (rest, chunk)
        [] -> ([], "")
      request = setRequestBodyChunks nextChunk defaultRequest
        { requestMethod = "PUT"
        , requestHeaders = [("Authorization", "Token " <> encodeUtf8 token), ("Content-Type", "application/json")]
        }
  runHandlerResponseBody app $ withRequest request $ putApiV1ArkhamGameDecksR gameId

-- The production sweep's dispatch for one deck: owner-library validation,
-- not Remote.push's no-op validator.
pushValidated :: App -> ArkhamDBSettings -> ArkhamDBClient IO -> Fixture -> Int -> IO ()
pushValidated app settings client f seconds = unsafeHandler app $
  runWriteBack (persistentStore $ validateWriteBackDeckFor userCustomCards f.uid) writeBackStore
    (hoistArkhamDBClient liftIO client) settings (addUTCTime (fromIntegral seconds) f.now) False f.uid f.deckId

remoteSlots :: Remotes -> Text -> IO (Maybe (Map CardCode Int))
remoteSlots remotes key = fmap (\remote -> remote.deck.slots) . Map.lookup key <$> MVar.readMVar remotes.decks

spec :: Harness -> App -> ArkhamDBSettings -> ArkhamDBClient IO -> ArkhamDBRemoteDeck -> Seed -> Spec
spec harness app settings unexpected source seed = do
  dreamTeamsSpec harness app settings unexpected source seed
  savedUpgradeSpec harness app settings unexpected source seed

dreamTeamsSpec :: Harness -> App -> ArkhamDBSettings -> ArkhamDBClient IO -> ArkhamDBRemoteDeck -> Seed -> Spec
dreamTeamsSpec harness app settings unexpected source seed = describe "Dream-Eaters end-to-end write-back" do
  it "pushes each same-investigator team only to its own associated deck through the game answer" $
    withConnection harness \connection -> do
      f <- fixture seed app connection "e2e-dream-teams"
      let importer = hoistArkhamDBClient liftIO $ unexpected
            {loadDeck = \_ key -> pure $ Right source {remoteDeckId = key}}
      Entity deckB _ <- either (fail . show) pure =<< runStoreActions app connection
        (\store -> runExceptT $ importRemoteDeck store importer settings f.now f.uid "456" forbiddenPublic)
      unsafeHandler app (writeBackStore.toggleWriteBack f.now f.uid deckB True) `shouldReturn` True
      let deckA = f.deckId
          fB = f {deckId = deckB}
      (urlA, urlB) <- transaction connection $ (,)
        <$> (arkhamDeckUrl <$> P.getJust deckA) <*> (arkhamDeckUrl <$> P.getJust deckB)
      staleUrl <- maybe (fail "Deck A has no URL") pure urlA
      urlB `shouldNotBe` urlA
      let gameKey = UUID.toText (coerce f.gameId) <> "/01001"
          questKey = gameKey <> "/dreamQuest"
          webKey = gameKey <> "/webOfDreams"
      -- These rows stand in for the associations earlier campaign-copy pushes
      -- would have created: each team's part key already names its own deck.
      transaction connection do
        P.insert_ $ ArkhamDBCampaignDeck questKey deckA f.uid 0
        P.insert_ $ ArkhamDBCampaignDeck webKey deckB f.uid 0
      remotes <- newRemotes unexpected source
      let answerWith game earned = do
            transaction connection $ P.update f.gameId [ArkhamGameCurrentData P.=. game]
            -- A fresh, never-started executor: dispatch is driven explicitly
            -- per deck below, never by a shared global sweep.
            executor <- newArkhamDBExecutor
            let foundation = app {appArkhamDBWriteBackExecutor = executor}
                database :: forall a. ReaderT SqlBackend Handler a -> Handler a
                database = (`runSqlConn` connection)
            unsafeHandler foundation (updateGameWithAnswerRejectionWith database mempty (xpAnswer earned) f.gameId Nothing)
              `shouldReturn` Nothing
            stopArkhamDBExecutor executor
          -- One account admits a write-back attempt only every 30 seconds, so
          -- each dispatch round runs a minute after the previous one.
          pushBoth seconds = for_ [f, fB] \target -> push app settings remotes.client target False seconds

      answerWith (dreamGame f staleUrl Dream.TheDreamQuest 5) 6
      questJobs <- pushes connection deckA
      map (\job -> (arkhamDBPushCampaignKey job, arkhamDBPushStatus job)) questJobs `shouldBe` [(questKey, "pending")]
      (map arkhamDBPushCampaignKey <$> pushes connection deckB) `shouldReturn` []
      pushBoth 0
      -- The full ordered call list: team A's push touches only 123 and its
      -- upgrade 124; deck B has no work yet and makes no remote call.
      MVar.readMVar remotes.calls `shouldReturn`
        ["load/123", "upgrade/123/6", "load/124", "save/124", "load/124"]

      answerWith (dreamGame f staleUrl Dream.TheWebOfDreams 5) 3
      webJobs <- pushes connection deckB
      map (\job -> (arkhamDBPushCampaignKey job, arkhamDBPushStatus job)) webJobs `shouldBe` [(webKey, "pending")]
      map arkhamDBPushCampaignKey <$> pushes connection deckA `shouldReturn` [questKey]
      pushBoth 60
      -- Team B's round upgrades and saves only 456 and its upgrade 457. Its
      -- single read of 124 is runWriteBack's ownedElsewhere scan: before
      -- upgrading, it walks deck A's now campaign-bound chain (read-only) to
      -- prove 456 is not owned by another campaign. Deck A's finished push
      -- makes no further remote call of any kind.
      MVar.readMVar remotes.calls `shouldReturn`
        [ "load/123", "upgrade/123/6", "load/124", "save/124", "load/124"
        , "load/456", "load/124", "upgrade/456/3", "load/457", "save/457", "load/457" ]
      finalA <- pushes connection deckA
      finalB <- pushes connection deckB
      for_ [(finalA, deckA, questKey, "/123", 6), (finalB, deckB, webKey, "/456", 3)]
        \(jobs, deckId, key, suffix, earned) -> case jobs of
          [job] -> do
            arkhamDBPushDeckId job `shouldBe` deckId
            arkhamDBPushCampaignKey job `shouldBe` key
            arkhamDBPushTotalXp job `shouldBe` earned
            arkhamDBPushStatus job `shouldBe` "succeeded"
            payloadUrl job `shouldSatisfy` maybe False (suffix `T.isSuffixOf`)
          _ -> expectationFailure $ "Expected exactly one push for " <> toString key <> ", got " <> show (length jobs)
      associations <- transaction connection $
        map entityVal <$> P.selectList [ArkhamDBCampaignDeckCampaignKey P.<-. [questKey, webKey]] [P.Asc ArkhamDBCampaignDeckCampaignKey]
      map (\a -> (arkhamDBCampaignDeckCampaignKey a, arkhamDBCampaignDeckDeckId a)) associations
        `shouldBe` [(questKey, deckA), (webKey, deckB)]

-- The saved-upgrade PUT (putApiV1ArkhamGameDecksR) from a parked upgrade
-- window through enqueue to the ArkhamDB push.
savedUpgradeSpec :: Harness -> App -> ArkhamDBSettings -> ArkhamDBClient IO -> ArkhamDBRemoteDeck -> Seed -> Spec
savedUpgradeSpec harness app settings unexpected source seed = describe "Saved-upgrade end-to-end write-back" do
  it "adopts a saved upgrade only on the currently associated deck and fails closed beside an older full-campaign association" $
    withConnection harness \connection -> do
      f <- fixture seed app connection "e2e-saved-upgrade-current"
      deckB <- importDeckB app connection settings unexpected source f
      let deckA = f.deckId
          fB = f {deckId = deckB}
      savedA <- transaction connection $ P.getJust deckA
      savedB <- transaction connection $ P.getJust deckB
      staleUrl <- maybe (fail "Deck A has no URL") pure $ arkhamDeckUrl savedA
      let legacyKey = UUID.toText (coerce f.gameId) <> "/01001"
          questKey = legacyKey <> "/dreamQuest"
          associationRows = transaction connection $
            map ((\a -> (arkhamDBCampaignDeckCampaignKey a, arkhamDBCampaignDeckDeckId a, arkhamDBCampaignDeckUserId a, arkhamDBCampaignDeckLastPushedXp a)) . entityVal)
              <$> P.selectList [ArkhamDBCampaignDeckUserId P.==. f.uid] [P.Asc ArkhamDBCampaignDeckCampaignKey]
          userPushes = transaction connection $
            map ((\job -> (arkhamDBPushCampaignKey job, arkhamDBPushDeckId job)) . entityVal)
              <$> P.selectList [ArkhamDBPushUserId P.==. f.uid] []
      -- The unsuffixed key is the older full-campaign association, written
      -- before Dream-Eaters team keys carried a part; it still names deck A.
      -- The part key is the current association (as a campaign-copy push
      -- would have left it) and names deck B. The seat keeps deck A's URL.
      transaction connection do
        P.insert_ $ ArkhamDBCampaignDeck legacyKey deckA f.uid 0
        P.insert_ $ ArkhamDBCampaignDeck questKey deckB f.uid 0
      initialRows <- associationRows
      initialRows `shouldBe` [(legacyKey, deckA, f.uid, 0), (questKey, deckB, f.uid, 0)]
      let upgraded = (arkhamDeckList savedB) {Deck.slots = Map.fromList [("01006", 2), ("01007", 1)]}
      openUpgradeWindow connection f $ dreamGame f staleUrl Dream.TheDreamQuest 4
      -- A fresh, never-started executor: the handler's wake is inert and
      -- dispatch is driven explicitly per deck below.
      executor <- newArkhamDBExecutor
      let foundation = app {appArkhamDBWriteBackExecutor = executor}
      upgradeRoute foundation f.uid f.gameId upgraded `shouldReturn` Right ()
      stopArkhamDBExecutor executor

      -- The part-aware key selects only the current association: deck B
      -- adopts the list and deck A (older association, stale URL) is untouched.
      afterA <- transaction connection $ P.getJust deckA
      afterB <- transaction connection $ P.getJust deckB
      toJSON afterA `shouldBe` toJSON savedA
      Deck.slots (arkhamDeckList afterB) `shouldBe` upgraded.slots
      -- Fail-closed policy: writeBackCampaignIdentities always relates this
      -- game's unsuffixed key, and campaignKeyAmbiguous treats it as ambiguous
      -- for an existing full Dream-Eaters game, so admission enqueues nothing
      -- under any key rather than guess which team the old row meant.
      userPushes `shouldReturn` []
      remotes <- newRemotes unexpected source
      -- The PUT ran at wall-clock time, after the fixture's clock.
      for_ [f, fB] \target -> push foundation settings remotes.client target False 60
      MVar.readMVar remotes.calls `shouldReturn` []
      userPushes `shouldReturn` []
      associationRows `shouldReturn` initialRows

  it "pushes an overlay upgrade naming the owner's own custom card after a server restart" $
    withConnection harness \connection -> do
      f <- fixture seed app connection "e2e-restart-custom-overlay"
      -- No other test registers this code, so this process's custom-card
      -- registry has never seen it: the cold registry of a restarted server.
      let code = "*e2e-restart-owned-overlay" :: CardCode
          overlay = DeckOverlay Nothing mempty (Map.singleton code 1) mempty
          gameKey = UUID.toText (coerce f.gameId) <> "/01001"
      -- Saved before the restart: the owner's custom card and an overlay on
      -- the seat's saved deck naming it. Written directly, never through a
      -- handler, so nothing registers the card in this process.
      transaction connection do
        setId <- P.insert $ ArkhamCustomCardSet f.uid "e2e-restart-set" Nothing Nothing Nothing False Nothing
          (toJSON ([] :: [Text])) (toJSON ([] :: [Text])) f.now f.now
        P.insert_ $ ArkhamCustomCard f.uid setId (unCardCode code)
          (toJSON (missingCustomCardDef AssetType code) {cdTags = []}) Nothing f.now f.now
        P.update f.deckId [ArkhamDeckOverlay P.=. Just overlay, ArkhamDeckOverlayAttachmentId P.=. Just UUID.nil]
      isNothing (lookupCustomCard code) `shouldBe` True
      saved <- transaction connection $ P.getJust f.deckId
      let upgraded = (arkhamDeckList saved) {Deck.slots = Map.singleton "01006" 2}
      openUpgradeWindow connection f $ prepareAnswer f
      remotes <- newRemotes unexpected source
      -- A fresh foundation and executor: the server after the restart.
      withProductionApp harness \restarted -> do
        upgradeRoute restarted f.uid f.gameId upgraded `shouldReturn` Right ()
        adopted <- transaction connection $ P.getJust f.deckId
        Deck.slots (arkhamDeckList adopted) `shouldBe` upgraded.slots
        arkhamDeckOverlay adopted `shouldBe` Just overlay
        map (\job -> (arkhamDBPushCampaignKey job, arkhamDBPushStatus job)) <$> pushes connection f.deckId
          `shouldReturn` [(gameKey, "pending")]
        pushValidated restarted settings remotes.client f 60
        stopArkhamDBExecutor $ appArkhamDBWriteBackExecutor restarted
      MVar.readMVar remotes.calls `shouldReturn` ["load/123", "upgrade/123/5", "load/124", "save/124", "load/124"]
      jobs <- pushes connection f.deckId
      case jobs of
        [job] -> do
          arkhamDBPushStatus job `shouldBe` "succeeded"
          arkhamDBPushError job `shouldBe` Nothing
          payloadUrl job `shouldSatisfy` maybe False ("/123" `T.isSuffixOf`)
          payloadSlots job `shouldSatisfy` maybe False \slots ->
            Map.lookup code slots == Just 1 && Map.lookup "01006" slots == Just 2
        _ -> expectationFailure $ "Expected exactly one push, got " <> show (length jobs)
      (remoteSlots remotes "124" <&> (>>= Map.lookup code)) `shouldReturn` Just 1
