{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE NoFieldSelectors #-}
{-# LANGUAGE OverloadedStrings #-}

-- Real store/queue integration. This deliberately does not claim coverage of
-- Games.Shared's answer transaction or the authenticated HTTP route.
module Postgres.WriteBackSpec (spec, Seed, Fixture (..), fixture, queue, seedCampaignGame, undoCampaignWriteBack) where

import Api.Arkham.ArkhamDBStore (loadCampaignIdentities)
import Api.Arkham.ArkhamDBWriteBack
import Api.Arkham.Types.MultiplayerVariant (MultiplayerVariant (Solo))
import Api.Handler.Arkham.Decks (deleteOwnedSavedDeck)
import Arkham.ArkhamDB (ArkhamDBRemoteDeck (..))
import Arkham.ArkhamDB qualified as ArkhamDB
import Arkham.ArkhamDB.Campaign (CampaignGameIdentity (..), campaignAdmissionAllowed)
import Arkham.ArkhamDB.Service
import Arkham.ArkhamDB.WriteBack
import Arkham.Campaign (lookupCampaign)
import Arkham.Campaign.Types (campaignDecks, campaignStep, campaignStoryCards)
import Arkham.CampaignStep (CampaignStep (..))
import Arkham.Card (Card (PlayerCard))
import Arkham.Card.Id (unsafeMakeCardId)
import Arkham.Card.PlayerCard (PlayerCard (..))
import Arkham.Classes.Entity (overAttrs)
import Control.Exception (throwIO)
import Arkham.Decklist (ArkhamDBDecklist (..))
import Arkham.Difficulty (Difficulty (Easy))
import Arkham.Entities (entitiesInvestigators)
import Arkham.Game qualified as Game
import Arkham.Game.State (GameState (IsActive))
import Arkham.Helpers (Deck (..))
import Arkham.Id (PlayerId (..))
import Arkham.Investigator (lookupInvestigator)
import Arkham.Investigator.Types (investigatorDeckUrl, investigatorSideDeck, investigatorTaboo, investigatorXp)
import Arkham.Prelude (These (..))
import Arkham.Scenario (lookupScenario)
import Arkham.Taboo.Types (TabooList (TabooList25))
import Data.Aeson.Diff (Patch (..))
import Data.Map.Strict qualified as Map
import Data.Text qualified as T
import Data.Time.Clock (UTCTime, addUTCTime, getCurrentTime)
import Data.UUID qualified as UUID
import Database.Persist (Entity (..), toPersistValue)
import Database.Persist qualified as P
import Database.Persist.Sql (Single (..), SqlBackend, rawSql, runSqlConn)
import Entity.Arkham.ArkhamDBAccount
import Entity.Arkham.Deck
import Entity.Arkham.Game
import Entity.Arkham.GameRaw
import Entity.Arkham.Player
import Entity.Arkham.Step
import Entity.User (UserId)
import Foundation (App, Handler, unsafeHandler)
import Postgres.Harness
import Postgres.Production
import Relude hiding (first, second)
import Test.Hspec
import UnliftIO.Async (link, wait, withAsync)
import UnliftIO.Exception qualified as Exception

type Seed = SqlBackend -> Text -> IO (UserId, ArkhamDBAccountId, ArkhamDeckId)

data Fixture = Fixture
  { uid :: UserId, generation :: ArkhamDBAccountId, deckId :: ArkhamDeckId
  , gameId :: ArkhamGameId, game :: Game.Game, campaign :: Text, now :: UTCTime
  }

fixture :: Seed -> App -> SqlBackend -> Text -> IO Fixture
fixture seed app connection label = do
  (uid, generation, deckId) <- seed connection label
  now <- getCurrentTime
  unsafeHandler app (writeBackStore.toggleWriteBack now uid deckId True) `shouldReturn` True
  transaction connection do
    saved <- P.getJust deckId
    let base = (arkhamDeckList saved)
          { sideSlots = Map.singleton "01008" 1
          , meta = Just "{\"alternate_front\":\"01001\",\"extra_deck\":{\"05010\":1}}"
          }
        initial = Game.newCampaign "01" Nothing 1 1 Easy False
        card code n = MkPlayerCard (unsafeMakeCardId $ UUID.fromWords 0 0 0 n)
          Nothing code code mempty Nothing Nothing Nothing Nothing Nothing
        mainCards = [card "01006" 1, card "01100" 2, card "01007" 3]
        story = card "01007" 4
    P.update deckId [ArkhamDeckList P.=. base]
    gameId <- P.insert $ ArkhamGame label initial 2 Solo now now False
    player <- P.insert $ ArkhamPlayer uid gameId "01001"
    let investigator = overAttrs (\attrs -> attrs
          { investigatorXp = 5, investigatorDeckUrl = arkhamDeckUrl saved
          , investigatorSideDeck = Just [card "05010" 5], investigatorTaboo = Just TabooList25
          }) $ lookupInvestigator "01001" (PlayerId $ coerce player)
        game = initial
          { Game.gameGameState = IsActive
          , Game.gameEntities = initial.gameEntities {entitiesInvestigators = Map.singleton "01001" investigator}
          , Game.gameMode = This $ overAttrs (\attrs -> attrs
              { campaignStep = UpgradeDeckStep PrologueStep
              , campaignDecks = Map.singleton "01001" $ Deck mainCards
              , campaignStoryCards = Map.singleton "01001" [PlayerCard story]
              }) $ lookupCampaign "01" Easy
          }
        campaign = UUID.toText (coerce gameId) <> "/01001"
    P.replace gameId $ ArkhamGame label game 2 Solo now now False
    for_ [1, 2] \step -> P.insert_ $ ArkhamStep gameId (Choice (Patch []) []) step (ActionDiff [])
    pure Fixture {..}

queue :: Fixture -> SqlBackend -> Int -> IO ()
queue f connection step = transaction connection $
  queueCampaignWriteBack f.gameId step f.now "scenarioEnd" f.game

-- Write-back admits a legacy gameId/investigator key only for an existing
-- ordinary campaign game; a key that cannot prove its game is fenced.
seedCampaignGame :: SqlBackend -> Text -> UTCTime -> IO ArkhamGameId
seedCampaignGame connection label now = transaction connection $
  P.insert $ ArkhamGame label (Game.newCampaign "01" Nothing 1 1 Easy False) 2 Solo now now False

-- Undo passes the stored pre-undo game JSON, read in its own transaction.
undoCampaignWriteBack :: ArkhamGameId -> Int -> ReaderT SqlBackend IO ()
undoCampaignWriteBack gameId restoredStep = do
  raw <- P.getJust $ ArkhamGameRawKey gameId
  cancelCampaignWriteBack True gameId raw.currentData restoredStep

runWrites :: App -> SqlBackend -> (ArkhamDBWriteBackStore Handler -> Handler a) -> IO a
runWrites app connection action = unsafeHandler app $ writeTransaction connection action

writeTransaction :: SqlBackend -> (ArkhamDBWriteBackStore Handler -> Handler a) -> Handler a
writeTransaction connection action = runSqlConn
  (lift $ action $ writeBackStoreWith (`runReaderT` connection)) connection

data FinishRollback = FinishRollback deriving stock (Eq, Show)
instance Exception FinishRollback

claim :: App -> Fixture -> IO (Entity ArkhamDBPush)
claim app f = unsafeHandler app (writeBackStore.claimPush (addUTCTime 120 f.now) False f.uid f.deckId)
  >>= maybe (fail "Expected production claim") pure

advance :: App -> Entity ArkhamDBPush -> Text -> Text -> Maybe Text -> IO (Entity ArkhamDBPush)
advance app previous status stage target = do
  let updated = (P.entityVal previous)
        { arkhamDBPushStatus = status, arkhamDBPushStage = stage, arkhamDBPushTargetId = target
        , arkhamDBPushError = if status == "conflict" then Just "remoteChanged" else Nothing
        }
  unsafeHandler app (writeBackStore.checkpointPush previous updated) `shouldReturn` True
  pure $ Entity (P.entityKey previous) updated

-- The original linked list belongs to another campaign. Real resolve/claim
-- callbacks create the independent replacement that must insert a local copy.
copyClaim :: App -> Fixture -> SqlBackend -> IO (Entity ArkhamDBPush)
copyClaim app f connection = do
  queue f connection 1
  transaction connection $ P.updateWhere [ArkhamDBDeckLinkDeckId P.==. f.deckId]
    [ArkhamDBDeckLinkCampaignKey P.=. Just "first-campaign"]
  original <- claim app f
  void $ advance app original "conflict" "created" (Just "uncertain-predecessor")
  unsafeHandler app (writeBackStore.resolvePush (addUTCTime 121 f.now) f.uid f.deckId "new") `shouldReturn` True
  unsafeHandler app (writeBackStore.claimPush (addUTCTime 240 f.now) False f.uid f.deckId)
    >>= maybe (fail "Expected replacement claim") pure

pendingAt :: SqlBackend -> ArkhamDeckId -> IO Int
pendingAt connection deckId = transaction connection $
  P.count [ArkhamDBPushDeckId P.==. deckId, ArkhamDBPushStatus P.==. "pending"]

-- No arkham_games row ever has this id. Its text form has hex letters, so the
-- uppercase key spelling differs from UUID.toText.
missingGame :: UUID.UUID
missingGame = UUID.fromWords 0x0a1b2c3d 0x4e5f6071 0x8293a4b5 0xc6d7e8f9

-- Fixture-only historical row on the fixture deck (no production writer may
-- recreate a deleted game's push). It copies the live push's columns, older.
historicalPush :: SqlBackend -> Fixture -> ArkhamDBPushId -> Text -> Text -> Text -> Maybe Text -> IO ArkhamDBPushId
historicalPush connection f template label key status reason = transaction connection do
  job <- P.getJust template
  P.insert job
    { arkhamDBPushEventKey = "historical/" <> label, arkhamDBPushCampaignKey = key
    , arkhamDBPushStatus = status, arkhamDBPushError = reason
    , arkhamDBPushCreatedAt = addUTCTime (-60) f.now, arkhamDBPushUpdatedAt = addUTCTime (-60) f.now
    }

pushState :: SqlBackend -> ArkhamDBPushId -> IO (Text, Maybe Text)
pushState connection key = transaction connection do
  job <- P.getJust key
  pure (arkhamDBPushStatus job, arkhamDBPushError job)

spec :: Harness -> App -> ArkhamDBRemoteDeck -> Seed -> (SqlBackend -> UserId -> Text -> IO ArkhamDeckId) -> Spec
spec harness app remote seed relinkImport = describe "Production write-back queue and associations" do
  it "captures campaign cards/story/weakness, sideboard/meta/taboo and earned XP exactly once" $
    withConnection harness \connection -> do
      f <- fixture seed app connection "queue-snapshot"
      queue f connection 1
      queue f connection 1
      jobs <- transaction connection $ P.selectList [ArkhamDBPushDeckId P.==. f.deckId] []
      length jobs `shouldBe` 1
      job <- maybe (fail "Expected captured job") (pure . P.entityVal) $ viaNonEmpty head jobs
      (prepared, captured) <- either fail pure $ decodePushDeckPayload $ arkhamDBPushPayload job
      prepared `shouldBe` False
      slots captured `shouldBe` Map.fromList [("01006", 1), ("01100", 1), ("01007", 1)]
      sideSlots captured `shouldBe` Map.singleton "01008" 1
      meta captured `shouldBe` Just "{\"alternate_front\":\"01001\",\"extra_deck\":{\"05010\":1}}"
      taboo_id captured `shouldBe` Just 10
      arkhamDBPushTotalXp job `shouldBe` 5
      arkhamDBPushCampaignKey job `shouldBe` f.campaign
      arkhamDBPushGeneration job `shouldBe` f.generation

  for_ [False, True] \copyFirst ->
    it ("does not enqueue onto an obsolete association when copy commits " <> if copyFirst then "first" else "second") $
      withConnection harness \first -> withConnection harness \second -> withConnection harness \observer -> do
        f <- fixture seed app first $ if copyFirst then "copy-first" else "queue-first"
        replacement <- copyClaim app f first
        let accepted :: ArkhamDBRemoteDeck
            accepted = remote {ArkhamDB.remoteDeckId = "456"}
        barrier <- newBarrier
        resulting <- if copyFirst then do
          -- Pause after the real unlocked association lookup, not before the
          -- request. Completion commits while enqueue holds a stale hint.
          withAsync (transaction first $
            queueCampaignWriteBackWith (pauseAt barrier) f.gameId 2 f.now "upgrade" f.game) \enqueuing -> do
              link enqueuing
              awaitBarrier barrier
              copied <- unsafeHandler app (writeBackStore.finishPush replacement accepted)
                >>= maybe (fail "Expected copied deck") pure
              releaseBarrier barrier
              within "enqueue after committed copy" $ wait enqueuing
              pure copied
        else do
          firstPid <- backendPid first
          secondPid <- backendPid second
          withAsync (transaction first do
            queueCampaignWriteBack f.gameId 2 f.now "upgrade" f.game
            liftIO $ pauseAt barrier) \enqueuing -> do
              link enqueuing
              awaitBarrier barrier
              withAsync (runWrites app second \store -> store.finishPush replacement accepted) \finishing -> do
                link finishing
                awaitBlockedBy observer firstPid secondPid
                releaseBarrier barrier
                within "committed enqueue" $ wait enqueuing
                within "copy after enqueue" (wait finishing) >>= maybe (fail "Expected copied deck") pure
        resulting `shouldNotBe` f.deckId
        pendingAt observer f.deckId `shouldReturn` 0
        pendingAt observer resulting `shouldReturn` 1
        association <- transaction observer (P.getBy $ UniqueArkhamDBCampaignDeck f.campaign)
          >>= maybe (fail "Expected moved association") (pure . P.entityVal)
        arkhamDBCampaignDeckDeckId association `shouldBe` resulting
        arkhamDBCampaignDeckLastPushedXp association `shouldBe` 5
        -- Both the copied deck and its accounting survive ordinary deletion
        -- attempts, including after unlink removes credentials/mappings.
        transaction observer (deleteOwnedSavedDeck f.uid resulting) `shouldReturn` False
        unsafeHandler app (writeBackStore.toggleWriteBack f.now f.uid resulting False) `shouldReturn` True
        transaction observer (deleteOwnedSavedDeck f.uid resulting) `shouldReturn` False
        runStoreActions app observer \store -> store.unlinkAccount f.uid
        transaction observer (deleteOwnedSavedDeck f.uid resulting) `shouldReturn` False
        unsafeHandler app (writeBackStore.campaignXpBaseline f.campaign) `shouldReturn` 5
        imported <- relinkImport observer f.uid $ if copyFirst then "copy-first-reimport" else "queue-first-reimport"
        imported `shouldNotBe` resulting
        unsafeHandler app (writeBackStore.toggleWriteBack f.now f.uid imported True) `shouldReturn` True
        -- Reimporting does not rebind the campaign's durable accounting to a
        -- new baseline-zero row, even though its investigator URL is unchanged.
        queue f observer 2
        pendingAt observer imported `shouldReturn` 0
        pendingAt observer resulting `shouldReturn` 0
        unsafeHandler app (writeBackStore.campaignXpBaseline f.campaign) `shouldReturn` 5

  it "skips a busy existing association instead of falling back to the original URL" $
    withConnection harness \first -> withConnection harness \second -> do
      f <- fixture seed app first "busy-copy-association"
      replacement <- copyClaim app f first
      let accepted :: ArkhamDBRemoteDeck
          accepted = remote {ArkhamDB.remoteDeckId = "456"}
      barrier <- newBarrier
      withAsync (runWrites app first \store -> do
        copied <- store.finishPush replacement accepted
        liftIO $ pauseAt barrier
        pure copied) \finishing -> do
          link finishing
          awaitBarrier barrier
          within "queue skips copying association" $ queue f second 2
          releaseBarrier barrier
          copied <- within "busy copy commit" (wait finishing) >>= maybe (fail "Expected copy") pure
          pendingAt second f.deckId `shouldReturn` 0
          pendingAt second copied `shouldReturn` 0

  it "guards unresolved creation even without a campaign association" $
    withConnection harness \connection -> do
      f <- fixture seed app connection "unassociated-creation"
      void $ copyClaim app f connection
      -- Fixture-only legacy missing-association state; the creation checkpoint
      -- itself was produced by real claim/checkpoint/resolve callbacks.
      transaction connection $ P.deleteBy $ UniqueArkhamDBCampaignDeck f.campaign
      transaction connection (deleteOwnedSavedDeck f.uid f.deckId) `shouldReturn` False
      transaction connection (isJust <$> P.get f.deckId) `shouldReturn` True

  for_ [False, True] \optOutFirst ->
    it ("serializes actual queue/opt-out with opt-out " <> if optOutFirst then "first without blocking the game path" else "second cancelling committed work") $
      withConnection harness \first -> withConnection harness \second -> withConnection harness \observer -> do
        f <- fixture seed app first $ if optOutFirst then "optout-first" else "optout-second"
        barrier <- newBarrier
        if optOutFirst then
          withAsync (runWrites app first \store -> do
            changed <- store.toggleWriteBack f.now f.uid f.deckId False
            liftIO $ changed `shouldBe` True
            liftIO $ pauseAt barrier) \disabling -> do
              link disabling
              awaitBarrier barrier
              within "nonblocking admission behind opt-out" $ queue f second 1
              releaseBarrier barrier
              within "opt-out commit" $ wait disabling
        else do
          firstPid <- backendPid first
          secondPid <- backendPid second
          withAsync (transaction first do
            queueCampaignWriteBack f.gameId 1 f.now "scenarioEnd" f.game
            liftIO $ pauseAt barrier) \enqueuing -> do
              link enqueuing
              awaitBarrier barrier
              withAsync (runWrites app second \store -> store.toggleWriteBack f.now f.uid f.deckId False) \disabling -> do
                link disabling
                awaitBlockedBy observer firstPid secondPid
                releaseBarrier barrier
                within "enqueue commit" $ wait enqueuing
                within "opt-out cancellation" (wait disabling) `shouldReturn` True
        unsafeHandler app (writeBackStore.toggleWriteBack f.now f.uid f.deckId True) `shouldReturn` True
        unsafeHandler app (isNothing <$> writeBackStore.claimPush (addUTCTime 120 f.now) True f.uid f.deckId) `shouldReturn` True
        pendingAt observer f.deckId `shouldReturn` 0
        transaction observer (P.count [ArkhamDBPushDeckId P.==. f.deckId])
          `shouldReturn` if optOutFirst then 0 else 1
        transaction observer (P.count [ArkhamDBPushDeckId P.==. f.deckId, ArkhamDBPushStatus P.==. "cancelled"])
          `shouldReturn` if optOutFirst then 0 else 1

  for_ ["finish", "disable", "undo", "unlink"] \completion ->
    it ("retains nested save-as-new predecessors until atomic adoption or fencing by " <> toString completion) $
      withConnection harness \first -> withConnection harness \observer -> do
        f <- fixture seed app first $ "nested-" <> completion
        replacement <- copyClaim app f first
        void $ advance app replacement "conflict" "created" (Just "uncertain-second")
        unsafeHandler app (writeBackStore.resolvePush (addUTCTime 241 f.now) f.uid f.deckId "new") `shouldReturn` True
        latest <- unsafeHandler app (writeBackStore.claimPush (addUTCTime 360 f.now) False f.uid f.deckId)
          >>= maybe (fail "Expected nested replacement claim") pure
        P.entityKey latest `shouldNotBe` P.entityKey replacement
        queue f first 2
        predecessors <- transaction first $ P.selectList
          [ArkhamDBPushDeckId P.==. f.deckId, ArkhamDBPushStage P.==. "created"] []
        length predecessors `shouldBe` 2
        let accepted :: ArkhamDBRemoteDeck
            accepted = remote {ArkhamDB.remoteDeckId = "456"}
            inspectPreserved = do
              for_ predecessors \(Entity key old) -> do
                current <- transaction observer $ P.getJust key
                arkhamDBPushStatus current `shouldSatisfy` (`notElem` ["succeeded", "cancelled"])
                arkhamDBPushTargetId current `shouldBe` arkhamDBPushTargetId old
                arkhamDBPushPayload current `shouldBe` arkhamDBPushPayload old
              unsafeHandler app (writeBackStore.campaignXpBaseline f.campaign) `shouldReturn` 0
        if completion == "finish" then do
          -- Even after the actual callback performs all writes, throwing
          -- before commit must roll back copy, movement, XP and retirement.
          aborted <- unsafeHandler app $ Exception.try $ writeTransaction first \store -> do
            copied <- store.finishPush latest accepted
            liftIO $ copied `shouldSatisfy` isJust
            liftIO $ copied `shouldNotBe` Just f.deckId
            baseline <- store.campaignXpBaseline f.campaign
            liftIO $ baseline `shouldBe` 5
            for_ copied \deckId -> do
              saved <- runReaderT (P.get deckId) first
              liftIO $ saved `shouldSatisfy` isJust
            liftIO $ throwIO FinishRollback
          (aborted :: Either FinishRollback ()) `shouldBe` Left FinishRollback
          inspectPreserved
          transaction observer (P.count [ArkhamDeckUserId P.==. f.uid]) `shouldReturn` 1
          pendingAt observer f.deckId `shouldReturn` 1
          barrier <- newBarrier
          withAsync (runWrites app first \store -> do
            copied <- store.finishPush latest accepted
            liftIO $ pauseAt barrier
            pure copied) \finishing -> do
              link finishing
              awaitBarrier barrier
              inspectPreserved
              pendingAt observer f.deckId `shouldReturn` 1
              releaseBarrier barrier
              copied <- within "atomic nested adoption" (wait finishing)
                >>= maybe (fail "Expected copy") pure
              pendingAt observer f.deckId `shouldReturn` 0
              pendingAt observer copied `shouldReturn` 1
              unsafeHandler app (writeBackStore.campaignXpBaseline f.campaign) `shouldReturn` 5
          for_ predecessors \(Entity key _) ->
            transaction observer (arkhamDBPushStatus <$> P.getJust key) `shouldReturn` "cancelled"
          transaction observer (arkhamDBPushStatus <$> P.getJust (P.entityKey latest)) `shouldReturn` "succeeded"
        else do
          case completion of
            "disable" -> do
              unsafeHandler app (writeBackStore.toggleWriteBack f.now f.uid f.deckId False) `shouldReturn` True
              unsafeHandler app (writeBackStore.toggleWriteBack f.now f.uid f.deckId True) `shouldReturn` True
            "undo" -> transaction first $ undoCampaignWriteBack f.gameId 0
            "unlink" -> runStoreActions app first \store -> store.unlinkAccount f.uid
            _ -> fail "Unknown completion fixture"
          inspectPreserved
          selected <- unsafeHandler app $ writeBackStore.claimPush (addUTCTime 600 f.now) True f.uid f.deckId
          case completion of
            "disable" -> fmap P.entityKey selected `shouldBe` Just (P.entityKey replacement)
            _ -> selected `shouldSatisfy` isNothing
          when (completion /= "disable") $
            for_ ["overwrite", "new", "keep"] \mode ->
              unsafeHandler app (writeBackStore.resolvePush f.now f.uid f.deckId mode) `shouldReturn` False

  it "uses checkpoint CAS, read-only keep resolution and acknowledged XP high water" $
    withConnection harness \connection -> do
      f <- fixture seed app connection "keep-highwater"
      queue f connection 1
      original <- claim app f
      void $ advance app original "conflict" "created" (Just "456")
      unsafeHandler app (writeBackStore.checkpointPush original $ P.entityVal original) `shouldReturn` False
      unsafeHandler app (writeBackStore.resolvePush f.now f.uid f.deckId "overwrite") `shouldReturn` False
      unsafeHandler app (writeBackStore.resolvePush (addUTCTime 121 f.now) f.uid f.deckId "keep") `shouldReturn` True
      keeping <- unsafeHandler app (writeBackStore.claimPush (addUTCTime 240 f.now) False f.uid f.deckId)
        >>= maybe (fail "Expected keep claim") pure
      arkhamDBPushTargetId (P.entityVal keeping) `shouldBe` Just "456"
      let accepted :: ArkhamDBRemoteDeck
          accepted = remote {ArkhamDB.remoteDeckId = "456"}
      unsafeHandler app (writeBackStore.keepRemoteDeck keeping accepted) `shouldReturn` True
      transaction connection (arkhamDBPushStatus <$> P.getJust (P.entityKey original)) `shouldReturn` "cancelled"
      saved <- transaction connection $ P.getJust f.deckId
      arkhamDeckList saved `shouldBe` accepted.deck
      arkhamDeckOverlay saved `shouldBe` Nothing
      unsafeHandler app (writeBackStore.campaignXpBaseline f.campaign) `shouldReturn` 5
      let lower = f.game {Game.gameEntities = f.game.gameEntities
            {entitiesInvestigators = Map.map (overAttrs \attrs -> attrs {investigatorXp = 3}) $
              entitiesInvestigators f.game.gameEntities}}
      transaction connection $ queueCampaignWriteBack f.gameId 2 f.now "upgrade" lower
      next <- unsafeHandler app (writeBackStore.claimPush (addUTCTime 360 f.now) False f.uid f.deckId)
        >>= maybe (fail "Expected later boundary") pure
      arkhamDBPushTotalXp (P.entityVal next) `shouldBe` 3
      unsafeHandler app (writeBackStore.finishPush next accepted) `shouldReturn` Just f.deckId
      unsafeHandler app (writeBackStore.campaignXpBaseline f.campaign) `shouldReturn` 5

  it "batch-loads legacy campaign identities by uuid, through These modes, and That-only games as malformed" $
    withConnection harness \connection -> do
      now <- getCurrentTime
      let insertGame label game = transaction connection $ P.insert $ ArkhamGame label game 2 Solo now now False
          legacyKey gid = UUID.toText (coerce gid) <> "/01001"
          missing :: ArkhamGameId
          missing = coerce missingGame
          campaign = Game.newCampaign "01" Nothing 1 1 Easy False
      -- A campaign with its scenario in progress stores sibling This/That keys.
      these <- insertGame "identity-these" campaign
        {Game.gameMode = These (lookupCampaign "01" Easy) (lookupScenario "01104" Easy)}
      thisOnly <- insertGame "identity-this" campaign
      thatOnly <- insertGame "identity-that" $ Game.newScenario "01104" 1 1 Easy False
      -- Fixture check: the stored jsonb really has the three gameMode shapes.
      shapes <- transaction connection $ rawSql
        "SELECT id, jsonb_typeof(current_data #> '{gameMode,This}'), jsonb_typeof(current_data #> '{gameMode,That}') FROM arkham_games WHERE id IN (?, ?, ?)"
        [toPersistValue these, toPersistValue thisOnly, toPersistValue thatOnly]
      Map.fromList [(gid, (this, that)) | (Single gid, Single this, Single that) <- shapes] `shouldBe` Map.fromList
        [ (these, (Just "object" :: Maybe Text, Just "object" :: Maybe Text))
        , (thisOnly, (Just "object", Nothing)), (thatOnly, (Nothing, Just "object")) ]
      -- One batch: uuid parameters and decoded uuid row ids, either key case,
      -- duplicates collapsed, missing rows absent, non-legacy keys never read.
      let keys =
            [ legacyKey these, T.toUpper (legacyKey thisOnly), legacyKey thisOnly, legacyKey thatOnly
            , T.toUpper (legacyKey missing), "not-a-uuid/01001", legacyKey thatOnly <> "/dreamQuest" ]
      identities <- transaction connection $ loadCampaignIdentities mempty keys
      identities `shouldBe` Map.fromList
        [(these, OrdinaryCampaignGame), (thisOnly, OrdinaryCampaignGame), (thatOnly, MalformedCampaignGame)]
      -- A present That-only row cannot prove a part, so it taints; a deleted
      -- (missing) game is inert, whichever case its key was written in.
      campaignAdmissionAllowed identities [legacyKey thatOnly] (legacyKey these) `shouldBe` False
      campaignAdmissionAllowed identities [T.toUpper (legacyKey missing)] (T.toUpper (legacyKey thisOnly)) `shouldBe` True

  for_
    [ ("lowercase-missing", const $ UUID.toText missingGame <> "/01001", True)
    , ("uppercase-missing", const $ T.toUpper (UUID.toText missingGame) <> "/01001", True)
    , ("malformed-uuid", const "0123abcd-0123/01001", True)
    , ("uppercase-existing", \gid -> T.toUpper (UUID.toText $ coerce gid) <> "/01001", False)
    ] \(label, keyFor, orphan) ->
    it ((if orphan then "skips" else "keeps") <> " an older fenced " <> toString label <> " legacy push when claiming") $
      withConnection harness \connection -> do
        f <- fixture seed app connection $ "orphan-claim-" <> label
        queue f connection 1
        Entity live _ <- transaction connection (P.selectFirst [ArkhamDBPushDeckId P.==. f.deckId] [])
          >>= maybe (fail "Expected live push") pure
        existing <- seedCampaignGame connection ("orphan-claim-game-" <> label) f.now
        stale <- historicalPush connection f live label (keyFor existing) "failed" (Just "validationFailed")
        claimed <- unsafeHandler app $ writeBackStore.claimPush (addUTCTime 120 f.now) False f.uid f.deckId
        -- An orphan (including an uncastable key) no longer heads the queue;
        -- an existing game's fenced push still does, in either key case.
        fmap P.entityKey claimed `shouldBe` if orphan then Just live else Nothing
        pushState connection live `shouldReturn` (if orphan then "running" else "pending", Nothing)
        pushState connection stale `shouldReturn` ("failed", Just "validationFailed")

  it "keep-remote cancels non-orphan work but retains fenced orphan legacy pushes" $
    withConnection harness \connection -> do
      f <- fixture seed app connection "keep-orphans"
      queue f connection 1
      original <- claim app f
      void $ advance app original "conflict" "created" (Just "456")
      unsafeHandler app (writeBackStore.resolvePush (addUTCTime 121 f.now) f.uid f.deckId "keep") `shouldReturn` True
      keeping <- unsafeHandler app (writeBackStore.claimPush (addUTCTime 240 f.now) False f.uid f.deckId)
        >>= maybe (fail "Expected keep claim") pure
      existing <- seedCampaignGame connection "keep-orphans-game" f.now
      let missing = UUID.toText missingGame
          historical (label, key, status, reason) = historicalPush connection f (P.entityKey original) label key status reason
          accepted :: ArkhamDBRemoteDeck
          accepted = remote {ArkhamDB.remoteDeckId = "456"}
      orphans <- traverse historical
        [ ("orphan-lower", missing <> "/01001", "failed", Just "validationFailed")
        , ("orphan-upper", T.toUpper missing <> "/01002", "failed", Just "validationFailed")
        , ("orphan-malformed", "0123abcd-0123/01001", "failed", Just "validationFailed")
        ]
      others <- traverse historical
        [ ("existing-upper", T.toUpper (UUID.toText $ coerce existing) <> "/01001", "failed", Just "validationFailed")
        , ("missing-pending", missing <> "/01003", "pending", Nothing)
        , ("missing-other-error", missing <> "/01004", "failed", Just "remoteUnavailable")
        , ("missing-part", missing <> "/01001/dreamQuest", "failed", Just "validationFailed")
        ]
      unsafeHandler app (writeBackStore.keepRemoteDeck keeping accepted) `shouldReturn` True
      for_ orphans \key -> pushState connection key `shouldReturn` ("failed", Just "validationFailed")
      for_ others \key -> pushState connection key `shouldReturn` ("cancelled", Nothing)
      pushState connection (P.entityKey original) `shouldReturn` ("cancelled", Nothing)
      arkhamDeckList <$> transaction connection (P.getJust f.deckId) `shouldReturn` accepted.deck

  it "keeps owned DELETE non-disclosing for foreign/missing ids while allowing an unused owned deck" $
    withConnection harness \connection -> do
      f <- fixture seed app connection "unused-owned-delete"
      (other, _, _) <- seed connection "other-delete-owner"
      transaction connection (deleteOwnedSavedDeck other f.deckId) `shouldReturn` True
      transaction connection (isJust <$> P.get f.deckId) `shouldReturn` True
      transaction connection (deleteOwnedSavedDeck f.uid f.deckId) `shouldReturn` True
      transaction connection (deleteOwnedSavedDeck f.uid f.deckId) `shouldReturn` True
      transaction connection (isNothing <$> P.get f.deckId) `shouldReturn` True

  for_ [False, True] \deleteFirst ->
    it ("runs the owned DELETE transaction " <> if deleteFirst then "before nonblocking queue admission" else "after queue and refuses accounting deletion") $
      withConnection harness \first -> withConnection harness \second -> withConnection harness \observer -> do
        f <- fixture seed app first $ if deleteFirst then "delete-first" else "delete-second"
        barrier <- newBarrier
        if deleteFirst then
          withAsync (transaction first do
            removed <- deleteOwnedSavedDeck f.uid f.deckId
            liftIO $ removed `shouldBe` True
            liftIO $ pauseAt barrier) \deleting -> do
              link deleting
              awaitBarrier barrier
              within "queue skips owned deleting row" $ queue f second 1
              releaseBarrier barrier
              within "owned DELETE commit" $ wait deleting
        else do
          firstPid <- backendPid first
          secondPid <- backendPid second
          withAsync (transaction first do
            queueCampaignWriteBack f.gameId 1 f.now "scenarioEnd" f.game
            liftIO $ pauseAt barrier) \enqueuing -> do
              link enqueuing
              awaitBarrier barrier
              withAsync (transaction second $ deleteOwnedSavedDeck f.uid f.deckId) \deleting -> do
                link deleting
                awaitBlockedBy observer firstPid secondPid
                releaseBarrier barrier
                within "queue commit before DELETE guard" $ wait enqueuing
                within "guarded DELETE" (wait deleting) `shouldReturn` False
        transaction observer (isNothing <$> P.get f.deckId) `shouldReturn` deleteFirst
        transaction observer (isJust <$> P.get f.gameId) `shouldReturn` True
        pendingAt observer f.deckId `shouldReturn` if deleteFirst then 0 else 1
