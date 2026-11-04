{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE NoFieldSelectors #-}
{-# LANGUAGE OverloadedStrings #-}

-- Whole-flow write-back coverage: real Games.Shared answers, real enqueue and
-- real runWriteBack against PostgreSQL. Only ArkhamDB HTTP is simulated.
module Postgres.EndToEndSpec (spec) where

import Api.Arkham.ArkhamDBWriteBack (writeBackStore)
import Api.Handler.Arkham.Games.Shared (updateGameWithAnswerRejectionWith)
import Arkham.ArkhamDB
import Arkham.ArkhamDB.Service (importRemoteDeck)
import Arkham.ArkhamDB.WriteBack (ArkhamDBWriteBackStore (..), decodePushDeckPayload, newArkhamDBExecutor, stopArkhamDBExecutor)
import Arkham.Campaign (lookupCampaign)
import Arkham.Campaign.Types (campaignDecks, campaignMeta, campaignStep, campaignStoryCards)
import Arkham.Campaigns.TheDreamEaters.Meta qualified as Dream
import Arkham.Classes.Entity (overAttrs, toAttrs)
import Arkham.Decklist qualified as Deck
import Arkham.Difficulty (Difficulty (Easy))
import Arkham.Entities (entitiesInvestigators)
import Arkham.Game qualified as Game
import Arkham.Investigator.Types (investigatorDeckUrl, investigatorXp)
import Arkham.Message (Message (..))
import Arkham.Prelude (These (This))
import Control.Concurrent.MVar qualified as MVar
import Data.Aeson (toJSON)
import Data.Map.Strict qualified as Map
import Data.Text qualified as T
import Data.UUID qualified as UUID
import Database.Persist (Entity (..))
import Database.Persist qualified as P
import Database.Persist.Sql (SqlBackend, runSqlConn)
import Entity.Answer (Answer (Raw))
import Entity.Arkham.ArkhamDBAccount
import Entity.Arkham.Deck
import Entity.Arkham.Game
import Foundation (App (..), Handler, unsafeHandler)
import Postgres.DispatchSpec (prepareAnswer)
import Postgres.Harness
import Postgres.Production (runStoreActions)
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

spec :: Harness -> App -> ArkhamDBSettings -> ArkhamDBClient IO -> ArkhamDBRemoteDeck -> Seed -> Spec
spec harness app settings unexpected source seed = describe "Dream-Eaters end-to-end write-back" do
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

      answerWith (dreamGame f staleUrl Dream.TheWebOfDreams 5) 3
      webJobs <- pushes connection deckB
      map (\job -> (arkhamDBPushCampaignKey job, arkhamDBPushStatus job)) webJobs `shouldBe` [(webKey, "pending")]
      map arkhamDBPushCampaignKey <$> pushes connection deckA `shouldReturn` [questKey]
      pushBoth 60

      filter (T.isPrefixOf "upgrade/") <$> MVar.readMVar remotes.calls `shouldReturn`
        ["upgrade/123/6", "upgrade/456/3"]
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
