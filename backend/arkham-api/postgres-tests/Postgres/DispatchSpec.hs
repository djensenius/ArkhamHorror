{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE NoFieldSelectors #-}
{-# LANGUAGE OverloadedStrings #-}

module Postgres.DispatchSpec (spec, prepareAnswer, answer) where

import Api.Handler.Arkham.Games.Shared (updateGameWithAnswerRejection, updateGameWithAnswerRejectionWith)
import Arkham.ArkhamDB
import Arkham.ArkhamDB.WriteBack
import Arkham.Campaign.Types (campaignStep)
import Arkham.CampaignStep (CampaignStep (..), Continuation (..))
import Arkham.Classes.Entity (overAttrs, toAttrs)
import Arkham.Entities (entitiesInvestigators)
import Arkham.Game qualified as Game
import Arkham.Investigator.Types (investigatorPlayerId, investigatorXp)
import Arkham.Message (Message (..))
import Arkham.Prelude (These (This))
import Control.Concurrent.MVar qualified as MVar
import Control.Exception (throwIO)
import Data.Map.Strict qualified as Map
import Database.Persist qualified as P
import Database.Persist.Sql (SqlBackend, runSqlConn)
import Entity.Answer (Answer (Raw))
import Entity.Arkham.ArkhamDBAccount
import Entity.Arkham.Game
import Entity.Arkham.Step
import Foundation (App (..), Handler, unsafeHandler)
import Postgres.Harness
import Postgres.Remote
import Postgres.WriteBackSpec (Fixture (..), Seed, fixture)
import Relude
import Settings (ArkhamDBSettings)
import Test.Hspec
import UnliftIO.Async (link, wait, withAsync)
import UnliftIO.Exception qualified as Exception

data AnswerRollback = AnswerRollback deriving stock (Eq, Show)
instance Exception AnswerRollback

-- An actual legal campaign continuation. The real answer changes XP and
-- finishes upgrading; Shared's message observer must capture the boundary.
prepareAnswer :: Fixture -> Game.Game
prepareAnswer f = f.game
  { Game.gamePlayers = [pid], Game.gameActivePlayerId = pid
  , Game.gamePlayerOrder = ["01001"], Game.gameLeadInvestigatorId = "01001"
  , Game.gameActiveInvestigatorId = "01001"
  , Game.gameMode = case f.game.gameMode of
      This campaign -> This $ overAttrs (\attrs -> attrs
        {campaignStep = UpgradeDeckStep $ ContinueCampaignStep $ Continuation PrologueStep False False Nothing False}) campaign
      _ -> error "Expected campaign fixture"
  }
 where
  pid = investigatorPlayerId $ toAttrs $ fromMaybe (error "Expected investigator") $
    Map.lookup "01001" $ entitiesInvestigators f.game.gameEntities

answer :: Answer
answer = Raw $ Run [SetXP "01001" 6, FinishedUpgradingDecks]

xp :: Game.Game -> Int
xp game = investigatorXp $ toAttrs $ fromMaybe (error "Expected investigator") $
  Map.lookup "01001" $ entitiesInvestigators game.gameEntities

spec :: Harness -> App -> ArkhamDBSettings -> ArkhamDBClient IO -> ArkhamDBRemoteDeck -> Seed -> Spec
spec harness app settings unexpected source seed = describe "Actual Games.Shared answer and dispatch" do
  it "rolls back the actual answer's XP, history and enqueued intent together before any dispatch" $
    withConnection harness \writer -> withConnection harness \observer -> do
      f <- fixture seed app writer "answer-rollback"
      transaction writer $ P.update f.gameId [ArkhamGameCurrentData P.=. prepareAnswer f]
      executor <- newArkhamDBExecutor
      let foundation = app {appArkhamDBWriteBackExecutor = executor}
          database :: forall a. ReaderT SqlBackend Handler a -> Handler a
          database action = runSqlConn (do
            result <- action
            current <- P.getJust f.gameId
            liftIO $ arkhamGameStep current `shouldBe` 3
            liftIO $ xp (arkhamGameCurrentData current) `shouldBe` 6
            jobs <- P.count [ArkhamDBPushDeckId P.==. f.deckId]
            liftIO $ jobs `shouldBe` 1
            history <- P.getBy $ UniqueStep f.gameId 3
            liftIO $ history `shouldSatisfy` isJust
            liftIO $ throwIO AnswerRollback
            pure result) writer
      outcome <- unsafeHandler foundation $ Exception.try $
        updateGameWithAnswerRejectionWith database mempty answer f.gameId Nothing
      (outcome :: Either AnswerRollback (Maybe Text)) `shouldBe` Left AnswerRollback
      committed <- transaction observer $ P.getJust f.gameId
      arkhamGameStep committed `shouldBe` 2
      xp (arkhamGameCurrentData committed) `shouldBe` 5
      transaction observer (P.count [ArkhamDBPushDeckId P.==. f.deckId]) `shouldReturn` 0
      transaction observer (P.count [ArkhamStepArkhamGameId P.==. f.gameId, ArkhamStepStep P.==. 3]) `shouldReturn` 0
      MVar.tryReadMVar executor.executorWake `shouldReturn` Nothing

  for_ [False, True] \outage ->
    it (if outage then "keeps the committed real answer when ArkhamDB fails" else "wakes the real executor only after the actual answer and queue commit") $
      withConnection harness \writer -> withConnection harness \observer -> do
        f <- fixture seed app writer $ if outage then "answer-outage" else "answer-dispatch"
        transaction writer $ P.update f.gameId [ArkhamGameCurrentData P.=. prepareAnswer f]
        remote <- newRemote unexpected source
        -- A fresh executor isolates this answer from older test wake signals.
        executor <- newArkhamDBExecutor
        let foundation = app {appArkhamDBWriteBackExecutor = executor}
        seen <- MVar.newEmptyMVar
        swept <- MVar.newEmptyMVar
        barrier <- newBarrier
        let verifyCommitted = do
              committed <- transaction observer $ P.getJust f.gameId
              arkhamGameStep committed `shouldBe` 3
              xp (arkhamGameCurrentData committed) `shouldBe` 6
              transaction observer (isJust <$> P.getBy (UniqueStep f.gameId 3)) `shouldReturn` True
              P.Entity _ job <- storedJob observer f
              arkhamDBPushTotalXp job `shouldBe` 6
              pure job
            client = remote.client
              { loadDeck = \token key -> do
                  void verifyCommitted
                  void $ MVar.tryPutMVar seen ()
                  if outage then pure $ Left $ ArkhamDBRemoteError "private upstream outage"
                  else remote.client.loadDeck token key
              , upgradeDeck = \token key earned metadata -> do
                  job <- verifyCommitted
                  arkhamDBPushStage job `shouldBe` "creating"
                  decodePushDeckPayload (arkhamDBPushPayload job) `shouldSatisfy` either (const False) fst
                  earned `shouldBe` 6
                  remote.client.upgradeDeck token key earned metadata
              }
            database :: forall a. ReaderT SqlBackend Handler a -> Handler a
            database action = runSqlConn (do
              result <- action
              liftIO $ pauseAt barrier
              pure result) writer
            sweep = push foundation settings client f False 0 >> MVar.putMVar swept ()
        withAsync (runArkhamDBExecutor executor sweep) \worker -> do
          link worker
          withAsync (unsafeHandler foundation $
            updateGameWithAnswerRejectionWith database mempty answer f.gameId Nothing) \answering -> do
              link answering
              awaitBarrier barrier
              committed <- transaction observer $ P.getJust f.gameId
              arkhamGameStep committed `shouldBe` 2
              xp (arkhamGameCurrentData committed) `shouldBe` 5
              transaction observer (P.count [ArkhamDBPushDeckId P.==. f.deckId]) `shouldReturn` 0
              MVar.tryReadMVar seen `shouldReturn` Nothing
              MVar.tryReadMVar executor.executorWake `shouldReturn` Nothing
              releaseBarrier barrier
              -- Linked child exceptions propagate; completion is independently
              -- signalled only after real dispatch, transport and persistence.
              within "postcommit production dispatch" $ MVar.takeMVar swept
              within "actual answer response" (wait answering) `shouldReturn` Nothing
              void verifyCommitted
              P.Entity _ job <- storedJob observer f
              arkhamDBPushStatus job `shouldBe` if outage then "failed" else "succeeded"
              when outage $ arkhamDBPushError job `shouldBe` Just "remoteUnavailable"
        -- The directly run worker is already cancelled; stop is permanent and
        -- has no registered thread to join.
        stopArkhamDBExecutor executor

  it "executes the unparameterized production answer entry point" $
    withConnection harness \connection -> do
      f <- fixture seed app connection "answer-default"
      transaction connection $ P.update f.gameId [ArkhamGameCurrentData P.=. prepareAnswer f]
      unsafeHandler app (updateGameWithAnswerRejection mempty answer f.gameId Nothing) `shouldReturn` Nothing
      committed <- transaction connection $ P.getJust f.gameId
      arkhamGameStep committed `shouldBe` 3
      xp (arkhamGameCurrentData committed) `shouldBe` 6
      transaction connection (P.count [ArkhamDBPushDeckId P.==. f.deckId]) `shouldReturn` 1
