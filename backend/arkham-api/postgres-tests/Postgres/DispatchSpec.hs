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
import Control.Concurrent (ThreadId, threadDelay)
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
import GHC.Conc (ThreadStatus (..), threadStatus)
import Postgres.Harness
import Postgres.Remote
import Postgres.WriteBackSpec (Fixture (..), Seed, fixture)
import Relude
import Settings (ArkhamDBSettings)
import System.Timeout (timeout)
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
            -- Every sweep reports its outcome: the executor thread itself
            -- does not propagate a sweep's synchronous exception.
            sweep = (Exception.try (push foundation settings client f False 0) :: IO (Either SomeException ()))
              >>= MVar.putMVar swept . first (show @Text)
            swept' label = within label (MVar.takeMVar swept) >>= either (expectationFailure . toString) pure
        -- The production executor thread, as the server starts it.
        startArkhamDBExecutor executor $ runArkhamDBExecutor executor sweep
        flip Exception.finally (void $ timeout 2000000 $ stopArkhamDBExecutor executor) do
          -- Startup fills the wake slot, so a first sweep runs at once with
          -- nothing queued; drain it before the answer.
          swept' "startup sweep"
          MVar.tryReadMVar seen `shouldReturn` Nothing
          withAsync (unsafeHandler foundation $
            updateGameWithAnswerRejectionWith database mempty answer f.gameId Nothing) \answering -> do
              link answering
              awaitBarrier barrier
              committed <- transaction observer $ P.getJust f.gameId
              arkhamGameStep committed `shouldBe` 2
              xp (arkhamGameCurrentData committed) `shouldBe` 5
              transaction observer (P.count [ArkhamDBPushDeckId P.==. f.deckId]) `shouldReturn` 0
              -- The running executor would take a wake at once, so also
              -- require that no sweep and no remote call has happened.
              MVar.tryReadMVar swept `shouldReturn` Nothing
              MVar.tryReadMVar seen `shouldReturn` Nothing
              MVar.tryReadMVar executor.executorWake `shouldReturn` Nothing
              releaseBarrier barrier
              -- Completion is independently signalled only after real
              -- dispatch, transport and persistence.
              swept' "postcommit production dispatch"
              within "actual answer response" (wait answering) `shouldReturn` Nothing
              void verifyCommitted
              P.Entity _ job <- storedJob observer f
              arkhamDBPushStatus job `shouldBe` if outage then "failed" else "succeeded"
              when outage $ arkhamDBPushError job `shouldBe` Just "remoteUnavailable"
          running <- runningExecutor executor
          timeout 2000000 (stopArkhamDBExecutor executor) `shouldReturn` Just ()
          assertExecutorStopped executor running

  it "executes the unparameterized production answer entry point" $
    withConnection harness \connection -> do
      f <- fixture seed app connection "answer-default"
      transaction connection $ P.update f.gameId [ArkhamGameCurrentData P.=. prepareAnswer f]
      unsafeHandler app (updateGameWithAnswerRejection mempty answer f.gameId Nothing) `shouldReturn` Nothing
      committed <- transaction connection $ P.getJust f.gameId
      arkhamGameStep committed `shouldBe` 3
      xp (arkhamGameCurrentData committed) `shouldBe` 6
      transaction connection (P.count [ArkhamDBPushDeckId P.==. f.deckId]) `shouldReturn` 1

-- The started executor's registered thread, still running.
runningExecutor :: ArkhamDBExecutor -> IO (ThreadId, MVar ())
runningExecutor executor = do
  current <- MVar.readMVar executor.executorState
  (tid, done) <- maybe (fail "The executor was never started") pure current.executorThread
  MVar.tryReadMVar done `shouldReturn` Nothing
  pure (tid, done)

-- After stop: the thread's completion is signalled, the executor refuses
-- wakes, and the thread itself has exited (a blocked thread is still alive).
assertExecutorStopped :: ArkhamDBExecutor -> (ThreadId, MVar ()) -> IO ()
assertExecutorStopped executor (tid, done) = do
  MVar.tryReadMVar done `shouldReturn` Just ()
  wakeArkhamDBExecutor executor `shouldReturn` False
  let exited = threadStatus tid >>= \case
        ThreadFinished -> pure ()
        ThreadDied -> pure ()
        _ -> threadDelay 10000 >> exited
  timeout 2000000 exited `shouldReturn` Just ()
