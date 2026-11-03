{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Independently runnable harness checks. These establish connection/barrier
-- and rollback plumbing, NOT production ArkhamDB lifecycle coverage (Main.hs).
module Postgres.HarnessSpec where

import Control.Exception (throwIO)
import Database.Persist.Sql (Single (..), SqlBackend, rawExecute, rawSql)
import Postgres.Harness
import Relude hiding (first, second)
import Test.Hspec
import UnliftIO.Async (link, wait, withAsync)

data ProbeAbort = ProbeAbort deriving stock (Show)
instance Exception ProbeAbort

main :: IO ()
main = withHarness $ hspec . spec

spec :: Harness -> Spec
spec harness = describe "PostgreSQL harness plumbing (not production lifecycle coverage)" do
  it "uses three distinct sessions and observes a real lock wait before releasing" $
    withConnection harness \first -> withConnection harness \second -> withConnection harness \observer -> do
      firstPid <- backendPid first
      secondPid <- backendPid second
      observerPid <- backendPid observer
      firstPid `shouldNotBe` secondPid
      observerPid `shouldNotBe` firstPid
      observerPid `shouldNotBe` secondPid
      barrier <- newBarrier
      -- Advisory locks are harness probes only. No ArkhamDB SQL is copied here.
      let lock = void
            (rawSql "SELECT 1::bigint FROM pg_advisory_xact_lock(246246)" [] :: ReaderT SqlBackend IO [Single Int64])
      withAsync
        (transaction first $ lock >> liftIO (pauseAt barrier))
        \holder -> do
          link holder
          awaitBarrier barrier
          withAsync (transaction second lock) \waiter -> do
            link waiter
            awaitBlockedBy observer firstPid secondPid
            releaseBarrier barrier
            within "probe holder" (wait holder)
            within "probe waiter" (wait waiter)

  it "rolls back a transaction on exceptions and can reuse the connection" $
    withConnection harness \connection -> do
      transaction connection $ rawExecute "CREATE TEMP TABLE harness_probe (value integer)" []
      let abort = transaction connection do
            rawExecute "INSERT INTO harness_probe VALUES (1)" []
            liftIO $ throwIO ProbeAbort
      abort `shouldThrow` (\ProbeAbort -> True)
      rows <- transaction connection $ rawSql "SELECT count(*) FROM harness_probe" []
      rows `shouldBe` [Single (0 :: Int64)]

  it "bounds SQL and idle transaction waits at the server" $
    withConnection harness \connection -> do
      rows <- transaction connection $ rawSql
        "SELECT current_setting('statement_timeout'), current_setting('lock_timeout'), current_setting('idle_in_transaction_session_timeout')" []
      rows `shouldBe` [(Single ("8s" :: Text), Single ("3s" :: Text), Single ("10s" :: Text))]
