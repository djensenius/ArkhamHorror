{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Real connections only. The launcher owns migrations and cluster lifetime;
-- this module owns pools, pinned sessions, deadlines, and lock-observation gates.
module Postgres.Harness where

import Control.Concurrent (threadDelay)
import Control.Concurrent.MVar qualified as MVar
import Control.Exception (bracket)
import Control.Monad.Logger (runNoLoggingT)
import Data.Pool (destroyAllResources)
import Data.Text qualified as T
import Database.Persist.Postgresql (ConnectionString, createPostgresqlPool, withPostgresqlConn)
import Database.Persist.Sql (ConnectionPool, Single (..), SqlBackend, rawSql, runSqlConn)
import Database.Persist.Types (PersistValue (..))
import Relude
import System.Directory (canonicalizePath)
import System.Environment (getEnv)
import System.FilePath ((</>))
import System.Timeout (timeout)

data Harness = Harness
  { connectionString :: ConnectionString
  , connectionPool :: ConnectionPool
  }

-- Fail closed without the launcher's capability and exact private socket.
-- Never read DATABASE_URL or accept a caller-supplied libpq connection string.
withHarness :: (Harness -> IO a) -> IO a
withHarness action = do
  root <- canonicalizePath =<< getEnv "ARKHAMDB_TEST_ROOT"
  token <- getEnv "ARKHAMDB_TEST_TOKEN"
  marker <- readFileBS (root </> "owner")
  unless (not (null token) && marker == encodeUtf8 (toText token)) $
    fail "PostgreSQL launcher ownership marker does not match"
  host <- getEnv "PGHOST"
  port <- getEnv "PGPORT"
  database <- getEnv "PGDATABASE"
  user <- getEnv "PGUSER"
  unless (host == root </> "s" && port == "25446" && database == "arkhamdb_test" && user == "arkhamdb_harness") $
    fail "Refusing a non-harness PostgreSQL target"
  let quote = ("'" <>) . (<> "'") . T.replace "'" "\\'" . T.replace "\\" "\\\\" . toText
      connectionString = encodeUtf8 $ "host=" <> quote host <> " port=25446 dbname=arkhamdb_test user=arkhamdb_harness connect_timeout=5"
  bracket
    (runNoLoggingT $ createPostgresqlPool connectionString 3)
    destroyAllResources
    (\connectionPool -> action Harness {..})

-- A pinned backend is never used concurrently. Nest this helper for independent
-- sessions; runSqlConn commits on success and rolls back on exceptions.
withConnection :: Harness -> (SqlBackend -> IO a) -> IO a
withConnection harness action =
  runNoLoggingT $ withPostgresqlConn harness.connectionString (liftIO . action)

transaction :: SqlBackend -> ReaderT SqlBackend IO a -> IO a
transaction = flip runSqlConn

backendPid :: SqlBackend -> IO Int64
backendPid connection = transaction connection do
  rows <- rawSql "SELECT pg_backend_pid()::bigint" []
  case rows of
    [Single pid] -> pure pid
    _ -> liftIO $ fail "Expected one PostgreSQL backend PID"

within :: Text -> IO a -> IO a
within label action =
  timeout 10000000 action >>= maybe (fail $ toString $ "Timed out: " <> label) pure

data Barrier = Barrier (MVar.MVar ()) (MVar.MVar ())

newBarrier :: IO Barrier
newBarrier = Barrier <$> MVar.newEmptyMVar <*> MVar.newEmptyMVar

-- Call inside a transaction, after the operation under test has taken its lock.
pauseAt :: Barrier -> IO ()
pauseAt (Barrier reached released) = within "transaction barrier release" do
  MVar.putMVar reached ()
  MVar.takeMVar released

awaitBarrier :: Barrier -> IO ()
awaitBarrier (Barrier reached _) = within "transaction reached barrier" $ MVar.readMVar reached

releaseBarrier :: Barrier -> IO ()
releaseBarrier (Barrier _ released) = within "release transaction barrier" $ MVar.putMVar released ()

-- A started thread is not evidence that PostgreSQL has attempted its UPDATE.
-- Observe the exact blocking session before releasing the competing transaction.
-- The third connection is instrumentation only, not a business-SQL substitute.
awaitBlockedBy :: SqlBackend -> Int64 -> Int64 -> IO ()
awaitBlockedBy observer blocker waiter = within "PostgreSQL lock dependency" loop
 where
  loop = do
    rows <- transaction observer $
      rawSql "SELECT ?::integer = ANY(pg_blocking_pids(?::integer))"
        [PersistInt64 blocker, PersistInt64 waiter]
    case rows of
      [Single True] -> pure ()
      [Single False] -> threadDelay 10000 >> loop
      _ -> fail "Expected one PostgreSQL lock observation"
