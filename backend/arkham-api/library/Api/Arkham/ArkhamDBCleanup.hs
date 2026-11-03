module Api.Arkham.ArkhamDBCleanup where

import Control.Concurrent (threadDelay)
import Data.Time.Clock (UTCTime, getCurrentTime)
import Database.Persist.Sql
  ( ConnectionPool, Single, SqlBackend, rawExecute, rawSql, runSqlPool )
import Database.Persist.Types (PersistValue (..))
import Relude
import System.Timeout (timeout)
import UnliftIO.Async (withAsync)
import UnliftIO.Exception (tryAny)

cleanupBatchSize :: Int
cleanupBatchSize = 128

-- Only expired authorizations are invalidated. Accounts, generations, and deck
-- links are never touched. Locked callbacks are skipped rather than delayed;
-- another pass can collect them after their transaction releases its lock.
purgeExpiredLinks :: MonadIO m => UTCTime -> ReaderT SqlBackend m Int
purgeExpiredLinks now = do
  rawExecute "SET LOCAL lock_timeout = '1s'" []
  rawExecute "SET LOCAL statement_timeout = '2s'" []
  rows <- rawSql
    "WITH expired AS (SELECT id FROM arkhamdb_link_states WHERE expires_at <= ? ORDER BY expires_at, id LIMIT ? FOR UPDATE SKIP LOCKED) DELETE FROM arkhamdb_link_states AS states USING expired WHERE states.id = expired.id AND states.expires_at <= ? RETURNING states.id::text"
    [PersistUTCTime now, PersistInt64 $ fromIntegral cleanupBatchSize, PersistUTCTime now]
  pure $ length (rows :: [Single Text])

-- An immediate pass, then one bounded batch every minute. The outer deadline
-- also bounds pool acquisition; database statement/lock limits are local to
-- the transaction and cannot leak into subsequent requests on that connection.
withArkhamDBCleanup :: ConnectionPool -> IO () -> IO a -> IO a
withArkhamDBCleanup pool warn = withCleanupWorker 60000000 5000000 sweep warn
 where
  sweep = do
    now <- getCurrentTime
    void $ runSqlPool (purgeExpiredLinks now) pool

-- The warning callback deliberately receives no exception: driver exceptions
-- can contain query parameters, including encrypted pending credentials.
-- tryAny does not swallow async cancellation; withAsync joins the worker on
-- normal return or exceptions before the enclosing server closes its pool.
withCleanupWorker :: Int -> Int -> IO () -> IO () -> IO a -> IO a
withCleanupWorker interval budget sweep warn action =
  withAsync worker $ const action
 where
  worker = forever do
    result <- tryAny $ timeout budget sweep
    case result of
      Right (Just ()) -> pure ()
      _ -> void $ tryAny $ timeout budget warn
    threadDelay interval
