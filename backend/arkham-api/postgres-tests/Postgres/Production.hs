{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Run the actual Handler stores against the disposable database. Do not call
-- makeFoundation: it starts background services and reads deployment settings.
-- Every App field is initialized; there is no undefined/fake SQL interpreter.
module Postgres.Production where

import Api.Arkham.ArkhamDBStore (persistentStoreWith)
import Arkham.ArkhamDB.Service (ArkhamDBStore)
import Arkham.ArkhamDB.WriteBack (newArkhamDBExecutor)
import Arkham.Decklist (ArkhamDBDecklist)
import Config (makeYesodLogger)
import Control.Concurrent.MVar qualified as MVar
import Control.Exception (bracket)
import Data.Bugsnag.Settings qualified as Bugsnag
import Data.Time.Clock (getCurrentTime)
import Database.Persist.Postgresql (pgConnStr, pgPoolSize)
import Database.Persist.Sql (SqlBackend, runSqlConn)
import Foundation (App (..), Handler, MessageBroker (..), unsafeHandler)
import Network.HTTP.Client (defaultManagerSettings, newManager)
import Network.HTTP.Types (Status, status401)
import Network.Wai (Request)
import Postgres.Harness
import Relude
import Settings (AppSettings (..), compileTimeAppSettings)
import System.Log.FastLogger (defaultBufSize, newStderrLoggerSet, rmLoggerSet)
import ThirdEdition.Store qualified as ThirdEdition
import UnliftIO.Exception qualified as Exception
import Yesod.Core.Types (ErrorResponse (NotAuthenticated), HandlerContents (..), HandlerFor (..), HandlerData (..), YesodRequest (..))

withProductionApp :: Harness -> (App -> IO a) -> IO a
withProductionApp harness action =
  bracket (newStderrLoggerSet defaultBufSize) rmLoggerSet \loggerSet -> do
    appLogger <- makeYesodLogger loggerSet
    -- http-client owns manager cleanup; the explicit stub transports below
    -- never issue requests through this foundation's manager.
    appHttpManager <- newManager defaultManagerSettings
    appArkhamDBWriteBackExecutor <- newArkhamDBExecutor
    appGameRooms <- MVar.newMVar mempty
    appEventRooms <- MVar.newMVar mempty
    appThirdEditionRooms <- MVar.newMVar mempty
    appThirdEditionStore <- ThirdEdition.newMemoryStore
    appPubSubHealth <- newTVarIO =<< getCurrentTime
    let appSettings = compileTimeAppSettings
          { appDatabaseConf = compileTimeAppSettings.appDatabaseConf
              { pgConnStr = harness.connectionString, pgPoolSize = 3 }
          , appRedisConnectionInfo = Nothing
          , appArkhamDB = Nothing
          }
        appConnPool = harness.connectionPool
        appMessageBroker = WebSocketBroker
        appPubSubSupervisorThread = Nothing
        appRoomHeartbeatThread = Nothing
        appArkhamDBClient = Nothing
        appBugsnag = Bugsnag.defaultSettings ""
    action App {..}

-- Match normal runDB boundaries: each store action commits independently,
-- except withRefreshLock, whose production callback reuses its locked backend.
runStoreActions :: App -> SqlBackend -> (ArkhamDBStore Handler -> Handler a) -> IO a
runStoreActions = runStoreActionsWith (const $ pure ())

runStoreActionsWith :: (ArkhamDBDecklist -> Handler ()) -> App -> SqlBackend -> (ArkhamDBStore Handler -> Handler a) -> IO a
runStoreActionsWith validate app connection action = unsafeHandler app $
  action $ persistentStoreWith (lift . validate) (`runSqlConn` connection)

-- The same connection/transaction reuse as production withRefreshLock. This
-- permits an outer rollback test without reimplementing any store operations.
-- Only card validation is a no-op: these SQL tests do not test card legality.
runStore :: App -> SqlBackend -> (ArkhamDBStore Handler -> Handler a) -> IO a
runStore = runStoreWith (const $ pure ())

runStoreWith :: (ArkhamDBDecklist -> Handler ()) -> App -> SqlBackend -> (ArkhamDBStore Handler -> Handler a) -> IO a
runStoreWith validate app connection action = unsafeHandler app $ storeTransaction validate connection action

storeTransaction :: (ArkhamDBDecklist -> Handler ()) -> SqlBackend -> (ArkhamDBStore Handler -> Handler a) -> Handler a
storeTransaction validate connection action =
  runSqlConn (lift $ action store) connection
 where
  store = persistentStoreWith (lift . validate) (`runReaderT` connection)

-- fakeHandler cannot return an HTTP response. Catch the actual HandlerContents
-- only AFTER the SQL runner has rolled back; never swallow it in a transaction.
-- Unexpected exceptions/redirects still fail the test. NotAuthenticated is the
-- explicit JWT rejection case; other HCError responses are not swallowed.
runHandlerResponse :: App -> Handler a -> IO (Either Status a)
runHandlerResponse app action = unsafeHandler app $
  (Right <$> action) `Exception.catch` \response -> case response of
    HCContent status _ -> pure $ Left status
    HCError NotAuthenticated -> pure $ Left status401
    _ -> Exception.throwIO response

-- Supply an actual request to the existing fake-Handler runner. Authentication
-- and the Handler's DB/HTTP response logic are unchanged; this is not WAI routing.
withRequest :: Request -> Handler a -> Handler a
withRequest request action = HandlerFor \environment ->
  unHandlerFor action environment
    {handlerRequest = (handlerRequest environment) {reqWaiRequest = request}}
