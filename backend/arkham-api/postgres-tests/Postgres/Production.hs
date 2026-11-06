{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Run the actual Handler stores against the disposable database. Do not call
-- makeFoundation: it starts background services and reads deployment settings.
-- Every App field is initialized; there is no undefined/fake SQL interpreter.
module Postgres.Production where

import Api.Arkham.ArkhamDBStore (persistentStoreWith)
-- The YesodDispatch App instance (an orphan generated in Application).
import Application ()
import Arkham.ArkhamDB.Service (ArkhamDBStore)
import Arkham.ArkhamDB.WriteBack (newArkhamDBExecutor)
import Arkham.Decklist (ArkhamDBDecklist)
import Config (makeYesodLogger)
import Control.Concurrent.MVar qualified as MVar
import Control.Exception (bracket)
import Data.Aeson.Encoding (encodingToLazyByteString, unsafeToEncoding)
import Data.Bugsnag.Settings qualified as Bugsnag
import Data.Time.Clock (getCurrentTime)
import Database.Persist.Postgresql (pgConnStr, pgPoolSize)
import Database.Persist.Sql (SqlBackend, runSqlConn)
import Entity.Arkham.Deck (ArkhamDeckId)
import Entity.User (UserId)
import Foundation (App (..), Handler, MessageBroker (..), unsafeHandler, userIdToToken)
import Network.HTTP.Client (defaultManagerSettings, newManager)
import Network.HTTP.Types (Header, Method, Status, status401)
import Network.Wai (Application, Request, requestHeaders, requestMethod)
import Network.Wai.Test qualified as WaiTest
import Postgres.Harness
import Relude
import Settings (AppSettings (..), compileTimeAppSettings)
import System.Log.FastLogger (defaultBufSize, newStderrLoggerSet, rmLoggerSet)
import ThirdEdition.Store qualified as ThirdEdition
import UnliftIO.Exception qualified as Exception
import Yesod.Core (makeLogger, toPathPiece)
import Yesod.Core.Dispatch (defaultGen, getGetMaxExpires, toWaiAppYre)
import Yesod.Core.Types (Content (ContentBuilder), ErrorResponse (NotAuthenticated), HandlerContents (..), HandlerFor (..), HandlerData (..), TypedContent (..), YesodRequest (..), YesodRunnerEnv (..))

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
    appThirdEditionStore <- ThirdEdition.newPostgresStore harness.connectionPool
    appPubSubHealth <- newTVarIO =<< getCurrentTime
    appMaintenance <- newTVarIO Nothing
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
        appArchiveThreads = []
        appPodId = "arkhamdb-postgres-tests"
        appArkhamDBClient = Nothing
        appBugsnag = quietBugsnag
    action App {..}

-- WAI-dispatched requests run bugsnagYesodMiddleware. No release stage is
-- enabled and every exception is ignored, so an unexpected 500 in a test is
-- never reported over the network (the API key is also empty).
quietBugsnag :: Bugsnag.Settings
quietBugsnag = (Bugsnag.defaultSettings "")
  { Bugsnag.settings_enabledReleaseStages = []
  , Bugsnag.settings_ignoreException = const True
  }

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

-- Like runHandlerResponse, but also returns the rendered body so callers can
-- assert an error code (many ArkhamDB codes share the default 502 status).
-- Only built responses (e.g. sendStatusJSON) are captured; others rethrow.
runHandlerResponseBody :: App -> Handler a -> IO (Either (Status, LByteString) a)
runHandlerResponseBody app action = unsafeHandler app $
  (Right <$> action) `Exception.catch` \response -> case response of
    HCContent status (TypedContent _ (ContentBuilder body _)) ->
      pure $ Left (status, encodingToLazyByteString $ unsafeToEncoding body)
    _ -> Exception.throwIO response

-- Supply an actual request to the existing fake-Handler runner. Authentication
-- and the Handler's DB/HTTP response logic are unchanged; this is not WAI routing.
withRequest :: Request -> Handler a -> Handler a
withRequest request action = HandlerFor \environment ->
  unHandlerFor action environment
    {handlerRequest = (handlerRequest environment) {reqWaiRequest = request}}

-- The production WAI application as Yesod.Core.toWaiAppPlain builds it: route
-- dispatch (404/405), yesodMiddleware (maintenance, Bugsnag and
-- defaultYesodMiddleware) and isAuthorized. Unlike toWaiAppPlain there is no
-- session backend: the API authenticates with JWT headers only, and the
-- client-session backend would read (or create) config/client_session_key.aes
-- relative to the working directory. Application's outer gzip, request-logging
-- and CORS middleware are not applied.
productionApplication :: App -> IO Application
productionApplication app = do
  yreLogger <- makeLogger app
  yreGetMaxExpires <- getGetMaxExpires
  pure $ toWaiAppYre YesodRunnerEnv
    {yreLogger, yreSite = app, yreSessionBackend = Nothing, yreGen = defaultGen, yreGetMaxExpires}

-- One request through productionApplication: method, path (with any query),
-- headers and body in; the response status and body out.
routeRequest :: App -> Method -> ByteString -> [Header] -> LByteString -> IO (Status, LByteString)
routeRequest app method path headers body = do
  application <- productionApplication app
  response <- WaiTest.runSession
    (WaiTest.srequest $ WaiTest.SRequest
      (WaiTest.setPath WaiTest.defaultRequest path) {requestMethod = method, requestHeaders = headers}
      body)
    application
  pure (WaiTest.simpleStatus response, WaiTest.simpleBody response)

-- The production JWT Authorization header for a user.
authorizationFor :: App -> UserId -> IO Header
authorizationFor app uid = do
  token <- unsafeHandler app $ userIdToToken uid
  pure ("Authorization", "Token " <> encodeUtf8 token)

-- A saved deck's API path, e.g. deckPath deckId "/overlay".
deckPath :: ArkhamDeckId -> Text -> ByteString
deckPath deckId suffix = encodeUtf8 $ "/api/v1/arkham/decks/" <> toPathPiece deckId <> suffix
