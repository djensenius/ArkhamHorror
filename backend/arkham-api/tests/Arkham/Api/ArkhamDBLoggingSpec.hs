module Arkham.Api.ArkhamDBLoggingSpec where

import Application (makeLogWare)
import Control.Exception (bracket)
import Data.ByteString qualified as BS
import Network.HTTP.Types (status200)
import Network.Wai qualified as Wai
import Network.Wai.Test qualified as WaiTest
import Relude
import Settings (AppSettings (..), compileTimeAppSettings)
import System.Directory (getTemporaryDirectory, removeFile)
import System.IO (hClose, openTempFile)
import System.Log.FastLogger (defaultBufSize, flushLogStr, newFileLoggerSet, rmLoggerSet)
import Test.Hspec

spec :: Spec
spec = for_ [True, False] \detailed ->
  describe (if detailed then "ArkhamDB Detailed logger exclusion" else "ArkhamDB Apache logger exclusion") do
    for_ protectedPaths \path ->
      it ("excludes OAuth query and completion body at " <> toString (decodeUtf8 path :: Text)) do
        output <- captureLog detailed path
        output `shouldBe` ""
    for_ ordinaryPaths \path ->
      it ("continues logging ordinary route " <> toString (decodeUtf8 path :: Text)) do
        output <- captureLog detailed path
        output `shouldSatisfy` BS.isInfixOf path
        when detailed $ output `shouldSatisfy` BS.isInfixOf "probe-completion-secret"

protectedPaths :: [ByteString]
protectedPaths =
  [ "/api/v1/arkham/arkhamdb"
  , "/api/v1/arkham/arkhamdb/"
  , "/api/v1/arkham/arkhamdb/callback"
  , "/api/v1/arkham/arkhamdb/callback/"
  , "/api/v1/arkham/arkhamdb/link/complete"
  , "/api/v1/arkham/arkhamdb/link/complete/"
  , "/api/v1/arkham/%61rkham%64b/callback"
  , "/%61pi/v1/arkham/arkhamdb/link/%63omplete"
  , "/API/V1/ARKHAM/ARKHAMDB/callback"
  , "/api/v1/arkham/ArkhamDB/link/complete"
  , "/api/v1/arkham/arkhamdb%2Fcallback"
  , "/api/v1/arkham%2Farkhamdb/link/complete"
  , "/api//v1/arkham/arkhamdb"
  , "//api/v1/arkham/arkhamdb"
  , "/api/v1/arkham/arkhamdb//callback"
  , "/api/v1//arkham/arkhamdb/callback"
  , "//api/v1/arkham/arkhamdb/callback"
  , "/api/v1/arkham/arkhamdb/link//complete"
  , "/api/v1/arkham//arkhamdb/link/complete"
  , "//api/v1/arkham/arkhamdb/link/complete"
  , "/api%2F%2Fv1/arkham/arkhamdb"
  , "/api/v1/arkham/arkhamdb%2F%2Fcallback"
  , "/api/v1%2F%2Farkham/arkhamdb/callback"
  , "/api/v1/arkham/arkhamdb/link%2F%2Fcomplete"
  , "/api/v1/arkham%2F%2Farkhamdb/link/complete"
  -- arkham.build: the login callback (code and state ride in the query that
  -- captureLog appends), then the authorize and verify bodies.
  , "/api/auth/arkham-build/callback"
  , "/api/auth/arkham-build/callback/"
  , "/api/v1/arkham-build/authorize"
  , "/api/v1/arkham-build/verify"
  , "/API/Auth/Arkham-Build/callback"
  , "/api/V1/ARKHAM-BUILD/verify"
  , "/api/auth/%61rkham-build/callback"
  , "/api/auth%2Farkham-build/callback"
  ]
    <> dotSegmentPaths

-- Dot segments and ';' path parameters must not bypass the exclusion for any
-- credential route, whether matched literally or after dot resolution.
dotSegmentPaths :: [ByteString]
dotSegmentPaths =
  [ -- ArkhamDB
    "/api/v1/arkham/arkhamdb/./callback"
  , "/api/v1/arkham/./arkhamdb/callback"
  , "/./api/v1/arkham/arkhamdb/link/complete"
  , "/api/v1/arkham/arkhamdb/link/./complete/."
  , "/api/v1/arkham/%2E/arkhamdb/callback"
  , "/api/v1/arkham/x/../arkhamdb/callback"
  , "/../api/v1/arkham/arkhamdb/callback"
  , "/api/v1/arkham/arkhamdb/../arkhamdb/link/complete"
  , "/api/v1/arkham/arkhamdb/../decks"
  , "/api/v1/arkham/arkhamdb;x=1/callback"
  , "/api;x/v1/arkham/arkhamdb/callback"
  , "/api/v1/arkham/arkhamdb/callback;x=1"
  , "/api/v1/;x/arkham/arkhamdb/link/complete"
  , "/api/v1/arkham/arkhamdb%3Bx/callback"
  , "/api/v1/./arkham/x;y/../arkhamdb;z/./callback"
  , "/api/v1/arkham/.;x/arkhamdb/callback"
  , "/api/v1/arkham/x/..;y/arkhamdb/link/complete"
  , -- arkham.build login callback
    "/api/auth/./arkham-build/callback"
  , "/./api/auth/arkham-build/callback"
  , "/api/auth/x/../arkham-build/callback"
  , "/../api/auth/arkham-build/callback"
  , "/api/auth/arkham-build;x=1/callback"
  , "/api;x/auth/arkham-build/callback"
  , "/api/./auth;x/y/../arkham-build/./callback"
  , -- arkham.build authorize and verify
    "/api/v1/./arkham-build/authorize"
  , "/api/v1/arkham-build/./verify"
  , "/api/v1/x/../arkham-build/verify"
  , "/../api/v1/arkham-build/authorize"
  , "/api/v1/arkham-build;x=1/authorize"
  , "/api/v1;x/arkham-build/verify"
  , "/api/./v1;x/y/../arkham-build;z/./verify"
  ]

-- Empty segments must not suppress logging of ordinary routes.
ordinaryPaths :: [ByteString]
ordinaryPaths =
  [ "/api/v1/arkham/arkhamdb-other"
  , "/api/v1/arkham/arkhamdb2"
  , "/api/v1/arkham/decks"
  , "/api//v1/arkham/decks"
  , "//api/v1/arkham/decks"
  , "/api/v1/arkham//arkhamdb-other"
  , "/api/auth/arkham-build-other/callback"
  , "/api/v1/arkham-builder/verify"
  , "/api/v1/site-settings"
  -- Dot segments and path parameters must not suppress ordinary routes, nor
  -- may resolution that leaves no credential prefix.
  , "/api/v1/./arkham/decks"
  , "/api/v1/arkham/decks;x=1"
  , "/api/v1/arkham/arkhamdb-other;x=1"
  , "/api/v1/arkham/./arkhamdb2"
  , "/api/v1/arkham/x/../games"
  , "/api/v1/arkham/arkhamdb-other/../decks"
  , "/api/v1/arkham/../arkhamdb/callback"
  , "/api/auth/./arkham-build-other/callback"
  , "/api/v1/arkham-builder;x/../verify"
  ]

-- Use the production middleware, including its body-capture path, for both
-- formats. No separately implemented exclusion predicate participates here.
captureLog :: Bool -> ByteString -> IO ByteString
captureLog detailed path = do
  directory <- lookupEnv "ARKHAM_TEST_LOG_DIR" >>= maybe getTemporaryDirectory pure
  bracket (openTempFile directory "arkhamdb-variants.log") (removeFile . fst) \(file, handle) -> do
    hClose handle
    bracket (newFileLoggerSet defaultBufSize file) rmLoggerSet \logger -> do
      middleware <- makeLogWare compileTimeAppSettings {appDetailedRequestLogging = detailed} logger
      let request = WaiTest.SRequest
            (WaiTest.setPath WaiTest.defaultRequest
              { Wai.requestMethod = "POST"
              , Wai.requestHeaders = [("Content-Type", "application/json")]
              } $ path <> "?code=probe-oauth-code&state=probe-oauth-state")
            "{\"completion\":\"probe-completion-secret\"}"
          application req respond = do
            Wai.strictRequestBody req `shouldReturn` WaiTest.simpleRequestBody request
            respond $ Wai.responseLBS status200 [] "ok"
      void $ WaiTest.runSession (WaiTest.srequest request) $ middleware application
      flushLogStr logger
    BS.readFile file
