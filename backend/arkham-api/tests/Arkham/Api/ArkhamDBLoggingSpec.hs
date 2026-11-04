module Arkham.Api.ArkhamDBLoggingSpec where

import Application (makeLogWare)
import Control.Exception (bracket)
import Data.ByteString qualified as BS
import Network.HTTP.Types (decodePathSegments, parseQuery, status200)
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
        output `shouldSatisfy` BS.isInfixOf (loggedPath path)
        when detailed $ output `shouldSatisfy` BS.isInfixOf "probe-completion-secret"
    for_ rawProtectedPaths \path ->
      it ("excludes OAuth query and completion body at raw " <> toString (decodeUtf8 path :: Text)) do
        output <- captureRawLog detailed path
        output `shouldBe` ""
    for_ rawOrdinaryPaths \path ->
      it ("continues logging ordinary raw route " <> toString (decodeUtf8 path :: Text)) do
        output <- captureRawLog detailed path
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
  , "/api/v1/arkham/x/%2E%2E/arkhamdb/callback"
  , "/api/v1/arkham/x/%2e%2e/arkhamdb/link/complete"
  , "/api/v1/arkham/%2e/arkhamdb/callback"
  , "/api/v1/arkham;%2F..%2F/arkhamdb/callback"
  -- ".." pops an empty segment before empty segments are dropped (RFC 3986).
  , "/api/v1/arkham//../arkhamdb/callback"
  , "/api//../v1/arkham/arkhamdb/callback"
  , "/api/v1/arkham///../../arkhamdb/link/complete"
  , "/api/v1/arkham/x//../../arkhamdb/callback"
  , "/api/v1/arkham//../arkhamdb/callback/."
  , "/api/v1/arkham//%2e%2e/arkhamdb/callback"
  , "/api/v1/arkham%2F%2F..%2Farkhamdb/callback"
  , "/api/v1/arkham/;x/../arkhamdb/callback"
  , "/api/v1/arkham//;x/../../arkhamdb/link/complete"
  -- ".." pops a whole decoded segment, "%2F" included, as on the raw path.
  , "/api/v1/arkham/q%2Fr/../arkhamdb/callback"
  , "/api/v1/arkham/q%2Fr/%2e%2e/arkhamdb/callback"
  , "/api/v1/arkham/q%2Fr;x/../arkhamdb/callback"
  , "/api/v1/arkham/q%2Fr/x/../../arkhamdb/link/complete"
  , "/api/v1/arkham/q%2Fr//../../arkhamdb/callback"
  , -- arkham.build login callback
    "/api/auth/./arkham-build/callback"
  , "/./api/auth/arkham-build/callback"
  , "/api/auth/x/../arkham-build/callback"
  , "/../api/auth/arkham-build/callback"
  , "/api/auth/arkham-build;x=1/callback"
  , "/api;x/auth/arkham-build/callback"
  , "/api/./auth;x/y/../arkham-build/./callback"
  , "/api/auth/x/%2E%2E/arkham-build/callback"
  , "/api/auth/%2e/arkham-build/callback"
  , "/api/auth;%2F..%2F/arkham-build/callback"
  , "/api/auth//../arkham-build/callback"
  , "/api//../auth/arkham-build/callback"
  , "/api/auth///../../arkham-build/callback/."
  , "/api/auth//%2E%2E/arkham-build/callback"
  , "/api/auth/;x/../arkham-build/callback"
  , "/api/auth/q%2Fr/../arkham-build/callback"
  , "/api/auth/q%2Fr;x/%2E%2E/arkham-build/callback"
  , -- arkham.build authorize and verify
    "/api/v1/./arkham-build/authorize"
  , "/api/v1/arkham-build/./verify"
  , "/api/v1/x/../arkham-build/verify"
  , "/../api/v1/arkham-build/authorize"
  , "/api/v1/arkham-build;x=1/authorize"
  , "/api/v1;x/arkham-build/verify"
  , "/api/./v1;x/y/../arkham-build;z/./verify"
  , "/api/v1/x/%2e%2e/arkham-build/verify"
  , "/api/v1/%2E/arkham-build/authorize"
  , "/api/v1;%2F..%2F/arkham-build/authorize"
  , "/api/v1//../arkham-build/authorize"
  , "/api//../v1/arkham-build/verify"
  , "/api/v1//../../v1/arkham-build/verify"
  , "/api/v1///../../arkham-build/verify/."
  , "/api/v1//%2e%2e/arkham-build/authorize"
  , "/api/v1/;x/../arkham-build/authorize"
  , "/api/v1/q%2Fr/../arkham-build/verify"
  , "/api/v1/q%2F%2Fr/../arkham-build/authorize"
  ]
    <> irregularCredentialPaths

-- The fail-closed backstop: an irregular path that mentions a credential route
-- name is never logged, whatever a proxy would normalise it to. These cover
-- proxies that keep "%2e" encoded, treat ';' as data, mix those, or decode
-- twice ("%252F", "%2561").
irregularCredentialPaths :: [ByteString]
irregularCredentialPaths =
  [ "/api/v1/arkham/%2e%2e/../arkhamdb/callback"
  , "/api/v1/arkham/%2e/../arkhamdb/callback"
  , "/api/auth/%2e/../arkham-build/callback"
  , "/api/auth/%2e%2e/../arkham-build/callback"
  , "/api/v1/%2e%2e/../arkham-build/verify"
  , "/api/v1/%2e/../arkham-build/authorize"
  , "/api/v1/arkham/..;x/%2F../arkhamdb/callback"
  , "/api/v1/arkham/..;x/..%2F/arkhamdb/callback"
  , "/api/auth/..;x/%2F../arkham-build/callback"
  , "/api/auth/..;x/..%2F/arkham-build/callback"
  , "/api/v1/..;x/%2F../arkham-build/callback"
  , "/api/v1/..;x/..%2F/arkham-build/verify"
  , "/api/v1/arkham/q%2F../arkhamdb/..;x/callback"
  , "/api/auth/q%2F../arkham-build/..;x/callback"
  , "/api/v1/q%2F../arkham-build/..;x/callback"
  , "/api/v1/arkham/x%252F..%252F/arkhamdb/callback"
  , "/api/auth/x%252F..%252F/arkham-build/callback"
  , "/api/v1/x%252F..%252F/arkham-build/verify"
  , "/api/v1/arkham%252Farkhamdb/callback"
  , "/api/v1/arkham/%2561rkhamdb/callback"
  , "/api/auth/%2561rkham-build/callback"
  , "/api/v1/%2561rkham-build/authorize"
  -- Over-exclusion is deliberate: these resolve to no credential route, but
  -- they are irregular and name one, and they 404 or redirect without reaching
  -- a handler.
  , "/api/v1/arkham/../arkhamdb/callback"
  , "/api/v1//../arkhamdb/callback"
  , "/api/v1/q%2Fr/../arkhamdb/callback"
  -- A regular non-credential ArkhamDB route stays excluded by its prefix.
  , "/api/v1/arkham/arkhamdb/status"
  ]
    <> replacementCharPaths

-- Overlong UTF-8 separators ("%C0%AF" and "%E0%80%AF" are '/', "%C0%AE" is
-- '.') decode leniently to U+FFFD, which a proxy that wrongly decodes them
-- would treat as a separator or dot. A segment with U+FFFD is irregular, and
-- credential names are split on it.
replacementCharPaths :: [ByteString]
replacementCharPaths =
  [ "/api/v1/arkham%C0%AFarkhamdb/callback"
  , "/api/v1/arkham%E0%80%AFarkhamdb/callback"
  , "/api/auth%C0%AFarkham-build/callback"
  , "/api/auth%E0%80%AFarkham-build/callback"
  , "/api/v1%C0%AFarkham-build/verify"
  , "/api/v1%E0%80%AFarkham-build/authorize"
  -- A literal U+FFFD between credential names.
  , "/api/v1/arkham%EF%BF%BDarkhamdb/callback"
  , "/api/v1/arkham%EF%BF%BDarkhamdb/link/complete"
  , "/api/auth%EF%BF%BDarkham-build/callback"
  , "/api/v1%EF%BF%BDarkham-build/verify"
  , "/api/v1%EF%BF%BDarkham-build/authorize"
  -- Only the irregular half matters: "arkhamdb" is already an exact piece.
  , "/api/v1/arkham/x/%C0%AE%C0%AE/arkhamdb/callback"
  , "/api/auth/x/%C0%AE%C0%AE/arkham-build/callback"
  , "/api/v1/x/%C0%AE%C0%AE/arkham-build/verify"
  -- Only the split half matters: the decoded segment already contains '%'.
  , "/api/v1/arkham%25C0%25AFarkhamdb/callback"
  , "/api/auth%25C0%25AFarkham-build/callback"
  , "/api/v1%25C0%25AFarkham-build/verify"
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
  , "/api/auth/./arkham-build-other/callback"
  , "/api/v1/arkham-builder;x/../verify"
  , "/api/v1/arkham;%2F../decks"
  -- Resolving ".." against an empty segment leaves no credential prefix.
  , "/api/v1/arkham//../decks"
  , "/api/v1/arkham/x//../../games"
  , "/api/v1/arkham/;x/../decks"
  , "/api/auth//../arkham-build-other/callback"
  , "/api/v1//../arkham-builder/verify"
  -- Popping a whole decoded segment leaves no credential prefix.
  , "/api/v1/arkham/q%2Fr/../decks"
  , "/api/auth/q%2Fr/../arkham-build-other/callback"
  -- An irregular path that names no credential route is still logged.
  , "/api/v1/arkham/x/../decks;y"
  -- A U+FFFD or non-ASCII segment that names no exact credential route is
  -- still logged.
  , "/api/v1/arkham/decks%EF%BF%BD"
  , "/api/v1/arkham/arkhamdb-other%EF%BF%BD"
  , "/api/v1/arkham/card/%C3%A9"
  ]

-- Raw paths exactly as a client sent them, which Warp keeps in rawPathInfo
-- (see captureRawLog), so an overlong "%C0%AF" is not re-encoded.
rawProtectedPaths :: [ByteString]
rawProtectedPaths =
  [ "/api/v1/arkham%C0%AFarkhamdb/callback"
  , "/api/v1/arkham%c0%afarkhamdb/link/complete"
  , "/api/auth%C0%AFarkham-build/callback"
  , "/api/v1%C0%AFarkham-build/verify"
  , "/api/v1/arkham/x/%C0%AE%C0%AE/arkhamdb/callback"
  -- 3- and 4-byte overlong '/' and a double-encoded overlong '/'.
  , "/api/v1/arkham%E0%80%AFarkhamdb/callback"
  , "/api/v1/arkham%F0%80%80%AFarkhamdb/link/complete"
  , "/api/v1/arkham%25C0%25AFarkhamdb/callback"
  , "/api/auth%E0%80%AFarkham-build/callback"
  , "/api/auth%F0%80%80%AFarkham-build/callback"
  , "/api/auth%25C0%25AFarkham-build/callback"
  , "/api/v1%E0%80%AFarkham-build/verify"
  , "/api/v1%F0%80%80%AFarkham-build/verify"
  , "/api/v1%25C0%25AFarkham-build/verify"
  ]

rawOrdinaryPaths :: [ByteString]
rawOrdinaryPaths =
  [ "/api/v1/arkham/decks%C0%AF"
  , "/api/v1/arkham/arkhamdb-other%C0%AF"
  ]

-- WaiTest.setPath re-encodes the decoded segments into rawPathInfo, which the
-- logger prints, so a literal ';' in a probe path is logged as "%3B". This
-- relies on ordinary probe paths avoiding any other character setPath re-encodes.
-- It also re-encodes raw paths that are not canonical: an overlong "%C0%AF"
-- decodes to two U+FFFD and is logged as "%EF%BF%BD%EF%BF%BD". Those probes
-- still reach the guard, but captureRawLog keeps the raw bytes as Warp does.
loggedPath :: ByteString -> ByteString
loggedPath = BS.intercalate "%3B" . BS.split 59

probeQuery :: ByteString
probeQuery = "?code=probe-oauth-code&state=probe-oauth-state"

probeRequest :: Wai.Request
probeRequest =
  WaiTest.defaultRequest
    { Wai.requestMethod = "POST"
    , Wai.requestHeaders = [("Content-Type", "application/json")]
    }

captureLog :: Bool -> ByteString -> IO ByteString
captureLog detailed path = captureLogRequest detailed $ WaiTest.setPath probeRequest (path <> probeQuery)

-- Like Warp: rawPathInfo is the path as received and pathInfo is decoded from
-- it with lenient UTF-8. WaiTest.srequest leaves these fields unchanged.
captureRawLog :: Bool -> ByteString -> IO ByteString
captureRawLog detailed raw =
  captureLogRequest detailed probeRequest
    { Wai.rawPathInfo = raw
    , Wai.pathInfo = decodePathSegments raw
    , Wai.rawQueryString = probeQuery
    , Wai.queryString = parseQuery probeQuery
    }

-- Use the production middleware, including its body-capture path, for both
-- formats. No separately implemented exclusion predicate participates here.
captureLogRequest :: Bool -> Wai.Request -> IO ByteString
captureLogRequest detailed probe = do
  directory <- lookupEnv "ARKHAM_TEST_LOG_DIR" >>= maybe getTemporaryDirectory pure
  bracket (openTempFile directory "arkhamdb-variants.log") (removeFile . fst) \(file, handle) -> do
    hClose handle
    bracket (newFileLoggerSet defaultBufSize file) rmLoggerSet \logger -> do
      middleware <- makeLogWare compileTimeAppSettings {appDetailedRequestLogging = detailed} logger
      let request = WaiTest.SRequest probe "{\"completion\":\"probe-completion-secret\"}"
          application req respond = do
            Wai.strictRequestBody req `shouldReturn` WaiTest.simpleRequestBody request
            respond $ Wai.responseLBS status200 [] "ok"
      void $ WaiTest.runSession (WaiTest.srequest request) $ middleware application
      flushLogStr logger
    BS.readFile file
