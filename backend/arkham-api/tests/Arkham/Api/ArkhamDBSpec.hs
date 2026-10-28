{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-orphans -Wno-missing-deriving-strategies #-}

module Arkham.Api.ArkhamDBSpec where

import Api.Handler.Arkham.ArkhamDB (arkhamDBCallback, runArkhamDB)
import Api.Handler.Arkham.Decks (toDeckErrors)
import Application (makeLogWare)
import Arkham.ArkhamDB
import Arkham.ArkhamDB.WriteBack (ArkhamDBStatusResponse (..))
import Arkham.ArkhamDB.Service
import Arkham.Custom.Overlay (DeckOverlay (..))
import Arkham.Decklist (ArkhamDBDecklist (..))
import Base.Api.Types.Capabilities qualified as Capabilities
import Codec.Compression.GZip qualified as GZip
import Control.Concurrent (threadDelay, modifyMVar, newEmptyMVar, newMVar, putMVar, takeMVar, tryPutMVar, withMVar)
import Control.Exception (bracket, throwIO)
import Data.Aeson
import Data.Aeson qualified as Aeson
import Data.Aeson.KeyMap qualified as KeyMap
import Data.Aeson.Types (parseEither)
import Data.ByteString qualified as BS
import Data.ByteString.Char8 qualified as BS8
import Data.ByteString.Lazy qualified as BSL
import Data.List (lookup)
import Data.Map.Strict qualified as Map
import Data.Text qualified as T
import Data.Time.Clock (UTCTime, addUTCTime, getCurrentTime)
import Data.UUID qualified as UUID
import Database.Persist (Entity (..), Update (..), PersistUpdate (Assign), fieldLens)
import Database.PostgreSQL.Simple (SqlError (..), ExecStatus (FatalError))
import Entity.Arkham.ArkhamDBAccount
import Entity.Arkham.Deck
import Entity.User
import Helpers.AppSettings (loadAppSettings)
import Network.HTTP.Client
import Network.HTTP.Client.Internal (managerProxyInsecure)
import Network.HTTP.Types (hLocation, status200, status302, status400, status409, status502)
import Numeric (showHex)
import Network.Wai qualified as Wai
import Network.Wai.Test qualified as WaiTest
import Relude hiding (modify, newEmptyMVar, newMVar, putMVar, second, state, takeMVar, tryPutMVar)
import Settings (AppSettings (..), ArkhamDBSettings (..), compileTimeAppSettings, parseArkhamDBSettings)
import System.Directory (doesFileExist, getTemporaryDirectory, removeFile)
import System.IO (hClose, openTempFile)
import System.IO.Error (userError)
import System.IO.Unsafe (unsafeInterleaveIO)
import System.Log.FastLogger (defaultBufSize, flushLogStr, newFileLoggerSet, rmLoggerSet)
import System.Timeout (timeout)
import Test.Hspec hiding (pending)
import Text.Read (read)
import UnliftIO (withRunInIO)
import UnliftIO.Async (wait, withAsync)
import Yesod.Core qualified as Yesod

-- Dispatch the same callback body as the production route through Yesod/WAI.
-- Only persistence and remote HTTP are replaced; redirects are not simulated.
data CallbackSite = CallbackSite (ArkhamDBStore IO) (ArkhamDBClient IO)
Yesod.mkYesod "CallbackSite" [Yesod.parseRoutes|
/api/v1/arkham/arkhamdb/callback CallbackR GET
/lock-timeout LockTimeoutR GET
/overlay-sync OverlaySyncR POST
/overlay-import OverlayImportR POST
|]

instance Yesod.Yesod CallbackSite where
  makeSessionBackend _ = pure Nothing

getCallbackR :: Yesod.HandlerFor CallbackSite Yesod.Html
getCallbackR = do
  CallbackSite store client <- Yesod.getYesod
  arkhamDBCallback (handlerStore store) (pure $ hoistArkhamDBClient liftIO client) settings

-- Exercise the real handler boundary after PostgreSQL aborts a transaction.
getLockTimeoutR :: Yesod.HandlerFor CallbackSite ()
getLockTimeoutR = runArkhamDB $ liftIO $ throwIO $
  SqlError "55P03" FatalError "private-access-token" "server-secret" "private-refresh-token"

postOverlaySyncR :: Yesod.HandlerFor CallbackSite Value
postOverlaySyncR = do
  CallbackSite store client <- Yesod.getYesod
  toJSON <$> runArkhamDB (syncRemoteDeck (handlerStore store) (hoistArkhamDBClient liftIO client)
    settings now userId deckId (liftIO . forbiddenPublic))

postOverlayImportR :: Yesod.HandlerFor CallbackSite Value
postOverlayImportR = do
  CallbackSite store client <- Yesod.getYesod
  toJSON <$> runArkhamDB (importRemoteDeck (handlerStore store) (hoistArkhamDBClient liftIO client)
    settings now userId "123" (liftIO . forbiddenPublic))

handlerStore :: ArkhamDBStore IO -> ArkhamDBStore (Yesod.HandlerFor CallbackSite)
handlerStore store = ArkhamDBStore
  { withRefreshLock = \uid action -> withRunInIO \run -> withRefreshLock store uid (run . action . handlerStore)
  , ownsPushClaim = liftIO . store.ownsPushClaim
  , saveLinkState = liftIO . store.saveLinkState
  , claimLinkState = \time nonce -> liftIO $ store.claimLinkState time nonce
  , findAccountGeneration = liftIO . store.findAccountGeneration
  , findAccount = liftIO . store.findAccount
  , stagePendingLink = \time nonce digest encrypted -> liftIO $ store.stagePendingLink time nonce digest encrypted
  , findPendingLink = \time uid digest -> liftIO $ store.findPendingLink time uid digest
  , finalizeLink = \time uid digest account -> liftIO $ store.finalizeLink time uid digest account
  , updateAccount = \old updated -> liftIO $ store.updateAccount old updated
  , unlinkAccount = liftIO . store.unlinkAccount
  , findDeckLinks = liftIO . store.findDeckLinks
  , findDeck = liftIO . store.findDeck
  , saveImportedDeck = \generation deck remoteDeck time -> liftIO $ store.saveImportedDeck generation deck remoteDeck time
  , saveSyncedDeck = \generation key deck remoteDeck time -> liftIO $ store.saveSyncedDeck generation key deck remoteDeck time
  , dropDeckLink = \generation uid key -> liftIO $ store.dropDeckLink generation uid key
  , validateDeck = liftIO . store.validateDeck
  }

-- The real logger configuration, including Detailed True and its destination.
-- Reading the JSON body here exercises the logger's body-capture path.
captureRequestLog :: WaiTest.SRequest -> IO ByteString
captureRequestLog request = do
  directory <- getTemporaryDirectory
  bracket (openTempFile directory "arkhamdb-request.log") (removeFile . fst) \(path, handle) -> do
    hClose handle
    bracket (newFileLoggerSet defaultBufSize path) rmLoggerSet \logger -> do
      middleware <- makeLogWare compileTimeAppSettings {appDetailedRequestLogging = True} logger
      let application req respond = do
            body <- Wai.strictRequestBody req
            body `shouldBe` WaiTest.simpleRequestBody request
            respond $ Wai.responseLBS status200 [] "ok"
      _ <- WaiTest.runSession (WaiTest.srequest request) $ middleware application
      flushLogStr logger
    BS.readFile path

now :: UTCTime
now = read "2026-10-19 12:00:00 UTC"

userId, otherUserId :: UserId
userId = UserKey 1
otherUserId = UserKey 2

deckId :: ArkhamDeckId
deckId = ArkhamDeckKey $ UUID.fromWords 0 0 0 23

settings :: ArkhamDBSettings
settings = ArkhamDBSettings "client" "server-secret" "MDEyMzQ1Njc4OWFiY2RlZjAxMjM0NTY3ODlhYmNkZWY=" "https://arkhamdb.example" "https://server.example"

tokens :: ArkhamDBOAuthTokens
tokens = ArkhamDBOAuthTokens "private-access-token" (Just "private-refresh-token") (Just $ addUTCTime 3600 now) (Just "42") (Just "Investigator")

remote :: ArkhamDBRemoteDeck
remote = ArkhamDBRemoteDeck
  (ArkhamDBDecklist mempty mempty "01001" "Roland Banks" Nothing Nothing Nothing (Just "123") (Just "Private deck"))
  "123" (Just "2026-10-19T11:00:00Z") (Just "1.0") Nothing True "Guide" "campaign" (Map.singleton "01006" 1) (Just "too_few_cards")

stubClient :: ArkhamDBClient IO
stubClient = ArkhamDBClient
  { upgradeDeck = \_ _ _ _ -> expectationFailure "Unexpected upgrade" >> pure (Left ArkhamDBNotFound)
  , createDeck = \_ _ -> expectationFailure "Unexpected create" >> pure (Left ArkhamDBNotFound)
  , saveDeck = \_ _ _ -> expectationFailure "Unexpected save" >> pure (Left ArkhamDBNotFound)
  , exchangeCode = \code uri -> do
      code `shouldBe` "authorization-code"
      uri `shouldBe` "https://server.example/api/v1/arkham/arkhamdb/callback"
      pure $ Right tokens
  , refreshTokens = \token -> do
      token `shouldBe` "private-refresh-token"
      pure $ Right tokens {accessToken = "refreshed-token", refreshToken = Nothing}
  , listDecks = \token -> do
      token `shouldBe` "private-access-token"
      pure $ Right [ArkhamDBDeckSummary "123" "Private deck" Nothing (Just "Roland Banks") remote.dateUpdate remote.deckVersion Nothing]
  , loadDeck = \token remoteId -> do
      token `shouldBe` "private-access-token"
      remoteId `shouldBe` "123"
      pure $ Right remote
  }

-- The same service used by the handlers, with an in-memory transactional store
-- and a strict stub transport. No test can reach an ArkhamDB network endpoint.
data Stored = Stored
  { storedStates :: Map Text (Entity ArkhamDBLinkState)
  , storedAccounts :: Map UserId ArkhamDBAccount
  , storedGenerations :: Map UserId ArkhamDBAccountId
  , storedNextGeneration :: Word32
  , storedNextDeck :: Word32
  , storedDecks :: Map ArkhamDeckId ArkhamDeck
  , storedLinks :: Map ArkhamDeckId ArkhamDBDeckLink
  }

newStore :: IO (ArkhamDBStore IO, IORef Stored)
newStore = do
  ref <- newIORef $ Stored mempty mempty mempty 1 23 mempty mempty
  refreshLocks <- newMVar Map.empty
  let withRefreshLock :: UserId -> (ArkhamDBStore IO -> IO a) -> IO a
      withRefreshLock uid action = do
        lock <- modifyMVar refreshLocks \locks -> case Map.lookup uid locks of
          Just lock -> pure (locks, lock)
          Nothing -> do
            lock <- newMVar ()
            pure (Map.insert uid lock locks, lock)
        withMVar lock $ const $ action store
      ownsPushClaim = const $ pure False
      store = ArkhamDBStore {..}
      modify = modifyIORef' ref
      saveLinkState state = modify \s -> s {storedStates = Map.insert (arkhamDBLinkStateNonce state) (Entity (ArkhamDBLinkStateKey UUID.nil) state) s.storedStates}
      claimLinkState time nonce = atomicModifyIORef' ref \s -> case Map.lookup nonce s.storedStates of
        Just (Entity key value) | isNothing (arkhamDBLinkStateClaimedAt value) && arkhamDBLinkStateExpiresAt value > time ->
          let claimed = Entity key value {arkhamDBLinkStateClaimedAt = Just time}
           in (s {storedStates = Map.insert nonce claimed s.storedStates}, Just claimed)
        _ -> (s, Nothing)
      findAccountGeneration uid = Map.lookup uid . storedGenerations <$> readIORef ref
      findAccount uid = Map.lookup uid . storedAccounts <$> readIORef ref
      stagePendingLink time nonce digest encrypted = atomicModifyIORef' ref \s -> case Map.lookup nonce s.storedStates of
        Just (Entity key value) | isJust (arkhamDBLinkStateClaimedAt value) && arkhamDBLinkStateExpiresAt value > time ->
          (s {storedStates = Map.insert nonce (Entity key value {arkhamDBLinkStateCompletionHash = Just digest, arkhamDBLinkStateEncryptedPendingTokens = Just encrypted}) s.storedStates}, True)
        _ -> (s, False)
      findPendingLink time uid digest = do
        s <- readIORef ref
        pure $ find (pendingMatches time uid digest) $ map entityValue $ Map.elems s.storedStates
      finalizeLink time uid digest account = atomicModifyIORef' ref \s -> case find (pendingMatches time uid digest . entityValue) $ Map.elems s.storedStates of
        Just _ -> (s
          { storedAccounts = Map.insert uid account s.storedAccounts
          , storedGenerations = Map.insert uid (ArkhamDBAccountKey $ UUID.fromWords 0 0 0 s.storedNextGeneration) s.storedGenerations
          , storedNextGeneration = s.storedNextGeneration + 1
          , storedStates = Map.filter ((/= uid) . arkhamDBLinkStateUserId . entityValue) s.storedStates
          }, True)
        _ -> (s, False)
      updateAccount old updated = atomicModifyIORef' ref \s -> case Map.lookup (arkhamDBAccountUserId old) s.storedAccounts of
        Just current | arkhamDBAccountEncryptedAccessToken current == arkhamDBAccountEncryptedAccessToken old ->
          (s {storedAccounts = Map.insert (arkhamDBAccountUserId old) updated s.storedAccounts}, True)
        _ -> (s, False)
      unlinkAccount uid = modify \s -> s
        { storedAccounts = Map.delete uid s.storedAccounts
        , storedGenerations = Map.delete uid s.storedGenerations
        , storedStates = Map.filter ((/= uid) . arkhamDBLinkStateUserId . entityValue) s.storedStates
        , storedLinks = Map.filterWithKey (\key _ -> maybe False ((/= uid) . arkhamDeckUserId) $ Map.lookup key s.storedDecks) s.storedLinks
        }
      findDeckLinks uid = do
        s <- readIORef ref
        pure [(key, link) | (key, link) <- Map.toList s.storedLinks, maybe False ((== uid) . arkhamDeckUserId) $ Map.lookup key s.storedDecks]
      findDeck key = do
        s <- readIORef ref
        pure do
          deck <- Map.lookup key s.storedDecks
          pure (deck, Map.lookup key s.storedLinks)
      saveImportedDeck generation deck remoteDeck time = atomicModifyIORef' ref \s ->
        if Map.lookup (arkhamDeckUserId deck) s.storedGenerations /= Just generation then (s, Nothing) else
        case find (\(_, link) -> arkhamDBDeckLinkUserId link == arkhamDeckUserId deck && arkhamDBDeckLinkRemoteDeckId link == remoteDeck.remoteDeckId) $ Map.toList s.storedLinks of
          Just (key, _) -> (s, Entity key <$> Map.lookup key s.storedDecks)
          Nothing ->
            let key = ArkhamDeckKey $ UUID.fromWords 0 0 0 s.storedNextDeck
                link = ArkhamDBDeckLink key (arkhamDeckUserId deck) remoteDeck.remoteDeckId remoteDeck.dateUpdate remoteDeck.deckVersion False Nothing Nothing time time
             in (s {storedNextDeck = s.storedNextDeck + 1, storedDecks = Map.insert key deck s.storedDecks, storedLinks = Map.insert key link s.storedLinks}, Just $ Entity key deck)
      saveSyncedDeck generation key deck remoteDeck time = atomicModifyIORef' ref \s ->
        if Map.lookup (arkhamDeckUserId deck) s.storedGenerations /= Just generation || Map.notMember key s.storedLinks then (s, False) else
        (s
          { storedDecks = Map.adjust (applySyncedDeckUpdates key deck) key s.storedDecks
          , storedLinks = Map.adjust (\link -> link {arkhamDBDeckLinkLastSyncedDateUpdate = remoteDeck.dateUpdate, arkhamDBDeckLinkLastSyncedVersion = remoteDeck.deckVersion, arkhamDBDeckLinkUpdatedAt = time}) key s.storedLinks
          }, True)
      dropDeckLink generation uid key = atomicModifyIORef' ref \s ->
        if Map.lookup uid s.storedGenerations /= Just generation then (s, False) else (s {storedLinks = Map.delete key s.storedLinks}, True)
      validateDeck _ = pure ()
  pure (store, ref)
 where
  entityValue (Entity _ value) = value
  pendingMatches time uid digest value = arkhamDBLinkStateUserId value == uid && arkhamDBLinkStateCompletionHash value == Just digest && arkhamDBLinkStateExpiresAt value > time && isJust (arkhamDBLinkStateEncryptedPendingTokens value)

-- Exercise the actual SQL field assignments, not whole-row replacement.
applySyncedDeckUpdates :: ArkhamDeckId -> ArkhamDeck -> ArkhamDeck -> ArkhamDeck
applySyncedDeckUpdates key updated previous = entityVal $ foldl' apply (Entity key previous) $ syncedDeckUpdates updated
 where
  apply entity (Update field value Assign) = runIdentity $ fieldLens field (const $ Identity value) entity
  apply _ _ = error "Only field assignments are used by linked sync"

linkAccount :: ArkhamDBStore IO -> IO ()
linkAccount store = do
  runExceptT (startLink store settings now userId "nonce") >>= (`shouldSatisfy` isRight)
  runExceptT (completeLink store stubClient settings now "nonce" "authorization-code" "completion-secret") `shouldReturn` Right ()
  runExceptT (finalizePendingLink store settings now userId "completion-secret") `shouldReturn` Right ()

forbiddenPublic :: ArkhamDeckId -> IO (Entity ArkhamDeck)
forbiddenPublic _ = expectationFailure "unexpected public fallback" >> fail "public fallback"

expectCode :: Text -> Either ArkhamDBError a -> Expectation
expectCode code = \case
  Left err -> arkhamDBErrorCode err `shouldBe` code
  Right _ -> expectationFailure $ "Expected ArkhamDB error " <> toString code

-- Deliberately omit Content-Length and deliver wire chunks separately. This is
-- the real http-client parser/decompressor, not a stubbed ArkhamDBClient.
chunkedClient :: ByteString -> BSL.ByteString -> IO (ArkhamDBClient IO)
chunkedClient headers body = do
  let frame chunk = BS8.pack (showHex (BS.length chunk) "") <> "\r\n" <> chunk <> "\r\n"
      wireChunks = ("HTTP/1.1 200 OK\r\nTransfer-Encoding: chunked\r\nConnection: close\r\n" <> headers <> "\r\n")
        : map frame (BSL.toChunks body) <> ["0\r\n\r\n"]
  incoming <- newIORef wireChunks
  manager <- newManager defaultManagerSettings
    { managerRawConnection = pure $ \_ _ _ -> makeConnection
        (atomicModifyIORef' incoming \case [] -> ([], mempty); chunk : rest -> (rest, chunk))
        (const $ pure ()) (pure ())
    , managerProxyInsecure = noProxy
    }
  pure $ httpArkhamDBClient manager "http://arkhamdb.stub" "client" "secret"

paddedResponse :: Int -> BSL.ByteString -> BSL.ByteString
paddedResponse size body = body <> BSL.replicate (fromIntegral size - BSL.length body) 32

spec :: Spec
spec = describe "ArkhamDB account linking and private decks" do
  for_ ["/overlay-sync", "/overlay-import"] \path ->
    it ("returns actual HTTP 400 for ambiguous overlay adoption through " <> BS8.unpack path) do
      (store, ref) <- newStore
      linkAccount store
      runExceptT (importRemoteDeck store stubClient settings now userId "123" forbiddenPublic) >>= (`shouldSatisfy` isRight)
      let overlay = DeckOverlay Nothing mempty (Map.singleton "01007" 1) mempty
          incoming = remote {deck = remote.deck {meta = Just "{\"ahn_push\":\"legacy-push\"}"}}
          client = stubClient {loadDeck = \_ _ -> pure $ Right incoming}
      modifyIORef' ref \s -> s {storedDecks = Map.adjust (\d -> d {arkhamDeckOverlay = Just overlay}) deckId s.storedDecks}
      before <- Map.lookup deckId . storedDecks <$> readIORef ref
      application <- Yesod.toWaiAppPlain $ CallbackSite store client
      response <- WaiTest.runSession
        (WaiTest.request $ (WaiTest.setPath WaiTest.defaultRequest path) {Wai.requestMethod = "POST"}) application
      WaiTest.simpleStatus response `shouldBe` status400
      eitherDecode @Value (WaiTest.simpleBody response) `shouldBe` Right (object
        ["code" .= String "validationFailed", "error" .= String "Campaign overlay provenance is unavailable; refusing ambiguous adoption"])
      after <- Map.lookup deckId . storedDecks <$> readIORef ref
      fmap arkhamDeckList after `shouldBe` fmap arkhamDeckList before
      fmap arkhamDeckOverlay after `shouldBe` Just (Just overlay)

  for_ [("upgrade", "PUT /api/oauth2/deck/upgrade/123", "xp=5"), ("new", "POST /api/oauth2/deck/new", "investigator=01001"), ("save", "PUT /api/oauth2/deck/save/124", "slots=%7B%2201006%22%3A1%7D")] \(operation, methodPath, fieldName) ->
    it ("uses ArkhamDB's documented method and form for " <> operation) do
      incoming <- newIORef "HTTP/1.1 200 OK\r\nContent-Length: 26\r\nConnection: close\r\n\r\n{\"success\":true,\"msg\":124}"
      outgoing <- newIORef mempty
      manager <- newManager defaultManagerSettings
        { managerRawConnection = pure $ \_ _ _ -> makeConnection
            (atomicModifyIORef' incoming (\bytes -> (mempty, bytes)))
            (\bytes -> modifyIORef' outgoing (<> bytes)) (pure ())
        , managerProxyInsecure = noProxy
        }
      let client = httpArkhamDBClient manager "http://arkhamdb.stub" "client" "secret"
      result <- case operation of
        "upgrade" -> client.upgradeDeck "token" "123" 5 (Just "{}")
        "new" -> client.createDeck "token" remote.deck
        _ -> client.saveDeck "token" "124" remote {deck = remote.deck {slots = Map.singleton "01006" 1, sideSlots = Map.singleton "01007" 1}}
      result `shouldBe` Right "124"
      request <- readIORef outgoing
      request `shouldSatisfy` BS8.isInfixOf methodPath
      request `shouldSatisfy` BS8.isInfixOf fieldName
      when (operation == "save") do
        for_ ["description_md=Guide", "tags=campaign", "ignored=%7B%2201006%22%3A1%7D", "problem=too_few_cards"] \field ->
          request `shouldSatisfy` BS8.isInfixOf field

  it "loads the remote guide tags ignored slots and problem for lossless save forwarding" do
    let raw = object
          [ "id" .= (123 :: Int), "investigator_code" .= ("01001" :: Text), "slots" .= object []
          , "description_md" .= ("Campaign guide" :: Text), "tags" .= ("solo campaign" :: Text)
          , "ignoreDeckLimitSlots" .= object ["01006" .= (2 :: Int)], "problem" .= ("too_many_cards" :: Text)]
    remote <- either (fail . show) pure $ case fromJSON raw of Error err -> Left err; Success value -> Right (value :: ArkhamDBRemoteDeck)
    remote.descriptionMd `shouldBe` "Campaign guide"
    remote.tags `shouldBe` "solo campaign"
    remote.ignoreDeckLimitSlots `shouldBe` Map.singleton "01006" 2
    remote.problem `shouldBe` Just "too_many_cards"

  it "sanitizes a rejected HTTP write envelope and enforces the decoded write-body cap" do
    rejected <- chunkedClient "" "{\"success\":false,\"msg\":124,\"error\":\"private-access-token\"}"
    rejected.upgradeDeck "token" "123" 5 Nothing `shouldReturn` Left (ArkhamDBRemoteError "Write rejected")
    oversized <- chunkedClient "" $ paddedResponse (1024 * 1024 + 1) "{\"success\":true,\"msg\":124}"
    oversized.saveDeck "token" "124" remote `shouldReturn` Left (ArkhamDBRemoteError "Response too large")

  it "sanitizes PostgreSQL lock timeout as retryable remoteUnavailable" do
    (store, _) <- newStore
    application <- Yesod.toWaiAppPlain $ CallbackSite store stubClient
    response <- WaiTest.runSession (WaiTest.request $ WaiTest.setPath WaiTest.defaultRequest "/lock-timeout") application
    WaiTest.simpleStatus response `shouldBe` status502
    eitherDecode @Value (WaiTest.simpleBody response) `shouldBe` Right (object
      ["code" .= ("remoteUnavailable" :: Text), "error" .= ("ArkhamDB request failed" :: Text)])

  it "bounds oversized chunked token responses without Content-Length" do
    client <- chunkedClient "" $ paddedResponse (1024 * 1024 + 1) "{\"access_token\":\"private-access-token\"}"
    client.refreshTokens "refresh-secret" `shouldReturn` Left (ArkhamDBRemoteError "Response too large")

  it "bounds oversized chunked API list responses without Content-Length" do
    client <- chunkedClient "" $ paddedResponse (1024 * 1024 + 1) "[]"
    client.listDecks "bearer-secret" `shouldReturn` Left (ArkhamDBRemoteError "Response too large")

  it "bounds oversized chunked API deck responses without Content-Length" do
    client <- chunkedClient "" $ paddedResponse (1024 * 1024 + 1) $ encode remote.deck
    client.loadDeck "bearer-secret" "123" `shouldReturn` Left (ArkhamDBRemoteError "Response too large")

  it "bounds decompressed chunked token responses rather than compressed wire bytes" do
    let body = GZip.compress $ paddedResponse (1024 * 1024 + 1) "{\"access_token\":\"private-access-token\"}"
    BSL.length body `shouldSatisfy` (< 1024 * 1024)
    client <- chunkedClient "Content-Encoding: gzip\r\n" body
    client.refreshTokens "refresh-secret" `shouldReturn` Left (ArkhamDBRemoteError "Response too large")

  it "accepts token and API responses exactly at the decoded byte cap" do
    tokenClient <- chunkedClient "" $ paddedResponse (1024 * 1024) "{\"access_token\":\"private-access-token\"}"
    result <- tokenClient.refreshTokens "refresh-secret"
    fmap (.accessToken) result `shouldBe` Right "private-access-token"
    apiClient <- chunkedClient "" $ paddedResponse (1024 * 1024) "[]"
    apiClient.listDecks "bearer-secret" `shouldReturn` Right []

  -- Delayed thunks make evaluation boundaries deterministic without depending
  -- on CPU speed or allocating attacker-sized integers. Use the same timed
  -- request runner as the real HTTP client with a short test-only deadline.
  it "times out lazy OAuth classification inside the request deadline" do
    classification <- unsafeInterleaveIO $ threadDelay 1000000 >> pure (Left ArkhamDBUnauthorized :: Either ArkhamDBClientError ArkhamDBOAuthTokens)
    runArkhamDBRequest 20000 (pure classification) `shouldReturn` Left (ArkhamDBRemoteError "Transport timed out")

  it "times out lazy malformed token decoding inside the request deadline" do
    body <- unsafeInterleaveIO $ threadDelay 1000000 >> pure "{malformed-private-body"
    runArkhamDBRequest 20000 (pure $ parseTokenResponse now body) `shouldReturn` Left (ArkhamDBRemoteError "Transport timed out")

  it "times out lazy decoded token fields inside the request deadline" do
    name <- unsafeInterleaveIO $ threadDelay 1000000 >> pure "private-account-name"
    runArkhamDBRequest 20000 (pure $ Right tokens {remoteUserName = Just name}) `shouldReturn` Left (ArkhamDBRemoteError "Transport timed out")

  it "keeps the completion secret out of the real callback 302 body" do
    (store, ref) <- newStore
    time <- getCurrentTime
    _ <- runExceptT $ startLink store settings time userId "nonce"
    application <- Yesod.toWaiAppPlain $ CallbackSite store stubClient
    response <- WaiTest.runSession (WaiTest.request $ WaiTest.setPath WaiTest.defaultRequest
      "/api/v1/arkham/arkhamdb/callback?state=nonce&code=authorization-code") application
    WaiTest.simpleStatus response `shouldBe` status302
    location <- maybe (fail "missing Location") pure $ lookup hLocation $ WaiTest.simpleHeaders response
    let prefix = "arkhamhorror://arkhamdb-link?result=pending&completion="
    location `shouldSatisfy` BS.isPrefixOf prefix
    let secret = BS.drop (BS.length prefix) location
    BS.length secret `shouldBe` 43
    secret `shouldSatisfy` BS.all (\c -> c >= 65 && c <= 90 || c >= 97 && c <= 122 || c >= 48 && c <= 57 || c == 45 || c == 95)
    toStrict (WaiTest.simpleBody response) `shouldSatisfy` (not . BS.isInfixOf secret)
    stored <- readIORef ref
    Map.null stored.storedAccounts `shouldBe` True
    fmap (arkhamDBLinkStateCompletionHash . entityValue) (Map.lookup "nonce" stored.storedStates)
      `shouldBe` Just (Just $ completionHash $ decodeUtf8 secret)

  it "ignores web=1 and always returns the fixed app-scheme callback redirect" do
    (store, _) <- newStore
    time <- getCurrentTime
    _ <- runExceptT $ startLink store settings time userId "nonce"
    application <- Yesod.toWaiAppPlain $ CallbackSite store stubClient
    response <- WaiTest.runSession (WaiTest.request $ WaiTest.setPath WaiTest.defaultRequest
      "/api/v1/arkham/arkhamdb/callback?state=nonce&code=authorization-code&web=1") application
    WaiTest.simpleStatus response `shouldBe` status302
    fromMaybe "" (lookup hLocation $ WaiTest.simpleHeaders response)
      `shouldSatisfy` BS.isPrefixOf "arkhamhorror://arkhamdb-link?result=pending&completion="

  it "uses only fixed callback errors and never exchanges error callbacks through Yesod" do
    for_ [("access_denied", "access_denied"), ("https://evil.example/?secret=reflected", "link_failed")] \(oauthError, reason) -> do
      (store, _) <- newStore
      time <- getCurrentTime
      _ <- runExceptT $ startLink store settings time userId "nonce"
      let client = stubClient {exchangeCode = \_ _ -> expectationFailure "error callback exchanged OAuth code" >> pure (Right tokens)}
      application <- Yesod.toWaiAppPlain $ CallbackSite store client
      response <- WaiTest.runSession (WaiTest.request $ WaiTest.setPath WaiTest.defaultRequest $
        "/api/v1/arkham/arkhamdb/callback?state=nonce&code=must-not-exchange&error=" <> oauthError) application
      WaiTest.simpleStatus response `shouldBe` status302
      lookup hLocation (WaiTest.simpleHeaders response) `shouldBe` Just ("arkhamhorror://arkhamdb-link?result=error&reason=" <> reason)
      toStrict (WaiTest.simpleBody response) `shouldSatisfy` (not . BS.isInfixOf "reflected")

  it "does not log the completion JSON or callback OAuth query with Detailed True" do
    let complete = WaiTest.SRequest
          (WaiTest.setPath WaiTest.defaultRequest {Wai.requestMethod = "POST", Wai.requestHeaders = [("Content-Type", "application/json")]} "/api/v1/arkham/arkhamdb/link/complete")
          "{\"completion\":\"probe-completion-secret\"}"
        callback = WaiTest.SRequest
          (WaiTest.setPath WaiTest.defaultRequest "/api/v1/arkham/arkhamdb/callback?code=probe-oauth-code&state=probe-oauth-state") ""
    outputs <- forM [("completion", complete), ("callback", callback)] \(label, request) -> do
      output <- captureRequestLog request
      BS8.putStrLn $ "Detailed True " <> label <> " probe: " <> output
      pure output
    for_ outputs \output ->
      for_ ["probe-completion-secret", "probe-oauth-code", "probe-oauth-state"] \secret ->
        output `shouldSatisfy` (not . BS.isInfixOf secret)
    -- An exclusion must not accidentally switch off unrelated request logging.
    control <- captureRequestLog $ complete {WaiTest.simpleRequest = WaiTest.setPath (WaiTest.simpleRequest complete) "/api/v1/arkham/arkhamdb-other"}
    control `shouldSatisfy` BS.isInfixOf "arkhamdb-other"
    control `shouldSatisfy` BS.isInfixOf "probe-completion-secret"

  it "rejects a wrong nonce without exchanging a code" do
    (store, _) <- newStore
    _ <- runExceptT $ startLink store settings now userId "nonce"
    result <- runExceptT $ completeLink store stubClient settings now "wrong" "must-not-exchange" "completion-secret"
    expectCode "oauthCallbackInvalidState" result

  it "rejects an expired nonce without exchanging a code" do
    (store, _) <- newStore
    _ <- runExceptT $ startLink store settings (addUTCTime (-601) now) userId "nonce"
    let expired = Entity (ArkhamDBLinkStateKey UUID.nil) $ ArkhamDBLinkState "nonce" userId now (addUTCTime (-600) now) Nothing Nothing Nothing
    validateLinkState now "nonce" (Just expired) `shouldSatisfy` isLeft
    result <- runExceptT $ completeLink store stubClient settings now "nonce" "must-not-exchange" "completion-secret"
    expectCode "oauthCallbackInvalidState" result

  it "rejects a reused nonce without exchanging a second code" do
    (store, _) <- newStore
    linkAccount store
    result <- runExceptT $ completeLink store stubClient settings now "nonce" "must-not-exchange" "completion-secret"
    expectCode "oauthCallbackInvalidState" result

  it "rejects a mismatched stored nonce as well as an absent nonce" do
    let state = Entity (ArkhamDBLinkStateKey UUID.nil) $ ArkhamDBLinkState "stored" userId (addUTCTime 600 now) now Nothing Nothing Nothing
    validateLinkState now "different" (Just state) `shouldSatisfy` isLeft

  it "rejects a transferred completion from a different app user without linking either account" do
    (store, ref) <- newStore
    _ <- runExceptT $ startLink store settings now userId "nonce"
    runExceptT (completeLink store stubClient settings now "nonce" "authorization-code" "completion-secret") `shouldReturn` Right ()
    runExceptT (finalizePendingLink store settings now otherUserId "completion-secret") >>= expectCode "oauthCompletionInvalid"
    s <- readIORef ref
    Map.null s.storedAccounts `shouldBe` True
    let pending = entityValue <$> Map.lookup "nonce" s.storedStates
    fmap arkhamDBLinkStateCompletionHash pending `shouldBe` Just (Just $ completionHash "completion-secret")
    fmap arkhamDBLinkStateEncryptedPendingTokens pending `shouldSatisfy` \case
      Just (Just encrypted) -> not $ T.isInfixOf "private-access-token" encrypted
      _ -> False
    runExceptT (finalizePendingLink store settings now userId "completion-secret") `shouldReturn` Right ()

  it "rejects wrong completion secrets" do
    (store, _) <- newStore
    _ <- runExceptT $ startLink store settings now userId "nonce"
    runExceptT (completeLink store stubClient settings now "nonce" "authorization-code" "completion-secret") `shouldReturn` Right ()
    runExceptT (finalizePendingLink store settings now userId "wrong") >>= expectCode "oauthCompletionInvalid"
    runExceptT (finalizePendingLink store settings now userId "completion-secret") `shouldReturn` Right ()

  it "rejects replayed completion secrets" do
    (store, _) <- newStore
    linkAccount store
    runExceptT (finalizePendingLink store settings now userId "completion-secret") >>= expectCode "oauthCompletionInvalid"

  it "rejects an expired pending completion" do
    (store, ref) <- newStore
    _ <- runExceptT $ startLink store settings now userId "nonce"
    runExceptT (completeLink store stubClient settings now "nonce" "authorization-code" "completion-secret") `shouldReturn` Right ()
    runExceptT (finalizePendingLink store settings (addUTCTime 600 now) userId "completion-secret") >>= expectCode "oauthCompletionInvalid"
    s <- readIORef ref
    Map.null s.storedAccounts `shouldBe` True

  it "rechecks pending completion expiry atomically at finalization" do
    (store, ref) <- newStore
    _ <- runExceptT $ startLink store settings now userId "nonce"
    runExceptT (completeLink store stubClient settings now "nonce" "authorization-code" "completion-secret") `shouldReturn` Right ()
    let racing = store {findPendingLink = \time uid digest -> do
          pending <- store.findPendingLink time uid digest
          modifyIORef' ref \s -> s {storedStates = fmap (\(Entity key value) -> Entity key value {arkhamDBLinkStateExpiresAt = now}) s.storedStates}
          pure pending}
    runExceptT (finalizePendingLink racing settings now userId "completion-secret") >>= expectCode "oauthCompletionInvalid"
    s <- readIORef ref
    Map.null s.storedAccounts `shouldBe` True

  it "returns only the fixed app scheme and fixed callback error reasons" do
    callbackLocation (Right "secret") `shouldBe` "arkhamhorror://arkhamdb-link?result=pending&completion=secret"
    callbackLocation (Left "access_denied") `shouldBe` "arkhamhorror://arkhamdb-link?result=error&reason=access_denied"
    callbackLocation (Left "https://evil.example") `shouldBe` "arkhamhorror://arkhamdb-link?result=error&reason=link_failed"

  it "stores encrypted access and refresh tokens, with authenticated randomized ciphertext" do
    (store, ref) <- newStore
    linkAccount store
    s <- readIORef ref
    let account = fromMaybe (error "missing account") $ Map.lookup userId s.storedAccounts
        access = arkhamDBAccountEncryptedAccessToken account
        refresh = fromMaybe (error "missing refresh") $ arkhamDBAccountEncryptedRefreshToken account
    access `shouldSatisfy` (not . T.isInfixOf "private-access-token")
    refresh `shouldSatisfy` (not . T.isInfixOf "private-refresh-token")
    decryptToken settings.arkhamDBTokenKey (TokenCiphertext access) `shouldBe` Right "private-access-token"
    decryptToken settings.arkhamDBTokenKey (TokenCiphertext refresh) `shouldBe` Right "private-refresh-token"
    second <- encryptToken settings.arkhamDBTokenKey "private-access-token"
    second `shouldNotBe` TokenCiphertext access
    decryptToken "wrong-key" (TokenCiphertext access) `shouldSatisfy` isLeft
    decryptToken settings.arkhamDBTokenKey (TokenCiphertext $ access <> "A") `shouldSatisfy` isLeft

  it "refreshes an expired access token before listing and retains an unrotated refresh token" do
    (store, ref) <- newStore
    linkAccount store
    modifyIORef' ref \s -> s {storedAccounts = Map.adjust (\a -> a {arkhamDBAccountExpiresAt = Just now}) userId s.storedAccounts}
    received <- newIORef Nothing
    let client = stubClient {listDecks = \token -> writeIORef received (Just token) $> Right []}
    runExceptT (listRemoteDecks store client settings now userId) `shouldReturn` Right []
    readIORef received `shouldReturn` Just "refreshed-token"
    account <- store.findAccount userId >>= maybe (fail "missing account") pure
    decryptToken settings.arkhamDBTokenKey . TokenCiphertext <$> arkhamDBAccountEncryptedRefreshToken account
      `shouldBe` Just (Right "private-refresh-token")

  it "uses the concurrent refresh winner instead of reporting notLinked or tokenExpired" do
    for_ [False, True] \revokedLoser -> do
      (store, ref) <- newStore
      linkAccount store
      modifyIORef' ref \s -> s {storedAccounts = Map.adjust (\a -> a {arkhamDBAccountExpiresAt = Just now}) userId s.storedAccounts}
      let client = stubClient {refreshTokens = \_ -> do
            winner <- encryptedAccount settings now userId tokens {accessToken = "winner-token"}
            modifyIORef' ref \s -> s {storedAccounts = Map.insert userId winner s.storedAccounts}
            pure $ if revokedLoser then Left ArkhamDBUnauthorized else Right tokens {accessToken = "loser-token"}}
      runExceptT (accessTokenFor store client settings now userId) `shouldReturn` Right "winner-token"

  it "serializes refreshes while the remote winner is waiting to persist" do
    (store, ref) <- newStore
    linkAccount store
    modifyIORef' ref \s -> s {storedAccounts = Map.adjust (\a -> a {arkhamDBAccountExpiresAt = Just now}) userId s.storedAccounts}
    beforePersist <- newEmptyMVar
    allowPersist <- newEmptyMVar
    loserRead <- newEmptyMVar
    calls <- newIORef (0 :: Int)
    let client = stubClient {refreshTokens = \_ -> do
          call <- atomicModifyIORef' calls (\n -> (n + 1, n))
          pure $ if call == 0 then Right tokens {accessToken = "winner-token", refreshToken = Just "rotated-refresh"} else Left ArkhamDBUnauthorized}
        delayPersistence locked = locked {updateAccount = \old updated -> do
          putMVar beforePersist ()
          takeMVar allowPersist
          locked.updateAccount old updated}
        winnerStore = store
          { withRefreshLock = \uid action -> withRefreshLock store uid (action . delayPersistence)
          , updateAccount = (delayPersistence store).updateAccount
          }
        loserStore = store {findAccount = \uid -> do
          account <- store.findAccount uid
          void $ tryPutMVar loserRead ()
          pure account}
    withAsync (runExceptT $ accessTokenFor winnerStore client settings now userId) \winner -> do
      timeout 2000000 (takeMVar beforePersist) `shouldReturn` Just ()
      withAsync (runExceptT $ accessTokenFor loserStore client settings now userId) \loser -> do
        timeout 2000000 (takeMVar loserRead) `shouldReturn` Just ()
        -- On the old service there is no lock callback, but it completes the
        -- rejected stale refresh immediately, before the winner can persist.
        timeout 100000 (wait loser) `shouldReturn` Nothing
        putMVar allowPersist ()
        timeout 2000000 (wait winner) `shouldReturn` Just (Right "winner-token")
        timeout 2000000 (wait loser) `shouldReturn` Just (Right "winner-token")
    readIORef calls `shouldReturn` 1
    account <- store.findAccount userId >>= maybe (fail "missing account") pure
    fmap (decryptToken settings.arkhamDBTokenKey . TokenCiphertext) (arkhamDBAccountEncryptedRefreshToken account)
      `shouldBe` Just (Right "rotated-refresh")

  it "retries a rejected bearer token once with a refresh" do
    (store, _) <- newStore
    linkAccount store
    calls <- newIORef ([] :: [Text])
    let client = stubClient {listDecks = \token -> do
          modifyIORef' calls (<> [token])
          pure $ if token == "private-access-token" then Left ArkhamDBUnauthorized else Right []}
    runExceptT (listRemoteDecks store client settings now userId) `shouldReturn` Right []
    readIORef calls `shouldReturn` ["private-access-token", "refreshed-token"]

  it "maps refresh HTTP 400 invalid_grant and 401 to tokenExpired using the real HTTP client" do
    for_ ["400 Bad Request", "401 Unauthorized"] \status -> do
      (store, ref) <- newStore
      linkAccount store
      modifyIORef' ref \s -> s {storedAccounts = Map.adjust (\a -> a {arkhamDBAccountExpiresAt = Just now}) userId s.storedAccounts}
      let body = "{\"error\":\"invalid_grant\"}"
          response = "HTTP/1.1 " <> status <> "\r\nContent-Length: 25\r\nConnection: close\r\n\r\n" <> body
      incoming <- newIORef response
      manager <- newManager defaultManagerSettings
        { managerRawConnection = pure $ \_ _ _ -> makeConnection (atomicModifyIORef' incoming (\bytes -> (mempty, bytes))) (const $ pure ()) (pure ())
        , managerProxyInsecure = noProxy
        }
      let client = httpArkhamDBClient manager "http://arkhamdb.stub" "client" "secret"
      runExceptT (listRemoteDecks store client settings now userId) >>= expectCode "tokenExpired"

  for_ ["invalid_client", "invalid_request", "malformed"] \errorName ->
    it ("keeps HTTP 400 " <> BS8.unpack errorName <> " a remote error, not tokenExpired") do
      (store, ref) <- newStore
      linkAccount store
      modifyIORef' ref \s -> s {storedAccounts = Map.adjust (\a -> a {arkhamDBAccountExpiresAt = Just now}) userId s.storedAccounts}
      let body = if errorName == "malformed" then "{not-json" else "{\"error\":\"" <> errorName <> "\"}"
      incoming <- newIORef $ "HTTP/1.1 400 Bad Request\r\nContent-Length: " <> BS8.pack (show $ BS.length body) <> "\r\nConnection: close\r\n\r\n" <> body
      manager <- newManager defaultManagerSettings
        { managerRawConnection = pure $ \_ _ _ -> makeConnection (atomicModifyIORef' incoming (\bytes -> (mempty, bytes))) (const $ pure ()) (pure ())
        , managerProxyInsecure = noProxy
        }
      let client = httpArkhamDBClient manager "http://arkhamdb.stub" "client" "secret"
      runExceptT (accessTokenFor store client settings now userId) >>= expectCode "remoteUnavailable"

  it "keeps token endpoint failures remote errors without deleting mappings or public fallback" do
    for_ ["302 Found", "403 Forbidden", "404 Not Found", "500 Internal Server Error"] \status -> do
      (store, ref) <- newStore
      linkAccount store
      _ <- runExceptT $ importRemoteDeck store stubClient settings now userId "123" forbiddenPublic
      modifyIORef' ref \s -> s {storedAccounts = Map.adjust (\a -> a {arkhamDBAccountExpiresAt = Just now}) userId s.storedAccounts}
      incoming <- newIORef $ "HTTP/1.1 " <> status <> "\r\nContent-Length: 0\r\nConnection: close\r\n\r\n"
      manager <- newManager defaultManagerSettings
        { managerRawConnection = pure $ \_ _ _ -> makeConnection (atomicModifyIORef' incoming (\bytes -> (mempty, bytes))) (const $ pure ()) (pure ())
        , managerProxyInsecure = noProxy
        }
      let client = httpArkhamDBClient manager "http://arkhamdb.stub" "client" "secret"
      runExceptT (syncRemoteDeck store client settings now userId deckId forbiddenPublic) >>= expectCode "remoteUnavailable"
      stored <- readIORef ref
      Map.member deckId stored.storedLinks `shouldBe` True

  it "requires relinking when expired credentials have no refresh token" do
    (store, ref) <- newStore
    linkAccount store
    modifyIORef' ref \s -> s {storedAccounts = Map.adjust (\a -> a {arkhamDBAccountExpiresAt = Just now, arkhamDBAccountEncryptedRefreshToken = Nothing}) userId s.storedAccounts}
    runExceptT (listRemoteDecks store stubClient settings now userId) >>= expectCode "tokenExpired"

  it "unlinks credentials, pending states and remote mappings but keeps local decks" do
    (store, ref) <- newStore
    linkAccount store
    _ <- runExceptT $ importRemoteDeck store stubClient settings now userId "123" forbiddenPublic
    _ <- runExceptT $ startLink store settings now userId "pending"
    runExceptT (completeLink store stubClient settings now "pending" "authorization-code" "pending-secret") `shouldReturn` Right ()
    runExceptT (unlinkLinkedAccount store userId) `shouldReturn` Right ()
    s <- readIORef ref
    Map.null s.storedAccounts `shouldBe` True
    Map.null s.storedStates `shouldBe` True
    Map.null s.storedLinks `shouldBe` True
    Map.member deckId s.storedDecks `shouldBe` True
    runExceptT (listRemoteDecks store stubClient settings now userId) >>= expectCode "notLinked"
    runExceptT (finalizePendingLink store settings now userId "pending-secret") >>= expectCode "oauthCompletionInvalid"

  it "does not recreate credentials when unlink races an in-flight refresh" do
    (store, ref) <- newStore
    linkAccount store
    modifyIORef' ref \s -> s {storedAccounts = Map.adjust (\a -> a {arkhamDBAccountExpiresAt = Just now}) userId s.storedAccounts}
    let client = stubClient {refreshTokens = \_ -> do
          runExceptT (unlinkLinkedAccount store userId) `shouldReturn` Right ()
          pure $ Right tokens {accessToken = "refreshed-token"}}
    runExceptT (accessTokenFor store client settings now userId) >>= expectCode "notLinked"
    s <- readIORef ref
    Map.null s.storedAccounts `shouldBe` True

  it "does not recreate credentials when unlink races an in-flight callback" do
    (store, ref) <- newStore
    _ <- runExceptT $ startLink store settings now userId "nonce"
    let client = stubClient {exchangeCode = \_ _ -> do
          runExceptT (unlinkLinkedAccount store userId) `shouldReturn` Right ()
          pure $ Right tokens}
    runExceptT (completeLink store client settings now "nonce" "authorization-code" "completion-secret") >>= expectCode "oauthCallbackInvalidState"
    s <- readIORef ref
    Map.null s.storedAccounts `shouldBe` True
    Map.null s.storedStates `shouldBe` True

  it "lists private remote decks and attaches only this user's local mapping" do
    (store, _) <- newStore
    linkAccount store
    _ <- runExceptT $ importRemoteDeck store stubClient settings now userId "123" forbiddenPublic
    result <- runExceptT $ listRemoteDecks store stubClient settings now userId
    fmap (map (.linkedLocalDeckId)) result `shouldBe` Right [Just deckId]
    runExceptT (listRemoteDecks store stubClient settings now otherUserId) >>= expectCode "notLinked"

  it "imports a private OAuth deck and persists date_update and dotted version markers" do
    (store, ref) <- newStore
    linkAccount store
    result <- runExceptT $ importRemoteDeck store stubClient settings now userId "123" forbiddenPublic
    fmap (arkhamDeckName . entityValue) result `shouldBe` Right "Private deck"
    s <- readIORef ref
    let mapping = fromMaybe (error "missing link") $ Map.lookup deckId s.storedLinks
    arkhamDBDeckLinkRemoteDeckId mapping `shouldBe` "123"
    arkhamDBDeckLinkLastSyncedDateUpdate mapping `shouldBe` remote.dateUpdate
    arkhamDBDeckLinkLastSyncedVersion mapping `shouldBe` Just "1.0"
    arkhamDBDeckLinkWriteBack mapping `shouldBe` False
    (arkhamDeckUrl <$> Map.lookup deckId s.storedDecks) `shouldBe` Just (Just "https://arkhamdb.example/api/public/deck/123")

  it "syncs linked decks through OAuth without invoking the public fallback" do
    (store, ref) <- newStore
    linkAccount store
    _ <- runExceptT $ importRemoteDeck store stubClient settings now userId "123" forbiddenPublic
    let changed = remote {deck = remote.deck {decklist_name = Just "Changed private deck"}, dateUpdate = Just "2026-10-19T12:00:00Z", deckVersion = Just "2.0"}
        client = stubClient {loadDeck = \token remoteId -> do
          token `shouldBe` "private-access-token"
          remoteId `shouldBe` "123"
          pure $ Right changed}
        forbidden _ = expectationFailure "public fallback used for a private linked deck" >> fail "public fallback"
    result <- runExceptT $ syncRemoteDeck store client settings now userId deckId forbidden
    fmap (arkhamDeckName . entityValue) result `shouldBe` Right "Changed private deck"
    fmap (url . arkhamDeckList . entityValue) result `shouldBe` Right (Just "https://arkhamdb.example/api/public/deck/123")
    s <- readIORef ref
    (arkhamDBDeckLinkLastSyncedDateUpdate <$> Map.lookup deckId s.storedLinks) `shouldBe` Just changed.dateUpdate
    (arkhamDBDeckLinkLastSyncedVersion <$> Map.lookup deckId s.storedLinks) `shouldBe` Just (Just "2.0")
    runExceptT (syncRemoteDeck store client settings now otherUserId deckId forbidden) >>= expectCode "notFound"

  for_ [False, True] \viaImport -> do
    let entryPoint = if viaImport then "import" else "sync"
        update store client = if viaImport
          then importRemoteDeck store client settings now userId "123" forbiddenPublic
          else syncRemoteDeck store client settings now userId deckId forbiddenPublic
        validating store = store {validateDeck = \decklist ->
          unless (null $ toDeckErrors decklist) $ throwIO $ userError "unimplemented effective deck"}

    it ("rejects a newly activated unsupported overlay swap through " <> entryPoint) do
      (store, ref) <- newStore
      linkAccount store
      _ <- runExceptT $ importRemoteDeck store stubClient settings now userId "123" forbiddenPublic
      let overlay = DeckOverlay Nothing (Map.singleton "01016" "unimplemented-card") mempty mempty
      modifyIORef' ref \s -> s {storedDecks = Map.adjust (\deck -> deck {arkhamDeckOverlay = Just overlay}) deckId s.storedDecks}
      (existingDeck, _) <- store.findDeck deckId >>= maybe (fail "missing deck") pure
      toDeckErrors (arkhamDeckPlayList existingDeck) `shouldBe` []
      let changed = remote {deck = remote.deck {slots = Map.singleton "01016" 1}, deckVersion = Just "2.0"}
          client = stubClient {loadDeck = \_ _ -> pure $ Right changed}
      toDeckErrors changed.deck `shouldBe` []
      runExceptT (update (validating store) client) `shouldThrow` anyIOException
      stored <- readIORef ref
      fmap arkhamDeckList (Map.lookup deckId stored.storedDecks) `shouldBe` Just (arkhamDeckList existingDeck)
      fmap arkhamDBDeckLinkLastSyncedVersion (Map.lookup deckId stored.storedLinks) `shouldBe` Just remote.deckVersion

    it ("accepts an unsupported remote card removed by the overlay through " <> entryPoint) do
      (store, ref) <- newStore
      linkAccount store
      _ <- runExceptT $ importRemoteDeck store stubClient settings now userId "123" forbiddenPublic
      let overlay = DeckOverlay Nothing mempty mempty (Map.singleton "unimplemented-card" 1)
      modifyIORef' ref \s -> s {storedDecks = Map.adjust (\deck -> deck {arkhamDeckOverlay = Just overlay}) deckId s.storedDecks}
      let changed = remote {deck = remote.deck {slots = Map.singleton "unimplemented-card" 1}, deckVersion = Just "2.0"}
          client = stubClient {loadDeck = \_ _ -> pure $ Right changed}
      toDeckErrors changed.deck `shouldSatisfy` (not . null)
      result <- runExceptT (update (validating store) client) >>= either (fail . show) pure
      slots (arkhamDeckList $ entityValue result) `shouldBe` Map.singleton "unimplemented-card" 1
      slots (arkhamDeckPlayList $ entityValue result) `shouldBe` mempty
      stored <- readIORef ref
      fmap arkhamDeckOverlay (Map.lookup deckId stored.storedDecks) `shouldBe` Just (Just overlay)
      fmap arkhamDBDeckLinkLastSyncedVersion (Map.lookup deckId stored.storedLinks) `shouldBe` Just changed.deckVersion

  it "retains mappings across relink and drops inaccessible mappings before public fallback" do
    for_ [False, True] \importCheck -> do
      (store, ref) <- newStore
      linkAccount store
      _ <- runExceptT $ importRemoteDeck store stubClient settings now userId "123" forbiddenPublic
      linkAccount store
      s <- readIORef ref
      Map.member deckId s.storedLinks `shouldBe` True
      let client = stubClient {loadDeck = \_ _ -> pure $ Left ArkhamDBNotFound}
          public key = do
            current <- readIORef ref
            Map.member key current.storedLinks `shouldBe` False
            deck <- maybe (fail "missing deck") pure $ Map.lookup key current.storedDecks
            arkhamDeckUrl deck `shouldBe` Just "https://arkhamdb.example/api/public/deck/123"
            pure $ Entity key deck
      result <- runExceptT $ if importCheck then importRemoteDeck store client settings now userId "123" public else syncRemoteDeck store client settings now userId deckId public
      result `shouldSatisfy` isRight

  it "maps remote HTTP 403 and 404 to inaccessible decks" do
    for_ ["403 Forbidden", "404 Not Found"] \status -> do
      incoming <- newIORef $ "HTTP/1.1 " <> status <> "\r\nContent-Length: 0\r\nConnection: close\r\n\r\n"
      manager <- newManager defaultManagerSettings
        { managerRawConnection = pure $ \_ _ _ -> makeConnection (atomicModifyIORef' incoming (\bytes -> (mempty, bytes))) (const $ pure ()) (pure ())
        , managerProxyInsecure = noProxy
        }
      (httpArkhamDBClient manager "http://arkhamdb.stub" "client" "secret").loadDeck "token" "123" `shouldReturn` Left ArkhamDBNotFound

  it "keeps imports of the same remote deck separate and owned by each local user" do
    (store, ref) <- newStore
    linkAccount store
    _ <- runExceptT $ startLink store settings now otherUserId "other-nonce"
    runExceptT (completeLink store stubClient settings now "other-nonce" "authorization-code" "other-secret") `shouldReturn` Right ()
    runExceptT (finalizePendingLink store settings now otherUserId "other-secret") `shouldReturn` Right ()
    firstImport <- runExceptT (importRemoteDeck store stubClient settings now userId "123" forbiddenPublic) >>= either (fail . show) pure
    otherImport <- runExceptT (importRemoteDeck store stubClient settings now otherUserId "123" forbiddenPublic) >>= either (fail . show) pure
    entityKey firstImport `shouldNotBe` entityKey otherImport
    arkhamDeckUserId (entityValue firstImport) `shouldBe` userId
    arkhamDeckUserId (entityValue otherImport) `shouldBe` otherUserId
    for_ [(userId, firstImport), (otherUserId, otherImport)] \(uid, imported) -> do
      repeated <- runExceptT $ importRemoteDeck store stubClient settings now uid "123" forbiddenPublic
      fmap entityKey repeated `shouldBe` Right (entityKey imported)
      links <- store.findDeckLinks uid
      map fst links `shouldBe` [entityKey imported]
      map (arkhamDBDeckLinkUserId . snd) links `shouldBe` [uid]
    runExceptT (syncRemoteDeck store stubClient settings now userId (entityKey otherImport) forbiddenPublic) >>= expectCode "notFound"
    runExceptT (syncRemoteDeck store stubClient settings now otherUserId (entityKey firstImport) forbiddenPublic) >>= expectCode "notFound"
    stored <- readIORef ref
    Map.size stored.storedDecks `shouldBe` 2
    Map.size stored.storedLinks `shouldBe` 2

  it "returns an existing imported deck without duplicates" do
    (store, ref) <- newStore
    linkAccount store
    firstImport <- runExceptT $ importRemoteDeck store stubClient settings now userId "123" forbiddenPublic
    second <- runExceptT $ importRemoteDeck store stubClient settings now userId "123" forbiddenPublic
    fmap entityKey firstImport `shouldBe` fmap entityKey second
    s <- readIORef ref
    Map.size s.storedDecks `shouldBe` 1
    Map.size s.storedLinks `shouldBe` 1

  for_ [False, True] \relink -> do
    it ("rejects import persistence after in-flight " <> if relink then "relink" else "unlink") do
      (store, ref) <- newStore
      linkAccount store
      let client = stubClient {loadDeck = \_ _ -> do
            if relink then linkAccount store else runExceptT (unlinkLinkedAccount store userId) >>= (`shouldBe` Right ())
            pure $ Right remote}
      runExceptT (importRemoteDeck store client settings now userId "123" forbiddenPublic) >>= expectCode "notLinked"
      s <- readIORef ref
      Map.null s.storedDecks `shouldBe` True
      Map.null s.storedLinks `shouldBe` True

    it ("rejects sync persistence after in-flight " <> if relink then "relink" else "unlink") do
      (store, ref) <- newStore
      linkAccount store
      _ <- runExceptT $ importRemoteDeck store stubClient settings now userId "123" forbiddenPublic
      let client = stubClient {loadDeck = \_ _ -> do
            if relink then linkAccount store else runExceptT (unlinkLinkedAccount store userId) >>= (`shouldBe` Right ())
            pure $ Right remote {deck = remote.deck {decklist_name = Just "Must not save"}}}
      runExceptT (syncRemoteDeck store client settings now userId deckId forbiddenPublic) >>= expectCode "notLinked"
      s <- readIORef ref
      fmap arkhamDeckName (Map.lookup deckId s.storedDecks) `shouldBe` Just "Private deck"
      Map.null s.storedLinks `shouldBe` not relink

  it "retains public sync for a deck without an OAuth mapping" do
    (store, ref) <- newStore
    let deck = ArkhamDeck userId (Just "https://arkhamdb.example/api/public/deck/123") "Public deck" "Roland Banks" remote.deck Nothing Nothing
    modifyIORef' ref \s -> s {storedDecks = Map.singleton deckId deck}
    called <- newIORef False
    let public key = do
          key `shouldBe` deckId
          writeIORef called True
          pure $ Entity key deck
    result <- runExceptT $ syncRemoteDeck store stubClient settings now userId deckId public
    fmap (arkhamDeckName . entityValue) result `shouldBe` Right "Public deck"
    readIORef called `shouldReturn` True

  it "does not save an import or a sync when ArkhamDB fails" do
    (store, ref) <- newStore
    linkAccount store
    let client = stubClient {loadDeck = \_ _ -> pure $ Left $ ArkhamDBRemoteError "stub outage"}
    runExceptT (importRemoteDeck store client settings now userId "123" forbiddenPublic) >>= expectCode "remoteUnavailable"
    s <- readIORef ref
    Map.null s.storedDecks `shouldBe` True
    Map.null s.storedLinks `shouldBe` True

  it "disables the integration and capability unless all required settings exist" do
    runExceptT (configuredSettings Nothing :: ArkhamDBAction IO ArkhamDBSettings) >>= expectCode "disabled"
    let fields = ["arkhamdb-client-id" .= String "id", "arkhamdb-client-secret" .= String "secret", "arkhamdb-token-key" .= String settings.arkhamDBTokenKey, "arkhamdb-public-server-base" .= String "https://server.example"]
        parse value = parseEither (withObject "settings" parseArkhamDBSettings) value
    parse (object []) `shouldBe` Right Nothing
    for_ [0 .. 3] \index -> parse (object $ take index fields <> drop (index + 1) fields) `shouldBe` Right Nothing
    configured <- either fail pure $ parse $ object fields
    isJust configured `shouldBe` True
    (Capabilities.serverCapabilities Nothing (isJust configured)).capabilities `shouldContain` [arkhamDBAccountLinkingCapability]
    (Capabilities.serverCapabilities Nothing False).capabilities `shouldNotContain` [arkhamDBAccountLinkingCapability]

  it "rejects invalid encryption keys without exposing secrets" do
    let parse = parseEither (withObject "settings" parseArkhamDBSettings)
    for_ ["key", "   ", "not-base64-secret!", "MDEyMzQ1Njc4OWFiY2RlZg==", "MDEyMzQ1Njc4OWFiY2RlZjAxMjM0NTY3ODlhYmNkZWZh"] \key -> do
      let result = parse $ object ["arkhamdb-token-key" .= String key]
      result `shouldSatisfy` isLeft
      for_ (leftToMaybe result) \message -> message `shouldBe` "Error in $: ARKHAMDB_TOKEN_KEY must be base64 encoding exactly 32 bytes"

  it "treats an empty token key and base URL as absent in settings JSON" do
    let parse = parseEither (withObject "settings" parseArkhamDBSettings)
    parse (object ["arkhamdb-token-key" .= String ""]) `shouldBe` Right Nothing
    parse (object ["arkhamdb-base-url" .= String ""]) `shouldBe` Right Nothing
    parse (object ["arkhamdb-base-url" .= String "   "]) `shouldSatisfy` isLeft

  it "rejects non-empty invalid environment token keys" do
    for_ ["invalid-nonempty-key", "false", "[]"] \key ->
      isLeft (loadAppSettings [("ARKHAMDB_TOKEN_KEY", key)]) `shouldBe` True

  it "treats empty ArkhamDB environment values as unset without failing startup" do
    let environment =
          [ ("ARKHAMDB_CLIENT_ID", "client")
          , ("ARKHAMDB_CLIENT_SECRET", "secret")
          , ("ARKHAMDB_TOKEN_KEY", settings.arkhamDBTokenKey)
          , ("ARKHAMDB_PUBLIC_SERVER_BASE", "https://server.example")
          , ("ARKHAMDB_BASE_URL", "https://arkhamdb.example")
          ]
    for_ environment \(name, _) -> do
      let overridden = map (\(key, value) -> (key, if key == name then "" else value)) environment
      parsed <- either (fail . toString) pure $ loadAppSettings overridden
      if name == "ARKHAMDB_BASE_URL"
        then fmap arkhamDBBaseUrl parsed.appArkhamDB `shouldBe` Just "https://arkhamdb.com"
        else parsed.appArkhamDB `shouldBe` Nothing
    allEmpty <- either (fail . toString) pure $ loadAppSettings $ map (\(key, _) -> (key, "")) environment
    allEmpty.appArkhamDB `shouldBe` Nothing

  it "disables account linking for a whitespace-only public server base in environment settings" do
    for_ ["   ", "\t", " \t\r\n "] \publicServerBase -> do
      parsed <- either (fail . toString) pure $ loadAppSettings
        [ ("ARKHAMDB_CLIENT_ID", "client")
        , ("ARKHAMDB_CLIENT_SECRET", "secret")
        , ("ARKHAMDB_TOKEN_KEY", settings.arkhamDBTokenKey)
        , ("ARKHAMDB_PUBLIC_SERVER_BASE", publicServerBase)
        ]
      parsed.appArkhamDB `shouldBe` Nothing
      (Capabilities.serverCapabilities Nothing (isJust parsed.appArkhamDB)).capabilities
        `shouldNotContain` [arkhamDBAccountLinkingCapability]

  it "returns HTTP 409 rather than expiring the app session for ArkhamDB auth states" do
    for_ ["notLinked", "tokenExpired"] \code -> do
      let err = ArkhamDBError code "relink required"
      arkhamDBErrorStatus err `shouldBe` status409
      field (toJSON err) "code" `shouldBe` String code

  it "matches the enabled capability fixture without changing the compatibility floor" do
    fixture <- loadNamedFixture "capabilities-arkhamdb.json"
    let response = toJSON $ Capabilities.serverCapabilities Nothing True
        normalized = case response of
          Object value -> Object $ KeyMap.insert "campaignCatalog" (field fixture "campaignCatalog") value
          value -> value
    normalized `shouldBe` fixture

  it "stores and reports a missing OAuth account identity as null, not an invented identifier" do
    parsed <- either (fail . show) pure $ parseTokenResponse now "{\"access_token\":\"access\",\"expires_in\":3600}"
    parsed.remoteUserId `shouldBe` Nothing
    parsed.remoteUserName `shouldBe` Nothing
    (store, ref) <- newStore
    _ <- runExceptT $ startLink store settings now userId "nonce"
    let client = stubClient {exchangeCode = \_ _ -> pure $ Right parsed}
    runExceptT (completeLink store client settings now "nonce" "authorization-code" "completion-secret") `shouldReturn` Right ()
    runExceptT (finalizePendingLink store settings now userId "completion-secret") `shouldReturn` Right ()
    status <- runExceptT (linkStatus store userId) >>= either (fail . show) pure
    status.linked `shouldBe` True
    field (toJSON status) "remoteAccountId" `shouldBe` Null
    field (toJSON status) "remoteAccountName" `shouldBe` Null
    stored <- readIORef ref
    fmap arkhamDBAccountRemoteUserId (Map.lookup userId stored.storedAccounts) `shouldBe` Just Nothing

  it "decodes string and numeric remote versions without losing dotted string values" do
    for_ [(String "1.0", "1.0"), (Number 2, "2")] \(version, expected) -> do
      let raw = object ["id" .= (123 :: Int), "name" .= String "Private deck", "slots" .= object [], "investigator_code" .= String "01001", "investigator_name" .= String "Roland Banks", "version" .= version]
      fmap (.deckVersion) (fromJSON raw :: Result ArkhamDBRemoteDeck) `shouldBe` Success (Just expected)
      fmap (.remoteDeckId) (fromJSON raw :: Result ArkhamDBRemoteDeck) `shouldBe` Success "123"
      fmap (.deck.decklist_id) (fromJSON raw :: Result ArkhamDBRemoteDeck) `shouldBe` Success (Just "123")
      fmap (.version) (fromJSON raw :: Result ArkhamDBDeckSummary) `shouldBe` Success (Just expected)

  it "rejects a remote import identifier containing path or query components" do
    (fromJSON (object ["deckId" .= String "123/../../other?token=bad"]) :: Result ArkhamDBImportRequest) `shouldSatisfy` \case
      Error _ -> True
      _ -> False

  it "matches the ArkhamDB fixtures generated by the real service and encoders" do
    fixture <- loadFixture
    generated <- arkhamDBFixture
    generated `shouldBe` fixture

 where
  entityValue (Entity _ value) = value

-- The fixture is constructed from real link/list/import/sync responses, not a
-- public-deck fixture copied into an OAuth envelope.
arkhamDBFixture :: IO Value
arkhamDBFixture = do
  (store, _) <- newStore
  start <- runExceptT (startLink store settings now userId "nonce") >>= either (fail . show) pure
  unlinked <- runExceptT (linkStatus store userId) >>= either (fail . show) pure
  runExceptT (completeLink store stubClient settings now "nonce" "authorization-code" "completion-secret") `shouldReturn` Right ()
  runExceptT (finalizePendingLink store settings now userId "completion-secret") `shouldReturn` Right ()
  linked <- runExceptT (linkStatus store userId) >>= either (fail . show) pure
  summaries <- runExceptT (listRemoteDecks store stubClient settings now userId) >>= either (fail . show) pure
  imported <- runExceptT (importRemoteDeck store stubClient settings now userId "123" forbiddenPublic) >>= either (fail . show) pure
  synced <- runExceptT (syncRemoteDeck store stubClient settings now userId deckId forbiddenPublic) >>= either (fail . show) pure
  pure $ object
    [ "linkStart" .= start
    , "linkedStatus" .= ArkhamDBStatusResponse linked []
    , "unlinkedStatus" .= ArkhamDBStatusResponse unlinked []
    , "decks" .= summaries
    , "completionRequest" .= ArkhamDBLinkCompleteRequest "completion-secret"
    , "importRequest" .= object ["deckId" .= String "123"]
    , "importedDeck" .= imported
    , "syncedDeck" .= synced
    , "error" .= ArkhamDBError "notLinked" "ArkhamDB account is not linked"
    , "tokenExpiredError" .= ArkhamDBError "tokenExpired" "ArkhamDB rejected the token; relink required"
    ]

field :: Value -> Aeson.Key -> Value
field (Object value) key = fromMaybe (error "missing fixture field") $ KeyMap.lookup key value
field _ _ = error "fixture is not an object"

loadFixture :: IO Value
loadFixture = loadNamedFixture "arkhamdb.json"

loadNamedFixture :: FilePath -> IO Value
loadNamedFixture name = do
  let candidates = [prefix <> name | prefix <- ["../contracts/fixtures/", "../../contracts/fixtures/", "contracts/fixtures/"]]
  paths <- filterM doesFileExist candidates
  path <- maybe (fail "ArkhamDB contract fixture not found") pure $ viaNonEmpty head paths
  eitherDecodeFileStrict' path >>= either fail pure
