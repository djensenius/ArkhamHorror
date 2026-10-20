module Arkham.Api.ArkhamDBSpec (spec) where

import Arkham.ArkhamDB
import Arkham.ArkhamDB.Service
import Arkham.Decklist (ArkhamDBDecklist (..))
import Base.Api.Types.Capabilities qualified as Capabilities
import Data.Aeson
import Data.Aeson qualified as Aeson
import Data.Aeson.KeyMap qualified as KeyMap
import Data.Aeson.Types (parseEither)
import Data.Map.Strict qualified as Map
import Data.Text qualified as T
import Data.Time.Clock (UTCTime, addUTCTime)
import Data.UUID qualified as UUID
import Database.Persist (Entity (..))
import Entity.Arkham.ArkhamDBAccount
import Entity.Arkham.Deck
import Entity.User
import Network.HTTP.Client
import Network.HTTP.Types.Status (status409)
import Network.HTTP.Client.Internal (managerProxyInsecure)
import Relude hiding (modify, second, state)
import Settings (ArkhamDBSettings (..), parseArkhamDBSettings)
import System.Directory (doesFileExist)
import Test.Hspec hiding (pending)
import Text.Read (read)

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
  "123" (Just "2026-10-19T11:00:00Z") (Just "1.0")

stubClient :: ArkhamDBClient IO
stubClient = ArkhamDBClient
  { exchangeCode = \code uri -> do
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
  , storedDecks :: Map ArkhamDeckId ArkhamDeck
  , storedLinks :: Map ArkhamDeckId ArkhamDBDeckLink
  }

newStore :: IO (ArkhamDBStore IO, IORef Stored)
newStore = do
  ref <- newIORef $ Stored mempty mempty mempty 1 mempty mempty
  let modify = modifyIORef' ref
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
        case find ((== remoteDeck.remoteDeckId) . arkhamDBDeckLinkRemoteDeckId . snd) $ Map.toList s.storedLinks of
          Just (key, _) -> (s, Entity key <$> Map.lookup key s.storedDecks)
          Nothing ->
            let link = ArkhamDBDeckLink deckId (arkhamDeckUserId deck) remoteDeck.remoteDeckId remoteDeck.dateUpdate remoteDeck.deckVersion False Nothing time time
             in (s {storedDecks = Map.insert deckId deck s.storedDecks, storedLinks = Map.insert deckId link s.storedLinks}, Just $ Entity deckId deck)
      saveSyncedDeck generation key deck remoteDeck time = atomicModifyIORef' ref \s ->
        if Map.lookup (arkhamDeckUserId deck) s.storedGenerations /= Just generation || Map.notMember key s.storedLinks then (s, False) else
        (s
          { storedDecks = Map.insert key deck s.storedDecks
          , storedLinks = Map.adjust (\link -> link {arkhamDBDeckLinkLastSyncedDateUpdate = remoteDeck.dateUpdate, arkhamDBDeckLinkLastSyncedVersion = remoteDeck.deckVersion, arkhamDBDeckLinkUpdatedAt = time}) key s.storedLinks
          }, True)
      dropDeckLink generation uid key = atomicModifyIORef' ref \s ->
        if Map.lookup uid s.storedGenerations /= Just generation then (s, False) else (s {storedLinks = Map.delete key s.storedLinks}, True)
      validateDeck _ = pure ()
  pure (ArkhamDBStore {..}, ref)
 where
  entityValue (Entity _ value) = value
  pendingMatches time uid digest value = arkhamDBLinkStateUserId value == uid && arkhamDBLinkStateCompletionHash value == Just digest && arkhamDBLinkStateExpiresAt value > time && isJust (arkhamDBLinkStateEncryptedPendingTokens value)

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

spec :: Spec
spec = describe "ArkhamDB account linking and private decks" do
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
    for_ ["", "key", "   ", "not-base64-secret!", "MDEyMzQ1Njc4OWFiY2RlZg==", "MDEyMzQ1Njc4OWFiY2RlZjAxMjM0NTY3ODlhYmNkZWZh"] \key -> do
      let result = parse $ object ["arkhamdb-token-key" .= String key]
      result `shouldSatisfy` isLeft
      for_ (leftToMaybe result) \message -> message `shouldBe` "Error in $: ARKHAMDB_TOKEN_KEY must be base64 encoding exactly 32 bytes"
  it "rejects an empty ArkhamDB base URL" do
    let parse = parseEither (withObject "settings" parseArkhamDBSettings)
    parse (object ["arkhamdb-base-url" .= String ""]) `shouldSatisfy` isLeft
    parse (object ["arkhamdb-base-url" .= String "   "]) `shouldSatisfy` isLeft

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
    , "linkedStatus" .= linked
    , "unlinkedStatus" .= unlinked
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
