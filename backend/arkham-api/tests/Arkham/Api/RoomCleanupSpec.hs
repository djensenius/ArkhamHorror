{- | Proves the post-commit room-cleanup diagnostics AND retry architecture in
'Api.Arkham.Helpers' -- the seam every deletion path
('Api.Handler.Arkham.Games.Shared.forceDeleteRoom', and through it
'deleteRoom'\/'deleteEventRoom') and the ordinary "last subscriber left"
path ('releaseRoomIfEmpty'\/'releaseGameRoomIfEmpty'\/'releaseEventRoomIfEmpty')
both delegate to -- directly against constructed 'Room's, with no live
server or 'App' required.

Before the retry fix, a synchronous Redis unsubscribe failure (whether
after a committed deletion, or an ordinary empty-room release) was either
swallowed entirely by 'tryRedis_' (the ordinary release path, which also
unconditionally erased the room's own map entry regardless of success --
turning a retryable leak into a permanently unretryable one, with no
retry path for either game or event rooms) or reported but never
genuinely retried (the forced deletion path, which also logged the raw
'SomeException' text, risking leaking arbitrary connection details into
application logs).

These tests exercise, directly:

* 'attemptRoomTeardown' -- the exact per-room decision both paths make
  under the rooms 'MVar' -- against a 'Room' built via 'newRoom', with its
  'roomUnsubscribe' action stubbed to either succeed or throw
  synchronously;
* 'retryRoomTeardown' -- the exact closure registered for durable retry --
  proving it never tears down an unrelated, newer room instance created
  under the same key since the original failure, never tears down a room
  that has regained a live subscriber under an ordinary (non-forced)
  eligibility guard, and DOES tear down regardless of occupancy under a
  forced eligibility guard;
* the full production sequence end-to-end: a first teardown attempt
  fails and is durably registered via 'registerRoomCleanupRetry', the
  room stays retained and reachable, a first retry (standing in for
  another failed disconnect) still fails and the room stays retained
  again, and only a later retry (standing in for
  'roomHeartbeat''s periodic 'Api.Arkham.Lifecycle.drainOwnedCleanup'
  call) finally succeeds and removes it -- proven for BOTH a game-keyed
  and an event-keyed rooms map, since the retry owner is domain-agnostic;
* genuine cross-thread cancellation: a real 'Control.Exception.throwTo'
  'Control.Exception.ThreadKilled' delivered to a thread genuinely
  blocked inside 'attemptRoomTeardown''s own unsubscribe action
  propagates unchanged, rather than being converted into any
  'RoomCleanupOutcome' value (a same-thread 'throwIO' would prove
  nothing here: 'UnliftIO.Exception.tryAny' catches that regardless of
  whether the exception type is normally "asynchronous", so only a real
  cross-thread delivery while genuinely blocked distinguishes correct
  propagation from an accidental catch);
* 'classifyRoomCleanupFailure' -- proving a 'SomeException' carrying an
  arbitrary, secret-looking message is reduced to one of two closed,
  non-sensitive categories whose own 'Show' output never contains that
  original text;
* that 'registerRoomCleanupRetry' genuinely hands a failure off to the
  real, process-global 'PendingCleanupOwner.globalPendingCleanupOwner' --
  the exact singleton 'Api.Arkham.Lifecycle.drainOwnedCleanup' (and so
  'roomHeartbeat') drains -- by polling the returned receipt directly via
  'PendingCleanupOwner.attemptCleanupReceipt', which (unlike a
  whole-owner drain) only ever touches this one specific entry and so is
  safe regardless of whatever else this shared, process-global owner may
  independently hold; one additional test also drains through the exact
  'Api.Arkham.Lifecycle.drainOwnedCleanup' entry point itself (the one
  'roomHeartbeat' genuinely calls in production) to prove that whole path
  end-to-end, and is wrapped in 'sequential' -- like
  'AwsEnvSupervisorLifecycleSpec', which establishes this precedent --
  since (unlike a single-receipt poll) draining the WHOLE owner could
  otherwise race a concurrently-running module's own use of the same
  global singleton.

Database deletion commit semantics remain entirely orthogonal to all of
this -- none of these functions ever touch 'runDB' or any 'SqlPersistT'
action, only the in-memory rooms map, the room's own 'TVar (IO ())', and
(for the registration tests) the separate, already independently-tested
'PendingCleanupOwner' -- so no test here needs, or could meaningfully
exercise, a database at all.
-}
module Arkham.Api.RoomCleanupSpec (spec) where

import Api.Arkham.Helpers (
  RoomCleanupFailureCategory (..),
  RoomCleanupOutcome (..),
  attemptRoomTeardown,
  classifyRoomCleanupFailure,
  registerRoomCleanupRetry,
  retryRoomTeardown,
 )
import Api.Arkham.Lifecycle (drainOwnedCleanup)
import Control.Concurrent (forkIO, threadDelay)
import Control.Exception (AsyncException (..), ErrorCall (..))
import Control.Exception qualified as Exception
import Data.Map.Strict qualified as Map
import Data.UUID qualified as UUID
import Entity.Arkham.Epic qualified as Epic
import Entity.Arkham.Game qualified as GameEntity
import Foundation (Room (..), newRoom, roomClientCount, subscribeToRoom)
import PendingCleanupOwner (ReceiptOutcome (..), attemptCleanupReceipt, globalPendingCleanupOwner)
import TestImport

-- | 'Either SomeException ()' has no 'Eq' instance ('SomeException' can't
-- reasonably have one), so 'shouldBe' can't be used against
-- 'retryRoomTeardown''s own result type directly; assert the success case
-- by pattern match instead.
shouldBeRightUnit :: HasCallStack => Either SomeException () -> Expectation
shouldBeRightUnit = \case
  Right () -> pure ()
  Left e -> expectationFailure $ "expected Right (), got Left: " <> show e

-- | 'ReceiptOutcome' likewise has no 'Eq' instance (it carries a
-- 'SomeException' in its 'ReceiptFailed' case); assert the specific
-- success case by pattern match instead.
shouldBeReceiptSucceeded :: HasCallStack => ReceiptOutcome -> Expectation
shouldBeReceiptSucceeded = \case
  ReceiptSucceeded -> pure ()
  other -> expectationFailure $ "expected ReceiptSucceeded, got: " <> show other

-- | A fixed, arbitrary Redis channel: no test here ever inspects it, only
-- the room's own 'roomUnsubscribe' behavior and the map it lives in.
fixtureChannel :: ByteString
fixtureChannel = "arkham-room-cleanup-spec"

-- | A freshly constructed room whose unsubscribe action always succeeds.
fixtureCleanRoom :: IO Room
fixtureCleanRoom = newRoom fixtureChannel

-- | A freshly constructed room whose unsubscribe action always throws the
-- given exception synchronously (never asynchronously -- 'throwIO' inside a
-- plain 'IO' action run by 'tryAny' is exactly the synchronous case
-- 'attemptRoomTeardown' is documented to catch).
fixtureFailingRoom :: SomeException -> IO Room
fixtureFailingRoom e = do
  r <- newRoom fixtureChannel
  atomically $ writeTVar r.roomUnsubscribe (throwIO e)
  pure r

-- | A room whose unsubscribe action fails exactly the first @n@ times it
-- is invoked (via a shared counter), then succeeds on every call after
-- that -- standing in for a transient Redis connection failure that a
-- later retry genuinely resolves.
fixtureFlakyRoom :: Int -> SomeException -> IO Room
fixtureFlakyRoom failuresRemaining e = do
  counter <- newIORef failuresRemaining
  r <- newRoom fixtureChannel
  atomically
    $ writeTVar
      r.roomUnsubscribe
      do
        n <- atomicModifyIORef' counter (\k -> (max 0 (k - 1), k))
        when (n > 0) (throwIO e)
  pure r

-- | Fixture game/event ids, following the 'UUID.fromWords 0 0 0 tag'
-- convention established by 'EpicGameLockOrderSpec.fixtureGameId'.
fixtureGameId :: Int -> GameEntity.ArkhamGameId
fixtureGameId tag = GameEntity.ArkhamGameKey $ UUID.fromWords 0 0 0 (fromIntegral tag)

fixtureEventId :: Int -> Epic.ArkhamEpicEventId
fixtureEventId tag = Epic.ArkhamEpicEventKey $ UUID.fromWords 0 0 0 (fromIntegral tag)

spec :: Spec
spec = do
  describe "attemptRoomTeardown (per-room teardown decision, production-used)" do
    it "an absent key reports RoomCleanupAbsent and leaves the map completely unchanged" do
      let rooms = Map.empty :: Map Int Room
      (rooms', outcome) <- attemptRoomTeardown (1 :: Int) rooms
      case outcome of
        RoomCleanupAbsent -> pure ()
        other -> expectationFailure $ "expected RoomCleanupAbsent, got: " <> show other
      Map.keys rooms' `shouldBe` Map.keys rooms

    it "a present room whose unsubscribe succeeds reports RoomCleanupClean and is removed from the map" do
      room <- fixtureCleanRoom
      let rooms = Map.fromList [(1 :: Int, room)]
      (rooms', outcome) <- attemptRoomTeardown 1 rooms
      case outcome of
        RoomCleanupClean -> pure ()
        other -> expectationFailure $ "expected RoomCleanupClean, got: " <> show other
      Map.member 1 rooms' `shouldBe` False

    it "a present room whose unsubscribe throws SYNCHRONOUSLY reports RoomCleanupUnsubscribeFailed carrying that exception, and is left IN the map (retryable)" do
      let boom = ErrorCall "unsubscribe boom"
      room <- fixtureFailingRoom (toException boom)
      let rooms = Map.fromList [(1 :: Int, room)]
      (rooms', outcome) <- attemptRoomTeardown 1 rooms
      case outcome of
        RoomCleanupUnsubscribeFailed e -> case fromException e of
          Just (ErrorCall msg) -> msg `shouldBe` "unsubscribe boom"
          Nothing -> expectationFailure $ "expected the stubbed ErrorCall to survive unchanged, got: " <> show e
        other -> expectationFailure $ "expected RoomCleanupUnsubscribeFailed, got: " <> show other
      -- Deliberately left IN the map: dropping it would discard the only
      -- reachable handle to its still-registered callback.
      Map.member 1 rooms' `shouldBe` True

    it "a failed teardown for one key never touches an unrelated room present under a different key" do
      let boom = ErrorCall "unsubscribe boom"
      failing <- fixtureFailingRoom (toException boom)
      other <- fixtureCleanRoom
      let rooms = Map.fromList [(1 :: Int, failing), (2, other)]
      (rooms', outcome) <- attemptRoomTeardown 1 rooms
      case outcome of
        RoomCleanupUnsubscribeFailed _ -> pure ()
        unexpected -> expectationFailure $ "expected RoomCleanupUnsubscribeFailed, got: " <> show unexpected
      Map.member 1 rooms' `shouldBe` True
      Map.member 2 rooms' `shouldBe` True

    it "a successful teardown for one key never touches an unrelated room present under a different key" do
      clean <- fixtureCleanRoom
      let boom = ErrorCall "unrelated room stays untouched"
      untouched <- fixtureFailingRoom (toException boom)
      let rooms = Map.fromList [(1 :: Int, clean), (2, untouched)]
      (rooms', outcome) <- attemptRoomTeardown 1 rooms
      case outcome of
        RoomCleanupClean -> pure ()
        unexpected -> expectationFailure $ "expected RoomCleanupClean, got: " <> show unexpected
      Map.member 1 rooms' `shouldBe` False
      Map.member 2 rooms' `shouldBe` True

    it "propagates a genuine asynchronous cancellation delivered while genuinely blocked, instead of converting it into any outcome" do
      blockedSignal <- newEmptyMVar
      release <- newEmptyMVar
      room <- newRoom fixtureChannel
      atomically
        $ writeTVar
          room.roomUnsubscribe
          do
            putMVar blockedSignal ()
            takeMVar release
      let rooms = Map.fromList [(1 :: Int, room)]
      workerDone <- newEmptyMVar
      -- Deliberately 'Exception.try' (the plain, catches-everything
      -- version), NOT the ambient async-preserving 'try' also in scope
      -- here (the same one 'attemptRoomTeardown' itself uses internally
      -- via 'UE.tryAny'): this outer boundary exists purely to OBSERVE
      -- whether the cancellation escaped 'attemptRoomTeardown' unchanged,
      -- which requires actually catching it here rather than also
      -- re-throwing it out of this worker thread uncaught (which would
      -- just silently kill the thread and hang this test on
      -- 'takeMVar workerDone' forever, proving nothing).
      workerTid <- forkIO $ putMVar workerDone =<< Exception.try @SomeException (void $ attemptRoomTeardown 1 rooms)
      takeMVar blockedSignal
      throwTo workerTid ThreadKilled
      result <- takeMVar workerDone
      case result of
        Left e -> case fromException e of
          Just ThreadKilled -> pure ()
          _ -> expectationFailure $ "expected ThreadKilled to propagate unchanged, got: " <> show e
        Right () -> expectationFailure "expected the cancellation to propagate, not be converted into a RoomCleanupClean/Failed outcome"

  describe "retryRoomTeardown (the exact closure durably registered for retry)" do
    it "is a no-op success (Right ()) when nothing is tracked under this key at all" do
      roomsVar <- newMVar (Map.empty :: Map Int Room)
      expected <- fixtureCleanRoom
      result <- retryRoomTeardown (const True) roomsVar 1 expected
      shouldBeRightUnit result

    it "still reports Left and leaves the room retained when the unsubscribe still fails" do
      let boom = ErrorCall "still failing"
      room <- fixtureFailingRoom (toException boom)
      roomsVar <- newMVar (Map.fromList [(1 :: Int, room)])
      result <- retryRoomTeardown (const True) roomsVar 1 room
      case result of
        Left e -> case fromException e of
          Just (ErrorCall msg) -> msg `shouldBe` "still failing"
          Nothing -> expectationFailure $ "expected the stubbed ErrorCall, got: " <> show e
        Right () -> expectationFailure "expected Left, the unsubscribe stub always fails"
      rooms' <- readMVar roomsVar
      Map.member 1 rooms' `shouldBe` True

    it "succeeds and removes the room once the unsubscribe eventually succeeds" do
      room <- fixtureCleanRoom
      roomsVar <- newMVar (Map.fromList [(1 :: Int, room)])
      result <- retryRoomTeardown (const True) roomsVar 1 room
      shouldBeRightUnit result
      rooms' <- readMVar roomsVar
      Map.member 1 rooms' `shouldBe` False

    it "is a no-op success and NEVER invokes the current room's unsubscribe when the room under this key has since been replaced by a DIFFERENT instance" do
      staleFailingRoom <- fixtureFailingRoom (toException (ErrorCall "should never run"))
      invoked <- newIORef False
      newerRoom <- newRoom fixtureChannel
      atomically $ writeTVar newerRoom.roomUnsubscribe (writeIORef invoked True)
      roomsVar <- newMVar (Map.fromList [(1 :: Int, newerRoom)])
      result <- retryRoomTeardown (const True) roomsVar 1 staleFailingRoom
      shouldBeRightUnit result
      readIORef invoked `shouldReturn` False
      rooms' <- readMVar roomsVar
      case Map.lookup 1 rooms' of
        Just r | roomUnsubscribe r == roomUnsubscribe newerRoom -> pure ()
        _ -> expectationFailure "expected the map to still hold the exact newer room instance, untouched"

    it "is a no-op success under an ORDINARY (non-forced) eligibility guard when the room has regained a live subscriber, and never invokes its unsubscribe" do
      invoked <- newIORef False
      room <- newRoom fixtureChannel
      atomically $ writeTVar room.roomUnsubscribe (writeIORef invoked True)
      _ <- subscribeToRoom room
      n <- roomClientCount room
      n `shouldBe` 1
      roomsVar <- newMVar (Map.fromList [(1 :: Int, room)])
      result <- retryRoomTeardown (== 0) roomsVar 1 room
      shouldBeRightUnit result
      readIORef invoked `shouldReturn` False
      rooms' <- readMVar roomsVar
      Map.member 1 rooms' `shouldBe` True

    it "DOES tear the room down regardless of occupancy under a FORCED (const True) eligibility guard, matching forceDeleteRoom's semantics" do
      room <- fixtureCleanRoom
      _ <- subscribeToRoom room
      n <- roomClientCount room
      n `shouldBe` 1
      roomsVar <- newMVar (Map.fromList [(1 :: Int, room)])
      result <- retryRoomTeardown (const True) roomsVar 1 room
      shouldBeRightUnit result
      rooms' <- readMVar roomsVar
      Map.member 1 rooms' `shouldBe` False

  describe "end-to-end production sequencing: delete-cleanup-fails -> retained -> retry-fails -> still retained -> later retry succeeds" do
    it "for a game-keyed rooms map" do
      let boom = ErrorCall "redis unavailable"
      room <- fixtureFlakyRoom 2 (toException boom)
      let gid = fixtureGameId 1
      roomsVar <- newMVar (Map.fromList [(gid, room)])

      -- First attempt (standing in for forceDeleteRoom's own first,
      -- post-commit attempt): fails, room stays retained.
      (rooms1, outcome1) <- attemptRoomTeardown gid =<< readMVar roomsVar
      case outcome1 of
        RoomCleanupUnsubscribeFailed _ -> pure ()
        other -> expectationFailure $ "expected first attempt to fail, got: " <> show other
      _ <- swapMVar roomsVar rooms1
      Map.member gid rooms1 `shouldBe` True

      -- First retry (standing in for another disconnect, or an early
      -- heartbeat tick): still fails, room stays retained.
      retry1 <- retryRoomTeardown (const True) roomsVar gid room
      case retry1 of
        Left _ -> pure ()
        Right () -> expectationFailure "expected the first retry to still fail"
      afterRetry1 <- readMVar roomsVar
      Map.member gid afterRetry1 `shouldBe` True

      -- Later retry (standing in for roomHeartbeat's periodic
      -- drainOwnedCleanup-driven call): finally succeeds, room removed.
      retry2 <- retryRoomTeardown (const True) roomsVar gid room
      shouldBeRightUnit retry2
      afterRetry2 <- readMVar roomsVar
      Map.member gid afterRetry2 `shouldBe` False

    it "for an event-keyed rooms map (the retry mechanism is domain-agnostic)" do
      let boom = ErrorCall "redis unavailable"
      room <- fixtureFlakyRoom 2 (toException boom)
      let eid = fixtureEventId 1
      roomsVar <- newMVar (Map.fromList [(eid, room)])

      (rooms1, outcome1) <- attemptRoomTeardown eid =<< readMVar roomsVar
      case outcome1 of
        RoomCleanupUnsubscribeFailed _ -> pure ()
        other -> expectationFailure $ "expected first attempt to fail, got: " <> show other
      _ <- swapMVar roomsVar rooms1
      Map.member eid rooms1 `shouldBe` True

      retry1 <- retryRoomTeardown (const True) roomsVar eid room
      case retry1 of
        Left _ -> pure ()
        Right () -> expectationFailure "expected the first retry to still fail"
      afterRetry1 <- readMVar roomsVar
      Map.member eid afterRetry1 `shouldBe` True

      retry2 <- retryRoomTeardown (const True) roomsVar eid room
      shouldBeRightUnit retry2
      afterRetry2 <- readMVar roomsVar
      Map.member eid afterRetry2 `shouldBe` False

  describe "registerRoomCleanupRetry (durably hands a failure off to the real, process-global PendingCleanupOwner)" do
    it "genuinely registers a still-failing capability, observable and independently pollable via its own returned receipt" do
      let boom = ErrorCall "redis unavailable, registration test"
      room <- fixtureFailingRoom (toException boom)
      roomsVar <- newMVar (Map.fromList [("registration-still-failing" :: Text, room)])
      receipt <- registerRoomCleanupRetry (const True) roomsVar "registration-still-failing" room
      outcome <- attemptCleanupReceipt globalPendingCleanupOwner receipt
      case outcome of
        ReceiptFailed _ -> pure ()
        other -> expectationFailure $ "expected ReceiptFailed (the stub always fails), got: " <> show other
      rooms' <- readMVar roomsVar
      Map.member ("registration-still-failing" :: Text) rooms' `shouldBe` True
      -- A still-failing entry is deliberately left 'Queued' in the real,
      -- process-global owner for a genuinely later retry (see
      -- 'PendingCleanupOwner.drainPendingCleanup''s own Haddock) -- never
      -- leave one permanently unresolved here, since this exact
      -- process-wide singleton is shared by every OTHER test (in this
      -- module and others, e.g. 'AwsEnvSupervisorLifecycleSpec') that
      -- also touches it. Fix the underlying stub (same 'TVar' identity,
      -- only its content changes) and drive this exact receipt to
      -- completion before returning.
      atomically $ writeTVar room.roomUnsubscribe (pure ())
      finalOutcome <- attemptCleanupReceipt globalPendingCleanupOwner receipt
      shouldBeReceiptSucceeded finalOutcome
      roomsAfter <- readMVar roomsVar
      Map.member ("registration-still-failing" :: Text) roomsAfter `shouldBe` False

    it "a registered capability that later succeeds is observably cleared, and its room removed from the map" do
      room <- fixtureFlakyRoom 1 (toException (ErrorCall "one transient failure"))
      let key = "registration-eventually-succeeds" :: Text
      roomsVar <- newMVar (Map.fromList [(key, room)])
      -- First teardown attempt fails and is registered, mirroring
      -- 'registerRoomCleanupRetryIfFailed''s own call site.
      (rooms1, outcome1) <- attemptRoomTeardown key =<< readMVar roomsVar
      _ <- swapMVar roomsVar rooms1
      case outcome1 of
        RoomCleanupUnsubscribeFailed _ -> pure ()
        other -> expectationFailure $ "expected the first attempt to fail, got: " <> show other
      receipt <- registerRoomCleanupRetry (const True) roomsVar key room
      outcome2 <- attemptCleanupReceipt globalPendingCleanupOwner receipt
      shouldBeReceiptSucceeded outcome2
      -- Polling again observes the same already-resolved outcome, never
      -- re-running the (by-now-removed) capability a second time.
      outcome3 <- attemptCleanupReceipt globalPendingCleanupOwner receipt
      shouldBeReceiptSucceeded outcome3
      rooms' <- readMVar roomsVar
      Map.member key rooms' `shouldBe` False

  -- Draining the WHOLE process-global owner (rather than polling one
  -- specific receipt) could otherwise race a concurrently-running
  -- module's own use of the same singleton; wrapped in 'sequential'
  -- following the precedent 'AwsEnvSupervisorLifecycleSpec' already
  -- establishes for this exact concern.
  sequential $ describe "drainOwnedCleanup (the exact production entry point roomHeartbeat calls every tick)" do
    it "genuinely drains a registered room-cleanup retry through the SAME entry point roomHeartbeat uses in production" do
      room <- fixtureFlakyRoom 1 (toException (ErrorCall "drained by roomHeartbeat's own mechanism"))
      let key = "drain-entry-point" :: Text
      roomsVar <- newMVar (Map.fromList [(key, room)])
      (rooms1, outcome1) <- attemptRoomTeardown key =<< readMVar roomsVar
      _ <- swapMVar roomsVar rooms1
      case outcome1 of
        RoomCleanupUnsubscribeFailed _ -> pure ()
        other -> expectationFailure $ "expected the first attempt to fail, got: " <> show other
      _receipt <- registerRoomCleanupRetry (const True) roomsVar key room
      -- Retry until the shared owner's drain has actually picked this
      -- capability up: a concurrent, unrelated caller could in principle
      -- claim-and-run it between registration and our first drain call,
      -- so poll briefly rather than asserting success on the very first
      -- call.
      let attempts = 20 :: Int
          loop 0 = expectationFailure "drainOwnedCleanup never cleared the registered retry"
          loop n = do
            _ <- drainOwnedCleanup
            rooms' <- readMVar roomsVar
            if Map.member key rooms'
              then threadDelay 10000 >> loop (n - 1)
              else pure ()
      loop attempts
      rooms' <- readMVar roomsVar
      Map.member key rooms' `shouldBe` False

  describe "classifyRoomCleanupFailure (closed, non-sensitive failure category -- never the raw exception text)" do
    it "classifies an IOException as RoomCleanupConnectionFailure" do
      let e = toException (userError "connection refused: secret-host-1.internal:6379")
      classifyRoomCleanupFailure e `shouldBe` RoomCleanupConnectionFailure

    it "classifies anything else as RoomCleanupOtherFailure" do
      let e = toException (ErrorCall "totally-unexpected-secret-token-abcdef123456")
      classifyRoomCleanupFailure e `shouldBe` RoomCleanupOtherFailure

    it "never leaks the original exception's message text through the category's own Show output, for either category" do
      let secret = "super-secret-connection-string-do-not-log-me"
      let ioE = toException (userError secret)
      let otherE = toException (ErrorCall secret)
      show (classifyRoomCleanupFailure ioE) `shouldNotContain` secret
      show (classifyRoomCleanupFailure otherE) `shouldNotContain` secret
