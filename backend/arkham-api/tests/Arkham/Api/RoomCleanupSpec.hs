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
  PreparedRoomCleanup (..),
  RoomCleanupFailureCategory (..),
  RoomCleanupMode (..),
  RoomCleanupOutcome (..),
  attemptRoomTeardown,
  classifyRoomCleanupFailure,
  executePreparedRoomCleanup,
  prepareRoomCleanup,
  registerRoomCleanupRetry,
  retryRoomTeardown,
  runRoomHeartbeatTick,
 )
import Api.Arkham.Lifecycle (drainOwnedCleanup)
import Control.Concurrent (forkIO, threadDelay)
import Control.Exception (AsyncException (..), ErrorCall (..))
import Control.Exception qualified as Exception
import Data.Map.Strict qualified as Map
import Data.UUID qualified as UUID
import Entity.Arkham.Epic qualified as Epic
import Entity.Arkham.Game qualified as GameEntity
import Foundation (Room (..), newRoom, roomClientCount, subscribeToRoom, unsubscribeFromRoom)
import PendingCleanupOwner (
  CleanupAttempt (..),
  ReceiptOutcome (..),
  attemptCleanupReceipt,
  drainPendingCleanupBounded,
  globalPendingCleanupOwner,
  newPendingCleanupOwner,
  transferPendingCleanup,
 )
import System.Timeout qualified as Timeout
import TestImport

shouldBeCleanupComplete :: HasCallStack => CleanupAttempt -> Expectation
shouldBeCleanupComplete = \case
  CleanupComplete -> pure ()
  CleanupFailed e -> expectationFailure $ "expected CleanupComplete, got failure: " <> show e
  CleanupDeferred -> expectationFailure "expected CleanupComplete, got CleanupDeferred"

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
  describe "attemptRoomTeardown (isolated low-level unsubscribe decision)" do
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
    it "completes without an unsubscribe when nothing is tracked under this key" do
      roomsVar <- newMVar (Map.empty :: Map Int Room)
      expected <- fixtureCleanRoom
      result <- retryRoomTeardown CleanupForced roomsVar 1 expected
      shouldBeCleanupComplete result

    it "reports CleanupFailed and leaves the room retained when unsubscribe still fails" do
      let boom = ErrorCall "still failing"
      room <- fixtureFailingRoom (toException boom)
      roomsVar <- newMVar (Map.fromList [(1 :: Int, room)])
      result <- retryRoomTeardown CleanupForced roomsVar 1 room
      case result of
        CleanupFailed e -> case fromException e of
          Just (ErrorCall msg) -> msg `shouldBe` "still failing"
          Nothing -> expectationFailure $ "expected the stubbed ErrorCall, got: " <> show e
        other -> expectationFailure $ "expected CleanupFailed, got " <> show other
      rooms' <- readMVar roomsVar
      Map.member 1 rooms' `shouldBe` True

    it "succeeds and removes the room once the unsubscribe eventually succeeds" do
      room <- fixtureCleanRoom
      roomsVar <- newMVar (Map.fromList [(1 :: Int, room)])
      result <- retryRoomTeardown CleanupForced roomsVar 1 room
      shouldBeCleanupComplete result
      rooms' <- readMVar roomsVar
      Map.member 1 rooms' `shouldBe` False

    it "is a no-op success and NEVER invokes the current room's unsubscribe when the room under this key has since been replaced by a DIFFERENT instance" do
      staleFailingRoom <- fixtureFailingRoom (toException (ErrorCall "should never run"))
      invoked <- newIORef False
      newerRoom <- newRoom fixtureChannel
      atomically $ writeTVar newerRoom.roomUnsubscribe (writeIORef invoked True)
      roomsVar <- newMVar (Map.fromList [(1 :: Int, newerRoom)])
      result <- retryRoomTeardown CleanupForced roomsVar 1 staleFailingRoom
      shouldBeCleanupComplete result
      readIORef invoked `shouldReturn` False
      rooms' <- readMVar roomsVar
      case Map.lookup 1 rooms' of
        Just r | roomUnsubscribe r == roomUnsubscribe newerRoom -> pure ()
        _ -> expectationFailure "expected the map to still hold the exact newer room instance, untouched"

    it "defers under an ordinary eligibility guard when the room has regained a live subscriber, and never invokes unsubscribe" do
      invoked <- newIORef False
      room <- newRoom fixtureChannel
      atomically $ writeTVar room.roomUnsubscribe (writeIORef invoked True)
      _ <- subscribeToRoom room
      n <- roomClientCount room
      n `shouldBe` 1
      roomsVar <- newMVar (Map.fromList [(1 :: Int, room)])
      result <- retryRoomTeardown CleanupWhenEmpty roomsVar 1 room
      case result of
        CleanupDeferred -> pure ()
        other -> expectationFailure $ "expected CleanupDeferred, got " <> show other
      readIORef invoked `shouldReturn` False
      rooms' <- readMVar roomsVar
      Map.member 1 rooms' `shouldBe` True

    it "tears the room down regardless of occupancy in forced mode, matching forceDeleteRoom's semantics" do
      room <- fixtureCleanRoom
      _ <- subscribeToRoom room
      n <- roomClientCount room
      n `shouldBe` 1
      roomsVar <- newMVar (Map.fromList [(1 :: Int, room)])
      result <- retryRoomTeardown CleanupForced roomsVar 1 room
      shouldBeCleanupComplete result
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
      retry1 <- retryRoomTeardown CleanupForced roomsVar gid room
      case retry1 of
        CleanupFailed _ -> pure ()
        other -> expectationFailure $ "expected the first retry to fail, got " <> show other
      afterRetry1 <- readMVar roomsVar
      Map.member gid afterRetry1 `shouldBe` True

      -- Later retry (standing in for roomHeartbeat's periodic
      -- drainOwnedCleanup-driven call): finally succeeds, room removed.
      retry2 <- retryRoomTeardown CleanupForced roomsVar gid room
      shouldBeCleanupComplete retry2
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

      retry1 <- retryRoomTeardown CleanupForced roomsVar eid room
      case retry1 of
        CleanupFailed _ -> pure ()
        other -> expectationFailure $ "expected the first retry to fail, got " <> show other
      afterRetry1 <- readMVar roomsVar
      Map.member eid afterRetry1 `shouldBe` True

      retry2 <- retryRoomTeardown CleanupForced roomsVar eid room
      shouldBeCleanupComplete retry2
      afterRetry2 <- readMVar roomsVar
      Map.member eid afterRetry2 `shouldBe` False

  describe "durable per-room cleanup ownership" do
    it "hands off every game/event room before post-commit cancellation can interrupt an unsubscribe" do
      gameStarted <- newEmptyMVar
      neverFinishGame <- newEmptyMVar
      gameRoom <- newRoom fixtureChannel
      atomically $
        writeTVar gameRoom.roomUnsubscribe do
          putMVar gameStarted ()
          takeMVar neverFinishGame
      eventRuns <- newIORef (0 :: Int)
      eventRoom <- newRoom fixtureChannel
      atomically $
        writeTVar eventRoom.roomUnsubscribe $
          atomicModifyIORef' eventRuns (\n -> (n + 1, ()))
      let gid = fixtureGameId 31
          eid = fixtureEventId 31
      gameRooms <- newMVar $ Map.singleton gid gameRoom
      eventRooms <- newMVar $ Map.singleton eid eventRoom

      -- This is the exact masked handoff shape used by deleteEventRooms:
      -- both domains are owned before either ticket is executed.
      (Just gameCleanup, Just eventCleanup) <-
        Exception.uninterruptibleMask_ $
          (,) <$> prepareRoomCleanup CleanupForced gameRooms gid
            <*> prepareRoomCleanup CleanupForced eventRooms eid
      isJust <$> readTVarIO gameRoom.roomCleanupReceipt `shouldReturn` True
      isJust <$> readTVarIO eventRoom.roomCleanupReceipt `shouldReturn` True

      gameDone <- newEmptyMVar
      gameTid <- forkIO $ putMVar gameDone =<< Exception.try @SomeException (executePreparedRoomCleanup gameCleanup)
      takeMVar gameStarted
      throwTo gameTid ThreadKilled
      takeMVar gameDone >>= \case
        Left e -> fromException e `shouldBe` Just ThreadKilled
        Right outcome -> expectationFailure $ "expected cancellation, got " <> show outcome

      -- The event cleanup is independent of the cancelled request and
      -- remains directly executable from its already-owned receipt.
      executePreparedRoomCleanup eventCleanup >>= \case
        RoomCleanupClean -> pure ()
        outcome -> expectationFailure $ "expected event cleanup to succeed, got " <> show outcome
      readIORef eventRuns `shouldReturn` 1
      Map.member eid <$> readMVar eventRooms `shouldReturn` False

      -- Cancellation requeued the game receipt and reopened the retained map
      -- entry before propagating, so a later owner can finish it.
      atomically $ writeTVar gameRoom.roomUnsubscribe (pure ())
      executePreparedRoomCleanup gameCleanup >>= \case
        RoomCleanupClean -> pure ()
        outcome -> expectationFailure $ "expected game retry to succeed, got " <> show outcome
      Map.member gid <$> readMVar gameRooms `shouldReturn` False
      readTVarIO gameRoom.roomCleanupReceipt `shouldReturn` Nothing
      readTVarIO eventRoom.roomCleanupReceipt `shouldReturn` Nothing

    it "coalesces repeated failures for one room into one receipt and retires the owner/map exactly once" do
      attempts <- newIORef (0 :: Int)
      room <- newRoom fixtureChannel
      atomically $
        writeTVar room.roomUnsubscribe do
          atomicModifyIORef' attempts (\n -> (n + 1, ()))
          throwIO $ ErrorCall "redis unavailable"
      rooms <- newMVar $ Map.singleton ("same-room" :: Text) room
      Just prepared <- prepareRoomCleanup CleanupForced rooms "same-room"
      duplicate1 <- registerRoomCleanupRetry CleanupForced rooms "same-room" room
      duplicate2 <- registerRoomCleanupRetry CleanupForced rooms "same-room" room
      duplicate1 `shouldBe` prepared.preparedReceipt
      duplicate2 `shouldBe` prepared.preparedReceipt

      executePreparedRoomCleanup prepared >>= \case
        RoomCleanupUnsubscribeFailed _ -> pure ()
        outcome -> expectationFailure $ "expected unsubscribe failure, got " <> show outcome
      duplicate3 <- registerRoomCleanupRetry CleanupForced rooms "same-room" room
      duplicate3 `shouldBe` prepared.preparedReceipt
      readIORef attempts `shouldReturn` 1
      Map.member "same-room" <$> readMVar rooms `shouldReturn` True

      atomically $ writeTVar room.roomUnsubscribe $ atomicModifyIORef' attempts (\n -> (n + 1, ()))
      executePreparedRoomCleanup prepared >>= \case
        RoomCleanupClean -> pure ()
        outcome -> expectationFailure $ "expected successful retry, got " <> show outcome
      executePreparedRoomCleanup prepared >>= \case
        RoomCleanupClean -> pure ()
        outcome -> expectationFailure $ "expected retired receipt to stay successful, got " <> show outcome
      readIORef attempts `shouldReturn` 2
      Map.member "same-room" <$> readMVar rooms `shouldReturn` False
      readTVarIO room.roomCleanupReceipt `shouldReturn` Nothing

    it "defers one empty-only receipt across rejoin and reuses it on the later disconnect without double-unsubscribe" do
      room <- fixtureFlakyRoom 1 (toException $ ErrorCall "first disconnect fails")
      rooms <- newMVar $ Map.singleton ("rejoin" :: Text) room
      Just firstCleanup <- prepareRoomCleanup CleanupWhenEmpty rooms "rejoin"
      executePreparedRoomCleanup firstCleanup >>= \case
        RoomCleanupUnsubscribeFailed _ -> pure ()
        outcome -> expectationFailure $ "expected first failure, got " <> show outcome

      (subId, _) <- subscribeToRoom room
      executePreparedRoomCleanup firstCleanup >>= \case
        RoomCleanupStillOccupied -> pure ()
        outcome -> expectationFailure $ "expected rejoined room to remain occupied, got " <> show outcome
      Map.member "rejoin" <$> readMVar rooms `shouldReturn` True
      readTVarIO room.roomCleanupReceipt `shouldReturn` Just firstCleanup.preparedReceipt

      unsubscribeFromRoom room subId
      Just secondCleanup <- prepareRoomCleanup CleanupWhenEmpty rooms "rejoin"
      secondCleanup.preparedReceipt `shouldBe` firstCleanup.preparedReceipt
      executePreparedRoomCleanup secondCleanup >>= \case
        RoomCleanupClean -> pure ()
        outcome -> expectationFailure $ "expected later disconnect cleanup, got " <> show outcome
      Map.member "rejoin" <$> readMVar rooms `shouldReturn` False
      readTVarIO room.roomCleanupReceipt `shouldReturn` Nothing

    it "upgrades a deferred empty-only generation to forced cleanup without creating a second receipt" do
      runs <- newIORef (0 :: Int)
      room <- newRoom fixtureChannel
      atomically $ writeTVar room.roomUnsubscribe $ atomicModifyIORef' runs (\n -> (n + 1, ()))
      _ <- subscribeToRoom room
      rooms <- newMVar $ Map.singleton ("forced-upgrade" :: Text) room
      Just ordinaryCleanup <- prepareRoomCleanup CleanupWhenEmpty rooms "forced-upgrade"
      executePreparedRoomCleanup ordinaryCleanup >>= \case
        RoomCleanupStillOccupied -> pure ()
        outcome -> expectationFailure $ "expected deferred ordinary cleanup, got " <> show outcome
      readIORef runs `shouldReturn` 0

      Just forcedCleanup <- prepareRoomCleanup CleanupForced rooms "forced-upgrade"
      forcedCleanup.preparedReceipt `shouldBe` ordinaryCleanup.preparedReceipt
      executePreparedRoomCleanup forcedCleanup >>= \case
        RoomCleanupClean -> pure ()
        outcome -> expectationFailure $ "expected forced cleanup to finish, got " <> show outcome
      readIORef runs `shouldReturn` 1
      Map.member "forced-upgrade" <$> readMVar rooms `shouldReturn` False
      readTVarIO room.roomCleanupReceipt `shouldReturn` Nothing

    it "does not hold the rooms MVar while the network unsubscribe is blocked" do
      started <- newEmptyMVar
      neverFinish <- newEmptyMVar
      room <- newRoom fixtureChannel
      atomically $
        writeTVar room.roomUnsubscribe do
          putMVar started ()
          takeMVar neverFinish
      rooms <- newMVar $ Map.singleton (1 :: Int) room
      Just prepared <- prepareRoomCleanup CleanupForced rooms 1
      done <- newEmptyMVar
      tid <- forkIO $ putMVar done =<< Exception.try @SomeException (executePreparedRoomCleanup prepared)
      takeMVar started
      isJust <$> tryReadMVar rooms `shouldReturn` True
      throwTo tid ThreadKilled
      takeMVar done >>= \case
        Left e -> fromException e `shouldBe` Just ThreadKilled
        Right outcome -> expectationFailure $ "expected cancellation, got " <> show outcome
      atomically $ writeTVar room.roomUnsubscribe (pure ())
      executePreparedRoomCleanup prepared >>= \case
        RoomCleanupClean -> pure ()
        outcome -> expectationFailure $ "expected retry success, got " <> show outcome

  describe "bounded heartbeat cleanup" do
    it "refreshes the active-room work first and still returns when an unrelated cleanup action blocks" do
      owner <- newPendingCleanupOwner
      blocker <- newEmptyMVar
      _ <-
        transferPendingCleanup owner do
          takeMVar blocker
          pure (Right ())
      refreshed <- newIORef False
      completed <-
        Timeout.timeout 500_000 $
          runRoomHeartbeatTick
            (writeIORef refreshed True)
            (drainPendingCleanupBounded owner 1 20_000)
      completed `shouldBe` Just ()
      readIORef refreshed `shouldReturn` True
      putMVar blocker ()
      drainPendingCleanupBounded owner 1 20_000

  describe "registerRoomCleanupRetry (durably hands a failure off to the real, process-global PendingCleanupOwner)" do
    it "genuinely registers a still-failing capability, observable and independently pollable via its own returned receipt" do
      let boom = ErrorCall "redis unavailable, registration test"
      room <- fixtureFailingRoom (toException boom)
      roomsVar <- newMVar (Map.fromList [("registration-still-failing" :: Text, room)])
      receipt <- registerRoomCleanupRetry CleanupForced roomsVar "registration-still-failing" room
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
      receipt <- registerRoomCleanupRetry CleanupForced roomsVar key room
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
      _receipt <- registerRoomCleanupRetry CleanupForced roomsVar key room
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
