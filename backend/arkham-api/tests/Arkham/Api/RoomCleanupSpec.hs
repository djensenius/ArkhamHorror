{- | Proves the post-commit room-cleanup diagnostics described in
'Api.Handler.Arkham.Games.Shared.attemptRoomTeardown' (the seam
'forceDeleteRoom' -- and, through it, 'deleteRoom'\/'deleteEventRoom' --
delegates to) directly against a constructed 'Room', with no live server,
'MVar', or 'App' required.

Before this fix, a synchronous Redis unsubscribe failure after a
successfully committed 'runDB' deletion was swallowed entirely by
'tryAny'\/'tryRedis_': the HTTP response still reported success, but there
was no diagnostic anywhere, and the still-registered callback's only
reachable handle (the room's own map entry) was silently dropped along
with it, turning a retryable leak into a permanently unretryable one.

These tests exercise 'attemptRoomTeardown' -- the exact per-room decision
'forceDeleteRoom' performs under the rooms 'MVar' in production -- against
a 'Room' built via 'newRoom', with its 'roomUnsubscribe' action stubbed to
either succeed or throw synchronously, proving:

* an absent key reports 'RoomCleanupAbsent' and leaves the map completely
  unchanged;
* a present room whose unsubscribe action succeeds reports
  'RoomCleanupClean' and is removed from the returned map;
* a present room whose unsubscribe action throws SYNCHRONOUSLY reports
  'RoomCleanupUnsubscribeFailed' carrying that exception, and is
  deliberately left IN the returned map (retryable), never silently
  dropped;
* an unrelated room, present under a different key, is never touched by a
  teardown targeting some other key, whichever outcome that other key's
  teardown reports.

Database deletion commit semantics are entirely orthogonal to this
function -- 'attemptRoomTeardown' never touches 'runDB' or any
'SqlPersistT' action, only the in-memory rooms map and the room's own
'TVar (IO ())' -- so no test here needs (or could meaningfully exercise) a
database at all; that the already-committed deletion's success is never
rolled back by a cleanup failure is instead a structural property of
'Api.Handler.Arkham.Events.deleteApiV1ArkhamEventR' calling 'deleteRoom'\/'deleteEventRoom'
only strictly AFTER 'runDB' has already returned (see its own Haddock).
-}
module Arkham.Api.RoomCleanupSpec (spec) where

import Api.Handler.Arkham.Games.Shared (RoomCleanupOutcome (..), attemptRoomTeardown)
import Control.Exception (ErrorCall (..))
import Data.Map.Strict qualified as Map
import Foundation (Room (..), newRoom)
import TestImport

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

spec :: Spec
spec = describe "attemptRoomTeardown (post-commit room cleanup, production-used)" do
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
