{- | Regression for #46.  'getApiV1ArkhamGameR' used to authenticate the
caller, then unconditionally call @webSocketsOptions $ gameStream gameId@,
and only /afterwards/ -- inside the ordinary REST branch, which a successful
WebSocket upgrade never reaches -- run the @getBy404 (UniquePlayer userId
gameId)@ membership lookup. An authenticated non-participant could reach
'Api.Handler.Arkham.Games.Shared.gameStream' (join its room, increment its
member count, receive full-game snapshots, and submit answers) without ever
being a recorded player of that game.

The handler now runs the membership lookup through 'withGameStreamMember'
/before/ attempting the WebSocket upgrade, and reuses the resulting player
entity in the REST body, so there is exactly one authorization decision.
This spec exercises 'withGameStreamMember' itself -- the exact function
'getApiV1ArkhamGameR' calls, with production wiring the real
@getBy404 (UniquePlayer userId gameId)@ lookup in as @authorize@ and both the
WebSocket upgrade and the REST body in as @onAuthorized@ -- against plain
'IO' stand-ins for gameStream's join/count/snapshot/answer side effects, with
no live database or Redis connection required.
-}
module Arkham.Api.GameStreamAuthSpec (spec) where

import Api.Handler.Arkham.Games (withGameStreamMember)
import Arkham.Prelude
import Data.Either (isLeft)
import Test.Hspec

-- | Everything 'Api.Handler.Arkham.Games.Shared.gameStream' does once a
-- WebSocket upgrade has been accepted for a game: join the room, increment
-- its member count, deliver at least one snapshot, and (if the client sends
-- one) accept a submitted answer. Recorded via 'IORef's so a test can assert
-- on exactly which of these happened.
data StreamEffects = StreamEffects
  { roomJoined :: IORef Bool
  , memberCountIncremented :: IORef Bool
  , snapshotDelivered :: IORef Bool
  , answerSubmitted :: IORef Bool
  }

newStreamEffects :: IO StreamEffects
newStreamEffects =
  StreamEffects
    <$> newIORef False
    <*> newIORef False
    <*> newIORef False
    <*> newIORef False

-- | Stand-in for the real 'Api.Handler.Arkham.Games.Shared.gameStream':
-- performs every one of its observable effects, in the same order gameStream
-- performs them (join, then count, then at least one snapshot, then an
-- answer if submitted).
runProtectedGameStream :: StreamEffects -> playerEntity -> IO ()
runProtectedGameStream effects _player = do
  writeIORef effects.roomJoined True
  writeIORef effects.memberCountIncremented True
  writeIORef effects.snapshotDelivered True
  writeIORef effects.answerSubmitted True

allEffects :: StreamEffects -> IO [Bool]
allEffects effects =
  sequence
    [ readIORef effects.roomJoined
    , readIORef effects.memberCountIncremented
    , readIORef effects.snapshotDelivered
    , readIORef effects.answerSubmitted
    ]

-- | A fixture "authorized player" value, standing in for the
-- @Entity ArkhamPlayer@ 'getApiV1ArkhamGameR' reuses to pick 'WithFriends'
-- vs 'Solo' player selection; its identity doesn't matter to this spec,
-- only that it is the one value threaded through from @authorize@ to
-- @onAuthorized@.
data FixturePlayer = FixturePlayer
  deriving stock (Eq, Show)

notAMember :: IO FixturePlayer
notAMember = throwIO $ userError "not a participant" -- mirrors production's nondisclosing getBy404 404

aMember :: IO FixturePlayer
aMember = pure FixturePlayer

spec :: Spec
spec = describe "withGameStreamMember (participant game-stream authorization gate, #46)" do
  it "never joins the room, counts a member, delivers a snapshot, or accepts an answer for an authenticated non-member" do
    effects <- newStreamEffects
    (result :: Either SomeException ()) <-
      try $ withGameStreamMember notAMember (runProtectedGameStream effects)
    result `shouldSatisfy` isLeft
    allEffects effects `shouldReturn` [False, False, False, False]

  it "joins the room, counts a member, delivers a snapshot, and accepts an answer for an authorized participant" do
    effects <- newStreamEffects
    withGameStreamMember aMember (runProtectedGameStream effects)
    allEffects effects `shouldReturn` [True, True, True, True]

  it "calls authorize exactly once and reuses its result, matching one-authorization-decision reuse in the REST branch" do
    calls <- newIORef (0 :: Int)
    received <- newIORef Nothing
    let authorize = modifyIORef' calls (+ 1) >> pure FixturePlayer
    withGameStreamMember authorize (writeIORef received . Just)
    readIORef calls `shouldReturn` 1
    readIORef received `shouldReturn` Just FixturePlayer

  it "the pre-fix ordering (protected work unconditional, authorization checked too late to matter) is unsafe -- proving the old ordering must fail this regression" do
    -- This mirrors the *shape* of the bug fixed here: the old handler called
    -- @webSocketsOptions $ gameStream gameId@ immediately after
    -- authenticating the caller, with the @UniquePlayer@ membership lookup
    -- reachable only afterwards, inside a REST branch a successful upgrade
    -- never reaches. Modelled here as running the protected action first and
    -- discarding whatever @authorize@ would have decided.
    let oldUnsafeOrdering :: IO FixturePlayer -> (FixturePlayer -> IO ()) -> IO ()
        oldUnsafeOrdering _authorize onAuthorized = onAuthorized FixturePlayer
    effectsOld <- newStreamEffects
    oldUnsafeOrdering notAMember (const $ runProtectedGameStream effectsOld FixturePlayer)
    allEffects effectsOld `shouldReturn` [True, True, True, True] -- old shape: non-member still got in

    -- The function actually wired into 'getApiV1ArkhamGameR' does not exhibit
    -- this: the same non-member authorize action blocks every effect.
    effectsNew <- newStreamEffects
    (_ :: Either SomeException ()) <-
      try $ withGameStreamMember notAMember (runProtectedGameStream effectsNew)
    allEffects effectsNew `shouldReturn` [False, False, False, False]
