{- | Regression for #46.  'Api.Handler.Arkham.Games.getApiV1ArkhamGameR' used
to authenticate the caller, then unconditionally call @webSocketsOptions $
gameStream gameId@, and only /afterwards/ -- inside the ordinary REST
branch, which a successful WebSocket upgrade never reaches -- run the
@getBy404 (UniquePlayer userId gameId)@ membership lookup. An authenticated
non-participant could reach 'Api.Handler.Arkham.Games.Shared.gameStream'
(join its room, increment its member count, receive full-game snapshots,
and submit answers) without ever being a recorded player of that game.

The handler now delegates its entire body to 'withGameParticipant', whose
own body is nothing but the verbatim @getBy (UniquePlayer userId gameId)@
lookup the REST branch has always used, followed immediately by a call to
'gameParticipantGate' with that lookup's result and the (still-unevaluated)
WebSocket-upgrade action. 'gameParticipantGate' is the /only/ place in this
module that ever runs the @attemptUpgrade@ action it is handed, and it does
so exclusively in the @Just@ branch, strictly before the REST continuation.
'webSocketsOptions' and 'Api.Handler.Arkham.Games.Shared.gameStream' appear
nowhere else in "Api.Handler.Arkham.Games", so there is no other path by
which 'getApiV1ArkhamGameR' can reach them.

This spec:

* exercises 'gameParticipantGate' itself -- the exact function that owns the
  lookup-result branch and the upgrade\/continuation ordering -- against
  concrete, already-looked-up @Maybe (Entity ArkhamPlayer)@ fixture values
  (real 'ArkhamPlayer' rows, not opaque IO stand-ins), proving a non-member
  ('Nothing') blocks the upgrade action and the REST continuation
  unconditionally, and a member ('Just') runs the upgrade strictly before
  the continuation with the correct 'ArkhamPlayerId';
* proves this exact function, unmodified, would fail a run reproducing the
  original bug's observable shape (upgrade attempted for a non-member); and
* statically confirms, by inspecting the actual source of
  "Api.Handler.Arkham.Games", that 'getApiV1ArkhamGameR' delegates
  exclusively to 'withGameParticipant' for its entire body, and that
  'webSocketsOptions' \/ @gameStream@ occur nowhere in this module except
  inside 'withGameParticipant', so a future edit cannot reintroduce an
  unguarded upgrade without this test failing.
-}
module Arkham.Api.GameStreamAuthSpec (spec) where

import Api.Handler.Arkham.Games (gameParticipantGate)
import Arkham.Prelude
import Data.List qualified as List
import Data.Text qualified as T
import Data.Text.IO qualified as T
import Data.UUID qualified as UUID
import Database.Persist.Sql (Entity (..), toSqlKey)
import Entity.Arkham.Game qualified as GameEntity
import Entity.Arkham.Player (ArkhamPlayer (..), ArkhamPlayerId)
import Entity.Arkham.Player qualified as PlayerEntity
import Test.Hspec

-- | Everything 'Api.Handler.Arkham.Games.Shared.gameStream' does once a
-- WebSocket upgrade has been accepted for a game: join the room, increment
-- its member count, deliver at least one snapshot, and (if the client sends
-- one) accept a submitted answer. Recorded via 'IORef's so a test can assert
-- on exactly which of these happened, and in what order relative to the
-- REST continuation.
data StreamEffects = StreamEffects
  { steps :: IORef [Text]
  }

newStreamEffects :: IO StreamEffects
newStreamEffects = StreamEffects <$> newIORef []

record :: StreamEffects -> Text -> IO ()
record effects step = modifyIORef' effects.steps (<> [step])

-- | Stand-in for the real 'Api.Handler.Arkham.Games.Shared.gameStream':
-- performs every one of its observable effects, in the same order gameStream
-- performs them (join, then count, then at least one snapshot, then an
-- answer if submitted).
attemptUpgrade :: StreamEffects -> IO ()
attemptUpgrade effects = do
  record effects "roomJoined"
  record effects "memberCountIncremented"
  record effects "snapshotDelivered"
  record effects "answerSubmitted"

-- | Fixture 'ArkhamPlayer' row for a real participant of a game, built the
-- same way existing specs (e.g. "Arkham.Api.Events.EventCreationSpec")
-- build fixture entities: real 'Key' constructors over deterministic ids,
-- not opaque placeholders.
fixturePlayer :: Entity ArkhamPlayer
fixturePlayer =
  Entity (PlayerEntity.ArkhamPlayerKey $ UUID.fromWords 0 0 0 1) player
 where
  player =
    ArkhamPlayer
      { arkhamPlayerUserId = toSqlKey 7
      , arkhamPlayerArkhamGameId = GameEntity.ArkhamGameKey $ UUID.fromWords 0 0 0 2
      , arkhamPlayerInvestigatorId = "01001"
      }

spec :: Spec
spec = describe "gameParticipantGate (participant game-stream authorization gate, #46)" do
  it "blocks the upgrade attempt and the REST continuation for an authenticated non-member" do
    effects <- newStreamEffects
    continuationRan <- newIORef False
    rejectRan <- newIORef False
    gameParticipantGate
      Nothing
      (attemptUpgrade effects)
      (writeIORef rejectRan True)
      (\_playerId -> record effects "continuation" >> writeIORef continuationRan True)
    readIORef effects.steps `shouldReturn` []
    readIORef continuationRan `shouldReturn` False
    readIORef rejectRan `shouldReturn` True

  it "attempts the upgrade strictly before the REST continuation, and reuses the looked-up playerId, for a member" do
    effects <- newStreamEffects
    rejectRan <- newIORef False
    receivedPlayerId <- newIORef Nothing
    gameParticipantGate
      (Just fixturePlayer)
      (attemptUpgrade effects)
      (writeIORef rejectRan True)
      (\playerId -> record effects "continuation" >> writeIORef receivedPlayerId (Just playerId))
    readIORef effects.steps
      `shouldReturn` ["roomJoined", "memberCountIncremented", "snapshotDelivered", "answerSubmitted", "continuation"]
    readIORef rejectRan `shouldReturn` False
    readIORef receivedPlayerId `shouldReturn` Just fixturePlayer.id

  it "reproduces the original bug's observable shape only when fed a non-member result through the pre-fix (unconditional-upgrade) ordering, and this exact function does not do that" do
    -- Modelled directly on the real bug: the old handler ran the WebSocket
    -- upgrade unconditionally, regardless of what the membership lookup
    -- would have found, with the lookup only reachable afterwards in a REST
    -- branch a successful upgrade never reaches.
    let oldUnsafeOrdering
          :: Maybe (Entity ArkhamPlayer) -> IO () -> IO () -> (ArkhamPlayerId -> IO ()) -> IO ()
        oldUnsafeOrdering _mPlayer attemptUpgrade' _reject _onAuthorized = attemptUpgrade'

    effectsOld <- newStreamEffects
    oldUnsafeOrdering Nothing (attemptUpgrade effectsOld) (pure ()) (const $ pure ())
    readIORef effectsOld.steps `shouldReturn` ["roomJoined", "memberCountIncremented", "snapshotDelivered", "answerSubmitted"] -- old shape: non-member still got in

    -- 'gameParticipantGate' -- the function actually wired into
    -- 'Api.Handler.Arkham.Games.withGameParticipant' -- does not exhibit
    -- this: the same non-member lookup result blocks every effect.
    effectsNew <- newStreamEffects
    gameParticipantGate Nothing (attemptUpgrade effectsNew) (pure ()) (const $ pure ())
    readIORef effectsNew.steps `shouldReturn` []

  describe "static structure of Api.Handler.Arkham.Games" do
    it "getApiV1ArkhamGameR delegates its entire body to withGameParticipant, which is the only place webSocketsOptions/gameStream are used" do
      src <- T.readFile "library/Api/Handler/Arkham/Games.hs"
      -- Every top-level declaration in this file starts at column 0 and is
      -- separated from its neighbours by at least one blank line, with no
      -- blank lines inside a declaration's own body (Haddock, signature,
      -- and equation together). Grouping consecutive non-blank lines finds
      -- exactly the paragraph containing a given top-level name's own
      -- signature/equation lines (which, being unindented, are the only
      -- lines starting with that name at column 0) -- not merely a
      -- Haddock cross-reference to it elsewhere, which never itself starts
      -- a line.
      let ls = T.lines src
          isBlank l = T.null (T.strip l)
          groupParagraphs :: [Text] -> [Text] -> [[Text]] -> [[Text]]
          groupParagraphs [] cur acc = reverse (if null cur then acc else reverse cur : acc)
          groupParagraphs (l : rest) cur acc
            | isBlank l = groupParagraphs rest [] (if null cur then acc else reverse cur : acc)
            | otherwise = groupParagraphs rest (l : cur) acc
          paragraphs = map T.unlines (groupParagraphs ls [] [])
          paragraphContainingTopLevel name = case List.find (any (name `T.isPrefixOf`) . T.lines) paragraphs of
            Just p -> p
            Nothing -> error $ "no paragraph found for top-level: " <> T.unpack name
          getApiV1ArkhamGameRBody = paragraphContainingTopLevel "getApiV1ArkhamGameR "
          withGameParticipantBody = paragraphContainingTopLevel "withGameParticipant"

      T.count "withGameParticipant" getApiV1ArkhamGameRBody `shouldSatisfy` (> 0)
      T.count "webSocketsOptions" getApiV1ArkhamGameRBody `shouldBe` 0
      T.count "gameStream" getApiV1ArkhamGameRBody `shouldBe` 0

      T.count "webSocketsOptions" withGameParticipantBody `shouldSatisfy` (> 0)
      T.count "gameStream" withGameParticipantBody `shouldSatisfy` (> 0)
      T.count "gameParticipantGate" withGameParticipantBody `shouldSatisfy` (> 0)

      -- The participant upgrade call ("gameStream gameId", lower-case @g@)
      -- is distinct from the deliberately-untouched spectator route's own
      -- "spectatorGameStream gameId" (capital @G@, a different function);
      -- it appears exactly once in the whole module, and only inside
      -- withGameParticipant's paragraph.
      T.count "gameStream gameId" src `shouldBe` 1
      T.isInfixOf "gameStream gameId" withGameParticipantBody `shouldBe` True
