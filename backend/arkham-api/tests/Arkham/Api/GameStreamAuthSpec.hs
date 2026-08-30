{- | Regression for #46.  'Api.Handler.Arkham.Games.getApiV1ArkhamGameR' used
to authenticate the caller, then unconditionally call @webSocketsOptions $
gameStream gameId@, and only /afterwards/ -- inside the ordinary REST
branch, which a successful WebSocket upgrade never reaches -- run the
@getBy404 (UniquePlayer userId gameId)@ membership lookup. An authenticated
non-participant could reach 'Api.Handler.Arkham.Games.Shared.gameStream'
(join its room, increment its member count, receive full-game snapshots,
and submit answers) without ever being a recorded player of that game.

The handler delegates its body to 'withGameParticipant', which supplies the
real lookup and WebSocket-upgrade effects to 'withGameParticipantIn'.
'withGameParticipantIn' owns their sequencing: it executes the lookup first,
then passes its result and the still-unevaluated upgrade action to
'gameParticipantGate'.  The gate runs the upgrade only for a participant,
strictly before the REST continuation.

This spec:

* executes 'withGameParticipantIn' with recording effects, proving the
  membership lookup happens first and a missing participant blocks every
  upgrade and continuation effect;
* exercises 'gameParticipantGate' against concrete @ArkhamPlayer@ entities,
  proving member identity and upgrade\/continuation ordering; and
* narrowly checks the real route still authenticates, delegates to the gate,
  contains no direct stream upgrade, and uses the authorized participant id
  for multiplayer projection.
-}
module Arkham.Api.GameStreamAuthSpec (spec) where

import Api.Handler.Arkham.Games (gameParticipantGate, withGameParticipantIn)
import Arkham.Prelude
import Data.List qualified as List
import Data.Text qualified as T
import Data.Text.IO qualified as T
import Data.UUID qualified as UUID
import Database.Persist.Sql (Entity (..), toSqlKey)
import Entity.Arkham.Game qualified as GameEntity
import Entity.Arkham.Player (ArkhamPlayer (..), ArkhamPlayerId)
import Entity.Arkham.Player qualified as PlayerEntity
import System.Directory (doesFileExist)
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
-- answer). Unlike the real 'gameStream', which only submits an answer if
-- the client actually sends one, this stand-in always records
-- @"answerSubmitted"@ -- it exists to prove that a non-member is blocked
-- from *all* of these effects, so always including the answer step (the
-- strictest case) only strengthens that guarantee.
attemptUpgrade :: StreamEffects -> IO ()
attemptUpgrade effects = do
  record effects "roomJoined"
  record effects "memberCountIncremented"
  record effects "snapshotDelivered"
  record effects "answerSubmitted"

-- | Read the real source of "Api.Handler.Arkham.Games", for the static
-- structure assertions below. @stack test@\/@cabal test@ always run with
-- the package directory (@backend\/arkham-api@) as the working directory,
-- but tries a couple of other plausible working directories too (repo
-- root, and @backend\/@) so this test doesn't spuriously fail if it is
-- ever invoked a different way, instead of silently trusting a single
-- hard-coded relative path.
readGamesHsSource :: IO Text
readGamesHsSource = go candidatePaths
 where
  candidatePaths :: [FilePath]
  candidatePaths =
    [ "library/Api/Handler/Arkham/Games.hs"
    , "backend/arkham-api/library/Api/Handler/Arkham/Games.hs"
    , "arkham-api/library/Api/Handler/Arkham/Games.hs"
    ]
  go [] =
    error
      $ "GameStreamAuthSpec: could not find Games.hs under any of the candidate paths (tried relative to cwd): "
      <> show candidatePaths
  go (path : rest) = do
    exists <- doesFileExist path
    if exists then T.readFile path else go rest

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
spec = do
  describe "withGameParticipantIn (participant lookup and stream gate, #46)" do
    it "looks up membership before rejecting a non-member without attempting the upgrade" do
      effects <- newStreamEffects
      withGameParticipantIn
        (record effects "membershipLookup" >> pure Nothing)
        (attemptUpgrade effects)
        (record effects "rejected")
        (\_playerId -> record effects "continuation")
      readIORef effects.steps `shouldReturn` ["membershipLookup", "rejected"]

    it "looks up membership before upgrading and passes the authorized participant id onward" do
      effects <- newStreamEffects
      receivedPlayerId <- newIORef Nothing
      withGameParticipantIn
        (record effects "membershipLookup" >> pure (Just fixturePlayer))
        (attemptUpgrade effects)
        (record effects "rejected")
        (\playerId -> record effects "continuation" >> writeIORef receivedPlayerId (Just playerId))
      readIORef effects.steps
        `shouldReturn`
          [ "membershipLookup"
          , "roomJoined"
          , "memberCountIncremented"
          , "snapshotDelivered"
          , "answerSubmitted"
          , "continuation"
          ]
      readIORef receivedPlayerId `shouldReturn` Just fixturePlayer.id

  describe "gameParticipantGate (participant game-stream authorization gate, #46)" do
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

    it "does not reproduce the original unconditional-upgrade ordering for a non-member" do
      let oldUnsafeOrdering
            :: Maybe (Entity ArkhamPlayer) -> IO () -> IO () -> (ArkhamPlayerId -> IO ()) -> IO ()
          oldUnsafeOrdering _mPlayer attemptUpgrade' _reject _onAuthorized = attemptUpgrade'

      effectsOld <- newStreamEffects
      oldUnsafeOrdering Nothing (attemptUpgrade effectsOld) (pure ()) (const $ pure ())
      readIORef effectsOld.steps
        `shouldReturn` ["roomJoined", "memberCountIncremented", "snapshotDelivered", "answerSubmitted"]

      effectsNew <- newStreamEffects
      withGameParticipantIn
        (record effectsNew "membershipLookup" >> pure Nothing)
        (attemptUpgrade effectsNew)
        (record effectsNew "rejected")
        (const $ record effectsNew "continuation")
      readIORef effectsNew.steps `shouldReturn` ["membershipLookup", "rejected"]

  describe "static structure of Api.Handler.Arkham.Games" do
    it "authenticates, delegates participant authorization, and preserves the authorized multiplayer identity" do
      src <- readGamesHsSource
      let ls = T.lines src
          firstLineStartingWith prefix = case List.findIndex (prefix `T.isPrefixOf`) ls of
            Just i -> i
            Nothing -> error $ "no line in Games.hs starts with: " <> T.unpack prefix
          sliceBetween startName endName =
            let start = firstLineStartingWith startName
                end = firstLineStartingWith endName
            in T.unlines $ take (end - start) $ drop start ls
          getApiV1ArkhamGameRBody = sliceBetween "getApiV1ArkhamGameR " "getApiV1ArkhamGameSpectateR"
          normalizedHandler = T.unwords $ T.words getApiV1ArkhamGameRBody

      T.isInfixOf "userId <- getRequestUserId" normalizedHandler `shouldBe` True
      T.isInfixOf "withGameParticipant userId gameId \\playerId ->" normalizedHandler `shouldBe` True
      T.count "webSocketsOptions" getApiV1ArkhamGameRBody `shouldBe` 0
      T.count "gameStream" getApiV1ArkhamGameRBody `shouldBe` 0
      T.isInfixOf "WithFriends -> coerce playerId" normalizedHandler `shouldBe` True
