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

TASK-2.67 adds the same shape for spectating: 'withSpectatableGameIn' runs
the game-existence lookup before the spectator upgrade, so an unknown id is
rejected without opening a room, and the spectate route delegates to it
without requiring sign-in.

TASK-2.92 gives the admin game stream the same gate: 'withAdminGameIn' runs
the game-existence lookup before the admin's participant-stream upgrade, so
an unknown id is rejected without opening (and then holding) a room.
-}
module Arkham.Api.GameStreamAuthSpec (spec) where

import Api.Handler.Arkham.Games (
  gameParticipantGate,
  withGameParticipantIn,
  withSpectatableGameIn,
 )
import Api.Handler.Arkham.Games.Admin (withAdminGameIn)
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
-- structure assertions below.
readGamesHsSource :: IO Text
readGamesHsSource = readLibrarySource "Api/Handler/Arkham/Games.hs"

-- | Read the real source of "Api.Handler.Arkham.Games.Admin" (TASK-2.92).
readAdminHsSource :: IO Text
readAdminHsSource = readLibrarySource "Api/Handler/Arkham/Games/Admin.hs"

-- | Read a module's source, given its path under @library\/@. @stack
-- test@\/@cabal test@ always run with the package directory
-- (@backend\/arkham-api@) as the working directory, but tries a couple of
-- other plausible working directories too (repo root, and @backend\/@) so
-- this test doesn't spuriously fail if it is ever invoked a different way,
-- instead of silently trusting a single hard-coded relative path.
readLibrarySource :: FilePath -> IO Text
readLibrarySource modulePath = go candidatePaths
 where
  candidatePaths :: [FilePath]
  candidatePaths =
    [ "library/" <> modulePath
    , "backend/arkham-api/library/" <> modulePath
    , "arkham-api/library/" <> modulePath
    ]
  go [] =
    error
      $ "GameStreamAuthSpec: could not find "
      <> modulePath
      <> " under any of the candidate paths (tried relative to cwd): "
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

  describe "withSpectatableGameIn (spectate existence check, TASK-2.67)" do
    it "looks up the game before rejecting an unknown id without attempting the upgrade" do
      effects <- newStreamEffects
      withSpectatableGameIn
        (record effects "existenceLookup" >> pure False)
        (attemptUpgrade effects)
        (record effects "rejected")
        (record effects "continuation")
      readIORef effects.steps `shouldReturn` ["existenceLookup", "rejected"]

    it "looks up the game before upgrading, then runs the REST continuation, for an existing game" do
      effects <- newStreamEffects
      withSpectatableGameIn
        (record effects "existenceLookup" >> pure True)
        (attemptUpgrade effects)
        (record effects "rejected")
        (record effects "continuation")
      readIORef effects.steps
        `shouldReturn`
          [ "existenceLookup"
          , "roomJoined"
          , "memberCountIncremented"
          , "snapshotDelivered"
          , "answerSubmitted"
          , "continuation"
          ]

    it "runs the continuation after the lookup for an existing game when the upgrade is a no-op (plain GET)" do
      effects <- newStreamEffects
      withSpectatableGameIn
        (record effects "existenceLookup" >> pure True)
        (pure ())
        (record effects "rejected")
        (record effects "continuation")
      readIORef effects.steps `shouldReturn` ["existenceLookup", "continuation"]

  describe "static structure of the spectate handler (TASK-2.67)" do
    it "delegates the spectate handler to the existence gate without requiring sign-in" do
      src <- readGamesHsSource
      let ls = T.lines src
          -- The handler's own top-level block: its signature line up to the
          -- next unindented line that does not belong to it.
          start = case List.findIndex ("getApiV1ArkhamGameSpectateR ::" `T.isPrefixOf`) ls of
            Just i -> i
            Nothing -> error "no getApiV1ArkhamGameSpectateR signature in Games.hs"
          isOwnLine l =
            T.null l
              || " " `T.isPrefixOf` l
              || "getApiV1ArkhamGameSpectateR" `T.isPrefixOf` l
          spectateBody = T.unlines $ takeWhile isOwnLine $ drop start ls
          normalized = T.unwords $ T.words spectateBody

      T.isInfixOf "withSpectatableGame gameId $" normalized `shouldBe` True
      T.isInfixOf "get404 gameId" normalized `shouldBe` True
      T.count "servePongAware" spectateBody `shouldBe` 0
      T.count "spectatorStream" spectateBody `shouldBe` 0
      T.count "getRequestUserId" spectateBody `shouldBe` 0
      T.count "requireAuth" spectateBody `shouldBe` 0

    it "wires the existence lookup, then the spectator upgrade, then notFound into the gate" do
      src <- readGamesHsSource
      let ls = T.lines src
          -- The top-level block of withSpectatableGame, sliced as above. A
          -- single servePongAware, after withSpectatableGameIn, means nothing
          -- upgrades before the gate runs its lookup.
          start = case List.findIndex ("withSpectatableGame ::" `T.isPrefixOf`) ls of
            Just i -> i
            Nothing -> error "no withSpectatableGame signature in Games.hs"
          isOwnLine l =
            T.null l
              || " " `T.isPrefixOf` l
              || "withSpectatableGame " `T.isPrefixOf` l
          wiringBody = T.unwords $ T.words $ T.unlines $ takeWhile isOwnLine $ drop start ls
          positionOf needle
            | needle `T.isInfixOf` wiringBody = T.length $ fst $ T.breakOn needle wiringBody
            | otherwise = error $ "withSpectatableGame does not mention: " <> T.unpack needle
          order =
            map
              positionOf
              ["withSpectatableGameIn", "spectatableGameExists", "servePongAware", "notFound"]

      order `shouldBe` List.sort order
      T.count "servePongAware" wiringBody `shouldBe` 1
      T.count "spectatorStream" wiringBody `shouldBe` 1
      T.isInfixOf "servePongAware $ spectatorStream gameId" wiringBody `shouldBe` True

  describe "withAdminGameIn (admin stream existence check, TASK-2.92)" do
    it "looks up the game before rejecting an unknown id without attempting the upgrade" do
      effects <- newStreamEffects
      withAdminGameIn
        (record effects "existenceLookup" >> pure False)
        (attemptUpgrade effects)
        (record effects "rejected")
        (record effects "continuation")
      readIORef effects.steps `shouldReturn` ["existenceLookup", "rejected"]

    it "looks up the game before upgrading, then runs the REST continuation, for an existing game" do
      effects <- newStreamEffects
      withAdminGameIn
        (record effects "existenceLookup" >> pure True)
        (attemptUpgrade effects)
        (record effects "rejected")
        (record effects "continuation")
      readIORef effects.steps
        `shouldReturn`
          [ "existenceLookup"
          , "roomJoined"
          , "memberCountIncremented"
          , "snapshotDelivered"
          , "answerSubmitted"
          , "continuation"
          ]

    it "runs the continuation after the lookup for an existing game when the upgrade is a no-op (plain GET)" do
      effects <- newStreamEffects
      withAdminGameIn
        (record effects "existenceLookup" >> pure True)
        (pure ())
        (record effects "rejected")
        (record effects "continuation")
      readIORef effects.steps `shouldReturn` ["existenceLookup", "continuation"]

  describe "static structure of the admin game handler (TASK-2.92)" do
    it "delegates the admin game handler to the existence gate" do
      src <- readAdminHsSource
      let ls = T.lines src
          -- The handler's own top-level block: its signature line up to the
          -- next unindented line that does not belong to it.
          start = case List.findIndex ("getApiV1AdminGameR ::" `T.isPrefixOf`) ls of
            Just i -> i
            Nothing -> error "no getApiV1AdminGameR signature in Admin.hs"
          isOwnLine l =
            T.null l
              || " " `T.isPrefixOf` l
              || "getApiV1AdminGameR" `T.isPrefixOf` l
          adminBody = T.unlines $ takeWhile isOwnLine $ drop start ls
          normalized = T.unwords $ T.words adminBody

      T.isInfixOf "withAdminGame gameId" normalized `shouldBe` True
      T.isInfixOf "get404 gameId" normalized `shouldBe` True
      T.count "servePongAware" adminBody `shouldBe` 0
      T.count "gameStream" adminBody `shouldBe` 0

    it "wires the existence lookup, then the participant upgrade, then notFound into the gate" do
      src <- readAdminHsSource
      let ls = T.lines src
          -- The top-level block of withAdminGame, sliced as above. A single
          -- servePongAware, after withAdminGameIn, means nothing upgrades
          -- before the gate runs its lookup.
          start = case List.findIndex ("withAdminGame ::" `T.isPrefixOf`) ls of
            Just i -> i
            Nothing -> error "no withAdminGame signature in Admin.hs"
          isOwnLine l =
            T.null l
              || " " `T.isPrefixOf` l
              || "withAdminGame " `T.isPrefixOf` l
          wiringBody = T.unwords $ T.words $ T.unlines $ takeWhile isOwnLine $ drop start ls
          positionOf needle
            | needle `T.isInfixOf` wiringBody = T.length $ fst $ T.breakOn needle wiringBody
            | otherwise = error $ "withAdminGame does not mention: " <> T.unpack needle
          order =
            map
              positionOf
              ["withAdminGameIn", "adminGameExists", "servePongAware", "notFound"]

      order `shouldBe` List.sort order
      T.count "servePongAware" wiringBody `shouldBe` 1
      T.count "gameStream" wiringBody `shouldBe` 1
      -- The admin keeps the participant stream, not the spectator one.
      T.count "spectatorStream" wiringBody `shouldBe` 0
      T.isInfixOf "servePongAware $ gameStream gameId" wiringBody `shouldBe` True

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
