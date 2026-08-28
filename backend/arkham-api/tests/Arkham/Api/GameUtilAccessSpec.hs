module Arkham.Api.GameUtilAccessSpec (spec) where

import Api.Handler.Arkham.Games.Shared (withGameAccess)
import Arkham.Prelude
import Test.Hspec

data AccessResult = Rejected | Protected
  deriving stock (Eq, Show)

-- Regression for #29.  The utility routes
--   GET  /games/{gameId}/replay/{step}
--   GET  /games/{gameId}/export
--   GET  /games/{gameId}/scenario-export
--   POST /games/{gameId}/file-bug
-- previously authenticated the caller but did not verify game membership,
-- allowing any authenticated account to access another user's game data or
-- upload a bug report on their behalf.
--
-- All four routes now call 'withGameAccess' around all protected game
-- work, supplying @getBy (UniquePlayer userId gameId)@ as the membership
-- action and @notFound@ as the rejection action so that game absence and
-- missing membership remain indistinguishable.
spec :: Spec
spec = describe "Game utility route authorization (withGameAccess)" do
  it "admits an admin without querying membership and runs protected work" do
    membershipQueried <- newIORef False
    protectedStarted <- newIORef False
    result <-
      withGameAccess
        True
        (writeIORef membershipQueried True $> False)
        (pure Rejected)
        (writeIORef protectedStarted True $> Protected)
    readIORef membershipQueried `shouldReturn` False
    readIORef protectedStarted `shouldReturn` True
    result `shouldBe` Protected

  it "admits a game participant and runs protected work" do
    membershipQueried <- newIORef False
    protectedStarted <- newIORef False
    result <-
      withGameAccess
        False
        (writeIORef membershipQueried True $> True)
        (pure Rejected)
        (writeIORef protectedStarted True $> Protected)
    readIORef membershipQueried `shouldReturn` True
    readIORef protectedStarted `shouldReturn` True
    result `shouldBe` Protected

  it "rejects a non-participant without beginning protected work" do
    membershipQueried <- newIORef False
    protectedStarted <- newIORef False
    result <-
      withGameAccess
        False
        (writeIORef membershipQueried True $> False)
        (pure Rejected)
        (writeIORef protectedStarted True $> Protected)
    readIORef membershipQueried `shouldReturn` True
    readIORef protectedStarted `shouldReturn` False
    result `shouldBe` Rejected
