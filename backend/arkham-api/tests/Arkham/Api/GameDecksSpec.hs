module Arkham.Api.GameDecksSpec (spec) where

import Api.Handler.Arkham.Decks (requireGameDecksAccess)
import Arkham.Prelude
import Test.Hspec

-- Regression for #25.  'putApiV1ArkhamGameDecksR' used to authenticate the
-- caller but skip the @ArkhamPlayer@ membership check, allowing any
-- authenticated account to mutate another game's pending deck choice.
--
-- The handler now calls this tested gate before decoding the request body,
-- supplying @getBy (UniquePlayer userId gameId)@ as the membership action and
-- @notFound@ as the rejection action.
spec :: Spec
spec = describe "PUT /games/{gameId}/decks authorization" do
  it "admits an admin without querying game membership" do
    membershipQueried <- newIORef False
    requireGameDecksAccess
      True
      (writeIORef membershipQueried True $> False)
      (expectationFailure "admin access was rejected")
    readIORef membershipQueried `shouldReturn` False

  it "admits a game participant who is not an admin" do
    membershipQueried <- newIORef False
    rejected <- newIORef False
    requireGameDecksAccess
      False
      (writeIORef membershipQueried True $> True)
      (writeIORef rejected True)
    readIORef membershipQueried `shouldReturn` True
    readIORef rejected `shouldReturn` False

  it "rejects an authenticated non-participant" do
    membershipQueried <- newIORef False
    rejected <- newIORef False
    requireGameDecksAccess
      False
      (writeIORef membershipQueried True $> False)
      (writeIORef rejected True)
    readIORef membershipQueried `shouldReturn` True
    readIORef rejected `shouldReturn` True
