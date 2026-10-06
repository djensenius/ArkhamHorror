module Arkham.Api.GameDecksSpec (spec) where

import Api.Handler.Arkham.Decks (replacementDeckRejection, requireGameDecksAccess)
import Arkham.Card.CardCode (unCardCode)
import Arkham.Decklist qualified as Decklist
import Arkham.Id
import Arkham.Prelude
import Data.Set qualified as Set
import Test.Hspec

-- Regression for #25.  'putApiV1ArkhamGameDecksR' used to authenticate the
-- caller but skip the @ArkhamPlayer@ membership check, allowing any
-- authenticated account to mutate another game's pending deck choice.
--
-- The handler now calls this tested gate before decoding the request body,
-- supplying @getBy (UniquePlayer userId gameId)@ as the membership action and
-- @notFound@ as the rejection action.
spec :: Spec
spec = do
  describe "PUT /games/{gameId}/decks authorization" do
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

  describe "replacement deck validation" do
    let roland = InvestigatorId "c01001"
        daisy = InvestigatorId "c01002"
        killedOrInsane = Set.singleton roland

    it "rejects continuing without upgrading when the investigator must be replaced" do
      replacementDeckRejection roland True killedOrInsane Nothing
        `shouldBe` Just "That investigator was killed or driven insane and must be replaced"

    it "rejects re-choosing the killed or insane investigator's deck as its own replacement" do
      replacementDeckRejection roland True killedOrInsane (Just $ decklistFor roland)
        `shouldBe` Just "That investigator was killed or driven insane and must be replaced"

    it "accepts a replacement deck for a live investigator" do
      replacementDeckRejection roland True killedOrInsane (Just $ decklistFor daisy)
        `shouldBe` Nothing

    it "does not block ordinary non-replacement upgrade skips" do
      replacementDeckRejection roland False killedOrInsane Nothing
        `shouldBe` Nothing

decklistFor :: InvestigatorId -> Decklist.ArkhamDBDecklist
decklistFor iid@(InvestigatorId rawId) =
  let investigatorCode = unCardCode rawId
   in Decklist.ArkhamDBDecklist
      { Decklist.slots = mempty
      , Decklist.sideSlots = mempty
      , Decklist.investigator_code = iid
      , Decklist.investigator_name = investigatorCode
      , Decklist.meta = Nothing
      , Decklist.taboo_id = Nothing
      , Decklist.url = Nothing
      , Decklist.decklist_id = Nothing
      , Decklist.decklist_name = Nothing
      }
