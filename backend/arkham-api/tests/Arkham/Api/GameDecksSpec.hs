module Arkham.Api.GameDecksSpec (spec) where

import Api.Handler.Arkham.Decks (
  killedOrInsaneInvestigatorIdsFromCampaignLog,
  mustReplaceInvestigatorDeck,
  replacementDeckRejection,
  requireGameDecksAccess,
 )
import Arkham.CampaignLog (mkCampaignLog, setCampaignLogRecorded)
import Arkham.CampaignLogKey (CampaignLogKey (DrivenInsaneInvestigators, KilledInvestigators), recorded)
import Arkham.Card.CardCode (CardCode, unCardCode)
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
    let roland = InvestigatorId "01001"
        daisy = InvestigatorId "01002"
        agnes = InvestigatorId "01004"
        rolandAlternateArt = InvestigatorId "01501"
        killedOrInsane = Set.singleton roland
        noTakenInvestigators = mempty

    it "rejects continuing without upgrading when the investigator must be replaced" do
      replacementDeckRejection roland True killedOrInsane noTakenInvestigators Nothing
        `shouldBe` Just "That investigator was killed or driven insane and must be replaced"

    it "rejects re-choosing the killed or insane investigator's deck as its own replacement" do
      replacementDeckRejection roland True killedOrInsane noTakenInvestigators (Just $ decklistFor roland)
        `shouldBe` Just "That investigator was killed or driven insane and must be replaced"

    it "rejects an alternate-art deck for the killed or insane investigator as its own replacement" do
      replacementDeckRejection roland True killedOrInsane noTakenInvestigators (Just $ decklistFor rolandAlternateArt)
        `shouldBe` Just "That investigator was killed or driven insane and must be replaced"

    it "rejects replacing with a different killed or insane investigator" do
      replacementDeckRejection roland True (Set.fromList [roland, daisy]) noTakenInvestigators (Just $ decklistFor daisy)
        `shouldBe` Just "That investigator was killed or driven insane"

    it "rejects replacing with an already-taken investigator" do
      replacementDeckRejection roland True killedOrInsane (Set.singleton daisy) (Just $ decklistFor daisy)
        `shouldBe` Just "This investigator is already taken"

    it "rejects switching to a killed or insane investigator even when the answering investigator is alive" do
      replacementDeckRejection agnes False killedOrInsane noTakenInvestigators (Just $ decklistFor roland)
        `shouldBe` Just "That investigator was killed or driven insane"

    it "rejects alternate art for a killed or insane investigator when chosen by another seat" do
      replacementDeckRejection agnes False killedOrInsane noTakenInvestigators (Just $ decklistFor rolandAlternateArt)
        `shouldBe` Just "That investigator was killed or driven insane"

    it "rejects alternate art for another seat's investigator" do
      replacementDeckRejection agnes False mempty (Set.singleton roland) (Just $ decklistFor rolandAlternateArt)
        `shouldBe` Just "This investigator is already taken"

    it "accepts a genuinely different investigator when alternate-art collisions are blocked" do
      replacementDeckRejection agnes False killedOrInsane (Set.singleton roland) (Just $ decklistFor daisy)
        `shouldBe` Nothing

    it "accepts a replacement deck for a live untaken investigator" do
      replacementDeckRejection roland True killedOrInsane noTakenInvestigators (Just $ decklistFor daisy)
        `shouldBe` Nothing

    it "does not block ordinary non-replacement upgrade skips" do
      replacementDeckRejection roland False killedOrInsane noTakenInvestigators Nothing
        `shouldBe` Nothing

    it "detects killed and driven-insane investigators from the campaign log" do
      let campaignLog =
            setCampaignLogRecorded KilledInvestigators [recorded ("01001" :: CardCode)]
              $ setCampaignLogRecorded DrivenInsaneInvestigators [recorded ("01002" :: CardCode)] mkCampaignLog
          killedOrInsaneFromLog = killedOrInsaneInvestigatorIdsFromCampaignLog campaignLog
      killedOrInsaneFromLog `shouldBe` Set.fromList [roland, daisy]
      mustReplaceInvestigatorDeck roland False killedOrInsaneFromLog `shouldBe` True
      mustReplaceInvestigatorDeck daisy False killedOrInsaneFromLog `shouldBe` True
      mustReplaceInvestigatorDeck agnes False killedOrInsaneFromLog `shouldBe` False
      mustReplaceInvestigatorDeck agnes True mempty `shouldBe` True

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
