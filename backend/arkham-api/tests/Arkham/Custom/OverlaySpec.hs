{-# LANGUAGE OverloadedStrings #-}

module Arkham.Custom.OverlaySpec (spec) where

import Arkham.Card.CardCode (CardCode (..))
import Arkham.Card.CardDef (CardDef (..), emptyCardDef)
import Arkham.Card.CardType (CardType (InvestigatorType))
import Arkham.Card.CustomCard (CustomCard (..), arkhamBuildCustomCardCode, registerCustomCards)
import Arkham.Custom.Overlay
import Arkham.Decklist.Type (ArkhamDBDecklist (..))
import Arkham.Id (InvestigatorId (..))
import Arkham.Investigator (decklistInvestigatorError, isKnownInvestigator)
import Arkham.Name (Name)
import Data.Aeson (Value, decode, toJSON)
import Data.ByteString.Lazy qualified as BL
import Data.Foldable (for_)
import Data.Map.Strict qualified as Map
import Data.Maybe (isJust)
import Data.Text (Text)
import Data.Text.Encoding (encodeUtf8)
import Entity.Arkham.Deck (ArkhamDeck (..), arkhamDeckPlayList, arkhamDeckPlayListFor)
import Entity.User (Key (UserKey))
import GHC.Records (getField)
import Test.Hspec
import Prelude

-- A deck built at arkham.build for an investigator built here is built on one of
-- that site's placeholders -- it cannot know our cards -- and plays under whoever
-- an overlay lays over it. The upgrade path applied no overlay, so the engine was
-- handed the placeholder and threw from inside building it (#5891).
spec :: Spec
spec = describe "a deck built on an arkham.build placeholder" do
  _ <- runIO $ registerCustomCards (Map.fromList [(ownCode, own), (signatureCode, signature)])

  it "is recognized by the placeholder's own code" do
    needsOverlayInvestigator janeDoeDeck `shouldBe` True

  it "is recognized by the pack the deck says the card came from" do
    needsOverlayInvestigator unlistedPlaceholderDeck `shouldBe` True

  it "is not what an ordinary deck looks like" do
    needsOverlayInvestigator printedDeck `shouldBe` False

  it "cannot be seated as it stands" do
    decklistInvestigatorError janeDoeDeck `shouldSatisfy` isJust

  it "plays as whoever is laid over it, with their signatures" do
    let played = applyOverlay (investigatorOverlay (InvestigatorId ownCode)) janeDoeDeck
    getField @"investigator" played `shouldBe` InvestigatorId ownCode
    Map.lookup signatureCode (slots played) `shouldBe` Just 1
    decklistInvestigatorError played `shouldBe` Nothing

  -- An alternate front outranks the deck's own investigator, so one left in place
  -- would hand back the investigator the overlay just swapped away.
  it "does not keep the alternate front it was swapped away from" do
    let played = applyOverlay (investigatorOverlay (InvestigatorId ownCode)) alternateFrontDeck
    getField @"investigator" played `shouldBe` InvestigatorId ownCode

  -- The parallel faces belong to the deck's investigator: a real swap drops both,
  -- an overlay naming that same investigator keeps both (#219 review).
  describe "an overlay over a deck with parallel faces" do
    it "seats a different investigator over a parallel front, dropping both faces" do
      let played = applyOverlay (investigatorOverlay (InvestigatorId ownCode)) parallelDeck
      getField @"investigator" parallelDeck `shouldBe` "90024"
      getField @"investigator" played `shouldBe` InvestigatorId ownCode
      metaKeys played `shouldBe` Just ["taboo"]

    it "drops an alternate back when a different investigator is laid over it" do
      let played = applyOverlay (investigatorOverlay (InvestigatorId ownCode)) alternateBackDeck
      getField @"investigator" played `shouldBe` InvestigatorId ownCode
      metaKeys played `shouldBe` Just ["taboo"]

    it "keeps both faces when the overlay names the deck's own investigator" do
      let played = applyOverlay (investigatorOverlay "01001") parallelDeck
      meta played `shouldBe` meta parallelDeck
      getField @"investigator" played `shouldBe` "90024"

    it "keeps both faces when the overlay names no investigator" do
      let played = applyOverlay (DeckOverlay Nothing mempty mempty mempty) parallelDeck
      meta played `shouldBe` meta parallelDeck
      getField @"investigator" played `shouldBe` "90024"

    it "keeps both faces when the overlay names the seated parallel front" do
      let played = applyOverlay (investigatorOverlay "90024") parallelDeck
      meta played `shouldBe` meta parallelDeck
      getField @"investigator" played `shouldBe` "90024"

    it "keeps both faces when the overlay names another printing of the deck's investigator" do
      let played = applyOverlay (investigatorOverlay "01501") parallelDeck
      meta played `shouldBe` meta parallelDeck
      investigator_code played `shouldBe` "01501"
      getField @"investigator" played `shouldBe` "90024"

    it "keeps both faces when the deck is on another printing of the overlay's investigator" do
      let reprinted = parallelDeck {investigator_code = "01501"}
          played = applyOverlay (investigatorOverlay "01001") reprinted
      meta played `shouldBe` meta reprinted
      investigator_code played `shouldBe` "01001"
      getField @"investigator" played `shouldBe` "90024"

    it "leaves meta that is not a JSON object as it came" do
      let overlay = investigatorOverlay (InvestigatorId ownCode)
      for_ ["not json", "[\"alternate_front\"]"] \raw -> do
        let played = applyOverlay overlay printedDeck {meta = Just raw}
        meta played `shouldBe` Just raw
        getField @"investigator" played `shouldBe` InvestigatorId ownCode

  -- An upgrade's fallback lays the seat's own investigator over a list whose front
  -- this app cannot build; keeping that front would reject the upgrade (TASK-2.50 review).
  describe "a seat's upgrade through applySeatOverlay" do
    it "seats the base code over a parallel front this app does not know" do
      let seat = Just (investigatorOverlay "01001")
      decklistInvestigatorError (applyOverlay (investigatorOverlay "01001") unknownFrontDeck)
        `shouldSatisfy` isJust
      let played = applySeatOverlay isKnownInvestigator seat unknownFrontDeck
      getField @"investigator" played `shouldBe` "01001"
      decklistInvestigatorError played `shouldBe` Nothing
      metaKeys played `shouldBe` Just ["taboo"]

    it "keeps a parallel front this app knows" do
      let played = applySeatOverlay isKnownInvestigator (Just (investigatorOverlay "01001")) parallelDeck
      meta played `shouldBe` meta parallelDeck
      getField @"investigator" played `shouldBe` "90024"
      decklistInvestigatorError played `shouldBe` Nothing

    it "leaves an unknown front alone when no overlay names an investigator" do
      for_ [Nothing, Just (DeckOverlay Nothing mempty mempty mempty)] \overlay ->
        meta (applySeatOverlay isKnownInvestigator overlay unknownFrontDeck)
          `shouldBe` meta unknownFrontDeck

  -- The saved deck's play list and a per-game overlay over it (DeckAnswer) seat
  -- through the same helper, so neither keeps a front this app cannot build.
  describe "a saved deck's play list" do
    it "seats its overlay's base code over an unknown front" do
      let played = arkhamDeckPlayList (savedDeck unknownFrontDeck (Just (investigatorOverlay "01001")))
      getField @"investigator" played `shouldBe` "01001"
      decklistInvestigatorError played `shouldBe` Nothing

    it "seats a per-game overlay's base code over an unknown front" do
      let played = arkhamDeckPlayListFor (Just (investigatorOverlay "01001")) (savedDeck unknownFrontDeck Nothing)
      getField @"investigator" played `shouldBe` "01001"
      decklistInvestigatorError played `shouldBe` Nothing

    it "keeps a parallel front this app knows" do
      let deck = savedDeck parallelDeck (Just (investigatorOverlay "01001"))
      getField @"investigator" (arkhamDeckPlayList deck) `shouldBe` "90024"
      getField @"investigator" (arkhamDeckPlayListFor (Just (investigatorOverlay "01001")) deck)
        `shouldBe` "90024"

  describe "isKnownInvestigator" do
    it "knows a printed investigator" do
      isKnownInvestigator "01001" `shouldBe` True

    it "knows a custom one in the registry" do
      isKnownInvestigator (InvestigatorId ownCode) `shouldBe` True

    it "does not know a placeholder, which no registry holds" do
      isKnownInvestigator (InvestigatorId janeDoeCode) `shouldBe` False

ownCode :: CardCode
ownCode = "*0ver1a1d00000000000000000000000"

signatureCode :: CardCode
signatureCode = "*0ver1a1d51gnature0000000000000"

own :: CustomCard
own =
  CustomCard
    (emptyCardDef ownCode ("Overlaid" :: Name) InvestigatorType)
      { cdMeta = Map.fromList [("_signatures", toJSON [signatureCode])]
      }
    Nothing

signature :: CustomCard
signature = CustomCard (emptyCardDef signatureCode ("Overlaid's Own" :: Name) InvestigatorType) Nothing

janeDoeCode :: CardCode
janeDoeCode = arkhamBuildCustomCardCode "c13c4114-0769-410c-864b-e43f5a596c0d"

decklist :: ArkhamDBDecklist
decklist =
  ArkhamDBDecklist
    { slots = Map.empty
    , sideSlots = Map.empty
    , investigator_code = "01001"
    , investigator_name = "Roland Banks"
    , meta = Nothing
    , taboo_id = Nothing
    , url = Nothing
    , decklist_id = Nothing
    , decklist_name = Nothing
    }

janeDoeDeck :: ArkhamDBDecklist
janeDoeDeck =
  decklist
    { investigator_code = InvestigatorId janeDoeCode
    , investigator_name = "Jane Doe"
    , slots = Map.fromList [("01087", 2)]
    }

{- | As the deck arrives from arkham.build: the card is named by an id nothing
here lists, and only the pack it came from says what it is.
-}
unlistedPlaceholderDeck :: ArkhamDBDecklist
unlistedPlaceholderDeck =
  decklist
    { investigator_code =
        InvestigatorId (arkhamBuildCustomCardCode "00000000-0000-0000-0000-000000000001")
    , investigator_name = "Someone Nobody"
    , meta =
        Just
          "{\"fan_made_content\":{\"cards\":{\"00000000-0000-0000-0000-000000000001\":{\"pack_code\":\"d12a4d9c-8c65-4df0-be0c-c2e14af65a21\"}}}}"
    }

printedDeck :: ArkhamDBDecklist
printedDeck = decklist {slots = Map.fromList [("01020", 1)]}

-- | Roland Banks played with the parallel front and back, plus an unrelated key.
parallelDeck :: ArkhamDBDecklist
parallelDeck =
  decklist
    { meta =
        Just "{\"alternate_front\":\"90024\",\"alternate_back\":\"90024\",\"taboo\":1}"
    }

alternateBackDeck :: ArkhamDBDecklist
alternateBackDeck = decklist {meta = Just "{\"alternate_back\":\"90024\",\"taboo\":1}"}

-- | Roland Banks with an arkham.build front nothing here registers.
unknownFrontDeck :: ArkhamDBDecklist
unknownFrontDeck =
  decklist
    { meta =
        Just "{\"alternate_front\":\"*00000000000000000000000000000f00\",\"alternate_back\":\"01001\",\"taboo\":1}"
    }

-- | A deck row as saved, with an overlay laid over it or not.
savedDeck :: ArkhamDBDecklist -> Maybe DeckOverlay -> ArkhamDeck
savedDeck list overlay = ArkhamDeck (UserKey 1) Nothing "Deck" "Roland Banks" list overlay Nothing Nothing

metaKeys :: ArkhamDBDecklist -> Maybe [Text]
metaKeys d = Map.keys <$> (decode @(Map.Map Text Value) . BL.fromStrict . encodeUtf8 =<< meta d)

alternateFrontDeck :: ArkhamDBDecklist
alternateFrontDeck =
  janeDoeDeck {meta = Just "{\"alternate_front\":\"*c13c41140769410c864be43f5a596c0d0\"}"}
