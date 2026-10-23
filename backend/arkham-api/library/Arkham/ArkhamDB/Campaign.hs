module Arkham.ArkhamDB.Campaign where

import Arkham.Card (Card, CardCode, PlayerCard, onlyPlayerCards, toCardCode)
import Arkham.Helpers.Deck (partitionReloadedDeck)
import Arkham.CampaignStep (CampaignStep (..))
import Arkham.Decklist qualified as Decklist
import Arkham.Message (Message (..))
import Arkham.Taboo.Types (TabooList, fromTabooId)
import Arkham.Target (Target (GameTarget))
import Data.Map.Strict qualified as Map
import Relude

isWriteBackBoundary :: Message -> Bool
isWriteBackBoundary = \case
  ForTarget GameTarget (EndOfScenario _) -> True
  FinishedUpgradingDecks -> True
  _ -> False

campaignWriteBackBoundary :: Maybe CampaignStep -> Message -> Maybe Text
campaignWriteBackBoundary step message = case message of
  ForTarget GameTarget (EndOfScenario _) -> Just "scenarioEnd"
  FinishedUpgradingDecks -> case step of
    Just ChooseDecksStep {} -> Nothing
    _ -> Just "upgrade"
  _ -> Nothing

-- Reconcile exactly as ReloadDecks: story cards supersede saved-deck copies
-- by canonical printing, and campaign-invalid cards are not exported.
-- investigatorSideDeck is an engine extra deck (e.g. Joe's hunch deck), NOT
-- ArkhamDB sideSlots. No campaign operation currently mutates the sideboard.
campaignDeckSnapshot :: Decklist.ArkhamDBDecklist -> [PlayerCard] -> [Card] -> [CardCode] -> Maybe [PlayerCard] -> Maybe TabooList -> Decklist.ArkhamDBDecklist
campaignDeckSnapshot original cards story invalid _side taboo = original
  { Decklist.slots = counts $ fst (partitionReloadedDeck story invalid cards) <> onlyPlayerCards story
  , Decklist.taboo_id = find (\n -> fromTabooId (Just n) == taboo) [1..10] <|> original.taboo_id
  }
 where
  counts = Map.fromListWith (+) . map (\card -> (toCardCode card, 1))
