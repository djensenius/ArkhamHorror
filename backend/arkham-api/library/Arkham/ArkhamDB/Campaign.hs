module Arkham.ArkhamDB.Campaign where

import Arkham.Card (Card (..), PlayerCard, onlyPlayerCards, toCardCode, toCardId)
import Arkham.Decklist qualified as Decklist
import Arkham.Message (Message (..))
import Arkham.Taboo.Types (TabooList, fromTabooId)
import Arkham.Target (Target (GameTarget))
import Data.List (nubBy)
import Data.Map.Strict qualified as Map
import Relude

isWriteBackBoundary :: Message -> Bool
isWriteBackBoundary = \case
  ForTarget GameTarget (EndOfScenario _) -> True
  FinishedUpgradingDecks -> True
  _ -> False

-- Campaign story cards can also occur in its stored deck: deduplicate identity,
-- not code, so two distinct copies of one card keep their actual quantity.
-- investigatorSideDeck is an engine extra deck (e.g. Joe's hunch deck), NOT
-- ArkhamDB sideSlots. No campaign operation currently mutates the sideboard.
campaignDeckSnapshot :: Decklist.ArkhamDBDecklist -> [PlayerCard] -> [Card] -> Maybe [PlayerCard] -> Maybe TabooList -> Decklist.ArkhamDBDecklist
campaignDeckSnapshot original cards story _side taboo = original
  { Decklist.slots = counts $ nubBy (\a b -> toCardId a == toCardId b) $ map PlayerCard $ cards <> onlyPlayerCards story
  , Decklist.taboo_id = find (\n -> fromTabooId (Just n) == taboo) [1..10] <|> original.taboo_id
  }
 where
  counts = Map.fromListWith (+) . map (\card -> (toCardCode card, 1))
