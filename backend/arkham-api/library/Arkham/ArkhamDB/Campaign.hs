module Arkham.ArkhamDB.Campaign where

import Arkham.Card (Card (..), PlayerCard, toCardCode, toCardId)
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
campaignDeckSnapshot :: Decklist.ArkhamDBDecklist -> [PlayerCard] -> [Card] -> Maybe [PlayerCard] -> Maybe TabooList -> Decklist.ArkhamDBDecklist
campaignDeckSnapshot original cards story side taboo = original
  { Decklist.slots = counts $ nubBy (\a b -> toCardId a == toCardId b) $ map PlayerCard cards <> story
  , Decklist.sideSlots = maybe original.sideSlots (counts . map PlayerCard) side
  , Decklist.taboo_id = find (\n -> fromTabooId (Just n) == taboo) [1..10]
  }
 where
  counts = Map.fromListWith (+) . map (\card -> (toCardCode card, 1))
