module Arkham.ArkhamDB.Campaign where

import Arkham.Card (Card, CardCode, PlayerCard, onlyPlayerCards, toCardCode)
import Arkham.Card.CardCode (unCardCode)
import Arkham.Custom.Overlay (DeckOverlay)
import Arkham.Id (InvestigatorId (..))
import Arkham.ArkhamDB.Service (withExportedOverlay, withoutExportedOverlay)
import Arkham.Helpers.Deck (partitionReloadedDeck)
import Arkham.Campaign.Types (campaignStep)
import Arkham.Classes.Entity (toAttrs)
import Arkham.Game (Game (..), RunObservers (..), noRunObservers)
import Arkham.Game.Utils (modeCampaign)
import Arkham.CampaignStep (CampaignStep (..))
import Arkham.Decklist qualified as Decklist
import Arkham.Message (Message (..))
import Arkham.Taboo.Types (TabooList, fromTabooId)
import Arkham.Target (Target (GameTarget, CampaignTarget))
import Data.Aeson (Value, toJSON, parseJSON)
import Data.Aeson.Key qualified as Key
import Data.Aeson.Types (parseMaybe)
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

-- Capture the pre-message campaign/roster together with the label. Later
-- continuations can swap campaigns or clear investigators (Dream-Eaters).
-- Callers persist this only after a successful drain, in the game transaction.
observeCampaignWriteBackBoundary :: IORef Game -> IORef (Maybe (Text, Game)) -> Message -> IO ()
observeCampaignWriteBackBoundary gameRef boundaryRef message = when (isWriteBackBoundary message) do
  current <- readIORef gameRef
  for_ (campaignWriteBackBoundary (campaignStep . toAttrs <$> modeCampaign current.gameMode) message) \boundary ->
    writeIORef boundaryRef $ Just (boundary, current)

-- A remaining seat/question is not a completed upgrade, even though its
-- continuation is already in the queue. Exceptions never return a snapshot.
withCampaignWriteBackBoundary :: IORef Game -> (RunObservers -> IO ()) -> IO (Maybe (Text, Game))
withCampaignWriteBackBoundary gameRef drain = do
  boundaryRef <- newIORef Nothing
  drain noRunObservers {observeMessage = Just $ observeCampaignWriteBackBoundary gameRef boundaryRef}
  readIORef boundaryRef

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

-- Capture at deck seating, when the saved overlay is actually materialized,
-- not at a later boundary when the saved row may already have a different one.
-- The existing campaign store is persisted and restored together with undo.
campaignDeckOverlayKey :: InvestigatorId -> Text
campaignDeckOverlayKey iid = "ahn_overlay/" <> unCardCode (unInvestigatorId iid)

recordCampaignDeckOverlay :: InvestigatorId -> Maybe DeckOverlay -> Message
recordCampaignDeckOverlay iid overlay =
  SetGlobal CampaignTarget (Key.fromText $ campaignDeckOverlayKey iid) (toJSON overlay)

campaignWriteBackSnapshot :: Map Text Value -> InvestigatorId -> Decklist.ArkhamDBDecklist -> [PlayerCard] -> [Card] -> [CardCode] -> Maybe [PlayerCard] -> Maybe TabooList -> Decklist.ArkhamDBDecklist
campaignWriteBackSnapshot store iid original cards story invalid side taboo =
  let snapshot = campaignDeckSnapshot original cards story invalid side taboo
  in case Map.lookup (campaignDeckOverlayKey iid) store >>= parseMaybe parseJSON of
    Just overlay -> withExportedOverlay overlay snapshot
    Nothing -> withoutExportedOverlay snapshot
