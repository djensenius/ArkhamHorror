module Arkham.ArkhamDB.Campaign (
  module Arkham.ArkhamDB.Campaign,
  campaignDeckOverlayKey,
  recordCampaignDeckOverlay,
) where

import Arkham.Card (Card, CardCode, PlayerCard, onlyPlayerCards, toCardCode)
import Arkham.ArkhamDB.Overlay
import Arkham.Id (InvestigatorId)
import Arkham.ArkhamDB.Service (withExportedOverlay, withoutExportedOverlay)
import Arkham.Helpers.Deck (partitionReloadedDeck)
import Arkham.Campaign.Types (campaignStep, campaignStore, campaignDecks)
import Arkham.Classes.Entity (toAttrs)
import Arkham.Game (Game (..), RunObservers (..), noRunObservers)
import Arkham.Game.Utils (modeCampaign, gameInvestigators)
import Arkham.Investigator.Types (investigatorForm, InvestigatorForm (HomunculusForm))
import Arkham.CampaignStep (CampaignStep (..))
import Arkham.Decklist qualified as Decklist
import Arkham.Message (Message (..))
import Arkham.Taboo.Types (TabooList, fromTabooId)
import Arkham.Target (Target (GameTarget))
import Data.Aeson (Value, parseJSON)
import Data.Aeson.Types (parseMaybe)
import Data.Map.Strict qualified as Map
import Relude

-- Resolve materialized cards/proof separately from the durable association id.
-- A Homunculus's live id changes only on reset; both phases share the origin.
campaignWriteBackIdentity :: Map Text Value -> [InvestigatorId] -> InvestigatorId -> Bool -> Maybe (InvestigatorId, InvestigatorId)
campaignWriteBackIdentity store occupied liveId homunculus = do
  let deckId = if homunculus then "11068b" else liveId
      recorded iid = Map.lookup (campaignDeckIdentityKey iid) store >>= parseMaybe parseJSON
  guard $ deckId `elem` occupied
  stableId <- if homunculus then recorded deckId
    else case Map.lookup (campaignDeckIdentityKey deckId) store of
      Nothing -> Just liveId
      Just value -> parseMaybe parseJSON value
  guard $ liveId == deckId || liveId == stableId
  guard $ homunculus || stableId == liveId
  -- Do not let another seated deck claim this same association/baseline.
  guard $ all (\other -> other == deckId || (other /= stableId && recorded other /= Just stableId)) occupied
  pure (deckId, stableId)

campaignWriteBackSeatIdentity :: Game -> InvestigatorId -> Maybe (InvestigatorId, InvestigatorId)
campaignWriteBackSeatIdentity game iid = do
  campaign <- toAttrs <$> modeCampaign game.gameMode
  investigator <- Map.lookup iid $ gameInvestigators game
  campaignWriteBackIdentity (campaignStore campaign) (Map.keys $ campaignDecks campaign)
    iid (investigatorForm (toAttrs investigator) == HomunculusForm)

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

-- Use the provenance of the latest materialization, not a forever-valid seat
-- capture. Whole-deck replacements reset it; incremental campaign edits retain
-- their source lineage. The store follows ordinary persistence and undo.
campaignWriteBackSnapshot :: Map Text Value -> InvestigatorId -> Decklist.ArkhamDBDecklist -> [PlayerCard] -> [Card] -> [CardCode] -> Maybe [PlayerCard] -> Maybe TabooList -> Decklist.ArkhamDBDecklist
campaignWriteBackSnapshot store iid original cards story invalid side taboo =
  let snapshot = campaignDeckSnapshot original cards story invalid side taboo
  in case Map.lookup (campaignDeckOverlayKey iid) store >>= parseMaybe parseJSON of
    Just overlay -> withExportedOverlay overlay snapshot
    Nothing -> withoutExportedOverlay snapshot
