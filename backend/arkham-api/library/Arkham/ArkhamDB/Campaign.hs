module Arkham.ArkhamDB.Campaign (
  module Arkham.ArkhamDB.Campaign,
  campaignDeckOverlayKey,
  recordCampaignDeckOverlay,
) where

import Arkham.Card (Card, CardCode, PlayerCard, onlyPlayerCards, toCardCode)
import Arkham.ArkhamDB.Overlay
import Arkham.Id (InvestigatorId (..), CampaignId (..))
import Arkham.ArkhamDB.Service (withOverlayProvenance, withoutExportedOverlay)
import Arkham.Helpers.Deck (partitionReloadedDeck)
import Arkham.Campaign.Types (campaignStep, campaignStore, campaignDecks, campaignId, campaignMeta)
import Arkham.Campaigns.TheDreamEaters.Meta qualified as Dream
import Arkham.Classes.Entity (toAttrs)
import Arkham.Game (Game (..), RunObservers (..), noRunObservers)
import Arkham.Game.Utils (modeCampaign, gameInvestigators)
import Arkham.Investigator.Types (investigatorForm, InvestigatorForm (HomunculusForm))
import Arkham.CampaignStep (CampaignStep (..))
import Arkham.Decklist qualified as Decklist
import Arkham.Message (Message (..))
import Arkham.Taboo.Types (TabooList, fromTabooId)
import Arkham.Target (Target (GameTarget))
import Data.Aeson (Value, parseJSON, withObject, (.:), (.:?))
import Data.Aeson.Types (parseMaybe)
import Data.Map.Strict qualified as Map
import Data.Text qualified as T
import Data.UUID qualified as UUID
import Entity.Arkham.Game
import Arkham.Card.CardCode (unCardCode)
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

-- Only the full Dream-Eaters game has independent teams in one game row.
-- Never use the partial campaign's throwing .part accessor for unknown state.
-- Overlay/identity metadata already lives in the independent CampaignAttrs.
campaignWriteBackNamespace :: CampaignId -> Value -> Maybe Text
campaignWriteBackNamespace cid metadata
  | cid /= CampaignId "06" = Just ""
  | otherwise = do
      (mode, current) <- parseMaybe (withObject "Dream-Eaters write-back identity" \o ->
        (,) <$> o .: "campaignMode" <*> o .:? "currentCampaignMode") metadata
      case mode of
        Dream.PartialMode _ -> Just ""
        Dream.FullMode -> case current of
          Just Dream.TheDreamQuest -> Just "dreamQuest"
          Just Dream.TheWebOfDreams -> Just "webOfDreams"
          Nothing -> Nothing

-- Both boundary enqueue and saved upgrade selection use the pre-transition
-- game, including its stable Homunculus origin and active independent part.
campaignWriteBackKey :: ArkhamGameId -> Game -> InvestigatorId -> Maybe Text
campaignWriteBackKey gameId game iid = do
  (stable, namespace) <- case modeCampaign game.gameMode of
    Nothing -> Just (iid, "")
    Just campaign -> do
      (_, stable) <- campaignWriteBackSeatIdentity game iid
      let attrs = toAttrs campaign
      namespace <- campaignWriteBackNamespace (campaignId attrs) (campaignMeta attrs)
      pure (stable, namespace)
  pure $ UUID.toText (coerce gameId) <> "/" <> unCardCode (unInvestigatorId stable)
    <> if T.null namespace then "" else "/" <> namespace

data CampaignKeyIdentity
  = LegacyCampaignKey ArkhamGameId
  | PartCampaignKey ArkhamGameId Dream.CampaignPart
  | InvalidCampaignKey
  deriving stock (Eq, Show)

data CampaignGameIdentity
  = MissingCampaignGame
  | OrdinaryCampaignGame
  | FullDreamEatersGame Dream.CampaignPart
  | MalformedCampaignGame
  deriving stock (Eq, Show)

-- New keys prove their part without consulting the current game/roster.
classifyCampaignKey :: Text -> CampaignKeyIdentity
classifyCampaignKey key = case T.splitOn "/" key of
  game : iid : suffix | not (T.null iid), Just uuid <- UUID.fromText game ->
    case suffix of
      [] -> LegacyCampaignKey $ coerce uuid
      ["dreamQuest"] -> PartCampaignKey (coerce uuid) Dream.TheDreamQuest
      ["webOfDreams"] -> PartCampaignKey (coerce uuid) Dream.TheWebOfDreams
      _ -> InvalidCampaignKey
  _ -> InvalidCampaignKey

campaignGameIdentity :: CampaignId -> Value -> CampaignGameIdentity
campaignGameIdentity cid metadata = case campaignWriteBackNamespace cid metadata of
  Just "" -> OrdinaryCampaignGame
  Just "dreamQuest" -> FullDreamEatersGame Dream.TheDreamQuest
  Just "webOfDreams" -> FullDreamEatersGame Dream.TheWebOfDreams
  _ -> MalformedCampaignGame

-- A present row with invalid metadata is NOT a deleted game.
campaignGameIdentityFromJSON :: Value -> CampaignGameIdentity
campaignGameIdentityFromJSON value = fromMaybe MalformedCampaignGame do
  (cid, metadata) <- parseMaybe (withObject "Campaign identity" \o -> (,) <$> o .: "id" <*> o .: "meta") value
  pure $ campaignGameIdentity cid metadata

legacyCampaignGameIds :: [Text] -> [ArkhamGameId]
legacyCampaignGameIds keys = ordNub [gid | LegacyCampaignKey gid <- map classifyCampaignKey keys]

-- Exactly one batch for deduplicated legacy candidates; new/invalid formats
-- and the already-captured game's identity never require another game read.
loadCampaignIdentitiesWith
  :: Monad m
  => ([ArkhamGameId] -> m (Map ArkhamGameId CampaignGameIdentity))
  -> Map ArkhamGameId CampaignGameIdentity
  -> [Text]
  -> m (Map ArkhamGameId CampaignGameIdentity)
loadCampaignIdentitiesWith loadGames known keys = do
  let needed = filter (`Map.notMember` known) $ legacyCampaignGameIds keys
  if null needed then pure known else Map.union known <$> loadGames needed

-- A deleted game's leftover association/link is inert. Only a legacy key
-- belonging to an EXISTING full (or malformed) game can taint another intent.
campaignKeyAmbiguous :: Map ArkhamGameId CampaignGameIdentity -> Text -> Bool
campaignKeyAmbiguous games key = case classifyCampaignKey key of
  LegacyCampaignKey gid -> case Map.findWithDefault MissingCampaignGame gid games of
    FullDreamEatersGame _ -> True
    MalformedCampaignGame -> True
    _ -> False
  _ -> False

campaignJobKeySafe :: Map ArkhamGameId CampaignGameIdentity -> Text -> Bool
campaignJobKeySafe games key = case classifyCampaignKey key of
  PartCampaignKey {} -> True
  LegacyCampaignKey gid -> Map.lookup gid games == Just OrdinaryCampaignGame
  InvalidCampaignKey -> False

-- Missing OWN legacy games cannot prove a part. Fence that job, not other
-- campaigns using the deck; unlike missing history, it would itself dispatch.
campaignAdmissionAllowed :: Map ArkhamGameId CampaignGameIdentity -> [Text] -> Text -> Bool
campaignAdmissionAllowed games related own =
  campaignJobKeySafe games own && not (any (campaignKeyAmbiguous games) related)

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
  in case Map.lookup (campaignDeckOverlayKey iid) store of
    Just proof -> withOverlayProvenance (Just proof) snapshot
    Nothing -> withoutExportedOverlay snapshot
