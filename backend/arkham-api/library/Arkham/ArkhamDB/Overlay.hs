module Arkham.ArkhamDB.Overlay where

import Arkham.Card.CardCode (unCardCode)
import Arkham.Custom.Overlay (DeckOverlay)
import Arkham.Id (InvestigatorId (..))
import Arkham.Message (Message (SetGlobal))
import Arkham.Target (Target (CampaignTarget))
import Data.Aeson (Value, toJSON, parseJSON)
import Data.Aeson.Key qualified as Key
import Data.Aeson.Types (parseMaybe)
import Data.Map.Strict qualified as Map
import Relude

campaignDeckOverlayKey :: InvestigatorId -> Text
campaignDeckOverlayKey iid = "ahn_overlay/" <> unCardCode (unInvestigatorId iid)

recordCampaignDeckOverlay :: InvestigatorId -> Maybe DeckOverlay -> Message
recordCampaignDeckOverlay iid overlay =
  SetGlobal CampaignTarget (Key.fromText $ campaignDeckOverlayKey iid) (toJSON overlay)

campaignDeckIdentityKey :: InvestigatorId -> Text
campaignDeckIdentityKey iid = "ahn_write_back_identity/" <> unCardCode (unInvestigatorId iid)

-- This is write-back identity only: gameplay keeps its own live id until reset.
-- Null marks an unknown/colliding origin; never guess a new XP lineage.
moveCampaignDeckIdentity :: InvestigatorId -> InvestigatorId -> [InvestigatorId] -> Map Text Value -> Map Text Value
moveCampaignDeckIdentity source target occupied store =
  let origin = do
        guard $ source `elem` occupied && (source == target || target `notElem` occupied)
        case Map.lookup (campaignDeckIdentityKey source) store of
          Nothing -> Just source
          Just value -> parseMaybe parseJSON value
  in Map.insert (campaignDeckIdentityKey target) (toJSON origin) $
    Map.delete (campaignDeckIdentityKey source) store
