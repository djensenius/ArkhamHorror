module Arkham.ArkhamDB.Overlay where

import Arkham.Card.CardCode (unCardCode)
import Arkham.Custom.Overlay (DeckOverlay (..))
import Arkham.Id (InvestigatorId (..))
import Arkham.Message (Message (SetGlobal))
import Arkham.Target (Target (CampaignTarget))
import Data.Aeson (Value (..), toJSON, parseJSON, object, (.=))
import Data.Aeson.Key qualified as Key
import Data.Aeson.KeyMap qualified as KM
import Data.Aeson.Types (parseMaybe)
import Data.Map.Strict qualified as Map
import Data.UUID (UUID)
import Relude

campaignDeckOverlayKey :: InvestigatorId -> Text
campaignDeckOverlayKey iid = "ahn_overlay/" <> unCardCode (unInvestigatorId iid)

recordCampaignDeckOverlay :: InvestigatorId -> Maybe DeckOverlay -> Message
recordCampaignDeckOverlay iid overlay =
  SetGlobal CampaignTarget (Key.fromText $ campaignDeckOverlayKey iid) (toJSON overlay)

-- Null proves no attachment was materialized. Legacy captures without an id
-- remain distinguishable from a proven attachment; never upgrade them by value.
overlayAttachmentProvenance :: Maybe UUID -> Maybe DeckOverlay -> Value
overlayAttachmentProvenance attachment overlay = case (attachment, overlay) of
  (Just key, Just value) -> object ["attachmentId" .= key, "overlay" .= value]
  _ -> toJSON overlay

recordCampaignDeckAttachment :: InvestigatorId -> Maybe UUID -> Maybe DeckOverlay -> Message
recordCampaignDeckAttachment iid attachment overlay =
  -- A per-game overlay can supersede the saved overlay's investigator. Capture
  -- the final resolved seat alongside (not in place of) its attachment proof.
  -- Keeping both in the materialization record gives them the same lifecycle.
  SetGlobal CampaignTarget (Key.fromText $ campaignDeckOverlayKey iid) $
    case (overlay >>= overlayInvestigator, overlayAttachmentProvenance attachment overlay) of
      (Just _, Object proof) -> Object $ KM.insert "materializedInvestigator" (toJSON iid) proof
      (_, proof) -> proof

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
