module Arkham.ArkhamDB.Overlay where

import Arkham.Card.CardCode (unCardCode)
import Arkham.Custom.Overlay (DeckOverlay)
import Arkham.Id (InvestigatorId (..))
import Arkham.Message (Message (SetGlobal))
import Arkham.Target (Target (CampaignTarget))
import Data.Aeson (toJSON)
import Data.Aeson.Key qualified as Key
import Relude

campaignDeckOverlayKey :: InvestigatorId -> Text
campaignDeckOverlayKey iid = "ahn_overlay/" <> unCardCode (unInvestigatorId iid)

recordCampaignDeckOverlay :: InvestigatorId -> Maybe DeckOverlay -> Message
recordCampaignDeckOverlay iid overlay =
  SetGlobal CampaignTarget (Key.fromText $ campaignDeckOverlayKey iid) (toJSON overlay)
