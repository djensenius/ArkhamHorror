module Api.Arkham.Types.Achievement (
  ClearAchievements (..),
) where

import Arkham.Achievement.Types (Achievement)
import Data.Aeson
import Data.Aeson.Types (parseFail)
import Relude

data ClearAchievements
  = ClearAll
  | ClearCampaign Text
  | ClearAchievement Achievement
  deriving stock (Eq, Show)

instance FromJSON ClearAchievements where
  parseJSON = withObject "ClearAchievements" \o ->
    (o .: "scope") >>= \case
      ("all" :: Text) -> pure ClearAll
      "campaign" -> ClearCampaign <$> o .: "campaign"
      "achievement" -> ClearAchievement <$> o .: "achievement"
      other -> parseFail $ "Unknown clear scope: " <> show other
