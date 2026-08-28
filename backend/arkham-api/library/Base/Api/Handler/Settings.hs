{-# LANGUAGE DuplicateRecordFields #-}

module Base.Api.Handler.Settings where

import Base.Api.Types.Account
import Database.Esqueleto.Experimental
import Import hiding (update, (=.), (==.))

newtype SiteSettings = SiteSettings
  { assetHost :: Maybe Text
  }

instance ToJSON SiteSettings where
  toJSON SiteSettings {assetHost} = object ["assetHost" .= assetHost]

getApiV1SiteSettingsR :: Handler SiteSettings
getApiV1SiteSettingsR = SiteSettings <$> getsApp (appAssetHost . appSettings)

putApiV1SettingsR :: Handler UpdatedUser
putApiV1SettingsR = do
  userId <- getRequestUserId
  settings <- requireCheckJsonBody :: Handler UserSettings
  runDB do
    update \u -> do
      set u [UserBeta =. val settings.betaSetting]
      where_ $ u.id ==. val userId
    User {..} <- get404 userId
    pure $ UpdatedUser userUsername userEmail userBeta
