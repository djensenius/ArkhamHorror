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

putApiV1SettingsR :: Handler SettingsUser
putApiV1SettingsR = do
  userId <- getRequestUserId
  settings <- requireCheckJsonBody :: Handler UserSettings
  runDB do
    let UserSettings mBeta mPhaseTransitionNotifications = settings
    update \u -> do
      for_ mBeta \value -> set u [UserBeta =. val value]
      for_ mPhaseTransitionNotifications \value ->
        set u [UserPhaseTransitionNotifications =. val value]
      where_ $ u.id ==. val userId
    User { userUsername, userEmail, userBeta, userPhaseTransitionNotifications } <- get404 userId
    pure $ SettingsUser userUsername userEmail userBeta userPhaseTransitionNotifications
