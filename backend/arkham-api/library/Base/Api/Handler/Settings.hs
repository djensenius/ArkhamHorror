{-# LANGUAGE DuplicateRecordFields #-}

module Base.Api.Handler.Settings where

import Base.Api.Types.Account
import Database.Esqueleto.Experimental
import Import hiding (update, (=.), (==.))

data SiteSettings = SiteSettings
  { assetHost :: Maybe Text
  , maintenance :: Bool
  }

instance ToJSON SiteSettings where
  toJSON SiteSettings {assetHost, maintenance} =
    object ["assetHost" .= assetHost, "maintenance" .= maintenance]

{- | Allowed through 'Foundation.maintenanceMiddleware', and so the one thing
the client can still ask during a window. It reads no database -- the asset
host is a setting and the flag is a 'TVar' -- which is what makes it safe to
leave open.
-}
getApiV1SiteSettingsR :: Handler SiteSettings
getApiV1SiteSettingsR = do
  assetHost <- getsApp (appAssetHost . appSettings)
  maintenance <- isJust <$> (getsApp appMaintenance >>= readTVarIO)
  pure SiteSettings {..}

putApiV1SettingsR :: Handler SettingsUser
putApiV1SettingsR = do
  userId <- getRequestUserId
  settings <- requireCheckJsonBody :: Handler UserSettings
  runDB do
    let UserSettings mBeta mPhaseTransitionNotifications mAllowSpectatorsByDefault = settings
    update \u -> do
      for_ mBeta \value -> set u [UserBeta =. val value]
      for_ mPhaseTransitionNotifications \value ->
        set u [UserPhaseTransitionNotifications =. val value]
      for_ mAllowSpectatorsByDefault \value ->
        set u [UserAllowSpectatorsByDefault =. val value]
      where_ $ u.id ==. val userId
    User
      { userUsername
      , userEmail
      , userBeta
      , userPhaseTransitionNotifications
      , userAllowSpectatorsByDefault
      } <-
      get404 userId
    pure
      $ SettingsUser
        userUsername
        userEmail
        userBeta
        userPhaseTransitionNotifications
        userAllowSpectatorsByDefault
