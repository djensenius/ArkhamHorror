{-# LANGUAGE NoFieldSelectors #-}

module Base.Api.Types.Account (
  PasswordResetRequest (..),
  PasswordResetUpdate (..),
  SettingsUser (..),
  UserSettings (..),
) where

import Data.Aeson
import Relude

newtype PasswordResetRequest = PasswordResetRequest {resetEmail :: Text}
  deriving stock (Eq, Show)

instance FromJSON PasswordResetRequest where
  parseJSON = withObject "PasswordResetRequest" \o ->
    PasswordResetRequest <$> o .: "email"

newtype PasswordResetUpdate = PasswordResetUpdate {resetPassword :: Text}
  deriving stock (Eq, Show)

instance FromJSON PasswordResetUpdate where
  parseJSON = withObject "PasswordResetUpdate" \o ->
    PasswordResetUpdate <$> o .: "password"

newtype UserSettings = UserSettings {betaSetting :: Bool}
  deriving stock (Eq, Show)

instance FromJSON UserSettings where
  parseJSON = withObject "UserSettings" \o ->
    UserSettings <$> o .: "beta"

data SettingsUser = SettingsUser
  { username :: Text
  , email :: Text
  , beta :: Bool
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass ToJSON
