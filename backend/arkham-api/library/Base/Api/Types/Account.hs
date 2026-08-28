{-# LANGUAGE NoFieldSelectors #-}

module Base.Api.Types.Account (
  PasswordResetPassword (..),
  PasswordResetRequest (..),
  UpdatedUser (..),
  UserSettings (..),
) where

import Data.Aeson
import Relude

newtype PasswordResetRequest = PasswordResetRequest {resetEmail :: Text}
  deriving stock (Eq, Show)

instance FromJSON PasswordResetRequest where
  parseJSON = withObject "PasswordResetRequest" \o ->
    PasswordResetRequest <$> o .: "email"

newtype PasswordResetPassword = PasswordResetPassword {resetPassword :: Text}
  deriving stock (Eq, Show)

instance FromJSON PasswordResetPassword where
  parseJSON = withObject "PasswordResetPassword" \o ->
    PasswordResetPassword <$> o .: "password"

newtype UserSettings = UserSettings {betaSetting :: Bool}
  deriving stock (Eq, Show)

instance FromJSON UserSettings where
  parseJSON = withObject "UserSettings" \o ->
    UserSettings <$> o .: "beta"

data UpdatedUser = UpdatedUser
  { username :: Text
  , email :: Text
  , beta :: Bool
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass ToJSON
