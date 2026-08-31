{- | Parse 'AppSettings' the way the running server does.

@Application.appMain@ loads @config\/settings.yml@ (embedded as
'configSettingsYmlValue') and applies the environment with
@Data.Yaml.Config.useEnv@, which is @applyEnvValue False@. Doing exactly that
here — rather than hand-building an 'AppSettings' or a synthetic @Value@ —
means a test exercises the real settings keys, the real @_env:@ defaults, and
the real 'FromJSON' instance, so a startup failure a deployment would hit is a
test failure here too.
-}
module Helpers.AppSettings (
  loadAppSettings,
  overrideEnv,
) where

import Data.Aeson (Result (..), fromJSON)
import Data.Aeson.Key qualified as AesonKey
import Data.Aeson.KeyMap qualified as AesonKeyMap
import Data.Yaml.Config (applyEnvValue)
import Relude
import Settings (AppSettings, configSettingsYmlValue)

loadAppSettings :: [(Text, Text)] -> Either Text AppSettings
loadAppSettings environment =
  case fromJSON (applyEnvValue False (asEnvironment environment) configSettingsYmlValue) of
    Error message -> Left (toText message)
    Success settings -> Right settings
 where
  asEnvironment = AesonKeyMap.fromList . map (first AesonKey.fromText)

-- | Apply overrides to an environment, last write winning.
overrideEnv :: [(Text, Text)] -> [(Text, Text)] -> [(Text, Text)]
overrideEnv overrides environment =
  [entry | entry@(name, _) <- environment, name `notElem` map fst overrides] <> overrides
