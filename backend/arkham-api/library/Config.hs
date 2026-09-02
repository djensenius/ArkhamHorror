{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Some next-gen helper functions for the scaffolding's configuration system.
module Config (
  -- * Locally defined
  configSettingsYml,
  getDevSettings,
  loadYamlSettingsValuesUseEnv,
  makeYesodLogger,

  -- * Re-exports from Data.Yaml.Config
  applyCurrentEnv,
  getCurrentEnv,
  applyEnvValue,
  loadYamlSettings,
  loadYamlSettingsArgs,
  EnvUsage,
  ignoreEnv,
  useEnv,
  requireEnv,
  useCustomEnv,
  requireCustomEnv,
) where

import Import.NoFoundation

import Data.Aeson (Result (..), fromJSON)
import Data.Aeson.KeyMap qualified as KeyMap
import Data.List (lookup)
import Data.Yaml.Config
import Network.Wai.Handler.Warp
import Network.Wai.Logger (clockDateCacher)
import System.Environment (getEnvironment)
import System.Log.FastLogger (LoggerSet)
import Yesod.Core.Types (Logger (Logger))

-- | Location of the default config file.
configSettingsYml :: FilePath
configSettingsYml = "config/settings.yml"

{- | Apply the @useEnv@ branch of 'loadYamlSettings' to settings values that
were already decoded from their runtime files.

The locale-catalog preflight must inspect a parsed runtime file before any
environment substitution and then consume that exact parsed value; rereading
the pathname through 'loadYamlSettings' would leave a time-of-check/time-of-use
gap. The merge and conversion here intentionally match
'Data.Yaml.Config.loadYamlSettings': earlier values take precedence
recursively, then the current environment is applied.
-}
loadYamlSettingsValuesUseEnv :: FromJSON settings => [Value] -> IO settings
loadYamlSettingsValuesUseEnv values = do
  value <- applyCurrentEnv False $ mergeSettingsValues values
  case fromJSON value of
    Error message -> error $ "Could not convert to expected type: " <> toText message
    Success settings -> pure settings

mergeSettingsValues :: [Value] -> Value
mergeSettingsValues = \case
  [] -> error "loadYamlSettings: No configuration provided"
  headValue : rest -> foldl' mergeValues headValue rest
 where
  mergeValues (Object left) (Object right) = Object $ KeyMap.unionWith mergeValues left right
  mergeValues left _ = left

{- | Helper for getApplicationRepl. Looks up PORT and DISPLAY_PORT and prints
 appropriate messages.
-}
getDevSettings :: Settings -> IO Settings
getDevSettings settings = do
  env <- getEnvironment
  let p = fromMaybe (getPort settings) $ lookup "PORT" env >>= readMaybe
      pdisplay = fromMaybe p $ lookup "DISPLAY_PORT" env >>= readMaybe
  putStrLn $ "Devel application launched: http://localhost:" ++ show pdisplay
  pure $ setPort p settings

{- | Create a 'Logger' value (from yesod-core) out of a 'LoggerSet' (from
 fast-logger).
-}
makeYesodLogger :: LoggerSet -> IO Logger
makeYesodLogger loggerSet' = do
  (getter, _) <- clockDateCacher
  pure $! Yesod.Core.Types.Logger loggerSet' getter
