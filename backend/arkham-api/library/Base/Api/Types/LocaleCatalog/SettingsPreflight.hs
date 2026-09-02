{- | Fail-closed validation for locale-catalog environment mappings in runtime
YAML settings files.

'Data.Yaml.Config.loadYamlSettingsArgs' decodes every command-line file with
@!include@ support, left-biases them over the embedded settings value, and only
then substitutes @_env:@ markers. This module deliberately examines every
source first: an overridden file must not be able to hide a noncanonical
environment alias behind a later, safe-looking value.
-}
module Base.Api.Types.LocaleCatalog.SettingsPreflight (
  preflightLocaleCatalogSettings,
  validateLocaleCatalogSettingsValues,
) where

import Base.Api.Types.LocaleCatalog (
  LocaleCatalogSetting,
  localeCatalogSettingEnvVar,
  localeCatalogSettingKey,
  validateLocaleCatalogEnvironment,
  validateLocaleCatalogRawEnvironmentValue,
 )
import Data.Aeson (Value (..))
import Data.Aeson.Key qualified as Key
import Data.Aeson.KeyMap qualified as KeyMap
import Data.Conduit ((.|), runConduitRes)
import Data.Conduit.List qualified as Conduit
import Data.Set qualified as Set
import Data.Text qualified as Text
import Data.Yaml.Include qualified as YamlInclude
import Relude
import System.Directory (canonicalizePath)
import System.FilePath ((</>), takeDirectory)
import Text.Libyaml qualified as Libyaml

{- | Decode and preflight exactly the runtime files and compile-time values
that 'Data.Yaml.Config.loadYamlSettingsArgs' would load, before it can
substitute any environment variable. The returned values are the exact
runtime-file values the caller must merge and load.
-}
preflightLocaleCatalogSettings
  :: [FilePath]
  -> [Value]
  -> [(Text, Text)]
  -> IO [Value]
preflightLocaleCatalogSettings runtimeFiles compileValues environment = do
  runtimeValues <- traverse loadRuntimeSettingsValue runtimeFiles
  either (fail . toString) pure
    $ validateLocaleCatalogSettingsValues (runtimeValues <> compileValues)
  either (fail . toString) pure $ validateLocaleCatalogEnvironment environment
  traverse_
    (\(name, value) ->
      for_ (settingForEnvironmentVariable name) \setting ->
        either (fail . toString) pure $ validateLocaleCatalogRawEnvironmentValue setting value
    )
    environment
  pure runtimeValues

-- | Check decoded, unresolved-for-environment settings values. This is
-- exported for focused specs; production callers should use
-- 'preflightLocaleCatalogSettings' so duplicate YAML keys and
-- @!include@ sources are checked too.
validateLocaleCatalogSettingsValues :: [Value] -> Either Text ()
validateLocaleCatalogSettingsValues = traverse_ validateValue

loadRuntimeSettingsValue :: FilePath -> IO Value
loadRuntimeSettingsValue path = do
  resolved <-
    YamlInclude.decodeFileEither path >>= \case
      Left _ -> fail "locale catalog configuration is invalid: unable to parse a runtime settings file"
      Right value -> pure value
  validateRawYamlFile path
  pure resolved

validateRawYamlFile :: FilePath -> IO ()
validateRawYamlFile = go []
 where
  go seen path = do
    canonicalPath <- canonicalizePath path
    when (canonicalPath `elem` seen)
      $ fail "locale catalog configuration is invalid: cyclic runtime settings include"
    events <- runConduitRes $ Libyaml.decodeFile canonicalPath .| Conduit.consume
    either (fail . toString) pure $ validateNoDuplicateLocaleCatalogKeys events
    includes <- either (fail . toString) pure $ includePaths events
    traverse_ (go (canonicalPath : seen) . (takeDirectory canonicalPath </>)) includes

validateValue :: Value -> Either Text ()
validateValue = \case
  Object object ->
    traverse_
      (\(key, value) -> do
        for_ (settingForKey $ Key.toText key) (`validateEnvironmentMapping` value)
        validateValue value
      )
      (KeyMap.toList object)
  Array values -> traverse_ validateValue values
  _ -> Right ()

settingForKey :: Text -> Maybe LocaleCatalogSetting
settingForKey key =
  find (\setting -> localeCatalogSettingKey setting == key) [minBound .. maxBound]

settingForEnvironmentVariable :: Text -> Maybe LocaleCatalogSetting
settingForEnvironmentVariable name =
  find (\setting -> localeCatalogSettingEnvVar setting == name) [minBound .. maxBound]

validateEnvironmentMapping :: LocaleCatalogSetting -> Value -> Either Text ()
validateEnvironmentMapping setting = \case
  String marker
    | Just suffix <- Text.stripPrefix "_env:" marker ->
        case Text.break (== ':') suffix of
          (name, rest)
            | Text.null rest ->
                reject setting "is an incomplete _env: mapping"
            | name == localeCatalogSettingEnvVar setting -> Right ()
            | otherwise ->
                reject setting "must use only its canonical ARKHAM_LOCALE_CATALOG_* environment variable"
  _ -> Right ()

validateNoDuplicateLocaleCatalogKeys :: [Libyaml.Event] -> Either Text ()
validateNoDuplicateLocaleCatalogKeys = parseDocuments . filter significantEvent
 where
  significantEvent = \case
    Libyaml.EventStreamStart -> False
    Libyaml.EventStreamEnd -> False
    Libyaml.EventDocumentStart -> False
    Libyaml.EventDocumentEnd -> False
    _ -> True

parseDocuments :: [Libyaml.Event] -> Either Text ()
parseDocuments = go
 where
  go [] = Right ()
  go events = parseNode events >>= go

parseNode :: [Libyaml.Event] -> Either Text [Libyaml.Event]
parseNode = \case
  Libyaml.EventAlias _ : rest -> Right rest
  Libyaml.EventScalar _ _ _ _ : rest -> Right rest
  Libyaml.EventSequenceStart _ _ _ : rest -> parseSequence rest
  Libyaml.EventMappingStart _ _ _ : rest -> parseMapping Set.empty rest
  _ -> Left "locale catalog configuration is invalid: malformed runtime settings YAML"

parseSequence :: [Libyaml.Event] -> Either Text [Libyaml.Event]
parseSequence = \case
  Libyaml.EventSequenceEnd : rest -> Right rest
  events -> parseNode events >>= parseSequence

parseMapping :: Set LocaleCatalogSetting -> [Libyaml.Event] -> Either Text [Libyaml.Event]
parseMapping seen = \case
  Libyaml.EventMappingEnd : rest -> Right rest
  events -> do
    (key, afterKey) <- parseMappingKey events
    setting <- case settingForKey key of
      Just localeSetting
        | localeSetting `Set.member` seen ->
            reject localeSetting "is represented more than once in one YAML mapping"
        | otherwise -> Right $ Set.insert localeSetting seen
      Nothing -> Right seen
    afterValue <- parseNode afterKey
    parseMapping setting afterValue

parseMappingKey :: [Libyaml.Event] -> Either Text (Text, [Libyaml.Event])
parseMappingKey = \case
  Libyaml.EventScalar bytes _ _ _ : rest ->
    first
      (const "locale catalog configuration is invalid: runtime settings YAML has a non-text mapping key")
      ((,rest) <$> decodeUtf8' bytes)
  Libyaml.EventAlias _ : _ ->
    Left "locale catalog configuration is invalid: runtime settings YAML uses an ambiguous alias as a mapping key"
  _ -> Left "locale catalog configuration is invalid: malformed runtime settings YAML"

includePaths :: [Libyaml.Event] -> Either Text [FilePath]
includePaths = traverse includePath . filter isInclude
 where
  isInclude = \case
    Libyaml.EventScalar _ (Libyaml.UriTag "!include") _ _ -> True
    _ -> False

  includePath = \case
    Libyaml.EventScalar bytes (Libyaml.UriTag "!include") _ _ ->
      first
        (const "locale catalog configuration is invalid: runtime settings YAML has a non-text !include path")
        (toString <$> decodeUtf8' bytes)
    _ -> Left "locale catalog configuration is invalid: malformed runtime settings YAML"

reject :: LocaleCatalogSetting -> Text -> Either Text a
reject setting reason =
  Left
    $ "locale catalog configuration is invalid: "
    <> localeCatalogSettingKey setting
    <> " ("
    <> localeCatalogSettingEnvVar setting
    <> ") "
    <> reason
