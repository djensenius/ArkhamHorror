{- | Regressions for the one startup settings snapshot.

'Base.Api.Types.LocaleCatalog.SettingsPreflight' is only trustworthy if it
loads exactly what @Data.Yaml.Config.loadYamlSettings@ would load, so the
equivalence group below does not describe that loader in prose: it runs the
package's own function over the same files, values and environment and
requires an identical result, including nested @!include@s, a
right-associated left-biased merge and @_env:@ substitution.

The remaining groups cover what the snapshot adds: raw provenance (a
noncanonical locale mapping is refused wherever it appears, even behind an
anchor, an alias, a @\<\<@ merge, an @!include@ or a source another source
overrides), immutability (the environment, the settings bytes and the include
bytes this process loaded cannot be changed underneath it), and the raw
environment grammar (checked exactly for the canonical markers that survive
the effective merge, and for nothing else).

These examples mutate the process environment and files on disk, so this
module runs 'sequential' inside the suite's parallel default.
-}
module Arkham.Api.LocaleCatalogSettingsPreflightSpec (spec) where

import Base.Api.Types.LocaleCatalog (
  LocaleCatalogSetting (..),
  localeCatalogSettingEnvVar,
  localeCatalogSettingKey,
 )
import Base.Api.Types.LocaleCatalog.SettingsPreflight (
  captureSettingsSnapshot,
  captureSettingsSnapshotWithEnvironment,
  loadSettingsSnapshot,
  mergeSettingsValues,
  settingsSnapshotFromValues,
 )
import Control.Exception qualified as Exception
import Data.Aeson (Value (..), object, (.=))
import Data.Aeson.Key qualified as Key
import Data.Aeson.KeyMap (KeyMap)
import Data.Aeson.KeyMap qualified as KeyMap
import Data.Text qualified as Text
import Data.Yaml qualified as Yaml
import Data.Yaml.Config qualified as YamlConfig
import Relude
import System.Directory (
  createDirectoryIfMissing,
  createFileLink,
  getCurrentDirectory,
  removeFile,
  removePathForcibly,
  renameFile,
 )
import System.Environment (setEnv, unsetEnv)
import System.FilePath ((</>), takeFileName)
import Test.Hspec

spec :: Spec
spec = sequential $ describe "locale catalog settings snapshot" do
  describe "loader equivalence" do
    it "merges values exactly as the package's right-associated left-biased merge does" do
      for_ mergeMatrix \values -> do
        fromPackage <- YamlConfig.loadYamlSettings [] values YamlConfig.ignoreEnv :: IO Value
        mergeSettingsValues values `shouldBe` fromPackage

    it "applies the captured environment exactly as the package applies its own" do
      for_ (environmentMatrix <> mergeMatrix) \values -> do
        fromSnapshot <- loadSettingsSnapshot (settingsSnapshotFromValues loaderEnvironment values) :: IO Value
        fromPackage <-
          YamlConfig.loadYamlSettings [] values (YamlConfig.useCustomEnv loaderEnvironment) :: IO Value
        fromSnapshot `shouldBe` fromPackage

    it "matches the package over runtime files, nested includes and embedded defaults" do
      withSettingsWorkspace "loader-equivalence" \workspace -> do
        let root = workspace </> "root.yml"
            included = workspace </> "included.yml"
            nested = workspace </> "nested.yml"
            secondary = workspace </> "secondary.yml"
            embedded =
              encodeUtf8
                @Text
                ( "shared:\n"
                    <> "  from: embedded\n"
                    <> "  only-embedded: true\n"
                    <> "locale-catalog-default-locale: \"_env:ARKHAM_LOCALE_CATALOG_DEFAULT_LOCALE:\"\n"
                )
            environment =
              environmentMap
                [ ("ARKHAM_LOCALE_CATALOG_DEFAULT_LOCALE", "fr")
                , ("SHARED_DEPTH", "2")
                ]
        writeSettings root
          $ unlines
            [ "shared:"
            , "  from: root"
            , "  depth: \"_env:SHARED_DEPTH:1\""
            , "included: !include included.yml"
            ]
        writeSettings included
          $ unlines
            [ "nested: !include nested.yml"
            , "value: from-include"
            ]
        writeSettings nested "deep: from-nested\n"
        writeSettings secondary
          $ unlines
            [ "shared:"
            , "  from: secondary"
            , "  only-secondary: true"
            ]
        fromSnapshot <- captureAndLoad [root, secondary] [embedded] environment
        fromPackage <-
          YamlConfig.loadYamlSettings
            [root, secondary]
            [decodeSettingsBytes embedded]
            (YamlConfig.useCustomEnv environment)
            :: IO Value
        fromSnapshot `shouldBe` fromPackage

    it "keeps an earlier runtime file ahead of a later file and of the embedded defaults" do
      withSettingsWorkspace "loader-precedence" \workspace -> do
        let firstFile = workspace </> "first.yml"
            secondFile = workspace </> "second.yml"
            embedded = encodeUtf8 @Text "settings:\n  a: embedded\n  b: embedded\n  c: embedded\n"
        writeSettings firstFile "settings:\n  a: first\n"
        writeSettings secondFile "settings:\n  a: second\n  b: second\n"
        value <- captureAndLoad [firstFile, secondFile] [embedded] mempty
        value
          `shouldBe` object
            [ "settings"
                .= object ["a" .= ("first" :: Text), "b" .= ("second" :: Text), "c" .= ("embedded" :: Text)]
            ]

    it "matches the package for an empty runtime file and a comment-only one" do
      withSettingsWorkspace "empty-sources" \workspace -> do
        let emptyFile = workspace </> "empty.yml"
            commentFile = workspace </> "comments.yml"
            embedded = encodeUtf8 @Text "settings:\n  a: embedded\n"
        writeSettings emptyFile ""
        writeSettings commentFile "# nothing but a comment\n"
        for_ [emptyFile, commentFile] \source -> do
          fromSnapshot <- captureAndLoad [source] [embedded] mempty
          fromPackage <-
            YamlConfig.loadYamlSettings [source] [decodeSettingsBytes embedded] YamlConfig.ignoreEnv
              :: IO Value
          fromSnapshot `shouldBe` fromPackage

  describe "raw locale mapping provenance" do
    it "rejects a noncanonical marker reached through an alias key and a merge" do
      withSettingsWorkspace "alias-merge" \workspace -> do
        let settings = workspace </> "alias-merge.yml"
        writeSettings settings
          $ unlines
            [ "locale-key: &locale_key locale-catalog-default-locale"
            , "base: &base"
            , "  *locale_key: \"_env:LOCALE_ALIAS:\""
            , "<<: *base"
            ]
        captureAndLoad [settings] [] (environmentMap [("LOCALE_ALIAS", "en")])
          `shouldFailWith` "must use only its canonical"

    it "rejects a noncanonical marker an included merge source contributes" do
      withSettingsWorkspace "include-merge" \workspace -> do
        let parent = workspace </> "parent.yml"
            included = workspace </> "included.yml"
        writeSettings included "locale-catalog-default-locale: \"_env:LOCALE_ALIAS:\"\n"
        writeSettings parent "<<: !include included.yml\n"
        captureAndLoad [parent] [] (environmentMap [("LOCALE_ALIAS", "en")])
          `shouldFailWith` "ARKHAM_LOCALE_CATALOG_DEFAULT_LOCALE"

    it "rejects a noncanonical marker a higher-precedence source overrides" do
      withSettingsWorkspace "shadowed-noncanonical" \workspace -> do
        let literal = workspace </> "literal.yml"
            noncanonical = workspace </> "noncanonical.yml"
        writeSettings literal "locale-catalog-default-locale: en\n"
        writeSettings noncanonical "locale-catalog-default-locale: \"_env:LOCALE_ALIAS:\"\n"
        captureAndLoad [literal, noncanonical] [] (environmentMap [("LOCALE_ALIAS", "en")])
          `shouldFailWith` "must use only its canonical"

    it "rejects a noncanonical marker in the embedded compile-time settings" do
      captureAndLoad [] [encodeUtf8 @Text "locale-catalog-revision: \"_env:LOCALE_ALIAS:\"\n"] mempty
        `shouldFailWith` "must use only its canonical"

    it "rejects an incomplete _env: mapping for every locale setting" do
      withSettingsWorkspace "incomplete-marker" \workspace ->
        for_ [minBound .. maxBound] \setting -> do
          let settings = workspace </> (toString (localeCatalogSettingKey setting) <> "-incomplete.yml")
          writeSettings settings
            $ localeCatalogSettingKey setting
            <> ": \"_env:"
            <> localeCatalogSettingEnvVar setting
            <> "\"\n"
          captureAndLoad [settings] [] mempty `shouldFailWith` "is an incomplete _env: mapping"

    it "rejects a locale key duplicated by a scalar alias and a direct key" do
      withSettingsWorkspace "alias-duplicate" \workspace -> do
        let settings = workspace </> "alias-duplicate.yml"
        writeSettings settings
          $ unlines
            [ "locale-key: &locale_key locale-catalog-default-locale"
            , "*locale_key: en"
            , "locale-catalog-default-locale: fr"
            ]
        captureAndLoad [settings] [] mempty `shouldFailWith` "is represented more than once"

    it "rejects ambiguous locale keys contributed by a sequence merge" do
      withSettingsWorkspace "sequence-merge" \workspace -> do
        let settings = workspace </> "sequence-merge.yml"
        writeSettings settings
          $ unlines
            [ "first: &first"
            , "  locale-catalog-default-locale: en"
            , "second: &second"
            , "  locale-catalog-default-locale: fr"
            , "<<: [*first, *second]"
            ]
        captureAndLoad [settings] [] mempty `shouldFailWith` "is represented more than once"

    it "rejects a noncanonical marker nested under a locale settings key" do
      withSettingsWorkspace "nested-marker" \workspace -> do
        let settings = workspace </> "nested-marker.yml"
        writeSettings settings "locale-catalog-locales: [\"_env:LOCALE_ALIAS:\"]\n"
        captureAndLoad [settings] [] (environmentMap [("LOCALE_ALIAS", "en")])
          `shouldFailWith` "must use only its canonical"

    it "accepts canonical markers behind anchors, aliases and merge keys" do
      withSettingsWorkspace "canonical-anchors" \workspace -> do
        let settings = workspace </> "canonical-anchors.yml"
            environment =
              environmentMap [(localeCatalogSettingEnvVar DefaultLocaleSetting, "en")]
        writeSettings settings
          $ unlines
            ( ["locale-catalog-base: &locale_catalog"]
                <> [ "  "
                    <> localeCatalogSettingKey setting
                    <> ": \"_env:"
                    <> localeCatalogSettingEnvVar setting
                    <> ":\""
                   | setting <- [minBound .. maxBound]
                   ]
                <> ["<<: *locale_catalog"]
            )
        fromSnapshot <- captureAndLoad [settings] [] environment
        fromPackage <-
          YamlConfig.loadYamlSettings [settings] [] (YamlConfig.useCustomEnv environment) :: IO Value
        fromSnapshot `shouldBe` fromPackage
        settingValue DefaultLocaleSetting fromSnapshot `shouldBe` Just (String "en")

    it "preserves unrelated aliases and merge keys the standard loader accepts" do
      withSettingsWorkspace "unrelated-aliases" \workspace -> do
        let settings = workspace </> "unrelated.yml"
        writeSettings settings
          $ unlines
            [ "alias-source: &ordinary ordinary-key"
            , "*ordinary: harmless"
            , "defaults: &defaults"
            , "  retries: 3"
            , "service:"
            , "  <<: *defaults"
            , "  name: api"
            ]
        fromSnapshot <- captureAndLoad [settings] [] mempty
        fromPackage <- YamlConfig.loadYamlSettings [settings] [] YamlConfig.ignoreEnv :: IO Value
        fromSnapshot `shouldBe` fromPackage
        lookupKey "ordinary-key" fromSnapshot `shouldBe` Just (String "harmless")

  describe "immutable snapshots" do
    it "loads the environment captured before the process environment changed" do
      withSettingsWorkspace "environment-snapshot" \workspace -> do
        let settings = workspace </> "settings.yml"
            name = toString (localeCatalogSettingEnvVar DefaultLocaleSetting)
        writeSettings settings (canonicalMarkerSource DefaultLocaleSetting)
        withEnvironmentVariable name "en" do
          snapshot <- captureSettingsSnapshot [settings] []
          setEnv name "fr"
          value <- loadSettingsSnapshot snapshot :: IO Value
          settingValue DefaultLocaleSetting value `shouldBe` Just (String "en")

    it "loads the root settings bytes captured before the path was replaced" do
      withSettingsWorkspace "root-snapshot" \workspace -> do
        let root = workspace </> "settings.yml"
            replacement = workspace </> "replacement.yml"
        writeSettings root "root: before\n"
        snapshot <- captureSettingsSnapshotWithEnvironment [root] [] mempty
        writeSettings replacement "root: after\n"
        renameFile replacement root
        value <- loadSettingsSnapshot snapshot :: IO Value
        value `shouldBe` object ["root" .= ("before" :: Text)]

    it "validates and loads the same captured bytes and the same captured environment" do
      withSettingsWorkspace "validated-bytes" \workspace -> do
        let settings = workspace </> "settings.yml"
            name = toString (localeCatalogSettingEnvVar DefaultLocaleSetting)
        writeSettings settings (canonicalMarkerSource DefaultLocaleSetting)
        withEnvironmentVariable name "en" do
          snapshot <- captureSettingsSnapshot [settings] []
          -- Both inputs the preflight accepted now become inputs it would
          -- refuse; a reread at load time would either fail or publish them.
          writeSettings settings "locale-catalog-default-locale: \"_env:LOCALE_ALIAS:\"\n"
          setEnv name "en\r"
          value <- loadSettingsSnapshot snapshot :: IO Value
          settingValue DefaultLocaleSetting value `shouldBe` Just (String "en")

    it "loads the include bytes captured before the included file was rewritten" do
      withSettingsWorkspace "include-snapshot" \workspace -> do
        let root = workspace </> "settings.yml"
            included = workspace </> "included.yml"
        writeSettings root "!include included.yml\n"
        writeSettings included "included: before\n"
        snapshot <- captureSettingsSnapshotWithEnvironment [root] [] mempty
        writeSettings included "included: after\n"
        value <- loadSettingsSnapshot snapshot :: IO Value
        value `shouldBe` object ["included" .= ("before" :: Text)]

    it "loads the symlink target bytes captured before the link was retargeted" do
      withSettingsWorkspace "symlink-snapshot" \workspace -> do
        let linked = workspace </> "linked.yml"
        writeSettings (workspace </> "first-target.yml") "linked: first\n"
        writeSettings (workspace </> "second-target.yml") "linked: second\n"
        createFileLink "first-target.yml" linked
        snapshot <- captureSettingsSnapshotWithEnvironment [linked] [] mempty
        removeFile linked
        createFileLink "second-target.yml" linked
        value <- loadSettingsSnapshot snapshot :: IO Value
        value `shouldBe` object ["linked" .= ("first" :: Text)]

  describe "raw environment values" do
    it "refuses a prohibited raw value for every canonical marker that survives the merge" do
      withSettingsWorkspace "prohibited-raw" \workspace ->
        for_ [minBound .. maxBound] \setting ->
          for_ prohibitedRawValues \(label, forbidden) -> do
            let settings =
                  workspace
                    </> (toString (localeCatalogSettingKey setting) <> "-" <> label <> ".yml")
                raw =
                  if setting == SupportedLocalesSetting
                    then "en" <> forbidden <> ",de"
                    else "trusted" <> forbidden <> "value"
            writeSettings settings (canonicalMarkerSource setting)
            captureAndLoad [settings] [] (environmentMap [(localeCatalogSettingEnvVar setting, raw)])
              `shouldFailWith` "prohibited raw"

    it "refuses a raw value its own setting's grammar rejects" do
      withSettingsWorkspace "invalid-raw" \workspace -> do
        let settings = workspace </> "sha256.yml"
        writeSettings settings (canonicalMarkerSource ManifestSha256Setting)
        captureAndLoad
          [settings]
          []
          (environmentMap [(localeCatalogSettingEnvVar ManifestSha256Setting, "deadbeef")])
          `shouldFailWith` "has an invalid raw environment value"

    it "ignores an invalid raw value no surviving marker can reach" do
      withSettingsWorkspace "ignored-raw" \workspace -> do
        let literal = workspace </> "literal.yml"
            marker = workspace </> "marker.yml"
        writeSettings literal "locale-catalog-default-locale: en\n"
        writeSettings marker (canonicalMarkerSource DefaultLocaleSetting)
        value <-
          captureAndLoad
            [literal, marker]
            []
            (environmentMap [(localeCatalogSettingEnvVar DefaultLocaleSetting, "en\r")])
        settingValue DefaultLocaleSetting value `shouldBe` Just (String "en")

    it "ignores an invalid value for a variable no locale setting names" do
      withSettingsWorkspace "unrelated-variable" \workspace -> do
        let settings = workspace </> "settings.yml"
        writeSettings settings (canonicalMarkerSource DefaultLocaleSetting)
        value <-
          captureAndLoad
            [settings]
            []
            ( environmentMap
                [ (localeCatalogSettingEnvVar DefaultLocaleSetting, "en")
                , ("LOCALE_ALIAS", "en\r")
                ]
            )
        settingValue DefaultLocaleSetting value `shouldBe` Just (String "en")

  describe "bounded traversal" do
    it "refuses an include chain deeper than the configured limit" do
      withSettingsWorkspace "deep-includes" \workspace -> do
        let files = [workspace </> ("include-" <> show number <> ".yml") | number <- [(0 :: Int) .. 40]]
        for_ (zip files (drop 1 files)) \(parent, child) ->
          writeSettings parent ("!include " <> toText (takeFileName child) <> "\n")
        for_ (viaNonEmpty last files) \deepest -> writeSettings deepest "answer: final\n"
        for_ (viaNonEmpty head files) \root ->
          captureAndLoad [root] [] mempty `shouldFailWith` "include depth exceeds"

    it "refuses a cyclic include" do
      withSettingsWorkspace "cyclic-include" \workspace -> do
        let firstFile = workspace </> "first.yml"
            secondFile = workspace </> "second.yml"
        writeSettings firstFile "!include second.yml\n"
        writeSettings secondFile "!include first.yml\n"
        captureAndLoad [firstFile] [] mempty `shouldFailWith` "cyclic settings include"

    it "refuses an alias that references its own incomplete anchor" do
      withSettingsWorkspace "cyclic-alias" \workspace -> do
        let settings = workspace </> "cyclic-alias.yml"
        writeSettings settings "cycle: &cycle [*cycle]\n"
        captureAndLoad [settings] [] mempty `shouldFailWith` "anchor that is not defined yet"

-- | Value lists whose merge alone distinguishes association order and bias.
mergeMatrix :: [[Value]]
mergeMatrix =
  [ [object ["x" .= (1 :: Int)]]
  , [object [], object ["x" .= (1 :: Int)]]
  , [object [], Null, object ["x" .= (1 :: Int)]]
  , [Null, object ["x" .= (1 :: Int)], object ["x" .= (2 :: Int)]]
  , [object ["x" .= (1 :: Int)], String "scalar", object ["x" .= (2 :: Int)]]
  , [String "root", object ["x" .= (1 :: Int)]]
  , [object ["x" .= object ["nested" .= (1 :: Int)]], object ["x" .= Null]]
  , [object ["x" .= Null], object ["x" .= object ["nested" .= (1 :: Int)]]]
  , [ object ["a" .= object ["b" .= (1 :: Int)]]
    , object ["a" .= object ["c" .= (2 :: Int)], "d" .= (3 :: Int)]
    ]
  , -- Association order is the whole difference between this merge and a
    -- left fold: right-associated, the middle scalar hides the last object.
    [ object ["a" .= object ["b" .= (1 :: Int)]]
    , object ["a" .= String "scalar"]
    , object ["a" .= object ["c" .= (2 :: Int)]]
    ]
  ]

-- | Values whose @_env:@ markers exercise every branch of 'applyEnvValue'.
environmentMatrix :: [[Value]]
environmentMatrix =
  [
    [ object
        [ "string" .= ("_env:PLAIN:default" :: Text)
        , "number" .= ("_env:NUMBER:1" :: Text)
        , "quoted" .= ("_env:PADDED:\"1\"" :: Text)
        , "bare" .= ("_env:PLAIN" :: Text)
        , "missing" .= ("_env:ABSENT" :: Text)
        , "missing-default" .= ("_env:ABSENT:fallback" :: Text)
        , "nested" .= [("_env:NUMBER:0" :: Text)]
        , "untouched" .= ("plain value" :: Text)
        ]
    ]
  ]

loaderEnvironment :: KeyMap Text
loaderEnvironment =
  environmentMap [("PLAIN", "text"), ("NUMBER", "2"), ("PADDED", "007")]

prohibitedRawValues :: [(String, Text)]
prohibitedRawValues =
  [("cr", "\r"), ("lf", "\n"), ("crlf", "\r\n"), ("bom", "\xfeff")]

canonicalMarkerSource :: LocaleCatalogSetting -> Text
canonicalMarkerSource setting =
  localeCatalogSettingKey setting <> ": \"_env:" <> localeCatalogSettingEnvVar setting <> ":\"\n"

environmentMap :: [(Text, Text)] -> KeyMap Text
environmentMap = KeyMap.fromList . map (first Key.fromText)

captureAndLoad :: [FilePath] -> [ByteString] -> KeyMap Text -> IO Value
captureAndLoad runtimeFiles embeddedBytes environment =
  captureSettingsSnapshotWithEnvironment runtimeFiles embeddedBytes environment >>= loadSettingsSnapshot

decodeSettingsBytes :: ByteString -> Value
decodeSettingsBytes bytes =
  case Yaml.decodeEither' bytes of
    Left message -> error $ "the spec's own settings bytes are invalid: " <> show message
    Right value -> value

settingValue :: LocaleCatalogSetting -> Value -> Maybe Value
settingValue = lookupKey . localeCatalogSettingKey

lookupKey :: Text -> Value -> Maybe Value
lookupKey name = \case
  Object fields -> KeyMap.lookup (Key.fromText name) fields
  _ -> Nothing

writeSettings :: FilePath -> Text -> IO ()
writeSettings path = writeFileBS path . encodeUtf8

{- | The snapshot refuses invalid configuration by failing the 'IO' action that
would have produced settings, so a regression here is a value, not a diagnostic.
-}
shouldFailWith :: IO a -> Text -> Expectation
shouldFailWith action fragment =
  Exception.try (void action) >>= \case
    Right () -> expectationFailure "the settings snapshot was accepted"
    Left (exception :: Exception.SomeException) ->
      toText (Exception.displayException exception) `shouldSatisfy` Text.isInfixOf fragment

withEnvironmentVariable :: String -> String -> IO a -> IO a
withEnvironmentVariable name value action =
  Exception.bracket
    (lookupEnv name)
    (maybe (unsetEnv name) (setEnv name))
    (const $ setEnv name value >> action)

{- | A directory this example owns, under the package's own build directory so
nothing outside the worktree is written and a parallel suite cannot collide.
-}
withSettingsWorkspace :: FilePath -> (FilePath -> IO a) -> IO a
withSettingsWorkspace label action = do
  root <- getCurrentDirectory
  let workspace = root </> ".stack-work" </> "locale-settings-preflight-spec" </> label
  Exception.bracket
    (removePathForcibly workspace >> createDirectoryIfMissing True workspace $> workspace)
    removePathForcibly
    action
