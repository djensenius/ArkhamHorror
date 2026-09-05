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
import Control.Concurrent (forkIO)
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
  doesPathExist,
  getCurrentDirectory,
  removeFile,
  removePathForcibly,
  renameFile,
  renamePath,
 )
import System.Environment (setEnv, unsetEnv)
import System.FilePath ((</>), takeFileName)
import System.Timeout (timeout)
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

  describe "bounded sources" do
    it "refuses a source that is not a regular file before reading it" do
      -- A character device or a FIFO answers reads forever, so the type is
      -- checked through the same handle the bytes would have come from. The
      -- devices have to exist, or this example would prove nothing.
      devices <- filterM doesPathExist ["/dev/zero", "/dev/random"]
      devices `shouldSatisfy` not . null
      for_ devices \device ->
        captureAndLoad [device] [] mempty `shouldFailPromptlyWith` "not a regular file"

    it "refuses a directory named as a settings source" do
      withSettingsWorkspace "directory-source" \workspace ->
        captureAndLoad [workspace] [] mempty `shouldFailPromptlyWith` "unable to read a settings source"

    it "refuses a single source larger than the configured byte limit" do
      withSettingsWorkspace "oversized-source" \workspace -> do
        let settings = workspace </> "oversized.yml"
        writeSettings settings ("key: " <> Text.replicate (5 * 1024 * 1024) "a" <> "\n")
        captureAndLoad [settings] [] mempty `shouldFailPromptlyWith` "byte limit"

    it "refuses sources whose combined bytes exceed the configured limit" do
      withSettingsWorkspace "aggregate-bytes" \workspace -> do
        let firstFile = workspace </> "first.yml"
            secondFile = workspace </> "second.yml"
            padding = Text.replicate (2 * 1024 * 1024 + 512) "a"
        writeSettings firstFile ("first: " <> padding <> "\n")
        writeSettings secondFile ("second: " <> padding <> "\n")
        captureAndLoad [firstFile, secondFile] [] mempty `shouldFailPromptlyWith` "byte limit"

    it "loads the bytes captured before a source was appended to" do
      withSettingsWorkspace "appended-source" \workspace -> do
        let settings = workspace </> "settings.yml"
        writeSettings settings "before: true\n"
        snapshot <- captureSettingsSnapshotWithEnvironment [settings] [] mempty
        appendSettings settings "after: true\n"
        value <- loadSettingsSnapshot snapshot :: IO Value
        value `shouldBe` object ["before" .= True]

    it "refuses an event-dense source promptly" do
      withSettingsWorkspace "event-dense" \workspace -> do
        let settings = workspace </> "dense.yml"
        writeSettings settings ("[" <> Text.replicate 200000 "a," <> "a]\n")
        captureAndLoad [settings] [] mempty `shouldFailPromptlyWith` "YAML event limit"

    it "refuses a source that nests deeper than the configured limit promptly" do
      withSettingsWorkspace "deep-nesting" \workspace -> do
        let settings = workspace </> "deep.yml"
            depth = 100000
        writeSettings settings
          $ "root: "
          <> Text.replicate depth "["
          <> "0"
          <> Text.replicate depth "]"
          <> "\n"
        captureAndLoad [settings] [] mempty `shouldFailPromptlyWith` "nests deeper than"

    it "refuses an alias expansion bomb promptly" do
      withSettingsWorkspace "alias-bomb" \workspace -> do
        let settings = workspace </> "bomb.yml"
        writeSettings settings (aliasFanOut 10 7)
        captureAndLoad [settings] [] mempty `shouldFailPromptlyWith` "analysis limit"

    it "still accepts nesting and event counts inside the configured bounds" do
      withSettingsWorkspace "within-bounds" \workspace -> do
        let settings = workspace </> "within-bounds.yml"
            depth = 100
        writeSettings settings
          $ "root: "
          <> Text.replicate depth "["
          <> Text.intercalate "," (replicate 1000 "\"item\"")
          <> Text.replicate depth "]"
          <> "\n"
        fromSnapshot <- captureAndLoad [settings] [] mempty
        fromPackage <- YamlConfig.loadYamlSettings [settings] [] YamlConfig.ignoreEnv :: IO Value
        fromSnapshot `shouldBe` fromPackage

  describe "snapshot-wide budgets" do
    it "refuses a source naming more distinct includes than the traversal budget promptly" do
      withSettingsWorkspace "many-distinct-includes" \workspace -> do
        let settings = workspace </> "many-includes.yml"
        -- Resolving a spelling is filesystem work, so the budget has to stop
        -- this before 60000 canonicalizations, not after them.
        writeSettings settings
          $ Text.concat ["- !include missing-" <> (show index :: Text) <> ".yml\n" | index <- [1 .. (60000 :: Int)]]
        captureAndLoad [settings] [] mempty `shouldFailPromptlyWith` "traversal limit"

    it "spends one raw-event budget across every captured source" do
      withSettingsWorkspace "shared-raw-events" \workspace -> do
        let roots = [workspace </> ("compact-" <> show index <> ".yml") | index <- [(0 :: Int) .. 7]]
            -- Compact YAML is about two bytes an event, so these eight sources
            -- stay well inside the byte budget and inside the per-source event
            -- budget while together holding millions of events.
            source = "[" <> Text.replicate 130000 "a," <> "a]\n"
        for_ roots \root -> writeSettings root source
        single <-
          timeout promptMicroseconds (captureSettingsSnapshotWithEnvironment (take 1 roots) [] mempty)
        isJust single `shouldBe` True
        captureAndLoad roots [] mempty `shouldFailPromptlyWith` "total YAML event limit"

    it "keeps a separate expanded-event budget for include multiplication" do
      withSettingsWorkspace "shared-expansion-budget" \workspace -> do
        let root = workspace </> "root.yml"
            shared = workspace </> "shared.yml"
        -- Barely any raw events: the multiplication is all in the includes.
        writeSettings shared ("[" <> Text.replicate 2000 "a," <> "a]\n")
        writeSettings root $ Text.concat (replicate 200 "- !include shared.yml\n")
        captureAndLoad [root] [] mempty `shouldFailPromptlyWith` "expand past the configured event limit"

    it "spends one analysis budget across every captured source" do
      withSettingsWorkspace "shared-analysis-budget" \workspace -> do
        let bombs = [workspace </> ("bomb-" <> show index <> ".yml") | index <- [(0 :: Int) .. 1]]
        for_ bombs \bomb -> writeSettings bomb (aliasFanOut 60 4)
        -- Aliases are shared nodes, so this costs steps rather than bytes: one
        -- source fits the budget and two do not.
        single <-
          timeout promptMicroseconds (captureSettingsSnapshotWithEnvironment (take 1 bombs) [] mempty)
        isJust single `shouldBe` True
        captureAndLoad bombs [] mempty `shouldFailPromptlyWith` "analysis limit"

    it "loads a repeated runtime root without multiplying its budget" do
      withSettingsWorkspace "duplicate-root-budget" \workspace -> do
        let settings = workspace </> "root.yml"
        -- Forty roots' worth of this source would be an order of magnitude
        -- past the snapshot's expansion budget; one root's worth is not.
        writeSettings settings ("[" <> Text.replicate 130000 "a," <> "a]\n")
        fromSnapshot <- captureAndLoadPromptly (replicate 40 settings) [] mempty
        fromPackage <- YamlConfig.loadYamlSettings [settings] [] YamlConfig.ignoreEnv :: IO Value
        fromSnapshot `shouldBe` fromPackage

    it "matches the package for a root repeated past the include-graph budget" do
      withSettingsWorkspace "duplicate-root-traversal" \workspace -> do
        let settings = workspace </> "root.yml"
            aliasedSpelling = workspace </> "." </> "root.yml"
        writeSettings settings "shared:\n  a: first\n  b: first\n"
        fromPackage <- YamlConfig.loadYamlSettings [settings] [] YamlConfig.ignoreEnv :: IO Value
        -- More repeats than the include-graph budget has steps: naming a file
        -- again is not traversal work, so it cannot spend that budget.
        repeated <- captureAndLoadPromptly (replicate 5000 settings) [] mempty
        repeated `shouldBe` fromPackage
        -- Two spellings of one file are one file too.
        aliased <- captureAndLoadPromptly [settings, aliasedSpelling, settings] [] mempty
        aliased `shouldBe` fromPackage

    it "matches the package when runtime roots repeat" do
      withSettingsWorkspace "duplicate-root-precedence" \workspace -> do
        let firstFile = workspace </> "first.yml"
            secondFile = workspace </> "second.yml"
            roots = [firstFile, firstFile, secondFile, firstFile, secondFile]
            embedded = encodeUtf8 @Text "shared:\n  a: embedded\n  d: embedded\n"
        writeSettings firstFile "shared:\n  a: first\n  b: first\n"
        writeSettings secondFile "shared:\n  a: second\n  c: second\n"
        fromSnapshot <- captureAndLoad roots [embedded] mempty
        fromPackage <-
          YamlConfig.loadYamlSettings roots [decodeSettingsBytes embedded] YamlConfig.ignoreEnv :: IO Value
        fromSnapshot `shouldBe` fromPackage

  describe "merge key semantics" do
    it "matches the package for merge values the loader ignores" do
      withSettingsWorkspace "ignored-merge-values" \workspace ->
        for_ (zip [(1 :: Int) ..] ignoredMergeSources) \(index, source) -> do
          let settings = workspace </> ("merge-" <> show index <> ".yml")
          writeSettings settings source
          fromSnapshot <- captureAndLoad [settings] [] mempty
          fromPackage <- YamlConfig.loadYamlSettings [settings] [] YamlConfig.ignoreEnv :: IO Value
          fromSnapshot `shouldBe` fromPackage

    it "still rejects two immediate mapping elements representing one setting" do
      withSettingsWorkspace "immediate-merge-duplicate" \workspace -> do
        let settings = workspace </> "immediate.yml"
        writeSettings settings
          "<<: [{locale-catalog-default-locale: en}, {locale-catalog-default-locale: de}]\n"
        captureAndLoad [settings] [] mempty `shouldFailWith` "is represented more than once"

  describe "include resolution" do
    it "gives every occurrence of one include spelling the same captured target" do
      withSettingsWorkspace "duplicate-include" \workspace -> do
        let root = workspace </> "root.yml"
            link = workspace </> "link.yml"
        writeSettings (workspace </> "first-target.yml") "value: from-first\n"
        writeSettings (workspace </> "second-target.yml") "value: from-second\n"
        writeSettings root
          $ unlines
            [ "left: !include link.yml"
            , "right: !include link.yml"
            ]
        createFileLink "first-target.yml" link
        snapshot <- captureSettingsSnapshotWithEnvironment [root] [] mempty
        -- The retarget lands between capture and load, and between the two
        -- occurrences of the spelling on the next capture; neither may split
        -- the two branches or reach into the snapshot already taken.
        removeFile link
        createFileLink "second-target.yml" link
        captured <- loadSettingsSnapshot snapshot :: IO Value
        lookupKey "left" captured `shouldBe` lookupKey "right" captured
        lookupKey "left" captured `shouldBe` Just (object ["value" .= ("from-first" :: Text)])
        recaptured <- captureAndLoad [root] [] mempty
        lookupKey "left" recaptured `shouldBe` lookupKey "right" recaptured
        lookupKey "left" recaptured `shouldBe` Just (object ["value" .= ("from-second" :: Text)])

    it "resolves and traverses a repeated include spelling exactly once" do
      withSettingsWorkspace "repeated-include" \workspace -> do
        let root = workspace </> "root.yml"
            shared = workspace </> "shared.yml"
            occurrences = 5000 :: Int
        writeSettings shared "value: shared\n"
        writeSettings root
          $ unlines
            ["key" <> (show index :: Text) <> ": !include shared.yml" | index <- [1 .. occurrences :: Int]]
        value <- captureAndLoad [root] [] mempty
        lookupKey "key1" value `shouldBe` Just (object ["value" .= ("shared" :: Text)])
        lookupKey ("key" <> show occurrences) value
          `shouldBe` Just (object ["value" .= ("shared" :: Text)])

    it "analyzes every occurrence of a duplicated include spelling" do
      withSettingsWorkspace "duplicate-include-analysis" \workspace -> do
        let root = workspace </> "root.yml"
            included = workspace </> "included.yml"
        writeSettings included "locale-catalog-default-locale: \"_env:LOCALE_ALIAS:\"\n"
        writeSettings root
          $ unlines
            [ "left: !include included.yml"
            , "right: !include included.yml"
            ]
        captureAndLoad [root] [] (environmentMap [("LOCALE_ALIAS", "en")])
          `shouldFailWith` "must use only its canonical"

    it "keeps duplicate spellings consistent while a symlink is retargeted underneath" do
      withSettingsWorkspace "include-retarget-race" \workspace -> do
        let root = workspace </> "root.yml"
            link = workspace </> "link.yml"
            staging = workspace </> "staging.yml"
            retarget target = do
              createFileLink target staging
              renamePath staging link
        writeSettings (workspace </> "first-target.yml") "value: from-first\n"
        writeSettings (workspace </> "second-target.yml") "value: from-second\n"
        writeSettings root
          $ unlines
            [ "left: !include link.yml"
            , "right: !include link.yml"
            ]
        createFileLink "first-target.yml" link
        retargeted <- newEmptyMVar
        _ <-
          forkIO
            $ Exception.try @Exception.SomeException
              (for_ [1 .. (400 :: Int)] \index -> retarget (if even index then "first-target.yml" else "second-target.yml"))
            >>= putMVar retargeted
        -- A retarget landing inside a capture may refuse the snapshot, which
        -- is fail-closed; what it may never do is give one spelling's two
        -- occurrences two different files.
        loaded <- forM [1 .. (60 :: Int)] \_ -> do
          result <- Exception.try (captureAndLoad [root] [] mempty)
          case result of
            Left (_ :: Exception.SomeException) -> pure (0 :: Int)
            Right value -> do
              lookupKey "left" value `shouldBe` lookupKey "right" value
              pure 1
        raced <- takeMVar retargeted
        raced `shouldSatisfy` isRight
        sum loaded `shouldSatisfy` (> 0)

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

-- | Work a bound is supposed to make unnecessary has to stay unnecessary: a
-- snapshot that only finishes eventually is a failure here.
captureAndLoadPromptly :: [FilePath] -> [ByteString] -> KeyMap Text -> IO Value
captureAndLoadPromptly runtimeFiles embeddedBytes environment =
  timeout promptMicroseconds (captureAndLoad runtimeFiles embeddedBytes environment)
    >>= maybe (fail "the settings snapshot did not finish inside its bound") pure

{- | A YAML document whose aliases fan out: @leafWidth@ scalars behind one
anchor, then @levels@ sequences that each reference the level below ten times.
The bytes stay tiny while the work of walking it grows as a power, which is
what makes it a bound test rather than a size test.
-}
aliasFanOut :: Int -> Int -> Text
aliasFanOut leafWidth levels = unlines (leaf : map level [1 .. levels])
 where
  leaf = "leaf: &l0 [" <> Text.intercalate "," (replicate leafWidth "\"x\"") <> "]"
  level index =
    "level"
      <> (show index :: Text)
      <> ": &l"
      <> (show index :: Text)
      <> " ["
      <> Text.intercalate "," (replicate 10 ("*l" <> (show (index - 1) :: Text)))
      <> "]"

{- | Merge values @Data.Yaml.Internal@'s @mergeObjects@ discards: it keeps the
immediate mapping elements of a merge sequence and ignores everything else, so
none of these may be read as a second representation of a locale setting.
-}
ignoredMergeSources :: [Text]
ignoredMergeSources =
  [ "<<: [[{locale-catalog-default-locale: en}]]\nlocale-catalog-default-locale: fr\n"
  , "<<: [[[{locale-catalog-default-locale: en}]], [{locale-catalog-revision: 1.abc}]]\n"
  , "<<: [\"scalar\", {locale-catalog-default-locale: en}]\n"
  , "<<: [[{locale-catalog-default-locale: en}], [{locale-catalog-default-locale: de}]]\nlocale-catalog-default-locale: fr\n"
  , "<<: not-a-mapping\nlocale-catalog-default-locale: fr\n"
  , "<<: [{locale-catalog-default-locale: en}, [{locale-catalog-default-locale: de}]]\n"
  ]

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

appendSettings :: FilePath -> Text -> IO ()
appendSettings path = appendFileBS path . encodeUtf8

{- | The snapshot refuses invalid configuration by failing the 'IO' action that
would have produced settings, so a regression here is a value, not a diagnostic.
-}
shouldFailWith :: IO a -> Text -> Expectation
shouldFailWith action fragment =
  Exception.try (void action) >>= \case
    Right () -> expectationFailure "the settings snapshot was accepted"
    Left (exception :: Exception.SomeException) ->
      toText (Exception.displayException exception) `shouldSatisfy` Text.isInfixOf fragment

{- | A bound the snapshot advertises has to be reached while the work is being
done, so an adversarial source is refused rather than merely finished.
-}
shouldFailPromptlyWith :: IO a -> Text -> Expectation
shouldFailPromptlyWith action fragment =
  timeout promptMicroseconds (Exception.try (void action)) >>= \case
    Nothing -> expectationFailure "the settings snapshot did not finish inside its bound"
    Just (Right ()) -> expectationFailure "the settings snapshot was accepted"
    Just (Left (exception :: Exception.SomeException)) ->
      toText (Exception.displayException exception) `shouldSatisfy` Text.isInfixOf fragment

promptMicroseconds :: Int
promptMicroseconds = 30 * 1000 * 1000

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
