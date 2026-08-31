{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NoFieldSelectors #-}

{- | Startup configuration and runtime handler behavior for the optional
locale-catalog pointer in @GET \/api\/v1\/capabilities@ (see
"Base.Api.Types.LocaleCatalog").

Every case here goes through the production settings pipeline
('Helpers.AppSettings.loadAppSettings') and the production handler body
('capabilitiesResponse'), so what is asserted is what a deployment with that
environment would actually serve — or refuse to start with.

The exact wire bytes for both response shapes are pinned separately, against
the governed contract fixtures, in "Arkham.Api.JsonContractsSpec".
-}
module Arkham.Api.LocaleCatalogCapabilitySpec (spec) where

import Base.Api.Types.Capabilities (ServerCapabilities (..))
import Base.Api.Types.LocaleCatalog
import Data.Aeson (FromJSON (..), withObject, (.:))
import Data.Aeson qualified as Aeson
import Data.Aeson.Key qualified as AesonKey
import Data.Aeson.KeyMap qualified as AesonKeyMap
import Data.Char qualified as Char
import Data.List qualified as List
import Data.Text qualified as T
import Helpers.AppSettings
import Helpers.Contracts (loadContractJson)
import Helpers.LocaleCatalog
import Relude
import Settings (AppSettings (..))
import Test.Hspec

responseFor :: [(Text, Text)] -> Either Text ServerCapabilities
responseFor = runtimeCapabilities

configuredCatalogFor :: [(Text, Text)] -> Either Text (Maybe LocaleCatalog)
configuredCatalogFor = fmap appLocaleCatalog . loadAppSettings

catalogFor :: [(Text, Text)] -> Either Text LocaleCatalog
catalogFor environment =
  responseFor environment >>= \response -> case response.localeCatalog of
    Nothing -> Left "the response advertised no locale catalog"
    Just catalog -> Right catalog

capabilitiesFor :: [(Text, Text)] -> Either Text [Text]
capabilitiesFor = fmap (.capabilities) . responseFor

manifestUrlFor :: [(Text, Text)] -> Either Text Text
manifestUrlFor = fmap (.manifestUrl) . catalogFor

-- | The startup diagnostic a deployment would be refused with.
startupErrorFor :: [(Text, Text)] -> Text
startupErrorFor environment = case loadAppSettings environment of
  Left message -> message
  Right _ -> "<the settings parsed successfully, but a startup failure was expected>"

-- | Both directions of the capability/object pairing, checked on one response.
catalogAdvertisedCoherently :: Either Text ServerCapabilities -> Either Text Bool
catalogAdvertisedCoherently =
  fmap \response ->
    (localeCatalogCapability `elem` response.capabilities) == isJust response.localeCatalog

catalogConfig :: Text -> Text -> Text -> Text -> Text -> Text -> LocaleCatalogConfig
catalogConfig url revision schemaVersion locale locales digest =
  LocaleCatalogConfig
    (Just url)
    (Just revision)
    (Just schemaVersion)
    (Just locale)
    (Just locales)
    (Just digest)

{- | A configuration whose only interesting values are the ones under test.
The revision and digest are well-formed but arbitrary: these cases are about
the locale and schema-version rules, and binding them to the synthetic
manifest would say nothing extra.
-}
configWith :: Text -> Text -> Text -> LocaleCatalogConfig
configWith schemaVersion locale locales =
  catalogConfig
    "/locale-catalog/manifest.json"
    "1.00000000000000000000000000000000"
    schemaVersion
    locale
    locales
    (T.replicate 64 "0")

{- | @contracts\/manifest.json@'s @manifestUrlChecks@ table, read here so the
schema and this validator are held to one governed list rather than to two
hand-maintained copies. @scripts\/validate-contract-fixtures.py@ drives the
same rows through the published JSON Schema.
-}
data ManifestUrlChecks = ManifestUrlChecks
  { accepted :: [AcceptedManifestUrl]
  , rejected :: [RejectedManifestUrl]
  }

data AcceptedManifestUrl = AcceptedManifestUrl
  { configured :: Text
  , published :: Text
  }
  deriving stock (Eq, Show)

data RejectedManifestUrl = RejectedManifestUrl
  { configured :: Text
  , reason :: Text
  }
  deriving stock (Eq, Show)

instance FromJSON ManifestUrlChecks where
  parseJSON = withObject "manifestUrlChecks" \o ->
    ManifestUrlChecks <$> o .: "accepted" <*> o .: "rejected"

instance FromJSON AcceptedManifestUrl where
  parseJSON = withObject "manifestUrlChecks.accepted" \o ->
    AcceptedManifestUrl <$> o .: "configured" <*> o .: "published"

instance FromJSON RejectedManifestUrl where
  parseJSON = withObject "manifestUrlChecks.rejected" \o ->
    RejectedManifestUrl <$> o .: "configured" <*> o .: "reason"

{- | @legacyCompatibilityChecks@: the exact response this contract served at
0.1.22, before the optional pointer existed, plus the members a revision is
allowed to differ in.
-}
data LegacyCompatibility = LegacyCompatibility
  { baselineRevision :: Text
  , baselineResponse :: Aeson.Object
  , addedCapability :: Text
  , allowedDifferences :: AllowedDifferences
  }

data AllowedDifferences = AllowedDifferences
  { disabled :: [Text]
  , advertised :: [Text]
  }

instance FromJSON LegacyCompatibility where
  parseJSON = withObject "legacyCompatibilityChecks" \o ->
    LegacyCompatibility
      <$> o .: "baselineRevision"
      <*> o .: "baselineResponse"
      <*> o .: "addedCapability"
      <*> o .: "allowedDifferences"

instance FromJSON AllowedDifferences where
  parseJSON = withObject "allowedDifferences" \o ->
    AllowedDifferences <$> o .: "disabled" <*> o .: "advertised"

{- | @catalogRevisionChecks@: the same two-sided binding as
@manifestUrlChecks@, for the revision grammar.
-}
data CatalogRevisionChecks = CatalogRevisionChecks
  { schemaVersion :: Text
  , accepted :: [AcceptedCatalogRevision]
  , rejected :: [RejectedCatalogRevision]
  }

newtype AcceptedCatalogRevision = AcceptedCatalogRevision {configured :: Text}

data RejectedCatalogRevision = RejectedCatalogRevision
  { configured :: Text
  , reason :: Text
  }

instance FromJSON CatalogRevisionChecks where
  parseJSON = withObject "catalogRevisionChecks" \o ->
    CatalogRevisionChecks <$> o .: "schemaVersion" <*> o .: "accepted" <*> o .: "rejected"

instance FromJSON AcceptedCatalogRevision where
  parseJSON = withObject "catalogRevisionChecks.accepted" \o ->
    AcceptedCatalogRevision <$> o .: "configured"

instance FromJSON RejectedCatalogRevision where
  parseJSON = withObject "catalogRevisionChecks.rejected" \o ->
    RejectedCatalogRevision <$> o .: "configured" <*> o .: "reason"

data ContractManifest = ContractManifest
  { manifestUrlChecks :: ManifestUrlChecks
  , catalogRevisionChecks :: CatalogRevisionChecks
  , legacyCompatibilityChecks :: LegacyCompatibility
  }

instance FromJSON ContractManifest where
  parseJSON = withObject "contracts/manifest.json" \o ->
    ContractManifest
      <$> o .: "manifestUrlChecks"
      <*> o .: "catalogRevisionChecks"
      <*> o .: "legacyCompatibilityChecks"

-- | The constructor name, as the governed table spells it.
configErrorName :: LocaleCatalogConfigError -> Text
configErrorName = \case
  PartiallyConfigured _ -> "PartiallyConfigured"
  InvalidManifestUrl _ -> "InvalidManifestUrl"
  InvalidCatalogRevision _ -> "InvalidCatalogRevision"
  UnsupportedSchemaVersion _ -> "UnsupportedSchemaVersion"
  CatalogRevisionSchemaMismatch _ _ -> "CatalogRevisionSchemaMismatch"
  InvalidManifestSha256 _ -> "InvalidManifestSha256"
  InvalidLocaleTag _ _ -> "InvalidLocaleTag"
  DuplicateLocaleTag _ -> "DuplicateLocaleTag"
  TooManySupportedLocales _ -> "TooManySupportedLocales"
  DefaultLocaleNotSupported _ -> "DefaultLocaleNotSupported"

{- | Drop exactly the members a revision is allowed to differ in, so what is
left has to be the 0.1.22 shape and nothing else.
-}
normalizeAgainstBaseline :: Text -> [Text] -> Aeson.Value -> Aeson.Object
normalizeAgainstBaseline added allowed = \case
  Aeson.Object fields ->
    let dropped = AesonKeyMap.filterWithKey (\key _ -> AesonKey.toText key `notElem` allowed) fields
     in if "capabilities" `elem` allowed
          then AesonKeyMap.insert "capabilities" (withoutAdded fields) dropped
          else dropped
  _ -> AesonKeyMap.empty
 where
  withoutAdded fields = case AesonKeyMap.lookup "capabilities" fields of
    Just (Aeson.Array capabilities) ->
      Aeson.toJSON $ filter (/= Aeson.String added) (toList capabilities)
    _ -> Aeson.Null

-- | The constructor name, as the governed table spells it.
rejectionName :: ManifestUrlRejection -> Text
rejectionName = show

legacyCapabilities :: [Text]
legacyCapabilities =
  [ "events.shared-state-versioning"
  , "games.step-probe"
  , "websockets.authorization-header"
  , "websockets.spectator-read-only"
  ]

spec :: Spec
spec = do
  catalog <- runIO loadSyntheticCatalog
  contractManifest <- runIO (loadContractJson "contracts/manifest.json" :: IO ContractManifest)
  let checks = contractManifest.manifestUrlChecks
      revisions = contractManifest.catalogRevisionChecks
      legacy = contractManifest.legacyCompatibilityChecks
      configWithRevision revision =
        catalogConfig
          "/locale-catalog/manifest.json"
          revision
          revisions.schemaVersion
          "en"
          "de,en"
          (T.replicate 63 "0" <> "a")

  let fixtureCatalogEnv = catalogEnvFor catalog
      -- Every locale-catalog variable present but blank, which is how an
      -- operator turns the pointer off without editing the settings file.
      blankCatalogEnv = [(name, "") | (name, _) <- fixtureCatalogEnv]
      withEnv overrides = overrideEnv overrides fixtureCatalogEnv

  describe "locale catalog capability" do
   describe "when the deployment publishes no catalog" do
    it "parses with no locale-catalog environment at all" do
      configuredCatalogFor [] `shouldBe` Right Nothing

    it "treats every blank value as unconfigured rather than invalid" do
      configuredCatalogFor blankCatalogEnv `shouldBe` Right Nothing

    it "omits the object and the capability string together" do
      (fmap (.localeCatalog) . responseFor) [] `shouldBe` Right Nothing
      catalogAdvertisedCoherently (responseFor []) `shouldBe` Right True

    it "keeps the exact legacy capability list" do
      capabilitiesFor [] `shouldBe` Right legacyCapabilities

   describe "when the deployment publishes a catalog" do
    it "advertises the capability alongside the object" do
      catalogAdvertisedCoherently (responseFor fixtureCatalogEnv) `shouldBe` Right True

    it "inserts the capability without disturbing the existing identifiers" do
      capabilitiesFor fixtureCatalogEnv
        `shouldBe` Right
          [ "events.shared-state-versioning"
          , "games.step-probe"
          , "i18n.locale-catalog.v1"
          , "websockets.authorization-header"
          , "websockets.spectator-read-only"
          ]

    it "publishes the hosted, same-origin manifest path unchanged" do
      manifestUrlFor fixtureCatalogEnv `shouldBe` Right "/locale-catalog/manifest.json"

    it "publishes the revision, schema version and digest it was configured with" do
      fmap (.catalogRevision) (catalogFor fixtureCatalogEnv) `shouldBe` Right catalog.catalogRevision
      fmap (.schemaVersion) (catalogFor fixtureCatalogEnv) `shouldBe` Right catalog.schemaVersion
      fmap (.manifestSha256) (catalogFor fixtureCatalogEnv) `shouldBe` Right catalog.manifestSha256

    it "orders supported locales deterministically, whatever order they were configured in" do
      -- catalogEnvFor deliberately configures the manifest's locales in
      -- descending order, so ascending output can only come from the server.
      fmap (.supportedLocales) (catalogFor fixtureCatalogEnv)
        `shouldBe` Right (List.sort catalog.supportedLocales)
      fmap (.defaultLocale) (catalogFor fixtureCatalogEnv) `shouldBe` Right catalog.defaultLocale
      catalog.supportedLocales `shouldSatisfy` ((> 1) . length)

    it "canonicalizes locale tags rather than publishing what was typed" do
      let environment =
            withEnv
              [ ("ARKHAM_LOCALE_CATALOG_LOCALES", "EN,pt-br,ZH-hant,fr-CH")
              , ("ARKHAM_LOCALE_CATALOG_DEFAULT_LOCALE", "eN")
              ]
      fmap (.supportedLocales) (catalogFor environment)
        `shouldBe` Right ["en", "fr-CH", "pt-BR", "zh-Hant"]
      fmap (.defaultLocale) (catalogFor environment) `shouldBe` Right "en"

   describe "self-hosted, split deployments" do
    it "accepts an absolute https manifest URL" do
      manifestUrlFor
        (withEnv [("ARKHAM_LOCALE_CATALOG_MANIFEST_URL", "https://static.example.org/l10n/manifest.json")])
        `shouldBe` Right "https://static.example.org/l10n/manifest.json"

    it "normalizes the scheme and host case to one spelling" do
      manifestUrlFor
        (withEnv [("ARKHAM_LOCALE_CATALOG_MANIFEST_URL", "HTTPS://Static.Example.ORG/l10n/manifest.json")])
        `shouldBe` Right "https://static.example.org/l10n/manifest.json"

    it "drops a redundant :443 so one deployment has one URL" do
      manifestUrlFor
        (withEnv [("ARKHAM_LOCALE_CATALOG_MANIFEST_URL", "https://static.example.org:443/l10n/manifest.json")])
        `shouldBe` Right "https://static.example.org/l10n/manifest.json"

    it "keeps an explicit non-default port" do
      manifestUrlFor
        (withEnv [("ARKHAM_LOCALE_CATALOG_MANIFEST_URL", "https://static.example.org:8443/l10n/manifest.json")])
        `shouldBe` Right "https://static.example.org:8443/l10n/manifest.json"

   describe "startup validation" do
    it "fails when the pointer is only partially configured" do
      let message = startupErrorFor (withEnv [("ARKHAM_LOCALE_CATALOG_MANIFEST_SHA256", "")])
      message `shouldSatisfy` T.isInfixOf "partially configured"
      message `shouldSatisfy` T.isInfixOf "ARKHAM_LOCALE_CATALOG_MANIFEST_SHA256"

    it "fails on an insecure manifest URL" do
      startupErrorFor (withEnv [("ARKHAM_LOCALE_CATALOG_MANIFEST_URL", "http://cdn.example.com/manifest.json")])
        `shouldSatisfy` T.isInfixOf "http is not accepted, only https"

    it "never echoes the rejected manifest URL, which may carry credentials" do
      let message =
            startupErrorFor
              (withEnv [("ARKHAM_LOCALE_CATALOG_MANIFEST_URL", "https://admin:hunter2@cdn.example.com/manifest.json")])
      message `shouldSatisfy` T.isInfixOf "credentials"
      message `shouldSatisfy` (not . T.isInfixOf "hunter2")

    it "fails on a malformed catalog revision" do
      startupErrorFor (withEnv [("ARKHAM_LOCALE_CATALOG_REVISION", "1.0.0")])
        `shouldSatisfy` T.isInfixOf "must be a catalog revision"

    it "fails on an unsupported catalog schema version" do
      startupErrorFor (withEnv [("ARKHAM_LOCALE_CATALOG_SCHEMA_VERSION", "2.0.0")])
        `shouldSatisfy` T.isInfixOf "must be one of 1.0.0"

    it "fails when the revision does not belong to the configured schema version" do
      startupErrorFor (withEnv [("ARKHAM_LOCALE_CATALOG_REVISION", "2.c5f4828c5dcde2c11707c5b926b3f324")])
        `shouldSatisfy` T.isInfixOf "does not belong to catalog schema version 1.0.0"

    it "fails on a manifest digest that is not a SHA-256" do
      startupErrorFor (withEnv [("ARKHAM_LOCALE_CATALOG_MANIFEST_SHA256", "deadbeef")])
        `shouldSatisfy` T.isInfixOf "64 lowercase hex characters"

    it "fails on an invalid locale tag" do
      startupErrorFor (withEnv [("ARKHAM_LOCALE_CATALOG_LOCALES", "en,english")])
        `shouldSatisfy` T.isInfixOf "invalid locale tag"

    it "fails on a duplicate locale, including one that only collides once canonicalized" do
      startupErrorFor (withEnv [("ARKHAM_LOCALE_CATALOG_LOCALES", "en-US,de,en-us")])
        `shouldSatisfy` T.isInfixOf "more than once"

    it "fails when more locales are configured than a catalog can publish" do
      let tooMany =
            T.intercalate "," (take 65 [T.pack ['a', middle, final] | middle <- ['a' .. 'z'], final <- ['a' .. 'z']])
      startupErrorFor (withEnv [("ARKHAM_LOCALE_CATALOG_LOCALES", tooMany)])
        `shouldSatisfy` T.isInfixOf "at most 64"

    it "fails when the default locale is not one of the supported locales" do
      startupErrorFor
        ( withEnv
            [ ("ARKHAM_LOCALE_CATALOG_LOCALES", "en,fr")
            , ("ARKHAM_LOCALE_CATALOG_DEFAULT_LOCALE", "de")
            ]
        )
        `shouldSatisfy` T.isInfixOf "is not listed in"

    it "refuses a value YAML read as a number instead of a quoted string" do
      -- A digest of only decimal digits, or a version typed as 1.0, is a Number
      -- by the time Data.Yaml.Config has re-parsed the substituted value.
      let message = startupErrorFor (withEnv [("ARKHAM_LOCALE_CATALOG_MANIFEST_SHA256", T.replicate 64 "0")])
      message `shouldSatisfy` T.isInfixOf "must be a quoted string"
      message `shouldSatisfy` T.isInfixOf "ARKHAM_LOCALE_CATALOG_MANIFEST_SHA256"

    it "names the settings key and its environment variable" do
      let message = startupErrorFor (withEnv [("ARKHAM_LOCALE_CATALOG_MANIFEST_SHA256", "deadbeef")])
      message `shouldSatisfy` T.isInfixOf "locale-catalog-manifest-sha256"
      message `shouldSatisfy` T.isInfixOf "ARKHAM_LOCALE_CATALOG_MANIFEST_SHA256"

   describe "configuration parsing" do
    it "reports no catalog when nothing is supplied" do
      parseLocaleCatalogConfig (LocaleCatalogConfig Nothing Nothing Nothing Nothing Nothing Nothing)
        `shouldBe` Right Nothing

    it "lists every missing setting when the pointer is partially configured" do
      parseLocaleCatalogConfig
        (LocaleCatalogConfig (Just "/locale-catalog/manifest.json") Nothing Nothing Nothing Nothing Nothing)
        `shouldBe` Left
          ( PartiallyConfigured
              [ CatalogRevisionSetting
              , SchemaVersionSetting
              , DefaultLocaleSetting
              , SupportedLocalesSetting
              , ManifestSha256Setting
              ]
          )

    it "rejects a duplicate locale in canonical form" do
      parseLocaleCatalogConfig (configWith "1.0.0" "en" "en,EN")
        `shouldBe` Left (DuplicateLocaleTag "en")

    it "rejects a default locale outside the supported set" do
      parseLocaleCatalogConfig (configWith "1.0.0" "fr" "de,en")
        `shouldBe` Left (DefaultLocaleNotSupported "fr")

    it "rejects an unsupported schema version" do
      parseLocaleCatalogConfig (configWith "0.9.0" "en" "de,en")
        `shouldBe` Left (UnsupportedSchemaVersion "0.9.0")

    it "rejects an invalid locale tag without echoing anything but a sanitized excerpt" do
      parseLocaleCatalogConfig (configWith "1.0.0" "en" "en,e n$")
        `shouldBe` Left (InvalidLocaleTag SupportedLocalesSetting "e?n?")

   describe "manifest URL binding" do
    it "publishes exactly what the governed contract table says it publishes" do
      for_ checks.accepted \accepted ->
        (accepted.configured, parseManifestUrl accepted.configured)
          `shouldBe` (accepted.configured, Right accepted.published)

    it "refuses every governed spelling for its stated reason" do
      for_ checks.rejected \refused ->
        (refused.configured, first rejectionName (parseManifestUrl refused.configured))
          `shouldBe` (refused.configured, Left refused.reason)

    it "covers every rejection the closed type declares" do
      List.sort (List.nub (map (.reason) checks.rejected))
        `shouldBe` List.sort (map rejectionName [minBound .. maxBound])

    it "never resolves a host a URL parser could read as an address" do
      let numericHosts =
            [ refused.configured
            | refused <- checks.rejected
            , refused.reason == "ManifestUrlAmbiguousNumericHost"
            ]
      numericHosts `shouldSatisfy` ((>= 8) . length)
      for_ numericHosts \url ->
        (url, parseManifestUrl url) `shouldBe` (url, Left ManifestUrlAmbiguousNumericHost)

   describe "the synthetic catalog manifest the contract fixture is derived from" do
    it "is the manifest the advertised pointer describes" do
      fmap (.manifestUrl) (catalogFor fixtureCatalogEnv) `shouldBe` Right catalog.manifestUrl
      fmap (.catalogRevision) (catalogFor fixtureCatalogEnv) `shouldBe` Right catalog.catalogRevision
      fmap (.schemaVersion) (catalogFor fixtureCatalogEnv) `shouldBe` Right catalog.schemaVersion
      fmap (.defaultLocale) (catalogFor fixtureCatalogEnv) `shouldBe` Right catalog.defaultLocale
      fmap (.manifestSha256) (catalogFor fixtureCatalogEnv) `shouldBe` Right catalog.manifestSha256

    it "carries a digest of the manifest's real bytes, not a copied constant" do
      T.length catalog.manifestSha256 `shouldBe` 64
      catalog.manifestSha256 `shouldSatisfy` T.all (\c -> Char.isDigit c || (c >= 'a' && c <= 'f'))

  describe "non-ASCII lookalikes" do
    -- Every grammar here is a wire grammar, and the schemas that mirror it are
    -- ASCII. A digit or letter that only looks like one has to be refused, or a
    -- client comparing by string equality would read a different catalog.
    it "refuses a digest whose hex is not ASCII" do
      startupErrorFor (withEnv [("ARKHAM_LOCALE_CATALOG_MANIFEST_SHA256", T.replicate 63 "0" <> "\1632")])
        `shouldSatisfy` T.isInfixOf "64 lowercase hex characters"

    it "refuses a digest of fullwidth digits" do
      startupErrorFor (withEnv [("ARKHAM_LOCALE_CATALOG_MANIFEST_SHA256", T.replicate 64 "\65296")])
        `shouldSatisfy` T.isInfixOf "64 lowercase hex characters"

    it "refuses a locale subtag spelled with a Cyrillic confusable" do
      startupErrorFor (withEnv [("ARKHAM_LOCALE_CATALOG_LOCALES", "de,d\1072")])
        `shouldSatisfy` T.isInfixOf "invalid locale tag"

    it "refuses a region subtag of fullwidth digits" do
      startupErrorFor (withEnv [("ARKHAM_LOCALE_CATALOG_DEFAULT_LOCALE", "en-\65296\65296")])
        `shouldSatisfy` T.isInfixOf "invalid locale tag"

    it "refuses a host that only becomes ASCII under Unicode case folding" do
      -- \8490 (KELVIN SIGN) lowercases to 'k'; the URL is refused as non-ASCII
      -- before any normalization can turn it into a name that looks legitimate.
      startupErrorFor
        (withEnv [("ARKHAM_LOCALE_CATALOG_MANIFEST_URL", "https://\8490elvin.example/manifest.json")])
        `shouldSatisfy` T.isInfixOf "non-ASCII"

    it "refuses a port written in Arabic-Indic digits" do
      startupErrorFor
        (withEnv [("ARKHAM_LOCALE_CATALOG_MANIFEST_URL", "https://cdn.example.com:\1632\1632\1632\1632/manifest.json")])
        `shouldSatisfy` T.isInfixOf "non-ASCII"

    it "canonicalizes only ASCII case, so a folded lookalike cannot become a locale" do
      -- \304 (LATIN CAPITAL LETTER I WITH DOT ABOVE) is not an ASCII letter, so
      -- it is refused rather than folded to 'i'.
      startupErrorFor (withEnv [("ARKHAM_LOCALE_CATALOG_LOCALES", "de,\304t")])
        `shouldSatisfy` T.isInfixOf "invalid locale tag"

  describe "catalog revision binding" do
    it "accepts exactly the revisions the governed contract table publishes" do
      for_ revisions.accepted \row ->
        (row.configured, first configErrorName (parseLocaleCatalogConfig (configWithRevision row.configured)))
          `shouldSatisfy` (isRight . snd)

    it "refuses every governed spelling for its stated reason" do
      for_ revisions.rejected \row ->
        (row.configured, first configErrorName (parseLocaleCatalogConfig (configWithRevision row.configured)))
          `shouldBe` (row.configured, Left row.reason)

    it "covers both a wrong major and a non-canonical one" do
      let reasons = map (.reason) revisions.rejected
      reasons `shouldSatisfy` elem "CatalogRevisionSchemaMismatch"
      reasons `shouldSatisfy` elem "InvalidCatalogRevision"

  describe "compatibility with the pre-feature response" do
    it "keeps the exact legacy shape when no catalog is configured" do
      let baseline = normalizeAgainstBaseline legacy.addedCapability ["schemaRevision"]
      fmap (normalizeAgainstBaseline legacy.addedCapability legacy.allowedDifferences.disabled . Aeson.toJSON) (responseFor [])
        `shouldBe` Right (baseline (Aeson.Object legacy.baselineResponse))

    it "keeps the exact legacy shape underneath the advertised catalog" do
      let baseline = normalizeAgainstBaseline legacy.addedCapability ["schemaRevision"]
      fmap
        (normalizeAgainstBaseline legacy.addedCapability legacy.allowedDifferences.advertised . Aeson.toJSON)
        (responseFor fixtureCatalogEnv)
        `shouldBe` Right (baseline (Aeson.Object legacy.baselineResponse))

    it "reports this server's real contract revision, not the baseline's" do
      -- schemaRevision identifies the whole contract bundle, so under-reporting
      -- it to look byte-identical would lie to every client that negotiates on
      -- it. Older clients compare numeric components, so they are unaffected.
      fmap (.schemaRevision) (responseFor []) `shouldBe` Right "0.1.23"
      fmap (.schemaRevision) (responseFor []) `shouldNotBe` Right legacy.baselineRevision

    it "still refuses to drop or rename a legacy capability" do
      let dropped =
            Aeson.Object
              $ AesonKeyMap.insert "capabilities" (Aeson.toJSON ["games.step-probe" :: Text])
              $ legacy.baselineResponse
      normalizeAgainstBaseline legacy.addedCapability legacy.allowedDifferences.disabled dropped
        `shouldNotBe` normalizeAgainstBaseline legacy.addedCapability ["schemaRevision"] (Aeson.Object legacy.baselineResponse)
