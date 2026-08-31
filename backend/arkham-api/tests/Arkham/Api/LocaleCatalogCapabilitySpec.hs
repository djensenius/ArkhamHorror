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
import Data.List qualified as List
import Data.Text qualified as T
import Helpers.AppSettings
import Helpers.LocaleCatalog
import Relude
import Settings (AppSettings (..))
import Test.Hspec

-- | Every locale-catalog variable present but blank, which is how an operator
-- turns the pointer off without editing the settings file.
blankCatalogEnv :: [(Text, Text)]
blankCatalogEnv = [(name, "") | (name, _) <- fixtureCatalogEnv]

withEnv :: [(Text, Text)] -> [(Text, Text)]
withEnv overrides = overrideEnv overrides fixtureCatalogEnv

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

configWith :: Text -> Text -> Text -> LocaleCatalogConfig
configWith schemaVersion locale locales =
  catalogConfig "/locale-catalog/manifest.json" fixtureCatalogRevision schemaVersion locale locales fixtureCatalogDigest

{- | One sample per 'ManifestUrlRejection'. The spec asserts this table covers
every constructor, so a rejection can never be added to the type — or made
unreachable by a reordered guard — without a case that proves it fires.
-}
manifestUrlRejections :: [(Text, ManifestUrlRejection)]
manifestUrlRejections =
  [ ("/" <> T.replicate 512 "a" <> ".json", ManifestUrlTooLong)
  , ("/locale-catalog/manifest.json\n", ManifestUrlControlCharacter)
  , ("/locale-catalog/manif\232st.json", ManifestUrlNonAscii)
  , ("/locale catalog/manifest.json", ManifestUrlWhitespace)
  , ("\\\\server\\share\\manifest.json", ManifestUrlBackslash)
  , ("/locale-catalog/manifest.json#fragment", ManifestUrlFragment)
  , ("/locale-catalog/manifest.json?revision=1", ManifestUrlQuery)
  , ("/locale%2Dcatalog/manifest.json", ManifestUrlPercentEncoded)
  , ("//cdn.example.com/locale-catalog/manifest.json", ManifestUrlSchemeRelative)
  , ("http://cdn.example.com/locale-catalog/manifest.json", ManifestUrlInsecureScheme)
  , ("ftp://cdn.example.com/locale-catalog/manifest.json", ManifestUrlUnsupportedScheme)
  , ("C:/locale-catalog/manifest.json", ManifestUrlPlatformPath)
  , ("locale-catalog/manifest.json", ManifestUrlNotAbsolute)
  , ("https://user:secret@cdn.example.com/manifest.json", ManifestUrlCredentials)
  , ("https://-cdn.example.com/manifest.json", ManifestUrlInvalidHost)
  , ("https://cdn.example.com:0/manifest.json", ManifestUrlInvalidPort)
  , ("https://cdn.example.com", ManifestUrlNotAbsolutePath)
  , ("/locale-catalog//manifest.json", ManifestUrlEmptyPathSegment)
  , ("/locale-catalog/../manifest.json", ManifestUrlDotSegment)
  , ("/locale-catalog/manifest:1.json", ManifestUrlInvalidPathCharacter)
  , ("/locale-catalog/manifest", ManifestUrlNotJsonPath)
  ]

legacyCapabilities :: [Text]
legacyCapabilities =
  [ "events.shared-state-versioning"
  , "games.step-probe"
  , "websockets.authorization-header"
  , "websockets.spectator-read-only"
  ]

spec :: Spec
spec = describe "locale catalog capability" do
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
      fmap (.catalogRevision) (catalogFor fixtureCatalogEnv) `shouldBe` Right fixtureCatalogRevision
      fmap (.schemaVersion) (catalogFor fixtureCatalogEnv) `shouldBe` Right "1.0.0"
      fmap (.manifestSha256) (catalogFor fixtureCatalogEnv) `shouldBe` Right fixtureCatalogDigest

    it "orders supported locales deterministically, whatever order they were configured in" do
      fmap (.supportedLocales) (catalogFor fixtureCatalogEnv)
        `shouldBe` Right ["de", "en", "es", "fr", "it", "ko", "zh"]
      fmap (.defaultLocale) (catalogFor fixtureCatalogEnv) `shouldBe` Right "en"

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
    it "accepts the hosted, same-origin path" do
      parseManifestUrl "/locale-catalog/manifest.json"
        `shouldBe` Right "/locale-catalog/manifest.json"

    it "refuses every unsupported spelling for its stated reason" do
      for_ manifestUrlRejections \(url, rejection) ->
        (url, parseManifestUrl url) `shouldBe` (url, Left rejection)

    it "covers every rejection the type declares" do
      List.sort (List.nub (map snd manifestUrlRejections)) `shouldBe` [minBound .. maxBound]
