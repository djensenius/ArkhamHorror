{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NoFieldSelectors #-}

{- | The locale-catalog configuration the governed capabilities fixture
advertises, derived from the bytes of a committed synthetic v1 catalog
manifest rather than from constants copied into this module.

@contracts\/fixtures\/locale-catalog-manifest.json@ is a __test artifact__: it
is schema-valid against the published v1 manifest schema, carries no narrative
text, and is never regenerated from @frontend\/src\/locales\/**@. That is what
keeps the governed contract stable while production catalog content changes
freely — a real deployment's revision and digest are runtime configuration
(see @docs\/locale-catalog.md@), not contract bytes.

Because every value here is read from that one file, a change to it that is
not reflected in @contracts\/fixtures\/capabilities-locale-catalog.json@ (or
the reverse) fails the fixture assertions in "Arkham.Api.JsonContractsSpec",
and @scripts\/validate-contract-fixtures.py@ enforces the same derivation
independently.
-}
module Helpers.LocaleCatalog (
  SyntheticCatalog (..),
  catalogEnvFor,
  loadSyntheticCatalog,
  runtimeCapabilities,
  syntheticCatalogManifestPath,
) where

import Base.Api.Handler.Capabilities (capabilitiesResponse)
import Base.Api.Types.Capabilities (ServerCapabilities)
import Crypto.Hash.SHA256 qualified as SHA256
import Data.Aeson (FromJSON (..), withObject, (.:))
import Data.ByteString.Base16 qualified as Base16
import Data.List qualified as List
import Data.Text qualified as T
import Helpers.AppSettings (loadAppSettings)
import Helpers.Contracts (loadContractBytes, loadContractJson)
import Relude

syntheticCatalogManifestPath :: FilePath
syntheticCatalogManifestPath = "contracts/fixtures/locale-catalog-manifest.json"

-- | Only the fields a capability pointer is built from; the rest of the v1
-- manifest is the frontend's business and is validated against its own schema
-- by @scripts\/validate-contract-fixtures.py@.
data CatalogManifest = CatalogManifest
  { manifestPath :: Text
  , catalogRevision :: Text
  , schemaVersion :: Text
  , defaultLocale :: Text
  , locales :: [Text]
  }

instance FromJSON CatalogManifest where
  parseJSON = withObject "locale catalog manifest" \o ->
    CatalogManifest
      <$> o .: "manifestPath"
      <*> o .: "catalogRevision"
      <*> o .: "schemaVersion"
      <*> o .: "defaultLocale"
      <*> (o .: "locales" >>= traverse (withObject "locale record" (.: "locale")))

-- | What a deployment publishing that manifest would configure.
data SyntheticCatalog = SyntheticCatalog
  { manifestUrl :: Text
  , catalogRevision :: Text
  , schemaVersion :: Text
  , defaultLocale :: Text
  , supportedLocales :: [Text]
  , manifestSha256 :: Text
  }
  deriving stock (Eq, Show)

loadSyntheticCatalog :: IO SyntheticCatalog
loadSyntheticCatalog = do
  contents <- loadContractBytes syntheticCatalogManifestPath
  manifest <- loadContractJson syntheticCatalogManifestPath :: IO CatalogManifest
  pure
    SyntheticCatalog
      { manifestUrl = manifest.manifestPath
      , catalogRevision = manifest.catalogRevision
      , schemaVersion = manifest.schemaVersion
      , defaultLocale = manifest.defaultLocale
      , supportedLocales = List.sort manifest.locales
      , manifestSha256 = decodeUtf8 (Base16.encode (SHA256.hash contents))
      }

{- | The six settings a deployment publishing this manifest would set. The
locale list is deliberately given in descending order, which no manifest and
no client would use, so the ascending order the response publishes is proven
to come from the server rather than from the configuration.
-}
catalogEnvFor :: SyntheticCatalog -> [(Text, Text)]
catalogEnvFor catalog =
  [ ("ARKHAM_LOCALE_CATALOG_MANIFEST_URL", catalog.manifestUrl)
  , ("ARKHAM_LOCALE_CATALOG_REVISION", catalog.catalogRevision)
  , ("ARKHAM_LOCALE_CATALOG_SCHEMA_VERSION", catalog.schemaVersion)
  , ("ARKHAM_LOCALE_CATALOG_DEFAULT_LOCALE", catalog.defaultLocale)
  , ("ARKHAM_LOCALE_CATALOG_LOCALES", T.intercalate "," (reverse catalog.supportedLocales))
  , ("ARKHAM_LOCALE_CATALOG_MANIFEST_SHA256", catalog.manifestSha256)
  ]

-- | Exactly what @GET \/api\/v1\/capabilities@ would answer for a deployment
-- started with this environment, or the diagnostic it would refuse to start
-- with.
runtimeCapabilities :: [(Text, Text)] -> Either Text ServerCapabilities
runtimeCapabilities = fmap capabilitiesResponse . loadAppSettings
