{- | The locale-catalog configuration the governed contract fixture
@contracts\/fixtures\/capabilities-locale-catalog.json@ describes, plus the
production handler body applied to it.

Both the fixture spec ("Arkham.Api.JsonContractsSpec") and the behavioral spec
("Arkham.Api.LocaleCatalogCapabilitySpec") drive the same environment through
the same production path, so the bytes one pins are the bytes the other
reasons about.
-}
module Helpers.LocaleCatalog (
  fixtureCatalogDigest,
  fixtureCatalogEnv,
  fixtureCatalogRevision,
  runtimeCapabilities,
) where

import Base.Api.Handler.Capabilities (capabilitiesResponse)
import Base.Api.Types.Capabilities (ServerCapabilities)
import Helpers.AppSettings (loadAppSettings)
import Relude

{- | The locales are deliberately given in the web client's own @uiLocales@
order rather than sorted, so the response's ordering is proven to come from
the server and not from the configuration.
-}
fixtureCatalogEnv :: [(Text, Text)]
fixtureCatalogEnv =
  [ ("ARKHAM_LOCALE_CATALOG_MANIFEST_URL", "/locale-catalog/manifest.json")
  , ("ARKHAM_LOCALE_CATALOG_REVISION", fixtureCatalogRevision)
  , ("ARKHAM_LOCALE_CATALOG_SCHEMA_VERSION", "1.0.0")
  , ("ARKHAM_LOCALE_CATALOG_DEFAULT_LOCALE", "en")
  , ("ARKHAM_LOCALE_CATALOG_LOCALES", "en,fr,it,ko,es,zh,de")
  , ("ARKHAM_LOCALE_CATALOG_MANIFEST_SHA256", fixtureCatalogDigest)
  ]

fixtureCatalogRevision :: Text
fixtureCatalogRevision = "1.c5f4828c5dcde2c11707c5b926b3f324"

fixtureCatalogDigest :: Text
fixtureCatalogDigest = "cdf9d565e7b6e710e8504a8e79f58bdd60c9dc044f2edc796819dc269e275ff3"

-- | Exactly what @GET \/api\/v1\/capabilities@ would answer for a deployment
-- started with this environment, or the diagnostic it would refuse to start
-- with.
runtimeCapabilities :: [(Text, Text)] -> Either Text ServerCapabilities
runtimeCapabilities = fmap capabilitiesResponse . loadAppSettings
