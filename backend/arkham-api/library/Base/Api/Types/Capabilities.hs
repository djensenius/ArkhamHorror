{-# LANGUAGE NoFieldSelectors #-}

module Base.Api.Types.Capabilities (
  ServerCapabilities (..),
  semanticQuestionPresentationCapability,
  serverCapabilities,
) where

import Base.Api.Types.LocaleCatalog (LocaleCatalog, localeCatalogCapability)
import Data.Aeson
import Data.List qualified as List
import Relude

{- | The public @GET \/api\/v1\/capabilities@ body.

@localeCatalog@ is the only optional field: it is present exactly when the
deployment has a valid locale-catalog pointer configured, and omitted (not
null) otherwise.

A deployment without a catalog therefore omits both the @localeCatalog@ member
and the @i18n.locale-catalog.v1@ identifier. Other additive, globally available
capabilities remain advertised; in particular,
@questions.semantic-presentation.v1@ is present whether or not a catalog is
configured. The response is deliberately not byte-identical to the historical
baseline:
@schemaRevision@ reports the current contract bundle (the catalog capability
first appeared in 0.1.23), because it describes the whole server contract
rather than one optional runtime feature. A server that under-reported it would
lie to every client that negotiates on it. Clients compare the three numeric
revision components and ignore unknown identifiers, so a client built against
0.1.22 behaves exactly as it did.
@contracts\/manifest.json@'s @legacyCompatibilityChecks@ pins that baseline and
both this repository's contract validator and
@Arkham.Api.LocaleCatalogCapabilitySpec@ compare the real response against it.
-}
data ServerCapabilities = ServerCapabilities
  { schemaRevision :: Text
  , status :: Text
  , apiBasePath :: Text
  , nativeClientMinimumRevision :: Text
  , capabilities :: [Text]
  , localeCatalog :: Maybe LocaleCatalog
  }
  deriving stock (Eq, Show, Generic)

{- | @omitNothingFields@ is what keeps the legacy response shape exact, and
both encoders are generated from this one value, so the @toJSON@ a fixture
test asserts against and the @toEncoding@ the wire actually uses cannot drift
apart.
-}
serverCapabilitiesOptions :: Options
serverCapabilitiesOptions = defaultOptions {omitNothingFields = True}

instance ToJSON ServerCapabilities where
  toJSON = genericToJSON serverCapabilitiesOptions
  toEncoding = genericToEncoding serverCapabilitiesOptions

semanticQuestionPresentationCapability :: Text
semanticQuestionPresentationCapability = "questions.semantic-presentation.v1"

{- | The running server's contract identity, given whatever locale catalog the
deployment has configured (see "Base.Api.Types.LocaleCatalog").

The catalog capability string and the @localeCatalog@ object are derived from
the same 'Maybe', so a client can never be shown one without the other.
-}
serverCapabilities :: Maybe LocaleCatalog -> ServerCapabilities
serverCapabilities localeCatalog =
  ServerCapabilities
    { schemaRevision = "0.1.43"
    , status = "baseline-incomplete"
    , apiBasePath = "/api/v1"
    , nativeClientMinimumRevision = "0.1.0"
    , capabilities = List.sort $ baseCapabilities <> catalogCapabilities
    , localeCatalog = localeCatalog
    }
 where
  baseCapabilities =
    [ "events.shared-state-versioning"
    , "games.step-probe"
    , semanticQuestionPresentationCapability
    , "websockets.authorization-header"
    , "websockets.spectator-read-only"
    ]
  catalogCapabilities = [localeCatalogCapability | isJust localeCatalog]
