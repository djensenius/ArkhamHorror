{-# LANGUAGE NoFieldSelectors #-}

module Base.Api.Types.Capabilities (
  ServerCapabilities (..),
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

A deployment without a catalog therefore answers with the exact /field and
capability shape/ it did before this field existed — no @localeCatalog@ member,
no @i18n.locale-catalog.v1@ identifier, every other field including the full
capability list unchanged. It is deliberately not byte-identical:
@schemaRevision@ advances to 0.1.23, because it describes this server's whole
contract bundle rather than one optional runtime feature, and a server that
under-reported it would lie to every client that negotiates on it. Clients
compare the three numeric revision components and ignore unknown identifiers,
so a client built against 0.1.22 behaves exactly as it did.
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

{- | The running server's contract identity, given whatever locale catalog the
deployment has configured (see "Base.Api.Types.LocaleCatalog").

The catalog capability string and the @localeCatalog@ object are derived from
the same 'Maybe', so a client can never be shown one without the other.
-}
serverCapabilities :: Maybe LocaleCatalog -> ServerCapabilities
serverCapabilities localeCatalog =
  ServerCapabilities
    { schemaRevision = "0.1.23"
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
    , "websockets.authorization-header"
    , "websockets.spectator-read-only"
    ]
  catalogCapabilities = [localeCatalogCapability | isJust localeCatalog]
