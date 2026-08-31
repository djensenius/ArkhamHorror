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
null) otherwise, so a server without one answers with bytes identical to every
revision before the field existed.
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
