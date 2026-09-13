module Base.Api.Handler.Capabilities (
  capabilitiesResponse,
  capabilitiesResponseHeaders,
  getApiV1CapabilitiesR,
) where

import Arkham.Replay.ImportAuthority
import Arkham.Replay.ServerBuildIdentity (serverBuildIdentity)
import Base.Api.Types.Capabilities
import Import

{- | The handler's whole body, as a pure function of the runtime settings, so
the exact bytes a deployment would serve can be asserted directly from a
parsed 'AppSettings' (see @Arkham.Api.JsonContractsSpec@).
-}
capabilitiesResponse :: AppSettings -> ServerCapabilities
capabilitiesResponse = serverCapabilities . appLocaleCatalog

capabilitiesResponseHeaders :: [(Text, Text)]
capabilitiesResponseHeaders =
  backendBuildIdentityHeaders serverBuildIdentity

getApiV1CapabilitiesR :: Handler ServerCapabilities
getApiV1CapabilitiesR = do
  traverse_ (uncurry addHeader) capabilitiesResponseHeaders
  capabilitiesResponse <$> getsYesod appSettings
