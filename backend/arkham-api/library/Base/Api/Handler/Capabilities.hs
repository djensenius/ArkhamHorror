module Base.Api.Handler.Capabilities (
  capabilitiesResponse,
  getApiV1CapabilitiesR,
) where

import Base.Api.Types.Capabilities
import Import

{- | The handler's whole body, as a pure function of the runtime settings, so
the exact bytes a deployment would serve can be asserted directly from a
parsed 'AppSettings' (see @Arkham.Api.JsonContractsSpec@).
-}
capabilitiesResponse :: AppSettings -> ServerCapabilities
capabilitiesResponse = serverCapabilities . appLocaleCatalog

getApiV1CapabilitiesR :: Handler ServerCapabilities
getApiV1CapabilitiesR = capabilitiesResponse <$> getsYesod appSettings
