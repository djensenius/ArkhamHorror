module Base.Api.Handler.Capabilities (
  getApiV1CapabilitiesR,
) where

import Base.Api.Types.Capabilities
import Import

getApiV1CapabilitiesR :: Handler ServerCapabilities
getApiV1CapabilitiesR = pure serverCapabilities
