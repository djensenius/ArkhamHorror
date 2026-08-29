{-# LANGUAGE NoFieldSelectors #-}

module Base.Api.Types.Capabilities (
  ServerCapabilities (..),
  serverCapabilities,
) where

import Data.Aeson
import Relude

data ServerCapabilities = ServerCapabilities
  { schemaRevision :: Text
  , status :: Text
  , apiBasePath :: Text
  , nativeClientMinimumRevision :: Text
  , capabilities :: [Text]
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass ToJSON

serverCapabilities :: ServerCapabilities
serverCapabilities =
  ServerCapabilities
    { schemaRevision = "0.1.17"
    , status = "baseline-incomplete"
    , apiBasePath = "/api/v1"
    , nativeClientMinimumRevision = "0.1.0"
    , capabilities =
        [ "events.shared-state-versioning"
        , "games.step-probe"
        , "websockets.authorization-header"
        , "websockets.spectator-read-only"
        ]
    }
