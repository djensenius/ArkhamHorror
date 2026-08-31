{-# LANGUAGE NoImplicitPrelude #-}

{- | Emit the exact @GET \/api\/v1\/capabilities@ bytes this deployment's
environment produces, or fail the way the server would fail to start.

This is the deployment seam for the locale-catalog capability. It re-implements
nothing: it loads settings through the same @loadYamlSettings@ call
@Application.appMain@ uses (the embedded @config\/settings.yml@ plus the real
process environment), builds the response through the handler's own body, and
writes it with 'Data.Aeson.encode', which is defined as
@encodingToLazyByteString . toEncoding@ — the same encoder the REST route
serves. A configuration the server would refuse therefore exits non-zero here
with the server's own diagnostic, and never prints a body.

@scripts\/check-locale-catalog-settings.py@ drives it with settings derived
from a real generated catalog manifest, validates the bytes it prints against
the governed schema, and corrupts each setting in turn to prove the failure.
-}
module Main (main) where

import Base.Api.Handler.Capabilities (capabilitiesResponse)
import Data.Aeson qualified as Aeson
import Data.ByteString.Lazy.Char8 qualified as LazyChar8
import Data.Yaml.Config (loadYamlSettings, useEnv)
import Relude
import Settings (configSettingsYmlValue)

main :: IO ()
main = do
  settings <- loadYamlSettings [] [configSettingsYmlValue] useEnv
  LazyChar8.putStrLn $ Aeson.encode (capabilitiesResponse settings)
