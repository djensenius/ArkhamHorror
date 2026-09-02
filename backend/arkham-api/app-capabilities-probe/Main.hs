{-# LANGUAGE NoImplicitPrelude #-}

{- | Emit the exact @GET \/api\/v1\/capabilities@ bytes this deployment's
configuration produces, or fail the way the server would fail to start.

This is the deployment seam for the locale-catalog capability, and it
re-implements nothing. Settings come from 'Application.loadAppSettingsArgs' —
the very function @Application.appMain@ starts from, so config files named on
the command line, the compile-time @config\/settings.yml@ fallback and
environment overrides all apply in production precedence. The body comes from
the handler's own 'capabilitiesResponse'. The bytes come from
'Data.Aeson.encode', which is @encodingToLazyByteString . toEncoding@: the same
encoder the REST route serves.

Nothing is appended to those bytes — not even a newline — so a caller can
assert the production @toEncoding@ output exactly rather than a normalized
re-encoding of it. A configuration the server would refuse exits non-zero with
the server's own diagnostic and prints no body at all.

@scripts\/check-locale-catalog-settings.py@ drives it from a real generated
catalog manifest.
-}
module Main (main) where

import Application (loadAppSettingsArgs)
import Base.Api.Handler.Capabilities (capabilitiesResponse)
import Data.Aeson qualified as Aeson
import Data.ByteString.Lazy qualified as LazyByteString
import Relude
import System.IO (hSetBinaryMode)

main :: IO ()
main = do
  settings <- loadAppSettingsArgs
  hSetBinaryMode stdout True
  LazyByteString.hPut stdout $ Aeson.encode (capabilitiesResponse settings)
