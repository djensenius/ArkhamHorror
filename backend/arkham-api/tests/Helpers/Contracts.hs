{- | Read governed contract artifacts from the test suite.

The specs run from whichever directory Stack happens to invoke them in, so
every governed path is resolved against the same small candidate list rather
than being duplicated per call site.
-}
module Helpers.Contracts (
  loadContractBytes,
  loadContractJson,
) where

import Data.Aeson qualified as Aeson
import Data.ByteString qualified as ByteString
import Relude
import System.IO.Error qualified as IOError

{- | The exact bytes of a governed artifact, which is what a digest has to be
taken over: re-encoding a parsed value would prove nothing about the file the
contract pins.
-}
loadContractBytes :: FilePath -> IO ByteString
loadContractBytes relativePath = go candidates
 where
  candidates = [prefix <> relativePath | prefix <- ["", "../", "../../"]]

  go = \case
    [] ->
      fail
        $ "Could not find governed contract artifact "
        <> relativePath
        <> "; searched: "
        <> show candidates
    path : remaining ->
      IOError.tryIOError (ByteString.readFile path) >>= \case
        Left err
          | IOError.isDoesNotExistError err -> go remaining
          | otherwise -> IOError.ioError err
        Right contents -> pure contents

-- | Decode a governed artifact through the real Aeson decoder.
loadContractJson :: Aeson.FromJSON a => FilePath -> IO a
loadContractJson relativePath =
  loadContractBytes relativePath >>= \contents -> case Aeson.eitherDecodeStrict' contents of
    Left err -> fail $ "Could not decode governed contract artifact " <> relativePath <> ": " <> err
    Right decoded -> pure decoded
