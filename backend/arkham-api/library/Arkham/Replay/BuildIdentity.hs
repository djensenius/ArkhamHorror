{-# LANGUAGE TemplateHaskell #-}

module Arkham.Replay.BuildIdentity (
  ReplayBuildAttestation (..),
  ReplayBuildIdentity (..),
  embedReplayBuildIdentity,
  validateReplayBuildIdentity,
) where

import Arkham.Git (GitSha (..))
import Arkham.Json (aesonOptions)
import Arkham.Prelude
import Control.Monad.Fail (fail)
import Crypto.Hash.SHA256 qualified as SHA256
import Data.Aeson.Types (Parser)
import Data.ByteString qualified as BS
import Data.ByteString.Base16 qualified as Base16
import Data.ByteString.Char8 qualified as BSC
import Data.List qualified as List
import Data.Text qualified as T
import Language.Haskell.TH (Exp, Q)
import Language.Haskell.TH.Syntax (addDependentFile, runIO)
import Language.Haskell.TH.Syntax qualified as TH
import System.Directory (doesFileExist, getCurrentDirectory)
import System.Environment (lookupEnv)
import System.Exit (ExitCode (..))
import System.FilePath qualified as FilePath
import System.Process (readProcessWithExitCode)

data ReplayBuildAttestation
  = ReplayBuildGitClean
  | ReplayBuildSourceSha256
  | ReplayBuildUnattested
  deriving stock (Eq, Show)

instance ToJSON ReplayBuildAttestation where
  toJSON = String . \case
    ReplayBuildGitClean -> "git-clean"
    ReplayBuildSourceSha256 -> "source-sha256"
    ReplayBuildUnattested -> "unattested"

instance FromJSON ReplayBuildAttestation where
  parseJSON = withText "ReplayBuildAttestation" \case
    "git-clean" -> pure ReplayBuildGitClean
    "source-sha256" -> pure ReplayBuildSourceSha256
    "unattested" -> pure ReplayBuildUnattested
    other -> fail $ "unsupported replay build attestation: " <> T.unpack other

data ReplayBuildIdentity = ReplayBuildIdentity
  { replayBuildGitRevision :: GitSha
  , replayBuildGitTree :: GitSha
  , replayBuildSourceSha256 :: Text
  , replayBuildSourceClean :: Bool
  , replayBuildAttestation :: ReplayBuildAttestation
  }
  deriving stock (Eq, Generic, Show)

instance ToJSON ReplayBuildIdentity where
  toJSON = genericToJSON $ aesonOptions $ Just "replayBuild"

instance FromJSON ReplayBuildIdentity where
  parseJSON value = do
    build@ReplayBuildIdentity {..} <-
      genericParseJSON (aesonOptions $ Just "replayBuild") value
    void $ parseGitSha "build gitRevision" replayBuildGitRevision
    void $ parseGitSha "build gitTree" replayBuildGitTree
    void $ parseHash "build sourceSha256" 64 replayBuildSourceSha256
    pure build

validateReplayBuildIdentity :: ReplayBuildIdentity -> Either String ()
validateReplayBuildIdentity ReplayBuildIdentity {..} =
  case replayBuildAttestation of
    ReplayBuildUnattested ->
      Left "backend build has no build attestation for its embedded source identity"
    ReplayBuildGitClean
      | not replayBuildSourceClean ->
          Left "backend build claims a clean-Git attestation for dirty sources"
    _ -> Right ()

embedReplayBuildIdentity :: Q Exp
embedReplayBuildIdentity = do
  (identity, dependencies) <- runIO discoverBuildIdentity
  traverse_ addDependentFile dependencies
  let revision = unGitSha identity.replayBuildGitRevision
      tree = unGitSha identity.replayBuildGitTree
      source = identity.replayBuildSourceSha256
      clean = identity.replayBuildSourceClean
      attestation = identity.replayBuildAttestation
  [|
    ReplayBuildIdentity
      { replayBuildGitRevision = GitSha $(TH.lift revision)
      , replayBuildGitTree = GitSha $(TH.lift tree)
      , replayBuildSourceSha256 = $(TH.lift source)
      , replayBuildSourceClean = $(TH.lift clean)
      , replayBuildAttestation = $(liftAttestation attestation)
      }
    |]

discoverBuildIdentity :: IO (ReplayBuildIdentity, [FilePath])
discoverBuildIdentity =
  (do
    cwd <- getCurrentDirectory
    root <- git cwd ["rev-parse", "--show-toplevel"]
    revision <- git root ["rev-parse", "HEAD"]
    tree <- git root ["rev-parse", "HEAD^{tree}"]
    status <- git root ["status", "--porcelain=v1", "--untracked-files=all", "--", "backend"]
    diffBytes <- BSC.pack <$> git root ["diff", "--binary", "--no-ext-diff", "HEAD", "--", "backend"]
    untracked <- lines <$> git root ["ls-files", "--others", "--exclude-standard", "--", "backend"]
    untrackedBytes <- fmap BS.concat . for (List.sort untracked) $ \path ->
      (\bytes -> BSC.pack path <> "\0" <> bytes <> "\0") <$> BS.readFile (root </> path)
    let source =
          hashBytes
            $ BSC.intercalate "\0" [BSC.pack revision, BSC.pack tree, diffBytes]
            <> "\0"
            <> untrackedBytes
        clean = null status
    expected <- lookupEnv "ARKHAM_REPLAY_ATTEST_SOURCE_SHA256"
    dependencies <- gitDependencies root
    pure
      ( ReplayBuildIdentity
          (GitSha $ T.pack revision)
          (GitSha $ T.pack tree)
          source
          clean
          ( if clean
              then ReplayBuildGitClean
              else if expected == Just (T.unpack source) then ReplayBuildSourceSha256 else ReplayBuildUnattested
          )
      , dependencies
      )
  )
  `catch` \(_ :: IOException) -> pure (unattestedIdentity, [])

git :: FilePath -> [String] -> IO String
git root args = do
  (code, out, err) <- readProcessWithExitCode "git" ("-C" : root : args) ""
  case code of
    ExitSuccess -> pure $ trimEnd out
    ExitFailure _ -> ioError $ userError $ "git " <> unwords args <> " failed: " <> trimEnd err

gitDependencies :: FilePath -> IO [FilePath]
gitDependencies root = do
  gitDir <- git root ["rev-parse", "--absolute-git-dir"]
  ref <- git root ["symbolic-ref", "-q", "HEAD"] `catch` \(_ :: IOException) -> pure ""
  refPath <-
    if null ref
      then pure Nothing
      else do
        path <- git root ["rev-parse", "--git-path", ref]
        pure $ Just $ if FilePath.isAbsolute path then path else root </> path
  filterM doesFileExist $ [gitDir </> "HEAD", gitDir </> "index"] <> maybeToList refPath

unattestedIdentity :: ReplayBuildIdentity
unattestedIdentity =
  ReplayBuildIdentity (GitSha $ T.replicate 40 "0") (GitSha $ T.replicate 40 "0")
    (T.replicate 64 "0") False ReplayBuildUnattested

liftAttestation :: ReplayBuildAttestation -> Q Exp
liftAttestation = \case
  ReplayBuildGitClean -> [|ReplayBuildGitClean|]
  ReplayBuildSourceSha256 -> [|ReplayBuildSourceSha256|]
  ReplayBuildUnattested -> [|ReplayBuildUnattested|]

parseGitSha :: String -> GitSha -> Parser GitSha
parseGitSha label value@(GitSha sha) = value <$ either fail pure (validateHex label 40 sha)

parseHash :: String -> Int -> Text -> Parser Text
parseHash label size value = either fail pure $ validateHex label size value

validateHex :: String -> Int -> Text -> Either String Text
validateHex label size value
  | T.length value /= size || T.any (not . isLowerHex) value =
      Left $ label <> " must contain exactly " <> show size <> " lowercase hexadecimal characters"
  | otherwise = Right value
 where
  isLowerHex c = ('0' <= c && c <= '9') || c `elem` ['a' .. 'f']

hashBytes :: BS.ByteString -> Text
hashBytes = decodeUtf8 . Base16.encode . SHA256.hash

trimEnd :: String -> String
trimEnd = reverse . dropWhile (`elem` ['\n', '\r']) . reverse
