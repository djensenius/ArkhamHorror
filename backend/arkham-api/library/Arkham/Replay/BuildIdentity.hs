{-# LANGUAGE TemplateHaskell #-}

module Arkham.Replay.BuildIdentity (
  ReplayBuildAttestation (..),
  ReplayBuildIdentity (..),
  embedReplayBuildIdentity,
  validateCleanReplayBuildIdentity,
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
import Data.ByteString.Lazy qualified as BSL
import Data.List qualified as List
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import Language.Haskell.TH (Exp, Q)
import Language.Haskell.TH.Syntax (addDependentFile, runIO)
import Language.Haskell.TH.Syntax qualified as TH
import System.Directory (doesFileExist, getCurrentDirectory)
import System.Environment (lookupEnv)
import System.Exit (ExitCode (..))
import System.FilePath qualified as FilePath
import System.Posix.Files qualified as Posix
import System.Process.Typed qualified as Process

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
    unless (value == toJSON build) $
      fail "replay build identity contains non-canonical or unknown fields"
    pure build

validateReplayBuildIdentity :: ReplayBuildIdentity -> Either String ()
validateReplayBuildIdentity ReplayBuildIdentity {..} =
  case replayBuildAttestation of
    ReplayBuildUnattested ->
      Left "backend build has no build attestation for its embedded source identity"
    ReplayBuildGitClean
      | replayBuildSourceClean -> Right ()
      | otherwise ->
          Left "backend build claims a clean-Git attestation for dirty sources"
    ReplayBuildSourceSha256
      | replayBuildSourceClean ->
          Left "backend build claims a source-SHA attestation for clean sources"
      | otherwise -> Right ()

validateCleanReplayBuildIdentity :: ReplayBuildIdentity -> Either String ()
validateCleanReplayBuildIdentity identity@ReplayBuildIdentity {..} = do
  validateReplayBuildIdentity identity
  unless replayBuildSourceClean $
    Left "backend build source is not clean"
  unless (replayBuildAttestation == ReplayBuildGitClean) $
    Left "backend build is not attested by a clean Git tree"

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
    root <- gitText cwd ["rev-parse", "--show-toplevel"]
    revisionBytes <- gitBytes root ["rev-parse", "HEAD"]
    revision <- decodeGitOutput "revision" revisionBytes
    treeBytes <- gitBytes root ["rev-parse", "HEAD^{tree}"]
    tree <- decodeGitOutput "tree" treeBytes
    status <- gitRawBytes root ["status", "--porcelain=v1", "--untracked-files=all", "--", "backend"]
    diffBytes <- gitRawBytes root ["diff", "--binary", "--no-ext-diff", "HEAD", "--", "backend"]
    untracked <- gitPathEntries root ["ls-files", "--others", "--exclude-standard", "-z", "--", "backend"]
    ignored <-
      gitPathEntries
        root
        (["ls-files", "--others", "--ignored", "--exclude-standard", "-z", "--"] <> compiledSourceRoots)
        >>= filterM (isRegularFile . (root </>) . snd)
    untrackedBytes <- fmap BS.concat . for (List.sortOn fst $ untracked <> ignored) $ \(pathBytes, path) ->
      frameSourceRecord pathBytes <$> BS.readFile (root </> path)
    let source =
          hashBytes
            $ BS.intercalate "\0" [revisionBytes, treeBytes, diffBytes]
            <> "\0"
            <> untrackedBytes
        clean = BS.null status && null ignored
    expected <- lookupEnv "ARKHAM_REPLAY_ATTEST_SOURCE_SHA256"
    gitDependencyPaths <- gitDependencies root
    sourceDependencyPaths <- sourceDependencies root (snd <$> untracked) (snd <$> ignored)
    let dependencies = List.sort $ ordNub $ gitDependencyPaths <> sourceDependencyPaths
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

compiledSourceRoots :: [FilePath]
compiledSourceRoots =
  [ "backend/arkham-api/library"
  , "backend/arkham-api/app"
  , "backend/arkham-api/app-replay"
  , "backend/arkham-api/app-capabilities-probe"
  , "backend/cards-discover/library"
  , "backend/cards-discover/app"
  , "backend/devel-store-lock/library"
  ]

gitBytes :: FilePath -> [String] -> IO BS.ByteString
gitBytes root args = trimEndBytes <$> gitRawBytes root args

gitRawBytes :: FilePath -> [String] -> IO BS.ByteString
gitRawBytes root args = do
  (code, out, err) <- Process.readProcess $ Process.proc "git" ("-C" : root : args)
  case code of
    ExitSuccess -> pure $ BSL.toStrict out
    ExitFailure _ ->
      ioError
        $ userError
        $ "git "
        <> unwords args
        <> " failed: "
        <> BSC.unpack (trimEndBytes $ BSL.toStrict err)

gitText :: FilePath -> [String] -> IO String
gitText root args =
  gitBytes root args >>= decodeGitOutput ("git " <> unwords args)

gitPathEntries :: FilePath -> [String] -> IO [(BS.ByteString, FilePath)]
gitPathEntries root args = do
  paths <- splitNullBytes <$> gitRawBytes root args
  for paths $ \pathBytes -> do
    path <- decodeGitOutput "Git path" pathBytes
    pure (pathBytes, path)

decodeGitOutput :: String -> BS.ByteString -> IO String
decodeGitOutput label bytes = case TE.decodeUtf8' bytes of
  Left err -> ioError $ userError $ label <> " is not valid UTF-8: " <> show err
  Right value -> pure $ T.unpack value

sourceDependencies :: FilePath -> [FilePath] -> [FilePath] -> IO [FilePath]
sourceDependencies root untracked ignored = do
  tracked <- map snd <$> gitPathEntries root ["ls-files", "--cached", "-z", "--", "backend"]
  let candidates = tracked <> untracked <> ignored
  regular <- filterM (isRegularFile . (root </>)) candidates
  pure $ (root </>) <$> List.sort regular

isRegularFile :: FilePath -> IO Bool
isRegularFile path = do
  exists <- doesFileExist path
  if exists then Posix.isRegularFile <$> Posix.getFileStatus path else pure False

gitDependencies :: FilePath -> IO [FilePath]
gitDependencies root = do
  gitDir <- gitText root ["rev-parse", "--absolute-git-dir"]
  ref <- gitText root ["symbolic-ref", "-q", "HEAD"] `catch` \(_ :: IOException) -> pure ""
  refPath <-
    if null ref
      then pure Nothing
      else do
        path <- gitText root ["rev-parse", "--git-path", ref]
        pure $ Just $ if FilePath.isAbsolute path then path else root </> path
  globalExcludes <-
    gitText root ["config", "--path", "--get", "core.excludesFile"]
      `catch` \(_ :: IOException) -> pure ""
  let files =
        [ gitDir </> "HEAD"
        , gitDir </> "index"
        , gitDir </> "config"
        , gitDir </> "info" </> "exclude"
        , root </> ".gitignore"
        ]
          <> maybeToList refPath
          <> [globalExcludes | not $ null globalExcludes]
  filterM doesFileExist files

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

frameSourceRecord :: BS.ByteString -> BS.ByteString -> BS.ByteString
frameSourceRecord path bytes =
  frameSourceComponent path <> frameSourceComponent bytes

frameSourceComponent :: BS.ByteString -> BS.ByteString
frameSourceComponent bytes =
  BSC.pack (show $ BS.length bytes) <> ":" <> bytes

trimEndBytes :: BS.ByteString -> BS.ByteString
trimEndBytes =
  BS.reverse . BS.dropWhile (`elem` [0x0A, 0x0D]) . BS.reverse

splitNullBytes :: BS.ByteString -> [BS.ByteString]
splitNullBytes value
  | BS.null value = []
  | otherwise =
      let (path, rest) = BS.break (== 0) value
       in path : if BS.null rest then [] else splitNullBytes (BS.tail rest)
