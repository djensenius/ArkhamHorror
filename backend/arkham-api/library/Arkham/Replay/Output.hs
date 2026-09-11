module Arkham.Replay.Output (
  ReplayInput,
  replayInputBytes,
  replayInputSha256,
  withReplayInput,
  revalidateReplayInputs,
  ReplayOutputRole (..),
  ReplayOutputRequest (..),
  ReplayOutputArtifact (..),
  ReplayOutputPlan,
  ReplayPublishPhase (..),
  prepareReplayOutputs,
  publishReplayOutputs,
  publishReplayOutputsWithHook,
) where

import Arkham.Prelude
import Arkham.Replay.Checkpoint (sha256Strict)
import Control.Exception (allowInterrupt)
import Data.ByteString qualified as BS
import Data.ByteString.Lazy qualified as BSL
import Data.List qualified as List
import Data.Map.Strict qualified as Map
import Data.Text qualified as T
import System.Directory (canonicalizePath, makeAbsolute, removeFile, renameFile)
import System.FilePath (normalise, takeDirectory, takeFileName)
import System.IO (openBinaryTempFile)
import System.Posix.Files (
  FileStatus,
  createLink,
  deviceID,
  fileID,
  getFdStatus,
  getFileStatus,
  getSymbolicLinkStatus,
  isDirectory,
  isRegularFile,
  isSymbolicLink,
 )
import System.Posix.IO (
  OpenFileFlags (..),
  OpenMode (ReadOnly),
  closeFd,
  defaultFileFlags,
  fdSeek,
  handleToFd,
  openFd,
 )
import System.Posix.IO.ByteString (fdRead, fdWrite)
import System.Posix.Types (DeviceID, Fd, FileID)
import System.Posix.Unistd (fileSynchronise)

type FileIdentity = (DeviceID, FileID)

data ReplayInput = ReplayInput
  { replayInputOriginal :: FilePath
  , replayInputCanonical :: FilePath
  , replayInputDescriptor :: Fd
  , replayInputIdentity :: FileIdentity
  , replayInputBytes :: BS.ByteString
  , replayInputSha256 :: Text
  }

data ReplayOutputRole
  = ReplayCheckpointOutput
  | ReplayFinalGameOutput
  | ReplayMetricsOutput
  deriving stock (Eq, Ord, Show)

data ReplayOutputRequest = ReplayOutputRequest
  { replayOutputRequestRole :: ReplayOutputRole
  , replayOutputRequestPath :: FilePath
  }
  deriving stock (Eq, Show)

data ReplayOutputArtifact = ReplayOutputArtifact
  { replayOutputArtifactRole :: ReplayOutputRole
  , replayOutputArtifactBytes :: BSL.ByteString
  }

data ReplayPublishPhase
  = ReplayStageCompleted ReplayOutputRole
  | ReplayOutputsStaged
  | ReplaySecondaryPublished ReplayOutputRole
  | ReplayBeforeCheckpointPublish
  | ReplayCheckpointLinked
  deriving stock (Eq, Show)

data ResolvedOutput = ResolvedOutput
  { resolvedOutputRole :: ReplayOutputRole
  , resolvedOutputOriginal :: FilePath
  , resolvedOutputCanonical :: FilePath
  , resolvedOutputParent :: FilePath
  , resolvedOutputParentIdentity :: FileIdentity
  , resolvedOutputExistingIdentity :: Maybe FileIdentity
  }

data ReplayOutputPlan = ReplayOutputPlan
  { replayOutputPlanInputs :: [ReplayInput]
  , replayOutputPlanOutputs :: [ResolvedOutput]
  }

data StagedOutput = StagedOutput
  { stagedOutputResolved :: ResolvedOutput
  , stagedOutputPath :: FilePath
  , stagedOutputIdentity :: FileIdentity
  }

withReplayInput :: FilePath -> (ReplayInput -> IO a) -> IO a
withReplayInput path = bracket (openReplayInput path) (closeFd . replayInputDescriptor)

openReplayInput :: FilePath -> IO ReplayInput
openReplayInput original = do
  absolute <- normalise <$> makeAbsolute original
  let parentSpelling = takeDirectory absolute
      fileName = takeFileName absolute
  when (null fileName || fileName == "." || fileName == "..") $
    replayOutputFailure $ "invalid replay input path: " <> original
  parent <- canonicalizePath parentSpelling `catch` pathFailure "resolve" original
  let canonical = parent </> fileName
      flags = defaultFileFlags {nofollow = True, cloexec = True, nonBlock = True}
  bracketOnError
    (openFd canonical ReadOnly flags `catch` pathFailure "open" original)
    closeFd
    \fd -> do
      status <- getFdStatus fd
      unless (isRegularFile status) $
        replayOutputFailure $ "replay input is not a regular file: " <> original
      verifyInputPath original canonical $ statusIdentity status
      bytes <- readDescriptor fd
      finalStatus <- getFdStatus fd
      unless (isRegularFile finalStatus && statusIdentity finalStatus == statusIdentity status) $
        replayOutputFailure $ "replay input changed while it was read: " <> original
      pure
        ReplayInput
          { replayInputOriginal = original
          , replayInputCanonical = canonical
          , replayInputDescriptor = fd
          , replayInputIdentity = statusIdentity status
          , replayInputBytes = bytes
          , replayInputSha256 = sha256Strict bytes
          }

revalidateReplayInputs :: [ReplayInput] -> IO ()
revalidateReplayInputs = traverse_ \input -> do
  status <- getFdStatus input.replayInputDescriptor
  unless (isRegularFile status && statusIdentity status == input.replayInputIdentity) $
    changedInput input
  verifyInputPath input.replayInputOriginal input.replayInputCanonical input.replayInputIdentity
  currentBytes <- readDescriptor input.replayInputDescriptor
  unless (sha256Strict currentBytes == input.replayInputSha256) $ changedInput input

readDescriptor :: Fd -> IO BS.ByteString
readDescriptor fd = do
  void $ fdSeek fd AbsoluteSeek 0
  BS.concat . reverse <$> go []
 where
  go chunks = do
    chunk <-
      fdRead fd 65536 `catch` \err ->
        if isEOFError err then pure BS.empty else throwIO (err :: IOException)
    if BS.null chunk then pure chunks else go (chunk : chunks)

verifyInputPath :: FilePath -> FilePath -> FileIdentity -> IO ()
verifyInputPath original canonical expected = do
  status <- getSymbolicLinkStatus canonical `catch` pathFailure "revalidate" original
  unless (isRegularFile status && not (isSymbolicLink status) && statusIdentity status == expected) $
    replayOutputFailure $ "replay input changed during execution: " <> original

changedInput :: ReplayInput -> IO a
changedInput input =
  replayOutputFailure $ "replay input changed during execution: " <> input.replayInputOriginal

prepareReplayOutputs :: [ReplayInput] -> [ReplayOutputRequest] -> IO ReplayOutputPlan
prepareReplayOutputs inputs requests = do
  when (null inputs) $ replayOutputFailure "at least one replay input is required"
  when (null requests) $ replayOutputFailure "at least one replay output path is required"
  let roles = map replayOutputRequestRole requests
  unless (length roles == length (List.nub roles)) $
    replayOutputFailure "replay output roles must be unique"
  unless (length (filter (== ReplayCheckpointOutput) roles) == 1) $
    replayOutputFailure "exactly one checkpoint output is required"
  outputs <- traverse resolveOutput requests
  rejectAliases
    $ [("input " <> i.replayInputOriginal, i.replayInputCanonical, Just i.replayInputIdentity) | i <- inputs]
    <> [ ("output " <> o.resolvedOutputOriginal, o.resolvedOutputCanonical, o.resolvedOutputExistingIdentity)
       | o <- outputs
       ]
  for_ outputs \output ->
    when
      ( output.resolvedOutputRole == ReplayCheckpointOutput
          && isJust output.resolvedOutputExistingIdentity
      )
      $ replayOutputFailure
      $ "checkpoint output already exists: "
      <> output.resolvedOutputOriginal
  pure ReplayOutputPlan {replayOutputPlanInputs = inputs, replayOutputPlanOutputs = outputs}

publishReplayOutputs :: ReplayOutputPlan -> [ReplayOutputArtifact] -> IO ()
publishReplayOutputs = publishReplayOutputsWithHook (const $ pure ())

publishReplayOutputsWithHook
  :: (ReplayPublishPhase -> IO ())
  -> ReplayOutputPlan
  -> [ReplayOutputArtifact]
  -> IO ()
publishReplayOutputsWithHook hook plan artifacts = mask \restore -> do
  artifactMap <- validateArtifacts plan artifacts
  staged <- stageAll restore hook plan.replayOutputPlanOutputs artifactMap
  let cleanupAll = traverse_ cleanupStage staged
      withCleanup action = restore (allowInterrupt >> action) `onException` cleanupAll
      (checkpointStages, secondaryStages) =
        List.partition
          ((== ReplayCheckpointOutput) . resolvedOutputRole . stagedOutputResolved)
          staged
  checkpoint <- case checkpointStages of
    [value] -> pure value
    _ -> cleanupAll >> replayOutputFailure "internal error: checkpoint stage is not unique"
  for_ secondaryStages \secondary -> do
    withCleanup $ revalidateReplayInputs plan.replayOutputPlanInputs
    withCleanup $ revalidateOutput plan secondary.stagedOutputResolved
    withCleanup $ publishStage secondary
    withCleanup $ hook $ ReplaySecondaryPublished secondary.stagedOutputResolved.resolvedOutputRole
  withCleanup $ revalidateReplayInputs plan.replayOutputPlanInputs
  withCleanup $ hook ReplayBeforeCheckpointPublish
  withCleanup $ publishCheckpoint hook plan checkpoint
  cleanupAll

validateArtifacts
  :: ReplayOutputPlan
  -> [ReplayOutputArtifact]
  -> IO (Map ReplayOutputRole BSL.ByteString)
validateArtifacts plan artifacts = do
  let expected = List.sort $ map resolvedOutputRole plan.replayOutputPlanOutputs
      supplied = List.sort $ map replayOutputArtifactRole artifacts
  unless (length supplied == length (List.nub supplied)) $
    replayOutputFailure "replay output artifacts contain duplicate roles"
  unless (supplied == expected) $
    replayOutputFailure "replay output artifacts do not match the prepared output plan"
  pure $ Map.fromList [(a.replayOutputArtifactRole, a.replayOutputArtifactBytes) | a <- artifacts]

stageAll
  :: (IO () -> IO ())
  -> (ReplayPublishPhase -> IO ())
  -> [ResolvedOutput]
  -> Map ReplayOutputRole BSL.ByteString
  -> IO [StagedOutput]
stageAll restore hook outputs artifacts = go outputs
 where
  go [] = hook ReplayOutputsStaged $> []
  go (output : rest) = do
    bytes <- maybe (replayOutputFailure "internal error: missing output artifact") pure
      $ Map.lookup output.resolvedOutputRole artifacts
    staged <- stageOutput restore hook output bytes
    remaining <- go rest `onException` cleanupStage staged
    pure $ staged : remaining

stageOutput
  :: (IO () -> IO ())
  -> (ReplayPublishPhase -> IO ())
  -> ResolvedOutput
  -> BSL.ByteString
  -> IO StagedOutput
stageOutput restore hook output bytes = do
  acquired@(stagePath, fd, identity) <- openStage output.resolvedOutputParent template
  let staged =
        StagedOutput
          { stagedOutputResolved = output
          , stagedOutputPath = stagePath
          , stagedOutputIdentity = identity
          }
      complete = do
        restore $ traverse_ (writeDescriptor fd) $ BSL.toChunks bytes
        restore $ fileSynchronise fd
        restore $ closeFd fd
        hook $ ReplayStageCompleted output.resolvedOutputRole
        pure staged
  complete `onException` cleanupOpenStage acquired
 where
  template = "." <> takeFileName output.resolvedOutputCanonical <> ".arkham-replay-stage"

openStage :: FilePath -> String -> IO (FilePath, Fd, FileIdentity)
openStage parent template = do
  (path, outputHandle) <- openBinaryTempFile parent template
  fd <-
    handleToFd outputHandle `onException` do
      ignoreIOException $ hClose outputHandle
  status <- getFdStatus fd `onException` ignoreIOException (closeFd fd)
  pure (path, fd, statusIdentity status)

cleanupOpenStage :: (FilePath, Fd, FileIdentity) -> IO ()
cleanupOpenStage (path, fd, identity) = do
  ignoreIOException $ closeFd fd
  removePathIfOwned path identity

writeDescriptor :: Fd -> BS.ByteString -> IO ()
writeDescriptor _ bytes | BS.null bytes = pure ()
writeDescriptor fd bytes = do
  written <- fromIntegral <$> fdWrite fd bytes
  when (written <= 0) $ replayOutputFailure "failed to write replay staging file"
  writeDescriptor fd $ BS.drop written bytes

cleanupStage :: StagedOutput -> IO ()
cleanupStage staged =
  removePathIfOwned staged.stagedOutputPath staged.stagedOutputIdentity

publishStage :: StagedOutput -> IO ()
publishStage staged = do
  renameFile staged.stagedOutputPath staged.stagedOutputResolved.resolvedOutputCanonical
  syncDirectory staged.stagedOutputResolved.resolvedOutputParent

publishCheckpoint
  :: (ReplayPublishPhase -> IO ())
  -> ReplayOutputPlan
  -> StagedOutput
  -> IO ()
publishCheckpoint hook plan staged = mask \restore -> do
  let output = staged.stagedOutputResolved
      destination = output.resolvedOutputCanonical
      rollback = removePublishedCheckpointIfOwned staged
  restore (revalidateOutput plan output)
  restore (createLink staged.stagedOutputPath destination) `onException` rollback
  ( restore $ do
      hook ReplayCheckpointLinked
      preserved <- destinationHasIdentity destination staged.stagedOutputIdentity
      unless preserved $
        replayOutputFailure "checkpoint publication did not preserve the staged file identity"
      syncDirectory output.resolvedOutputParent
    )
    `onException` rollback
  cleanupStage staged
  restore $ syncDirectory output.resolvedOutputParent

removePublishedCheckpointIfOwned :: StagedOutput -> IO ()
removePublishedCheckpointIfOwned staged = do
  let output = staged.stagedOutputResolved
      destination = output.resolvedOutputCanonical
  stageOwned <- pathHasIdentity staged.stagedOutputPath staged.stagedOutputIdentity
  destinationOwned <- destinationHasIdentity destination staged.stagedOutputIdentity
  when (stageOwned && destinationOwned) do
    ignoreIOException $ removeFile destination
    ignoreIOException $ syncDirectory output.resolvedOutputParent

revalidateOutput :: ReplayOutputPlan -> ResolvedOutput -> IO ()
revalidateOutput plan output = do
  parentStatus <- getFileStatus output.resolvedOutputParent
  unless
    (isDirectory parentStatus && statusIdentity parentStatus == output.resolvedOutputParentIdentity)
    $ replayOutputFailure
    $ "replay output parent changed during execution: "
    <> output.resolvedOutputOriginal
  currentStatus <- symbolicStatusMaybe output.resolvedOutputCanonical
  currentIdentity <- case currentStatus of
    Nothing -> pure Nothing
    Just status
      | isSymbolicLink status ->
          replayOutputFailure $ "replay output became a symbolic link: " <> output.resolvedOutputOriginal
      | not (isRegularFile status) ->
          replayOutputFailure $ "replay output became a special file: " <> output.resolvedOutputOriginal
      | otherwise -> pure $ Just $ statusIdentity status
  unless (currentIdentity == output.resolvedOutputExistingIdentity) $
    replayOutputFailure $ "replay output changed during execution: " <> output.resolvedOutputOriginal
  for_ currentIdentity \identity ->
    when (identity `elem` map replayInputIdentity plan.replayOutputPlanInputs) $
      replayOutputFailure $ "replay output became a hard-link alias of an input: " <> output.resolvedOutputOriginal

resolveOutput :: ReplayOutputRequest -> IO ResolvedOutput
resolveOutput ReplayOutputRequest {..} = do
  absolute <- normalise <$> makeAbsolute replayOutputRequestPath
  let parentSpelling = takeDirectory absolute
      fileName = takeFileName absolute
  when (null fileName || fileName == "." || fileName == "..") $
    replayOutputFailure $ "invalid replay output path: " <> replayOutputRequestPath
  parent <- canonicalizePath parentSpelling `catch` pathFailure "resolve output parent" parentSpelling
  parentStatus <- getFileStatus parent
  unless (isDirectory parentStatus) $
    replayOutputFailure $ "replay output parent is not a directory: " <> parentSpelling
  let canonical = parent </> fileName
  existingStatus <- symbolicStatusMaybe canonical
  existingIdentity <- case existingStatus of
    Nothing -> pure Nothing
    Just status
      | isSymbolicLink status ->
          replayOutputFailure $ "replay output is a symbolic link: " <> replayOutputRequestPath
      | isDirectory status ->
          replayOutputFailure $ "replay output is a directory: " <> replayOutputRequestPath
      | not (isRegularFile status) ->
          replayOutputFailure $ "replay output is a special file: " <> replayOutputRequestPath
      | otherwise -> pure $ Just $ statusIdentity status
  pure
    ResolvedOutput
      { resolvedOutputRole = replayOutputRequestRole
      , resolvedOutputOriginal = replayOutputRequestPath
      , resolvedOutputCanonical = canonical
      , resolvedOutputParent = parent
      , resolvedOutputParentIdentity = statusIdentity parentStatus
      , resolvedOutputExistingIdentity = existingIdentity
      }

rejectAliases :: [(String, FilePath, Maybe FileIdentity)] -> IO ()
rejectAliases entries =
  for_
    [ (left, right)
    | (entryIndex, left) <- zip [0 :: Int ..] entries
    , right <- drop (entryIndex + 1) entries
    ]
    \((leftDescription, leftPath, leftIdentity), (rightDescription, rightPath, rightIdentity)) -> do
      when (pathKey leftPath == pathKey rightPath) $
        replayOutputFailure
          $ "replay paths alias after canonical/case normalization: "
          <> leftDescription
          <> " and "
          <> rightDescription
      for_ ((,) <$> leftIdentity <*> rightIdentity) \(leftId, rightId) ->
        when (leftId == rightId) $
          replayOutputFailure
            $ "replay paths are hard-link aliases: "
            <> leftDescription
            <> " and "
            <> rightDescription

pathKey :: FilePath -> Text
pathKey = T.toCaseFold . T.pack . normalise

symbolicStatusMaybe :: FilePath -> IO (Maybe FileStatus)
symbolicStatusMaybe path =
  tryIOError (getSymbolicLinkStatus path) >>= \case
    Right status -> pure $ Just status
    Left err
      | isDoesNotExistError err -> pure Nothing
      | otherwise -> throwIO err

destinationHasIdentity :: FilePath -> FileIdentity -> IO Bool
destinationHasIdentity = pathHasIdentity

pathHasIdentity :: FilePath -> FileIdentity -> IO Bool
pathHasIdentity path expected =
  symbolicStatusMaybe path <&> \case
    Just status -> isRegularFile status && statusIdentity status == expected
    Nothing -> False

removePathIfOwned :: FilePath -> FileIdentity -> IO ()
removePathIfOwned path identity = do
  owned <- pathHasIdentity path identity
  when owned $ ignoreIOException $ removeFile path

statusIdentity :: FileStatus -> FileIdentity
statusIdentity status = (deviceID status, fileID status)

syncDirectory :: FilePath -> IO ()
syncDirectory path =
  bracket
    (openFd path ReadOnly defaultFileFlags {cloexec = True, directory = True})
    closeFd
    fileSynchronise

pathFailure :: String -> FilePath -> IOException -> IO a
pathFailure operation path err =
  replayOutputFailure $ "unable to " <> operation <> " replay path " <> path <> ": " <> show err

ignoreIOException :: IO () -> IO ()
ignoreIOException action = action `catch` \(_ :: IOException) -> pure ()

replayOutputFailure :: String -> IO a
replayOutputFailure = ioError . userError
