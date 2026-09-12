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
import Foreign.C.Error (throwErrnoIfMinus1_)
import Foreign.C.String (CString, withCString)
import Foreign.C.Types (CInt (..))
import System.Directory (canonicalizePath, makeAbsolute)
import System.FilePath (normalise, takeDirectory, takeFileName)
import System.Posix.Files (
  FileStatus,
  deviceID,
  fileMode,
  fileOwner,
  fileID,
  getFdStatus,
  getFileStatus,
  getSymbolicLinkStatus,
  groupModes,
  groupWriteMode,
  intersectFileModes,
  isDirectory,
  isRegularFile,
  isSymbolicLink,
  nullFileMode,
  otherModes,
  otherWriteMode,
  ownerModes,
  ownerReadMode,
  ownerWriteMode,
  setFdMode,
  unionFileModes,
 )
import System.Posix.IO (
  OpenFileFlags (..),
  OpenMode (ReadOnly, ReadWrite, WriteOnly),
  closeFd,
  defaultFileFlags,
  fdSeek,
  openFd,
  openFdAt,
 )
import System.Posix.IO.ByteString (fdRead, fdWrite)
import System.Posix.Types (DeviceID, Fd, FileID, FileMode)
import System.Posix.Unistd (fileSynchronise)
import System.Posix.User (getEffectiveUserID)

type FileIdentity = (DeviceID, FileID)

data ReplayInput = ReplayInput
  { replayInputOriginal :: FilePath
  , replayInputCanonical :: FilePath
  , replayInputParentSpelling :: FilePath
  , replayInputParent :: FilePath
  , replayInputParentIdentity :: FileIdentity
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
  = ReplayBeforeStageCreate ReplayOutputRole
  | ReplayStageCompleted ReplayOutputRole
  | ReplayOutputsStaged
  | ReplayBeforeSecondaryPublish ReplayOutputRole
  | ReplayBeforeDestinationCreate ReplayOutputRole
  | ReplayDestinationCreated ReplayOutputRole
  | ReplaySecondaryPublished ReplayOutputRole
  | ReplayBeforeCheckpointPublish
  | ReplayCheckpointLinked
  deriving stock (Eq, Show)

data ResolvedOutput = ResolvedOutput
  { resolvedOutputRole :: ReplayOutputRole
  , resolvedOutputOriginal :: FilePath
  , resolvedOutputCanonical :: FilePath
  , resolvedOutputParentSpelling :: FilePath
  , resolvedOutputParent :: FilePath
  , resolvedOutputName :: FilePath
  , resolvedOutputParentIdentity :: FileIdentity
  , resolvedOutputExistingIdentity :: Maybe FileIdentity
  }

data ReplayOutputPlan = ReplayOutputPlan
  { replayOutputPlanInputs :: [ReplayInput]
  , replayOutputPlanOutputs :: [ResolvedOutput]
  }

data OpenOutput = OpenOutput
  { openOutputResolved :: ResolvedOutput
  , openOutputParentDescriptor :: Fd
  }

data StagedOutput = StagedOutput
  { stagedOutputOpen :: OpenOutput
  , stagedOutputDescriptor :: Fd
  , stagedOutputIdentity :: FileIdentity
  , stagedOutputSha256 :: Text
  }

data CapturedOutput = CapturedOutput
  { capturedOutputOpen :: OpenOutput
  , capturedOutputName :: FilePath
  , capturedOutputDescriptor :: Fd
  , capturedOutputIdentity :: FileIdentity
  , capturedOutputMode :: FileMode
  }

data CreatedOutput = CreatedOutput
  { createdOutputOpen :: OpenOutput
  , createdOutputName :: FilePath
  , createdOutputDescriptor :: Fd
  , createdOutputIdentity :: FileIdentity
  }

stagedOutputRole :: StagedOutput -> ReplayOutputRole
stagedOutputRole = resolvedOutputRole . openOutputResolved . stagedOutputOpen

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
  parentStatus <- getFileStatus parent `catch` pathFailure "resolve" original
  spelledParentStatus <-
    getFileStatus parentSpelling `catch` pathFailure "resolve" original
  unless (isDirectory parentStatus) $
    replayOutputFailure $ "replay input parent is not a directory: " <> original
  unless
    ( isDirectory spelledParentStatus
        && statusIdentity spelledParentStatus == statusIdentity parentStatus
    )
    $ replayOutputFailure
    $ "replay input parent changed while it was resolved: "
    <> original
  let canonical = parent </> fileName
      flags = defaultFileFlags {nofollow = True, cloexec = True, nonBlock = True}
  bracketOnError
    (openFd canonical ReadOnly flags `catch` pathFailure "open" original)
    closeFd
    \fd -> do
      status <- getFdStatus fd
      unless (isRegularFile status) $
        replayOutputFailure $ "replay input is not a regular file: " <> original
      verifyInputPath
        original
        parentSpelling
        parent
        (statusIdentity parentStatus)
        canonical
        (statusIdentity status)
      bytes <- readDescriptor fd
      finalStatus <- getFdStatus fd
      unless (isRegularFile finalStatus && statusIdentity finalStatus == statusIdentity status) $
        replayOutputFailure $ "replay input changed while it was read: " <> original
      pure
        ReplayInput
          { replayInputOriginal = original
          , replayInputCanonical = canonical
          , replayInputParentSpelling = parentSpelling
          , replayInputParent = parent
          , replayInputParentIdentity = statusIdentity parentStatus
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
  verifyInputPath
    input.replayInputOriginal
    input.replayInputParentSpelling
    input.replayInputParent
    input.replayInputParentIdentity
    input.replayInputCanonical
    input.replayInputIdentity
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

verifyInputPath
  :: FilePath
  -> FilePath
  -> FilePath
  -> FileIdentity
  -> FilePath
  -> FileIdentity
  -> IO ()
verifyInputPath original parentSpelling parent expectedParent canonical expectedInput = do
  spelledParentStatus <-
    getFileStatus parentSpelling `catch` pathFailure "revalidate" original
  unless
    ( isDirectory spelledParentStatus
        && statusIdentity spelledParentStatus == expectedParent
    )
    $ replayOutputFailure
    $ "replay input parent changed during execution: "
    <> original
  parentStatus <-
    getSymbolicLinkStatus parent `catch` pathFailure "revalidate" original
  unless
    ( isDirectory parentStatus
        && not (isSymbolicLink parentStatus)
        && statusIdentity parentStatus == expectedParent
    )
    $ replayOutputFailure
    $ "replay input parent changed during execution: "
    <> original
  status <- getSymbolicLinkStatus canonical `catch` pathFailure "revalidate" original
  unless
    ( isRegularFile status
        && not (isSymbolicLink status)
        && statusIdentity status == expectedInput
    )
    $ replayOutputFailure
    $ "replay input changed during execution: "
    <> original

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
  opened <- openReplayOutputs plan.replayOutputPlanOutputs
  let closeParents = traverse_ closeOpenOutput opened
  ( do
      staged <- stageAll restore hook opened artifactMap
      let cleanupAll = traverse_ cleanupStage staged
          withCleanup action = restore (allowInterrupt >> action) `onException` cleanupAll
          (checkpointStages, secondaryStages) =
            List.partition ((== ReplayCheckpointOutput) . stagedOutputRole) staged
      checkpoint <- case checkpointStages of
        [value] -> pure value
        _ -> cleanupAll >> replayOutputFailure "internal error: checkpoint stage is not unique"
      for_ secondaryStages \secondary -> do
        withCleanup $ revalidateReplayInputs plan.replayOutputPlanInputs
        withCleanup $ revalidateOutput plan secondary.stagedOutputOpen
        withCleanup $ hook $ ReplayBeforeSecondaryPublish $ stagedOutputRole secondary
        withCleanup $ publishStage hook plan secondary
        withCleanup $ hook $ ReplaySecondaryPublished $ stagedOutputRole secondary
      withCleanup $ revalidateReplayInputs plan.replayOutputPlanInputs
      withCleanup $ hook ReplayBeforeCheckpointPublish
      withCleanup $ publishCheckpoint hook plan checkpoint
      cleanupAll
    )
    `finally` closeParents

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
  -> [OpenOutput]
  -> Map ReplayOutputRole BSL.ByteString
  -> IO [StagedOutput]
stageAll restore hook outputs artifacts = go outputs
 where
  go [] = hook ReplayOutputsStaged $> []
  go (output : rest) = do
    bytes <- maybe (replayOutputFailure "internal error: missing output artifact") pure
      $ Map.lookup output.openOutputResolved.resolvedOutputRole artifacts
    staged <- stageOutput restore hook output bytes
    remaining <- go rest `onException` cleanupStage staged
    pure $ staged : remaining

stageOutput
  :: (IO () -> IO ())
  -> (ReplayPublishPhase -> IO ())
  -> OpenOutput
  -> BSL.ByteString
  -> IO StagedOutput
stageOutput restore hook output bytes = do
  hook $ ReplayBeforeStageCreate role
  staged <- openStage output template $ sha256Strict $ BSL.toStrict bytes
  let complete = do
        restore $ traverse_ (writeDescriptor staged.stagedOutputDescriptor) $ BSL.toChunks bytes
        restore $ fileSynchronise staged.stagedOutputDescriptor
        hook $ ReplayStageCompleted role
        pure staged
  complete `onException` cleanupOpenStage staged
 where
  role = output.openOutputResolved.resolvedOutputRole
  template = "." <> output.openOutputResolved.resolvedOutputName <> ".arkham-replay-stage"

openReplayOutputs :: [ResolvedOutput] -> IO [OpenOutput]
openReplayOutputs = go
 where
  go [] = pure []
  go (output : rest) = do
    opened <- openReplayOutput output
    remaining <- go rest `onException` closeOpenOutput opened
    pure $ opened : remaining

openReplayOutput :: ResolvedOutput -> IO OpenOutput
openReplayOutput output =
  bracketOnError
    ( openFd
        output.resolvedOutputParent
        ReadOnly
        defaultFileFlags {cloexec = True, directory = True, nofollow = True}
        `catch` pathFailure "open output parent" output.resolvedOutputOriginal
    )
    closeFd
    \fd -> do
      status <- getFdStatus fd
      unless
        (isDirectory status && statusIdentity status == output.resolvedOutputParentIdentity)
        $ replayOutputFailure
        $ "replay output parent changed during execution: "
        <> output.resolvedOutputOriginal
      validateOutputParentPermissions output.resolvedOutputOriginal status
      pure OpenOutput {openOutputResolved = output, openOutputParentDescriptor = fd}

closeOpenOutput :: OpenOutput -> IO ()
closeOpenOutput = ignoreIOException . closeFd . openOutputParentDescriptor

openStage :: OpenOutput -> String -> Text -> IO StagedOutput
openStage output template contentSha256 = do
  name <- freshRelativeName template
  let flags =
        defaultFileFlags
          { cloexec = True
          , creat = Just $ ownerReadMode `unionFileModes` ownerWriteMode
          , exclusive = True
          , nofollow = True
          }
      parentFd = output.openOutputParentDescriptor
      cleanupCreated fd =
        ignoreIOException
          ( do
              status <- getFdStatus fd
              when (isRegularFile status) $
                void $
                  removeReadableEntryWithIdentity output name $ statusIdentity status
          )
          `finally` ignoreIOException (closeFd fd)
  tryIOError (openFdAt (Just parentFd) name ReadWrite flags) >>= \case
    Left err
      | isAlreadyExistsError err -> openStage output template contentSha256
      | otherwise -> throwIO err
    Right fd ->
      bracketOnError (pure fd) cleanupCreated \ownedFd -> do
        status <- getFdStatus ownedFd
        unless (isRegularFile status) $
          replayOutputFailure "replay staging path is not a regular file"
        let identity = statusIdentity status
        -- Detach the stage immediately. Publication copies only from this
        -- retained descriptor, so no later pathname replacement can select
        -- bytes for publication or cleanup.
        detached <- removeReadableEntryWithIdentity output name identity
        unless detached $
          replayOutputFailure "replay staging path changed during acquisition"
        detachedStatus <- getFdStatus ownedFd
        unless (isRegularFile detachedStatus && statusIdentity detachedStatus == identity) $
          replayOutputFailure "replay staging descriptor changed during acquisition"
        pure
          StagedOutput
            { stagedOutputOpen = output
            , stagedOutputDescriptor = ownedFd
            , stagedOutputIdentity = identity
            , stagedOutputSha256 = contentSha256
            }

cleanupOpenStage :: StagedOutput -> IO ()
cleanupOpenStage = ignoreIOException . closeFd . stagedOutputDescriptor

writeDescriptor :: Fd -> BS.ByteString -> IO ()
writeDescriptor _ bytes | BS.null bytes = pure ()
writeDescriptor fd bytes = do
  written <- fromIntegral <$> fdWrite fd bytes
  when (written <= 0) $ replayOutputFailure "failed to write replay staging file"
  writeDescriptor fd $ BS.drop written bytes

cleanupStage :: StagedOutput -> IO ()
cleanupStage = ignoreIOException . closeFd . stagedOutputDescriptor

publishStage
  :: (ReplayPublishPhase -> IO ())
  -> ReplayOutputPlan
  -> StagedOutput
  -> IO ()
publishStage hook plan staged =
  publishStagedOutput hook plan staged $ pure ()

publishCheckpoint
  :: (ReplayPublishPhase -> IO ())
  -> ReplayOutputPlan
  -> StagedOutput
  -> IO ()
publishCheckpoint hook plan staged =
  publishStagedOutput hook plan staged $ hook ReplayCheckpointLinked

publishStagedOutput
  :: (ReplayPublishPhase -> IO ())
  -> ReplayOutputPlan
  -> StagedOutput
  -> IO ()
  -> IO ()
publishStagedOutput hook plan staged afterPublish = mask \restore -> do
  let output = staged.stagedOutputOpen
      role = stagedOutputRole staged
  restore $ revalidateOutput plan output
  captured <- captureExpectedOutput output
  let restoreCaptured = traverse_ restoreCapturedOutput captured
  created <-
    ( do
        restore $ hook $ ReplayBeforeDestinationCreate role
        restore $ revalidateOutputParent output
        createOutput output
    )
      `onException` restoreCaptured
  let rollback =
        removeCreatedOutputIfOwned created
          `finally` ( restoreCaptured
                        `finally` ignoreIOException (closeFd created.createdOutputDescriptor)
                    )
  ( restore $ do
      hook $ ReplayDestinationCreated role
      revalidateOutputParent output
      copyStagedOutput staged created
      revalidateOutputParent output
      afterPublish
    )
    `onException` rollback
  ignoreIOException $ closeFd created.createdOutputDescriptor
  traverse_ (ignoreIOException . discardCapturedOutput) captured

createOutput :: OpenOutput -> IO CreatedOutput
createOutput output =
  createNamedOutput output output.openOutputResolved.resolvedOutputName

createNamedOutput :: OpenOutput -> FilePath -> IO CreatedOutput
createNamedOutput output destination = do
  let parentFd = output.openOutputParentDescriptor
      flags =
        defaultFileFlags
          { cloexec = True
          , creat = Just ownerWriteMode
          , exclusive = True
          , nofollow = True
          }
  fd <-
    openFdAt (Just parentFd) destination ReadWrite flags
      `catch` pathFailure "create" output.openOutputResolved.resolvedOutputOriginal
  bracketOnError (pure fd) closeFd \ownedFd -> do
    status <- getFdStatus ownedFd
    unless (isRegularFile status) $
      replayOutputFailure "created replay output is not a regular file"
    pure
      CreatedOutput
        { createdOutputOpen = output
        , createdOutputName = destination
        , createdOutputDescriptor = ownedFd
        , createdOutputIdentity = statusIdentity status
        }

copyStagedOutput :: StagedOutput -> CreatedOutput -> IO ()
copyStagedOutput staged created = do
  bytes <-
    readRetainedDescriptor
      staged.stagedOutputDescriptor
      staged.stagedOutputIdentity
  unless (sha256Strict bytes == staged.stagedOutputSha256) $
    replayOutputFailure "replay staging descriptor contents changed before publication"
  publishDescriptorBytes
    created
    (ownerReadMode `unionFileModes` ownerWriteMode)
    bytes

ensureCreatedOutputOwned :: CreatedOutput -> IO ()
ensureCreatedOutputOwned created = do
  descriptorStatus <- getFdStatus created.createdOutputDescriptor
  let output = created.createdOutputOpen
  pathOwned <-
    entryHasIdentityAtWithMode
      WriteOnly
      output.openOutputParentDescriptor
      created.createdOutputName
      created.createdOutputIdentity
  unless
    ( isRegularFile descriptorStatus
        && statusIdentity descriptorStatus == created.createdOutputIdentity
        && pathOwned
    )
    $ replayOutputFailure "created replay output changed before publication"

publishDescriptorBytes :: CreatedOutput -> FileMode -> BS.ByteString -> IO ()
publishDescriptorBytes created mode bytes = do
  ensureCreatedOutputOwned created
  writeDescriptor created.createdOutputDescriptor bytes
  fileSynchronise created.createdOutputDescriptor
  -- The destination is owner-write-only until every byte is durable. fchmod
  -- is the single readable-publication gate and acts on the retained fd.
  setFdMode created.createdOutputDescriptor mode
  fileSynchronise created.createdOutputDescriptor
  descriptorStatus <- getFdStatus created.createdOutputDescriptor
  unless
    (isRegularFile descriptorStatus && statusIdentity descriptorStatus == created.createdOutputIdentity)
    $ replayOutputFailure "created replay output descriptor changed during publication"
  let output = created.createdOutputOpen
      parentFd = output.openOutputParentDescriptor
      destination = created.createdOutputName
  preserved <-
    entryHasIdentityAtWithMode WriteOnly parentFd destination created.createdOutputIdentity
  unless preserved $
    replayOutputFailure "replay output path changed before the readable-publication gate completed"
  syncDirectoryDescriptor parentFd

readRetainedDescriptor :: Fd -> FileIdentity -> IO BS.ByteString
readRetainedDescriptor fd expected = do
  before <- getFdStatus fd
  unless (isRegularFile before && statusIdentity before == expected) $
    replayOutputFailure "retained replay descriptor changed before publication"
  bytes <- readDescriptor fd
  after <- getFdStatus fd
  unless (isRegularFile after && statusIdentity after == expected) $
    replayOutputFailure "retained replay descriptor changed during publication"
  pure bytes

revalidateOutput :: ReplayOutputPlan -> OpenOutput -> IO ()
revalidateOutput plan output = do
  revalidateOutputParent output
  let resolved = output.openOutputResolved
  currentStatus <- symbolicStatusMaybe resolved.resolvedOutputCanonical
  currentIdentity <- case currentStatus of
    Nothing -> pure Nothing
    Just status
      | isSymbolicLink status ->
          replayOutputFailure $ "replay output became a symbolic link: " <> resolved.resolvedOutputOriginal
      | not (isRegularFile status) ->
          replayOutputFailure $ "replay output became a special file: " <> resolved.resolvedOutputOriginal
      | otherwise -> pure $ Just $ statusIdentity status
  unless (currentIdentity == resolved.resolvedOutputExistingIdentity) $
    replayOutputFailure $ "replay output changed during execution: " <> resolved.resolvedOutputOriginal
  for_ currentIdentity \identity ->
    when (identity `elem` map replayInputIdentity plan.replayOutputPlanInputs) $
      replayOutputFailure $ "replay output became a hard-link alias of an input: " <> resolved.resolvedOutputOriginal

revalidateOutputParent :: OpenOutput -> IO ()
revalidateOutputParent output = do
  descriptorStatus <- getFdStatus output.openOutputParentDescriptor
  let resolved = output.openOutputResolved
  unless
    ( isDirectory descriptorStatus
        && statusIdentity descriptorStatus == resolved.resolvedOutputParentIdentity
    )
    $ replayOutputFailure
    $ "replay output parent descriptor changed during execution: "
    <> resolved.resolvedOutputOriginal
  validateOutputParentPermissions resolved.resolvedOutputOriginal descriptorStatus
  spelledParentStatus <-
    getFileStatus resolved.resolvedOutputParentSpelling
      `catch` pathFailure "revalidate output parent" resolved.resolvedOutputOriginal
  unless
    ( isDirectory spelledParentStatus
        && statusIdentity spelledParentStatus == resolved.resolvedOutputParentIdentity
    )
    $ replayOutputFailure
    $ "replay output parent changed during execution: "
    <> resolved.resolvedOutputOriginal
  validateOutputParentPermissions resolved.resolvedOutputOriginal spelledParentStatus
  parentStatus <- getSymbolicLinkStatus resolved.resolvedOutputParent
  unless
    ( isDirectory parentStatus
        && not (isSymbolicLink parentStatus)
        && statusIdentity parentStatus == resolved.resolvedOutputParentIdentity
    )
    $ replayOutputFailure
    $ "replay output parent changed during execution: "
    <> resolved.resolvedOutputOriginal
  validateOutputParentPermissions resolved.resolvedOutputOriginal parentStatus

resolveOutput :: ReplayOutputRequest -> IO ResolvedOutput
resolveOutput ReplayOutputRequest {..} = do
  absolute <- normalise <$> makeAbsolute replayOutputRequestPath
  let parentSpelling = takeDirectory absolute
      fileName = takeFileName absolute
  when (null fileName || fileName == "." || fileName == "..") $
    replayOutputFailure $ "invalid replay output path: " <> replayOutputRequestPath
  parent <- canonicalizePath parentSpelling `catch` pathFailure "resolve output parent" parentSpelling
  parentStatus <- getFileStatus parent
  spelledParentStatus <-
    getFileStatus parentSpelling `catch` pathFailure "resolve output parent" parentSpelling
  unless (isDirectory parentStatus) $
    replayOutputFailure $ "replay output parent is not a directory: " <> parentSpelling
  unless
    ( isDirectory spelledParentStatus
        && statusIdentity spelledParentStatus == statusIdentity parentStatus
    )
    $ replayOutputFailure
    $ "replay output parent changed while it was resolved: "
    <> replayOutputRequestPath
  validateOutputParentPermissions replayOutputRequestPath parentStatus
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
      , resolvedOutputParentSpelling = parentSpelling
      , resolvedOutputParent = parent
      , resolvedOutputName = fileName
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

validateOutputParentPermissions :: FilePath -> FileStatus -> IO ()
validateOutputParentPermissions original status = do
  effectiveUser <- getEffectiveUserID
  unless (fileOwner status == effectiveUser) $
    replayOutputFailure $ "replay output parent is not owned by the running user: " <> original
  let unsafeWriteModes = groupWriteMode `unionFileModes` otherWriteMode
  unless (fileMode status `intersectFileModes` unsafeWriteModes == nullFileMode) $
    replayOutputFailure $ "replay output parent has group/world write mode bits: " <> original

captureExpectedOutput :: OpenOutput -> IO (Maybe CapturedOutput)
captureExpectedOutput output =
  for output.openOutputResolved.resolvedOutputExistingIdentity \expected -> do
    let parentFd = output.openOutputParentDescriptor
        destination = output.openOutputResolved.resolvedOutputName
    capturedName <-
      freshRelativeName $ "." <> destination <> ".arkham-replay-replaced"
    opened <- openCapturedOutput output destination
    unless (opened.capturedOutputIdentity == expected) $ do
      ignoreIOException $ closeFd opened.capturedOutputDescriptor
      replayOutputFailure
        $ "replay output changed before it was captured: "
        <> output.openOutputResolved.resolvedOutputOriginal
    let captured = opened {capturedOutputName = capturedName}
    renameAt parentFd destination parentFd capturedName
      `catch` \err -> do
        ignoreIOException $ closeFd captured.capturedOutputDescriptor
        pathFailure "capture existing output" output.openOutputResolved.resolvedOutputOriginal err
    ( do
        capturedPathOwned <-
          entryHasIdentityAtWithMode ReadOnly parentFd capturedName expected
        unless capturedPathOwned $
          replayOutputFailure
            $ "replay output changed while it was captured: "
            <> output.openOutputResolved.resolvedOutputOriginal
        pure captured
      )
      `onException` restoreCapturedOutput captured

openCapturedOutput :: OpenOutput -> FilePath -> IO CapturedOutput
openCapturedOutput = openCapturedOutputWithMode ReadOnly

openCapturedOutputWithMode :: OpenMode -> OpenOutput -> FilePath -> IO CapturedOutput
openCapturedOutputWithMode mode output name = do
  let parentFd = output.openOutputParentDescriptor
      flags = defaultFileFlags {cloexec = True, nonBlock = True, nofollow = True}
  fd <-
    openFdAt (Just parentFd) name mode flags
      `catch` pathFailure "open captured output" output.openOutputResolved.resolvedOutputOriginal
  bracketOnError (pure fd) closeFd \ownedFd -> do
    status <- getFdStatus ownedFd
    unless (isRegularFile status) $
      replayOutputFailure "captured replay output is not a regular file"
    pure
      CapturedOutput
        { capturedOutputOpen = output
        , capturedOutputName = name
        , capturedOutputDescriptor = ownedFd
        , capturedOutputIdentity = statusIdentity status
        , capturedOutputMode =
            fileMode status
              `intersectFileModes` (ownerModes `unionFileModes` groupModes `unionFileModes` otherModes)
        }

restoreCapturedOutput :: CapturedOutput -> IO ()
restoreCapturedOutput captured =
  ( do
      restored <-
        try @_ @IOException do
          created <-
            createNamedOutput
              captured.capturedOutputOpen
              captured.capturedOutputOpen.openOutputResolved.resolvedOutputName
          let cleanupCreated =
                removeCreatedOutputIfOwned created
                  `finally` ignoreIOException (closeFd created.createdOutputDescriptor)
          bytes <-
            readRetainedDescriptor
              captured.capturedOutputDescriptor
              captured.capturedOutputIdentity
              `onException` cleanupCreated
          publishDescriptorBytes created captured.capturedOutputMode bytes
            `onException` cleanupCreated
          ignoreIOException $ closeFd created.createdOutputDescriptor
          pure ()
      case restored of
        Left _ -> pure ()
        Right () ->
          ignoreIOException $
            void $
              removeReadableEntryWithIdentity
                captured.capturedOutputOpen
                captured.capturedOutputName
                captured.capturedOutputIdentity
    )
    `finally` ignoreIOException (closeFd captured.capturedOutputDescriptor)

discardCapturedOutput :: CapturedOutput -> IO ()
discardCapturedOutput captured =
  ( do
      removed <-
        removeReadableEntryWithIdentity
          captured.capturedOutputOpen
          captured.capturedOutputName
          captured.capturedOutputIdentity
      unless removed $
        replayOutputFailure
          $ "captured prior replay output changed before cleanup: "
          <> captured.capturedOutputOpen.openOutputResolved.resolvedOutputOriginal
    )
    `finally` ignoreIOException (closeFd captured.capturedOutputDescriptor)

removeCreatedOutputIfOwned :: CreatedOutput -> IO ()
removeCreatedOutputIfOwned created = do
  removed <-
    removeCreatedEntryWithIdentity
      created.createdOutputOpen
      created.createdOutputName
      created.createdOutputIdentity
  when removed $
    ignoreIOException
      $ syncDirectoryDescriptor created.createdOutputOpen.openOutputParentDescriptor

removeReadableEntryWithIdentity :: OpenOutput -> FilePath -> FileIdentity -> IO Bool
removeReadableEntryWithIdentity =
  removeEntryWithIdentity ReadOnly preserveCapturedEntry

removeCreatedEntryWithIdentity :: OpenOutput -> FilePath -> FileIdentity -> IO Bool
removeCreatedEntryWithIdentity =
  removeEntryWithIdentity WriteOnly \_ _ -> pure ()

removeEntryWithIdentity
  :: OpenMode
  -> (CapturedOutput -> FilePath -> IO ())
  -> OpenOutput
  -> FilePath
  -> FileIdentity
  -> IO Bool
removeEntryWithIdentity mode preserveMismatch output source expected = do
  let parentFd = output.openOutputParentDescriptor
  initiallyOwned <- entryHasIdentityAtWithMode mode parentFd source expected
  if not initiallyOwned
    then pure False
    else do
      capturedName <- freshRelativeName ".arkham-replay-capture"
      tryIOError (renameAt parentFd source parentFd capturedName) >>= \case
        Left err
          | isDoesNotExistError err -> pure False
          | otherwise -> throwIO err
        Right () -> do
          captured <- openCapturedOutputWithMode mode output capturedName
          if captured.capturedOutputIdentity == expected
            then
              ( do
                  -- Portable POSIX has no unlink-by-fd. The verification fd
                  -- stays open through unlink, and the parent is owner-only;
                  -- a cooperating same-UID process can still race this final
                  -- pathname operation, which is documented as a limit.
                  unlinkAt parentFd capturedName
                  status <- getFdStatus captured.capturedOutputDescriptor
                  unless (statusIdentity status == expected) $
                    replayOutputFailure "captured replay output descriptor changed during cleanup"
                  pure True
              )
                `finally` ignoreIOException (closeFd captured.capturedOutputDescriptor)
            else do
              preserveMismatch captured source
              ignoreIOException $ closeFd captured.capturedOutputDescriptor
              pure False

preserveCapturedEntry :: CapturedOutput -> FilePath -> IO ()
preserveCapturedEntry captured destination =
  void
    $ try @_ @IOException do
      created <- createNamedOutput captured.capturedOutputOpen destination
      let cleanupCreated =
            removeCreatedOutputIfOwned created
              `finally` ignoreIOException (closeFd created.createdOutputDescriptor)
      bytes <-
        readRetainedDescriptor
          captured.capturedOutputDescriptor
          captured.capturedOutputIdentity
          `onException` cleanupCreated
      publishDescriptorBytes created captured.capturedOutputMode bytes
        `onException` cleanupCreated
      ignoreIOException $ closeFd created.createdOutputDescriptor

entryHasIdentityAtWithMode :: OpenMode -> Fd -> FilePath -> FileIdentity -> IO Bool
entryHasIdentityAtWithMode mode parentFd path expected =
  tryIOError
    ( bracket
        ( openFdAt
            (Just parentFd)
            path
            mode
            defaultFileFlags {cloexec = True, nonBlock = True, nofollow = True}
        )
        closeFd
        \fd -> do
          status <- getFdStatus fd
          pure $ isRegularFile status && statusIdentity status == expected
    )
    <&> either (const False) id

freshRelativeName :: String -> IO FilePath
freshRelativeName prefix = do
  left <- getRandom :: IO Word64
  right <- getRandom :: IO Word64
  pure $ prefix <> "-" <> show left <> "-" <> show right

statusIdentity :: FileStatus -> FileIdentity
statusIdentity status = (deviceID status, fileID status)

syncDirectoryDescriptor :: Fd -> IO ()
syncDirectoryDescriptor = fileSynchronise

renameAt :: Fd -> FilePath -> Fd -> FilePath -> IO ()
renameAt oldDirectory oldPath newDirectory newPath =
  withCString oldPath \oldPathCString ->
    withCString newPath \newPathCString ->
      throwErrnoIfMinus1_ "renameat" $
        c_renameat
          (fromIntegral oldDirectory)
          oldPathCString
          (fromIntegral newDirectory)
          newPathCString

unlinkAt :: Fd -> FilePath -> IO ()
unlinkAt directory path =
  withCString path \pathCString ->
    throwErrnoIfMinus1_ "unlinkat" $
      c_unlinkat (fromIntegral directory) pathCString 0

foreign import ccall unsafe "renameat"
  c_renameat :: CInt -> CString -> CInt -> CString -> IO CInt

foreign import ccall unsafe "unlinkat"
  c_unlinkat :: CInt -> CString -> CInt -> IO CInt

pathFailure :: String -> FilePath -> IOException -> IO a
pathFailure operation path err =
  replayOutputFailure $ "unable to " <> operation <> " replay path " <> path <> ": " <> show err

ignoreIOException :: IO () -> IO ()
ignoreIOException action = action `catch` \(_ :: IOException) -> pure ()

replayOutputFailure :: String -> IO a
replayOutputFailure = ioError . userError
