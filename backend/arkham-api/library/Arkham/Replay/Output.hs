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
  fileID,
  getFdStatus,
  getFileStatus,
  getSymbolicLinkStatus,
  isDirectory,
  isRegularFile,
  isSymbolicLink,
  ownerReadMode,
  ownerWriteMode,
  unionFileModes,
 )
import System.Posix.IO (
  OpenFileFlags (..),
  OpenMode (ReadOnly, WriteOnly),
  closeFd,
  defaultFileFlags,
  fdSeek,
  openFd,
  openFdAt,
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
  = ReplayBeforeStageCreate ReplayOutputRole
  | ReplayStageCompleted ReplayOutputRole
  | ReplayOutputsStaged
  | ReplayBeforeSecondaryPublish ReplayOutputRole
  | ReplaySecondaryPublished ReplayOutputRole
  | ReplayBeforeCheckpointPublish
  | ReplayCheckpointLinked
  deriving stock (Eq, Show)

data ResolvedOutput = ResolvedOutput
  { resolvedOutputRole :: ReplayOutputRole
  , resolvedOutputOriginal :: FilePath
  , resolvedOutputCanonical :: FilePath
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
  , stagedOutputName :: FilePath
  , stagedOutputDescriptor :: Fd
  , stagedOutputIdentity :: FileIdentity
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
        withCleanup $ publishStage secondary
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
  staged <- openStage output template
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
      pure OpenOutput {openOutputResolved = output, openOutputParentDescriptor = fd}

closeOpenOutput :: OpenOutput -> IO ()
closeOpenOutput = ignoreIOException . closeFd . openOutputParentDescriptor

openStage :: OpenOutput -> String -> IO StagedOutput
openStage output template = do
  name <- freshRelativeName template
  let flags =
        defaultFileFlags
          { cloexec = True
          , creat = Just $ ownerReadMode `unionFileModes` ownerWriteMode
          , exclusive = True
          , nofollow = True
          }
      parentFd = output.openOutputParentDescriptor
  tryIOError (openFdAt (Just parentFd) name WriteOnly flags) >>= \case
    Left err
      | isAlreadyExistsError err -> openStage output template
      | otherwise -> throwIO err
    Right fd ->
      bracketOnError (pure fd) closeFd \ownedFd -> do
        status <- getFdStatus ownedFd
        unless (isRegularFile status) $
          replayOutputFailure "replay staging path is not a regular file"
        -- Retaining this descriptor prevents inode reuse after a path replacement
        -- and carries ownership through publication or cleanup.
        pure
          StagedOutput
            { stagedOutputOpen = output
            , stagedOutputName = name
            , stagedOutputDescriptor = ownedFd
            , stagedOutputIdentity = statusIdentity status
            }

cleanupOpenStage :: StagedOutput -> IO ()
cleanupOpenStage staged =
  ignoreIOException (void $ removeEntryIfOwned staged staged.stagedOutputName)
    `finally` ignoreIOException (closeFd staged.stagedOutputDescriptor)

writeDescriptor :: Fd -> BS.ByteString -> IO ()
writeDescriptor _ bytes | BS.null bytes = pure ()
writeDescriptor fd bytes = do
  written <- fromIntegral <$> fdWrite fd bytes
  when (written <= 0) $ replayOutputFailure "failed to write replay staging file"
  writeDescriptor fd $ BS.drop written bytes

cleanupStage :: StagedOutput -> IO ()
cleanupStage staged =
  ignoreIOException (void $ removeEntryIfOwned staged staged.stagedOutputName)
    `finally` ignoreIOException (closeFd staged.stagedOutputDescriptor)

publishStage :: StagedOutput -> IO ()
publishStage staged = mask \restore -> do
  captured <- captureOwnedStage staged staged.stagedOutputName
  let parentFd = staged.stagedOutputOpen.openOutputParentDescriptor
      destination = staged.stagedOutputOpen.openOutputResolved.resolvedOutputName
      cleanupCaptured = ignoreIOException $ void $ removeEntryIfOwned staged captured
  restore (renameAt parentFd captured parentFd destination) `onException` cleanupCaptured
  restore $ syncDirectoryDescriptor parentFd

publishCheckpoint
  :: (ReplayPublishPhase -> IO ())
  -> ReplayOutputPlan
  -> StagedOutput
  -> IO ()
publishCheckpoint hook plan staged = mask \restore -> do
  let output = staged.stagedOutputOpen
      parentFd = output.openOutputParentDescriptor
      destination = output.openOutputResolved.resolvedOutputName
  restore $ revalidateOutput plan output
  captured <- captureOwnedStage staged staged.stagedOutputName
  let rollback = do
        removePublishedCheckpointIfOwned staged destination
        ignoreIOException $ void $ removeEntryIfOwned staged captured
  restore (linkAt parentFd captured parentFd destination) `onException` rollback
  ( restore $ do
      hook ReplayCheckpointLinked
      preserved <- entryHasIdentityAt parentFd destination staged.stagedOutputIdentity
      unless preserved $
        replayOutputFailure "checkpoint publication did not preserve the staged file identity"
      syncDirectoryDescriptor parentFd
    )
    `onException` rollback
  ignoreIOException $ void $ removeEntryIfOwned staged captured
  restore $ syncDirectoryDescriptor parentFd

removePublishedCheckpointIfOwned :: StagedOutput -> FilePath -> IO ()
removePublishedCheckpointIfOwned staged destination = do
  removed <- removeEntryIfOwned staged destination
  when removed $
    ignoreIOException $ syncDirectoryDescriptor staged.stagedOutputOpen.openOutputParentDescriptor

revalidateOutput :: ReplayOutputPlan -> OpenOutput -> IO ()
revalidateOutput plan output = do
  descriptorStatus <- getFdStatus output.openOutputParentDescriptor
  let resolved = output.openOutputResolved
  unless
    ( isDirectory descriptorStatus
        && statusIdentity descriptorStatus == resolved.resolvedOutputParentIdentity
    )
    $ replayOutputFailure
    $ "replay output parent descriptor changed during execution: "
    <> resolved.resolvedOutputOriginal
  parentStatus <- getSymbolicLinkStatus resolved.resolvedOutputParent
  unless
    ( isDirectory parentStatus
        && not (isSymbolicLink parentStatus)
        && statusIdentity parentStatus == resolved.resolvedOutputParentIdentity
    )
    $ replayOutputFailure
    $ "replay output parent changed during execution: "
    <> resolved.resolvedOutputOriginal
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

data CapturedEntry
  = CapturedEntryMissing
  | CapturedEntryForeign
  | CapturedEntryOwned FilePath

captureOwnedStage :: StagedOutput -> FilePath -> IO FilePath
captureOwnedStage staged source =
  captureEntry staged source >>= \case
    CapturedEntryOwned captured -> pure captured
    CapturedEntryMissing ->
      replayOutputFailure
        $ "replay staging file disappeared before publication: "
        <> staged.stagedOutputOpen.openOutputResolved.resolvedOutputOriginal
    CapturedEntryForeign ->
      replayOutputFailure
        $ "replay staging file changed before publication: "
        <> staged.stagedOutputOpen.openOutputResolved.resolvedOutputOriginal

removeEntryIfOwned :: StagedOutput -> FilePath -> IO Bool
removeEntryIfOwned staged source = do
  owned <- entryHasIdentityAt parentFd source staged.stagedOutputIdentity
  if not owned
    then pure False
    else
      captureEntry staged source >>= \case
        CapturedEntryOwned captured -> unlinkAt parentFd captured $> True
        CapturedEntryMissing -> pure False
        CapturedEntryForeign -> pure False
 where
  parentFd = staged.stagedOutputOpen.openOutputParentDescriptor

captureEntry :: StagedOutput -> FilePath -> IO CapturedEntry
captureEntry staged source = do
  ownerStatus <- getFdStatus staged.stagedOutputDescriptor
  unless
    (isRegularFile ownerStatus && statusIdentity ownerStatus == staged.stagedOutputIdentity)
    $ replayOutputFailure "replay staging descriptor changed during execution"
  captured <- freshRelativeName ".arkham-replay-capture"
  let parentFd = staged.stagedOutputOpen.openOutputParentDescriptor
  -- Atomically move the directory entry out of the attacker-visible name
  -- before checking or unlinking it.
  tryIOError (renameAt parentFd source parentFd captured) >>= \case
    Left err
      | isDoesNotExistError err -> pure CapturedEntryMissing
      | otherwise -> throwIO err
    Right () -> do
      owned <- entryHasIdentityAt parentFd captured staged.stagedOutputIdentity
      if owned
        then pure $ CapturedEntryOwned captured
        else restoreCapturedEntry parentFd captured source $> CapturedEntryForeign

restoreCapturedEntry :: Fd -> FilePath -> FilePath -> IO ()
restoreCapturedEntry parentFd captured destination =
  -- linkat is the no-clobber restoration step: a concurrent replacement at
  -- destination is preserved, and so is the captured foreign entry.
  tryIOError (linkAt parentFd captured parentFd destination) >>= \case
    Right () -> ignoreIOException $ unlinkAt parentFd captured
    Left _ -> pure ()

entryHasIdentityAt :: Fd -> FilePath -> FileIdentity -> IO Bool
entryHasIdentityAt parentFd path expected =
  tryIOError
    ( bracket
        ( openFdAt
            (Just parentFd)
            path
            ReadOnly
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

linkAt :: Fd -> FilePath -> Fd -> FilePath -> IO ()
linkAt oldDirectory oldPath newDirectory newPath =
  withCString oldPath \oldPathCString ->
    withCString newPath \newPathCString ->
      throwErrnoIfMinus1_ "linkat" $
        c_linkat
          (fromIntegral oldDirectory)
          oldPathCString
          (fromIntegral newDirectory)
          newPathCString
          0

unlinkAt :: Fd -> FilePath -> IO ()
unlinkAt directory path =
  withCString path \pathCString ->
    throwErrnoIfMinus1_ "unlinkat" $
      c_unlinkat (fromIntegral directory) pathCString 0

foreign import ccall unsafe "renameat"
  c_renameat :: CInt -> CString -> CInt -> CString -> IO CInt

foreign import ccall unsafe "linkat"
  c_linkat :: CInt -> CString -> CInt -> CString -> CInt -> IO CInt

foreign import ccall unsafe "unlinkat"
  c_unlinkat :: CInt -> CString -> CInt -> IO CInt

pathFailure :: String -> FilePath -> IOException -> IO a
pathFailure operation path err =
  replayOutputFailure $ "unable to " <> operation <> " replay path " <> path <> ": " <> show err

ignoreIOException :: IO () -> IO ()
ignoreIOException action = action `catch` \(_ :: IOException) -> pure ()

replayOutputFailure :: String -> IO a
replayOutputFailure = ioError . userError
