module Arkham.Replay.OutputSpec (spec) where

import Arkham.Replay.Output
import Control.Concurrent (ThreadId, myThreadId, yield)
import Control.Concurrent.Async (async)
import Control.Exception qualified as E
import Data.ByteString.Lazy.Char8 qualified as BSL8
import Data.Either (isLeft, isRight)
import Data.List qualified as List
import GHC.Conc (BlockReason (..), ThreadStatus (..), threadStatus)
import System.Directory (
  createDirectory,
  createDirectoryIfMissing,
  createFileLink,
  doesFileExist,
  getCurrentDirectory,
  listDirectory,
  removeFile,
  removePathForcibly,
  renameFile,
 )
import System.Posix.Files (
  createLink,
  createNamedPipe,
  ownerReadMode,
  ownerWriteMode,
  unionFileModes,
 )
import TestImport.New

spec :: Spec
spec = describe "deterministic replay file handling" do
  it "opens regular inputs without following symlinks or accepting special files" $
    withWorkspace "input-types" \workspace -> do
      let target = workspace </> "target"
          symlinkInput = workspace </> "link"
          directory = workspace </> "directory"
          fifo = workspace </> "fifo"
      BSL8.writeFile target "source"
      createFileLink target symlinkInput
      createDirectory directory
      createNamedPipe fifo $ ownerReadMode `unionFileModes` ownerWriteMode
      expectIOExceptionContaining "open" $ withReplayInput symlinkInput $ const $ pure ()
      expectIOExceptionContaining "regular file" $ withReplayInput directory $ const $ pure ()
      expectIOExceptionContaining "regular file" $ withReplayInput fifo $ const $ pure ()

  it "retains descriptor bytes and rejects path retargeting or in-place changes" $
    withWorkspace "input-races" \workspace -> do
      let input = workspace </> "source"
          original = workspace </> "original"
      BSL8.writeFile input "source"
      withReplayInput input \opened -> do
        replayInputBytes opened `shouldBe` "source"
        renameFile input original
        BSL8.writeFile input "replacement"
        replayInputBytes opened `shouldBe` "source"
        expectIOExceptionContaining "changed" $ revalidateReplayInputs [opened]
      withReplayInput original \opened -> do
        BSL8.writeFile original "mutated"
        expectIOExceptionContaining "changed" $ revalidateReplayInputs [opened]

  it "rejects path, case, hard-link, directory, symlink, and special output aliases" $
    withWorkspace "output-types" \workspace -> do
      let input = workspace </> "source.json"
          directory = workspace </> "directory"
          symlink = workspace </> "symlink"
          hardlink = workspace </> "hardlink"
          fifo = workspace </> "fifo"
          target = workspace </> "target"
      BSL8.writeFile input "source"
      BSL8.writeFile target "target"
      createDirectory directory
      createFileLink target symlink
      createLink input hardlink
      createNamedPipe fifo $ ownerReadMode `unionFileModes` ownerWriteMode
      withReplayInput input \opened ->
        for_
          [ ("alias", workspace </> "." </> "source.json")
          , ("alias", workspace </> "SOURCE.JSON")
          , ("directory", directory)
          , ("symbolic link", symlink)
          , ("hard-link", hardlink)
          , ("special file", fifo)
          ]
          \(message, output) ->
            expectIOExceptionContaining message $
              prepareReplayOutputs [opened] [checkpointRequest output]

  it "transfers each completed stage to masked ownership before cancellation" $
    withWorkspace "stage-acquisition-cancellation" \workspace -> do
      let input = workspace </> "source"
          checkpoint = workspace </> "checkpoint"
          secondary = workspace </> "game"
      BSL8.writeFile input "source"
      withReplayInput input \opened -> do
        plan <-
          prepareReplayOutputs
            [opened]
            [checkpointRequest checkpoint, ReplayOutputRequest ReplayFinalGameOutput secondary]
        checkpointStage <- newEmptyMVar
        foreignPath <- newEmptyMVar
        completed <- newEmptyMVar
        sender <- newEmptyMVar
        armed <- newEmptyMVar
        worker <-
          async
            $ publishReplayOutputsWithHook
              ( \case
                  ReplayStageCompleted ReplayCheckpointOutput -> do
                    stagedPaths workspace >>= \case
                      [path] -> void $ tryPutMVar checkpointStage path
                      other -> expectationFailure ("expected one completed stage, got " <> show other)
                  ReplayStageCompleted ReplayFinalGameOutput -> do
                    path <-
                      tryReadMVar checkpointStage
                        >>= maybe (expectationFailure "checkpoint stage was not retained" >> error "unreachable") pure
                    removeFile path
                    BSL8.writeFile path "foreign"
                    void $ tryPutMVar foreignPath path
                    holdMaskedHandoff completed sender armed
                  _ -> pure ()
              )
              plan
              [artifact ReplayCheckpointOutput "checkpoint", artifact ReplayFinalGameOutput "game"]
        cancelPendingAt worker completed sender armed
        path <- readMVar foreignPath
        BSL8.readFile path `shouldReturn` "foreign"
        doesFileExist checkpoint `shouldReturn` False
        doesFileExist secondary `shouldReturn` False
        stagedPaths workspace `shouldReturn` [path]
        removeFile path
      assertNoStages workspace

  it "arms cleanup before delivering cancellation pending at the acquisition handoff" $
    withWorkspace "acquisition-cancellation" \workspace -> do
      let input = workspace </> "source"
          checkpoint = workspace </> "checkpoint"
          secondary = workspace </> "game"
      BSL8.writeFile input "source"
      withReplayInput input \opened -> do
        plan <-
          prepareReplayOutputs
            [opened]
            [checkpointRequest checkpoint, ReplayOutputRequest ReplayFinalGameOutput secondary]
        foreignPath <- newEmptyMVar
        acquired <- newEmptyMVar
        sender <- newEmptyMVar
        armed <- newEmptyMVar
        worker <-
          async
            $ publishReplayOutputsWithHook
              ( \case
                  ReplayOutputsStaged -> do
                    paths <- stagedPaths workspace
                    stagePath <- case paths of
                      [path, _] -> pure path
                      other -> expectationFailure ("expected two stages, got " <> show other) >> error "unreachable"
                    removeFile stagePath
                    BSL8.writeFile stagePath "foreign"
                    void $ tryPutMVar foreignPath stagePath
                    holdMaskedHandoff acquired sender armed
                  _ -> pure ()
              )
              plan
              [artifact ReplayCheckpointOutput "checkpoint", artifact ReplayFinalGameOutput "game"]
        cancelPendingAt worker acquired sender armed
        path <- readMVar foreignPath
        BSL8.readFile path `shouldReturn` "foreign"
        doesFileExist checkpoint `shouldReturn` False
        doesFileExist secondary `shouldReturn` False
        stagedPaths workspace `shouldReturn` [path]
        removeFile path
      assertNoStages workspace

  it "cleans the checkpoint stage when cancelled at the secondary publication handoff" $
    withWorkspace "post-secondary-cancellation" \workspace -> do
      let input = workspace </> "source"
          secondary = workspace </> "game"
          checkpoint = workspace </> "checkpoint"
      BSL8.writeFile input "source"
      withReplayInput input \opened -> do
        plan <-
          prepareReplayOutputs
            [opened]
            [checkpointRequest checkpoint, ReplayOutputRequest ReplayFinalGameOutput secondary]
        cancelPublisherAt
          ReplayBeforeCheckpointPublish
          (pure ())
          plan
          [artifact ReplayCheckpointOutput "checkpoint", artifact ReplayFinalGameOutput "game"]
        BSL8.readFile secondary `shouldReturn` "game"
        doesFileExist checkpoint `shouldReturn` False
      assertNoStages workspace

  it "publishes secondaries first and leaves no checkpoint after their failure" $
    withWorkspace "checkpoint-last" \workspace -> do
      let input = workspace </> "source"
          secondary = workspace </> "game"
          checkpoint = workspace </> "checkpoint"
      BSL8.writeFile input "source"
      withReplayInput input \opened -> do
        plan <-
          prepareReplayOutputs
            [opened]
            [checkpointRequest checkpoint, ReplayOutputRequest ReplayFinalGameOutput secondary]
        publishReplayOutputsWithHook
          ( \case
              ReplayBeforeCheckpointPublish -> do
                doesFileExist secondary `shouldReturn` True
                doesFileExist checkpoint `shouldReturn` False
              _ -> pure ()
          )
          plan
          [artifact ReplayCheckpointOutput "checkpoint", artifact ReplayFinalGameOutput "game"]
        BSL8.readFile checkpoint `shouldReturn` "checkpoint"
        BSL8.readFile secondary `shouldReturn` "game"
      removePathForcibly checkpoint
      removePathForcibly secondary
      withReplayInput input \opened -> do
        plan <-
          prepareReplayOutputs
            [opened]
            [checkpointRequest checkpoint, ReplayOutputRequest ReplayFinalGameOutput secondary]
        createDirectory secondary
        expectIOExceptionContaining "special file" $
          publishReplayOutputs
            plan
            [artifact ReplayCheckpointOutput "checkpoint", artifact ReplayFinalGameOutput "game"]
        doesFileExist checkpoint `shouldReturn` False
      assertNoStages workspace

  it "atomically permits exactly one concurrent checkpoint publisher without clobbering" $
    withWorkspace "no-clobber" \workspace -> do
      let input = workspace </> "source"
          checkpoint = workspace </> "checkpoint"
      BSL8.writeFile input "source"
      withReplayInput input \leftInput -> withReplayInput input \rightInput -> do
        leftPlan <- prepareReplayOutputs [leftInput] [checkpointRequest checkpoint]
        rightPlan <- prepareReplayOutputs [rightInput] [checkpointRequest checkpoint]
        waiting <- newEmptyMVar
        release <- newEmptyMVar
        let hook = \case
              ReplayBeforeCheckpointPublish -> putMVar waiting () >> readMVar release
              _ -> pure ()
            publish plan bytes =
              publishReplayOutputsWithHook hook plan [artifact ReplayCheckpointOutput bytes]
        left <- async $ publish leftPlan "left"
        right <- async $ publish rightPlan "right"
        takeMVar waiting
        takeMVar waiting
        putMVar release ()
        results <- traverse waitCatch [left, right]
        length (filter isRight results) `shouldBe` 1
        BSL8.readFile checkpoint >>= (`shouldSatisfy` (`elem` ["left", "right"]))
      assertNoStages workspace

  it "cancels interruptibly after linking and removes only its unpublished inode" $
    withWorkspace "cancellation" \workspace -> do
      let input = workspace </> "source"
          checkpoint = workspace </> "checkpoint"
      BSL8.writeFile input "source"
      withReplayInput input \opened -> do
        plan <- prepareReplayOutputs [opened] [checkpointRequest checkpoint]
        cancelPublisherAt
          ReplayCheckpointLinked
          (pure ())
          plan
          [artifact ReplayCheckpointOutput "checkpoint"]
        doesFileExist checkpoint `shouldReturn` False
      assertNoStages workspace

checkpointRequest :: FilePath -> ReplayOutputRequest
checkpointRequest = ReplayOutputRequest ReplayCheckpointOutput

artifact :: ReplayOutputRole -> BSL8.ByteString -> ReplayOutputArtifact
artifact = ReplayOutputArtifact

cancelPublisherAt
  :: ReplayPublishPhase
  -> IO ()
  -> ReplayOutputPlan
  -> [ReplayOutputArtifact]
  -> Expectation
cancelPublisherAt phase beforePhase plan artifacts = do
  reached <- newEmptyMVar
  blocked <- newEmptyMVar
  worker <-
    async
      $ publishReplayOutputsWithHook
        (\actual -> when (actual == phase) $ beforePhase >> putMVar reached () >> takeMVar blocked)
        plan
        artifacts
  timeout 2000000 (takeMVar reached) `shouldReturn` Just ()
  timeout 2000000 (cancel worker) `shouldReturn` Just ()
  waitCatch worker >>= (`shouldSatisfy` isLeft)

awaitValue :: MVar a -> IO a
awaitValue value = tryReadMVar value >>= maybe (awaitValue value) pure

holdMaskedHandoff :: MVar () -> MVar ThreadId -> MVar () -> IO ()
holdMaskedHandoff reached sender armed = do
  E.getMaskingState `shouldReturn` E.MaskedInterruptible
  void $ tryPutMVar reached ()
  senderThread <- awaitValue sender
  awaitBlockedOnException senderThread
  replicateM_ 64 $ yield >> requireBlockedOnException senderThread
  void $ tryPutMVar armed ()

awaitBlockedOnException :: ThreadId -> IO ()
awaitBlockedOnException sender =
  threadStatus sender >>= \case
    ThreadBlocked BlockedOnException -> pure ()
    ThreadRunning -> awaitBlockedOnException sender
    status -> expectationFailure $ "canceller was not pending at masked handoff: " <> show status

requireBlockedOnException :: ThreadId -> IO ()
requireBlockedOnException sender =
  threadStatus sender >>= \case
    ThreadBlocked BlockedOnException -> pure ()
    status -> expectationFailure $ "pending cancellation changed before restore: " <> show status

cancelPendingAt
  :: Async ()
  -> MVar ()
  -> MVar ThreadId
  -> MVar ()
  -> Expectation
cancelPendingAt worker reached sender armed = do
  timeout 2000000 (takeMVar reached) `shouldReturn` Just ()
  canceller <- async $ do
    tid <- myThreadId
    void $ tryPutMVar sender tid
    E.throwTo (asyncThreadId worker) E.ThreadKilled
  timeout 2000000 (takeMVar armed) `shouldReturn` Just ()
  timeout 2000000 (waitCatch canceller) >>= \case
    Just (Right ()) -> pure ()
    other -> expectationFailure $ "canceller did not finish: " <> show other
  waitCatch worker >>= \case
    Left err | Just E.ThreadKilled <- E.fromException err -> pure ()
    other -> expectationFailure $ "worker did not receive pending cancellation: " <> show other

stagedPaths :: FilePath -> IO [FilePath]
stagedPaths workspace =
  map (workspace </>)
    . List.sort
    . filter (List.isInfixOf ".arkham-replay-stage")
    <$> listDirectory workspace

withWorkspace :: String -> (FilePath -> IO a) -> IO a
withWorkspace name action = do
  cwd <- getCurrentDirectory
  let parent = cwd </> ".replay-output-spec"
      workspace = parent </> name
  removePathForcibly workspace `E.catch` \(_ :: E.IOException) -> pure ()
  createDirectoryIfMissing True workspace
  action workspace `E.finally` do
    removePathForcibly workspace `E.catch` \(_ :: E.IOException) -> pure ()
    remaining <- listDirectory parent
    when (null remaining) $
      removePathForcibly parent `E.catch` \(_ :: E.IOException) -> pure ()

expectIOExceptionContaining :: String -> IO a -> Expectation
expectIOExceptionContaining needle action =
  action `shouldThrow` \(err :: E.IOException) -> needle `List.isInfixOf` show err

assertNoStages :: FilePath -> Expectation
assertNoStages workspace =
  listDirectory workspace
    >>= (`shouldSatisfy` all (not . List.isInfixOf ".arkham-replay-stage"))
