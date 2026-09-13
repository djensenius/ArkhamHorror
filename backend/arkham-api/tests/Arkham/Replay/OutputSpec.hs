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
  createDirectoryLink,
  createDirectoryIfMissing,
  createFileLink,
  doesFileExist,
  getCurrentDirectory,
  listDirectory,
  removeFile,
  removePathForcibly,
  renameDirectory,
  renameFile,
 )
import System.Posix.Files (
  createLink,
  createNamedPipe,
  fileMode,
  getFileStatus,
  groupWriteMode,
  intersectFileModes,
  nullFileMode,
  ownerExecuteMode,
  ownerReadMode,
  ownerWriteMode,
  setFileMode,
  unionFileModes,
 )
import TestImport.New

spec :: Spec
-- These examples share a cleanup root and deliberately coordinate
-- asynchronous cancellation. The project's default parallel hook can let one
-- example remove that root during another example's acquisition handoff.
-- Keep examples sequential while preserving each example's internal races.
spec = sequential $ describe "deterministic replay file handling" do
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

  it "rejects retargeting the caller-provided symlink input parent" $
    withWorkspace "input-parent-retarget" \workspace -> do
      let firstParent = workspace </> "first"
          secondParent = workspace </> "second"
          inputParent = workspace </> "input"
          input = inputParent </> "source"
      createDirectory firstParent
      createDirectory secondParent
      BSL8.writeFile (firstParent </> "source") "first"
      BSL8.writeFile (secondParent </> "source") "second"
      createDirectoryLink firstParent inputParent
      withReplayInput input \opened -> do
        replayInputBytes opened `shouldBe` "first"
        removeFile inputParent
        createDirectoryLink secondParent inputParent
        replayInputBytes opened `shouldBe` "first"
        expectIOExceptionContaining "parent changed" $ revalidateReplayInputs [opened]

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

  it "rejects output parents writable by another user or group" $
    withWorkspace "shared-output-parent" \workspace -> do
      let input = workspace </> "source"
          outputParent = workspace </> "shared"
          checkpoint = outputParent </> "checkpoint"
      BSL8.writeFile input "source"
      createDirectory outputParent
      setFileMode outputParent $
        ownerReadMode
          `unionFileModes` ownerWriteMode
          `unionFileModes` ownerExecuteMode
          `unionFileModes` groupWriteMode
      withReplayInput input \opened ->
        expectIOExceptionContaining "group/world write mode bits" $
          prepareReplayOutputs [opened] [checkpointRequest checkpoint]

  it "stages through a retained no-follow parent descriptor after a symlink retarget" $
    withWorkspace "parent-retarget" \workspace -> do
      let input = workspace </> "source"
          outputParent = workspace </> "output"
          retainedParent = workspace </> "retained-output"
          escapeParent = workspace </> "escape"
          checkpoint = outputParent </> "checkpoint"
      BSL8.writeFile input "source"
      createDirectory outputParent
      createDirectory escapeParent
      withReplayInput input \opened -> do
        plan <- prepareReplayOutputs [opened] [checkpointRequest checkpoint]
        expectIOExceptionContaining "parent changed" $
          publishReplayOutputsWithHook
            ( \case
                ReplayBeforeStageCreate ReplayCheckpointOutput -> do
                  renameDirectory outputParent retainedParent
                  createDirectoryLink escapeParent outputParent
                _ -> pure ()
            )
            plan
            [artifact ReplayCheckpointOutput "checkpoint"]
        listDirectory escapeParent `shouldReturn` []
        doesFileExist (retainedParent </> "checkpoint") `shouldReturn` False
        assertNoInternalArtifacts retainedParent

  it "rejects retargeting the caller-provided symlink parent" $
    withWorkspace "spelled-parent-retarget" \workspace -> do
      let input = workspace </> "source"
          firstParent = workspace </> "first"
          secondParent = workspace </> "second"
          outputParent = workspace </> "output"
          checkpoint = outputParent </> "checkpoint"
      BSL8.writeFile input "source"
      createDirectory firstParent
      createDirectory secondParent
      createDirectoryLink firstParent outputParent
      withReplayInput input \opened -> do
        plan <- prepareReplayOutputs [opened] [checkpointRequest checkpoint]
        expectIOExceptionContaining "parent changed" $
          publishReplayOutputsWithHook
            ( \case
                ReplayBeforeStageCreate ReplayCheckpointOutput -> do
                  removeFile outputParent
                  createDirectoryLink secondParent outputParent
                _ -> pure ()
            )
            plan
            [artifact ReplayCheckpointOutput "checkpoint"]
        listDirectory firstParent `shouldReturn` []
        listDirectory secondParent `shouldReturn` []
        assertNoInternalArtifacts firstParent

  it "keeps completed stages anonymous and preserves stage-shaped files during cleanup" $
    withWorkspace "anonymous-cleanup" \workspace -> do
      let input = workspace </> "source"
          checkpoint = workspace </> "checkpoint"
          decoy = stageDecoy workspace "checkpoint"
      BSL8.writeFile input "source"
      withReplayInput input \opened -> do
        plan <- prepareReplayOutputs [opened] [checkpointRequest checkpoint]
        expectIOExceptionContaining "force cleanup" $
          publishReplayOutputsWithHook
            ( \case
                ReplayOutputsStaged -> do
                  assertNoInternalArtifacts workspace
                  BSL8.writeFile decoy "foreign"
                  ioError $ userError "force cleanup"
                _ -> pure ()
            )
            plan
            [artifact ReplayCheckpointOutput "checkpoint"]
        BSL8.readFile decoy `shouldReturn` "foreign"
        doesFileExist checkpoint `shouldReturn` False
        removeFile decoy
      assertNoInternalArtifacts workspace

  it "transfers each anonymous stage to masked ownership before cancellation" $
    withWorkspace "stage-acquisition-cancellation" \workspace -> do
      let input = workspace </> "source"
          checkpoint = workspace </> "checkpoint"
          secondary = workspace </> "game"
          decoy = stageDecoy workspace "checkpoint"
      BSL8.writeFile input "source"
      withReplayInput input \opened -> do
        plan <-
          prepareReplayOutputs
            [opened]
            [checkpointRequest checkpoint, ReplayOutputRequest ReplayFinalGameOutput secondary]
        checkpointCompleted <- newEmptyMVar
        completed <- newEmptyMVar
        sender <- newEmptyMVar
        armed <- newEmptyMVar
        worker <-
          async
            $ publishReplayOutputsWithHook
              ( \case
                  ReplayStageCompleted ReplayCheckpointOutput -> do
                    assertNoInternalArtifacts workspace
                    void $ tryPutMVar checkpointCompleted ()
                  ReplayStageCompleted ReplayFinalGameOutput -> do
                    readMVar checkpointCompleted
                    assertNoInternalArtifacts workspace
                    BSL8.writeFile decoy "foreign"
                    holdMaskedHandoff completed sender armed
                  _ -> pure ()
              )
              plan
              [artifact ReplayCheckpointOutput "checkpoint", artifact ReplayFinalGameOutput "game"]
        cancelPendingAt worker completed sender armed
        BSL8.readFile decoy `shouldReturn` "foreign"
        doesFileExist checkpoint `shouldReturn` False
        doesFileExist secondary `shouldReturn` False
        removeFile decoy
      assertNoInternalArtifacts workspace

  it "removes a renamed stage when cancellation interrupts the descriptor handoff" $
    withWorkspace "capture-handoff-cancellation" \workspace -> do
      let input = workspace </> "source"
          checkpoint = workspace </> "checkpoint"
      BSL8.writeFile input "source"
      withReplayInput input \opened -> do
        plan <- prepareReplayOutputs [opened] [checkpointRequest checkpoint]
        cancelPublisherAt
          (ReplayEntryRenamedForRemoval ReplayCheckpointOutput)
          (pure ())
          plan
          [artifact ReplayCheckpointOutput "checkpoint"]
        doesFileExist checkpoint `shouldReturn` False
      assertNoInternalArtifacts workspace

  it "removes a renamed stage when opening its capture descriptor fails" $
    withWorkspace "capture-handoff-open-failure" \workspace -> do
      let input = workspace </> "source"
          checkpoint = workspace </> "checkpoint"
      BSL8.writeFile input "source"
      withReplayInput input \opened -> do
        plan <- prepareReplayOutputs [opened] [checkpointRequest checkpoint]
        expectIOExceptionContaining "open captured output" $
          publishReplayOutputsWithHook
            ( \case
                ReplayEntryRenamedForRemoval ReplayCheckpointOutput -> do
                  captures <-
                    filter (List.isPrefixOf ".arkham-replay-capture-")
                      <$> listDirectory workspace
                  case captures of
                    [capture] -> setFileMode (workspace </> capture) nullFileMode
                    other ->
                      expectationFailure
                        ("expected one renamed capture, got " <> show other)
                _ -> pure ()
            )
            plan
            [artifact ReplayCheckpointOutput "checkpoint"]
        doesFileExist checkpoint `shouldReturn` False
      assertNoInternalArtifacts workspace

  it "arms cleanup before delivering cancellation after all anonymous stages are acquired" $
    withWorkspace "acquisition-cancellation" \workspace -> do
      let input = workspace </> "source"
          checkpoint = workspace </> "checkpoint"
          secondary = workspace </> "game"
          decoy = stageDecoy workspace "game"
      BSL8.writeFile input "source"
      withReplayInput input \opened -> do
        plan <-
          prepareReplayOutputs
            [opened]
            [checkpointRequest checkpoint, ReplayOutputRequest ReplayFinalGameOutput secondary]
        acquired <- newEmptyMVar
        sender <- newEmptyMVar
        armed <- newEmptyMVar
        worker <-
          async
            $ publishReplayOutputsWithHook
              ( \case
                  ReplayOutputsStaged -> do
                    assertNoInternalArtifacts workspace
                    BSL8.writeFile decoy "foreign"
                    holdMaskedHandoff acquired sender armed
                  _ -> pure ()
              )
              plan
              [artifact ReplayCheckpointOutput "checkpoint", artifact ReplayFinalGameOutput "game"]
        cancelPendingAt worker acquired sender armed
        BSL8.readFile decoy `shouldReturn` "foreign"
        doesFileExist checkpoint `shouldReturn` False
        doesFileExist secondary `shouldReturn` False
        removeFile decoy
      assertNoInternalArtifacts workspace

  it "publishes retained bytes despite stage-path decoys before every secondary role" $
    for_
      [(ReplayFinalGameOutput, "game"), (ReplayMetricsOutput, "metrics")]
      \(role, outputName) ->
        withWorkspace ("secondary-stage-decoy-" <> outputName) \workspace -> do
          let input = workspace </> "source"
              checkpoint = workspace </> "checkpoint"
              secondary = workspace </> outputName
              decoy = stageDecoy workspace outputName
          BSL8.writeFile input "source"
          withReplayInput input \opened -> do
            plan <-
              prepareReplayOutputs
                [opened]
                [checkpointRequest checkpoint, ReplayOutputRequest role secondary]
            publishReplayOutputsWithHook
              ( \case
                  ReplayBeforeSecondaryPublish actual | actual == role -> do
                    assertNoInternalArtifacts workspace
                    BSL8.writeFile decoy "foreign"
                  _ -> pure ()
              )
              plan
              [artifact ReplayCheckpointOutput "checkpoint", artifact role "owned"]
            BSL8.readFile decoy `shouldReturn` "foreign"
            BSL8.readFile secondary `shouldReturn` "owned"
            BSL8.readFile checkpoint `shouldReturn` "checkpoint"
            removeFile decoy
          assertNoInternalArtifacts workspace

  it "publishes retained checkpoint bytes despite a stage-path decoy" $
    withWorkspace "checkpoint-stage-decoy" \workspace -> do
      let input = workspace </> "source"
          checkpoint = workspace </> "checkpoint"
          decoy = stageDecoy workspace "checkpoint"
      BSL8.writeFile input "source"
      withReplayInput input \opened -> do
        plan <- prepareReplayOutputs [opened] [checkpointRequest checkpoint]
        publishReplayOutputsWithHook
          ( \case
              ReplayBeforeCheckpointPublish -> do
                assertNoInternalArtifacts workspace
                BSL8.writeFile decoy "foreign"
              _ -> pure ()
          )
          plan
          [artifact ReplayCheckpointOutput "checkpoint"]
        BSL8.readFile decoy `shouldReturn` "foreign"
        BSL8.readFile checkpoint `shouldReturn` "checkpoint"
        removeFile decoy
      assertNoInternalArtifacts workspace

  it "does not clobber a destination created immediately before checkpoint creation" $
    withWorkspace "checkpoint-create-race" \workspace -> do
      let input = workspace </> "source"
          checkpoint = workspace </> "checkpoint"
      BSL8.writeFile input "source"
      withReplayInput input \opened -> do
        plan <- prepareReplayOutputs [opened] [checkpointRequest checkpoint]
        expectIOException $
          publishReplayOutputsWithHook
            ( \case
                ReplayBeforeDestinationCreate ReplayCheckpointOutput ->
                  BSL8.writeFile checkpoint "foreign"
                _ -> pure ()
            )
            plan
            [artifact ReplayCheckpointOutput "owned"]
        BSL8.readFile checkpoint `shouldReturn` "foreign"
      assertNoInternalArtifacts workspace

  it "does not write, publish, or delete a destination replaced after creation" $
    withWorkspace "checkpoint-created-replacement" \workspace -> do
      let input = workspace </> "source"
          checkpoint = workspace </> "checkpoint"
      BSL8.writeFile input "source"
      withReplayInput input \opened -> do
        plan <- prepareReplayOutputs [opened] [checkpointRequest checkpoint]
        expectIOExceptionContaining "created replay output changed before publication" $
          publishReplayOutputsWithHook
            ( \case
                ReplayDestinationCreated ReplayCheckpointOutput -> do
                  status <- getFileStatus checkpoint
                  fileMode status `intersectFileModes` ownerReadMode
                    `shouldBe` nullFileMode
                  removeFile checkpoint
                  BSL8.writeFile checkpoint "foreign"
                _ -> pure ()
            )
            plan
            [artifact ReplayCheckpointOutput "owned"]
        BSL8.readFile checkpoint `shouldReturn` "foreign"
      assertNoInternalArtifacts workspace

  it "preserves both a raced secondary destination and its captured prior bytes" $
    withWorkspace "secondary-create-race" \workspace -> do
      let input = workspace </> "source"
          checkpoint = workspace </> "checkpoint"
          secondary = workspace </> "game"
      BSL8.writeFile input "source"
      BSL8.writeFile secondary "prior"
      withReplayInput input \opened -> do
        plan <-
          prepareReplayOutputs
            [opened]
            [checkpointRequest checkpoint, ReplayOutputRequest ReplayFinalGameOutput secondary]
        expectIOException $
          publishReplayOutputsWithHook
            ( \case
                ReplayBeforeDestinationCreate ReplayFinalGameOutput ->
                  BSL8.writeFile secondary "foreign"
                _ -> pure ()
            )
            plan
            [artifact ReplayCheckpointOutput "checkpoint", artifact ReplayFinalGameOutput "owned"]
        BSL8.readFile secondary `shouldReturn` "foreign"
        recovery <- replacementFor workspace "game"
        BSL8.readFile recovery `shouldReturn` "prior"
        doesFileExist checkpoint `shouldReturn` False
        removeFile recovery
      assertNoInternalArtifacts workspace

  it "preserves an unreadable existing secondary when capture open fails" $
    withWorkspace "secondary-capture-open-failure" \workspace -> do
      let input = workspace </> "source"
          checkpoint = workspace </> "checkpoint"
          secondary = workspace </> "game"
      BSL8.writeFile input "source"
      BSL8.writeFile secondary "prior"
      setFileMode secondary ownerWriteMode
      withReplayInput input \opened -> do
        plan <-
          prepareReplayOutputs
            [opened]
            [checkpointRequest checkpoint, ReplayOutputRequest ReplayFinalGameOutput secondary]
        expectIOExceptionContaining "open captured output" $
          publishReplayOutputs
            plan
            [artifact ReplayCheckpointOutput "checkpoint", artifact ReplayFinalGameOutput "owned"]
        doesFileExist secondary `shouldReturn` True
        setFileMode secondary $ ownerReadMode `unionFileModes` ownerWriteMode
        BSL8.readFile secondary `shouldReturn` "prior"
        doesFileExist checkpoint `shouldReturn` False
      assertNoInternalArtifacts workspace

  it "replaces an expected secondary from retained descriptors without capture residue" $
    withWorkspace "secondary-existing" \workspace -> do
      let input = workspace </> "source"
          checkpoint = workspace </> "checkpoint"
          secondary = workspace </> "game"
      BSL8.writeFile input "source"
      BSL8.writeFile secondary "prior"
      withReplayInput input \opened -> do
        plan <-
          prepareReplayOutputs
            [opened]
            [checkpointRequest checkpoint, ReplayOutputRequest ReplayFinalGameOutput secondary]
        publishReplayOutputs
          plan
          [artifact ReplayCheckpointOutput "checkpoint", artifact ReplayFinalGameOutput "owned"]
        BSL8.readFile secondary `shouldReturn` "owned"
        BSL8.readFile checkpoint `shouldReturn` "checkpoint"
      assertNoInternalArtifacts workspace

  it "cannot escape a retained parent retargeted before destination creation" $
    withWorkspace "destination-parent-retarget" \workspace -> do
      let input = workspace </> "source"
          outputParent = workspace </> "output"
          retainedParent = workspace </> "retained-output"
          escapeParent = workspace </> "escape"
          checkpoint = outputParent </> "checkpoint"
      BSL8.writeFile input "source"
      createDirectory outputParent
      createDirectory escapeParent
      withReplayInput input \opened -> do
        plan <- prepareReplayOutputs [opened] [checkpointRequest checkpoint]
        expectIOExceptionContaining "parent changed" $
          publishReplayOutputsWithHook
            ( \case
                ReplayBeforeDestinationCreate ReplayCheckpointOutput -> do
                  renameDirectory outputParent retainedParent
                  createDirectoryLink escapeParent outputParent
                _ -> pure ()
            )
            plan
            [artifact ReplayCheckpointOutput "checkpoint"]
        listDirectory escapeParent `shouldReturn` []
        doesFileExist (retainedParent </> "checkpoint") `shouldReturn` False
        assertNoInternalArtifacts retainedParent

  it "rolls back through the retained parent after a post-create retarget" $
    withWorkspace "created-parent-retarget" \workspace -> do
      let input = workspace </> "source"
          outputParent = workspace </> "output"
          retainedParent = workspace </> "retained-output"
          escapeParent = workspace </> "escape"
          checkpoint = outputParent </> "checkpoint"
      BSL8.writeFile input "source"
      createDirectory outputParent
      createDirectory escapeParent
      withReplayInput input \opened -> do
        plan <- prepareReplayOutputs [opened] [checkpointRequest checkpoint]
        expectIOExceptionContaining "parent changed" $
          publishReplayOutputsWithHook
            ( \case
                ReplayDestinationCreated ReplayCheckpointOutput -> do
                  renameDirectory outputParent retainedParent
                  createDirectoryLink escapeParent outputParent
                _ -> pure ()
            )
            plan
            [artifact ReplayCheckpointOutput "checkpoint"]
        listDirectory escapeParent `shouldReturn` []
        doesFileExist (retainedParent </> "checkpoint") `shouldReturn` False
        assertNoInternalArtifacts retainedParent

  it "rolls back a published secondary when cancelled before checkpoint publication" $
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
        doesFileExist secondary `shouldReturn` False
        doesFileExist checkpoint `shouldReturn` False
      assertNoInternalArtifacts workspace

  it "preserves a foreign replacement when cancellation follows readable publication" $
    withWorkspace "cancellation-replacement" \workspace -> do
      let input = workspace </> "source"
          checkpoint = workspace </> "checkpoint"
      BSL8.writeFile input "source"
      withReplayInput input \opened -> do
        plan <- prepareReplayOutputs [opened] [checkpointRequest checkpoint]
        cancelPublisherAt
          ReplayCheckpointLinked
          (removeFile checkpoint >> BSL8.writeFile checkpoint "foreign")
          plan
          [artifact ReplayCheckpointOutput "owned"]
        BSL8.readFile checkpoint `shouldReturn` "foreign"
      assertNoInternalArtifacts workspace

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
      assertNoInternalArtifacts workspace

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

  it "restores a winner's secondary when a later publisher loses the checkpoint race" $
    withWorkspace "concurrent-secondary-rollback" \workspace -> do
      let input = workspace </> "source"
          secondary = workspace </> "game"
          checkpoint = workspace </> "checkpoint"
      BSL8.writeFile input "source"
      withReplayInput input \leftInput -> withReplayInput input \rightInput -> do
        leftPlan <-
          prepareReplayOutputs
            [leftInput]
            [checkpointRequest checkpoint, ReplayOutputRequest ReplayFinalGameOutput secondary]
        leftSecondaryPublished <- newEmptyMVar
        releaseLeftCheckpoint <- newEmptyMVar
        let leftHook = \case
              ReplaySecondaryPublished ReplayFinalGameOutput ->
                putMVar leftSecondaryPublished ()
              ReplayBeforeCheckpointPublish ->
                readMVar releaseLeftCheckpoint
              _ -> pure ()
        left <-
          async $
            publishReplayOutputsWithHook
              leftHook
              leftPlan
              [artifact ReplayCheckpointOutput "left-checkpoint", artifact ReplayFinalGameOutput "left-game"]
        takeMVar leftSecondaryPublished
        rightPlan <-
          prepareReplayOutputs
            [rightInput]
            [checkpointRequest checkpoint, ReplayOutputRequest ReplayFinalGameOutput secondary]
        rightAtCheckpoint <- newEmptyMVar
        releaseRightCheckpoint <- newEmptyMVar
        let rightHook = \case
              ReplayBeforeCheckpointPublish ->
                putMVar rightAtCheckpoint () >> readMVar releaseRightCheckpoint
              _ -> pure ()
        right <-
          async $
            publishReplayOutputsWithHook
              rightHook
              rightPlan
              [ artifact ReplayCheckpointOutput "right-checkpoint"
              , artifact ReplayFinalGameOutput "right-game"
              ]
        takeMVar rightAtCheckpoint
        putMVar releaseLeftCheckpoint ()
        leftResult <- waitCatch left
        leftResult `shouldSatisfy` isRight
        putMVar releaseRightCheckpoint ()
        rightResult <- waitCatch right
        rightResult `shouldSatisfy` isLeft
        BSL8.readFile checkpoint `shouldReturn` "left-checkpoint"
        BSL8.readFile secondary `shouldReturn` "left-game"
      assertNoInternalArtifacts workspace

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

stageDecoy :: FilePath -> FilePath -> FilePath
stageDecoy workspace outputName =
  workspace </> ("." <> outputName <> ".arkham-replay-stage-decoy")

replacementFor :: FilePath -> FilePath -> IO FilePath
replacementFor workspace outputName =
  listDirectory workspace >>= \entries ->
    case
      filter
        (List.isPrefixOf $ "." <> outputName <> ".arkham-replay-replaced-")
        entries
    of
      [entry] -> pure $ workspace </> entry
      other ->
        expectationFailure
          ("expected one retained prior output for " <> outputName <> ", got " <> show other)
          >> error "unreachable"

withWorkspace :: String -> (FilePath -> IO a) -> IO a
withWorkspace name action = do
  cwd <- getCurrentDirectory
  let parent = cwd </> ".replay-output-spec"
      workspace = parent </> name
  removePathForcibly workspace `E.catch` \(_ :: E.IOException) -> pure ()
  createDirectoryIfMissing True workspace
  action workspace `E.finally` do
    removePathForcibly workspace `E.catch` \(_ :: E.IOException) -> pure ()
    remaining <-
      listDirectory parent `E.catch` \err ->
        if isDoesNotExistError err then pure [] else E.throwIO (err :: E.IOException)
    when (null remaining) $
      removePathForcibly parent `E.catch` \(_ :: E.IOException) -> pure ()

expectIOExceptionContaining :: String -> IO a -> Expectation
expectIOExceptionContaining needle action =
  action `shouldThrow` \(err :: E.IOException) -> needle `List.isInfixOf` show err

expectIOException :: IO a -> Expectation
expectIOException action =
  action `shouldThrow` \(_ :: E.IOException) -> True

assertNoStages :: FilePath -> Expectation
assertNoStages workspace =
  listDirectory workspace
    >>= (`shouldSatisfy` all (not . List.isInfixOf ".arkham-replay-stage"))

assertNoInternalArtifacts :: FilePath -> Expectation
assertNoInternalArtifacts workspace =
  listDirectory workspace
    >>= ( `shouldSatisfy`
            all
              ( \entry ->
                  all
                    (`notElemIn` entry)
                    [ ".arkham-replay-stage"
                    , ".arkham-replay-capture"
                    , ".arkham-replay-replaced"
                    ]
              )
        )
 where
  notElemIn needle haystack = not $ needle `List.isInfixOf` haystack
