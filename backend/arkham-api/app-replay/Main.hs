{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fforce-recomp #-}

-- | Headless replay CLI.
--
-- Loads a game export (from /api/v1/arkham/games/:id/export), optionally pushes
-- a list of raw 'Message's, runs the engine, and prints the resulting 'Game'.
-- No DB, no Yesod, no frontend. The point is to reproduce a bug in <1s instead
-- of the full investigate-bug stack.
module Main where

import Api.Arkham.Export
  ( ArkhamExport (..)
  , ArkhamGameExportData (..)
  )
import Api.Arkham.Helpers (GameApp (..), runGameApp)
import Arkham.Classes.GameLogger (ClientMessage (..))
import Arkham.Classes.HasQueue (newQueue, pushAll)
import Arkham.Game (Game (..), PublicGame (..), runMessages)
import Arkham.Game.Diff (diff, patchValueWithRecovery)
import Arkham.Game.Runner (handleActionDiff)
import Arkham.Message (Message (ClearUI, SetActivePlayer))
import Arkham.Metrics (dumpMetricsTo, enableMetrics, formatMetrics, withMetric)
import Arkham.Queue (queueToRef)
import Arkham.Replay.BuildIdentity (embedReplayBuildIdentity)
import Arkham.Replay.Checkpoint
import Arkham.Replay.Output
import Control.Exception (evaluate)
import Control.Monad (forM_, void, when)
import Control.Monad.Random (mkStdGen)
import Data.Aeson (Result (..), Value, eitherDecodeFileStrict', eitherDecode, encode, fromJSON, object, toJSON, (.=))
import Data.ByteString.Lazy qualified as BSL
import Data.ByteString.Lazy.Char8 qualified as BL8
import Data.Foldable (for_)
import Data.IORef (newIORef, readIORef)
import Data.List (sortOn)
import Data.Maybe (fromMaybe, isJust, isNothing, maybeToList)
import Data.Ord (Down (..))
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import Data.Word (Word64)
import Entity.Answer (Reply (..), answerPlayer, handleAnswerPure)
import Entity.Arkham.Step (ArkhamStep (..), Choice (..))
import GHC.Clock (getMonotonicTimeNSec)
import System.Environment (getArgs)
import System.Exit (die, exitSuccess)
import System.IO (hPutStrLn, stderr)
import Prelude

data Opts = Opts
  { optExport :: FilePath
  , optAnswers :: Maybe FilePath
  , optOutput :: Maybe FilePath
  , optTrace :: Bool
  , optUndo :: Int
  , optMetrics :: Maybe (Maybe FilePath)
  -- ^ Outer Maybe = enabled?  Inner Maybe = where to dump (Nothing = stderr).
  , optMetricsTopN :: Int
  , optReplayAll :: Bool
  , optPerStepReport :: Maybe FilePath
  , optPerStepTopN :: Int
  , optSimulateServer :: Bool
  , optBenchActionDiff :: Int
  , optReplayScript :: Maybe FilePath
  , optCheckpointOutput :: Maybe FilePath
  , optInspectCheckpoint :: Bool
  }

defaultOpts :: Opts
defaultOpts = Opts "" Nothing Nothing False 0 Nothing 50 False Nothing 30 False 0 Nothing Nothing False

replayBuildIdentity :: ReplayBuildIdentity
replayBuildIdentity = $(embedReplayBuildIdentity)

{- | One-line rendering of a client (UI) message. 'ClientMessage' has no 'Show'
instance, and the embedded card 'Value's are enormous, so keep it terse.
-}
formatClientMessage :: ClientMessage -> String
formatClientMessage = \case
  ClientText t -> "text " <> T.unpack t
  ClientError t -> "error " <> T.unpack t
  ClientCard t v -> "card " <> T.unpack t <> " " <> briefValue v
  ClientCardOnly pid t v -> "cardOnly[" <> show pid <> "] " <> T.unpack t <> " " <> briefValue v
  ClientTarot v -> "tarot " <> briefValue v
  ClientShowDiscard iid -> "showDiscard " <> show iid
  ClientShowUnder iid -> "showUnder " <> show iid
  ClientUI t -> "ui " <> T.unpack t
  ClientAudio t -> "audio " <> T.unpack t
  ClientPlayabilityReport _ t _ -> "playabilityReport " <> T.unpack t
 where
  briefValue v = let s = BL8.unpack (encode v) in if length s > 200 then take 200 s <> "..." else s

usage :: String
usage =
  unlines
    [ "Usage: arkham-replay <export.json> [--undo N] [--answers answers.json] [--output out.json]"
    , "                                   [--trace] [--metrics [FILE]] [--metrics-top N]"
    , "                                   [--replay-all] [--per-step-report FILE] [--per-step-top N]"
    , "                                   [--replay-script plan.json --checkpoint-output checkpoint.json]"
    , "                                   [--inspect-checkpoint]"
    , ""
    , "  <export.json>      Game export from /api/v1/arkham/games/:id/export"
    , "  --undo N           Step back N steps before resuming (applies choicePatchDown)"
    , "  --answers FILE     JSON list of Answer values to apply one at a time"
    , "                     (Answer accepts {\"tag\":\"Raw\",...}, {\"tag\":\"Answer\",...}, etc.)"
    , "  --output FILE      Write final Game state JSON here (default: stdout)"
    , "  --trace            Print every Message processed to stderr, plus every client"
    , "                     (UI) message as \"client> ...\""
    , "  --metrics [FILE]   Record per-span wall-clock timings; dump table to FILE (or stderr)"
    , "  --metrics-top N    Show top-N spans in the metrics table (default 50)"
    , "  --replay-all       Undo to the earliest retained step, then replay forward."
    , "                     With --undo N, only undo N steps and replay those N forward."
    , "                     Top slowest steps are printed to stderr; combine with --metrics for"
    , "                     per-span breakdown across the whole replay."
    , "  --per-step-report FILE  Write step,duration_ms,server_ms,messages CSV (works with --replay-all)"
    , "  --per-step-top N   Show top-N slowest steps on stderr (default 30)"
    , "  --simulate-server  After each replayed step, perform the same JSON work the real"
    , "                     handler does per answer: force the accumulated action diffs,"
    , "                     compute the step's undo diff, encode the game for the DB write,"
    , "                     re-parse it (DB row load), and encode the PublicGame broadcast."
    , "                     Timings appear as server/* spans in --metrics and server_ms in"
    , "                     the per-step report."
    , "  --replay-script FILE  Replay exact Answers against bound prompts/source/build."
    , "                     Only mode=answers is deterministic; history is rejected."
    , "  --checkpoint-output FILE  Publish a verified step-0 checkpoint envelope."
    , "                     Required with --replay-script; accepted by normal game import."
    , "  --inspect-checkpoint  Print provenance and prompts without draining the queue."
    , "  --build-identity   Print the executable's embedded replay build identity, then exit."
    ]

parseArgs :: [String] -> IO Opts
parseArgs = go defaultOpts
 where
  go o [] = pure o
  go _ ("--help" : _) = die usage
  go _ ("-h" : _) = die usage
  go o ("--answers" : f : rest) = go o {optAnswers = Just f} rest
  go o ("--output" : f : rest) = go o {optOutput = Just f} rest
  go o ("--trace" : rest) = go o {optTrace = True} rest
  go o ("--undo" : n : rest) = case reads n of
    [(k, "")] | k >= 0 -> go o {optUndo = k} rest
    [(k, "")] -> die $ "--undo expects a non-negative integer, got: " <> show k
    _ -> die $ "--undo expects an integer, got: " <> n
  go o ("--metrics" : f : rest) | take 2 f /= "--" =
    go o {optMetrics = Just (Just f)} rest
  go o ("--metrics" : rest) = go o {optMetrics = Just Nothing} rest
  go o ("--metrics-top" : n : rest) = case reads n of
    [(k, "")] -> go o {optMetricsTopN = k} rest
    _ -> die $ "--metrics-top expects an integer, got: " <> n
  go o ("--replay-all" : rest) = go o {optReplayAll = True} rest
  go o ("--simulate-server" : rest) = go o {optSimulateServer = True} rest
  go o ("--bench-action-diff" : n : rest) = case reads n of
    [(k, "")] -> go o {optBenchActionDiff = k} rest
    _ -> die $ "--bench-action-diff expects an integer, got: " <> n
  go o ("--per-step-report" : f : rest) = go o {optPerStepReport = Just f} rest
  go o ("--per-step-top" : n : rest) = case reads n of
    [(k, "")] -> go o {optPerStepTopN = k} rest
    _ -> die $ "--per-step-top expects an integer, got: " <> n
  go o ("--replay-script" : f : rest) = go o {optReplayScript = Just f} rest
  go o ("--checkpoint-output" : f : rest) = go o {optCheckpointOutput = Just f} rest
  go o ("--inspect-checkpoint" : rest) = go o {optInspectCheckpoint = True} rest
  go o (x : rest)
    | optExport o == "" = go o {optExport = x} rest
    | otherwise = die $ "Unexpected argument: " <> x <> "\n" <> usage

main :: IO ()
main = do
  args <- getArgs
  when (args == ["--build-identity"]) $ BL8.putStrLn (encode replayBuildIdentity) >> exitSuccess
  opts <- parseArgs args
  when (optExport opts == "") $ die usage
  withReplayInput (optExport opts) \exportInput ->
    case optReplayScript opts of
      Nothing -> runReplay opts exportInput Nothing
      Just path -> withReplayInput path $ runReplay opts exportInput . Just

runReplay :: Opts -> ReplayInput -> Maybe ReplayInput -> IO ()
runReplay opts exportInput scriptInput = do
  let exportBytes = replayInputBytes exportInput
  ( sourceExport@ArkhamExport {aeCampaignData = ArkhamGameExportData {..}}
    , inputKind
    , inputProvenance
    ) <-
    either die pure $ decodeReplayInput replayBuildIdentity exportBytes
  let sourceExportSha = replayInputSha256 exportInput

  replayPlan <-
    case scriptInput of
      Nothing -> pure Nothing
      Just input -> do
        let bytes = replayInputBytes input
        plan <- either (die . ("Failed to parse replay script: " <>)) pure $ decodeReplayPlan bytes
        pure $ Just (plan, replayInputSha256 input)

  validateOptions opts replayPlan
  case replayPlan of
    Nothing -> pure ()
    Just (plan, _) ->
      either (die . ("Replay provenance mismatch: " <>)) pure
        $ validateReplaySource replayBuildIdentity sourceExportSha inputKind agedCurrentData plan

  replayOutputPlan <- case (replayPlan, scriptInput, optCheckpointOutput opts) of
    (Just _, Just script, Just checkpointPath) ->
      Just
        <$> prepareReplayOutputs
          [exportInput, script]
          ( ReplayOutputRequest ReplayCheckpointOutput checkpointPath
              : [ReplayOutputRequest ReplayFinalGameOutput path | path <- maybeToList $ optOutput opts]
              <> [ ReplayOutputRequest ReplayMetricsOutput path
                 | path <- maybeToList $ optMetrics opts >>= id
                 ]
          )
    (Nothing, _, _) -> pure Nothing
    _ -> die "Internal error: replay output plan is incomplete"

  answers <-
    case optAnswers opts of
      Nothing -> pure []
      Just f ->
        either (die . ("Failed to parse answers: " <>)) pure
          =<< eitherDecodeFileStrict' f

  when (optUndo opts > length agedSteps) $
    die
      $ "--undo requested "
      <> show (optUndo opts)
      <> " steps, but the export retains only "
      <> show (length agedSteps)

  when (isJust replayPlan || optInspectCheckpoint opts) $
    either (die . ("Invalid retained replay state: " <>)) pure
      $ validateRetainedSteps agedStep agedSteps

  -- Apply --undo (or full undo for --replay-all): step back N steps by
  -- replaying their choicePatchDown patches (most-recent-step first).
  -- agedSteps is ordered by desc step (see generateExport), so the first
  -- N entries are exactly the steps to undo.
  -- With --replay-all --undo N, only undo (and then replay) the last N steps;
  -- some exports cannot be unwound to step 0 (e.g. across scenario
  -- transitions), and a recent window is usually what you want to time anyway.
  -- For a full replay-all we unwind to the EARLIEST retained step, not past
  -- it. An export keeps only the most recent N steps, so unwinding all N lands
  -- at (minStep - 1) — a step that isn't retained and may predate the current
  -- scenario (the mode collapses to campaign-only `This`). Replaying forward
  -- then can't rebuild the scenario from queued messages (a campaign->scenario
  -- transition isn't message-reconstructable), so `scenarioField` throws. Stop
  -- one step short to resume from a retained, scenario-consistent state; that
  -- earliest step's queue is still executed as the resume queue below.
  let undoCount =
        if optReplayAll opts && optUndo opts == 0
          then max 0 (length agedSteps - 1)
          else optUndo opts
  let stepsToUndo = take undoCount (sortOn (Down . arkhamStepStep) agedSteps)
  let targetStep = agedStep - length stepsToUndo
  currentData <-
    case foldl applyUndo (Right (toJSON agedCurrentData)) stepsToUndo of
      Left e -> die $ "undo failed: " <> e
      Right v -> case fromJSON v of
        Error e -> die $ "undo result deserialise failed: " <> e
        Success g -> pure (g :: Game)

  -- Isolated benchmark of the in-action diff bookkeeping: run K minimal
  -- in-action messages through handleActionDiff on the loaded game, then
  -- force the accumulated gameActionDiff exactly like the per-answer save
  -- (ActionDiff $ view actionDiffL ge) does. This is the work a real save
  -- performs after K messages of an action have resolved.
  when (optBenchActionDiff opts > 0) $ do
    let k = optBenchActionDiff opts
    let g0 = currentData {gameInAction = True, gameActionDiff = []}
    t0 <- getMonotonicTimeNSec
    let stepG g i = handleActionDiff g (g {gameSeed = gameSeed g + i})
    let gk = foldl' stepG g0 [1 .. k]
    bytes <- evaluate (BSL.length (encode (gameActionDiff gk)))
    t1 <- getMonotonicTimeNSec
    hPutStrLn stderr
      $ "bench-action-diff: "
      <> show k
      <> " in-action messages; forcing the save cost took "
      <> printfMs (fromIntegral (t1 - t0) / 1_000_000)
      <> " ms ("
      <> show bytes
      <> " bytes of actionDiff JSON)"
    exitSuccess

  -- The queue waiting at the resume step.
  resumeQueue <-
    case retainedQueueAt targetStep agedSteps of
      Right queue -> pure queue
      Left err
        | isJust replayPlan || optInspectCheckpoint opts -> die err
        | targetStep == 0 && null agedSteps -> pure []
        | otherwise -> pure []

  metricsRef <- case optMetrics opts of
    Nothing -> pure Nothing
    Just _ -> Just <$> enableMetrics

  let tracerCallback
        | optTrace opts = Just (\m -> hPutStrLn stderr ("> " <> show m))
        | otherwise = Nothing

  gameRef <- newIORef currentData
  queueRef <- newQueue resumeQueue
  genRef <- newIORef (mkStdGen currentData.gameSeed)
  -- Client messages (card popups, log lines, UI pokes) never touch the Game
  -- state, so a bug that only drops one is invisible in --output. Surface them
  -- on stderr under --trace so they can be asserted on headlessly.
  let clientLogger m
        | optTrace opts = hPutStrLn stderr ("client> " <> formatClientMessage m)
        | otherwise = pure ()

  let app = GameApp gameRef queueRef genRef clientLogger Nothing

  for_ replayPlan \(plan, _) ->
    case checkQuestionCheckpoint currentData plan.replayPlanStopAt of
      CheckpointReached
        | null plan.replayPlanAnswers -> pure ()
        | otherwise ->
            die
              $ "Stop checkpoint reached with "
              <> show (length plan.replayPlanAnswers)
              <> " scripted answers still unused"
      CheckpointMismatch err -> die $ "Stop checkpoint mismatch: " <> err
      CheckpointNotReached
        | null plan.replayPlanAnswers ->
            either (die . ("Stop checkpoint not reached: " <>)) pure
              $ requireQuestionCheckpoint currentData plan.replayPlanStopAt
        | otherwise -> pure ()

  wallStart <- getMonotonicTimeNSec
  let drainResumeQueue =
        not (optInspectCheckpoint opts)
          && not (isJust replayPlan)
  when drainResumeQueue $ runGameApp app (runMessages "headless" tracerCallback)

  when (optInspectCheckpoint opts) $ do
    inspectedGame <- readIORef gameRef
    checkpoints <- either die pure $ questionCheckpoints inspectedGame
    when (null checkpoints) $ die "No open question checkpoint in the selected state"
    revalidateReplayInputs [exportInput]
    BL8.putStrLn
      $ encode
      $ object
        [ "schemaVersion" .= (1 :: Int)
        , "source"
            .= ReplaySource
              { replaySourceExportSha256 = sourceExportSha
              , replaySourceInputKind = inputKind
              , replaySourceGameGitRevision = gameGitRevision agedCurrentData
              , replaySourceReplayBuild = replayBuildIdentity
              , replaySourceSchemaRevision = replayContractSchemaRevision
              }
        , "inputProvenance" .= inputProvenance
        , "questions" .= checkpoints
        ]
    exitSuccess

  perStepTimings <-
    if optReplayAll opts
      then do
        -- agedSteps in the export are sorted desc by step. Replay forward
        -- by ascending step index, pushing each step's choiceMessages and
        -- timing the resulting drain.
        let forwardSteps =
              sortOn arkhamStepStep
                $ filter ((> targetStep) . arkhamStepStep) agedSteps
        let total = length forwardSteps
        hPutStrLn stderr $ "Replaying " <> show total <> " steps forward..."
        let
          go _ timings [] = pure $ reverse timings
          go idx timings (step : rest) = do
            let msgs = choiceMessages (arkhamStepChoice step)
            when (idx `mod` 100 == 0)
              $ hPutStrLn stderr ("  step " <> show idx <> "/" <> show total)
            gBefore <- readIORef gameRef
            runGameApp app (pushAll (ClearUI : msgs))
            t0 <- getMonotonicTimeNSec
            runGameApp app (runMessages "headless" tracerCallback)
            t1 <- getMonotonicTimeNSec
            ge <- readIORef gameRef
            serverNs <-
              if optSimulateServer opts
                then simulateServerWork gBefore ge
                else pure 0
            let timing = (arkhamStepStep step, t1 - t0, serverNs, length msgs)
            go (idx + 1) (timing : timings) rest
        go (1 :: Int) [] forwardSteps
      else do
        case replayPlan of
          Just (ReplayPlan {replayPlanMode = ReplayAnswers, replayPlanAnswers, replayPlanStopAt}, _) -> do
            let
              go _ [] = do
                game <- readIORef gameRef
                either (die . ("Stop checkpoint not reached: " <>)) pure
                  $ requireQuestionCheckpoint game replayPlanStopAt
              go idx (scriptStep : rest) = do
                g <- readIORef gameRef
                case checkQuestionCheckpoint g replayPlanStopAt of
                  CheckpointReached ->
                    die
                      $ "Stop checkpoint reached with "
                      <> show (length (scriptStep : rest))
                      <> " scripted answers still unused"
                  CheckpointMismatch err -> die $ "Stop checkpoint mismatch: " <> err
                  CheckpointNotReached -> pure ()
                answerPid <-
                  either (die . (("script answer " <> show idx <> ": ") <>)) pure
                    $ validateReplayAnswer g scriptStep
                handleAnswerPure g answerPid scriptStep.replayAnswerValue >>= \case
                  Unhandled reason ->
                    die
                      $ "script answer "
                      <> show idx
                      <> " unhandled: "
                      <> T.unpack reason
                  Handled msgs -> do
                    let activePid = gameActivePlayerId g
                        bracketed =
                          [SetActivePlayer answerPid | activePid /= answerPid]
                            <> msgs
                            <> [SetActivePlayer activePid | activePid /= answerPid]
                    runGameApp app (pushAll (ClearUI : bracketed))
                    runGameApp app (runMessages "headless" tracerCallback)
                    ge <- readIORef gameRef
                    when (optSimulateServer opts) $ void $ simulateServerWork g ge
                    case checkQuestionCheckpoint ge replayPlanStopAt of
                      CheckpointReached
                        | null rest -> pure ()
                        | otherwise ->
                            die
                              $ "Stop checkpoint reached with "
                              <> show (length rest)
                              <> " scripted answers still unused"
                      CheckpointMismatch err -> die $ "Stop checkpoint mismatch: " <> err
                      CheckpointNotReached
                        | null rest ->
                            either (die . ("Stop checkpoint not reached: " <>)) pure
                              $ requireQuestionCheckpoint ge replayPlanStopAt
                        | otherwise -> go (idx + 1) rest
            go (0 :: Int) replayPlanAnswers
          _ ->
            forM_ (zip [(0 :: Int) ..] answers) $ \(idx, ans) -> do
              g <- readIORef gameRef
              let activePid = gameActivePlayerId g
                  answerPid = fromMaybe activePid (answerPlayer ans)
              handleAnswerPure g answerPid ans >>= \case
                Unhandled reason ->
                  hPutStrLn stderr
                    $ "answer "
                    <> show idx
                    <> " unhandled: "
                    <> T.unpack reason
                Handled msgs -> do
                  let bracketed =
                        [SetActivePlayer answerPid | activePid /= answerPid]
                          <> msgs
                          <> [SetActivePlayer activePid | activePid /= answerPid]
                  runGameApp app (pushAll (ClearUI : bracketed))
                  runGameApp app (runMessages "headless" tracerCallback)
        pure []

  wallEnd <- getMonotonicTimeNSec
  finalGame <- readIORef gameRef
  finalQueue <- readIORef $ queueToRef queueRef

  metricsOutputBytes <- case (optMetrics opts, metricsRef) of
    (Just destination, Just ref) -> do
      let elapsedMs = fromIntegral (wallEnd - wallStart) / (1_000_000 :: Double)
      hPutStrLn stderr
        $ "Replay wall-clock (excluding load + final encode): "
        <> show elapsedMs
        <> " ms"
      case destination of
        Nothing -> dumpMetricsTo Nothing ref (optMetricsTopN opts) >> pure Nothing
        Just _ ->
          Just . BSL.fromStrict . TE.encodeUtf8
            <$> formatMetrics ref (optMetricsTopN opts)
    _ -> pure Nothing

  perStepOutputBytes <-
    if null perStepTimings
      then pure Nothing
      else do
        printPerStepSummary opts perStepTimings
        pure
          $ Just
          $ BSL.fromStrict
          . TE.encodeUtf8
          . T.pack
          . formatPerStepCsv
          $ perStepTimings

  case replayPlan of
    Nothing -> do
      case optOutput opts of
        Nothing -> BL8.putStrLn (encode finalGame)
        Just path -> BSL.writeFile path (encode finalGame)
      for_ ((,) <$> (optMetrics opts >>= id) <*> metricsOutputBytes) $
        uncurry BSL.writeFile
      for_ ((,) <$> optPerStepReport opts <*> perStepOutputBytes) $
        uncurry BSL.writeFile
    Just (plan, planSha) -> do
      either (die . ("Final checkpoint mismatch: " <>)) pure
        $ requireQuestionCheckpoint finalGame plan.replayPlanStopAt
      let checkpoint = makeCheckpointExport sourceExport finalGame finalQueue
          provenance =
            ReplayProvenance
              { provenanceSchemaVersion = 1
              , provenanceContractSchemaRevision = replayContractSchemaRevision
              , provenancePlanSha256 = planSha
              , provenanceSourceExportSha256 = sourceExportSha
              , provenanceSourceInputKind = inputKind
              , provenanceSourceGameGitRevision = gameGitRevision agedCurrentData
              , provenanceReplayBuild = replayBuildIdentity
              , provenanceMode = ReplayAnswers
              , provenanceUndoSteps = undoCount
              , provenanceAnswersApplied = length plan.replayPlanAnswers
              , provenanceCheckpoint = plan.replayPlanStopAt
              , provenanceCheckpointGameSha256 = sha256Lazy $ encode finalGame
              , provenanceCheckpointQueueSha256 = sha256Lazy $ encode finalQueue
              }
          checkpointBytes = encode $ checkpointExportValue checkpoint provenance
          artifacts =
            ReplayOutputArtifact ReplayCheckpointOutput checkpointBytes
              : [ReplayOutputArtifact ReplayFinalGameOutput $ encode finalGame | isJust $ optOutput opts]
              <> [ ReplayOutputArtifact ReplayMetricsOutput bytes
                 | bytes <- maybeToList metricsOutputBytes
                 ]
      outputPath <- maybe (die "Internal error: replay script has no checkpoint output") pure
        $ optCheckpointOutput opts
      prepared <- maybe (die "Internal error: replay output plan was not prepared") pure replayOutputPlan
      publishReplayOutputs prepared artifacts
      hPutStrLn stderr
        $ "Published checkpoint "
        <> T.unpack plan.replayPlanStopAt.checkpointName
        <> " to "
        <> outputPath
        <> " (sha256 "
        <> T.unpack (sha256Lazy checkpointBytes)
        <> ")"

printPerStepSummary :: Opts -> [(Int, Word64, Word64, Int)] -> IO ()
printPerStepSummary opts perStepTimings = do
    let toMs ns = fromIntegral ns / (1_000_000 :: Double)
        totalNs (_, drainNs, serverNs, _) = drainNs + serverNs
        sortedDesc = sortOn (Down . totalNs) perStepTimings
        slowest = take (optPerStepTopN opts) sortedDesc
        sumDrain = sum [d | (_, d, _, _) <- perStepTimings]
        sumServer = sum [s | (_, _, s, _) <- perStepTimings]
    hPutStrLn stderr ""
    hPutStrLn stderr
      $ "Aggregate: drain "
      <> printfMs (toMs sumDrain)
      <> " ms, server-sim "
      <> printfMs (toMs sumServer)
      <> " ms over "
      <> show (length perStepTimings)
      <> " steps"
    hPutStrLn stderr "Top slowest steps (descending by drain+server time):"
    hPutStrLn stderr "  step      duration_ms     server_ms   messages_pushed"
    forM_ slowest $ \(step, ns, serverNs, msgs) ->
      hPutStrLn stderr
        $ "  "
        <> padLeft 8 (show step)
        <> "  "
        <> padLeft 11 (printfMs (toMs ns))
        <> "  "
        <> padLeft 12 (printfMs (toMs serverNs))
        <> "  "
        <> padLeft 5 (show msgs)

formatPerStepCsv :: [(Int, Word64, Word64, Int)] -> String
formatPerStepCsv perStepTimings =
  "step,duration_ms,server_ms,messages_pushed\n"
    <> concatMap
      ( \(step, durationNs, serverNs, messages) ->
          show step
            <> ","
            <> printfMs (toMs durationNs)
            <> ","
            <> printfMs (toMs serverNs)
            <> ","
            <> show messages
            <> "\n"
      )
      perStepTimings
 where
  toMs ns = fromIntegral ns / (1_000_000 :: Double)

-- | Apply one step's choicePatchDown to the running JSON value. Stops on the
-- first failure.
applyUndo :: Either String Value -> ArkhamStep -> Either String Value
applyUndo (Left e) _ = Left e
applyUndo (Right v) step =
  case patchValueWithRecovery v (choicePatchDown (arkhamStepChoice step)) of
    Error e -> Left $ "step " <> show (arkhamStepStep step) <> ": " <> e
    Success v' -> Right v'

validateOptions :: Opts -> Maybe (ReplayPlan, T.Text) -> IO ()
validateOptions opts replayPlan = do
  when (optInspectCheckpoint opts) $ do
    when
      ( isJust replayPlan
          || isJust (optAnswers opts)
          || isJust (optOutput opts)
          || isJust (optCheckpointOutput opts)
          || isJust (optMetrics opts)
          || isJust (optPerStepReport opts)
          || optReplayAll opts
          || optSimulateServer opts
          || optBenchActionDiff opts > 0
      )
      $ die "--inspect-checkpoint only accepts the export, --undo, and --trace"

  case replayPlan of
    Nothing ->
      when (isJust $ optCheckpointOutput opts) $
        die "--checkpoint-output requires --replay-script"
    Just _ -> do
      when (optInspectCheckpoint opts) $
        die "--inspect-checkpoint cannot be combined with --replay-script"
      when (isJust $ optAnswers opts) $
        die "--answers cannot be combined with --replay-script; put exact answers in the replay script"
      when (optBenchActionDiff opts > 0) $
        die "--bench-action-diff cannot be combined with --replay-script"
      when (isNothing $ optCheckpointOutput opts) $
        die "--replay-script requires --checkpoint-output"
      when (optReplayAll opts) $
        die "deterministic replay scripts cannot use --replay-all; retained steps contain residual queues, not Answers"
      when (isJust $ optPerStepReport opts) $
        die "--per-step-report requires legacy --replay-all and cannot be combined with --replay-script"

simulateServerWork :: Game -> Game -> IO Word64
simulateServerWork gBefore ge = do
  s0 <- getMonotonicTimeNSec
  -- Mirror Api.Handler.Arkham.Games.Shared.updateGame, in order.
  _ <- withMetric "server/forceActionDiff" $ evaluate (BSL.length (encode (gameActionDiff ge)))
  _ <- withMetric "server/diffDown" $ evaluate (BSL.length (encode (diff ge gBefore)))
  gameBytes <- withMetric "server/encodeGame" $ do
    let bytes = encode ge
    _ <- evaluate (BSL.length bytes)
    pure bytes
  _ <- withMetric "server/parseGame" $ evaluate $ case eitherDecode @Game gameBytes of
    Left err -> error ("simulate-server: game failed to re-parse: " <> err)
    Right (game :: Game) -> gameSeed game
  _ <-
    withMetric "server/encodePublicGame"
      $ evaluate (BSL.length (encode (PublicGame ("headless" :: T.Text) "bench" [] ge)))
  s1 <- getMonotonicTimeNSec
  pure $ s1 - s0

padLeft :: Int -> String -> String
padLeft n s = replicate (max 0 (n - length s)) ' ' <> s

printfMs :: Double -> String
printfMs ms =
  let scaled = (round (ms * 100) :: Integer)
      whole = scaled `div` 100
      frac = scaled `mod` 100
   in show whole <> "." <> padFrac frac
 where
  padFrac f = (if f < 10 then "0" else "") <> show f
