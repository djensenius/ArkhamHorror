module Arkham.Replay.Checkpoint (
  ReplayBuildAttestation (..),
  ReplayBuildIdentity (..),
  validateReplayBuildIdentity,
  ReplayMode (..),
  ReplayInputKind (..),
  ReplaySource (..),
  QuestionCheckpoint (..),
  ReplayAnswerStep (..),
  ReplayPlan (..),
  ReplayProvenance (..),
  ReplayCheckpointEnvelope (..),
  CheckpointStatus (..),
  replayContractSchemaRevision,
  sha256Strict,
  sha256Lazy,
  decodeReplayPlan,
  decodeReplayInput,
  validateReplaySource,
  validateRetainedSteps,
  retainedQueueAt,
  prependReplayAnswerMessages,
  questionCheckpoints,
  checkQuestionCheckpoint,
  requireQuestionCheckpoint,
  validateReplayAnswer,
  makeCheckpointExport,
  checkpointExportValue,
) where

import Api.Arkham.Export
import Arkham.Game (Game (..))
import Arkham.Git (GitSha (..))
import Arkham.Id (PlayerId)
import Arkham.Json (aesonOptions)
import Arkham.Message (Message)
import Arkham.Prelude
import Arkham.Question (Question (..))
import Arkham.Replay.BuildIdentity
import Base.Api.Types.Capabilities qualified as Capabilities
import Control.Monad.Fail (fail)
import Crypto.Hash.SHA256 qualified as SHA256
import Data.Aeson
import Data.Aeson.Diff (Patch)
import Data.Aeson.KeyMap qualified as KeyMap
import Data.Aeson.Types (Parser)
import Data.ByteString qualified as BS
import Data.ByteString.Base16 qualified as Base16
import Data.ByteString.Lazy qualified as BSL
import Data.Map.Strict qualified as Map
import Data.Text qualified as T
import Data.UUID qualified as UUID
import Entity.Answer
import Entity.Arkham.Game qualified as GameEntity
import Entity.Arkham.Step (ActionDiff (..), ArkhamStep (..), Choice (..))

data ReplayMode = ReplayAnswers
  deriving stock (Eq, Show)

instance ToJSON ReplayMode where
  toJSON = String . \case
    ReplayAnswers -> "answers"

instance FromJSON ReplayMode where
  parseJSON = withText "ReplayMode" \case
    "answers" -> pure ReplayAnswers
    other ->
      fail
        $ "unsupported replay mode: "
        <> T.unpack other
        <> "; deterministic history replay is unavailable because retained steps contain residual queues, not Answers"

data ReplayInputKind = ReplayOrdinaryExport | ReplayCheckpoint
  deriving stock (Eq, Show)

instance ToJSON ReplayInputKind where
  toJSON = String . \case
    ReplayOrdinaryExport -> "export"
    ReplayCheckpoint -> "checkpoint"

instance FromJSON ReplayInputKind where
  parseJSON = withText "ReplayInputKind" \case
    "export" -> pure ReplayOrdinaryExport
    "checkpoint" -> pure ReplayCheckpoint
    other -> fail $ "unsupported replay inputKind: " <> T.unpack other

data ReplaySource = ReplaySource
  { replaySourceExportSha256 :: Text
  , replaySourceInputKind :: ReplayInputKind
  , replaySourceGameGitRevision :: GitSha
  , replaySourceReplayBuild :: ReplayBuildIdentity
  , replaySourceSchemaRevision :: Text
  }
  deriving stock (Eq, Generic, Show)

instance ToJSON ReplaySource where
  toJSON = genericToJSON $ aesonOptions $ Just "replaySource"

instance FromJSON ReplaySource where
  parseJSON value = do
    source@ReplaySource {..} <- genericParseJSON (aesonOptions $ Just "replaySource") value
    void $ parseSha256 "source exportSha256" replaySourceExportSha256
    void $ parseGitSha "source gameGitRevision" replaySourceGameGitRevision
    when (T.null replaySourceSchemaRevision) $ fail "source schemaRevision must not be empty"
    pure source

data QuestionCheckpoint = QuestionCheckpoint
  { checkpointName :: Text
  , checkpointQuestionVersion :: Int
  , checkpointPlayerId :: PlayerId
  , checkpointPromptTag :: Text
  , checkpointPromptSha256 :: Text
  }
  deriving stock (Eq, Show)

instance ToJSON QuestionCheckpoint where
  toJSON QuestionCheckpoint {..} =
    object
      [ "type" .= String "question"
      , "name" .= checkpointName
      , "questionVersion" .= checkpointQuestionVersion
      , "playerId" .= checkpointPlayerId
      , "promptTag" .= checkpointPromptTag
      , "promptSha256" .= checkpointPromptSha256
      ]

instance FromJSON QuestionCheckpoint where
  parseJSON = withObject "QuestionCheckpoint" \o -> do
    checkpointType :: Text <- o .: "type"
    unless (checkpointType == "question") $ fail "checkpoint type must be \"question\""
    checkpointName <- o .: "name"
    when (T.null checkpointName) $ fail "checkpoint name must not be empty"
    checkpointQuestionVersion <- o .: "questionVersion"
    when (checkpointQuestionVersion <= 0) $ fail "checkpoint questionVersion must be positive"
    checkpointPlayerId <- o .: "playerId"
    checkpointPromptTag <- o .: "promptTag"
    when (T.null checkpointPromptTag) $ fail "checkpoint promptTag must not be empty"
    checkpointPromptSha256 <- o .: "promptSha256" >>= parseSha256 "checkpoint promptSha256"
    pure QuestionCheckpoint {..}

data ReplayAnswerStep = ReplayAnswerStep
  { replayAnswerExpected :: QuestionCheckpoint
  , replayAnswerValue :: Answer
  }
  deriving stock Show

instance FromJSON ReplayAnswerStep where
  parseJSON = withObject "ReplayAnswerStep" \o ->
    ReplayAnswerStep
      <$> o .: "expect"
      <*> o .: "answer"

data ReplayPlan = ReplayPlan
  { replayPlanSchemaVersion :: Int
  , replayPlanMode :: ReplayMode
  , replayPlanSource :: ReplaySource
  , replayPlanAnswers :: [ReplayAnswerStep]
  , replayPlanStopAt :: QuestionCheckpoint
  }
  deriving stock Show

instance FromJSON ReplayPlan where
  parseJSON = withObject "ReplayPlan" \o -> do
    replayPlanSchemaVersion <- o .: "schemaVersion"
    unless (replayPlanSchemaVersion == 1) $ fail "replay plan schemaVersion must be 1"
    replayPlanMode <- o .: "mode"
    replayPlanSource <- o .: "source"
    replayPlanAnswers <- o .: "answers"
    replayPlanStopAt <- o .: "stopAt"
    pure ReplayPlan {..}

data ReplayProvenance = ReplayProvenance
  { provenanceSchemaVersion :: Int
  , provenanceContractSchemaRevision :: Text
  , provenancePlanSha256 :: Text
  , provenanceSourceExportSha256 :: Text
  , provenanceSourceInputKind :: ReplayInputKind
  , provenanceSourceGameGitRevision :: GitSha
  , provenanceReplayBuild :: ReplayBuildIdentity
  , provenanceMode :: ReplayMode
  , provenanceUndoSteps :: Int
  , provenanceAnswersApplied :: Int
  , provenanceCheckpoint :: QuestionCheckpoint
  , provenanceCheckpointGameSha256 :: Text
  , provenanceCheckpointQueueSha256 :: Text
  }
  deriving stock (Eq, Generic, Show)

instance ToJSON ReplayProvenance where
  toJSON = genericToJSON $ aesonOptions $ Just "provenance"

instance FromJSON ReplayProvenance where
  parseJSON value = do
    provenance@ReplayProvenance {..} <-
      genericParseJSON (aesonOptions $ Just "provenance") value
    unless (provenanceSchemaVersion == 1) $ fail "replay provenance schemaVersion must be 1"
    when (T.null provenanceContractSchemaRevision) $
      fail "replay provenance contractSchemaRevision must not be empty"
    void $ parseSha256 "provenance planSha256" provenancePlanSha256
    void $ parseSha256 "provenance sourceExportSha256" provenanceSourceExportSha256
    void $ parseGitSha "provenance sourceGameGitRevision" provenanceSourceGameGitRevision
    when (any (< 0) [provenanceUndoSteps, provenanceAnswersApplied]) $
      fail "replay provenance counts must not be negative"
    void $ parseSha256 "provenance checkpointGameSha256" provenanceCheckpointGameSha256
    void $ parseSha256 "provenance checkpointQueueSha256" provenanceCheckpointQueueSha256
    pure provenance

data ReplayCheckpointEnvelope = ReplayCheckpointEnvelope
  { replayCheckpointProvenance :: ReplayProvenance
  , replayCheckpointEnvelopeSha256 :: Text
  }
  deriving stock (Eq, Show)

instance ToJSON ReplayCheckpointEnvelope where
  toJSON ReplayCheckpointEnvelope {..} =
    object
      [ "type" .= String "arkham-replay-checkpoint"
      , "provenance" .= replayCheckpointProvenance
      , "envelopeSha256" .= replayCheckpointEnvelopeSha256
      ]

instance FromJSON ReplayCheckpointEnvelope where
  parseJSON = withObject "ReplayCheckpointEnvelope" \o -> do
    envelopeType :: Text <- o .: "type"
    unless (envelopeType == "arkham-replay-checkpoint") $
      fail "replay checkpoint envelope type must be \"arkham-replay-checkpoint\""
    replayCheckpointProvenance <- o .: "provenance"
    replayCheckpointEnvelopeSha256 <-
      o .: "envelopeSha256" >>= parseSha256 "checkpoint envelopeSha256"
    pure ReplayCheckpointEnvelope {..}

data CheckpointStatus
  = CheckpointNotReached
  | CheckpointReached
  | CheckpointMismatch String
  deriving stock (Eq, Show)

replayContractSchemaRevision :: Text
replayContractSchemaRevision = (Capabilities.serverCapabilities Nothing).schemaRevision

sha256Strict :: BS.ByteString -> Text
sha256Strict = decodeUtf8 . Base16.encode . SHA256.hash

sha256Lazy :: BSL.ByteString -> Text
sha256Lazy = decodeUtf8 . Base16.encode . SHA256.hashlazy

decodeReplayPlan :: BS.ByteString -> Either String ReplayPlan
decodeReplayPlan = eitherDecodeStrict'

decodeReplayInput
  :: ReplayBuildIdentity
  -> BS.ByteString
  -> Either String (ArkhamExport, ReplayInputKind, Maybe ReplayProvenance)
decodeReplayInput buildIdentity bytes = do
  value <- eitherDecodeStrict' bytes
  export <- resultToEither "Failed to parse export" $ fromJSON value
  envelopeValue <- case value of
    Object o -> do
      when (KeyMap.member "replayProvenance" o) $
        Left "legacy replayProvenance is not a valid checkpoint envelope"
      Right $ KeyMap.lookup "replayCheckpoint" o
    _ -> Left "Failed to parse export: expected a JSON object"
  case envelopeValue of
    Nothing -> do
      when (isSyntheticCheckpointExport export) $
        Left "replay checkpoint envelope is missing"
      pure (export, ReplayOrdinaryExport, Nothing)
    Just encodedEnvelope -> do
      envelope <-
        resultToEither "Failed to parse replayCheckpoint" $ fromJSON encodedEnvelope
      validateCheckpointExport buildIdentity export envelope
      pure (export, ReplayCheckpoint, Just envelope.replayCheckpointProvenance)

validateReplaySource
  :: ReplayBuildIdentity
  -> Text
  -> ReplayInputKind
  -> Game
  -> ReplayPlan
  -> Either String ()
validateReplaySource
  actualBuildIdentity
  actualExportSha
  actualInputKind
  game
  ReplayPlan {replayPlanSource = ReplaySource {..}} = do
  unless (actualExportSha == replaySourceExportSha256) $
    Left
      $ "source exportSha256 mismatch: expected "
      <> T.unpack replaySourceExportSha256
      <> ", got "
      <> T.unpack actualExportSha
  unless (actualInputKind == replaySourceInputKind) $
    Left "source inputKind mismatch"
  unless (gameGitRevision game == replaySourceGameGitRevision) $
    Left "source gameGitRevision mismatch"
  validateReplayBuildIdentity actualBuildIdentity
  unless (actualBuildIdentity == replaySourceReplayBuild) $
    Left "replay build identity mismatch"
  unless (replayContractSchemaRevision == replaySourceSchemaRevision) $
    Left "source schemaRevision mismatch"

validateRetainedSteps :: Int -> [ArkhamStep] -> Either String ()
validateRetainedSteps currentStep steps = do
  when (null steps) $ Left "replay input has no retained steps"
  let stepNumbers = map arkhamStepStep steps
  when (any (< 0) stepNumbers) $ Left "replay input contains a negative step"
  let ordered = sort stepNumbers
  unless (length ordered == length (ordNub ordered)) $
    Left "replay input contains duplicate retained steps"
  latest <- maybe (Left "replay input has no retained steps") Right $ lastMay ordered
  unless (latest == currentStep) $
    Left
      $ "latest retained step "
      <> show latest
      <> " does not match export step "
      <> show currentStep
  unless (and $ zipWith (\a b -> b == a + 1) ordered (drop 1 ordered)) $
    Left "replay input contains a missing retained step"

retainedQueueAt :: Int -> [ArkhamStep] -> Either String [Message]
retainedQueueAt step steps =
  case filter ((== step) . arkhamStepStep) steps of
    [retained] -> Right $ choiceMessages retained.arkhamStepChoice
    [] -> Left $ "missing retained resume step " <> show step
    _ -> Left $ "duplicate retained resume step " <> show step

prependReplayAnswerMessages :: [message] -> [message] -> [message]
prependReplayAnswerMessages answerMessages retainedQueue =
  answerMessages <> retainedQueue

questionCheckpoints :: Game -> Either String [QuestionCheckpoint]
questionCheckpoints game =
  traverse
    (\(pid, question) -> questionCheckpoint ("question-" <> tshow game.gameScenarioSteps) game pid question)
    (Map.toAscList game.gameQuestion)

checkQuestionCheckpoint :: Game -> QuestionCheckpoint -> CheckpointStatus
checkQuestionCheckpoint game QuestionCheckpoint {..}
  | checkpointQuestionVersion > game.gameScenarioSteps = CheckpointNotReached
  | checkpointQuestionVersion < game.gameScenarioSteps =
      mismatch
        $ "is stale: expected questionVersion "
        <> show checkpointQuestionVersion
        <> ", current questionVersion is "
        <> show game.gameScenarioSteps
  | otherwise = case Map.lookup checkpointPlayerId game.gameQuestion of
      Nothing ->
        mismatch
          $ "reached questionVersion "
          <> show checkpointQuestionVersion
          <> " but player "
          <> show checkpointPlayerId
          <> " is not being asked"
      Just question -> case questionCheckpoint checkpointName game checkpointPlayerId question of
        Left err -> CheckpointMismatch err
        Right actual
          | actual.checkpointPromptTag /= checkpointPromptTag ->
              mismatch
                $ "promptTag mismatch: expected "
                <> T.unpack checkpointPromptTag
                <> ", got "
                <> T.unpack actual.checkpointPromptTag
          | actual.checkpointPromptSha256 /= checkpointPromptSha256 ->
              mismatch
                $ "promptSha256 mismatch: expected "
                <> T.unpack checkpointPromptSha256
                <> ", got "
                <> T.unpack actual.checkpointPromptSha256
          | otherwise -> CheckpointReached
 where
  mismatch message = CheckpointMismatch $ "checkpoint " <> T.unpack checkpointName <> " " <> message

requireQuestionCheckpoint :: Game -> QuestionCheckpoint -> Either String ()
requireQuestionCheckpoint game checkpoint = case checkQuestionCheckpoint game checkpoint of
  CheckpointReached -> Right ()
  CheckpointMismatch err -> Left err
  CheckpointNotReached ->
    Left
      $ "checkpoint " <> T.unpack checkpoint.checkpointName
      <> " not reached: expected questionVersion "
      <> show checkpoint.checkpointQuestionVersion
      <> ", current questionVersion is "
      <> show game.gameScenarioSteps

validateReplayAnswer :: Game -> ReplayAnswerStep -> Either String PlayerId
validateReplayAnswer game ReplayAnswerStep {..} = do
  requireQuestionCheckpoint game replayAnswerExpected
  let expectedVersion = replayAnswerExpected.checkpointQuestionVersion
      expectedPlayer = replayAnswerExpected.checkpointPlayerId
      requireVersionAndPlayer label mVersion mPlayer = do
        version <- maybe (Left $ label <> " is missing questionVersion") Right mVersion
        unless (version == expectedVersion) $
          Left
            $ label
            <> " questionVersion mismatch: expected "
            <> show expectedVersion
            <> ", got "
            <> show version
        player <- maybe (Left $ label <> " is missing playerId") Right mPlayer
        unless (player == expectedPlayer) $ Left $ label <> " playerId does not match expected checkpoint player"
  case replayAnswerValue of
    Answer QuestionResponse {..} -> requireVersionAndPlayer "Answer" qrQuestionVersion qrPlayerId
    AmountsAnswer AmountsResponse {..} -> requireVersionAndPlayer "AmountsAnswer" arQuestionVersion arPlayerId
    PaymentAmountsAnswer PaymentAmountsResponse {..} ->
      requireVersionAndPlayer "PaymentAmountsAnswer" parQuestionVersion parPlayerId
    DeckAnswer _ player -> unless (player == expectedPlayer) $ Left "DeckAnswer playerId does not match expected checkpoint player"
    DeckListAnswer _ player -> unless (player == expectedPlayer) $ Left "DeckListAnswer playerId does not match expected checkpoint player"
    _ -> pure ()
  prompt <-
    maybe
      (Left "expected checkpoint player is no longer being asked")
      Right
      $ Map.lookup expectedPlayer game.gameQuestion
  unless (replayAnswerMatchesPrompt replayAnswerValue prompt) $
    Left
      $ replayAnswerConstructor replayAnswerValue
      <> " is not compatible with checkpoint prompt "
      <> T.unpack replayAnswerExpected.checkpointPromptTag
  pure expectedPlayer

replayAnswerMatchesPrompt :: Answer -> Question Message -> Bool
replayAnswerMatchesPrompt answer prompt = case answer of
  Answer {} -> isChoicePrompt $ stripPromptWrappers prompt
  Raw {} -> False
  PaymentAmountsAnswer {} -> isPaymentAmountsPrompt prompt
  AmountsAnswer {} -> isAmountsPrompt prompt
  StandaloneSettingsAnswer {} -> case stripPromptWrappers prompt of
    PickScenarioSettings -> True
    _ -> False
  CampaignSettingsAnswer {} -> case stripPromptWrappers prompt of
    PickCampaignSettings -> True
    _ -> False
  DeckAnswer {} -> isDeckPrompt $ stripPromptWrappers prompt
  DeckListAnswer {} -> isDeckPrompt $ stripPromptWrappers prompt
  PickDestinyAnswer {} -> case stripPromptWrappers prompt of
    PickDestiny {} -> True
    _ -> False
  CampaignSpecificAnswer {} -> case stripPromptWrappers prompt of
    PickCampaignSpecific {} -> True
    _ -> False
  ScenarioSpecificAnswer {} -> case stripPromptWrappers prompt of
    PickScenarioSpecific {} -> True
    _ -> False
  ExchangeAmountsAnswer answerSource answerFrom answerTo answerToken _ ->
    case stripPromptWrappers prompt of
      ChooseExchangeAmounts promptSource firstInvestigator _ secondInvestigator _ promptToken ->
        answerSource == promptSource
          && answerToken == promptToken
          && ( (answerFrom == firstInvestigator && answerTo == secondInvestigator)
                || (answerFrom == secondInvestigator && answerTo == firstInvestigator)
             )
      _ -> False
  CampaignStepAnswer {} -> case stripPromptWrappers prompt of
    ContinueCampaign -> True
    _ -> False

stripPromptWrappers :: Question message -> Question message
stripPromptWrappers = \case
  QuestionLabel _ _ prompt -> stripPromptWrappers prompt
  PayCostQuestion _ prompt -> stripPromptWrappers prompt
  QuestionWithSource _ _ prompt -> stripPromptWrappers prompt
  prompt -> prompt

isChoicePrompt :: Question message -> Bool
isChoicePrompt = \case
  ChooseOne {} -> True
  PlayerWindowChooseOne {} -> True
  WindowChooseOne {} -> True
  ChooseOneFromEach {} -> True
  ChooseN {} -> True
  ChooseSome {} -> True
  ChooseSome1 {} -> True
  ChooseUpToN {} -> True
  ChooseOneAtATime {} -> True
  ChooseOneAtATimeWithAuto {} -> True
  Read {} -> True
  ChooseOneWizard {} -> True
  PickSupplies {} -> True
  DropDown {} -> True
  _ -> False

isAmountsPrompt :: Question message -> Bool
isAmountsPrompt = \case
  ChooseAmounts {} -> True
  QuestionLabel _ _ (ChooseAmounts {}) -> True
  _ -> False

isPaymentAmountsPrompt :: Question message -> Bool
isPaymentAmountsPrompt = \case
  ChoosePaymentAmounts {} -> True
  PayCostQuestion _ (ChoosePaymentAmounts {}) -> True
  _ -> False

isDeckPrompt :: Question message -> Bool
isDeckPrompt = \case
  ChooseDeck -> True
  ChooseUpgradeDeck -> True
  _ -> False

replayAnswerConstructor :: Answer -> String
replayAnswerConstructor = \case
  Answer {} -> "Answer"
  Raw {} -> "Raw"
  PaymentAmountsAnswer {} -> "PaymentAmountsAnswer"
  AmountsAnswer {} -> "AmountsAnswer"
  StandaloneSettingsAnswer {} -> "StandaloneSettingsAnswer"
  CampaignSettingsAnswer {} -> "CampaignSettingsAnswer"
  DeckAnswer {} -> "DeckAnswer"
  DeckListAnswer {} -> "DeckListAnswer"
  PickDestinyAnswer {} -> "PickDestinyAnswer"
  CampaignSpecificAnswer {} -> "CampaignSpecificAnswer"
  ScenarioSpecificAnswer {} -> "ScenarioSpecificAnswer"
  ExchangeAmountsAnswer {} -> "ExchangeAmountsAnswer"
  CampaignStepAnswer {} -> "CampaignStepAnswer"

makeCheckpointExport :: ArkhamExport -> Game -> [Message] -> ArkhamExport
makeCheckpointExport source game pendingQueue =
  source
    { aeCampaignData =
        (aeCampaignData source)
          { agedCurrentData = game
          , agedStep = 0
          , agedSteps =
              [ArkhamStep (GameEntity.ArkhamGameKey UUID.nil) (Choice mempty pendingQueue) 0 (ActionDiff [])]
          , agedLog = []
          }
    }

checkpointExportValue :: ArkhamExport -> ReplayProvenance -> Value
checkpointExportValue export provenance = case toJSON export of
  Object o ->
    let envelope =
          ReplayCheckpointEnvelope
            { replayCheckpointProvenance = provenance
            , replayCheckpointEnvelopeSha256 = checkpointEnvelopeDigest export provenance
            }
     in Object $ KeyMap.insert "replayCheckpoint" (toJSON envelope) o
  _ -> error "ArkhamExport did not encode as an object"

checkpointEnvelopeDigest :: ArkhamExport -> ReplayProvenance -> Text
checkpointEnvelopeDigest export provenance =
  sha256Lazy $ encode $ object ["type" .= String "arkham-replay-checkpoint", "export" .= export, "provenance" .= provenance]

questionCheckpoint :: ToJSON message => Text -> Game -> PlayerId -> Question message -> Either String QuestionCheckpoint
questionCheckpoint name game player question = do
  when (T.null name) $ Left "checkpoint name must not be empty"
  when (game.gameScenarioSteps <= 0) $ Left "open question has a non-positive questionVersion"
  tag <- case toJSON question of
    Object o -> case KeyMap.lookup "tag" o of
      Just (String value) -> Right value
      _ -> Left "question JSON has no string tag"
    _ -> Left "question JSON is not an object"
  pure
    QuestionCheckpoint
      { checkpointName = name
      , checkpointQuestionVersion = game.gameScenarioSteps
      , checkpointPlayerId = player
      , checkpointPromptTag = tag
      , checkpointPromptSha256 = sha256Lazy $ encode question
      }

validateCheckpointExport
  :: ReplayBuildIdentity
  -> ArkhamExport
  -> ReplayCheckpointEnvelope
  -> Either String ()
validateCheckpointExport
  actualBuildIdentity
  export@ArkhamExport {aeCampaignData = ArkhamGameExportData {..}}
  ReplayCheckpointEnvelope
    { replayCheckpointProvenance = provenance@ReplayProvenance {..}
    , ..
    } = do
  unless
    (checkpointEnvelopeDigest export provenance == replayCheckpointEnvelopeSha256)
    $ Left "replay checkpoint envelope hash does not match its export and provenance"
  unless (agedStep == 0) $ Left "replay checkpoint provenance requires export step 0"
  unless (null agedLog) $ Left "replay checkpoint provenance requires an empty log"
  pendingQueue <- case agedSteps of
    [ArkhamStep gameId (Choice patch queue) step (ActionDiff actionDiffs)] -> do
      unless (gameId == GameEntity.ArkhamGameKey UUID.nil) $
        Left "replay checkpoint queue carrier must use the checkpoint game identity"
      unless (step == 0) $ Left "replay checkpoint queue carrier must be step 0"
      unless (toJSON patch == toJSON (mempty :: Patch)) $
        Left "replay checkpoint queue carrier must have an empty undo patch"
      unless (null actionDiffs) $
        Left "replay checkpoint queue carrier must have an empty action diff"
      pure queue
    _ -> Left "replay checkpoint provenance requires exactly one step-0 queue carrier"
  unless (gameGitRevision agedCurrentData == provenanceSourceGameGitRevision) $
    Left "replay checkpoint gameGitRevision does not match embedded provenance"
  validateReplayBuildIdentity actualBuildIdentity
  unless (provenanceReplayBuild == actualBuildIdentity) $
    Left "replay checkpoint build identity does not match this replay binary"
  unless (provenanceContractSchemaRevision == replayContractSchemaRevision) $
    Left "replay checkpoint schemaRevision does not match this backend contract"
  unless (sha256Lazy (encode agedCurrentData) == provenanceCheckpointGameSha256) $
    Left "replay checkpoint game hash does not match embedded provenance"
  unless (sha256Lazy (encode pendingQueue) == provenanceCheckpointQueueSha256) $
    Left "replay checkpoint pending queue hash does not match embedded provenance"
  requireQuestionCheckpoint agedCurrentData provenanceCheckpoint

isSyntheticCheckpointExport :: ArkhamExport -> Bool
isSyntheticCheckpointExport
  ArkhamExport
    { aeCampaignData =
        ArkhamGameExportData
          { agedStep = 0
          , agedSteps = [ArkhamStep gameId _ 0 _]
          }
    } =
    gameId == GameEntity.ArkhamGameKey UUID.nil
isSyntheticCheckpointExport _ = False

parseSha256 :: String -> Text -> Parser Text
parseSha256 label value =
  either fail pure $ validateLowerHex label 64 value

parseGitSha :: String -> GitSha -> Parser GitSha
parseGitSha label value@(GitSha sha) =
  value <$ either fail pure (validateLowerHex label 40 sha)

validateLowerHex :: String -> Int -> Text -> Either String Text
validateLowerHex label expectedLength value
  | T.length value /= expectedLength || T.any (not . isLowerHex) value =
      Left $ label <> " must contain exactly " <> show expectedLength <> " lowercase hexadecimal characters"
  | otherwise = Right value
 where
  isLowerHex c = ('0' <= c && c <= '9') || c `elem` ['a' .. 'f']

resultToEither :: String -> Result a -> Either String a
resultToEither label = \case
  Error err -> Left $ label <> ": " <> err
  Success value -> Right value
