module Arkham.Replay.ImportAuthority (
  ReplayValidatedPrompt (..),
  ReplayValidatedCheckpoint (..),
  ReplayImportAuthority (..),
  ReplayPlayerRemapping (..),
  ReplayImportReceipt (..),
  ReplayAttestation (..),
  backendBuildIdentityHeaderName,
  replayImportReceiptHeaderName,
  backendBuildIdentityHeaderValue,
  replayImportReceiptHeaderValue,
  backendBuildIdentityHeaders,
  replayImportResponseHeaders,
  replayImportCheckpointPlayerId,
  decodeReplayImport,
  makeReplayImportReceipt,
  makeReplayAttestation,
  validateReplayImportReceipt,
) where

import Api.Arkham.Export
import Arkham.Git (GitSha (..))
import Arkham.Id (PlayerId (..))
import Arkham.Json (aesonOptions)
import Arkham.Prelude
import Arkham.Replay.BuildIdentity
import Arkham.Replay.Checkpoint
import Control.Monad.Fail (fail)
import Data.ByteString qualified as BS
import Data.ByteString.Lazy qualified as BSL
import Data.List qualified as List
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import Data.UUID qualified as UUID

data ReplayImportAuthority = ReplayImportAuthority
  { replayImportCheckpointSha256 :: Text
  , replayImportCanonicalEnvelopeSha256 :: Text
  , replayImportGameGitRevision :: GitSha
  , replayImportBackendBuild :: ReplayBuildIdentity
  , replayImportValidatedCheckpoint :: ReplayValidatedCheckpoint
  }
  deriving stock (Eq, Show)

data ReplayValidatedPrompt = ReplayValidatedPrompt
  { replayValidatedPromptQuestionVersion :: Int
  , replayValidatedPromptPlayerId :: PlayerId
  , replayValidatedPromptPromptTag :: Text
  , replayValidatedPromptPromptSha256 :: Text
  }
  deriving stock (Eq, Generic, Show)

instance ToJSON ReplayValidatedPrompt where
  toJSON = genericToJSON $ aesonOptions $ Just "replayValidatedPrompt"
  toEncoding = genericToEncoding $ aesonOptions $ Just "replayValidatedPrompt"

instance FromJSON ReplayValidatedPrompt where
  parseJSON value = do
    prompt <-
      genericParseJSON (aesonOptions $ Just "replayValidatedPrompt") value
    unless (value == toJSON prompt) $
      fail "validated replay prompt contains non-canonical or unknown fields"
    either fail pure $ validateReplayValidatedPrompt prompt
    pure prompt

data ReplayValidatedCheckpoint = ReplayValidatedCheckpoint
  { replayValidatedCheckpointSchemaVersion :: Int
  , replayValidatedCheckpointContractSchemaRevision :: Text
  , replayValidatedCheckpointPrompt :: ReplayValidatedPrompt
  , replayValidatedCheckpointGameSha256 :: Text
  , replayValidatedCheckpointQueueSha256 :: Text
  }
  deriving stock (Eq, Generic, Show)

instance ToJSON ReplayValidatedCheckpoint where
  toJSON ReplayValidatedCheckpoint {..} =
    object
      [ "schemaVersion" .= replayValidatedCheckpointSchemaVersion
      , "contractSchemaRevision"
          .= replayValidatedCheckpointContractSchemaRevision
      , "prompt" .= replayValidatedCheckpointPrompt
      , "checkpointGameSha256" .= replayValidatedCheckpointGameSha256
      , "checkpointQueueSha256" .= replayValidatedCheckpointQueueSha256
      ]

instance FromJSON ReplayValidatedCheckpoint where
  parseJSON value = do
    checkpoint <-
      withObject "ReplayValidatedCheckpoint"
        ( \o ->
            ReplayValidatedCheckpoint
              <$> o .: "schemaVersion"
              <*> o .: "contractSchemaRevision"
              <*> o .: "prompt"
              <*> o .: "checkpointGameSha256"
              <*> o .: "checkpointQueueSha256"
        )
        value
    unless (value == toJSON checkpoint) $
      fail "validated replay checkpoint contains non-canonical or unknown fields"
    either fail pure $ validateReplayValidatedCheckpoint checkpoint
    pure checkpoint

data ReplayPlayerRemapping = ReplayPlayerRemapping
  { replayPlayerInvestigatorId :: Text
  , replayPlayerCheckpointPlayerId :: PlayerId
  , replayPlayerImportedPlayerId :: Text
  , replayPlayerLivePlayerId :: Text
  , replayPlayerStateRemapped :: Bool
  }
  deriving stock (Eq, Generic, Show)

instance ToJSON ReplayPlayerRemapping where
  toJSON = genericToJSON $ aesonOptions $ Just "replayPlayer"
  toEncoding = genericToEncoding $ aesonOptions $ Just "replayPlayer"

instance FromJSON ReplayPlayerRemapping where
  parseJSON value = do
    remapping <-
      genericParseJSON (aesonOptions $ Just "replayPlayer") value
    either fail pure $ validateReplayPlayerRemapping remapping
    pure remapping

data ReplayImportReceipt = ReplayImportReceipt
  { replayImportReceiptSchemaVersion :: Int
  , replayImportReceiptGameId :: Text
  , replayImportReceiptGameGitRevision :: GitSha
  , replayImportReceiptBackendBuild :: ReplayBuildIdentity
  , replayImportReceiptCheckpointSha256 :: Text
  , replayImportReceiptCanonicalEnvelopeSha256 :: Text
  , replayImportReceiptValidatedCheckpoint :: ReplayValidatedCheckpoint
  , replayImportReceiptPlayerRemappings :: [ReplayPlayerRemapping]
  , replayImportReceiptSha256 :: Text
  }
  deriving stock (Eq, Generic, Show)

instance ToJSON ReplayImportReceipt where
  toJSON ReplayImportReceipt {..} =
    object
      [ "schemaVersion" .= replayImportReceiptSchemaVersion
      , "gameId" .= replayImportReceiptGameId
      , "gameGitRevision" .= replayImportReceiptGameGitRevision
      , "backendBuild" .= replayImportReceiptBackendBuild
      , "checkpointSha256" .= replayImportReceiptCheckpointSha256
      , "canonicalEnvelopeSha256" .= replayImportReceiptCanonicalEnvelopeSha256
      , "validatedCheckpoint" .= replayImportReceiptValidatedCheckpoint
      , "playerRemappings" .= replayImportReceiptPlayerRemappings
      , "receiptSha256" .= replayImportReceiptSha256
      ]

instance FromJSON ReplayImportReceipt where
  parseJSON value = do
    receipt <-
      withObject "ReplayImportReceipt"
        ( \o ->
            ReplayImportReceipt
              <$> o .: "schemaVersion"
              <*> o .: "gameId"
              <*> o .: "gameGitRevision"
              <*> o .: "backendBuild"
              <*> o .: "checkpointSha256"
              <*> o .: "canonicalEnvelopeSha256"
              <*> o .: "validatedCheckpoint"
              <*> o .: "playerRemappings"
              <*> o .: "receiptSha256"
        )
        value
    unless (value == toJSON receipt) $
      fail "replay import receipt contains non-canonical or unknown fields"
    either fail pure $ validateReplayImportReceipt receipt
    pure receipt

data ReplayAttestation = ReplayAttestation
  { replayAttestationSchemaVersion :: Int
  , replayAttestationGameId :: Text
  , replayAttestationGameGitRevision :: GitSha
  , replayAttestationCheckpointSha256 :: Text
  , replayAttestationCanonicalEnvelopeSha256 :: Text
  , replayAttestationValidatedCheckpoint :: ReplayValidatedCheckpoint
  , replayAttestationRunningServerBuild :: ReplayBuildIdentity
  , replayAttestationImportReceipt :: ReplayImportReceipt
  }
  deriving stock (Eq, Generic, Show)

instance ToJSON ReplayAttestation where
  toJSON = genericToJSON $ aesonOptions $ Just "replayAttestation"
  toEncoding = genericToEncoding $ aesonOptions $ Just "replayAttestation"

backendBuildIdentityHeaderName :: Text
backendBuildIdentityHeaderName = "X-Arkham-Backend-Build-Identity"

replayImportReceiptHeaderName :: Text
replayImportReceiptHeaderName = "X-Arkham-Replay-Import-Receipt"

backendBuildIdentityHeaderValue :: ReplayBuildIdentity -> Text
backendBuildIdentityHeaderValue = TE.decodeUtf8 . BSL.toStrict . encode

replayImportReceiptHeaderValue :: ReplayImportReceipt -> Text
replayImportReceiptHeaderValue = TE.decodeUtf8 . BSL.toStrict . encode

backendBuildIdentityHeaders :: ReplayBuildIdentity -> [(Text, Text)]
backendBuildIdentityHeaders buildIdentity =
  [(backendBuildIdentityHeaderName, backendBuildIdentityHeaderValue buildIdentity)]

replayImportResponseHeaders
  :: ReplayBuildIdentity
  -> Maybe ReplayImportReceipt
  -> [(Text, Text)]
replayImportResponseHeaders buildIdentity receipt =
  backendBuildIdentityHeaders buildIdentity
    <> [ (replayImportReceiptHeaderName, replayImportReceiptHeaderValue value)
       | value <- maybeToList receipt
       ]

replayImportCheckpointPlayerId :: ReplayImportAuthority -> PlayerId
replayImportCheckpointPlayerId ReplayImportAuthority {..} =
  replayImportValidatedCheckpoint.replayValidatedCheckpointPrompt.replayValidatedPromptPlayerId

decodeReplayImport
  :: ReplayBuildIdentity
  -> BS.ByteString
  -> Either String (ArkhamExport, Maybe ReplayImportAuthority)
decodeReplayImport buildIdentity bytes = do
  (export, inputKind, envelope) <- decodeReplayInputEnvelope buildIdentity bytes
  authority <- case (inputKind, envelope) of
    (ReplayOrdinaryExport, Nothing) -> pure Nothing
    (ReplayCheckpoint, Just ReplayCheckpointEnvelope {..}) -> do
      validateCleanReplayBuildIdentity buildIdentity
      let provenance = replayCheckpointProvenance
          checkpoint = provenance.provenanceCheckpoint
      pure
        $ Just
        $ ReplayImportAuthority
          { replayImportCheckpointSha256 = sha256Strict bytes
          , replayImportCanonicalEnvelopeSha256 =
              canonicalReplayCheckpointEnvelopeSha256 export provenance
          , replayImportGameGitRevision =
              provenance.provenanceSourceGameGitRevision
          , replayImportBackendBuild = buildIdentity
          , replayImportValidatedCheckpoint =
              ReplayValidatedCheckpoint
                { replayValidatedCheckpointSchemaVersion = 1
                , replayValidatedCheckpointContractSchemaRevision =
                    provenance.provenanceContractSchemaRevision
                , replayValidatedCheckpointPrompt =
                    ReplayValidatedPrompt
                      { replayValidatedPromptQuestionVersion =
                          checkpoint.checkpointQuestionVersion
                      , replayValidatedPromptPlayerId =
                          checkpoint.checkpointPlayerId
                      , replayValidatedPromptPromptTag =
                          checkpoint.checkpointPromptTag
                      , replayValidatedPromptPromptSha256 =
                          checkpoint.checkpointPromptSha256
                      }
                , replayValidatedCheckpointGameSha256 =
                    provenance.provenanceCheckpointGameSha256
                , replayValidatedCheckpointQueueSha256 =
                    provenance.provenanceCheckpointQueueSha256
                }
          }
    _ -> Left "replay import authority does not match the decoded input kind"
  pure (export, authority)

makeReplayImportReceipt
  :: Text
  -> [ReplayPlayerRemapping]
  -> ReplayImportAuthority
  -> ReplayImportReceipt
makeReplayImportReceipt gameId remappings ReplayImportAuthority {..} =
  let receiptWithoutDigest =
        ReplayImportReceipt
          { replayImportReceiptSchemaVersion = 1
          , replayImportReceiptGameId = gameId
          , replayImportReceiptGameGitRevision = replayImportGameGitRevision
          , replayImportReceiptBackendBuild = replayImportBackendBuild
          , replayImportReceiptCheckpointSha256 = replayImportCheckpointSha256
          , replayImportReceiptCanonicalEnvelopeSha256 =
              replayImportCanonicalEnvelopeSha256
          , replayImportReceiptValidatedCheckpoint =
              replayImportValidatedCheckpoint
          , replayImportReceiptPlayerRemappings =
              List.sortOn (.replayPlayerInvestigatorId) remappings
          , replayImportReceiptSha256 = ""
          }
   in receiptWithoutDigest
        { replayImportReceiptSha256 = replayImportReceiptDigest receiptWithoutDigest
        }

validateReplayImportReceipt :: ReplayImportReceipt -> Either String ()
validateReplayImportReceipt receipt@ReplayImportReceipt {..} = do
  unless (replayImportReceiptSchemaVersion == 1) $
    Left "replay import receipt schemaVersion must be 1"
  when (T.null replayImportReceiptGameId) $
    Left "replay import receipt gameId must not be empty"
  unless (isJust $ UUID.fromText replayImportReceiptGameId) $
    Left "replay import receipt gameId must be a UUID"
  validateLowerHex "replay import receipt gameGitRevision" 40 $
    unGitSha replayImportReceiptGameGitRevision
  validateCleanReplayBuildIdentity replayImportReceiptBackendBuild
  validateLowerHex
    "replay import receipt checkpointSha256"
    64
    replayImportReceiptCheckpointSha256
  validateLowerHex
    "replay import receipt canonicalEnvelopeSha256"
    64
    replayImportReceiptCanonicalEnvelopeSha256
  validateLowerHex
    "replay import receipt receiptSha256"
    64
    replayImportReceiptSha256
  validateReplayValidatedCheckpoint replayImportReceiptValidatedCheckpoint
  when (null replayImportReceiptPlayerRemappings) $
    Left "replay import receipt must contain a player remapping"
  traverse_ validateReplayPlayerRemapping replayImportReceiptPlayerRemappings
  let boundCheckpointPlayerId =
        replayImportReceiptValidatedCheckpoint.replayValidatedCheckpointPrompt.replayValidatedPromptPlayerId
  unless
    ( boundCheckpointPlayerId
        `elem` map (.replayPlayerCheckpointPlayerId) replayImportReceiptPlayerRemappings
    )
    $ Left "replay import receipt is not bound to the checkpoint prompt player"
  let remappedInvestigators =
        map (.replayPlayerInvestigatorId) replayImportReceiptPlayerRemappings
  unless (length remappedInvestigators == length (ordNub remappedInvestigators)) $
    Left "replay import receipt contains duplicate investigator remappings"
  let checkpointPlayerIds =
        map (.replayPlayerCheckpointPlayerId) replayImportReceiptPlayerRemappings
  unless (length checkpointPlayerIds == length (ordNub checkpointPlayerIds)) $
    Left "replay import receipt contains duplicate checkpoint player remappings"
  unless
    ( replayImportReceiptValidatedCheckpoint.replayValidatedCheckpointContractSchemaRevision
        == replayContractSchemaRevision
    )
    $ Left "replay import receipt validated checkpoint contract revision does not match this backend"
  let expectedDigest =
        replayImportReceiptDigest receipt {replayImportReceiptSha256 = ""}
  unless (replayImportReceiptSha256 == expectedDigest) $
    Left "replay import receipt digest does not match its contents"

makeReplayAttestation
  :: ReplayBuildIdentity
  -> Text
  -> GitSha
  -> ReplayImportReceipt
  -> Either String ReplayAttestation
makeReplayAttestation runningBuild gameId gameGitRevision receipt@ReplayImportReceipt {..} = do
  validateReplayImportReceipt receipt
  validateCleanReplayBuildIdentity runningBuild
  unless (replayImportReceiptGameId == gameId) $
    Left "replay import receipt is bound to a different game"
  unless (replayImportReceiptGameGitRevision == gameGitRevision) $
    Left "replay import receipt game revision no longer matches the game"
  unless (replayImportReceiptBackendBuild == runningBuild) $
    Left "running server build no longer matches the replay import authority"
  pure
    ReplayAttestation
      { replayAttestationSchemaVersion = 1
      , replayAttestationGameId = gameId
      , replayAttestationGameGitRevision = gameGitRevision
      , replayAttestationCheckpointSha256 =
          replayImportReceiptCheckpointSha256
      , replayAttestationCanonicalEnvelopeSha256 =
          replayImportReceiptCanonicalEnvelopeSha256
      , replayAttestationValidatedCheckpoint =
          replayImportReceiptValidatedCheckpoint
      , replayAttestationRunningServerBuild = runningBuild
      , replayAttestationImportReceipt = receipt
      }

replayImportReceiptDigest :: ReplayImportReceipt -> Text
replayImportReceiptDigest ReplayImportReceipt {..} =
  canonicalJsonSha256
    $ object
      [ "schemaVersion" .= replayImportReceiptSchemaVersion
      , "gameId" .= replayImportReceiptGameId
      , "gameGitRevision" .= replayImportReceiptGameGitRevision
      , "backendBuild" .= replayImportReceiptBackendBuild
      , "checkpointSha256" .= replayImportReceiptCheckpointSha256
      , "canonicalEnvelopeSha256" .= replayImportReceiptCanonicalEnvelopeSha256
      , "validatedCheckpoint" .= replayImportReceiptValidatedCheckpoint
      , "playerRemappings" .= replayImportReceiptPlayerRemappings
      ]

validateReplayValidatedPrompt :: ReplayValidatedPrompt -> Either String ()
validateReplayValidatedPrompt ReplayValidatedPrompt {..} = do
  when (replayValidatedPromptQuestionVersion <= 0) $
    Left "validated replay prompt questionVersion must be positive"
  when (T.null replayValidatedPromptPromptTag) $
    Left "validated replay prompt promptTag must not be empty"
  validateLowerHex
    "validated replay prompt promptSha256"
    64
    replayValidatedPromptPromptSha256

validateReplayValidatedCheckpoint :: ReplayValidatedCheckpoint -> Either String ()
validateReplayValidatedCheckpoint ReplayValidatedCheckpoint {..} = do
  unless (replayValidatedCheckpointSchemaVersion == 1) $
    Left "validated replay checkpoint schemaVersion must be 1"
  when (T.null replayValidatedCheckpointContractSchemaRevision) $
    Left "validated replay checkpoint contractSchemaRevision must not be empty"
  validateReplayValidatedPrompt replayValidatedCheckpointPrompt
  validateLowerHex
    "validated replay checkpoint gameSha256"
    64
    replayValidatedCheckpointGameSha256
  validateLowerHex
    "validated replay checkpoint queueSha256"
    64
    replayValidatedCheckpointQueueSha256

validateReplayPlayerRemapping :: ReplayPlayerRemapping -> Either String ()
validateReplayPlayerRemapping ReplayPlayerRemapping {..} = do
  when (T.null replayPlayerInvestigatorId) $
    Left "replay player remapping investigatorId must not be empty"
  unless (isJust $ UUID.fromText replayPlayerImportedPlayerId) $
    Left "replay player remapping importedPlayerId must be a UUID"
  unless (isJust $ UUID.fromText replayPlayerLivePlayerId) $
    Left "replay player remapping livePlayerId must be a UUID"
  let checkpointPlayerId = UUID.toText $ unPlayerId replayPlayerCheckpointPlayerId
  if replayPlayerStateRemapped
    then
      unless (replayPlayerLivePlayerId == replayPlayerImportedPlayerId) $
        Left "remapped replay player livePlayerId must equal importedPlayerId"
    else
      unless (replayPlayerLivePlayerId == checkpointPlayerId) $
        Left "unremapped replay player livePlayerId must equal checkpointPlayerId"

validateLowerHex :: String -> Int -> Text -> Either String ()
validateLowerHex label size value
  | T.length value /= size || T.any (not . isLowerHex) value =
      Left $ label <> " must contain exactly " <> show size <> " lowercase hexadecimal characters"
  | otherwise = Right ()
 where
  isLowerHex c = ('0' <= c && c <= '9') || c `elem` ['a' .. 'f']
