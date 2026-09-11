module Arkham.Replay.ImportAuthority (
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
  , replayImportCheckpointProvenance :: ReplayProvenance
  }
  deriving stock (Eq, Show)

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
  , replayImportReceiptCheckpointProvenance :: ReplayProvenance
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
      , "checkpointProvenance" .= replayImportReceiptCheckpointProvenance
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
              <*> o .: "checkpointProvenance"
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
  , replayAttestationCheckpointProvenance :: ReplayProvenance
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
  replayImportCheckpointProvenance.provenanceCheckpoint.checkpointPlayerId

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
      pure
        $ Just
        $ ReplayImportAuthority
          { replayImportCheckpointSha256 = sha256Strict bytes
          , replayImportCanonicalEnvelopeSha256 =
              canonicalReplayCheckpointEnvelopeSha256 export provenance
          , replayImportGameGitRevision =
              provenance.provenanceSourceGameGitRevision
          , replayImportCheckpointProvenance = provenance
          }
    _ -> Left "replay import authority does not match the decoded input kind"
  pure (export, authority)

makeReplayImportReceipt
  :: ReplayBuildIdentity
  -> Text
  -> [ReplayPlayerRemapping]
  -> ReplayImportAuthority
  -> ReplayImportReceipt
makeReplayImportReceipt buildIdentity gameId remappings ReplayImportAuthority {..} =
  let receiptWithoutDigest =
        ReplayImportReceipt
          { replayImportReceiptSchemaVersion = 1
          , replayImportReceiptGameId = gameId
          , replayImportReceiptGameGitRevision = replayImportGameGitRevision
          , replayImportReceiptBackendBuild = buildIdentity
          , replayImportReceiptCheckpointSha256 = replayImportCheckpointSha256
          , replayImportReceiptCanonicalEnvelopeSha256 =
              replayImportCanonicalEnvelopeSha256
          , replayImportReceiptCheckpointProvenance =
              replayImportCheckpointProvenance
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
  when (null replayImportReceiptPlayerRemappings) $
    Left "replay import receipt must contain a player remapping"
  traverse_ validateReplayPlayerRemapping replayImportReceiptPlayerRemappings
  let boundCheckpointPlayerId =
        replayImportReceiptCheckpointProvenance.provenanceCheckpoint.checkpointPlayerId
  unless
    ( boundCheckpointPlayerId
        `elem` map (.replayPlayerCheckpointPlayerId) replayImportReceiptPlayerRemappings
    )
    $ Left "replay import receipt is not bound to the checkpoint prompt player"
  let remappedInvestigators =
        map (.replayPlayerInvestigatorId) replayImportReceiptPlayerRemappings
  unless (length remappedInvestigators == length (ordNub remappedInvestigators)) $
    Left "replay import receipt contains duplicate investigator remappings"
  unless
    ( replayImportReceiptCheckpointProvenance.provenanceSourceGameGitRevision
        == replayImportReceiptGameGitRevision
    )
    $ Left "replay import receipt gameGitRevision does not match checkpoint provenance"
  unless
    ( replayImportReceiptCheckpointProvenance.provenanceReplayBuild
        == replayImportReceiptBackendBuild
    )
    $ Left "replay import receipt backendBuild does not match checkpoint provenance"
  unless
    ( replayImportReceiptCheckpointProvenance.provenanceContractSchemaRevision
        == replayContractSchemaRevision
    )
    $ Left "replay import receipt checkpoint contract revision does not match this backend"
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
      , replayAttestationCheckpointProvenance =
          replayImportReceiptCheckpointProvenance
      , replayAttestationRunningServerBuild = runningBuild
      , replayAttestationImportReceipt = receipt
      }

replayImportReceiptDigest :: ReplayImportReceipt -> Text
replayImportReceiptDigest ReplayImportReceipt {..} =
  sha256Lazy
    $ encode
    $ object
      [ "schemaVersion" .= replayImportReceiptSchemaVersion
      , "gameId" .= replayImportReceiptGameId
      , "gameGitRevision" .= replayImportReceiptGameGitRevision
      , "backendBuild" .= replayImportReceiptBackendBuild
      , "checkpointSha256" .= replayImportReceiptCheckpointSha256
      , "canonicalEnvelopeSha256" .= replayImportReceiptCanonicalEnvelopeSha256
      , "checkpointProvenance" .= replayImportReceiptCheckpointProvenance
      , "playerRemappings" .= replayImportReceiptPlayerRemappings
      ]

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
