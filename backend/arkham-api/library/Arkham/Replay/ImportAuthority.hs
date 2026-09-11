module Arkham.Replay.ImportAuthority (
  ReplayImportAuthority (..),
  ReplayImportReceipt (..),
  backendBuildIdentityHeaderName,
  replayImportReceiptHeaderName,
  backendBuildIdentityHeaderValue,
  replayImportReceiptHeaderValue,
  backendBuildIdentityHeaders,
  replayImportResponseHeaders,
  decodeReplayImport,
  makeReplayImportReceipt,
) where

import Api.Arkham.Export
import Arkham.Git (GitSha (..))
import Arkham.Json (aesonOptions)
import Arkham.Prelude
import Arkham.Replay.BuildIdentity
import Arkham.Replay.Checkpoint
import Control.Monad.Fail (fail)
import Data.Aeson.Types (Parser)
import Data.ByteString qualified as BS
import Data.ByteString.Lazy qualified as BSL
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE

data ReplayImportAuthority = ReplayImportAuthority
  { replayImportCheckpointSha256 :: Text
  , replayImportEnvelopeSha256 :: Text
  , replayImportGameGitRevision :: GitSha
  }
  deriving stock (Eq, Show)

data ReplayImportReceipt = ReplayImportReceipt
  { replayImportReceiptSchemaVersion :: Int
  , replayImportReceiptGameId :: Text
  , replayImportReceiptGameGitRevision :: GitSha
  , replayImportReceiptBackendBuild :: ReplayBuildIdentity
  , replayImportReceiptCheckpointSha256 :: Text
  , replayImportReceiptEnvelopeSha256 :: Text
  }
  deriving stock (Eq, Generic, Show)

instance ToJSON ReplayImportReceipt where
  toJSON = genericToJSON $ aesonOptions $ Just "replayImportReceipt"
  toEncoding = genericToEncoding $ aesonOptions $ Just "replayImportReceipt"

instance FromJSON ReplayImportReceipt where
  parseJSON value = do
    receipt@ReplayImportReceipt {..} <-
      genericParseJSON (aesonOptions $ Just "replayImportReceipt") value
    unless (replayImportReceiptSchemaVersion == 1) $
      fail "replay import receipt schemaVersion must be 1"
    when (T.null replayImportReceiptGameId) $
      fail "replay import receipt gameId must not be empty"
    void $ parseLowerHex "replay import receipt gameGitRevision" 40 $ unGitSha replayImportReceiptGameGitRevision
    either fail pure $ validateReplayBuildIdentity replayImportReceiptBackendBuild
    void $ parseLowerHex "replay import receipt checkpointSha256" 64 replayImportReceiptCheckpointSha256
    void $ parseLowerHex "replay import receipt envelopeSha256" 64 replayImportReceiptEnvelopeSha256
    pure receipt

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

decodeReplayImport
  :: ReplayBuildIdentity
  -> BS.ByteString
  -> Either String (ArkhamExport, Maybe ReplayImportAuthority)
decodeReplayImport buildIdentity bytes = do
  (export, inputKind, envelope) <- decodeReplayInputEnvelope buildIdentity bytes
  authority <- case (inputKind, envelope) of
    (ReplayOrdinaryExport, Nothing) -> pure Nothing
    (ReplayCheckpoint, Just ReplayCheckpointEnvelope {..}) ->
      pure
        $ Just
        $ ReplayImportAuthority
          { replayImportCheckpointSha256 = sha256Strict bytes
          , replayImportEnvelopeSha256 = replayCheckpointEnvelopeSha256
          , replayImportGameGitRevision =
              replayCheckpointProvenance.provenanceSourceGameGitRevision
          }
    _ -> Left "replay import authority does not match the decoded input kind"
  pure (export, authority)

makeReplayImportReceipt
  :: ReplayBuildIdentity
  -> Text
  -> ReplayImportAuthority
  -> ReplayImportReceipt
makeReplayImportReceipt buildIdentity gameId ReplayImportAuthority {..} =
  ReplayImportReceipt
    { replayImportReceiptSchemaVersion = 1
    , replayImportReceiptGameId = gameId
    , replayImportReceiptGameGitRevision = replayImportGameGitRevision
    , replayImportReceiptBackendBuild = buildIdentity
    , replayImportReceiptCheckpointSha256 = replayImportCheckpointSha256
    , replayImportReceiptEnvelopeSha256 = replayImportEnvelopeSha256
    }

parseLowerHex :: String -> Int -> Text -> Parser Text
parseLowerHex label size value
  | T.length value /= size || T.any (not . isLowerHex) value =
      fail $ label <> " must contain exactly " <> show size <> " lowercase hexadecimal characters"
  | otherwise = pure value
 where
  isLowerHex c = ('0' <= c && c <= '9') || c `elem` ['a' .. 'f']
