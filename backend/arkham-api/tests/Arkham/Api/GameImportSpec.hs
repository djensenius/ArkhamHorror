module Arkham.Api.GameImportSpec (spec) where

import Api.Handler.Arkham.Game.Debug
  ( makeReplayPlayerRemapping
  , selectUploadedExportFile
  )
import Arkham.Id (PlayerId (..))
import Arkham.Prelude
import Arkham.Replay.ImportAuthority
import Data.Either (isLeft)
import Data.Text qualified as T
import Data.Text.IO qualified as T
import Data.UUID qualified as UUID
import System.Directory (doesFileExist)
import Test.Hspec

{- | Regression for #30: 'postApiV1ArkhamGamesImportR' used
@'Safe.fromJustNote' "No export file uploaded" . headMay@ to pick the
uploaded multipart file, which threw and returned a 500 whenever a request
carried no file. It now branches on 'selectUploadedExportFile' explicitly
and rejects a missing file with 'invalidArgs' (400) before ever reaching
decoding. These tests exercise that selector -- the exact function the
production handler branches on -- directly, covering both the
empty-upload ('Nothing') and file-present ('Just') control-flow branches.
-}
spec :: Spec
spec = describe "selectUploadedExportFile" do
  it "returns Nothing for an empty upload" do
    selectUploadedExportFile ([] :: [(Text, Int)]) `shouldBe` Nothing

  it "returns the first uploaded file's payload when present" do
    selectUploadedExportFile [("export", 42 :: Int), ("other", 7)] `shouldBe` Just 42

  describe "makeReplayPlayerRemapping" do
    let checkpointPlayerId = "00000000-0000-0000-0000-000000000000"
        importedPlayerId = "00000000-0000-0000-0000-000000000002"
        checkpointUUID = UUID.nil

    it "records a solo import without claiming that game state was rewritten" do
      makeReplayPlayerRemapping
        "c01001"
        checkpointPlayerId
        importedPlayerId
        False
        `shouldBe` Right
          ReplayPlayerRemapping
            { replayPlayerInvestigatorId = "c01001"
            , replayPlayerCheckpointPlayerId = PlayerId checkpointUUID
            , replayPlayerImportedPlayerId = importedPlayerId
            , replayPlayerLivePlayerId = checkpointPlayerId
            , replayPlayerStateRemapped = False
            }

    it "binds a multiplayer import to the replacement live player ID" do
      makeReplayPlayerRemapping
        "c01001"
        checkpointPlayerId
        importedPlayerId
        True
        `shouldBe` Right
          ReplayPlayerRemapping
            { replayPlayerInvestigatorId = "c01001"
            , replayPlayerCheckpointPlayerId = PlayerId checkpointUUID
            , replayPlayerImportedPlayerId = importedPlayerId
            , replayPlayerLivePlayerId = importedPlayerId
            , replayPlayerStateRemapped = True
            }

    it "fails closed on malformed checkpoint or imported player IDs" do
      makeReplayPlayerRemapping "c01001" "not-a-uuid" importedPlayerId False
        `shouldSatisfy` isLeft
      makeReplayPlayerRemapping "c01001" checkpointPlayerId "not-a-uuid" False
        `shouldSatisfy` isLeft

  it "preserves the production PublicGame import body and authority headers" do
    source <- readDebugSource
    let normalized = T.unwords $ T.words source
        handlerAndRest =
          snd $ T.breakOn "postApiV1ArkhamGamesImportR = do" normalized
        importHandler =
          fst
            $ T.breakOn
              "getApiV1ArkhamGameReplayAttestationR"
              handlerAndRest
        position needle =
          let (prefix, suffix) = T.breakOn needle importHandler
           in if T.null suffix
                then expectationFailure ("missing import-handler source: " <> T.unpack needle) >> error "missing source"
                else pure $ T.length prefix
    normalized
      `shouldSatisfy` T.isInfixOf
        "postApiV1ArkhamGamesImportR :: Handler (PublicGame ArkhamGameId)"
    decodePosition <- position "decodeExportBytes"
    transactionPosition <- position "(key, importReceipt) <- runDB"
    headerPosition <-
      position
        "replayImportResponseHeaders serverBuildIdentity importReceipt"
    publicGamePosition <- position "$ toPublicGame"
    decodePosition `shouldSatisfy` (< transactionPosition)
    headerPosition `shouldSatisfy` (< publicGamePosition)
    importHandler
      `shouldSatisfy` T.isInfixOf
        "for_ importReceipt \\receipt -> do"
    importHandler
      `shouldSatisfy` T.isInfixOf
        "traverse_ (uncurry addHeader) (replayImportResponseHeaders serverBuildIdentity importReceipt)"
    importHandler
      `shouldSatisfy` T.isInfixOf
        "$ toPublicGame (Entity key $ ArkhamGame agedName agedCurrentData agedStep variant now now)"

  it "binds replay attestation reads to the authenticated game membership" do
    source <- readDebugSource
    let normalized = T.unwords $ T.words source
    normalized
      `shouldSatisfy` T.isInfixOf
        "getApiV1ArkhamGameReplayAttestationR gameId = do Entity userId user <- getRequestUser withGameAccess user.admin (isJust <$> runDB (getBy $ UniquePlayer userId gameId)) notFound"
    normalized
      `shouldSatisfy` T.isInfixOf
        "Persist.get $ ArkhamReplayAttestationKey gameId"
    normalized
      `shouldSatisfy` T.isInfixOf
        "makeReplayAttestation serverBuildIdentity (toPathPiece gameId) (gameGitRevision game.currentData) receipt"

readDebugSource :: IO Text
readDebugSource = go candidatePaths
 where
  candidatePaths :: [FilePath]
  candidatePaths =
    [ "library/Api/Handler/Arkham/Game/Debug.hs"
    , "backend/arkham-api/library/Api/Handler/Arkham/Game/Debug.hs"
    , "arkham-api/library/Api/Handler/Arkham/Game/Debug.hs"
    ]
  go [] = error $ "could not find Debug.hs under: " <> show candidatePaths
  go (path : rest) = do
    exists <- doesFileExist path
    if exists then T.readFile path else go rest
