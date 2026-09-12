module Arkham.Api.GameImportSpec (spec) where

import Api.Handler.Arkham.Game.Debug
  ( makeReplayPlayerIdMap
  , makeReplayPlayerIdReplacement
  , makeReplayPlayerRemapping
  , remapReplayActionDiffPlayerIds
  , remapReplayMessagePlayerIds
  , remapReplayPatchPlayerIds
  , selectUploadedExportFile
  , tryImportDecode
  , validateReplayCheckpointPlayerId
  )
import Arkham.Game.Diff (patchValueWithRecovery)
import Arkham.Id (PlayerId (..))
import Arkham.Message (Message (..))
import Arkham.Prelude
import Arkham.Question (Question (..))
import Arkham.Replay.ImportAuthority
import Control.Exception qualified as E
import Data.Aeson (Result (..))
import Data.Aeson.Key qualified as Key
import Data.Aeson.KeyMap qualified as KeyMap
import Data.Aeson.Patch (Operation (..), Patch (..))
import Data.Aeson.Pointer (Key (..), Pointer (..))
import Data.Either (isLeft)
import Data.List qualified as List
import Data.Map.Strict qualified as Map
import Data.Text qualified as T
import Data.Text.IO qualified as T
import Data.UUID qualified as UUID
import Entity.Arkham.Step (ActionDiff (..))
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

  describe "tryImportDecode" do
    it "converts synchronous decoding failures into import errors" do
      result <- tryImportDecode (E.throwIO (userError "invalid gzip") :: IO ())
      result
        `shouldSatisfy` \case
          Left err -> "invalid gzip" `List.isInfixOf` err
          Right () -> False

    it "rethrows asynchronous cancellation instead of returning a normal import error" do
      result <-
        E.try @E.AsyncException $
          tryImportDecode (E.throwIO E.ThreadKilled :: IO ())
      result
        `shouldSatisfy` \case
          Left E.ThreadKilled -> True
          _ -> False

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

    it "binds the selected investigator to the checkpoint prompt player" do
      validateReplayCheckpointPlayerId
        (PlayerId checkpointUUID)
        checkpointPlayerId
        `shouldBe` Right ()
      validateReplayCheckpointPlayerId
        (PlayerId $ UUID.fromWords 0 0 0 1)
        checkpointPlayerId
        `shouldSatisfy` isLeft
      validateReplayCheckpointPlayerId
        (PlayerId checkpointUUID)
        "not-a-uuid"
        `shouldSatisfy` isLeft

    it "builds only complete persisted-state remappings and rejects malformed live IDs" do
      let importedUUID = UUID.fromWords 0 0 0 2
          remapped =
            ReplayPlayerRemapping
              { replayPlayerInvestigatorId = "c01001"
              , replayPlayerCheckpointPlayerId = PlayerId checkpointUUID
              , replayPlayerImportedPlayerId = importedPlayerId
              , replayPlayerLivePlayerId = importedPlayerId
              , replayPlayerStateRemapped = True
              }
          unremapped =
            remapped
              { replayPlayerLivePlayerId = checkpointPlayerId
              , replayPlayerStateRemapped = False
              }
      makeReplayPlayerIdMap [remapped]
        `shouldBe` Right
          (Map.singleton (PlayerId checkpointUUID) (PlayerId importedUUID))
      makeReplayPlayerIdMap [unremapped] `shouldBe` Right Map.empty
      makeReplayPlayerIdMap
        [ remapped
        , remapped {replayPlayerInvestigatorId = "c02001"}
        ]
        `shouldSatisfy` isLeft
      makeReplayPlayerIdMap
        [ remapped
            { replayPlayerImportedPlayerId = "not-a-uuid"
            , replayPlayerLivePlayerId = "not-a-uuid"
            }
        ]
        `shouldSatisfy` isLeft
      makeReplayPlayerIdMap
        [remapped {replayPlayerLivePlayerId = "not-a-uuid"}]
        `shouldSatisfy` isLeft

  describe "remapReplayMessagePlayerIds" do
    let checkpointPlayerId = PlayerId UUID.nil
        importedPlayerId = PlayerId $ UUID.fromWords 0 0 0 2
        unrelatedPlayerId = PlayerId $ UUID.fromWords 0 0 0 3
        question = ChooseOne []
        replacements = Map.singleton checkpointPlayerId importedPlayerId

    it "remaps direct and nested player references in retained queue messages" do
      remapReplayMessagePlayerIds
        replacements
        [ SetActivePlayer checkpointPlayerId
        , Ask checkpointPlayerId question
        , Run [SetActivePlayer checkpointPlayerId, SetActivePlayer unrelatedPlayerId]
        ]
        `shouldBe` [ SetActivePlayer importedPlayerId
                   , Ask importedPlayerId question
                   , Run [SetActivePlayer importedPlayerId, SetActivePlayer unrelatedPlayerId]
                   ]

    it "remaps player IDs used as keys while preserving unrelated seats" do
      remapReplayMessagePlayerIds
        replacements
        [ AskMap $
            Map.fromList
              [ (checkpointPlayerId, question)
              , (unrelatedPlayerId, question)
              ]
        ]
        `shouldBe` [ AskMap $
                       Map.fromList
                         [ (importedPlayerId, question)
                         , (unrelatedPlayerId, question)
                         ]
                   ]

    it "remaps ordinary multiplayer retained prompts from the imported state replacement" do
      let originalPlayerId = "00000000-0000-0000-0000-000000000000"
          importedPlayerIdText = "00000000-0000-0000-0000-000000000002"
      ordinaryReplacements <- case makeReplayPlayerIdReplacement originalPlayerId importedPlayerIdText of
        Left err -> expectationFailure (T.unpack err) >> error "invalid player replacement"
        Right value -> pure value
      remapReplayMessagePlayerIds
        ordinaryReplacements
        [ Ask checkpointPlayerId question
        , AskMap $ Map.singleton checkpointPlayerId question
        ]
        `shouldBe` [ Ask importedPlayerId question
                   , AskMap $ Map.singleton importedPlayerId question
                   ]
      makeReplayPlayerIdReplacement "not-a-uuid" importedPlayerIdText
        `shouldSatisfy` isLeft
      makeReplayPlayerIdReplacement originalPlayerId "not-a-uuid"
        `shouldSatisfy` isLeft

  describe "remapReplayPatchPlayerIds" do
    let checkpointPlayerId = PlayerId UUID.nil
        importedPlayerId = PlayerId $ UUID.fromWords 0 0 0 2
        unrelatedPlayerId = PlayerId $ UUID.fromWords 0 0 0 3
        checkpointText = UUID.toText $ unPlayerId checkpointPlayerId
        importedText = UUID.toText $ unPlayerId importedPlayerId
        unrelatedText = UUID.toText $ unPlayerId unrelatedPlayerId
        replacements = Map.singleton checkpointPlayerId importedPlayerId
        retainedUndoPatch =
          Patch
            [ Rep
                (Pointer [OKey "gameActivePlayerId"])
                (String checkpointText)
            , Rep
                ( Pointer
                    [ OKey "gameQuestion"
                    , OKey $ Key.fromText checkpointText
                    , OKey "playerId"
                    ]
                )
                (String checkpointText)
            , Rep
                (Pointer [OKey "owners"])
                ( Object
                    $ KeyMap.singleton
                      (Key.fromText checkpointText)
                      (String checkpointText)
                )
            ]
        importedCurrent =
          object
            [ "gameActivePlayerId" .= unrelatedText
            , "gameQuestion"
                .= object
                  [ Key.fromText importedText
                      .= object ["playerId" .= unrelatedText]
                  ]
            , "owners" .= object []
            ]
        expectedUndoState =
          object
            [ "gameActivePlayerId" .= importedText
            , "gameQuestion"
                .= object
                  [ Key.fromText importedText
                      .= object ["playerId" .= importedText]
                  ]
            , "owners"
                .= Object
                  ( KeyMap.singleton
                      (Key.fromText importedText)
                      (String importedText)
                  )
            ]

    it "keeps retained undo patches aligned with a multiplayer player remap" do
      remappedPatch <- case remapReplayPatchPlayerIds replacements retainedUndoPatch of
        Left err -> expectationFailure (T.unpack err) >> error "patch remapping failed"
        Right value -> pure value
      patchValueWithRecovery importedCurrent remappedPatch
        `shouldBe` Success expectedUndoState

      case remapReplayActionDiffPlayerIds replacements (ActionDiff [retainedUndoPatch]) of
        Left err -> expectationFailure (T.unpack err)
        Right (ActionDiff [remappedActionPatch]) ->
          patchValueWithRecovery importedCurrent remappedActionPatch
            `shouldBe` Success expectedUndoState
        Right _ -> expectationFailure "expected one remapped action patch"

    it "fails closed when remapping would collapse JSON object keys" do
      let collisionPatch =
            Patch
              [ Rep
                  (Pointer [OKey "owners"])
                  ( Object
                      $ KeyMap.fromList
                        [ (Key.fromText checkpointText, toJSON (0 :: Int))
                        , (Key.fromText importedText, toJSON (1 :: Int))
                        ]
                  )
              ]
      remapReplayPatchPlayerIds replacements collisionPatch
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
        checkpointPlayerBindingNeedle =
          "validateReplayCheckpointPlayerId (replayImportCheckpointPlayerId authority) checkpointPlayerId"
        checkpointPlayerBindingPositions =
          map (T.length . fst) $ T.breakOnAll checkpointPlayerBindingNeedle importHandler
    normalized
      `shouldSatisfy` T.isInfixOf
        "postApiV1ArkhamGamesImportR :: Handler (PublicGame ArkhamGameId)"
    decodePosition <- position "decodeExportBytes"
    checkpointBindingPosition <-
      position
        "checkpointPlayerId <- forM importAuthority \\authority -> do"
    transactionPosition <- position "(importedGame, importReceipt) <- runDB"
    investigatorRemapPosition <-
      position
        "mRemappedPlayerId <- remapInvestigatorUUID gameId selectedInvestigator newPlayerId"
    ordinaryQueueMapPosition <-
      position
        "$ makeReplayPlayerIdReplacement remappedFromPlayerId (toPathPiece newPlayerId)"
    patchRemapPosition <-
      position
        "$ remapReplayPatchPlayerIds replayPlayerIds s.choice.choicePatchDown"
    actionDiffRemapPosition <-
      position
        "$ remapReplayActionDiffPlayerIds replayPlayerIds s.actionDiff"
    queueRemapPosition <-
      position
        "choiceMessages = remapReplayMessagePlayerIds replayPlayerIds s.choice.choiceMessages"
    stepInsertPosition <-
      position
        "insertMany_ importedSteps"
    headerPosition <-
      position
        "replayImportResponseHeaders serverBuildIdentity importReceipt"
    publicGamePosition <- position "$ toPublicGame importedGame"
    decodePosition `shouldSatisfy` (< transactionPosition)
    checkpointBindingPosition `shouldSatisfy` (< transactionPosition)
    case checkpointPlayerBindingPositions of
      [bindingPosition] -> bindingPosition `shouldSatisfy` (< transactionPosition)
      _ -> expectationFailure "expected one pre-transaction checkpoint-player validation"
    transactionPosition `shouldSatisfy` (< investigatorRemapPosition)
    investigatorRemapPosition `shouldSatisfy` (< ordinaryQueueMapPosition)
    ordinaryQueueMapPosition `shouldSatisfy` (< patchRemapPosition)
    patchRemapPosition `shouldSatisfy` (< actionDiffRemapPosition)
    actionDiffRemapPosition `shouldSatisfy` (< queueRemapPosition)
    queueRemapPosition `shouldSatisfy` (< stepInsertPosition)
    headerPosition `shouldSatisfy` (< publicGamePosition)
    importHandler
      `shouldSatisfy` T.isInfixOf
        "for_ importReceipt \\receipt -> do"
    importHandler
      `shouldSatisfy` T.isInfixOf
        "traverse_ (uncurry addHeader) (replayImportResponseHeaders serverBuildIdentity importReceipt)"
    importHandler
      `shouldSatisfy` T.isInfixOf
        "game <- get404 gameId pure (Entity gameId game, importReceipt)"
    importHandler
      `shouldSatisfy` T.isInfixOf
        "$ toPublicGame importedGame"

  it "governs the full import body, authority headers, and fail-closed checkpoint build" do
    source <- readOpenApiSource
    let normalized = T.unwords $ T.words source
        importAndRest = snd $ T.breakOn "/arkham/games/import:" normalized
        importContract =
          fst $ T.breakOn "/arkham/games/{gameId}:" importAndRest
    importContract
      `shouldSatisfy` T.isInfixOf
        "body is the complete `PublicGame` snapshot"
    importContract
      `shouldSatisfy` T.isInfixOf
        "$ref: \"#/components/schemas/PublicGame\""
    importContract
      `shouldSatisfy` T.isInfixOf
        "X-Arkham-Backend-Build-Identity: required: true"
    importContract
      `shouldSatisfy` T.isInfixOf
        "$ref: \"./schemas/replay-attestation.schema.json#/$defs/buildIdentity\""
    importContract
      `shouldSatisfy` T.isInfixOf
        "X-Arkham-Replay-Import-Receipt:"
    importContract
      `shouldSatisfy` T.isInfixOf
        "present only when the uploaded bytes were accepted as a validated replay checkpoint"
    importContract
      `shouldSatisfy` T.isInfixOf
        "`sourceClean: true` and `attestation: \"git-clean\"`"
    importContract
      `shouldSatisfy` T.isInfixOf
        "Dirty, `source-sha256`-attested, unattested, mismatched, or tampered checkpoints fail with 400 before game creation"

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

readOpenApiSource :: IO Text
readOpenApiSource = go candidatePaths
 where
  candidatePaths :: [FilePath]
  candidatePaths =
    [ "contracts/openapi.yaml"
    , "../contracts/openapi.yaml"
    , "../../contracts/openapi.yaml"
    ]
  go [] = error $ "could not find contracts/openapi.yaml under: " <> show candidatePaths
  go (path : rest) = do
    exists <- doesFileExist path
    if exists then T.readFile path else go rest
