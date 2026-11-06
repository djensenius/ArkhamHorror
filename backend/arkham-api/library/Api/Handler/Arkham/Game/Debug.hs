module Api.Handler.Arkham.Game.Debug (
  getApiV1ArkhamGameExportR,
  getApiV1ArkhamGameFullExportR,
  getApiV1ArkhamGameScenarioExportR,
  postApiV1ArkhamGamesImportR,
  postApiV1ArkhamGamesFixR,
  getApiV1ArkhamGamesReloadR,
  getApiV1ArkhamGameReloadR,
  getApiV1ArkhamGameReplayAttestationR,
  getApiV1ArkhamGameOpenSeatsR,
  postApiV1ArkhamGameClaimSeatR,
  getApiV1ArkhamGameCustomCardsR,

  -- * Exposed for regression tests
  makeReplayPlayerIdReplacement,
  makeReplayPlayerIdMap,
  makeReplayPlayerRemapping,
  remapReplayActionDiffPlayerIds,
  checkpointInvestigatorPlayerId,
  remapReplayMessagePlayerIds,
  remapReplayPatchPlayerIds,
  selectUploadedExportFile,
  decompressReplayImport,
  tryImportDecode,
  validateReplayCheckpointPlayerId,
  checkpointInvestigatorForPlayerId,
) where

import Api.Arkham.Epic (
  GroupMembershipReservation (..),
  lockEpicEventRow,
  lookupGameEvent,
  reserveGroupMembership,
 )
import Api.Arkham.Export
import Api.Arkham.Helpers
import Api.Arkham.Types.Game (ClaimSeatPost (..))
import Api.Arkham.Types.MultiplayerVariant
import Api.Arkham.Viewer (RedactedGame (..), viewerForUser)
import Api.Handler.Arkham.CustomCards (forRendering, setPayloadEtag, userCustomCards)
import Api.Handler.Arkham.Games.Shared (broadcastEventChanged, withGameAccess)
import Arkham.Card.CardCode
import Arkham.Card.CardDef (cdCardCode)
import Arkham.Card.CustomCard
import Arkham.Classes.Entity (attr)
import Arkham.Entities (entitiesInvestigators)
import Arkham.Game
import Arkham.Id
import Arkham.Investigator.Types (investigatorPlayerId)
import Arkham.Message (Message)
import Arkham.Replay.ImportAuthority
import Arkham.Replay.ServerBuildIdentity (serverBuildIdentity)
import Codec.Compression.GZip qualified as GZip
import Conduit
import Control.Exception (SomeAsyncException, evaluate)
import Data.Aeson qualified as Aeson
import Data.Aeson.Key qualified as Key
import Data.Aeson.KeyMap qualified as KeyMap
import Data.Aeson.Patch qualified as Patch
import Data.Aeson.Pointer qualified as Pointer
import Data.Aeson.Types (parseMaybe)
import Data.Data (Data, cast, gmapT)
import Data.ByteString qualified as BS
import Data.ByteString.Lazy qualified as BSL
import Data.Map.Strict qualified as Map
import Data.Text qualified as T
import Data.Time.Clock
import Data.UUID qualified as UUID
import Database.Esqueleto.Experimental hiding (update)
import Database.Persist qualified as Persist
import Entity.Arkham.Player
import Entity.Arkham.ReplayAttestation
import Entity.Arkham.Step
import Import hiding (delete, exists, on, (==.))
import Json
import UnliftIO.Exception (catch, throwIO, try)

normalizeJsonInvestigatorId :: Text -> Text
normalizeJsonInvestigatorId iid = if "c" `T.isPrefixOf` iid then iid else "c" <> iid

isGzipped :: BS.ByteString -> Bool
isGzipped bs = BS.take 2 bs == BS.pack [0x1f, 0x8b]

maximumDecompressedReplayImportBytes :: Int
maximumDecompressedReplayImportBytes = 200 * 1024 * 1024

{- | Select the uploaded multipart export file, if any, without a partial
selector. A 'Nothing' result must short-circuit before any decoding is
attempted; the production handler wires this directly to an explicit
'invalidArgs' branch instead of 'Safe.fromJustNote'.
-}
selectUploadedExportFile :: [(Text, a)] -> Maybe a
selectUploadedExportFile = fmap snd . headMay

tryImportDecode :: IO a -> IO (Either String a)
tryImportDecode action =
  try @_ @SomeException action >>= \case
    Left err
      | isJust (fromException @SomeAsyncException err) -> throwIO err
      | otherwise -> pure $ Left $ displayException err
    Right value -> pure $ Right value

decompressReplayImport :: Int -> BS.ByteString -> IO (Either String BS.ByteString)
decompressReplayImport maximumBytes bytes
  | maximumBytes < 0 = pure $ Left "decompressed replay import limit must not be negative"
  | otherwise = do
      eDecompressed <-
        tryImportDecode $
          evaluate $
            BSL.toStrict $
              BSL.take (fromIntegral maximumBytes + 1) $
                GZip.decompress $
                  BSL.fromStrict bytes
      pure $ eDecompressed >>= \decompressed ->
        if BS.length decompressed > maximumBytes
          then
            Left $
              "decompressed replay import exceeds "
                <> show maximumBytes
                <> "-byte limit"
          else Right decompressed

-- Both halves report what actually went wrong: the import UI shows the string
-- straight to the player, and "this export is from a newer engine" and "this
-- file is not an export at all" are not the same problem.
decodeExportBytes
  :: BS.ByteString
  -> Handler (Either String (ArkhamExport, Maybe ReplayImportAuthority))
decodeExportBytes bytes
  | BS.null bytes = pure $ Left "The uploaded file was empty."
  | isGzipped bytes = do
      eDecompressed <-
        liftIO $
          decompressReplayImport maximumDecompressedReplayImportBytes bytes
      pure $
        first ("The compressed export could not be unpacked: " <>) eDecompressed
          >>= describeDecode . decodeReplayImport serverBuildIdentity
  | otherwise = pure $ describeDecode $ decodeReplayImport serverBuildIdentity bytes
 where
  describeDecode =
    first ("The export does not match what this version of the game can read: " <>)

-- Compress each emitted JSON chunk as its own gzip member. Gzip readers are
-- required to handle concatenated members, and this guarantees we keep sending
-- bytes throughout the export instead of buffering until the whole response is
-- compressed.
gzipConduit :: Monad m => ConduitT BS.ByteString BS.ByteString m ()
gzipConduit = awaitForever $ yield . BSL.toStrict . GZip.compress . BSL.fromStrict

generateFullExportSource :: ArkhamGameId -> ConduitT () BS.ByteString Handler ()
generateFullExportSource gameId = do
  (ge, players) <- lift $ runDB do
    ge <- get404 gameId
    players <- select do
      p <- from $ table @ArkhamPlayer
      where_ $ p.arkhamGameId ==. val gameId
      pure p
    pure (ge, players)
  let campaignPlayers = map (arkhamPlayerInvestigatorId . entityVal) players
  yieldBS "{\"campaignPlayers\":"
  yieldLBS $ encode campaignPlayers
  yieldBS ",\"campaignData\":{\"name\":"
  yieldLBS $ encode ge.name
  yieldBS ",\"currentData\":"
  yieldLBS $ encode ge.currentData
  yieldBS ",\"step\":"
  yieldLBS $ encode ge.step
  yieldBS ",\"steps\":["
  isFirstRef <- liftIO $ newIORef True
  stepsAcquire <-
    lift
      $ runDB
      $ Persist.selectSourceRes [ArkhamStepArkhamGameId Persist.==. gameId] [Desc ArkhamStepStep]
  (_, stepSource) <- allocateAcquire stepsAcquire
  stepSource .| awaitForever \(Entity _ s) -> do
    isFirst <- liftIO $ readIORef isFirstRef
    unless isFirst $ yieldBS ","
    liftIO $ writeIORef isFirstRef False
    yieldLBS $ encode s
  yieldBS "],\"log\":[],\"multiplayerVariant\":"
  yieldLBS $ encode ge.multiplayerVariant
  yieldBS "}}"
 where
  yieldBS = yield
  yieldLBS = mapM_ yield . BSL.toChunks

-- Swap the investigator's original player UUID for the new one by doing a
-- text-level replace on the stored JSONB, then casting back.
storedInvestigatorPlayerId :: ArkhamGameId -> Text -> DB (Maybe Text)
storedInvestigatorPlayerId gameId iCode = do
  results :: [Single (Maybe Text)] <-
    rawSql
      "SELECT current_data->'gameEntities'->'investigators'->?->>'playerId' \
      \FROM arkham_games WHERE id = ?"
      [PersistText iCode, PersistText (toPathPiece gameId)]
  pure $ join $ unSingle <$> headMay results

remapInvestigatorUUID :: ArkhamGameId -> Text -> ArkhamPlayerId -> DB (Maybe Text)
remapInvestigatorUUID gameId iCode newPlayerId = do
  let newUUID = toPathPiece newPlayerId
  storedInvestigatorPlayerId gameId iCode >>= \case
    Just origUUID -> do
      rawExecute
        "UPDATE arkham_games \
        \SET current_data = replace(current_data::text, ?, ?)::jsonb \
        \WHERE id = ?"
        [ PersistText ("\"" <> origUUID <> "\"")
        , PersistText ("\"" <> newUUID <> "\"")
        , PersistText (toPathPiece gameId)
        ]
      currentUUID <- storedInvestigatorPlayerId gameId iCode
      pure $ origUUID <$ guard (currentUUID == Just newUUID)
    Nothing -> pure Nothing

makeReplayPlayerRemapping
  :: Text
  -> Text
  -> Text
  -> Bool
  -> Either Text ReplayPlayerRemapping
makeReplayPlayerRemapping investigatorId checkpointPlayerId importedPlayerId stateRemapped = do
  checkpointUUID <-
    maybe
      (Left "Checkpoint investigator playerId is not a UUID")
      Right
      $ UUID.fromText checkpointPlayerId
  unless (isJust $ UUID.fromText importedPlayerId) $
    Left "Imported Arkham player ID is not a UUID"
  pure
    ReplayPlayerRemapping
      { replayPlayerInvestigatorId = investigatorId
      , replayPlayerCheckpointPlayerId = PlayerId checkpointUUID
      , replayPlayerImportedPlayerId = importedPlayerId
      , replayPlayerLivePlayerId =
          if stateRemapped then importedPlayerId else checkpointPlayerId
      , replayPlayerStateRemapped = stateRemapped
      }

validateReplayCheckpointPlayerId :: PlayerId -> Text -> Either Text ()
validateReplayCheckpointPlayerId expected checkpointPlayerId = do
  checkpointUUID <-
    maybe
      (Left "Checkpoint investigator playerId is not a UUID")
      Right
      $ UUID.fromText checkpointPlayerId
  unless (PlayerId checkpointUUID == expected) $
    Left "Selected investigator playerId does not match the replay checkpoint player"

checkpointInvestigatorPlayerId :: Game -> Text -> Either Text PlayerId
checkpointInvestigatorPlayerId game investigatorId =
  maybe
    (Left "Selected investigator is not present in replay checkpoint game data")
    (Right . attr investigatorPlayerId)
    $ Map.lookup
      (InvestigatorId $ CardCode $ T.dropWhile (== 'c') investigatorId)
      (entitiesInvestigators $ gameEntities game)

checkpointInvestigatorForPlayerId :: Game -> [Text] -> PlayerId -> Either Text (Text, Text)
checkpointInvestigatorForPlayerId _ [] _ = Left "No investigators found in game data"
checkpointInvestigatorForPlayerId game investigatorIds expectedPlayerId = do
  let matches =
        [ (investigatorId, UUID.toText $ unPlayerId playerId)
        | investigatorId <- investigatorIds
        , Right playerId <- [checkpointInvestigatorPlayerId game investigatorId]
        , playerId == expectedPlayerId
        ]
  case matches of
    [match] -> Right match
    [] -> Left "Replay checkpoint prompt playerId is not assigned to any imported investigator"
    _ -> Left "Replay checkpoint prompt playerId matches multiple imported investigators"

makeReplayPlayerIdMap
  :: [ReplayPlayerRemapping]
  -> Either Text (Map PlayerId PlayerId)
makeReplayPlayerIdMap remappings = do
  remappingPairs <- catMaybes <$> traverse toPair remappings
  let result = Map.fromList remappingPairs
  unless (Map.size result == length remappingPairs) $
    Left "Replay player remappings contain duplicate checkpoint player IDs"
  pure result
 where
  toPair mapping
    | not mapping.replayPlayerStateRemapped = Right Nothing
    | otherwise = do
        importedUUID <-
          maybe
            (Left "Remapped replay player ID is not a UUID")
            Right
            $ UUID.fromText mapping.replayPlayerImportedPlayerId
        unless
          (mapping.replayPlayerLivePlayerId == mapping.replayPlayerImportedPlayerId)
          $ Left "Remapped replay player live ID does not match the imported player"
        pure
          $ Just
            ( mapping.replayPlayerCheckpointPlayerId
            , PlayerId importedUUID
            )

makeReplayPlayerIdReplacement :: Text -> Text -> Either Text (Map PlayerId PlayerId)
makeReplayPlayerIdReplacement originalPlayerId importedPlayerId = do
  originalUUID <-
    maybe
      (Left "Original replay player ID is not a UUID")
      Right
      $ UUID.fromText originalPlayerId
  importedUUID <-
    maybe
      (Left "Imported Arkham player ID is not a UUID")
      Right
      $ UUID.fromText importedPlayerId
  pure $ Map.singleton (PlayerId originalUUID) (PlayerId importedUUID)

remapReplayMessagePlayerIds :: Map PlayerId PlayerId -> [Message] -> [Message]
remapReplayMessagePlayerIds replacements = map go
 where
  go :: Data value => value -> value
  go value = case cast value :: Maybe PlayerId of
    Just playerId ->
      maybe value (fromMaybe value . cast) $ Map.lookup playerId replacements
    Nothing -> gmapT go value

remapReplayPatchPlayerIds
  :: Map PlayerId PlayerId
  -> Patch.Patch
  -> Either Text Patch.Patch
remapReplayPatchPlayerIds replacements (Patch.Patch operations) =
  Patch.Patch <$> traverse remapOperation operations
 where
  replacementText =
    Map.fromList
      [ (UUID.toText $ unPlayerId source, UUID.toText $ unPlayerId destination)
      | (source, destination) <- Map.toList replacements
      ]

  remapText value = Map.findWithDefault value value replacementText

  remapKey = Key.fromText . remapText . Key.toText

  remapPointerKey = \case
    Pointer.OKey key -> Pointer.OKey $ remapKey key
    Pointer.AKey index -> Pointer.AKey index

  remapPointer (Pointer.Pointer keys) =
    Pointer.Pointer $ map remapPointerKey keys

  remapValue = \case
    String value -> Right $ String $ remapText value
    Array values -> Array <$> traverse remapValue values
    Object values -> do
      entries <-
        traverse
          (\(key, value) -> (remapKey key,) <$> remapValue value)
          (KeyMap.toList values)
      let remapped = KeyMap.fromList entries
      unless (KeyMap.size remapped == length entries) $
        Left "Replay player remapping collapses JSON object keys"
      Right $ Object remapped
    value -> Right value

  remapOperation = \case
    Patch.Add pointer value ->
      Patch.Add (remapPointer pointer) <$> remapValue value
    Patch.Cpy pointer source ->
      Right $ Patch.Cpy (remapPointer pointer) (remapPointer source)
    Patch.Mov pointer source ->
      Right $ Patch.Mov (remapPointer pointer) (remapPointer source)
    Patch.Rem pointer ->
      Right $ Patch.Rem $ remapPointer pointer
    Patch.Rep pointer value ->
      Patch.Rep (remapPointer pointer) <$> remapValue value
    Patch.Tst pointer value ->
      Patch.Tst (remapPointer pointer) <$> remapValue value

remapReplayActionDiffPlayerIds
  :: Map PlayerId PlayerId
  -> ActionDiff
  -> Either Text ActionDiff
remapReplayActionDiffPlayerIds replacements (ActionDiff patches) =
  ActionDiff <$> traverse (remapReplayPatchPlayerIds replacements) patches

getApiV1ArkhamGameExportR :: ArkhamGameId -> Handler ArkhamExport
getApiV1ArkhamGameExportR gameId = do
  Entity userId user <- getRequestUser
  withGameAccess
    user.admin
    (isJust <$> runDB (getBy $ UniquePlayer userId gameId))
    notFound
    (generateExport gameId 30)

getApiV1ArkhamGameScenarioExportR :: ArkhamGameId -> Handler ArkhamExport
getApiV1ArkhamGameScenarioExportR gameId = do
  Entity userId user <- getRequestUser
  withGameAccess
    user.admin
    (isJust <$> runDB (getBy $ UniquePlayer userId gameId))
    notFound
    (generateScenarioExport gameId)

getApiV1ArkhamGameFullExportR :: ArkhamGameId -> Handler TypedContent
getApiV1ArkhamGameFullExportR gameId = do
  gzip <- (== Just "true") <$> lookupGetParam "gzip"
  if gzip
    then do
      addHeader "Content-Disposition" $ "attachment; filename=arkham-full-export-" <> toPathPiece gameId <> ".json.gz"
      respondSource "application/gzip" $
        generateFullExportSource gameId .| gzipConduit .| awaitForever \chunk -> sendChunkBS chunk >> sendFlush
    else do
      addHeader "Content-Disposition" $ "attachment; filename=arkham-full-export-" <> toPathPiece gameId <> ".json"
      respondSource "application/json" $
        generateFullExportSource gameId .| awaitForever \chunk -> sendChunkBS chunk >> sendFlush

postApiV1ArkhamGamesFixR :: Handler ()
postApiV1ArkhamGamesFixR = do
  gameIds <- runDB $ selectKeysList @ArkhamGame [] []
  for_ gameIds \gameId -> do
    let handleBrokenGame :: SomeException -> Handler ()
        handleBrokenGame _ = void $ runDB (Persist.delete gameId)
    void (runDB (Persist.get gameId) :: Handler (Maybe ArkhamGame)) `catch` handleBrokenGame

getApiV1ArkhamGamesReloadR :: Handler ()
getApiV1ArkhamGamesReloadR = do
  gameIds <- runDB $ selectKeysList @ArkhamGame [] []
  for_ gameIds \gameId -> do
    try @_ @SomeException (runDB $ Persist.get gameId >>= traverse_ (Persist.replace gameId))

  stepIds <- runDB $ selectKeysList @ArkhamStep [] []
  for_ stepIds \stepId -> do
    try @_ @SomeException (runDB $ Persist.get stepId >>= traverse_ (Persist.replace stepId))

getApiV1ArkhamGameReloadR :: ArkhamGameId -> Handler ()
getApiV1ArkhamGameReloadR gameId = do
  _ <- try @_ @SomeException (runDB $ Persist.get gameId >>= traverse_ (Persist.replace gameId))

  stepIds <- runDB $ selectKeysList @ArkhamStep [ArkhamStepArkhamGameId Persist.==. gameId] []
  for_ stepIds \stepId -> do
    try @_ @SomeException (runDB $ Persist.get stepId >>= traverse_ (Persist.replace stepId))

{- | Import an export as a new game. The response is redacted for the
importer's seats like any other read ('viewerForUser'): the importer holds
every seat row the import inserts, so in practice they see the whole game.
-}
postApiV1ArkhamGamesImportR :: Handler (RedactedGame (PublicGame ArkhamGameId))
postApiV1ArkhamGamesImportR = do
  userId <- getRequestUserId
  mVariantOverride <- lookupGetParam "multiplayerVariant"
  (params, files) <- runRequestBody
  let
    mInvestigatorId = fmap normalizeJsonInvestigatorId $ snd <$> find ((== "investigatorId") . fst) params
  -- An explicit branch, rather than a partial selector, so a request with no
  -- multipart file gets a stable actionable 400 instead of a 500.
  uploadedFile <- case selectUploadedExportFile files of
    Nothing -> invalidArgs ["No export file uploaded"]
    Just fi -> pure fi
  eExportData :: Either String (ArkhamExport, Maybe ReplayImportAuthority) <-
    decodeExportBytes =<< fileSourceByteString uploadedFile
  now <- liftIO getCurrentTime

  case eExportData of
    Left err -> invalidArgs [T.pack err]
    Right (export, importAuthority) -> do
      when (isJust importAuthority && isJust mVariantOverride) $
        invalidArgs ["Replay checkpoint imports do not allow multiplayerVariant overrides"]
      let
        ArkhamGameExportData {..} = aeCampaignData export
        exportVariant = agedMultiplayerVariant
        variant = case mVariantOverride of
          Just "WithFriends" -> WithFriends
          Just "Solo" -> Solo
          _ -> exportVariant
        allInvestigatorIds =
          map normalizeJsonInvestigatorId
            $ maybe [] toList
            $ asum
              [ nonEmpty (map (unCardCode . unInvestigatorId) (gamePlayerOrder agedCurrentData))
              , nonEmpty (aeCampaignPlayers export)
              ]
        campaignInvestigatorIds = map normalizeJsonInvestigatorId $ aeCampaignPlayers export
      (selectedInvestigator, checkpointPlayerId) <- case variant of
        Solo -> case importAuthority of
          Just authority -> do
            (investigatorId, playerId) <-
              either
                (invalidArgs . pure)
                pure
                $ checkpointInvestigatorForPlayerId
                  agedCurrentData
                  allInvestigatorIds
                  (replayImportCheckpointPlayerId authority)
            pure (investigatorId, Just playerId)
          Nothing -> case headMay allInvestigatorIds of
            Nothing -> invalidArgs ["No investigators found in game data"]
            Just iid -> pure (iid, Nothing)
        WithFriends -> do
          selectedInvestigator <- case mInvestigatorId <|> headMay campaignInvestigatorIds of
            Nothing -> invalidArgs ["No investigator specified"]
            Just iid -> pure iid
          checkpointPlayerId <- forM importAuthority \authority -> do
            playerId <-
              either
                (invalidArgs . pure)
                pure
                $ checkpointInvestigatorPlayerId agedCurrentData selectedInvestigator
            let checkpointPlayerId = UUID.toText $ unPlayerId playerId
            either
              (invalidArgs . pure)
              pure
              $ validateReplayCheckpointPlayerId
                (replayImportCheckpointPlayerId authority)
                checkpointPlayerId
            pure checkpointPlayerId
          pure (selectedInvestigator, checkpointPlayerId)
      (importedGame, importReceipt) <- runDB $ do
        gameId <- insert $ ArkhamGame agedName agedCurrentData agedStep variant now now False
        (playerRemappings, replayPlayerIds) <- case variant of
          Solo -> do
            newPlayerIds <-
              forM allInvestigatorIds \investigatorId -> do
                playerId <- insert $ ArkhamPlayer userId gameId investigatorId
                pure (investigatorId, playerId)
            playerRemappings <- case checkpointPlayerId of
              Nothing -> pure []
              Just originalPlayerId -> do
                newPlayerId <-
                  maybe
                    (lift $ invalidArgs ["Replay checkpoint investigator was not inserted"])
                    pure
                    $ snd <$> find ((== selectedInvestigator) . fst) newPlayerIds
                mapping <-
                  either
                    (lift . invalidArgs . pure)
                    pure
                    $ makeReplayPlayerRemapping
                      selectedInvestigator
                      originalPlayerId
                      (toPathPiece newPlayerId)
                      False
                pure [mapping]
            replayPlayerIds <-
              either
                (lift . invalidArgs . pure)
                pure
                $ makeReplayPlayerIdMap playerRemappings
            pure (playerRemappings, replayPlayerIds)
          WithFriends -> do
            newPlayerId <- insert $ ArkhamPlayer userId gameId selectedInvestigator
            mRemappedPlayerId <-
              remapInvestigatorUUID gameId selectedInvestigator newPlayerId
            remappedFromPlayerId <-
              maybe
                (lift $ invalidArgs ["Imported investigator playerId remapping failed"])
                pure
                mRemappedPlayerId
            replayPlayerIds <-
              either
                (lift . invalidArgs . pure)
                pure
                $ makeReplayPlayerIdReplacement
                  remappedFromPlayerId
                  (toPathPiece newPlayerId)
            playerRemappings <- case checkpointPlayerId of
              Nothing -> pure []
              Just originalPlayerId -> do
                unless (remappedFromPlayerId == originalPlayerId) $
                  lift $ invalidArgs ["Replay checkpoint investigator playerId changed during import"]
                mapping <-
                  either
                    (lift . invalidArgs . pure)
                    pure
                    $ makeReplayPlayerRemapping
                      selectedInvestigator
                      originalPlayerId
                      (toPathPiece newPlayerId)
                      True
                pure [mapping]
            pure (playerRemappings, replayPlayerIds)
        rawExecute
          "DO $$ \
          \BEGIN \
          \  IF EXISTS ( \
          \    SELECT 1 \
          \    FROM pg_trigger t \
          \    JOIN pg_class c ON c.oid = t.tgrelid \
          \    JOIN pg_namespace n ON n.oid = c.relnamespace \
          \    WHERE t.tgname = 'enforce_step_order_per_game' \
          \      AND c.relname = 'arkham_steps' \
          \      AND n.nspname = 'public' \
          \  ) THEN \
          \    EXECUTE 'ALTER TABLE public.arkham_steps DISABLE TRIGGER enforce_step_order_per_game'; \
          \  END IF; \
          \END$$;"
          []
        importedSteps <- forM agedSteps \s -> do
          importedPatch <-
            either
              (lift . invalidArgs . pure)
              pure
              $ remapReplayPatchPlayerIds
                replayPlayerIds
                s.choice.choicePatchDown
          importedActionDiff <-
            either
              (lift . invalidArgs . pure)
              pure
              $ remapReplayActionDiffPlayerIds replayPlayerIds s.actionDiff
          let
            importedChoice =
              s.choice
                { choicePatchDown = importedPatch
                , choiceMessages =
                    remapReplayMessagePlayerIds replayPlayerIds s.choice.choiceMessages
                }
          pure $ ArkhamStep gameId importedChoice s.step importedActionDiff
        insertMany_ importedSteps

        rawExecute
          "DO $$ \
          \BEGIN \
          \  IF EXISTS ( \
          \    SELECT 1 \
          \    FROM pg_trigger t \
          \    JOIN pg_class c ON c.oid = t.tgrelid \
          \    JOIN pg_namespace n ON n.oid = c.relnamespace \
          \    WHERE t.tgname = 'enforce_step_order_per_game' \
          \      AND c.relname = 'arkham_steps' \
          \      AND n.nspname = 'public' \
          \  ) THEN \
          \    EXECUTE 'ALTER TABLE public.arkham_steps ENABLE TRIGGER enforce_step_order_per_game'; \
          \  END IF; \
          \END$$;"
          []
        let importReceipt =
              makeReplayImportReceipt
                (toPathPiece gameId)
                playerRemappings
                <$> importAuthority
        for_ importReceipt \receipt -> do
          either
            (lift . invalidArgs . pure . T.pack)
            pure
            $ validateReplayImportReceipt receipt
          insertKey
            (ArkhamReplayAttestationKey gameId)
            (ArkhamReplayAttestation $ toJSON receipt)
        game <- get404 gameId
        pure (Entity gameId game, importReceipt)
      traverse_
        (uncurry addHeader)
        (replayImportResponseHeaders serverBuildIdentity importReceipt)
      -- Read after the import committed, so it sees the inserted seat rows.
      gameView <- runDB $ viewerForUser userId (entityKey importedGame)
      pure
        $ RedactedGame gameView
        $ toPublicGame
          importedGame
          -- An import's rows come straight from the export, so hand them over
          -- the same way a live read would: structured where the row has a
          -- payload, legacy text otherwise.
          (GameLog $ map toLogRow agedLog)

getApiV1ArkhamGameReplayAttestationR
  :: ArkhamGameId
  -> Handler ReplayAttestation
getApiV1ArkhamGameReplayAttestationR gameId = do
  Entity userId user <- getRequestUser
  withGameAccess
    user.admin
    (isJust <$> runDB (getBy $ UniquePlayer userId gameId))
    notFound
    do
      (game, storedAttestation) <- runDB do
        game <- get404 gameId
        storedAttestation <- Persist.get $ ArkhamReplayAttestationKey gameId
        pure (game, storedAttestation)
      ArkhamReplayAttestation receiptValue <-
        maybe notFound pure storedAttestation
      receipt <- case fromJSON receiptValue of
        Error err ->
          invalidArgs ["Stored replay attestation is invalid: " <> T.pack err]
        Success value -> pure value
      either
        (invalidArgs . pure . ("Replay attestation is unavailable: " <>) . T.pack)
        pure
        $ makeReplayAttestation
          serverBuildIdentity
          (toPathPiece gameId)
          (gameGitRevision game.currentData)
          receipt

getApiV1ArkhamGameOpenSeatsR :: ArkhamGameId -> Handler [Text]
getApiV1ArkhamGameOpenSeatsR gameId = do
  _ <- getRequestUserId
  runDB do
    g <- get404 gameId
    let allInvestigators =
          map (normalizeJsonInvestigatorId . unCardCode . unInvestigatorId) $ gamePlayerOrder g.currentData
    assignedInvestigators <-
      map unValue <$> select do
        players <- from $ table @ArkhamPlayer
        where_ $ players.arkhamGameId ==. val gameId
        pure players.investigatorId
    pure $ filter (`notElem` assignedInvestigators) allInvestigators

postApiV1ArkhamGameClaimSeatR :: ArkhamGameId -> Handler ()
postApiV1ArkhamGameClaimSeatR gameId = do
  userId <- getRequestUserId
  ClaimSeatPost {investigatorId = rawId} <- requireCheckJsonBody
  let investigatorId = normalizeJsonInvestigatorId rawId
      -- Seat rows are written in two formats: deck selection stores the bare
      -- code ("01001"), claim seat and import the 'c'-prefixed one ("c01001").
      -- Compare both ends stripped, as 'Shared' does for achievements, so a
      -- deck-selected seat still counts as taken. Strip at most one 'c', so a
      -- malformed code such as "cc01001" still matches no seat.
      seatCode t = fromMaybe t (T.stripPrefix "c" t)
  mReservedEvent <- runDB do
    -- Lock the game first, so concurrent claims (and joins) for this game run
    -- one at a time and every check below sees the committed seats.
    lockGame gameId
    g <- get404 gameId
    when (g.multiplayerVariant /= WithFriends) do
      lift $ permissionDenied "This game is not a multiplayer game"
    let allSeats =
          map (seatCode . unCardCode . unInvestigatorId)
            $ gamePlayerOrder g.currentData
    unless (seatCode investigatorId `elem` allSeats) do
      lift $ invalidArgs ["Invalid investigator for this game"]
    takenSeats <-
      map unValue <$> select do
        players <- from $ table @ArkhamPlayer
        where_ $ players.arkhamGameId ==. val gameId
        pure players.investigatorId
    when (seatCode investigatorId `elem` map seatCode takenSeats) do
      lift $ permissionDenied "This seat is already taken"
    mAlreadyJoined <- getBy $ UniquePlayer userId gameId
    when (isJust mAlreadyJoined) do
      lift $ permissionDenied "You already have a seat in this game"
    -- Epic Multiplayer: a player may occupy seats in only one group of an
    -- event, so the seat reserves the same membership a pending join would.
    -- The event row is locked FOR UPDATE first, as the pending join does, so
    -- both keep the game -> event -> member order. Otherwise this insert's
    -- foreign-key KEY SHARE waits behind a join's event lock while that join
    -- waits on this uncommitted membership row. An event row that is gone
    -- (only a delete bypassing 'canonicalEpicGameLockOrder' can do that) is
    -- answered 404, like the pending join.
    mEvent <- lookupGameEvent gameId
    mReserved <- forM mEvent \(eventEntity, ordinal) -> do
      let eventId = entityKey eventEntity
      whenNothingM_ (lockEpicEventRow eventId) $ lift notFound
      reserveGroupMembership eventId userId ordinal >>= \case
        GroupMembershipInserted -> pure (Just eventId)
        GroupMembershipExisting -> pure Nothing
        GroupMembershipInOtherGroup _ ->
          lift $ permissionDenied "You already occupy a seat in another group in this event"
    newPlayerId <- insert $ ArkhamPlayer userId gameId investigatorId
    void $ remapInvestigatorUUID gameId investigatorId newPlayerId
    pure $ join mReserved
  for_ mReservedEvent broadcastEventChanged


{- | The debug-authored cards defined in this game.

Served on its own rather than folded into the game payload: a custom card
carries its art inline (a data URI for a dropped image), which has no business
riding every websocket update.

Read through @gameCustomCards@ rather than by loading the game. @current_data@
decodes to a whole 'Arkham.Game.Game' -- every location, asset and card, 97kB of
JSON and several times that for a late campaign -- and this wants one key of it,
which Postgres can hand over on its own.

Stripped to 'forRendering', because the script on a custom card is read by the
engine out of the library and by the card builder out of its own request, and by
nothing here. 'setPayloadEtag' then lets a reader that already has these cards
be told so, which is what makes moving between pages free rather than another
copy of the table's whole card set.
-}
getApiV1ArkhamGameCustomCardsR :: ArkhamGameId -> Handler [CustomCard]
getApiV1ArkhamGameCustomCardsR gameId = do
  userId <- getRequestUserId
  recorded <- runDB do
    rows :: [Single Aeson.Value] <-
      rawSql
        "SELECT COALESCE(current_data -> 'gameCustomCards', '{}'::jsonb) FROM arkham_games WHERE id = ?"
        [toPersistValue gameId]
    case rows of
      [] -> notFound
      Single raw : _ -> pure $ fromMaybe mempty $ parseMaybe parseJSON raw
  -- Your own library wins over the copy recorded on the game, so editing a card
  -- shows through at the table without having to re-add it.
  library <- userCustomCards userId
  {- Re-keyed by each card's own code before being listed, because the library is
  keyed by every code a card answers to -- its own and the arkham.build aliases
  'userCustomCards' adds underneath -- and listing it as it stands would send an
  aliased card once per alias. The reader derives the aliases again from the def
  it is given, so they were never worth the bytes. -}
  let
    deduped = Map.fromList [(cdCardCode (customCardDef c), c) | c <- toList (library <> recorded)]
    payload = map forRendering (toList deduped)
  setPayloadEtag payload
  pure payload
