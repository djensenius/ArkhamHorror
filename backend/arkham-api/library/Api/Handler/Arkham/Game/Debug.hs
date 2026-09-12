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

  -- * Exposed for regression tests
  makeReplayPlayerIdReplacement,
  makeReplayPlayerIdMap,
  makeReplayPlayerRemapping,
  checkpointInvestigatorPlayerId,
  remapReplayMessagePlayerIds,
  selectUploadedExportFile,
  validateReplayCheckpointPlayerId,
) where

import Api.Arkham.Export
import Api.Arkham.Helpers
import Api.Arkham.Types.Game (ClaimSeatPost (..))
import Api.Arkham.Types.MultiplayerVariant
import Api.Handler.Arkham.Games.Shared (withGameAccess)
import Arkham.Card.CardCode
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
import Control.Exception (evaluate)
import Data.Data (Data, cast, gmapT)
import Data.ByteString qualified as BS
import Data.ByteString.Lazy qualified as BSL
import Data.Map.Strict qualified as Map
import Data.Text qualified as T
import Data.Time.Clock
import Data.UUID qualified as UUID
import Database.Esqueleto.Experimental hiding (update)
import Database.Persist qualified as Persist
import Entity.Arkham.LogEntry
import Entity.Arkham.Player
import Entity.Arkham.ReplayAttestation
import Entity.Arkham.Step
import Import hiding (delete, exists, on, (==.))
import Json
import UnliftIO.Exception (catch, try)

normalizeJsonInvestigatorId :: Text -> Text
normalizeJsonInvestigatorId iid = if "c" `T.isPrefixOf` iid then iid else "c" <> iid

isGzipped :: BS.ByteString -> Bool
isGzipped bs = BS.take 2 bs == BS.pack [0x1f, 0x8b]

{- | Select the uploaded multipart export file, if any, without a partial
selector. A 'Nothing' result must short-circuit before any decoding is
attempted; the production handler wires this directly to an explicit
'invalidArgs' branch instead of 'Safe.fromJustNote'.
-}
selectUploadedExportFile :: [(Text, a)] -> Maybe a
selectUploadedExportFile = fmap snd . headMay

decodeExportBytes
  :: BS.ByteString
  -> Handler (Either String (ArkhamExport, Maybe ReplayImportAuthority))
decodeExportBytes bytes
  | isGzipped bytes = do
      eDecompressed <- liftIO $ try @_ @SomeException $ evaluate $ BSL.toStrict $ GZip.decompress $ BSL.fromStrict bytes
      pure $ case eDecompressed of
        Left err -> Left $ displayException err
        Right decompressed -> decodeReplayImport serverBuildIdentity decompressed
  | otherwise = pure $ decodeReplayImport serverBuildIdentity bytes

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
    lift $ runDB $ Persist.selectSourceRes [ArkhamStepArkhamGameId Persist.==. gameId] [Desc ArkhamStepStep]
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

postApiV1ArkhamGamesImportR :: Handler (PublicGame ArkhamGameId)
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
      selectedInvestigator <- case variant of
        Solo -> case headMay allInvestigatorIds of
          Nothing -> invalidArgs ["No investigators found in game data"]
          Just iid -> pure iid
        WithFriends -> case mInvestigatorId <|> headMay campaignInvestigatorIds of
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
      (importedGame, importReceipt) <- runDB $ do
        gameId <- insert $ ArkhamGame agedName agedCurrentData agedStep variant now now
        (playerRemappings, replayPlayerIds) <- case variant of
          Solo -> do
            newPlayerId <- insert $ ArkhamPlayer userId gameId selectedInvestigator
            playerRemappings <- case checkpointPlayerId of
              Nothing -> pure []
              Just originalPlayerId -> do
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
        let importedChoice s =
              s.choice
                { choiceMessages =
                    remapReplayMessagePlayerIds replayPlayerIds s.choice.choiceMessages
                }
        insertMany_ [ArkhamStep gameId (importedChoice s) s.step s.actionDiff | s <- agedSteps]

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
                serverBuildIdentity
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
      pure
        $ toPublicGame
          importedGame
          (GameLog $ map arkhamLogEntryBody agedLog)

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
  runDB do
    g <- get404 gameId
    when (g.multiplayerVariant /= WithFriends) do
      lift $ permissionDenied "This game is not a multiplayer game"
    let allInvestigators =
          map (normalizeJsonInvestigatorId . unCardCode . unInvestigatorId)
            $ gamePlayerOrder g.currentData
    unless (investigatorId `elem` allInvestigators) do
      lift $ invalidArgs ["Invalid investigator for this game"]
    mTaken <- selectOne do
      players <- from $ table @ArkhamPlayer
      where_ $ players.arkhamGameId ==. val gameId
      where_ $ players.investigatorId ==. val investigatorId
      pure players
    when (isJust mTaken) do
      lift $ permissionDenied "This seat is already taken"
    mAlreadyJoined <- getBy $ UniquePlayer userId gameId
    when (isJust mAlreadyJoined) do
      lift $ permissionDenied "You already have a seat in this game"
    newPlayerId <- insert $ ArkhamPlayer userId gameId investigatorId
    void $ remapInvestigatorUUID gameId investigatorId newPlayerId
