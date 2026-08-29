module Api.Handler.Arkham.Game.Debug (
  getApiV1ArkhamGameExportR,
  getApiV1ArkhamGameFullExportR,
  getApiV1ArkhamGameScenarioExportR,
  postApiV1ArkhamGamesImportR,
  postApiV1ArkhamGamesFixR,
  getApiV1ArkhamGamesReloadR,
  getApiV1ArkhamGameReloadR,
  getApiV1ArkhamGameOpenSeatsR,
  postApiV1ArkhamGameClaimSeatR,
  ClaimSeatFailure (..),
  ClaimSeatOutcome (..),
  MonadClaimSeat (..),
  planAndExecuteClaimSeat,
) where

import Api.Arkham.Epic (EpicGroupReservation (..), lockEpicEventRow, lookupGameEvent, reserveEpicGroupMembership)
import Api.Arkham.Export
import Api.Arkham.Helpers
import Api.Arkham.Types.Game (ClaimSeatPost (..))
import Api.Arkham.Types.MultiplayerVariant
import Api.Handler.Arkham.Games.Shared (withGameAccess)
import Arkham.Card.CardCode
import Arkham.Epic.Types (GroupOrdinal (..))
import Arkham.Game
import Arkham.Id
import Codec.Compression.GZip qualified as GZip
import Conduit
import Control.Exception (evaluate)
import Data.ByteString qualified as BS
import Data.ByteString.Lazy qualified as BSL
import Data.Text qualified as T
import Data.Time.Clock
import Database.Esqueleto.Experimental hiding (update)
import Database.Persist qualified as Persist
import Entity.Arkham.LogEntry
import Entity.Arkham.Player
import Entity.Arkham.Step
import Import hiding (delete, exists, on, (==.))
import Json
import Safe (fromJustNote)
import UnliftIO.Exception (catch, try)

normalizeJsonInvestigatorId :: Text -> Text
normalizeJsonInvestigatorId iid = if "c" `T.isPrefixOf` iid then iid else "c" <> iid

isGzipped :: BS.ByteString -> Bool
isGzipped bs = BS.take 2 bs == BS.pack [0x1f, 0x8b]

decodeExportBytes :: BS.ByteString -> Handler (Either String ArkhamExport)
decodeExportBytes bytes
  | isGzipped bytes = do
      eDecompressed <- liftIO $ try @_ @SomeException $ evaluate $ BSL.toStrict $ GZip.decompress $ BSL.fromStrict bytes
      pure $ case eDecompressed of
        Left err -> Left $ displayException err
        Right decompressed -> eitherDecodeStrict' decompressed
  | otherwise = pure $ eitherDecodeStrict' bytes

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
remapInvestigatorUUID :: ArkhamGameId -> Text -> ArkhamPlayerId -> DB ()
remapInvestigatorUUID gameId iCode newPlayerId = do
  let newUUID = toPathPiece newPlayerId
  results :: [Single (Maybe Text)] <-
    rawSql
      "SELECT current_data->'gameEntities'->'investigators'->?->>'playerId' \
      \FROM arkham_games WHERE id = ?"
      [PersistText iCode, PersistText (toPathPiece gameId)]
  case results of
    (Single (Just origUUID) : _) ->
      rawExecute
        "UPDATE arkham_games \
        \SET current_data = replace(current_data::text, ?, ?)::jsonb \
        \WHERE id = ?"
        [ PersistText ("\"" <> origUUID <> "\"")
        , PersistText ("\"" <> newUUID <> "\"")
        , PersistText (toPathPiece gameId)
        ]
    _ -> pure ()

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
  eExportData :: Either String ArkhamExport <-
    decodeExportBytes
      =<< ( fileSourceByteString
              . snd
              . fromJustNote "No export file uploaded"
              . headMay
              $ files
          )
  now <- liftIO getCurrentTime

  case eExportData of
    Left err -> invalidArgs [T.pack err]
    Right export -> do
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
      key <- runDB $ do
        gameId <- insert $ ArkhamGame agedName agedCurrentData agedStep variant now now
        case variant of
          Solo -> do
            iid <- case headMay allInvestigatorIds of
              Nothing -> lift $ invalidArgs ["No investigators found in game data"]
              Just iid -> pure iid
            insert_ $ ArkhamPlayer userId gameId iid
          WithFriends -> do
            let mChosen = mInvestigatorId <|> headMay campaignInvestigatorIds
            chosenInvestigator <- case mChosen of
              Nothing -> lift $ invalidArgs ["No investigator specified"]
              Just iid -> pure iid
            newPlayerId <- insert $ ArkhamPlayer userId gameId chosenInvestigator
            remapInvestigatorUUID gameId chosenInvestigator newPlayerId
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
        insertMany_ [ArkhamStep gameId s.choice s.step s.actionDiff | s <- agedSteps]

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
        pure gameId
      pure
        $ toPublicGame
          (Entity key $ ArkhamGame agedName agedCurrentData agedStep variant now now)
          (GameLog $ map arkhamLogEntryBody agedLog)

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

{- | Every distinct reason a game-seat claim is rejected, decided in the
fixed order 'planAndExecuteClaimSeat' checks them, all AFTER the target
game is already locked (see 'lockClaimSeatGame') -- so a concurrent Main
Street swap that has already locked this same game (see
'Api.Handler.Arkham.Games.Shared.lockSwapGame') can never be interleaved
with any of these checks or the eventual player insert\/remap. Mirrors
'Api.Handler.Arkham.Events.EventDeletionOutcome' and
'Api.Handler.Arkham.Games.Shared.MainStreetSwapOutcome': the handler maps
each reason to a distinct, non-leaking HTTP outcome.
-}
data ClaimSeatFailure
  = -- | No such game, or one that vanished concurrently before this lock.
    ClaimSeatMissingGame
  | -- | The locked game is not a "WithFriends" multiplayer game.
    ClaimSeatNotMultiplayer
  | -- | The requested investigator id is not part of this game at all.
    ClaimSeatInvalidInvestigator
  | -- | The requesting user's 'GroupPlayer' membership for this game's Epic
    -- event could not be reserved under this game's own group ordinal,
    -- because a membership row already exists for a DIFFERENT ordinal --
    -- the existing "one event group per user" restriction (see
    -- 'Api.Handler.Arkham.PendingGames.putApiV1ArkhamPendingGameR', which
    -- enforces the same invariant for its own join flow). Unlike a bare
    -- read, this actually RESERVES the membership (see
    -- 'reserveClaimSeatMembership') so a user with no prior membership can
    -- no longer claim seats in two different groups of the same event --
    -- checked\/reserved only after the target game's own row (and, for an
    -- Epic-linked game, the event row too) are already locked.
    ClaimSeatEventMembershipConflict
  | -- | Some OTHER player already occupies this investigator slot in this
    -- game.
    ClaimSeatTaken
  | -- | This user already holds a seat (any investigator) in this game.
    ClaimSeatAlreadyJoined
  deriving stock (Eq, Show)

{- | The result of attempting to plan and execute a game-seat claim, decided
entirely inside one transaction that locks the target game FIRST (see
'planAndExecuteClaimSeat'). Only 'ClaimSeatClaimed' means a player row was
actually inserted.
-}
data ClaimSeatOutcome
  = ClaimSeatRejected ClaimSeatFailure
  | ClaimSeatClaimed
  deriving stock (Eq, Show)

{- | Abstract persistence steps needed to plan and execute a game-seat claim.
Mirrors 'Api.Handler.Arkham.Games.Shared.MonadMainStreetSwap' and
'Api.Handler.Arkham.Events.MonadEpicEventDeletion': a small set of typed,
individually testable steps, threaded by 'planAndExecuteClaimSeat' into the
exact sequencing production runs.

Do not write an instance that catches a write failure inside
'insertClaimSeatPlayer' and converts it into an ordinary return value of
this class: as with the sibling classes above, 'runDB' only rolls back on
an actual uncaught exception, so silently turning one into a normal result
here would defeat the rollback guarantee.
-}
class Monad m => MonadClaimSeat m where
  -- | Row-lock the target game ('FOR UPDATE' in production) and return its
  -- CURRENT row if still present, or 'Nothing' if it never existed or
  -- vanished concurrently. The FIRST mutable database action this flow
  -- performs -- establishing the same game-before-player (and, when the
  -- game is Epic-linked, game-before-event) order Main Street swap\/event
  -- deletion already use (see
  -- 'Api.Handler.Arkham.Games.Shared.lockSwapGame' and
  -- 'Api.Handler.Arkham.Events.MonadEpicEventDeletion'), so this endpoint
  -- can never insert a player row (or observe a game's or event's state)
  -- before that lock is held, and can never be reversed relative to those
  -- other writers.
  lockClaimSeatGame :: ArkhamGameId -> m (Maybe ArkhamGame)
  -- | Resolve whether the LOCKED game belongs to an Epic event and, if so,
  -- which group ordinal it is. A plain, non-locking read (mirrors
  -- 'lookupGameEvent'), safe here because the game itself is already
  -- locked by the time 'planAndExecuteClaimSeat' calls this.
  lookupClaimSeatEvent :: ArkhamGameId -> m (Maybe (ArkhamEpicEventId, Int))
  -- | Whether some OTHER player already occupies this investigator slot in
  -- this game.
  isClaimSeatTaken :: ArkhamGameId -> Text -> m Bool
  -- | Whether this user already holds ANY seat in this game.
  isClaimSeatAlreadyJoined :: UserId -> ArkhamGameId -> m Bool
  -- | Row-lock the event ('FOR UPDATE' in production; see
  -- 'Api.Arkham.Epic.lockEpicEventRow') and report whether it is still
  -- present. Called from exactly ONE place: after the target game is
  -- locked and every seat\/already-joined check has passed, strictly
  -- BEFORE 'reserveClaimSeatMembership' -- preserving the game(s)-before-
  -- event order 'Api.Handler.Arkham.Events.deleteEpicEventAggregate' uses.
  -- Only called when 'lookupClaimSeatEvent' found this game linked to an
  -- event; 'False' would mean that event vanished concurrently, which is
  -- unreachable in practice (deleting it would first require locking, and
  -- deleting, THIS already-locked game as one of its linked games), but is
  -- still handled as a typed no-op rather than assumed impossible.
  lockClaimSeatEvent :: ArkhamEpicEventId -> m Bool
  -- | Attempt to reserve this user's 'GroupPlayer' membership for the
  -- ALREADY-LOCKED (per 'lockClaimSeatEvent') event under the requested
  -- ordinal -- see 'Api.Arkham.Epic.reserveEpicGroupMembership', the SAME
  -- unique-key reservation 'Api.Handler.Arkham.PendingGames' also calls
  -- (via its own 'Api.Handler.Arkham.PendingGames.MonadPendingJoin'
  -- production instance) for its own join flow, so the invariant cannot
  -- independently drift between the two entry points. Because the event
  -- is already locked exclusively, two concurrent claims into DIFFERENT
  -- games of the SAME event can no longer both observe "no conflict yet":
  -- one must wait for the other's lock, closing the race a bare
  -- 'UniqueEpicMember' insert alone could not.
  reserveClaimSeatMembership :: ArkhamEpicEventId -> UserId -> Int -> m EpicGroupReservation
  -- | Insert the new 'Entity.Arkham.Player.ArkhamPlayer' row and remap its
  -- investigator's stored player UUID. Called from exactly ONE place:
  -- after every check above has passed, using the SAME locked game this
  -- whole flow has held throughout.
  insertClaimSeatPlayer :: UserId -> ArkhamGameId -> Text -> m ()

{- | The game-seat claim decision:

1. Lock the target game FIRST (see 'lockClaimSeatGame'). A missing\/vanished
   game reports 'ClaimSeatMissingGame' before any other check.
2. Reject a non-"WithFriends" game ('ClaimSeatNotMultiplayer') and an
   investigator id not part of this game ('ClaimSeatInvalidInvestigator'),
   both read from the locked snapshot.
3. Reject an already-taken investigator slot ('ClaimSeatTaken') or an
   already-held seat for this user ('ClaimSeatAlreadyJoined').
4. Only now, with the target game locked and every cheaper check passed, is
   an Epic-linked game's event locked ('lockClaimSeatEvent') and this
   user's 'GroupPlayer' membership actually RESERVED
   ('reserveClaimSeatMembership') -- a genuine cross-game reservation
   through 'UniqueEpicMember's own unique key, not merely a non-locking
   read: a user with no prior membership can no longer claim seats in two
   different groups of the same event, sequentially or concurrently,
   because the event row lock serializes every such reservation attempt.
   A conflicting pre-existing reservation under a DIFFERENT ordinal reports
   'ClaimSeatEventMembershipConflict', with no player row inserted or
   modified.
5. Otherwise, insert the player row and remap it (see
   'insertClaimSeatPlayer'), reporting 'ClaimSeatClaimed'.

This is the single seam production and tests both exercise directly,
mirroring 'Api.Handler.Arkham.Games.Shared.planAndExecuteMainStreetSwap' and
'Api.Handler.Arkham.Events.deleteEpicEventAggregate'.
-}
planAndExecuteClaimSeat :: MonadClaimSeat m => ArkhamGameId -> UserId -> Text -> m ClaimSeatOutcome
planAndExecuteClaimSeat gameId userId investigatorId = do
  mGame <- lockClaimSeatGame gameId
  case mGame of
    Nothing -> pure (ClaimSeatRejected ClaimSeatMissingGame)
    Just game -> do
      let gameInvestigatorIds =
            map (normalizeJsonInvestigatorId . unCardCode . unInvestigatorId) $ gamePlayerOrder game.currentData
      if game.multiplayerVariant /= WithFriends
        then pure (ClaimSeatRejected ClaimSeatNotMultiplayer)
        else
          if investigatorId `notElem` gameInvestigatorIds
            then pure (ClaimSeatRejected ClaimSeatInvalidInvestigator)
            else do
              taken <- isClaimSeatTaken gameId investigatorId
              if taken
                then pure (ClaimSeatRejected ClaimSeatTaken)
                else do
                  joined <- isClaimSeatAlreadyJoined userId gameId
                  if joined
                    then pure (ClaimSeatRejected ClaimSeatAlreadyJoined)
                    else do
                      conflict <- claimSeatEventMembershipConflict gameId userId
                      if conflict
                        then pure (ClaimSeatRejected ClaimSeatEventMembershipConflict)
                        else do
                          insertClaimSeatPlayer userId gameId investigatorId
                          pure ClaimSeatClaimed

{- | Whether this user's 'GroupPlayer' membership for this game's Epic event
(if any) cannot be reserved under this game's own ordinal -- see
'ClaimSeatEventMembershipConflict'. Locks the event row ('lockClaimSeatEvent')
and performs the ACTUAL reservation ('reserveClaimSeatMembership'), it does
not merely read existing state: a user with no membership row yet is
reserved into THIS game's group here, closing the gap that previously let
an unlimited number of different groups be claimed with no membership ever
recorded.
-}
claimSeatEventMembershipConflict :: MonadClaimSeat m => ArkhamGameId -> UserId -> m Bool
claimSeatEventMembershipConflict gameId userId = do
  mEvent <- lookupClaimSeatEvent gameId
  case mEvent of
    Nothing -> pure False
    Just (eventId, requestedOrdinal) -> do
      eventStillPresent <- lockClaimSeatEvent eventId
      if not eventStillPresent
        then -- Unreachable in practice (see 'lockClaimSeatEvent' Haddoc): treat
        -- as "nothing to reserve against" rather than assuming impossible.
          pure False
        else do
          reservation <- reserveClaimSeatMembership eventId userId requestedOrdinal
          pure $ case reservation of
            EpicGroupReserved -> False
            EpicGroupReservationConflict -> True

instance MonadClaimSeat (SqlPersistT Handler) where
  lockClaimSeatGame gid = do
    locked <- select do
      g <- from $ table @ArkhamGame
      where_ $ g.id ==. val gid
      locking forUpdate
      pure g
    pure $ entityVal <$> listToMaybe locked
  lookupClaimSeatEvent gid = fmap (\(e, GroupOrdinal o) -> (entityKey e, o)) <$> lookupGameEvent gid
  isClaimSeatTaken gid investigatorId = do
    mTaken <- selectOne do
      players <- from $ table @ArkhamPlayer
      where_ $ players.arkhamGameId ==. val gid
      where_ $ players.investigatorId ==. val investigatorId
      pure players
    pure $ isJust mTaken
  isClaimSeatAlreadyJoined userId gid = isJust <$> getBy (UniquePlayer userId gid)
  lockClaimSeatEvent eid = isJust <$> lockEpicEventRow eid
  reserveClaimSeatMembership = reserveEpicGroupMembership
  insertClaimSeatPlayer userId gid investigatorId = do
    newPlayerId <- insert $ ArkhamPlayer userId gid investigatorId
    remapInvestigatorUUID gid investigatorId newPlayerId

postApiV1ArkhamGameClaimSeatR :: ArkhamGameId -> Handler ()
postApiV1ArkhamGameClaimSeatR gameId = do
  userId <- getRequestUserId
  ClaimSeatPost {investigatorId = rawId} <- requireCheckJsonBody
  let investigatorId = normalizeJsonInvestigatorId rawId
  outcome <- runDB $ planAndExecuteClaimSeat gameId userId investigatorId
  case outcome of
    ClaimSeatRejected ClaimSeatMissingGame -> notFound
    ClaimSeatRejected ClaimSeatNotMultiplayer -> permissionDenied "This game is not a multiplayer game"
    ClaimSeatRejected ClaimSeatInvalidInvestigator -> invalidArgs ["Invalid investigator for this game"]
    ClaimSeatRejected ClaimSeatEventMembershipConflict ->
      permissionDenied "You already occupy a seat in another group in this event"
    ClaimSeatRejected ClaimSeatTaken -> permissionDenied "This seat is already taken"
    ClaimSeatRejected ClaimSeatAlreadyJoined -> permissionDenied "You already have a seat in this game"
    ClaimSeatClaimed -> pure ()
