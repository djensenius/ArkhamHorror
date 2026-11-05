module Api.Handler.Arkham.PendingGames (
  getApiV1ArkhamPendingGameR,
  putApiV1ArkhamPendingGameR,
) where

import Import hiding (on, (==.))

import Api.Arkham.Epic (
  GroupMembershipReservation (GroupMembershipInOtherGroup),
  applyEpicDeltasLocked,
  lockEpicEventRow,
  lookupGameEvent,
  mkEpicEnv,
  reserveGroupMembership,
 )
import Api.Arkham.Helpers
import Api.Arkham.Viewer (RedactedGame (..), viewerForUser)
import Api.Handler.Arkham.Games.Shared (
  broadcastEventChanged,
  epicSyncMessages,
  propagateShared,
  publishToRoom,
  RunMessagesTimeout (RunMessagesTimeout),
  runMessagesTimeoutMicros,
 )
import Arkham.Classes.HasQueue
import Arkham.Epic.Types (
  EpicRole (GroupPlayer),
  GroupOrdinal (..),
  epicEnvDeltaRef,
  epicEnvGroup,
  epicEnvSharedRef,
 )
import Arkham.Game
import Arkham.Game.State
import Arkham.Game.Utils (gameInvestigators)
import Arkham.Id
import Arkham.Message (Message (JoinCampaign))
import Arkham.Queue
import Control.Lens (view)
import Control.Monad.Random (mkStdGen)
import Data.Time.Clock
import Database.Persist ((==.))
import Entity.Answer (atCampaignContinuation, maxInvestigators)
import Entity.Arkham.Step
import UnliftIO.Exception (throwIO, try)
import UnliftIO.Timeout (timeout)

{- | The game previewed before joining, redacted to what the caller's current
seats may see ('viewerForUser'): someone not yet seated gets the spectator
view.
-}
getApiV1ArkhamPendingGameR :: ArkhamGameId -> Handler (RedactedGame (PublicGame ArkhamGameId))
getApiV1ArkhamPendingGameR gameId = do
  userId <- getRequestUserId
  (ge, gameView) <- runDB $ (,) <$> get404 gameId <*> viewerForUser userId gameId
  pure $ RedactedGame gameView $ toPublicGame (Entity gameId ge) mempty

-- | Refuses a join inside the game-locked transaction. Thrown rather than
-- answered there, so the transaction rolls back; 'putApiV1ArkhamPendingGameR'
-- turns it into the 403 (or, for 'EpicEventGone', the 404) once runDB has
-- returned.
data PendingJoinRefused
  = SeatedInAnotherGroup
  | {- | The event row was gone when the join locked it. The event delete
    handler cannot cause this: it locks the game first, through
    'canonicalEpicGameLockOrder', so the join sees the event either whole or
    not at all. Only a delete that bypasses that order (raw SQL, or a future
    path) can; it is answered 404 rather than failing the member insert.
    -}
    EpicEventGone
  deriving stock Show
  deriving anyclass Exception

{- | Join, then answer with the game redacted for the caller's seats as they
stand after the join committed, so a new player sees their own seat's zones.
-}
putApiV1ArkhamPendingGameR :: ArkhamGameId -> Handler (RedactedGame (PublicGame ArkhamGameId))
putApiV1ArkhamPendingGameR gameId = do
  userId <- getRequestUserId
  now <- liftIO getCurrentTime
  -- A player may occupy seats in only one group of an event. The normal UI
  -- enforces this too, but direct invite URLs/API calls must not be able to move
  -- the membership row while leaving ArkhamPlayer rows in multiple games. The
  -- check runs under the game lock and, where the join seats the player,
  -- reserves the membership row before the seat insert, so two concurrent
  -- first joins to different groups of one event cannot both pass it.
  -- Lock order follows 'canonicalEpicGameLockOrder': the game, then the event
  -- row FOR UPDATE, then the member row. The event is locked before the member
  -- insert: that insert's foreign key takes KEY SHARE on the event row, and
  -- two first joins to different groups holding KEY SHARE would deadlock when
  -- each then asks for FOR UPDATE to commit its setup's shared deltas.
  joined <- try $ runDB $ atomicallyWithGame gameId \original@ArkhamGame {..} -> do
    mEpicCtx <- lookupGameEvent gameId
    let
      -- Paths that do not seat the player only read the membership.
      requireNoOtherGroup = for_ mEpicCtx \(eventEntity, GroupOrdinal requestedOrdinal) -> do
        mMembership <- getBy (UniqueEpicMember (entityKey eventEntity) userId GroupPlayer)
        for_ mMembership \(Entity _ membership) ->
          when (arkhamEpicMemberGroupOrdinal membership /= Just requestedOrdinal)
            $ liftIO
            $ throwIO SeatedInAnotherGroup
      -- Paths that seat the player lock the event row, then reserve the
      -- membership. Seating changes the event roster, so it is announced once
      -- the join commits. Returns the event as read under the lock.
      reserveMembership = forM mEpicCtx \(eventEntity, ordinal) -> do
        lockedEvent <-
          lockEpicEventRow (entityKey eventEntity)
            >>= maybe (liftIO $ throwIO EpicEventGone) pure
        reserveGroupMembership (entityKey lockedEvent) userId ordinal >>= \case
          GroupMembershipInOtherGroup _ -> liftIO $ throwIO SeatedInAnotherGroup
          _ -> pure (lockedEvent, ordinal)
      -- Cap the engine run like updateGame does: a runaway setup must not
      -- hold the game lock, and the timeout rolls the join back.
      runWithTimeout action =
        liftIO (timeout runMessagesTimeoutMicros action) >>= \case
          Just a -> pure a
          Nothing -> liftIO $ throwIO $ RunMessagesTimeout gameId runMessagesTimeoutMicros
      -- Clear any step rows above the current step before the join inserts
      -- its own, as updateGame does: a leftover row (old data, a debug
      -- import, an edit made outside the app) would otherwise fail that
      -- insert on UniqueStep. It must run before the replace bumps the game's
      -- step, because the step-deletion trigger refuses rows at or below it.
      clearStaleSteps = deleteWhere [ArkhamStepArkhamGameId ==. gameId, ArkhamStepStep >. arkhamGameStep]
      unchanged = do
        requireNoOtherGroup
        pure (original, Nothing, Nothing)
    case gameGameState arkhamGameCurrentData of
      -- Between scenarios a campaign is open to new players (Rules Reference,
      -- "Joining or Leaving a Campaign"). The seat is added to the live game and
      -- picks a deck from the investigators nobody has used yet.
      IsActive
        | atCampaignContinuation arkhamGameCurrentData
        , length (gameInvestigators arkhamGameCurrentData) < maxInvestigators -> do
            alreadyExists <- exists [ArkhamPlayerArkhamGameId ==. gameId, ArkhamPlayerUserId ==. userId]
            if alreadyExists
              then unchanged
              else do
                -- No shared deltas are committed here, but the event is locked
                -- FOR UPDATE all the same, so every seating join keeps one lock
                -- order, and a vanished event answers the same 404.
                mLockedCtx <- reserveMembership
                mLastStep <- getBy (UniqueStep gameId arkhamGameStep)
                let currentQueue = maybe [] (choiceMessages . arkhamStepChoice . entityVal) mLastStep

                gameRef <- liftIO $ newIORef arkhamGameCurrentData
                queueRef <- liftIO $ newQueue currentQueue
                genRef <- liftIO $ newIORef (mkStdGen (gameSeed arkhamGameCurrentData))

                pid <- insert $ ArkhamPlayer userId gameId "00000"

                runWithTimeout $ runGameApp (GameApp gameRef queueRef genRef (pure . const ()) Nothing) do
                  pushEnd $ JoinCampaign (PlayerId $ coerce pid)
                  runMessages (gameIdToText gameId) noRunObservers

                updatedGame <- liftIO $ readIORef gameRef
                updatedQueue <- liftIO $ readIORef (queueToRef queueRef)

                let
                  game' =
                    ArkhamGame
                      arkhamGameName
                      updatedGame
                      (arkhamGameStep + 1)
                      arkhamGameMultiplayerVariant
                      arkhamGameCreatedAt
                      now
                      arkhamGameAllowSpectators

                clearStaleSteps
                replace gameId game'
                insert_
                  $ ArkhamStep
                    gameId
                    (Choice mempty updatedQueue)
                    (arkhamGameStep + 1)
                    (ActionDiff $ view actionDiffL updatedGame)

                pure (game', Nothing, entityKey . fst <$> mLockedCtx)
      IsPending _ -> do
        alreadyExists <- exists [ArkhamPlayerArkhamGameId ==. gameId, ArkhamPlayerUserId ==. userId]

        if alreadyExists
          then unchanged
          else do
            mLockedCtx <- reserveMembership
            mLastStep <- getBy (UniqueStep gameId arkhamGameStep)
            let currentQueue = maybe [] (choiceMessages . arkhamStepChoice . entityVal) mLastStep

            gameRef <- liftIO $ newIORef arkhamGameCurrentData
            queueRef <- liftIO $ newQueue currentQueue
            genRef <- liftIO $ newIORef (mkStdGen (gameSeed arkhamGameCurrentData))

            pid <- insert $ ArkhamPlayer userId gameId "00000"

            -- Epic Multiplayer: joining enrolled the user as a member (above),
            -- and setup must run event-aware so the group's shared values
            -- (countermeasures, Subject 8L-08 health) start from the CURRENT
            -- pool rather than 0/seed — otherwise a group set up after others
            -- have acted would not reflect their spends/damage.
            mEpicEnv <- traverse (uncurry mkEpicEnv) mLockedCtx

            runWithTimeout $ runGameApp (GameApp gameRef queueRef genRef (pure . const ()) mEpicEnv) $ do
              addPlayer (PlayerId $ coerce pid)
              -- Run setup. For a multiplayer lobby this PAUSES at ChooseDeck
              -- (IsChooseDecks) until players pick decks, so we must NOT run any
              -- further messages here or we'd blast past deck selection and start
              -- the scenario with zero investigators ("No lead found").
              runMessages (gameIdToText gameId) noRunObservers
              -- Only when setup actually completed in this request (e.g. a fully
              -- pre-decked/AI group) do we reconcile the board to the shared pool.
              -- Otherwise the reconcile happens after deck selection via the
              -- action pull (StartScenario preserves EpicShared counts) + push.
              setupState <- gameGameState <$> liftIO (readIORef gameRef)
              when (setupState == IsActive) $ for_ mEpicEnv \epic -> do
                shared <- liftIO $ readIORef (epicEnvSharedRef epic)
                pushAll (epicSyncMessages (epicEnvGroup epic) shared)
                runMessages (gameIdToText gameId) noRunObservers

            updatedGame <- liftIO $ readIORef gameRef
            updatedQueue <- liftIO $ readIORef (queueToRef queueRef)

            -- Commit any shared deltas emitted during setup (same transaction).
            mShared <- case (mLockedCtx, mEpicEnv) of
              (Just (eventEntity, _), Just epic) -> do
                deltas <- liftIO $ readIORef (epicEnvDeltaRef epic)
                if null deltas
                  then pure Nothing
                  else do
                    s <- applyEpicDeltasLocked (entityKey eventEntity) (Just gameId) (Just (arkhamGameStep + 1)) deltas
                    pure $ Just (entityKey eventEntity, s)
              _ -> pure Nothing

            let
              game' =
                ArkhamGame
                  arkhamGameName
                  updatedGame
                  (arkhamGameStep + 1)
                  arkhamGameMultiplayerVariant
                  arkhamGameCreatedAt
                  now
                  arkhamGameAllowSpectators

            clearStaleSteps
            replace gameId game'
            insert_
              $ ArkhamStep
                gameId
                (Choice mempty updatedQueue)
                (arkhamGameStep + 1)
                (ActionDiff $ view actionDiffL updatedGame)

            pure (game', mShared, entityKey . fst <$> mLockedCtx)
      _ -> unchanged

  (game@ArkhamGame {..}, mShared, mChangedEvent) <- case joined of
    Left SeatedInAnotherGroup ->
      permissionDenied "You already occupy a seat in another group in this event"
    Left EpicEventGone -> notFound
    Right result -> pure result
  for_ mShared \(eid, s) -> propagateShared eid (Just gameId) s
  for_ mChangedEvent broadcastEventChanged
  publishToRoom gameId
    $ GameUpdate
    $ PublicGame gameId arkhamGameName [] arkhamGameCurrentData
  -- A fresh read after the join's transaction committed, so it sees the
  -- seat row the join inserted.
  gameView <- runDB $ viewerForUser userId gameId
  pure $ RedactedGame gameView $ toPublicGame (Entity gameId game) mempty
