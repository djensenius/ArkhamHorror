module Api.Handler.Arkham.PendingGames (
  getApiV1ArkhamPendingGameR,
  putApiV1ArkhamPendingGameR,
) where

import Import hiding (on, (==.))

import Api.Arkham.Epic (
  GroupMembershipReservation (GroupMembershipInOtherGroup),
  applyEpicDeltasLocked,
  lookupGameEvent,
  mkEpicEnv,
  reserveGroupMembership,
 )
import Api.Arkham.Helpers
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

getApiV1ArkhamPendingGameR :: ArkhamGameId -> Handler (PublicGame ArkhamGameId)
getApiV1ArkhamPendingGameR gameId = do
  _ <- getRequestUserId
  ge <- runDB $ get404 gameId
  pure $ toPublicGame (Entity gameId ge) mempty

-- | Refuses a join inside the game-locked transaction. Thrown rather than
-- answered there, so the transaction rolls back; 'putApiV1ArkhamPendingGameR'
-- turns it into the 403 once runDB has returned.
data PendingJoinRefused = SeatedInAnotherGroup
  deriving stock Show
  deriving anyclass Exception

putApiV1ArkhamPendingGameR :: ArkhamGameId -> Handler (PublicGame ArkhamGameId)
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
  -- (the KEY SHARE that the member insert's foreign key takes on the event row
  -- counts as an event lock), then the member row.
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
      -- Paths that seat the player reserve the membership first. Seating
      -- changes the event roster, so it is announced once the join commits.
      reserveMembership = forM mEpicCtx \(eventEntity, ordinal) ->
        reserveGroupMembership (entityKey eventEntity) userId ordinal >>= \case
          GroupMembershipInOtherGroup _ -> liftIO $ throwIO SeatedInAnotherGroup
          _ -> pure (entityKey eventEntity)
      -- Cap the engine run like updateGame does: a runaway setup must not
      -- hold the game lock, and the timeout rolls the join back.
      runWithTimeout action =
        liftIO (timeout runMessagesTimeoutMicros action) >>= \case
          Just a -> pure a
          Nothing -> liftIO $ throwIO $ RunMessagesTimeout gameId runMessagesTimeoutMicros
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
                mChangedEvent <- reserveMembership
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

                replace gameId game'
                insert_
                  $ ArkhamStep
                    gameId
                    (Choice mempty updatedQueue)
                    (arkhamGameStep + 1)
                    (ActionDiff $ view actionDiffL updatedGame)

                pure (game', Nothing, mChangedEvent)
      IsPending _ -> do
        alreadyExists <- exists [ArkhamPlayerArkhamGameId ==. gameId, ArkhamPlayerUserId ==. userId]

        if alreadyExists
          then unchanged
          else do
            mChangedEvent <- reserveMembership
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
            mEpicEnv <- traverse (uncurry mkEpicEnv) mEpicCtx

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
            mShared <- case (mEpicCtx, mEpicEnv) of
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

            replace gameId game'
            insert_
              $ ArkhamStep
                gameId
                (Choice mempty updatedQueue)
                (arkhamGameStep + 1)
                (ActionDiff $ view actionDiffL updatedGame)

            pure (game', mShared, mChangedEvent)
      _ -> unchanged

  (game@ArkhamGame {..}, mShared, mChangedEvent) <- case joined of
    Left SeatedInAnotherGroup ->
      permissionDenied "You already occupy a seat in another group in this event"
    Right result -> pure result
  for_ mShared \(eid, s) -> propagateShared eid (Just gameId) s
  for_ mChangedEvent broadcastEventChanged
  publishToRoom gameId
    $ GameUpdate
    $ PublicGame gameId arkhamGameName [] arkhamGameCurrentData
  pure $ toPublicGame (Entity gameId game) mempty
