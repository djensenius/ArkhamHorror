{-# LANGUAGE TemplateHaskell #-}

module Api.Handler.Arkham.Replay (getApiV1ArkhamGameReplayR) where

import Api.Arkham.Helpers
import Api.Arkham.Viewer (Redacted (..), viewerForUser)
import Api.Handler.Arkham.Games.Shared (withGameAccess)
import Arkham.Game
import Database.Esqueleto.Experimental
import Entity.Arkham.LogEntry
import Entity.Arkham.Step
import Import hiding (delete, on, (<.), (==.))
import Network.HTTP.Types.Status qualified as Status

data GetReplayJson = GetReplayJson
  { totalSteps :: Int
  , game :: PublicGame ArkhamGameId
  }
  deriving stock (Show, Generic)
  deriving anyclass ToJSON

newtype ReplayId = ReplayId {id :: ArkhamGameId}
  deriving stock (Show, Generic)
  deriving anyclass ToJSON

-- | Stable, non-secret envelope for a replay that cannot be reconstructed
-- from stored data. Deliberately omits the underlying patch error, which may
-- reference internal game state shapes.
newtype ReplayError = ReplayError {message :: Text}
  deriving stock (Show, Generic)
  deriving anyclass ToJSON

{- | A replay step, redacted to the caller's seats ('viewerForUser'). An
administrator without a seat gets the spectator view.
-}
getApiV1ArkhamGameReplayR :: ArkhamGameId -> Int -> Handler (Redacted GetReplayJson)
getApiV1ArkhamGameReplayR gameId step = do
  Entity userId user <- getRequestUser
  withGameAccess
    user.admin
    (isJust <$> runDB (getBy $ UniquePlayer userId gameId))
    notFound
    $ do
      (ge, allChoices, gameLog, gameView) <- runDB do
        ge <- get404 gameId
        gameView <- viewerForUser userId gameId
        allChoices <- select do
          steps <- from $ table @ArkhamStep
          where_ $ steps.arkhamGameId ==. val gameId
          orderBy [asc steps.step]
          pure steps
        -- Logs are stamped with the pre-action step. Only show actions already
        -- included in this replay snapshot, using stored step IDs rather than indices.
        let replayStep = maybe 0 (arkhamStepStep . entityVal) $ headMay $ reverse $ take step allChoices
        gameLog <-
          GameLog . map (toLogRow . entityVal) <$> select do
            entries <- from $ table @ArkhamLogEntry
            where_ $ entries.arkhamGameId ==. val gameId
            where_ $ entries.step <. val replayStep
            orderBy [asc entries.step, asc entries.id]
            pure entries
        pure (ge, allChoices, gameLog, gameView)
      let gameJson = arkhamGameCurrentData ge
      let choices = map (arkhamStepChoice . entityVal) $ reverse $ drop step allChoices
      case replayChoices gameJson [mconcat $ map choicePatchDown choices] of
        -- Stored patches failing to reconstruct is server-side data
        -- corruption/incompatibility, not malformed client input, so we
        -- respond 422 Unprocessable Entity rather than 400 or 500.
        -- 'ReplayChoicesError' is opaque/nullary by construction -- the
        -- underlying patch/parser error text is discarded at the source in
        -- 'Arkham.Game.replayChoices' and never reaches this handler, so
        -- only a static diagnostic (with the game id) is recorded
        -- server-side.
        Left ReplayChoicesPatchFailed -> do
          $(logWarn) $ "replay reconstruction failed for game " <> toPathPiece gameId
          sendStatusJSON Status.status422 $ ReplayError "Unable to reconstruct replay from stored data"
        Right gameJson' ->
          pure
            $ Redacted gameView
            $ GetReplayJson (length allChoices)
            $ toPublicGame
              ( Entity gameId
                  $ ArkhamGame
                    (arkhamGameName ge)
                    gameJson'
                    (arkhamGameStep ge)
                    (arkhamGameMultiplayerVariant ge)
                    (arkhamGameCreatedAt ge)
                    (arkhamGameUpdatedAt ge)
                    (arkhamGameAllowSpectators ge)
              )
              gameLog
