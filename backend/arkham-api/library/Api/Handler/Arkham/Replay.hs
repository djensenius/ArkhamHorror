{-# LANGUAGE TemplateHaskell #-}

module Api.Handler.Arkham.Replay (getApiV1ArkhamGameReplayR) where

import Api.Arkham.Helpers
import Api.Handler.Arkham.Games.Shared (withGameAccess)
import Arkham.Game
import Database.Esqueleto.Experimental
import Entity.Arkham.Step
import Import hiding (delete, on, (==.))
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

getApiV1ArkhamGameReplayR :: ArkhamGameId -> Int -> Handler GetReplayJson
getApiV1ArkhamGameReplayR gameId step = do
  Entity userId user <- getRequestUser
  withGameAccess
    user.admin
    (isJust <$> runDB (getBy $ UniquePlayer userId gameId))
    notFound
    $ do
      (ge, allChoices) <- runDB do
        ge <- get404 gameId
        allChoices <- select do
          steps <- from $ table @ArkhamStep
          where_ $ steps.arkhamGameId ==. val gameId
          orderBy [asc steps.step]
          pure steps
        pure (ge, allChoices)
      let gameJson = arkhamGameCurrentData ge
      let choices = map (arkhamStepChoice . entityVal) $ reverse $ drop step allChoices
      case replayChoices gameJson [mconcat $ map choicePatchDown choices] of
        -- Stored patches failing to reconstruct is server-side data
        -- corruption/incompatibility, not malformed client input, so we
        -- respond 422 Unprocessable Entity rather than 400 or 500. The
        -- underlying patch error is deliberately not logged: it can embed
        -- fragments of stored game/user content, so only a static
        -- diagnostic (with the game id) is recorded server-side.
        Left _err -> do
          $(logWarn) $ "replay reconstruction failed for game " <> toPathPiece gameId
          sendStatusJSON Status.status422 $ ReplayError "Unable to reconstruct replay from stored data"
        Right gameJson' ->
          pure
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
              )
              mempty
