{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NoFieldSelectors #-}

module Api.Arkham.Types.Game (
  CampaignDetails (..),
  GameDetails (..),
  GameDetailsEntry (..),
  GetGameJson (..),
  InvestigatorDetails (..),
  ScenarioDetails (..),
) where

import Api.Arkham.Types.MultiplayerVariant
import Arkham.Campaigns.TheDreamEaters.Meta qualified as TheDreamEaters
import Arkham.ClassSymbol
import Arkham.Difficulty
import Arkham.Game (PublicGame)
import Arkham.Game.State
import Arkham.Id
import Arkham.Name
import Data.Aeson
import Entity.Arkham.Epic (ArkhamEpicEventId)
import Entity.Arkham.Game (ArkhamGameId)
import Relude

data GetGameJson = GetGameJson
  { playerId :: Maybe PlayerId
  , multiplayerMode :: MultiplayerVariant
  , game :: PublicGame ArkhamGameId
  , eventId :: Maybe ArkhamEpicEventId
  {- ^ the Epic Multiplayer event this game is a group of, if any. Lets the client
  engage the event (shared state, start barrier, time limit) regardless of how
  the player reached the game (so it doesn't depend on a @?event@ URL query).
  -}
  }
  deriving stock (Show, Generic)

instance ToJSON GetGameJson where
  toJSON = genericToJSON defaultOptions
  toEncoding = genericToEncoding defaultOptions

data InvestigatorDetails = InvestigatorDetails
  { id :: InvestigatorId
  , classSymbol :: ClassSymbol
  }
  deriving stock (Show, Generic)
  deriving anyclass ToJSON

data ScenarioDetails = ScenarioDetails
  { id :: ScenarioId
  , difficulty :: Difficulty
  , name :: Name
  , variant :: Maybe Text
  }
  deriving stock (Show, Generic)
  deriving anyclass ToJSON

data CampaignDetails = CampaignDetails
  { id :: CampaignId
  , difficulty :: Difficulty
  , currentCampaignMode :: Maybe TheDreamEaters.CampaignPart
  }
  deriving stock (Show, Generic)
  deriving anyclass ToJSON

data GameDetails = GameDetails
  { id :: ArkhamGameId
  , scenario :: Maybe ScenarioDetails
  , campaign :: Maybe CampaignDetails
  , gameState :: GameState
  , name :: Text
  , investigators :: [InvestigatorDetails]
  , otherInvestigators :: [InvestigatorDetails]
  , multiplayerVariant :: MultiplayerVariant
  , hasOpenSeats :: Bool
  }
  deriving stock (Show, Generic)
  deriving anyclass ToJSON

data GameDetailsEntry = FailedGameDetails Text | SuccessGameDetails GameDetails
  deriving stock (Show, Generic)

instance ToJSON GameDetailsEntry where
  toJSON = \case
    FailedGameDetails message -> object ["error" .= message]
    SuccessGameDetails details -> toJSON details
