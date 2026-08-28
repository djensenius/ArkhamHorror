{-# LANGUAGE NoFieldSelectors #-}

module Api.Arkham.Types.Deck (
  CreateDeckRequest (..),
  FetchDeckRequest (..),
  DeckValidationError (..),
  DeckOperationError (..),
  UpgradeDeckPost (..),
) where

import Arkham.Card.CardCode (CardCode)
import Arkham.Decklist (ArkhamDBDecklist)
import Arkham.Id (InvestigatorId)
import Data.Aeson
import Json (aesonOptions)
import Relude

data CreateDeckRequest = CreateDeckRequest
  { deckId :: Text
  , deckName :: Text
  , deckUrl :: Maybe Text
  , deckList :: ArkhamDBDecklist
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass FromJSON

newtype FetchDeckRequest = FetchDeckRequest
  { fetchDeckUrl :: Text
  }
  deriving stock (Eq, Show, Generic)

instance FromJSON FetchDeckRequest where
  parseJSON = genericParseJSON $ aesonOptions $ Just "fetchDeck"

newtype DeckValidationError = UnimplementedCard CardCode
  deriving stock (Eq, Show, Generic)

instance ToJSON DeckValidationError where
  toJSON = genericToJSON $ defaultOptions {tagSingleConstructors = True}

newtype DeckOperationError = DeckOperationError {errorMsg :: Text}
  deriving stock (Eq, Show, Generic)
  deriving anyclass (FromJSON, ToJSON)

data UpgradeDeckPost = UpgradeDeckPost
  { udpInvestigatorId :: InvestigatorId
  , udpDeckUrl :: Maybe Text
  , udpDeckList :: Maybe ArkhamDBDecklist
  }
  deriving stock (Eq, Show, Generic)

instance FromJSON UpgradeDeckPost where
  parseJSON = genericParseJSON $ aesonOptions $ Just "udp"
