module Api.Arkham.Deck (
  deckFromCreateRequest,
  DeckError (..),
  toDeckErrors,
  toDeckErrorsWith,
) where

import Api.Arkham.Types.Deck (CreateDeckRequest (..))
import Arkham.Card.CardCode (CardCode)
import Arkham.Card.CustomCard (lookupCustomCardDef)
import Arkham.Decklist (ArkhamDBDecklist (..))
import Arkham.PlayerCard (allPlayerCards)
import Data.Aeson
import Data.Map.Strict qualified as Map
import Entity.Arkham.Deck (ArkhamDeck (..))
import Entity.User (UserId)
import Relude

deckFromCreateRequest :: UserId -> CreateDeckRequest -> ArkhamDeck
deckFromCreateRequest userId CreateDeckRequest {deckName, deckUrl, deckList} =
  ArkhamDeck
    { arkhamDeckUserId = userId
    , arkhamDeckUrl = deckUrl
    , arkhamDeckInvestigatorName = investigator_name deckList
    , arkhamDeckName = deckName
    , arkhamDeckList = deckList
    , arkhamDeckOverlay = Nothing
    , arkhamDeckOverlayAttachmentId = Nothing
    , arkhamDeckLastUsedAt = Nothing
    }

newtype DeckError = UnimplementedCard CardCode
  deriving stock (Show, Eq, Generic)

instance ToJSON DeckError where
  toJSON = genericToJSON $ defaultOptions {tagSingleConstructors = True}

toDeckErrors :: ArkhamDBDecklist -> [DeckError]
toDeckErrors = toDeckErrorsWith (isJust . lookupCustomCardDef)

-- Background work must resolve custom cards from the queued owner's library,
-- not another user's definitions left in the process-wide engine registry.
toDeckErrorsWith :: (CardCode -> Bool) -> ArkhamDBDecklist -> [DeckError]
toDeckErrorsWith customCardSupported decklist = flip mapMaybe (Map.keys decklist.slots) \cardCode ->
  if Map.member cardCode allPlayerCards || customCardSupported cardCode
    then Nothing
    else Just $ UnimplementedCard cardCode
