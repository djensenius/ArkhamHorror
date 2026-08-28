module Api.Arkham.Deck (
  deckFromCreateRequest,
) where

import Api.Arkham.Types.Deck (CreateDeckRequest (..))
import Arkham.Decklist (investigator_name)
import Entity.Arkham.Deck (ArkhamDeck (..))
import Entity.User (UserId)

deckFromCreateRequest :: UserId -> CreateDeckRequest -> ArkhamDeck
deckFromCreateRequest userId CreateDeckRequest {deckName, deckUrl, deckList} =
  ArkhamDeck
    { arkhamDeckUserId = userId
    , arkhamDeckUrl = deckUrl
    , arkhamDeckInvestigatorName = investigator_name deckList
    , arkhamDeckName = deckName
    , arkhamDeckList = deckList
    }
