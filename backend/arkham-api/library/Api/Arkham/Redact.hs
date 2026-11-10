{- | Per-viewer redaction of the 'Arkham.Game.PublicGame' wire value.

Why this exists. Every client path sends the same @PublicGame@ JSON to every
participant, and that JSON carries each investigator's hand and ordered deck.
A modified client could read another player's hidden cards straight off the
wire. 'redactFor' takes the encoded game and removes what the viewer is not
entitled to see.

It works on the encoded 'Value', not on 'Arkham.Game.Game', on purpose: both
encoders ('toJSON' and the hand-written 'toEncoding') produce the same JSON,
so redacting after encoding covers both, and the function stays pure and
independent of the engine monads. Nothing calls it yet; wiring it into REST,
the websocket and the contracts are later slices of TASK-2.22.

What it does, for every investigator object in @investigators@,
@otherInvestigators@, @killedInvestigators@ and @retiredInvestigators@:

  * With 'includeHandCount' on, adds @handCount@ (the length of @hand@) to
    every investigator, controlled or not, so the shape is uniform. If
    @handCount@ is already present it is kept, which is what makes the
    redaction idempotent. It is off in 'defaultRedactOptions' (and so in
    'redactFor'): @handCount@ is not in the published contract yet, and
    TASK-2.22.3 turns it on together with the contract change.
  * Leaves investigators the viewer controls otherwise untouched. Control is
    the investigator's @playerId@ field matching one of the viewer's seats; an
    investigator with no @playerId@, or with the nil @playerId@ PublicGame
    uses for seatless investigators, is controlled by nobody.
  * For every other investigator, empties the hidden zones listed in
    'hiddenZoneKeys' (array stays an array, object stays an object, @null@
    stays @null@). That includes @excludeFromMulligan@, which holds the card
    ids of the pre-mulligan hand and so, read against the top-level @cards@
    registry, would reveal it. It also sets @drawing@ and @discarding@ to @null@ (they are
    @Maybe@ records holding drawn/discarded cards, and @{}@ is not a valid
    value of either type), and empties @search.searchFoundCards@ and
    @search.searchDrawnCards@ (the rest of the search stays, so the table can
    still see that a search is under way). @deckSize@ is kept as is.
  * Keeps only the top card of @deck@ when the investigator's @modifiers@
    include 'TopCardOfDeckIsRevealed' (Norman Withers, Montmartre); that
    card is public information at the table.

Each of those four maps is expected to be an object keyed by investigator id
(the spec pins that against the real encoder). Anything else fails closed: a
map that is not an object becomes @{}@, and a map entry that is not an object
is dropped.

And for the @log@ tail: a structured row (or child entry) whose @audience@ is
@OnlyPlayer p@ is dropped unless the viewer holds seat @p@ (the nil player
id is never a seat); a spectator sees
only @Everyone@ entries. A structured row with no @contents@, a structured
entry that is not an object, and a structured entry with no readable audience
are all dropped (fail closed), and a @log@ or @children@ that is not an
array becomes @[]@. Legacy rows carry no audience and are kept.

Known leak surfaces deliberately not touched here (follow-ups):

  * @question@ and @questionPresentation@ (a prompt can name hand or deck
    cards of the player it is for);
  * the top-level @cards@ registry (every card by id, including hand cards);
  * top-level @foundCards@ and @focusedCards@;
  * the @card@ payload on each entry of an investigator's @modifiers@;
  * investigator @meta@ and @formMeta@ (shapes vary per investigator, and
    generic redaction could break rendering);
  * the encounter deck order.
-}
module Api.Arkham.Redact (
  Viewer (..),
  RedactOptions (..),
  defaultRedactOptions,
  redactFor,
  redactWith,
  viewerControls,
  viewerSeats,
  logRowFor,
  hiddenZoneKeys,
  investigatorMapKeys,
) where

import Arkham.Id (PlayerId (PlayerId))
import Arkham.Log.Entry (LogAudience (Everyone, OnlyPlayer), LogEntry (..), LogRow (..))
import Arkham.Modifier (ModifierType (TopCardOfDeckIsRevealed))
import Data.Aeson
import Data.Aeson.KeyMap qualified as KeyMap
import Data.UUID (nil)
import Data.Vector qualified as V
import Relude

{- | Who a game payload is being prepared for.

A user can hold several seats (solo or multi-handed play) and must see every
investigator they control, so a participant is a set of seats rather than one.
-}
data Viewer
  = ViewerSeats (Set PlayerId)
  | ViewerSpectator
  deriving stock (Show, Eq)

{- | The viewer's real seats. The nil player id is never a seat: PublicGame
uses it as the sentinel for investigators nobody plays (killed ones and
other-campaign ones rebuilt from deck lists), so it is dropped here.
-}
viewerSeats :: Viewer -> [PlayerId]
viewerSeats = \case
  ViewerSeats seats -> filter (/= nilPlayer) (toList seats)
  ViewerSpectator -> []

nilPlayer :: PlayerId
nilPlayer = PlayerId nil

{- | Whether the viewer controls an investigator with this encoded @playerId@.
An investigator whose @playerId@ is the nil sentinel is controlled by nobody,
even a viewer whose seats include the nil player id.
-}
viewerControls :: Viewer -> Value -> Bool
viewerControls viewer pid =
  pid /= toJSON nilPlayer && any ((== pid) . toJSON) (viewerSeats viewer)

-- | The top-level PublicGame maps whose values are investigator objects.
investigatorMapKeys :: [Key]
investigatorMapKeys =
  ["investigators", "otherInvestigators", "killedInvestigators", "retiredInvestigators"]

{- | Investigator fields hidden from anyone who doesn't control the
investigator, emptied in place. @deck@ is handled separately (top card) and
@drawing@\/@discarding@\/@search@ are handled separately (see module docs).
-}
hiddenZoneKeys :: [Key]
hiddenZoneKeys =
  ["hand", "sideDeck", "decks", "drawnCards", "cardsUnderneath", "excludeFromMulligan"]

-- | Switches for what 'redactWith' adds to the wire value.
newtype RedactOptions = RedactOptions
  { includeHandCount :: Bool
  -- ^ add @handCount@ to every investigator (see the module docs)
  }
  deriving stock (Show, Eq)

-- | What the server sends today: no @handCount@ (TASK-2.22.3 turns it on).
defaultRedactOptions :: RedactOptions
defaultRedactOptions = RedactOptions {includeHandCount = False}

-- | 'redactWith' 'defaultRedactOptions'.
redactFor :: Viewer -> Value -> Value
redactFor = redactWith defaultRedactOptions

redactWith :: RedactOptions -> Viewer -> Value -> Value
redactWith options viewer = \case
  Object game ->
    Object
      . adjust (redactLog viewer) "log"
      $ foldl' (\g k -> adjust (redactInvestigatorMap options viewer) k g) game investigatorMapKeys
  other -> other

redactInvestigatorMap :: RedactOptions -> Viewer -> Value -> Value
redactInvestigatorMap options viewer = \case
  Object investigators ->
    Object (KeyMap.map (redactInvestigator options viewer) (KeyMap.filter isObject investigators))
  _ -> Object mempty
 where
  isObject = \case
    Object _ -> True
    _ -> False

redactInvestigator :: RedactOptions -> Viewer -> Value -> Value
redactInvestigator options viewer = \case
  Object investigator ->
    let counted = if options.includeHandCount then withHandCount investigator else investigator
        controlled = maybe False (viewerControls viewer) (KeyMap.lookup "playerId" investigator)
     in Object $ if controlled then counted else hideZones counted
  other -> other

withHandCount :: Object -> Object
withHandCount investigator
  | KeyMap.member "handCount" investigator = investigator
  | otherwise = KeyMap.insert "handCount" (toJSON handCount) investigator
 where
  handCount = case KeyMap.lookup "hand" investigator of
    Just (Array cards) -> V.length cards
    _ -> 0

hideZones :: Object -> Object
hideZones investigator =
  adjust emptySearch "search"
    . adjust deck "deck"
    . adjust nullify "drawing"
    . adjust nullify "discarding"
    $ foldl' (\o k -> adjust emptied k o) investigator hiddenZoneKeys
 where
  deck
    | topCardRevealed investigator = \case
        Array cards -> Array (V.take 1 cards)
        other -> emptied other
    | otherwise = emptied
  emptySearch = \case
    Object search ->
      Object
        . adjust emptied "searchDrawnCards"
        $ adjust emptied "searchFoundCards" search
    other -> other
  nullify = const Null

-- | Apply to a key's value if present. aeson's KeyMap has no @adjust@.
adjust :: (Value -> Value) -> Key -> Object -> Object
adjust f k o = maybe o (\v -> KeyMap.insert k (f v) o) (KeyMap.lookup k o)

emptied :: Value -> Value
emptied = \case
  Array _ -> Array mempty
  Object _ -> Object mempty
  other -> other

topCardRevealed :: Object -> Bool
topCardRevealed investigator = case KeyMap.lookup "modifiers" investigator of
  Just (Array modifiers) -> any isTopCardRevealed modifiers
  _ -> False
 where
  revealed = toJSON TopCardOfDeckIsRevealed
  isTopCardRevealed = \case
    Object modifier -> KeyMap.lookup "type" modifier == Just revealed
    _ -> False

redactLog :: Viewer -> Value -> Value
redactLog viewer = \case
  Array rows -> Array (V.mapMaybe (redactLogRow viewer) rows)
  _ -> Array mempty

redactLogRow :: Viewer -> Value -> Maybe Value
redactLogRow viewer = \case
  Object o
    | KeyMap.lookup "tag" o == Just (String "LogRowStructured") ->
        case KeyMap.lookup "contents" o of
          Just entry ->
            (\e -> Object (KeyMap.insert "contents" e o)) <$> redactLogEntry viewer entry
          Nothing -> Nothing
  row -> Just row

redactLogEntry :: Viewer -> Value -> Maybe Value
redactLogEntry viewer = \case
  Object entry
    | maybe False (audienceVisible viewer) (KeyMap.lookup "audience" entry) ->
        Just . Object $ adjust children "children" entry
    | otherwise -> Nothing
  _ -> Nothing
 where
  children = \case
    Array entries -> Array (V.mapMaybe (redactLogEntry viewer) entries)
    _ -> Array mempty

{- | The @log@ tail's rule ('redactLog') for one typed row, for the log reads
that are not a game payload (the scrollback page and the log export): a
structured row the viewer may not read is dropped, its children are pruned the
same way, and a legacy row is kept.
-}
logRowFor :: Viewer -> LogRow -> Maybe LogRow
logRowFor viewer = \case
  LogRowStructured entry -> LogRowStructured <$> logEntryFor viewer entry
  row@(LogRowLegacy _ _) -> Just row

logEntryFor :: Viewer -> LogEntry -> Maybe LogEntry
logEntryFor viewer entry
  | audienceVisible viewer (toJSON entry.logEntryAudience) =
      Just entry {logEntryChildren = mapMaybe (logEntryFor viewer) entry.logEntryChildren}
  | otherwise = Nothing

audienceVisible :: Viewer -> Value -> Bool
audienceVisible viewer audience =
  audience == toJSON Everyone
    || any (\p -> audience == toJSON (OnlyPlayer p)) (viewerSeats viewer)
