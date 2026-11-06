{- | Per-viewer redaction of the 'Arkham.Game.PublicGame' wire value.

Why this exists. Every client path sends the same @PublicGame@ JSON to every
participant, and that JSON carries each investigator's hand and ordered deck.
A modified client could read another player's hidden cards straight off the
wire. 'redactFor' takes the encoded game and removes what the viewer is not
entitled to see.

It works on the encoded 'Value', not on 'Arkham.Game.Game', on purpose: both
encoders ('toJSON' and the hand-written 'toEncoding') produce the same JSON,
so redacting after encoding covers both, and the function stays pure and
independent of the engine monads. 'Api.Arkham.Viewer' applies it to the REST
game, spectate and replay responses; the websocket and the contracts are later
slices of TASK-2.22.

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

Hidden card ids (TASK-2.22.5 P3). Before any zone is emptied, 'hiddenCardIds'
collects the card ids the viewer is not entitled to see:

  * from every investigator the viewer does not control (the four maps above
    plus the campaign's @meta.otherCampaignPlayers@): the cards in @hand@,
    @deck@ (less the top card when it is revealed, see above), @sideDeck@,
    @decks@, @drawnCards@, @cardsUnderneath@, @drawing@, @discarding@ and
    @search.searchFoundCards@\/@search.searchDrawnCards@, and the ids in
    @excludeFromMulligan@ (exactly the zones the investigator redaction empties);
  * the found and drawn cards of the scenario-level @search@ when the searching
    investigator is not one the viewer controls;

less every id that is public or already the viewer's: ids in any
investigator's @meta.revealedCards@ (Bob Jenkins reveals hands to the table),
the @cardId@ of every entity in play, the cards in investigators' @discard@
piles, @activeCard@ and @skillTest@ (committed cards are shown to the table),
the cards held by the investigators the viewer controls (their @modifiers@,
@meta@ and @formMeta@ aside) and by the viewer's own scenario-level search,
and any string in the viewer's own @question@ entries. Those ids are then removed from:

  * the top-level @cards@ registry (the entry is dropped; cards in play stay);
  * top-level @focusedCards@, @highlightedCards@ and each zone list of
    top-level @foundCards@ (zone keys are kept), and the @searchFoundCards@
    lists and @searchDrawnCards@ of another seat's scenario-level @search@
    (zone keys kept; a card that is visible to the viewer, e.g. named in their
    own question, stays; a value that is not an array becomes empty);
  * every @modifiers@ list: top-level @modifiers@ (pairs whose target names a
    hidden id are dropped) and the @modifiers@ of each investigator, location,
    enemy, asset, act, agenda, treachery, event, story and skill, of the skill
    test and of the focused and skill-test chaos tokens. A modifier whose
    @card@ is hidden gets @card: null@; one whose type or source still names a
    hidden id (e.g. @AsIfInHand@ of a hand card) is dropped;
  * @meta@ and @formMeta@ of every investigator the viewer does not control: a
    hidden card or id inside an array is removed, as an object value it becomes
    @null@, and an object key that is a hidden id is dropped. Everything else
    (@revealedCards@, a class choice and so on) stays;
  * @playProhibitions@, @commitProhibitions@ (@[cardId, refs]@ pairs for
    cards in hand or played as if in hand), @cannotDiscoverClues@ and
    @cannotSpendClues@ (@null@ or a list of log refs, which can carry card
    ids) of every investigator, the viewer's own included (an @AsIfInHand@
    effect can put another seat's card in them): an array entry naming a
    hidden id anywhere is dropped (an array stays an array; for the play and
    commit prohibitions an object keyed by card id stays an object, filtered),
    and any other value naming a hidden id becomes @null@;
  * the campaign's @meta.otherCampaignPlayers@ (full investigator data for the
    other half of a split campaign): each entry the viewer does not control
    gets the same zone clearing as above (no @handCount@ is added, so the shape
    is the stored one) and the same @meta@\/@formMeta@ scrub.

Surfaces justified as public:

  * campaign @decks@ and scenario @playerDecks@: the deck lists chosen at
    setup. They are not the live deck: the investigator's deck is shuffled from
    them at @SetupInvestigator@, so they reveal composition (public at the
    table), not order. They do map every card id to its card, though, so any
    channel that still carries another seat's hidden id reveals that card
    through them: every such channel must be closed, which is what the
    hidden-id filters above are for.

Known leak surfaces deliberately not touched here (follow-ups):

  * @question@ and @questionPresentation@ (a prompt can name hand or deck
    cards of the player it is for; TASK-2.22.5 P4);
  * the encounter deck order (P5) and shared scenario decks (TASK-2.22.6);
  * other id channels: @usedAbilities@ sources, entity and scenario @meta@,
    @searchFoundStrategy@, the campaign's @otherCampaignAttrs@, the viewer's
    own @meta@, and the websocket (TASK-2.22.2).
-}
module Api.Arkham.Redact (
  Viewer (..),
  RedactOptions (..),
  defaultRedactOptions,
  redactFor,
  redactWith,
  viewerControls,
  hiddenZoneKeys,
  investigatorMapKeys,
  hiddenCardIds,
) where

import Arkham.Id (PlayerId (PlayerId))
import Arkham.Log.Entry (LogAudience (Everyone, OnlyPlayer))
import Arkham.Modifier (ModifierType (TopCardOfDeckIsRevealed))
import Data.Aeson
import Data.Aeson.Key qualified as Key
import Data.Aeson.KeyMap qualified as KeyMap
import Data.List (partition)
import Data.Set qualified as Set
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
    let hidden = hiddenCardIds viewer (Object game)
     in Object
          . redactHiddenIds viewer hidden
          . adjust (redactLog viewer) "log"
          $ foldl' (\g k -> adjust (redactInvestigatorMap options viewer hidden) k g) game investigatorMapKeys
  other -> other

redactInvestigatorMap :: RedactOptions -> Viewer -> Set Text -> Value -> Value
redactInvestigatorMap options viewer hidden = \case
  Object investigators ->
    Object (KeyMap.map (redactInvestigator options viewer hidden) (KeyMap.filter isObject investigators))
  _ -> Object mempty
 where
  isObject = \case
    Object _ -> True
    _ -> False

redactInvestigator :: RedactOptions -> Viewer -> Set Text -> Value -> Value
redactInvestigator options viewer hidden = \case
  Object investigator ->
    let counted = if options.includeHandCount then withHandCount investigator else investigator
        controlled = controls viewer investigator
     in Object
          $ if controlled
            then scrubProhibitions hidden counted
            else scrubMetas hidden (hideZones counted)
  other -> other

controls :: Viewer -> Object -> Bool
controls viewer = maybe False (viewerControls viewer) . KeyMap.lookup "playerId"

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

-- * Hidden card ids (see the module docs)

{- | The card ids the viewer may not see anywhere in the game value. Computed
on the unredacted value, before 'hideZones' empties the zones that hold them.
-}
hiddenCardIds :: Viewer -> Value -> Set Text
hiddenCardIds viewer = \case
  Object game ->
    let investigators = allInvestigators game
        (mine, others) = partition (controls viewer) investigators
        ownIds = Set.fromList [i | o <- mine, Just (String i) <- [KeyMap.lookup "id" o]]
        scenarioSearch = [s | Just (Object s) <- [scenarioSearchOf game]]
        (ownSearches, otherSearches) = partition (searchedBy ownIds) scenarioSearch
        collected =
          concatMap hiddenZoneIds others
            <> concatMap (\s -> foldMap (cardIdsIn . (s `at`)) searchResultKeys) otherSearches
        visible =
          concatMap (cardIdsIn . Object . ownCards) mine
            <> concatMap (cardIdsIn . Object) ownSearches
            <> concatMap (\o -> foldMap allStrings (KeyMap.lookup "meta" o >>= revealedCards)) investigators
            <> concatMap (cardIdsIn . (`at` "discard")) investigators
            <> [i | k <- entityMapKeys, Object es <- [game `at` k], Object e <- KeyMap.elems es, String i <- [e `at` "cardId"]]
            <> cardIdsIn (game `at` "activeCard")
            <> cardIdsIn (game `at` "skillTest")
            <> ownQuestions game
     in Set.fromList collected `Set.difference` Set.fromList visible
  _ -> mempty
 where
  -- The viewer's own cards, but not what modifiers or meta name: those are
  -- where another seat's hidden card would show up.
  ownCards o = foldl' (flip KeyMap.delete) o ["modifiers", "meta", "formMeta"]
  revealedCards = \case
    Object meta -> KeyMap.lookup "revealedCards" meta
    _ -> Nothing
  ownQuestions game = case game `at` "question" of
    Object questions ->
      concat
        [ allStrings q
        | String p <- map toJSON (viewerSeats viewer)
        , Just q <- [KeyMap.lookup (Key.fromText p) questions]
        ]
    _ -> []

-- | Missing keys read as 'Null'.
at :: Object -> Key -> Value
at o k = fromMaybe Null (KeyMap.lookup k o)

-- | Every investigator object, including the campaign's other-half players.
allInvestigators :: Object -> [Object]
allInvestigators game =
  [inv | k <- investigatorMapKeys, Object invs <- [game `at` k], Object inv <- KeyMap.elems invs]
    <> [inv | Object players <- [otherCampaignPlayers game], Object inv <- KeyMap.elems players]

otherCampaignPlayers :: Object -> Value
otherCampaignPlayers game = case game `at` "mode" of
  Object mode
    | Object campaign <- mode `at` "This"
    , Object meta <- campaign `at` "meta" ->
        meta `at` "otherCampaignPlayers"
  _ -> Null

scenarioSearchOf :: Object -> Maybe Value
scenarioSearchOf game = case game `at` "mode" of
  Object mode | Object scenario <- mode `at` "That" -> KeyMap.lookup "search" scenario
  _ -> Nothing

searchedBy :: Set Text -> Object -> Bool
searchedBy iids search = case search `at` "searchInvestigator" of
  String iid -> iid `Set.member` iids
  _ -> False

searchResultKeys :: [Key]
searchResultKeys = ["searchFoundCards", "searchDrawnCards"]

-- | The top-level maps of in-play entities that carry @cardId@ and @modifiers@.
entityMapKeys :: [Key]
entityMapKeys =
  ["locations", "enemies", "assets", "acts", "agendas", "treacheries", "events", "stories", "skills"]

-- | The card ids in the zones 'hideZones' empties.
hiddenZoneIds :: Object -> [Text]
hiddenZoneIds investigator =
  foldMap (cardIdsIn . (investigator `at`)) ["hand", "sideDeck", "decks", "drawnCards", "cardsUnderneath", "drawing", "discarding"]
    <> deckIds
    <> [i | Array ids <- [investigator `at` "excludeFromMulligan"], String i <- toList ids]
    <> case investigator `at` "search" of
      Object search -> foldMap (cardIdsIn . (search `at`)) searchResultKeys
      _ -> []
 where
  deckIds = case investigator `at` "deck" of
    Array cards | topCardRevealed investigator -> cardIdsIn (Array (V.drop 1 cards))
    other -> cardIdsIn other

{- | The id of an encoded card: a 'Arkham.Card.Card' (@{tag, contents}@,
vengeance cards nesting another) or bare card contents (a @PlayerCard@ or
@EncounterCard@, which have both @id@ and @cardCode@). Any other object with
both keys is taken for a card too; that can only over-redact.
-}
cardIdOf :: Value -> Maybe Text
cardIdOf = \case
  Object o
    | String i <- o `at` "id"
    , KeyMap.member "cardCode" o ->
        Just i
    | String tag <- o `at` "tag"
    , tag `elem` ["PlayerCard", "EncounterCard", "VengeanceCard"] ->
        cardIdOf (o `at` "contents")
  _ -> Nothing

-- | Every card id anywhere inside a value.
cardIdsIn :: Value -> [Text]
cardIdsIn v = maybeToList (cardIdOf v) <> children
 where
  children = case v of
    Object o -> foldMap cardIdsIn (KeyMap.elems o)
    Array xs -> foldMap cardIdsIn xs
    _ -> []

-- | Every string and object key inside a value.
allStrings :: Value -> [Text]
allStrings = \case
  String s -> [s]
  Object o -> map Key.toText (KeyMap.keys o) <> foldMap allStrings (KeyMap.elems o)
  Array xs -> foldMap allStrings xs
  _ -> []

-- | Whether a value names a hidden id anywhere (string or object key).
mentions :: Set Text -> Value -> Bool
mentions hidden = any (`Set.member` hidden) . allStrings

isHiddenCard :: Set Text -> Value -> Bool
isHiddenCard hidden = maybe False (`Set.member` hidden) . cardIdOf

-- | Apply the hidden-id filters to the game, after the investigator redaction.
redactHiddenIds :: Viewer -> Set Text -> Object -> Object
redactHiddenIds viewer hidden game =
  adjust (redactMode viewer ownIds hidden) "mode" $ if Set.null hidden then game else filtered
 where
  ownIds =
    Set.fromList [i | inv <- allInvestigators game, controls viewer inv, String i <- [inv `at` "id"]]
  filtered =
    adjust (onObject (KeyMap.filterWithKey (\k _ -> not (Key.toText k `Set.member` hidden)))) "cards"
      . adjust dropCards "focusedCards"
      . adjust (onArray (V.filter (not . mentions hidden))) "highlightedCards"
      . adjust (onObject (KeyMap.map dropCards)) "foundCards"
      . adjust (redactModifierMap hidden) "modifiers"
      . adjust withModifiers "skillTest"
      . adjust (onArray (V.map withModifiers)) "focusedChaosTokens"
      . adjust (onArray (V.map withModifiers)) "skillTestChaosTokens"
      $ foldl' (\g k -> adjust (onObject (KeyMap.map withModifiers)) k g) game (investigatorMapKeys <> entityMapKeys)
  dropCards = onArray (V.filter (not . isHiddenCard hidden))
  withModifiers = onObject (adjust (redactModifiers hidden) "modifiers")

-- | The campaign's other-half players and another seat's scenario search.
redactMode :: Viewer -> Set Text -> Set Text -> Value -> Value
redactMode viewer ownIds hidden =
  onObject
    $ adjust (onObject (adjust (onObject (adjust otherPlayers "otherCampaignPlayers")) "meta")) "This"
    . adjust (onObject (adjust scenarioSearch "search")) "That"
 where
  otherPlayers = onObject . KeyMap.map . onObject $ \inv ->
    if controls viewer inv then inv else scrubMetas hidden (hideZones inv)
  scenarioSearch = onObject \search ->
    if searchedBy ownIds search
      then search
      else
        adjust searchCards "searchDrawnCards"
          $ adjust (onObject (KeyMap.map searchCards)) "searchFoundCards" search
  -- Another seat's search results: every card it found is in @hidden@ unless
  -- the viewer may see it anyway (e.g. their own question names it).
  searchCards = \case
    Array cards -> Array (V.filter (not . mentions hidden) cards)
    other -> emptied other

{- | Top-level @modifiers@ (a @Map Target [Modifier]@, encoded as an array of
@[target, modifiers]@ pairs; an object keyed by target is handled too).
-}
redactModifierMap :: Set Text -> Value -> Value
redactModifierMap hidden = \case
  Array entries -> Array (V.mapMaybe pair entries)
  Object o ->
    Object
      . KeyMap.mapMaybe remaining
      $ KeyMap.filterWithKey (\k _ -> not (Key.toText k `Set.member` hidden)) o
  other -> other
 where
  pair = \case
    Array xs
      | [target, mods] <- toList xs ->
          if mentions hidden target
            then Nothing
            else (\ms -> Array (V.fromList [target, ms])) <$> remaining mods
    other
      | mentions hidden other -> Nothing
      | otherwise -> Just other
  remaining mods = case redactModifiers hidden mods of
    Array ms | V.null ms -> Nothing
    ms -> Just ms

{- | A list of encoded 'Arkham.Modifier.Modifier's: a hidden @card@ becomes
@null@, then a modifier still naming a hidden id is dropped.
-}
redactModifiers :: Set Text -> Value -> Value
redactModifiers hidden = onArray (V.mapMaybe modifier)
 where
  modifier = \case
    Object m ->
      let m' = adjust (\c -> if isHiddenCard hidden c then Null else c) "card" m
       in if mentions hidden (Object m') then Nothing else Just (Object m')
    other
      | mentions hidden other -> Nothing
      | otherwise -> Just other

{- | The hidden-id scrub of an investigator the viewer does not control: its
@meta@ and @formMeta@ (see 'scrub') and its prohibitions (see
'scrubProhibitions').
-}
scrubMetas :: Set Text -> Object -> Object
scrubMetas hidden
  | Set.null hidden = id
  | otherwise =
      adjust (scrub hidden) "meta"
        . adjust (scrub hidden) "formMeta"
        . scrubProhibitions hidden

{- | The prohibition channels of any investigator, the viewer's own included:
@playProhibitions@ and @commitProhibitions@ (@[cardId, refs]@ pairs; an
object keyed by card id is handled too) and @cannotDiscoverClues@ and
@cannotSpendClues@ (@null@ or a list of log refs). An array entry (or keyed
prohibition) naming a hidden id anywhere goes; any other value naming one
becomes @null@. The viewer's own
cards are never in @hidden@, so only another seat's card (an @AsIfInHand@
one, say) is removed from the viewer's own lists.
-}
scrubProhibitions :: Set Text -> Object -> Object
scrubProhibitions hidden
  | Set.null hidden = id
  | otherwise =
      adjust prohibitions "playProhibitions"
        . adjust prohibitions "commitProhibitions"
        . adjust refs "cannotDiscoverClues"
        . adjust refs "cannotSpendClues"
 where
  prohibitions = \case
    Object o ->
      Object
        . KeyMap.filter (not . mentions hidden)
        $ KeyMap.filterWithKey (\k _ -> not (Key.toText k `Set.member` hidden)) o
    other -> refs other
  refs = \case
    Array entries -> Array (V.filter (not . mentions hidden) entries)
    other
      | mentions hidden other -> Null
      | otherwise -> other

{- | Remove hidden cards and ids from free-form JSON: from arrays they are
removed, as object values they become @null@, and object keys that are hidden
ids are dropped.
-}
scrub :: Set Text -> Value -> Value
scrub hidden = go
 where
  go v
    | isHidden v = Null
    | otherwise = case v of
        Object o ->
          Object . KeyMap.map go $ KeyMap.filterWithKey (\k _ -> not (Key.toText k `Set.member` hidden)) o
        Array xs -> Array (V.map go (V.filter (not . isHidden) xs))
        other -> other
  isHidden = \case
    String s -> s `Set.member` hidden
    v -> isHiddenCard hidden v

onObject :: (Object -> Object) -> Value -> Value
onObject f = \case
  Object o -> Object (f o)
  other -> other

onArray :: (V.Vector Value -> V.Vector Value) -> Value -> Value
onArray f = \case
  Array xs -> Array (f xs)
  other -> other

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

audienceVisible :: Viewer -> Value -> Bool
audienceVisible viewer audience =
  audience == toJSON Everyone
    || any (\p -> audience == toJSON (OnlyPlayer p)) (viewerSeats viewer)
