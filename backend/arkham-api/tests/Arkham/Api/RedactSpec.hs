module Arkham.Api.RedactSpec (spec) where

import Api.Arkham.Redact (Viewer (..), redactFor)
import Arkham.Classes.HasGame (getGame)
import Arkham.Investigator.Cards qualified as Investigators
import Arkham.Investigator.Deck (InvestigatorDeckKey (HunchDeck))
import Arkham.Log.Entry (
  LogAudience (OnlyPlayer),
  LogEntry (..),
  LogKind (Chat),
  LogPart (LogText),
  LogRow (LogRowLegacy, LogRowStructured),
  mkLogEntry,
 )
import Arkham.Matcher.Card (CardMatcher (AnyCard), ExtendedCardMatcher (BasicCardMatch))
import Arkham.Modifier (Modifier (..), ModifierType (TopCardOfDeckIsRevealed))
import Arkham.Search (Search (..), SearchType (Searching), mkSearch)
import Data.Aeson qualified as Aeson
import Data.Aeson.KeyMap qualified as KeyMap
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import TestImport

{- | Two seated investigators, each with every hidden zone populated, plus a
log tail with an Everyone row, an OnlyPlayer row per seat, a legacy row, and
an Everyone row holding an OnlyPlayer child for seat B.
-}
data Table = Table
  { tableA :: PlayerId
  , tableB :: PlayerId
  , tableGame :: PublicGame Text
  }

withTable :: (Table -> TestAppT ()) -> IO ()
withTable body = gameTest \jenny -> do
  daisy <- addInvestigator Investigators.daisyWalker
  for_ [jenny, daisy] populate
  let a = investigatorPlayerId (toAttrs jenny)
      b = investigatorPlayerId (toAttrs daisy)
  game <- getGame
  body Table {tableA = a, tableB = b, tableGame = PublicGame "game" "Redact" (logRows a b) game}
 where
  populate inv = do
    deck <- testPlayerCards 5
    hand <- map PlayerCard <$> testPlayerCards 3
    side <- testPlayerCards 2
    hunch <- map PlayerCard <$> testPlayerCards 2
    drawn <- testPlayerCards 1
    under <- map PlayerCard <$> testPlayerCards 1
    found <- map PlayerCard <$> testPlayerCards 2
    let searching =
          (mkSearch Searching inv inv inv [] (BasicCardMatch AnyCard) (DrawFound (toId inv) 1))
            { searchFoundCards = Map.singleton FromDeck found
            }
    updateInvestigator inv \attrs ->
      attrs
        { investigatorDeck = Deck deck
        , investigatorHand = hand
        , investigatorSideDeck = Just side
        , investigatorDecks = Map.singleton HunchDeck hunch
        , investigatorDrawnCards = drawn
        , investigatorCardsUnderneath = under
        , investigatorSearch = Just searching
        }

logRows :: PlayerId -> PlayerId -> [LogRow]
logRows a b =
  [ LogRowStructured (chat "everyone")
  , LogRowStructured (chat "only A") {logEntryAudience = OnlyPlayer a}
  , LogRowStructured (chat "only B") {logEntryAudience = OnlyPlayer b}
  , LogRowLegacy "legacy" Nothing
  , LogRowStructured
      (chat "parent") {logEntryChildren = [(chat "child B") {logEntryAudience = OnlyPlayer b}]}
  ]
 where
  chat t = mkLogEntry Chat [LogText t]

investigatorFor :: PlayerId -> Aeson.Value -> Aeson.Object
investigatorFor pid = \case
  Aeson.Object top
    | Just (Aeson.Object invs) <- KeyMap.lookup "investigators" top
    , [inv] <-
        [o | Aeson.Object o <- KeyMap.elems invs, KeyMap.lookup "playerId" o == Just (toJSON pid)] ->
        inv
  _ -> error "investigatorFor: no unique investigator for player"

field' :: Aeson.Key -> Aeson.Object -> Aeson.Value
field' k o = fromJustNote ("missing field " <> show k) (KeyMap.lookup k o)

arrayLength :: Aeson.Value -> Int
arrayLength = \case
  Aeson.Array xs -> length xs
  _ -> error "arrayLength: not an array"

searchField :: Aeson.Key -> Aeson.Object -> Aeson.Value
searchField k inv = case field' "search" inv of
  Aeson.Object s -> field' k s
  _ -> error "search is not an object"

logTexts :: Aeson.Value -> [Text]
logTexts = \case
  Aeson.Object top | Just (Aeson.Array rows) <- KeyMap.lookup "log" top -> concatMap rowTexts (toList rows)
  _ -> error "logTexts: no log"
 where
  rowTexts = \case
    Aeson.Object row -> case (KeyMap.lookup "tag" row, KeyMap.lookup "contents" row) of
      (Just "LogRowStructured", Just entry) -> entryTexts entry
      (Just "LogRowLegacy", _) -> ["legacy"]
      _ -> error "unexpected log row"
    _ -> error "unexpected log row"
  entryTexts = \case
    Aeson.Object e ->
      [ t
      | Just (Aeson.Array body) <- [KeyMap.lookup "body" e]
      , Aeson.Object p <- toList body
      , Just (Aeson.String t) <- [KeyMap.lookup "contents" p]
      ]
        <> case KeyMap.lookup "children" e of
          Just (Aeson.Array cs) -> concatMap entryTexts (toList cs)
          _ -> []
    _ -> []

-- | The hidden zones are what 'populate' put there, in order.
shouldKeepZones :: Aeson.Object -> Aeson.Object -> IO ()
shouldKeepZones original redacted =
  for_ ["hand", "deck", "sideDeck", "decks", "drawnCards", "cardsUnderneath", "search", "deckSize"] \k ->
    field' k redacted `shouldBe` field' k original

shouldHideZones :: Aeson.Object -> IO ()
shouldHideZones inv = do
  for_ ["hand", "deck", "sideDeck", "decks", "drawnCards", "cardsUnderneath"] \k ->
    (k, arrayLength (field' k inv)) `shouldBe` (k, 0)
  field' "drawing" inv `shouldBe` Aeson.Null
  field' "discarding" inv `shouldBe` Aeson.Null
  searchField "searchFoundCards" inv `shouldBe` Aeson.Object mempty
  searchField "searchDrawnCards" inv `shouldBe` Aeson.Array mempty

spec :: Spec
spec = describe "Api.Arkham.Redact.redactFor" do
  it "keeps the viewer's own zones in order and empties the other seat's (a)" $ withTable \Table {..} -> liftIO do
    let original = toJSON tableGame
        redacted = redactFor (ViewerSeats (Set.singleton tableA)) original
        ownBefore = investigatorFor tableA original
        own = investigatorFor tableA redacted
        other = investigatorFor tableB redacted
        otherBefore = investigatorFor tableB original
    -- The fixture really populated every zone that is about to be hidden.
    for_ ["hand", "deck", "sideDeck", "decks", "drawnCards", "cardsUnderneath"] \k ->
      (k, arrayLength (field' k otherBefore) > 0) `shouldBe` (k, True)
    searchField "searchFoundCards" otherBefore `shouldNotBe` Aeson.Object mempty
    shouldKeepZones ownBefore own
    shouldHideZones other
    field' "deckSize" other `shouldBe` field' "deckSize" (investigatorFor tableB original)
    field' "deckSize" other `shouldBe` toJSON (5 :: Int)
    field' "handCount" own `shouldBe` toJSON (3 :: Int)
    field' "handCount" other `shouldBe` toJSON (3 :: Int)
    -- Nothing else on the investigator changes.
    KeyMap.delete "handCount" own `shouldBe` ownBefore

  it "shows everything to a viewer holding both seats (b)" $ withTable \Table {..} -> liftIO do
    let original = toJSON tableGame
        redacted = redactFor (ViewerSeats (Set.fromList [tableA, tableB])) original
    for_ [tableA, tableB] \pid -> do
      let inv = investigatorFor pid redacted
      KeyMap.delete "handCount" inv `shouldBe` investigatorFor pid original
      field' "handCount" inv `shouldBe` toJSON (3 :: Int)
    logTexts redacted `shouldBe` logTexts original

  it "shows a spectator no hidden zones and only Everyone log rows (c)" $ withTable \Table {..} -> liftIO do
    let redacted = redactFor ViewerSpectator (toJSON tableGame)
    for_ [tableA, tableB] (shouldHideZones . (`investigatorFor` redacted))
    logTexts redacted `shouldBe` ["everyone", "legacy", "parent"]

  it "keeps OnlyPlayer log rows for the viewer's seat and drops the others' (d)" $ withTable \Table {..} -> liftIO do
    logTexts (toJSON tableGame) `shouldBe` ["everyone", "only A", "only B", "legacy", "parent", "child B"]
    logTexts (redactFor (ViewerSeats (Set.singleton tableA)) (toJSON tableGame))
      `shouldBe` ["everyone", "only A", "legacy", "parent"]
    logTexts (redactFor (ViewerSeats (Set.singleton tableB)) (toJSON tableGame))
      `shouldBe` ["everyone", "only B", "legacy", "parent", "child B"]

  it "gives the same result through toJSON and the toEncoding wire path (e)" $ withTable \Table {..} -> liftIO do
    let wire = fromJustNote "PublicGame encoding did not parse" (Aeson.decode (Aeson.encode tableGame))
    for_ [ViewerSpectator, ViewerSeats (Set.singleton tableA), ViewerSeats (Set.fromList [tableA, tableB])] \v ->
      redactFor v wire `shouldBe` redactFor v (toJSON tableGame)

  it "keeps only the top card of a deck whose top card is revealed (f)" $ withTable \Table {..} -> do
    game <- getGame
    let daisy =
          fromJustNote "no investigator for seat B"
            $ find ((== tableB) . investigatorPlayerId . toAttrs) (toList $ game ^. entitiesL . investigatorsL)
        revealed = Modifier (toSource daisy) TopCardOfDeckIsRevealed False Nothing Nothing
    overTest \g -> g {gameModifiers = Map.insertWith (<>) (toTarget daisy) [revealed] (gameModifiers g)}
    game' <- getGame
    liftIO do
      let original = toJSON (PublicGame ("game" :: Text) "Redact" (logRows tableA tableB) game')
          redacted = redactFor (ViewerSeats (Set.singleton tableA)) original
          other = investigatorFor tableB redacted
      case field' "deck" (investigatorFor tableB original) of
        Aeson.Array cards -> field' "deck" other `shouldBe` Aeson.Array (take 1 cards)
        _ -> expectationFailure "deck is not an array"
      field' "deckSize" other `shouldBe` toJSON (5 :: Int)
      arrayLength (field' "hand" other) `shouldBe` 0
      -- Seat A has no such modifier, so a spectator still sees none of A's deck.
      arrayLength (field' "deck" (investigatorFor tableA (redactFor ViewerSpectator original))) `shouldBe` 0

  it "is idempotent (g)" $ withTable \Table {..} -> liftIO do
    let original = toJSON tableGame
    for_ [ViewerSpectator, ViewerSeats (Set.singleton tableA), ViewerSeats (Set.fromList [tableA, tableB])] \v ->
      redactFor v (redactFor v original) `shouldBe` redactFor v original
