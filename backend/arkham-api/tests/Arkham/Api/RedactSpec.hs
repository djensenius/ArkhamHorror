module Arkham.Api.RedactSpec (spec) where

import Api.Arkham.Redact (RedactOptions (..), Viewer (..), defaultRedactOptions, investigatorMapKeys, redactWith)
import Api.Arkham.Redact qualified as Redact
import Arkham.Campaign (lookupCampaign)
import Arkham.Campaign.Types (CampaignAttrs (..))
import Arkham.CampaignLog (setCampaignLogRecorded)
import Arkham.CampaignLogKey (CampaignLogKey (KilledInvestigators), recorded)
import Arkham.Classes.HasGame (getGame)
import Arkham.Deck qualified as Deck
import Arkham.Difficulty (Difficulty (Easy))
import Arkham.Discard (
  DiscardDestination (ToDiscardPile),
  DiscardStrategy (DiscardChoose),
  HandDiscard (..),
 )
import Arkham.Draw.Types (CardDraw (..), newCardDraw)
import Arkham.Investigator.Cards qualified as Investigators
import Arkham.Investigator.Deck (InvestigatorDeckKey (HunchDeck))
import Arkham.Log.Entry (
  LogAudience (Everyone, OnlyPlayer),
  LogEntry (..),
  LogKind (Chat),
  LogPart (LogText),
  LogRow (LogRowLegacy, LogRowStructured),
  mkLogEntry,
 )
import Arkham.Matcher.Card (CardMatcher (AnyCard), ExtendedCardMatcher (BasicCardMatch))
import Arkham.Modifier (Modifier (..), ModifierType (AsIfInHand, CannotBeFlipped, CannotPlay, TopCardOfDeckIsRevealed))
import Arkham.Scenario.Types (ScenarioAttrs (..))
import Arkham.Search (Search (..), SearchType (Searching), mkSearch)
import Data.Aeson qualified as Aeson
import Data.Aeson.Key qualified as Key
import Data.Aeson.KeyMap qualified as KeyMap
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Data.UUID (fromWords, nil)
import TestImport

{- | A running campaign scenario (Night of the Zealot, The Gathering) with:

  * two seated investigators, A (Jenny) and B (Daisy);
  * C (Agnes), playing the other half of a split campaign, in
    @otherInvestigators@ with her own seat;
  * D (Wendy), retired, in @retiredInvestigators@ with her own seat;
  * Skids, recorded as killed, in @killedInvestigators@ (no seat: the encoder
    rebuilds him with the nil player id);

where A to D each have every hidden zone populated with real engine values,
plus a log tail with an Everyone row, an OnlyPlayer row per seat, a legacy
row, and an Everyone row holding an OnlyPlayer child for seat B.
-}
data Table = Table
  { tableA :: PlayerId
  , tableB :: PlayerId
  , tableC :: PlayerId
  , tableD :: PlayerId
  , tableGame :: PublicGame Text
  }

withTable :: (Table -> TestAppT ()) -> IO ()
withTable body = gameTest \jenny -> do
  daisy <- addInvestigator Investigators.daisyWalker
  for_ [jenny, daisy] \inv -> do
    attrs <- populated (toAttrs inv)
    updateInvestigator inv (const attrs)
  agnes <- populated . toAttrs =<< testInvestigator Investigators.agnesBaker
  wendyBase <- testInvestigator Investigators.wendyAdams
  wendy <- populated (toAttrs wendyBase)
  let
    campaign =
      overAttrs
        ( \c ->
            c
              { campaignMeta =
                  Aeson.object
                    ["otherCampaignPlayers" .= Map.singleton (investigatorPlayerId agnes) agnes]
              , campaignLog =
                  setCampaignLogRecorded
                    KilledInvestigators
                    [recorded (CardCode "01003")]
                    (campaignLog c)
              }
        )
        (lookupCampaign (CampaignId "01") Easy)
  overTest \g ->
    g
      { gameMode = case gameMode g of
          That s -> These campaign s
          other -> other
      , gameRetiredInvestigators =
          Map.singleton (toId wendyBase) (overAttrs (const wendy) wendyBase)
      }
  let a = investigatorPlayerId (toAttrs jenny)
      b = investigatorPlayerId (toAttrs daisy)
  game <- getGame
  body
    Table
      { tableA = a
      , tableB = b
      , tableC = investigatorPlayerId agnes
      , tableD = investigatorPlayerId wendy
      , tableGame = PublicGame "game" "Redact" (logRows a b) game
      }

-- | Every hidden zone filled: 5 deck cards, 3 hand cards, and so on.
populated :: InvestigatorAttrs -> TestAppT InvestigatorAttrs
populated attrs = do
  deck <- testPlayerCards 5
  hand <- map PlayerCard <$> testPlayerCards 3
  side <- testPlayerCards 2
  hunch <- map PlayerCard <$> testPlayerCards 2
  drawn <- testPlayerCards 1
  under <- map PlayerCard <$> testPlayerCards 1
  found <- map PlayerCard <$> testPlayerCards 2
  searchDrawn <- map PlayerCard <$> testPlayerCards 1
  drawing <- map PlayerCard <$> testPlayerCards 1
  discarding <- map PlayerCard <$> testPlayerCards 1
  let
    iid = investigatorId attrs
    searching =
      (mkSearch Searching iid attrs attrs [] (BasicCardMatch AnyCard) (DrawFound iid 1))
        { searchFoundCards = Map.singleton FromDeck found
        , searchDrawnCards = searchDrawn
        }
    cardDraw =
      (newCardDraw attrs (Deck.InvestigatorDeck iid) 1) {cardDrawAlreadyDrawn = drawing}
    handDiscard =
      HandDiscard
        { discardStrategy = DiscardChoose
        , discardInvestigator = iid
        , discardSource = toSource attrs
        , discardTarget = Nothing
        , discardFilter = AnyCard
        , discardAmount = 1
        , discardThen = Nothing
        , discardDestination = ToDiscardPile
        , discardBatchCards = discarding
        }
  pure
    attrs
      { investigatorDeck = Deck deck
      , investigatorHand = hand
      , investigatorSideDeck = Just side
      , investigatorDecks = Map.singleton HunchDeck hunch
      , investigatorDrawnCards = drawn
      , investigatorCardsUnderneath = under
      , investigatorSearch = Just searching
      , investigatorDrawing = Just cardDraw
      , investigatorDiscarding = Just handDiscard
      , investigatorExcludeFromMulligan = map toCardId hand
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
investigatorFor = investigatorIn "investigators"

investigatorIn :: Aeson.Key -> PlayerId -> Aeson.Value -> Aeson.Object
investigatorIn mapKey pid = \case
  Aeson.Object top
    | Just (Aeson.Object invs) <- KeyMap.lookup mapKey top
    , [inv] <-
        [o | Aeson.Object o <- KeyMap.elems invs, KeyMap.lookup "playerId" o == Just (toJSON pid)] ->
        inv
  _ -> error $ "investigatorIn: no unique investigator for player in " <> show mapKey

-- | The only investigator in a map, for the seatless killed investigator.
onlyInvestigatorIn :: Aeson.Key -> Aeson.Value -> Aeson.Object
onlyInvestigatorIn mapKey = \case
  Aeson.Object top
    | Just (Aeson.Object invs) <- KeyMap.lookup mapKey top
    , [Aeson.Object inv] <- KeyMap.elems invs ->
        inv
  _ -> error $ "onlyInvestigatorIn: expected exactly one investigator in " <> show mapKey

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

-- | Every zone 'populated' fills, which must survive for the controller.
hiddenFields :: [Aeson.Key]
hiddenFields =
  [ "hand"
  , "deck"
  , "sideDeck"
  , "decks"
  , "drawnCards"
  , "cardsUnderneath"
  , "excludeFromMulligan"
  , "search"
  , "drawing"
  , "discarding"
  , "deckSize"
  ]

-- | The hidden zones are what 'populated' put there, in order.
shouldKeepZones :: Aeson.Object -> Aeson.Object -> IO ()
shouldKeepZones original redacted =
  for_ hiddenFields \k -> field' k redacted `shouldBe` field' k original

-- | The original really carries something in every zone about to be hidden.
shouldBePopulated :: Aeson.Object -> IO ()
shouldBePopulated inv = do
  for_ arrayZones \k -> (k, arrayLength (field' k inv) > 0) `shouldBe` (k, True)
  for_ ["drawing", "discarding"] \k -> (k, field' k inv /= Aeson.Null) `shouldBe` (k, True)
  searchField "searchFoundCards" inv `shouldNotBe` Aeson.Object mempty
  arrayLength (searchField "searchDrawnCards" inv) `shouldBe` 1
  case field' "drawing" inv of
    Aeson.Object d -> arrayLength (field' "cardDrawAlreadyDrawn" d) `shouldBe` 1
    _ -> expectationFailure "drawing is not an object"
  case field' "discarding" inv of
    Aeson.Object d -> arrayLength (field' "discardBatchCards" d) `shouldBe` 1
    _ -> expectationFailure "discarding is not an object"

arrayZones :: [Aeson.Key]
arrayZones =
  ["hand", "deck", "sideDeck", "decks", "drawnCards", "cardsUnderneath", "excludeFromMulligan"]

shouldHideZones :: Aeson.Object -> IO ()
shouldHideZones inv = do
  for_ arrayZones \k -> (k, arrayLength (field' k inv)) `shouldBe` (k, 0)
  field' "drawing" inv `shouldBe` Aeson.Null
  field' "discarding" inv `shouldBe` Aeson.Null
  searchField "searchFoundCards" inv `shouldBe` Aeson.Object mempty
  searchField "searchDrawnCards" inv `shouldBe` Aeson.Array mempty

-- | Drop @handCount@ from every investigator in every investigator map.
stripHandCount :: Aeson.Value -> Aeson.Value
stripHandCount = \case
  Aeson.Object top ->
    Aeson.Object $ KeyMap.mapWithKey (\k v -> if k `elem` investigatorMapKeys then strip v else v) top
  other -> other
 where
  strip = \case
    Aeson.Object invs -> Aeson.Object (KeyMap.map dropCount invs)
    other -> other
  dropCount = \case
    Aeson.Object inv -> Aeson.Object (KeyMap.delete "handCount" inv)
    other -> other

allViewers :: Table -> [Viewer]
allViewers Table {..} =
  [ ViewerSpectator
  , ViewerSeats (Set.singleton tableA)
  , ViewerSeats (Set.fromList [tableA, tableB])
  , ViewerSeats (Set.singleton tableC)
  ]

withTopCardRevealed :: Table -> PlayerId -> TestAppT Aeson.Value
withTopCardRevealed Table {..} pid = do
  game <- getGame
  let inv =
        fromJustNote "no investigator for seat"
          $ find
            ((== pid) . investigatorPlayerId . toAttrs)
            (toList $ game ^. entitiesL . investigatorsL)
      revealed = Modifier (toSource inv) TopCardOfDeckIsRevealed False Nothing Nothing
  overTest \g ->
    g {gameModifiers = Map.insertWith (<>) (toTarget inv) [revealed] (gameModifiers g)}
  game' <- getGame
  pure $ toJSON (PublicGame ("game" :: Text) "Redact" (logRows tableA tableB) game')

{- | The cases below pin the @handCount@ shape TASK-2.22.3 will ship, so they
run with it on; the last case pins that the default leaves it out.
-}
redactFor :: Viewer -> Aeson.Value -> Aeson.Value
redactFor = redactWith RedactOptions {includeHandCount = True}

{- | Seat B's hidden cards placed, on purpose, on every surface the hidden-id
filter covers (TASK-2.22.5 P3):

  * @h0@ (B's hand) is listed in A's @meta.revealedCards@, so it is public;
  * @h1@ (B's hand) is in @cards@, @focusedCards@, @foundCards@, the @card@ of
    a modifier on A, B's @meta@, and Agnes's (seat C, other campaign half)
    stored @meta@;
  * @h2@ (B's hand) is in @cards@, B's scenario-level search, an
    @AsIfInHand@ modifier on A, the target of a top-level modifier, and B's
    @formMeta@ (also as an object key in @meta@);
  * @d0@ (B's deck) is in @cards@ and in B's @meta@ as a bare id;
  * @pub@ is a card in no hidden zone, sitting next to them everywhere.
-}
data Planted = Planted
  { plantedGame :: Aeson.Value
  , h0, h1, h2, d0, pub :: Text
  , agnesKey :: Aeson.Key
  }

plantHidden :: Table -> TestAppT Planted
plantHidden Table {..} = do
  game <- getGame
  let seated pid =
        fromJustNote "no investigator for seat"
          $ find ((== pid) . investigatorPlayerId . toAttrs) (toList $ game ^. entitiesL . investigatorsL)
      jenny = seated tableA
      daisy = seated tableB
      attrsB = toAttrs daisy
  (c0, c1, c2) <- case investigatorHand attrsB of
    [x, y, z] -> pure (x, y, z)
    _ -> error "plantHidden: expected three hand cards"
  deckTop <- case investigatorDeck attrsB of
    Deck (x : _) -> pure (PlayerCard x)
    _ -> error "plantHidden: empty deck"
  public <- PlayerCard <$> testPlayerCard id
  let modifierOn card ty = Modifier (toSource jenny) ty False card Nothing
      bSearch =
        ( mkSearch Searching (toId daisy) attrsB attrsB [] (BasicCardMatch AnyCard) (DrawFound (toId daisy) 1)
        )
          { searchFoundCards = Map.singleton FromDeck [c2]
          , searchDrawnCards = [c2]
          }
      withSearch = overAttrs (\s -> s {scenarioSearch = Just bSearch})
      agnesMeta = \case
        Aeson.Object meta
          | Just players <- KeyMap.lookup "otherCampaignPlayers" meta
          , Aeson.Success ps <- decodePlayers players ->
              Aeson.Object
                $ KeyMap.insert
                  "otherCampaignPlayers"
                  (toJSON (Map.map (\a -> a {investigatorMeta = Aeson.object ["secret" .= c1]}) ps))
                  meta
        _ -> error "plantHidden: no otherCampaignPlayers"
      decodePlayers :: Aeson.Value -> Aeson.Result (Map PlayerId InvestigatorAttrs)
      decodePlayers = Aeson.fromJSON
  updateInvestigator jenny \a ->
    a {investigatorMeta = Aeson.object ["revealedCards" .= Map.singleton (toId daisy) [toCardId c0]]}
  updateInvestigator daisy \a ->
    a
      { investigatorMeta =
          Aeson.object
            [ "secret" .= c1
            , "ids" .= [toCardId deckTop, toCardId public]
            , "byId" .= Map.singleton (toCardId c2) True
            , "revealed" .= toCardId c0
            , "class" .= ("Seeker" :: Text)
            ]
      , investigatorFormMeta = toJSON [c2, public]
      }
  overTest \g ->
    g
      { gameCards =
          Map.fromList [(toCardId c, c) | c <- [c0, c1, c2, deckTop, public] <> investigatorHand (toAttrs jenny)]
            <> gameCards g
      , gameFocusedCards = [[c1, public]]
      , gameFoundCards = Map.singleton FromDeck [c1, public]
      , gameModifiers =
          Map.insertWith
            (<>)
            (toTarget jenny)
            [ modifierOn (Just c1) CannotBeFlipped
            , modifierOn Nothing (AsIfInHand c2)
            , modifierOn (Just public) CannotBeFlipped
            ]
            $ Map.insert (CardIdTarget (toCardId c2)) [modifierOn Nothing CannotBeFlipped] (gameModifiers g)
      , gameMode = case gameMode g of
          These c s -> These (overAttrs (\ca -> ca {campaignMeta = agnesMeta (campaignMeta ca)}) c) (withSearch s)
          other -> other
      }
  game' <- getGame
  let idText c = case toJSON (toCardId c) of
        Aeson.String t -> t
        _ -> error "card id is not a string"
  pure
    Planted
      { plantedGame = toJSON (PublicGame ("game" :: Text) "Redact" (logRows tableA tableB) game')
      , h0 = idText c0
      , h1 = idText c1
      , h2 = idText c2
      , d0 = idText deckTop
      , pub = idText public
      , agnesKey = Key.fromText (idTextOf (toJSON tableC))
      }
 where
  idTextOf = \case
    Aeson.String t -> t
    _ -> error "player id is not a string"

topKey :: Aeson.Key -> Aeson.Value -> Aeson.Value
topKey k = \case
  Aeson.Object o -> field' k o
  _ -> error "topKey: not an object"

registryKeys :: Aeson.Value -> [Text]
registryKeys v = case topKey "cards" v of
  Aeson.Object cards -> map Key.toText (KeyMap.keys cards)
  _ -> error "cards is not an object"

-- | Card ids of an array of encoded cards.
cardIdsOf :: Aeson.Value -> [Text]
cardIdsOf = \case
  Aeson.Array cards ->
    [ i
    | Aeson.Object c <- toList cards
    , Just (Aeson.Object contents) <- [KeyMap.lookup "contents" c]
    , Just (Aeson.String i) <- [KeyMap.lookup "id" contents]
    ]
  _ -> error "cardIdsOf: not an array"

-- | The card ids of each zone of a @Map Zone [Card]@, in key order.
zoneCards :: Aeson.Value -> [[Text]]
zoneCards = \case
  Aeson.Object zones -> map cardIdsOf (KeyMap.elems zones)
  _ -> error "zoneCards: not an object"

zoneKeys :: Aeson.Value -> [Aeson.Key]
zoneKeys = \case
  Aeson.Object zones -> KeyMap.keys zones
  _ -> error "zoneKeys: not an object"

-- | Every string and object key in a value.
stringsIn :: Aeson.Value -> [Text]
stringsIn = \case
  Aeson.String s -> [s]
  Aeson.Object o -> map Key.toText (KeyMap.keys o) <> concatMap stringsIn (KeyMap.elems o)
  Aeson.Array xs -> concatMap stringsIn (toList xs)
  _ -> []

scenarioSearchOf :: Aeson.Value -> Aeson.Object
scenarioSearchOf v = case topKey "mode" v of
  Aeson.Object mode
    | Just (Aeson.Object scenario) <- KeyMap.lookup "That" mode
    , Just (Aeson.Object s) <- KeyMap.lookup "search" scenario ->
        s
  _ -> error "no scenario search"

agnesStored :: Aeson.Key -> Aeson.Value -> Aeson.Object
agnesStored key v = case topKey "mode" v of
  Aeson.Object mode
    | Just (Aeson.Object campaign) <- KeyMap.lookup "This" mode
    , Just (Aeson.Object meta) <- KeyMap.lookup "meta" campaign
    , Just (Aeson.Object players) <- KeyMap.lookup "otherCampaignPlayers" meta
    , Just (Aeson.Object agnes) <- KeyMap.lookup key players ->
        agnes
  _ -> error "no otherCampaignPlayers entry"

-- | The object key a player id encodes to.
playerKey :: PlayerId -> Aeson.Key
playerKey pid = case toJSON pid of
  Aeson.String t -> Key.fromText t
  _ -> error "player id is not a string"

-- | Replace a top-level field of the game value.
setTop :: Aeson.Key -> Aeson.Value -> Aeson.Value -> Aeson.Value
setTop k v = \case
  Aeson.Object o -> Aeson.Object (KeyMap.insert k v o)
  _ -> error "setTop: not an object"

-- | Change the seated investigator of a player in @investigators@.
overSeat :: PlayerId -> (Aeson.Object -> Aeson.Object) -> Aeson.Value -> Aeson.Value
overSeat pid f = \case
  Aeson.Object top
    | Just (Aeson.Object invs) <- KeyMap.lookup "investigators" top ->
        Aeson.Object
          $ KeyMap.insert "investigators" (Aeson.Object (KeyMap.map seat invs)) top
  _ -> error "overSeat: no investigators"
 where
  seat = \case
    Aeson.Object inv | KeyMap.lookup "playerId" inv == Just (toJSON pid) -> Aeson.Object (f inv)
    other -> other

-- | The card ids that key a list of @[cardId, refs]@ prohibition pairs.
prohibitionIds :: Aeson.Value -> [Text]
prohibitionIds = \case
  Aeson.Array entries -> [i | Aeson.Array e <- toList entries, Aeson.String i : _ <- [toList e]]
  _ -> error "prohibitions are not an array"

spec :: Spec
spec = describe "Api.Arkham.Redact.redactFor" do
  it "adds no handCount by default, and otherwise redacts the same way" $ withTable \table@Table {..} -> liftIO do
    defaultRedactOptions `shouldBe` RedactOptions {includeHandCount = False}
    let original = toJSON tableGame
    for_ (allViewers table) \v -> do
      let plain = Redact.redactFor v original
      for_ [tableA, tableB] \pid ->
        KeyMap.member "handCount" (investigatorFor pid plain) `shouldBe` False
      plain `shouldBe` stripHandCount (redactFor v original)
      Redact.redactFor v plain `shouldBe` plain

  it "keeps the viewer's own zones in order and empties the other seat's (a)" $ withTable \Table {..} -> liftIO do
    let original = toJSON tableGame
        redacted = redactFor (ViewerSeats (Set.singleton tableA)) original
        ownBefore = investigatorFor tableA original
        own = investigatorFor tableA redacted
        other = investigatorFor tableB redacted
        otherBefore = investigatorFor tableB original
    -- The fixture really populated every zone that is about to be hidden.
    shouldBePopulated otherBefore
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

  it "keeps only the top card of a deck whose top card is revealed (f)" $ withTable \table@Table {..} -> do
    original <- withTopCardRevealed table tableB
    liftIO do
      let redacted = redactFor (ViewerSeats (Set.singleton tableA)) original
          other = investigatorFor tableB redacted
      case field' "deck" (investigatorFor tableB original) of
        Aeson.Array cards -> do
          length cards `shouldBe` 5
          field' "deck" other `shouldBe` Aeson.Array (take 1 cards)
        _ -> expectationFailure "deck is not an array"
      field' "deckSize" other `shouldBe` toJSON (5 :: Int)
      arrayLength (field' "hand" other) `shouldBe` 0

  it "still empties B's whole deck when only A's top card is revealed (f2)" $ withTable \table@Table {..} -> do
    original <- withTopCardRevealed table tableA
    liftIO do
      let redacted = redactFor ViewerSpectator original
      case field' "deck" (investigatorFor tableA original) of
        Aeson.Array cards ->
          field' "deck" (investigatorFor tableA redacted) `shouldBe` Aeson.Array (take 1 cards)
        _ -> expectationFailure "deck is not an array"
      arrayLength (field' "deck" (investigatorFor tableB original)) `shouldBe` 5
      arrayLength (field' "deck" (investigatorFor tableB redacted)) `shouldBe` 0

  it "is idempotent (g)" $ withTable \table -> liftIO do
    let original = toJSON (tableGame table)
    for_ (allViewers table) \v ->
      redactFor v (redactFor v original) `shouldBe` redactFor v original

  it "redacts otherInvestigators, retiredInvestigators and killedInvestigators too (h)" $ withTable \Table {..} -> liftIO do
    let original = toJSON tableGame
        asA = redactFor (ViewerSeats (Set.singleton tableA)) original
        asC = redactFor (ViewerSeats (Set.singleton tableC)) original
    -- Agnes (seat C) in otherInvestigators, Wendy (seat D) in retiredInvestigators.
    for_ [("otherInvestigators", tableC), ("retiredInvestigators", tableD)] \(mapKey, pid) -> do
      shouldBePopulated (investigatorIn mapKey pid original)
      shouldHideZones (investigatorIn mapKey pid asA)
      field' "handCount" (investigatorIn mapKey pid asA) `shouldBe` toJSON (3 :: Int)
    -- Seat C sees its own other-campaign investigator in full.
    shouldKeepZones
      (investigatorIn "otherInvestigators" tableC original)
      (investigatorIn "otherInvestigators" tableC asC)
    -- Seat C does not see seats A, B or D.
    shouldHideZones (investigatorFor tableA asC)
    shouldHideZones (investigatorIn "retiredInvestigators" tableD asC)
    -- Killed Skids has no seat (nil player id), so nobody controls him.
    let killed = onlyInvestigatorIn "killedInvestigators" asA
    field' "handCount" killed `shouldBe` toJSON (0 :: Int)
    arrayLength (field' "hand" killed) `shouldBe` 0

  it "pins the four investigator maps as objects keyed by investigator id, on both encoders (i)" $ withTable \table@Table {..} -> liftIO do
    let wire = fromJustNote "PublicGame encoding did not parse" (Aeson.decode (Aeson.encode tableGame))
    for_ [toJSON tableGame, wire] \encoded -> case encoded of
      Aeson.Object top -> for_ investigatorMapKeys \mapKey -> case KeyMap.lookup mapKey top of
        Just (Aeson.Object invs) -> do
          (mapKey, KeyMap.null invs) `shouldBe` (mapKey, False)
          for_ (KeyMap.toList invs) \(k, inv) -> case inv of
            Aeson.Object o -> do
              (mapKey, KeyMap.lookup "id" o) `shouldBe` (mapKey, Just (Aeson.String (Key.toText k)))
              (mapKey, KeyMap.member "playerId" o) `shouldBe` (mapKey, True)
            _ -> expectationFailure $ show mapKey <> " holds a non-object investigator"
          -- Redaction keeps every investigator under its key.
          for_ (allViewers table) \v -> case redactFor v encoded of
            Aeson.Object r | Just (Aeson.Object rinvs) <- KeyMap.lookup mapKey r ->
              KeyMap.keys rinvs `shouldBe` KeyMap.keys invs
            _ -> expectationFailure $ show mapKey <> " lost its object shape"
        _ -> expectationFailure $ show mapKey <> " is not an object"
      _ -> expectationFailure "PublicGame is not an object"

  it "treats an investigator with no playerId as uncontrolled, and fails closed on odd map shapes (j)" do
    let pid = PlayerId nil
        game =
          Aeson.object
            [ "investigators"
                .= Aeson.object
                  [ "seatless" .= Aeson.object ["hand" .= [1 :: Int, 2], "deck" .= [3 :: Int]]
                  , "broken" .= Aeson.String "not an investigator"
                  ]
            , "otherInvestigators" .= [Aeson.object ["playerId" .= pid, "hand" .= [1 :: Int]]]
            ]
        redacted = redactFor (ViewerSeats (Set.singleton pid)) game
    redacted
      `shouldBe` Aeson.object
        [ "investigators"
            .= Aeson.object
              [ "seatless"
                  .= Aeson.object
                    ["hand" .= ([] :: [Int]), "deck" .= ([] :: [Int]), "handCount" .= (2 :: Int)]
              ]
        , "otherInvestigators" .= Aeson.object []
        ]

  it "treats the nil playerId sentinel as uncontrolled, even for a viewer seated as nil (l)" do
    let nilSeat = PlayerId nil
        realSeat = PlayerId (fromWords 0 0 0 1)
        investigator pid =
          Aeson.object ["playerId" .= pid, "hand" .= [1 :: Int, 2], "deck" .= [3 :: Int]]
        hidden pid =
          Aeson.object
            [ "playerId" .= pid
            , "hand" .= ([] :: [Int])
            , "deck" .= ([] :: [Int])
            , "handCount" .= (2 :: Int)
            ]
        shown pid =
          Aeson.object
            ["playerId" .= pid, "hand" .= [1 :: Int, 2], "deck" .= [3 :: Int], "handCount" .= (2 :: Int)]
        onlyNil =
          toJSON
            (LogRowStructured (mkLogEntry Chat [LogText "only nil"]) {logEntryAudience = OnlyPlayer nilSeat})
        game =
          Aeson.object
            [ "investigators"
                .= Aeson.object ["seatless" .= investigator nilSeat, "seated" .= investigator realSeat]
            , "killedInvestigators" .= Aeson.object ["killed" .= investigator nilSeat]
            , "log" .= [onlyNil]
            ]
        expected seatedView =
          Aeson.object
            [ "investigators"
                .= Aeson.object ["seatless" .= hidden nilSeat, "seated" .= seatedView realSeat]
            , "killedInvestigators" .= Aeson.object ["killed" .= hidden nilSeat]
            , "log" .= ([] :: [Aeson.Value])
            ]
    -- (a) A viewer whose only seat is the nil player id controls nothing.
    redactFor (ViewerSeats (Set.singleton nilSeat)) game `shouldBe` expected hidden
    -- (b) A viewer holding a real seat plus the nil id sees only the real seat's
    -- zones; the same holds for an ordinary viewer and a spectator.
    redactFor (ViewerSeats (Set.fromList [realSeat, nilSeat])) game `shouldBe` expected shown
    redactFor (ViewerSeats (Set.singleton realSeat)) game `shouldBe` expected shown
    redactFor ViewerSpectator game `shouldBe` expected hidden

  it "fails closed on malformed structured log rows (k)" do
    let everyone = Aeson.object ["tag" .= ("Everyone" :: Text)]
        entry t = Aeson.object ["audience" .= everyone, "body" .= [t :: Text]]
        structured c = Aeson.object ["tag" .= ("LogRowStructured" :: Text), "contents" .= c]
        legacy = Aeson.object ["tag" .= ("LogRowLegacy" :: Text), "contents" .= ("x" :: Text, Aeson.Null)]
        game =
          Aeson.object
            [ "log"
                .= [ structured (entry "kept")
                   , Aeson.object ["tag" .= ("LogRowStructured" :: Text)]
                   , structured (Aeson.String "not an entry")
                   , structured (Aeson.object ["body" .= ["no audience" :: Text]])
                   , legacy
                   ]
            ]
    toJSON Everyone `shouldBe` everyone
    redactFor ViewerSpectator game
      `shouldBe` Aeson.object ["log" .= [structured (entry "kept"), legacy]]
    redactFor ViewerSpectator (Aeson.object ["log" .= Aeson.String "odd"])
      `shouldBe` Aeson.object ["log" .= ([] :: [Aeson.Value])]

  it "drops seat B's hidden card ids from every surface for seat A, and keeps them for B (m)" $ withTable \table -> do
    Planted {..} <- plantHidden table
    let Table {..} = table
    liftIO do
      let original = plantedGame
          asA = redactFor (ViewerSeats (Set.singleton tableA)) original
          asB = redactFor (ViewerSeats (Set.singleton tableB)) original
          asAB = redactFor (ViewerSeats (Set.fromList [tableA, tableB])) original
          leaked = [h1, h2, d0]
          ownHand = cardIdsOf (field' "hand" (investigatorFor tableA original))
      -- The fixture really planted every id where it is about to be hidden.
      for_ (h0 : pub : leaked <> ownHand) \i -> (i, i `elem` registryKeys original) `shouldBe` (i, True)
      cardIdsOf (topKey "focusedCards" original) `shouldBe` [h1, pub]
      length ownHand `shouldBe` 3

      -- (1) the cards registry: B's hidden ids go, the revealed card, the
      -- public card and A's own hand stay.
      for_ leaked \i -> (i, i `elem` registryKeys asA) `shouldBe` (i, False)
      for_ (h0 : pub : ownHand) \i -> (i, i `elem` registryKeys asA) `shouldBe` (i, True)
      -- (2) focusedCards and (3) foundCards keep the public card and the zone key.
      cardIdsOf (topKey "focusedCards" asA) `shouldBe` [pub]
      zoneCards (topKey "foundCards" original) `shouldBe` [[h1, pub]]
      zoneCards (topKey "foundCards" asA) `shouldBe` [[pub]]
      zoneKeys (topKey "foundCards" asA) `shouldBe` zoneKeys (topKey "foundCards" original)
      -- (3) B's scenario-level search keeps its zone keys, with no cards.
      let searchA = scenarioSearchOf asA
      zoneCards (field' "searchFoundCards" (scenarioSearchOf original)) `shouldBe` [[h2]]
      zoneCards (field' "searchFoundCards" searchA) `shouldBe` [[]]
      zoneKeys (field' "searchFoundCards" searchA)
        `shouldBe` zoneKeys (field' "searchFoundCards" (scenarioSearchOf original))
      field' "searchDrawnCards" searchA `shouldBe` Aeson.Array mempty
      -- (4) modifiers: the hidden card payload is nulled, AsIfInHand of a hand
      -- card and the pair targeting a hand card are dropped.
      for_ [topKey "modifiers" asA, field' "modifiers" (investigatorFor tableA asA)] \mods -> do
        filter (`elem` leaked) (stringsIn mods) `shouldBe` []
        pub `elem` stringsIn mods `shouldBe` True
      let jennyCardFields =
            [ KeyMap.lookup "card" m
            | Aeson.Object m <- case field' "modifiers" (investigatorFor tableA asA) of
                Aeson.Array ms -> toList ms
                _ -> []
            , KeyMap.lookup "type" m == Just (toJSON CannotBeFlipped)
            ]
      length jennyCardFields `shouldBe` 2
      Just Aeson.Null `elem` jennyCardFields `shouldBe` True
      -- (5) B's meta and formMeta: hidden cards and ids removed, the rest kept.
      let daisyA = investigatorFor tableB asA
      field' "meta" daisyA
        `shouldBe` Aeson.object
          [ "secret" .= Aeson.Null
          , "ids" .= [pub]
          , "byId" .= Aeson.object []
          , "revealed" .= h0
          , "class" .= ("Seeker" :: Text)
          ]
      cardIdsOf (field' "formMeta" daisyA) `shouldBe` [pub]
      -- (6) otherCampaignPlayers: Agnes's zones and meta are cleared for A.
      let agnesA = agnesStored agnesKey asA
      shouldBePopulated (agnesStored agnesKey original)
      for_ ["hand", "deck", "sideDeck", "drawnCards", "cardsUnderneath"] \k ->
        (k, arrayLength (field' k agnesA)) `shouldBe` (k, 0)
      field' "meta" agnesA `shouldBe` Aeson.object ["secret" .= Aeson.Null]
      KeyMap.member "handCount" agnesA `shouldBe` False
      -- And nowhere in A's view does a hidden id of B survive.
      filter (`elem` leaked) (stringsIn asA) `shouldBe` []

      -- Seat B's own view, and a viewer holding both seats, keep every surface;
      -- the full view is the unredacted value itself.
      for_ [asB, asAB] \v -> do
        for_ (h0 : pub : leaked) \i -> (i, i `elem` registryKeys v) `shouldBe` (i, True)
        for_ ["focusedCards", "foundCards", "modifiers"] \k -> (k, topKey k v) `shouldBe` (k, topKey k original)
        scenarioSearchOf v `shouldBe` scenarioSearchOf original
        for_ ["meta", "formMeta", "modifiers"] \k ->
          (k, field' k (investigatorFor tableB v)) `shouldBe` (k, field' k (investigatorFor tableB original))
      -- Seat C sees its own other-campaign entry in full.
      agnesStored agnesKey (redactFor (ViewerSeats (Set.singleton tableC)) original)
        `shouldBe` agnesStored agnesKey original

  it "keeps revealedCards public for every viewer (n)" $ withTable \table -> do
    Planted {..} <- plantHidden table
    let Table {..} = table
    liftIO do
      let original = plantedGame
      for_ [ViewerSpectator, ViewerSeats (Set.singleton tableB), ViewerSeats (Set.singleton tableC)] \v -> do
        let redacted = redactFor v original
        (v, h0 `elem` registryKeys redacted) `shouldBe` (v, True)
        field' "meta" (investigatorFor tableA redacted) `shouldBe` field' "meta" (investigatorFor tableA original)
      -- A spectator loses the other hidden ids, like seat A.
      let asSpectator = redactFor ViewerSpectator original
      filter (`elem` [h1, h2, d0]) (registryKeys asSpectator) `shouldBe` []

  it "is idempotent with hidden ids planted (o)" $ withTable \table -> do
    Planted {..} <- plantHidden table
    liftIO $ for_ (allViewers table) \v ->
      redactFor v (redactFor v plantedGame) `shouldBe` redactFor v plantedGame

  it "drops prohibition entries for seat B's hidden cards from seat A's view (p)" $ withTable \table -> do
    Planted {..} <- plantHidden table
    let Table {..} = table
    game <- getGame
    let daisy =
          fromJustNote "no investigator for seat B"
            $ find ((== tableB) . investigatorPlayerId . toAttrs) (toList $ game ^. entitiesL . investigatorsL)
        cannotPlay = Modifier (toSource daisy) (CannotPlay AnyCard) False Nothing Nothing
    overTest \g -> g {gameModifiers = Map.insertWith (<>) (toTarget daisy) [cannotPlay] (gameModifiers g)}
    game' <- getGame
    liftIO do
      -- commitProhibitions only fill during a skill test, so plant one there.
      let noRefs = [] :: [Text]
          original =
            overSeat
              tableB
              (KeyMap.insert "commitProhibitions" (toJSON [(h1, noRefs), (pub, noRefs)]))
              (toJSON (PublicGame ("game" :: Text) "Redact" (logRows tableA tableB) game'))
          asA = redactFor (ViewerSeats (Set.singleton tableA)) original
          asB = redactFor (ViewerSeats (Set.singleton tableB)) original
          daisyOf = investigatorFor tableB
      -- The engine really reports B's whole hand as prohibited.
      Set.fromList (prohibitionIds (field' "playProhibitions" (daisyOf original))) `shouldBe` Set.fromList [h0, h1, h2]
      -- A keeps only the revealed card's entry, and the public commit entry.
      prohibitionIds (field' "playProhibitions" (daisyOf asA)) `shouldBe` [h0]
      prohibitionIds (field' "commitProhibitions" (daisyOf asA)) `shouldBe` [pub]
      filter (`elem` [h1, h2, d0]) (stringsIn asA) `shouldBe` []
      -- B's own view keeps every entry.
      for_ ["playProhibitions", "commitProhibitions"] \k ->
        (k, field' k (daisyOf asB)) `shouldBe` (k, field' k (daisyOf original))

  it "keeps a hidden id visible to the viewer whose own question names it (q)" $ withTable \table -> do
    Planted {..} <- plantHidden table
    let Table {..} = table
    liftIO do
      -- A's prompt names h2 (in B's hand and B's search); B's names h1.
      let names i =
            Aeson.object
              [ "tag" .= ("ChooseOne" :: Text)
              , "contents"
                  .= [ Aeson.object
                         [ "tag" .= ("TargetLabel" :: Text)
                         , "target" .= Aeson.object ["tag" .= ("CardIdTarget" :: Text), "contents" .= i]
                         , "messages" .= ([] :: [Aeson.Value])
                         ]
                     ]
              ]
          original = setTop "question" (Aeson.object [playerKey tableA .= names h2, playerKey tableB .= names h1]) plantedGame
          asA = redactFor (ViewerSeats (Set.singleton tableA)) original
          asSpectator = redactFor ViewerSpectator original
      (h2 `elem` registryKeys asA) `shouldBe` True
      zoneCards (field' "searchFoundCards" (scenarioSearchOf asA)) `shouldBe` [[h2]]
      -- B's question does not make h1 visible to A, nor A's question h2 to a spectator.
      (h1 `elem` registryKeys asA) `shouldBe` False
      filter (`elem` [h1, h2]) (registryKeys asSpectator) `shouldBe` []
      zoneCards (field' "searchFoundCards" (scenarioSearchOf asSpectator)) `shouldBe` [[]]

  it "keeps a revealed top card, an in-play card and a discarded card visible (r)" $ withTable \table -> do
    Planted {..} <- plantHidden table
    let Table {..} = table
    revealed <- withTopCardRevealed table tableB
    liftIO do
      let asA = redactFor (ViewerSeats (Set.singleton tableA))
      -- B's revealed top card (d0) keeps its registry entry; the rest stay hidden.
      (d0 `elem` registryKeys (asA revealed)) `shouldBe` True
      (d0 `elem` registryKeys (asA plantedGame)) `shouldBe` False
      filter (`elem` [h1, h2]) (registryKeys (asA revealed)) `shouldBe` []
      -- An entity in play with h1's card id, and h2 in B's discard pile.
      let card i = fromJustNote "card not in registry" $ case topKey "cards" plantedGame of
            Aeson.Object cards -> KeyMap.lookup (Key.fromText i) cards
            _ -> Nothing
          inPlay =
            setTop "assets" (Aeson.object ["planted-asset" .= Aeson.object ["cardId" .= h1]])
              . overSeat tableB (KeyMap.insert "discard" (toJSON [card h2]))
              $ plantedGame
      (h1 `elem` registryKeys (asA inPlay)) `shouldBe` True
      (h2 `elem` registryKeys (asA inPlay)) `shouldBe` True
      (d0 `elem` registryKeys (asA inPlay)) `shouldBe` False
