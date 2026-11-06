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
import Arkham.Modifier (Modifier (..), ModifierType (TopCardOfDeckIsRevealed))
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
