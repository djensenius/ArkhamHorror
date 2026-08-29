module Arkham.Api.JsonContractsSpec (spec) where

import Api.Arkham.Deck (deckFromCreateRequest)
import Api.Arkham.Helpers (ApiResponse (..))
import Api.Arkham.Types.Achievement
import Api.Arkham.Types.Deck
import Api.Arkham.Types.Game
import Api.Arkham.Types.GameStep (GameStepJson (..))
import Api.Arkham.Types.MultiplayerVariant (MultiplayerVariant (Solo, WithFriends))
import Arkham.Achievement.Types
  ( Achievement (NightOfTheZealotAchievement, TheDunwichLegacyAchievement)
  , NightOfTheZealotAchievement (TheZealotsRevenge)
  , TheDunwichLegacyAchievement (TheGangsAllHere)
  )
import Arkham.Act (lookupAct)
import Arkham.Campaign (lookupCampaign)
import Arkham.Campaign.Types (Campaign)
import Arkham.Asset.Cards qualified as AssetCards
import Arkham.EnemyLocation (lookupEnemyLocation)
import Arkham.EnemyLocation.Cards qualified as EnemyLocationCards
import Arkham.Enemy.CardDefs.NightOfTheZealot.Rats qualified as EnemyCards (swarmOfRats)
import Arkham.Story (createStory)
import Arkham.Story.CardDefs.FortuneAndFolly qualified as StoryCardDefs (theStakeout)
import Arkham.Campaign.Option (CampaignOption (..))
import Arkham.Campaigns.TheDreamEaters.Meta (CampaignPart (TheDreamQuest))
import Arkham.ClassSymbol (ClassSymbol (Guardian, Rogue, Seeker))
import Arkham.Classes.HasGame (getGame)
import Arkham.Difficulty (Difficulty (Easy, Standard))
import Arkham.Decklist (ArkhamDBDecklist (..))
import Arkham.Epic.Types (SharedEventState (..))
import Arkham.Game.State (GameState (IsActive, IsChooseDecks, IsOver, IsPending))
import Arkham.Game.Settings (AsIfRuling (Chapter1AsIfRuling))
import Arkham.Homebrew.DarkMatter.CardDefs.Enemies qualified as DarkMatterCards
import Arkham.Investigator.Cards qualified as InvestigatorCards
import Arkham.Movement (Destination (ToLocation), Movement (..), MovementMeans (Direct))
import Arkham.Name (mkName)
import Arkham.Scenario.Types (Scenario)
import Arkham.UltimatumsAndBoons.Types
  ( Boon (BoonOfHades)
  , Ultimatum (UltimatumOfChaos)
  , UltimatumOrBoon (Boon, Ultimatum)
  )
import Base.Api.Types.Account
import Base.Api.Types.Capabilities
import Data.Aeson qualified as Aeson
import Data.Aeson.Key qualified as AesonKey
import Data.Aeson.KeyMap qualified as AesonKeyMap
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Data.Text qualified as Text
import Data.Time (secondsToDiffTime)
import Data.UUID qualified as UUID
import Database.Persist qualified as Persist
import Database.Persist.Sql (toSqlKey)
import Entity.Answer (Answer (..))
import Entity.Arkham.Achievement qualified as AchievementEntity
import Entity.Arkham.Deck qualified as DeckEntity
import Entity.Arkham.Game qualified as ArkhamGame
import Entity.Notification (Notification (..))
import System.IO.Error qualified as IOError
import System.IO.Unsafe (unsafePerformIO)
import System.Random (mkStdGen)
import TestImport

loadFixture :: FilePath -> IO Aeson.Value
loadFixture fileName = findFixture fixturePaths
 where
  fixturePaths =
    [ "contracts/fixtures/" <> fileName
    , "../contracts/fixtures/" <> fileName
    , "../../contracts/fixtures/" <> fileName
    ]

  findFixture = \case
    [] ->
      fail
        $ "Could not find contract fixture "
        <> fileName
        <> "; searched: "
        <> show fixturePaths
    path : remainingPaths -> IOError.tryIOError (Aeson.eitherDecodeFileStrict' path) >>= \case
      Left err
        | IOError.isDoesNotExistError err -> findFixture remainingPaths
        | otherwise -> IOError.ioError err
      Right (Left err) ->
        fail $ "Could not decode contract fixture " <> fileName <> " at " <> path <> ": " <> err
      Right (Right fixture) -> pure fixture

{- | Re-decode a value's *actual wire bytes* (@Aeson.encode@, which is
defined as @encodingToLazyByteString . toEncoding@) back into a 'Value' for
fixture comparison, as a second, independent check alongside plain
@Aeson.toJSON@.

This matters because @toJSON@ and @toEncoding@ can be, and in this codebase
have been, hand-written separately rather than one being derived from the
other: 'PublicGame' (Arkham\/Game.hs) defines both by hand, and its
'publicOtherInvestigators' haddock documents a real historical drift where
@toEncoding@ (the actual wire path) silently disagreed with @toJSON@ (what a
naive test asserts against). The wire itself only ever goes through
@toEncoding@: REST responses dispatch via @ToContent a where toContent =
toContent . toEncoding@ (Orphans.hs), and the WebSocket broadcaster calls
@Aeson.encode@ directly (Api\/Handler\/Arkham\/Games\/Shared.hs). A fixture
assertion against @toJSON@ alone would not catch a bug that lives only in
@toEncoding@.
-}
viaWireEncoding :: Aeson.ToJSON a => a -> Aeson.Value
viaWireEncoding value = case Aeson.eitherDecode (Aeson.encode value) of
  Left err ->
    error
      $ "viaWireEncoding: Aeson.encode (the actual toEncoding-driven wire path) "
      <> "produced bytes that do not even parse as JSON: "
      <> err
  Right decoded -> decoded

{- | A local type standing in for the exact class of historical bug
'viaWireEncoding' exists to catch: a hand-written 'toEncoding' that silently
disagrees with 'toJSON' for the same value (as 'PublicGame' once did per the
haddock on 'publicOtherInvestigators'). This is not itself a contract
fixture; it is a methodology self-test proving that if a *real* governed
type's @toEncoding@ ever drifted from its @toJSON@ like this, the
'viaWireEncoding' assertions threaded through every fixture spec below
would fail the suite rather than passing silently.
-}
newtype ToEncodingDriftProof = ToEncodingDriftProof Text

instance Aeson.ToJSON ToEncodingDriftProof where
  toJSON (ToEncodingDriftProof value) =
    Aeson.object ["tag" .= ("ToEncodingDriftProof" :: Text), "value" .= value]
  toEncoding (ToEncodingDriftProof value) =
    Aeson.pairs
      ("tag" .= ("ToEncodingDriftProof" :: Text) <> "value" .= (value <> "-toEncoding-only-drift"))

loadFixtureField :: Aeson.FromJSON a => FilePath -> Text -> IO a
loadFixtureField fileName fieldName = do
  fixture <- loadFixture fileName
  fieldValue <- case fixture of
    Aeson.Object fields ->
      maybe
        (fail $ "Missing field " <> Text.unpack fieldName <> " in " <> fileName)
        pure
        (AesonKeyMap.lookup (AesonKey.fromText fieldName) fields)
    _ -> fail $ "Expected an object fixture in " <> fileName
  case Aeson.fromJSON fieldValue of
    Aeson.Error err ->
      fail
        $ "Could not decode "
        <> Text.unpack fieldName
        <> " from "
        <> fileName
        <> ": "
        <> err
    Aeson.Success value -> pure value

-- | Look up an object key, erroring (via a pure partial function; only used
-- against fixtures we control) if it is missing or the value is not an object.
lookupValue :: Text -> Aeson.Value -> Aeson.Value
lookupValue k (Aeson.Object fields) =
  fromMaybe
    (error $ "Missing key " <> Text.unpack k)
    (AesonKeyMap.lookup (AesonKey.fromText k) fields)
lookupValue k _ = error $ "Expected an object when looking up " <> Text.unpack k

-- | Override just the @turn@ field of a real @mode.schema.json@-shaped
-- @{"That": {...}}@ value, used to prove a turn-zero encoding is otherwise
-- identical to an already schema-validated mode fixture.
setModeTurn :: Aeson.Value -> Int -> Aeson.Value
setModeTurn (Aeson.Object outer) turn =
  case AesonKeyMap.lookup "That" outer of
    Just (Aeson.Object inner) ->
      Aeson.Object
        $ AesonKeyMap.insert
          "That"
          (Aeson.Object $ AesonKeyMap.insert "turn" (Aeson.toJSON turn) inner)
          outer
    _ -> Aeson.Object outer
setModeTurn v _ = v

fixtureGameId :: ArkhamGame.ArkhamGameId
fixtureGameId = ArkhamGame.ArkhamGameKey $ UUID.fromWords 0 0 0 3

fixtureCampaignGameId :: ArkhamGame.ArkhamGameId
fixtureCampaignGameId = ArkhamGame.ArkhamGameKey $ UUID.fromWords 0 0 0 4

fixtureActiveGameId :: ArkhamGame.ArkhamGameId
fixtureActiveGameId = ArkhamGame.ArkhamGameKey $ UUID.fromWords 0 0 0 5

fixtureCompletedGameId :: ArkhamGame.ArkhamGameId
fixtureCompletedGameId = ArkhamGame.ArkhamGameKey $ UUID.fromWords 0 0 0 6

fixtureAchievements :: [Persist.Entity AchievementEntity.ArkhamAchievement]
fixtureAchievements =
  [ Persist.Entity
      (AchievementEntity.ArkhamAchievementKey $ UUID.fromWords 0 0 0 21)
      ( AchievementEntity.ArkhamAchievement
          (toSqlKey 7)
          (NightOfTheZealotAchievement TheZealotsRevenge)
          (Just $ UTCTime (fromGregorian 2026 2 3) (secondsToDiffTime 14706))
          (Just fixtureGameId)
          (Aeson.object ["count" .= (3 :: Int)])
      )
  , Persist.Entity
      (AchievementEntity.ArkhamAchievementKey $ UUID.fromWords 0 0 0 22)
      ( AchievementEntity.ArkhamAchievement
          (toSqlKey 7)
          (TheDunwichLegacyAchievement TheGangsAllHere)
          Nothing
          Nothing
          (Aeson.object ["DrHenryArmitage" .= True])
      )
  ]

fixtureDeckList :: ArkhamDBDecklist
fixtureDeckList =
  ArkhamDBDecklist
    { slots = Map.fromList [("01016", 2), ("01018", 1)]
    , sideSlots = mempty
    , investigator_code = "01001"
    , investigator_name = "Roland Banks"
    , meta = Just "{\"alternate_front\":\"c90001\"}"
    , taboo_id = Nothing
    , url = Just "https://arkhamdb.com/decklist/view/4242"
    , decklist_id = Just "4242.0"
    , decklist_name = Just "Contract deck"
    }

fixtureDeck :: Persist.Entity DeckEntity.ArkhamDeck
fixtureDeck =
  Persist.Entity
    (DeckEntity.ArkhamDeckKey $ UUID.fromWords 0 0 0 23)
    (deckFromCreateRequest (toSqlKey 7) fixtureCreateDeckRequest)

fixtureCreateDeckRequest :: CreateDeckRequest
fixtureCreateDeckRequest =
  CreateDeckRequest
    "external-4242"
    "Contract deck"
    (Just "https://arkhamdb.com/decklist/view/4242")
    fixtureDeckList

fixturePlayerId :: PlayerId
fixturePlayerId = PlayerId $ UUID.fromWords 0 0 0 1

{- | Fixed seed for 'fixtureBoardGame': every shuffle performed while it is built
(chaos bag construction, encounter set gathering, starting location draw) must
be exactly reproducible, so the fixture is rebuilt from this constant rather
than a live random seed.
-}
fixtureBoardSeed :: Int
fixtureBoardSeed = 20260214

-- | Roland Banks, seated with a fixed, non-random 'PlayerId' so the
-- @investigators@ map in the board snapshot is fully deterministic.
fixtureBoardInvestigator :: Investigator
fixtureBoardInvestigator =
  lookupInvestigator (InvestigatorId "01001") fixturePlayerId

-- | "The Gathering", Night of the Zealot's opening scenario -- the same
-- scenario id the previous (pending, empty-board) fixture used.
fixtureBoardScenario :: Scenario
fixtureBoardScenario = lookupScenario "01104" Easy

{- | Night of the Zealot, the same campaign "The Gathering" belongs to --
produced by the real production 'lookupCampaign' (Campaign.hs), the exact
constructor 'newCampaign' (Game.hs) uses to build a fresh campaign's
'CampaignAttrs' -- used to prove the real wire shape of 'GameMode's 'This'
(campaign, no active scenario) and 'These' (a running campaign scenario)
branches (issue: mode.schema.json's third 'oneOf' branch modeled a
nonexistent @{"These": {...}}@ wrapper; the 'these' package's actual encoding
of @These a b@ is sibling @{"This": a, "That": b}@ keys, matching
'Data.These'\'s behaviour empirically verified against this exact dependency
pin -- there is no custom\/orphan 'ToJSON (These a b)' instance in this
codebase).
-}
fixtureCampaign :: Campaign
fixtureCampaign = lookupCampaign (CampaignId "01") Easy

-- | Real production 'This' branch: a Campaign snapshot with no active
-- scenario (a between-scenario campaign screen), matching what
-- @newCampaign cid Nothing@ (Game.hs) sets 'gameMode' to.
fixtureCampaignOnlyMode :: These Campaign Scenario
fixtureCampaignOnlyMode = This fixtureCampaign

-- | Real production 'These' branch: both an active Campaign and its
-- currently running Scenario (reusing the same production 'lookupScenario'
-- value 'fixtureBoardGame' is built from), matching what
-- @newCampaign cid (Just sid)@ (Game.hs) sets 'gameMode' to. Encodes as
-- sibling @{"This": ..., "That": ...}@ keys, never a wrapping @"These"@ key.
fixtureRunningCampaignMode :: These Campaign Scenario
fixtureRunningCampaignMode = These fixtureCampaign fixtureBoardScenario

{- | A genuinely non-empty, deterministic post-'Setup' board: one seated
investigator, the act\/agenda decks in play, the chaos bag built, and the
opening location placed -- produced by running the real 'StandaloneSetup',
'Setup', and 'EndSetup' scenario message handlers (the exact same production
code every other scenario spec in this suite exercises) against a fixed seed,
rather than hand-writing a JSON sample (issue #44).

This intentionally starts the game from 'TestImport.newGame' (seats the
investigator directly) rather than the full production 'LoadScenario' chain,
because the latter blocks on an interactive mulligan question before 'Setup'
ever runs; that chain also folds in campaign-only concerns
('SetChaosTokensForScenario', 'HandleKilledOrInsaneInvestigators',
'CheckDestiny', tarot) that do not apply to a fresh standalone scenario and are
out of scope for this contract slice. Every message this fixture *does* push
is the real, unmodified production handler.
-}
fixtureBoardGame :: Game
fixtureBoardGame = unsafePerformIO buildFixtureBoardGame
{-# NOINLINE fixtureBoardGame #-}

buildFixtureBoardGame :: IO Game
buildFixtureBoardGame = do
  baseGame <- newGame fixtureBoardScenario fixtureBoardInvestigator
  let
    game =
      baseGame
        { gameSeed = fixtureBoardSeed
        , gameInitialSeed = fixtureBoardSeed
        , gameGitRevision = "contract-fixture"
        }
  gameRef <- newIORef game
  queueRef <- newQueue []
  genRef <- newIORef $ mkStdGen fixtureBoardSeed
  debugLevelRef <- newIORef 0
  let testApp = TestApp gameRef queueRef genRef Nothing (pure . const ()) debugLevelRef
  runReaderT (overGameM preloadModifiers) testApp
  runTestApp testApp do
    pushAndRunAll [StandaloneSetup, Setup, EndSetup]
    -- "The Gathering" opens with a setup flavor-text prompt (the same
    -- "Read"/continue prompt every real player clicks through); dismiss it so
    -- the queued placement messages behind it (act/agenda decks, starting
    -- location) actually run.
    chooseOnlyOption "advance past The Gathering's setup introduction"
    -- 'startAt' asks the lead investigator to reveal/enter the single starting
    -- location ("Study"); answering it (there is only one option) is what
    -- actually seats the investigator on the board.
    chooseOnlyOption "reveal and enter the starting location"
    getGame

fixtureGame :: Game
fixtureGame = fixtureBoardGame

{- | A minimal 'TestApp' wired to run pure @HasGame@ helpers (e.g.
'withActMetadata', 'withEnemyLocationAsLocationData') against an already-built
game, without repeating scenario setup. Used by small, narrowly-scoped
fixtures below that only need to prove a single field's real wire shape.
-}
runAgainstFixtureBoardGame :: TestAppT a -> IO a
runAgainstFixtureBoardGame action = do
  gameRef <- newIORef fixtureBoardGame
  queueRef <- newQueue []
  genRef <- newIORef $ mkStdGen fixtureBoardSeed
  debugLevelRef <- newIORef 0
  let testApp = TestApp gameRef queueRef genRef Nothing (pure . const ()) debugLevelRef
  runTestApp testApp action

{- | A second board built by omitting 'EndSetup' (whose handler is what queues
the real 'BeginRound' message that increments 'ScenarioAttrs.scenarioTurn'
from its initial 0 -- Scenario/Types.hs and Scenario/Runner.hs). This proves
the real production turn-zero encoding (issue: mode.schema.json's @turn@
minimum previously rejected the valid initial value 0).
-}
buildFixtureBoardGameAtTurnZero :: IO Game
buildFixtureBoardGameAtTurnZero = do
  baseGame <- newGame fixtureBoardScenario fixtureBoardInvestigator
  let
    game =
      baseGame
        { gameSeed = fixtureBoardSeed
        , gameInitialSeed = fixtureBoardSeed
        , gameGitRevision = "contract-fixture"
        }
  gameRef <- newIORef game
  queueRef <- newQueue []
  genRef <- newIORef $ mkStdGen fixtureBoardSeed
  debugLevelRef <- newIORef 0
  let testApp = TestApp gameRef queueRef genRef Nothing (pure . const ()) debugLevelRef
  runReaderT (overGameM preloadModifiers) testApp
  runTestApp testApp do
    pushAndRunAll [StandaloneSetup, Setup]
    chooseOnlyOption "advance past The Gathering's setup introduction"
    chooseOnlyOption "reveal and enter the starting location"
    getGame

fixtureBoardGameAtTurnZero :: Game
fixtureBoardGameAtTurnZero = unsafePerformIO buildFixtureBoardGameAtTurnZero
{-# NOINLINE fixtureBoardGameAtTurnZero #-}

{- | The real production @Arkham.EnemyLocation.Cards.shapelessCellar@
(card code 10547) instantiated via the same pure 'lookupEnemyLocation' that
handles the real @PlaceEnemyLocation@ message (Game/Runner.hs), then run
through the real 'withEnemyLocationAsLocationData' encoder (Game.hs) -- the
distinct, disjoint view PublicGame emits for enemy-location pseudo-locations,
as opposed to ordinary 'LocationAttrs'.
-}
fixtureEnemyLocationView :: Aeson.Value
fixtureEnemyLocationView = unsafePerformIO $ runAgainstFixtureBoardGame do
  let
    enemyLocationId = LocationId $ UUID.fromWords 0 0 0 50
    enemyLocationCardId = unsafeMakeCardId $ UUID.fromWords 0 0 0 51
    enemyLocation =
      lookupEnemyLocation
        (toCardCode EnemyLocationCards.shapelessCellar)
        enemyLocationId
        enemyLocationCardId
  withEnemyLocationAsLocationData enemyLocation
{-# NOINLINE fixtureEnemyLocationView #-}

{- | A real, non-random 'Movement' value (Arkham/Movement.hs), built with the
same production constructors 'move' itself uses, but with fixed ids so the
fixture is deterministic. Proves 'investigatorMovement :: Maybe Movement'
(Investigator/Types.hs) is a real object when present, not a bare location-id
string.
-}
fixtureMovement :: Movement
fixtureMovement =
  Movement
    { moveSource = InvestigatorSource (InvestigatorId "01001")
    , moveTarget = LocationTarget (LocationId $ UUID.fromWords 0 0 0 60)
    , moveDestination = ToLocation (LocationId $ UUID.fromWords 0 0 0 61)
    , moveMeans = Direct
    , moveCancelable = True
    , movePayAdditionalCosts = False
    , moveAfter = []
    , moveAdditionalEnterCosts = Free
    , moveSkipEngagement = False
    , moveId = MovementId $ UUID.fromWords 0 0 0 62
    , moveForced = False
    , moveFromInPlay = True
    }

{- | The real "Hot on Your Tail" act (All Or Nothing, card code 90014), the
production act whose 'actAdvanceCost' is genuinely 'Nothing' (its registered
'act (2, A) HotOnYourTail Cards.hotOnYourTail Nothing' builder --
Act/Cards/AllOrNothing/HotOnYourTail.hs), run through the real
'withActMetadata' encoder. Proves 'ActAttrs.actAdvanceCost :: Maybe Cost'
(Act/Types.hs) really does encode as JSON null, not an object, when absent.
-}
fixtureActNoAdvanceCost :: Aeson.Value
fixtureActNoAdvanceCost = unsafePerformIO $ runAgainstFixtureBoardGame do
  case lookupAct (ActId "90014") 1 (unsafeMakeCardId $ UUID.fromWords 0 0 0 70) of
    Left err -> liftIO $ IOError.ioError $ IOError.userError $ "Could not look up act 90014: " <> show err
    Right act' -> Aeson.toJSON <$> withActMetadata act'
{-# NOINLINE fixtureActNoAdvanceCost #-}

{- | A real production negative @unhealedHorrorThisRound@ regression: pushes
the actual @HealHorrorDirectly@ handler (Investigator\/Runner\/Damage.hs) for
more horror than the fixture investigator was ever assigned this round,
which the real @min 0 . subtract amount@ arithmetic genuinely drives negative
(issue: investigator.schema.json's field incorrectly had a @minimum: 0@ even
though production can, and does, emit negative values here). Extracted from
the same production @PublicGame@ envelope encoder every other fixture in
this file is bound to, not a hand-authored investigator payload.
-}
fixtureInvestigatorNegativeUnhealedHorror :: Aeson.Value
fixtureInvestigatorNegativeUnhealedHorror = unsafePerformIO $ runAgainstFixtureBoardGame do
  pushAndRunAll
    [HealHorrorDirectly (InvestigatorTarget $ InvestigatorId "01001") GameSource 3]
  game <- getGame
  let publicGame = PublicGame fixtureGameId "Contract fixture game" ["Contract fixture log entry."] game
  pure $ case Aeson.toJSON publicGame of
    Aeson.Object top -> case AesonKeyMap.lookup "investigators" top of
      Just (Aeson.Object invs) -> case AesonKeyMap.elems invs of
        [investigatorValue] -> investigatorValue
        _ -> error "fixtureInvestigatorNegativeUnhealedHorror: expected exactly one investigator"
      _ -> error "fixtureInvestigatorNegativeUnhealedHorror: missing investigators object"
    _ -> error "fixtureInvestigatorNegativeUnhealedHorror: expected a PublicGame object"
{-# NOINLINE fixtureInvestigatorNegativeUnhealedHorror #-}

{- | Real production evidence for the UUID-keyed entity-map key class shared
by @enemies@\/@assets@\/@treacheries@\/@events@\/@skills@\/@concealed@ (all
@EntityMap a = Map (EntityId a) a@ where @EntityId a@ is a UUID-backed
newtype whose @ToJSONKey@ is @deriving newtype@ from the wrapped UUID,
Arkham\/Id.hs) plus @question@ (@Map PlayerId ...@) and @cards@ (@Map CardId
Card@, Arkham\/Card\/Id.hs). Built via the real @createEnemy@ constructor
(Arkham\/Enemy.hs) against a real core enemy card def (Swarm of Rats,
Arkham\/Enemy\/CardDefs\/NightOfTheZealot\/Rats.hs) and encoded with the
real @Map@\/@ToJSONKey@ @Aeson.toJSON@ instance actually exercised by
@Game.hs@'s @PublicGame@ envelope encoder for every one of those fields --
not a hand-authored duplicate encoder. Every real @get-game.json@\/
@game-update.json@ fixture keeps these particular fields empty (no enemies
are on the board at fixture setup), so this focused standalone fixture is
what proves the shared UUID-map-key schema constraint against a genuinely
non-empty example instead of only ever validating an empty map.
-}
fixtureUuidEntityMap :: Aeson.Value
fixtureUuidEntityMap = Aeson.toJSON (Map.fromList [(enemyId, createEnemy card enemyId)])
 where
  enemyId = EnemyId (UUID.fromWords 0 0 0 900)
  card = lookupCard EnemyCards.swarmOfRats (unsafeMakeCardId (UUID.fromWords 0 0 0 901))
{-# NOINLINE fixtureUuidEntityMap #-}

{- | Real production evidence for the CardCode-keyed entity\/history-map key
class shared by @stories@\/@scarletKeys@ (@StoryId@\/@ScarletKeyId@, both
@CardCode@-backed newtypes whose @ToJSONKey@ is @deriving newtype@ from
CardCode) and @roundHistory@\/@phaseHistory@\/@turnHistory@ (@Map
InvestigatorId History@, Arkham\/Game\/Base.hs; @InvestigatorId@ is also
CardCode-backed). Built via the real @createStory@ constructor
(Arkham\/Story.hs) against a real story card def (The Stakeout,
Arkham\/Story\/CardDefs\/FortuneAndFolly.hs), keyed exactly as the real
engine does at the one real call site (@Game\/Runner.hs@:
@let storyId = StoryId $ toCardCode card@), and encoded with the real
@Map@\/@ToJSONKey@ @Aeson.toJSON@ instance. Every real @get-game.json@\/
@game-update.json@ fixture keeps @stories@\/@scarletKeys@\/@*History@ empty,
so this standalone fixture proves the shared CardCode-map-key schema
constraint against a genuinely non-empty example.
-}
fixtureCardCodeEntityMap :: Aeson.Value
fixtureCardCodeEntityMap = Aeson.toJSON (Map.fromList [(storyId, createStory card Nothing storyId)])
 where
  card = lookupCard StoryCardDefs.theStakeout (unsafeMakeCardId (UUID.fromWords 0 0 0 902))
  storyId = StoryId (toCardCode card)
{-# NOINLINE fixtureCardCodeEntityMap #-}

fixturePublicGame :: PublicGame ArkhamGame.ArkhamGameId
fixturePublicGame =
  PublicGame
    fixtureGameId
    "Contract fixture game"
    ["Contract fixture log entry."]
    fixtureGame

serverMessageFixtures :: [(FilePath, ApiResponse)]
serverMessageFixtures =
  [ ( "game-update.json"
    , GameUpdate fixturePublicGame
    )
  , ("game-message.json", GameMessage "A contract fixture message.")
  , ("game-error.json", GameError "The question changed before this answer arrived.")
  , ("game-ui.json", GameUI "contract:ui")
  , ("game-audio.json", GameAudio "contract.ogg")
  , ("game-card.json", GameCard "Contract card" fixtureCard)
  , ( "game-card-only.json"
    , GameCardOnly fixturePlayerId "Private contract card" fixtureCard
    )
  , ("game-tarot.json", GameTarot $ Aeson.object ["spread" .= ("fixture" :: Text)])
  , ("game-show-discard.json", GameShowDiscard "01001")
  , ("game-show-under.json", GameShowUnder "02002")
  , ("game-achievement.json", GameAchievement "fixture-achievement")
  , ( "game-playability-info.json"
    , GamePlayabilityInfo
        nullCardId
        "fixture-card"
        [("play", Nothing), ("fast", Just "Needs an action window")]
    )
  , ( "shared-state-update.json"
    , SharedStateUpdate
        $ SharedEventState
          7
          (Map.fromList [("act-progress:1", 2), ("countermeasures", 4)])
          4
          (Set.fromList ["delta-a", "delta-b"])
    )
  , ("event-changed.json", EventChanged)
  ]
 where
  fixtureCard = Aeson.object ["code" .= ("fixture-card" :: Text)]

fixtureGetGame :: GetGameJson
fixtureGetGame =
  GetGameJson
    (Just fixturePlayerId)
    Solo
    fixturePublicGame
    Nothing

fixtureGameList :: [GameDetailsEntry]
fixtureGameList =
  [ SuccessGameDetails
      $ GameDetails
        fixtureGameId
        (Just $ ScenarioDetails "01104" Easy (mkName "The Gathering") Nothing)
        Nothing
        (IsPending [])
        "Contract fixture game"
        []
        []
        Solo
        False
  , SuccessGameDetails
      $ GameDetails
        fixtureCampaignGameId
        Nothing
        (Just $ CampaignDetails "06" Easy (Just TheDreamQuest))
        (IsChooseDecks [fixturePlayerId])
        "Campaign contract fixture"
        [InvestigatorDetails "06001" Guardian, InvestigatorDetails "06002" Seeker]
        [InvestigatorDetails "06003" Rogue]
        WithFriends
        True
  , SuccessGameDetails
      $ GameDetails
        fixtureActiveGameId
        (Just $ ScenarioDetails "01104" Easy (mkName "The Gathering") Nothing)
        Nothing
        IsActive
        "Active contract fixture"
        []
        []
        Solo
        False
  , SuccessGameDetails
      $ GameDetails
        fixtureCompletedGameId
        (Just $ ScenarioDetails "01104" Easy (mkName "The Gathering") Nothing)
        Nothing
        IsOver
        "Completed contract fixture"
        []
        []
        Solo
        False
  , FailedGameDetails "Contract fixture failed to load."
  ]

clientAnswerFixtures :: [(FilePath, Text)]
clientAnswerFixtures =
  [ ("answer-question.json", "Answer")
  , ("answer-raw.json", "Raw")
  , ("answer-payment-amounts.json", "PaymentAmountsAnswer")
  , ("answer-amounts.json", "AmountsAnswer")
  , ("answer-standalone-settings.json", "StandaloneSettingsAnswer")
  , ("answer-campaign-settings.json", "CampaignSettingsAnswer")
  , ("answer-deck.json", "DeckAnswer")
  , ("answer-deck-list.json", "DeckListAnswer")
  , ("answer-pick-destiny.json", "PickDestinyAnswer")
  , ("answer-campaign-specific.json", "CampaignSpecificAnswer")
  , ("answer-scenario-specific.json", "ScenarioSpecificAnswer")
  , ("answer-exchange-amounts.json", "ExchangeAmountsAnswer")
  , ("answer-campaign-step.json", "CampaignStepAnswer")
  ]

answerConstructor :: Answer -> Text
answerConstructor = \case
  Answer _ -> "Answer"
  Raw _ -> "Raw"
  PaymentAmountsAnswer _ -> "PaymentAmountsAnswer"
  AmountsAnswer _ -> "AmountsAnswer"
  StandaloneSettingsAnswer _ -> "StandaloneSettingsAnswer"
  CampaignSettingsAnswer _ -> "CampaignSettingsAnswer"
  DeckAnswer _ _ -> "DeckAnswer"
  DeckListAnswer _ _ -> "DeckListAnswer"
  PickDestinyAnswer _ -> "PickDestinyAnswer"
  CampaignSpecificAnswer _ _ -> "CampaignSpecificAnswer"
  ScenarioSpecificAnswer _ _ -> "ScenarioSpecificAnswer"
  ExchangeAmountsAnswer _ _ _ _ _ -> "ExchangeAmountsAnswer"
  CampaignStepAnswer _ -> "CampaignStepAnswer"

spec :: Spec
spec = describe "Native client contract fixtures" do
  it "matches the runtime server-capabilities encoder" do
    fixture <- loadFixture "capabilities.json"

    Aeson.toJSON serverCapabilities `shouldBe` fixture

  it "decodes the real create-game request" do
    request <-
      loadFixtureField "game-lifecycle.json" "createGame"
        :: IO CreateGamePost

    request
      `shouldBe` CreateGamePost
        [Just $ DeckEntity.ArkhamDeckKey $ UUID.fromWords 0 0 0 23, Nothing]
        2
        (Just "01")
        Nothing
        Standard
        "Contract campaign"
        WithFriends
        True
        (Set.fromList [PerformIntro, CampaignVariant "return-to"])
        (Just False)
        (Just Chapter1AsIfRuling)
        (Set.fromList [Boon BoonOfHades, Ultimatum UltimatumOfChaos])
        False

  it "applies the real create-game request defaults" do
    request <-
      loadFixtureField "game-lifecycle.json" "createGameDefaults"
        :: IO CreateGamePost

    request
      `shouldBe` CreateGamePost
        []
        1
        Nothing
        (Just "01104")
        Easy
        "Contract standalone"
        Solo
        False
        mempty
        Nothing
        Nothing
        mempty
        True

  it "applies the real create-game request defaults to null fields" do
    request <-
      loadFixtureField "game-lifecycle.json" "createGameNullDefaults"
        :: IO CreateGamePost

    request
      `shouldBe` CreateGamePost
        []
        1
        Nothing
        (Just "01104")
        Easy
        "Contract standalone"
        Solo
        False
        mempty
        Nothing
        Nothing
        mempty
        True

  it "decodes the real choose-deck request" do
    request <-
      loadFixtureField "game-lifecycle.json" "chooseDeck"
        :: IO UpgradeDeckPost

    request
      `shouldBe` UpgradeDeckPost
        "01001"
        (Just "https://arkhamdb.com/decklist/view/4242")
        (Just fixtureDeckList)

  it "decodes the real continue-without-upgrade request" do
    request <-
      loadFixtureField "game-lifecycle.json" "continueWithoutUpgrade"
        :: IO UpgradeDeckPost

    request `shouldBe` UpgradeDeckPost "01001" Nothing Nothing

  it "decodes the real claim-seat request" do
    request <-
      loadFixtureField "game-lifecycle.json" "claimSeat"
        :: IO ClaimSeatPost

    request `shouldBe` ClaimSeatPost "01001"

  it "matches the real open-seats encoder" do
    fixture <- loadFixtureField "game-lifecycle.json" "openSeats"

    Aeson.toJSON (["c01001", "c01002"] :: [Text]) `shouldBe` fixture

  it "matches the real game-step encoder" do
    fixture <- loadFixture "game-step.json"

    Aeson.toJSON (GameStepJson 42) `shouldBe` fixture

  for_ serverMessageFixtures \(fileName, response) ->
    it ("matches the real server encoder for " <> fileName) do
      fixture <- loadFixture fileName

      Aeson.toJSON response `shouldBe` fixture
      -- Also bind the actual wire bytes (Aeson.encode, i.e. toEncoding --
      -- what Orphans.hs's ToContent and the WebSocket broadcaster's direct
      -- `encode` calls really send) to the same fixture, so this proves the
      -- REST/WebSocket ApiResponse encoders (every one of them: GameUpdate
      -- carries the same PublicGame as GetGame below) agree with toJSON
      -- rather than only ever exercising the naive path.
      viaWireEncoding response `shouldBe` fixture

  it "matches the real game-list encoder" do
    fixture <- loadFixture "game-list.json"

    Aeson.toJSON fixtureGameList `shouldBe` fixture
    viaWireEncoding fixtureGameList `shouldBe` fixture

  it "matches the real get-game encoder" do
    fixture <- loadFixture "get-game.json"

    Aeson.toJSON fixtureGetGame `shouldBe` fixture
    -- GetGameJson is the REST envelope around the very same PublicGame
    -- GameUpdate carries over WebSocket; binding its actual wire bytes here
    -- too proves both transports serialize the identical PublicGame shape
    -- through the identical (toEncoding-driven) real encoder, not merely
    -- through toJSON.
    viaWireEncoding fixtureGetGame `shouldBe` fixture

  it "proves viaWireEncoding actually exercises toEncoding, not toJSON twice (methodology self-test: if PublicGame's toEncoding ever silently drifted from its toJSON the way the historical bug documented on publicOtherInvestigators did, every viaWireEncoding assertion above would fail rather than pass silently)" do
    let driftProof = ToEncodingDriftProof "same-input"

    Aeson.toJSON driftProof `shouldNotBe` viaWireEncoding driftProof

  it "matches the real get-game mode encoder at turn zero (issue: mode.schema.json's turn minimum previously rejected the valid initial value 0, since ScenarioAttrs.scenarioTurn starts at 0 -- Scenario/Types.hs -- and is only incremented by EndSetup's queued BeginRound -- Scenario/Runner.hs)" do
    getGameFixture <- loadFixture "get-game.json"
    governedTurnZeroFixture <- loadFixture "mode-turn-zero.json"
    let
      embeddedMode = lookupValue "mode" $ lookupValue "game" getGameFixture
      turnZeroMode = Aeson.toJSON $ gameMode fixtureBoardGameAtTurnZero

    -- The real turn genuinely is 0 (not just schema-permitted).
    lookupValue "turn" (lookupValue "That" turnZeroMode) `shouldBe` Aeson.Number 0
    -- Every other field of the real production encoding is identical to the
    -- already schema-validated get-game.json fixture's mode -- this is a
    -- targeted single-field (turn) regression, not a second copy of the
    -- fixture's ~5KB scenario payload.
    setModeTurn embeddedMode (0 :: Int) `shouldBe` turnZeroMode
    -- Also bound to a governed, hashed, schema-validated fixture file
    -- (contracts/fixtures/mode-turn-zero.json) generated directly from this
    -- same real `toJSON (gameMode fixtureBoardGameAtTurnZero)` call, so the
    -- turn-zero regression has real golden provenance rather than living
    -- only as an in-memory diff -- a mutation test in
    -- `check-schema-revision-drift`/manifest self-tests proves the schema's
    -- `turn` minimum:0 bound is load-bearing against this exact fixture.
    turnZeroMode `shouldBe` governedTurnZeroFixture

  it "matches the real This-only mode encoder for a campaign with no active scenario (issue: mode.schema.json's third oneOf branch modeled a nonexistent {\"These\": {...}} wrapper; the real 'these' package encodes These a b as sibling {\"This\":a,\"That\":b} keys, verified empirically -- there is no custom ToJSON (These a b) instance anywhere in this codebase)" do
    fixture <- loadFixture "mode-campaign-only.json"
    Aeson.toJSON fixtureCampaignOnlyMode `shouldBe` fixture

  it "matches the real This+That mode encoder for a running campaign scenario (production's These constructor, encoded as sibling 'This'/'That' keys -- never a wrapping \"These\" key)" do
    fixture <- loadFixture "mode-campaign-scenario.json"
    Aeson.toJSON fixtureRunningCampaignMode `shouldBe` fixture

  it "matches the real enemy-location view encoder (issue: location.schema.json needed a disjoint oneOf for the enemyLocation:true view Game.hs's withEnemyLocationAsLocationData emits, distinct from ordinary LocationAttrs)" do
    fixture <- loadFixture "location-enemy-view.json"
    fixtureEnemyLocationView `shouldBe` fixture

  it "matches the real Movement encoder (issue: investigatorMovement is Maybe Movement, a real object, not a bare location-id string)" do
    fixture <- loadFixture "movement.json"
    Aeson.toJSON fixtureMovement `shouldBe` fixture

  it "matches the real act encoder for an act with no advance cost (issue: actAdvanceCost :: Maybe Cost encodes Nothing as null, not an object)" do
    fixture <- loadFixture "act-no-advance-cost.json"
    fixtureActNoAdvanceCost `shouldBe` fixture

  it "matches the real investigator encoder for a negative unhealedHorrorThisRound (issue: investigator.schema.json incorrectly had a minimum:0 even though production's min 0 . subtract amount in Runner/Damage.hs genuinely emits negative values on the wire when over-healing)" do
    fixture <- loadFixture "investigator-unhealed-horror-negative.json"
    fixtureInvestigatorNegativeUnhealedHorror `shouldBe` fixture

  it "matches the real UUID-keyed entity-map encoder (issue: PublicGame's enemies/assets/treacheries/events/skills/concealed/question/cards maps had no propertyNames constraint at all; every real get-game/game-update fixture keeps them empty, so this focused fixture proves the shared uuidMapKey grammar against a genuinely non-empty, real createEnemy-built map)" do
    fixture <- loadFixture "uuid-entity-map.json"
    fixtureUuidEntityMap `shouldBe` fixture

  it "matches the real CardCode-keyed entity-map encoder (issue: PublicGame's stories/scarletKeys/roundHistory/phaseHistory/turnHistory maps had no propertyNames constraint; this focused fixture proves the shared cardCodeMapKey grammar -- keyed exactly as the real Game/Runner.hs call site does, StoryId $ toCardCode card -- against a genuinely non-empty, real createStory-built map)" do
    fixture <- loadFixture "card-code-entity-map.json"
    fixtureCardCodeEntityMap `shouldBe` fixture

  it "decodes the real password-reset request" do
    request <-
      loadFixtureField "account.json" "passwordResetRequest"
        :: IO PasswordResetRequest

    request.resetEmail `shouldBe` "investigator@example.com"

  it "decodes the real password-reset update" do
    request <-
      loadFixtureField "account.json" "passwordResetUpdate"
        :: IO PasswordResetUpdate

    request.resetPassword `shouldBe` "new passphrase"

  it "decodes the real settings update" do
    settings <-
      loadFixtureField "account.json" "userSettings"
        :: IO UserSettings

    settings.betaSetting `shouldBe` True

  it "matches the real settings response encoder" do
    fixture <- loadFixtureField "account.json" "settingsUser"

    Aeson.toJSON (SettingsUser "Investigator" "investigator@example.com" True)
      `shouldBe` fixture

  it "matches the real notification-list encoder" do
    fixture <- loadFixtureField "account.json" "notifications"
    let
      notification =
        Persist.Entity
          (toSqlKey 17)
          ( Notification
              "Contract fixture announcement."
              (UTCTime (fromGregorian 2026 1 2) (secondsToDiffTime 11045))
          )

    Aeson.toJSON [notification] `shouldBe` fixture

  it "matches the real card-list encoder" do
    fixture <- loadFixtureField "catalog.json" "cards"

    Aeson.toJSON [AssetCards.machete] `shouldBe` fixture

  it "matches the real homebrew-card-list encoder" do
    fixture <- loadFixtureField "catalog.json" "homebrewCards"

    Aeson.toJSON [DarkMatterCards.rats] `shouldBe` fixture

  it "matches the real card-detail encoder" do
    fixture <- loadFixtureField "catalog.json" "card"

    Aeson.toJSON InvestigatorCards.rolandBanks `shouldBe` fixture

  it "matches the real investigator-artwork encoder" do
    fixture <- loadFixtureField "catalog.json" "investigators"
    let
      artwork =
        map cdArt [InvestigatorCards.rolandBanks, InvestigatorCards.daisyWalker]

    Aeson.toJSON artwork `shouldBe` fixture

  it "matches the real achievement-list encoder" do
    fixture <- loadFixtureField "achievements.json" "achievements"

    Aeson.toJSON fixtureAchievements `shouldBe` fixture

  it "normalizes the real imported-deck decoder through its encoder" do
    importedDeckList <-
      loadFixtureField "decks.json" "validateDeckList"
        :: IO ArkhamDBDecklist
    normalizedDeckList <- loadFixtureField "decks.json" "normalizedDeckList"

    importedDeckList `shouldBe` fixtureDeckList
    Aeson.toJSON importedDeckList `shouldBe` normalizedDeckList

  it "defaults every unparsable sideSlots value to an empty map" do
    deckListValue <-
      loadFixtureField "decks.json" "validateDeckList"
        :: IO Aeson.Value
    let
      withSideSlots value = case deckListValue of
        Aeson.Object fields ->
          Aeson.Object $ AesonKeyMap.insert "sideSlots" value fields
        _ -> error "Expected validateDeckList to be an object"
      unparsableValues =
        [ Aeson.Null
        , Aeson.Bool True
        , Aeson.String "not a card-quantity map"
        , Aeson.Number 42
        , Aeson.Array $ fromList [Aeson.String "not a key-value pair"]
        ]

    for_ unparsableValues \value ->
      case (Aeson.fromJSON (withSideSlots value) :: Aeson.Result ArkhamDBDecklist) of
        Aeson.Error err -> expectationFailure err
        Aeson.Success deckList -> sideSlots deckList `shouldBe` mempty

  it "defaults a null investigator_name from the card registry" do
    deckListValue <-
      loadFixtureField "decks.json" "validateDeckList"
        :: IO Aeson.Value
    let withNullName = case deckListValue of
          Aeson.Object fields ->
            Aeson.Object $ AesonKeyMap.insert "investigator_name" Aeson.Null fields
          _ -> error "Expected validateDeckList to be an object"

    Aeson.fromJSON withNullName `shouldBe` Aeson.Success fixtureDeckList

  it "decodes the real create-deck request" do
    request <-
      loadFixtureField "decks.json" "createDeck"
        :: IO CreateDeckRequest

    request `shouldBe` fixtureCreateDeckRequest

  it "decodes the real fetch-deck request" do
    request <-
      loadFixtureField "decks.json" "fetchDeck"
        :: IO FetchDeckRequest

    request `shouldBe` FetchDeckRequest "https://arkhamdb.com/decklist/view/4242"

  it "matches the real saved-deck encoder" do
    fixture <- loadFixtureField "decks.json" "deck"

    Aeson.toJSON fixtureDeck `shouldBe` fixture

  it "matches the real deck-validation error encoder" do
    fixture <- loadFixtureField "decks.json" "validationErrors"
    let errors = [UnimplementedCard "99999"]

    Aeson.toJSON errors `shouldBe` fixture

  it "matches the real deck-validation success encoder" do
    fixture <- loadFixtureField "decks.json" "validationSuccess"

    Aeson.toJSON () `shouldBe` fixture

  it "matches the real deck-operation error encoder" do
    fixture <- loadFixtureField "decks.json" "operationError"

    Aeson.toJSON (DeckOperationError "Could not sync deck") `shouldBe` fixture

  for_
    ( [ ("clearAll", ClearAll)
      , ("clearCampaign", ClearCampaign "51")
      , ("clearAchievement", ClearAchievement $ NightOfTheZealotAchievement TheZealotsRevenge)
      ]
        :: [(Text, ClearAchievements)]
    )
    \(fieldName, expectedRequest) ->
      it ("decodes the real achievement clear request for " <> Text.unpack fieldName) do
        request <-
          loadFixtureField "achievements.json" fieldName
            :: IO ClearAchievements

        request `shouldBe` expectedRequest

  for_ clientAnswerFixtures \(fileName, expectedConstructor) ->
    it ("decodes the real client answer for " <> fileName) do
      fixture <- loadFixture fileName

      case Aeson.fromJSON fixture of
        Aeson.Error err -> expectationFailure $ "Could not decode " <> fileName <> ": " <> err
        Aeson.Success answer -> answerConstructor answer `shouldBe` expectedConstructor
