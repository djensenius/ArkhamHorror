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
import Arkham.Agenda.Types (AgendaAttrs (agendaDoom), Field (AgendaFlipped))
import Arkham.Action qualified as Action
import Arkham.Ability (abilityActions)
import Arkham.Ability.Limit qualified as AbilityLimit
import Arkham.Ability.Type qualified as AbilityType
import Arkham.Ability.Types
  ( abilityCardCode
  , abilityCriteria
  , abilityIndex
  , abilityLimit
  , abilityRequestor
  , abilitySource
  , abilityTarget
  , abilityType
  , abilityWindow
  )
import Arkham.Attack.Types
  ( AttackTarget (SingleAttackTarget)
  , EnemyAttackDetails (..)
  , EnemyAttackType (RegularAttack)
  )
import Arkham.Campaign (lookupCampaign)
import Arkham.Campaign.Types (Campaign)
import Arkham.Asset.Cards qualified as AssetCards
import Arkham.EnemyLocation (lookupEnemyLocation)
import Arkham.EnemyLocation.Cards qualified as EnemyLocationCards
import Arkham.Enemy.CardDefs.NightOfTheZealot.Ghouls qualified as GhoulCards (ghoulMinion)
import Arkham.Enemy.CardDefs.NightOfTheZealot.Rats qualified as EnemyCards (swarmOfRats)
import Arkham.Enemy.Creation (EnemyCreation (..))
import Arkham.Enemy.Types qualified as Enemy
import Arkham.Placement (Placement (InThreatArea))
import Arkham.Projection (field)
import Arkham.Question.Presentation qualified as QuestionPresentation
import Arkham.Story (createStory)
import Arkham.Story.CardDefs.FortuneAndFolly qualified as StoryCardDefs (theStakeout)
import Arkham.Token (Token (Clue, Damage, Horror, Resource), setTokens)
import Arkham.Campaign.Option (CampaignOption (..))
import Arkham.Campaigns.TheDreamEaters.Meta (CampaignPart (TheDreamQuest))
import Arkham.ClassSymbol (ClassSymbol (Guardian, Rogue, Seeker))
import Arkham.Classes.HasGame (getGame)
import Arkham.Cost qualified as Cost
import Arkham.Criteria qualified as Criteria
import Arkham.Deck qualified as Deck
import Arkham.Difficulty (Difficulty (Easy, Standard))
import Arkham.Decklist (ArkhamDBDecklist (..))
import Arkham.Decklist.CardPool (ArkhamBuildCardPool (..))
import Arkham.Draw.Types (newCardDraw)
import Arkham.Draw.Types qualified as Draw
import Arkham.Epic.Types (SharedEventState (..))
import Arkham.Event.Cards qualified as EventCards
import Arkham.Game.State (GameState (IsActive, IsChooseDecks, IsOver, IsPending))
import Arkham.Game.Settings (AsIfRuling (Chapter1AsIfRuling))
import Arkham.GameValue qualified as GameValue
import Arkham.Homebrew.DarkMatter.CardDefs.Enemies qualified as DarkMatterCards
import Arkham.Helpers.Message qualified as MessageHelpers (createEnemy)
import Arkham.Helpers.Scenario (scenarioField)
import Arkham.Investigator.Cards qualified as InvestigatorCards
import Arkham.Investigator.Types qualified as Investigator
import Arkham.Location.Types qualified as Location
import Arkham.Location.CardDefs.NightOfTheZealot.TheGathering qualified as Locations
import Arkham.Matcher
  ( ActMatcher (ActWithId)
  , AssetMatcher (AnyAsset)
  , CardMatcher (AnyCard)
  , EnemyMatcher (EnemyWithId)
  , InvestigatorMatcher (InvestigatorWithId)
  , LocationMatcher (LocationIs, LocationWithInvestigator)
  , TreacheryMatcher (TreacheryWithId)
  , WindowMatcher (RoundEnds)
  )
import Arkham.Matcher qualified as Matcher
import Arkham.Message qualified as Msg (storyWithCards)
import Arkham.Message.Lifted.Choose (chooseTargetM)
import Arkham.Message.Lifted.Location (unsafeReveal)
import Arkham.Message.Lifted.Move (placeAllAt)
import Arkham.Movement (Destination (ToLocation), Movement (..), MovementMeans (Direct))
import Arkham.Name (mkName)
import Arkham.Phase
  ( EnemyPhaseStep (ResolveAttacksStep)
  , InvestigationPhaseStep (InvestigatorTakesActionStep)
  , MythosPhaseStep (EachInvestigatorDrawsEncounterCardStep)
  , Phase (EnemyPhase, InvestigationPhase, MythosPhase, UpkeepPhase)
  , PhaseStep (EnemyPhaseStep, InvestigationPhaseStep, MythosPhaseStep, UpkeepPhaseStep)
  , UpkeepPhaseStep (UpkeepPhaseEndsStep)
  )
import Arkham.Replay.Checkpoint (canonicalQuestionSha256)
import Arkham.Replay.ImportAuthority (ReplayImportReceipt)
import Arkham.Scenario.Types (Field (ScenarioDiscard, ScenarioSetAsideCards), Scenario)
import Arkham.Timing qualified as Timing
import Arkham.Treachery.CardDefs.NightOfTheZealot qualified as WeaknessCards
import Arkham.Treachery.CardDefs.NightOfTheZealot.StrikingFear qualified as TreacheryCards
import Arkham.Treachery.Types qualified as Treachery
import Arkham.Window qualified as Window
import Arkham.UltimatumsAndBoons.Types
  ( Boon (BoonOfHades)
  , Ultimatum (UltimatumOfChaos)
  , UltimatumOrBoon (Boon, Ultimatum)
  )
import Base.Api.Types.Account
import Base.Api.Types.Capabilities
import Base.Api.Types.LocaleCatalog (localeCatalogCapability)
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
import Entity.Answer (Answer (..), QuestionResponse (..), Reply (..), handleAnswerPure)
import Entity.Arkham.Achievement qualified as AchievementEntity
import Entity.Arkham.Deck qualified as DeckEntity
import Entity.Arkham.Game qualified as ArkhamGame
import Entity.Notification (Notification (..))
import Helpers.Contracts (loadContractJson)
import System.IO.Error qualified as IOError
import Helpers.LocaleCatalog (catalogEnvFor, loadSyntheticCatalog, runtimeCapabilities)
import System.IO.Unsafe (unsafePerformIO)
import System.Random (mkStdGen)
import TestImport
import TestImport.New qualified as New

loadFixture :: FilePath -> IO Aeson.Value
loadFixture fileName = loadContractJson ("contracts/fixtures/" <> fileName)

loadQuestionFixture :: FilePath -> IO (Question Message)
loadQuestionFixture fileName = do
  fixture <- loadFixture fileName
  case Aeson.fromJSON fixture of
    Aeson.Error err ->
      expectationFailure ("Could not decode " <> fileName <> ": " <> err)
        >> error "unreachable"
    Aeson.Success question -> pure question

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

{- | The exact production 'Read'\/'BasicReadChoices' setup-instructions prompt
and the 'ChooseOne'\/'TargetLabel(LocationTarget)' 'startAt' prompt that
follows it -- the same two questions 'buildFixtureBoardGame' above already
dismisses via 'chooseOnlyOption' on its way to the fully-set-up board, captured
here instead of dismissed. This rebuilds the identical deterministic sequence
(same 'fixtureBoardScenario'\/'fixtureBoardInvestigator'\/'fixtureBoardSeed',
and the same real 'StandaloneSetup'\/'Setup'\/'EndSetup' production handlers)
so both questions are read directly off 'gameQuestion' at the exact points the
setup flow presents them, rather than being reconstructed in parallel.

- The first is queued by 'setupTheGathering's opening 'setup $ ul do
  ...' block (Scenarios\/NightOfTheZealot\/TheGathering.hs), which routes
  through 'Arkham.Helpers.FlavorText.setup' -> 'flavor' -> 'Arkham.Message.story'
  -- the exact 'Read flavorText (BasicReadChoices [Label "$continue" []])
  Nothing' shape 'storyWithContinue'\/'story' both build (Message.hs).
- The second is 'startAt's own 'chooseOneM lead $ targeting lid $ reveal lid
  >> placeAllAt lid' (Scenario\/Setup.hs), reached only once the first
  question is dismissed, revealing/entering the single starting location
  ("Study", the real 'd5a66e84-c729-4066-8475-d8a155609025' from
  get-game.json).
-}
buildFixtureOpeningQuestions :: IO (Question Message, Question Message)
buildFixtureOpeningQuestions = do
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
    setupQuestion <-
      lookupFixturePlayerQuestion "buildFixtureOpeningQuestions (setup instructions)"
    chooseOnlyOption "advance past The Gathering's setup instructions"
    startAtQuestion <- lookupFixturePlayerQuestion "buildFixtureOpeningQuestions (startAt)"
    pure (setupQuestion, startAtQuestion)
 where
  lookupFixturePlayerQuestion label = do
    questionMap <- gameQuestion <$> getGame
    case Map.lookup fixturePlayerId questionMap of
      Just question -> pure question
      Nothing ->
        liftIO
          $ IOError.ioError
          $ IOError.userError
          $ label
          <> ": fixture player has no active question"

fixtureOpeningQuestions :: (Question Message, Question Message)
fixtureOpeningQuestions = unsafePerformIO buildFixtureOpeningQuestions
{-# NOINLINE fixtureOpeningQuestions #-}

-- | The setup-instructions 'Read'\/'BasicReadChoices' continue prompt; see
-- 'buildFixtureOpeningQuestions'.
fixtureIntroReadQuestion :: Question Message
fixtureIntroReadQuestion = fst fixtureOpeningQuestions

-- | The 'startAt' 'ChooseOne'\/'TargetLabel(LocationTarget)' prompt with the
-- single real "Study" starting location; see 'buildFixtureOpeningQuestions'.
fixtureStartAtChooseOneQuestion :: Question Message
fixtureStartAtChooseOneQuestion = snd fixtureOpeningQuestions

{- | The real pre-scenario narrative prompt emitted by 'TheGathering's
'PreScenarioSetup' handler through
@flavor $ scope "intro" do h "title"; p "body"@. It is generated from the
same deterministic game seed and production handler as the live prompt, but
in a separate run so introducing this previously omitted lifecycle step does
not renumber the established setup\/location fixture UUIDs above.
-}
fixtureScenarioIntroReadQuestion :: Question Message
fixtureScenarioIntroReadQuestion = unsafePerformIO do
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
    pushAndRunAll [StandaloneSetup, PreScenarioSetup]
    questionMap <- gameQuestion <$> getGame
    case Map.lookup fixturePlayerId questionMap of
      Just question -> pure question
      Nothing ->
        liftIO
          $ IOError.ioError
          $ IOError.userError
            "fixtureScenarioIntroReadQuestion: fixture player has no active question"
{-# NOINLINE fixtureScenarioIntroReadQuestion #-}

{- | The non-null 'readCards' branch of the exact same production 'Read'
constructor, built via the real (pure, no 'ReverseQueue' needed)
'Arkham.Message.storyWithCards' -- the sibling of 'story' (which
'setupTheGathering's own setup-intro prompt above uses, always with
'readCards = Nothing') that supplies 'Just' a card-code list, e.g. a
scenario's "cards added to the encounter deck" story beat. Reuses the real
'EnemyCards.swarmOfRats' card def already used by 'fixtureUuidEntityMap'
above and a sanitized, non-narrative flavor body ('Arkham.Text.ft', which
builds the real 'BasicEntry' 'FlavorTextEntry' constructor) purely to prove
the wire shape; no official card or scenario text is reproduced.
-}
fixtureReadWithCardsQuestion :: Question Message
fixtureReadWithCardsQuestion =
  case Msg.storyWithCards
    [EnemyCards.swarmOfRats]
    [fixturePlayerId]
    (ft "Contract fixture flavor text.") of
    AskMap askMap ->
      fromMaybe
        (error "fixtureReadWithCardsQuestion: fixture player has no active question")
        (Map.lookup fixturePlayerId askMap)
    other -> error $ "fixtureReadWithCardsQuestion: expected AskMap, got " <> show other

{- | A deterministic opening hand built from real player-card values with fixed
card-instance ids. The production 'InvestigatorMulligan' handler below reads
this exact hand and constructs the governed question; the fixture does not
reimplement or hand-assemble any 'UI Message' constructor.
-}
fixtureMulliganCards :: [Card]
fixtureMulliganCards =
  [ lookupCard AssetCards.machete (unsafeMakeCardId $ UUID.fromWords 0 0 0 n)
  | n <- [960, 961, 962]
  ]

{- | The real production opening mulligan prompt emitted by
'Arkham.Investigator.Runner' for a replaceable hand: the localized
@"$label.doneWithMulligan"@ 'Label' followed by one
'TargetLabel'\/'CardIdTarget' per hand card, in authoritative hand order.

The surrounding deterministic board is reused only as a fully initialized
game environment. Its existing player-window question is cleared, the fixed
real cards above are installed as Roland's hand, and the actual
'InvestigatorMulligan' message is run through the ordinary game/investigator
dispatch before the resulting question is read back.
-}
fixtureMulliganQuestion :: Question Message
fixtureMulliganQuestion = unsafePerformIO $ runAgainstFixtureBoardGame do
  let iid = InvestigatorId "01001"
  overTest (questionL .~ mempty)
  overTest
    ( entitiesL
        . investigatorsL
        . ix iid
        %~ overAttrs (\attrs -> attrs {investigatorHand = fixtureMulliganCards})
    )
  pushAndRunAll [InvestigatorMulligan iid]
  questionMap <- gameQuestion <$> getGame
  case Map.lookup fixturePlayerId questionMap of
    Just question -> pure question
    Nothing ->
      liftIO
        $ IOError.ioError
        $ IOError.userError "fixtureMulliganQuestion: fixture player has no active question"
{-# NOINLINE fixtureMulliganQuestion #-}

{- | One fixed, real Magnifying Glass with Roland as its owner. Its Fast
playability and intellect icon cause the production window and commit
handlers to emit the same @TargetLabel(CardIdTarget)@ choice form observed in
the live basic-investigate flow.
-}
fixtureInvestigationCard :: Card
fixtureInvestigationCard =
  overPlayerCard (setPlayerCardOwner $ InvestigatorId "01001")
    $ lookupCard AssetCards.magnifyingGlass
    $ unsafeMakeCardId
    $ UUID.fromWords 0 0 0 963

fixtureInvestigateChoice :: UI Message
fixtureInvestigateChoice =
  case drop 3 fixtureBasicChoiceChoices of
    choice@AbilityLabel {} : _ -> choice
    _ ->
      error
        $ "fixtureInvestigateChoice: expected CORE investigate ability at zero-based index 3, got "
        <> show fixtureBasicChoiceChoices

{- | The real production prompt sequence for one basic investigation. Starting
from the deterministic post-setup board, this selects the actual CORE
investigate ability, skips both real fast windows, starts the actual skill
test, reveals a deterministic zero token, and stops at the authoritative
apply-results question.

The fixed Magnifying Glass remains in hand so both the fast-window and
commit-to-test handlers produce genuine @TargetLabel(CardIdTarget)@ choices
alongside the governed control buttons.
-}
fixtureInvestigationQuestions :: [Question Message]
fixtureInvestigationQuestions = unsafePerformIO $ runAgainstFixtureBoardGame do
  let iid = InvestigatorId "01001"
      lookupQuestion label = do
        questionMap <- gameQuestion <$> getGame
        case Map.lookup fixturePlayerId questionMap of
          Just question -> pure question
          Nothing ->
            liftIO
              $ IOError.ioError
              $ IOError.userError
              $ label
              <> ": fixture player has no active question"
  overTest (questionL .~ mempty)
  replaceCard (toCardId fixtureInvestigationCard) fixtureInvestigationCard
  overTest
    ( entitiesL
        . investigatorsL
        . ix iid
        %~ overAttrs
          ( \attrs ->
              attrs
                { investigatorHand = [fixtureInvestigationCard]
                , investigatorTokens = setTokens Resource 5 (investigatorTokens attrs)
                }
          )
    )
  pushAndRunAll [SetChaosTokens [Zero]]
  pushAndRunAll [uiToRun fixtureInvestigateChoice]
  firstWindow <- lookupQuestion "fixtureInvestigationQuestions (first fast window)"
  skip
  commitQuestion <- lookupQuestion "fixtureInvestigationQuestions (commit/start)"
  chooseOptionMatching "start fixture investigation skill test" \case
    StartSkillTestButton {} -> True
    _ -> False
  secondWindow <- lookupQuestion "fixtureInvestigationQuestions (second fast window)"
  skip
  applyQuestion <- lookupQuestion "fixtureInvestigationQuestions (apply results)"
  pure [firstWindow, commitQuestion, secondWindow, applyQuestion]
{-# NOINLINE fixtureInvestigationQuestions #-}

fixtureInvestigationQuestionFixtures :: [(FilePath, Question Message)]
fixtureInvestigationQuestionFixtures =
  zip
    [ "question-investigate-fast-window.json"
    , "question-investigate-commit.json"
    , "question-investigate-reveal-window.json"
    , "question-investigate-apply-results.json"
    ]
    fixtureInvestigationQuestions

-- | Run the real mythos-phase queue on the deterministic, initialized board.
-- The phase runner (not this fixture) builds the encounter-deck target and
-- nested DrawCards value via ForInvestigator/AllDrawEncounterCard.
fixtureEncounterDrawGame :: Game
fixtureEncounterDrawGame = unsafePerformIO $ runAgainstFixtureBoardGame do
  overTest (questionL .~ mempty)
  pushAndRunAll [Begin MythosPhase]
  getGame
{-# NOINLINE fixtureEncounterDrawGame #-}

fixtureEncounterDrawQuestion :: Question Message
fixtureEncounterDrawQuestion =
  fromMaybe
    (error "fixtureEncounterDrawQuestion: fixture player has no active question")
    (Map.lookup fixturePlayerId $ gameQuestion fixtureEncounterDrawGame)

fixtureEnemyAttackId :: EnemyId
fixtureEnemyAttackId = EnemyId $ UUID.fromWords 0 0 0 902

fixtureEnemyAttackCard :: Card
fixtureEnemyAttackCard =
  lookupCard EnemyCards.swarmOfRats (unsafeMakeCardId $ UUID.fromWords 0 0 0 903)

{- | Generate the first enemy-phase attack prompt from the production engine.
Starting from the deterministic post-setup board, the real enemy-creation
helper builds a fixed Swarm of Rats spawn engaged with Roland, the ordinary
'CreateEnemy' handler resolves that spawn, and 'Begin EnemyPhase' runs the real
enemy-phase queue through 'EnemiesAttack'. The resulting question is captured
directly from 'gameQuestion'; neither the question nor its attack details are
assembled by the fixture.
-}
fixtureEnemyAttackGame :: Game
fixtureEnemyAttackGame = unsafePerformIO $ runAgainstFixtureBoardGame do
  let iid = InvestigatorId "01001"
  overTest (questionL .~ mempty)
  creation <- MessageHelpers.createEnemy fixtureEnemyAttackCard iid
  pushAndRunAll [CreateEnemy creation {enemyCreationEnemyId = fixtureEnemyAttackId}]
  pushAndRunAll [Begin EnemyPhase]
  getGame
{-# NOINLINE fixtureEnemyAttackGame #-}

fixtureEnemyAttackQuestion :: Question Message
fixtureEnemyAttackQuestion =
  fromMaybe
    (error "fixtureEnemyAttackQuestion: fixture player has no active question")
    (Map.lookup fixturePlayerId $ gameQuestion fixtureEnemyAttackGame)

fixtureEnemyAttackAnswer :: Aeson.Value
fixtureEnemyAttackAnswer =
  Aeson.object
    [ "tag" .= ("Answer" :: Text)
    , "contents"
        .= Aeson.object
          [ "choice" .= (0 :: Int)
          , "playerId" .= fixturePlayerId
          , "questionVersion" .= gameScenarioSteps fixtureEnemyAttackGame
          ]
    ]

fixtureDamageAssignmentEnemyId :: EnemyId
fixtureDamageAssignmentEnemyId = EnemyId $ UUID.fromWords 0 0 0 904

fixtureDamageAssignmentEnemyCard :: Card
fixtureDamageAssignmentEnemyCard =
  lookupCard GhoulCards.ghoulMinion (unsafeMakeCardId $ UUID.fromWords 0 0 0 905)

{- | Build the ordinary investigation-phase action menu with one real Ghoul
Minion engaged with Roland. The production enemy creation and player-window
handlers add the basic Fight and Evade 'AbilityLabel' values beside the
existing resource, draw, end-turn, and investigate choices; the fixture does
not construct any choice or ability directly.
-}
fixtureEnemyActionGame :: Game
fixtureEnemyActionGame = unsafePerformIO $ runAgainstFixtureBoardGame do
  let iid = InvestigatorId "01001"
  overTest (questionL .~ mempty)
  creation <- MessageHelpers.createEnemy fixtureDamageAssignmentEnemyCard iid
  pushAndRunAll
    [CreateEnemy creation {enemyCreationEnemyId = fixtureDamageAssignmentEnemyId}]
  pushAndRunAll [PlayerWindow iid [] False False]
  getGame
{-# NOINLINE fixtureEnemyActionGame #-}

fixtureEnemyActionQuestion :: Question Message
fixtureEnemyActionQuestion =
  fromMaybe
    (error "fixtureEnemyActionQuestion: fixture player has no active question")
    (Map.lookup fixturePlayerId $ gameQuestion fixtureEnemyActionGame)

{- | Build the ordinary investigation-phase action menu after the production
enemy-evasion flow has exhausted and disengaged the same real Ghoul Minion.
The subsequent production 'PlayerWindow' replaces Evade with Engage while
retaining the backend-owned Fight choice; the fixture does not construct or
rewrite either ability.
-}
fixtureEngageActionGame :: Game
fixtureEngageActionGame = unsafePerformIO $ runAgainstFixtureBoardGame do
  prepareFixtureEngageAction
  getGame
{-# NOINLINE fixtureEngageActionGame #-}

prepareFixtureEngageAction :: TestAppT ()
prepareFixtureEngageAction = do
  let iid = InvestigatorId "01001"
  overTest (questionL .~ mempty)
  creation <- MessageHelpers.createEnemy fixtureDamageAssignmentEnemyCard iid
  pushAndRunAll
    [CreateEnemy creation {enemyCreationEnemyId = fixtureDamageAssignmentEnemyId}]
  pushAndRunAll [EnemyEvaded iid fixtureDamageAssignmentEnemyId]
  pushAndRunAll [PlayerWindow iid [] False False]

fixtureEngageActionQuestion :: Question Message
fixtureEngageActionQuestion =
  fromMaybe
    (error "fixtureEngageActionQuestion: fixture player has no active question")
    (Map.lookup fixturePlayerId $ gameQuestion fixtureEngageActionGame)

fixtureRolandDefeatEnemyId :: EnemyId
fixtureRolandDefeatEnemyId =
  EnemyId
    $ fromMaybe
      (error "fixtureRolandDefeatEnemyId: invalid UUID")
      (UUID.fromText "6420b429-88f1-44b2-9a51-e5eb8ae44598")

fixtureRolandDefeatEnemyCard :: Card
fixtureRolandDefeatEnemyCard =
  lookupCard EnemyCards.swarmOfRats (unsafeMakeCardId $ UUID.fromWords 0 0 0 907)

{- | Reproduce the exact post-Fight Roland Banks reaction through the ordinary
production game queue. A real Swarm of Rats is created engaged with Roland,
the backend-owned basic Fight ability is selected, a deterministic zero token
resolves the skill test, and the resulting one damage defeats the enemy. The
isolated fixture seeds only the version counter so its six production prompt
increments stop at the authoritative Q32 optional reaction window; it never
constructs an 'AbilityLabel', 'Window', or 'Question' directly.
-}
prepareFixtureRolandDefeatReaction :: TestAppT ()
prepareFixtureRolandDefeatReaction = do
  let iid = InvestigatorId "01001"
  overTest \game ->
    game
      { gameQuestion = mempty
      , gameScenarioSteps = 26
      }
  creation <- MessageHelpers.createEnemy fixtureRolandDefeatEnemyCard iid
  pushAndRunAll [CreateEnemy creation {enemyCreationEnemyId = fixtureRolandDefeatEnemyId}]
  pushAndRunAll [SetChaosTokens [Zero]]
  pushAndRunAll [PlayerWindow iid [] False False]
  chooseOptionMatching "fight the fixture Swarm of Rats" \case
    AbilityLabel _ ability _ _ _ ->
      abilitySource ability == EnemySource fixtureRolandDefeatEnemyId
        && abilityActions ability == [Action.Fight]
    _ -> False
  New.startSkillTest
  New.applyResults

fixtureRolandDefeatReactionGame :: Game
fixtureRolandDefeatReactionGame = unsafePerformIO $ runAgainstFixtureBoardGame do
  prepareFixtureRolandDefeatReaction
  getGame
{-# NOINLINE fixtureRolandDefeatReactionGame #-}

fixtureRolandDefeatReactionQuestion :: Question Message
fixtureRolandDefeatReactionQuestion =
  fromMaybe
    (error "fixtureRolandDefeatReactionQuestion: fixture player has no active question")
    (Map.lookup fixturePlayerId $ gameQuestion fixtureRolandDefeatReactionGame)

fixtureCoverUpTreacheryId :: TreacheryId
fixtureCoverUpTreacheryId =
  TreacheryId
    $ fromMaybe
      (error "fixtureCoverUpTreacheryId: invalid UUID")
      (UUID.fromText "fef723b4-ae76-4183-9441-b4f3cb8b1eb5")

fixtureCoverUpCard :: Card
fixtureCoverUpCard =
  lookupCard WeaknessCards.coverUp (unsafeMakeCardId $ UUID.fromWords 0 0 0 908)

{- | Continue the production Roland defeat flow into Cover Up's exact
'WouldDiscoverClues' replacement window. The weakness is created through the
ordinary treachery message before the fight, so its owner, placement, clue
tokens, ability, and trigger are all engine-produced. Selecting Roland's Q32
reaction then stops naturally at Q33; no 'AbilityLabel', 'Window', or
'Question' is constructed directly.
-}
prepareFixtureCoverUpReaction :: TestAppT ()
prepareFixtureCoverUpReaction = do
  let iid = InvestigatorId "01001"
  pushAndRunAll
    [CreateTreacheryAt fixtureCoverUpTreacheryId fixtureCoverUpCard (InThreatArea iid)]
  prepareFixtureRolandDefeatReaction
  chooseOptionMatching "use Roland's post-defeat reaction" \case
    AbilityLabel _ ability _ _ _ ->
      abilitySource ability == InvestigatorSource iid
        && abilityCardCode ability == "01001"
        && abilityIndex ability == 1
    _ -> False

fixtureCoverUpReactionGame :: Game
fixtureCoverUpReactionGame = unsafePerformIO $ runAgainstFixtureBoardGame do
  prepareFixtureCoverUpReaction
  getGame
{-# NOINLINE fixtureCoverUpReactionGame #-}

fixtureCoverUpReactionQuestion :: Question Message
fixtureCoverUpReactionQuestion =
  fromMaybe
    (error "fixtureCoverUpReactionQuestion: fixture player has no active question")
    (Map.lookup fixturePlayerId $ gameQuestion fixtureCoverUpReactionGame)

fixtureGatheringActObjectiveId :: ActId
fixtureGatheringActObjectiveId = ActId "01108"

fixtureGatheringNextActId :: ActId
fixtureGatheringNextActId = ActId "01109"

fixtureGatheringActObjectiveEnemyId :: EnemyId
fixtureGatheringActObjectiveEnemyId =
  EnemyId
    $ fromMaybe
      (error "fixtureGatheringActObjectiveEnemyId: invalid UUID")
      (UUID.fromText "2d2310e9-f351-4688-aa55-e33666bb356c")

fixtureGatheringActObjectiveEnemyCard :: Card
fixtureGatheringActObjectiveEnemyCard =
  lookupCard GhoulCards.ghoulMinion (unsafeMakeCardId $ UUID.fromWords 0 0 0 909)

fixtureGatheringActObjectiveHand :: [Card]
fixtureGatheringActObjectiveHand =
  zipWith
    ownedCard
    [ AssetCards.knife
    , AssetCards.beatCop
    , AssetCards.fortyFiveAutomatic
    , AssetCards.machete
    , EventCards.emergencyCache
    , AssetCards.guardDog
    ]
    [ "f971e011-b98d-4583-81b0-d2a378866698"
    , "42f70875-1782-4fa6-a034-c16a3406e8f9"
    , "001b6b9f-fc30-413d-855c-039ffcba4d15"
    , "1b215bc4-4405-41ac-ba8d-75bd2e4dab9a"
    , "d2458ecc-5f9f-4ba7-8b58-1474e70e8806"
    , "377b1268-96b7-4806-b999-5574020a28f6"
    ]
 where
  ownedCard cardDef uuidText =
    case lookupCard cardDef (unsafeMakeCardId $ parseUuid uuidText) of
      PlayerCard card -> PlayerCard card {pcOwner = Just (InvestigatorId "01001")}
      _ -> error "fixtureGatheringActObjectiveHand: expected a player card"
  parseUuid uuidText =
    fromMaybe
      (error $ "fixtureGatheringActObjectiveHand: invalid UUID " <> show uuidText)
      (UUID.fromText uuidText)

{- | Continue the production Cover Up skip branch into the first Gathering act
objective. Skipping the replacement effect completes Roland's pending clue
discovery, leaving him with the two clues the server requires before it offers
Trapped's group-clue objective at source index 12. The objective choice and
all preceding player-window choices remain entirely engine-produced.
-}
prepareFixtureGatheringActObjective :: TestAppT ()
prepareFixtureGatheringActObjective = do
  let iid = InvestigatorId "01001"
  setAsideCards <- scenarioField ScenarioSetAsideCards
  for_ setAsideCards \card -> replaceCard (toCardId card) card
  overTest
    ( entitiesL
        . investigatorsL
        . ix iid
        %~ overAttrs
          ( \attrs ->
              attrs
                { investigatorTokens = setTokens Clue 1 (investigatorTokens attrs)
                }
          )
    )
  creation <- MessageHelpers.createEnemy fixtureGatheringActObjectiveEnemyCard iid
  pushAndRunAll
    [CreateEnemy creation {enemyCreationEnemyId = fixtureGatheringActObjectiveEnemyId}]
  prepareFixtureCoverUpReaction
  for_ fixtureGatheringActObjectiveHand \card -> replaceCard (toCardId card) card
  overTest
    ( entitiesL
        . investigatorsL
        . ix iid
        %~ overAttrs
          ( \attrs ->
              attrs
                { investigatorHand = fixtureGatheringActObjectiveHand
                , investigatorTokens = setTokens Resource 5 (investigatorTokens attrs)
                }
          )
    )
  chooseOptionMatching "skip Cover Up's replacement reaction" \case
    SkipTriggersButton choiceIid -> choiceIid == iid
    _ -> False

fixtureGatheringActObjectiveGame :: Game
fixtureGatheringActObjectiveGame = unsafePerformIO $ runAgainstFixtureBoardGame do
  prepareFixtureGatheringActObjective
  getGame
{-# NOINLINE fixtureGatheringActObjectiveGame #-}

fixtureGatheringActObjectiveQuestion :: Question Message
fixtureGatheringActObjectiveQuestion =
  fromMaybe
    (error "fixtureGatheringActObjectiveQuestion: fixture player has no active question")
    (Map.lookup fixturePlayerId $ gameQuestion fixtureGatheringActObjectiveGame)

fixtureRoundTransitionTreacheryId :: TreacheryId
fixtureRoundTransitionTreacheryId =
  TreacheryId
    $ fromMaybe
      (error "fixtureRoundTransitionTreacheryId: invalid UUID")
      (UUID.fromText "9e2f9137-ff19-4993-b001-acc24d0d3736")

fixtureRoundTransitionTreacheryCard :: Card
fixtureRoundTransitionTreacheryCard =
  lookupCard
    TreacheryCards.dissonantVoices
    (unsafeMakeCardId $ UUID.fromWords 0 0 0 906)

fixtureRoundTransitionHand :: [Card]
fixtureRoundTransitionHand =
  [ case card of
      PlayerCard playerCard ->
        PlayerCard $ playerCard {pcOwner = Just $ InvestigatorId "01001"}
      other -> other
  | card <- fixtureMulliganCards
  ]

fixtureRoundTransitionAgendaId :: AgendaId
fixtureRoundTransitionAgendaId = AgendaId "01105"

{- | Reproduce the first round transition after the Engage slice using only
production messages. The deterministic board is placed at the real end-of-
upkeep boundary with two doom on Agenda 1, a real Dissonant Voices in Roland's
threat area, and a non-empty real hand. The ordinary EndRoundWindow/EndRound
and Mythos phase queues then generate Q24-Q27 without constructing any UI
choice or nested message in fixture code.
-}
prepareFixtureRoundTransition :: TestAppT ()
prepareFixtureRoundTransition = do
  let iid = InvestigatorId "01001"
  overTest \game ->
    game
      { gameQuestion = mempty
      , gameScenarioSteps = 23
      , gamePhase = UpkeepPhase
      , gamePhaseStep = Just $ UpkeepPhaseStep UpkeepPhaseEndsStep
      , gameCards =
          foldr
            (\card -> Map.insert (toCardId card) card)
            (gameCards game)
            fixtureRoundTransitionHand
      }
  overTest
    ( entitiesL
        . investigatorsL
        . ix iid
        %~ overAttrs
          ( \attrs ->
              attrs
                { investigatorHand = fixtureRoundTransitionHand
                }
          )
    )
  overTest
    ( entitiesL
        . agendasL
        . ix fixtureRoundTransitionAgendaId
        %~ overAttrs (\attrs -> attrs {agendaDoom = 2})
    )
  pushAndRunAll
    [ CreateTreacheryAt
        fixtureRoundTransitionTreacheryId
        fixtureRoundTransitionTreacheryCard
        (InThreatArea iid)
    , EndRoundWindow
    , EndRound
    ]

data RoundTransitionFixtures = RoundTransitionFixtures
  { roundEndForcedGame :: Game
  , agendaAdvanceGame :: Game
  , agendaConsequenceGame :: Game
  , agendaHorrorAssignmentGame :: Game
  , horrorEncounterDrawGame :: Game
  , dissonantVoicesWasDiscarded :: Bool
  , agendaOneWasAdvanced :: Bool
  , horrorBeforeAssignment :: Int
  , horrorAfterAssignment :: Int
  }

fixtureRoundTransition :: RoundTransitionFixtures
fixtureRoundTransition = unsafePerformIO $ runAgainstFixtureBoardGame do
  let iid = InvestigatorId "01001"
  prepareFixtureRoundTransition
  roundEndForcedGame <- getGame
  chooseOptionMatching "resolve Dissonant Voices at the end of the round" \case
    AbilityLabel _ ability _ _ _ ->
      abilitySource ability == TreacherySource fixtureRoundTransitionTreacheryId
        && abilityIndex ability == 1
    _ -> False
  agendaAdvanceGame <- getGame
  dissonantVoicesLeftPlay <-
    selectNone $ TreacheryWithId fixtureRoundTransitionTreacheryId
  encounterDiscard <- scenarioField ScenarioDiscard
  let
    dissonantVoicesWasDiscarded =
      dissonantVoicesLeftPlay
        && toCardCode fixtureRoundTransitionTreacheryCard
          `elem` map toCardCode encounterDiscard
  chooseOptionMatching "advance Agenda 1 with doom" \case
    TargetLabel (AgendaTarget aid) [AdvanceAgendaBy aid' AgendaAdvancedWithDoom] ->
      aid == fixtureRoundTransitionAgendaId && aid' == aid
    _ -> False
  agendaConsequenceGame <- getGame
  agendaOneWasAdvanced <- field AgendaFlipped fixtureRoundTransitionAgendaId
  horrorBeforeAssignment <- field InvestigatorHorror iid
  chooseOptionMatching "take the What's Going On horror consequence" \case
    Label "$nightOfTheZealot.theGathering.label.whatsGoingOn.horror" _ -> True
    _ -> False
  agendaHorrorAssignmentGame <- getGame
  chooseOptionMatching "assign the agenda horror to Roland" \case
    HorrorLabel iid' _ -> iid' == iid
    _ -> False
  horrorEncounterDrawGame <- getGame
  horrorAfterAssignment <- field InvestigatorHorror iid
  pure RoundTransitionFixtures {..}
{-# NOINLINE fixtureRoundTransition #-}

fixtureRoundEndForcedQuestion :: Question Message
fixtureRoundEndForcedQuestion =
  fromMaybe
    (error "fixtureRoundEndForcedQuestion: fixture player has no active question")
    (Map.lookup fixturePlayerId $ gameQuestion fixtureRoundTransition.roundEndForcedGame)

fixtureAgendaAdvanceQuestion :: Question Message
fixtureAgendaAdvanceQuestion =
  fromMaybe
    (error "fixtureAgendaAdvanceQuestion: fixture player has no active question")
    (Map.lookup fixturePlayerId $ gameQuestion fixtureRoundTransition.agendaAdvanceGame)

fixtureAgendaConsequenceQuestion :: Question Message
fixtureAgendaConsequenceQuestion =
  fromMaybe
    (error "fixtureAgendaConsequenceQuestion: fixture player has no active question")
    (Map.lookup fixturePlayerId $ gameQuestion fixtureRoundTransition.agendaConsequenceGame)

fixtureAgendaHorrorAssignmentQuestion :: Question Message
fixtureAgendaHorrorAssignmentQuestion =
  fromMaybe
    (error "fixtureAgendaHorrorAssignmentQuestion: fixture player has no active question")
    (Map.lookup fixturePlayerId $ gameQuestion fixtureRoundTransition.agendaHorrorAssignmentGame)

fixtureHorrorEncounterDrawQuestion :: Question Message
fixtureHorrorEncounterDrawQuestion =
  fromMaybe
    (error "fixtureHorrorEncounterDrawQuestion: fixture player has no active question")
    (Map.lookup fixturePlayerId $ gameQuestion fixtureRoundTransition.horrorEncounterDrawGame)

data DiscardRoundTransitionFixture = DiscardRoundTransitionFixture
  { discardHandSizeBefore :: Int
  , discardHandSizeAfter :: Int
  , discardEncounterDrawGame :: Game
  }

fixtureDiscardRoundTransition :: DiscardRoundTransitionFixture
fixtureDiscardRoundTransition = unsafePerformIO $ runAgainstFixtureBoardGame do
  let iid = InvestigatorId "01001"
  prepareFixtureRoundTransition
  chooseOptionMatching "resolve Dissonant Voices at the end of the round" \case
    AbilityLabel _ ability _ _ _ ->
      abilitySource ability == TreacherySource fixtureRoundTransitionTreacheryId
        && abilityIndex ability == 1
    _ -> False
  chooseOptionMatching "advance Agenda 1 with doom" \case
    TargetLabel (AgendaTarget aid) [AdvanceAgendaBy aid' AgendaAdvancedWithDoom] ->
      aid == fixtureRoundTransitionAgendaId && aid' == aid
    _ -> False
  discardHandSizeBefore <- length <$> field InvestigatorHand iid
  chooseOptionMatching "take the What's Going On discard consequence" \case
    Label "$nightOfTheZealot.theGathering.label.whatsGoingOn.discard" _ -> True
    _ -> False
  discardEncounterDrawGame <- getGame
  discardHandSizeAfter <- length <$> field InvestigatorHand iid
  pure DiscardRoundTransitionFixture {..}
{-# NOINLINE fixtureDiscardRoundTransition #-}

fixtureDiscardEncounterDrawQuestion :: Question Message
fixtureDiscardEncounterDrawQuestion =
  fromMaybe
    (error "fixtureDiscardEncounterDrawQuestion: fixture player has no active question")
    (Map.lookup fixturePlayerId $ gameQuestion fixtureDiscardRoundTransition.discardEncounterDrawGame)

{- | Advance the real enemy-phase flow one prompt beyond the regular attack.
A fixed Ghoul Minion is created engaged with Roland because its printed attack
deals exactly one damage and one horror. The production enemy runner emits the
regular attack choice; 'chooseOptionMatching' resolves that actual choice and
the production investigator damage runner constructs the nested
'QuestionWithSource' / 'QuestionLabel' / 'ChooseOne' assignment prompt.
Nothing in the resulting question or either choice is assembled by this
fixture.
-}
fixtureDamageAssignmentGame :: Game
fixtureDamageAssignmentGame = unsafePerformIO $ runAgainstFixtureBoardGame do
  let iid = InvestigatorId "01001"
  overTest (questionL .~ mempty)
  creation <- MessageHelpers.createEnemy fixtureDamageAssignmentEnemyCard iid
  pushAndRunAll
    [CreateEnemy creation {enemyCreationEnemyId = fixtureDamageAssignmentEnemyId}]
  pushAndRunAll [Begin EnemyPhase]
  chooseOptionMatching "resolve fixture Ghoul Minion attack" \case
    TargetLabel (EnemyTarget eid) [EnemyAttack details] ->
      eid == fixtureDamageAssignmentEnemyId
        && attackEnemy details == fixtureDamageAssignmentEnemyId
    _ -> False
  getGame
{-# NOINLINE fixtureDamageAssignmentGame #-}

fixtureDamageAssignmentQuestion :: Question Message
fixtureDamageAssignmentQuestion =
  fromMaybe
    (error "fixtureDamageAssignmentQuestion: fixture player has no active question")
    (Map.lookup fixturePlayerId $ gameQuestion fixtureDamageAssignmentGame)

fixtureAssignmentAnswer :: Game -> Int -> Aeson.Value
fixtureAssignmentAnswer game choice =
  Aeson.object
    [ "tag" .= ("Answer" :: Text)
    , "contents"
        .= Aeson.object
          [ "choice" .= choice
          , "playerId" .= fixturePlayerId
          , "questionVersion" .= gameScenarioSteps game
          ]
    ]

fixtureDamageAssignmentAnswer :: Int -> Aeson.Value
fixtureDamageAssignmentAnswer = fixtureAssignmentAnswer fixtureDamageAssignmentGame

{- | Resolve each real source-indexed choice from the combined Ghoul Minion
assignment prompt, then capture the next prompt emitted by the ordinary
investigator damage runner. Starting from 'fixtureDamageAssignmentGame'
preserves the exact production attack source, accumulated target arrays, and
question version; 'chooseOptionMatching' dispatches the choice's real messages
rather than reconstructing either continuation.
-}
fixtureRemainingHorrorAssignmentGame :: Game
fixtureRemainingHorrorAssignmentGame = unsafePerformIO $ runAgainstFixtureBoardGame do
  let iid = InvestigatorId "01001"
  overTest $ const fixtureDamageAssignmentGame
  chooseOptionMatching "assign fixture Ghoul Minion damage first" \case
    DamageLabel iid' _ -> iid' == iid
    _ -> False
  getGame
{-# NOINLINE fixtureRemainingHorrorAssignmentGame #-}

fixtureRemainingHorrorAssignmentQuestion :: Question Message
fixtureRemainingHorrorAssignmentQuestion =
  fromMaybe
    (error "fixtureRemainingHorrorAssignmentQuestion: fixture player has no active question")
    (Map.lookup fixturePlayerId $ gameQuestion fixtureRemainingHorrorAssignmentGame)

fixtureRemainingHorrorAssignmentAnswer :: Aeson.Value
fixtureRemainingHorrorAssignmentAnswer =
  fixtureAssignmentAnswer fixtureRemainingHorrorAssignmentGame 0

fixtureRemainingDamageAssignmentGame :: Game
fixtureRemainingDamageAssignmentGame = unsafePerformIO $ runAgainstFixtureBoardGame do
  let iid = InvestigatorId "01001"
  overTest $ const fixtureDamageAssignmentGame
  chooseOptionMatching "assign fixture Ghoul Minion horror first" \case
    HorrorLabel iid' _ -> iid' == iid
    _ -> False
  getGame
{-# NOINLINE fixtureRemainingDamageAssignmentGame #-}

fixtureRemainingDamageAssignmentQuestion :: Question Message
fixtureRemainingDamageAssignmentQuestion =
  fromMaybe
    (error "fixtureRemainingDamageAssignmentQuestion: fixture player has no active question")
    (Map.lookup fixturePlayerId $ gameQuestion fixtureRemainingDamageAssignmentGame)

fixtureRemainingDamageAssignmentAnswer :: Aeson.Value
fixtureRemainingDamageAssignmentAnswer =
  fixtureAssignmentAnswer fixtureRemainingDamageAssignmentGame 0

{- | Three real "The Gathering" location cards (Attic, Hallway, Parlor --
Location\/CardDefs\/NightOfTheZealot\/TheGathering.hs, the exact same
'setupTheGathering' encounter set 'fixtureBoardGame' itself draws from) used
only to prove that a real production 'ChooseOne'\/'TargetLabel(LocationTarget)'
prompt with /multiple/ choices preserves backend order -- something The
Gathering's own single-location 'startAt' can never by itself demonstrate.
-}
fixtureMultiLocationCardDefs :: [CardDef]
fixtureMultiLocationCardDefs = [Locations.attic, Locations.hallway, Locations.parlor]

{- | Fixed, out-of-range 'UUID.fromWords' ids (the same convention
'fixtureUuidEntityMap'\/'fixtureCardCodeEntityMap' above use for their
enemy\/card ids) for 'fixtureMultiLocationCardDefs', paired one-to-one and in
order. Fixed rather than random (unlike 'Arkham.Message.Lifted.Location.placeLocationCard',
whose 'genCard'\/'getRandom' draw is not reproducible for a bare 'TestAppT'
action run outside the real 'GameT' engine dispatch that seeds
'Arkham.GameEnv.instance MonadRandom GameT' from 'HasStdGen'; a real handler
resolves through that path, but a fixture built by calling a "Lifted"
combinator directly, as below, does not) so this fixture is exactly
reproducible.
-}
fixtureMultiLocationIds :: [LocationId]
fixtureMultiLocationIds = [LocationId (UUID.fromWords 0 0 0 n) | n <- [920, 921, 922]]

{- | Real production 'Location' entities (built via the exact 'createLocation'
constructor 'PlaceLocation''s handler uses, Location.hs) for
'fixtureMultiLocationCardDefs' at 'fixtureMultiLocationIds', inserted
directly into a fresh 'fixtureBoardGame' clone's real entity map so the
'Ask' validity check (Game.hs: a 'TargetLabel(LocationTarget)' choice is only
ever parked if its location genuinely exists) finds them, without going
through 'getRandom' at all.
-}
fixtureMultiLocationEntities :: Map LocationId Location
fixtureMultiLocationEntities =
  Map.fromList
    [ (lid, createLocation (lookupCard def (unsafeMakeCardId (UUID.fromWords 0 0 0 cardN))) lid)
    | (lid, def, cardN) <- zip3 fixtureMultiLocationIds fixtureMultiLocationCardDefs [940, 941, 942 :: Word32]
    ]

{- | A real production 'ChooseOne'\/'TargetLabel(LocationTarget)' prompt with
three ordered choices, generalizing 'startAt' (Scenario\/Setup.hs) from its
single real starting location to 'fixtureMultiLocationIds' via the exact
same shared production combinators 'startAt' itself is built from:
'chooseTargetM'\/'targeting' (Message\/Lifted\/Choose.hs), 'unsafeReveal'
(Message\/Lifted\/Location.hs -- the same unconditional branch 'reveal' takes
whenever 'getInSetup' is true, exactly as it is for every real call inside
'setupTheGathering'), and 'placeAllAt' (Message\/Lifted\/Move.hs). Run inside
'runQueueT' against a fresh clone of the already fully set-up
'fixtureBoardGame' (via 'runAgainstFixtureBoardGame', never the shared
'fixtureBoardGame' value itself) after inserting
'fixtureMultiLocationEntities' directly into that clone's real 'Entities'.
-}
fixtureMultiLocationChooseOne :: Question Message
fixtureMultiLocationChooseOne = unsafePerformIO $ runAgainstFixtureBoardGame do
  testApp <- get
  liftIO $ atomicModifyIORef' (game testApp) \g ->
    ( g
        { gameEntities =
            (gameEntities g)
              { entitiesLocations = entitiesLocations (gameEntities g) <> fixtureMultiLocationEntities
              }
        }
    , ()
    )
  runQueueT
    $ chooseTargetM (InvestigatorId "01001") fixtureMultiLocationIds \lid -> do
      unsafeReveal lid
      placeAllAt lid
  runMessages
  questionMap <- gameQuestion <$> getGame
  case Map.lookup fixturePlayerId questionMap of
    Just question -> pure question
    Nothing ->
      liftIO
        $ IOError.ioError
        $ IOError.userError "fixtureMultiLocationChooseOne: fixture player has no active question"
{-# NOINLINE fixtureMultiLocationChooseOne #-}

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

answerFixturePlayerQuestion :: Int -> TestAppT Int
answerFixturePlayerQuestion sourceIndex = do
  currentGame <- getGame
  let
    questionVersion = gameScenarioSteps currentGame
    answer =
      Answer
        QuestionResponse
          { qrChoice = sourceIndex
          , qrPlayerId = Just fixturePlayerId
          , qrQuestionVersion = Just questionVersion
          }
  liftIO (handleAnswerPure currentGame fixturePlayerId answer) >>= \case
    Unhandled reason ->
      liftIO
        $ expectationFailure
        $ "Fixture player answer at source index "
        <> show sourceIndex
        <> " was rejected: "
        <> Text.unpack reason
    Handled messages -> pushAndRunAll (ClearUI : messages)
  pure questionVersion

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

{- | Regression coverage for a real production drift: upstream's
'investigatorCardPool' field (@Maybe ArkhamBuildCardPool@,
Investigator\/Types.hs, added to support @arkham.build@ deck-pool
restrictions) rides along on 'InvestigatorAttrs'\' TH-derived @ToJSON@
because that same instance also has to round-trip full internal game state
(@Arkham.Game.Json@\/@Entities@ persistence, undo, replay). Nothing removed
@cardsUnderneath@ or any other governed field; the actual regression was
this new field silently and *additively* widening the public wire contract:
production started emitting @"cardPool": null@ in every investigator view,
a key @contracts\/schemas\/investigator.schema.json@ (@additionalProperties:
false@) and the governed 0.1.22 fixtures never declared.

The fix is a fork-only compatibility shim in @Arkham\/Game.hs@'s
'WithDeckSize' @ToJSON@ instance -- the single seam every public
investigator view (in-play @investigators@, @otherInvestigators@,
@killedInvestigators@) already passes through to add @deckSize@ -- that
strips @cardPool@ back out before it reaches the wire, restoring exact
0.1.22 behavior without touching 'InvestigatorAttrs'\' own encoder/decoder
(so saved games with a real card pool still round-trip) and without
bumping the contract revision.

This fixture proves both halves at once, using one real, running fixture
investigator (never a hand-authored payload): with a genuine, non-'Nothing'
'investigatorCardPool' actually set via 'cardPoolL',

  * the investigator's own @Investigator@\/'InvestigatorAttrs' encoder still
    emits @cardPool@ with real (non-null) content, proving internal
    persistence is untouched, and
  * the same investigator's public @PublicGame@ projection -- both
    @Aeson.toJSON@ and the @toEncoding@-driven 'viaWireEncoding' wire bytes,
    so a hand-written @toEncoding@ couldn't silently reintroduce the leak on
    just one of those two paths -- has no @cardPool@ key at all (not merely
    a @null@ one), while unrelated existing fields like @cardsUnderneath@
    remain present and unchanged, proving the shim is scoped to exactly the
    one additive key it targets.
-}
fixtureInvestigatorCardPoolShim
  :: (Aeson.Value, Aeson.Value, Aeson.Value)
  -- ^ (internal InvestigatorAttrs encoding, public toJSON, public viaWireEncoding)
fixtureInvestigatorCardPoolShim = unsafePerformIO $ runAgainstFixtureBoardGame do
  let iid = InvestigatorId "01001"
      pool = ArkhamBuildCardPool ["cycle:core"]
  testApp <- get
  liftIO $ atomicModifyIORef' (game testApp) \g ->
    (g & entitiesL . investigatorsL . ix iid %~ overAttrs (cardPoolL ?~ pool), ())
  game <- getGame
  let
    modifiedInvestigator = game ^?! entitiesL . investigatorsL . ix iid
    internalEncoding = Aeson.toJSON modifiedInvestigator
    publicGame = PublicGame fixtureGameId "Contract fixture game" ["Contract fixture log entry."] game
    extractInvestigator :: Aeson.Value -> Aeson.Value
    extractInvestigator = \case
      Aeson.Object top -> case AesonKeyMap.lookup "investigators" top of
        Just (Aeson.Object invs) -> case AesonKeyMap.lookup (AesonKey.fromText "c01001") invs of
          Just investigatorValue -> investigatorValue
          Nothing -> error "fixtureInvestigatorCardPoolShim: missing c01001 in investigators"
        _ -> error "fixtureInvestigatorCardPoolShim: missing investigators object"
      _ -> error "fixtureInvestigatorCardPoolShim: expected a PublicGame object"
  pure
    ( internalEncoding
    , extractInvestigator (Aeson.toJSON publicGame)
    , extractInvestigator (viaWireEncoding publicGame)
    )
{-# NOINLINE fixtureInvestigatorCardPoolShim #-}

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

{- | The active CORE prompt built by the deterministic production setup above.
Its choices are reused verbatim for the sibling constructors so every golden
is driven by the real @Question Message@ and @UI Message@ encoders rather than
by a parallel fixture-only representation.
-}
fixtureBasicChoiceChoices :: [UI Message]
fixtureBasicChoiceChoices =
  case Map.lookup fixturePlayerId (gameQuestion fixtureBoardGame) of
    Just (PlayerWindowChooseOne choices) -> choices
    Just question ->
      error
        $ "fixtureBasicChoiceChoices: expected PlayerWindowChooseOne, got "
        <> show question
    Nothing -> error "fixtureBasicChoiceChoices: fixture player has no active question"

fixtureEndTurnChoice :: UI Message
fixtureEndTurnChoice =
  case fixtureBasicChoiceChoices of
    [_, _, choice@EndTurnButton {}, _] -> choice
    choices ->
      error
        $ "fixtureEndTurnChoice: expected CORE EndTurnButton at zero-based index 2, got "
        <> show choices

fixtureBasicChoiceQuestions :: [(FilePath, Question Message)]
fixtureBasicChoiceQuestions =
  [ ("question-choose-one.json", ChooseOne [fixtureEndTurnChoice])
  , ("question-player-window-choose-one.json", PlayerWindowChooseOne fixtureBasicChoiceChoices)
  , ("question-window-choose-one.json", WindowChooseOne [fixtureEndTurnChoice])
  ]

{- | The Gathering's opening 'Read'\/'ChooseOne(LocationTarget)' prompt slice
(issues #50 and #61): the scenario-intro and setup-instructions continue
prompts, the real single-location 'startAt' prompt, the non-null 'readCards'
'Read' branch, and the multi-location 'ChooseOne' order-preservation proof.
Every fixture here is bound the same way 'fixtureBasicChoiceQuestions' above
is: to both 'Aeson.toJSON' and the real 'Aeson.encode'\/'toEncoding' wire path.
-}
fixtureOpeningQuestionFixtures :: [(FilePath, Question Message)]
fixtureOpeningQuestionFixtures =
  [ ("question-read-scenario-intro.json", fixtureScenarioIntroReadQuestion)
  , ("question-mulligan.json", fixtureMulliganQuestion)
  , ("question-read.json", fixtureIntroReadQuestion)
  , ("question-choose-one-location.json", fixtureStartAtChooseOneQuestion)
  , ("question-read-with-cards.json", fixtureReadWithCardsQuestion)
  , ("question-choose-one-location-multiple.json", fixtureMultiLocationChooseOne)
  ]

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

{- | The exact response a deployment started with @environment@ would serve,
built through the production handler body (@Base.Api.Handler.Capabilities@)
rather than by constructing a 'ServerCapabilities' here, so the fixtures below
are pinned to what the route actually answers.
-}
capabilitiesFor :: [(Text, Text)] -> IO ServerCapabilities
capabilitiesFor environment = case runtimeCapabilities environment of
  Left message -> fail $ "settings failed to parse: " <> Text.unpack message
  Right response -> pure response

{- | Strip exactly the two additive members the locale catalog contributes:
the @localeCatalog@ object and its capability identifier. Everything else must
be untouched, which is what makes the field additive for the Vue client and
for every native client built before it existed.
-}
withoutLocaleCatalog :: Aeson.Value -> Aeson.Value
withoutLocaleCatalog = \case
  Aeson.Object fields ->
    Aeson.Object
      $ AesonKeyMap.mapWithKey withoutCapability
      $ AesonKeyMap.delete "localeCatalog" fields
  value -> value
 where
  withoutCapability key value = case (key, value) of
    ("capabilities", Aeson.Array capabilities) ->
      Aeson.toJSON $ filter (/= Aeson.String localeCatalogCapability) (toList capabilities)
    _ -> value

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
  it "matches the runtime server-capabilities encoder with no catalog configured" do
    fixture <- loadFixture "capabilities.json"
    response <- capabilitiesFor []

    Aeson.toJSON response `shouldBe` fixture
    viaWireEncoding response `shouldBe` fixture

  it "matches the runtime server-capabilities encoder when a locale catalog is advertised" do
    fixture <- loadFixture "capabilities-locale-catalog.json"
    -- Configured from the committed synthetic catalog manifest's real bytes,
    -- so this fixture cannot quietly self-attest a revision or digest that no
    -- artifact in this repository actually has.
    response <- capabilitiesFor . catalogEnvFor =<< loadSyntheticCatalog

    Aeson.toJSON response `shouldBe` fixture
    viaWireEncoding response `shouldBe` fixture

  it "decodes the replay-attestation fixture with a valid receipt digest" do
    _ <-
      loadFixtureField "replay-attestation.json" "importReceipt"
        :: IO ReplayImportReceipt
    pure ()

  it "adds the locale catalog to the legacy response without changing anything else" do
    legacy <- loadFixture "capabilities.json"
    advertised <- loadFixture "capabilities-locale-catalog.json"

    withoutLocaleCatalog advertised `shouldBe` legacy

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

  for_ fixtureOpeningQuestionFixtures \(fileName, question) ->
    it ("matches the real opening-prompt encoder for " <> fileName) do
      fixture <- loadFixture fileName

      Aeson.toJSON question `shouldBe` fixture
      viaWireEncoding question `shouldBe` fixture

  for_ fixtureInvestigationQuestionFixtures \(fileName, question) ->
    it ("matches the real basic-investigation question encoder for " <> fileName) do
      fixture <- loadFixture fileName

      Aeson.toJSON question `shouldBe` fixture
      viaWireEncoding question `shouldBe` fixture

  it "keeps the production basic-investigation controls, CardIdTarget choices, and zero-based answer indices stable" do
    let
      iid = InvestigatorId "01001"
      cid = toCardId fixtureInvestigationCard
      objectKeys choice = case Aeson.toJSON choice of
        Aeson.Object fields -> Set.fromList $ AesonKeyMap.keys fields
        other -> error $ "Expected a choice object, got " <> show other

    case fixtureInvestigationQuestions of
      [ WindowChooseOne
          [ TargetLabel (CardIdTarget firstWindowCardId) _
            , firstSkip@(SkipTriggersButton firstWindowInvestigatorId)
            ]
        , ChooseOne
          [ TargetLabel (CardIdTarget commitCardId) _
            , start@(StartSkillTestButton commitInvestigatorId)
            ]
        , WindowChooseOne
          [ TargetLabel (CardIdTarget revealWindowCardId) _
            , revealSkip@(SkipTriggersButton revealWindowInvestigatorId)
            ]
        , ChooseOne [apply@SkillTestApplyResultsButton]
        ] -> do
          firstWindowCardId `shouldBe` cid
          commitCardId `shouldBe` cid
          revealWindowCardId `shouldBe` cid
          firstWindowInvestigatorId `shouldBe` iid
          commitInvestigatorId `shouldBe` iid
          revealWindowInvestigatorId `shouldBe` iid
          objectKeys firstSkip `shouldBe` Set.fromList ["tag", "investigatorId"]
          objectKeys start `shouldBe` Set.fromList ["tag", "investigatorId"]
          objectKeys revealSkip `shouldBe` Set.fromList ["tag", "investigatorId"]
          objectKeys apply `shouldBe` Set.fromList ["tag"]
      other ->
        expectationFailure
          $ "Expected fast-window, commit, reveal-window, and apply-results questions, got "
          <> show other

  it "binds each production investigation control to its exact source choice index" do
    let
      tags question = case stripQuestionWrappers question of
        ChooseOne choices -> map (lookupValue "tag" . Aeson.toJSON) choices
        WindowChooseOne choices -> map (lookupValue "tag" . Aeson.toJSON) choices
        other -> error $ "Expected a governed basic-choice question, got " <> show other

    map (zip ([0 ..] :: [Int]) . tags) fixtureInvestigationQuestions
      `shouldBe` [ [ (0, Aeson.String "TargetLabel")
                   , (1, Aeson.String "SkipTriggersButton")
                   ]
                 , [ (0, Aeson.String "TargetLabel")
                   , (1, Aeson.String "StartSkillTestButton")
                   ]
                 , [ (0, Aeson.String "TargetLabel")
                   , (1, Aeson.String "SkipTriggersButton")
                   ]
                 , [(0, Aeson.String "SkillTestApplyResultsButton")]
                 ]

  it "matches the real mythos encounter-deck draw prompt on both encoder paths" do
    fixture <- loadFixture "question-encounter-deck-draw.json"
    presentationFixture <- loadFixture "question-presentation-encounter-deck-draw.json"
    Aeson.toJSON fixtureEncounterDrawQuestion `shouldBe` fixture
    viaWireEncoding fixtureEncounterDrawQuestion `shouldBe` fixture
    let presentation = QuestionPresentation.questionPresentation 41 fixtureEncounterDrawQuestion
    Aeson.toJSON presentation `shouldBe` presentationFixture
    viaWireEncoding presentation `shouldBe` presentationFixture
    presentation
      `shouldBe` QuestionPresentation.QuestionPresentation
        41
        "chooseOne"
        1
        [ QuestionPresentation.ChoicePresentation
            0
            QuestionPresentation.DrawEncounterCard
            (Just $ InvestigatorId "01001")
            Nothing
            Nothing
            Nothing
            Nothing
        ]
    schema <- loadContractJson "contracts/schemas/basic-choice-question.schema.json"
    lookupValue "title" (lookupValue "encounterDeckDrawLabel" $ lookupValue "$defs" schema)
      `shouldBe` Aeson.String "Draw encounter card"
    gamePhase fixtureEncounterDrawGame `shouldBe` MythosPhase
    gamePhaseStep fixtureEncounterDrawGame
      `shouldBe` Just (MythosPhaseStep EachInvestigatorDrawsEncounterCardStep)

  it "fails closed unless every governed encounter-draw field and wrapper matches" do
    let
      iid = InvestigatorId "01001"
      otherIid = InvestigatorId "01002"
      exactDraw = newCardDraw GameSource Deck.EncounterDeck 1
      exactChoice = TargetLabel EncounterDeckTarget [DrawCards iid exactDraw]
      hasEncounterDraw question =
        case QuestionPresentation.questionPresentation 41 question of
          QuestionPresentation.QuestionPresentation _ _ _ choices ->
            any
              ( \(QuestionPresentation.ChoicePresentation _ kind _ _ _ _ _) ->
                  kind == QuestionPresentation.DrawEncounterCard
              )
              choices
      drawMutations :: [(Text, Draw.CardDraw Message)]
      drawMutations =
        [ ("source", exactDraw {Draw.cardDrawSource = InvestigatorSource iid})
        , ("deck", exactDraw {Draw.cardDrawDeck = Deck.InvestigatorDeck iid})
        , ("amount", exactDraw {Draw.cardDrawAmount = 2})
        , ("state", exactDraw {Draw.cardDrawState = Draw.InProgress []})
        , ("target", exactDraw {Draw.cardDrawTarget = Just $ InvestigatorTarget iid})
        , ("action", exactDraw {Draw.cardDrawAction = True})
        , ("kind", exactDraw {Draw.cardDrawKind = Draw.StartingHandCardDraw})
        , ("position", exactDraw {Draw.cardDrawPosition = Draw.DrawFromBottom})
        , ( "rules"
          , exactDraw
              { Draw.cardDrawRules = Set.singleton $ Draw.AfterDrawDiscard 1
              }
          )
        , ("andThen", exactDraw {Draw.cardDrawAndThen = Just $ ChooseEndTurn iid})
        , ("discard", exactDraw {Draw.cardDrawDiscard = Just AnyCard})
        ]
      structuralMutations :: [(Text, Question Message)]
      structuralMutations =
        [ ("question kind", ChooseOneAtATime [exactChoice])
        , ("choice count", ChooseOne [exactChoice, exactChoice])
        , ( "target"
          , ChooseOne [TargetLabel (InvestigatorTarget iid) [DrawCards iid exactDraw]]
          )
        , ("message count zero", ChooseOne [TargetLabel EncounterDeckTarget []])
        , ( "message count two"
          , ChooseOne
              [ TargetLabel
                  EncounterDeckTarget
                  [DrawCards iid exactDraw, DrawCards iid exactDraw]
              ]
          )
        ]
    hasEncounterDraw (ChooseOne [exactChoice]) `shouldBe` True
    QuestionPresentation.questionPresentation
      41
      (ChooseOne [TargetLabel EncounterDeckTarget [DrawCards otherIid exactDraw]])
      `shouldBe` QuestionPresentation.QuestionPresentation
        41
        "chooseOne"
        1
        [ QuestionPresentation.ChoicePresentation
            0
            QuestionPresentation.DrawEncounterCard
            (Just otherIid)
            Nothing
            Nothing
            Nothing
            Nothing
        ]
    map
      ( \(fieldName, cardDraw) ->
          ( fieldName
          , hasEncounterDraw
              $ ChooseOne [TargetLabel EncounterDeckTarget [DrawCards iid cardDraw]]
          )
      )
      drawMutations
      `shouldBe` [(fieldName, False) | (fieldName, _) <- drawMutations]
    map
      (\(fieldName, question) -> (fieldName, hasEncounterDraw question))
      structuralMutations
      `shouldBe` [(fieldName, False) | (fieldName, _) <- structuralMutations]
    hasEncounterDraw
      ( ChooseOne
          [ TargetLabel
              EncounterDeckTarget
              [ DrawCards
                  iid
                  exactDraw
                    { Draw.cardDrawAlreadyDrawn =
                        [fixtureRoundTransitionTreacheryCard]
                    }
              ]
          ]
      )
      `shouldBe` False

  it "preserves encounter draw source index zero and the exact versioned Answer" do
    let
      game = fixtureEncounterDrawGame
      expectedDraw = DrawCards (InvestigatorId "01001") $ newCardDraw GameSource Deck.EncounterDeck 1
      answerValue choice version =
        Aeson.object
          [ "tag" .= ("Answer" :: Text)
          , "contents"
              .= Aeson.object
                [ "choice" .= (choice :: Int)
                , "playerId" .= fixturePlayerId
                , "questionVersion" .= (version :: Int)
                ]
          ]
      checkAnswer choice version check =
        case Aeson.fromJSON (answerValue choice version) of
          Aeson.Error err -> expectationFailure $ "Could not decode encounter draw Answer: " <> err
          Aeson.Success answer -> handleAnswerPure game fixturePlayerId answer >>= check
    case fixtureEncounterDrawQuestion of
      ChooseOne choices -> do
        length choices `shouldBe` 1
        choices !!? 0 `shouldBe` Just (TargetLabel EncounterDeckTarget [expectedDraw])
      other -> expectationFailure $ "Expected the mythos encounter draw ChooseOne, got " <> show other
    checkAnswer 0 (gameScenarioSteps game) \case
      Handled messages -> messages `shouldBe` [Run [expectedDraw]]
      Unhandled reason -> expectationFailure $ "Encounter draw Answer rejected: " <> Text.unpack reason
    checkAnswer 0 (gameScenarioSteps game + 1) \case
      Unhandled reason -> reason `shouldBe` "Stale question"
      Handled _ -> expectationFailure "A stale encounter draw Answer must not resolve"
    checkAnswer 1 (gameScenarioSteps game) \case
      Handled messages -> messages `shouldBe` [Ask fixturePlayerId fixtureEncounterDrawQuestion]
      Unhandled reason -> expectationFailure $ "Expected the unchanged prompt: " <> Text.unpack reason

  it "matches the real enemy-phase attack prompt on both encoder paths" do
    fixture <- loadFixture "question-enemy-attack.json"
    Aeson.toJSON fixtureEnemyAttackQuestion `shouldBe` fixture
    viaWireEncoding fixtureEnemyAttackQuestion `shouldBe` fixture
    gamePhase fixtureEnemyAttackGame `shouldBe` EnemyPhase
    gamePhaseStep fixtureEnemyAttackGame
      `shouldBe` Just (EnemyPhaseStep ResolveAttacksStep)
    gameScenarioSteps fixtureEnemyAttackGame `shouldBe` 5

  it "binds every enemy-attack identity and field to the production values" do
    let iid = InvestigatorId "01001"
    case fixtureEnemyAttackQuestion of
      ChooseOneAtATime [TargetLabel (EnemyTarget targetEnemy) [EnemyAttack details]] -> do
        targetEnemy `shouldBe` fixtureEnemyAttackId
        attackEnemy details `shouldBe` targetEnemy
        attackSource details `shouldBe` EnemySource targetEnemy
        attackTarget details `shouldBe` SingleAttackTarget (InvestigatorTarget iid)
        attackOriginalTarget details `shouldBe` SingleAttackTarget (InvestigatorTarget iid)
        attackType details `shouldBe` RegularAttack
        attackDamageStrategy details `shouldBe` DamageAny
        attackExhaustsEnemy details `shouldBe` True
        attackCanBeCanceled details `shouldBe` True
        attackAfter details `shouldBe` []
        attackDamaged details `shouldBe` Map.empty
        attackDealDamage details `shouldBe` True
        attackDespiteExhausted details `shouldBe` False
        attackCancelled details `shouldBe` False
      other ->
        expectationFailure
          $ "Expected one production ChooseOneAtATime enemy attack, got "
          <> show other

  it "preserves enemy attack source index zero and the exact versioned Answer" do
    fixture <- loadFixture "answer-enemy-attack.json"
    fixture `shouldBe` fixtureEnemyAttackAnswer
    let
      game = fixtureEnemyAttackGame
      checkAnswer answerValue check =
        case Aeson.fromJSON answerValue of
          Aeson.Error err -> expectationFailure $ "Could not decode enemy attack Answer: " <> err
          Aeson.Success answer -> handleAnswerPure game fixturePlayerId answer >>= check
      withChoiceAndVersion choice version =
        Aeson.object
          [ "tag" .= ("Answer" :: Text)
          , "contents"
              .= Aeson.object
                [ "choice" .= (choice :: Int)
                , "playerId" .= fixturePlayerId
                , "questionVersion" .= (version :: Int)
                ]
          ]
    case Aeson.fromJSON fixture of
      Aeson.Error err -> expectationFailure $ "Could not decode answer-enemy-attack.json: " <> err
      Aeson.Success (Answer (QuestionResponse choice playerId questionVersion)) -> do
        choice `shouldBe` 0
        playerId `shouldBe` Just fixturePlayerId
        questionVersion `shouldBe` Just (gameScenarioSteps game)
      Aeson.Success other ->
        expectationFailure $ "Expected a versioned Answer fixture, got " <> show other
    case fixtureEnemyAttackQuestion of
      ChooseOneAtATime [choice] -> do
        checkAnswer fixture \case
          Handled messages -> messages `shouldBe` [uiToRun choice]
          Unhandled reason -> expectationFailure $ "Enemy attack Answer rejected: " <> Text.unpack reason
        checkAnswer (withChoiceAndVersion 0 $ gameScenarioSteps game + 1) \case
          Unhandled reason -> reason `shouldBe` "Stale question"
          Handled _ -> expectationFailure "A stale enemy attack Answer must not resolve"
        checkAnswer (withChoiceAndVersion 1 $ gameScenarioSteps game) \case
          Handled messages -> messages `shouldBe` [Ask fixturePlayerId fixtureEnemyAttackQuestion]
          Unhandled reason -> expectationFailure $ "Expected the unchanged prompt: " <> Text.unpack reason
      other ->
        expectationFailure
          $ "Expected one production ChooseOneAtATime enemy attack, got "
          <> show other

  it "matches the real post-attack damage assignment prompt on both encoder paths" do
    fixture <- loadFixture "question-enemy-attack-damage-assignment.json"
    Aeson.toJSON fixtureDamageAssignmentQuestion `shouldBe` fixture
    viaWireEncoding fixtureDamageAssignmentQuestion `shouldBe` fixture
    gamePhase fixtureDamageAssignmentGame `shouldBe` EnemyPhase
    gamePhaseStep fixtureDamageAssignmentGame
      `shouldBe` Just (EnemyPhaseStep ResolveAttacksStep)
    gameScenarioSteps fixtureDamageAssignmentGame `shouldBe` 6

  it "binds both damage-assignment choices to the same production attack and investigator" do
    let
      iid = InvestigatorId "01001"
      source = EnemyAttackSource fixtureDamageAssignmentEnemyId
      expectedDamageMessages =
        [ InvestigatorDamage iid source 1 0
        , InvestigatorDoAssignDamage
            iid
            source
            DamageAny
            AnyAsset
            0
            1
            [InvestigatorTarget iid]
            []
        ]
      expectedHorrorMessages =
        [ InvestigatorDamage iid source 0 1
        , InvestigatorDoAssignDamage
            iid
            source
            DamageAny
            AnyAsset
            1
            0
            []
            [InvestigatorTarget iid]
        ]
    case fixtureDamageAssignmentQuestion of
      QuestionWithSource source' Nothing
        (QuestionLabel label Nothing (ChooseOne [DamageLabel damageIid damageMessages, HorrorLabel horrorIid horrorMessages])) -> do
          source' `shouldBe` source
          label `shouldBe` "Assign 1 damage and 1 horror"
          damageIid `shouldBe` iid
          horrorIid `shouldBe` iid
          damageMessages `shouldBe` expectedDamageMessages
          horrorMessages `shouldBe` expectedHorrorMessages
      other ->
        expectationFailure
          $ "Expected the production 1 damage / 1 horror assignment prompt, got "
          <> show other

  it "preserves both assignment source indices and their exact versioned Answers" do
    damageFixture <- loadFixture "answer-enemy-attack-assign-damage.json"
    horrorFixture <- loadFixture "answer-enemy-attack-assign-horror.json"
    damageFixture `shouldBe` fixtureDamageAssignmentAnswer 0
    horrorFixture `shouldBe` fixtureDamageAssignmentAnswer 1
    let
      game = fixtureDamageAssignmentGame
      checkAnswer answerValue check =
        case Aeson.fromJSON answerValue of
          Aeson.Error err -> expectationFailure $ "Could not decode damage assignment Answer: " <> err
          Aeson.Success answer -> handleAnswerPure game fixturePlayerId answer >>= check
      withChoiceAndVersion choice version =
        Aeson.object
          [ "tag" .= ("Answer" :: Text)
          , "contents"
              .= Aeson.object
                [ "choice" .= (choice :: Int)
                , "playerId" .= fixturePlayerId
                , "questionVersion" .= (version :: Int)
                ]
          ]
      assertFixture expectedChoice fixture =
        case Aeson.fromJSON fixture of
          Aeson.Error err -> expectationFailure $ "Could not decode assignment Answer fixture: " <> err
          Aeson.Success (Answer (QuestionResponse choice playerId questionVersion)) -> do
            choice `shouldBe` expectedChoice
            playerId `shouldBe` Just fixturePlayerId
            questionVersion `shouldBe` Just (gameScenarioSteps game)
          Aeson.Success other ->
            expectationFailure $ "Expected a versioned Answer fixture, got " <> show other
    assertFixture 0 damageFixture
    assertFixture 1 horrorFixture
    case fixtureDamageAssignmentQuestion of
      QuestionWithSource _ _ (QuestionLabel _ _ (ChooseOne choices@[damageChoice, horrorChoice])) -> do
        checkAnswer damageFixture \case
          Handled messages -> messages `shouldBe` [uiToRun damageChoice]
          Unhandled reason -> expectationFailure $ "Damage assignment Answer rejected: " <> Text.unpack reason
        checkAnswer horrorFixture \case
          Handled messages -> messages `shouldBe` [uiToRun horrorChoice]
          Unhandled reason -> expectationFailure $ "Horror assignment Answer rejected: " <> Text.unpack reason
        checkAnswer (withChoiceAndVersion 0 $ gameScenarioSteps game + 1) \case
          Unhandled reason -> reason `shouldBe` "Stale question"
          Handled _ -> expectationFailure "A stale damage assignment Answer must not resolve"
        checkAnswer (withChoiceAndVersion (length choices) $ gameScenarioSteps game) \case
          Handled messages -> messages `shouldBe` [Ask fixturePlayerId fixtureDamageAssignmentQuestion]
          Unhandled reason -> expectationFailure $ "Expected the unchanged prompt: " <> Text.unpack reason
      other ->
        expectationFailure
          $ "Expected two production damage assignment choices, got "
          <> show other

  it "matches both real single-type continuation prompts on both encoder paths" do
    horrorFixture <- loadFixture "question-enemy-attack-remaining-horror-assignment.json"
    damageFixture <- loadFixture "question-enemy-attack-remaining-damage-assignment.json"
    Aeson.toJSON fixtureRemainingHorrorAssignmentQuestion `shouldBe` horrorFixture
    viaWireEncoding fixtureRemainingHorrorAssignmentQuestion `shouldBe` horrorFixture
    Aeson.toJSON fixtureRemainingDamageAssignmentQuestion `shouldBe` damageFixture
    viaWireEncoding fixtureRemainingDamageAssignmentQuestion `shouldBe` damageFixture
    gamePhase fixtureRemainingHorrorAssignmentGame `shouldBe` EnemyPhase
    gamePhase fixtureRemainingDamageAssignmentGame `shouldBe` EnemyPhase
    gamePhaseStep fixtureRemainingHorrorAssignmentGame
      `shouldBe` Just (EnemyPhaseStep ResolveAttacksStep)
    gamePhaseStep fixtureRemainingDamageAssignmentGame
      `shouldBe` Just (EnemyPhaseStep ResolveAttacksStep)
    gameScenarioSteps fixtureRemainingHorrorAssignmentGame `shouldBe` 7
    gameScenarioSteps fixtureRemainingDamageAssignmentGame `shouldBe` 7

  it "binds both single-type continuations to the same production attack and investigator" do
    let
      iid = InvestigatorId "01001"
      source = EnemyAttackSource fixtureDamageAssignmentEnemyId
      completedTargets = [InvestigatorTarget iid]
      expectedHorrorMessages =
        [ InvestigatorDamage iid source 0 1
        , InvestigatorDoAssignDamage
            iid
            source
            DamageAny
            AnyAsset
            0
            0
            completedTargets
            completedTargets
        ]
      expectedDamageMessages =
        [ InvestigatorDamage iid source 1 0
        , InvestigatorDoAssignDamage
            iid
            source
            DamageAny
            AnyAsset
            0
            0
            completedTargets
            completedTargets
        ]
    case fixtureRemainingHorrorAssignmentQuestion of
      QuestionWithSource source' Nothing
        (QuestionLabel label Nothing (ChooseOne [HorrorLabel componentIid messages])) -> do
          source' `shouldBe` source
          label `shouldBe` "Assign 1 horror"
          componentIid `shouldBe` iid
          messages `shouldBe` expectedHorrorMessages
      other ->
        expectationFailure
          $ "Expected the production remaining-horror assignment prompt, got "
          <> show other
    case fixtureRemainingDamageAssignmentQuestion of
      QuestionWithSource source' Nothing
        (QuestionLabel label Nothing (ChooseOne [DamageLabel componentIid messages])) -> do
          source' `shouldBe` source
          label `shouldBe` "Assign 1 damage"
          componentIid `shouldBe` iid
          messages `shouldBe` expectedDamageMessages
      other ->
        expectationFailure
          $ "Expected the production remaining-damage assignment prompt, got "
          <> show other

  it "preserves continuation Answers and exact wrappers on invalid source indices" do
    horrorFixture <- loadFixture "answer-enemy-attack-assign-remaining-horror.json"
    damageFixture <- loadFixture "answer-enemy-attack-assign-remaining-damage.json"
    horrorFixture `shouldBe` fixtureRemainingHorrorAssignmentAnswer
    damageFixture `shouldBe` fixtureRemainingDamageAssignmentAnswer
    let
      checkAnswer game answerValue check =
        case Aeson.fromJSON answerValue of
          Aeson.Error err -> expectationFailure $ "Could not decode continuation Answer: " <> err
          Aeson.Success answer -> handleAnswerPure game fixturePlayerId answer >>= check
      withChoiceAndVersion choice version =
        Aeson.object
          [ "tag" .= ("Answer" :: Text)
          , "contents"
              .= Aeson.object
                [ "choice" .= (choice :: Int)
                , "playerId" .= fixturePlayerId
                , "questionVersion" .= (version :: Int)
                ]
          ]
      assertFixture game fixture =
        case Aeson.fromJSON fixture of
          Aeson.Error err -> expectationFailure $ "Could not decode continuation Answer fixture: " <> err
          Aeson.Success (Answer (QuestionResponse choice playerId questionVersion)) -> do
            choice `shouldBe` 0
            playerId `shouldBe` Just fixturePlayerId
            questionVersion `shouldBe` Just (gameScenarioSteps game)
          Aeson.Success other ->
            expectationFailure $ "Expected a versioned Answer fixture, got " <> show other
      assertContinuation game question fixture =
        case question of
          QuestionWithSource _ _ (QuestionLabel _ _ (ChooseOne [choice])) -> do
            assertFixture game fixture
            checkAnswer game fixture \case
              Handled messages -> messages `shouldBe` [uiToRun choice]
              Unhandled reason ->
                expectationFailure $ "Continuation Answer rejected: " <> Text.unpack reason
            checkAnswer
              game
              (withChoiceAndVersion 0 $ gameScenarioSteps game + 1)
              \case
                Unhandled reason -> reason `shouldBe` "Stale question"
                Handled _ -> expectationFailure "A stale continuation Answer must not resolve"
            checkAnswer
              game
              (withChoiceAndVersion 1 $ gameScenarioSteps game)
              \case
                Handled messages -> messages `shouldBe` [Ask fixturePlayerId question]
                Unhandled reason ->
                  expectationFailure $ "Expected the exact wrapped prompt: " <> Text.unpack reason
          other ->
            expectationFailure
              $ "Expected one wrapped production continuation choice, got "
              <> show other
    assertContinuation
      fixtureRemainingHorrorAssignmentGame
      fixtureRemainingHorrorAssignmentQuestion
      horrorFixture
    assertContinuation
      fixtureRemainingDamageAssignmentGame
      fixtureRemainingDamageAssignmentQuestion
      damageFixture

  it "matches the real engaged-enemy action menu on both encoder paths" do
    fixture <- loadFixture "question-player-window-enemy-actions.json"
    Aeson.toJSON fixtureEnemyActionQuestion `shouldBe` fixture
    viaWireEncoding fixtureEnemyActionQuestion `shouldBe` fixture
    gamePhase fixtureEnemyActionGame `shouldBe` InvestigationPhase
    gamePhaseStep fixtureEnemyActionGame
      `shouldBe` Just (InvestigationPhaseStep InvestigatorTakesActionStep)

  it "binds basic Fight and Evade to their production enemy ability identities and source indices" do
    let
      iid = InvestigatorId "01001"
      enemySource = EnemySource fixtureDamageAssignmentEnemyId
      expectedCardCode = toCardCode fixtureDamageAssignmentEnemyCard
    case fixtureEnemyActionQuestion of
      PlayerWindowChooseOne
        [ ResourceLabel resourceIid _
          , ComponentLabel (InvestigatorDeckComponent drawIid) _
          , EndTurnButton endTurnIid _
          , AbilityLabel investigateIid investigateAbility _ _ _
          , fightChoice@(AbilityLabel fightIid fightAbility _ _ _)
          , evadeChoice@(AbilityLabel evadeIid evadeAbility _ _ _)
          ] -> do
            resourceIid `shouldBe` iid
            drawIid `shouldBe` iid
            endTurnIid `shouldBe` iid
            investigateIid `shouldBe` iid
            fightIid `shouldBe` iid
            evadeIid `shouldBe` iid
            abilityActions investigateAbility `shouldBe` [Action.Investigate]
            abilitySource fightAbility `shouldBe` enemySource
            abilityCardCode fightAbility `shouldBe` expectedCardCode
            abilityIndex fightAbility `shouldBe` 100
            abilityActions fightAbility `shouldBe` [Action.Fight]
            abilitySource evadeAbility `shouldBe` enemySource
            abilityCardCode evadeAbility `shouldBe` expectedCardCode
            abilityIndex evadeAbility `shouldBe` 101
            abilityActions evadeAbility `shouldBe` [Action.Evade]
            let checkChoice sourceIndex choice = do
                  let
                    answerValue version =
                      Aeson.object
                        [ "tag" .= ("Answer" :: Text)
                        , "contents"
                            .= Aeson.object
                              [ "choice" .= (sourceIndex :: Int)
                              , "playerId" .= fixturePlayerId
                              , "questionVersion" .= version
                              ]
                        ]
                    checkAnswer version check =
                      case Aeson.fromJSON (answerValue version) of
                        Aeson.Error err ->
                          expectationFailure
                            $ "Could not decode enemy-action Answer: "
                            <> err
                        Aeson.Success answer ->
                          handleAnswerPure fixtureEnemyActionGame fixturePlayerId answer
                            >>= check
                    expectCurrent = \case
                      Handled messages -> messages `shouldBe` [uiToRun choice]
                      Unhandled reason ->
                        expectationFailure
                          $ "Enemy-action Answer rejected: "
                          <> Text.unpack reason
                    expectStale = \case
                      Unhandled reason -> reason `shouldBe` "Stale question"
                      Handled _ ->
                        expectationFailure "A stale enemy-action Answer must not resolve"
                  checkAnswer (gameScenarioSteps fixtureEnemyActionGame) expectCurrent
                  checkAnswer (gameScenarioSteps fixtureEnemyActionGame + 1) expectStale
            checkChoice 4 fightChoice
            checkChoice 5 evadeChoice
      other ->
        expectationFailure
          $ "Expected the production resource/draw/end/investigate/fight/evade action menu, got "
          <> show other

  it "matches the real post-Evade Engage action menu on both encoder paths" do
    fixture <- loadFixture "question-player-window-engage-action.json"
    Aeson.toJSON fixtureEngageActionQuestion `shouldBe` fixture
    viaWireEncoding fixtureEngageActionQuestion `shouldBe` fixture
    gamePhase fixtureEngageActionGame `shouldBe` InvestigationPhase
    gamePhaseStep fixtureEngageActionGame
      `shouldBe` Just (InvestigationPhaseStep InvestigatorTakesActionStep)

  it "binds basic Engage to its production enemy ability identity and source index" do
    let
      iid = InvestigatorId "01001"
      enemySource = EnemySource fixtureDamageAssignmentEnemyId
      expectedCardCode = toCardCode fixtureDamageAssignmentEnemyCard
    case fixtureEngageActionQuestion of
      PlayerWindowChooseOne
        [ ResourceLabel resourceIid _
          , ComponentLabel (InvestigatorDeckComponent drawIid) _
          , EndTurnButton endTurnIid _
          , AbilityLabel investigateIid investigateAbility _ _ _
          , AbilityLabel fightIid fightAbility _ _ _
          , engageChoice@(AbilityLabel engageIid engageAbility _ _ _)
          ] -> do
            resourceIid `shouldBe` iid
            drawIid `shouldBe` iid
            endTurnIid `shouldBe` iid
            investigateIid `shouldBe` iid
            fightIid `shouldBe` iid
            engageIid `shouldBe` iid
            abilityActions investigateAbility `shouldBe` [Action.Investigate]
            abilitySource fightAbility `shouldBe` enemySource
            abilityCardCode fightAbility `shouldBe` expectedCardCode
            abilityIndex fightAbility `shouldBe` 100
            abilityActions fightAbility `shouldBe` [Action.Fight]
            abilitySource engageAbility `shouldBe` enemySource
            abilityCardCode engageAbility `shouldBe` expectedCardCode
            abilityIndex engageAbility `shouldBe` 102
            abilityActions engageAbility `shouldBe` [Action.Engage]
            let
              answerValue version =
                Aeson.object
                  [ "tag" .= ("Answer" :: Text)
                  , "contents"
                      .= Aeson.object
                        [ "choice" .= (5 :: Int)
                        , "playerId" .= fixturePlayerId
                        , "questionVersion" .= version
                        ]
                  ]
              checkAnswer version check =
                case Aeson.fromJSON (answerValue version) of
                  Aeson.Error err ->
                    expectationFailure
                      $ "Could not decode Engage Answer: "
                      <> err
                  Aeson.Success answer ->
                    handleAnswerPure fixtureEngageActionGame fixturePlayerId answer
                      >>= check
              expectCurrent = \case
                Handled messages -> messages `shouldBe` [uiToRun engageChoice]
                Unhandled reason ->
                  expectationFailure
                    $ "Engage Answer rejected: "
                    <> Text.unpack reason
              expectStale = \case
                Unhandled reason -> reason `shouldBe` "Stale question"
                Handled _ ->
                  expectationFailure "A stale Engage Answer must not resolve"
            checkAnswer (gameScenarioSteps fixtureEngageActionGame) expectCurrent
            checkAnswer (gameScenarioSteps fixtureEngageActionGame + 1) expectStale
      other ->
        expectationFailure
          $ "Expected the production resource/draw/end/investigate/fight/engage action menu, got "
          <> show other

  it "executes the production Engage answer into authoritative enemy placement" do
    let iid = InvestigatorId "01001"
    placement <- runAgainstFixtureBoardGame do
      prepareFixtureEngageAction
      game <- getGame
      let
        answer =
          Answer
            QuestionResponse
              { qrChoice = 5
              , qrPlayerId = Just fixturePlayerId
              , qrQuestionVersion = Just $ gameScenarioSteps game
              }
      liftIO (handleAnswerPure game fixturePlayerId answer) >>= \case
        Unhandled reason ->
          liftIO
            $ expectationFailure
            $ "Engage Answer rejected: "
            <> Text.unpack reason
        Handled messages -> pushAndRunAll (ClearUI : messages)
      field Enemy.EnemyPlacement fixtureDamageAssignmentEnemyId
    placement `shouldBe` InThreatArea iid

  it "matches the exact production Roland Banks defeat reaction on both encoder paths and canonical replay digest" do
    fixture <- loadFixture "question-roland-defeat-reaction.json"
    Aeson.toJSON fixtureRolandDefeatReactionQuestion `shouldBe` fixture
    viaWireEncoding fixtureRolandDefeatReactionQuestion `shouldBe` fixture
    canonicalQuestionSha256 fixtureRolandDefeatReactionQuestion
      `shouldBe` "7ff7e00af7be0a2b933ed1a817e7e2a54d3eaa5ae2bdce17f73f7153e59f23a9"

  it "binds Roland's optional reaction and skip control to their exact source indices" do
    let
      iid = InvestigatorId "01001"
      source = InvestigatorSource iid
    case fixtureRolandDefeatReactionQuestion of
      WindowChooseOne
        [ reactionChoice@(AbilityLabel choiceIid ability windows beforeMessages messages)
          , skipChoice@(SkipTriggersButton skipIid)
          ] -> do
            choiceIid `shouldBe` iid
            skipIid `shouldBe` iid
            abilitySource ability `shouldBe` source
            abilityRequestor ability `shouldBe` source
            abilityCardCode ability `shouldBe` "01001"
            abilityIndex ability `shouldBe` 1
            abilityType ability `shouldSatisfy` \case
              AbilityType.ReactionAbility {} -> True
              _ -> False
            abilityActions ability `shouldBe` []
            length windows `shouldBe` 1
            beforeMessages `shouldBe` []
            messages `shouldBe` []
            let
              answerValue sourceIndex version =
                Aeson.object
                  [ "tag" .= ("Answer" :: Text)
                  , "contents"
                      .= Aeson.object
                        [ "choice" .= sourceIndex
                        , "playerId" .= fixturePlayerId
                        , "questionVersion" .= version
                        ]
                  ]
              checkAnswer sourceIndex version check =
                case Aeson.fromJSON (answerValue sourceIndex version) of
                  Aeson.Error err ->
                    expectationFailure
                      $ "Could not decode Roland defeat reaction Answer: "
                      <> err
                  Aeson.Success answer ->
                    handleAnswerPure fixtureRolandDefeatReactionGame fixturePlayerId answer
                      >>= check
              expectCurrent expected = \case
                Handled actual -> actual `shouldBe` [uiToRun expected]
                Unhandled reason ->
                  expectationFailure
                    $ "Roland defeat reaction Answer rejected: "
                    <> Text.unpack reason
              expectStale = \case
                Unhandled reason -> reason `shouldBe` "Stale question"
                Handled _ ->
                  expectationFailure "A stale Roland defeat reaction Answer must not resolve"
              currentVersion = gameScenarioSteps fixtureRolandDefeatReactionGame
            currentVersion `shouldBe` 32
            checkAnswer (0 :: Int) currentVersion (expectCurrent reactionChoice)
            checkAnswer (1 :: Int) currentVersion (expectCurrent skipChoice)
            checkAnswer (0 :: Int) (currentVersion - 1) expectStale
            checkAnswer (1 :: Int) (currentVersion - 1) expectStale
      other ->
        expectationFailure
          $ "Expected Roland's production reaction followed by SkipTriggersButton, got "
          <> show other

  it "executes Roland's reaction and skip branches through the authoritative game queue" do
    let
      iid = InvestigatorId "01001"
      runChoice sourceIndex = runAgainstFixtureBoardGame do
        prepareFixtureRolandDefeatReaction
        cluesBefore <- field InvestigatorClues iid
        game <- getGame
        let
          answer =
            Answer
              QuestionResponse
                { qrChoice = sourceIndex
                , qrPlayerId = Just fixturePlayerId
                , qrQuestionVersion = Just $ gameScenarioSteps game
                }
        liftIO (handleAnswerPure game fixturePlayerId answer) >>= \case
          Unhandled reason ->
            liftIO
              $ expectationFailure
              $ "Roland defeat reaction Answer rejected: "
              <> Text.unpack reason
          Handled messages -> pushAndRunAll (ClearUI : messages)
        cluesAfter <- field InvestigatorClues iid
        pure (cluesBefore, cluesAfter)
    reactionClues <- runChoice 0
    skippedClues <- runChoice 1
    reactionClues `shouldSatisfy` \(cluesBefore, cluesAfter) -> cluesAfter == cluesBefore + 1
    skippedClues `shouldSatisfy` \(cluesBefore, cluesAfter) -> cluesAfter == cluesBefore

  it "matches the exact production Cover Up reaction on both encoder paths and canonical replay digest" do
    fixture <- loadFixture "question-cover-up-reaction.json"
    Aeson.toJSON fixtureCoverUpReactionQuestion `shouldBe` fixture
    viaWireEncoding fixtureCoverUpReactionQuestion `shouldBe` fixture
    canonicalQuestionSha256 fixtureCoverUpReactionQuestion
      `shouldBe` "8857d15cd056ac0e4d8556676dfc5746c5f9addaa2054d0b35c245610dbf71b9"

  it "binds Cover Up's optional reaction and skip control to their exact source indices" do
    let
      iid = InvestigatorId "01001"
      source = TreacherySource fixtureCoverUpTreacheryId
    case fixtureCoverUpReactionQuestion of
      WindowChooseOne
        [ reactionChoice@(AbilityLabel choiceIid ability windows beforeMessages messages)
          , skipChoice@(SkipTriggersButton skipIid)
          ] -> do
            choiceIid `shouldBe` iid
            skipIid `shouldBe` iid
            abilitySource ability `shouldBe` source
            abilityRequestor ability `shouldBe` source
            abilityCardCode ability `shouldBe` "01007"
            abilityIndex ability `shouldBe` 1
            abilityType ability `shouldSatisfy` \case
              AbilityType.ReactionAbility {} -> True
              _ -> False
            abilityActions ability `shouldBe` []
            length windows `shouldBe` 1
            beforeMessages `shouldBe` []
            messages `shouldBe` []
            let
              answerValue sourceIndex version =
                Aeson.object
                  [ "tag" .= ("Answer" :: Text)
                  , "contents"
                      .= Aeson.object
                        [ "choice" .= sourceIndex
                        , "playerId" .= fixturePlayerId
                        , "questionVersion" .= version
                        ]
                  ]
              checkAnswer sourceIndex version check =
                case Aeson.fromJSON (answerValue sourceIndex version) of
                  Aeson.Error err ->
                    expectationFailure
                      $ "Could not decode Cover Up reaction Answer: "
                      <> err
                  Aeson.Success answer ->
                    handleAnswerPure fixtureCoverUpReactionGame fixturePlayerId answer
                      >>= check
              expectCurrent expected = \case
                Handled actual -> actual `shouldBe` [uiToRun expected]
                Unhandled reason ->
                  expectationFailure
                    $ "Cover Up reaction Answer rejected: "
                    <> Text.unpack reason
              expectStale = \case
                Unhandled reason -> reason `shouldBe` "Stale question"
                Handled _ ->
                  expectationFailure "A stale Cover Up reaction Answer must not resolve"
              currentVersion = gameScenarioSteps fixtureCoverUpReactionGame
            currentVersion `shouldBe` 33
            checkAnswer (0 :: Int) currentVersion (expectCurrent reactionChoice)
            checkAnswer (1 :: Int) currentVersion (expectCurrent skipChoice)
            checkAnswer (0 :: Int) (currentVersion - 1) expectStale
            checkAnswer (1 :: Int) (currentVersion - 1) expectStale
      other ->
        expectationFailure
          $ "Expected Cover Up's production reaction followed by SkipTriggersButton, got "
          <> show other

  it "executes Cover Up's reaction and skip branches through the authoritative game queue" do
    let
      iid = InvestigatorId "01001"
      runChoice sourceIndex = runAgainstFixtureBoardGame do
        prepareFixtureCoverUpReaction
        locationId <- selectJust $ LocationWithInvestigator (InvestigatorWithId iid)
        investigatorCluesBefore <- field InvestigatorClues iid
        locationCluesBefore <- field Location.LocationClues locationId
        coverUpCluesBefore <- field Treachery.TreacheryClues fixtureCoverUpTreacheryId
        game <- getGame
        let
          answer =
            Answer
              QuestionResponse
                { qrChoice = sourceIndex
                , qrPlayerId = Just fixturePlayerId
                , qrQuestionVersion = Just $ gameScenarioSteps game
                }
        liftIO (handleAnswerPure game fixturePlayerId answer) >>= \case
          Unhandled reason ->
            liftIO
              $ expectationFailure
              $ "Cover Up reaction Answer rejected: "
              <> Text.unpack reason
          Handled messages -> pushAndRunAll (ClearUI : messages)
        investigatorCluesAfter <- field InvestigatorClues iid
        locationCluesAfter <- field Location.LocationClues locationId
        coverUpCluesAfter <- field Treachery.TreacheryClues fixtureCoverUpTreacheryId
        pure
          ( investigatorCluesBefore
          , investigatorCluesAfter
          , locationCluesBefore
          , locationCluesAfter
          , coverUpCluesBefore
          , coverUpCluesAfter
          )
    reactionState <- runChoice 0
    skippedState <- runChoice 1
    reactionState
      `shouldSatisfy` \(investigatorBefore, investigatorAfter, locationBefore, locationAfter, coverUpBefore, coverUpAfter) ->
        investigatorAfter == investigatorBefore
          && locationAfter == locationBefore
          && coverUpAfter == coverUpBefore - 1
    skippedState
      `shouldSatisfy` \(investigatorBefore, investigatorAfter, locationBefore, locationAfter, coverUpBefore, coverUpAfter) ->
        investigatorAfter == investigatorBefore + 1
          && locationAfter == locationBefore - 1
          && coverUpAfter == coverUpBefore

  it "matches the exact production Gathering act objective on both encoder paths and canonical replay digest" do
    fixture <- loadFixture "question-gathering-act-objective.json"
    Aeson.toJSON fixtureGatheringActObjectiveQuestion `shouldBe` fixture
    viaWireEncoding fixtureGatheringActObjectiveQuestion `shouldBe` fixture
    canonicalQuestionSha256 fixtureGatheringActObjectiveQuestion
      `shouldBe` "c18ca7e7ab353583d977dcf56434b5703993e415f00d3e5f4ea3125c462692a8"

  it "projects every Gathering act-objective choice with exact source alignment and metadata" do
    fixture <- loadFixture "question-presentation-gathering-act-objective.json"
    let
      presentation =
        QuestionPresentation.questionPresentation
          (gameScenarioSteps fixtureGatheringActObjectiveGame)
          fixtureGatheringActObjectiveQuestion
    Aeson.toJSON presentation `shouldBe` fixture
    viaWireEncoding presentation `shouldBe` fixture
    case presentation of
      QuestionPresentation.QuestionPresentation version questionKind choiceCount choices -> do
        version `shouldBe` 34
        questionKind `shouldBe` "playerWindowChooseOne"
        choiceCount `shouldBe` 13
        let
          sourceIndexes =
            [ sourceIndex
            | QuestionPresentation.ChoicePresentation sourceIndex _ _ _ _ _ _ <- choices
            ]
          choiceKinds =
            [ choiceKind
            | QuestionPresentation.ChoicePresentation _ choiceKind _ _ _ _ _ <- choices
            ]
        sourceIndexes `shouldBe` [0 .. 12]
        choiceKinds
          `shouldBe` [ QuestionPresentation.GainResource
                     , QuestionPresentation.DrawCard
                     ]
            <> replicate 6 QuestionPresentation.ChooseTarget
            <> [ QuestionPresentation.EndTurn
               , QuestionPresentation.Investigate
               , QuestionPresentation.Fight
               , QuestionPresentation.Evade
               , QuestionPresentation.AdvanceAct
               ]
        drop 12 choices
          `shouldBe` [ QuestionPresentation.ChoicePresentation
                         12
                         QuestionPresentation.AdvanceAct
                         (Just $ InvestigatorId "01001")
                         (Just $ QuestionPresentation.ActEntity fixtureGatheringActObjectiveId)
                         Nothing
                         ( Just
                             $ QuestionPresentation.AbilityPresentation
                               "01108"
                               999
                               "objective"
                               []
                               True
                         )
                         ( Just
                             $ QuestionPresentation.GroupCluePresentationCost
                               (QuestionPresentation.PerPlayerAmount 2)
                               QuestionPresentation.AnywhereScope
                         )
                     ]

  it "projects the exact Gathering movement-entry sequence with native semantics" do
    let
      fixtureCases =
        [ ( "question-gathering-movement.json"
          , "question-presentation-gathering-movement.json"
          , 36
          , "ccc03aba15081592b2163fac1b61e433b20333486b650e1ed359ac598d2262f8"
          )
        , ( "question-gathering-attic-entry-forced.json"
          , "question-presentation-gathering-attic-entry-forced.json"
          , 37
          , "e2bdcb51bb439cf4e0e4b3e143658ef8bdc56607369e4a4a0e9a6209863e0fef"
          )
        , ( "question-gathering-cellar-entry-forced.json"
          , "question-presentation-gathering-cellar-entry-forced.json"
          , 37
          , "81226881d2744c27b99dc9e0169dc6da66b50616628d0cfe770fd42411adab7f"
          )
        , ( "question-gathering-attic-horror-assignment.json"
          , "question-presentation-gathering-attic-horror-assignment.json"
          , 38
          , "63ef3583c5440bb0eea5cc0c8f05bcf21d633ec5e9508d2f3bd45572a7abb43d"
          )
        , ( "question-gathering-cellar-damage-assignment.json"
          , "question-presentation-gathering-cellar-damage-assignment.json"
          , 38
          , "8259de19c744c3b781b434e40b4a78cd5d8a8af0a324aec8c16f009111f9ed2b"
          )
        ]
    for_ fixtureCases \(questionFile, presentationFile, version, expectedDigest) -> do
      questionFixture <- loadFixture questionFile
      question <- loadQuestionFixture questionFile
      presentationFixture <- loadFixture presentationFile
      Aeson.toJSON question `shouldBe` questionFixture
      viaWireEncoding question `shouldBe` questionFixture
      canonicalQuestionSha256 question `shouldBe` expectedDigest
      let presentation = QuestionPresentation.questionPresentation version question
      Aeson.toJSON presentation `shouldBe` presentationFixture
      viaWireEncoding presentation `shouldBe` presentationFixture

    movement <- loadQuestionFixture "question-gathering-movement.json"
    case QuestionPresentation.questionPresentation 36 movement of
      QuestionPresentation.QuestionPresentation _ _ _ choices -> do
        let
          sourceIndexes =
            [ sourceIndex
            | QuestionPresentation.ChoicePresentation sourceIndex _ _ _ _ _ _ <- choices
            ]
          choiceKinds =
            [ kind
            | QuestionPresentation.ChoicePresentation _ kind _ _ _ _ _ <- choices
            ]
        sourceIndexes `shouldBe` [0 .. 11]
        drop 9 choiceKinds
          `shouldBe` [ QuestionPresentation.Move
                     , QuestionPresentation.Move
                     , QuestionPresentation.Investigate
                     ]

    for_
      [ "question-gathering-attic-entry-forced.json"
      , "question-gathering-cellar-entry-forced.json"
      ]
      \questionFile -> do
        question <- loadQuestionFixture questionFile
        case QuestionPresentation.questionPresentation 37 question of
          QuestionPresentation.QuestionPresentation
            _
            _
            _
            [ QuestionPresentation.ChoicePresentation
                0
                QuestionPresentation.ResolveForcedAbility
                (Just actorId)
                (Just (QuestionPresentation.LocationEntity _))
                Nothing
                (Just ability)
                (Just QuestionPresentation.FreePresentationCost)
              ] -> do
                actorId `shouldBe` InvestigatorId "01001"
                ability
                  `shouldSatisfy` \(QuestionPresentation.AbilityPresentation _ 1 "forced" [] True) ->
                    True
          other ->
            expectationFailure
              $ "Expected one native Gathering forced-ability descriptor, got "
              <> show other

    let
      assertAssignment questionFile expectedKind = do
        question <- loadQuestionFixture questionFile
        QuestionPresentation.questionPresentation 38 question
          `shouldBe` QuestionPresentation.QuestionPresentation
            38
            "chooseOne"
            1
            [ QuestionPresentation.ChoicePresentation
                0
                expectedKind
                Nothing
                ( Just
                    $ QuestionPresentation.InvestigatorEntity
                    $ InvestigatorId "01001"
                )
                Nothing
                Nothing
                Nothing
            ]
    assertAssignment
      "question-gathering-attic-horror-assignment.json"
      QuestionPresentation.AssignHorror
    assertAssignment
      "question-gathering-cellar-damage-assignment.json"
      QuestionPresentation.AssignDamage

  it "does not classify non-assignment damage labels as assignment choices" do
    let
      iid = InvestigatorId "01001"
      question = ChooseOne [DamageLabel iid [GameOver]] :: Question Message
    QuestionPresentation.questionPresentation 38 question
      `shouldBe` QuestionPresentation.QuestionPresentation
        38
        "chooseOne"
        1
        []

  it "keeps unsupported forced variants generic instead of overpromising raw support" do
    let
      makeSilent = \case
        AbilityLabel investigatorId ability windows beforeMessages messages ->
          case abilityType ability of
            AbilityType.ForcedAbility window ->
              AbilityLabel
                investigatorId
                (ability {abilityType = AbilityType.SilentForcedAbility window})
                windows
                beforeMessages
                messages
            _ -> error "Expected a ForcedAbility fixture"
        _ -> error "Expected an AbilityLabel fixture"

    forced <- loadQuestionFixture "question-gathering-attic-entry-forced.json"
    case forced of
      WindowChooseOne [choice] ->
        case
          QuestionPresentation.questionPresentation
            37
            (WindowChooseOne [makeSilent choice])
          of
            QuestionPresentation.QuestionPresentation
              _
              _
              _
              [QuestionPresentation.ChoicePresentation _ kind _ _ _ _ _] ->
                kind `shouldBe` QuestionPresentation.UseAbility
            other ->
              expectationFailure
                $ "Expected one generic ability descriptor, got "
                <> show other
      other ->
        expectationFailure
          $ "Expected the Gathering Attic forced prompt, got "
          <> show other

  it "fails closed for malformed location semantics without hiding other move abilities" do
    let
      locationTargetFor = \case
        LocationSource locationId -> Just $ LocationTarget locationId
        _ -> error "Expected a LocationSource fixture"
      removeProjectedSource = \case
        AbilityLabel investigatorId ability windows beforeMessages messages ->
          AbilityLabel
            investigatorId
            ( ability
                { abilitySource = GameSource
                , abilityTarget = locationTargetFor $ abilitySource ability
                }
            )
            windows
            beforeMessages
            messages
        other -> other

    movement <- loadQuestionFixture "question-gathering-movement.json"
    case movement of
      PlayerWindowChooseOne choices -> do
        let
          sourceLessMovement =
            PlayerWindowChooseOne
              [ if sourceIndex == 9 then removeProjectedSource choice else choice
              | (sourceIndex, choice) <- zip [0 :: Int ..] choices
              ]
        case QuestionPresentation.questionPresentation 36 sourceLessMovement of
          QuestionPresentation.QuestionPresentation _ _ choiceCount presentations -> do
            let
              sourceIndexes =
                [ sourceIndex
                | QuestionPresentation.ChoicePresentation sourceIndex _ _ _ _ _ _ <-
                    presentations
                ]
            choiceCount `shouldBe` 12
            sourceIndexes `shouldBe` [0 .. 8] <> [10, 11]

        case drop 9 choices of
          AbilityLabel _ cellarAbility _ _ _
            : AbilityLabel _ atticAbility _ _ _
            : _ -> do
            let
              replaceSource = \case
                AbilityLabel investigatorId ability windows beforeMessages messages ->
                  AbilityLabel
                    investigatorId
                    ( ability
                        { abilitySource = abilitySource atticAbility
                        , abilityTarget =
                            locationTargetFor $ abilitySource cellarAbility
                        }
                    )
                    windows
                    beforeMessages
                    messages
                otherChoice -> otherChoice
              mismatchedMovement =
                PlayerWindowChooseOne
                  [ if sourceIndex == 9 then replaceSource choice else choice
                  | (sourceIndex, choice) <- zip [0 :: Int ..] choices
                  ]
            case QuestionPresentation.questionPresentation 36 mismatchedMovement of
              QuestionPresentation.QuestionPresentation _ _ choiceCount presentations -> do
                let
                  sourceIndexes =
                    [ sourceIndex
                    | QuestionPresentation.ChoicePresentation sourceIndex _ _ _ _ _ _ <-
                        presentations
                    ]
                choiceCount `shouldBe` 12
                sourceIndexes `shouldBe` [0 .. 8] <> [10, 11]
          otherChoices ->
            expectationFailure
              $ "Expected both Gathering movement choices, got "
              <> show otherChoices

        case drop 9 choices of
          AbilityLabel investigatorId ability windows beforeMessages messages : _ -> do
            let
              assetId = AssetId $ UUID.fromWords 0 0 0 909
              assetMovement =
                AbilityLabel
                  investigatorId
                  ( ability
                      { abilitySource = AssetSource assetId
                      , abilityRequestor = AssetSource assetId
                      , abilityTarget = Nothing
                      , abilityCardCode = "08127"
                      , abilityIndex = 1
                      }
                  )
                  windows
                  beforeMessages
                  messages
            QuestionPresentation.questionPresentation 36 (ChooseOne [assetMovement])
              `shouldBe` QuestionPresentation.QuestionPresentation
                36
                "chooseOne"
                1
                [ QuestionPresentation.ChoicePresentation
                    0
                    QuestionPresentation.UseAbility
                    (Just investigatorId)
                    (Just $ QuestionPresentation.AssetEntity assetId)
                    Nothing
                    ( Just
                        $ QuestionPresentation.AbilityPresentation
                          "08127"
                          1
                          "action"
                          ["move"]
                          True
                    )
                    ( Just
                        $ QuestionPresentation.AllPresentationCosts
                          [ QuestionPresentation.ActionPresentationCost 1
                          , QuestionPresentation.OtherPresentationCost
                          ]
                    )
                ]
          otherChoices ->
            expectationFailure
              $ "Expected the Gathering Cellar movement choice, got "
              <> show otherChoices
      other ->
        expectationFailure
          $ "Expected the Gathering movement player window, got "
          <> show other

    forced <- loadQuestionFixture "question-gathering-attic-entry-forced.json"
    case forced of
      WindowChooseOne [choice] ->
        QuestionPresentation.questionPresentation
          37
          (WindowChooseOne [removeProjectedSource choice])
          `shouldBe` QuestionPresentation.QuestionPresentation
            37
            "windowChooseOne"
            1
            []
      other ->
        expectationFailure
          $ "Expected the Gathering Attic forced prompt, got "
          <> show other

  it "omits forced-ability semantics for matching non-location source and target" do
    let
      replaceWithInvestigatorSource = \case
        AbilityLabel investigatorId ability windows beforeMessages messages ->
          AbilityLabel
            investigatorId
            ( ability
                { abilitySource = InvestigatorSource investigatorId
                , abilityTarget = Just $ InvestigatorTarget investigatorId
                }
            )
            windows
            beforeMessages
            messages
        otherChoice -> otherChoice

    forced <- loadQuestionFixture "question-gathering-attic-entry-forced.json"
    case forced of
      WindowChooseOne [choice] ->
        QuestionPresentation.questionPresentation
          37
          (WindowChooseOne [replaceWithInvestigatorSource choice])
          `shouldBe` QuestionPresentation.QuestionPresentation
            37
            "windowChooseOne"
            1
            []
      other ->
        expectationFailure
          $ "Expected the Gathering Attic forced prompt, got "
          <> show other

  it "keeps Gathering semantic choices fail-closed without changing raw source indexes" do
    let
      iid = InvestigatorId "01001"
      question =
        ChooseOne
          [ Label "$fixture.supported" []
          , InvalidLabel "$fixture.unsupported"
          , EndTurnButton iid []
          ] ::
          Question Message
    QuestionPresentation.questionPresentation 7 question
      `shouldBe` QuestionPresentation.QuestionPresentation
        7
        "chooseOne"
        3
        [ QuestionPresentation.ChoicePresentation
            0
            QuestionPresentation.LocalizedLabel
            Nothing
            Nothing
            (Just $ QuestionPresentation.EmbeddedI18nLabel "$fixture.supported")
            Nothing
            Nothing
        , QuestionPresentation.ChoicePresentation
            2
            QuestionPresentation.EndTurn
            (Just iid)
            Nothing
            Nothing
            Nothing
            Nothing
        ]

  it "fails closed when a synthetic auto action shifts every real answer index" do
    let
      iid = InvestigatorId "01001"
      question =
        ChooseOneAtATimeWithAuto
          "$fixture.resolveAll"
          [ Label "$fixture.first" [ClearUI]
          , EndTurnButton iid [GameOver]
          ]
      game =
        fixtureBoardGame
          { gameQuestion = singletonMap fixturePlayerId question
          , gameRetainedQuestion = False
          }
      answer sourceIndex =
        Answer
          QuestionResponse
            { qrChoice = sourceIndex
            , qrPlayerId = Just fixturePlayerId
            , qrQuestionVersion = Nothing
            }
      assertAnswer sourceIndex expected =
        handleAnswerPure game fixturePlayerId (answer sourceIndex) >>= \case
          Unhandled reason ->
            expectationFailure
              $ "auto-choice answer rejected: "
              <> Text.unpack reason
          Handled messages -> messages `shouldBe` expected
    QuestionPresentation.questionPresentation 7 question
      `shouldBe` QuestionPresentation.QuestionPresentation
        7
        "unsupported"
        0
        []
    assertAnswer 0 [Run [ClearUI], Run [GameOver]]
    assertAnswer
      1
      [ Run [ClearUI]
      , Ask fixturePlayerId
          $ ChooseOneAtATime [EndTurnButton iid [GameOver]]
      ]

  it "binds the Gathering act objective to source index twelve and its exact server-owned cost" do
    let
      iid = InvestigatorId "01001"
      source = ActSource fixtureGatheringActObjectiveId
    case fixtureGatheringActObjectiveQuestion of
      PlayerWindowChooseOne choices -> do
        let (precedingChoices, objectiveChoices) = splitAt 12 choices
        length precedingChoices `shouldBe` 12
        case objectiveChoices of
          [objectiveChoice@(AbilityLabel choiceIid ability windows beforeMessages messages)] -> do
            choiceIid `shouldBe` iid
            abilitySource ability `shouldBe` source
            abilityRequestor ability `shouldBe` source
            abilityCardCode ability `shouldBe` "01108"
            abilityIndex ability `shouldBe` 999
            abilityType ability
              `shouldBe` AbilityType.Objective
                ( AbilityType.FastAbility'
                    (Cost.GroupClueCost (GameValue.PerPlayer 2) Matcher.Anywhere)
                    mempty
                )
            abilityActions ability `shouldBe` []
            abilityLimit ability `shouldBe` AbilityLimit.NoLimit
            abilityWindow ability `shouldBe` Matcher.FastPlayerWindow
            abilityCriteria ability `shouldBe` Criteria.DuringTurn Matcher.Anyone
            windows
              `shouldBe` [ Window.mkWindow Timing.When (Window.DuringTurn iid)
                         , Window.mkWindow Timing.When Window.FastPlayerWindow
                         , Window.mkWindow Timing.When Window.NonFast
                         ]
            beforeMessages `shouldBe` []
            messages `shouldBe` []
            let
              answerValue version =
                Aeson.object
                  [ "tag" .= ("Answer" :: Text)
                  , "contents"
                      .= Aeson.object
                        [ "choice" .= (12 :: Int)
                        , "playerId" .= fixturePlayerId
                        , "questionVersion" .= version
                        ]
                  ]
              checkAnswer version check =
                case Aeson.fromJSON (answerValue version) of
                  Aeson.Error err ->
                    expectationFailure
                      $ "Could not decode Gathering act objective Answer: "
                      <> err
                  Aeson.Success answer ->
                    handleAnswerPure fixtureGatheringActObjectiveGame fixturePlayerId answer
                      >>= check
              expectCurrent = \case
                Handled actual -> actual `shouldBe` [uiToRun objectiveChoice]
                Unhandled reason ->
                  expectationFailure
                    $ "Gathering act objective Answer rejected: "
                    <> Text.unpack reason
              expectStale = \case
                Unhandled reason -> reason `shouldBe` "Stale question"
                Handled _ ->
                  expectationFailure "A stale Gathering act objective Answer must not resolve"
              currentVersion = gameScenarioSteps fixtureGatheringActObjectiveGame
            currentVersion `shouldBe` 34
            checkAnswer currentVersion expectCurrent
            checkAnswer (currentVersion - 1) expectStale
          other ->
            expectationFailure
              $ "Expected one Gathering objective at source index twelve, got "
              <> show other
      other ->
        expectationFailure
          $ "Expected the production Gathering player window, got "
          <> show other

  it "pays the Gathering objective and requires the authoritative act-advance confirmation" do
    rawAdvanceFixture <- loadFixture "question-gathering-act-advance.json"
    presentationAdvanceFixture <-
      loadFixture "question-presentation-gathering-act-advance.json"
    let iid = InvestigatorId "01001"
    ( cluesBefore
      , cluesAfterPayment
      , locationBefore
      , locationAfterPayment
      , confirmationVersion
      , locationAfterAdvance
      , finalVersion
      ) <-
      runAgainstFixtureBoardGame do
        prepareFixtureGatheringActObjective
        cluesBefore <- field InvestigatorClues iid
        locationBefore <- field InvestigatorLocation iid
        game <- getGame
        let
          answer =
            Answer
              QuestionResponse
                { qrChoice = 12
                , qrPlayerId = Just fixturePlayerId
                , qrQuestionVersion = Just $ gameScenarioSteps game
                }
        liftIO (handleAnswerPure game fixturePlayerId answer) >>= \case
          Unhandled reason ->
            liftIO
              $ expectationFailure
              $ "Gathering act objective Answer rejected: "
              <> Text.unpack reason
          Handled messages -> pushAndRunAll (ClearUI : messages)
        cluesAfterPayment <- field InvestigatorClues iid
        locationAfterPayment <- field InvestigatorLocation iid
        confirmationGame <- getGame
        liftIO
          $ case Map.lookup fixturePlayerId (gameQuestion confirmationGame) of
            Just
              question@( ChooseOne
                  [ TargetLabel
                      (ActTarget targetActId)
                      [AdvanceAct messageActId source AdvancedWithClues]
                    ]
                ) -> do
                Aeson.toJSON question `shouldBe` rawAdvanceFixture
                viaWireEncoding question `shouldBe` rawAdvanceFixture
                canonicalQuestionSha256 question
                  `shouldBe` "f4d33a06562c03ad21632689f2e30c69c6a27820cda0800e65da297cb230138a"
                targetActId `shouldBe` fixtureGatheringActObjectiveId
                messageActId `shouldBe` fixtureGatheringActObjectiveId
                source `shouldBe` ActSource fixtureGatheringActObjectiveId
            other ->
              expectationFailure
                $ "Expected the version-35 Gathering act-advance confirmation, got "
                <> show other
        let
          confirmationVersion = gameScenarioSteps confirmationGame
          confirmationPresentation =
            Map.lookup
              fixturePlayerId
              ( QuestionPresentation.questionPresentations
                  confirmationVersion
                  (gameQuestion confirmationGame)
              )
        liftIO
          $ case confirmationPresentation of
            Nothing ->
              expectationFailure
                "Expected a semantic presentation for the version-35 Gathering confirmation"
            Just presentation -> do
              Aeson.toJSON presentation `shouldBe` presentationAdvanceFixture
              viaWireEncoding presentation `shouldBe` presentationAdvanceFixture
              presentation
                `shouldBe` QuestionPresentation.QuestionPresentation
                  35
                  "chooseOne"
                  1
                  [ QuestionPresentation.ChoicePresentation
                      0
                      QuestionPresentation.AdvanceAct
                      Nothing
                      (Just $ QuestionPresentation.ActEntity fixtureGatheringActObjectiveId)
                      Nothing
                      Nothing
                      Nothing
                  ]
        let
          confirmationAnswer =
            Answer
              QuestionResponse
                { qrChoice = 0
                , qrPlayerId = Just fixturePlayerId
                , qrQuestionVersion = Just confirmationVersion
                }
        liftIO (handleAnswerPure confirmationGame fixturePlayerId confirmationAnswer) >>= \case
          Unhandled reason ->
            liftIO
              $ expectationFailure
              $ "Gathering act-advance confirmation rejected: "
              <> Text.unpack reason
          Handled messages -> pushAndRunAll (ClearUI : messages)
        locationAfterAdvance <- field InvestigatorLocation iid
        finalGame <- getGame
        hallwayId <- selectJust $ LocationIs "01112"
        atticId <- selectJust $ LocationIs "01113"
        cellarId <- selectJust $ LocationIs "01114"
        _parlorId <- selectJust $ LocationIs "01115"
        hallwayRevealed <- field Location.LocationRevealed hallwayId
        ghoulRemoved <- selectNone $ EnemyWithId fixtureGatheringActObjectiveEnemyId
        studyRemoved <- selectNone $ LocationIs "01111"
        oldActRemoved <- selectNone $ ActWithId fixtureGatheringActObjectiveId
        nextActId <- selectJust $ ActWithId fixtureGatheringNextActId
        liftIO do
          locationAfterAdvance `shouldBe` Just hallwayId
          hallwayRevealed `shouldBe` True
          ghoulRemoved `shouldBe` True
          studyRemoved `shouldBe` True
          oldActRemoved `shouldBe` True
          nextActId `shouldBe` fixtureGatheringNextActId
          case
              ( fixtureGatheringActObjectiveQuestion
              , Map.lookup fixturePlayerId (gameQuestion finalGame)
              )
            of
              ( PlayerWindowChooseOne objectiveChoices
                , Just (PlayerWindowChooseOne nextChoices)
                ) -> do
                  length nextChoices `shouldBe` 12
                  take 9 nextChoices `shouldBe` take 9 objectiveChoices
                  case drop 9 nextChoices of
                    [ AbilityLabel atticIid atticAbility _ atticBefore atticMessages
                      , AbilityLabel hallwayIid hallwayAbility _ hallwayBefore hallwayMessages
                      , AbilityLabel cellarIid cellarAbility _ cellarBefore cellarMessages
                      ] -> do
                        atticIid `shouldBe` iid
                        abilitySource atticAbility `shouldBe` LocationSource atticId
                        abilityCardCode atticAbility `shouldBe` "01113"
                        abilityIndex atticAbility `shouldBe` 104
                        abilityActions atticAbility `shouldBe` [Action.Move]
                        atticBefore `shouldBe` []
                        atticMessages `shouldBe` []
                        hallwayIid `shouldBe` iid
                        abilitySource hallwayAbility `shouldBe` LocationSource hallwayId
                        abilityCardCode hallwayAbility `shouldBe` "01112"
                        abilityIndex hallwayAbility `shouldBe` 103
                        abilityActions hallwayAbility `shouldBe` [Action.Investigate]
                        hallwayBefore `shouldBe` []
                        hallwayMessages `shouldBe` []
                        cellarIid `shouldBe` iid
                        abilitySource cellarAbility `shouldBe` LocationSource cellarId
                        abilityCardCode cellarAbility `shouldBe` "01114"
                        abilityIndex cellarAbility `shouldBe` 104
                        abilityActions cellarAbility `shouldBe` [Action.Move]
                        cellarBefore `shouldBe` []
                        cellarMessages `shouldBe` []
                    other ->
                      expectationFailure
                        $ "Expected the stable version-36 Hallway action suffix, got "
                        <> show other
              other ->
                expectationFailure
                  $ "Expected the version-36 Gathering player window, got "
                  <> show other
          case
              Map.lookup
                fixturePlayerId
                ( QuestionPresentation.questionPresentations
                    (gameScenarioSteps finalGame)
                    (gameQuestion finalGame)
                )
            of
              Just
                ( QuestionPresentation.QuestionPresentation
                    version
                    questionKind
                    choiceCount
                    choices
                  ) -> do
                    version `shouldBe` 36
                    questionKind `shouldBe` "playerWindowChooseOne"
                    choiceCount `shouldBe` 12
                    let
                      sourceIndexes =
                        [ sourceIndex
                        | QuestionPresentation.ChoicePresentation sourceIndex _ _ _ _ _ _ <- choices
                        ]
                      choiceKinds =
                        [ choiceKind
                        | QuestionPresentation.ChoicePresentation _ choiceKind _ _ _ _ _ <- choices
                        ]
                    sourceIndexes `shouldBe` [0 .. 11]
                    choiceKinds
                      `shouldBe` [ QuestionPresentation.GainResource
                                 , QuestionPresentation.DrawCard
                                 ]
                        <> replicate 6 QuestionPresentation.ChooseTarget
                        <> [ QuestionPresentation.EndTurn
                           , QuestionPresentation.Move
                           , QuestionPresentation.Investigate
                           , QuestionPresentation.Move
                           ]
              other ->
                expectationFailure
                  $ "Expected the version-36 Gathering semantic presentation, got "
                  <> show other
        pure
          ( cluesBefore
          , cluesAfterPayment
          , locationBefore
          , locationAfterPayment
          , confirmationVersion
          , locationAfterAdvance
          , gameScenarioSteps finalGame
          )
    cluesBefore `shouldBe` 2
    cluesAfterPayment `shouldBe` 0
    locationAfterPayment `shouldBe` locationBefore
    confirmationVersion `shouldBe` 35
    locationAfterAdvance `shouldNotBe` locationBefore
    finalVersion `shouldBe` 36

  it "executes Gathering Q39 actions through their exact Q42 successor windows" do
    let
      iid = InvestigatorId "01001"
      -- Source indexes are scoped to one question. This fixed-seed board's
      -- location UUID order differs from the replay-derived Q36 fixture,
      -- whose production source indexes remain pinned separately above.
      branchCases =
        [ ( "Cellar"
          , "01114"
          , 11
          , 10
          , QuestionPresentation.AssignDamage
          , 2
          , 3
          )
        , ( "Attic"
          , "01113"
          , 9
          , 10
          , QuestionPresentation.AssignHorror
          , 1
          , 4
          )
        ]
    for_
      branchCases
      \(branchName, locationCardCode, movementSourceIndex, q39SourceIndex, assignmentKind, finalDamage, finalHorror) ->
        runAgainstFixtureBoardGame do
          overTest
            ( entitiesL
                . investigatorsL
                . ix iid
                %~ overAttrs
                  ( \attrs ->
                      attrs
                        { investigatorTokens =
                            setTokens Damage 1
                              $ setTokens Horror 3
                              $ investigatorTokens attrs
                        }
                  )
            )
          prepareFixtureGatheringActObjective
          q34Version <- answerFixturePlayerQuestion 12
          q35Version <- answerFixturePlayerQuestion 0
          destinationId <- selectJust $ LocationIs locationCardCode
          hallwayId <- selectJust $ LocationIs "01112"
          q36Game <- getGame
          q36Location <- field InvestigatorLocation iid
          q36Actions <- field InvestigatorRemainingActions iid
          q36Damage <- field Investigator.InvestigatorDamage iid
          q36Horror <- field InvestigatorHorror iid
          liftIO do
            gameScenarioSteps q36Game `shouldBe` 36
            q36Actions `shouldBe` 2
            q36Damage `shouldBe` 1
            q36Horror `shouldBe` 3
            q36Location `shouldNotBe` Just destinationId
            case
                ( Map.lookup fixturePlayerId (gameQuestion q36Game)
                , Map.lookup
                    fixturePlayerId
                    ( QuestionPresentation.questionPresentations
                        (gameScenarioSteps q36Game)
                        (gameQuestion q36Game)
                    )
                )
              of
                ( Just (PlayerWindowChooseOne rawChoices)
                  , Just
                      ( QuestionPresentation.QuestionPresentation
                          36
                          "playerWindowChooseOne"
                          12
                          presentationChoices
                        )
                  ) -> do
                    case drop movementSourceIndex rawChoices of
                      AbilityLabel choiceIid ability _ beforeMessages messages : _ -> do
                        choiceIid `shouldBe` iid
                        abilitySource ability `shouldBe` LocationSource destinationId
                        abilityCardCode ability `shouldBe` locationCardCode
                        abilityIndex ability `shouldBe` 104
                        abilityActions ability `shouldBe` [Action.Move]
                        beforeMessages `shouldBe` []
                        messages `shouldBe` []
                      other ->
                        expectationFailure
                          $ "Expected "
                          <> branchName
                          <> " movement at source index "
                          <> show movementSourceIndex
                          <> ", got "
                          <> show other
                    find
                      ( \(QuestionPresentation.ChoicePresentation sourceIndex _ _ _ _ _ _) ->
                          sourceIndex == movementSourceIndex
                      )
                      presentationChoices
                      `shouldBe` Just
                        ( QuestionPresentation.ChoicePresentation
                            movementSourceIndex
                            QuestionPresentation.Move
                            (Just iid)
                            (Just $ QuestionPresentation.LocationEntity destinationId)
                            Nothing
                            ( Just
                                $ QuestionPresentation.AbilityPresentation
                                  locationCardCode
                                  104
                                  "action"
                                  ["move"]
                                  True
                            )
                            ( Just
                                $ QuestionPresentation.AllPresentationCosts
                                  [ QuestionPresentation.ActionPresentationCost 1
                                  , QuestionPresentation.OtherPresentationCost
                                  ]
                            )
                        )
                other ->
                  expectationFailure
                    $ "Expected the version-36 "
                    <> branchName
                    <> " movement question and presentation, got "
                    <> show other

          q36AnswerVersion <- answerFixturePlayerQuestion movementSourceIndex
          q37Game <- getGame
          q37Location <- field InvestigatorLocation iid
          q37Actions <- field InvestigatorRemainingActions iid
          q37Damage <- field Investigator.InvestigatorDamage iid
          q37Horror <- field InvestigatorHorror iid
          q37Revealed <- field Location.LocationRevealed destinationId
          liftIO do
            q37Location `shouldBe` Just destinationId
            q37Actions `shouldBe` 1
            q37Damage `shouldBe` 1
            q37Horror `shouldBe` 3
            q37Revealed `shouldBe` True
            case
                ( Map.lookup fixturePlayerId (gameQuestion q37Game)
                , Map.lookup
                    fixturePlayerId
                    ( QuestionPresentation.questionPresentations
                        (gameScenarioSteps q37Game)
                        (gameQuestion q37Game)
                    )
                )
              of
                ( Just
                    ( WindowChooseOne
                        [AbilityLabel choiceIid ability _ beforeMessages messages]
                      )
                  , Just presentation
                  ) -> do
                    choiceIid `shouldBe` iid
                    abilitySource ability `shouldBe` LocationSource destinationId
                    abilityCardCode ability `shouldBe` locationCardCode
                    abilityIndex ability `shouldBe` 1
                    abilityActions ability `shouldBe` []
                    beforeMessages `shouldBe` []
                    messages `shouldBe` []
                    presentation
                      `shouldBe` QuestionPresentation.QuestionPresentation
                        37
                        "windowChooseOne"
                        1
                        [ QuestionPresentation.ChoicePresentation
                            0
                            QuestionPresentation.ResolveForcedAbility
                            (Just iid)
                            (Just $ QuestionPresentation.LocationEntity destinationId)
                            Nothing
                            ( Just
                                $ QuestionPresentation.AbilityPresentation
                                  locationCardCode
                                  1
                                  "forced"
                                  []
                                  True
                            )
                            (Just QuestionPresentation.FreePresentationCost)
                        ]
                other ->
                  expectationFailure
                    $ "Expected the version-37 "
                    <> branchName
                    <> " forced-entry question and presentation, got "
                    <> show other

          q37AnswerVersion <- answerFixturePlayerQuestion 0
          q38Game <- getGame
          q38Location <- field InvestigatorLocation iid
          q38Actions <- field InvestigatorRemainingActions iid
          q38Damage <- field Investigator.InvestigatorDamage iid
          q38Horror <- field InvestigatorHorror iid
          liftIO do
            q38Location `shouldBe` Just destinationId
            q38Actions `shouldBe` 1
            q38Damage `shouldBe` 1
            q38Horror `shouldBe` 3
            Map.lookup
              fixturePlayerId
              ( QuestionPresentation.questionPresentations
                  (gameScenarioSteps q38Game)
                  (gameQuestion q38Game)
              )
              `shouldBe` Just
                ( QuestionPresentation.QuestionPresentation
                    38
                    "chooseOne"
                    1
                    [ QuestionPresentation.ChoicePresentation
                        0
                        assignmentKind
                        Nothing
                        (Just $ QuestionPresentation.InvestigatorEntity iid)
                        Nothing
                        Nothing
                        Nothing
                    ]
                )

          q38AnswerVersion <- answerFixturePlayerQuestion 0
          q39Game <- getGame
          q39Location <- field InvestigatorLocation iid
          q39Actions <- field InvestigatorRemainingActions iid
          q39Damage <- field Investigator.InvestigatorDamage iid
          q39Horror <- field InvestigatorHorror iid
          liftIO do
            [q34Version, q35Version, q36AnswerVersion, q37AnswerVersion, q38AnswerVersion]
              `shouldBe` [34, 35, 36, 37, 38]
            gameScenarioSteps q39Game `shouldBe` 39
            q39Location `shouldBe` Just destinationId
            q39Actions `shouldBe` 1
            q39Damage `shouldBe` finalDamage
            q39Horror `shouldBe` finalHorror
            case
                ( Map.lookup fixturePlayerId (gameQuestion q39Game)
                , Map.lookup
                    fixturePlayerId
                    ( QuestionPresentation.questionPresentations
                        (gameScenarioSteps q39Game)
                        (gameQuestion q39Game)
                    )
                )
              of
                ( Just (PlayerWindowChooseOne rawChoices)
                  , Just
                      ( QuestionPresentation.QuestionPresentation
                          39
                          "playerWindowChooseOne"
                          choiceCount
                          presentationChoices
                        )
                  ) -> do
                    rawChoices `shouldSatisfy` (not . null)
                    presentationChoices `shouldSatisfy` (not . null)
                    choiceCount `shouldBe` length rawChoices
                    find
                      ( \(QuestionPresentation.ChoicePresentation sourceIndex _ _ _ _ _ _) ->
                          sourceIndex == q39SourceIndex
                      )
                      presentationChoices
                      `shouldBe` Just
                        ( if branchName == "Cellar"
                            then
                              QuestionPresentation.ChoicePresentation
                                q39SourceIndex
                                QuestionPresentation.Investigate
                                (Just iid)
                                (Just $ QuestionPresentation.LocationEntity destinationId)
                                Nothing
                                ( Just
                                    $ QuestionPresentation.AbilityPresentation
                                      "01114"
                                      103
                                      "action"
                                      ["investigate"]
                                      True
                                )
                                (Just $ QuestionPresentation.ActionPresentationCost 1)
                            else
                              QuestionPresentation.ChoicePresentation
                                q39SourceIndex
                                QuestionPresentation.Move
                                (Just iid)
                                (Just $ QuestionPresentation.LocationEntity hallwayId)
                                Nothing
                                ( Just
                                    $ QuestionPresentation.AbilityPresentation
                                      "01112"
                                      104
                                      "action"
                                      ["move"]
                                      True
                                )
                                (Just $ QuestionPresentation.ActionPresentationCost 1)
                        )
                other ->
                  expectationFailure
                    $ "Expected the version-39 "
                    <> branchName
                    <> " player window and presentation, got "
                    <> show other

          q39AnswerVersion <- answerFixturePlayerQuestion q39SourceIndex
          q40Game <- getGame
          liftIO do
            q39AnswerVersion `shouldBe` 39
            gameScenarioSteps q40Game `shouldBe` 40
          case branchName of
            "Cellar" -> do
              liftIO do
                Map.lookup fixturePlayerId (gameQuestion q40Game)
                  `shouldBe` Just (ChooseOne [StartSkillTestButton iid])
                Map.lookup
                  fixturePlayerId
                  ( QuestionPresentation.questionPresentations
                      (gameScenarioSteps q40Game)
                      (gameQuestion q40Game)
                  )
                  `shouldBe` Just
                    ( QuestionPresentation.QuestionPresentation
                        40
                        "chooseOne"
                        1
                        [ QuestionPresentation.ChoicePresentation
                            0
                            QuestionPresentation.StartSkillTest
                            (Just iid)
                            Nothing
                            Nothing
                            Nothing
                            Nothing
                        ]
                    )
              q40AnswerVersion <- answerFixturePlayerQuestion 0
              q41Game <- getGame
              liftIO do
                q40AnswerVersion `shouldBe` 40
                gameScenarioSteps q41Game `shouldBe` 41
                gamePhase q41Game `shouldBe` InvestigationPhase
                gamePhaseStep q41Game
                  `shouldBe` Just (InvestigationPhaseStep InvestigatorTakesActionStep)
                Map.lookup fixturePlayerId (gameQuestion q41Game)
                  `shouldBe` Just (ChooseOne [SkillTestApplyResultsButton])
                Map.lookup
                  fixturePlayerId
                  ( QuestionPresentation.questionPresentations
                      (gameScenarioSteps q41Game)
                      (gameQuestion q41Game)
                  )
                  `shouldBe` Just
                    ( QuestionPresentation.QuestionPresentation
                        41
                        "chooseOne"
                        1
                        [ QuestionPresentation.ChoicePresentation
                            0
                            QuestionPresentation.ApplySkillTestResults
                            Nothing
                            Nothing
                            Nothing
                            Nothing
                            Nothing
                        ]
                    )
              q41AnswerVersion <- answerFixturePlayerQuestion 0
              q42Game <- getGame
              q42Actions <- field InvestigatorRemainingActions iid
              liftIO do
                q41AnswerVersion `shouldBe` 41
                gameScenarioSteps q42Game `shouldBe` 42
                gamePhase q42Game `shouldBe` InvestigationPhase
                gamePhaseStep q42Game
                  `shouldBe` Just (InvestigationPhaseStep InvestigatorTakesActionStep)
                q42Actions `shouldBe` 0
                case Map.lookup fixturePlayerId (gameQuestion q42Game) of
                  Just (PlayerWindowChooseOne [EndTurnButton choiceIid messages]) -> do
                    choiceIid `shouldBe` iid
                    messages `shouldBe` [ChooseEndTurn iid]
                  other ->
                    expectationFailure
                      $ "Expected Cellar Q42 end-turn choice, got "
                      <> show other
                Map.lookup
                  fixturePlayerId
                  ( QuestionPresentation.questionPresentations
                      (gameScenarioSteps q42Game)
                      (gameQuestion q42Game)
                  )
                  `shouldBe` Just
                    ( QuestionPresentation.QuestionPresentation
                        42
                        "playerWindowChooseOne"
                        1
                        [ QuestionPresentation.ChoicePresentation
                            0
                            QuestionPresentation.EndTurn
                            (Just iid)
                            Nothing
                            Nothing
                            Nothing
                            Nothing
                        ]
                    )
            "Attic" -> do
              liftIO do
                case Map.lookup fixturePlayerId (gameQuestion q40Game) of
                  Just (PlayerWindowChooseOne [EndTurnButton choiceIid messages]) -> do
                    choiceIid `shouldBe` iid
                    messages `shouldBe` [ChooseEndTurn iid]
                  other ->
                    expectationFailure
                      $ "Expected Attic Q40 end-turn choice, got "
                      <> show other
                Map.lookup
                  fixturePlayerId
                  ( QuestionPresentation.questionPresentations
                      (gameScenarioSteps q40Game)
                      (gameQuestion q40Game)
                  )
                  `shouldBe` Just
                    ( QuestionPresentation.QuestionPresentation
                        40
                        "playerWindowChooseOne"
                        1
                        [ QuestionPresentation.ChoicePresentation
                            0
                            QuestionPresentation.EndTurn
                            (Just iid)
                            Nothing
                            Nothing
                            Nothing
                            Nothing
                        ]
                    )
              q40AnswerVersion <- answerFixturePlayerQuestion 0
              q41Game <- getGame
              liftIO do
                q40AnswerVersion `shouldBe` 40
                gameScenarioSteps q41Game `shouldBe` 41
                gamePhase q41Game `shouldBe` MythosPhase
                gamePhaseStep q41Game
                  `shouldBe` Just (MythosPhaseStep EachInvestigatorDrawsEncounterCardStep)
                Map.lookup fixturePlayerId (gameQuestion q41Game)
                  `shouldBe` Just fixtureEncounterDrawQuestion
                Map.lookup
                  fixturePlayerId
                  ( QuestionPresentation.questionPresentations
                      (gameScenarioSteps q41Game)
                      (gameQuestion q41Game)
                  )
                  `shouldBe` Just
                    ( QuestionPresentation.QuestionPresentation
                        41
                        "chooseOne"
                        1
                        [ QuestionPresentation.ChoicePresentation
                            0
                            QuestionPresentation.DrawEncounterCard
                            (Just iid)
                            Nothing
                            Nothing
                            Nothing
                            Nothing
                        ]
                    )
              q41AnswerVersion <- answerFixturePlayerQuestion 0
              q42Game <- getGame
              q42Location <- field InvestigatorLocation iid
              q42Actions <- field InvestigatorRemainingActions iid
              liftIO do
                q41AnswerVersion `shouldBe` 41
                gameScenarioSteps q42Game `shouldBe` 42
                gamePhase q42Game `shouldBe` InvestigationPhase
                gamePhaseStep q42Game
                  `shouldBe` Just (InvestigationPhaseStep InvestigatorTakesActionStep)
                q42Location `shouldBe` Just hallwayId
                q42Actions `shouldBe` 3
                case
                    ( Map.lookup fixturePlayerId (gameQuestion q42Game)
                    , Map.lookup
                        fixturePlayerId
                        ( QuestionPresentation.questionPresentations
                            (gameScenarioSteps q42Game)
                            (gameQuestion q42Game)
                        )
                    )
                  of
                    ( Just (PlayerWindowChooseOne rawChoices)
                      , Just
                          ( QuestionPresentation.QuestionPresentation
                              42
                              "playerWindowChooseOne"
                              12
                              presentationChoices
                            )
                      ) -> do
                        length rawChoices `shouldBe` 12
                        map
                          ( \(QuestionPresentation.ChoicePresentation sourceIndex _ _ _ _ _ _) ->
                              sourceIndex
                          )
                          presentationChoices
                          `shouldBe` [0 .. 11]
                        map
                          ( \(QuestionPresentation.ChoicePresentation _ kind _ _ _ _ _) ->
                              kind
                          )
                          presentationChoices
                          `shouldBe` [ QuestionPresentation.GainResource
                                     , QuestionPresentation.DrawCard
                                     ]
                            <> replicate 6 QuestionPresentation.ChooseTarget
                            <> [ QuestionPresentation.EndTurn
                               , QuestionPresentation.Move
                               , QuestionPresentation.Investigate
                               , QuestionPresentation.Move
                               ]
                    other ->
                      expectationFailure
                        $ "Expected Attic route Q42 player action window, got "
                        <> show other
            other ->
              liftIO $ expectationFailure $ "Unknown Gathering branch " <> other

  it "matches every production round-transition prompt on both encoder paths and canonical replay digest" do
    let
      fixtures =
        [ ( "question-round-end-forced-ability.json"
          , fixtureRoundEndForcedQuestion
          , "e52ee8942cce5602a7ae68a7f8cfd98ad77b7970893ab5216413170bc0b7de44"
          )
        , ( "question-agenda-advance.json"
          , fixtureAgendaAdvanceQuestion
          , "4d665ec3dfd3caf6dce7952ae289cae90310ca600601e3a7dbcdbb1a18727bd7"
          )
        , ( "question-agenda-consequence.json"
          , fixtureAgendaConsequenceQuestion
          , "15d2e7587a32b1ab4ba4c7bb2de77645c05b3e83c03595448d49e3540749689e"
          )
        , ( "question-agenda-horror-assignment.json"
          , fixtureAgendaHorrorAssignmentQuestion
          , "5c3ece0a8ceebbd8c2a883c89bb9db753427b198fc4f5f2c9d834b036f8fe4c9"
          )
        ]
    for_ fixtures \(fileName, question, expectedDigest) -> do
      fixture <- loadFixture fileName
      Aeson.toJSON question `shouldBe` fixture
      viaWireEncoding question `shouldBe` fixture
      canonicalQuestionSha256 question `shouldBe` expectedDigest
    map
      gameScenarioSteps
      [ fixtureRoundTransition.roundEndForcedGame
      , fixtureRoundTransition.agendaAdvanceGame
      , fixtureRoundTransition.agendaConsequenceGame
      , fixtureRoundTransition.agendaHorrorAssignmentGame
      ]
      `shouldBe` [24, 25, 26, 27]

  it "classifies the production agenda confirmation as semantic advanceAgenda" do
    QuestionPresentation.questionPresentation
      (gameScenarioSteps fixtureRoundTransition.agendaAdvanceGame)
      fixtureAgendaAdvanceQuestion
      `shouldBe` QuestionPresentation.QuestionPresentation
        25
        "chooseOne"
        1
        [ QuestionPresentation.ChoicePresentation
            0
            QuestionPresentation.AdvanceAgenda
            Nothing
            (Just $ QuestionPresentation.AgendaEntity fixtureRoundTransitionAgendaId)
            Nothing
            Nothing
            Nothing
        ]

  it "binds the round-end forced ability to Dissonant Voices and its exact window" do
    let
      iid = InvestigatorId "01001"
      source = TreacherySource fixtureRoundTransitionTreacheryId
    case fixtureRoundEndForcedQuestion of
      WindowChooseOne
        [ AbilityLabel
            choiceIid
            ability
            windows
            beforeMessages
            messages
          ] -> do
            choiceIid `shouldBe` iid
            abilitySource ability `shouldBe` source
            abilityRequestor ability `shouldBe` source
            abilityCardCode ability `shouldBe` toCardCode fixtureRoundTransitionTreacheryCard
            abilityIndex ability `shouldBe` 1
            abilityType ability `shouldBe` AbilityType.ForcedAbility (RoundEnds Timing.When)
            abilityWindow ability `shouldBe` RoundEnds Timing.When
            windows `shouldBe` [Window.mkWindow Timing.When Window.AtEndOfRound]
            beforeMessages `shouldBe` []
            messages `shouldBe` []
      other ->
        expectationFailure
          $ "Expected the production Dissonant Voices round-end prompt, got "
          <> show other

  it "binds Agenda 1 advancement and both What's Going On consequences exactly" do
    let
      iid = InvestigatorId "01001"
      source = AgendaSource fixtureRoundTransitionAgendaId
    fixtureAgendaAdvanceQuestion
      `shouldBe` ChooseOne
        [ TargetLabel
            (AgendaTarget fixtureRoundTransitionAgendaId)
            [AdvanceAgendaBy fixtureRoundTransitionAgendaId AgendaAdvancedWithDoom]
        ]
    fixtureAgendaConsequenceQuestion
      `shouldBe` ChooseOne
        [ Label
            "$nightOfTheZealot.theGathering.label.whatsGoingOn.horror"
            [InvestigatorAssignDamage iid source DamageAny 0 2]
        , Label
            "$nightOfTheZealot.theGathering.label.whatsGoingOn.discard"
            [AllRandomDiscard source AnyCard]
        ]

  it "binds the agenda-sourced two-horror assignment shape exactly" do
    let
      iid = InvestigatorId "01001"
      source = AgendaSource fixtureRoundTransitionAgendaId
      expectedMessages =
        [ InvestigatorDamage iid source 0 2
        , InvestigatorDoAssignDamage
            iid
            source
            DamageAny
            AnyAsset
            0
            0
            []
            [InvestigatorTarget iid, InvestigatorTarget iid]
        ]
    fixtureAgendaHorrorAssignmentQuestion
      `shouldBe` QuestionWithSource
        source
        Nothing
        ( QuestionLabel
            "Assign 2 horror"
            Nothing
            (ChooseOne [HorrorLabel iid expectedMessages])
        )

  it "accepts only current versions for every governed round-transition choice" do
    let
      answerValue sourceIndex version =
        Aeson.object
          [ "tag" .= ("Answer" :: Text)
          , "contents"
              .= Aeson.object
                [ "choice" .= sourceIndex
                , "playerId" .= fixturePlayerId
                , "questionVersion" .= version
                ]
          ]
      assertVersionedChoice label game question sourceIndex =
        case stripQuestionWrappers question of
          ChooseOne choices -> case choices !!? sourceIndex of
            Nothing ->
              expectationFailure
                $ label
                <> ": missing source choice "
                <> show sourceIndex
            Just choice -> do
              let checkAnswer version check =
                    case Aeson.fromJSON (answerValue sourceIndex version) of
                      Aeson.Error err ->
                        expectationFailure
                          $ label
                          <> ": could not decode Answer: "
                          <> err
                      Aeson.Success answer ->
                        handleAnswerPure game fixturePlayerId answer >>= check
              checkAnswer (gameScenarioSteps game) \case
                Handled messages -> messages `shouldBe` [uiToRun choice]
                Unhandled reason ->
                  expectationFailure
                    $ label
                    <> ": current Answer rejected: "
                    <> Text.unpack reason
              checkAnswer (gameScenarioSteps game - 1) \case
                Unhandled reason -> reason `shouldBe` "Stale question"
                Handled _ ->
                  expectationFailure
                    $ label
                    <> ": a stale Answer must not resolve"
          other ->
            expectationFailure
              $ label
              <> ": expected a source-indexed ChooseOne prompt, got "
              <> show other
    assertVersionedChoice
      "Q24 Dissonant Voices"
      fixtureRoundTransition.roundEndForcedGame
      fixtureRoundEndForcedQuestion
      0
    assertVersionedChoice
      "Q25 agenda advance"
      fixtureRoundTransition.agendaAdvanceGame
      fixtureAgendaAdvanceQuestion
      0
    assertVersionedChoice
      "Q26 horror consequence"
      fixtureRoundTransition.agendaConsequenceGame
      fixtureAgendaConsequenceQuestion
      0
    assertVersionedChoice
      "Q26 discard consequence"
      fixtureRoundTransition.agendaConsequenceGame
      fixtureAgendaConsequenceQuestion
      1
    assertVersionedChoice
      "Q27 horror assignment"
      fixtureRoundTransition.agendaHorrorAssignmentGame
      fixtureAgendaHorrorAssignmentQuestion
      0

  it "executes every round-transition outcome in Haskell and reconverges on encounter draw" do
    fixtureRoundTransition.dissonantVoicesWasDiscarded `shouldBe` True
    fixtureRoundTransition.agendaOneWasAdvanced `shouldBe` True
    fixtureDiscardRoundTransition.discardHandSizeBefore `shouldBe` 3
    fixtureDiscardRoundTransition.discardHandSizeAfter `shouldBe` 2
    fixtureRoundTransition.horrorBeforeAssignment `shouldBe` 0
    fixtureRoundTransition.horrorAfterAssignment `shouldBe` 2
    fixtureDiscardEncounterDrawQuestion `shouldBe` fixtureEncounterDrawQuestion
    fixtureHorrorEncounterDrawQuestion `shouldBe` fixtureEncounterDrawQuestion
    canonicalQuestionSha256 fixtureDiscardEncounterDrawQuestion
      `shouldBe` "f47283f0c5a1537cbee8cdcb7b2b5faef740a5fa44ec8a8341d92a64e10594f1"
    canonicalQuestionSha256 fixtureHorrorEncounterDrawQuestion
      `shouldBe` "f47283f0c5a1537cbee8cdcb7b2b5faef740a5fa44ec8a8341d92a64e10594f1"
    gameScenarioSteps fixtureDiscardRoundTransition.discardEncounterDrawGame `shouldBe` 27
    gameScenarioSteps fixtureRoundTransition.horrorEncounterDrawGame `shouldBe` 28

  it "keeps the mulligan done action first and preserves every CardIdTarget hand index" do
    case fixtureMulliganQuestion of
      ChooseOne (Label "$label.doneWithMulligan" [FinishedWithMulligan iid] : choices) -> do
        iid `shouldBe` InvestigatorId "01001"
        let targets = [cardId | TargetLabel (CardIdTarget cardId) _ <- choices]
        length choices `shouldBe` length fixtureMulliganCards
        targets `shouldBe` map toCardId fixtureMulliganCards
      other ->
        expectationFailure
          $ "Expected Label(doneWithMulligan) followed by ordered CardIdTargets, got "
          <> show other

  it "keeps The Gathering's scenario intro HeaderEntry and body key stable" do
    case fixtureScenarioIntroReadQuestion of
      Read
        (FlavorText (Just "$nightOfTheZealot.theGathering.intro.title")
          [ HeaderEntry 1 "nightOfTheZealot.theGathering.intro.title"
          , I18nEntry "nightOfTheZealot.theGathering.intro.body" variables
          ])
        (BasicReadChoices [Label "$continue" []])
        Nothing ->
          variables `shouldBe` Map.empty
      other ->
        expectationFailure
          $ "Expected the governed scenario-intro HeaderEntry shape, got "
          <> show other

  it "keeps The Gathering's setup-instructions Read continue choice singular and unconditionally opaque" do
    case fixtureIntroReadQuestion of
      Read _ (BasicReadChoices [Label "$continue" []]) Nothing -> pure ()
      other -> expectationFailure $ "Expected the governed BasicReadChoices continue shape, got " <> show other

  it "keeps startAt's single real Study location choice and its zero-based answer index stable" do
    case fixtureStartAtChooseOneQuestion of
      ChooseOne [TargetLabel (LocationTarget lid) _] ->
        Aeson.toJSON lid `shouldBe` Aeson.String "d5a66e84-c729-4066-8475-d8a155609025"
      other -> expectationFailure $ "Expected a single TargetLabel(LocationTarget) choice, got " <> show other

  it "preserves backend order and zero-based answer indices across multiple LocationTarget choices (high-risk: reindexing)" do
    case fixtureMultiLocationChooseOne of
      ChooseOne choices -> do
        length choices `shouldBe` 3
        let
          targets = [t | TargetLabel t _ <- choices]
          expected = map LocationTarget fixtureMultiLocationIds
        targets `shouldBe` expected
        -- The real Answer.choice index into this exact array must select the
        -- corresponding real location, matching production's `qs !!? qrChoice
        -- response` (Entity/Answer.hs) -- not a client-side reconstruction.
        case (choices !!? 1, fixtureMultiLocationIds !!? 1) of
          (Just (TargetLabel t _), Just lid) -> t `shouldBe` LocationTarget lid
          other -> expectationFailure $ "Expected index 1 to resolve on both sides, got " <> show other
      other -> expectationFailure $ "Expected a multi-choice ChooseOne, got " <> show other

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

  for_ fixtureBasicChoiceQuestions \(fileName, question) ->
    it ("matches the real basic-choice question encoder for " <> fileName) do
      fixture <- loadFixture fileName

      Aeson.toJSON question `shouldBe` fixture
      viaWireEncoding question `shouldBe` fixture

  it "keeps the CORE choice order and semantic labels stable" do
    let tags = map (lookupValue "tag" . Aeson.toJSON) fixtureBasicChoiceChoices

    zip ([0 ..] :: [Int]) tags
      `shouldBe` [ (0, Aeson.String "ComponentLabel")
                 , (1, Aeson.String "ComponentLabel")
                 , (2, Aeson.String "EndTurnButton")
                 , (3, Aeson.String "AbilityLabel")
                 ]
    Aeson.toJSON fixturePlayerId
      `shouldBe` Aeson.String "00000000-0000-0000-0000-000000000001"

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

  it "keeps investigatorCardPool out of the public wire while leaving it in the internal encoder (see Haddock on fixtureInvestigatorCardPoolShim above)" do
    let (internalEncoding, publicToJson, publicWireEncoding) = fixtureInvestigatorCardPoolShim
        hasKey k = \case
          Aeson.Object o -> AesonKeyMap.member (AesonKey.fromText k) o
          _ -> False
        lookupKey k = \case
          Aeson.Object o -> AesonKeyMap.lookup (AesonKey.fromText k) o
          _ -> Nothing

    -- The internal InvestigatorAttrs/persistence encoder must still emit the
    -- real, non-null cardPool value: this fix must never touch save/undo/replay.
    hasKey "cardPool" internalEncoding `shouldBe` True
    lookupKey "cardPool" internalEncoding `shouldNotBe` Just Aeson.Null
    lookupKey "cardPool" internalEncoding
      `shouldBe` Just (Aeson.toJSON (ArkhamBuildCardPool ["cycle:core"]))

    -- Neither public wire path (plain toJSON, nor the toEncoding-driven
    -- wire-byte path) may emit a cardPool key at all -- not even a null one.
    hasKey "cardPool" publicToJson `shouldBe` False
    hasKey "cardPool" publicWireEncoding `shouldBe` False

    -- Unrelated, previously-governed fields must remain present and
    -- unaffected by the shim.
    hasKey "cardsUnderneath" publicToJson `shouldBe` True
    hasKey "cardsUnderneath" publicWireEncoding `shouldBe` True

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

  it "pairs the native Answer with the authoritative CORE question version" do
    fixture <- loadFixture "answer-question.json"

    case Aeson.fromJSON fixture of
      Aeson.Error err -> expectationFailure $ "Could not decode answer-question.json: " <> err
      Aeson.Success (Answer (QuestionResponse choice playerId questionVersion)) -> do
        choice `shouldBe` 2
        playerId `shouldBe` Just fixturePlayerId
        questionVersion `shouldBe` Just (gameScenarioSteps fixtureBoardGame)
        case drop choice fixtureBasicChoiceChoices of
          EndTurnButton {} : _ -> pure ()
          _ -> expectationFailure "Answer.choice 2 must select the CORE EndTurnButton"
      Aeson.Success answer ->
        expectationFailure $ "Expected Answer QuestionResponse, got " <> show answer
