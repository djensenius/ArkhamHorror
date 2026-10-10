module Arkham.Campaign.TheScarletKeys.ConcealedSpec (spec) where

import Arkham.Act (lookupAct)
import Arkham.Act.CardDefs.TheScarletKeys.DancingMad qualified as DancingActs
import Arkham.Act.CardDefs.TheScarletKeys.DealingsInTheDark qualified as Acts
import Arkham.Act.Types (Act)
import Arkham.Agenda.CardDefs.TheScarletKeys.WithoutATrace qualified as WithoutATraceAgendas
import Arkham.Agenda.Types (Agenda)
import Arkham.Asset.Cards qualified as Assets
import Arkham.Campaigns.TheScarletKeys.Concealed (Field (..), mkConcealedCard)
import Arkham.Campaigns.TheScarletKeys.Concealed.Helpers (turnOverAllConcealed)
import Arkham.Campaigns.TheScarletKeys.Concealed.Kind
import Arkham.Card.CardDef
import Arkham.Classes.HasGame
import Arkham.Enemy.CardDefs.TheScarletKeys.CongressOfTheKeys qualified as CongressEnemies
import Arkham.Enemy.CardDefs.TheScarletKeys.CrimsonConspiracy qualified as Enemies
import Arkham.Enemy.CardDefs.TheScarletKeys.MysteriesAbound qualified as MysteriesEnemies
import Arkham.Enemy.Types (Enemy)
import Arkham.Entities qualified as Entities
import Arkham.Location.CardDefs.TheScarletKeys.BeyondTheBeyond qualified as BeyondLocations
import Arkham.Location.CardDefs.TheScarletKeys.CongressOfTheKeys qualified as Cards
import Arkham.Location.CardDefs.TheScarletKeys.DealingsInTheDark qualified as Locations
import Arkham.Location.CardDefs.TheScarletKeys.ShadesOfSuffering qualified as ShadesLocations
import Arkham.Location.Grid
import Arkham.Location.Types (revealedL)
import Arkham.Matcher
import Arkham.Placement
import Arkham.Projection
import Arkham.Token (Token (Charge))
import Data.Text qualified as T
import TestImport.New

{- | Put Coterie Agent (A) in the shadows with its mini-card at @location@, the way resolving its
@concealed 2@ keyword would.
-}
concealAgentAt :: Investigator -> Location -> TestAppT Enemy
concealAgentAt self location = do
  agent <- testEnemyWithDef Enemies.coterieAgentA id
  run $ PlaceEnemy (toId agent) InTheShadows
  card <- mkConcealedCard CoterieAgentA
  run $ CreateConcealedCard card
  run $ PlaceConcealedCard (toId self) card.id (AtLocation $ toId location)
  pure agent

{- | Walk the prompts that follow choosing to expose: pick the mini-card, confirm the flip, then
decline Coterie Agent (A)'s own "when exposed" free reaction (discard itself), which pauses the
queue before the enemy moves out of the shadows.
-}
exposeConcealedCard :: HasCallStack => TestAppT ()
exposeConcealedCard = do
  clickLabel "$label.exposeConcealedCard"
  click "choose concealed card"
  click "flip concealed card"
  skip

realAct :: CardDef -> TestAppT Act
realAct def = do
  card <- genCard def
  let actId' = ActId (toCardCode card)
      act' = either (error . show) id $ lookupAct actId' 1 (toCardId card)
  overTest $ entitiesL . Entities.actsL %~ insertEntity act'
  pure act'

realAgenda :: CardDef -> TestAppT Agenda
realAgenda = realAgendaSide 1

realAgendaSide :: Int -> CardDef -> TestAppT Agenda
realAgendaSide side def = do
  card <- genCard def
  let agendaId' = AgendaId (toCardCode card)
      agenda' = lookupAgenda agendaId' side (toCardId card)
  overTest $ entitiesL . Entities.agendasL %~ insertEntity agenda'
  pure agenda'

assertLabelDisabled :: HasCallStack => Text -> TestAppT ()
assertLabelDisabled suffix = do
  questionMap <- gameQuestion <$> getGame
  let
    choicesOf question = case stripQuestionWrappers question of
      ChooseOne xs -> xs
      PlayerWindowChooseOne xs -> xs
      WindowChooseOne xs -> xs
      ChooseN _ xs -> xs
      ChooseSome xs -> xs
      ChooseSome1 _ xs -> xs
      ChooseUpToN _ xs -> xs
      ChooseOneAtATime xs -> xs
      ChooseOneAtATimeWithAuto _ xs -> xs
      _ -> []
    labelMatches label = suffix `T.isSuffixOf` label
    choices = concatMap (choicesOf . snd) (mapToList questionMap)
    enabled = any (\case Label label _ -> labelMatches label; _ -> False) choices
    disabled = any (\case InvalidLabel label -> labelMatches label; _ -> False) choices
  when enabled $ expectationFailure $ "expected label ending in " <> T.unpack suffix <> " to be disabled, but it was enabled"
  unless disabled
    $ expectationFailure
    $ "expected disabled label ending in "
    <> T.unpack suffix
    <> " but no matching InvalidLabel was found; choices were: "
    <> show choices

setAsideGrandBazaars :: TestAppT ()
setAsideGrandBazaars = do
  grandBazaars <-
    traverse
      genCard
      [ Locations.grandBazaarBusyWalkway
      , Locations.grandBazaarCrowdedShops
      , Locations.grandBazaarDarkenedAlley
      , Locations.grandBazaarJewelersRoad
      , Locations.grandBazaarMarbleFountain
      , Locations.grandBazaarPublicBaths
      , Locations.grandBazaarRooftopAccess
      ]
  run $ SetAsideCards grandBazaars

spec :: Spec
spec = describe "Concealed mini-cards" do
  -- #5387: exposure by investigating used to hang off the clue-discovery pipeline, so an
  -- investigation that discovered nothing could never expose.
  context "exposing by investigating" do
    it "is offered when the location has no clues to discover" . gameTest $ \self -> do
      withProp @"intellect" 3 self
      setChaosTokens [Zero]
      location <- testLocation & prop @"clues" 0 & prop @"shroud" 0
      self `moveTo` location
      _ <- concealAgentAt self location

      self `investigate` location
      startSkillTest
      applyResults
      exposeConcealedCard

      assertNone $ EnemyWithPlacement InTheShadows
      assertNone ConcealedCardAny

    it "is offered when an empty Divination discovers no clues" . gameTest $ \self -> do
      withProp @"intellect" 3 self
      setChaosTokens [Zero]
      location <- testLocation & prop @"clues" 3 & prop @"shroud" 0
      self `moveTo` location
      _ <- concealAgentAt self location

      divination <- self `putAssetIntoPlay` Assets.divination1
      run $ SpendUses (toSource self) (toTarget divination) Charge 4

      [doInvestigate] <- self `getActionsFrom` divination
      self `useAbility` doInvestigate
      clickLabel "$label.cards.divination1.useIntellect"
      startSkillTest
      applyResults
      exposeConcealedCard

      assertNone $ EnemyWithPlacement InTheShadows
      assertNone ConcealedCardAny
      -- exposing replaces the standard effects of the ability that exposed it
      location.clues `shouldReturn` 3
      self.clues `shouldReturn` 0

    it "does not prompt twice when the investigation also discovers a clue" . gameTest $ \self -> do
      withProp @"intellect" 3 self
      setChaosTokens [Zero]
      location <- testLocation & prop @"clues" 1 & prop @"shroud" 0
      self `moveTo` location
      _ <- concealAgentAt self location

      self `investigate` location
      startSkillTest
      applyResults
      clickLabel "$label.doNotExposeConcealed"

      location.clues `shouldReturn` 0
      self.clues `shouldReturn` 1
      assertAny ConcealedCardAny

  context "exposing grid-position concealed cards" do
    it "resolves an exposed enemy mini-card from its grid position" . gameTest $ \self -> do
      location <- testLocation
      self `moveTo` location
      agent <- testEnemyWithDef Enemies.coterieAgentA id
      run $ PlaceEnemy (toId agent) InTheShadows
      card <- mkConcealedCard CoterieAgentA
      run $ CreateConcealedCard card
      run $ PlaceConcealedCard (toId self) card.id (InPosition $ Pos 0 0)

      run $ Flip (toId self) (toSource self) (toTarget card.id)
      chooseTarget card.id
      skip

      assertNone $ EnemyWithPlacement InTheShadows
      assertNone ConcealedCardAny
      agent.location `shouldReturn` Just (toId location)

  context "placing concealed cards" do
    it "falls back to candidate locations when the investigator is temporarily unplaced" . gameTest $ \self -> do
      firstLocation <- testLocation
      secondLocation <- testLocation
      thirdLocation <- testLocation
      card <- mkConcealedCard AcolyteAny
      decoy <- mkConcealedCard Decoy
      run $ CreateConcealedCard card
      run $ CreateConcealedCard decoy
      run $ PlaceInvestigator (toId self) Unplaced

      run $ PlaceConcealedCards (toId self) [card.id, decoy.id] (map toId [firstLocation, secondLocation, thirdLocation])

      assertTarget firstLocation
      chooseTarget firstLocation
      assertTarget secondLocation
      chooseTarget secondLocation
      field ConcealedCardPlacement card.id `shouldReturn` AtLocation (toId firstLocation)
      field ConcealedCardPlacement decoy.id `shouldReturn` AtLocation (toId secondLocation)

  context "exposing every concealed card in play" do
    it "does not offer unplaced or flipped mini-cards" . gameTest $ \self -> do
      galata <- testLocationWithDef Locations.galata (revealedL .~ True)
      self `moveTo` galata
      live <- mkConcealedCard SinisterAspirantC
      run $ CreateConcealedCard live
      run $ PlaceConcealedCard (toId self) live.id (AtLocation $ toId galata)
      flippedInPosition <- mkConcealedCard CoterieAgentA
      run $ CreateConcealedCard flippedInPosition
      run $ PlaceConcealedCard (toId self) flippedInPosition.id (InPosition $ Pos 0 0)
      run $ DoStep 0 $ Flip (toId self) (toSource self) (toTarget flippedInPosition.id)
      unplaced <- mkConcealedCard AcolyteAny
      run $ CreateConcealedCard unplaced
      run $ DoStep 0 $ Flip (toId self) (toSource self) (toTarget unplaced.id)

      run $ UseCardAbility (toId self) (toSource galata) 1 [] NoPayment

      assertTarget live.id
      assertNotTarget flippedInPosition.id
      assertNotTarget unplaced.id
      chooseTarget live.id
      chooseTarget live.id

      assertNone $ ConcealedCardWithId live.id

    it "offers only unexposed in-play mini-cards when Search for the Talisman advances" . gameTest $ \self -> do
      location <- testLocation
      self `moveTo` location
      act <- realAct Acts.searchForTheTalisman
      liveAtLocation <- mkConcealedCard SinisterAspirantC
      run $ CreateConcealedCard liveAtLocation
      run $ PlaceConcealedCard (toId self) liveAtLocation.id (AtLocation $ toId location)
      liveInPosition <- mkConcealedCard CoterieAgentA
      run $ CreateConcealedCard liveInPosition
      run $ PlaceConcealedCard (toId self) liveInPosition.id (InPosition $ Pos 0 0)
      unplaced <- mkConcealedCard AcolyteAny
      run $ CreateConcealedCard unplaced

      run $ AdvanceAct act.id (TestSource mempty) AdvancedWithOther
      run ClearUI
      run $ Do $ AdvanceAct act.id (TestSource mempty) AdvancedWithOther

      assertTarget liveAtLocation.id
      assertTarget liveInPosition.id
      assertNotTarget unplaced.id

  context "Gravity-Defying Climb" do
    it "turns every in-play mini-card face-down after a wrong-order exposure" . gameTest $ \self -> do
      location <- testLocationWithDef Cards.gravityDefyingClimb (revealedL .~ True)
      self `moveTo` location
      exposed <- mkConcealedCard CityOfRemnantsL
      run $ CreateConcealedCard exposed
      run $ PlaceConcealedCard (toId self) exposed.id (InPosition $ Pos (-1) 1)
      run $ DoStep 0 $ Flip (toId self) (toSource self) (toTarget exposed.id)
      hidden <- mkConcealedCard CityOfRemnantsM
      run $ CreateConcealedCard hidden
      run $ PlaceConcealedCard (toId self) hidden.id (InPosition $ Pos 1 1)

      run $ Flip (toId self) (toSource self) (toTarget hidden.id)
      chooseTarget hidden.id
      useForcedAbility

      assertNone $ ConcealedCardWithId exposed.id <> ExposedConcealedCard
      assertNone $ ConcealedCardWithId hidden.id <> ExposedConcealedCard

    it "offers only hidden mini-cards after a correct-order exposure" . gameTest $ \self -> do
      location <- testLocationWithDef Cards.gravityDefyingClimb (revealedL .~ True)
      self `moveTo` location
      expected <- mkConcealedCard CityOfRemnantsL
      run $ CreateConcealedCard expected
      run $ PlaceConcealedCard (toId self) expected.id (InPosition $ Pos (-1) 1)
      hidden <- mkConcealedCard CityOfRemnantsM
      run $ CreateConcealedCard hidden
      run $ PlaceConcealedCard (toId self) hidden.id (InPosition $ Pos 1 1)
      alreadyFlipped <- mkConcealedCard CityOfRemnantsR
      run $ CreateConcealedCard alreadyFlipped
      run $ PlaceConcealedCard (toId self) alreadyFlipped.id (AtLocation $ toId location)
      run $ DoStep 0 $ Flip (toId self) (toSource self) (toTarget alreadyFlipped.id)

      run $ Flip (toId self) (toSource self) (toTarget expected.id)
      chooseTarget expected.id
      useForcedAbility

      assertTarget hidden.id
      assertNotTarget alreadyFlipped.id

  context "audited concealed-card callers" do
    it "turnOverAllConcealed turns over only in-play mini-cards" . gameTest $ \self -> do
      location <- testLocation
      live <- mkConcealedCard SinisterAspirantC
      run $ CreateConcealedCard live
      run $ PlaceConcealedCard (toId self) live.id (AtLocation $ toId location)
      unplaced <- mkConcealedCard Decoy
      run $ CreateConcealedCard unplaced

      runQueueT $ turnOverAllConcealed (TestSource mempty)
      runMessages

      assertAny $ ConcealedCardWithId live.id <> ExposedConcealedCard
      assertNone $ ConcealedCardWithId unplaced.id <> ExposedConcealedCard

    it "Search for the Manuscript redistributes only in-play mini-cards" . gameTest $ \self -> do
      location <- testLocation
      live <- mkConcealedCard SinisterAspirantC
      run $ CreateConcealedCard live
      run $ PlaceConcealedCard (toId self) live.id (AtLocation $ toId location)
      unplaced <- mkConcealedCard Decoy
      run $ CreateConcealedCard unplaced
      setAsideGrandBazaars
      act <- realAct Acts.searchForTheManuscript
      token <- createChaosToken Zero

      run $ RequestedChaosTokens (toSource act) Nothing [token]
      clickLabel "$label.continue"

      field ConcealedCardPlacement live.id `shouldNotReturn` Unplaced
      field ConcealedCardPlacement unplaced.id `shouldReturn` Unplaced

    it "False Step (v. II) redistributes only in-play mini-cards" . gameTest $ \self -> do
      firstLocation <- testLocation
      _secondLocation <- testLocation
      live <- mkConcealedCard SinisterAspirantC
      run $ CreateConcealedCard live
      run $ PlaceConcealedCard (toId self) live.id (AtLocation $ toId firstLocation)
      unplaced <- mkConcealedCard Decoy
      run $ CreateConcealedCard unplaced
      act <- realAct DancingActs.falseStepV2

      run $ Do $ AdvanceAct act.id (TestSource mempty) AdvancedWithOther

      field ConcealedCardPlacement unplaced.id `shouldReturn` Unplaced
      assertAny $ ConcealedCardWithId live.id
      assertAny $ ConcealedCardWithId unplaced.id

    it "Coterie Envoy does not offer its defeat reaction for only unplaced mini-cards" . gameTest $ \self -> do
      location <- testLocation
      self `moveTo` location
      envoy <- testEnemyWithDef MysteriesEnemies.coterieEnvoy id
      envoy `spawnAt` location
      unplaced <- mkConcealedCard Decoy
      run $ CreateConcealedCard unplaced

      run $ Defeated (toTarget envoy) (toCardId envoy) (InvestigatorSource $ toId self) []

      assertNoReactionOf envoy

    it "Otherworldly Horror disables the shuffle option for only unplaced mini-cards" . gameTest $ \_ -> do
      unplaced <- mkConcealedCard Decoy
      run $ CreateConcealedCard unplaced
      agenda <- realAgendaSide 2 WithoutATraceAgendas.otherworldlyHorror

      run $ AdvanceAgendaBy agenda.id AgendaAdvancedWithOther
      chooseTarget agenda

      assertLabelDisabled "otherworldlyHorror.shuffleAllConcealed"

    it "Otherworldly Lambs disables the shuffle option for only unplaced mini-cards" . gameTest $ \_ -> do
      unplaced <- mkConcealedCard Decoy
      run $ CreateConcealedCard unplaced
      agenda <- realAgendaSide 2 WithoutATraceAgendas.otherworldlyLambs

      run $ AdvanceAgendaBy agenda.id AgendaAdvancedWithOther
      chooseTarget agenda

      assertLabelDisabled "otherworldlyLambs.shuffleAllConcealed"

    it "Melati's Shop only offers in-play mini-cards" . gameTest $ \self -> do
      melatisShop <- testLocationWithDef ShadesLocations.melatisShop (revealedL .~ True)
      self `moveTo` melatisShop
      live <- mkConcealedCard TzuSanNiang
      run $ CreateConcealedCard live
      run $ PlaceConcealedCard (toId self) live.id (AtLocation $ toId melatisShop)
      unplaced <- mkConcealedCard Decoy
      run $ CreateConcealedCard unplaced

      run $ UseCardAbility (toId self) (toSource melatisShop) 1 [] NoPayment

      assertTarget live.id
      assertNotTarget unplaced.id

    it "Mimetic Nemesis only turns over in-play mini-cards" . gameTest $ \self -> do
      location <- testLocation
      self `moveTo` location
      mimeticNemesis <- testEnemyWithDef CongressEnemies.mimeticNemesisInfiltratorOfRealities id
      run $ PlaceEnemy (toId mimeticNemesis) (AtLocation $ toId location)
      live <- mkConcealedCard MimeticNemesis
      run $ CreateConcealedCard live
      run $ PlaceConcealedCard (toId self) live.id (InPosition $ Pos 0 0)
      unplaced <- mkConcealedCard Decoy
      run $ CreateConcealedCard unplaced

      run $ UseCardAbility (toId self) (toSource mimeticNemesis) 1 [] NoPayment

      assertTarget live.id
      assertNotTarget unplaced.id

    it "Otherworldly Slaughter only reveals in-play mini-cards" . gameTest $ \self -> do
      location <- testLocation
      self `moveTo` location
      live <- mkConcealedCard Decoy
      run $ CreateConcealedCard live
      run $ PlaceConcealedCard (toId self) live.id (InPosition $ Pos 0 0)
      unplaced <- mkConcealedCard Decoy
      run $ CreateConcealedCard unplaced
      agenda <- realAgenda WithoutATraceAgendas.otherworldlySlaughter

      run $ UseCardAbility (toId self) (toSource agenda) 2 [] NoPayment

      assertTarget live.id
      assertNotTarget unplaced.id

  context "Weald of Effigies" do
    it "does not offer unplaced mini-cards for swaps" . gameTest $ \self -> do
      weald <- testLocationWithDef BeyondLocations.wealdOfEffigiesA (revealedL .~ True)
      self `moveTo` weald
      firstCard <- mkConcealedCard SinisterAspirantC
      run $ CreateConcealedCard firstCard
      run $ PlaceConcealedCard (toId self) firstCard.id (AtLocation $ toId weald)
      secondCard <- mkConcealedCard CoterieAgentA
      run $ CreateConcealedCard secondCard
      run $ PlaceConcealedCard (toId self) secondCard.id (InPosition $ Pos 0 0)
      unplaced <- mkConcealedCard AcolyteAny
      run $ CreateConcealedCard unplaced

      run $ UseCardAbility (toId self) (toSource weald) 2 [] NoPayment
      clickLabel "$theScarletKeys.label.wealdOfEffigies.miniCards"

      assertTarget firstCard.id
      assertTarget secondCard.id
      assertNotTarget unplaced.id
