module Arkham.Question.Presentation (
  QuestionPresentation (..),
  ChoicePresentation (..),
  ChoicePresentationKind (..),
  PresentationEntity (..),
  PresentationLabel (..),
  AbilityPresentation (..),
  PresentationCost (..),
  PresentationAmount (..),
  PresentationScope (..),
  questionPresentationProtocolVersion,
  questionPresentation,
  questionPresentations,
) where

import Arkham.Ability (abilityActions, abilityCost)
import Arkham.Ability.Type qualified as AbilityType
import Arkham.Ability.Types qualified as Ability
import Arkham.Action qualified as Action
import Arkham.Card.CardCode (CardCode)
import Arkham.Card.Id (CardId)
import Arkham.Cost qualified as Cost
import Arkham.GameValue qualified as GameValue
import Arkham.Id
import Arkham.Matcher.Location qualified as Location
import Arkham.Message qualified as Message
import Arkham.Prelude
import Arkham.Question
import Arkham.Source
import Arkham.Target
import Data.Map.Strict qualified as Map

questionPresentationProtocolVersion :: Int
questionPresentationProtocolVersion = 1

data QuestionPresentation = QuestionPresentation
  Int
  Text
  Int
  [ChoicePresentation]
  deriving stock (Show, Eq)

data ChoicePresentation = ChoicePresentation
  Int
  ChoicePresentationKind
  (Maybe InvestigatorId)
  (Maybe PresentationEntity)
  (Maybe PresentationLabel)
  (Maybe AbilityPresentation)
  (Maybe PresentationCost)
  deriving stock (Show, Eq)

data ChoicePresentationKind
  = AdvanceAct
  | AdvanceAgenda
  | ApplySkillTestResults
  | AssignDamage
  | AssignHorror
  | ChooseTarget
  | DrawCard
  | EndTurn
  | Engage
  | Evade
  | Fight
  | GainResource
  | Investigate
  | LocalizedLabel
  | Move
  | ResolveForcedAbility
  | SkipTriggers
  | StartSkillTest
  | UseAbility
  deriving stock (Show, Eq)

data PresentationEntity
  = ActEntity ActId
  | AgendaEntity AgendaId
  | AssetEntity AssetId
  | CardEntity CardId
  | CardCodeEntity CardCode
  | EffectEntity EffectId
  | EnemyEntity EnemyId
  | EventEntity EventId
  | InvestigatorEntity InvestigatorId
  | LocationEntity LocationId
  | PlayerEntity PlayerId
  | ScenarioEntity Text
  | SkillEntity SkillId
  | StoryEntity StoryId
  | TreacheryEntity TreacheryId
  deriving stock (Show, Eq)

newtype PresentationLabel = EmbeddedI18nLabel Text
  deriving stock (Show, Eq)

data AbilityPresentation = AbilityPresentation
  CardCode
  Int
  Text
  [Text]
  Bool
  deriving stock (Show, Eq)

data PresentationCost
  = FreePresentationCost
  | ActionPresentationCost Int
  | ResourcePresentationCost Int
  | CluePresentationCost PresentationAmount
  | GroupCluePresentationCost PresentationAmount PresentationScope
  | GroupResourcePresentationCost PresentationAmount PresentationScope
  | AllPresentationCosts [PresentationCost]
  | AlternativePresentationCosts [PresentationCost]
  | OtherPresentationCost
  deriving stock (Show, Eq)

data PresentationAmount
  = FixedAmount Int
  | PerPlayerAmount Int
  | FixedPlusPerPlayerAmount Int Int
  | ByPlayerCountAmount Int Int Int Int
  | XAmount
  | StarAmount
  | UnknownAmount
  deriving stock (Show, Eq)

data PresentationScope
  = AnywhereScope
  | SameLocationScope
  | LocationScope LocationId
  | OtherScope
  deriving stock (Show, Eq)

instance ToJSON QuestionPresentation where
  toJSON (QuestionPresentation version kind choiceCount choices) =
    object
      [ "protocolVersion" .= questionPresentationProtocolVersion
      , "questionVersion" .= version
      , "questionKind" .= kind
      , "choiceCount" .= choiceCount
      , "choices" .= choices
      ]

instance ToJSON ChoicePresentation where
  toJSON (ChoicePresentation sourceIndex kind actor entity label ability cost) =
    object
      $ [ "sourceIndex" .= sourceIndex
        , "kind" .= choiceKindText kind
        ]
      <> ["actorId" .= value | Just value <- [actor]]
      <> ["entity" .= value | Just value <- [entity]]
      <> ["label" .= value | Just value <- [label]]
      <> ["ability" .= value | Just value <- [ability]]
      <> ["cost" .= value | Just value <- [cost]]

instance ToJSON PresentationEntity where
  toJSON = \case
    ActEntity entityId -> entityJson "act" entityId
    AgendaEntity entityId -> entityJson "agenda" entityId
    AssetEntity entityId -> entityJson "asset" entityId
    CardEntity entityId -> entityJson "card" entityId
    CardCodeEntity entityId -> entityJson "cardCode" entityId
    EffectEntity entityId -> entityJson "effect" entityId
    EnemyEntity entityId -> entityJson "enemy" entityId
    EventEntity entityId -> entityJson "event" entityId
    InvestigatorEntity entityId -> entityJson "investigator" entityId
    LocationEntity entityId -> entityJson "location" entityId
    PlayerEntity entityId -> entityJson "player" entityId
    ScenarioEntity entityId -> entityJson "scenario" entityId
    SkillEntity entityId -> entityJson "skill" entityId
    StoryEntity entityId -> entityJson "story" entityId
    TreacheryEntity entityId -> entityJson "treachery" entityId
   where
    entityJson :: ToJSON entityId => Text -> entityId -> Value
    entityJson kind entityId = object ["kind" .= kind, "id" .= entityId]

instance ToJSON PresentationLabel where
  toJSON (EmbeddedI18nLabel label) =
    object ["kind" .= String "embeddedI18n", "text" .= label]

instance ToJSON AbilityPresentation where
  toJSON (AbilityPresentation cardCode abilityIndex kind actions canBeCancelled) =
    object
      [ "cardCode" .= cardCode
      , "index" .= abilityIndex
      , "type" .= kind
      , "actions" .= actions
      , "canBeCancelled" .= canBeCancelled
      ]

instance ToJSON PresentationCost where
  toJSON = \case
    FreePresentationCost -> object ["kind" .= String "free"]
    ActionPresentationCost amount ->
      object ["kind" .= String "action", "amount" .= amount]
    ResourcePresentationCost amount ->
      object ["kind" .= String "resource", "amount" .= amount]
    CluePresentationCost amount ->
      object ["kind" .= String "clue", "amount" .= amount]
    GroupCluePresentationCost amount scope ->
      object ["kind" .= String "groupClue", "amount" .= amount, "scope" .= scope]
    GroupResourcePresentationCost amount scope ->
      object ["kind" .= String "groupResource", "amount" .= amount, "scope" .= scope]
    AllPresentationCosts costs ->
      object ["kind" .= String "all", "costs" .= costs]
    AlternativePresentationCosts costs ->
      object ["kind" .= String "choice", "costs" .= costs]
    OtherPresentationCost -> object ["kind" .= String "other"]

instance ToJSON PresentationAmount where
  toJSON = \case
    FixedAmount value -> object ["kind" .= String "fixed", "value" .= value]
    PerPlayerAmount value -> object ["kind" .= String "perPlayer", "value" .= value]
    FixedPlusPerPlayerAmount fixed perPlayer ->
      object
        [ "kind" .= String "fixedPlusPerPlayer"
        , "fixed" .= fixed
        , "perPlayer" .= perPlayer
        ]
    ByPlayerCountAmount one two three four ->
      object ["kind" .= String "byPlayerCount", "values" .= [one, two, three, four]]
    XAmount -> object ["kind" .= String "x"]
    StarAmount -> object ["kind" .= String "star"]
    UnknownAmount -> object ["kind" .= String "unknown"]

instance ToJSON PresentationScope where
  toJSON = \case
    AnywhereScope -> object ["kind" .= String "anywhere"]
    SameLocationScope -> object ["kind" .= String "sameLocation"]
    LocationScope locationId ->
      object ["kind" .= String "location", "locationId" .= locationId]
    OtherScope -> object ["kind" .= String "other"]

choiceKindText :: ChoicePresentationKind -> Text
choiceKindText = \case
  AdvanceAct -> "advanceAct"
  AdvanceAgenda -> "advanceAgenda"
  ApplySkillTestResults -> "applySkillTestResults"
  AssignDamage -> "assignDamage"
  AssignHorror -> "assignHorror"
  ChooseTarget -> "chooseTarget"
  DrawCard -> "drawCard"
  EndTurn -> "endTurn"
  Engage -> "engage"
  Evade -> "evade"
  Fight -> "fight"
  GainResource -> "gainResource"
  Investigate -> "investigate"
  LocalizedLabel -> "localizedLabel"
  Move -> "move"
  ResolveForcedAbility -> "resolveForcedAbility"
  SkipTriggers -> "skipTriggers"
  StartSkillTest -> "startSkillTest"
  UseAbility -> "useAbility"

data ChoiceContext = PlayerWindowContext | GeneralChoiceContext
  deriving stock Eq

questionPresentation :: Int -> Question Message.Message -> QuestionPresentation
questionPresentation version question =
  let (kind, context, choices) = questionChoices question
   in QuestionPresentation
        version
        kind
        (length choices)
        (mapMaybe (uncurry $ presentChoice context) $ zip [0 ..] choices)

questionPresentations
  :: Int
  -> Map PlayerId (Question Message.Message)
  -> Map PlayerId QuestionPresentation
questionPresentations version = Map.map (questionPresentation version)

questionChoices :: Question msg -> (Text, ChoiceContext, [UI msg])
questionChoices = \case
  ChooseOne choices -> ("chooseOne", GeneralChoiceContext, choices)
  PlayerWindowChooseOne choices -> ("playerWindowChooseOne", PlayerWindowContext, choices)
  WindowChooseOne choices -> ("windowChooseOne", GeneralChoiceContext, choices)
  ChooseN _ choices -> ("chooseN", GeneralChoiceContext, choices)
  ChooseSome choices -> ("chooseSome", GeneralChoiceContext, choices)
  ChooseSome1 _ choices -> ("chooseSome", GeneralChoiceContext, choices)
  ChooseUpToN _ choices -> ("chooseUpToN", GeneralChoiceContext, choices)
  ChooseOneAtATime choices -> ("chooseOneAtATime", GeneralChoiceContext, choices)
  -- Answer index 0 is a synthetic "resolve all" action that is not present in
  -- the raw choices array, so v1 cannot safely expose this as ordinary indices.
  ChooseOneAtATimeWithAuto _ _ -> ("unsupported", GeneralChoiceContext, [])
  QuestionLabel _ _ question -> questionChoices question
  PayCostQuestion _ question -> questionChoices question
  QuestionWithSource _ _ question -> questionChoices question
  Read _ readChoices _ ->
    ("read", GeneralChoiceContext, readChoiceList readChoices)
  _ -> ("unsupported", GeneralChoiceContext, [])

readChoiceList :: ReadChoices msg -> [UI msg]
readChoiceList = \case
  BasicReadChoices choices -> choices
  BasicReadChoicesN _ choices -> choices
  BasicReadChoicesUpToN _ choices -> choices
  LeadInvestigatorMustDecide choices -> choices

presentChoice :: ChoiceContext -> Int -> UI Message.Message -> Maybe ChoicePresentation
presentChoice context sourceIndex = \case
  Label label _ -> Just $ labeledChoice sourceIndex label Nothing
  TooltipLabel label _ _ -> Just $ labeledChoice sourceIndex label Nothing
  CardLabel cardCode _ _ ->
    Just $ targetChoice sourceIndex ChooseTarget (CardCodeEntity cardCode)
  PortraitLabel investigatorId _ ->
    Just $ targetChoice sourceIndex ChooseTarget (InvestigatorEntity investigatorId)
  TargetLabel target messages -> do
    entity <- entityFromTarget target
    pure $ targetChoice sourceIndex (targetChoiceKind entity messages) entity
  EvadeLabel enemyId _ ->
    Just $ actionTargetChoice sourceIndex Evade (EnemyEntity enemyId)
  EvadeLabelWithSkill enemyId _ _ ->
    Just $ actionTargetChoice sourceIndex Evade (EnemyEntity enemyId)
  FightLabel enemyId _ ->
    Just $ actionTargetChoice sourceIndex Fight (EnemyEntity enemyId)
  FightLabelWithSkill enemyId _ _ ->
    Just $ actionTargetChoice sourceIndex Fight (EnemyEntity enemyId)
  EngageLabel enemyId _ ->
    Just $ actionTargetChoice sourceIndex Engage (EnemyEntity enemyId)
  AbilityLabel investigatorId ability _ _ _ ->
    abilityChoice sourceIndex investigatorId ability
  ComponentLabel component messages ->
    case component of
      InvestigatorComponent investigatorId DamageToken
        | any (assignsDamageTo investigatorId) messages ->
        Just $ targetChoice sourceIndex AssignDamage (InvestigatorEntity investigatorId)
      InvestigatorComponent investigatorId HorrorToken
        | any (assignsHorrorTo investigatorId) messages ->
        Just $ targetChoice sourceIndex AssignHorror (InvestigatorEntity investigatorId)
      InvestigatorComponent investigatorId ResourceToken
        | context == PlayerWindowContext ->
            Just
              $ ChoicePresentation
                sourceIndex
                GainResource
                (Just investigatorId)
                Nothing
                Nothing
                Nothing
                Nothing
      InvestigatorDeckComponent investigatorId
        | context == PlayerWindowContext ->
            Just
              $ ChoicePresentation
                sourceIndex
                DrawCard
                (Just investigatorId)
                Nothing
                Nothing
                Nothing
                Nothing
      _ -> Nothing
  EndTurnButton investigatorId _ ->
    Just
      $ ChoicePresentation
        sourceIndex
        EndTurn
        (Just investigatorId)
        Nothing
        Nothing
        Nothing
        Nothing
  StartSkillTestButton investigatorId ->
    Just
      $ ChoicePresentation
        sourceIndex
        StartSkillTest
        (Just investigatorId)
        Nothing
        Nothing
        Nothing
        Nothing
  SkillTestApplyResultsButton ->
    Just
      $ ChoicePresentation
        sourceIndex
        ApplySkillTestResults
        Nothing
        Nothing
        Nothing
        Nothing
        Nothing
  Done label -> Just $ labeledChoice sourceIndex label Nothing
  SkipTriggersButton investigatorId ->
    Just
      $ ChoicePresentation
        sourceIndex
        SkipTriggers
        (Just investigatorId)
        Nothing
        Nothing
        Nothing
        Nothing
  GridLabel label _ -> Just $ labeledChoice sourceIndex label Nothing
  SkillLabelWithLabel label _ _ -> Just $ labeledChoice sourceIndex label Nothing
  ScenarioLabel label scenarioId _ ->
    Just $ labeledChoice sourceIndex label (Just $ ScenarioEntity scenarioId)
  _ -> Nothing

labeledChoice
  :: Int
  -> Text
  -> Maybe PresentationEntity
  -> ChoicePresentation
labeledChoice sourceIndex label entity =
  ChoicePresentation
    sourceIndex
    LocalizedLabel
    Nothing
    entity
    (Just $ EmbeddedI18nLabel label)
    Nothing
    Nothing

targetChoice :: Int -> ChoicePresentationKind -> PresentationEntity -> ChoicePresentation
targetChoice sourceIndex kind entity =
  ChoicePresentation
    sourceIndex
    kind
    Nothing
    (Just entity)
    Nothing
    Nothing
    Nothing

targetChoiceKind :: PresentationEntity -> [Message.Message] -> ChoicePresentationKind
targetChoiceKind entity messages
  | ActEntity {} <- entity, any isAdvanceAct messages = AdvanceAct
  | AgendaEntity {} <- entity, any isAdvanceAgenda messages = AdvanceAgenda
  | otherwise = ChooseTarget
 where
  isAdvanceAct = \case
    Message.AdvanceAct _ _ _ -> True
    _ -> False
  isAdvanceAgenda = \case
    Message.AdvanceAgenda _ -> True
    _ -> False

assignsDamageTo :: InvestigatorId -> Message.Message -> Bool
assignsDamageTo investigatorId = \case
  Message.InvestigatorAssignDamage targetId _ _ damage horror ->
    targetId == investigatorId && damage > 0 && horror == 0
  Message.InvestigatorDamage targetId _ damage horror ->
    targetId == investigatorId && damage > 0 && horror == 0
  _ -> False

assignsHorrorTo :: InvestigatorId -> Message.Message -> Bool
assignsHorrorTo investigatorId = \case
  Message.InvestigatorAssignDamage targetId _ _ damage horror ->
    targetId == investigatorId && damage == 0 && horror > 0
  Message.InvestigatorDamage targetId _ damage horror ->
    targetId == investigatorId && damage == 0 && horror > 0
  _ -> False

actionTargetChoice
  :: Int
  -> ChoicePresentationKind
  -> PresentationEntity
  -> ChoicePresentation
actionTargetChoice sourceIndex kind entity =
  ChoicePresentation
    sourceIndex
    kind
    Nothing
    (Just entity)
    Nothing
    Nothing
    Nothing

abilityChoice :: Int -> InvestigatorId -> Ability.Ability -> Maybe ChoicePresentation
abilityChoice sourceIndex investigatorId ability =
  let
    sourceEntity = entityFromSource (Ability.abilitySource ability)
    entity =
      (Ability.abilityTarget ability >>= entityFromTarget)
        <|> sourceEntity
    actions = mapMaybe actionText (abilityActions ability)
    kind = abilityChoiceKind (Ability.abilityType ability) entity actions
    abilityPresentation =
      AbilityPresentation
        (Ability.abilityCardCode ability)
        (Ability.abilityIndex ability)
        (abilityTypeText $ Ability.abilityType ability)
        actions
        (Ability.abilityCanBeCancelled ability)
    totalCost
      | Ability.abilityIgnoreAllCosts ability = Cost.Free
      | otherwise =
          abilityCost ability <> mconcat (Ability.abilityAdditionalCosts ability)
    choice =
      ChoicePresentation
        sourceIndex
        kind
        (Just investigatorId)
        entity
        Nothing
        (Just abilityPresentation)
        (Just $ presentCost totalCost)
   in case (kind, sourceEntity, entity) of
        (Move, Just source@LocationEntity {}, Just projected)
          | source == projected -> Just choice
        (Move, _, _) -> Nothing
        (ResolveForcedAbility, Just source, Just projected)
          | source == projected -> Just choice
        (ResolveForcedAbility, _, _) -> Nothing
        _ -> Just choice

abilityChoiceKind
  :: AbilityType.AbilityType
  -> Maybe PresentationEntity
  -> [Text]
  -> ChoicePresentationKind
abilityChoiceKind abilityType entity actions
  | isObjectiveAbility abilityType =
      case entity of
        Just (ActEntity _) -> AdvanceAct
        Just (AgendaEntity _) -> AdvanceAgenda
        _ -> UseAbility
  | isForcedAbility abilityType = ResolveForcedAbility
  | "investigate" `elem` actions = Investigate
  | "fight" `elem` actions = Fight
  | "evade" `elem` actions = Evade
  | "engage" `elem` actions = Engage
  | "move" `elem` actions = Move
  | otherwise = UseAbility

isObjectiveAbility :: AbilityType.AbilityType -> Bool
isObjectiveAbility = \case
  AbilityType.Objective _ -> True
  AbilityType.DelayedAbility abilityType -> isObjectiveAbility abilityType
  AbilityType.ForcedWhen _ abilityType -> isObjectiveAbility abilityType
  _ -> False

isForcedAbility :: AbilityType.AbilityType -> Bool
isForcedAbility = \case
  AbilityType.ForcedAbility {} -> True
  _ -> False

abilityTypeText :: AbilityType.AbilityType -> Text
abilityTypeText = \case
  AbilityType.ActionAbility {} -> "action"
  AbilityType.FastAbility' {} -> "fast"
  AbilityType.ReactionAbility {} -> "reaction"
  AbilityType.CustomizationReaction {} -> "reaction"
  AbilityType.ConstantReaction {} -> "reaction"
  AbilityType.ForcedAbility {} -> "forced"
  AbilityType.SilentForcedAbility {} -> "forced"
  AbilityType.ForcedAbilityWithCost {} -> "forced"
  AbilityType.ForcedWhen {} -> "forced"
  AbilityType.Objective {} -> "objective"
  AbilityType.DelayedAbility {} -> "delayed"
  AbilityType.AbilityEffect {} -> "effect"
  _ -> "other"

actionText :: Action.Action -> Maybe Text
actionText = \case
  Action.Activate -> Just "activate"
  Action.Draw -> Just "draw"
  Action.Engage -> Just "engage"
  Action.Evade -> Just "evade"
  Action.Fight -> Just "fight"
  Action.Investigate -> Just "investigate"
  Action.Move -> Just "move"
  Action.Parley -> Just "parley"
  Action.Play -> Just "play"
  Action.Resign -> Just "resign"
  Action.Resource -> Just "resource"
  Action.Explore -> Just "explore"
  Action.Circle -> Just "circle"
  Action.HomebrewAction _ -> Nothing

entityFromSource :: Source -> Maybe PresentationEntity
entityFromSource = \case
  IndexedSource _ source -> entityFromSource source
  AbilitySource source _ -> entityFromSource source
  UseAbilitySource _ source _ -> entityFromSource source
  PaymentSource source -> entityFromSource source
  ProxySource source originalSource ->
    entityFromSource source <|> entityFromSource originalSource
  ActSource entityId -> Just $ ActEntity entityId
  AgendaSource entityId -> Just $ AgendaEntity entityId
  AssetSource entityId -> Just $ AssetEntity entityId
  CardCodeSource entityId -> Just $ CardCodeEntity entityId
  CardIdSource entityId -> Just $ CardEntity entityId
  EncounterCardSource entityId -> Just $ CardEntity entityId
  EffectSource entityId -> Just $ EffectEntity entityId
  EnemyAttackSource entityId -> Just $ EnemyEntity entityId
  EnemyDefeatSource entityId -> Just $ EnemyEntity entityId
  EnemySource entityId -> Just $ EnemyEntity entityId
  EventSource entityId -> Just $ EventEntity entityId
  InvestigatorSource entityId -> Just $ InvestigatorEntity entityId
  LocationSource entityId -> Just $ LocationEntity entityId
  ResourceSource entityId -> Just $ InvestigatorEntity entityId
  SkillSource entityId -> Just $ SkillEntity entityId
  StorySource entityId -> Just $ StoryEntity entityId
  TreacherySource entityId -> Just $ TreacheryEntity entityId
  ElderSignEffectSource entityId -> Just $ InvestigatorEntity entityId
  _ -> Nothing

entityFromTarget :: Target -> Maybe PresentationEntity
entityFromTarget = \case
  AssetTarget entityId -> Just $ AssetEntity entityId
  EnemyTarget entityId -> Just $ EnemyEntity entityId
  ScenarioTarget -> Just $ ScenarioEntity "scenario"
  EffectTarget entityId -> Just $ EffectEntity entityId
  InvestigatorTarget entityId -> Just $ InvestigatorEntity entityId
  InvestigatorDiscardTarget entityId -> Just $ InvestigatorEntity entityId
  LocationTarget entityId -> Just $ LocationEntity entityId
  TreacheryTarget entityId -> Just $ TreacheryEntity entityId
  AgendaTarget entityId -> Just $ AgendaEntity entityId
  ActTarget entityId -> Just $ ActEntity entityId
  CardIdTarget entityId -> Just $ CardEntity entityId
  CardCostTarget entityId -> Just $ CardEntity entityId
  CardCodeTarget entityId -> Just $ CardCodeEntity entityId
  SearchedCardTarget entityId -> Just $ CardEntity entityId
  EventTarget entityId -> Just $ EventEntity entityId
  SkillTarget entityId -> Just $ SkillEntity entityId
  ResourceTarget entityId -> Just $ InvestigatorEntity entityId
  InvestigationTarget _ locationId -> Just $ LocationEntity locationId
  StoryTarget entityId -> Just $ StoryEntity entityId
  AbilityTarget entityId _ -> Just $ InvestigatorEntity entityId
  LabeledTarget _ target -> entityFromTarget target
  IndexedTarget _ target -> entityFromTarget target
  ProxyTarget target originalTarget ->
    entityFromTarget target <|> entityFromTarget originalTarget
  _ -> Nothing

presentCost :: Cost.Cost -> PresentationCost
presentCost = \case
  Cost.Free -> FreePresentationCost
  Cost.ActionCost amount -> ActionPresentationCost amount
  Cost.ResourceCost amount -> ResourcePresentationCost amount
  Cost.ScenarioResourceCost amount -> ResourcePresentationCost amount
  Cost.ClueCost amount -> CluePresentationCost $ presentAmount amount
  Cost.GroupClueCost amount scope ->
    GroupCluePresentationCost (presentAmount amount) (presentScope scope)
  Cost.SameLocationGroupClueCost amount scope ->
    GroupCluePresentationCost
      (presentAmount amount)
      (case scope of Location.LocationWithId locationId -> LocationScope locationId; _ -> SameLocationScope)
  Cost.GroupResourceCost amount scope ->
    GroupResourcePresentationCost (presentAmount amount) (presentScope scope)
  Cost.Costs [] -> FreePresentationCost
  Cost.Costs costs -> AllPresentationCosts $ map presentCost costs
  Cost.OrCost [] -> OtherPresentationCost
  Cost.OrCost costs -> AlternativePresentationCosts $ map presentCost costs
  _ -> OtherPresentationCost

presentAmount :: GameValue.GameValue -> PresentationAmount
presentAmount = \case
  GameValue.Static value -> FixedAmount value
  GameValue.PerPlayer value -> PerPlayerAmount value
  GameValue.StaticWithPerPlayer fixed perPlayer ->
    FixedPlusPerPlayerAmount fixed perPlayer
  GameValue.ByPlayerCount one two three four ->
    ByPlayerCountAmount one two three four
  GameValue.ValueX -> XAmount
  GameValue.ValueStar -> StarAmount
  GameValue.ValueUnknown -> UnknownAmount

presentScope :: Location.LocationMatcher -> PresentationScope
presentScope = \case
  Location.Anywhere -> AnywhereScope
  Location.SameLocation -> SameLocationScope
  Location.LocationWithId locationId -> LocationScope locationId
  _ -> OtherScope
