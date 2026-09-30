{-# LANGUAGE PatternSynonyms #-}

module Arkham.Question.Presentation (
  QuestionPresentation (..),
  pattern QuestionPresentation,
  pattern QuestionPresentationWithMetadata,
  ChoicePresentation (..),
  pattern ChoicePresentation,
  pattern ChoicePresentationWithMetadata,
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
import Arkham.Deck (DeckSignifier (EncounterDeck))
import Arkham.Draw.Types
  ( CardDraw (..)
  , CardDrawKind (StandardCardDraw)
  , CardDrawPosition (DrawFromTop)
  , CardDrawState (UnresolvedCardDraw)
  )
import Arkham.GameValue qualified as GameValue
import Arkham.Id
import Arkham.Matcher.Location qualified as Location
import Arkham.Message qualified as Message
import Arkham.Prelude
import Arkham.Question
import Arkham.Source
import Arkham.Target
import Data.Aeson qualified as Aeson
import Data.Aeson.Key qualified as AesonKey
import Data.Aeson.KeyMap qualified as AesonKeyMap
import Data.Map.Strict qualified as Map

questionPresentationProtocolVersion :: Int
questionPresentationProtocolVersion = 2

data QuestionPresentation = QuestionPresentationV2
  Int
  Text
  Int
  [ChoicePresentation]
  (Map Text Value)
  deriving stock (Show, Eq)

pattern QuestionPresentation :: Int -> Text -> Int -> [ChoicePresentation] -> QuestionPresentation
pattern QuestionPresentation version kind choiceCount choices <-
  QuestionPresentationV2 version kind choiceCount choices _
  where
    QuestionPresentation version kind choiceCount choices =
      QuestionPresentationV2 version kind choiceCount choices (defaultQuestionMetadata kind)

pattern QuestionPresentationWithMetadata :: Int -> Text -> Int -> [ChoicePresentation] -> Map Text Value -> QuestionPresentation
pattern QuestionPresentationWithMetadata version kind choiceCount choices metadata =
  QuestionPresentationV2 version kind choiceCount choices metadata

{-# COMPLETE QuestionPresentationWithMetadata #-}

data ChoicePresentation = ChoicePresentationV2
  Int
  ChoicePresentationKind
  (Maybe InvestigatorId)
  (Maybe PresentationEntity)
  (Maybe PresentationLabel)
  (Maybe AbilityPresentation)
  (Maybe PresentationCost)
  (Map Text Value)
  deriving stock (Show, Eq)

pattern ChoicePresentation
  :: Int
  -> ChoicePresentationKind
  -> Maybe InvestigatorId
  -> Maybe PresentationEntity
  -> Maybe PresentationLabel
  -> Maybe AbilityPresentation
  -> Maybe PresentationCost
  -> ChoicePresentation
pattern ChoicePresentation sourceIndex kind actor entity label ability cost <-
  ChoicePresentationV2 sourceIndex kind actor entity label ability cost _
  where
    ChoicePresentation sourceIndex kind actor entity label ability cost =
      ChoicePresentationV2 sourceIndex kind actor entity label ability cost mempty

pattern ChoicePresentationWithMetadata
  :: Int
  -> ChoicePresentationKind
  -> Maybe InvestigatorId
  -> Maybe PresentationEntity
  -> Maybe PresentationLabel
  -> Maybe AbilityPresentation
  -> Maybe PresentationCost
  -> Map Text Value
  -> ChoicePresentation
pattern ChoicePresentationWithMetadata sourceIndex kind actor entity label ability cost metadata =
  ChoicePresentationV2 sourceIndex kind actor entity label ability cost metadata

{-# COMPLETE ChoicePresentationWithMetadata #-}

data ChoicePresentationKind
  = AdvanceAct
  | AdvanceAgenda
  | ApplySkillTestResults
  | AssignDamage
  | AssignHorror
  | AutoChoice
  | CardPileChoice
  | ChaosTokenChoice
  | ChaosTokenGroupChoiceKind
  | ChooseTarget
  | ComponentChoice
  | AuxiliaryComponentChoice
  | ConnectionChoice
  | CostChoice
  | DrawCard
  | DrawEncounterCard
  | EffectActionChoice
  | EndTurn
  | Engage
  | Evade
  | Fight
  | GainResource
  | InfoChoice
  | InvalidChoice
  | Investigate
  | KeyChoice
  | LocalizedLabel
  | Move
  | OpaqueChoice
  | ResolveForcedAbility
  | SkillChoice
  | SkipTriggers
  | StartSkillTest
  | TarotChoice
  | UseAbility
  | WizardChoiceKind
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
  toJSON (QuestionPresentationV2 version kind choiceCount choices metadata) =
    jsonObject
      $ [ ("protocolVersion", Aeson.toJSON questionPresentationProtocolVersion)
        , ("questionVersion", Aeson.toJSON version)
        , ("questionKind", Aeson.toJSON kind)
        , ("choiceCount", Aeson.toJSON choiceCount)
        , ("choices", Aeson.toJSON choices)
        ]
      <> Map.toList metadata

instance ToJSON ChoicePresentation where
  toJSON (ChoicePresentationV2 sourceIndex kind actor entity label ability cost metadata) =
    jsonObject
      $ [ ("sourceIndex", Aeson.toJSON sourceIndex)
        , ("kind", Aeson.toJSON $ choiceKindText kind)
        , ("selectable", Aeson.toJSON $ choiceSelectable kind)
        ]
      <> [("actorId", Aeson.toJSON value) | Just value <- [actor]]
      <> [("entity", Aeson.toJSON value) | Just value <- [entity]]
      <> [("label", Aeson.toJSON value) | Just value <- [label]]
      <> [("ability", Aeson.toJSON value) | Just value <- [ability]]
      <> [("cost", Aeson.toJSON value) | Just value <- [cost]]
      <> Map.toList metadata

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

jsonObject :: [(Text, Value)] -> Value
jsonObject fields =
  Aeson.Object
    $ AesonKeyMap.fromList
    $ map (\(key, value) -> (AesonKey.fromText key, value)) fields

choiceKindText :: ChoicePresentationKind -> Text
choiceKindText = \case
  AdvanceAct -> "advanceAct"
  AdvanceAgenda -> "advanceAgenda"
  ApplySkillTestResults -> "applySkillTestResults"
  AssignDamage -> "assignDamage"
  AssignHorror -> "assignHorror"
  AutoChoice -> "auto"
  CardPileChoice -> "cardPile"
  ChaosTokenChoice -> "chaosTokenLabel"
  ChaosTokenGroupChoiceKind -> "chaosTokenGroupChoice"
  ChooseTarget -> "chooseTarget"
  ComponentChoice -> "componentLabel"
  AuxiliaryComponentChoice -> "auxiliaryComponentLabel"
  ConnectionChoice -> "connectionLabel"
  CostChoice -> "costLabel"
  DrawCard -> "drawCard"
  DrawEncounterCard -> "drawEncounterCard"
  EffectActionChoice -> "effectActionButton"
  EndTurn -> "endTurn"
  Engage -> "engage"
  Evade -> "evade"
  Fight -> "fight"
  GainResource -> "gainResource"
  InfoChoice -> "info"
  InvalidChoice -> "invalidLabel"
  Investigate -> "investigate"
  KeyChoice -> "keyLabel"
  LocalizedLabel -> "localizedLabel"
  Move -> "move"
  OpaqueChoice -> "opaque"
  ResolveForcedAbility -> "resolveForcedAbility"
  SkillChoice -> "skillLabel"
  SkipTriggers -> "skipTriggers"
  StartSkillTest -> "startSkillTest"
  TarotChoice -> "tarotLabel"
  UseAbility -> "useAbility"
  WizardChoiceKind -> "wizardChoice"

choiceSelectable :: ChoicePresentationKind -> Bool
choiceSelectable = \case
  InvalidChoice -> False
  InfoChoice -> False
  _ -> True

data ChoiceContext = PlayerWindowContext | GeneralChoiceContext
  deriving stock Eq

defaultQuestionMetadata :: Text -> Map Text Value
defaultQuestionMetadata kind = Map.singleton "answer" $ answerEnvelopeFor kind

answerEnvelopeFor :: Text -> Value
answerEnvelopeFor = \case
  "chooseAmounts" -> answer "amounts" "AmountsAnswer"
  "choosePaymentAmounts" -> answer "paymentAmounts" "PaymentAmountsAnswer"
  "chooseExchangeAmounts" -> answer "exchangeAmounts" "ExchangeAmountsAnswer"
  "chooseDeck" -> deckAnswer
  "chooseUpgradeDeck" -> deckAnswer
  "chooseJoinDeck" -> deckAnswer
  "pickScenarioSettings" -> answer "standaloneSettings" "StandaloneSettingsAnswer"
  "pickCampaignSettings" -> answer "campaignSettings" "CampaignSettingsAnswer"
  "pickDestiny" -> answer "pickDestiny" "PickDestinyAnswer"
  "pickCampaignSpecific" -> answer "campaignSpecific" "CampaignSpecificAnswer"
  "pickScenarioSpecific" -> answer "scenarioSpecific" "ScenarioSpecificAnswer"
  "continueCampaign" ->
    jsonObject
      [ ("kind", String "continueCampaign")
      , ( "tags"
        , Aeson.toJSON
            [ "CampaignStepAnswer" :: Text
            , "RetireInvestigatorAnswer"
            , "RejoinInvestigatorAnswer"
            , "ApplyOverlayAnswer"
            , "JoinCampaignAnswer"
            ]
        )
      ]
  "unsupported" -> jsonObject [("kind", String "unsupported")]
  "chooseOneAtATime" -> orderedCapableAnswer
  "chooseOneAtATimeWithAuto" -> orderedCapableAnswer
  _ -> answer "singleChoice" "Answer"
 where
  answer kind tag = jsonObject [("kind", String kind), ("tag", String tag)]
  deckAnswer =
    jsonObject
      [ ("kind", String "deck")
      , ("tags", Aeson.toJSON ["DeckAnswer" :: Text, "DeckListAnswer"])
      ]
  orderedCapableAnswer =
    jsonObject
      [ ("kind", String "singleChoice")
      , ("tag", String "Answer")
      , ("alternateTags", Aeson.toJSON ["OrderedAnswer" :: Text])
      ]

questionPresentation :: Int -> Question Message.Message -> QuestionPresentation
questionPresentation version question
  | Just investigatorId <- encounterDeckDrawInvestigator question =
      QuestionPresentationWithMetadata
        version
        "chooseOne"
        1
        [ ChoicePresentation
            0
            DrawEncounterCard
            (Just investigatorId)
            Nothing
            Nothing
            Nothing
            Nothing
        ]
        (defaultQuestionMetadata "chooseOne")
  | otherwise =
      let QuestionPresentationWithMetadata _ kind choiceCount choices metadata = presentQuestion version question
       in QuestionPresentationWithMetadata version kind choiceCount choices metadata

questionPresentations
  :: Int
  -> Map PlayerId (Question Message.Message)
  -> Map PlayerId QuestionPresentation
questionPresentations version = Map.map (questionPresentation version)

presentQuestion :: Int -> Question Message.Message -> QuestionPresentation
presentQuestion version = \case
  ChooseOne choices -> choiceQuestion version "chooseOne" GeneralChoiceContext choices Null
  PlayerWindowChooseOne choices -> choiceQuestion version "playerWindowChooseOne" PlayerWindowContext choices Null
  WindowChooseOne choices -> choiceQuestion version "windowChooseOne" GeneralChoiceContext choices Null
  ChooseOneFromEach groups ->
    let choices = groupedPresentations groups
        metadata =
          Map.fromList
            [ ("selection", selection 1 1)
            , ("groups", Aeson.toJSON $ map length groups)
            ]
     in QuestionPresentationWithMetadata
          version
          "chooseOneFromEach"
          (sum $ map length groups)
          choices
          (defaultQuestionMetadata "chooseOneFromEach" <> metadata)
  ChooseN amount choices ->
    choiceQuestion version "chooseN" GeneralChoiceContext choices $ selection amount amount
  ChooseSome choices ->
    choiceQuestion version "chooseSome" GeneralChoiceContext choices $ selection 0 (length choices)
  ChooseSome1 label choices ->
    choiceQuestion version "chooseSome1" GeneralChoiceContext choices (selection 1 $ length choices)
      & addQuestionField "completionLabel" (Aeson.toJSON $ EmbeddedI18nLabel label)
  ChooseUpToN amount choices ->
    choiceQuestion version "chooseUpToN" GeneralChoiceContext choices $ selection 0 amount
  ChooseOneAtATime choices ->
    choiceQuestion version "chooseOneAtATime" GeneralChoiceContext choices $ selection 1 1
  ChooseOneAtATimeWithAuto label choices ->
    QuestionPresentationWithMetadata
      version
      "chooseOneAtATimeWithAuto"
      (length choices + 1)
      (autoChoice label : zipWith (presentChoice GeneralChoiceContext) [1 ..] choices)
      (defaultQuestionMetadata "chooseOneAtATimeWithAuto" <> Map.singleton "selection" (selection 1 1))
  ChoosePaymentAmounts label target choices ->
    amountQuestion
      version
      "choosePaymentAmounts"
      [ ("label", Aeson.toJSON $ EmbeddedI18nLabel label)
      , ("target", Aeson.toJSON target)
      , ("paymentChoices", Aeson.toJSON $ map paymentAmountChoice choices)
      ]
  ChooseAmounts label target choices target' ->
    amountQuestion
      version
      "chooseAmounts"
      [ ("label", Aeson.toJSON $ EmbeddedI18nLabel label)
      , ("target", Aeson.toJSON target)
      , ("resolveTarget", Aeson.toJSON target')
      , ("amountChoices", Aeson.toJSON choices)
      ]
  ChooseUpgradeDeck -> deckQuestion version "chooseUpgradeDeck" mempty
  ChooseDeck -> deckQuestion version "chooseDeck" mempty
  ChooseJoinDeck usedInvestigators ->
    deckQuestion version "chooseJoinDeck" $ Map.singleton "usedInvestigators" (Aeson.toJSON usedInvestigators)
  QuestionLabel label card question ->
    presentQuestion version question
      & addQuestionField "questionLabel" (Aeson.toJSON $ EmbeddedI18nLabel label)
      & maybe id (addQuestionField "cardCode" . Aeson.toJSON) card
  PayCostQuestion cost question ->
    presentQuestion version question
      & addQuestionField "payCost" (Aeson.toJSON $ presentCost cost)
  QuestionWithSource source tooltip question ->
    presentQuestion version question
      & addQuestionField "questionSource" (sourceMetadata source)
      & maybe id (addQuestionField "tooltip" . Aeson.toJSON) tooltip
  Read flavor readChoices readCards ->
    let (choices, metadata) = readChoicePresentation readChoices
     in QuestionPresentationWithMetadata
          version
          "read"
          (length choices)
          (zipWith (presentChoice GeneralChoiceContext) [0 ..] choices)
          ( defaultQuestionMetadata "read"
              <> metadata
              <> Map.fromList
                ( [ ("flavorText", Aeson.toJSON flavor) ]
                    <> [("readCards", Aeson.toJSON cards) | Just cards <- [readCards]]
                )
          )
  ChooseOneWizard flavor choices confirmLabel backLabel ->
    QuestionPresentationWithMetadata
      version
      "chooseOneWizard"
      (length choices)
      (zipWith wizardChoicePresentation [0 ..] choices)
      ( defaultQuestionMetadata "chooseOneWizard"
          <> Map.fromList
            [ ("flavorText", Aeson.toJSON flavor)
            , ("confirmLabel", Aeson.toJSON $ EmbeddedI18nLabel confirmLabel)
            , ("backLabel", Aeson.toJSON $ EmbeddedI18nLabel backLabel)
            ]
      )
  PickSupplies pointsRemaining chosenSupplies choices resupply ->
    choiceQuestion version "pickSupplies" GeneralChoiceContext choices Null
      & addQuestionField "pointsRemaining" (Aeson.toJSON pointsRemaining)
      & addQuestionField "chosenSupplies" (Aeson.toJSON chosenSupplies)
      & addQuestionField "resupply" (Aeson.toJSON resupply)
  PickDestiny drawings ->
    noChoiceQuestion version "pickDestiny" $ Map.singleton "drawings" (Aeson.toJSON drawings)
  DropDown options ->
    QuestionPresentationWithMetadata
      version
      "dropDown"
      (length options)
      (zipWith dropDownChoice [0 ..] options)
      (defaultQuestionMetadata "dropDown")
  PickScenarioSettings -> noChoiceQuestion version "pickScenarioSettings" mempty
  PickCampaignSettings -> noChoiceQuestion version "pickCampaignSettings" mempty
  PickCampaignSpecific key value ->
    noChoiceQuestion version "pickCampaignSpecific" $ Map.fromList [("key", Aeson.toJSON key), ("value", value)]
  PickScenarioSpecific key value ->
    noChoiceQuestion version "pickScenarioSpecific" $ Map.fromList [("key", Aeson.toJSON key), ("value", value)]
  ChooseExchangeAmounts source investigator1 initial1 investigator2 initial2 token ->
    noChoiceQuestion version "chooseExchangeAmounts"
      $ Map.fromList
        [ ("source", sourceMetadata source)
        , ("fromInvestigator", Aeson.toJSON investigator1)
        , ("fromInitialAmount", Aeson.toJSON initial1)
        , ("toInvestigator", Aeson.toJSON investigator2)
        , ("toInitialAmount", Aeson.toJSON initial2)
        , ("token", Aeson.toJSON token)
        ]
  ContinueCampaign -> noChoiceQuestion version "continueCampaign" mempty

choiceQuestion
  :: Int
  -> Text
  -> ChoiceContext
  -> [UI Message.Message]
  -> Value
  -> QuestionPresentation
choiceQuestion version kind context choices selectionMetadata =
  let metadata =
        defaultQuestionMetadata kind
          <> if selectionMetadata == Null then mempty else Map.singleton "selection" selectionMetadata
   in QuestionPresentationWithMetadata
        version
        kind
        (length choices)
        (zipWith (presentChoice context) [0 ..] choices)
        metadata

noChoiceQuestion :: Int -> Text -> Map Text Value -> QuestionPresentation
noChoiceQuestion version kind metadata =
  QuestionPresentationWithMetadata version kind 0 [] (defaultQuestionMetadata kind <> metadata)

deckQuestion :: Int -> Text -> Map Text Value -> QuestionPresentation
deckQuestion = noChoiceQuestion

amountQuestion :: Int -> Text -> [(Text, Value)] -> QuestionPresentation
amountQuestion version kind fields =
  noChoiceQuestion version kind (Map.fromList fields)

selection :: Int -> Int -> Value
selection minCount maxCount =
  jsonObject [("min", Aeson.toJSON minCount), ("max", Aeson.toJSON maxCount)]

addQuestionField :: Text -> Value -> QuestionPresentation -> QuestionPresentation
addQuestionField key value (QuestionPresentationWithMetadata version kind choiceCount choices metadata) =
  QuestionPresentationWithMetadata version kind choiceCount choices (Map.insert key value metadata)

autoChoice :: Text -> ChoicePresentation
autoChoice label =
  ChoicePresentation 0 AutoChoice Nothing Nothing (Just $ EmbeddedI18nLabel label) Nothing Nothing

groupedPresentations :: [[UI Message.Message]] -> [ChoicePresentation]
groupedPresentations groups = go 0 0 groups
 where
  go _ _ [] = []
  go sourceIndex groupIndex (group : rest) =
    let rendered =
          [ addChoiceField "groupIndex" (Aeson.toJSON groupIndex) $ presentChoice GeneralChoiceContext idx choice
          | (idx, choice) <- zip [sourceIndex ..] group
          ]
     in rendered <> go (sourceIndex + length group) (groupIndex + 1) rest

readChoicePresentation :: ReadChoices Message.Message -> ([UI Message.Message], Map Text Value)
readChoicePresentation = \case
  BasicReadChoices choices -> (choices, Map.singleton "readChoiceKind" (String "basic"))
  BasicReadChoicesN amount choices ->
    ( choices
    , Map.fromList
        [ ("readChoiceKind", String "chooseN")
        , ("selection", selection amount amount)
        ]
    )
  BasicReadChoicesUpToN amount choices ->
    ( choices
    , Map.fromList
        [ ("readChoiceKind", String "chooseUpToN")
        , ("selection", selection 0 amount)
        ]
    )
  LeadInvestigatorMustDecide choices ->
    (choices, Map.singleton "readChoiceKind" (String "leadInvestigatorMustDecide"))

paymentAmountChoice :: PaymentAmountChoice Message.Message -> Value
paymentAmountChoice PaymentAmountChoice {..} =
  jsonObject
    [ ("choiceId", Aeson.toJSON choiceId)
    , ("investigatorId", Aeson.toJSON investigatorId)
    , ("min", Aeson.toJSON minBound)
    , ("max", Aeson.toJSON maxBound)
    , ("title", Aeson.toJSON $ EmbeddedI18nLabel title)
    ]

wizardChoicePresentation :: Int -> WizardChoice Message.Message -> ChoicePresentation
wizardChoicePresentation sourceIndex WizardChoice {..} =
  ChoicePresentationWithMetadata
    sourceIndex
    WizardChoiceKind
    Nothing
    Nothing
    (Just $ EmbeddedI18nLabel label)
    Nothing
    Nothing
    (Map.singleton "flavorText" $ Aeson.toJSON flavorText)

dropDownChoice :: Int -> (Text, Message.Message) -> ChoicePresentation
dropDownChoice sourceIndex (label, _) = labeledChoice sourceIndex label Nothing

presentChoice :: ChoiceContext -> Int -> UI Message.Message -> ChoicePresentation
presentChoice context sourceIndex choice = case choice of
  Label label _ -> labeledChoice sourceIndex label Nothing
  InvalidLabel label ->
    ChoicePresentation sourceIndex InvalidChoice Nothing Nothing (Just $ EmbeddedI18nLabel label) Nothing Nothing
  TooltipLabel label tooltip _ ->
    addChoiceField "tooltip" (Aeson.toJSON tooltip) $ labeledChoice sourceIndex label Nothing
  CostLabel cost _ ->
    ChoicePresentation sourceIndex CostChoice Nothing Nothing Nothing Nothing (Just $ presentCost cost)
  CardLabel cardCode flippable _ ->
    addChoiceField "flippable" (Aeson.toJSON flippable)
      $ targetChoice sourceIndex ChooseTarget (CardCodeEntity cardCode)
  ChaosTokenLabel face _ ->
    ChoicePresentationWithMetadata
      sourceIndex
      ChaosTokenChoice
      Nothing
      Nothing
      Nothing
      Nothing
      Nothing
      (Map.singleton "face" $ Aeson.toJSON face)
  KeyLabel key _ ->
    ChoicePresentationWithMetadata
      sourceIndex
      KeyChoice
      Nothing
      Nothing
      Nothing
      Nothing
      Nothing
      (Map.singleton "key" $ Aeson.toJSON key)
  PortraitLabel investigatorId _ ->
    targetChoice sourceIndex ChooseTarget (InvestigatorEntity investigatorId)
  TargetLabel target messages ->
    case entityFromTarget target of
      Just entity -> targetChoice sourceIndex (targetChoiceKind entity messages) entity
      Nothing -> opaqueChoice sourceIndex "TargetLabel" Nothing $ Map.singleton "target" (Aeson.toJSON target)
  EvadeLabel enemyId _ ->
    actionTargetChoice sourceIndex Evade (EnemyEntity enemyId)
  EvadeLabelWithSkill enemyId skillType _ ->
    addChoiceField "skillType" (Aeson.toJSON skillType)
      $ actionTargetChoice sourceIndex Evade (EnemyEntity enemyId)
  FightLabel enemyId _ ->
    actionTargetChoice sourceIndex Fight (EnemyEntity enemyId)
  FightLabelWithSkill enemyId skillType _ ->
    addChoiceField "skillType" (Aeson.toJSON skillType)
      $ actionTargetChoice sourceIndex Fight (EnemyEntity enemyId)
  EngageLabel enemyId _ ->
    actionTargetChoice sourceIndex Engage (EnemyEntity enemyId)
  GridLabel label _ -> labeledChoice sourceIndex label Nothing
  ConnectionLabel connection _ ->
    ChoicePresentationWithMetadata
      sourceIndex
      ConnectionChoice
      Nothing
      Nothing
      Nothing
      Nothing
      Nothing
      (Map.singleton "connection" $ Aeson.toJSON connection)
  TarotLabel tarotCard _ ->
    ChoicePresentationWithMetadata
      sourceIndex
      TarotChoice
      Nothing
      Nothing
      Nothing
      Nothing
      Nothing
      (Map.singleton "tarotCard" $ Aeson.toJSON tarotCard)
  AbilityLabel investigatorId ability _ _ _ ->
    abilityChoice sourceIndex investigatorId ability
  ComponentLabel component messages ->
    case component of
      InvestigatorComponent investigatorId DamageToken
        | any (assignsDamageTo investigatorId) messages ->
            targetChoice sourceIndex AssignDamage (InvestigatorEntity investigatorId)
      InvestigatorComponent investigatorId HorrorToken
        | any (assignsHorrorTo investigatorId) messages ->
            targetChoice sourceIndex AssignHorror (InvestigatorEntity investigatorId)
      InvestigatorComponent investigatorId ResourceToken
        | context == PlayerWindowContext ->
            ChoicePresentation
              sourceIndex
              GainResource
              (Just investigatorId)
              Nothing
              Nothing
              Nothing
              Nothing
      InvestigatorDeckComponent investigatorId
        | context == PlayerWindowContext ->
            ChoicePresentation
              sourceIndex
              DrawCard
              (Just investigatorId)
              Nothing
              Nothing
              Nothing
              Nothing
      _ -> componentChoice sourceIndex ComponentChoice component
  AuxiliaryComponentLabel component _ ->
    componentChoice sourceIndex AuxiliaryComponentChoice component
  EndTurnButton investigatorId _ ->
    ChoicePresentation
      sourceIndex
      EndTurn
      (Just investigatorId)
      Nothing
      Nothing
      Nothing
      Nothing
  StartSkillTestButton investigatorId ->
    ChoicePresentation
      sourceIndex
      StartSkillTest
      (Just investigatorId)
      Nothing
      Nothing
      Nothing
      Nothing
  SkillTestApplyResultsButton ->
    ChoicePresentation
      sourceIndex
      ApplySkillTestResults
      Nothing
      Nothing
      Nothing
      Nothing
      Nothing
  ChaosTokenGroupChoice source investigatorId step ->
    ChoicePresentationWithMetadata
      sourceIndex
      ChaosTokenGroupChoiceKind
      (Just investigatorId)
      (entityFromSource source)
      Nothing
      Nothing
      Nothing
      ( Map.fromList
          [ ("source", sourceMetadata source)
          , ("step", Aeson.toJSON step)
          ]
      )
  EffectActionButton tooltip effectId _ ->
    ChoicePresentationWithMetadata
      sourceIndex
      EffectActionChoice
      Nothing
      (Just $ EffectEntity effectId)
      Nothing
      Nothing
      Nothing
      (Map.singleton "tooltip" $ Aeson.toJSON tooltip)
  Done label -> labeledChoice sourceIndex label Nothing
  SkipTriggersButton investigatorId ->
    ChoicePresentation
      sourceIndex
      SkipTriggers
      (Just investigatorId)
      Nothing
      Nothing
      Nothing
      Nothing
  CardPile pile _ ->
    ChoicePresentationWithMetadata
      sourceIndex
      CardPileChoice
      Nothing
      Nothing
      Nothing
      Nothing
      Nothing
      (Map.singleton "cards" $ Aeson.toJSON pile)
  Info flavor ->
    ChoicePresentationWithMetadata
      sourceIndex
      InfoChoice
      Nothing
      Nothing
      Nothing
      Nothing
      Nothing
      (Map.singleton "flavorText" $ Aeson.toJSON flavor)
  SkillLabel skillType _ ->
    ChoicePresentationWithMetadata
      sourceIndex
      SkillChoice
      Nothing
      Nothing
      Nothing
      Nothing
      Nothing
      (Map.singleton "skillType" $ Aeson.toJSON skillType)
  SkillLabelWithLabel label skillType _ ->
    ChoicePresentationWithMetadata
      sourceIndex
      SkillChoice
      Nothing
      Nothing
      (Just $ EmbeddedI18nLabel label)
      Nothing
      Nothing
      (Map.singleton "skillType" $ Aeson.toJSON skillType)
  ScenarioLabel label scenarioId _ ->
    labeledChoice sourceIndex label (Just $ ScenarioEntity scenarioId)
 where
  opaqueChoice sourceIndex tag mLabel metadata =
    ChoicePresentationWithMetadata
      sourceIndex
      OpaqueChoice
      Nothing
      Nothing
      (Just $ EmbeddedI18nLabel $ fromMaybe "$choice.opaque" mLabel)
      Nothing
      Nothing
      (Map.insert "uiTag" (Aeson.toJSON tag) metadata)

componentChoice :: Int -> ChoicePresentationKind -> Component -> ChoicePresentation
componentChoice sourceIndex kind component =
  ChoicePresentationWithMetadata
    sourceIndex
    kind
    Nothing
    (entityFromComponent component)
    Nothing
    Nothing
    Nothing
    (Map.singleton "component" $ Aeson.toJSON component)

entityFromComponent :: Component -> Maybe PresentationEntity
entityFromComponent = \case
  InvestigatorComponent investigatorId _ -> Just $ InvestigatorEntity investigatorId
  InvestigatorDeckComponent investigatorId -> Just $ InvestigatorEntity investigatorId
  AssetComponent assetId _ -> Just $ AssetEntity assetId

addChoiceField :: Text -> Value -> ChoicePresentation -> ChoicePresentation
addChoiceField key value (ChoicePresentationWithMetadata sourceIndex kind actor entity label ability cost metadata) =
  ChoicePresentationWithMetadata sourceIndex kind actor entity label ability cost (Map.insert key value metadata)

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

encounterDeckDrawInvestigator :: Question Message.Message -> Maybe InvestigatorId
encounterDeckDrawInvestigator
  ( ChooseOne
      [ TargetLabel
          EncounterDeckTarget
          [ Message.DrawCards
              investigatorId
              CardDraw
                { cardDrawSource = GameSource
                , cardDrawDeck = EncounterDeck
                , cardDrawAmount = 1
                , cardDrawState = UnresolvedCardDraw
                , cardDrawTarget = Nothing
                , cardDrawAction = False
                , cardDrawKind = StandardCardDraw
                , cardDrawPosition = DrawFromTop
                , cardDrawRules = rules
                , cardDrawAndThen = Nothing
                , cardDrawAlreadyDrawn = []
                , cardDrawDiscard = Nothing
                }
            ]
        ]
  )
    | null rules = Just investigatorId
encounterDeckDrawInvestigator _ = Nothing

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

abilityChoice :: Int -> InvestigatorId -> Ability.Ability -> ChoicePresentation
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
    choiceFor choiceKind =
      ChoicePresentation
        sourceIndex
        choiceKind
        (Just investigatorId)
        entity
        Nothing
        (Just abilityPresentation)
        (Just $ presentCost totalCost)
   in case (kind, sourceEntity, entity) of
        (Move, Just source@LocationEntity {}, Just projected)
          | source == projected -> choiceFor Move
        (Move, Just LocationEntity {}, _) -> choiceFor UseAbility
        (Move, Nothing, _) -> choiceFor UseAbility
        (Move, Just _, _) -> choiceFor UseAbility
        (ResolveForcedAbility, Just source, Just projected)
          | isSupportedForcedAbilityEntity source
          , source == projected ->
              choiceFor ResolveForcedAbility
        (ResolveForcedAbility, _, _) -> choiceFor UseAbility
        _ -> choiceFor kind

isSupportedForcedAbilityEntity :: PresentationEntity -> Bool
isSupportedForcedAbilityEntity = \case
  LocationEntity {} -> True
  TreacheryEntity {} -> True
  _ -> False

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

sourceMetadata :: Source -> Value
sourceMetadata source =
  jsonObject
    $ [("raw", Aeson.toJSON source)]
    <> [("entity", Aeson.toJSON entity) | Just entity <- [entityFromSource source]]

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
