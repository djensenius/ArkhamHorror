#!/usr/bin/env python3

"""Tests for the backend emitted-key registry extractor.

Two kinds of check live here. The first feeds synthetic Haskell modules to the
real extractor, one construct at a time, so every resolution rule (aliases and
their re-exports, scope templates, call-site propagation, `withVars`, amount
labels, presentation modifiers) has a case that fails without it. The second
asserts properties of the committed registry itself: the keys the review cited
by name resolve, no unparsed module hides an emitter, and every unresolved site
carries a classified reason.

No prose from the game is used; every fixture string is synthetic.
"""

from __future__ import annotations

import json
import sys
import tempfile
from pathlib import Path

import extract_backend_i18n_keys as extractor

ROOT = Path(__file__).resolve().parents[1]
ARTIFACT = ROOT / "backend" / "arkham-api" / "i18n-emitted-keys.json"

FAILURES: list[str] = []


def check(condition: bool, message: str) -> None:
    if not condition:
        FAILURES.append(message)


def registry_of(modules: dict[str, str], icon_tags: set[str] | None = None) -> dict:
    """Runs the production extractor over a synthetic module set."""
    with tempfile.TemporaryDirectory() as directory:
        library = Path(directory)
        for name, text in modules.items():
            path = library / name
            path.parent.mkdir(parents=True, exist_ok=True)
            path.write_text(text, encoding="utf-8")
        return extractor.build_artifact(library=library, icon_tags=icon_tags)


def keys_of(modules: dict[str, str]) -> set[str]:
    return {entry["key"] for entry in registry_of(modules)["keys"]}


def variables_of(modules: dict[str, str], key: str) -> set[str]:
    return set(variable_types_of(modules, key))


def variable_types_of(
    modules: dict[str, str], key: str, icon_tags: set[str] | None = None
) -> dict[str, str]:
    for entry in registry_of(modules, icon_tags)["keys"]:
        if entry["key"] == key:
            return {variable["name"]: variable["type"] for variable in entry["variables"]}
    return {}


HELPERS = """module Test.Helpers where

import Arkham.I18n

campaignI18n :: (HasI18n => a) -> a
campaignI18n = standaloneI18n "testCampaign"
"""


CHAOS_TOKEN_TYPES = """module Arkham.ChaosToken.Types where

data ChaosTokenFace
  = PlusOne
  | Zero
  | Skull
  | ElderThing
  | BlessToken
  | CustomToken Text

instance ToDisplay ChaosTokenFace where
  toDisplay = \\case
    PlusOne -> "+1"
    Zero -> "0"
    Skull -> "{skull}"
    ElderThing -> "{elderThing}"
    BlessToken -> "{bless}"
    CustomToken slug -> "{" <> customTokenKey slug <> "}"

allChaosTokenFaces :: [ChaosTokenFace]
allChaosTokenFaces =
  [ PlusOne
  , Zero
  , Skull
  , ElderThing
  , BlessToken
  ]

customTokenKey :: Text -> Text
customTokenKey slug = slug

capitalizeFirst :: Text -> Text
capitalizeFirst slug = slug

chaosTokenLabel :: ChaosTokenFace -> Text
chaosTokenLabel = \\case
  PlusOne -> "+1"
  Zero -> "0"
  Skull -> "Skull"
  ElderThing -> "Elder Thing"
  BlessToken -> "Bless"
  CustomToken slug -> capitalizeFirst (customTokenKey slug)
"""


KEY_I18N = """module Arkham.Key where

import Arkham.ChaosToken.Types

data ArkhamKey
  = TokenKey ChaosToken
  | RedKey
  | BlueKey
  | GreenKey
  | YellowKey
  | PurpleKey
  | BlackKey
  | WhiteKey
  | UnrevealedKey ArkhamKey

keyName :: ArkhamKey -> Text
keyName = \\case
  TokenKey token -> chaosTokenLabel token.face
  RedKey -> "Red"
  BlueKey -> "Blue"
  GreenKey -> "Green"
  YellowKey -> "Yellow"
  PurpleKey -> "Purple"
  BlackKey -> "Black"
  WhiteKey -> "White"
  UnrevealedKey _ -> "Unrevealed"
"""


SKILL_I18N = """module Arkham.I18n where

skillVar :: HasI18n => SkillType -> (HasI18n => a) -> a
skillVar v a = case v of
  SkillWillpower -> withVar "skill" (String "willpower") a
  SkillIntellect -> withVar "skill" (String "intellect") a
  SkillCombat -> withVar "skill" (String "combat") a
  SkillAgility -> withVar "skill" (String "agility") a

skillIconVar :: HasI18n => SkillIcon -> (HasI18n => a) -> a
skillIconVar v a = case v of
  SkillIcon kind -> case kind of
    SkillWillpower -> withVar "skillIcon" (String "willpower") a
    SkillIntellect -> withVar "skillIcon" (String "intellect") a
    SkillCombat -> withVar "skillIcon" (String "combat") a
    SkillAgility -> withVar "skillIcon" (String "agility") a
  WildIcon -> withVar "skillIcon" (String "wild") a
  WildMinusIcon -> withVar "skillIcon" (String "wildMinus") a

toScope :: Text -> Scope
toScope t = case T.words t of
  [] -> ""
  (x : xs) -> lowerFirst x <> mconcat (map capitalizeFirst xs)
 where
  lowerFirst txt = case T.uncons txt of
    Just (c, r) -> T.cons (Char.toLower c) r
    Nothing -> txt
  capitalizeFirst txt = case T.uncons txt of
    Just (c, r) -> T.cons (Char.toUpper c) r
    Nothing -> txt
"""

PRELUDE_I18N = """module Arkham.Prelude (module X) where

import ClassyPrelude as X
"""

ASPECT_I18N = """module Arkham.Aspect where

skillTypeKey :: SkillType -> Text
skillTypeKey = \\case
  SkillWillpower -> "willpower"
  SkillIntellect -> "intellect"
  SkillCombat -> "combat"
  SkillAgility -> "agility"
"""

SEAL_I18N = """module Arkham.Campaigns.EdgeOfTheEarth.Seal where

data SealKind = SealA | SealB | SealC | SealD | SealE
  deriving stock (Show, Eq, Ord, Bounded, Enum, Generic, Data)
"""

SKILL_TYPE_I18N = """module Arkham.SkillType where

data SkillIcon = SkillIcon SkillType | WildIcon | WildMinusIcon

instance IsLabel "willpower" SkillIcon where
  fromLabel = SkillIcon SkillWillpower

instance IsLabel "intellect" SkillIcon where
  fromLabel = SkillIcon SkillIntellect

instance IsLabel "combat" SkillIcon where
  fromLabel = SkillIcon SkillCombat

instance IsLabel "agility" SkillIcon where
  fromLabel = SkillIcon SkillAgility

instance IsLabel "wild" SkillIcon where
  fromLabel = WildIcon

instance IsLabel "wildMinus" SkillIcon where
  fromLabel = WildMinusIcon
"""


def test_point_free_alias_through_a_direct_import() -> None:
    keys = keys_of(
        {
            "Test/Helpers.hs": HELPERS,
            "Test/Card.hs": """module Test.Card where

import Test.Helpers

run = campaignI18n $ labeled' "card.doTheThing" $ pure ()
""",
        }
    )
    check(
        "standalone.testCampaign.label.card.doTheThing" in keys,
        f"point-free alias not resolved: {sorted(keys)}",
    )


def test_alias_reached_through_an_aliased_module_reexport() -> None:
    keys = keys_of(
        {
            "Test/Helpers.hs": HELPERS,
            "Test/Hub.hs": """module Test.Hub (module X) where

import Test.Helpers as X
""",
            "Test/Card.hs": """module Test.Card where

import Test.Hub

run = campaignI18n $ labeled' "card.viaHub" $ pure ()
""",
        }
    )
    check(
        "standalone.testCampaign.label.card.viaHub" in keys,
        f"`module X` re-export not followed: {sorted(keys)}",
    )


def test_a_restricted_reexport_does_not_create_a_rival_alias() -> None:
    # `import Other as X (helper)` re-exports only `helper`; treating it as a
    # second `campaignI18n` made every key in the importer ambiguous.
    keys = keys_of(
        {
            "Test/Helpers.hs": HELPERS,
            "Test/Other.hs": """module Test.Other where

import Arkham.I18n

campaignI18n :: (HasI18n => a) -> a
campaignI18n = standaloneI18n "otherCampaign"

helper :: Int
helper = 1
""",
            "Test/Hub.hs": """module Test.Hub (module H, module X) where

import Test.Helpers as H
import Test.Other as X (helper)
""",
            "Test/Card.hs": """module Test.Card where

import Test.Hub

run = campaignI18n $ labeled' "card.restricted" $ pure ()
""",
        }
    )
    check(
        "standalone.testCampaign.label.card.restricted" in keys,
        f"import list on a re-export ignored: {sorted(keys)}",
    )
    check(
        "standalone.otherCampaign.label.card.restricted" not in keys,
        "a name that is not re-exported still reached the use site",
    )


def test_scope_template_resolved_from_the_call_site() -> None:
    keys = keys_of(
        {
            "Test/Helpers.hs": HELPERS
            + """
scenarioI18n :: Int -> (HasI18n => a) -> a
scenarioI18n n a = campaignI18n $ scope ("chapter" <> tshow n) a
""",
            "Test/Scenario.hs": """module Test.Scenario where

import Test.Helpers

run = scenarioI18n 2 $ story $ i18nWithTitle "intro"
""",
        }
    )
    check(
        "standalone.testCampaign.chapter2.intro.body" in keys,
        f"parameterized scope alias not resolved: {sorted(keys)}",
    )


def test_conditional_and_local_binding_scopes_fan_out() -> None:
    keys = keys_of(
        {
            "Test/Helpers.hs": HELPERS,
            "Test/Scenario.hs": """module Test.Scenario where

import Test.Helpers

run headedWest = campaignI18n $ do
  scope (if headedWest then "west" else "east") $ story $ p "body"
""",
        }
    )
    for expected in ("standalone.testCampaign.west.body", "standalone.testCampaign.east.body"):
        check(expected in keys, f"conditional scope branch missing {expected}: {sorted(keys)}")


def test_a_local_helpers_key_parameter_is_read_from_its_call_sites() -> None:
    keys = keys_of(
        {
            "Test/Helpers.hs": HELPERS,
            "Test/Campaign.hs": """module Test.Campaign where

import Test.Helpers

run = campaignI18n $ do
  let interlude k = story $ setTitle "title" >> p k
  interlude "firstEntry"
  interlude "secondEntry"
""",
        }
    )
    for expected in ("standalone.testCampaign.firstEntry", "standalone.testCampaign.secondEntry"):
        check(expected in keys, f"call-site key propagation missing {expected}: {sorted(keys)}")


def test_presentation_modifiers_keep_the_key_and_shift_validate() -> None:
    keys = keys_of(
        {
            "Test/Helpers.hs": HELPERS,
            "Test/Scenario.hs": """module Test.Scenario where

import Test.Helpers

run ok = campaignI18n $ story $ do
  p.green "styledBody"
  ul $ li.validate ok "validatedItem"
""",
        }
    )
    check("standalone.testCampaign.styledBody" in keys, f"p.green lost its key: {sorted(keys)}")
    check(
        "standalone.testCampaign.validatedItem" in keys,
        f"li.validate resolved the predicate instead of the key: {sorted(keys)}",
    )


def test_withvars_declares_the_names_the_backend_sends() -> None:
    variables = variables_of(
        {
            "Test/Helpers.hs": HELPERS,
            "Test/Scenario.hs": """module Test.Scenario where

import Test.Helpers

run xp shelter = campaignI18n $ withVars ["xp" .= xp, "shelterValue" .= shelter] $ story $ p "body"
""",
        },
        "standalone.testCampaign.body",
    )
    check(
        variables == {"xp", "shelterValue"},
        f"withVars binders not modelled: {sorted(variables)}",
    )


def test_withvars_token_literals_are_typed_as_chaos_token_faces() -> None:
    artifact = registry_of(
        {
            "Arkham/ChaosToken/Types.hs": CHAOS_TOKEN_TYPES,
            "Test/Helpers.hs": HELPERS,
            "Test/Scenario.hs": """module Test.Scenario where

import Test.Helpers

run headedWest = campaignI18n $
  withVars ["token" .= String (if headedWest then "elderThing" else "skull")] $ story $ p "addToken"
""",
        }
    )
    variables = variable_types_of(
        {
            "Arkham/ChaosToken/Types.hs": CHAOS_TOKEN_TYPES,
            "Test/Helpers.hs": HELPERS,
            "Test/Scenario.hs": """module Test.Scenario where

import Test.Helpers

run headedWest = campaignI18n $
  withVars ["token" .= String (if headedWest then "elderThing" else "skull")] $ story $ p "addToken"
""",
        },
        "standalone.testCampaign.addToken",
    )
    check(
        variables.get("token") == extractor.CHAOS_TOKEN_FACE_TYPE,
        f"token was not typed as a chaos-token face: {variables}",
    )
    check(
        artifact["variableTypes"][extractor.CHAOS_TOKEN_FACE_TYPE]["values"]
        == ["skull", "elderThing", "bless"],
        f"chaos-token icon faces were not derived from source: {artifact['variableTypes']}",
    )
    check(
        "openCustomFaces" not in artifact["variableTypes"][extractor.CHAOS_TOKEN_FACE_TYPE],
        "chaos-token icon variable type must not claim custom-face coverage",
    )


def test_token_face_proof_rejects_dynamic_expressions() -> None:
    modules = {
        "Arkham/ChaosToken/Types.hs": CHAOS_TOKEN_TYPES,
        "Test/Helpers.hs": HELPERS,
        "Test/Scenario.hs": """module Test.Scenario where

import Test.Helpers

tokenName :: Text -> Text
tokenName face = face

run suffix face dynamicToken = campaignI18n $ story $ do
  withVar "token" (String ("skull" <> suffix)) $ p "concatToken"
  withVar "token" (String (tokenName "skull")) $ p "calledToken"
  withVar "token" (String dynamicToken) $ p "unresolvedToken"
""",
    }

    expected_types = {
        "standalone.testCampaign.concatToken": "unknown",
        "standalone.testCampaign.calledToken": "unknown",
        "standalone.testCampaign.unresolvedToken": "unknown",
    }
    for key, expected in expected_types.items():
        variables = variable_types_of(modules, key)
        check(
            variables.get("token") == expected,
            f"dynamic token expression did not fall back to {expected} for {key}: {variables}",
        )


def test_if_token_literals_are_typed_as_chaos_token_faces() -> None:
    variables = variable_types_of(
        {
            "Arkham/ChaosToken/Types.hs": CHAOS_TOKEN_TYPES,
            "Test/Helpers.hs": HELPERS,
            "Test/Scenario.hs": """module Test.Scenario where

import Test.Helpers

run headedWest = campaignI18n $
  withVar "token" (String (if headedWest then "elderThing" else "skull")) $ story $ p "addToken"
""",
        },
        "standalone.testCampaign.addToken",
    )
    check(
        variables.get("token") == extractor.CHAOS_TOKEN_FACE_TYPE,
        f"if token branches were not typed as chaos-token faces: {variables}",
    )


def test_case_token_literals_are_typed_as_chaos_token_faces() -> None:
    variables = variable_types_of(
        {
            "Arkham/ChaosToken/Types.hs": CHAOS_TOKEN_TYPES,
            "Test/Helpers.hs": HELPERS,
            "Test/Scenario.hs": """module Test.Scenario where

import Test.Helpers

run face = campaignI18n $
  withVar "token" (String (case face of
    Skull -> "skull"
    ElderThing -> "elderThing"
  )) $ story $ p "addToken"
""",
        },
        "standalone.testCampaign.addToken",
    )
    check(
        variables.get("token") == extractor.CHAOS_TOKEN_FACE_TYPE,
        f"case token branches were not typed as chaos-token faces: {variables}",
    )


def test_guarded_case_token_faces_reject_dynamic_results() -> None:
    variables = variable_types_of(
        {
            "Arkham/ChaosToken/Types.hs": CHAOS_TOKEN_TYPES,
            "Test/Helpers.hs": HELPERS,
            "Test/Scenario.hs": """module Test.Scenario where

import Test.Helpers

run face dynamicToken = campaignI18n $
  withVar "token" (String (case face of
    token | shouldUseDynamic token -> dynamicToken
          | otherwise -> "skull"
  )) $ story $ p "addToken"
""",
        },
        "standalone.testCampaign.addToken",
    )
    check(
        variables.get("token") == "unknown",
        f"guarded case dynamic result did not fail closed: {variables}",
    )


def test_icon_variable_type_conflicts_downgrade_to_unknown() -> None:
    variables = variable_types_of(
        {
            "Arkham/ChaosToken/Types.hs": CHAOS_TOKEN_TYPES,
            "Test/Helpers.hs": HELPERS,
            "Test/Scenario.hs": """module Test.Scenario where

import Test.Helpers

run useDynamic dynamicToken = campaignI18n $ story $ do
  if useDynamic
    then withVar "token" (String dynamicToken) $ p "addToken"
    else withVar "token" (String "skull") $ p "addToken"
""",
        },
        "standalone.testCampaign.addToken",
    )
    check(
        variables.get("token") == "unknown",
        f"conflicting token variable evidence did not fail closed: {variables}",
    )


def test_icon_variable_type_conflicts_downgrade_to_unknown_when_proven_site_is_first() -> None:
    variables = variable_types_of(
        {
            "Arkham/ChaosToken/Types.hs": CHAOS_TOKEN_TYPES,
            "Test/Helpers.hs": HELPERS,
            "Test/Scenario.hs": """module Test.Scenario where

import Test.Helpers

run useStatic dynamicToken = campaignI18n $ story $ do
  if useStatic
    then withVar "token" (String "skull") $ p "addToken"
    else withVar "token" (String dynamicToken) $ p "addToken"
""",
        },
        "standalone.testCampaign.addToken",
    )
    check(
        variables.get("token") == "unknown",
        f"proven-first token variable conflict did not fail closed: {variables}",
    )


def test_icon_variable_type_conflicts_downgrade_across_modules() -> None:
    variables = variable_types_of(
        {
            "Arkham/ChaosToken/Types.hs": CHAOS_TOKEN_TYPES,
            "Test/Helpers.hs": HELPERS,
            "Test/A.hs": """module Test.A where

import Test.Helpers

run dynamicToken = campaignI18n $ story $ withVar "token" (String dynamicToken) $ p "addToken"
""",
            "Test/B.hs": """module Test.B where

import Test.Helpers

run = campaignI18n $ story $ withVar "token" (String "skull") $ p "addToken"
""",
        },
        "standalone.testCampaign.addToken",
    )
    check(
        variables.get("token") == "unknown",
        f"cross-module token variable conflict did not fail closed: {variables}",
    )


def test_key_name_string_wrapped_key_variable_is_text() -> None:
    variables = variable_types_of(
        {
            "Arkham/ChaosToken/Types.hs": CHAOS_TOKEN_TYPES,
            "Arkham/Key.hs": KEY_I18N,
            "Test/Helpers.hs": HELPERS,
            "Test/Scenario.hs": """module Test.Scenario where

import Arkham.Key
import Test.Helpers

run key = campaignI18n $ story $ withVar "key" (String $ keyName key) $ labeled' "placeKeyOnTheAmalgam"
""",
        },
        "standalone.testCampaign.label.placeKeyOnTheAmalgam",
    )
    check(
        variables.get("key") == "text",
        f"String-wrapped keyName variable was not typed as text: {variables}",
    )


def check_key_name_positive_control(context: str) -> None:
    variables = variable_types_of(
        {
            "Arkham/ChaosToken/Types.hs": CHAOS_TOKEN_TYPES,
            "Arkham/Key.hs": KEY_I18N,
            "Test/Helpers.hs": HELPERS,
            "Test/Scenario.hs": """module Test.Scenario where

import Arkham.Key
import Test.Helpers

run key = campaignI18n $ story $ withVar "key" (String $ keyName key) $ labeled' "placeKeyOnTheAmalgam"
""",
        },
        "standalone.testCampaign.label.placeKeyOnTheAmalgam",
    )
    check(
        variables.get("key") == "text",
        f"{context}: positive keyName control was not typed as text: {variables}",
    )


def test_key_name_text_proof_rejects_another_callee() -> None:
    check_key_name_positive_control("another callee boundary")
    variables = variable_types_of(
        {
            "Arkham/ChaosToken/Types.hs": CHAOS_TOKEN_TYPES,
            "Arkham/Key.hs": KEY_I18N,
            "Test/Helpers.hs": HELPERS,
            "Test/Scenario.hs": """module Test.Scenario where

import Arkham.Key
import Test.Helpers

keyLabel _ = "Red"

run key = campaignI18n $ story $ withVar "key" (String $ keyLabel key) $ labeled' "placeKeyOnTheAmalgam"
""",
        },
        "standalone.testCampaign.label.placeKeyOnTheAmalgam",
    )
    check(
        variables.get("key") == "unknown",
        f"non-keyName callee was accepted as a key text proof: {variables}",
    )


def test_key_name_text_proof_rejects_a_shadowed_key_name() -> None:
    check_key_name_positive_control("shadowed keyName boundary")
    variables = variable_types_of(
        {
            "Arkham/ChaosToken/Types.hs": CHAOS_TOKEN_TYPES,
            "Arkham/Key.hs": KEY_I18N,
            "Test/Helpers.hs": HELPERS,
            "Test/Scenario.hs": """module Test.Scenario where

import Arkham.Key
import Test.Helpers

keyName _ = "Red"

run key = campaignI18n $ story $ withVar "key" (String $ keyName key) $ labeled' "placeKeyOnTheAmalgam"
""",
        },
        "standalone.testCampaign.label.placeKeyOnTheAmalgam",
    )
    check(
        variables.get("key") == "unknown",
        f"shadowed keyName was accepted as a key text proof: {variables}",
    )


def test_key_name_text_proof_requires_the_string_wrapper() -> None:
    check_key_name_positive_control("non-String wrapper boundary")
    variables = variable_types_of(
        {
            "Arkham/ChaosToken/Types.hs": CHAOS_TOKEN_TYPES,
            "Arkham/Key.hs": KEY_I18N,
            "Test/Helpers.hs": HELPERS,
            "Test/Scenario.hs": """module Test.Scenario where

import Arkham.Key
import Test.Helpers

run key = campaignI18n $ story $ withVar "key" (keyName key) $ labeled' "placeKeyOnTheAmalgam"
""",
        },
        "standalone.testCampaign.label.placeKeyOnTheAmalgam",
    )
    check(
        variables.get("key") == "unknown",
        f"bare keyName was accepted without the String wrapper: {variables}",
    )


def test_key_name_text_proof_rejects_mixed_call_sites() -> None:
    check_key_name_positive_control("mixed call-site boundary")
    variables = variable_types_of(
        {
            "Arkham/ChaosToken/Types.hs": CHAOS_TOKEN_TYPES,
            "Arkham/Key.hs": KEY_I18N,
            "Test/Helpers.hs": HELPERS,
            "Test/A.hs": """module Test.A where

import Arkham.Key
import Test.Helpers

run key = campaignI18n $ story $ withVar "key" (String $ keyName key) $ labeled' "placeKeyOnTheAmalgam"
""",
            "Test/B.hs": """module Test.B where

import Arkham.Key
import Test.Helpers

run key = campaignI18n $ story $ withVar "key" (keyName key) $ labeled' "placeKeyOnTheAmalgam"
""",
        },
        "standalone.testCampaign.label.placeKeyOnTheAmalgam",
    )
    check(
        variables.get("key") == "unknown",
        f"mixed proven and unproven keyName call sites did not fail closed: {variables}",
    )


def test_key_name_text_proof_rejects_mixed_call_sites_when_unproven_site_is_first() -> None:
    check_key_name_positive_control("mixed call-site reverse-order boundary")
    variables = variable_types_of(
        {
            "Arkham/ChaosToken/Types.hs": CHAOS_TOKEN_TYPES,
            "Arkham/Key.hs": KEY_I18N,
            "Test/Helpers.hs": HELPERS,
            "Test/A.hs": """module Test.A where

import Arkham.Key
import Test.Helpers

run key = campaignI18n $ story $ withVar "key" (keyName key) $ labeled' "placeKeyOnTheAmalgam"
""",
            "Test/B.hs": """module Test.B where

import Arkham.Key
import Test.Helpers

run key = campaignI18n $ story $ withVar "key" (String $ keyName key) $ labeled' "placeKeyOnTheAmalgam"
""",
        },
        "standalone.testCampaign.label.placeKeyOnTheAmalgam",
    )
    check(
        variables.get("key") == "unknown",
        f"reverse-order mixed keyName call sites did not fail closed: {variables}",
    )


def test_skill_icon_registry_matches_skill_var_when_every_value_has_a_glyph() -> None:
    artifact = registry_of(
        {
            "Arkham/I18n.hs": SKILL_I18N,
            "Test/Helpers.hs": HELPERS,
            "Test/Scenario.hs": """module Test.Scenario where

import Test.Helpers

run = campaignI18n $ story $ skillVar #willpower $ labeled' "test"
""",
        },
        icon_tags={"willpower", "intellect", "combat", "agility"},
    )
    variables = {
        entry["key"]: {variable["name"]: variable["type"] for variable in entry["variables"]}
        for entry in artifact["keys"]
    }
    values = artifact["variableTypes"][extractor.SKILL_ICON_TYPE]["values"]

    check(values == extractor.SKILL_ICON_VALUES, f"skill icon registry did not match skillVar: {values}")
    check(
        variables.get("standalone.testCampaign.label.test", {}).get("skill")
        == extractor.SKILL_ICON_TYPE,
        f"skillVar was not typed as a skill icon: {variables}",
    )


def test_skill_var_falls_back_to_text_when_icon_registry_is_incomplete() -> None:
    variables = variable_types_of(
        {
            "Arkham/I18n.hs": SKILL_I18N,
            "Test/Helpers.hs": HELPERS,
            "Test/Scenario.hs": """module Test.Scenario where

import Test.Helpers

run = campaignI18n $ story $ skillVar #willpower $ labeled' "test"
""",
        },
        "standalone.testCampaign.label.test",
        icon_tags={"willpower", "combat", "agility"},
    )
    check(
        variables.get("skill") == "text",
        f"skillVar did not fall back to text with an incomplete icon registry: {variables}",
    )


def test_skill_var_falls_back_to_text_when_i18n_source_is_missing() -> None:
    variables = variable_types_of(
        {
            "Test/Helpers.hs": HELPERS,
            "Test/Scenario.hs": """module Test.Scenario where

import Test.Helpers

run = campaignI18n $ story $ skillVar #willpower $ labeled' "test"
""",
        },
        "standalone.testCampaign.label.test",
        icon_tags={"willpower", "intellect", "combat", "agility"},
    )
    check(
        variables.get("skill") == "text",
        f"skillVar did not fall back to text when Arkham/I18n.hs was missing: {variables}",
    )


def test_skill_var_falls_back_to_text_when_emitted_set_mismatches_the_registry() -> None:
    variables = variable_types_of(
        {
            "Arkham/I18n.hs": SKILL_I18N.replace(
                '  SkillAgility -> withVar "skill" (String "agility") a\n',
                "",
            ),
            "Test/Helpers.hs": HELPERS,
            "Test/Scenario.hs": """module Test.Scenario where

import Test.Helpers

run = campaignI18n $ story $ skillVar #willpower $ labeled' "test"
""",
        },
        "standalone.testCampaign.label.test",
        icon_tags={"willpower", "intellect", "combat", "agility"},
    )
    check(
        variables.get("skill") == "text",
        f"skillVar did not fall back to text when its emitted set changed: {variables}",
    )


def test_skill_var_falls_back_to_text_for_an_extra_value_without_a_glyph() -> None:
    variables = variable_types_of(
        {
            "Arkham/I18n.hs": SKILL_I18N.replace(
                '  SkillAgility -> withVar "skill" (String "agility") a\n',
                '  SkillAgility -> withVar "skill" (String "agility") a\n  SkillWild -> withVar "skill" (String "wild") a\n',
            ),
            "Test/Helpers.hs": HELPERS,
            "Test/Scenario.hs": """module Test.Scenario where

import Test.Helpers

run = campaignI18n $ story $ skillVar #willpower $ labeled' "test"
""",
        },
        "standalone.testCampaign.label.test",
        icon_tags={"willpower", "intellect", "combat", "agility"},
    )
    check(
        variables.get("skill") == "text",
        f"skillVar did not fall back to text for an extra value without a glyph: {variables}",
    )


def test_skill_var_falls_back_to_text_when_a_branch_does_not_use_withvar() -> None:
    variables = variable_types_of(
        {
            "Arkham/I18n.hs": SKILL_I18N.replace(
                '  SkillAgility -> withVar "skill" (String "agility") a\n',
                '  SkillAgility -> withVar "skill" (String "agility") a\n  SkillWild -> keyVar "skill" "wild" a\n',
            ),
            "Test/Helpers.hs": HELPERS,
            "Test/Scenario.hs": """module Test.Scenario where

import Test.Helpers

run = campaignI18n $ story $ skillVar #willpower $ labeled' "test"
""",
        },
        "standalone.testCampaign.label.test",
        icon_tags={"willpower", "intellect", "combat", "agility"},
    )
    check(
        variables.get("skill") == "text",
        f"skillVar did not fall back to text for a non-withVar branch: {variables}",
    )


def test_skill_var_falls_back_to_text_when_the_variable_name_is_not_literal() -> None:
    variables = variable_types_of(
        {
            "Arkham/I18n.hs": SKILL_I18N.replace(
                'skillVar v a = case v of\n',
                'skillName = "skill"\n\nskillVar v a = case v of\n',
            ).replace(
                '  SkillAgility -> withVar "skill" (String "agility") a\n',
                '  SkillAgility -> withVar "skill" (String "agility") a\n  SkillWild -> withVar skillName (String "wild") a\n',
            ),
            "Test/Helpers.hs": HELPERS,
            "Test/Scenario.hs": """module Test.Scenario where

import Test.Helpers

run = campaignI18n $ story $ skillVar #willpower $ labeled' "test"
""",
        },
        "standalone.testCampaign.label.test",
        icon_tags={"willpower", "intellect", "combat", "agility"},
    )
    check(
        variables.get("skill") == "text",
        f"skillVar did not fall back to text for a non-literal variable name: {variables}",
    )


def test_skill_var_falls_back_to_text_when_definition_has_a_guard() -> None:
    variables = variable_types_of(
        {
            "Arkham/I18n.hs": SKILL_I18N.replace(
                'skillVar v a = case v of\n',
                'skillVar v a\n  | otherwise = case v of\n',
            ),
            "Test/Helpers.hs": HELPERS,
            "Test/Scenario.hs": """module Test.Scenario where

import Test.Helpers

run = campaignI18n $ story $ skillVar #willpower $ labeled' "test"
""",
        },
        "standalone.testCampaign.label.test",
        icon_tags={"willpower", "intellect", "combat", "agility"},
    )
    check(
        variables.get("skill") == "text",
        f"skillVar did not fall back to text for a guarded definition: {variables}",
    )


def test_skill_icon_var_registry_matches_i18n_when_every_value_has_a_glyph() -> None:
    artifact = registry_of(
        {
            "Arkham/I18n.hs": SKILL_I18N,
            "Test/Helpers.hs": HELPERS,
            "Test/Scenario.hs": """module Test.Scenario where

import Test.Helpers

run = campaignI18n $ story $ skillIconVar #wild $ labeled' "test"
""",
        },
        icon_tags={"willpower", "intellect", "combat", "agility", "wild", "wildMinus"},
    )
    variables = {
        entry["key"]: {variable["name"]: variable["type"] for variable in entry["variables"]}
        for entry in artifact["keys"]
    }
    values = artifact["variableTypes"][extractor.SKILL_ICON_FACE_TYPE]["values"]

    check(values == extractor.SKILL_ICON_FACE_VALUES, f"skillIconVar registry did not match: {values}")
    check(
        variables.get("standalone.testCampaign.label.test", {}).get("skillIcon")
        == extractor.SKILL_ICON_FACE_TYPE,
        f"skillIconVar was not typed as a skill icon face: {variables}",
    )


def test_skill_icon_var_falls_back_to_text_for_an_extra_value_without_a_glyph() -> None:
    variables = variable_types_of(
        {
            "Arkham/I18n.hs": SKILL_I18N,
            "Test/Helpers.hs": HELPERS,
            "Test/Scenario.hs": """module Test.Scenario where

import Test.Helpers

run = campaignI18n $ story $ skillIconVar #wild $ labeled' "test"
""",
        },
        "standalone.testCampaign.label.test",
        icon_tags={"willpower", "intellect", "combat", "agility", "wild"},
    )
    check(
        variables.get("skillIcon") == "text",
        f"skillIconVar did not fall back to text when wildMinus had no glyph: {variables}",
    )


def test_skill_icon_var_falls_back_to_text_when_a_branch_does_not_use_withvar() -> None:
    variables = variable_types_of(
        {
            "Arkham/I18n.hs": SKILL_I18N.replace(
                '  WildMinusIcon -> withVar "skillIcon" (String "wildMinus") a\n',
                '  WildMinusIcon -> keyVar "skillIcon" "wildMinus" a\n',
            ),
            "Test/Helpers.hs": HELPERS,
            "Test/Scenario.hs": """module Test.Scenario where

import Test.Helpers

run = campaignI18n $ story $ skillIconVar #wild $ labeled' "test"
""",
        },
        "standalone.testCampaign.label.test",
        icon_tags={"willpower", "intellect", "combat", "agility", "wild", "wildMinus"},
    )
    check(
        variables.get("skillIcon") == "text",
        f"skillIconVar did not fall back to text for a non-withVar branch: {variables}",
    )


def test_skill_icon_var_falls_back_to_text_when_variable_name_is_not_literal() -> None:
    variables = variable_types_of(
        {
            "Arkham/I18n.hs": SKILL_I18N.replace(
                'skillIconVar v a = case v of\n',
                'skillIconName = "skillIcon"\n\nskillIconVar v a = case v of\n',
            ).replace(
                '  WildMinusIcon -> withVar "skillIcon" (String "wildMinus") a\n',
                '  WildMinusIcon -> withVar skillIconName (String "wildMinus") a\n',
            ),
            "Test/Helpers.hs": HELPERS,
            "Test/Scenario.hs": """module Test.Scenario where

import Test.Helpers

run = campaignI18n $ story $ skillIconVar #wild $ labeled' "test"
""",
        },
        "standalone.testCampaign.label.test",
        icon_tags={"willpower", "intellect", "combat", "agility", "wild", "wildMinus"},
    )
    check(
        variables.get("skillIcon") == "text",
        f"skillIconVar did not fall back to text for a non-literal variable name: {variables}",
    )


def test_skill_icon_var_falls_back_to_text_when_definition_has_a_guard() -> None:
    variables = variable_types_of(
        {
            "Arkham/I18n.hs": SKILL_I18N.replace(
                'skillIconVar v a = case v of\n',
                'skillIconVar v a\n  | otherwise = case v of\n',
            ),
            "Test/Helpers.hs": HELPERS,
            "Test/Scenario.hs": """module Test.Scenario where

import Test.Helpers

run = campaignI18n $ story $ skillIconVar #wild $ labeled' "test"
""",
        },
        "standalone.testCampaign.label.test",
        icon_tags={"willpower", "intellect", "combat", "agility", "wild", "wildMinus"},
    )
    check(
        variables.get("skillIcon") == "text",
        f"skillIconVar did not fall back to text for a guarded definition: {variables}",
    )


def test_replaced_skill_key_is_typed_as_a_skill_icon_when_skill_type_key_is_proven() -> None:
    variables = variable_types_of(
        {
            "Arkham/I18n.hs": SKILL_I18N,
            "Arkham/Aspect.hs": ASPECT_I18N,
            "Test/Helpers.hs": HELPERS,
            "Test/Scenario.hs": """module Test.Scenario where

import Arkham.Aspect
import Test.Helpers

run replaced = campaignI18n $ story $ keyVar "replacedSkill" (skillTypeKey replaced) $ labeled' "test"
""",
        },
        "standalone.testCampaign.label.test",
        icon_tags={"willpower", "intellect", "combat", "agility"},
    )
    check(
        variables.get("replacedSkill") == extractor.SKILL_ICON_TYPE,
        f"replacedSkill was not typed as a skill icon: {variables}",
    )


def test_replaced_skill_key_is_typed_inside_the_canonical_skill_type_key_module() -> None:
    variables = variable_types_of(
        {
            "Arkham/I18n.hs": SKILL_I18N,
            "Arkham/Aspect.hs": ASPECT_I18N
            + '\nrun skillType replaced = withI18n\n  $ skillVar skillType\n  $ keyVar "replacedSkill" (skillTypeKey replaced)\n  $ labeled\' "ignoreUseSkillTypeInsteadOf"\n',
        },
        "label.ignoreUseSkillTypeInsteadOf",
        icon_tags={"willpower", "intellect", "combat", "agility"},
    )
    check(
        variables.get("replacedSkill") == extractor.SKILL_ICON_TYPE,
        f"same-module skillTypeKey call was treated as a shadow: {variables}",
    )


def test_seal_key_is_typed_as_a_seal_icon_when_seal_kind_is_proven() -> None:
    variables = variable_types_of(
        {
            "Arkham/I18n.hs": SKILL_I18N,
            "Arkham/Prelude.hs": PRELUDE_I18N,
            "Arkham/Campaigns/EdgeOfTheEarth/Seal.hs": SEAL_I18N,
            "Test/Helpers.hs": HELPERS,
            "Test/Scenario.hs": """module Test.Scenario where

import Arkham.I18n
import Arkham.Prelude
import Test.Helpers

run seal = campaignI18n $ story $ keyVar "seal" (toScope $ tshow seal.kind) $ labeled' "test"
""",
        },
        "standalone.testCampaign.label.test",
        icon_tags={"sealA", "sealB", "sealC", "sealD", "sealE"},
    )
    check(
        variables.get("seal") == extractor.SEAL_ICON_TYPE,
        f"seal was not typed as a seal icon: {variables}",
    )


def test_seal_falls_back_to_text_for_a_module_level_to_scope_shadow() -> None:
    variables = variable_types_of(
        {
            "Arkham/I18n.hs": SKILL_I18N,
            "Arkham/Prelude.hs": PRELUDE_I18N,
            "Arkham/Campaigns/EdgeOfTheEarth/Seal.hs": SEAL_I18N,
            "Test/Helpers.hs": HELPERS,
            "Test/Scenario.hs": """module Test.Scenario where

import Arkham.I18n
import Arkham.Prelude
import Test.Helpers

toScope _ = "sealA"

run seal = campaignI18n $ story $ keyVar "seal" (toScope $ tshow seal.kind) $ labeled' "test"
""",
        },
        "standalone.testCampaign.label.test",
        icon_tags={"sealA", "sealB", "sealC", "sealD", "sealE"},
    )
    check(
        variables.get("seal") == "text",
        f"seal did not fall back to text for a module-level toScope shadow: {variables}",
    )


def test_seal_falls_back_to_text_for_a_module_level_tshow_shadow() -> None:
    variables = variable_types_of(
        {
            "Arkham/I18n.hs": SKILL_I18N,
            "Arkham/Prelude.hs": PRELUDE_I18N,
            "Arkham/Campaigns/EdgeOfTheEarth/Seal.hs": SEAL_I18N,
            "Test/Helpers.hs": HELPERS,
            "Test/Scenario.hs": """module Test.Scenario where

import Arkham.I18n
import Arkham.Prelude
import Test.Helpers

tshow _ = "SealA"

run seal = campaignI18n $ story $ keyVar "seal" (toScope $ tshow seal.kind) $ labeled' "test"
""",
        },
        "standalone.testCampaign.label.test",
        icon_tags={"sealA", "sealB", "sealC", "sealD", "sealE"},
    )
    check(
        variables.get("seal") == "text",
        f"seal did not fall back to text for a module-level tshow shadow: {variables}",
    )


def test_seal_falls_back_to_text_for_a_let_bound_to_scope_shadow() -> None:
    variables = variable_types_of(
        {
            "Arkham/I18n.hs": SKILL_I18N,
            "Arkham/Prelude.hs": PRELUDE_I18N,
            "Arkham/Campaigns/EdgeOfTheEarth/Seal.hs": SEAL_I18N,
            "Test/Helpers.hs": HELPERS,
            "Test/Scenario.hs": """module Test.Scenario where

import Arkham.I18n
import Arkham.Prelude
import Test.Helpers

run seal = campaignI18n $ story $ do
  let toScope _ = "sealA"
  keyVar "seal" (toScope $ tshow seal.kind) $ labeled' "test"
""",
        },
        "standalone.testCampaign.label.test",
        icon_tags={"sealA", "sealB", "sealC", "sealD", "sealE"},
    )
    check(
        variables.get("seal") == "text",
        f"seal did not fall back to text for a let-bound toScope shadow: {variables}",
    )


def test_seal_falls_back_to_text_for_a_where_bound_tshow_shadow() -> None:
    variables = variable_types_of(
        {
            "Arkham/I18n.hs": SKILL_I18N,
            "Arkham/Prelude.hs": PRELUDE_I18N,
            "Arkham/Campaigns/EdgeOfTheEarth/Seal.hs": SEAL_I18N,
            "Test/Helpers.hs": HELPERS,
            "Test/Scenario.hs": """module Test.Scenario where

import Arkham.I18n
import Arkham.Prelude
import Test.Helpers

run seal = campaignI18n $ story $ keyVar "seal" (toScope $ tshow seal.kind) $ labeled' "test"
 where
  tshow _ = "SealA"
""",
        },
        "standalone.testCampaign.label.test",
        icon_tags={"sealA", "sealB", "sealC", "sealD", "sealE"},
    )
    check(
        variables.get("seal") == "text",
        f"seal did not fall back to text for a where-bound tshow shadow: {variables}",
    )


def test_discard_matching_icons_is_typed_from_literal_skill_icon_call_sites() -> None:
    artifact = registry_of(
        {
            "Arkham/I18n.hs": SKILL_I18N,
            "Arkham/SkillType.hs": SKILL_TYPE_I18N,
            "Test/DiscardA.hs": """module Test.DiscardA where

import Arkham.I18n

run = withI18n $ skillIconVar #combat $ labeled' "discardCardsWithMatchingIcons"
""",
            "Test/DiscardB.hs": """module Test.DiscardB where

import Arkham.I18n

run = withI18n $ skillIconVar #wild $ labeled' "discardCardsWithMatchingIcons"
""",
        },
        icon_tags={"combat", "wild"},
    )
    variables = {
        entry["key"]: {variable["name"]: variable["type"] for variable in entry["variables"]}
        for entry in artifact["keys"]
    }
    values = artifact["variableTypes"][extractor.SKILL_ICON_DISCARD_TYPE]["values"]
    check(values == ["combat", "wild"], f"discard icon registry did not match call sites: {values}")
    check(
        variables.get("label.discardCardsWithMatchingIcons", {}).get("skillIcon")
        == extractor.SKILL_ICON_DISCARD_TYPE,
        f"discardCardsWithMatchingIcons was not typed from literal skillIconVar sites: {variables}",
    )


def test_discard_matching_icons_is_typed_from_a_qualified_skill_icon_var_call_site() -> None:
    variables = variable_types_of(
        {
            "Arkham/I18n.hs": SKILL_I18N,
            "Arkham/SkillType.hs": SKILL_TYPE_I18N,
            "Test/Discard.hs": """module Test.Discard where

import Arkham.I18n qualified as I

run = withI18n $ I.skillIconVar #combat $ labeled' "discardCardsWithMatchingIcons"
""",
        },
        "label.discardCardsWithMatchingIcons",
        icon_tags={"combat"},
    )
    check(
        variables.get("skillIcon") == extractor.SKILL_ICON_DISCARD_TYPE,
        f"qualified skillIconVar call site was not resolved to Arkham.I18n: {variables}",
    )


def test_discard_matching_icons_falls_back_to_text_for_a_variable_skill_icon() -> None:
    variables = variable_types_of(
        {
            "Arkham/I18n.hs": SKILL_I18N,
            "Arkham/SkillType.hs": SKILL_TYPE_I18N,
            "Test/Discard.hs": """module Test.Discard where

import Arkham.I18n

run icon = withI18n $ skillIconVar icon $ labeled' "discardCardsWithMatchingIcons"
""",
        },
        "label.discardCardsWithMatchingIcons",
        icon_tags={"combat", "wild"},
    )
    check(
        variables.get("skillIcon") == "text",
        f"discardCardsWithMatchingIcons did not fall back to text for a variable argument: {variables}",
    )


def test_discard_matching_icons_falls_back_to_text_without_a_glyph_for_a_literal_site() -> None:
    variables = variable_types_of(
        {
            "Arkham/I18n.hs": SKILL_I18N,
            "Arkham/SkillType.hs": SKILL_TYPE_I18N,
            "Test/Discard.hs": """module Test.Discard where

import Arkham.I18n

run = withI18n $ skillIconVar #wild $ labeled' "discardCardsWithMatchingIcons"
""",
        },
        "label.discardCardsWithMatchingIcons",
        icon_tags={"combat"},
    )
    check(
        variables.get("skillIcon") == "text",
        f"discardCardsWithMatchingIcons did not fall back to text without a glyph: {variables}",
    )


def test_discard_matching_icons_falls_back_to_text_for_an_ikey_variable_skill_icon() -> None:
    variables = variable_types_of(
        {
            "Arkham/I18n.hs": SKILL_I18N,
            "Arkham/SkillType.hs": SKILL_TYPE_I18N,
            "Test/DiscardLiteral.hs": """module Test.DiscardLiteral where

import Arkham.I18n

run = withI18n $ skillIconVar #combat $ labeled' "discardCardsWithMatchingIcons"
""",
            "Test/DiscardVariable.hs": """module Test.DiscardVariable where

import Arkham.I18n

runV icon = withI18n $ skillIconVar icon $ ikey' "label.discardCardsWithMatchingIcons"
""",
        },
        "label.discardCardsWithMatchingIcons",
        icon_tags={"combat", "wild"},
    )
    check(
        variables.get("skillIcon") == "text",
        f"ikey' discardCardsWithMatchingIcons did not fall back to text for a variable argument: {variables}",
    )


def test_discard_matching_icons_falls_back_to_text_for_a_local_helper_forwarded_key() -> None:
    variables = variable_types_of(
        {
            "Arkham/I18n.hs": SKILL_I18N,
            "Arkham/SkillType.hs": SKILL_TYPE_I18N,
            "Test/DiscardLiteral.hs": """module Test.DiscardLiteral where

import Arkham.I18n

run = withI18n $ skillIconVar #combat $ labeled' "discardCardsWithMatchingIcons"
""",
            "Test/DiscardVariable.hs": """module Test.DiscardVariable where

import Arkham.I18n

runV icon = withI18n $ skillIconVar icon $ do
  let prompt key = labeled' key
  prompt "discardCardsWithMatchingIcons"
""",
        },
        "label.discardCardsWithMatchingIcons",
        icon_tags={"combat", "wild"},
    )
    check(
        variables.get("skillIcon") == "text",
        f"local-helper discardCardsWithMatchingIcons did not fall back to text: {variables}",
    )


def test_discard_matching_icons_falls_back_to_text_when_a_helper_literal_is_called_under_a_variable() -> None:
    variables = variable_types_of(
        {
            "Arkham/I18n.hs": SKILL_I18N,
            "Arkham/SkillType.hs": SKILL_TYPE_I18N,
            "Test/DiscardLiteral.hs": """module Test.DiscardLiteral where

import Arkham.I18n

run = withI18n $ skillIconVar #combat $ labeled' "discardCardsWithMatchingIcons"
""",
            "Test/DiscardVariable.hs": """module Test.DiscardVariable where

import Arkham.I18n

runV icon = withI18n $ skillIconVar #combat $ do
  let prompt key = labeled' key
  skillIconVar icon $ prompt "discardCardsWithMatchingIcons"
""",
        },
        "label.discardCardsWithMatchingIcons",
        icon_tags={"combat", "wild"},
    )
    check(
        variables.get("skillIcon") == "text",
        f"helper discardCardsWithMatchingIcons did not check the variable call-site skillIconVar: {variables}",
    )


def test_discard_matching_icons_falls_back_to_text_for_an_unknown_literal() -> None:
    variables = variable_types_of(
        {
            "Arkham/I18n.hs": SKILL_I18N,
            "Arkham/SkillType.hs": SKILL_TYPE_I18N,
            "Test/Discard.hs": """module Test.Discard where

import Arkham.I18n

run = withI18n $ skillIconVar #foo $ labeled' "discardCardsWithMatchingIcons"
""",
        },
        "label.discardCardsWithMatchingIcons",
        icon_tags={"combat", "wild"},
    )
    check(
        variables.get("skillIcon") == "text",
        f"discardCardsWithMatchingIcons did not fall back to text for an unknown literal: {variables}",
    )


def test_discard_matching_icons_falls_back_to_text_for_mixed_literal_and_variable_sites() -> None:
    variables = variable_types_of(
        {
            "Arkham/I18n.hs": SKILL_I18N,
            "Arkham/SkillType.hs": SKILL_TYPE_I18N,
            "Test/DiscardA.hs": """module Test.DiscardA where

import Arkham.I18n

run = withI18n $ skillIconVar #combat $ labeled' "discardCardsWithMatchingIcons"
""",
            "Test/DiscardB.hs": """module Test.DiscardB where

import Arkham.I18n

run icon = withI18n $ skillIconVar icon $ labeled' "discardCardsWithMatchingIcons"
""",
        },
        "label.discardCardsWithMatchingIcons",
        icon_tags={"combat", "wild"},
    )
    check(
        variables.get("skillIcon") == "text",
        f"discardCardsWithMatchingIcons did not fall back to text for mixed literal and variable sites: {variables}",
    )


def test_discard_matching_icons_falls_back_to_text_for_an_inner_skill_icon_override() -> None:
    variables = variable_types_of(
        {
            "Arkham/I18n.hs": SKILL_I18N,
            "Arkham/SkillType.hs": SKILL_TYPE_I18N,
            "Test/Discard.hs": """module Test.Discard where

import Arkham.I18n

run = withI18n $ skillIconVar #combat $ withVar "skillIcon" "wild" $ labeled' "discardCardsWithMatchingIcons"
""",
        },
        "label.discardCardsWithMatchingIcons",
        icon_tags={"combat", "wild"},
    )
    check(
        variables.get("skillIcon") == "text",
        f"discardCardsWithMatchingIcons did not fall back to text for an inner skillIcon override: {variables}",
    )


def test_discard_matching_icons_falls_back_to_text_for_nested_skill_icon_var_binders() -> None:
    variables = variable_types_of(
        {
            "Arkham/I18n.hs": SKILL_I18N,
            "Arkham/SkillType.hs": SKILL_TYPE_I18N,
            "Test/Discard.hs": """module Test.Discard where

import Arkham.I18n

run = withI18n $ skillIconVar #wildMinus $ skillIconVar #combat $ labeled' "discardCardsWithMatchingIcons"
""",
        },
        "label.discardCardsWithMatchingIcons",
        icon_tags={"combat"},
    )
    check(
        variables.get("skillIcon") == "text",
        f"discardCardsWithMatchingIcons did not fall back to text for nested skillIconVar binders: {variables}",
    )


def test_discard_matching_icons_falls_back_to_text_for_a_module_level_skill_icon_var_shadow() -> None:
    variables = variable_types_of(
        {
            "Arkham/I18n.hs": SKILL_I18N,
            "Arkham/SkillType.hs": SKILL_TYPE_I18N,
            "Test/Discard.hs": """module Test.Discard where

import Arkham.I18n

skillIconVar _ a = withVar "skillIcon" "combat" a

run = withI18n $ skillIconVar #combat $ labeled' "discardCardsWithMatchingIcons"
""",
        },
        "label.discardCardsWithMatchingIcons",
        icon_tags={"combat", "wild"},
    )
    check(
        variables.get("skillIcon") == "text",
        f"discardCardsWithMatchingIcons did not fall back to text for a module-level skillIconVar shadow: {variables}",
    )


def test_discard_matching_icons_falls_back_to_text_for_a_let_bound_skill_icon_var_shadow() -> None:
    variables = variable_types_of(
        {
            "Arkham/I18n.hs": SKILL_I18N,
            "Arkham/SkillType.hs": SKILL_TYPE_I18N,
            "Test/Discard.hs": """module Test.Discard where

import Arkham.I18n

run = withI18n $ do
  let skillIconVar _ a = withVar "skillIcon" "combat" a
  skillIconVar #combat $ labeled' "discardCardsWithMatchingIcons"
""",
        },
        "label.discardCardsWithMatchingIcons",
        icon_tags={"combat", "wild"},
    )
    check(
        variables.get("skillIcon") == "text",
        f"discardCardsWithMatchingIcons did not fall back to text for a let-bound skillIconVar shadow: {variables}",
    )


def test_discard_matching_icons_falls_back_to_text_for_a_where_bound_skill_icon_var_shadow() -> None:
    variables = variable_types_of(
        {
            "Arkham/I18n.hs": SKILL_I18N,
            "Arkham/SkillType.hs": SKILL_TYPE_I18N,
            "Test/Discard.hs": """module Test.Discard where

import Arkham.I18n

run = withI18n $ skillIconVar #combat $ labeled' "discardCardsWithMatchingIcons"
 where
  skillIconVar _ a = withVar "skillIcon" "combat" a
""",
        },
        "label.discardCardsWithMatchingIcons",
        icon_tags={"combat", "wild"},
    )
    check(
        variables.get("skillIcon") == "text",
        f"discardCardsWithMatchingIcons did not fall back to text for a where-bound skillIconVar shadow: {variables}",
    )


def test_replaced_skill_falls_back_to_text_for_an_extra_wildcard_skill_type_key_arm() -> None:
    variables = variable_types_of(
        {
            "Arkham/I18n.hs": SKILL_I18N,
            "Arkham/Aspect.hs": ASPECT_I18N + '  x -> T.toLower (tshow x)\n',
            "Test/Helpers.hs": HELPERS,
            "Test/Scenario.hs": """module Test.Scenario where

import Arkham.Aspect
import Test.Helpers

run replaced = campaignI18n $ story $ keyVar "replacedSkill" (skillTypeKey replaced) $ labeled' "test"
""",
        },
        "standalone.testCampaign.label.test",
        icon_tags={"willpower", "intellect", "combat", "agility"},
    )
    check(
        variables.get("replacedSkill") == "text",
        f"replacedSkill did not fall back to text for an extra wildcard skillTypeKey arm: {variables}",
    )


def test_replaced_skill_falls_back_to_text_for_a_computed_skill_type_key_arm() -> None:
    variables = variable_types_of(
        {
            "Arkham/I18n.hs": SKILL_I18N,
            "Arkham/Aspect.hs": ASPECT_I18N.replace(
                '  SkillCombat -> "combat"\n',
                '  SkillCombat -> "combat" <> suffix\n',
            ),
            "Test/Helpers.hs": HELPERS,
            "Test/Scenario.hs": """module Test.Scenario where

import Arkham.Aspect
import Test.Helpers

run replaced = campaignI18n $ story $ keyVar "replacedSkill" (skillTypeKey replaced) $ labeled' "test"
""",
        },
        "standalone.testCampaign.label.test",
        icon_tags={"willpower", "intellect", "combat", "agility"},
    )
    check(
        variables.get("replacedSkill") == "text",
        f"replacedSkill did not fall back to text for a computed skillTypeKey arm: {variables}",
    )


def test_replaced_skill_falls_back_to_text_for_a_locally_defined_skill_type_key() -> None:
    variables = variable_types_of(
        {
            "Arkham/I18n.hs": SKILL_I18N,
            "Arkham/Aspect.hs": ASPECT_I18N,
            "Test/Helpers.hs": HELPERS,
            "Test/Scenario.hs": """module Test.Scenario where

import Test.Helpers

skillTypeKey _ = "willpower"

run replaced = campaignI18n $ story $ keyVar "replacedSkill" (skillTypeKey replaced) $ labeled' "test"
""",
        },
        "standalone.testCampaign.label.test",
        icon_tags={"willpower", "intellect", "combat", "agility"},
    )
    check(
        variables.get("replacedSkill") == "text",
        f"replacedSkill did not fall back to text for a locally defined skillTypeKey: {variables}",
    )


def test_replaced_skill_falls_back_to_text_for_a_let_bound_skill_type_key_shadow() -> None:
    variables = variable_types_of(
        {
            "Arkham/I18n.hs": SKILL_I18N,
            "Arkham/Aspect.hs": ASPECT_I18N,
            "Test/Helpers.hs": HELPERS,
            "Test/Scenario.hs": """module Test.Scenario where

import Arkham.Aspect
import Test.Helpers

run replaced = campaignI18n $ story $ do
  let skillTypeKey _ = "willpower"
  keyVar "replacedSkill" (skillTypeKey replaced) $ labeled' "test"
""",
        },
        "standalone.testCampaign.label.test",
        icon_tags={"willpower", "intellect", "combat", "agility"},
    )
    check(
        variables.get("replacedSkill") == "text",
        f"replacedSkill did not fall back to text for a let-bound skillTypeKey shadow: {variables}",
    )


def test_replaced_skill_falls_back_to_text_for_a_where_bound_skill_type_key_shadow() -> None:
    variables = variable_types_of(
        {
            "Arkham/I18n.hs": SKILL_I18N,
            "Arkham/Aspect.hs": ASPECT_I18N,
            "Test/Helpers.hs": HELPERS,
            "Test/Scenario.hs": """module Test.Scenario where

import Arkham.Aspect
import Test.Helpers

run replaced = campaignI18n $ story $ keyVar "replacedSkill" (skillTypeKey replaced) $ labeled' "test"
 where
  skillTypeKey _ = "willpower"
""",
        },
        "standalone.testCampaign.label.test",
        icon_tags={"willpower", "intellect", "combat", "agility"},
    )
    check(
        variables.get("replacedSkill") == "text",
        f"replacedSkill did not fall back to text for a where-bound skillTypeKey shadow: {variables}",
    )


def test_replaced_skill_falls_back_to_text_when_a_glyph_is_missing() -> None:
    variables = variable_types_of(
        {
            "Arkham/I18n.hs": SKILL_I18N,
            "Arkham/Aspect.hs": ASPECT_I18N,
            "Test/Helpers.hs": HELPERS,
            "Test/Scenario.hs": """module Test.Scenario where

import Arkham.Aspect
import Test.Helpers

run replaced = campaignI18n $ story $ keyVar "replacedSkill" (skillTypeKey replaced) $ labeled' "test"
""",
        },
        "standalone.testCampaign.label.test",
        icon_tags={"willpower", "intellect", "combat"},
    )
    check(
        variables.get("replacedSkill") == "text",
        f"replacedSkill did not fall back to text when a glyph was missing: {variables}",
    )


def test_replaced_skill_keyvar_with_an_unproven_value_stays_text() -> None:
    variables = variable_types_of(
        {
            "Arkham/I18n.hs": SKILL_I18N,
            "Arkham/Aspect.hs": ASPECT_I18N,
            "Test/Helpers.hs": HELPERS,
            "Test/Scenario.hs": """module Test.Scenario where

import Test.Helpers

run replaced = campaignI18n $ story $ keyVar "replacedSkill" replaced $ labeled' "test"
""",
        },
        "standalone.testCampaign.label.test",
        icon_tags={"willpower", "intellect", "combat", "agility"},
    )
    check(
        variables.get("replacedSkill") == "text",
        f"unproven replacedSkill keyVar did not stay text: {variables}",
    )


def test_seal_falls_back_to_text_when_a_glyph_is_missing() -> None:
    variables = variable_types_of(
        {
            "Arkham/I18n.hs": SKILL_I18N,
            "Arkham/Prelude.hs": PRELUDE_I18N,
            "Arkham/Campaigns/EdgeOfTheEarth/Seal.hs": SEAL_I18N,
            "Test/Helpers.hs": HELPERS,
            "Test/Scenario.hs": """module Test.Scenario where

import Arkham.I18n
import Arkham.Prelude
import Test.Helpers

run seal = campaignI18n $ story $ keyVar "seal" (toScope $ tshow seal.kind) $ labeled' "test"
""",
        },
        "standalone.testCampaign.label.test",
        icon_tags={"sealA", "sealB", "sealC", "sealD"},
    )
    check(
        variables.get("seal") == "text",
        f"seal did not fall back to text when a glyph was missing: {variables}",
    )


def test_seal_falls_back_to_text_for_an_extra_constructor() -> None:
    variables = variable_types_of(
        {
            "Arkham/I18n.hs": SKILL_I18N,
            "Arkham/Prelude.hs": PRELUDE_I18N,
            "Arkham/Campaigns/EdgeOfTheEarth/Seal.hs": SEAL_I18N.replace(
                "data SealKind = SealA | SealB | SealC | SealD | SealE",
                "data SealKind = SealA | SealB | SealC | SealD | SealE | ElderSeal",
            ),
            "Test/Helpers.hs": HELPERS,
            "Test/Scenario.hs": """module Test.Scenario where

import Arkham.I18n
import Arkham.Prelude
import Test.Helpers

run seal = campaignI18n $ story $ keyVar "seal" (toScope $ tshow seal.kind) $ labeled' "test"
""",
        },
        "standalone.testCampaign.label.test",
        icon_tags={"sealA", "sealB", "sealC", "sealD", "sealE", "elderSeal"},
    )
    check(
        variables.get("seal") == "text",
        f"seal did not fall back to text for an extra constructor: {variables}",
    )


def test_seal_falls_back_to_text_for_a_constructor_with_fields() -> None:
    variables = variable_types_of(
        {
            "Arkham/I18n.hs": SKILL_I18N,
            "Arkham/Prelude.hs": PRELUDE_I18N,
            "Arkham/Campaigns/EdgeOfTheEarth/Seal.hs": SEAL_I18N.replace(
                "data SealKind = SealA | SealB | SealC | SealD | SealE",
                "data SealKind = SealA | SealB | SealC | SealD | SealE Text",
            ),
            "Test/Helpers.hs": HELPERS,
            "Test/Scenario.hs": """module Test.Scenario where

import Arkham.I18n
import Arkham.Prelude
import Test.Helpers

run seal = campaignI18n $ story $ keyVar "seal" (toScope $ tshow seal.kind) $ labeled' "test"
""",
        },
        "standalone.testCampaign.label.test",
        icon_tags={"sealA", "sealB", "sealC", "sealD", "sealE"},
    )
    check(
        variables.get("seal") == "text",
        f"seal did not fall back to text for a constructor with fields: {variables}",
    )


def test_seal_falls_back_to_text_for_a_custom_show_instance() -> None:
    variables = variable_types_of(
        {
            "Arkham/I18n.hs": SKILL_I18N,
            "Arkham/Prelude.hs": PRELUDE_I18N,
            "Arkham/Campaigns/EdgeOfTheEarth/Seal.hs": SEAL_I18N + '\ninstance Show SealKind where\n  show _ = "custom"\n',
            "Test/Helpers.hs": HELPERS,
            "Test/Scenario.hs": """module Test.Scenario where

import Arkham.I18n
import Arkham.Prelude
import Test.Helpers

run seal = campaignI18n $ story $ keyVar "seal" (toScope $ tshow seal.kind) $ labeled' "test"
""",
        },
        "standalone.testCampaign.label.test",
        icon_tags={"sealA", "sealB", "sealC", "sealD", "sealE"},
    )
    check(
        variables.get("seal") == "text",
        f"seal did not fall back to text for a custom Show instance: {variables}",
    )


def test_seal_falls_back_to_text_when_to_scope_changes_single_word_transform() -> None:
    variables = variable_types_of(
        {
            "Arkham/I18n.hs": SKILL_I18N.replace(
                "Just (c, r) -> T.cons (Char.toLower c) r",
                "Just (c, r) -> T.cons c r",
            ),
            "Arkham/Prelude.hs": PRELUDE_I18N,
            "Arkham/Campaigns/EdgeOfTheEarth/Seal.hs": SEAL_I18N,
            "Test/Helpers.hs": HELPERS,
            "Test/Scenario.hs": """module Test.Scenario where

import Arkham.I18n
import Arkham.Prelude
import Test.Helpers

run seal = campaignI18n $ story $ keyVar "seal" (toScope $ tshow seal.kind) $ labeled' "test"
""",
        },
        "standalone.testCampaign.label.test",
        icon_tags={"sealA", "sealB", "sealC", "sealD", "sealE"},
    )
    check(
        variables.get("seal") == "text",
        f"seal did not fall back to text when toScope changed: {variables}",
    )


def test_seal_keyvar_with_an_unproven_value_stays_text() -> None:
    variables = variable_types_of(
        {
            "Arkham/I18n.hs": SKILL_I18N,
            "Arkham/Prelude.hs": PRELUDE_I18N,
            "Arkham/Campaigns/EdgeOfTheEarth/Seal.hs": SEAL_I18N,
            "Test/Helpers.hs": HELPERS,
            "Test/Scenario.hs": """module Test.Scenario where

import Arkham.I18n
import Arkham.Prelude
import Test.Helpers

run seal = campaignI18n $ story $ keyVar "seal" (toScope $ tshow other.kind) $ labeled' "test"
""",
        },
        "standalone.testCampaign.label.test",
        icon_tags={"sealA", "sealB", "sealC", "sealD", "sealE"},
    )
    check(
        variables.get("seal") == "text",
        f"unproven seal keyVar did not stay text: {variables}",
    )


def test_amount_labels_are_choice_scoped_and_readers_are_ignored() -> None:
    keys = keys_of(
        {
            "Test/Prompt.hs": """module Test.Prompt where

run iid n = do
  chooseAmounts iid "prompt" (TotalAmountTarget n) [("$widgets", (0, n))]
  push $ ResolveAmounts iid (updateAmounts "$widgets") target
"""
        }
    )
    check("choice.widgets" in keys, f"amount label not scoped under choice.: {sorted(keys)}")
    check("widgets" not in keys, "an amount identifier was published as a root key")


def test_a_module_that_cannot_be_parsed_but_emits_keys_is_a_hard_failure() -> None:
    try:
        registry_of(
            {
                "Test/Broken.hs": (
                    "module Test.Broken where\n\n"
                    "run = case x of\n"
                    "  ) -> labeled' \"broken.key\"\n"
                )
            }
        )
    except SystemExit as error:
        check("does not parse" in str(error), f"unexpected failure message: {error}")
        return
    FAILURES.append("an unparsable module containing i18n tokens was accepted")


def test_same_named_local_scopes_do_not_share_their_call_sites() -> None:
    # TheDunwichLegacy binds `interlude` twice, under two different scopes.
    keys = keys_of(
        {
            "Test/Helpers.hs": HELPERS,
            "Test/Campaign.hs": """module Test.Campaign where

import Test.Helpers

run = campaignI18n $ do
  scope "one" $ do
    let interlude k = story $ p k
    interlude "alpha"
  scope "two" $ do
    let interlude k = story $ p k
    interlude "beta"
""",
        }
    )
    check(
        keys == {"standalone.testCampaign.one.alpha", "standalone.testCampaign.two.beta"},
        f"same-named local scopes were conflated: {sorted(keys)}",
    )


def test_a_local_helper_is_scoped_by_its_call_site() -> None:
    keys = keys_of(
        {
            "Test/Helpers.hs": HELPERS,
            "Test/Scenario.hs": """module Test.Scenario where

import Test.Helpers

run = campaignI18n $ scope "codex" $ do
  let entry k = setTitle "title" >> p k
  scope "firstPerson" $ flavor $ entry "one"
  scope "secondPerson" $ flavor $ entry "two"
""",
        }
    )
    for expected in (
        "standalone.testCampaign.codex.firstPerson.one",
        "standalone.testCampaign.codex.secondPerson.two",
    ):
        check(expected in keys, f"call-site scope missing {expected}: {sorted(keys)}")
    check(
        "standalone.testCampaign.codex.one" not in keys,
        "a local helper was filed under its definition's scope",
    )


def test_a_top_level_helper_is_resolved_across_modules_but_not_across_definitions() -> None:
    modules = {
        "Test/Helpers.hs": HELPERS,
        "Test/First/Helpers.hs": """module Test.First.Helpers where

import Test.Helpers

scenarioFlavorText entry = campaignI18n $ scope "first" $ scope entry $ p "body"
""",
        "Test/Second/Helpers.hs": """module Test.Second.Helpers where

import Test.Helpers

scenarioFlavorText entry = campaignI18n $ scope "second" $ scope entry $ p "body"
""",
        "Test/FirstScenario.hs": """module Test.FirstScenario where

import Test.First.Helpers

run = flavor $ scenarioFlavorText "introOne"
""",
        "Test/SecondScenario.hs": """module Test.SecondScenario where

import Test.Second.Helpers

run = flavor $ scenarioFlavorText "introTwo"
""",
    }
    keys = keys_of(modules)
    check(
        "standalone.testCampaign.first.introOne.body" in keys
        and "standalone.testCampaign.second.introTwo.body" in keys,
        f"cross-module helper arguments not resolved: {sorted(keys)}",
    )
    check(
        "standalone.testCampaign.first.introTwo.body" not in keys
        and "standalone.testCampaign.second.introOne.body" not in keys,
        "two helpers with the same name shared their call sites",
    )


def test_a_condition_that_already_chose_a_branch_is_not_fanned_out() -> None:
    keys = keys_of(
        {
            "Test/Helpers.hs": HELPERS,
            "Test/Scenario.hs": """module Test.Scenario where

import Test.Helpers

run headedWest = campaignI18n $ do
  scope (if headedWest then "west" else "east") $ story $ do
    if headedWest then li "westOnly" else li "eastOnly"
""",
        }
    )
    check(
        keys == {"standalone.testCampaign.west.westOnly", "standalone.testCampaign.east.eastOnly"},
        f"a correlated condition was fanned out: {sorted(keys)}",
    )


def test_a_scope_primitive_does_not_scope_arguments_it_never_takes() -> None:
    keys = keys_of(
        {
            "Test/Helpers.hs": HELPERS,
            "Test/Scenario.hs": """module Test.Scenario where

import Test.Helpers

run iid = campaignI18n $ scope "resolutions" $ chooseOneM iid do
  unscoped (countVar 1 $ labeled' "rootLabel") do
    gainXp iid attrs (ikey "scopedKey") 2
""",
        }
    )
    check(
        "standalone.testCampaign.resolutions.scopedKey" in keys,
        f"`unscoped` leaked into an argument it does not take: {sorted(keys)}",
    )
    check(
        "label.rootLabel" in keys,
        f"`unscoped` did not reset the scope of the label it wraps: {sorted(keys)}",
    )


def test_a_wrapper_emits_under_the_scope_it_pushes() -> None:
    keys = keys_of(
        {
            "Test/Helpers.hs": HELPERS,
            "Test/Scenario.hs": """module Test.Scenario where

import Test.Helpers

run = campaignI18n $ scope "someScenario" $ additionalRules "openSky"
""",
        }
    )
    for expected in (
        "standalone.testCampaign.someScenario.rules.openSky.title",
        "standalone.testCampaign.someScenario.rules.openSky.body",
    ):
        check(expected in keys, f"wrapper key missing {expected}: {sorted(keys)}")


def test_a_presentation_emitter_keeps_a_module_from_being_waived() -> None:
    # The waiver is gone entirely, but a module that only emits through a
    # presentation modifier must still be a hard failure when it cannot parse.
    try:
        registry_of(
            {
                "Test/Broken.hs": (
                    "module Test.Broken where\n\n"
                    "run = case x of\n"
                    "  ) -> p.green \"broken.key\"\n"
                )
            }
        )
    except SystemExit as error:
        check("does not parse" in str(error), f"unexpected failure message: {error}")
        return
    FAILURES.append("an unparsable module emitting through p.green was accepted")


def test_committed_registry_properties() -> None:
    artifact = json.loads(ARTIFACT.read_text(encoding="utf-8"))
    keys = {entry["key"] for entry in artifact["keys"]}
    by_key = {entry["key"]: entry for entry in artifact["keys"]}

    token_faces = artifact["variableTypes"][extractor.CHAOS_TOKEN_FACE_TYPE]["values"]
    for face in (
        "skull",
        "cultist",
        "tablet",
        "elderThing",
        "autoFail",
        "elderSign",
        "curse",
        "bless",
        "frost",
        "blood",
    ):
        check(face in token_faces, f"chaos token icon face missing from registry: {face}")
    for numeric_face in ("+1", "0", "-1", "-2", "-3", "-4", "-5", "-6", "-7", "-8"):
        check(numeric_face not in token_faces, f"numeric chaos token published as an icon: {numeric_face}")
    check(
        "openCustomFaces" not in artifact["variableTypes"][extractor.CHAOS_TOKEN_FACE_TYPE],
        "chaos-token icon variable type must not claim custom-face coverage",
    )
    add_token_types = {variable["name"]: variable["type"] for variable in by_key["addToken"]["variables"]}
    check(
        add_token_types.get("token") == extractor.CHAOS_TOKEN_FACE_TYPE,
        f"addToken token variable is not typed as {extractor.CHAOS_TOKEN_FACE_TYPE}: {add_token_types}",
    )
    test_types = {variable["name"]: variable["type"] for variable in by_key["label.test"]["variables"]}
    check(
        test_types.get("skill") == extractor.SKILL_ICON_TYPE,
        f"label.test skill variable is not typed as {extractor.SKILL_ICON_TYPE}: {test_types}",
    )
    discard_faces = artifact["variableTypes"][extractor.SKILL_ICON_DISCARD_TYPE]["values"]
    check(
        discard_faces == ["willpower", "intellect", "combat", "agility", "wild"],
        f"discardCardsWithMatchingIcons supported labels changed: {discard_faces}",
    )
    discard_types = {
        variable["name"]: variable["type"]
        for variable in by_key["label.discardCardsWithMatchingIcons"]["variables"]
    }
    check(
        discard_types.get("skillIcon") == extractor.SKILL_ICON_DISCARD_TYPE,
        f"discardCardsWithMatchingIcons skillIcon variable is not typed as {extractor.SKILL_ICON_DISCARD_TYPE}: {discard_types}",
    )
    check(
        by_key["label.discardCardsWithMatchingIcons"].get("sites") == 5,
        f"discardCardsWithMatchingIcons should be supported by the five PassengerCar sites: {by_key['label.discardCardsWithMatchingIcons']}",
    )

    # Keys the review named as reachable but missing from the earlier registry.
    for key in (
        "standalone.guardiansOfTheAbyss.label.theHourOfJudgment.destroyNeith",
        "theScarletKeys.congressOfTheKeys.resolutions.resolution1.body",
        "theScarletKeys.sanguineShadows.intro.intro4",
        "theCircleUndone.epilogue.survivedTheWatchersEmbrace",
        "theDunwichLegacy.interlude2.body",
    ):
        check(key in keys, f"registry lost a cited key: {key}")

    # `chooseAmount' iid "additionalActions" "$actions"` is an amount label, not
    # a root-level key.
    check("actions" not in keys, "the $actions false positive is back")
    check("choice.actions" in keys, "the amount label lost its choice. scope")

    check(
        "unparsedModules" not in artifact,
        "the registry still carries a parse waiver; every module must parse",
    )

    classes = set(artifact["dynamicSites"]["byClass"])
    check(
        classes <= set(extractor.DYNAMIC_CLASSES),
        f"unresolved sites carry classes outside the closed vocabulary: {sorted(classes)}",
    )
    for site in artifact["dynamicSites"]["sites"]:
        check("class" in site and "reason" in site, f"unclassified site {site}")


TESTS = (
    test_point_free_alias_through_a_direct_import,
    test_alias_reached_through_an_aliased_module_reexport,
    test_a_restricted_reexport_does_not_create_a_rival_alias,
    test_scope_template_resolved_from_the_call_site,
    test_conditional_and_local_binding_scopes_fan_out,
    test_a_local_helpers_key_parameter_is_read_from_its_call_sites,
    test_presentation_modifiers_keep_the_key_and_shift_validate,
    test_withvars_declares_the_names_the_backend_sends,
    test_withvars_token_literals_are_typed_as_chaos_token_faces,
    test_token_face_proof_rejects_dynamic_expressions,
    test_if_token_literals_are_typed_as_chaos_token_faces,
    test_case_token_literals_are_typed_as_chaos_token_faces,
    test_guarded_case_token_faces_reject_dynamic_results,
    test_icon_variable_type_conflicts_downgrade_to_unknown,
    test_icon_variable_type_conflicts_downgrade_to_unknown_when_proven_site_is_first,
    test_icon_variable_type_conflicts_downgrade_across_modules,
    test_key_name_string_wrapped_key_variable_is_text,
    test_key_name_text_proof_rejects_another_callee,
    test_key_name_text_proof_rejects_a_shadowed_key_name,
    test_key_name_text_proof_requires_the_string_wrapper,
    test_key_name_text_proof_rejects_mixed_call_sites,
    test_key_name_text_proof_rejects_mixed_call_sites_when_unproven_site_is_first,
    test_skill_icon_registry_matches_skill_var_when_every_value_has_a_glyph,
    test_skill_var_falls_back_to_text_when_icon_registry_is_incomplete,
    test_skill_var_falls_back_to_text_when_i18n_source_is_missing,
    test_skill_var_falls_back_to_text_when_emitted_set_mismatches_the_registry,
    test_skill_var_falls_back_to_text_for_an_extra_value_without_a_glyph,
    test_skill_var_falls_back_to_text_when_a_branch_does_not_use_withvar,
    test_skill_var_falls_back_to_text_when_the_variable_name_is_not_literal,
    test_skill_var_falls_back_to_text_when_definition_has_a_guard,
    test_skill_icon_var_registry_matches_i18n_when_every_value_has_a_glyph,
    test_skill_icon_var_falls_back_to_text_for_an_extra_value_without_a_glyph,
    test_skill_icon_var_falls_back_to_text_when_a_branch_does_not_use_withvar,
    test_skill_icon_var_falls_back_to_text_when_variable_name_is_not_literal,
    test_skill_icon_var_falls_back_to_text_when_definition_has_a_guard,
    test_replaced_skill_key_is_typed_as_a_skill_icon_when_skill_type_key_is_proven,
    test_replaced_skill_key_is_typed_inside_the_canonical_skill_type_key_module,
    test_seal_key_is_typed_as_a_seal_icon_when_seal_kind_is_proven,
    test_seal_falls_back_to_text_for_a_module_level_to_scope_shadow,
    test_seal_falls_back_to_text_for_a_module_level_tshow_shadow,
    test_seal_falls_back_to_text_for_a_let_bound_to_scope_shadow,
    test_seal_falls_back_to_text_for_a_where_bound_tshow_shadow,
    test_discard_matching_icons_is_typed_from_literal_skill_icon_call_sites,
    test_discard_matching_icons_is_typed_from_a_qualified_skill_icon_var_call_site,
    test_discard_matching_icons_falls_back_to_text_for_a_variable_skill_icon,
    test_discard_matching_icons_falls_back_to_text_without_a_glyph_for_a_literal_site,
    test_discard_matching_icons_falls_back_to_text_for_an_ikey_variable_skill_icon,
    test_discard_matching_icons_falls_back_to_text_for_a_local_helper_forwarded_key,
    test_discard_matching_icons_falls_back_to_text_when_a_helper_literal_is_called_under_a_variable,
    test_discard_matching_icons_falls_back_to_text_for_an_unknown_literal,
    test_discard_matching_icons_falls_back_to_text_for_mixed_literal_and_variable_sites,
    test_discard_matching_icons_falls_back_to_text_for_an_inner_skill_icon_override,
    test_discard_matching_icons_falls_back_to_text_for_nested_skill_icon_var_binders,
    test_discard_matching_icons_falls_back_to_text_for_a_module_level_skill_icon_var_shadow,
    test_discard_matching_icons_falls_back_to_text_for_a_let_bound_skill_icon_var_shadow,
    test_discard_matching_icons_falls_back_to_text_for_a_where_bound_skill_icon_var_shadow,
    test_replaced_skill_falls_back_to_text_for_an_extra_wildcard_skill_type_key_arm,
    test_replaced_skill_falls_back_to_text_for_a_computed_skill_type_key_arm,
    test_replaced_skill_falls_back_to_text_for_a_locally_defined_skill_type_key,
    test_replaced_skill_falls_back_to_text_for_a_let_bound_skill_type_key_shadow,
    test_replaced_skill_falls_back_to_text_for_a_where_bound_skill_type_key_shadow,
    test_replaced_skill_falls_back_to_text_when_a_glyph_is_missing,
    test_replaced_skill_keyvar_with_an_unproven_value_stays_text,
    test_seal_falls_back_to_text_when_a_glyph_is_missing,
    test_seal_falls_back_to_text_for_an_extra_constructor,
    test_seal_falls_back_to_text_for_a_constructor_with_fields,
    test_seal_falls_back_to_text_for_a_custom_show_instance,
    test_seal_falls_back_to_text_when_to_scope_changes_single_word_transform,
    test_seal_keyvar_with_an_unproven_value_stays_text,
    test_amount_labels_are_choice_scoped_and_readers_are_ignored,
    test_a_module_that_cannot_be_parsed_but_emits_keys_is_a_hard_failure,
    test_same_named_local_scopes_do_not_share_their_call_sites,
    test_a_local_helper_is_scoped_by_its_call_site,
    test_a_top_level_helper_is_resolved_across_modules_but_not_across_definitions,
    test_a_condition_that_already_chose_a_branch_is_not_fanned_out,
    test_a_scope_primitive_does_not_scope_arguments_it_never_takes,
    test_a_wrapper_emits_under_the_scope_it_pushes,
    test_a_presentation_emitter_keeps_a_module_from_being_waived,
    test_committed_registry_properties,
)


def main() -> int:
    for test in TESTS:
        try:
            test()
        except Exception as error:  # noqa: BLE001 - report, do not abort the suite
            FAILURES.append(f"{test.__name__} raised {error!r}")

    if FAILURES:
        for failure in FAILURES:
            print(f"backend-i18n-tests: {failure}", file=sys.stderr)
        return 1
    print(f"backend-i18n-tests: {len(TESTS)} tests passed")
    return 0


if __name__ == "__main__":
    sys.exit(main())
