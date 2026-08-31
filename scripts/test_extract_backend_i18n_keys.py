#!/usr/bin/env python3
# /// script
# requires-python = ">=3.14"
# dependencies = [
#   "tree-sitter==0.25.2",
#   "tree-sitter-haskell==0.23.1",
# ]
# ///

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

import importlib.util
import json
import sys
import tempfile
from pathlib import Path

ROOT = Path(__file__).resolve().parents[1]
EXTRACTOR = ROOT / "scripts" / "extract-backend-i18n-keys.py"
ARTIFACT = ROOT / "backend" / "arkham-api" / "i18n-emitted-keys.json"

_spec = importlib.util.spec_from_file_location("extract_backend_i18n_keys", EXTRACTOR)
extractor = importlib.util.module_from_spec(_spec)
_spec.loader.exec_module(extractor)

FAILURES: list[str] = []


def check(condition: bool, message: str) -> None:
    if not condition:
        FAILURES.append(message)


def registry_of(modules: dict[str, str]) -> dict:
    """Runs the production extractor over a synthetic module set."""
    with tempfile.TemporaryDirectory() as directory:
        library = Path(directory)
        for name, text in modules.items():
            path = library / name
            path.parent.mkdir(parents=True, exist_ok=True)
            path.write_text(text, encoding="utf-8")
        return extractor.build_artifact(library=library)


def keys_of(modules: dict[str, str]) -> set[str]:
    return {entry["key"] for entry in registry_of(modules)["keys"]}


def variables_of(modules: dict[str, str], key: str) -> set[str]:
    for entry in registry_of(modules)["keys"]:
        if entry["key"] == key:
            return {variable["name"] for variable in entry["variables"]}
    return set()


HELPERS = """module Test.Helpers where

import Arkham.I18n

campaignI18n :: (HasI18n => a) -> a
campaignI18n = standaloneI18n "testCampaign"
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


def main() -> int:
    tests = [value for name, value in sorted(globals().items()) if name.startswith("test_")]
    for test in tests:
        try:
            test()
        except Exception as error:  # noqa: BLE001 - report, do not abort the suite
            FAILURES.append(f"{test.__name__} raised {error!r}")

    if FAILURES:
        for failure in FAILURES:
            print(f"backend-i18n-tests: {failure}", file=sys.stderr)
        return 1
    print(f"backend-i18n-tests: {len(tests)} tests passed")
    return 0


if __name__ == "__main__":
    sys.exit(main())
