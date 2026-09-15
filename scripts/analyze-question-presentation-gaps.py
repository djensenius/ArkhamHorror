#!/usr/bin/env python3
"""Cluster authoritative question choices that lack semantic presentation.

The analyzer accepts standalone question fixtures, PublicGame snapshots, game
WebSocket frames, or directories containing any mixture of those artifacts.
It trusts a presentation descriptor only when protocol version, question
version, question kind, choice count, descriptor kind, uniqueness, and
source-index bounds all agree with the raw question.
"""

from __future__ import annotations

import argparse
import json
import sys
from collections import Counter, defaultdict
from dataclasses import dataclass
from pathlib import Path
from typing import Any, Iterable

import strict_json

MAX_ARTIFACT_BYTES = 64 * 1024 * 1024
WRAPPER_TAGS = {"PayCostQuestion", "QuestionLabel", "QuestionWithSource"}
QUESTION_TAGS = {
    "ChooseAmounts",
    "ChooseDeck",
    "ChooseExchangeAmounts",
    "ChooseN",
    "ChooseOne",
    "ChooseOneAtATime",
    "ChooseOneAtATimeWithAuto",
    "ChooseOneFromEach",
    "ChooseOneWizard",
    "ChoosePaymentAmounts",
    "ChooseSome",
    "ChooseSome1",
    "ChooseUpToN",
    "ChooseUpgradeDeck",
    "ContinueCampaign",
    "DropDown",
    "PayCostQuestion",
    "PickCampaignSettings",
    "PickCampaignSpecific",
    "PickDestiny",
    "PickScenarioSettings",
    "PickScenarioSpecific",
    "PickSupplies",
    "PlayerWindowChooseOne",
    "QuestionLabel",
    "QuestionWithSource",
    "Read",
    "WindowChooseOne",
}
READ_CHOICE_TAGS = {
    "BasicReadChoices",
    "BasicReadChoicesN",
    "BasicReadChoicesUpToN",
    "LeadInvestigatorMustDecide",
}
PRESENTATION_QUESTION_KINDS = {
    "ChooseN": "chooseN",
    "ChooseOne": "chooseOne",
    "ChooseOneAtATime": "chooseOneAtATime",
    "ChooseSome": "chooseSome",
    "ChooseSome1": "chooseSome",
    "ChooseUpToN": "chooseUpToN",
    "PlayerWindowChooseOne": "playerWindowChooseOne",
    "Read": "read",
    "WindowChooseOne": "windowChooseOne",
}
PRESENTATION_CHOICE_KINDS = {
    "advanceAct",
    "advanceAgenda",
    "applySkillTestResults",
    "chooseTarget",
    "drawCard",
    "endTurn",
    "engage",
    "evade",
    "fight",
    "gainResource",
    "investigate",
    "localizedLabel",
    "skipTriggers",
    "startSkillTest",
    "useAbility",
}


@dataclass(frozen=True, order=True)
class GapKey:
    root_tag: str
    choice_tag: str
    ability_type: str
    card_code: str
    source_type: str
    target_type: str
    component_type: str


@dataclass(frozen=True)
class Example:
    path: str
    pointer: str
    player_id: str | None
    source_index: int


@dataclass
class Analysis:
    counts: Counter[GapKey]
    examples: dict[GapKey, list[Example]]
    diagnostics: list[str]
    question_count: int
    choice_count: int
    described_choice_count: int


def require(condition: bool, message: str) -> None:
    if not condition:
        raise ValueError(message)


def escape_pointer_token(token: object) -> str:
    return str(token).replace("~", "~0").replace("/", "~1")


def child_pointer(pointer: str, token: object) -> str:
    return f"{pointer}/{escape_pointer_token(token)}"


def strict_load(path: Path) -> Any:
    if path.is_symlink():
        raise ValueError(f"refusing symlink artifact: {path}")
    if not path.is_file():
        raise ValueError(f"not a regular artifact file: {path}")
    size = path.stat().st_size
    if size > MAX_ARTIFACT_BYTES:
        raise ValueError(
            f"artifact exceeds {MAX_ARTIFACT_BYTES} byte limit ({size} bytes): {path}"
        )
    return strict_json.strict_json_loads(path.read_bytes(), source=str(path))


def artifact_paths(inputs: Iterable[Path]) -> list[Path]:
    resolved: set[Path] = set()
    for input_path in inputs:
        if input_path.is_symlink():
            raise ValueError(f"refusing symlink input: {input_path}")
        if input_path.is_dir():
            for candidate in input_path.rglob("*.json"):
                if candidate.is_symlink():
                    continue
                if candidate.is_file():
                    resolved.add(candidate.resolve())
        elif input_path.is_file():
            resolved.add(input_path.resolve())
        else:
            raise ValueError(f"input does not exist: {input_path}")
    return sorted(resolved, key=lambda path: str(path))


def unwrap_question(
    question: object,
) -> tuple[str, list[object], tuple[object, ...]] | None:
    current = question
    choice_path: list[object] = []
    while isinstance(current, dict) and current.get("tag") in WRAPPER_TAGS:
        current = current.get("question")
        choice_path.append("question")

    if not isinstance(current, dict):
        return None
    root_tag = current.get("tag")
    if not isinstance(root_tag, str):
        return None

    choices = current.get("choices")
    if isinstance(choices, list):
        return root_tag, choices, tuple(choice_path + ["choices"])

    if root_tag == "Read":
        read_choices = current.get("readChoices")
        if isinstance(read_choices, dict) and read_choices.get("tag") in READ_CHOICE_TAGS:
            contents = read_choices.get("contents")
            if isinstance(contents, list):
                if read_choices.get("tag") == "BasicReadChoicesN":
                    if len(contents) == 2 and isinstance(contents[1], list):
                        return (
                            root_tag,
                            contents[1],
                            tuple(choice_path + ["readChoices", "contents", 1]),
                        )
                return (
                    root_tag,
                    contents,
                    tuple(choice_path + ["readChoices", "contents"]),
                )
    return root_tag, [], tuple(choice_path)


def nested_tag(value: object, *path: str) -> str:
    current = value
    for component in path:
        if not isinstance(current, dict):
            return ""
        current = current.get(component)
    if isinstance(current, dict):
        tag = current.get("tag")
        return tag if isinstance(tag, str) else ""
    return ""


def ability_type(choice: dict[str, object]) -> str:
    ability = choice.get("ability")
    if not isinstance(ability, dict):
        return ""
    ability_kind = ability.get("type")
    if not isinstance(ability_kind, dict):
        return ""
    tags: list[str] = []
    while isinstance(ability_kind, dict):
        tag = ability_kind.get("tag")
        if not isinstance(tag, str):
            break
        tags.append(tag)
        nested = ability_kind.get("abilityType")
        if not isinstance(nested, dict):
            break
        ability_kind = nested
    return "/".join(tags)


def gap_key(root_tag: str, choice: object) -> GapKey:
    if not isinstance(choice, dict):
        return GapKey(root_tag, "<non-object>", "", "", "", "", "")
    choice_tag = choice.get("tag")
    if not isinstance(choice_tag, str):
        choice_tag = "<missing>"
    ability = choice.get("ability")
    card_code = ""
    source_type = ""
    if isinstance(ability, dict):
        candidate = ability.get("cardCode")
        if isinstance(candidate, str):
            card_code = candidate
        source_type = nested_tag(ability, "source")
    if not card_code:
        candidate = choice.get("cardCode")
        if isinstance(candidate, str):
            card_code = candidate
    if not source_type:
        source_type = nested_tag(choice, "source")
    return GapKey(
        root_tag=root_tag,
        choice_tag=choice_tag,
        ability_type=ability_type(choice),
        card_code=card_code,
        source_type=source_type,
        target_type=nested_tag(choice, "target"),
        component_type=nested_tag(choice, "component"),
    )


def trusted_source_indices(
    presentation: object,
    *,
    expected_version: int | None,
    expected_kind: str,
    choice_count: int,
    context: str,
    diagnostics: list[str],
) -> set[int]:
    if presentation is None:
        return set()
    if not isinstance(presentation, dict):
        diagnostics.append(f"{context}: presentation is not an object")
        return set()
    protocol_version = presentation.get("protocolVersion")
    if (
        isinstance(protocol_version, bool)
        or not isinstance(protocol_version, int)
        or protocol_version != 1
    ):
        diagnostics.append(f"{context}: presentation protocolVersion is not 1")
        return set()
    question_version = presentation.get("questionVersion")
    if (
        expected_version is not None
        and (
            isinstance(question_version, bool)
            or not isinstance(question_version, int)
            or question_version != expected_version
        )
    ):
        diagnostics.append(
            f"{context}: presentation questionVersion does not match {expected_version}"
        )
        return set()
    if presentation.get("questionKind") != expected_kind:
        diagnostics.append(
            f"{context}: presentation questionKind does not match {expected_kind}"
        )
        return set()
    presented_choice_count = presentation.get("choiceCount")
    if (
        isinstance(presented_choice_count, bool)
        or not isinstance(presented_choice_count, int)
        or presented_choice_count != choice_count
    ):
        diagnostics.append(
            f"{context}: presentation choiceCount does not match {choice_count}"
        )
        return set()
    descriptors = presentation.get("choices")
    if not isinstance(descriptors, list):
        diagnostics.append(f"{context}: presentation choices is not an array")
        return set()

    indices: list[int] = []
    valid = True
    for descriptor_index, descriptor in enumerate(descriptors):
        if not isinstance(descriptor, dict):
            diagnostics.append(
                f"{context}: descriptor {descriptor_index} is not an object"
            )
            valid = False
            continue
        descriptor_kind = descriptor.get("kind")
        if (
            not isinstance(descriptor_kind, str)
            or descriptor_kind not in PRESENTATION_CHOICE_KINDS
        ):
            diagnostics.append(
                f"{context}: descriptor {descriptor_index} kind is not a supported "
                "v1 choice kind"
            )
            valid = False
        source_index = descriptor.get("sourceIndex")
        if isinstance(source_index, bool) or not isinstance(source_index, int):
            diagnostics.append(
                f"{context}: descriptor {descriptor_index} sourceIndex is not an integer"
            )
            valid = False
            continue
        if not 0 <= source_index < choice_count:
            diagnostics.append(
                f"{context}: descriptor sourceIndex {source_index} is outside 0..{choice_count - 1}"
            )
            valid = False
        indices.append(source_index)
    if len(indices) != len(set(indices)):
        diagnostics.append(f"{context}: presentation contains duplicate sourceIndex values")
        valid = False
    return set(indices) if valid else set()


def analyze_question(
    analysis: Analysis,
    *,
    path: Path,
    pointer: str,
    player_id: str | None,
    question: object,
    presentation: object,
    expected_version: int | None,
) -> None:
    unwrapped = unwrap_question(question)
    if unwrapped is None:
        return
    root_tag, choices, choice_path = unwrapped
    analysis.question_count += 1
    analysis.choice_count += len(choices)
    context = f"{path}:{pointer or '/'}"
    expected_kind = PRESENTATION_QUESTION_KINDS.get(root_tag, "unsupported")
    trusted = trusted_source_indices(
        presentation,
        expected_version=expected_version,
        expected_kind=expected_kind,
        choice_count=0 if expected_kind == "unsupported" else len(choices),
        context=context,
        diagnostics=analysis.diagnostics,
    )
    analysis.described_choice_count += len(trusted)

    for source_index, choice in enumerate(choices):
        if source_index in trusted:
            continue
        key = gap_key(root_tag, choice)
        analysis.counts[key] += 1
        examples = analysis.examples[key]
        if len(examples) < 3:
            choice_pointer = pointer
            for component in choice_path:
                choice_pointer = child_pointer(choice_pointer, component)
            examples.append(
                Example(
                    path=str(path),
                    pointer=child_pointer(choice_pointer, source_index),
                    player_id=player_id,
                    source_index=source_index,
                )
            )


def walk_artifact(analysis: Analysis, path: Path, document: object) -> None:
    processed_question_ids: set[int] = set()

    def walk(value: object, pointer: str) -> None:
        if isinstance(value, dict):
            processed_child_keys: set[str] = set()
            questions = value.get("question")
            scenario_steps = value.get("scenarioSteps")
            if (
                isinstance(questions, dict)
                and isinstance(scenario_steps, int)
                and not isinstance(scenario_steps, bool)
            ):
                presentations = value.get("questionPresentation")
                if presentations is not None and not isinstance(presentations, dict):
                    analysis.diagnostics.append(
                        f"{path}:{child_pointer(pointer, 'questionPresentation')}: "
                        "questionPresentation is not an object"
                    )
                    presentations = {}
                for player_id, question in sorted(questions.items()):
                    if not isinstance(player_id, str):
                        continue
                    processed_question_ids.add(id(question))
                    presentation = (
                        presentations.get(player_id)
                        if isinstance(presentations, dict)
                        else None
                    )
                    analyze_question(
                        analysis,
                        path=path,
                        pointer=child_pointer(
                            child_pointer(pointer, "question"), player_id
                        ),
                        player_id=player_id,
                        question=question,
                        presentation=presentation,
                        expected_version=scenario_steps,
                    )
                processed_child_keys.update(("question", "questionPresentation"))
            if id(value) not in processed_question_ids:
                unwrapped = unwrap_question(value)
                if (
                    unwrapped is not None
                    and isinstance(value.get("tag"), str)
                    and value.get("tag") in QUESTION_TAGS
                ):
                    analyze_question(
                        analysis,
                        path=path,
                        pointer=pointer,
                        player_id=None,
                        question=value,
                        presentation=None,
                        expected_version=None,
                    )
                    processed_question_ids.add(id(value))
                    if value.get("tag") in WRAPPER_TAGS:
                        processed_child_keys.add("question")
            for key, child in value.items():
                if key in processed_child_keys:
                    continue
                walk(child, child_pointer(pointer, key))
        elif isinstance(value, list):
            for index, child in enumerate(value):
                walk(child, child_pointer(pointer, index))

    walk(document, "")


def analyze(paths: Iterable[Path]) -> Analysis:
    analysis = empty_analysis()
    for path in paths:
        walk_artifact(analysis, path, strict_load(path))
    analysis.diagnostics.sort()
    return analysis


def empty_analysis() -> Analysis:
    return Analysis(
        counts=Counter(),
        examples=defaultdict(list),
        diagnostics=[],
        question_count=0,
        choice_count=0,
        described_choice_count=0,
    )


def row_for(key: GapKey, count: int, examples: list[Example]) -> dict[str, object]:
    return {
        "count": count,
        "rootTag": key.root_tag,
        "choiceTag": key.choice_tag,
        "abilityType": key.ability_type or None,
        "cardCode": key.card_code or None,
        "sourceType": key.source_type or None,
        "targetType": key.target_type or None,
        "componentType": key.component_type or None,
        "examples": [
            {
                "path": example.path,
                "pointer": example.pointer or "/",
                "playerId": example.player_id,
                "sourceIndex": example.source_index,
            }
            for example in examples
        ],
    }


def result_document(analysis: Analysis) -> dict[str, object]:
    ordered = sorted(
        analysis.counts.items(),
        key=lambda item: (-item[1], item[0]),
    )
    return {
        "summary": {
            "questions": analysis.question_count,
            "choices": analysis.choice_count,
            "describedChoices": analysis.described_choice_count,
            "undescribedChoices": sum(analysis.counts.values()),
            "clusters": len(analysis.counts),
        },
        "diagnostics": analysis.diagnostics,
        "clusters": [
            row_for(key, count, analysis.examples[key]) for key, count in ordered
        ],
    }


def print_text(result: dict[str, object]) -> None:
    summary = result["summary"]
    assert isinstance(summary, dict)
    print(
        "questions={questions} choices={choices} described={describedChoices} "
        "undescribed={undescribedChoices} clusters={clusters}".format(**summary)
    )
    diagnostics = result["diagnostics"]
    assert isinstance(diagnostics, list)
    for diagnostic in diagnostics:
        print(f"diagnostic: {diagnostic}", file=sys.stderr)
    clusters = result["clusters"]
    assert isinstance(clusters, list)
    for cluster in clusters:
        assert isinstance(cluster, dict)
        facets = [
            f"root={cluster['rootTag']}",
            f"choice={cluster['choiceTag']}",
        ]
        for key in (
            "abilityType",
            "cardCode",
            "sourceType",
            "targetType",
            "componentType",
        ):
            value = cluster[key]
            if value is not None:
                facets.append(f"{key}={value}")
        print(f"{cluster['count']:>6}  {' '.join(facets)}")
        examples = cluster["examples"]
        assert isinstance(examples, list)
        for example in examples:
            assert isinstance(example, dict)
            player = (
                f" player={example['playerId']}" if example["playerId"] else ""
            )
            print(
                f"        {example['path']}#{example['pointer']}"
                f"{player} sourceIndex={example['sourceIndex']}"
            )


def run_self_test() -> None:
    question = {
        "tag": "PlayerWindowChooseOne",
        "choices": [
            {
                "tag": "ComponentLabel",
                "component": {
                    "tag": "InvestigatorComponent",
                    "investigatorId": "c01001",
                    "tokenType": "ResourceToken",
                },
                "messages": [],
            },
            {
                "tag": "AbilityLabel",
                "investigatorId": "c01001",
                "ability": {
                    "source": {"tag": "ActSource", "contents": "c01108"},
                    "cardCode": "c01108",
                    "type": {
                        "tag": "Objective",
                        "abilityType": {"tag": "FastAbility'"},
                    },
                },
                "messages": [],
            },
        ],
    }
    diagnostics: list[str] = []
    trusted = trusted_source_indices(
        {
            "protocolVersion": 1,
            "questionVersion": 34,
            "questionKind": "playerWindowChooseOne",
            "choiceCount": 2,
            "choices": [{"sourceIndex": 1, "kind": "advanceAct"}],
        },
        expected_version=34,
        expected_kind="playerWindowChooseOne",
        choice_count=2,
        context="self-test",
        diagnostics=diagnostics,
    )
    require(trusted == {1}, f"unexpected trusted indices: {trusted}")
    require(not diagnostics, f"unexpected diagnostics: {diagnostics}")
    root_tag, choices, _ = unwrap_question(
        {"tag": "QuestionWithSource", "question": question}
    ) or ("", [], ())
    require(root_tag == "PlayerWindowChooseOne", f"unexpected root tag: {root_tag}")
    require(len(choices) == 2, f"unexpected choice count: {len(choices)}")
    key = gap_key(root_tag, choices[1])
    require(key.ability_type == "Objective/FastAbility'", f"bad ability type: {key}")
    require(key.source_type == "ActSource", f"bad source type: {key}")

    unsupported_analysis = empty_analysis()
    analyze_question(
        unsupported_analysis,
        path=Path("unsupported.json"),
        pointer="",
        player_id=None,
        question={
            "tag": "ChooseDeck",
            "choices": [{"tag": "Label", "label": "deck", "messages": []}],
        },
        presentation={
            "protocolVersion": 1,
            "questionVersion": 1,
            "questionKind": "chooseOne",
            "choiceCount": 1,
            "choices": [{"sourceIndex": 0}],
        },
        expected_version=1,
    )
    require(
        unsupported_analysis.described_choice_count == 0,
        "unsupported raw kinds must not trust another semantic question kind",
    )
    require(
        sum(unsupported_analysis.counts.values()) == 1,
        "unsupported raw kinds must remain gaps after a kind mismatch",
    )
    require(
        unsupported_analysis.diagnostics
        == [
            "unsupported.json:/: presentation questionKind does not match unsupported"
        ],
        f"unexpected unsupported-kind diagnostics: {unsupported_analysis.diagnostics}",
    )

    auto_choice_analysis = empty_analysis()
    analyze_question(
        auto_choice_analysis,
        path=Path("auto-choice.json"),
        pointer="",
        player_id=None,
        question={
            "tag": "ChooseOneAtATimeWithAuto",
            "label": "$fixture.resolveAll",
            "choices": question["choices"],
        },
        presentation={
            "protocolVersion": 1,
            "questionVersion": 34,
            "questionKind": "unsupported",
            "choiceCount": 0,
            "choices": [],
        },
        expected_version=34,
    )
    require(
        (
            auto_choice_analysis.question_count,
            auto_choice_analysis.choice_count,
            auto_choice_analysis.described_choice_count,
            sum(auto_choice_analysis.counts.values()),
        )
        == (1, 2, 0, 2),
        "auto-prefixed answer indexes must remain unsupported and visible as gaps",
    )
    require(
        not auto_choice_analysis.diagnostics,
        f"unexpected auto-choice diagnostics: {auto_choice_analysis.diagnostics}",
    )

    public_game_analysis = empty_analysis()
    walk_artifact(
        public_game_analysis,
        Path("public-game.json"),
        {
            "scenarioSteps": 34,
            "question": {
                "player": {
                    "tag": "QuestionWithSource",
                    "question": question,
                }
            },
            "questionPresentation": {
                "player": {
                    "protocolVersion": 1,
                    "questionVersion": 34,
                    "questionKind": "playerWindowChooseOne",
                    "choiceCount": 2,
                    "choices": [{"sourceIndex": 1, "kind": "advanceAct"}],
                }
            },
        },
    )
    require(
        (
            public_game_analysis.question_count,
            public_game_analysis.choice_count,
            public_game_analysis.described_choice_count,
            sum(public_game_analysis.counts.values()),
        )
        == (1, 2, 1, 1),
        f"PublicGame questions were recounted: {public_game_analysis}",
    )

    wrapper_analysis = empty_analysis()
    walk_artifact(
        wrapper_analysis,
        Path("wrapper.json"),
        {"tag": "QuestionWithSource", "question": question},
    )
    require(
        (
            wrapper_analysis.question_count,
            wrapper_analysis.choice_count,
            wrapper_analysis.described_choice_count,
            sum(wrapper_analysis.counts.values()),
        )
        == (1, 2, 0, 2),
        f"standalone wrapped question was recounted: {wrapper_analysis}",
    )

    unknown_kind_analysis = empty_analysis()
    analyze_question(
        unknown_kind_analysis,
        path=Path("unknown-kind.json"),
        pointer="",
        player_id=None,
        question=question,
        presentation={
            "protocolVersion": 1,
            "questionVersion": 34,
            "questionKind": "playerWindowChooseOne",
            "choiceCount": 2,
            "choices": [{"sourceIndex": 1, "kind": "futureChoice"}],
        },
        expected_version=34,
    )
    require(
        (
            unknown_kind_analysis.described_choice_count,
            sum(unknown_kind_analysis.counts.values()),
        )
        == (0, 2),
        "an unknown descriptor kind must not suppress any raw choice gaps",
    )
    require(
        unknown_kind_analysis.diagnostics
        == [
            "unknown-kind.json:/: descriptor 0 kind is not a supported "
            "v1 choice kind"
        ],
        f"unexpected descriptor-kind diagnostics: {unknown_kind_analysis.diagnostics}",
    )

    diagnostics = []
    trusted = trusted_source_indices(
        {
            "protocolVersion": 1,
            "questionVersion": 33,
            "questionKind": "playerWindowChooseOne",
            "choiceCount": 2,
            "choices": [{"sourceIndex": 1}],
        },
        expected_version=34,
        expected_kind="playerWindowChooseOne",
        choice_count=2,
        context="self-test",
        diagnostics=diagnostics,
    )
    require(not trusted, "mismatched question version must fail closed")
    require(len(diagnostics) == 1, f"unexpected mismatch diagnostics: {diagnostics}")

    for field, value, expected_message in (
        ("protocolVersion", True, "protocolVersion is not 1"),
        ("questionKind", "chooseOne", "questionKind does not match"),
        ("choiceCount", 1, "choiceCount does not match"),
    ):
        presentation = {
            "protocolVersion": 1,
            "questionVersion": 34,
            "questionKind": "playerWindowChooseOne",
            "choiceCount": 2,
            "choices": [{"sourceIndex": 1}],
        }
        presentation[field] = value
        diagnostics = []
        trusted = trusted_source_indices(
            presentation,
            expected_version=34,
            expected_kind="playerWindowChooseOne",
            choice_count=2,
            context="self-test",
            diagnostics=diagnostics,
        )
        require(not trusted, f"mismatched {field} must fail closed")
        require(
            len(diagnostics) == 1 and expected_message in diagnostics[0],
            f"unexpected {field} diagnostics: {diagnostics}",
        )

    for choices_value, expected_message in (
        (
            [
                {"sourceIndex": 1, "kind": "advanceAct"},
                {"sourceIndex": 1, "kind": "advanceAct"},
            ],
            "duplicate sourceIndex",
        ),
        (
            [{"sourceIndex": 2, "kind": "advanceAct"}],
            "outside 0..1",
        ),
    ):
        diagnostics = []
        trusted = trusted_source_indices(
            {
                "protocolVersion": 1,
                "questionVersion": 34,
                "questionKind": "playerWindowChooseOne",
                "choiceCount": 2,
                "choices": choices_value,
            },
            expected_version=34,
            expected_kind="playerWindowChooseOne",
            choice_count=2,
            context="self-test",
            diagnostics=diagnostics,
        )
        require(not trusted, "invalid source indexes must fail closed")
        require(
            any(expected_message in diagnostic for diagnostic in diagnostics),
            f"unexpected source-index diagnostics: {diagnostics}",
        )


def parse_args() -> argparse.Namespace:
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("paths", nargs="*", type=Path)
    parser.add_argument("--format", choices=("text", "json"), default="text")
    parser.add_argument("--fail-on-gaps", action="store_true")
    parser.add_argument("--self-test", action="store_true")
    return parser.parse_args()


def main() -> int:
    args = parse_args()
    try:
        if args.self_test:
            run_self_test()
            if not args.paths:
                print("question presentation gap analyzer self-test passed")
                return 0
        require(bool(args.paths), "at least one artifact path is required")
        analysis = analyze(artifact_paths(args.paths))
        result = result_document(analysis)
        if args.format == "json":
            print(json.dumps(result, indent=2, sort_keys=True))
        else:
            print_text(result)
        return 1 if args.fail_on_gaps and analysis.counts else 0
    except (OSError, ValueError, strict_json.StrictJSONError) as error:
        print(f"question presentation gap analysis failed: {error}", file=sys.stderr)
        return 2


if __name__ == "__main__":
    raise SystemExit(main())
