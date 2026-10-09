#!/usr/bin/env python3
"""Build or check the native campaign/scenario catalog artifact.

The artifact is generated from the same static frontend data used by the web
create-game flow. It intentionally exposes i18n keys, not the English names in
those web data files; the generated default-locale name registry lets the
locale catalog resolve those keys while keeping the web data as the source of
truth.
"""

from __future__ import annotations

import argparse
import ast
import copy
import hashlib
import json
import re
from pathlib import Path

import strict_json
from jsonschema import FormatChecker
from jsonschema.validators import validator_for
from referencing import Registry, Resource

ROOT = Path(__file__).resolve().parents[1]
FRONTEND_DATA = ROOT / "frontend" / "src" / "arkham" / "data"
BACKEND_CATALOG = ROOT / "backend" / "arkham-api" / "data" / "campaign-catalog.json"
SCHEMA_PATH = ROOT / "contracts" / "schemas" / "campaign-catalog.schema.json"
NAME_REGISTRY = ROOT / "frontend" / "src" / "locales" / "en" / "gameBoard" / "catalogNames.json"
SCHEMA_VERSION = "1.0.0"
GENERATOR_NAME = "arkham-campaign-catalog"
GENERATOR_VERSION = "1.0.0"
ENDPOINT = "/api/v1/arkham/campaign-catalog"

DATA_SOURCES = (
    "frontend/src/arkham/data/campaigns.json",
    "frontend/src/arkham/data/scenarios.ts",
    "frontend/src/arkham/data/side-stories.json",
    "frontend/src/arkham/deckRestrictions.ts",
)
GAME_BOARD_MESSAGES = "frontend/src/locales/en/gameBoard/gameBoard.ts"
GENERATOR_SOURCES = (
    "scripts/build-campaign-catalog.py",
    "scripts/strict_json.py",
)

_NAME_RE = re.compile(r"^import\s+(\w+)\s+from\s+'@/arkham/data/([^']+\.json)'", re.MULTILINE)
_SPREAD_RE = re.compile(r"\.\.\.(\w+)")
_ID_RE = re.compile(r"^[A-Za-z0-9:_-]+$")
_DIFFICULTIES = {"Easy", "Standard", "Hard", "Expert"}
_TOKEN_FACES = {
    "AutoFail",
    "BloodToken",
    "Cultist",
    "ElderSign",
    "ElderThing",
    "FrostToken",
    "MinusEight",
    "MinusFive",
    "MinusFour",
    "MinusOne",
    "MinusSeven",
    "MinusSix",
    "MinusThree",
    "MinusTwo",
    "PlusOne",
    "Skull",
    "Tablet",
    "Zero",
}
_SOURCE_BYTE_OVERRIDES: dict[str, bytes] = {}


def require(condition: object, message: str) -> None:
    if not condition:
        raise SystemExit(f"campaign-catalog: {message}")


def canonical_bytes(value: object) -> bytes:
    return (json.dumps(value, indent=2, ensure_ascii=False, sort_keys=False) + "\n").encode("utf-8")


def sha256_hex(data: bytes) -> str:
    return hashlib.sha256(data).hexdigest()


def fileset_digest(entries: list[tuple[str, bytes]]) -> str:
    h = hashlib.sha256()
    for path, data in sorted(entries):
        h.update(path.encode("utf-8"))
        h.update(b"\0")
        h.update(sha256_hex(data).encode("ascii"))
        h.update(b"\n")
    return h.hexdigest()


def read_json(relative: str) -> object:
    return strict_json.strict_json_loads(source_bytes(relative), source=relative)


def load_campaign_catalog_schema() -> dict:
    schema = strict_json.strict_json_load_path(SCHEMA_PATH)
    require(isinstance(schema, dict), "campaign catalog schema must be a JSON object")
    validator_for(schema).check_schema(schema)
    return schema


def validate_campaign_catalog(value: object, schema: dict, *, source: str) -> None:
    validator_class = validator_for(schema)
    registry = Registry().with_resource(schema["$id"], Resource.from_contents(schema))
    validator = validator_class(schema, format_checker=FormatChecker(), registry=registry)
    errors = sorted(validator.iter_errors(value), key=lambda error: tuple(map(str, error.absolute_path)))
    if errors:
        details = "; ".join(
            f"{'/'.join(map(str, error.absolute_path)) or '<root>'}: {error.message}"
            for error in errors[:10]
        )
        raise SystemExit(f"campaign-catalog: {source} does not match {SCHEMA_PATH.relative_to(ROOT).as_posix()}: {details}")


def source_bytes(relative: str) -> bytes:
    if relative in _SOURCE_BYTE_OVERRIDES:
        return _SOURCE_BYTE_OVERRIDES[relative]
    path = ROOT / relative
    require(path.is_file() and not path.is_symlink(), f"{relative} is not a regular file")
    return path.read_bytes()


def source_digest_entries(paths: list[str] | tuple[str, ...]) -> list[dict[str, str]]:
    return [
        {"path": path, "sha256": sha256_hex(source_bytes(path))}
        for path in sorted(paths)
    ]


def validate_id(kind: str, value: object) -> str:
    require(isinstance(value, str) and _ID_RE.fullmatch(value), f"{kind} id must be an identifier string, got {value!r}")
    return value


def validate_name(kind: str, value: object) -> str:
    require(isinstance(value, str) and value.strip() == value and value, f"{kind} name must be a non-empty trimmed string")
    return value


def validate_release_flags(kind: str, item: dict) -> None:
    for flag in ("alpha", "beta", "dev"):
        require(flag not in item or isinstance(item[flag], bool), f"{kind} {item.get('id')} {flag} must be boolean")


def validate_card_code(kind: str, value: object) -> str:
    require(isinstance(value, str) and re.fullmatch(r"[0-9]{5}", value), f"{kind} card code must be a five-digit string, got {value!r}")
    return value


def validate_difficulty_levels(kind: str, item: dict) -> None:
    levels = item.get("difficultyLevels")
    if levels is None:
        return
    require(isinstance(levels, dict) and levels, f"{kind} {item.get('id')} difficultyLevels must be a non-empty object")
    for difficulty, tokens in levels.items():
        require(difficulty in _DIFFICULTIES, f"{kind} {item.get('id')} has unknown difficulty {difficulty!r}")
        require(isinstance(tokens, list) and tokens, f"{kind} {item.get('id')} {difficulty} token bag must be a non-empty array")
        for token in tokens:
            require(isinstance(token, str) and token, f"{kind} {item.get('id')} token faces must be strings")
            require(token in _TOKEN_FACES, f"{kind} {item.get('id')} has unknown token face {token!r}")


def without_english_name(item: dict, name_key: str, *, return_to_name_key: str | None = None) -> dict:
    result = copy.deepcopy(item)
    result.pop("name", None)
    result["nameKey"] = name_key
    if "returnToName" in result:
        result.pop("returnToName")
        require(return_to_name_key is not None, f"{item.get('id')} returnToName has no key")
        result["returnToNameKey"] = return_to_name_key
    return result


def assign_key(container: dict, path: list[str], value: str) -> None:
    current = container
    for segment in path[:-1]:
        current = current.setdefault(segment, {})
        require(isinstance(current, dict), f"name registry path {'.'.join(path)} collides")
    leaf = path[-1]
    require(leaf not in current or current[leaf] == value, f"duplicate name key {'.'.join(path)}")
    current[leaf] = value


def name_key(*parts: str) -> str:
    return "catalogNames." + ".".join(parts)


def add_name(registry: dict, key: str, value: str) -> None:
    parts = key.split(".")
    require(parts[0] == "catalogNames", f"unexpected generated name key {key}")
    assign_key(registry, parts[1:], value)


def parse_scenario_imports() -> list[str]:
    text = (FRONTEND_DATA / "scenarios.ts").read_text(encoding="utf-8")
    imports = {var: path for var, path in _NAME_RE.findall(text)}
    require(imports, "scenarios.ts declared no JSON imports")
    spreads = _SPREAD_RE.findall(text)
    require(spreads, "scenarios.ts declared no spread entries")
    paths: list[str] = []
    for var in spreads:
        if var in {"sideStories", "homebrewScenarios"}:
            continue
        require(var in imports, f"scenarios.ts spreads unknown import {var}")
        paths.append(f"frontend/src/arkham/data/{imports[var]}")
    require(paths, "scenarios.ts did not contribute any built-in scenario files")
    return paths


def parse_quoted_code_list(scenario_id: str, raw_codes: str) -> list[str]:
    raw_items = [item.strip() for item in raw_codes.split(",")]
    if raw_items and raw_items[-1] == "":
        raw_items.pop()
    require(raw_items, f"challenge scenario {scenario_id} required investigator code list is empty")
    literal_re = re.compile(r"(?:'[^'\\]*(?:\\.[^'\\]*)*')|(?:\"[^\"\\]*(?:\\.[^\"\\]*)*\")")
    codes = []
    for raw in raw_items:
        require(
            literal_re.fullmatch(raw) is not None,
            f"challenge scenario {scenario_id} required investigator code list item is not a quoted literal: {raw!r}",
        )
        codes.append(validate_card_code(f"challenge scenario {scenario_id}", ast.literal_eval(raw)))
    require(len(codes) == len(set(codes)), f"challenge scenario {scenario_id} required investigator code list has duplicates")
    return codes


def parse_required_investigator_codes() -> dict[str, list[str]]:
    text = source_bytes("frontend/src/arkham/deckRestrictions.ts").decode("utf-8")
    block_match = re.search(r"const\s+challengeScenarioInvestigators\s*:[^{]+\{(?P<body>.*?)\n\}", text, re.S)
    require(block_match, "deckRestrictions.ts declared no challengeScenarioInvestigators map")
    body = block_match.group("body")
    result: dict[str, list[str]] = {}
    entry_re = re.compile(
        r"['\"](?P<scenario>[0-9]{5})['\"]\s*:\s*requiredInvestigator\(\s*"
        r"(?P<name>(?:'[^'\\]*(?:\\.[^'\\]*)*')|(?:\"[^\"\\]*(?:\\.[^\"\\]*)*\"))\s*,\s*"
        r"\[(?P<codes>[^\]]*)\]",
        re.S,
    )
    declared_key_count = len(re.findall(r"['\"][0-9]{5}['\"]\s*:", body))
    required_investigator_call_count = len(re.findall(r":\s*requiredInvestigator\(", body))
    matches = list(entry_re.finditer(body))
    require(
        declared_key_count == len(matches) and required_investigator_call_count == len(matches),
        "deckRestrictions.ts challengeScenarioInvestigators contains entries that are not quoted five-digit keys mapped to requiredInvestigator(name, [quoted codes])",
    )
    for match in matches:
        scenario_id = validate_id("challenge scenario", match.group("scenario"))
        codes = parse_quoted_code_list(scenario_id, match.group("codes"))
        require(scenario_id not in result, f"duplicate challenge scenario restriction {scenario_id}")
        result[scenario_id] = codes
    require(result, "deckRestrictions.ts challengeScenarioInvestigators yielded no restrictions")
    return result


def campaign_catalog(registry: dict) -> list[dict]:
    raw = read_json("frontend/src/arkham/data/campaigns.json")
    require(isinstance(raw, list) and raw, "campaigns.json must be a non-empty array")
    seen: set[str] = set()
    campaigns = []
    for item in raw:
        require(isinstance(item, dict), "campaign entries must be objects")
        cid = validate_id("campaign", item.get("id"))
        require(cid not in seen, f"duplicate campaign id {cid}")
        seen.add(cid)
        title = validate_name(f"campaign {cid}", item.get("name"))
        validate_release_flags("campaign", item)
        validate_difficulty_levels("campaign", item)
        key = name_key("campaigns", cid, "name")
        add_name(registry, key, title)
        entry = without_english_name(item, key)
        return_to = entry.get("returnTo")
        if return_to is not None:
            require(isinstance(return_to, dict), f"campaign {cid} returnTo must be an object")
            validate_id(f"campaign {cid} returnTo", return_to.get("id"))
            validate_release_flags(f"campaign {cid} returnTo", return_to)
            rt_key = name_key("campaigns", cid, "returnTo", "name")
            add_name(registry, rt_key, f"Return to {title}")
            return_to["nameKey"] = rt_key
        campaigns.append(entry)
    return campaigns


def scenario_catalog(registry: dict, scenario_paths: list[str]) -> list[dict]:
    scenarios = []
    seen: set[str] = set()
    for relative in scenario_paths:
        raw = read_json(relative)
        require(isinstance(raw, list), f"{relative} must be an array")
        for item in raw:
            require(isinstance(item, dict), f"{relative} entries must be objects")
            sid = validate_id("scenario", item.get("id"))
            require(sid not in seen, f"duplicate scenario id {sid}")
            seen.add(sid)
            title = validate_name(f"scenario {sid}", item.get("name"))
            validate_release_flags("scenario", item)
            validate_difficulty_levels("scenario", item)
            key = name_key("scenarios", sid, "name")
            add_name(registry, key, title)
            return_to_key = None
            if "returnToName" in item:
                return_to_key = name_key("scenarios", sid, "returnTo", "name")
                add_name(registry, return_to_key, validate_name(f"scenario {sid} returnToName", item["returnToName"]))
            scenarios.append(without_english_name(item, key, return_to_name_key=return_to_key))
    require(scenarios, "no scenarios were generated")
    return scenarios


def side_story_catalog(registry: dict, required_investigator_codes: dict[str, list[str]]) -> list[dict]:
    raw = read_json("frontend/src/arkham/data/side-stories.json")
    require(isinstance(raw, list) and raw, "side-stories.json must be a non-empty array")
    seen: set[str] = set()
    stories = []
    for item in raw:
        require(isinstance(item, dict), "side-story entries must be objects")
        sid = validate_id("side story", item.get("id"))
        require(sid not in seen, f"duplicate side-story id {sid}")
        seen.add(sid)
        title = validate_name(f"side story {sid}", item.get("name"))
        validate_release_flags("side story", item)
        validate_difficulty_levels("side story", item)
        key = name_key("sideStories", sid, "name")
        add_name(registry, key, title)
        entry = without_english_name(item, key)
        if "requiredInvestigator" in entry:
            codes = required_investigator_codes.get(sid)
            require(codes is not None, f"side-story {sid} requiredInvestigator has no deck restriction code source")
            entry["requiredInvestigatorCodes"] = codes
        if "scenarios" in entry:
            require(isinstance(entry["scenarios"], list) and entry["scenarios"], f"side-story {sid} scenarios must be a non-empty array")
            for part in entry["scenarios"]:
                require(isinstance(part, dict), f"side-story {sid} scenario parts must be objects")
                part_id = validate_id(f"side-story {sid} part", part.get("id"))
                part_title = validate_name(f"side-story {sid} part {part_id}", part.get("name"))
                part.pop("name", None)
                part_key = name_key("sideStories", sid, "parts", part_id, "name")
                add_name(registry, part_key, part_title)
                part["nameKey"] = part_key
        stories.append(entry)
    return stories


def build_catalog() -> tuple[dict, dict]:
    scenario_paths = parse_scenario_imports()
    registry: dict = {}
    campaigns = campaign_catalog(registry)
    scenarios = scenario_catalog(registry, scenario_paths)
    required_investigator_codes = parse_required_investigator_codes()
    side_stories = side_story_catalog(registry, required_investigator_codes)

    data_source_paths = sorted({*DATA_SOURCES, *scenario_paths})
    data_entries = [(path, source_bytes(path)) for path in data_source_paths]
    generator_entries = [(path, source_bytes(path)) for path in GENERATOR_SOURCES]
    schema_entries = [("contracts/schemas/campaign-catalog.schema.json", SCHEMA_PATH.read_bytes())]
    provenance_basis = {
        "generator": GENERATOR_NAME,
        "generatorVersion": GENERATOR_VERSION,
        "schemaVersion": SCHEMA_VERSION,
        "dataSourcesSha256": fileset_digest(data_entries),
        "generatorSha256": fileset_digest(generator_entries),
        "schemasSha256": fileset_digest(schema_entries),
    }
    provenance_sha = sha256_hex(canonical_bytes(provenance_basis))
    catalog = {
        "schemaVersion": SCHEMA_VERSION,
        "catalogRevision": f"1.{provenance_sha[:32]}",
        "endpoint": ENDPOINT,
        "digestAlgorithm": "sha256",
        "campaigns": campaigns,
        "scenarios": scenarios,
        "sideStories": side_stories,
        "provenance": {
            **provenance_basis,
            "sha256": provenance_sha,
            "sources": source_digest_entries(data_source_paths),
        },
    }
    output_sha = sha256_hex(canonical_bytes(catalog))
    catalog["provenance"]["outputSha256"] = output_sha
    return catalog, registry


def collect_name_keys(value: object) -> list[str]:
    keys: list[str] = []
    if isinstance(value, dict):
        for key, item in value.items():
            if key.endswith("NameKey") or key == "nameKey":
                require(isinstance(item, str), f"{key} must be a string")
                keys.append(item)
            else:
                keys.extend(collect_name_keys(item))
    elif isinstance(value, list):
        for item in value:
            keys.extend(collect_name_keys(item))
    return keys


def resolves_locale_key(messages: dict, key: str) -> bool:
    current: object = messages
    for segment in key.split("."):
        if not isinstance(current, dict) or segment not in current:
            return False
        current = current[segment]
    return isinstance(current, str) and bool(current)


def assert_game_board_mounts_catalog_names() -> None:
    text = source_bytes(GAME_BOARD_MESSAGES).decode("utf-8")
    require(
        "import catalogNames from '@/locales/en/gameBoard/catalogNames.json'" in text,
        "frontend/src/locales/en/gameBoard/gameBoard.ts must import catalogNames.json as catalogNames",
    )
    export_match = re.search(r"export\s+default\s*\{(?P<body>.*)\}\s*$", text, re.S)
    require(export_match, "frontend/src/locales/en/gameBoard/gameBoard.ts must export a gameBoard message object")
    body = export_match.group("body")
    members = [member.strip() for member in body.split(",")]
    require(
        "catalogNames" in members,
        "frontend/src/locales/en/gameBoard/gameBoard.ts must mount catalogNames with shorthand, not rename or wrap it",
    )


def assert_name_keys_resolve_in_registry(catalog: dict, registry: dict, *, source: str) -> None:
    require(isinstance(registry, dict), f"{source} must be a JSON object")
    require("catalogNames" not in registry, f"{source} must be mounted as catalogNames by gameBoard.ts, not wrapped")
    messages = {"catalogNames": registry}
    missing = sorted(key for key in set(collect_name_keys(catalog)) if not resolves_locale_key(messages, key))
    require(not missing, f"generated name keys do not resolve in {source}: " + ", ".join(missing[:10]))


def assert_generated_name_keys_resolve(catalog: dict, registry: dict) -> None:
    assert_game_board_mounts_catalog_names()
    assert_name_keys_resolve_in_registry(catalog, registry, source=NAME_REGISTRY.relative_to(ROOT).as_posix())


def assert_committed_name_keys_resolve(catalog: dict) -> None:
    assert_game_board_mounts_catalog_names()
    registry = strict_json.strict_json_load_path(NAME_REGISTRY)
    assert_name_keys_resolve_in_registry(catalog, registry, source=NAME_REGISTRY.relative_to(ROOT).as_posix())


def with_source_override(relative: str, data: bytes):
    class Override:
        def __enter__(self):
            _SOURCE_BYTE_OVERRIDES[relative] = data

        def __exit__(self, exc_type, exc, tb):
            _SOURCE_BYTE_OVERRIDES.pop(relative, None)
            return False

    return Override()


def write_if_changed(path: Path, data: bytes) -> None:
    path.parent.mkdir(parents=True, exist_ok=True)
    if path.exists() and path.read_bytes() == data:
        return
    path.write_bytes(data)


def check_file(path: Path, expected: bytes) -> list[str]:
    if not path.exists():
        return [f"missing {path.relative_to(ROOT).as_posix()}"]
    actual = path.read_bytes()
    if actual != expected:
        return [f"stale {path.relative_to(ROOT).as_posix()}"]
    return []


def expect_system_exit(description: str, action) -> None:
    try:
        action()
    except SystemExit:
        return
    raise SystemExit(f"campaign-catalog: self-test failure: {description} was accepted")


def run_self_test() -> None:
    catalog, registry = build_catalog()
    assert_generated_name_keys_resolve(catalog, registry)
    schema = load_campaign_catalog_schema()
    validate_campaign_catalog(catalog, schema, source="self-test generated campaign catalog")
    require(catalog["campaigns"][0]["nameKey"].startswith("catalogNames.campaigns."), "self-test campaign name key missing")
    require("The Night of the Zealot" in canonical_bytes(registry).decode("utf-8"), "self-test name registry missing source title")

    invalid_catalog = copy.deepcopy(catalog)
    invalid_catalog["schemaVersion"] = "0.0.0"
    expect_system_exit(
        "schema-violating campaign catalog",
        lambda: validate_campaign_catalog(invalid_catalog, schema, source="self-test schema violation"),
    )

    wrapped_registry = {"catalogNames": copy.deepcopy(registry)}
    expect_system_exit(
        "wrapped catalogNames registry",
        lambda: assert_name_keys_resolve_in_registry(catalog, wrapped_registry, source="self-test wrapped registry"),
    )

    renamed_mount = source_bytes(GAME_BOARD_MESSAGES).decode("utf-8").replace(
        "catalogNames, ultimatumsAndBoons",
        "campaignCatalogNames: catalogNames, ultimatumsAndBoons",
        1,
    )
    require(renamed_mount != source_bytes(GAME_BOARD_MESSAGES).decode("utf-8"), "self-test could not mutate gameBoard catalogNames mount")
    with with_source_override(GAME_BOARD_MESSAGES, renamed_mount.encode("utf-8")):
        expect_system_exit("renamed catalogNames gameBoard mount", assert_game_board_mounts_catalog_names)

    # A web data edit must move the provenance that feeds the catalog revision.
    campaign_path = "frontend/src/arkham/data/campaigns.json"
    original = source_bytes(campaign_path)
    mutated = original.replace(b"The Night of the Zealot", b"The Night of the Zealot!", 1)
    require(mutated != original, "self-test could not mutate the campaign source title")
    source_paths = [entry["path"] for entry in catalog["provenance"]["sources"]]
    original_entries = [(path, source_bytes(path)) for path in source_paths]
    mutated_entries = [(path, mutated if path == campaign_path else source_bytes(path)) for path in source_paths]
    require(
        fileset_digest(original_entries) != fileset_digest(mutated_entries),
        "self-test failure: a web data name change did not move the source digest",
    )

    malformed_campaigns = copy.deepcopy(read_json(campaign_path))
    require(isinstance(malformed_campaigns, list) and malformed_campaigns, "self-test expected campaigns array")
    malformed_campaigns[0]["name"] = ""
    with with_source_override(campaign_path, canonical_bytes(malformed_campaigns)):
        expect_system_exit("malformed input", build_catalog)

    deck_restrictions_path = "frontend/src/arkham/deckRestrictions.ts"
    deck_restrictions = source_bytes(deck_restrictions_path).decode("utf-8")
    missing_map = deck_restrictions.replace("const challengeScenarioInvestigators", "const missingChallengeScenarioInvestigators", 1)
    require(missing_map != deck_restrictions, "self-test could not remove challengeScenarioInvestigators map")
    with with_source_override(deck_restrictions_path, missing_map.encode("utf-8")):
        expect_system_exit("missing challengeScenarioInvestigators map", parse_required_investigator_codes)

    constant_key_entry = deck_restrictions.replace("'90065': requiredInvestigator", "montereyJackScenario: requiredInvestigator", 1)
    require(constant_key_entry != deck_restrictions, "self-test could not introduce constant-name challengeScenarioInvestigators entry")
    with with_source_override(deck_restrictions_path, constant_key_entry.encode("utf-8")):
        expect_system_exit("constant-name challengeScenarioInvestigators entry", parse_required_investigator_codes)

    spread_code_item = deck_restrictions.replace("'90065': requiredInvestigator('Monterey Jack', ['08007', '90062'])", "'90065': requiredInvestigator('Monterey Jack', ['08007', ...MONTEREY_CODES])", 1)
    require(spread_code_item != deck_restrictions, "self-test could not introduce spread code list item")
    with with_source_override(deck_restrictions_path, spread_code_item.encode("utf-8")):
        expect_system_exit("spread challengeScenarioInvestigators code list item", parse_required_investigator_codes)


def main() -> None:
    parser = argparse.ArgumentParser()
    parser.add_argument("--check", action="store_true", help="fail when committed artifacts are stale")
    parser.add_argument("--self-test", action="store_true", help="run generator self-tests")
    args = parser.parse_args()

    if args.self_test:
        run_self_test()
        print("campaign-catalog: self-test passed")
        return

    catalog, registry = build_catalog()
    assert_generated_name_keys_resolve(catalog, registry)
    schema = load_campaign_catalog_schema()
    validate_campaign_catalog(catalog, schema, source="generated campaign catalog")
    catalog_bytes = canonical_bytes(catalog)
    registry_bytes = canonical_bytes(registry)

    if args.check:
        failures = []
        failures.extend(check_file(BACKEND_CATALOG, catalog_bytes))
        failures.extend(check_file(NAME_REGISTRY, registry_bytes))
        served_catalog = strict_json.strict_json_load_path(BACKEND_CATALOG)
        validate_campaign_catalog(served_catalog, schema, source=BACKEND_CATALOG.relative_to(ROOT).as_posix())
        assert_committed_name_keys_resolve(served_catalog)
        require(not failures, "generated artifacts are stale: " + ", ".join(failures))
        print(
            "campaign-catalog: verified "
            f"{len(catalog['campaigns'])} campaigns, "
            f"{len(catalog['scenarios'])} scenarios, "
            f"{len(catalog['sideStories'])} side stories at {catalog['catalogRevision']}"
        )
        return

    write_if_changed(BACKEND_CATALOG, catalog_bytes)
    write_if_changed(NAME_REGISTRY, registry_bytes)
    print(f"campaign-catalog: wrote {catalog['catalogRevision']}")


if __name__ == "__main__":
    main()
