#!/usr/bin/env python3
# /// script
# requires-python = ">=3.14"
# dependencies = [
#   "jsonschema==4.26.0",
#   "referencing==0.37.0",
# ]
# ///

import json
from pathlib import Path

from jsonschema import FormatChecker
from jsonschema.validators import validator_for
from referencing import Registry, Resource


ROOT = Path(__file__).resolve().parents[1]
CONTRACTS = ROOT / "contracts"


def load_json(path: Path) -> object:
    with path.open(encoding="utf-8") as handle:
        return json.load(handle)


def require(condition: bool, message: str) -> None:
    if not condition:
        raise SystemExit(message)


manifest = load_json(CONTRACTS / "manifest.json")
documents = manifest["documents"]
fixtures = manifest["fixtures"]
capabilities_fixtures = [
    fixture
    for fixture in fixtures
    if fixture.get("schema") == "contracts/schemas/capabilities.schema.json"
]

require(
    len(capabilities_fixtures) == 1,
    "manifest.json must register exactly one capabilities fixture",
)
capabilities_path = ROOT / capabilities_fixtures[0]["path"]
require(capabilities_path.is_file(), f"Missing fixture: {capabilities_fixtures[0]['path']}")
capabilities = load_json(capabilities_path)

require(isinstance(capabilities, dict), "capabilities.json must be an object")
for field in ("schemaRevision", "status", "apiBasePath"):
    require(
        capabilities.get(field) == manifest.get(field),
        f"capabilities.json {field} must match manifest.json",
    )
require(
    capabilities.get("nativeClientMinimumRevision")
    == manifest.get("compatibility", {}).get("nativeClientMinimumRevision"),
    "capabilities.json nativeClientMinimumRevision must match manifest compatibility",
)

for relative_path in documents:
    require((ROOT / relative_path).is_file(), f"Missing contract file: {relative_path}")

for path in sorted(CONTRACTS.rglob("*.json")):
    load_json(path)

registry = Registry()
schema_ids: dict[str, str] = {}
for relative_path in documents:
    if not relative_path.endswith(".json"):
        continue
    schema = load_json(ROOT / relative_path)
    if not isinstance(schema, dict) or "$id" not in schema:
        continue
    schema_id = schema["$id"]
    require(isinstance(schema_id, str), f"Schema $id must be a string: {relative_path}")
    require(
        schema_id not in schema_ids,
        f"Duplicate schema $id {schema_id}: {schema_ids.get(schema_id)} and {relative_path}",
    )
    validator_for(schema).check_schema(schema)
    schema_ids[schema_id] = relative_path
    registry = registry.with_resource(schema_id, Resource.from_contents(schema))

for fixture in fixtures:
    fixture_path = fixture["path"]
    schema_path = fixture["schema"]
    require((ROOT / fixture_path).is_file(), f"Missing fixture: {fixture_path}")
    require(schema_path in documents, f"Fixture schema is not a document: {schema_path}")

    schema = load_json(ROOT / schema_path)
    instance = load_json(ROOT / fixture_path)
    validator_class = validator_for(schema)
    validator_class.check_schema(schema)
    validator = validator_class(
        schema,
        format_checker=FormatChecker(),
        registry=registry,
    )
    errors = sorted(
        validator.iter_errors(instance),
        key=lambda error: tuple(map(str, error.absolute_path)),
    )

    if errors:
        details = "; ".join(
            f"{'/'.join(map(str, error.absolute_path)) or '<root>'}: {error.message}"
            for error in errors
        )
        raise SystemExit(f"{fixture_path} does not match {schema_path}: {details}")

negative_fixtures = manifest.get("negativeFixtures", [])
for fixture in negative_fixtures:
    fixture_path = fixture["path"]
    schema_path = fixture["schema"]
    require((ROOT / fixture_path).is_file(), f"Missing negative fixture: {fixture_path}")
    require(schema_path in documents, f"Negative fixture schema is not a document: {schema_path}")

    schema = load_json(ROOT / schema_path)
    instance = load_json(ROOT / fixture_path)
    validator_class = validator_for(schema)
    validator = validator_class(
        schema,
        format_checker=FormatChecker(),
        registry=registry,
    )
    errors = list(validator.iter_errors(instance))

    require(
        len(errors) > 0,
        f"Negative fixture {fixture_path} unexpectedly validated against {schema_path} "
        f"({fixture.get('description', 'no description')})",
    )

print(
    f"Validated {len(documents)} contract documents, {len(fixtures)} fixtures, "
    f"and {len(negative_fixtures)} negative regression fixtures."
)
