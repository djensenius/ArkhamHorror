#!/usr/bin/env python3
# /// script
# requires-python = ">=3.14"
# dependencies = [
#   "jsonschema==4.26.0",
# ]
# ///

import json
from pathlib import Path

from jsonschema import FormatChecker
from jsonschema.validators import validator_for


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

for relative_path in documents:
    require((ROOT / relative_path).is_file(), f"Missing contract file: {relative_path}")

for path in sorted(CONTRACTS.rglob("*.json")):
    load_json(path)

for fixture in fixtures:
    fixture_path = fixture["path"]
    schema_path = fixture["schema"]
    require((ROOT / fixture_path).is_file(), f"Missing fixture: {fixture_path}")
    require(schema_path in documents, f"Fixture schema is not a document: {schema_path}")

    schema = load_json(ROOT / schema_path)
    instance = load_json(ROOT / fixture_path)
    validator_class = validator_for(schema)
    validator_class.check_schema(schema)
    validator = validator_class(schema, format_checker=FormatChecker())
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

print(
    f"Validated {len(documents)} contract documents and {len(fixtures)} fixtures."
)
