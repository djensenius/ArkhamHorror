#!/usr/bin/env python3
# /// script
# requires-python = ">=3.14"
# dependencies = [
#   "jsonschema==4.26.0",
#   "referencing==0.37.0",
# ]
# ///

import copy
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
schemas_by_path: dict[str, dict] = {}
for relative_path in documents:
    if not relative_path.endswith(".json"):
        continue
    schema = load_json(ROOT / relative_path)
    schemas_by_path[relative_path] = schema
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


def flatten_errors(errors):
    for error in errors:
        yield error
        yield from flatten_errors(error.context or ())


# ---------------------------------------------------------------------------
# Negative regression fixtures
#
# Each negative fixture is a *single deterministic mutation* of a real,
# already schema-validated positive fixture value (looked up by JSON
# Pointer), rather than a hand-copied duplicate payload. This keeps every
# negative fixture provably derived from -- and unable to silently drift
# from -- a genuine production-encoded positive, and keeps the manifest diff
# small even for deeply-nested targets (e.g. a single chaos-bag token field)
# instead of repeating hundreds of unrelated lines of an unmodified sibling
# structure.
# ---------------------------------------------------------------------------


def resolve_json_pointer(document, pointer: str):
    """Resolve an RFC 6901 JSON Pointer against an in-memory document."""
    require(pointer == "" or pointer.startswith("/"), f"Invalid JSON Pointer: {pointer!r}")
    value = document
    for raw_token in pointer.split("/")[1:]:
        token = raw_token.replace("~1", "/").replace("~0", "~")
        if isinstance(value, list):
            value = value[int(token)]
        elif isinstance(value, dict):
            require(token in value, f"JSON Pointer segment {token!r} not found (pointer {pointer!r})")
            value = value[token]
        else:
            raise SystemExit(f"Cannot descend into scalar with pointer segment {token!r} ({pointer!r})")
    return value


def apply_mutation(value, mutation: dict):
    """Apply a single {op, pointer[, value]} mutation to a deep copy of `value`.

    A pointer of "" (JSON Pointer root) replaces the whole value itself --
    used for top-level wrong-type negatives (e.g. a Movement object replaced
    outright by a bare string) -- and only supports op "replace".
    """
    if mutation["pointer"] == "":
        require(mutation["op"] == "replace", "Root-pointer mutations must use op 'replace'")
        require("value" in mutation, "Mutation op 'replace' requires a 'value'")
        return copy.deepcopy(mutation["value"])

    mutated = copy.deepcopy(value)
    pointer = mutation["pointer"]
    require(pointer.startswith("/"), f"Mutation pointer must be non-root: {pointer!r}")
    *parent_tokens, last_raw = pointer.split("/")[1:]
    parent = mutated
    for raw_token in parent_tokens:
        token = raw_token.replace("~1", "/").replace("~0", "~")
        parent = parent[int(token)] if isinstance(parent, list) else parent[token]
    last = last_raw.replace("~1", "/").replace("~0", "~")

    op = mutation["op"]
    if op == "remove":
        if isinstance(parent, list):
            del parent[int(last)]
        else:
            del parent[last]
    elif op == "replace":
        require("value" in mutation, "Mutation op 'replace' requires a 'value'")
        if isinstance(parent, list):
            parent[int(last)] = mutation["value"]
        else:
            parent[last] = mutation["value"]
    elif op == "add":
        require("value" in mutation, "Mutation op 'add' requires a 'value'")
        if isinstance(parent, list):
            # RFC 6902 JSON-Patch "add" semantics for arrays: insert a new
            # element at the given index (shifting later elements right), or
            # append when the index is "-". This is intentionally distinct
            # from "replace", which overwrites the existing element in place.
            if last == "-":
                parent.append(mutation["value"])
            else:
                parent.insert(int(last), mutation["value"])
        else:
            parent[last] = mutation["value"]
    else:
        raise SystemExit(f"Unknown mutation op: {op!r}")
    return mutated


def extract_branch_schema(schema: dict, branch):
    """Extract a single `oneOf` branch (by $defs name or by array index) as
    its own standalone schema, rebased onto the parent schema's own $id so
    its relative $refs still resolve through the shared registry. This lets a
    negative fixture assert precisely against the one production branch it
    targets, instead of also accumulating unrelated "didn't match the other,
    structurally disjoint branch either" diagnostics that a `oneOf` root
    genuinely (and correctly) produces alongside it.
    """
    if isinstance(branch, str):
        require(
            branch in schema.get("$defs", {}),
            f"Unknown schema $defs branch {branch!r}",
        )
        sub = schema["$defs"][branch]
    elif isinstance(branch, int):
        one_of = schema.get("oneOf")
        require(isinstance(one_of, list), "schemaBranch index requires a oneOf-rooted schema")
        sub = one_of[branch]
    else:
        raise SystemExit(f"Unsupported schemaBranch type: {branch!r}")
    rebased = dict(sub)
    if "$id" in schema:
        rebased["$id"] = schema["$id"]
    if "$defs" in schema and "$defs" not in rebased:
        rebased["$defs"] = schema["$defs"]
    return rebased


def make_validator(schema: dict):
    validator_class = validator_for(schema)
    return validator_class(schema, format_checker=FormatChecker(), registry=registry)


def normalize_errors(errors):
    return sorted(
        (tuple(map(str, error.absolute_path)), str(error.validator), error.message)
        for error in errors
    )


def diagnose_negative(schema: dict, branch, instance, expected_errors: list[dict]):
    """Validate `instance` against `schema` (optionally scoped to one `oneOf`
    branch), and return (ok: bool, detail: str) describing whether the
    flattened error set exactly matches `expected_errors` -- every declared
    expectation must be satisfied by exactly one real error, and no
    unexplained extra errors may remain.
    """
    target_schema = extract_branch_schema(schema, branch) if branch is not None else schema
    validator = make_validator(target_schema)
    errors = list(flatten_errors(validator.iter_errors(instance)))

    if not errors:
        return False, "instance unexpectedly validated"

    remaining = list(errors)
    unmatched_expected = []
    for expected in expected_errors:
        match = next(
            (
                error
                for error in remaining
                if list(map(str, error.absolute_path)) == expected["path"]
                and error.validator == expected["keyword"]
                and expected["messageContains"] in error.message
            ),
            None,
        )
        if match is None:
            unmatched_expected.append(expected)
        else:
            remaining.remove(match)

    if unmatched_expected or remaining:
        parts = []
        if unmatched_expected:
            parts.append(f"missing expected: {unmatched_expected}")
        if remaining:
            parts.append(f"unexplained extra errors: {normalize_errors(remaining)}")
        return False, "; ".join(parts)
    return True, "ok"


negative_fixtures = manifest.get("negativeFixtures", [])
positive_fixture_cache: dict[str, object] = {}


def load_base_value(base_positive_fixture: str, base_pointer: str):
    if base_positive_fixture not in positive_fixture_cache:
        positive_fixture_cache[base_positive_fixture] = load_json(ROOT / base_positive_fixture)
    return resolve_json_pointer(positive_fixture_cache[base_positive_fixture], base_pointer)


for fixture in negative_fixtures:
    schema_path = fixture["schema"]
    base_positive_fixture = fixture["basePositiveFixture"]
    base_pointer = fixture["basePointer"]
    mutation = fixture["mutation"]
    expected_errors = fixture["expectedErrors"]
    schema_branch = fixture.get("schemaBranch")
    description = fixture.get("description", "no description")

    require((ROOT / base_positive_fixture).is_file(), f"Missing base positive fixture: {base_positive_fixture}")
    require(
        any(f["path"] == base_positive_fixture for f in fixtures),
        f"basePositiveFixture {base_positive_fixture!r} is not a registered positive fixture",
    )
    require(schema_path in documents, f"Negative fixture schema is not a document: {schema_path}")

    base_value = load_base_value(base_positive_fixture, base_pointer)
    mutated_instance = apply_mutation(base_value, mutation)
    schema = schemas_by_path[schema_path]

    ok, detail = diagnose_negative(schema, schema_branch, mutated_instance, expected_errors)
    require(
        ok,
        f"Negative fixture ({description}) derived from {base_positive_fixture}{base_pointer} "
        f"did not fail {schema_path} as expected: {detail}",
    )

    # When scoped to a single oneOf branch, also confirm the *unscoped* root
    # schema still genuinely rejects the mutated instance overall (the
    # oneOf-wrapper diagnostic itself), without re-enumerating the sibling
    # branches' inherent, unrelated "also doesn't match this disjoint shape"
    # noise.
    if schema_branch is not None:
        root_validator = make_validator(schema)
        root_errors = list(root_validator.iter_errors(mutated_instance))
        require(
            len(root_errors) > 0,
            f"Negative fixture ({description}) unexpectedly validated against the full "
            f"oneOf-rooted {schema_path}",
        )
        if "oneOf" in schema:
            require(
                any(error.validator == "oneOf" for error in root_errors),
                f"Negative fixture ({description}) failed {schema_path} for a reason other "
                f"than the expected oneOf-branch mismatch: {normalize_errors(root_errors)}",
            )


# ---------------------------------------------------------------------------
# Closed-enum boundary checks
#
# For a genuinely closed, all-nullary Haskell sum type (e.g. ActSide = A | B
# | ... | H), the *wire shape* risk (bare string vs. tagged object) is
# already fully retired by a single real production-encoded example. The
# remaining risk is purely "does the schema's enum list every constructor
# correctly" -- which the Haskell ADT declaration itself (cited in each
# schema's description) already answers authoritatively. These checks prove
# the schema's enum boundary members validate and an invented member does
# not, directly against the isolated enum sub-schema (located by JSON
# Pointer into the *schema* document, not an instance) -- they intentionally
# make no claim of being production-fixture-sourced JSON.
# ---------------------------------------------------------------------------

enum_boundary_checks = manifest.get("enumBoundaryChecks", [])
for check in enum_boundary_checks:
    schema_path = check["schema"]
    schema_pointer = check["schemaPointer"]
    description = check.get("description", "no description")
    require(schema_path in documents, f"enumBoundaryChecks schema is not a document: {schema_path}")

    sub_schema = resolve_json_pointer(schemas_by_path[schema_path], schema_pointer)
    require(isinstance(sub_schema, dict) and "enum" in sub_schema, f"{schema_path}{schema_pointer} is not an enum sub-schema")
    validator = make_validator(sub_schema)

    for value in check["validValues"]:
        errors = list(validator.iter_errors(value))
        require(
            not errors,
            f"enumBoundaryChecks ({description}): expected valid value {value!r} to validate "
            f"against {schema_path}{schema_pointer}, got: {normalize_errors(errors)}",
        )
    for value in check["invalidValues"]:
        errors = list(validator.iter_errors(value))
        require(
            any(error.validator == "enum" for error in errors),
            f"enumBoundaryChecks ({description}): expected invalid value {value!r} to fail "
            f"the enum constraint at {schema_path}{schema_pointer}, got: {normalize_errors(errors)}",
        )


def run_self_test() -> None:
    """Prove `diagnose_negative`'s exact-match semantics are actually
    discriminating: taking a real registered negative fixture and adding one
    *extra*, undeclared mutation must make the exact-error-set comparison
    fail, even though the originally-expected error is still present among
    the (now larger) error set.
    """
    require(len(negative_fixtures) > 0, "self-test requires at least one registered negative fixture")
    sample = negative_fixtures[0]
    schema = schemas_by_path[sample["schema"]]
    base_value = load_base_value(sample["basePositiveFixture"], sample["basePointer"])
    instance = apply_mutation(base_value, sample["mutation"])

    require(isinstance(instance, dict), "self-test sample fixture must mutate an object")
    extra_key = "__contract_selftest_unexpected_field__"
    require(extra_key not in instance, "self-test sentinel key collided with real fixture data")
    corrupted = dict(instance)
    corrupted[extra_key] = "unexpected"

    ok, _detail = diagnose_negative(schema, sample.get("schemaBranch"), corrupted, sample["expectedErrors"])
    require(
        not ok,
        "Self-test failure: diagnose_negative() did not detect an extra, undeclared mutation "
        f"(added key {extra_key!r}) on top of {sample['basePositiveFixture']}{sample['basePointer']}; "
        "the exact-error-set check is not actually discriminating.",
    )


def run_apply_mutation_self_test() -> None:
    """Prove `apply_mutation`'s array "add" is RFC 6902 JSON-Patch insertion
    (shifts later elements right, or appends for "-"), and is distinct from
    "replace" (which overwrites the element in place). This is a pure
    in-memory check independent of any registered fixture/schema, since no
    currently-registered negative fixture happens to mutate an array with
    "add".
    """
    base = {"items": ["a", "b", "c"]}

    inserted = apply_mutation(base, {"op": "add", "pointer": "/items/1", "value": "X"})
    require(
        inserted["items"] == ["a", "X", "b", "c"],
        f"apply_mutation add-by-index must insert, not overwrite; got {inserted['items']!r}",
    )

    appended = apply_mutation(base, {"op": "add", "pointer": "/items/-", "value": "Y"})
    require(
        appended["items"] == ["a", "b", "c", "Y"],
        f"apply_mutation add with '-' index must append; got {appended['items']!r}",
    )

    replaced = apply_mutation(base, {"op": "replace", "pointer": "/items/1", "value": "Z"})
    require(
        replaced["items"] == ["a", "Z", "c"],
        f"apply_mutation replace-by-index must overwrite in place; got {replaced['items']!r}",
    )

    require(
        base["items"] == ["a", "b", "c"],
        "apply_mutation must not mutate its input value in place",
    )


run_apply_mutation_self_test()
run_self_test()

print(
    f"Validated {len(documents)} contract documents, {len(fixtures)} fixtures, "
    f"{len(negative_fixtures)} negative regression fixtures (each derived from a real "
    f"positive fixture via a single mutation, plus a self-test proving exact-match precision), "
    f"and {len(enum_boundary_checks)} closed-enum boundary checks."
)
