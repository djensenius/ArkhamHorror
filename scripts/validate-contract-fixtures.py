#!/usr/bin/env python3
# /// script
# requires-python = ">=3.14"
# dependencies = [
#   "jsonschema==4.26.0",
#   "referencing==0.37.0",
# ]
# ///

import copy
import re
from pathlib import Path

from jsonschema import FormatChecker
from jsonschema.validators import validator_for
from referencing import Registry, Resource

import strict_json


ROOT = Path(__file__).resolve().parents[1]
CONTRACTS = ROOT / "contracts"

strict_json.run_self_tests()


def load_json(path: Path) -> object:
    return strict_json.strict_json_load_path(path)


def require_schema_document(schema_path, *, entry_kind: str) -> dict:
    """Validate that `schema_path` is a registered document AND that the
    loaded JSON value is actually schema-shaped, not merely any `.json`
    document listed in manifest.json's `documents[]` (e.g.
    `contracts/route-inventory.json`, which is a plain JSON object with no
    schema semantics, would otherwise silently pass an `isinstance(..., dict)`
    check). Every real schema document in `contracts/schemas/` declares a
    root `$schema` draft URI (verified across all of them), so that key's
    presence is used as the schema-shaped marker. Returns the loaded schema
    dict for convenience.
    """
    require(
        isinstance(schema_path, str),
        f"{entry_kind} schema must be a string document path, got {schema_path!r}",
    )
    require(schema_path in documents, f"{entry_kind} schema is not a document: {schema_path}")
    require(schema_path in schemas_by_path, f"{entry_kind} schema document is not JSON: {schema_path}")
    schema = schemas_by_path[schema_path]
    require(isinstance(schema, dict), f"{entry_kind} schema document is not a JSON object: {schema_path}")
    require(
        "$schema" in schema,
        f"{entry_kind} schema document has no root '$schema' marker, so it is not schema-shaped: {schema_path}",
    )
    return schema


def require_entry_keys(entry: object, required_keys: list, *, entry_kind: str, index: int) -> dict:
    """Validate that a manifest list entry (negativeFixtures/enumBoundaryChecks
    element, etc.) is a dict containing every one of ``required_keys`` before
    the caller indexes into it. Manifest entries are hand-authored JSON, so a
    typo or missing field must fail deterministically via ``require`` rather
    than raising a raw KeyError/TypeError once the caller starts indexing.
    """
    require(
        isinstance(entry, dict),
        f"{entry_kind} entry #{index} is not a JSON object: {entry!r}",
    )
    missing = [key for key in required_keys if key not in entry]
    require(
        not missing,
        f"{entry_kind} entry #{index} is missing required key(s) {missing}: {entry!r}",
    )
    return entry


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
schemas_by_path: dict[str, object] = {}
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

for fixture_index, fixture in enumerate(fixtures):
    require_entry_keys(fixture, ["path", "schema"], entry_kind="fixtures", index=fixture_index)
    fixture_path = fixture["path"]
    schema_path = fixture["schema"]
    require((ROOT / fixture_path).is_file(), f"Missing fixture: {fixture_path}")
    schema = require_schema_document(schema_path, entry_kind="Fixture")

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


_ARRAY_INDEX_RE = re.compile(r"^(?:0|[1-9][0-9]*)$")


def _require_array_index(raw_token: str, *, length: int, allow_equal_length: bool) -> int:
    """Parse and bounds-check a JSON-Pointer array token per RFC 6901/6902.

    RFC 6901 array tokens must be either the literal "-" (RFC 6902 "add"
    only, meaning "append") or a non-negative base-10 integer with no
    leading zero (except "0" itself) -- notably, no sign character at all.
    `allow_equal_length` distinguishes RFC 6902 "add" (index may equal
    `length`, meaning append-at-end) from every other op, where the index
    must reference an existing element (`0 <= index < length`). Silently
    clamping or wrapping an out-of-range index would let an invalid
    mutation pointer pass unnoticed.
    """
    require(
        _ARRAY_INDEX_RE.fullmatch(raw_token) is not None,
        f"Array index token must be a non-negative integer with no leading zero "
        f"or sign character: {raw_token!r}",
    )
    index = int(raw_token)
    upper = length if allow_equal_length else length - 1
    require(
        0 <= index <= upper,
        f"Array index {index} out of range for array of length {length} "
        f"(allowed 0..{upper}): {raw_token!r}",
    )
    return index


def resolve_json_pointer(document, pointer: str):
    """Resolve an RFC 6901 JSON Pointer against an in-memory document."""
    require(pointer == "" or pointer.startswith("/"), f"Invalid JSON Pointer: {pointer!r}")
    value = document
    for raw_token in pointer.split("/")[1:]:
        token = raw_token.replace("~1", "/").replace("~0", "~")
        if isinstance(value, list):
            value = value[_require_array_index(token, length=len(value), allow_equal_length=False)]
        elif isinstance(value, dict):
            require(token in value, f"JSON Pointer segment {token!r} not found (pointer {pointer!r})")
            value = value[token]
        else:
            raise SystemExit(f"Cannot descend into scalar with pointer segment {token!r} ({pointer!r})")
    return value


def _require_object_key(parent, key: str, *, pointer: str, must_exist: bool) -> None:
    """Require `parent` to actually be a JSON object (not some other scalar
    reached via an invalid/typo'd mutation pointer), and, for ops where the
    member must already be present (`remove`/`replace`), require `key` to
    exist in it. Fails deterministically via `require()`/`SystemExit`
    instead of leaking a raw `TypeError`/`KeyError` traceback. RFC 6902
    "add" on an object sets the member whether or not it already existed,
    so `must_exist=False` skips the existence check for that op while
    still guarding against a non-dict parent.
    """
    require(
        isinstance(parent, dict),
        f"Cannot apply an object-keyed mutation to a non-object value at "
        f"pointer {pointer!r} (parent is {type(parent).__name__})",
    )
    if must_exist:
        require(key in parent, f"JSON Pointer segment {key!r} not found (pointer {pointer!r})")


def apply_mutation(value, mutation: dict):
    """Apply a single {op, pointer[, value]} mutation to a deep copy of `value`.

    A pointer of "" (JSON Pointer root) replaces the whole value itself --
    used for top-level wrong-type negatives (e.g. a Movement object replaced
    outright by a bare string) -- and only supports op "replace".
    """
    require(
        isinstance(mutation, dict) and "op" in mutation and "pointer" in mutation,
        f"Mutation descriptor must be an object with at least 'op' and 'pointer' keys: {mutation!r}",
    )
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
        if isinstance(parent, list):
            parent = parent[_require_array_index(token, length=len(parent), allow_equal_length=False)]
        elif isinstance(parent, dict):
            _require_object_key(parent, token, pointer=pointer, must_exist=True)
            parent = parent[token]
        else:
            raise SystemExit(f"Cannot descend into scalar with pointer segment {token!r} ({pointer!r})")
    last = last_raw.replace("~1", "/").replace("~0", "~")

    op = mutation["op"]
    if op == "remove":
        if isinstance(parent, list):
            del parent[_require_array_index(last, length=len(parent), allow_equal_length=False)]
        else:
            _require_object_key(parent, last, pointer=pointer, must_exist=True)
            del parent[last]
    elif op == "replace":
        require("value" in mutation, "Mutation op 'replace' requires a 'value'")
        if isinstance(parent, list):
            parent[_require_array_index(last, length=len(parent), allow_equal_length=False)] = mutation["value"]
        else:
            _require_object_key(parent, last, pointer=pointer, must_exist=True)
            parent[last] = mutation["value"]
    elif op == "add":
        require("value" in mutation, "Mutation op 'add' requires a 'value'")
        if isinstance(parent, list):
            # RFC 6902 JSON-Patch "add" semantics for arrays: insert a new
            # element at the given index (shifting later elements right), or
            # append when the index is "-". This is intentionally distinct
            # from "replace", which overwrites the existing element in place.
            # An index equal to len(parent) is valid (append-at-end); any
            # index beyond that is an error rather than a silent append.
            if last == "-":
                parent.append(mutation["value"])
            else:
                parent.insert(_require_array_index(last, length=len(parent), allow_equal_length=True), mutation["value"])
        else:
            # RFC 6902 "add" on an object sets the member whether or not it
            # already existed, so (unlike remove/replace) no key-existence
            # check is required here -- but the parent must still actually
            # be an object, not some other scalar reached via an invalid
            # pointer.
            _require_object_key(parent, last, pointer=pointer, must_exist=False)
            parent[last] = mutation["value"]
    else:
        raise SystemExit(f"Unknown mutation op: {op!r}")
    return mutated


def preserve_schema_dialect(parent: dict, sub: dict) -> dict:
    """Return a copy of `sub` carrying `parent`'s explicit `$schema` draft
    marker, unless `sub` already declares its own. Any standalone schema
    fragment extracted out of a larger document (a `oneOf` branch, a `$defs`
    entry, an isolated enum sub-schema for boundary checks, ...) must be
    validated under the same JSON Schema draft as its parent document;
    otherwise `validator_for()` picks a dialect from jsonschema-library
    defaults instead of the contract's explicit draft (2020-12), which can
    silently change validation behavior across jsonschema versions.
    """
    rebased = dict(sub)
    if "$schema" in parent and "$schema" not in rebased:
        rebased["$schema"] = parent["$schema"]
    return rebased


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
        require(
            not isinstance(branch, bool) and 0 <= branch < len(one_of),
            f"schemaBranch index {branch!r} is out of range for a {len(one_of)}-branch oneOf",
        )
        sub = one_of[branch]
    else:
        raise SystemExit(f"Unsupported schemaBranch type: {branch!r}")

    require(
        isinstance(sub, dict),
        f"schemaBranch {branch!r} does not resolve to a JSON object sub-schema: {sub!r}",
    )
    rebased = dict(sub)
    if "$id" in schema:
        rebased["$id"] = schema["$id"]
    if "$defs" in schema and "$defs" not in rebased:
        rebased["$defs"] = schema["$defs"]
    return preserve_schema_dialect(schema, rebased)


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

    require(
        isinstance(expected_errors, list),
        f"expectedErrors must be a JSON array, got {expected_errors!r}",
    )

    for expected_index, expected in enumerate(expected_errors):
        require_entry_keys(
            expected,
            ["path", "keyword", "messageContains"],
            entry_kind="expectedErrors",
            index=expected_index,
        )

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


for negative_fixture_index, fixture in enumerate(negative_fixtures):
    require_entry_keys(
        fixture,
        ["schema", "basePositiveFixture", "basePointer", "mutation", "expectedErrors"],
        entry_kind="negativeFixtures",
        index=negative_fixture_index,
    )
    schema_path = fixture["schema"]
    base_positive_fixture = fixture["basePositiveFixture"]
    base_pointer = fixture["basePointer"]
    mutation = fixture["mutation"]
    expected_errors = fixture["expectedErrors"]
    schema_branch = fixture.get("schemaBranch")
    description = fixture.get("description", "no description")

    require((ROOT / base_positive_fixture).is_file(), f"Missing base positive fixture: {base_positive_fixture}")
    require(
        any(isinstance(f, dict) and f.get("path") == base_positive_fixture for f in fixtures),
        f"basePositiveFixture {base_positive_fixture!r} is not a registered positive fixture",
    )
    schema = require_schema_document(schema_path, entry_kind="Negative fixture")

    base_value = load_base_value(base_positive_fixture, base_pointer)
    mutated_instance = apply_mutation(base_value, mutation)

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
for enum_boundary_index, check in enumerate(enum_boundary_checks):
    require_entry_keys(
        check,
        ["schema", "schemaPointer", "validValues", "invalidValues"],
        entry_kind="enumBoundaryChecks",
        index=enum_boundary_index,
    )
    schema_path = check["schema"]
    schema_pointer = check["schemaPointer"]
    description = check.get("description", "no description")
    require(
        isinstance(check["validValues"], list),
        f"enumBoundaryChecks ({description}): validValues must be a JSON array, got {check['validValues']!r}",
    )
    require(
        isinstance(check["invalidValues"], list),
        f"enumBoundaryChecks ({description}): invalidValues must be a JSON array, got {check['invalidValues']!r}",
    )
    schema_document = require_schema_document(schema_path, entry_kind="enumBoundaryChecks")

    sub_schema = resolve_json_pointer(schema_document, schema_pointer)
    require(isinstance(sub_schema, dict) and "enum" in sub_schema, f"{schema_path}{schema_pointer} is not an enum sub-schema")
    validator = make_validator(preserve_schema_dialect(schema_document, sub_schema))

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

    try:
        diagnose_negative(schema, None, corrupted, [{"path": [], "keyword": "required"}])
    except SystemExit:
        pass
    else:
        raise SystemExit(
            "Self-test failure: diagnose_negative() must reject an expectedErrors entry missing "
            "'messageContains' via a controlled SystemExit, not raise a raw KeyError."
        )

    try:
        require_entry_keys({"path": "x"}, ["path", "schema"], entry_kind="selftest", index=0)
    except SystemExit:
        pass
    else:
        raise SystemExit(
            "Self-test failure: require_entry_keys() must reject a dict missing a required key "
            "via a controlled SystemExit."
        )

    try:
        require_entry_keys(["not", "a", "dict"], ["path"], entry_kind="selftest", index=0)
    except SystemExit:
        pass
    else:
        raise SystemExit(
            "Self-test failure: require_entry_keys() must reject a non-dict entry via a controlled "
            "SystemExit."
        )

    try:
        require_schema_document("contracts/openapi.yaml", entry_kind="selftest")
    except SystemExit:
        pass
    else:
        raise SystemExit(
            "Self-test failure: require_schema_document() must reject a registered document that is "
            "not a JSON schema (a YAML document) via a controlled SystemExit."
        )

    try:
        require_schema_document("contracts/route-inventory.json", entry_kind="selftest")
    except SystemExit:
        pass
    else:
        raise SystemExit(
            "Self-test failure: require_schema_document() must reject a registered .json document "
            "that loads as a JSON object but has no root '$schema' marker (contracts/route-inventory.json "
            "is not a JSON Schema, just a plain JSON object) via a controlled SystemExit."
        )

    try:
        require_schema_document({"not": "a string path"}, entry_kind="selftest")
    except SystemExit:
        pass
    else:
        raise SystemExit(
            "Self-test failure: require_schema_document() must reject a non-string, unhashable "
            "schema_path (e.g. a dict) via a controlled SystemExit, not raise a raw TypeError "
            "('unhashable type') from `schema_path in schemas_by_path`."
        )

    try:
        require_schema_document(["not", "a", "string", "path"], entry_kind="selftest")
    except SystemExit:
        pass
    else:
        raise SystemExit(
            "Self-test failure: require_schema_document() must reject a non-string, unhashable "
            "schema_path (e.g. a list) via a controlled SystemExit, not raise a raw TypeError "
            "('unhashable type') from `schema_path in schemas_by_path`."
        )

    one_of_schema = {"oneOf": [{"type": "object"}, {"type": "string"}]}
    try:
        extract_branch_schema(one_of_schema, 99)
    except SystemExit:
        pass
    else:
        raise SystemExit(
            "Self-test failure: extract_branch_schema() must reject an out-of-range oneOf index via "
            "a controlled SystemExit, not raise a raw IndexError."
        )

    try:
        extract_branch_schema(one_of_schema, -1)
    except SystemExit:
        pass
    else:
        raise SystemExit(
            "Self-test failure: extract_branch_schema() must reject a negative oneOf index via a "
            "controlled SystemExit (Python's negative-index wraparound would otherwise silently "
            "resolve to the last branch)."
        )

    try:
        extract_branch_schema({"oneOf": ["not-an-object"]}, 0)
    except SystemExit:
        pass
    else:
        raise SystemExit(
            "Self-test failure: extract_branch_schema() must reject a oneOf branch that does not "
            "resolve to a JSON object via a controlled SystemExit."
        )

    try:
        diagnose_negative(schema, None, corrupted, "not-a-list")
    except SystemExit:
        pass
    else:
        raise SystemExit(
            "Self-test failure: diagnose_negative() must reject a non-list expectedErrors via a "
            "controlled SystemExit, not raise a raw TypeError while iterating."
        )

    # Prove extract_branch_schema() actually propagates the parent's explicit
    # $schema dialect marker onto the extracted branch -- not merely that the
    # key is copied, but that it changes which validator class is selected
    # (jsonschema's own default dialect, absent an explicit $schema, would
    # otherwise silently take over for a bare extracted branch).
    draft07_one_of_schema = {
        "$schema": "http://json-schema.org/draft-07/schema#",
        "oneOf": [{"type": "object"}, {"type": "string"}],
    }
    branch_with_dialect = extract_branch_schema(draft07_one_of_schema, 0)
    require(
        branch_with_dialect.get("$schema") == "http://json-schema.org/draft-07/schema#",
        "Self-test failure: extract_branch_schema() must copy the parent schema's $schema dialect "
        f"marker onto the extracted branch; got {branch_with_dialect.get('$schema')!r}.",
    )
    require(
        validator_for(branch_with_dialect) is validator_for(draft07_one_of_schema),
        "Self-test failure: extract_branch_schema()'s branch must resolve to the same validator "
        "class (dialect) as its parent schema, not a jsonschema-library default.",
    )

    draft07_defs_schema = {
        "$schema": "http://json-schema.org/draft-07/schema#",
        "$defs": {"named": {"type": "object"}},
    }
    named_branch_with_dialect = extract_branch_schema(draft07_defs_schema, "named")
    require(
        named_branch_with_dialect.get("$schema") == "http://json-schema.org/draft-07/schema#",
        "Self-test failure: extract_branch_schema() must copy $defs's parent $schema dialect marker "
        f"onto a named $defs branch too; got {named_branch_with_dialect.get('$schema')!r}.",
    )

    branch_with_own_dialect = extract_branch_schema(
        {
            "$schema": "http://json-schema.org/draft-07/schema#",
            "oneOf": [{"$schema": "https://json-schema.org/draft/2020-12/schema", "type": "object"}],
        },
        0,
    )
    require(
        branch_with_own_dialect.get("$schema") == "https://json-schema.org/draft/2020-12/schema",
        "Self-test failure: extract_branch_schema() must not overwrite a branch's own already-declared "
        f"$schema with its parent's; got {branch_with_own_dialect.get('$schema')!r}.",
    )

    # Prove preserve_schema_dialect() itself -- the helper `enumBoundaryChecks`
    # relies on directly (not through extract_branch_schema()) -- both copies
    # a missing dialect marker and resolves to the parent's validator class.
    draft07_enum_document = {
        "$schema": "http://json-schema.org/draft-07/schema#",
        "properties": {"side": {"enum": ["A", "B"]}},
    }
    isolated_enum_sub_schema = draft07_enum_document["properties"]["side"]
    rebased_enum_sub_schema = preserve_schema_dialect(draft07_enum_document, isolated_enum_sub_schema)
    require(
        rebased_enum_sub_schema.get("$schema") == "http://json-schema.org/draft-07/schema#",
        "Self-test failure: preserve_schema_dialect() must copy the parent document's $schema onto an "
        f"isolated enum sub-schema; got {rebased_enum_sub_schema.get('$schema')!r}.",
    )
    require(
        validator_for(rebased_enum_sub_schema) is validator_for(draft07_enum_document),
        "Self-test failure: preserve_schema_dialect()'s rebased enum sub-schema must resolve to the "
        "same validator class (dialect) as its parent document, not a jsonschema-library default.",
    )
    require(
        "$schema" not in isolated_enum_sub_schema,
        "Self-test failure: preserve_schema_dialect() must not mutate the original sub-schema in "
        "place.",
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

    # An "add" index equal to length is a valid append; beyond that must be
    # rejected deterministically rather than silently appending regardless
    # (Python's `list.insert` clamps out-of-range indices instead of
    # raising, so this must be enforced explicitly).
    end_insert = apply_mutation(base, {"op": "add", "pointer": "/items/3", "value": "END"})
    require(
        end_insert["items"] == ["a", "b", "c", "END"],
        f"apply_mutation add at index == length must append; got {end_insert['items']!r}",
    )

    def _expect_rejected(mutation: dict, label: str, doc=None) -> None:
        try:
            apply_mutation(base if doc is None else doc, mutation)
        except SystemExit:
            return
        raise SystemExit(
            f"Self-test failure: apply_mutation accepted an out-of-range/invalid {label} "
            f"mutation instead of rejecting it: {mutation!r}"
        )

    _expect_rejected({"op": "add", "pointer": "/items/4", "value": "X"}, "add index (beyond length)")
    _expect_rejected({"op": "replace", "pointer": "/items/3", "value": "X"}, "replace index (== length)")
    _expect_rejected({"op": "remove", "pointer": "/items/3"}, "remove index (== length)")
    _expect_rejected({"op": "add", "pointer": "/items/01", "value": "X"}, "add index (leading zero)")
    _expect_rejected({"op": "add", "pointer": "/items/-1", "value": "X"}, "add index (negative)")
    _expect_rejected({"op": "add", "pointer": "/items/-0", "value": "X"}, "add index (negative zero)")

    # A malformed mutation descriptor (missing "op" and/or "pointer") must
    # fail via require()/SystemExit rather than a raw KeyError, since these
    # come from hand-authored manifest.json entries that could have a typo.
    _expect_rejected({"pointer": "/items/0", "value": "X"}, "mutation missing 'op'", doc=base)
    _expect_rejected({"op": "replace", "value": "X"}, "mutation missing 'pointer'", doc=base)
    _expect_rejected("not-a-dict", "mutation that isn't an object", doc=base)

    # Missing object keys must fail deterministically via require()/SystemExit
    # (a controlled validation error), not leak a raw KeyError traceback.
    obj_base = {"nested": {"present": 1}}
    _expect_rejected({"op": "remove", "pointer": "/missing"}, "remove (missing top-level key)", doc=obj_base)
    _expect_rejected(
        {"op": "replace", "pointer": "/missing", "value": "X"},
        "replace (missing top-level key)",
        doc=obj_base,
    )
    require(
        apply_mutation(obj_base, {"op": "remove", "pointer": "/nested/present"})["nested"] == {},
        "apply_mutation remove of a present key should still succeed",
    )
    try:
        apply_mutation(obj_base, {"op": "replace", "pointer": "/nested/missing/deeper", "value": "X"})
        raise SystemExit(
            "Self-test failure: apply_mutation accepted traversal through a missing "
            "intermediate object key instead of rejecting it"
        )
    except SystemExit as exc:
        require(
            "not found" in str(exc),
            f"apply_mutation should fail with a clear 'not found' message for a missing "
            f"intermediate key, got: {exc}",
        )
    # RFC 6902 "add" on an object sets the member regardless of prior
    # existence, so it must succeed even for a previously-absent key.
    added = apply_mutation(obj_base, {"op": "add", "pointer": "/brandNew", "value": "X"})
    require(
        added.get("brandNew") == "X" and "brandNew" not in obj_base,
        "apply_mutation add on an object must set a new key without mutating the input",
    )

    # Every object-keyed op (remove/replace/add) must reject a scalar parent
    # deterministically (e.g. a pointer that descends one segment too far,
    # landing on a string/int/bool/null) rather than raising a raw TypeError.
    scalar_parent_base = {"leaf": "a string, not an object"}
    _expect_rejected(
        {"op": "remove", "pointer": "/leaf/nested"}, "remove (scalar parent)", doc=scalar_parent_base
    )
    _expect_rejected(
        {"op": "replace", "pointer": "/leaf/nested", "value": "X"},
        "replace (scalar parent)",
        doc=scalar_parent_base,
    )
    _expect_rejected(
        {"op": "add", "pointer": "/leaf/nested", "value": "X"}, "add (scalar parent)", doc=scalar_parent_base
    )


run_apply_mutation_self_test()
run_self_test()

print(
    f"Validated {len(documents)} contract documents, {len(fixtures)} fixtures, "
    f"{len(negative_fixtures)} negative regression fixtures (each derived from a real "
    f"positive fixture via a single mutation, plus a self-test proving exact-match precision), "
    f"and {len(enum_boundary_checks)} closed-enum boundary checks."
)
