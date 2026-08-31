#!/usr/bin/env python3
# /// script
# requires-python = ">=3.14"
# dependencies = [
#   "jsonschema==4.26.0",
#   "referencing==0.37.0",
# ]
# ///

import copy
import hashlib
import re
from pathlib import Path

from jsonschema import FormatChecker
from jsonschema.validators import validator_for
from referencing import Registry, Resource

import strict_json


ROOT = Path(__file__).resolve().parents[1]
CONTRACTS = ROOT / "contracts"

strict_json.run_self_tests()
strict_json.run_governed_bytes_self_tests()
strict_json.run_governed_path_self_tests(ROOT)


def load_json(path: Path) -> object:
    return strict_json.strict_json_load_path(path)


def load_governed_json(relative_path: str) -> object:
    """Read and strictly parse a manifest-declared governed artifact path
    (a `documents`/`fixtures`/`basePositiveFixture` entry), routing it
    through `strict_json.read_governed_worktree_bytes` so a manifest entry
    can never cause this validator to silently follow a symlink, escape
    this contract's governed subtree via a non-canonical path, or read a
    non-regular file -- exactly the same path-safety guarantee
    `update-manifest-hashes.py`/`check-schema-revision-drift.py` apply
    before hashing the same governed paths.
    """
    content = strict_json.read_governed_worktree_bytes(ROOT, relative_path)
    return strict_json.strict_json_loads(content, source=str(ROOT / relative_path))


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


manifest = load_governed_json("contracts/manifest.json")
documents = manifest["documents"]
fixtures = manifest["fixtures"]
capabilities_fixtures = [
    fixture
    for fixture in fixtures
    if fixture.get("schema") == "contracts/schemas/capabilities.schema.json"
]

# The capabilities response has exactly two production shapes, and both are
# registered as real Aeson-encoded fixtures: the legacy one a deployment
# without a locale catalog serves (no `localeCatalog`, no
# `i18n.locale-catalog.v1`), and the one a deployment that publishes a catalog
# serves. Every one of them must still agree with this manifest's own
# identity, and must keep the object and its capability string together --
# that pairing is what a client relies on to gate the optional behavior, and
# in the backend both come from a single `Maybe` (see
# Base.Api.Types.Capabilities.serverCapabilities).
LOCALE_CATALOG_CAPABILITY = "i18n.locale-catalog.v1"

require(
    len(capabilities_fixtures) == 2,
    "manifest.json must register exactly two capabilities fixtures -- the legacy shape a "
    "deployment without a catalog serves and the one it serves with a catalog -- so both the "
    "optional field's presence and its exact absence are governed; got "
    f"{[fixture['path'] for fixture in capabilities_fixtures]}",
)

capabilities_shapes: dict[bool, str] = {}
for capabilities_fixture in capabilities_fixtures:
    capabilities_path = capabilities_fixture["path"]
    capabilities = load_governed_json(capabilities_path)

    require(isinstance(capabilities, dict), f"{capabilities_path} must be an object")
    for field in ("schemaRevision", "status", "apiBasePath"):
        require(
            capabilities.get(field) == manifest.get(field),
            f"{capabilities_path} {field} must match manifest.json",
        )
    require(
        capabilities.get("nativeClientMinimumRevision")
        == manifest.get("compatibility", {}).get("nativeClientMinimumRevision"),
        f"{capabilities_path} nativeClientMinimumRevision must match manifest compatibility",
    )

    capability_strings = capabilities.get("capabilities")
    require(
        isinstance(capability_strings, list),
        f"{capabilities_path} capabilities must be an array",
    )
    advertises_catalog = LOCALE_CATALOG_CAPABILITY in capability_strings
    has_catalog_object = "localeCatalog" in capabilities
    require(
        advertises_catalog == has_catalog_object,
        f"{capabilities_path} must advertise {LOCALE_CATALOG_CAPABILITY} exactly when it "
        "carries a localeCatalog object",
    )
    require(
        has_catalog_object not in capabilities_shapes,
        f"{capabilities_path} duplicates the "
        f"{'locale-catalog' if has_catalog_object else 'legacy'} capabilities shape already "
        f"registered by {capabilities_shapes.get(has_catalog_object)}",
    )
    capabilities_shapes[has_catalog_object] = capabilities_path

# The loop above already refuses a duplicate shape, so with exactly two
# fixtures this can only fail if both are the same shape -- named separately
# because that failure reads very differently from a miscount.
require(
    set(capabilities_shapes) == {False, True},
    "manifest.json registers two capabilities fixtures of the same shape; one must carry a "
    f"localeCatalog object and one must not; got {sorted(capabilities_shapes.values())}",
)

for relative_path in documents:
    strict_json.read_governed_worktree_bytes(ROOT, relative_path)

for path in sorted(CONTRACTS.rglob("*.json")):
    load_json(path)

registry = Registry()
schema_ids: dict[str, str] = {}
schemas_by_path: dict[str, object] = {}
for relative_path in documents:
    if not relative_path.endswith(".json"):
        continue
    schema = load_governed_json(relative_path)
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

    instance = load_governed_json(fixture_path)
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


_JSON_POINTER_INVALID_TILDE_RE = re.compile(r"~(?![01])")


def _decode_json_pointer_token(raw_token: str, *, pointer: str) -> str:
    """Decode a single RFC 6901 JSON Pointer reference-token, rejecting any
    `~` not immediately followed by `0` or `1` (the only two escape
    sequences RFC 6901 defines). A bare/malformed `~` (e.g. a typo'd
    pointer) must fail deterministically here rather than being silently
    treated as a literal `~` character, which could otherwise make a
    malformed mutation pointer resolve to -- and mutate -- the wrong
    location instead of failing closed.
    """
    require(
        _JSON_POINTER_INVALID_TILDE_RE.search(raw_token) is None,
        f"Invalid JSON Pointer escape in segment {raw_token!r} (pointer {pointer!r}): "
        "'~' must be immediately followed by '0' or '1'",
    )
    return raw_token.replace("~1", "/").replace("~0", "~")


def resolve_json_pointer(document, pointer: str):
    """Resolve an RFC 6901 JSON Pointer against an in-memory document."""
    require(pointer == "" or pointer.startswith("/"), f"Invalid JSON Pointer: {pointer!r}")
    value = document
    for raw_token in pointer.split("/")[1:]:
        token = _decode_json_pointer_token(raw_token, pointer=pointer)
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
    require(
        isinstance(mutation["op"], str),
        f"Mutation descriptor 'op' must be a string, got {mutation['op']!r}",
    )
    require(
        isinstance(mutation["pointer"], str),
        f"Mutation descriptor 'pointer' must be a string, got {mutation['pointer']!r}",
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
        token = _decode_json_pointer_token(raw_token, pointer=pointer)
        if isinstance(parent, list):
            parent = parent[_require_array_index(token, length=len(parent), allow_equal_length=False)]
        elif isinstance(parent, dict):
            _require_object_key(parent, token, pointer=pointer, must_exist=True)
            parent = parent[token]
        else:
            raise SystemExit(f"Cannot descend into scalar with pointer segment {token!r} ({pointer!r})")
    last = _decode_json_pointer_token(last_raw, pointer=pointer)

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


# ---------------------------------------------------------------------------
# Canonical signed-64 integer wire tokens
#
# JSON Schema intentionally treats 3, 3.0, and 3e0 as the same mathematical
# integer. Native choice/version fields additionally promise canonical integer
# token spelling, checked against governed fixture bytes before parsed values
# erase that distinction. The shared schema separately owns the exact range.
# ---------------------------------------------------------------------------

canonical_integer_checks = manifest.get("canonicalIntegerChecks", [])
for check_index, check in enumerate(canonical_integer_checks):
    require_entry_keys(
        check,
        [
            "schema",
            "validBoundaryValues",
            "invalidBoundaryValues",
            "invalidTokenSpellings",
            "fixtureFields",
        ],
        entry_kind="canonicalIntegerChecks",
        index=check_index,
    )
    description = check.get("description", "no description")
    schema = require_schema_document(
        check["schema"], entry_kind="canonicalIntegerChecks"
    )
    validator = make_validator(schema)

    for value in check["validBoundaryValues"]:
        errors = list(validator.iter_errors(value))
        require(
            not errors,
            f"canonicalIntegerChecks ({description}): valid boundary {value!r} "
            f"failed {check['schema']}: {[error.message for error in errors]}",
        )
    for value in check["invalidBoundaryValues"]:
        errors = list(validator.iter_errors(value))
        expected_keyword = "minimum" if value < 0 else "maximum"
        require(
            len(errors) == 1 and errors[0].validator == expected_keyword,
            f"canonicalIntegerChecks ({description}): invalid boundary {value!r} "
            f"must fail exactly the {expected_keyword} bound in {check['schema']}, "
            f"got {[(error.validator, error.message) for error in errors]}",
        )

    for field_index, field in enumerate(check["fixtureFields"]):
        require_entry_keys(
            field,
            ["path", "pointers"],
            entry_kind="canonicalIntegerChecks.fixtureFields",
            index=field_index,
        )
        fixture_path = field["path"]
        pointers = field["pointers"]
        require(
            any(
                isinstance(fixture, dict) and fixture.get("path") == fixture_path
                for fixture in fixtures
            ),
            f"canonicalIntegerChecks fixture is not registered: {fixture_path}",
        )
        require(
            isinstance(pointers, list) and pointers,
            f"canonicalIntegerChecks pointers must be a non-empty array: {pointers!r}",
        )
        content = strict_json.read_governed_worktree_bytes(ROOT, fixture_path)
        strict_json.strict_json_loads_requiring_canonical_nonnegative_integers(
            content,
            pointers,
            source=str(ROOT / fixture_path),
        )

    for raw in check["invalidTokenSpellings"]:
        require(
            isinstance(raw, str),
            f"canonicalIntegerChecks invalid token spelling must be a string: {raw!r}",
        )
        try:
            strict_json.strict_json_loads_requiring_canonical_nonnegative_integers(
                '{"value":' + raw + "}",
                ["/value"],
                source=f"<canonicalIntegerChecks invalid spelling {raw!r}>",
            )
        except strict_json.StrictJSONError:
            pass
        else:
            raise SystemExit(
                f"canonicalIntegerChecks ({description}): non-canonical token "
                f"{raw!r} unexpectedly validated"
            )


forward_compatibility_checks = manifest.get("forwardCompatibilityChecks", [])
for check_index, check in enumerate(forward_compatibility_checks):
    require_entry_keys(
        check,
        ["schema", "basePositiveFixture", "basePointer", "mutation"],
        entry_kind="forwardCompatibilityChecks",
        index=check_index,
    )
    description = check.get("description", "no description")
    base_fixture = check["basePositiveFixture"]
    require(
        any(
            isinstance(fixture, dict) and fixture.get("path") == base_fixture
            for fixture in fixtures
        ),
        f"forwardCompatibilityChecks base fixture is not registered: {base_fixture}",
    )
    base_document = load_governed_json(base_fixture)
    base_value = resolve_json_pointer(base_document, check["basePointer"])
    instance = apply_mutation(base_value, check["mutation"])
    schema = require_schema_document(
        check["schema"], entry_kind="forwardCompatibilityChecks"
    )
    errors = list(make_validator(schema).iter_errors(instance))
    require(
        not errors,
        f"forwardCompatibilityChecks ({description}) unexpectedly failed: "
        f"{[error.message for error in errors]}",
    )


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
        positive_fixture_cache[base_positive_fixture] = load_governed_json(base_positive_fixture)
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
    rejection_schema_branch = fixture.get("rejectionSchemaBranch")
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
    # (or an explicitly named containing union) still genuinely rejects the
    # same mutated instance overall, without re-enumerating sibling noise in
    # expectedErrors.
    if schema_branch is not None:
        rejection_schema = (
            extract_branch_schema(schema, rejection_schema_branch)
            if rejection_schema_branch is not None
            else schema
        )
        root_validator = make_validator(rejection_schema)
        root_errors = list(root_validator.iter_errors(mutated_instance))
        require(
            len(root_errors) > 0,
            f"Negative fixture ({description}) unexpectedly validated against the full "
            f"rejection schema {schema_path}"
            + (
                f" $defs/{rejection_schema_branch}"
                if rejection_schema_branch is not None
                else ""
            ),
        )
        if "oneOf" in rejection_schema:
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


# ---------------------------------------------------------------------------
# Manifest URL binding
#
# One governed table (`manifestUrlChecks`), checked here against the published
# schema and, in Arkham.Api.LocaleCatalogCapabilitySpec, against the
# production Haskell validator. Neither side owns the table, so the schema and
# the encoder cannot drift apart silently: a host spelling one accepts and the
# other refuses fails a build.
# ---------------------------------------------------------------------------

def advertised_schema_versions() -> set:
    """Catalog schema versions the governed capabilities fixtures advertise."""
    versions = set()
    for capabilities_path in capabilities_shapes.values():
        document = load_governed_json(capabilities_path)
        catalog = document.get("localeCatalog") if isinstance(document, dict) else None
        if isinstance(catalog, dict) and "schemaVersion" in catalog:
            versions.add(catalog["schemaVersion"])
    return versions


manifest_url_checks = manifest.get("manifestUrlChecks")
require(
    isinstance(manifest_url_checks, dict),
    "manifest.json must declare a manifestUrlChecks object binding the manifestUrl schema "
    "and the production validator to one table",
)
require_entry_keys(
    manifest_url_checks,
    ["schema", "pointer", "accepted", "rejected"],
    entry_kind="manifestUrlChecks",
    index=0,
)
manifest_url_schema_document = require_schema_document(
    manifest_url_checks["schema"], entry_kind="manifestUrlChecks"
)
manifest_url_sub_schema = resolve_json_pointer(
    manifest_url_schema_document, manifest_url_checks["pointer"]
)
require(
    isinstance(manifest_url_sub_schema, dict),
    f"manifestUrlChecks pointer {manifest_url_checks['pointer']!r} does not resolve to a sub-schema",
)
manifest_url_validator = make_validator(
    preserve_schema_dialect(manifest_url_schema_document, manifest_url_sub_schema)
)

require(
    isinstance(manifest_url_checks["accepted"], list) and manifest_url_checks["accepted"],
    "manifestUrlChecks.accepted must be a non-empty JSON array",
)
require(
    isinstance(manifest_url_checks["rejected"], list) and manifest_url_checks["rejected"],
    "manifestUrlChecks.rejected must be a non-empty JSON array",
)

for accepted_index, accepted in enumerate(manifest_url_checks["accepted"]):
    require_entry_keys(
        accepted, ["configured", "published"], entry_kind="manifestUrlChecks.accepted", index=accepted_index
    )
    published = accepted["published"]
    errors = list(manifest_url_validator.iter_errors(published))
    require(
        not errors,
        f"manifestUrlChecks.accepted[{accepted_index}]: the server would publish {published!r} for "
        f"configured value {accepted['configured']!r}, but the schema rejects it: {normalize_errors(errors)}",
    )

for rejected_index, rejected in enumerate(manifest_url_checks["rejected"]):
    require_entry_keys(
        rejected, ["configured", "reason"], entry_kind="manifestUrlChecks.rejected", index=rejected_index
    )
    configured = rejected["configured"]
    require(
        list(manifest_url_validator.iter_errors(configured)),
        f"manifestUrlChecks.rejected[{rejected_index}]: the server refuses {configured!r} as "
        f"{rejected['reason']}, but the schema would accept it -- the schema is more permissive "
        "than the encoder, which is the direction that lets an unsafe URL reach a client",
    )

configured_urls = [entry["configured"] for entry in manifest_url_checks["accepted"]] + [
    entry["configured"] for entry in manifest_url_checks["rejected"]
]
require(
    len(set(configured_urls)) == len(configured_urls),
    "manifestUrlChecks must not configure the same URL twice; several accepted rows do share a "
    "published URL on purpose, since normalization is exactly what they prove",
)

# ---------------------------------------------------------------------------
# Catalog revision binding
#
# Same shape as manifestUrlChecks: one governed table, checked here against the
# published schema and, in the backend spec, against the production config
# parser. The schema must not be looser than the encoder -- a revision the
# server refuses at startup must not be describable as a valid response.
# ---------------------------------------------------------------------------

catalog_revision_checks = manifest.get("catalogRevisionChecks")
require(
    isinstance(catalog_revision_checks, dict),
    "manifest.json must declare a catalogRevisionChecks object binding the catalogRevision schema "
    "and the production validator to one table",
)
require_entry_keys(
    catalog_revision_checks,
    ["schema", "pointer", "schemaVersion", "accepted", "rejected"],
    entry_kind="catalogRevisionChecks",
    index=0,
)
catalog_revision_schema_document = require_schema_document(
    catalog_revision_checks["schema"], entry_kind="catalogRevisionChecks"
)
catalog_revision_sub_schema = resolve_json_pointer(
    catalog_revision_schema_document, catalog_revision_checks["pointer"]
)
require(
    isinstance(catalog_revision_sub_schema, dict),
    f"catalogRevisionChecks pointer {catalog_revision_checks['pointer']!r} does not resolve to a "
    "sub-schema",
)
catalog_revision_validator = make_validator(
    preserve_schema_dialect(catalog_revision_schema_document, catalog_revision_sub_schema)
)
require(
    isinstance(catalog_revision_checks["accepted"], list) and catalog_revision_checks["accepted"],
    "catalogRevisionChecks.accepted must be a non-empty JSON array",
)
require(
    isinstance(catalog_revision_checks["rejected"], list) and catalog_revision_checks["rejected"],
    "catalogRevisionChecks.rejected must be a non-empty JSON array",
)
for revision_index, accepted in enumerate(catalog_revision_checks["accepted"]):
    require_entry_keys(
        accepted, ["configured"], entry_kind="catalogRevisionChecks.accepted", index=revision_index
    )
    errors = list(catalog_revision_validator.iter_errors(accepted["configured"]))
    require(
        not errors,
        f"catalogRevisionChecks.accepted[{revision_index}]: the server publishes "
        f"{accepted['configured']!r}, but the schema rejects it: {normalize_errors(errors)}",
    )
for revision_index, rejected in enumerate(catalog_revision_checks["rejected"]):
    require_entry_keys(
        rejected,
        ["configured", "reason"],
        entry_kind="catalogRevisionChecks.rejected",
        index=revision_index,
    )
    require(
        list(catalog_revision_validator.iter_errors(rejected["configured"])),
        f"catalogRevisionChecks.rejected[{revision_index}]: the server refuses "
        f"{rejected['configured']!r} as {rejected['reason']}, but the schema would accept it -- "
        "the schema must never be looser than the encoder",
    )
require(
    catalog_revision_checks["schemaVersion"] in advertised_schema_versions(),
    "catalogRevisionChecks.schemaVersion must be a schema version the advertised fixture uses",
)


# ---------------------------------------------------------------------------
# Locale catalog provenance
#
# The advertised `localeCatalog` block is not hand-maintained: it is derived
# from the bytes of a committed, synthetic v1 catalog manifest
# (`contracts/fixtures/locale-catalog-manifest.json`). That fixture is a test
# artifact, never regenerated from frontend/src/locales/**, so publishing new
# production catalog content does NOT churn this contract -- while any drift
# between the two governed files fails here.
# ---------------------------------------------------------------------------

CATALOG_MANIFEST_PATH = "contracts/fixtures/locale-catalog-manifest.json"
CATALOG_FIXTURE_PREFIX = "contracts/fixtures/locale-catalog-"
CATALOG_MANIFEST_SCHEMA = ROOT / "frontend" / "schemas" / "locale-catalog" / "v1" / "manifest.schema.json"

require(
    CATALOG_MANIFEST_PATH in documents,
    f"{CATALOG_MANIFEST_PATH} must be a governed document so its bytes are hash-pinned",
)

catalog_manifest_bytes = strict_json.read_governed_worktree_bytes(ROOT, CATALOG_MANIFEST_PATH)
catalog_manifest = strict_json.strict_json_loads(
    catalog_manifest_bytes, source=str(ROOT / CATALOG_MANIFEST_PATH)
)
require(isinstance(catalog_manifest, dict), f"{CATALOG_MANIFEST_PATH} must be a JSON object")

# Every authority the manifest's claims are derived from is governed too, so a
# digest it publishes cannot be true of a file this contract does not pin.
governed_catalog_files = sorted(
    path for path in documents if path.startswith(CATALOG_FIXTURE_PREFIX)
)
require(
    len(governed_catalog_files) >= 6,
    "the synthetic catalog's authority files (locale sources, backend registry, chunks, manifest) "
    f"must all be governed documents; found {governed_catalog_files}",
)
for role in ("backend-registry", "source-", "chunk-", "manifest"):
    require(
        any(f"{CATALOG_FIXTURE_PREFIX}{role}" in path for path in governed_catalog_files),
        f"the synthetic catalog is missing its committed {role!r} authority; every digest and "
        "count in its manifest must be backed by governed bytes",
    )

# `generatorSha256` and `schemasSha256` are digests of real repository sources
# (the fixture generator and the published v1 schemas) rather than of committed
# stand-ins, so they are checked where they are derived --
# scripts/build-locale-catalog-fixture.py's own provenance self-tests, run by
# `contracts:catalog-fixture` -- not against a governed descriptor here.

# scripts/build-locale-catalog-fixture.py owns the derivation itself (and its
# own mutation self-tests); `contracts:catalog-fixture` re-derives all of it.
# What is checked here is the seam this contract depends on: the manifest is
# schema-valid, internally consistent, and is exactly what the advertised
# pointer describes.
#
# The v1 catalog schema is deliberately frontend-owned and NOT a governed
# contract document (contracts/README.md), so it is loaded by path here rather
# than registered -- the synthetic fixture is still held to the real, published
# schema.
require(CATALOG_MANIFEST_SCHEMA.is_file(), f"Missing catalog manifest schema: {CATALOG_MANIFEST_SCHEMA}")
catalog_manifest_schema = load_json(CATALOG_MANIFEST_SCHEMA)
catalog_schema_validator_class = validator_for(catalog_manifest_schema)
catalog_schema_validator_class.check_schema(catalog_manifest_schema)
catalog_schema_errors = list(
    catalog_schema_validator_class(catalog_manifest_schema, format_checker=FormatChecker()).iter_errors(
        catalog_manifest
    )
)
require(
    not catalog_schema_errors,
    f"{CATALOG_MANIFEST_PATH} does not validate against the published v1 catalog manifest schema: "
    f"{normalize_errors(catalog_schema_errors)}",
)


def derive_locale_catalog(catalog: dict, catalog_bytes: bytes) -> dict:
    """The whole advertised block, derived from one manifest's exact bytes --
    the same derivation `docs/locale-catalog.md` documents for a real
    deployment, and the same one `Helpers.LocaleCatalog` drives the production
    settings pipeline with.
    """
    return {
        "manifestUrl": catalog["manifestPath"],
        "catalogRevision": catalog["catalogRevision"],
        "schemaVersion": catalog["schemaVersion"],
        "defaultLocale": catalog["defaultLocale"],
        "supportedLocales": sorted(record["locale"] for record in catalog["locales"]),
        "manifestSha256": hashlib.sha256(catalog_bytes).hexdigest(),
    }


def check_catalog_manifest_provenance(catalog: dict, catalog_bytes: bytes, advertised: dict) -> str | None:
    """Return a failure message, or None. Split out from the checks below so
    `run_locale_catalog_provenance_self_test` can prove the gate has teeth by
    running it against a mutated copy.
    """
    revision = catalog.get("catalogRevision")
    provenance = catalog.get("provenance", {})
    derived_revision = "1." + str(provenance.get("sha256", ""))[:32]
    if revision != derived_revision:
        return (
            f"catalogRevision {revision!r} is not '1.' plus the first 32 hex characters of "
            f"provenance.sha256 (expected {derived_revision!r})"
        )

    # Every published route is read from the manifest's own basePath rather
    # than re-spelled here, so the v1 schema's `const` declarations stay the one
    # place the route is decided (the generator reads them from there too).
    base_path = catalog.get("basePath")
    if not isinstance(base_path, str) or not base_path.startswith("/"):
        return f"basePath {base_path!r} is not an absolute published route"
    for field, expected_path in (
        ("manifestPath", f"{base_path}/manifest.json"),
        ("chunkPathPrefix", f"{base_path}/c/"),
    ):
        if catalog.get(field) != expected_path:
            return (
                f"{field} {catalog.get(field)!r} is not the route basePath {base_path!r} "
                f"defines (expected {expected_path!r})"
            )

    expected_revision_path = f"{base_path}/r/{revision}/manifest.json"
    if catalog.get("revisionManifestPath") != expected_revision_path:
        return (
            f"revisionManifestPath {catalog.get('revisionManifestPath')!r} does not address "
            f"catalogRevision {revision!r} under basePath {base_path!r} "
            f"(expected {expected_revision_path!r})"
        )

    if provenance.get("contractRevision") != manifest.get("schemaRevision"):
        return (
            f"provenance.contractRevision {provenance.get('contractRevision')!r} does not match "
            f"manifest.json schemaRevision {manifest.get('schemaRevision')!r}"
        )

    locales = catalog.get("locales", [])
    locale_tags = [record["locale"] for record in locales]
    if len(set(locale_tags)) != len(locale_tags):
        return f"locales are not unique: {locale_tags}"
    if catalog.get("defaultLocale") not in locale_tags:
        return f"defaultLocale {catalog.get('defaultLocale')!r} is not one of {locale_tags}"
    for record in locales:
        fallback = record.get("fallback")
        if fallback is not None and fallback not in locale_tags:
            return f"locale {record['locale']!r} falls back to unpublished {fallback!r}"
        if record.get("keys") != sum(chunk["keys"] for chunk in record["chunks"]):
            return f"locale {record['locale']!r} key count does not equal the sum of its chunks"
        if record.get("bytes") != sum(chunk["bytes"] for chunk in record["chunks"]):
            return f"locale {record['locale']!r} byte count does not equal the sum of its chunks"
        for chunk in record["chunks"]:
            expected_path = f"{catalog['chunkPathPrefix']}{chunk['sha256']}.json"
            if chunk["path"] != expected_path:
                return f"chunk {chunk['pack']!r} is not addressed by its own digest ({expected_path!r})"

    for entry in catalog.get("languageResolution", []):
        if entry["locale"] not in locale_tags:
            return f"languageResolution maps {entry['tag']!r} to unpublished locale {entry['locale']!r}"

    backend = catalog.get("backend", {})
    artifact_path = backend.get("artifactPath", "")
    if not artifact_path.startswith(CATALOG_FIXTURE_PREFIX):
        return (
            f"backend.artifactPath {artifact_path!r} is not one of this contract's committed "
            "synthetic authorities, so its digest and counts are unverifiable claims"
        )
    registry_bytes = strict_json.read_governed_worktree_bytes(ROOT, artifact_path)
    if backend.get("artifactSha256") != hashlib.sha256(registry_bytes).hexdigest():
        return f"backend.artifactSha256 is not the digest of {artifact_path}"
    registry = strict_json.strict_json_loads(registry_bytes, source=artifact_path)
    if backend.get("sourceSha256") != registry.get("source", {}).get("sha256"):
        return f"backend.sourceSha256 is not the source digest {artifact_path} declares"
    if backend.get("emittedKeys") != len(registry.get("emittedKeys", [])):
        return f"backend.emittedKeys does not count {artifact_path}'s emitted keys"
    if backend.get("requiredKeys") != len(registry.get("requiredKeys", [])):
        return f"backend.requiredKeys does not count {artifact_path}'s required keys"
    if backend.get("dynamicSites") != registry.get("dynamicSites"):
        return f"backend.dynamicSites is not what {artifact_path} reports"

    default_chunks = [
        chunk
        for record in locales
        if record["locale"] == catalog.get("defaultLocale")
        for chunk in record["chunks"]
    ]
    translated: set[str] = set()
    for chunk in default_chunks:
        chunk_path = f"{CATALOG_FIXTURE_PREFIX}chunk-{chunk['sha256']}.json"
        chunk_bytes = strict_json.read_governed_worktree_bytes(ROOT, chunk_path)
        if hashlib.sha256(chunk_bytes).hexdigest() != chunk["sha256"]:
            return f"{chunk_path} is not addressed by its own digest"
        if len(chunk_bytes) != chunk["bytes"]:
            return f"{chunk_path} byte count does not match the manifest"
        chunk_document = strict_json.strict_json_loads(chunk_bytes, source=chunk_path)
        if len(chunk_document.get("entries", {})) != chunk["keys"]:
            return f"{chunk_path} key count does not match the manifest"
        translated |= set(chunk_document.get("entries", {}))
    expected_untranslated = sorted(
        key for key in registry.get("emittedKeys", []) if key not in translated
    )
    if backend.get("untranslatedKeys") != expected_untranslated:
        return (
            "backend.untranslatedKeys is not the emitted keys the default locale leaves "
            f"untranslated (expected {expected_untranslated})"
        )

    if provenance.get("fixtureKeys") != sorted(registry.get("requiredKeys", [])):
        return f"provenance.fixtureKeys is not the required-key set {artifact_path} declares"
    if provenance.get("outputSha256") == provenance.get("sha256"):
        return "provenance.outputSha256 must be the rendered-output digest, not the revision digest"

    totals = catalog.get("totals", {})
    expected_totals = {
        "locales": len(locales),
        "chunks": sum(len(record["chunks"]) for record in locales),
        "bytes": sum(record["bytes"] for record in locales),
        "keys": sum(record["keys"] for record in locales),
        "unsupportedKeys": sum(
            chunk["unsupportedKeys"] for record in locales for chunk in record["chunks"]
        ),
    }
    if totals != expected_totals:
        return f"totals {totals} do not equal the catalog's own contents {expected_totals}"

    derived = derive_locale_catalog(catalog, catalog_bytes)
    if advertised != derived:
        return (
            "the advertised localeCatalog block is not the one this manifest describes: "
            f"{advertised} != {derived}"
        )
    return None


advertised_locale_catalog = None
for capabilities_path in capabilities_shapes.values():
    capabilities_document = load_governed_json(capabilities_path)
    if isinstance(capabilities_document, dict) and "localeCatalog" in capabilities_document:
        advertised_locale_catalog = capabilities_document["localeCatalog"]

require(
    isinstance(advertised_locale_catalog, dict),
    "the locale-catalog capabilities fixture must carry a localeCatalog object",
)
provenance_failure = check_catalog_manifest_provenance(
    catalog_manifest, catalog_manifest_bytes, advertised_locale_catalog
)
require(
    provenance_failure is None,
    f"{CATALOG_MANIFEST_PATH} provenance check failed: {provenance_failure}",
)


# ---------------------------------------------------------------------------
# Legacy compatibility
#
# The disabled response is deliberately NOT byte-identical to 0.1.22: the
# contract revision advances, as a monotonic bump requires, and schemaRevision
# describes the backend contract bundle rather than this optional runtime
# feature. What must hold is the exact legacy field and capability *shape*.
# ---------------------------------------------------------------------------

legacy_checks = manifest.get("legacyCompatibilityChecks")
require(
    isinstance(legacy_checks, dict),
    "manifest.json must declare legacyCompatibilityChecks pinning the pre-feature response",
)
require_entry_keys(
    legacy_checks,
    ["baselineRevision", "baselineResponse", "addedCapability", "allowedDifferences"],
    entry_kind="legacyCompatibilityChecks",
    index=0,
)
legacy_baseline = legacy_checks["baselineResponse"]
require(isinstance(legacy_baseline, dict), "legacyCompatibilityChecks.baselineResponse must be an object")
require(
    legacy_baseline.get("schemaRevision") == legacy_checks["baselineRevision"],
    "legacyCompatibilityChecks.baselineResponse must carry baselineRevision",
)
require(
    legacy_checks["addedCapability"] not in legacy_baseline.get("capabilities", []),
    "the baseline predates the locale-catalog capability, so it must not advertise it",
)
require(
    "localeCatalog" not in legacy_baseline,
    "the baseline predates the localeCatalog field, so it must not carry it",
)


def normalize_against_baseline(response: dict, allowed: list[str]) -> dict:
    """Drop exactly the members a revision is allowed to differ in, so what is
    left is the legacy shape and nothing else."""
    normalized = {key: value for key, value in response.items() if key not in allowed}
    if "capabilities" in allowed:
        normalized["capabilities"] = [
            capability
            for capability in response.get("capabilities", [])
            if capability != legacy_checks["addedCapability"]
        ]
    return normalized


legacy_expected = {key: value for key, value in legacy_baseline.items() if key != "schemaRevision"}
for advertises_catalog, capabilities_path in capabilities_shapes.items():
    response = load_governed_json(capabilities_path)
    allowed = legacy_checks["allowedDifferences"]["advertised" if advertises_catalog else "disabled"]
    require(
        isinstance(allowed, list) and "schemaRevision" in allowed,
        f"legacyCompatibilityChecks.allowedDifferences must allow schemaRevision for "
        f"{capabilities_path}",
    )
    normalized = normalize_against_baseline(response, allowed)
    require(
        normalized == legacy_expected,
        f"{capabilities_path} changes the legacy response shape beyond {allowed}: "
        f"{normalized} != {legacy_expected}",
    )
    require(
        response.get("schemaRevision") == manifest.get("schemaRevision")
        and response.get("schemaRevision") != legacy_checks["baselineRevision"],
        f"{capabilities_path} must report this revision, not the {legacy_checks['baselineRevision']} "
        "baseline -- a server that under-reports its contract revision lies to every client that "
        "negotiates on it",
    )


def run_legacy_compatibility_self_test() -> None:
    """Prove the normalization is not vacuous: it must still catch a change to
    a field the revision is *not* allowed to differ in.
    """
    disabled_allowed = legacy_checks["allowedDifferences"]["disabled"]
    mutated = copy.deepcopy(legacy_baseline)
    mutated["capabilities"] = mutated["capabilities"][:-1]
    require(
        normalize_against_baseline(mutated, disabled_allowed) != legacy_expected,
        "Self-test failure: dropping a legacy capability survived baseline normalization",
    )
    mutated = copy.deepcopy(legacy_baseline)
    mutated["nativeClientMinimumRevision"] = "9.9.9"
    require(
        normalize_against_baseline(mutated, disabled_allowed) != legacy_expected,
        "Self-test failure: changing the compatibility floor survived baseline normalization",
    )


run_legacy_compatibility_self_test()


def run_no_copied_revision_self_test() -> None:
    """The synthetic catalog's revision must appear only where it is *derived*
    -- in the manifest that computes it and in the advertised fixture derived
    from that manifest's bytes. A copy anywhere else (this manifest's own
    descriptor content, a schema, another fixture) is exactly the drift this
    whole binding exists to prevent, so it fails here.
    """
    revision = catalog_manifest["catalogRevision"]
    allowed = {CATALOG_MANIFEST_PATH} | set(capabilities_shapes.values())
    for relative_path in documents + [fixture["path"] for fixture in fixtures]:
        if relative_path in allowed:
            continue
        content = strict_json.read_governed_worktree_bytes(ROOT, relative_path)
        require(
            revision.encode("utf-8") not in content,
            f"{relative_path} hard-codes the synthetic catalog revision {revision}; it must be "
            "derived from the manifest's bytes instead",
        )


def run_locale_catalog_provenance_self_test() -> None:
    """Prove the provenance gate is discriminating rather than vacuous: a
    one-character change to the manifest's bytes must be caught, because the
    advertised digest no longer matches, and a revision that no longer derives
    from its own provenance digest must be caught too.
    """
    mutated_bytes = catalog_manifest_bytes + b"\n"
    require(
        check_catalog_manifest_provenance(
            catalog_manifest, mutated_bytes, advertised_locale_catalog
        )
        is not None,
        "Self-test failure: the provenance gate accepted a manifest whose bytes no longer hash "
        "to the advertised manifestSha256",
    )

    for field in ("basePath", "manifestPath", "chunkPathPrefix"):
        mutated_route = copy.deepcopy(catalog_manifest)
        mutated_route[field] = "/elsewhere"
        require(
            check_catalog_manifest_provenance(
                mutated_route, catalog_manifest_bytes, advertised_locale_catalog
            )
            is not None,
            f"Self-test failure: the provenance gate accepted a {field} the manifest's own routes "
            "do not agree on",
        )

    mutated_catalog = copy.deepcopy(catalog_manifest)
    mutated_catalog["catalogRevision"] = "1." + "0" * 32
    require(
        check_catalog_manifest_provenance(
            mutated_catalog, catalog_manifest_bytes, advertised_locale_catalog
        )
        is not None,
        "Self-test failure: the provenance gate accepted a catalogRevision that does not derive "
        "from its own provenance.sha256",
    )

    mutated_advertised = copy.deepcopy(advertised_locale_catalog)
    mutated_advertised["supportedLocales"] = mutated_advertised["supportedLocales"][:1]
    require(
        check_catalog_manifest_provenance(
            catalog_manifest, catalog_manifest_bytes, mutated_advertised
        )
        is not None,
        "Self-test failure: the provenance gate accepted an advertised locale set that is not the "
        "manifest's own",
    )


run_locale_catalog_provenance_self_test()
run_no_copied_revision_self_test()


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


def run_numeric_overflow_underflow_schema_self_test() -> None:
    """Prove, end-to-end through the actual `jsonschema` validator this
    tool uses (not merely in isolation inside `strict_json.py`'s own
    self-tests), that `strict_json.strict_json_loads`'s float-overflow/
    -underflow guards are load-bearing for real JSON Schema `minimum`/
    `maximum` semantics -- i.e. that skipping them would let a malformed
    numeric literal silently change whether a schema constraint appears
    satisfied, not merely fail to parse.

    Concretely: a schema `{"type": "number", "minimum": 0}` intends to
    accept only non-negative values. A JSON literal like `-1e-1000000` is
    genuinely negative (violates that constraint), but the bare `float()`
    constructor silently underflows it to `-0.0` -- and `-0.0 >= 0` is
    `True` in Python (and thus in `jsonschema`'s numeric comparison, which
    is plain Python `>=`), so a schema that used bare `float()` parsing
    would incorrectly consider this out-of-range value to satisfy
    `"minimum": 0`. This proves that danger directly against the real
    validator, and then proves `strict_json_loads` rejects the literal
    outright (as already exercised in `strict_json.run_self_tests()`) so
    this false-positive validation can never actually occur in this
    toolchain's real validation path.
    """
    minimum_zero_schema = {"type": "number", "minimum": 0}
    validator = make_validator(minimum_zero_schema)

    unsafe_bare_float = float("-1e-1000000")
    require(
        unsafe_bare_float == -0.0,
        "Self-test failure: this self-test's premise requires bare float() to underflow "
        f"'-1e-1000000' to -0.0, got {unsafe_bare_float!r}.",
    )
    require(
        len(list(validator.iter_errors(unsafe_bare_float))) == 0,
        "Self-test failure: this self-test's premise requires the real jsonschema validator "
        "to (incorrectly) consider -0.0 to satisfy {'minimum': 0} -- if this no longer holds, "
        "the danger this test exists to prove no longer applies and it should be revisited.",
    )

    try:
        strict_json.strict_json_loads("-1e-1000000", source="<selftest: minimum-0 danger case>")
    except SystemExit:
        pass
    else:
        raise SystemExit(
            "Self-test failure: strict_json_loads must reject '-1e-1000000' outright (a "
            "genuinely negative value that bare float() would silently underflow to -0.0, "
            "which would incorrectly satisfy a real {'minimum': 0} jsonschema constraint) "
            "rather than ever letting it reach schema validation as -0.0."
        )

    # A genuinely tiny but still representable positive value (well above
    # zero) must still validate as satisfying both a plain "minimum": 0 and
    # an "exclusiveMinimum": 0 constraint, through the real validator --
    # proving the guard above rejects only genuine underflow-to-zero, not
    # every small magnitude.
    tiny_positive_schema = {"type": "number", "exclusiveMinimum": 0, "maximum": 1}
    tiny_positive_validator = make_validator(tiny_positive_schema)
    tiny_positive_value = strict_json.strict_json_loads(
        "5e-324", source="<selftest: smallest representable positive double>"
    )
    require(
        len(list(tiny_positive_validator.iter_errors(tiny_positive_value))) == 0,
        "Self-test failure: the smallest representable positive double (5e-324) must satisfy "
        f"{{'exclusiveMinimum': 0, 'maximum': 1}}, got errors for {tiny_positive_value!r}.",
    )

    # A schema {"maximum": 1e300} boundary case: a value exactly at the
    # boundary must pass, and a value whose exact Decimal magnitude exceeds
    # float range entirely must never reach the validator at all (proven by
    # strict_json_loads rejecting it before this point, exercised already
    # in strict_json.run_self_tests(), not re-proven here) -- this proves
    # the boundary-adjacent ordinary value itself still validates correctly.
    boundary_schema = {"type": "number", "maximum": 1e300}
    boundary_validator = make_validator(boundary_schema)
    boundary_value = strict_json.strict_json_loads(
        "1e300", source="<selftest: float-max-adjacent ordinary value>"
    )
    require(
        len(list(boundary_validator.iter_errors(boundary_value))) == 0,
        f"Self-test failure: 1e300 must satisfy {{'maximum': 1e300}}, got errors for "
        f"{boundary_value!r}.",
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

    # RFC 6901 defines only two escape sequences ('~0' -> '~', '~1' -> '/');
    # a bare/malformed '~' not immediately followed by '0' or '1' must be
    # rejected deterministically rather than silently treated as a literal
    # '~' character, which could otherwise resolve a typo'd pointer to --
    # and mutate -- the wrong location instead of failing closed.
    tilde_base = {"a~b": "escaped-tilde-key", "a/b": "escaped-slash-key", "a~1b": "literal-tilde-one-key"}
    valid_tilde = apply_mutation(tilde_base, {"op": "replace", "pointer": "/a~0b", "value": "X"})
    require(
        valid_tilde["a~b"] == "X" and valid_tilde["a/b"] == "escaped-slash-key",
        "apply_mutation must decode a valid '~0' escape to a literal '~' and target only that key",
    )
    valid_slash = apply_mutation(tilde_base, {"op": "replace", "pointer": "/a~1b", "value": "Y"})
    require(
        valid_slash["a/b"] == "Y" and valid_slash["a~b"] == "escaped-tilde-key",
        "apply_mutation must decode a valid '~1' escape to a literal '/' and target only that key",
    )
    _expect_rejected(
        {"op": "replace", "pointer": "/a~b", "value": "X"},
        "replace (bare '~' not followed by '0' or '1')",
        doc=tilde_base,
    )
    _expect_rejected(
        {"op": "replace", "pointer": "/a~2b", "value": "X"},
        "replace ('~2', not a valid RFC 6901 escape)",
        doc=tilde_base,
    )
    _expect_rejected(
        {"op": "replace", "pointer": "/trailing~", "value": "X"},
        "replace (trailing bare '~' at end of segment)",
        doc=tilde_base,
    )


run_apply_mutation_self_test()
run_self_test()
run_numeric_overflow_underflow_schema_self_test()

print(
    f"Validated {len(documents)} contract documents, {len(fixtures)} fixtures, "
    f"{len(negative_fixtures)} negative regression fixtures (each derived from a real "
    f"positive fixture via a single mutation, plus a self-test proving exact-match precision), "
    f"{len(canonical_integer_checks)} canonical-integer boundary/token "
    f"{'check' if len(canonical_integer_checks) == 1 else 'checks'}, "
    f"{len(forward_compatibility_checks)} forward-compatibility "
    f"{'check' if len(forward_compatibility_checks) == 1 else 'checks'}, "
    f"{len(enum_boundary_checks)} closed-enum boundary checks, "
    f"{len(manifest_url_checks['accepted'])} accepted and {len(manifest_url_checks['rejected'])} "
    f"rejected manifest-URL bindings, {len(catalog_revision_checks['accepted'])} accepted and "
    f"{len(catalog_revision_checks['rejected'])} rejected catalog-revision bindings, "
    f"the locale-catalog provenance binding "
    f"({CATALOG_MANIFEST_PATH}), and the {legacy_checks['baselineRevision']} legacy-shape "
    "baseline."
)
