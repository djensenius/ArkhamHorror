"""A closed, in-repository validator for the JSON Schema subset the published
locale-catalog v1 schemas actually use.

This exists so the locale-catalog fixture generator has *no* third-party
executable dependency. A validator installed from an index is code the
generator executes but cannot hash, which would leave the claim "every
executable byte of this generator is covered by `generatorSha256`" false; a
module in `scripts/` is hashed by the generator's own import closure, so it is
covered by exactly the same digest as the generator itself.

The subset is deliberately tiny and deliberately __fail-closed__: every
keyword, every `$ref` spelling and every `$schema` dialect this module does not
implement is a refusal, never a keyword that is quietly ignored. A silently
ignored keyword is the one failure mode that matters here, because it turns a
constraint the published schema states into a constraint nothing enforces.

Implemented (draft 2020-12 semantics, for the keywords the v1 schemas use):

    $ref (local `#/$defs/<name>` only), $defs, $id, $schema, title, description
    type, const, enum, not, oneOf
    required, properties, additionalProperties, propertyNames
    items, minItems, maxItems, uniqueItems
    minimum, maximum, minLength, maxLength, pattern

`pattern` is compiled with a small ECMA-262 alignment step: an unescaped `$`
outside a character class becomes `\\Z`, because JSON Schema's regular
expressions are ECMA-262 ones where `$` is end-of-input, while Python's `$`
also matches just before a trailing newline. Without that, `"1.0.0\\n"` would
satisfy `^[0-9]+\\.[0-9]+\\.[0-9]+$`.
"""

import json
import math
import re

SUPPORTED_DIALECT = "https://json-schema.org/draft/2020-12/schema"

# Keywords with no effect on validation. Listed rather than skipped so that a
# typo (`descrption`) is a refusal instead of an ignored constraint.
ANNOTATION_KEYWORDS = frozenset({"title", "description"})

# Keywords only the schema resource root may carry.
ROOT_KEYWORDS = frozenset({"$schema", "$id", "$defs"})

ASSERTION_KEYWORDS = frozenset(
    {
        "$ref",
        "type",
        "const",
        "enum",
        "not",
        "oneOf",
        "required",
        "properties",
        "additionalProperties",
        "propertyNames",
        "items",
        "minItems",
        "maxItems",
        "uniqueItems",
        "minimum",
        "maximum",
        "minLength",
        "maxLength",
        "pattern",
    }
)

JSON_TYPE_NAMES = frozenset({"object", "array", "string", "integer", "number", "boolean", "null"})

REF_PREFIX = "#/$defs/"


class SchemaSubsetError(SystemExit):
    """A schema used a construct this closed subset does not implement."""

    def __init__(self, message: str) -> None:
        super().__init__(f"json-schema subset: {message}")


def _refuse(message: str) -> None:
    raise SchemaSubsetError(message)


def _type_name(value: object) -> str:
    if value is None:
        return "null"
    if isinstance(value, bool):
        return "boolean"
    if isinstance(value, int):
        return "integer"
    if isinstance(value, float):
        return "number"
    if isinstance(value, str):
        return "string"
    if isinstance(value, list):
        return "array"
    if isinstance(value, dict):
        return "object"
    return "unsupported"


def _matches_type(value: object, name: str) -> bool:
    actual = _type_name(value)
    if name == "number":
        return actual in ("integer", "number")
    if name == "integer" and actual == "number":
        return isinstance(value, float) and math.isfinite(value) and value.is_integer()
    return actual == name


def ecma_pattern(pattern: str) -> str:
    """Rewrite an ECMA-262 `$` into Python's `\\Z`.

    Only an unescaped `$` outside a character class is an anchor; inside `[...]`
    and after a backslash it is a literal. Everything else in the patterns these
    schemas use has the same meaning in both flavours.
    """
    rewritten: list[str] = []
    escaped = False
    in_class = False
    for character in pattern:
        if escaped:
            rewritten.append(character)
            escaped = False
            continue
        if character == "\\":
            rewritten.append(character)
            escaped = True
            continue
        if in_class:
            rewritten.append(character)
            if character == "]":
                in_class = False
            continue
        if character == "[":
            in_class = True
            rewritten.append(character)
            continue
        if character == "$":
            rewritten.append("\\Z")
            continue
        rewritten.append(character)
    if escaped or in_class:
        _refuse(f"pattern {pattern!r} ends inside an escape or character class")
    return "".join(rewritten)


def _canonical(value: object) -> str:
    return json.dumps(value, sort_keys=True, ensure_ascii=False)


def _json_equal(left: object, right: object) -> bool:
    if isinstance(left, bool) or isinstance(right, bool):
        return type(left) is type(right) and left == right
    if isinstance(left, (int, float)) and isinstance(right, (int, float)):
        return math.isfinite(left) and math.isfinite(right) and left == right
    if isinstance(left, list) and isinstance(right, list):
        return len(left) == len(right) and all(_json_equal(a, b) for a, b in zip(left, right))
    if isinstance(left, dict) and isinstance(right, dict):
        return left.keys() == right.keys() and all(_json_equal(left[key], right[key]) for key in left)
    return type(left) is type(right) and left == right


def check_schema(schema: object, *, source: str) -> None:
    """Refuse a schema this subset cannot enforce exactly.

    Walks the whole document, including `$defs`, so an unimplemented keyword in
    a definition nobody happens to reference is still a refusal.
    """
    if not isinstance(schema, dict):
        _refuse(f"{source} is not a JSON object")
    dialect = schema.get("$schema")
    if dialect != SUPPORTED_DIALECT:
        _refuse(f"{source} declares dialect {dialect!r}; only {SUPPORTED_DIALECT} is implemented")
    defs = schema.get("$defs", {})
    if not isinstance(defs, dict):
        _refuse(f"{source} has a non-object $defs")
    _check_node(schema, source=source, pointer="#", root=True, definitions=set(defs))
    for name, definition in sorted(defs.items()):
        _check_node(definition, source=source, pointer=f"#/$defs/{name}", root=False, definitions=set(defs))


def _check_node(node: object, *, source: str, pointer: str, root: bool, definitions: set[str]) -> None:
    if not isinstance(node, dict):
        _refuse(f"{source}{pointer} is not a JSON object; boolean schemas are not implemented")
    allowed = ASSERTION_KEYWORDS | ANNOTATION_KEYWORDS | (ROOT_KEYWORDS if root else frozenset())
    for keyword in sorted(node):
        if keyword not in allowed:
            _refuse(
                f"{source}{pointer} uses {keyword!r}, which this closed subset does not implement; "
                "implement it here or stop using it in the published schema"
            )

    reference = node.get("$ref")
    if reference is not None:
        if not isinstance(reference, str) or not reference.startswith(REF_PREFIX):
            _refuse(f"{source}{pointer} has $ref {reference!r}; only local {REF_PREFIX}<name> is implemented")
        name = reference[len(REF_PREFIX) :]
        if name not in definitions:
            _refuse(f"{source}{pointer} references unknown definition {reference!r}")

    declared = node.get("type")
    if declared is not None:
        names = declared if isinstance(declared, list) else [declared]
        for name in names:
            if not isinstance(name, str) or name not in JSON_TYPE_NAMES:
                _refuse(f"{source}{pointer} declares unsupported type {name!r}")

    if "enum" in node and (not isinstance(node["enum"], list) or not node["enum"]):
        _refuse(f"{source}{pointer} has an empty or non-array enum")
    if "required" in node:
        required = node["required"]
        if not isinstance(required, list) or not all(isinstance(name, str) for name in required):
            _refuse(f"{source}{pointer} has a required list that is not an array of strings")
    for keyword in ("minItems", "maxItems", "minLength", "maxLength"):
        if keyword in node and (not isinstance(node[keyword], int) or isinstance(node[keyword], bool)):
            _refuse(f"{source}{pointer} has a non-integer {keyword}")
    for keyword in ("minimum", "maximum"):
        if keyword in node and (isinstance(node[keyword], bool) or not isinstance(node[keyword], (int, float))):
            _refuse(f"{source}{pointer} has a non-numeric {keyword}")
    if "uniqueItems" in node and not isinstance(node["uniqueItems"], bool):
        _refuse(f"{source}{pointer} has a non-boolean uniqueItems")
    if "pattern" in node:
        if not isinstance(node["pattern"], str):
            _refuse(f"{source}{pointer} has a non-string pattern")
        re.compile(ecma_pattern(node["pattern"]))
    if "additionalProperties" in node and node["additionalProperties"] is not False:
        if not isinstance(node["additionalProperties"], dict):
            _refuse(
                f"{source}{pointer} sets additionalProperties to {node['additionalProperties']!r}; "
                "only `false` or a subschema is implemented, so an open object is never mistaken "
                "for a closed one"
            )
        _check_node(
            node["additionalProperties"],
            source=source,
            pointer=f"{pointer}/additionalProperties",
            root=False,
            definitions=definitions,
        )
    if "properties" in node:
        properties = node["properties"]
        if not isinstance(properties, dict):
            _refuse(f"{source}{pointer} has a non-object properties")
        for name, subschema in sorted(properties.items()):
            _check_node(
                subschema, source=source, pointer=f"{pointer}/properties/{name}", root=False, definitions=definitions
            )
    for keyword in ("items", "propertyNames", "not"):
        if keyword in node:
            _check_node(
                node[keyword], source=source, pointer=f"{pointer}/{keyword}", root=False, definitions=definitions
            )
    if "oneOf" in node:
        branches = node["oneOf"]
        if not isinstance(branches, list) or not branches:
            _refuse(f"{source}{pointer} has an empty or non-array oneOf")
        for index, branch in enumerate(branches):
            _check_node(
                branch, source=source, pointer=f"{pointer}/oneOf/{index}", root=False, definitions=definitions
            )


def iter_errors(schema: dict, instance: object, *, source: str) -> list[str]:
    """Every way `instance` fails `schema`, as human-readable messages.

    The schema is expected to have passed `check_schema` already; callers that
    validate more than one instance against the same schema check it once.
    """
    errors: list[str] = []
    _validate(schema, instance, schema, "", errors)
    return [f"{source}: {message}" for message in errors]


def _resolve(root: dict, reference: str) -> dict:
    definition = root.get("$defs", {}).get(reference[len(REF_PREFIX) :])
    if not isinstance(definition, dict):
        _refuse(f"unresolvable $ref {reference!r}")
    return definition


def _validate(node: dict, instance: object, root: dict, path: str, errors: list[str]) -> None:
    where = path or "<root>"

    reference = node.get("$ref")
    if reference is not None:
        _validate(_resolve(root, reference), instance, root, path, errors)

    declared = node.get("type")
    if declared is not None:
        names = declared if isinstance(declared, list) else [declared]
        if not any(_matches_type(instance, name) for name in names):
            errors.append(f"{where} is {_type_name(instance)}, expected {' or '.join(names)}")
            return

    if "const" in node and not _json_equal(instance, node["const"]):
        errors.append(f"{where} is {_canonical(instance)}, expected the constant {_canonical(node['const'])}")
    if "enum" in node and not any(_json_equal(instance, option) for option in node["enum"]):
        errors.append(f"{where} is {_canonical(instance)}, which is not one of {_canonical(node['enum'])}")

    if "not" in node:
        rejected: list[str] = []
        _validate(node["not"], instance, root, path, rejected)
        if not rejected:
            errors.append(f"{where} matches a schema it must not match")
    if "oneOf" in node:
        matched = 0
        for branch in node["oneOf"]:
            branch_errors: list[str] = []
            _validate(branch, instance, root, path, branch_errors)
            if not branch_errors:
                matched += 1
        if matched != 1:
            errors.append(f"{where} matches {matched} of {len(node['oneOf'])} oneOf branches, expected exactly 1")

    if isinstance(instance, str) and not isinstance(instance, bool):
        _validate_string(node, instance, where, errors)
    if isinstance(instance, (int, float)) and not isinstance(instance, bool):
        if "minimum" in node and instance < node["minimum"]:
            errors.append(f"{where} is {instance}, below the minimum {node['minimum']}")
        if "maximum" in node and instance > node["maximum"]:
            errors.append(f"{where} is {instance}, above the maximum {node['maximum']}")
    if isinstance(instance, list):
        _validate_array(node, instance, root, path, where, errors)
    if isinstance(instance, dict):
        _validate_object(node, instance, root, path, where, errors)


def _validate_string(node: dict, instance: str, where: str, errors: list[str]) -> None:
    if "minLength" in node and len(instance) < node["minLength"]:
        errors.append(f"{where} is shorter than the minimum {node['minLength']} characters")
    if "maxLength" in node and len(instance) > node["maxLength"]:
        errors.append(f"{where} is longer than the maximum {node['maxLength']} characters")
    if "pattern" in node and re.search(ecma_pattern(node["pattern"]), instance) is None:
        errors.append(f"{where} does not match {node['pattern']!r}")


def _validate_array(node: dict, instance: list, root: dict, path: str, where: str, errors: list[str]) -> None:
    if "minItems" in node and len(instance) < node["minItems"]:
        errors.append(f"{where} has {len(instance)} items, fewer than the minimum {node['minItems']}")
    if "maxItems" in node and len(instance) > node["maxItems"]:
        errors.append(f"{where} has {len(instance)} items, more than the maximum {node['maxItems']}")
    if node.get("uniqueItems"):
        for index, item in enumerate(instance):
            if any(_json_equal(item, prior) for prior in instance[:index]):
                errors.append(f"{where} repeats the item {_canonical(item)}")
                break
    if "items" in node:
        for index, item in enumerate(instance):
            _validate(node["items"], item, root, f"{path}/{index}", errors)


def _validate_object(node: dict, instance: dict, root: dict, path: str, where: str, errors: list[str]) -> None:
    for name in node.get("required", []):
        if name not in instance:
            errors.append(f"{where} is missing the required property {name!r}")
    properties = node.get("properties", {})
    additional = node.get("additionalProperties")
    if additional is not None:
        for name in sorted(instance):
            if name in properties:
                continue
            if additional is False:
                errors.append(f"{where} has the unexpected property {name!r}")
            else:
                _validate(additional, instance[name], root, f"{path}/{name}", errors)
    if "propertyNames" in node:
        for name in sorted(instance):
            _validate(node["propertyNames"], name, root, f"{path}/{name}", errors)
    for name, subschema in properties.items():
        if name in instance:
            _validate(subschema, instance[name], root, f"{path}/{name}", errors)


def run_self_tests() -> None:
    """Prove the parts that would fail silently if they were wrong: an
    unimplemented keyword is refused rather than ignored, and each implemented
    keyword actually bites.
    """

    def refused(schema: object, label: str) -> None:
        try:
            check_schema(schema, source="<self-test>")
        except SchemaSubsetError:
            return
        raise SystemExit(f"json-schema subset: Self-test failure: {label} was accepted by check_schema")

    base = {"$schema": SUPPORTED_DIALECT}
    refused({**base, "allOf": []}, "an unimplemented allOf")
    refused({**base, "patternProperties": {}}, "an unimplemented patternProperties")
    refused({**base, "format": "uri"}, "an unimplemented format")
    refused({**base, "descrption": "typo"}, "a misspelled description")
    refused({**base, "additionalProperties": True}, "an open additionalProperties")
    refused({**base, "additionalProperties": {"anyOf": []}}, "an unimplemented keyword under additionalProperties")
    refused({**base, "type": "integers"}, "an unknown type name")
    refused({**base, "$ref": "https://example.test/x"}, "a remote $ref")
    refused({**base, "$ref": "#/$defs/missing"}, "a dangling $ref")
    refused({"$schema": "https://json-schema.org/draft-07/schema#"}, "an unsupported dialect")
    refused({**base, "properties": {"a": {"maxLength": "3"}}}, "a non-integer maxLength")

    schema = {
        "$schema": SUPPORTED_DIALECT,
        "type": "object",
        "additionalProperties": False,
        "required": ["version", "items"],
        "properties": {
            "version": {"$ref": "#/$defs/version"},
            "items": {
                "type": "array",
                "minItems": 1,
                "maxItems": 2,
                "uniqueItems": True,
                "items": {"type": "integer", "minimum": 0, "maximum": 9},
            },
            "kind": {"enum": ["a", "b"]},
            "flag": {"const": True},
            "name": {"type": "string", "minLength": 1, "maxLength": 3},
            "choice": {"oneOf": [{"type": "string"}, {"type": "integer"}]},
            "safe": {"type": "string", "not": {"pattern": "\\.\\."}},
        },
        "$defs": {"version": {"type": "string", "pattern": "^[0-9]+\\.[0-9]+\\.[0-9]+$"}},
    }
    check_schema(schema, source="<self-test>")

    valid = {"version": "1.0.0", "items": [1, 2], "kind": "a", "flag": True, "name": "ok", "choice": 3, "safe": "x"}
    if iter_errors(schema, valid, source="<self-test>"):
        raise SystemExit("json-schema subset: Self-test failure: a valid instance was rejected")

    cases: list[tuple[str, dict]] = [
        ("a missing required property", {key: value for key, value in valid.items() if key != "version"}),
        ("an unexpected property", {**valid, "extra": 1}),
        ("a pattern violation", {**valid, "version": "1.0"}),
        ("a trailing newline past an ECMA $ anchor", {**valid, "version": "1.0.0\n"}),
        ("a wrong type", {**valid, "version": 100}),
        ("an out-of-range integer", {**valid, "items": [10]}),
        ("too few items", {**valid, "items": []}),
        ("too many items", {**valid, "items": [1, 2, 3]}),
        ("a duplicated item", {**valid, "items": [1, 1]}),
        ("a value outside the enum", {**valid, "kind": "c"}),
        ("a value that is not the const", {**valid, "flag": False}),
        ("a string that is too long", {**valid, "name": "long"}),
        ("a string that is too short", {**valid, "name": ""}),
        ("a value matching no oneOf branch", {**valid, "choice": []}),
        ("a value matching a forbidden pattern", {**valid, "safe": "a/../b"}),
        ("a boolean where an integer is required", {**valid, "items": [True]}),
    ]
    for label, instance in cases:
        if not iter_errors(schema, instance, source="<self-test>"):
            raise SystemExit(f"json-schema subset: Self-test failure: {label} was accepted")

    names_schema = {
        "$schema": SUPPORTED_DIALECT,
        "type": "object",
        "propertyNames": {"pattern": "^[a-z]+$"},
        "additionalProperties": {"type": "integer"},
    }
    check_schema(names_schema, source="<self-test>")
    if iter_errors(names_schema, {"ok": 1}, source="<self-test>"):
        raise SystemExit("json-schema subset: Self-test failure: a valid property name was rejected")
    if not iter_errors(names_schema, {"Bad1": 1}, source="<self-test>"):
        raise SystemExit("json-schema subset: Self-test failure: an invalid property name was accepted")
    if not iter_errors(names_schema, {"ok": "1"}, source="<self-test>"):
        raise SystemExit(
            "json-schema subset: Self-test failure: a subschema additionalProperties was not applied"
        )
    numeric_schema = {
        "$schema": SUPPORTED_DIALECT,
        "type": "array",
        "uniqueItems": True,
        "items": {"type": "integer"},
    }
    check_schema(numeric_schema, source="<self-test>")
    if iter_errors(numeric_schema, [1.0], source="<self-test>"):
        raise SystemExit("json-schema subset: Self-test failure: an integral JSON number was rejected as integer")
    if not iter_errors(numeric_schema, [1, 1.0], source="<self-test>"):
        raise SystemExit("json-schema subset: Self-test failure: JSON-equal numbers bypassed uniqueItems")

    if ecma_pattern("^a$") != "^a\\Z":
        raise SystemExit("json-schema subset: Self-test failure: the ECMA $ anchor was not rewritten")
    if ecma_pattern("^[a$]\\$$") != "^[a$]\\$\\Z":
        raise SystemExit("json-schema subset: Self-test failure: a literal $ was rewritten as an anchor")
