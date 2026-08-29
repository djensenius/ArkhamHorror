"""Shared strict JSON parsing for every contract-tooling script.

A bare `json.load`/`json.loads` silently accepts several things this
contract toolchain must not: duplicate object keys at any nesting depth
(including a plain key and a `\\uXXXX`-escaped form that decodes to the same
text -- Python's json decoder resolves such escapes into plain strings
*before* handing keys to `object_pairs_hook`, so ordinary string equality
already catches this), and the non-JSON `NaN`/`Infinity`/`-Infinity`
constants the stdlib `json` module accepts by default via `parse_constant`.
Strict UTF-8 decoding and rejection of trailing tokens after the top-level
value are both already the stdlib's default behavior (`json.loads` raises
`JSONDecodeError` for trailing garbage; explicit `.decode("utf-8", strict)`
below adds the same strictness for raw bytes, e.g. `git show` output, which
the stdlib json module would otherwise decode more leniently via its
internal latin-1 fallback path for bytes input).

Every governed JSON read across this contract tooling -- the manifest,
schemas, fixtures, negative-fixture descriptors, and the base-ref manifest
read via `git show <ref>:path` bytes -- must go through
`strict_json_loads`/`strict_json_load_path` rather than a bare
`json.load`/`json.loads`, so a hand-edited duplicate key (or an injected
NaN/Infinity) can never silently "win" via last-key-wins dict construction
without any tool in this toolchain ever noticing.

This module is deliberately a plain sibling `.py` file (not a package) next
to the scripts that import it: every script here is a standalone PEP 723
`uv run`-executed file, and Python always inserts an invoked script's own
containing directory at the front of `sys.path` before running it -- so
`import strict_json` resolves identically regardless of the caller's
current working directory, without needing `sys.path` manipulation, a
`PYTHONPATH` entry, or this repository being installed as a package. This
is verified directly by `check-schema-revision-drift.py`,
`validate-contract-fixtures.py`, and `update-manifest-hashes.py` all
importing it successfully when invoked as scripts (not merely by a unit
test importing it in isolation).
"""

from __future__ import annotations

import json
from pathlib import Path


class StrictJSONError(SystemExit):
    """Raised (as a `SystemExit` subclass, so it terminates the CLI the same
    way every other `require()` failure in this toolchain does, without
    needing a bespoke top-level except-clause at every call site) when JSON
    text violates one of this module's extra strictness rules."""


def _reject_duplicate_keys(pairs: list[tuple[str, object]]) -> dict:
    seen: dict[str, object] = {}
    for key, value in pairs:
        if key in seen:
            raise StrictJSONError(
                f"Duplicate JSON object key {key!r}: JSON object keys must be unique at "
                "every nesting depth (this includes a plain key and a distinct "
                "\\uXXXX-escaped form that decodes to the same text)."
            )
        seen[key] = value
    return seen


def _reject_constant(constant_name: str):
    raise StrictJSONError(
        f"Non-finite JSON constant {constant_name!r} is not permitted in governed contract "
        "artifacts; this toolchain requires strictly finite numeric values."
    )


def strict_json_loads(text_or_bytes, *, source: str = "<string>"):
    """Parse `text_or_bytes` (a `str`, or `bytes`/`bytearray` decoded as
    strict UTF-8) as JSON, rejecting duplicate object keys at any nesting
    depth and `NaN`/`Infinity`/`-Infinity` constants. `source` only makes
    error messages actionable; it does not affect parsing.
    """
    if isinstance(text_or_bytes, (bytes, bytearray)):
        try:
            text = text_or_bytes.decode("utf-8", errors="strict")
        except UnicodeDecodeError as exc:
            raise StrictJSONError(f"{source}: not valid strict UTF-8: {exc}") from exc
    else:
        text = text_or_bytes

    try:
        return json.loads(
            text,
            object_pairs_hook=_reject_duplicate_keys,
            parse_constant=_reject_constant,
        )
    except StrictJSONError:
        raise
    except json.JSONDecodeError as exc:
        raise StrictJSONError(f"{source}: invalid JSON: {exc}") from exc


def strict_json_load_path(path: Path):
    """Read and strictly parse a JSON file from disk (strict UTF-8, no
    duplicate keys at any depth, no NaN/Infinity constants)."""
    return strict_json_loads(path.read_bytes(), source=str(path))


def run_self_tests() -> None:
    """Prove every rejection this module claims to make, using small
    in-memory fixtures (no filesystem, no git) -- deterministic in any
    environment. Every script that imports this module calls this as part
    of its own `run_self_tests()`, so these are proven every time any of
    the three real CLI scripts runs, not merely by an isolated unit test.
    """

    def _expect_rejected(payload, label: str) -> None:
        try:
            strict_json_loads(payload, source=f"<selftest: {label}>")
        except StrictJSONError:
            pass
        else:
            raise SystemExit(
                f"Self-test failure: strict_json_loads accepted {label}, which it must reject."
            )

    # A well-formed document with no duplicates parses normally.
    ok = strict_json_loads('{"a": 1, "b": {"c": 2}}', source="<selftest: well-formed>")
    if ok != {"a": 1, "b": {"c": 2}}:
        raise SystemExit(
            "Self-test failure: strict_json_loads must parse a well-formed document unchanged."
        )

    # Duplicate key at the top level.
    _expect_rejected('{"id": 1, "id": 2}', "a duplicate top-level key")

    # Duplicate key nested inside another object.
    _expect_rejected('{"outer": {"id": 1, "id": 2}}', "a duplicate nested key")

    # Duplicate key specifically inside an `artifactHashes`-shaped map.
    _expect_rejected(
        '{"artifactHashes": {"a.json": "aaa", "a.json": "bbb"}}',
        "a duplicate key inside artifactHashes",
    )

    # Duplicate key specifically at a `schemaRevision`-adjacent position.
    _expect_rejected(
        '{"schemaRevision": "0.1.18", "schemaRevision": "0.1.19"}',
        "a duplicate schemaRevision key",
    )

    # A plain key and a distinct \\uXXXX escape sequence that decodes to the
    # exact same text ("id" via \\u0064 == "d") must still be treated as a
    # collision, since Python's json decoder resolves escapes into plain
    # strings before object_pairs_hook ever sees them.
    _expect_rejected('{"id": 1, "i\\u0064": 2}', "a plain key and its escaped-equivalent form")

    # NaN/Infinity/-Infinity constants.
    _expect_rejected("[NaN]", "a NaN constant")
    _expect_rejected("[Infinity]", "an Infinity constant")
    _expect_rejected("[-Infinity]", "a -Infinity constant")

    # Invalid UTF-8 bytes.
    _expect_rejected(b"{\"a\": 1, \xff\xfe}", "invalid UTF-8 bytes")

    # Trailing garbage after a complete top-level value.
    _expect_rejected('{"a": 1} garbage', "trailing tokens after the top-level value")

    # A non-duplicated, syntactically distinct pair of keys must NOT be
    # rejected merely for looking similar -- only an exact post-decode
    # string collision counts as a duplicate.
    ok2 = strict_json_loads('{"id": 1, "ids": 2}', source="<selftest: distinct keys>")
    if ok2 != {"id": 1, "ids": 2}:
        raise SystemExit(
            "Self-test failure: strict_json_loads must not reject distinct (non-colliding) keys."
        )
