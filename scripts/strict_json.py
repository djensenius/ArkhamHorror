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
`JSONDecodeError` for trailing garbage; for raw bytes input, e.g. `git show`
output, the stdlib `json` module already autodetects UTF-8/UTF-16/UTF-32 via
BOM/null-byte heuristics and would itself reject invalid UTF-8 -- the
explicit `.decode("utf-8", errors="strict")` below does not change that
strictness, it exists only to make the expected encoding explicit rather
than relying on autodetection, and to raise this module's own controlled
`StrictJSONError` with a clear message instead of a raw `UnicodeDecodeError`
propagating from inside the stdlib decoder).

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


def _reject_duplicate_keys(pairs: list[tuple[str, object]], *, source: str) -> dict:
    seen: dict[str, object] = {}
    for key, value in pairs:
        if key in seen:
            raise StrictJSONError(
                f"{source}: duplicate JSON object key {key!r}: JSON object keys must be unique "
                "at every nesting depth (this includes a plain key and a distinct "
                "\\uXXXX-escaped form that decodes to the same text)."
            )
        seen[key] = value
    return seen


def _reject_constant(constant_name: str, *, source: str):
    raise StrictJSONError(
        f"{source}: non-finite JSON constant {constant_name!r} is not permitted in governed "
        "contract artifacts; this toolchain requires strictly finite numeric values."
    )


def strict_json_loads(text_or_bytes, *, source: str = "<string>"):
    """Parse `text_or_bytes` (a `str`, or `bytes`/`bytearray` decoded as
    strict UTF-8) as JSON, rejecting duplicate object keys at any nesting
    depth and `NaN`/`Infinity`/`-Infinity` constants. `source` makes every
    error message actionable (including duplicate-key and NaN/Infinity
    rejections, which are otherwise raised deep inside `json.loads`'s
    `object_pairs_hook`/`parse_constant` callbacks with no built-in access
    to which file/ref was being parsed) and does not affect parsing. Any
    other input type (e.g. a `dict`, `Path`, or `int` passed by mistake) is
    rejected with the same controlled `StrictJSONError`/`SystemExit` this
    module always raises, rather than letting a raw `TypeError` escape from
    `json.loads` for a non-`str` argument.
    """
    if isinstance(text_or_bytes, (bytes, bytearray)):
        try:
            text = text_or_bytes.decode("utf-8", errors="strict")
        except UnicodeDecodeError as exc:
            raise StrictJSONError(f"{source}: not valid strict UTF-8: {exc}") from exc
    elif isinstance(text_or_bytes, str):
        text = text_or_bytes
    else:
        raise StrictJSONError(
            f"{source}: strict_json_loads requires a str, bytes, or bytearray, got "
            f"{type(text_or_bytes).__name__!r}."
        )

    try:
        return json.loads(
            text,
            object_pairs_hook=lambda pairs: _reject_duplicate_keys(pairs, source=source),
            parse_constant=lambda constant_name: _reject_constant(constant_name, source=source),
        )
    except StrictJSONError:
        raise
    except json.JSONDecodeError as exc:
        raise StrictJSONError(f"{source}: invalid JSON: {exc}") from exc


def strict_json_load_path(path: Path):
    """Read and strictly parse a JSON file from disk (strict UTF-8, no
    duplicate keys at any depth, no NaN/Infinity constants)."""
    return strict_json_loads(path.read_bytes(), source=str(path))


def canonicalize_manifest_bytes(manifest: dict) -> bytes:
    """The single shared implementation of contracts/manifest.json's
    self-hash canonicalization, used by both `update-manifest-hashes.py`
    (which writes `artifactHashes["contracts/manifest.json"]`) and
    `check-schema-revision-drift.py` (which recomputes and verifies it).
    Keeping exactly one implementation -- rather than two independently
    maintained copies -- means a future tweak to the canonical form (e.g.
    `separators`/`ensure_ascii`/`sort_keys`/`indent`/trailing newline)
    can never update one script but not the other, which would otherwise
    produce a hard-to-debug hash mismatch between the two tools.

    Zeroes out the self-referential `artifactHashes` map so the manifest
    can be hashed like any other governed artifact without hashing its own
    hash map (which would be circular).
    """
    canonical = dict(manifest)
    canonical["artifactHashes"] = {}
    return json.dumps(canonical, sort_keys=True, indent=2).encode("utf-8") + b"\n"


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

    # A caller accidentally passing a non-str/bytes/bytearray value (e.g. a
    # dict, Path, or int) must raise the toolchain's controlled
    # StrictJSONError, never a raw TypeError from json.loads.
    for bad_input, label in (
        ({"already": "parsed"}, "a dict"),
        (Path("contracts/manifest.json"), "a Path"),
        (42, "an int"),
        (None, "None"),
    ):
        try:
            strict_json_loads(bad_input, source=f"<selftest: {label}>")
        except StrictJSONError:
            pass
        except TypeError:
            raise SystemExit(
                f"Self-test failure: strict_json_loads raised a raw TypeError for {label} "
                "instead of the controlled StrictJSONError."
            )
        else:
            raise SystemExit(
                f"Self-test failure: strict_json_loads accepted {label}, which it must reject."
            )
