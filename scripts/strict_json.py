"""Shared strict JSON parsing for every contract-tooling script.

A bare `json.load`/`json.loads` silently accepts several things this
contract toolchain must not: duplicate object keys at any nesting depth
(including a plain key and a `\\uXXXX`-escaped form that decodes to the same
text -- Python's json decoder resolves such escapes into plain strings
*before* handing keys to `object_pairs_hook`, so ordinary string equality
already catches this), and the non-JSON `NaN`/`Infinity`/`-Infinity`
constants the stdlib `json` module accepts by default via `parse_constant`.
It also silently accepts ordinary (syntactically valid) number literals
whose exponent overflows finite `float` range -- e.g. `1e9999` -- by
coercing them to the float `inf` via the bare `float()` constructor with no
error at all; `parse_constant` cannot catch this, since it only intercepts
the three named literal tokens `NaN`/`Infinity`/`-Infinity`, not ordinary
number syntax that merely evaluates too large. This module's custom
`parse_float` (`_parse_float_or_reject_overflow`) closes that gap by first
parsing the exact value via `decimal.Decimal` (which never silently
overflows) to determine, precisely, whether it fits in a finite `float`,
and rejecting it outright if not, rather than ever returning `inf`.
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
import stat
import subprocess
import sys
from decimal import Decimal
from pathlib import Path

# The largest finite magnitude an IEEE-754 double (Python's `float`) can
# represent. Used only to *detect* exponent overflow exactly (via `Decimal`,
# which never silently overflows), not as a parsing float itself.
_FLOAT_MAX_MAGNITUDE = Decimal(sys.float_info.max)


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


def _parse_float_or_reject_overflow(raw: str, *, source: str) -> float:
    """`json.loads`'s default `parse_float` is the bare `float` constructor,
    which silently coerces a syntactically valid but astronomically large
    JSON number (e.g. the literal token `1e9999`, not the `Infinity`
    constant `_reject_constant` already rejects) into the float `inf`
    without raising anything -- `_reject_constant` cannot see this case at
    all, since it only ever runs for the three named non-JSON constant
    tokens, and `1e9999` is ordinary JSON number syntax.

    This first parses the raw number text as `decimal.Decimal`, which is
    exact and never silently overflows to infinity for any finite-length
    JSON number literal, so whether the value fits in a `float` can be
    determined precisely -- rather than trusting `float()`'s own silent
    rounding/overflow behavior to decide, which is exactly the bug this
    function exists to close. A value that fits is returned as an ordinary
    `float` (matching the stdlib's default `parse_float` return type
    exactly, so every downstream consumer keeps working unchanged): this
    toolchain's JSON Schema validation (the `jsonschema` library) checks
    `isinstance(x, (int, float))` for the `"number"` type and does not
    recognize `Decimal`, and `json.dumps` (used by
    `canonicalize_manifest_bytes` and every fixture-writing tool) does not
    serialize `Decimal` at all by default -- so keeping the arbitrary-precision
    `Decimal` value itself flowing downstream is not viable in this
    toolchain, and this function fails closed (rejects) instead of ever
    returning a non-finite `float`, rather than silently returning `inf`.
    """
    decimal_value = Decimal(raw)
    if abs(decimal_value) > _FLOAT_MAX_MAGNITUDE:
        raise StrictJSONError(
            f"{source}: JSON number {raw!r} overflows the finite range a JSON Schema "
            "\"number\"/\"float\" can represent in this toolchain (exact value computed via "
            "decimal.Decimal to avoid trusting float()'s own silent overflow-to-inf behavior); "
            "non-finite numeric values are not permitted in governed contract artifacts."
        )
    return float(raw)


def strict_json_loads(text_or_bytes, *, source: str = "<string>"):
    """Parse `text_or_bytes` (a `str`, or `bytes`/`bytearray` decoded as
    strict UTF-8) as JSON, rejecting duplicate object keys at any nesting
    depth, `NaN`/`Infinity`/`-Infinity` constants, and any ordinary
    fractional/exponent number literal that overflows finite `float` range
    (e.g. `1e9999`, which the bare stdlib `float()` constructor would
    silently coerce to `inf` rather than raising -- see
    `_parse_float_or_reject_overflow`). `source` makes every error message
    actionable (including duplicate-key, NaN/Infinity, and float-overflow
    rejections, which are otherwise raised deep inside `json.loads`'s
    `object_pairs_hook`/`parse_constant`/`parse_float` callbacks with no
    built-in access to which file/ref was being parsed) and does not affect
    parsing. Any other input type (e.g. a `dict`, `Path`, or `int` passed by
    mistake) is rejected with the same controlled `StrictJSONError`/
    `SystemExit` this module always raises, rather than letting a raw
    `TypeError` escape from `json.loads` for a non-`str` argument.
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
            parse_float=lambda raw: _parse_float_or_reject_overflow(raw, source=source),
        )
    except StrictJSONError:
        raise
    except json.JSONDecodeError as exc:
        raise StrictJSONError(f"{source}: invalid JSON: {exc}") from exc


def strict_json_load_path(path: Path):
    """Read and strictly parse a JSON file from disk (strict UTF-8, no
    duplicate keys at any depth, no NaN/Infinity constants)."""
    return strict_json_loads(path.read_bytes(), source=str(path))


def strict_validate_governed_bytes_if_json(relative_path: str, content: bytes, *, source: str) -> None:
    """Strictly parse `content` as JSON (raising this module's own
    `StrictJSONError` on any violation -- duplicate keys, NaN/Infinity,
    float overflow, invalid UTF-8, trailing tokens) when `relative_path`
    looks like a JSON file (ends with `.json`); does nothing for a governed
    path that isn't JSON (this contract's manifest currently governs exactly
    two such documents, `contracts/openapi.yaml` and `contracts/asyncapi.yaml`,
    each already validated by its own dedicated tool elsewhere in this
    toolchain rather than by this JSON-specific strictness layer).

    Every hashing tool in this toolchain (`update-manifest-hashes.py`,
    `check-schema-revision-drift.py`'s worktree *and* base-ref hashers) must
    call this immediately before hashing a governed artifact's raw bytes, so
    a hand-corrupted governed JSON file -- whether read from the current
    worktree or from an immutable historical `git show <ref>:path` blob -- is
    caught with a clear, source-qualified diagnostic at hash time, rather
    than being silently blessed into `artifactHashes` as opaque bytes that
    were never actually confirmed to be valid JSON at all.
    """
    if relative_path.endswith(".json"):
        strict_json_loads(content, source=source)


class GovernedPathError(SystemExit):
    """Raised (as a `SystemExit` subclass, like `StrictJSONError`) when a
    manifest-declared governed artifact path string is not already a
    normalized, canonical, repo-relative POSIX path confined to this
    contract's small, fixed set of intended governed locations, or when a
    path that does pass that check turns out, on disk or in a historical
    git tree, to not be an ordinary regular file/blob.
    """


# The complete, fixed set of locations this contract's manifest is allowed
# to declare a governed artifact under. Kept as an explicit allow-list
# (rather than merely rejecting '..'/absolute paths) so a manifest entry can
# never reference anything outside this contract's own tree at all, not
# even another legitimate part of this repository.
_GOVERNED_EXACT_PATHS = frozenset(
    {
        "contracts/manifest.json",
        "contracts/openapi.yaml",
        "contracts/asyncapi.yaml",
        "contracts/route-inventory.json",
    }
)
_GOVERNED_SINGLE_LEVEL_DIRECTORIES = ("contracts/schemas/", "contracts/fixtures/")


def validate_governed_path(relative_path: object) -> str:
    """Strictly validate a manifest-declared governed artifact path string
    and return it unchanged if it passes.

    Rejects (via `GovernedPathError`): a non-string value; an empty path; an
    absolute path (leading `/`); a backslash anywhere (this toolchain's
    paths are always POSIX-style forward-slash -- a literal backslash is
    either a mistake or a non-canonical alias on some filesystems); any
    ASCII control character; any `.`/`..`/empty path segment (which would
    make the string a non-canonical alias of some other, differently
    spelled path -- e.g. a `..` segment could reference a file entirely
    outside this contract's tree, and a `./`/`//`/trailing-slash segment
    resolves to the same file as its canonical form without being byte-
    identical to it, which this toolchain's exact string-keyed hash
    comparison is not designed to reconcile); and any path outside the
    small fixed set of locations this contract actually governs (exactly
    `contracts/manifest.json`, `contracts/openapi.yaml`,
    `contracts/asyncapi.yaml`, `contracts/route-inventory.json`, a single
    file directly under `contracts/schemas/`, or a single file directly
    under `contracts/fixtures/` -- never a nested subdirectory of either,
    since this contract has never had one and a manifest entry claiming one
    would not match any real schema/fixture-loading convention here).
    """
    if not isinstance(relative_path, str) or not relative_path:
        raise GovernedPathError(
            f"governed artifact path must be a non-empty string, got {relative_path!r}"
        )
    if relative_path.startswith("/"):
        raise GovernedPathError(f"governed artifact path {relative_path!r} must not be absolute.")
    if "\\" in relative_path:
        raise GovernedPathError(
            f"governed artifact path {relative_path!r} contains a backslash; this toolchain's "
            "governed paths are always POSIX-style forward-slash, never a Windows-style "
            "separator or an alias of one."
        )
    if any(ord(character) < 0x20 or ord(character) == 0x7F for character in relative_path):
        raise GovernedPathError(
            f"governed artifact path {relative_path!r} contains an ASCII control character."
        )
    segments = relative_path.split("/")
    if any(segment in ("", ".", "..") for segment in segments):
        raise GovernedPathError(
            f"governed artifact path {relative_path!r} is not a normalized, canonical path: an "
            "empty segment (leading/trailing/doubled slash), a '.' segment, or a '..' segment "
            "is never permitted -- every governed path must already be written in exactly its "
            "canonical form."
        )
    if relative_path in _GOVERNED_EXACT_PATHS:
        return relative_path
    for directory in _GOVERNED_SINGLE_LEVEL_DIRECTORIES:
        if relative_path.startswith(directory):
            remainder = relative_path[len(directory) :]
            if remainder and "/" not in remainder:
                return relative_path
    raise GovernedPathError(
        f"governed artifact path {relative_path!r} is not one of this contract's intended "
        "governed locations (contracts/manifest.json, contracts/openapi.yaml, "
        "contracts/asyncapi.yaml, contracts/route-inventory.json, a single file directly under "
        "contracts/schemas/, or a single file directly under contracts/fixtures/)."
    )


def read_governed_worktree_bytes(root: Path, relative_path: str) -> bytes:
    """Validate `relative_path` (`validate_governed_path`), read its bytes
    from the current worktree under `root` (rejecting anything that is not
    an ordinary regular file via `os.lstat` -- unlike `Path.is_file()`/
    `Path.read_bytes()`, `lstat` does not follow a symlink to whatever file
    it currently happens to point at), and, if the path looks like JSON
    (`strict_validate_governed_bytes_if_json`), strictly parse those bytes
    before returning them -- so a caller that goes on to hash the returned
    bytes never hashes content this toolchain has not already confirmed is
    both a genuine regular file and (for `.json` paths) well-formed strict
    JSON.

    The regular-file check matters specifically because git itself stores a
    symlink as a blob containing its target path text at mode `120000`: a
    governed manifest entry naming an on-disk symlink could cause this
    toolchain's worktree hash to silently reflect whatever arbitrary file
    the symlink currently resolves to, while a historical `git show`/
    `git ls-tree` read of that *same* pathname at an older commit would see
    the literal symlink-target text instead -- two readers of "the same
    governed path" silently disagreeing about what bytes it even contains.
    Rejecting any non-regular-file mode outright (symlink, directory,
    device, etc.) removes that ambiguity entirely rather than trying to make
    both readers agree on a resolved target.
    """
    validated = validate_governed_path(relative_path)
    absolute_path = root / validated
    try:
        file_stat = absolute_path.lstat()
    except FileNotFoundError as exc:
        raise GovernedPathError(
            f"governed artifact path {validated!r} does not exist on disk (resolved: "
            f"{absolute_path})."
        ) from exc
    if not stat.S_ISREG(file_stat.st_mode):
        raise GovernedPathError(
            f"governed artifact path {validated!r} is not a regular file on disk (resolved: "
            f"{absolute_path}, st_mode={oct(file_stat.st_mode)}); a symlink, directory, or other "
            "special file is never permitted as a governed artifact."
        )
    content = absolute_path.read_bytes()
    strict_validate_governed_bytes_if_json(validated, content, source=str(absolute_path))
    return content


def read_governed_git_ref_bytes(root: Path, ref: str, relative_path: str) -> bytes | None:
    """Validate `relative_path` (`validate_governed_path`), read its bytes
    at historical/base git ref `ref` under `root` (first verifying via `git
    ls-tree` that the path resolves to exactly one regular, non-executable
    blob at mode `100644` -- never a symlink (`120000`), an executable file
    (`100755`), a submodule/gitlink (`160000`), or a tree/directory --
    before reading its content with `git show`), and, if the path looks
    like JSON (`strict_validate_governed_bytes_if_json`), strictly parse
    those bytes before returning them. Returns `None` if the path does not
    exist at that ref (mirroring the existing `git_show` helper's
    None-for-missing convention), so callers that treat "missing at this
    ref" as a legitimate case (e.g. a governed artifact that was only added
    after the base commit) keep working unchanged.

    Verifying the tree entry's exact mode+type before ever reading content
    is what actually closes this gap: without it, `git show ref:path` alone
    happily prints a symlink's target-path text or an executable script's
    contents exactly as if either were ordinary JSON/YAML, with nothing to
    distinguish that from a real governed document.
    """
    validated = validate_governed_path(relative_path)
    ls_tree_result = subprocess.run(
        ["git", "ls-tree", "-z", ref, "--", validated],
        cwd=root,
        capture_output=True,
    )
    if ls_tree_result.returncode != 0:
        raise GovernedPathError(
            f"git ls-tree {ref}:{validated} failed: "
            f"{ls_tree_result.stderr.decode('utf-8', errors='replace')}"
        )
    entries = [entry for entry in ls_tree_result.stdout.split(b"\0") if entry]
    if not entries:
        return None
    if len(entries) != 1:
        raise GovernedPathError(
            f"git ls-tree {ref}:{validated} unexpectedly matched {len(entries)} entries; expected "
            "exactly one."
        )
    mode_bytes, remainder = entries[0].split(b" ", 1)
    object_type_bytes, _remainder = remainder.split(b" ", 1)
    mode = mode_bytes.decode("ascii", errors="replace")
    object_type = object_type_bytes.decode("ascii", errors="replace")
    if mode != "100644" or object_type != "blob":
        raise GovernedPathError(
            f"governed artifact path {validated!r} at {ref!r} is not a regular, non-executable "
            f"blob (mode 100644): got mode {mode!r}, type {object_type!r}. A symlink (120000), "
            "executable file (100755), submodule/gitlink (160000), or tree/directory is never "
            "permitted as a governed artifact."
        )
    show_result = subprocess.run(
        ["git", "show", f"{ref}:{validated}"],
        cwd=root,
        capture_output=True,
    )
    if show_result.returncode != 0:
        raise GovernedPathError(
            f"git show {ref}:{validated} failed even after git ls-tree confirmed a regular "
            f"blob there: {show_result.stderr.decode('utf-8', errors='replace')}"
        )
    content = show_result.stdout
    strict_validate_governed_bytes_if_json(validated, content, source=f"{ref}:{validated}")
    return content


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

    # An ordinary (syntactically valid) number literal whose exponent
    # overflows finite float range must be rejected explicitly, not
    # silently coerced to inf the way bare float() would.
    _expect_rejected("[1e9999]", "a positive exponent-overflow number literal")
    _expect_rejected("[-1e9999]", "a negative exponent-overflow number literal")

    # Ordinary finite fractional/exponent numbers must still parse exactly
    # like the stdlib default (as a plain `float`), unaffected by the
    # overflow guard.
    ok3 = strict_json_loads(
        '[1.5, -2.5e-3, 3.14159, 1e10, -1e10, 0.0, -0.0]',
        source="<selftest: ordinary fractional/exponent values>",
    )
    expected3 = [1.5, -2.5e-3, 3.14159, 1e10, -1e10, 0.0, -0.0]
    if ok3 != expected3 or any(not isinstance(value, float) for value in ok3):
        raise SystemExit(
            "Self-test failure: strict_json_loads must parse ordinary finite "
            f"fractional/exponent numbers exactly like the stdlib default, got {ok3!r}."
        )

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


def run_governed_bytes_self_tests() -> None:
    """Prove `strict_validate_governed_bytes_if_json`'s two-way behavior:
    strict for '.json'-suffixed governed paths, a deliberate no-op for
    non-JSON governed documents. Kept as a separate function (rather than
    folded into `run_self_tests`) so a caller that only cares about the core
    parser's rejections isn't forced to also exercise this governed-path
    dispatch helper, though every script that hashes governed artifacts
    calls both.
    """
    try:
        strict_validate_governed_bytes_if_json(
            "contracts/schemas/selftest.schema.json",
            b'{"a": 1, "a": 2}',
            source="<selftest: governed .json path with duplicate key>",
        )
    except StrictJSONError:
        pass
    else:
        raise SystemExit(
            "Self-test failure: strict_validate_governed_bytes_if_json must reject malformed "
            "(duplicate-key) content for a '.json'-suffixed governed path."
        )

    # A non-JSON governed document (e.g. contracts/openapi.yaml/asyncapi.yaml)
    # must never be rejected here, even when its bytes are not valid JSON at
    # all -- those documents are validated by their own dedicated tools
    # elsewhere in this toolchain, not by this JSON-specific strictness layer.
    strict_validate_governed_bytes_if_json(
        "contracts/openapi.yaml",
        b"openapi: 3.1.0\nnot: [valid, json",
        source="<selftest: non-JSON governed document>",
    )

    # A well-formed '.json'-suffixed governed path's bytes must pass silently.
    strict_validate_governed_bytes_if_json(
        "contracts/fixtures/selftest.json",
        b'{"a": 1}',
        source="<selftest: well-formed governed .json path>",
    )


def run_governed_path_self_tests(root: Path) -> None:
    """Prove `validate_governed_path`, `read_governed_worktree_bytes`, and
    `read_governed_git_ref_bytes`'s rejections and acceptances end-to-end
    against `root` (a real repository root, since these three functions do
    real filesystem/git I/O, not merely string validation). Every scratch
    file, symlink, and git object this creates uses a per-run unique
    sentinel name and is either cleaned up in a `finally` block (filesystem
    entries) or left as an ordinary dangling, unreferenced git object
    (git plumbing objects, exactly like `run_governed_json_strictness_self_tests`
    in `check-schema-revision-drift.py` already does), never touching any
    real branch, tag, the index, or this repository's actual history.
    """
    import uuid

    # -- Pure path-string validation (no I/O) --------------------------------
    for bad_path, label in (
        ("", "an empty path"),
        ("/contracts/schemas/x.schema.json", "an absolute path"),
        ("contracts\\schemas\\x.schema.json", "a backslash-separated path"),
        ("contracts/schemas/x\x00.schema.json", "a path with a control character"),
        ("contracts/schemas/../x.schema.json", "a path with a '..' segment"),
        ("contracts/schemas/./x.schema.json", "a path with a '.' segment"),
        ("contracts/schemas//x.schema.json", "a path with a doubled ('//' empty) segment"),
        ("contracts/schemas/x.schema.json/", "a path with a trailing slash (empty segment)"),
        ("backend/arkham-api/package.yaml", "a path outside the governed subtree"),
        ("contracts/schemas/nested/x.schema.json", "a nested-subdirectory schema path"),
        ("contracts/fixtures/nested/x.json", "a nested-subdirectory fixture path"),
        (42, "a non-string value"),
    ):
        try:
            validate_governed_path(bad_path)
        except GovernedPathError:
            pass
        else:
            raise SystemExit(
                f"Self-test failure: validate_governed_path accepted {label} ({bad_path!r}), "
                "which it must reject."
            )

    for good_path in (
        "contracts/manifest.json",
        "contracts/openapi.yaml",
        "contracts/asyncapi.yaml",
        "contracts/route-inventory.json",
        "contracts/schemas/mode.schema.json",
        "contracts/fixtures/get-game.json",
    ):
        if validate_governed_path(good_path) != good_path:
            raise SystemExit(
                f"Self-test failure: validate_governed_path must accept and return unchanged the "
                f"legitimately governed path {good_path!r}."
            )

    unique_name = f"__self-test-sentinel-{uuid.uuid4().hex}__.json"
    real_relative_path = f"contracts/fixtures/{unique_name}"

    # -- Worktree (head) side: real filesystem I/O ---------------------------
    real_file = root / real_relative_path
    real_file.write_bytes(b'{"a": 1}')
    try:
        content = read_governed_worktree_bytes(root, real_relative_path)
        if content != b'{"a": 1}':
            raise SystemExit(
                "Self-test failure: read_governed_worktree_bytes must return a well-formed "
                "regular governed file's exact bytes unchanged."
            )

        # A symlink pointing at that *same* file -- so its target bytes are
        # byte-for-byte identical to a legitimate governed file's content --
        # must still be rejected purely because of its file type (symlink),
        # not because its content differs from anything.
        symlink_relative_path = f"contracts/fixtures/__self-test-symlink-{uuid.uuid4().hex}__.json"
        symlink_path = root / symlink_relative_path
        symlink_path.symlink_to(real_file.name)
        try:
            try:
                read_governed_worktree_bytes(root, symlink_relative_path)
            except GovernedPathError:
                pass
            else:
                raise SystemExit(
                    "Self-test failure: read_governed_worktree_bytes must reject a symlink even "
                    "when its target's bytes are identical to a legitimate governed file's "
                    "content -- rejection must be based on file type (lstat), not content."
                )
        finally:
            symlink_path.unlink(missing_ok=True)
    finally:
        real_file.unlink(missing_ok=True)

    try:
        read_governed_worktree_bytes(
            root, f"contracts/fixtures/__self-test-missing-{uuid.uuid4().hex}__.json"
        )
    except GovernedPathError:
        pass
    else:
        raise SystemExit(
            "Self-test failure: read_governed_worktree_bytes must reject a governed path "
            "missing from disk."
        )

    # -- Historical/base side: real (throwaway, unreferenced) git objects ----
    def _git(args: list[str], input_bytes: bytes | None = None) -> str:
        result = subprocess.run(args, cwd=root, capture_output=True, input=input_bytes)
        if result.returncode != 0:
            raise SystemExit(
                f"Self-test setup failure: {args!r} exited {result.returncode}: "
                f"{result.stderr.decode('utf-8', errors='replace')}"
            )
        return result.stdout.decode("utf-8").strip()

    def _commit_with_single_entry(
        mode: str, object_type: str, object_id: str, relative_entry_path: str
    ) -> str:
        """Build a throwaway commit whose tree contains exactly one entry at
        `relative_entry_path` (which may contain '/' components), by
        wrapping `git mktree` bottom-up one directory level at a time --
        `git mktree` itself only ever builds a single flat tree level from
        its input lines, so a nested path like `contracts/fixtures/x.json`
        requires an innermost tree for the leaf blob, wrapped in a tree for
        `fixtures`, wrapped in a tree for `contracts`, exactly mirroring how
        git itself resolves a multi-component path via `git ls-tree`/
        `git show ref:path` down through nested tree objects.
        """
        path_segments = relative_entry_path.split("/")
        tree_sha = _git(
            ["git", "mktree"], f"{mode} {object_type} {object_id}\t{path_segments[-1]}\n".encode()
        )
        for directory_segment in reversed(path_segments[:-1]):
            tree_sha = _git(
                ["git", "mktree"], f"040000 tree {tree_sha}\t{directory_segment}\n".encode()
            )
        return _git(
            [
                "git",
                "commit-tree",
                tree_sha,
                "-m",
                "contract-tooling self-test throwaway commit (never attached to any ref/branch)",
            ]
        )

    well_formed_blob = _git(["git", "hash-object", "-w", "--stdin"], b'{"a": 1}')
    well_formed_commit = _commit_with_single_entry(
        "100644", "blob", well_formed_blob, real_relative_path
    )
    well_formed_content = read_governed_git_ref_bytes(root, well_formed_commit, real_relative_path)
    if well_formed_content != b'{"a": 1}':
        raise SystemExit(
            "Self-test failure: read_governed_git_ref_bytes must return a well-formed regular "
            "(mode 100644) blob's exact bytes unchanged."
        )

    symlink_blob = _git(["git", "hash-object", "-w", "--stdin"], unique_name.encode())
    symlink_commit = _commit_with_single_entry("120000", "blob", symlink_blob, real_relative_path)
    executable_commit = _commit_with_single_entry(
        "100755", "blob", well_formed_blob, real_relative_path
    )
    head_commit_sha = _git(["git", "rev-parse", "HEAD"])
    submodule_commit = _commit_with_single_entry(
        "160000", "commit", head_commit_sha, real_relative_path
    )

    for bad_commit, label in (
        (symlink_commit, "a symlink (mode 120000)"),
        (executable_commit, "an executable file (mode 100755)"),
        (submodule_commit, "a submodule/gitlink (mode 160000)"),
    ):
        try:
            read_governed_git_ref_bytes(root, bad_commit, real_relative_path)
        except GovernedPathError:
            pass
        else:
            raise SystemExit(
                f"Self-test failure: read_governed_git_ref_bytes must reject {label} rather than "
                "silently reading and hashing it as if it were an ordinary governed blob."
            )

    missing_content = read_governed_git_ref_bytes(
        root, well_formed_commit, f"contracts/fixtures/__self-test-missing-{uuid.uuid4().hex}__.json"
    )
    if missing_content is not None:
        raise SystemExit(
            "Self-test failure: read_governed_git_ref_bytes must return None for a path that "
            "does not exist at the given ref."
        )

    # -- Source-qualified diagnostics -----------------------------------------
    try:
        read_governed_worktree_bytes(root, "backend/arkham-api/package.yaml")
    except GovernedPathError as error:
        if "backend/arkham-api/package.yaml" not in str(error):
            raise SystemExit(
                "Self-test failure: GovernedPathError messages must quote the offending path."
            )
    else:
        raise SystemExit(
            "Self-test failure: read_governed_worktree_bytes must reject a path outside the "
            "governed subtree."
        )
