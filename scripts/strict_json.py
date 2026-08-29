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
overflows to infinity for any exponent within its own default context
range) to determine, precisely, whether it fits in a finite `float`, and
rejecting it outright if not, rather than ever returning `inf`. JSON syntax
itself places no bound on how many exponent digits a literal may have, so
`Decimal(raw)` can itself raise `decimal.InvalidOperation` for an exponent
extreme enough to exceed even `Decimal`'s own context range -- caught and
re-raised as this module's own controlled `StrictJSONError` rather than
letting a raw `decimal.InvalidOperation` escape uncaught.
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

import decimal
import json
import os
import stat
import subprocess
import sys
import uuid
from decimal import Decimal
from pathlib import Path

# The largest finite magnitude an IEEE-754 double (Python's `float`) can
# represent. Used only to *detect* exponent overflow exactly (via `Decimal`,
# which never silently overflows), not as a parsing float itself.
_FLOAT_MAX_MAGNITUDE = Decimal(sys.float_info.max)

# `json`'s stdlib default `parse_int` is the bare `int` constructor, which
# since Python 3.11 enforces a global digit-count safety limit
# (`sys.get_int_max_str_digits()`, 4300 by default) on str->int conversion
# and raises a raw `ValueError` for a syntactically valid JSON integer
# literal with more digits than that (e.g. a 5,000-digit integer) --
# `_parse_int_or_reject_unsupported` below catches this and re-raises it as
# this module's own controlled, source-qualified `StrictJSONError` instead.

# `git commit-tree` refuses to run without a configured author/committer
# identity, which a fresh CI runner never has (unlike most local developer
# checkouts). Every self-test that creates a throwaway, never-referenced git
# commit purely as internal plumbing (never actually attributed to a real
# author) sets these explicitly via the subprocess environment, rather than
# depending on the ambient git config of whatever machine happens to run it.
THROWAWAY_GIT_COMMIT_ENV_OVERRIDES = {
    "GIT_AUTHOR_NAME": "contract-tooling self-test",
    "GIT_AUTHOR_EMAIL": "contract-tooling-self-test@example.invalid",
    "GIT_COMMITTER_NAME": "contract-tooling self-test",
    "GIT_COMMITTER_EMAIL": "contract-tooling-self-test@example.invalid",
}


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
    exact and never silently overflows to infinity for any JSON number
    literal whose exponent fits `Decimal`'s own (very large, but still
    finite) default context range, so whether the value fits in a `float`
    can be determined precisely -- rather than trusting `float()`'s own
    silent rounding/overflow behavior to decide, which is exactly the bug
    this function exists to close. JSON syntax itself places no bound on
    how many exponent digits a number literal may have, so `Decimal(raw)`
    itself can raise `decimal.InvalidOperation` for an exponent large
    enough to exceed even `Decimal`'s default context's `Emax`/`Emin` (in
    the millions of digits) -- caught here and re-raised as this module's
    own controlled `StrictJSONError` rather than letting a raw
    `decimal.InvalidOperation` escape uncaught; such a value is, if
    anything, even more clearly non-representable than an ordinary
    `1e9999`-style float overflow.

    A value that fits is returned as an ordinary `float` (matching the
    stdlib's default `parse_float` return type exactly, so every downstream
    consumer keeps working unchanged): this toolchain's JSON Schema
    validation (the `jsonschema` library) checks
    `isinstance(x, (int, float))` for the `"number"` type and does not
    recognize `Decimal`, and `json.dumps` (used by
    `canonicalize_manifest_bytes` and every fixture-writing tool) does not
    serialize `Decimal` at all by default -- so keeping the arbitrary-precision
    `Decimal` value itself flowing downstream is not viable in this
    toolchain, and this function fails closed (rejects) instead of ever
    returning a non-finite `float`, rather than silently returning `inf`.

    Three further subtleties, all found and closed by review, are handled
    below rather than being left as latent bugs:

    1. Comparing an *unbounded*-magnitude `Decimal` (`abs(decimal_value)`,
       or the bare `>` operator on it) against `_FLOAT_MAX_MAGNITUDE` is
       itself a context-*rounded* arithmetic operation in the `decimal`
       module -- for a value whose exponent is extreme enough (e.g.
       `1e1000000`), the default context's own `Emax` makes that comparison
       raise `decimal.Overflow` internally, which would otherwise escape
       this function as a raw, uncontrolled exception instead of this
       module's own `StrictJSONError`. `Decimal.copy_abs()` (unlike the
       bare `abs()` builtin) is documented to perform no rounding and use
       no context at all, so magnitude is computed exactly first and only
       *then* compared -- and a plain `Decimal.__gt__` comparison (as
       opposed to e.g. subtraction) does not perform context-rounded
       arithmetic and cannot itself raise `Overflow`.
    2. A *nonzero* JSON number literal whose magnitude is smaller than the
       smallest positive value a `float` can represent at all (e.g.
       `1e-1000000`, or even an ordinary `1e-400`) silently converts via the
       bare `float()` constructor to positive/negative *zero* -- changing a
       nonzero value into zero is exactly the kind of silent, undetected
       precision loss this module exists to prevent (a schema `"minimum"`
       far above zero would then wrongly appear satisfied, for instance).
       This is detected generically, for any magnitude, by comparing the
       exact `Decimal` value against the converted `float` result rather
       than hard-coding any particular boundary.
    3. Any other `decimal.DecimalException` (the common base class for
       every exception the `decimal` module can raise, including
       `InvalidOperation` and `Overflow`) is caught as a final defensive
       fallback and re-raised as this module's own controlled
       `StrictJSONError`, so no raw `decimal` traceback can ever escape
       this function even for a case not explicitly enumerated above.
    """
    try:
        decimal_value = Decimal(raw)
        magnitude = decimal_value.copy_abs()
        if magnitude > _FLOAT_MAX_MAGNITUDE:
            raise StrictJSONError(
                f"{source}: JSON number {raw!r} overflows the finite range a JSON Schema "
                "\"number\"/\"float\" can represent in this toolchain (exact magnitude computed "
                "via Decimal.copy_abs(), which -- unlike the bare abs() builtin -- performs no "
                "context-rounded arithmetic and so cannot itself raise decimal.Overflow, to avoid "
                "trusting float()'s own silent overflow-to-inf behavior); non-finite numeric "
                "values are not permitted in governed contract artifacts."
            )
        result = float(raw)
        if decimal_value != 0 and result == 0.0:
            raise StrictJSONError(
                f"{source}: JSON number {raw!r} is nonzero but underflows to signed zero when "
                "converted to a finite-range float (its magnitude is smaller than the smallest "
                "positive value a float can represent); silently treating a nonzero value as "
                "zero could change whether a JSON Schema \"minimum\"/\"exclusiveMinimum\" "
                "constraint appears satisfied, so this toolchain rejects the conversion outright "
                "instead of returning 0.0."
            )
        return result
    except StrictJSONError:
        raise
    except decimal.InvalidOperation as exc:
        raise StrictJSONError(
            f"{source}: JSON number {raw!r} could not be parsed exactly as decimal.Decimal "
            f"({exc}); its exponent is too extreme even for Decimal's own default context "
            "range, so it is rejected outright rather than falling back to float()'s silent "
            "overflow-to-inf behavior."
        ) from exc
    except decimal.DecimalException as exc:
        raise StrictJSONError(
            f"{source}: JSON number {raw!r} could not be parsed exactly as decimal.Decimal "
            f"due to an unexpected {type(exc).__name__} ({exc}); rejected outright rather than "
            "letting a raw decimal exception escape or falling back to float()'s own silent "
            "rounding/overflow behavior."
        ) from exc


def _parse_int_or_reject_unsupported(raw: str, *, source: str) -> int:
    """`json.loads`'s default `parse_int` is the bare `int` constructor,
    which (since Python 3.11, see PEP 664 / bpo-95778) enforces a global
    digit-count safety limit (`sys.get_int_max_str_digits()`, 4300 digits by
    default) on any str->int conversion, to guard against a denial-of-service
    attack via a maliciously huge numeral -- but JSON integer syntax itself
    places no bound on digit count, so a syntactically valid (if unusual)
    JSON integer literal past that limit (e.g. a 5,000-digit integer) makes
    the bare `int()` constructor raise a raw `ValueError` that would
    otherwise escape uncontrolled from deep inside `json.loads`'s
    `parse_int` callback. This wraps that conversion and re-raises any such
    failure as this module's own controlled, source-qualified
    `StrictJSONError` instead, consistent with every other numeric-parsing
    guard in this module (this toolchain does not need to support integers
    at that scale for any governed contract artifact, so failing closed
    with a clear diagnostic -- rather than raising the process-wide safety
    limit via `sys.set_int_max_str_digits()`, which would weaken a
    deliberate CPython security guard for every caller of this process, not
    just this module -- is the correct behavior here).
    """
    try:
        return int(raw)
    except ValueError as exc:
        raise StrictJSONError(
            f"{source}: JSON integer literal {raw!r} could not be parsed as a Python int "
            f"({exc}); this toolchain does not support integer literals this large in governed "
            "contract artifacts."
        ) from exc


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
            parse_int=lambda raw: _parse_int_or_reject_unsupported(raw, source=source),
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


def _git_tracked_mode(root: Path, args: list[str], relative_path: str, *, label: str) -> str | None:
    """Run a `git ls-files --stage`/`git ls-tree`-style plumbing command
    (`args`, with the ref/flags already filled in by the caller) restricted
    to exactly `relative_path`, and return the 6-digit octal mode of the
    single matching entry (e.g. `"100644"`), or `None` if `relative_path` is
    not tracked at all in that context (a brand-new file never `git add`-ed,
    or a path that does not exist at the given ref/commit). `label` (e.g.
    `"git index"`, `"git HEAD tree"`) makes any diagnostic actionable about
    which git context the mode check failed in.
    """
    result = subprocess.run(
        [*args, "--", relative_path],
        cwd=root,
        capture_output=True,
    )
    if result.returncode != 0:
        raise GovernedPathError(
            f"{label} lookup failed for governed artifact path {relative_path!r}: "
            f"{result.stderr.decode('utf-8', errors='replace')}"
        )
    stdout = result.stdout
    if not stdout.strip():
        return None
    lines = [line for line in stdout.split(b"\n") if line.strip()]
    if len(lines) != 1:
        raise GovernedPathError(
            f"{label} lookup for governed artifact path {relative_path!r} unexpectedly matched "
            f"{len(lines)} entries; expected exactly 0 (untracked) or 1."
        )
    # Both `git ls-files --stage` ("<mode> <sha> <stage>\t<path>") and
    # `git ls-tree` ("<mode> <type> <sha>\t<path>") put the mode as the
    # first space-separated field before the first tab.
    header = lines[0].split(b"\t", 1)[0].decode("utf-8", errors="replace")
    return header.split(" ")[0]


def _require_governed_worktree_mode(
    root: Path, relative_path: str, *, require_exists: bool = False
) -> Path:
    """Shared preflight for every reader/writer that touches a governed
    artifact's *current worktree* copy: validate `relative_path`
    (`validate_governed_path`) and confirm the path, if it currently exists
    on disk, is an ordinary, non-executable regular file at git mode
    `100644` (via `os.lstat` -- which, unlike `Path.is_file()`, does not
    follow a symlink to whatever file it currently happens to point at --
    plus the git index/HEAD tree mode checks below). Returns the resolved
    absolute `Path` on success.

    Factored out of `read_governed_worktree_bytes` so
    `write_governed_worktree_bytes` can apply the *exact same* three-signal
    mode check (on-disk permission bits, git index mode, git HEAD tree mode)
    immediately before writing, not just when reading -- a writer that
    only validated on read, then wrote through `Path.write_text`/`open(...,
    "w")` unconditionally, would still follow a symlink at write time (since
    those APIs open-and-truncate whatever the final path component
    currently resolves to), silently corrupting whatever external file the
    symlink happened to point at.

    If `relative_path` does not exist on disk yet and `require_exists` is
    false (the default, appropriate for a writer that may be creating a
    brand-new governed artifact), no mode check applies (there is nothing
    to check) and this simply returns the resolved path the caller may go
    on to create. If `require_exists` is true (used by
    `read_governed_worktree_bytes`, which can never sensibly read bytes
    from a path that is not there), a missing path raises
    `GovernedPathError` immediately instead of silently returning as if
    there were nothing to validate.
    """
    validated = validate_governed_path(relative_path)
    absolute_path = root / validated
    try:
        file_stat = absolute_path.lstat()
    except FileNotFoundError as exc:
        if require_exists:
            raise GovernedPathError(
                f"governed artifact path {validated!r} does not exist on disk (resolved: "
                f"{absolute_path})."
            ) from exc
        return absolute_path
    if not stat.S_ISREG(file_stat.st_mode):
        raise GovernedPathError(
            f"governed artifact path {validated!r} is not a regular file on disk (resolved: "
            f"{absolute_path}, st_mode={oct(file_stat.st_mode)}); a symlink, directory, or other "
            "special file is never permitted as a governed artifact."
        )
    if stat.S_IMODE(file_stat.st_mode) & 0o111:
        raise GovernedPathError(
            f"governed artifact path {validated!r} is executable on disk (permission bits "
            f"{oct(stat.S_IMODE(file_stat.st_mode))}, resolved: {absolute_path}); governed "
            "artifacts must be non-executable regular files (git mode 100644), matching the "
            "exact mode read_governed_git_ref_bytes requires from history."
        )
    index_mode = _git_tracked_mode(
        root, ["git", "ls-files", "--stage"], validated, label="git index"
    )
    if index_mode is not None and index_mode != "100644":
        raise GovernedPathError(
            f"governed artifact path {validated!r} has git index (staged) mode {index_mode!r}, "
            "expected 100644 (non-executable regular blob); a staged 'git update-index "
            "--chmod=+x' (or equivalent) mode change is rejected even when the on-disk "
            "permission bits were left untouched."
        )
    head_mode = _git_tracked_mode(
        root, ["git", "ls-tree", "HEAD"], validated, label="git HEAD tree"
    )
    if head_mode is not None and head_mode != "100644":
        raise GovernedPathError(
            f"governed artifact path {validated!r} has git HEAD-committed mode {head_mode!r}, "
            "expected 100644 (non-executable regular blob)."
        )
    if index_mode is not None and head_mode is not None and index_mode != head_mode:
        raise GovernedPathError(
            f"governed artifact path {validated!r} has disagreeing git modes: index (staged) "
            f"mode {index_mode!r} vs. HEAD-committed mode {head_mode!r}; a governed artifact's "
            "staged and last-committed mode must agree."
        )
    return absolute_path


def write_governed_worktree_bytes(root: Path, relative_path: str, content: bytes) -> None:
    """Write `content` as the current worktree copy of governed artifact
    `relative_path`, without ever writing *through* a symlink at that path
    (unlike `Path.write_text`/`open(path, "w")`, which follow a symlink's
    final path component and overwrite whatever it currently points at).

    Sequence:

    1. `_require_governed_worktree_mode` re-validates the path and its
       current on-disk/index/HEAD mode *immediately* before writing (not
       merely once, earlier, at read time) -- this narrows, though does not
       claim to eliminate, the TOCTOU window between an earlier read-side
       validation and this write for a single local maintainer tool.
    2. The new content is written to a throwaway temporary file created in
       the *same parent directory* (so the final `os.replace` is on the
       same filesystem and therefore atomic), with its permission bits
       forced to exactly `0o644` regardless of the process umask.
    3. `os.replace(tmp_path, absolute_path)` atomically replaces the
       directory entry at `absolute_path`. Critically, `os.replace`
       (`rename(2)`) never dereferences a symlink at the *destination* --
       if `absolute_path` were a symlink, this unlinks the symlink itself
       and puts the new regular file in its place, rather than following it
       to overwrite whatever external file it pointed at. Combined with
       step 1 rejecting a symlink outright before ever reaching this point,
       an external symlink target is provably never written to by this
       function, whether or not step 1's check is somehow bypassed.
    """
    absolute_path = _require_governed_worktree_mode(root, relative_path)
    tmp_path = absolute_path.parent / f".{absolute_path.name}.tmp-{uuid.uuid4().hex}"
    try:
        with open(tmp_path, "wb") as handle:
            handle.write(content)
            handle.flush()
            os.fsync(handle.fileno())
        os.chmod(tmp_path, 0o644)
        os.replace(tmp_path, absolute_path)
    finally:
        # If os.replace succeeded, tmp_path no longer exists at this name
        # (it was atomically renamed onto absolute_path) -- this only fires
        # on an exception before/during the replace, cleaning up the
        # throwaway file rather than leaving it behind.
        if tmp_path.exists():
            tmp_path.unlink()


def read_governed_worktree_bytes(root: Path, relative_path: str) -> bytes:
    """Validate `relative_path` (`validate_governed_path`), read its bytes
    from the current worktree under `root` (rejecting anything that is not
    an ordinary, non-executable regular file at git mode `100644` via
    `os.lstat` -- unlike `Path.is_file()`/`Path.read_bytes()`, `lstat` does
    not follow a symlink to whatever file it currently happens to point
    at), and, if the path looks like JSON
    (`strict_validate_governed_bytes_if_json`), strictly parse those bytes
    before returning them -- so a caller that goes on to hash the returned
    bytes never hashes content this toolchain has not already confirmed is
    both a genuine, non-executable regular file and (for `.json` paths)
    well-formed strict JSON.

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

    The *executable*-bit and git-mode checks close a distinct, narrower gap
    review found: `stat.S_ISREG` alone does not distinguish a mode-`100644`
    regular file from a mode-`100755` (executable) one -- both are
    "regular files" as far as `S_ISREG` is concerned, and (for identical
    content) hash identically here. But `read_governed_git_ref_bytes`
    (used for the immutable base ref) *does* strictly require exact git
    mode `100644` via `git ls-tree`. Left unchecked here, a `chmod +x` (or
    a purely-staged `git update-index --chmod=+x`, with the on-disk bytes
    and permissions untouched) on a governed file would pass this worktree
    reader silently (same content, same hash, no revision-drift signal
    today) -- and then, once that state is ever committed and later reused
    as a *base* ref for some future revision-drift check, permanently and
    unfixably break every future base-artifact read for that historical
    revision (since the immutable base blob would forever carry the
    now-wrong mode). Rejecting the mode change the moment it is introduced
    -- here, at the worktree/head side -- means the hashing tools fail
    loudly immediately, rather than silently accepting it now and only
    discovering the breakage much later when it is no longer fixable.
    Three independent signals are checked, since any one of them alone can
    miss a real-world case the others catch:

    1. The actual on-disk POSIX permission bits (`file_stat.st_mode`'s
       executable bits) -- catches an ordinary `chmod +x`, staged or not.
    2. The git *index* (staged) mode, via `git ls-files --stage` -- catches
       a purely-staged `git update-index --chmod=+x` where the on-disk
       permission bits were never actually touched (signal 1 alone would
       miss this, since the file's real POSIX bits still say non-executable).
    3. The git *HEAD* (last-committed) tree mode, via `git ls-tree HEAD` --
       catches a committed mode change that has not yet been touched again
       in the index/worktree (signals 1-2 alone could miss this if HEAD and
       the index/worktree were checked out inconsistently).

    A path that is not yet tracked at all in a given context (a brand-new
    file never `git add`-ed, for the index; a file added on this branch but
    not yet committed, for HEAD) has no recorded mode to compare there,
    so only the signals that actually apply are checked -- but signal 1 (the
    real on-disk permission bits) always applies and is never skipped, so
    this function never silently passes an executable file through purely
    because it happens to be new/untracked in some other context.
    """
    validated = validate_governed_path(relative_path)
    absolute_path = _require_governed_worktree_mode(root, relative_path, require_exists=True)
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

    # An exponent so extreme it exceeds even decimal.Decimal's own default
    # context range must be rejected via the controlled StrictJSONError,
    # not let a raw decimal.InvalidOperation escape uncaught.
    _expect_rejected(
        "[1e99999999999999999999999999999999999999]",
        "a number literal whose exponent exceeds Decimal's own context range",
    )

    # An exponent extreme enough that even Decimal.copy_abs() magnitude
    # comparison against the float-max bound would raise decimal.Overflow if
    # a context-rounded operation (e.g. the bare abs() builtin, unlike
    # copy_abs()) were used instead -- must still be rejected via the
    # controlled StrictJSONError, both for a positive and a negative sign.
    _expect_rejected(
        "[1e1000000]",
        "a positive-sign, context-overflow-triggering exponent magnitude",
    )
    _expect_rejected(
        "[-1e1000000]",
        "a negative-sign, context-overflow-triggering exponent magnitude",
    )

    # A nonzero number literal whose magnitude is smaller than the smallest
    # positive float must be rejected rather than silently converted to
    # signed zero (which would make a positive JSON Schema "minimum" wrongly
    # appear satisfied) -- for both an ordinary small-exponent underflow and
    # an extreme-exponent underflow, and both signs.
    _expect_rejected("[1e-400]", "an ordinary-exponent positive underflow-to-zero value")
    _expect_rejected("[-1e-400]", "an ordinary-exponent negative underflow-to-zero value")
    _expect_rejected("[1e-1000000]", "an extreme-exponent positive underflow-to-zero value")
    _expect_rejected("[-1e-1000000]", "an extreme-exponent negative underflow-to-zero value")

    # decimal.Decimal's actual Emax/Emin boundary and subnormal-adjacent
    # values must still parse as ordinary finite floats when they are not
    # actually zero/overflow after conversion -- proving the guards above
    # reject only genuine overflow/underflow, not merely "large" exponents.
    ok_boundary = strict_json_loads(
        "[1e300, -1e300, 1e-300, -1e-300, 5e-324, -5e-324]",
        source="<selftest: decimal boundary-adjacent ordinary values>",
    )
    expected_boundary = [1e300, -1e300, 1e-300, -1e-300, 5e-324, -5e-324]
    if ok_boundary != expected_boundary:
        raise SystemExit(
            "Self-test failure: strict_json_loads must still accept ordinary finite "
            f"boundary-adjacent float values, got {ok_boundary!r}."
        )

    # A syntactically valid JSON integer literal with far more digits than
    # Python 3.11+'s default int-string-conversion safety limit (4300
    # digits) must be rejected via the controlled StrictJSONError, not let a
    # raw ValueError escape uncaught from the bare int() constructor.
    _expect_rejected("[" + "9" * 5000 + "]", "a 5,000-digit integer literal")

    # An ordinary integer, including one comfortably larger than any real
    # governed ID but still far below the digit-count safety limit, must
    # still parse exactly like the stdlib default (as a plain int).
    ok_int = strict_json_loads(
        "[0, -0, 1, -1, 123456789012345678901234567890]",
        source="<selftest: ordinary integers>",
    )
    expected_int = [0, 0, 1, -1, 123456789012345678901234567890]
    if ok_int != expected_int or any(not isinstance(value, int) for value in ok_int):
        raise SystemExit(
            "Self-test failure: strict_json_loads must parse ordinary integers exactly "
            f"like the stdlib default, got {ok_int!r}."
        )

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
        result = subprocess.run(
            args,
            cwd=root,
            capture_output=True,
            input=input_bytes,
            env={**os.environ, **THROWAWAY_GIT_COMMIT_ENV_OVERRIDES},
        )
        if result.returncode != 0:
            raise SystemExit(
                f"Self-test setup failure: {args!r} exited {result.returncode}: "
                f"{result.stderr.decode('utf-8', errors='replace')}"
            )
        return result.stdout.decode("utf-8").strip()

    # -- Worktree (head) side: executable-bit and git-mode consistency ------
    executable_relative_path = f"contracts/fixtures/__self-test-executable-{uuid.uuid4().hex}__.json"
    executable_file = root / executable_relative_path
    executable_file.write_bytes(b'{"a": 1}')
    executable_file.chmod(0o755)
    try:
        try:
            read_governed_worktree_bytes(root, executable_relative_path)
        except GovernedPathError:
            pass
        else:
            raise SystemExit(
                "Self-test failure: read_governed_worktree_bytes must reject a governed file "
                "that is executable on disk (mode 100755), even with otherwise well-formed "
                "content, since read_governed_git_ref_bytes requires exact mode 100644 from "
                "history and a mode-only 100644->100755 change would otherwise pass here "
                "silently (identical content hash) and only break much later, unfixably, once "
                "committed and reused as an immutable base ref."
            )
    finally:
        executable_file.unlink(missing_ok=True)

    # An ordinary non-executable (mode 100644) governed file, staged in an
    # isolated *throwaway* git index (never the real repository index --
    # using `GIT_INDEX_FILE` to point at a private scratch index file, so
    # this can never race with, or get raced by, any other concurrent
    # process's `git add`/`git reset`/`index.lock` on the real shared
    # index -- unlike an earlier version of this self-test, which mutated
    # the real index directly and could fail with a stale/contended
    # `index.lock` when multiple contract-tooling scripts run concurrently,
    # e.g. via `mise run contracts:validate`), must still be accepted --
    # proving the new executable/git-mode checks reject only genuine mode
    # disagreements, not every tracked file.
    def _with_throwaway_index(body):
        """Run `body(git_in_throwaway_index, ambient_env_restore)` with
        `GIT_INDEX_FILE` pointed at a brand-new, empty, per-call scratch
        index file, and `os.environ["GIT_INDEX_FILE"]` (which
        `_git_tracked_mode`'s ambient-environment `subprocess.run` call
        inherits, since it passes no explicit `env=`) set to match for the
        duration of the call -- so `read_governed_worktree_bytes`'s
        `git ls-files --stage` lookup transparently sees the throwaway
        index's contents instead of the real repository index's, without
        needing to change that function's signature. Always restores (or
        removes) the ambient `GIT_INDEX_FILE` environment variable and
        deletes the scratch index file afterward, regardless of outcome.

        The scratch index file is placed in the real, resolved git
        directory (`git rev-parse --absolute-git-dir`, e.g.
        `.git/worktrees/<name>` for a linked worktree) rather than assuming
        `root / ".git"` is itself a directory -- in a linked worktree (such
        as the one this contract-tooling branch was developed in), `.git`
        at the worktree root is an ordinary *file* containing a `gitdir:`
        pointer, not a directory, so writing directly under `root / ".git"`
        would fail with `NotADirectoryError`.
        """
        git_dir = Path(
            subprocess.run(
                ["git", "-C", str(root), "rev-parse", "--absolute-git-dir"],
                capture_output=True,
                check=True,
            ).stdout.decode("utf-8").strip()
        )
        throwaway_index_path = git_dir / f"selftest-index-{uuid.uuid4().hex}"
        throwaway_env = {
            **os.environ,
            **THROWAWAY_GIT_COMMIT_ENV_OVERRIDES,
            "GIT_INDEX_FILE": str(throwaway_index_path),
        }

        def _git_in_throwaway_index(args: list[str]) -> str:
            result = subprocess.run(args, cwd=root, capture_output=True, env=throwaway_env)
            if result.returncode != 0:
                raise SystemExit(
                    f"Self-test setup failure: {args!r} exited {result.returncode}: "
                    f"{result.stderr.decode('utf-8', errors='replace')}"
                )
            return result.stdout.decode("utf-8").strip()

        previous_ambient_value = os.environ.get("GIT_INDEX_FILE")
        os.environ["GIT_INDEX_FILE"] = str(throwaway_index_path)
        try:
            body(_git_in_throwaway_index)
        finally:
            if previous_ambient_value is None:
                os.environ.pop("GIT_INDEX_FILE", None)
            else:
                os.environ["GIT_INDEX_FILE"] = previous_ambient_value
            throwaway_index_path.unlink(missing_ok=True)

    ordinary_relative_path = (
        f"contracts/fixtures/__self-test-ordinary-mode-{uuid.uuid4().hex}__.json"
    )
    ordinary_file = root / ordinary_relative_path
    ordinary_file.write_bytes(b'{"a": 1}')
    ordinary_file.chmod(0o644)
    try:
        ordinary_blob = _git(["git", "hash-object", "-w", "--stdin"], b'{"a": 1}')

        def _stage_ordinary(git_in_throwaway_index):
            git_in_throwaway_index(
                ["git", "update-index", "--add", "--cacheinfo",
                 f"100644,{ordinary_blob},{ordinary_relative_path}"]
            )
            content = read_governed_worktree_bytes(root, ordinary_relative_path)
            if content != b'{"a": 1}':
                raise SystemExit(
                    "Self-test failure: read_governed_worktree_bytes must return a staged, "
                    "ordinary (mode 100644) governed file's exact bytes unchanged."
                )

        _with_throwaway_index(_stage_ordinary)
    finally:
        ordinary_file.unlink(missing_ok=True)

    # A file staged (in the same isolated throwaway index) with only its
    # *git index* mode flipped to executable via `git update-index
    # --chmod=+x` -- while its actual on-disk permission bits are left
    # completely untouched at 100644 -- must still be rejected: this is the
    # "staged/worktree mode disagreement" case the plain on-disk-
    # permission-bit check alone cannot see.
    staged_mode_relative_path = (
        f"contracts/fixtures/__self-test-staged-mode-{uuid.uuid4().hex}__.json"
    )
    staged_mode_file = root / staged_mode_relative_path
    staged_mode_file.write_bytes(b'{"a": 1}')
    staged_mode_file.chmod(0o644)
    try:
        staged_mode_blob = _git(["git", "hash-object", "-w", "--stdin"], b'{"a": 1}')

        def _stage_mode_disagreement(git_in_throwaway_index):
            git_in_throwaway_index(
                ["git", "update-index", "--add", "--cacheinfo",
                 f"100644,{staged_mode_blob},{staged_mode_relative_path}"]
            )
            git_in_throwaway_index(
                ["git", "update-index", "--chmod=+x", "--", staged_mode_relative_path]
            )
            on_disk_mode = stat.S_IMODE(staged_mode_file.lstat().st_mode)
            if on_disk_mode & 0o111:
                raise SystemExit(
                    "Self-test setup failure: 'git update-index --chmod=+x' unexpectedly "
                    "changed the on-disk permission bits too; this self-test's premise (a pure "
                    "index-only mode change) no longer holds."
                )
            try:
                read_governed_worktree_bytes(root, staged_mode_relative_path)
            except GovernedPathError:
                pass
            else:
                raise SystemExit(
                    "Self-test failure: read_governed_worktree_bytes must reject a governed "
                    "path whose git index (staged) mode is 100755 even when its actual on-disk "
                    "permission bits are still 100644 -- a purely-staged mode change must not "
                    "silently pass just because the on-disk executable-bit check alone did not "
                    "catch it."
                )

        _with_throwaway_index(_stage_mode_disagreement)
    finally:
        staged_mode_file.unlink(missing_ok=True)

    # -- write_governed_worktree_bytes: never writes through a symlink, and
    # rejects the exact same bad-mode states read_governed_worktree_bytes
    # rejects, *before* ever opening the destination for writing. These
    # prove the "current manifest" write authority a manifest-hashing tool
    # relies on when it publishes a recomputed contracts/manifest.json back
    # to disk -- not merely that reading a bad-mode path fails, but that
    # writing to one does too, and that a symlinked governed path's
    # external target is providably untouched by a rejected (or even
    # successful) write attempt.
    writer_relative_path = f"contracts/fixtures/__self-test-writer-{uuid.uuid4().hex}__.json"
    writer_file = root / writer_relative_path
    writer_file.write_bytes(b'{"a": 1}')
    writer_file.chmod(0o644)
    try:
        # Canonical case: an ordinary, non-executable, untracked (so no
        # index/HEAD mode to disagree with) governed file must accept a
        # write, replace its content exactly, and remain a plain regular
        # file at mode 644 afterward (not, say, inheriting the umask or the
        # temp file's permissions unpredictably).
        write_governed_worktree_bytes(root, writer_relative_path, b'{"a": 2}')
        if writer_file.read_bytes() != b'{"a": 2}':
            raise SystemExit(
                "Self-test failure: write_governed_worktree_bytes must replace a canonical "
                "(mode 100644) governed file's content with the exact bytes given."
            )
        post_write_stat = writer_file.lstat()
        if not stat.S_ISREG(post_write_stat.st_mode):
            raise SystemExit(
                "Self-test failure: write_governed_worktree_bytes must leave an ordinary "
                "regular file (never a symlink or other special file) at the destination path."
            )
        if stat.S_IMODE(post_write_stat.st_mode) != 0o644:
            raise SystemExit(
                "Self-test failure: write_governed_worktree_bytes must leave the destination "
                f"at exactly mode 100644 regardless of umask, got {oct(stat.S_IMODE(post_write_stat.st_mode))}."
            )
    finally:
        writer_file.unlink(missing_ok=True)

    # An executable-on-disk governed path must be rejected by the writer
    # (not just the reader) before any write is attempted -- the original
    # content must remain byte-for-byte untouched.
    writer_executable_relative_path = (
        f"contracts/fixtures/__self-test-writer-executable-{uuid.uuid4().hex}__.json"
    )
    writer_executable_file = root / writer_executable_relative_path
    writer_executable_file.write_bytes(b'{"a": 1}')
    writer_executable_file.chmod(0o755)
    try:
        try:
            write_governed_worktree_bytes(root, writer_executable_relative_path, b'{"a": 2}')
        except GovernedPathError:
            pass
        else:
            raise SystemExit(
                "Self-test failure: write_governed_worktree_bytes must reject writing to a "
                "governed path that is currently executable on disk (mode 100755)."
            )
        if writer_executable_file.read_bytes() != b'{"a": 1}':
            raise SystemExit(
                "Self-test failure: write_governed_worktree_bytes must leave an executable "
                "governed path's content completely untouched when the write is rejected."
            )
    finally:
        writer_executable_file.unlink(missing_ok=True)

    # A staged (index-only) mode-755 governed path -- on-disk bits still
    # 644 -- must also be rejected by the writer, exactly like the reader.
    writer_staged_relative_path = (
        f"contracts/fixtures/__self-test-writer-staged-mode-{uuid.uuid4().hex}__.json"
    )
    writer_staged_file = root / writer_staged_relative_path
    writer_staged_file.write_bytes(b'{"a": 1}')
    writer_staged_file.chmod(0o644)
    try:
        staged_blob = _git(["git", "hash-object", "-w", "--stdin"], b'{"a": 1}')

        def _writer_stage_mode_disagreement(git_in_throwaway_index):
            git_in_throwaway_index(
                ["git", "update-index", "--add", "--cacheinfo",
                 f"100644,{staged_blob},{writer_staged_relative_path}"]
            )
            git_in_throwaway_index(
                ["git", "update-index", "--chmod=+x", "--", writer_staged_relative_path]
            )
            try:
                write_governed_worktree_bytes(root, writer_staged_relative_path, b'{"a": 2}')
            except GovernedPathError:
                pass
            else:
                raise SystemExit(
                    "Self-test failure: write_governed_worktree_bytes must reject writing to a "
                    "governed path whose git index (staged) mode is 100755 even when its "
                    "on-disk permission bits are still 100644."
                )
            if writer_staged_file.read_bytes() != b'{"a": 1}':
                raise SystemExit(
                    "Self-test failure: write_governed_worktree_bytes must leave a staged/"
                    "worktree mode-disagreement governed path's content completely untouched "
                    "when the write is rejected."
                )

        _with_throwaway_index(_writer_stage_mode_disagreement)
    finally:
        writer_staged_file.unlink(missing_ok=True)

    # The critical symlink case: a governed path that is currently a
    # symlink pointing at some *external* file (outside the governed
    # subtree entirely) with genuinely valid, well-formed JSON content
    # identical in shape to a legitimate governed artifact. The writer must
    # both (a) reject the write outright (mode check fails before any I/O
    # against the destination), and (b) never have touched the external
    # target's bytes at all -- proving this is not merely "rejected, but
    # only after already corrupting the target".
    external_sentinel_path = root / f"__self-test-external-sentinel-{uuid.uuid4().hex}__.json"
    external_sentinel_path.write_bytes(b'{"external": true}')
    writer_symlink_relative_path = (
        f"contracts/fixtures/__self-test-writer-symlink-{uuid.uuid4().hex}__.json"
    )
    writer_symlink_path = root / writer_symlink_relative_path
    try:
        writer_symlink_path.symlink_to(external_sentinel_path)
        try:
            write_governed_worktree_bytes(root, writer_symlink_relative_path, b'{"a": 2}')
        except GovernedPathError:
            pass
        else:
            raise SystemExit(
                "Self-test failure: write_governed_worktree_bytes must reject writing to a "
                "governed path that is currently a symlink, rather than following it to "
                "overwrite whatever external file it points at."
            )
        if not writer_symlink_path.is_symlink():
            raise SystemExit(
                "Self-test failure: a rejected write_governed_worktree_bytes call must leave "
                "the symlink itself in place (unlinking/replacing it is only ever permitted "
                "for an already-validated mode-100644 regular file, never a symlink)."
            )
        if external_sentinel_path.read_bytes() != b'{"external": true}':
            raise SystemExit(
                "Self-test failure: write_governed_worktree_bytes must never modify the "
                "external file a governed-path symlink points at, whether the write is "
                "accepted or rejected."
            )
    finally:
        writer_symlink_path.unlink(missing_ok=True)
        external_sentinel_path.unlink(missing_ok=True)

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
