#!/usr/bin/env python3
"""Revision-drift gate for contracts/manifest.json.

Rule (see contracts/README.md "Release immutability"): once a revision of the
native-client contract has been merged to main, every governed artifact
(schema, OpenAPI/AsyncAPI document, fixture, or the manifest's own descriptor
content) at that revision is immutable. Any subsequent change to a governed
artifact's *content* must be accompanied by a strictly greater, numeric,
dot-separated `schemaRevision` in contracts/manifest.json -- a human updating
only the hash (or only the version, but not both) is not sufficient; this
script recomputes and cross-checks both sides.

This intentionally never makes a network call. Comparing against a "base"
commit uses only local git plumbing (`git show <ref>:path`), which reads from
the already-cloned repository's local object database. If no usable base ref
can be resolved locally (for example a shallow clone missing the relevant
history), the script fails loudly rather than silently skipping the gate --
it never falls back to fetching anything over the network.

Separately, `run_self_tests()` exercises the pure `evaluate_drift` comparison
logic against small, hard-coded, in-memory fixtures, so that part of the
gate's correctness can be proven deterministically in any environment,
independent of this repository's actual git history. `run_self_tests()` also
calls `compute_hashes_from_worktree()` (which does perform real filesystem
existence checks against this checkout) and
`run_resolve_base_ref_self_tests()` (which does invoke local `git` commands,
e.g. `git rev-parse HEAD`) to prove those failure modes too -- so, taken as a
whole, `run_self_tests()` is not entirely filesystem/git-free, though it
still never makes a network call or depends on a remote ref.
"""

import hashlib
import os
import re
import shutil
import subprocess
import sys
import uuid
import argparse
from pathlib import Path

import strict_json

ROOT = Path(__file__).resolve().parents[1]
MANIFEST_RELATIVE_PATH = "contracts/manifest.json"

# Set (to "1") in the subprocess environment by
# run_manifest_worktree_authority_self_tests() around every scratch-copy
# invocation of update-manifest-hashes.py/check-schema-revision-drift.py it
# launches, so that self-test never recursively re-runs itself inside a
# nested scratch copy of this same script (which is byte-identical to this
# one, including this very self-test) -- see that function's docstring.
_MANIFEST_AUTHORITY_SELFTEST_SKIP_ENV = "CONTRACT_TOOLING_SKIP_MANIFEST_AUTHORITY_SELFTEST"

# Last-resort deterministic local fallback: the actual main commit this
# contract slice (GitHub issue #44) was branched from. It is always present
# in this repository's local history (it is a direct ancestor of every
# commit on this branch), so falling back to it never requires a network
# call -- unlike a moving branch name such as `origin/main`, which may be
# absent from a shallow CI checkout.
FALLBACK_BASE_SHA = "6a1befbd7b01b4a0f763e41260ae4dd1a5d14c27"


def require(condition: bool, message: str) -> None:
    if not condition:
        raise SystemExit(message)


def load_head_manifest() -> object:
    """Read and strictly parse the *current worktree's* manifest, routed
    through the exact same governed-path reader
    (`read_governed_worktree_bytes`) every other governed artifact is read
    through for hashing -- unlike a bare `path.read_bytes()`, this rejects a
    manifest path that is currently a symlink, executable, or has a
    staged/HEAD git mode other than `100644` *before* this gate ever trusts
    its content or its `schemaRevision`/`artifactHashes` fields. Skipping
    this check here (while every other governed path already went through
    it) would let a head-side manifest mode change slip through unnoticed
    now, then permanently break `read_governed_git_ref_bytes`'s mode check
    the moment this state is committed and later used as an immutable base.
    """
    content = strict_json.read_governed_worktree_bytes(ROOT, MANIFEST_RELATIVE_PATH)
    return strict_json.strict_json_loads(content, source=f"<worktree>:{MANIFEST_RELATIVE_PATH}")



def require_manifest_schema_revision(manifest: object, label: str) -> str:
    """Read `schemaRevision` from a manifest, failing via a controlled
    SystemExit (not a raw KeyError/TypeError) if the manifest itself isn't an
    object, the key is missing, or its value isn't the strictly numeric
    dot-separated string (e.g. "0.1.13") this gate's monotonic-revision
    comparison requires. Format validation happens here (by delegating to
    `parse_revision`) rather than being deferred to the first caller that
    happens to parse the revision, so a malformed `schemaRevision` is
    rejected immediately and attributed to the specific manifest (`label`)
    that contained it.
    """
    require(isinstance(manifest, dict), f"{label} manifest.json is not a JSON object: {manifest!r}")
    require(
        "schemaRevision" in manifest,
        f"{label} manifest.json is missing required key 'schemaRevision'",
    )
    revision = manifest["schemaRevision"]
    require(
        isinstance(revision, str) and revision,
        f"{label} manifest.json 'schemaRevision' must be a non-empty string, got {revision!r}",
    )
    try:
        parse_revision(revision)
    except SystemExit as error:
        raise SystemExit(f"{label} manifest.json 'schemaRevision' is malformed: {error}") from None
    return revision


def require_manifest_artifact_hashes(manifest: dict, label: str) -> dict:
    """Read `artifactHashes` from a manifest, failing via a controlled
    SystemExit (not a raw TypeError from `recorded_hashes[path]` or
    `set(recorded_hashes)`) if present but not a JSON object.
    """
    recorded_hashes = manifest.get("artifactHashes", {})
    require(
        isinstance(recorded_hashes, dict),
        f"{label} manifest.json 'artifactHashes' must be a JSON object, got {recorded_hashes!r}",
    )
    return recorded_hashes


def governed_paths(manifest: dict) -> list[str]:
    documents = manifest.get("documents", [])
    require(
        isinstance(documents, list),
        f"manifest.json 'documents' must be a JSON array, got {documents!r}",
    )
    for index, document in enumerate(documents):
        require(
            isinstance(document, str),
            f"manifest.json documents entry #{index} must be a string path, got {document!r}",
        )
    paths = list(documents)
    fixtures = manifest.get("fixtures", [])
    require(
        isinstance(fixtures, list),
        f"manifest.json 'fixtures' must be a JSON array, got {fixtures!r}",
    )
    for index, fixture in enumerate(fixtures):
        require(
            isinstance(fixture, dict) and "path" in fixture,
            f"manifest.json fixtures entry #{index} is not an object with a 'path' key: {fixture!r}",
        )
        require(
            isinstance(fixture["path"], str),
            f"manifest.json fixtures entry #{index} 'path' must be a string, got {fixture['path']!r}",
        )
        if fixture["path"] not in paths:
            paths.append(fixture["path"])
    return paths


def compute_hashes_from_worktree(manifest: dict, *, root: Path = ROOT) -> dict[str, str]:
    hashes = {}
    for relative_path in governed_paths(manifest):
        content = strict_json.read_governed_worktree_bytes(root, relative_path)
        hashes[relative_path] = hashlib.sha256(content).hexdigest()
    hashes["contracts/manifest.json"] = hashlib.sha256(
        strict_json.canonicalize_manifest_bytes(manifest)
    ).hexdigest()
    return hashes


def resolve_ref(ref: str, *, root: Path = ROOT) -> bool:
    result = subprocess.run(
        strict_json.git_argv(["git", "rev-parse", "--verify", "--quiet", f"{ref}^{{commit}}"]),
        cwd=root,
        capture_output=True,
    )
    return result.returncode == 0


_HEX_SHA_RE = re.compile(r"^[0-9a-f]{40}$")
_ALL_ZERO_SHA_RE = re.compile(r"^0{7,40}$")


def resolve_base_ref(
    base_sha: str | None, *, allow_local_fallback: bool, root: Path = ROOT
) -> str:
    """Resolve the immutable base commit to diff governed contract artifacts
    against.

    The caller workflow must provide the positional `base_sha` explicitly,
    sourced per trigger type from an
    event-provided field that cannot be spoofed by the pushed branch itself:
      - `pull_request`: `github.event.pull_request.base.sha`
      - `push`:         `github.event.before`
      - `workflow_dispatch`: a required workflow input

    This deliberately never reads a base reference from environment variables:
    the sealed CI shell intentionally removes those variables. A missing,
    malformed, all-zero, unresolvable, non-ancestor, or self SHA fails
    closed. Local development can opt into a separately named fallback task,
    which is never used by CI.
    """
    if base_sha is not None:
        require(
            _HEX_SHA_RE.fullmatch(base_sha) and not _ALL_ZERO_SHA_RE.fullmatch(base_sha),
            f"base SHA must be exactly 40 lowercase non-zero hexadecimal characters, got {base_sha!r}.",
        )
        require(
            resolve_ref(base_sha, root=root),
            f"base SHA {base_sha!r} does not resolve to a commit in this checkout's local history.",
        )
        head_sha = subprocess.run(
            strict_json.git_argv(["git", "rev-parse", "HEAD"]),
            cwd=root,
            capture_output=True,
            text=True,
            check=True,
        ).stdout.strip()
        require(base_sha != head_sha, "base SHA must not name HEAD itself.")
        ancestor = subprocess.run(
            strict_json.git_argv(["git", "merge-base", "--is-ancestor", base_sha, "HEAD"]),
            cwd=root,
            capture_output=True,
        )
        require(ancestor.returncode == 0, f"base SHA {base_sha!r} is not an ancestor of HEAD.")
        return base_sha

    require(
        allow_local_fallback,
        "an explicit 40-character lowercase base SHA is required; CI must pass the event base as a positional argument.",
    )
    candidate_refs = ["fork/main", "origin/main", "main", FALLBACK_BASE_SHA]

    attempted = []
    for ref in candidate_refs:
        if ref in attempted:
            continue
        attempted.append(ref)
        if resolve_ref(ref, root=root):
            return ref

    raise SystemExit(
        "Could not resolve any local fallback base ref with local git history (tried: "
        f"{attempted}). This gate never falls back to a network call; ensure the "
        "checkout includes enough local history to resolve one of these refs, or pass "
        "an explicit 40-character lowercase ancestor base SHA positionally."
    )


def load_manifest_from_git_ref(ref: str) -> dict | None:
    content = strict_json.read_governed_git_ref_bytes(ROOT, ref, "contracts/manifest.json")
    if content is None:
        return None
    return strict_json.strict_json_loads(content, source=f"{ref}:contracts/manifest.json")


def compute_hashes_from_git_ref(ref: str, manifest: dict, *, root: Path = ROOT) -> dict[str, str]:
    hashes = {}
    for relative_path in governed_paths(manifest):
        content = strict_json.read_governed_git_ref_bytes(root, ref, relative_path)
        require(content is not None, f"Could not read {relative_path} at {ref} via local git history")
        hashes[relative_path] = hashlib.sha256(content).hexdigest()
    hashes["contracts/manifest.json"] = hashlib.sha256(
        strict_json.canonicalize_manifest_bytes(manifest)
    ).hexdigest()
    return hashes


def parse_revision(revision: str) -> tuple[int, ...]:
    parts = revision.split(".")
    require(
        len(parts) >= 1 and all(part.isdigit() for part in parts),
        f"schemaRevision must be strictly numeric dot-separated (e.g. '0.1.13'), got {revision!r}",
    )
    return tuple(int(part) for part in parts)


def diff_hash_maps(base_hashes: dict[str, str], head_hashes: dict[str, str]) -> dict[str, tuple]:
    """Return {path: (base_hash_or_None, head_hash_or_None)} for every path
    that was added, removed, or changed between base and head."""
    changes = {}
    for path in sorted(set(base_hashes) | set(head_hashes)):
        base_hash = base_hashes.get(path)
        head_hash = head_hashes.get(path)
        if base_hash != head_hash:
            changes[path] = (base_hash, head_hash)
    return changes


def evaluate_drift(
    base_revision: str,
    base_hashes: dict[str, str],
    head_revision: str,
    head_hashes: dict[str, str],
) -> tuple[bool, str]:
    """Pure comparison: no governed-artifact content change requires no
    revision bump; any governed-artifact content change requires
    head_revision to be strictly greater than base_revision. Returns
    (ok, message)."""
    changes = diff_hash_maps(base_hashes, head_hashes)
    if not changes:
        return True, "no governed artifact content changed; schemaRevision may stay the same"

    base_tuple = parse_revision(base_revision)
    head_tuple = parse_revision(head_revision)
    if head_tuple > base_tuple:
        return (
            True,
            f"{len(changes)} governed artifact(s) changed and schemaRevision increased "
            f"({base_revision} -> {head_revision}): {sorted(changes)}",
        )
    return (
        False,
        f"{len(changes)} governed artifact(s) changed relative to base revision "
        f"{base_revision} ({sorted(changes)}), but schemaRevision was not strictly "
        f"increased (base={base_revision!r}, head={head_revision!r}). Bump schemaRevision "
        "monotonically whenever a governed schema, fixture, OpenAPI/AsyncAPI document, or "
        "the manifest's own descriptor content changes.",
    )


def run_governed_json_strictness_self_tests() -> None:
    """Exercise both hash readers in a disposable, owner-authenticated repo."""
    scratch, token = strict_json.create_owned_selftest_scratch(ROOT, "revision-json")
    relative_path = f"contracts/fixtures/selftest-{uuid.uuid4().hex}.json"
    environment = {**os.environ, **strict_json.THROWAWAY_GIT_COMMIT_ENV_OVERRIDES}

    def run_git(arguments: list[str]) -> str:
        result = subprocess.run(
            strict_json.git_argv(arguments),
            cwd=scratch,
            capture_output=True,
            env=environment,
        )
        require(
            result.returncode == 0,
            f"Self-test setup failure: {arguments!r} exited {result.returncode}: "
            f"{result.stderr.decode('utf-8', errors='replace')}",
        )
        return result.stdout.decode("utf-8").strip()

    try:
        path = scratch / relative_path
        path.parent.mkdir(parents=True)
        (scratch / "README").write_text("revision JSON self-test\n", encoding="utf-8")
        run_git(["git", "init", "-q"])
        run_git(["git", "add", "-A"])
        run_git(["git", "commit", "-q", "-m", "revision JSON self-test seed"])

        path.write_bytes(b'{"a": 1, "a": 2}')
        try:
            compute_hashes_from_worktree({"documents": [relative_path], "fixtures": []}, root=scratch)
        except SystemExit:
            pass
        else:
            raise SystemExit(
                "Self-test failure: compute_hashes_from_worktree accepted malformed governed JSON."
            )

        path.write_bytes(b'{"a": 1}')
        hashes = compute_hashes_from_worktree(
            {"documents": [relative_path], "fixtures": []}, root=scratch
        )
        require(
            relative_path in hashes,
            "Self-test failure: compute_hashes_from_worktree rejected well-formed governed JSON.",
        )

        path.write_bytes(b'{"a": 1, "a": 2}')
        run_git(["git", "add", "--", relative_path])
        run_git(["git", "commit", "-q", "-m", "malformed historical fixture"])
        malformed_ref = run_git(["git", "rev-parse", "HEAD"])
        try:
            compute_hashes_from_git_ref(
                malformed_ref, {"documents": [relative_path], "fixtures": []}, root=scratch
            )
        except SystemExit:
            pass
        else:
            raise SystemExit(
                "Self-test failure: compute_hashes_from_git_ref accepted malformed historical JSON."
            )

        path.write_bytes(b'{"a": 1}')
        run_git(["git", "add", "--", relative_path])
        run_git(["git", "commit", "-q", "-m", "well-formed historical fixture"])
        hashes = compute_hashes_from_git_ref(
            run_git(["git", "rev-parse", "HEAD"]),
            {"documents": [relative_path], "fixtures": []},
            root=scratch,
        )
        require(
            relative_path in hashes,
            "Self-test failure: compute_hashes_from_git_ref rejected well-formed historical JSON.",
        )
    finally:
        strict_json.release_owned_selftest_scratch(scratch, token)


def run_self_tests() -> None:
    """Prove evaluate_drift()'s logic with small, hard-coded, in-memory
    fixtures, deterministic in any environment. Note this also calls
    compute_hashes_from_worktree() (real filesystem checks against this
    checkout) and run_resolve_base_ref_self_tests() (invokes local git
    commands) below, so this function as a whole is not itself git/
    filesystem-free -- only the core evaluate_drift comparisons are purely
    in-memory."""
    strict_json.run_self_tests()
    strict_json.run_governed_bytes_self_tests()
    strict_json.run_governed_path_self_tests(ROOT)

    same_revision_changed_hash_ok, _ = evaluate_drift(
        base_revision="0.1.12",
        base_hashes={"a.json": "aaaa", "b.json": "bbbb"},
        head_revision="0.1.12",
        head_hashes={"a.json": "aaaa", "b.json": "CHANGED"},
    )
    require(
        same_revision_changed_hash_ok is False,
        "Self-test failure: a changed artifact hash at an unchanged schemaRevision must fail the drift gate.",
    )

    bumped_revision_changed_hash_ok, _ = evaluate_drift(
        base_revision="0.1.12",
        base_hashes={"a.json": "aaaa", "b.json": "bbbb"},
        head_revision="0.1.13",
        head_hashes={"a.json": "aaaa", "b.json": "CHANGED"},
    )
    require(
        bumped_revision_changed_hash_ok is True,
        "Self-test failure: a changed artifact hash with a strictly greater schemaRevision must pass the drift gate.",
    )

    unchanged_ok, _ = evaluate_drift(
        base_revision="0.1.12",
        base_hashes={"a.json": "aaaa"},
        head_revision="0.1.12",
        head_hashes={"a.json": "aaaa"},
    )
    require(unchanged_ok is True, "Self-test failure: no changes at all must always pass the drift gate.")

    added_artifact_same_revision_ok, _ = evaluate_drift(
        base_revision="0.1.12",
        base_hashes={"a.json": "aaaa"},
        head_revision="0.1.12",
        head_hashes={"a.json": "aaaa", "new.json": "nnnn"},
    )
    require(
        added_artifact_same_revision_ok is False,
        "Self-test failure: a newly added governed artifact at an unchanged schemaRevision must fail the drift gate.",
    )

    require(
        parse_revision("0.1.13") > parse_revision("0.1.12"),
        "Self-test failure: parse_revision must compare dot-separated integer components numerically.",
    )
    require(
        parse_revision("0.2.0") > parse_revision("0.1.99"),
        "Self-test failure: parse_revision must compare left-to-right by component, not lexicographically.",
    )

    try:
        governed_paths({"documents": [], "fixtures": [{"notPath": "x"}]})
    except SystemExit:
        pass
    else:
        raise SystemExit(
            "Self-test failure: governed_paths must reject a fixtures[] entry missing 'path' via a "
            "controlled SystemExit, not silently proceed."
        )

    try:
        governed_paths({"documents": [], "fixtures": ["not-a-dict"]})
    except SystemExit:
        pass
    else:
        raise SystemExit(
            "Self-test failure: governed_paths must reject a non-dict fixtures[] entry via a "
            "controlled SystemExit, not silently proceed."
        )

    try:
        # Use a per-run unique sentinel filename (rather than a fixed
        # hardcoded name) so this self-test can never accidentally collide
        # with a real path some future commit legitimately adds to the repo.
        missing_sentinel_path = (
            f"contracts/schemas/__self-test-sentinel-{uuid.uuid4().hex}__.schema.json"
        )
        compute_hashes_from_worktree({"documents": [missing_sentinel_path], "fixtures": []})
    except SystemExit:
        pass
    else:
        raise SystemExit(
            "Self-test failure: compute_hashes_from_worktree must reject a governed path missing from "
            "disk via a controlled SystemExit, not raise a raw FileNotFoundError."
        )

    run_governed_json_strictness_self_tests()

    try:
        require_manifest_schema_revision(["not", "a", "dict"], "selftest")
    except SystemExit:
        pass
    else:
        raise SystemExit(
            "Self-test failure: require_manifest_schema_revision must reject a non-dict manifest via "
            "a controlled SystemExit, not raise a raw TypeError."
        )

    try:
        require_manifest_schema_revision({"artifactHashes": {}}, "selftest")
    except SystemExit:
        pass
    else:
        raise SystemExit(
            "Self-test failure: require_manifest_schema_revision must reject a manifest missing "
            "'schemaRevision' via a controlled SystemExit, not raise a raw KeyError."
        )

    try:
        require_manifest_schema_revision({"schemaRevision": 116}, "selftest")
    except SystemExit:
        pass
    else:
        raise SystemExit(
            "Self-test failure: require_manifest_schema_revision must reject a non-string "
            "'schemaRevision' (e.g. an int) via a controlled SystemExit."
        )

    try:
        require_manifest_schema_revision({"schemaRevision": ""}, "selftest")
    except SystemExit:
        pass
    else:
        raise SystemExit(
            "Self-test failure: require_manifest_schema_revision must reject an empty "
            "'schemaRevision' string via a controlled SystemExit."
        )

    try:
        require_manifest_schema_revision({"schemaRevision": "v1"}, "selftest-malformed-label")
    except SystemExit as error:
        require(
            "selftest-malformed-label" in str(error),
            "Self-test failure: require_manifest_schema_revision must attribute a malformed "
            f"(non-numeric) 'schemaRevision' rejection to its manifest label, got: {error}",
        )
    else:
        raise SystemExit(
            "Self-test failure: require_manifest_schema_revision must reject a non-numeric "
            "'schemaRevision' (e.g. 'v1') via a controlled SystemExit, not defer the failure to a "
            "later, unlabelled parse_revision() call."
        )

    require(
        require_manifest_schema_revision({"schemaRevision": "0.1.16"}, "selftest") == "0.1.16",
        "Self-test failure: require_manifest_schema_revision must return a well-formed 'schemaRevision' "
        "unchanged.",
    )

    try:
        require_manifest_artifact_hashes({"artifactHashes": ["not", "a", "dict"]}, "selftest")
    except SystemExit:
        pass
    else:
        raise SystemExit(
            "Self-test failure: require_manifest_artifact_hashes must reject a non-dict "
            "'artifactHashes' (e.g. a list) via a controlled SystemExit, not raise a raw TypeError "
            "later when treated as a mapping."
        )

    require(
        require_manifest_artifact_hashes({}, "selftest") == {},
        "Self-test failure: require_manifest_artifact_hashes must default a missing 'artifactHashes' "
        "to an empty object rather than failing.",
    )

    run_resolve_base_ref_self_tests()
    run_manifest_worktree_authority_self_tests()


def run_resolve_base_ref_self_tests() -> None:
    """Prove CI base-ref validation in an invocation-owned repository."""
    scratch, token = strict_json.create_owned_selftest_scratch(ROOT, "revision-base")
    environment = {**os.environ, **strict_json.THROWAWAY_GIT_COMMIT_ENV_OVERRIDES}

    def run_git(arguments: list[str]) -> str:
        result = subprocess.run(
            strict_json.git_argv(arguments),
            cwd=scratch,
            capture_output=True,
            env=environment,
        )
        require(
            result.returncode == 0,
            f"Self-test setup failure: {arguments!r} exited {result.returncode}: "
            f"{result.stderr.decode('utf-8', errors='replace')}",
        )
        return result.stdout.decode("utf-8").strip()

    try:
        (scratch / "README").write_text("base-ref self-test\n", encoding="utf-8")
        run_git(["git", "init", "-q", "-b", "main"])
        run_git(["git", "add", "-A"])
        run_git(["git", "commit", "-q", "-m", "base-ref self-test base"])
        (scratch / "README").write_text("base-ref self-test head\n", encoding="utf-8")
        run_git(["git", "add", "-A"])
        run_git(["git", "commit", "-q", "-m", "base-ref self-test head"])

        head_sha = run_git(["git", "rev-parse", "HEAD"])
        for invalid in (None, "not-a-valid-sha", "0" * 40, "F" * 40, "f" * 40, head_sha):
            try:
                resolve_base_ref(invalid, allow_local_fallback=False, root=scratch)
            except SystemExit:
                pass
            else:
                raise SystemExit(f"Self-test failure: invalid explicit base {invalid!r} was accepted.")

        non_ancestor = run_git(
            [
                "git",
                "commit-tree",
                run_git(["git", "rev-parse", "HEAD^{tree}"]),
                "-m",
                "revision-drift self-test non-ancestor",
            ]
        )
        require(
            resolve_ref(non_ancestor, root=scratch),
            "Self-test setup failure: the throwaway non-ancestor commit is not resolvable.",
        )
        try:
            resolve_base_ref(non_ancestor, allow_local_fallback=False, root=scratch)
        except SystemExit:
            pass
        else:
            raise SystemExit(
                "Self-test failure: a resolvable but non-ancestor base commit was accepted."
            )

        environment_names = (
            "CONTRACT_BASE_REF",
            "GITHUB_BASE_REF",
            "GITHUB_EVENT_BEFORE",
            "BASE_SHA",
            "LOCALE_CATALOG_BASE_SHA",
        )
        base = run_git(["git", "rev-parse", "HEAD~1"])
        previous = {name: os.environ.get(name) for name in environment_names}
        try:
            for name in environment_names:
                os.environ[name] = base
            try:
                resolve_base_ref(None, allow_local_fallback=False, root=scratch)
            except SystemExit:
                pass
            else:
                raise SystemExit(
                    "Self-test failure: CI mode resolved a base reference from the environment."
                )
        finally:
            for name, value in previous.items():
                if value is None:
                    os.environ.pop(name, None)
                else:
                    os.environ[name] = value

        require(
            resolve_base_ref(base, allow_local_fallback=False, root=scratch) == base,
            "Self-test failure: a valid explicit base SHA was not accepted.",
        )
        require(
            bool(resolve_base_ref(None, allow_local_fallback=True, root=scratch)),
            "Self-test failure: the local fallback chain resolved nothing.",
        )
    finally:
        strict_json.release_owned_selftest_scratch(scratch, token)


def run_manifest_worktree_authority_self_tests() -> None:
    """End-to-end proof -- real subprocess invocations of the *actual*
    `update-manifest-hashes.py` and `check-schema-revision-drift.py`
    scripts, not merely a direct unit call to the shared
    `strict_json.read_governed_worktree_bytes`/`write_governed_worktree_bytes`
    helpers both now route their *current* `contracts/manifest.json`
    read (and, for the updater, write) through -- that a bad-mode current
    manifest is rejected by both tools before its content is ever trusted
    or overwritten, and that a canonical mode-100644 manifest still works.

    Each scenario builds a small, throwaway, freestanding git repository
    (never nested inside -- nor sharing an index/HEAD with -- this actual
    repository) under a per-run unique directory inside this checkout
    (cleaned up via `shutil.rmtree` in a `finally` block, so it never
    survives past this function even on failure), containing copies of
    exactly the three files these two tools need (`strict_json.py`,
    `update-manifest-hashes.py`, `check-schema-revision-drift.py`) plus a
    minimal governed-artifact set (one schema, one fixture, a manifest
    naming both). Because both tools compute their own `ROOT` as
    `Path(__file__).resolve().parents[1]`, copying them into
    `<scratch>/scripts/` and invoking them with that interpreter is
    sufficient to make every governed-path read/write in this self-test
    resolve entirely inside `<scratch>`, never touching this actual
    repository's `contracts/` tree, index, or HEAD.

    Because the copied `check-schema-revision-drift.py` is a byte-for-byte
    copy of *this very script* (including this self-test function itself
    and its call from `run_self_tests()`), invoking it naively would
    recursively re-run this same self-test inside each scratch copy, which
    would build yet another nested scratch repo one level deeper, and so on
    without bound (in practice terminating only once a generated path
    exceeds the OS path-length limit). The `_MANIFEST_AUTHORITY_SELFTEST_
    SKIP_ENV` marker breaks this recursion: every subprocess this self-test
    launches has that variable set, and the guard immediately below returns
    without doing anything when it is present -- so the scratch copies'
    *other* self-tests (`strict_json.run_self_tests()`,
    `run_governed_bytes_self_tests()`, `run_governed_path_self_tests()`,
    this script's own `run_self_tests()` proper) still all run normally
    (a useful proof the copied scripts are self-contained and correct in
    isolation too), but this specific end-to-end scenario never recurses.
    """
    if os.environ.get(_MANIFEST_AUTHORITY_SELFTEST_SKIP_ENV) == "1":
        return

    scratch_parent, scratch_token = strict_json.create_owned_selftest_scratch(
        ROOT, "manifest-authority"
    )
    scratch_root = scratch_parent / "repo"

    def _build_scratch_repo(*, contract_in_root_commit: bool = True) -> str:
        """Populate a fresh scratch_root with the minimal script/artifact
        set, `git init` it, commit an initial baseline (manifest with an
        empty `artifactHashes`, matching what a hand-authored manifest
        looks like before the updater ever runs), and return that root
        commit's SHA. With `contract_in_root_commit=False` the root commit
        deliberately predates the governed contract entirely, which is the
        shape of the change that first introduces it."""
        (scratch_root / "scripts").mkdir(parents=True)
        (scratch_root / "contracts" / "schemas").mkdir(parents=True)
        (scratch_root / "contracts" / "fixtures").mkdir(parents=True)
        for name in ("strict_json.py", "update-manifest-hashes.py", "check-schema-revision-drift.py"):
            shutil.copyfile(ROOT / "scripts" / name, scratch_root / "scripts" / name)
        (scratch_root / "contracts" / "schemas" / "x.schema.json").write_bytes(b'{"type": "object"}\n')
        (scratch_root / "contracts" / "fixtures" / "x.json").write_bytes(b'{"a": 1}\n')
        (scratch_root / "contracts" / "manifest.json").write_bytes(
            b'{\n'
            b'  "schemaRevision": "0.1.0",\n'
            b'  "documents": ["contracts/schemas/x.schema.json"],\n'
            b'  "fixtures": [{"path": "contracts/fixtures/x.json"}],\n'
            b'  "artifactHashes": {}\n'
            b'}\n'
        )
        scratch_env = {**os.environ, **strict_json.THROWAWAY_GIT_COMMIT_ENV_OVERRIDES}

        def _git(args: list[str]) -> str:
            result = subprocess.run(
                strict_json.git_argv(args), cwd=scratch_root, capture_output=True, env=scratch_env
            )
            if result.returncode != 0:
                raise SystemExit(
                    f"Self-test setup failure: {args!r} exited {result.returncode}: "
                    f"{result.stderr.decode('utf-8', errors='replace')}"
                )
            return result.stdout.decode("utf-8").strip()

        _git(["git", "init", "-q"])
        if contract_in_root_commit:
            _git(["git", "add", "-A"])
        else:
            _git(["git", "add", "-A", "--", "scripts"])
        _git(["git", "commit", "-q", "-m", "scratch baseline (root commit)"])
        # The *root* commit is what every scenario below passes as its
        # immutable base: it stays a strict ancestor of the scratch HEAD (even
        # after scenario 4 amends that HEAD), so the drift gate's "a base SHA
        # may not name HEAD itself" rule never masks what a scenario is
        # actually proving.
        root_commit = _git(["git", "rev-parse", "HEAD"])
        # A second, trivial, content-irrelevant commit so this scratch repo
        # has real commit history (`git rev-list --count HEAD` > 1) --
        # check-schema-revision-drift.py's own run_resolve_base_ref_self_tests
        # (which every invocation of it runs first, including inside this
        # very self-test's subprocess calls below) asserts that an all-zero
        # base SHA must be rejected specifically because "this repository
        # has real commit history"; a single-commit scratch repo would
        # instead satisfy its own repository-initialization escape hatch
        # and make that unrelated internal self-test fail for a reason that
        # has nothing to do with what this self-test is actually proving.
        (scratch_root / "NOTES.txt").write_bytes(b"scratch repo second commit\n")
        _git(["git", "add", "-A"])
        _git(["git", "commit", "-q", "-m", "scratch baseline (second commit)"])
        return root_commit

    def _run(script_name: str) -> subprocess.CompletedProcess:
        env = {
            **os.environ,
            **strict_json.THROWAWAY_GIT_COMMIT_ENV_OVERRIDES,
            _MANIFEST_AUTHORITY_SELFTEST_SKIP_ENV: "1",
        }
        for key in ("GITHUB_ACTIONS", "CI"):
            env.pop(key, None)
        return subprocess.run(
            [sys.executable, str(scratch_root / "scripts" / script_name)],
            cwd=scratch_root,
            capture_output=True,
            env=env,
        )

    def _run_drift_checker(base_ref: str) -> subprocess.CompletedProcess:
        env = {
            **os.environ,
            **strict_json.THROWAWAY_GIT_COMMIT_ENV_OVERRIDES,
            _MANIFEST_AUTHORITY_SELFTEST_SKIP_ENV: "1",
        }
        return subprocess.run(
            [
                sys.executable,
                str(scratch_root / "scripts" / "check-schema-revision-drift.py"),
                base_ref,
            ],
            cwd=scratch_root,
            capture_output=True,
            env=env,
        )

    def _manifest_path() -> Path:
        return scratch_root / "contracts" / "manifest.json"

    def _git_scratch(args: list[str]) -> subprocess.CompletedProcess:
        return subprocess.run(
            strict_json.git_argv(args),
            cwd=scratch_root,
            capture_output=True,
            env={**os.environ, **strict_json.THROWAWAY_GIT_COMMIT_ENV_OVERRIDES},
        )

    # -- Scenario 1: canonical mode-100644 manifest works end-to-end -------
    scratch_root.mkdir()
    try:
        initial_commit = _build_scratch_repo()

        updater_result = _run("update-manifest-hashes.py")
        require(
            updater_result.returncode == 0,
            "Self-test failure: update-manifest-hashes.py must succeed end-to-end against a "
            f"canonical (mode 100644) scratch manifest. stderr: "
            f"{updater_result.stderr.decode('utf-8', errors='replace')}",
        )

        drift_result = _run_drift_checker(initial_commit)
        require(
            drift_result.returncode == 0,
            "Self-test failure: check-schema-revision-drift.py must succeed end-to-end against "
            f"a canonical (mode 100644) scratch manifest. stderr: "
            f"{drift_result.stderr.decode('utf-8', errors='replace')}",
        )
    finally:
        shutil.rmtree(scratch_root, ignore_errors=True)

    # -- Scenario 2: current manifest chmod +x on disk (worktree/head side,
    # index and HEAD both still 100644) must be rejected by both tools. ----
    scratch_root.mkdir()
    try:
        initial_commit = _build_scratch_repo()
        _manifest_path().chmod(0o755)

        updater_result = _run("update-manifest-hashes.py")
        require(
            updater_result.returncode != 0,
            "Self-test failure: update-manifest-hashes.py must reject a current manifest that "
            "is executable on disk (mode 100755), not silently hash/rewrite it.",
        )
        drift_result = _run_drift_checker(initial_commit)
        require(
            drift_result.returncode != 0,
            "Self-test failure: check-schema-revision-drift.py must reject a current manifest "
            "that is executable on disk (mode 100755).",
        )
    finally:
        shutil.rmtree(scratch_root, ignore_errors=True)

    # -- Scenario 3: manifest staged at mode 100755 in the git index while
    # its actual on-disk permission bits remain 100644 ("staged 100755" /
    # filesystem-vs-index disagreement) must be rejected by both tools. ---
    scratch_root.mkdir()
    try:
        initial_commit = _build_scratch_repo()
        chmod_result = _git_scratch(
            ["git", "update-index", "--chmod=+x", "--", "contracts/manifest.json"]
        )
        require(
            chmod_result.returncode == 0,
            f"Self-test setup failure: git update-index --chmod=+x failed: "
            f"{chmod_result.stderr.decode('utf-8', errors='replace')}",
        )
        on_disk_mode_bits = _manifest_path().lstat().st_mode & 0o111
        require(
            on_disk_mode_bits == 0,
            "Self-test setup failure: 'git update-index --chmod=+x' unexpectedly changed the "
            "on-disk permission bits too; this scenario's premise (an index-only mode change) "
            "no longer holds.",
        )

        updater_result = _run("update-manifest-hashes.py")
        require(
            updater_result.returncode != 0,
            "Self-test failure: update-manifest-hashes.py must reject a current manifest whose "
            "git index (staged) mode is 100755 even when its on-disk permission bits are still "
            "100644.",
        )
        drift_result = _run_drift_checker(initial_commit)
        require(
            drift_result.returncode != 0,
            "Self-test failure: check-schema-revision-drift.py must reject a current manifest "
            "whose git index (staged) mode is 100755 while its on-disk bits are still 100644.",
        )
    finally:
        shutil.rmtree(scratch_root, ignore_errors=True)

    # -- Scenario 4: manifest committed at HEAD with mode 100755, while the
    # current worktree/index copy is an ordinary mode-100644 file (a
    # committed-mode disagreement neither the on-disk nor the index check
    # alone would catch) must be rejected by both tools. ------------------
    scratch_root.mkdir()
    try:
        initial_commit = _build_scratch_repo()
        chmod_result = _git_scratch(
            ["git", "update-index", "--chmod=+x", "--", "contracts/manifest.json"]
        )
        require(chmod_result.returncode == 0, "Self-test setup failure: chmod +x in index failed.")
        commit_result = _git_scratch(
            ["git", "commit", "-q", "-m", "scratch manifest mode 100755"]
        )
        require(commit_result.returncode == 0, "Self-test setup failure: mode-change commit failed.")
        # Reset the index/worktree copy back to an ordinary mode-100644 file
        # -- HEAD now (incorrectly) records mode 100755 for this path, but
        # the current worktree/index copy is an ordinary file, exactly
        # mirroring "a mode change was committed, then later reverted on
        # disk/in the index without a new commit fixing HEAD".
        reset_result = _git_scratch(
            ["git", "update-index", "--chmod=-x", "--", "contracts/manifest.json"]
        )
        require(reset_result.returncode == 0, "Self-test setup failure: chmod -x in index failed.")

        updater_result = _run("update-manifest-hashes.py")
        require(
            updater_result.returncode != 0,
            "Self-test failure: update-manifest-hashes.py must reject a current manifest whose "
            "git HEAD-committed mode (100755) disagrees with its current index/on-disk mode "
            "(100644).",
        )
        drift_result = _run_drift_checker(initial_commit)
        require(
            drift_result.returncode != 0,
            "Self-test failure: check-schema-revision-drift.py must reject a current manifest "
            "whose git HEAD-committed mode disagrees with its current index/on-disk mode.",
        )
    finally:
        shutil.rmtree(scratch_root, ignore_errors=True)

    # -- Scenario 6: a base commit that predates the governed contract has
    # no released revision to protect, so the introducing change passes. --
    scratch_root.mkdir()
    try:
        initial_commit = _build_scratch_repo(contract_in_root_commit=False)
        updater_result = _run("update-manifest-hashes.py")
        require(
            updater_result.returncode == 0,
            "Self-test failure: update-manifest-hashes.py must succeed against a scratch "
            f"manifest introduced after the base commit. stderr: "
            f"{updater_result.stderr.decode('utf-8', errors='replace')}",
        )
        drift_result = _run_drift_checker(initial_commit)
        require(
            drift_result.returncode == 0,
            "Self-test failure: check-schema-revision-drift.py must accept a base commit that "
            "predates contracts/manifest.json entirely. stderr: "
            f"{drift_result.stderr.decode('utf-8', errors='replace')}",
        )
        require(
            b"does not exist at that commit" in drift_result.stdout,
            "Self-test failure: the introducing-change pass must say why it passed. stdout: "
            f"{drift_result.stdout.decode('utf-8', errors='replace')}",
        )
    finally:
        shutil.rmtree(scratch_root, ignore_errors=True)

    # -- Scenario 5: the current manifest path is a symlink pointing at an
    # external file (outside contracts/ entirely) containing genuinely
    # valid, well-formed JSON identical in shape to a legitimate manifest.
    # Both tools must reject this outright, and -- critically -- the
    # external target's bytes must remain provably untouched afterward,
    # proving neither tool ever writes/reads through the symlink to
    # whatever it currently resolves to. --------------------------------
    scratch_root.mkdir()
    try:
        initial_commit = _build_scratch_repo()
        external_sentinel = scratch_root / "external-sentinel.json"
        external_sentinel_bytes = _manifest_path().read_bytes()
        external_sentinel.write_bytes(external_sentinel_bytes)
        _manifest_path().unlink()
        (scratch_root / "contracts" / "manifest.json").symlink_to(
            Path("..") / "external-sentinel.json"
        )

        updater_result = _run("update-manifest-hashes.py")
        require(
            updater_result.returncode != 0,
            "Self-test failure: update-manifest-hashes.py must reject a current manifest path "
            "that is a symlink, rather than following it to read/overwrite whatever external "
            "file it points at.",
        )
        drift_result = _run_drift_checker(initial_commit)
        require(
            drift_result.returncode != 0,
            "Self-test failure: check-schema-revision-drift.py must reject a current manifest "
            "path that is a symlink.",
        )
        require(
            external_sentinel.read_bytes() == external_sentinel_bytes,
            "Self-test failure: neither tool may ever modify the external file a "
            "manifest-path symlink points at, whether the read/write attempt is accepted or "
            "rejected.",
        )
    finally:
        shutil.rmtree(scratch_root, ignore_errors=True)
        strict_json.release_owned_selftest_scratch(scratch_parent, scratch_token)


def main() -> None:
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("base_sha", nargs="?", help="authoritative 40-character lowercase ancestor commit")
    parser.add_argument(
        "--allow-local-fallback",
        action="store_true",
        help="use the documented local fallback chain when no positional base SHA is supplied",
    )
    arguments = parser.parse_args()
    run_self_tests()

    head_manifest = load_head_manifest()
    head_revision = require_manifest_schema_revision(head_manifest, "head")
    head_hashes = compute_hashes_from_worktree(head_manifest)

    recorded_hashes = require_manifest_artifact_hashes(head_manifest, "head")
    recorded_paths = set(recorded_hashes)
    actual_paths = set(head_hashes)
    missing = sorted(actual_paths - recorded_paths)
    stale_extra = sorted(recorded_paths - actual_paths)
    require(
        not missing,
        f"manifest.json artifactHashes is missing entries for governed artifacts: {missing}",
    )
    require(
        not stale_extra,
        f"manifest.json artifactHashes has stale/extra entries no longer governed: {stale_extra}",
    )
    mismatched = sorted(
        path for path in actual_paths if recorded_hashes[path] != head_hashes[path]
    )
    require(
        not mismatched,
        f"manifest.json artifactHashes is stale (recomputed sha256 differs) for: {mismatched}. "
        "Recompute and update artifactHashes whenever a governed artifact's content changes.",
    )

    base_ref = resolve_base_ref(arguments.base_sha, allow_local_fallback=arguments.allow_local_fallback)
    base_manifest = load_manifest_from_git_ref(base_ref)
    if base_manifest is None:
        # The governed contract did not exist at the base commit, so no
        # revision of it has been released and nothing there is immutable yet
        # -- the introducing change cannot possibly break release immutability.
        # This is deliberately narrow: `read_governed_git_ref_bytes` returns
        # None only when `git ls-tree <base> -- contracts/manifest.json`
        # matches nothing at all. A path that exists at the base but is a
        # symlink, an executable blob, a gitlink or a tree still raises, and
        # the head-side `artifactHashes` verification above already ran
        # unconditionally.
        print(
            f"Schema revision-drift gate passed (base ref {base_ref}): the governed contract "
            "does not exist at that commit, so no released revision is being changed."
        )
        print(
            f"artifactHashes verified for {len(head_hashes)} governed paths at revision "
            f"{head_revision}."
        )
        return

    base_revision = require_manifest_schema_revision(base_manifest, f"base ({base_ref})")
    base_hashes = compute_hashes_from_git_ref(base_ref, base_manifest)

    ok, detail = evaluate_drift(base_revision, base_hashes, head_revision, head_hashes)
    require(ok, f"Schema revision-drift gate failed (base ref {base_ref}): {detail}")

    print(
        f"Schema revision-drift gate passed (base ref {base_ref}, base revision "
        f"{base_revision}, head revision {head_revision}): {detail}"
    )
    print(f"artifactHashes verified for {len(head_hashes)} governed paths at revision {head_revision}.")


if __name__ == "__main__":
    main()
