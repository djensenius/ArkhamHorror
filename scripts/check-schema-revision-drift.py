#!/usr/bin/env python3
# /// script
# requires-python = ">=3.14"
# ///
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
logic against small, hard-coded, in-memory fixtures (no git, no filesystem),
so the gate's own correctness can be proven deterministically in any
environment, independent of this repository's actual git history.
"""

import hashlib
import json
import os
import re
import subprocess
from pathlib import Path

import strict_json

ROOT = Path(__file__).resolve().parents[1]
MANIFEST_PATH = ROOT / "contracts" / "manifest.json"

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


def load_json(path: Path) -> object:
    return strict_json.strict_json_load_path(path)


def canonicalize_manifest_bytes(manifest: dict) -> bytes:
    """Manifest content, with the self-referential `artifactHashes` map
    zeroed out, so the manifest can be hashed like any other governed
    artifact without hashing its own hash map (which would be circular)."""
    canonical = dict(manifest)
    canonical["artifactHashes"] = {}
    return json.dumps(canonical, sort_keys=True, indent=2).encode("utf-8") + b"\n"


def require_manifest_schema_revision(manifest: object, label: str) -> str:
    """Read `schemaRevision` from a manifest, failing via a controlled
    SystemExit (not a raw KeyError/TypeError) if the manifest itself isn't an
    object, the key is missing, or its value isn't the dot-separated numeric
    string this gate's monotonic-revision comparison requires.
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


def compute_hashes_from_worktree(manifest: dict) -> dict[str, str]:
    hashes = {}
    for relative_path in governed_paths(manifest):
        artifact_file = ROOT / relative_path
        require(
            artifact_file.is_file(),
            f"manifest.json references a governed artifact that does not exist on disk: "
            f"{relative_path} (resolved: {artifact_file})",
        )
        content = artifact_file.read_bytes()
        hashes[relative_path] = hashlib.sha256(content).hexdigest()
    hashes["contracts/manifest.json"] = hashlib.sha256(
        canonicalize_manifest_bytes(manifest)
    ).hexdigest()
    return hashes


def git_show(ref: str, path: str) -> bytes | None:
    result = subprocess.run(
        ["git", "show", f"{ref}:{path}"],
        cwd=ROOT,
        capture_output=True,
    )
    if result.returncode != 0:
        return None
    return result.stdout


def resolve_ref(ref: str) -> bool:
    result = subprocess.run(
        ["git", "rev-parse", "--verify", "--quiet", f"{ref}^{{commit}}"],
        cwd=ROOT,
        capture_output=True,
    )
    return result.returncode == 0


_HEX_SHA_RE = re.compile(r"^[0-9a-fA-F]{7,40}$")
_ALL_ZERO_SHA_RE = re.compile(r"^0{7,40}$")


def _is_ci_environment() -> bool:
    """GitHub Actions always sets both of these to the literal string
    'true' for every workflow run; checking both (rather than just one)
    keeps this detection resilient to any single-variable spoofing in a
    step's `env:` block, since a real Actions runner sets both consistently.
    """
    return os.environ.get("GITHUB_ACTIONS") == "true" and os.environ.get("CI") == "true"


def _is_repository_initialization() -> bool:
    """The only case where an all-zero/missing base SHA is legitimate: this
    push genuinely created the very first commit this repository has ever
    had (so there is, by construction, no prior governed-artifact state to
    have drifted from, and accepting it cannot weaken main -- there is no
    main to weaken yet). Detected by there being no second reachable commit
    from HEAD; never inferred merely from the base SHA being absent.
    """
    result = subprocess.run(
        ["git", "rev-list", "--count", "HEAD"],
        cwd=ROOT,
        capture_output=True,
        text=True,
    )
    if result.returncode != 0:
        return False
    try:
        commit_count = int(result.stdout.strip())
    except ValueError:
        return False
    return commit_count <= 1


def resolve_base_ref() -> str:
    """Resolve the immutable base commit to diff governed contract artifacts
    against.

    In CI (detected via the GitHub Actions-provided `GITHUB_ACTIONS`/`CI`
    environment variables), the caller workflow *must* provide
    `CONTRACT_BASE_REF` explicitly, sourced per trigger type from an
    event-provided field that cannot be spoofed by the pushed branch itself:
      - `pull_request`: `github.event.pull_request.base.sha`
      - `push`:         `github.event.before`
      - `workflow_dispatch`: a required workflow input

    This deliberately never infers a base from `fork/main`/`origin/main`/
    `main`/current HEAD in CI: any of those can resolve to the very branch
    being validated (e.g. a push where the remote-tracking `main` has
    already been fast-forwarded to the pushed commit itself), which would
    silently compare a revision to itself and let real drift through. A
    missing, malformed, or all-zero SHA fails the gate closed rather than
    silently falling back, except for the narrow, explicitly-checked
    repository-initialization case.

    Outside CI (local development), `CONTRACT_BASE_REF` is honored if set,
    else this falls back to a documented, deterministic chain of local refs
    for developer convenience -- this fallback path is never reachable in
    CI.
    """
    env_ref = os.environ.get("CONTRACT_BASE_REF")

    if _is_ci_environment():
        require(
            bool(env_ref),
            "CONTRACT_BASE_REF must be set explicitly in CI (from "
            "github.event.pull_request.base.sha / github.event.before / a required "
            "workflow_dispatch input) -- this gate never infers a base ref from "
            "fork/main, origin/main, main, or a hardcoded fallback in CI, since any "
            "of those can resolve to the branch being validated itself.",
        )
        if _ALL_ZERO_SHA_RE.fullmatch(env_ref or ""):
            require(
                _is_repository_initialization(),
                f"CONTRACT_BASE_REF {env_ref!r} is the all-zero SHA git uses for "
                "'no prior commit' (e.g. a newly created branch/ref), which is only "
                "acceptable if this repository has no commit history at all yet; it "
                "does, so refusing to silently skip the drift gate.",
            )
            require(
                resolve_ref("HEAD"),
                "Repository-initialization case detected but HEAD itself does not resolve.",
            )
            return "HEAD"
        require(
            _HEX_SHA_RE.fullmatch(env_ref),
            f"CONTRACT_BASE_REF must be a valid hex git commit SHA (7-40 hex characters), "
            f"got {env_ref!r}.",
        )
        require(
            resolve_ref(env_ref),
            f"CONTRACT_BASE_REF {env_ref!r} does not resolve to a commit in this checkout's "
            "local history. Ensure the workflow's checkout step fetches enough history "
            "(e.g. actions/checkout with fetch-depth: 0, or a targeted fetch of that SHA) "
            "-- this gate never falls back to a network call.",
        )
        return env_ref

    # Local development: honor an explicit override first, then fall back to
    # a documented, deterministic chain of local refs (never reachable in CI).
    candidate_refs = []
    if env_ref:
        candidate_refs.append(env_ref)
    candidate_refs.extend(["fork/main", "origin/main", "main", FALLBACK_BASE_SHA])

    attempted = []
    for ref in candidate_refs:
        if ref in attempted:
            continue
        attempted.append(ref)
        if resolve_ref(ref):
            return ref

    raise SystemExit(
        "Could not resolve any base ref with local git history (tried: "
        f"{attempted}). This gate never falls back to a network call; ensure the "
        "checkout includes enough local history to resolve one of these refs, or set "
        "CONTRACT_BASE_REF explicitly."
    )


def load_manifest_from_git_ref(ref: str) -> dict | None:
    content = git_show(ref, "contracts/manifest.json")
    if content is None:
        return None
    return strict_json.strict_json_loads(content, source=f"{ref}:contracts/manifest.json")


def compute_hashes_from_git_ref(ref: str, manifest: dict) -> dict[str, str]:
    hashes = {}
    for relative_path in governed_paths(manifest):
        content = git_show(ref, relative_path)
        require(content is not None, f"Could not read {relative_path} at {ref} via local git history")
        hashes[relative_path] = hashlib.sha256(content).hexdigest()
    hashes["contracts/manifest.json"] = hashlib.sha256(
        canonicalize_manifest_bytes(manifest)
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


def run_self_tests() -> None:
    """Prove evaluate_drift()'s logic with small, fully in-memory fixtures
    (no git, no filesystem) -- deterministic in any environment."""
    strict_json.run_self_tests()

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
        compute_hashes_from_worktree(
            {"documents": ["contracts/schemas/__does-not-exist__.schema.json"], "fixtures": []}
        )
    except SystemExit:
        pass
    else:
        raise SystemExit(
            "Self-test failure: compute_hashes_from_worktree must reject a governed path missing from "
            "disk via a controlled SystemExit, not raise a raw FileNotFoundError."
        )

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


def run_resolve_base_ref_self_tests() -> None:
    """Prove resolve_base_ref()'s CI-mode fail-closed behavior deterministically,
    by toggling only environment variables it reads (restored via try/finally
    regardless of outcome) -- never actual git state."""
    saved_env = {
        key: os.environ.get(key) for key in ("GITHUB_ACTIONS", "CI", "CONTRACT_BASE_REF")
    }

    def _restore() -> None:
        for key, value in saved_env.items():
            if value is None:
                os.environ.pop(key, None)
            else:
                os.environ[key] = value

    def _set_ci(base_ref_value: str | None) -> None:
        os.environ["GITHUB_ACTIONS"] = "true"
        os.environ["CI"] = "true"
        if base_ref_value is None:
            os.environ.pop("CONTRACT_BASE_REF", None)
        else:
            os.environ["CONTRACT_BASE_REF"] = base_ref_value

    try:
        _set_ci(None)
        try:
            resolve_base_ref()
        except SystemExit:
            pass
        else:
            raise SystemExit(
                "Self-test failure: resolve_base_ref must fail closed in CI when "
                "CONTRACT_BASE_REF is unset, not silently infer fork/main, origin/main, "
                "main, or a hardcoded fallback."
            )

        _set_ci("not-a-valid-sha")
        try:
            resolve_base_ref()
        except SystemExit:
            pass
        else:
            raise SystemExit(
                "Self-test failure: resolve_base_ref must reject a syntactically invalid "
                "CONTRACT_BASE_REF in CI."
            )

        _set_ci("0000000000000000000000000000000000000000")
        try:
            resolve_base_ref()
        except SystemExit:
            pass
        else:
            raise SystemExit(
                "Self-test failure: resolve_base_ref must reject the all-zero SHA in CI for "
                "this repository (which has real commit history), not treat it as the "
                "repository-initialization escape hatch."
            )

        _set_ci("0000000")
        try:
            resolve_base_ref()
        except SystemExit:
            pass
        else:
            raise SystemExit(
                "Self-test failure: resolve_base_ref must reject a short all-zero SHA in CI "
                "the same way as a full-length one."
            )

        _set_ci("ffffffffffffffffffffffffffffffffffffffff")
        try:
            resolve_base_ref()
        except SystemExit:
            pass
        else:
            raise SystemExit(
                "Self-test failure: resolve_base_ref must reject a syntactically valid but "
                "locally-unresolvable SHA in CI rather than silently falling back."
            )

        head_sha = subprocess.run(
            ["git", "rev-parse", "HEAD"], cwd=ROOT, capture_output=True, text=True, check=True
        ).stdout.strip()

        _set_ci(head_sha)
        require(
            resolve_base_ref() == head_sha,
            "Self-test failure: resolve_base_ref must honor an explicit, locally-resolvable "
            "CONTRACT_BASE_REF in CI.",
        )

        for key in ("GITHUB_ACTIONS", "CI"):
            os.environ.pop(key, None)
        os.environ["CONTRACT_BASE_REF"] = head_sha
        require(
            resolve_base_ref() == head_sha,
            "Self-test failure: resolve_base_ref must honor an explicit CONTRACT_BASE_REF "
            "outside CI too.",
        )
    finally:
        _restore()


def main() -> None:
    run_self_tests()

    head_manifest = load_json(MANIFEST_PATH)
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

    base_ref = resolve_base_ref()
    base_manifest = load_manifest_from_git_ref(base_ref)
    require(
        base_manifest is not None,
        f"Resolved base ref {base_ref!r} does not have a readable contracts/manifest.json via "
        "local git history. This gate never falls back to a network call; ensure the checkout "
        "includes enough local history to read that path at this ref.",
    )

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
