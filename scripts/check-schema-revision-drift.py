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
import subprocess
from pathlib import Path

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
    with path.open(encoding="utf-8") as handle:
        return json.load(handle)


def canonicalize_manifest_bytes(manifest: dict) -> bytes:
    """Manifest content, with the self-referential `artifactHashes` map
    zeroed out, so the manifest can be hashed like any other governed
    artifact without hashing its own hash map (which would be circular)."""
    canonical = dict(manifest)
    canonical["artifactHashes"] = {}
    return json.dumps(canonical, sort_keys=True, indent=2).encode("utf-8") + b"\n"


def governed_paths(manifest: dict) -> list[str]:
    paths = list(manifest.get("documents", []))
    for index, fixture in enumerate(manifest.get("fixtures", [])):
        require(
            isinstance(fixture, dict) and "path" in fixture,
            f"manifest.json fixtures entry #{index} is not an object with a 'path' key: {fixture!r}",
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


def load_manifest_from_git_ref(ref: str) -> dict | None:
    content = git_show(ref, "contracts/manifest.json")
    if content is None:
        return None
    return json.loads(content)


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


def main() -> None:
    run_self_tests()

    head_manifest = load_json(MANIFEST_PATH)
    head_revision = head_manifest["schemaRevision"]
    head_hashes = compute_hashes_from_worktree(head_manifest)

    recorded_hashes = head_manifest.get("artifactHashes", {})
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

    candidate_refs = []
    env_ref = os.environ.get("CONTRACT_BASE_REF")
    if env_ref:
        candidate_refs.append(env_ref)
    # Try this repository's own remote first (the actual fork this contract
    # slice lives in), then a conventionally-named upstream, then a local
    # branch, then the hard-coded historical commit this slice was branched
    # from -- all local git history lookups, never a network call.
    candidate_refs.extend(["fork/main", "origin/main", "main", FALLBACK_BASE_SHA])

    base_ref = None
    base_manifest = None
    attempted = []
    for ref in candidate_refs:
        if ref in attempted:
            continue
        attempted.append(ref)
        if not resolve_ref(ref):
            continue
        manifest_at_ref = load_manifest_from_git_ref(ref)
        if manifest_at_ref is not None:
            base_ref = ref
            base_manifest = manifest_at_ref
            break

    require(
        base_ref is not None,
        "Could not resolve any base ref with a readable contracts/manifest.json via local git "
        f"history (tried: {attempted}). This gate never falls back to a network call; ensure "
        "the checkout includes enough local history to resolve one of these refs.",
    )

    base_revision = base_manifest["schemaRevision"]
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
