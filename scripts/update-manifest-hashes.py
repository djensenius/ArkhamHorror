#!/usr/bin/env python3
"""Recompute contracts/manifest.json's `artifactHashes` field in place.

Maintainer tool: run this any time a governed schema, fixture, or the
manifest's own descriptor content (documents/fixtures/negativeFixtures/etc.)
changes -- it must be the LAST edit before committing, since it hashes the
current on-disk content of every governed artifact plus a canonicalized hash
of the manifest itself (with `artifactHashes` zeroed out to avoid hashing
itself). `scripts/check-schema-revision-drift.py` recomputes and strictly
verifies these same hashes; run this script first, then run that one.
"""
import hashlib
import json
from pathlib import Path

import strict_json

ROOT = Path(__file__).resolve().parents[1]
MANIFEST = ROOT / "contracts" / "manifest.json"
MANIFEST_RELATIVE_PATH = "contracts/manifest.json"


def require(condition: bool, message: str) -> None:
    if not condition:
        raise SystemExit(message)


def main() -> None:
    strict_json.run_self_tests()
    strict_json.run_governed_bytes_self_tests()
    strict_json.run_governed_path_self_tests(ROOT)

    # Route the *current* manifest itself through the exact same governed
    # worktree reader every other governed artifact is hashed through
    # (`read_governed_worktree_bytes`), rather than a bare
    # `strict_json.strict_json_load_path(MANIFEST)` (equivalent to
    # `MANIFEST.read_bytes()`), which would silently follow a manifest-path
    # symlink and skip the filesystem-permission/git-index/git-HEAD mode
    # checks entirely -- a current 100755 (or symlinked) manifest would
    # otherwise pass this updater silently now, only to permanently break
    # every future `read_governed_git_ref_bytes` base-ref read once that
    # state is committed and later reused as an immutable base.
    manifest = strict_json.strict_json_loads(
        strict_json.read_governed_worktree_bytes(ROOT, MANIFEST_RELATIVE_PATH),
        source=str(MANIFEST),
    )
    require(
        isinstance(manifest, dict),
        f"manifest.json is not a JSON object (got {type(manifest).__name__}): {manifest!r}",
    )

    documents = manifest.get("documents", [])
    require(
        isinstance(documents, list),
        f"manifest.json 'documents' must be a JSON array, got {documents!r}",
    )
    for document_index, document in enumerate(documents):
        require(
            isinstance(document, str),
            f"manifest.json documents entry #{document_index} must be a string path, got {document!r}",
        )
    paths = set(documents)

    fixtures = manifest.get("fixtures", [])
    require(
        isinstance(fixtures, list),
        f"manifest.json 'fixtures' must be a JSON array, got {fixtures!r}",
    )
    for fixture_index, fixture in enumerate(fixtures):
        require(
            isinstance(fixture, dict) and "path" in fixture,
            f"manifest.json fixtures entry #{fixture_index} is missing a 'path' key: {fixture!r}",
        )
        require(
            isinstance(fixture["path"], str),
            f"manifest.json fixtures entry #{fixture_index} 'path' must be a string, got "
            f"{fixture['path']!r}",
        )
        paths.add(fixture["path"])
    # Sort so `artifactHashes` ordering is stable and independent of the
    # (unordered-in-spirit) order documents/fixtures happen to be listed in,
    # keeping diffs minimal when entries are added/reordered elsewhere in the
    # manifest.
    sorted_paths = sorted(paths)

    hashes = {}
    for relative_path in sorted_paths:
        content = strict_json.read_governed_worktree_bytes(ROOT, relative_path)
        hashes[relative_path] = hashlib.sha256(content).hexdigest()

    manifest["artifactHashes"] = hashes
    canon_bytes = strict_json.canonicalize_manifest_bytes(manifest)
    manifest["artifactHashes"]["contracts/manifest.json"] = hashlib.sha256(canon_bytes).hexdigest()
    manifest["artifactHashes"] = dict(sorted(manifest["artifactHashes"].items()))

    # Publish through the same governed writer: it re-validates the current
    # on-disk/index/HEAD mode immediately before writing, and atomically
    # replaces the manifest's directory entry (`os.replace`) rather than
    # opening-and-truncating whatever it currently resolves to -- so even if
    # `contracts/manifest.json` somehow were a symlink, this can never write
    # through it to whatever external file it points at (the write either
    # fails the mode check outright, or, worst case, the replace unlinks the
    # symlink itself and leaves the external target provably untouched).
    strict_json.write_governed_worktree_bytes(
        ROOT,
        MANIFEST_RELATIVE_PATH,
        (json.dumps(manifest, indent=2) + "\n").encode("utf-8"),
    )
    print(f"Recomputed {len(manifest['artifactHashes'])} artifact hashes.")


if __name__ == "__main__":
    main()
