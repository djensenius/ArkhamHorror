#!/usr/bin/env python3
# /// script
# requires-python = ">=3.14"
# ///
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


def require(condition: bool, message: str) -> None:
    if not condition:
        raise SystemExit(message)


def main() -> None:
    strict_json.run_self_tests()
    strict_json.run_governed_bytes_self_tests()
    strict_json.run_governed_path_self_tests(ROOT)

    manifest = strict_json.strict_json_load_path(MANIFEST)
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

    MANIFEST.write_text(json.dumps(manifest, indent=2) + "\n", encoding="utf-8")
    print(f"Recomputed {len(manifest['artifactHashes'])} artifact hashes.")


if __name__ == "__main__":
    main()

