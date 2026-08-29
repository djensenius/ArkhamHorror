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

ROOT = Path(__file__).resolve().parents[1]
MANIFEST = ROOT / "contracts" / "manifest.json"

manifest = json.loads(MANIFEST.read_text(encoding="utf-8"))
paths = set(manifest.get("documents", []))
for fixture in manifest.get("fixtures", []):
    paths.add(fixture["path"])
# Sort so `artifactHashes` ordering is stable and independent of the
# (unordered-in-spirit) order documents/fixtures happen to be listed in,
# keeping diffs minimal when entries are added/reordered elsewhere in the
# manifest.
paths = sorted(paths)

hashes = {}
for relative_path in paths:
    hashes[relative_path] = hashlib.sha256((ROOT / relative_path).read_bytes()).hexdigest()

manifest["artifactHashes"] = hashes
canonical = dict(manifest)
canonical["artifactHashes"] = {}
canon_bytes = json.dumps(canonical, sort_keys=True, indent=2).encode("utf-8") + b"\n"
manifest["artifactHashes"]["contracts/manifest.json"] = hashlib.sha256(canon_bytes).hexdigest()
manifest["artifactHashes"] = dict(sorted(manifest["artifactHashes"].items()))

MANIFEST.write_text(json.dumps(manifest, indent=2) + "\n", encoding="utf-8")
print(f"Recomputed {len(manifest['artifactHashes'])} artifact hashes.")

