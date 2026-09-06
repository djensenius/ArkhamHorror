#!/usr/bin/env python3
"""Generate the public locale catalog through the one production Node entry.

The generator is JavaScript, and like every other committed file in this
repository it is trusted code reviewed through pull request. Nothing here
sandboxes it: it runs with full Node privileges. What this entry point buys is
narrower and more useful:

* **Centralisation.** Production generation happens in exactly one place. Every
  other production path -- the catalog validator, the serving gate, npm's
  `prebuild`, the container build, the offline build and packaging -- starts
  the same `frontend/scripts/locale-catalog/generator-launcher.mjs`.
* **Drift detection.** The generator directory must contain only declared
  files, and `generator-module-digests.json` must describe it exactly. The
  launcher then binds the module graph at run time with Node's resolve/load
  hooks, so a module that is not listed, or whose bytes no longer match, stops
  the run. That is a coordinated-change requirement, not an authorization root.
* **Exact input identity and provenance.** The whole closure -- generator
  sources, launcher, digest list, `package.json`, `package-lock.json` and the
  Node binary -- is hashed before Node starts and re-hashed after it returns,
  and the generator folds the same directory into the catalog's own provenance
  record. A published catalog therefore names the generator revision that
  produced it.

Post-run re-hashing detects a change that happened while the command ran on a
quiet machine; it is not a race defence, and nothing here claims one.
"""

from __future__ import annotations

import hashlib
import json
from pathlib import Path
import subprocess
import sys

import strict_json

ROOT = Path(__file__).resolve().parents[1]
FRONTEND = ROOT / "frontend"
GENERATOR_DIR = FRONTEND / "scripts" / "locale-catalog"
PACKAGE_JSON = FRONTEND / "package.json"
PACKAGE_LOCK = FRONTEND / "package-lock.json"

# The generator's module graph is enforced at run time by
# `frontend/scripts/locale-catalog/generator-launcher.mjs`, which installs
# Node resolve/load hooks before the entry module is imported. Lexical scanning
# was removed deliberately: leading whitespace, comments and computed or
# dynamic specifiers all defeat a regex, and none of them defeat a hook.
LAUNCHER = GENERATOR_DIR / "generator-launcher.mjs"
DIGESTS = GENERATOR_DIR / "generator-module-digests.json"
GENERATOR_ENTRY = "generate.mjs"


def require(condition: object, message: str) -> None:
    if not condition:
        raise SystemExit(f"locale-catalog generator: {message}")


def sha256_hex(data: bytes) -> str:
    return hashlib.sha256(data).hexdigest()


def generator_launcher_argv(root: Path, entry: str, arguments: list[str]) -> list[str]:
    """The one sanctioned way to start a governed Node entry point.

    Every authoritative path -- this generator, the catalog validator, the
    serving gate, the container build, the offline build and npm's own
    `prebuild` -- goes through the sealed launcher with the entry module named
    by file name, never by a direct path to a generator module.
    """
    launcher = (root / "frontend" / "scripts" / "locale-catalog" / "generator-launcher.mjs").resolve()
    return [str(launcher), entry, *arguments]


def generator_sources() -> list[Path]:
    require(
        not GENERATOR_DIR.is_symlink() and GENERATOR_DIR.is_dir(),
        "the locale-catalog generator directory is not a regular directory",
    )
    sources = []
    for path in sorted(GENERATOR_DIR.iterdir()):
        require(not path.is_symlink(), f"generator directory contains a symlink: {path.name}")
        require(path.is_file(), f"generator directory contains a non-regular entry: {path.name}")
        require(
            path.suffix in {".mjs", ".json"},
            f"generator directory contains an undeclared file type: {path.name}",
        )
        sources.append(path)
    for required in (LAUNCHER, DIGESTS, GENERATOR_DIR / GENERATOR_ENTRY):
        require(required in sources, f"the generator directory is missing {required.name}")
    return sources


def check_module_digests(sources: list[Path]) -> None:
    """The committed digest list must describe the generator directory exactly.

    `generator-module-digests.json` is a *drift and provenance record*, not an
    authorization root: every module it names is trusted committed code that
    was reviewed through pull request. What it buys is that a change to any of
    those modules -- or a module appearing or disappearing -- has to be a
    coordinated, reviewed edit that moves the catalog revision, instead of a
    quiet difference between what ran and what provenance describes. Production
    generation refuses to run on drift.
    """
    declared = json.loads(DIGESTS.read_text(encoding="utf-8"))
    require(isinstance(declared, dict) and declared, "the generator module digest list is empty")
    expected = {
        path.name: sha256_hex(path.read_bytes())
        for path in sources
        if path not in (LAUNCHER, DIGESTS)
    }
    require(
        declared == expected,
        "the committed generator module digests have drifted from the generator directory; "
        f"missing {sorted(set(expected) - set(declared))}, "
        f"unexpected {sorted(set(declared) - set(expected))}, "
        f"changed {sorted(name for name in set(expected) & set(declared) if expected[name] != declared[name])}",
    )


def closure_identity(sources: list[Path], node: str) -> dict[str, str]:
    """The exact bytes this run is about to hand to, and execute with, Node."""
    identity = {
        path.relative_to(ROOT).as_posix(): sha256_hex(path.read_bytes()) for path in sources
    }
    for path in (PACKAGE_JSON, PACKAGE_LOCK):
        require(not path.is_symlink() and path.is_file(), f"{path.name} is not a regular file")
        identity[path.relative_to(ROOT).as_posix()] = sha256_hex(path.read_bytes())
    identity[f"node:{node}"] = sha256_hex(Path(node).read_bytes())
    return identity


def main() -> None:
    node = strict_json.trusted_node()
    sources = generator_sources()
    check_module_digests(sources)
    before = closure_identity(sources, node)

    result = subprocess.run(
        [node, *generator_launcher_argv(ROOT, GENERATOR_ENTRY, sys.argv[1:])],
        cwd=FRONTEND.resolve(),
        check=False,
    )

    after = closure_identity(generator_sources(), node)
    drifted = sorted(key for key in before if before.get(key) != after.get(key))
    drifted += sorted(key for key in after if key not in before)
    require(
        not drifted,
        "the locale-catalog generator closure changed while it ran; the catalog this command "
        f"produced is not the one its provenance describes: {drifted}",
    )
    if result.returncode != 0:
        raise SystemExit(result.returncode)


if __name__ == "__main__":
    main()
