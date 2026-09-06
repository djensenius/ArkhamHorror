#!/usr/bin/env python3
"""Generate the public locale catalog with the sealed Node authority.

The generator itself is JavaScript, so this entry point is where the Node side
of the boundary is bound. Under T1 -- hostile or mistaken *committed* source --
the question is whether the bytes Node executes are exactly the committed,
provenance-hashed closure, and whether that module graph can reach anything
outside it.

The graph is enforced at run time rather than by reading source text: every
governed Node entry point starts through
`frontend/scripts/locale-catalog/sealed-node-launcher.mjs`, which installs
Node's synchronous resolve/load hooks before importing the entry module and
refuses anything that is not a `node:` builtin, a locked `node_modules`
package, or a committed allowlisted generator module served from its hashed
bytes. Leading whitespace, comments, computed specifiers and dynamic
`import()` are therefore all covered, because none of them changes what the
loader is asked to resolve.

This module adds the surrounding checks: the generator directory contains only
declared files, the allowlist describes that directory exactly, and the whole
closure -- generator sources, launcher, allowlist, `package.json`,
`package-lock.json` and the bound Node binary -- is hashed before Node starts
and re-hashed after it returns. That detects a stable-run change; it does not
prevent a concurrent same-UID process from swapping those bytes between the
check and Node's read. That is T2, which this boundary does not claim -- CI
runs each governed command in an isolated ephemeral job. See
docs/locale-catalog.md.

The generator hashes the same directory into the catalog's own `provenance`
record, so the bytes validated here are the bytes provenance describes.
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
# `frontend/scripts/locale-catalog/sealed-node-launcher.mjs`, which installs
# Node resolve/load hooks before the entry module is imported. Lexical scanning
# was removed deliberately: leading whitespace, comments and computed or
# dynamic specifiers all defeat a regex, and none of them defeat a hook.
LAUNCHER = GENERATOR_DIR / "sealed-node-launcher.mjs"
ALLOWLIST = GENERATOR_DIR / "sealed-node-allowlist.json"
GENERATOR_ENTRY = "generate.mjs"


def require(condition: object, message: str) -> None:
    if not condition:
        raise SystemExit(f"locale-catalog generator: {message}")


def sha256_hex(data: bytes) -> str:
    return hashlib.sha256(data).hexdigest()


def sealed_node_argv(root: Path, entry: str, arguments: list[str]) -> list[str]:
    """The one sanctioned way to start a governed Node entry point.

    Every authoritative path -- this generator, the catalog validator, the
    serving gate, the container build, the offline build and npm's own
    `prebuild` -- goes through the sealed launcher with the entry module named
    by file name, never by a direct path to a generator module.
    """
    launcher = (root / "frontend" / "scripts" / "locale-catalog" / "sealed-node-launcher.mjs").resolve()
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
    for required in (LAUNCHER, ALLOWLIST, GENERATOR_DIR / GENERATOR_ENTRY):
        require(required in sources, f"the generator directory is missing {required.name}")
    return sources


def check_allowlist_covers(sources: list[Path]) -> None:
    """Every governed generator file must be in the launcher's allowlist.

    The launcher refuses to import anything that is not listed, so a file left
    out could never load anyway -- but an *unlisted* file in the directory is
    still hashed into provenance, and leaving it unenforced would be a silent
    inconsistency between what provenance describes and what may run.
    """
    declared = json.loads(ALLOWLIST.read_text(encoding="utf-8"))
    require(isinstance(declared, dict) and declared, "the Node module allowlist is empty")
    expected = {
        path.name: sha256_hex(path.read_bytes())
        for path in sources
        if path not in (LAUNCHER, ALLOWLIST)
    }
    require(
        declared == expected,
        "the Node module allowlist does not describe the generator directory exactly; "
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
    check_allowlist_covers(sources)
    before = closure_identity(sources, node)

    result = subprocess.run(
        [node, *sealed_node_argv(ROOT, GENERATOR_ENTRY, sys.argv[1:])],
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
