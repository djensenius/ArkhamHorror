#!/usr/bin/env python3
"""Generate the public locale catalog with the sealed Node authority.

The generator itself is JavaScript, so this entry point is where the Node side
of the boundary is bound. Under T1 -- hostile or mistaken *committed* source --
the question is whether the bytes Node executes are exactly the committed,
provenance-hashed closure, and whether that module graph can reach anything
outside it. So before Node starts, this module:

* resolves every static `import`/`export ... from` specifier in the generator
  directory and requires each to be a `node:` builtin, a file inside that same
  directory, or a package `frontend/package-lock.json` pins;
* requires every dynamic `import(...)` to be one of the exact call texts
  declared below, so a new module-graph edge cannot appear silently;
* hashes the complete closure -- every generator source, `package.json`,
  `package-lock.json` and the bound Node binary itself.

After Node returns, the same closure is hashed again and any drift fails the
command. That detects a stable-run change (a generator that rewrites its own
sources, or a mid-run edit on a quiet host); it does *not* prevent a concurrent
same-UID process from swapping those bytes between the check and Node's read.
That is T2, which this boundary does not claim -- CI runs each governed command
in an isolated ephemeral job. See docs/locale-catalog.md.

The generator hashes the same directory into the catalog's own `provenance`
record, so the bytes validated here are the bytes provenance describes.
"""

from __future__ import annotations

import hashlib
import json
from pathlib import Path
import re
import subprocess
import sys

import strict_json

ROOT = Path(__file__).resolve().parents[1]
FRONTEND = ROOT / "frontend"
GENERATOR_DIR = FRONTEND / "scripts" / "locale-catalog"
GENERATOR = GENERATOR_DIR / "generate.mjs"
PACKAGE_JSON = FRONTEND / "package.json"
PACKAGE_LOCK = FRONTEND / "package-lock.json"

STATIC_SPECIFIER = re.compile(
    r"^(?:import|export)\s[^\n]*?\sfrom\s+['\"]([^'\"]+)['\"]|^import\s+['\"]([^'\"]+)['\"]",
    re.MULTILINE,
)
DYNAMIC_IMPORT = re.compile(r"\bimport\s*\(")

# Every dynamic module-graph edge the committed generator is allowed to have,
# by exact call text. `vite` is a locked dependency; the last entry is the
# generator importing the bundle it just built inside its own output directory.
# Anything else -- including a changed spelling of these -- is refused.
ALLOWED_DYNAMIC_IMPORTS: dict[str, tuple[str, ...]] = {
    "sources.mjs": (
        "import('vite')",
        "import('vite')",
        "import(pathToFileURL(join(outDir, 'entry.mjs')).href)",
    ),
}


def require(condition: object, message: str) -> None:
    if not condition:
        raise SystemExit(f"locale-catalog generator: {message}")


def sha256_hex(data: bytes) -> str:
    return hashlib.sha256(data).hexdigest()


def locked_packages() -> set[str]:
    require(
        not PACKAGE_LOCK.is_symlink() and PACKAGE_LOCK.is_file(),
        "frontend/package-lock.json is not a regular file",
    )
    document = json.loads(PACKAGE_LOCK.read_text(encoding="utf-8"))
    packages = document.get("packages")
    require(isinstance(packages, dict) and packages, "frontend/package-lock.json pins no packages")
    names = set()
    marker = "node_modules/"
    for key in packages:
        index = key.rfind(marker)
        if index >= 0:
            names.add(key[index + len(marker) :])
    require(names, "frontend/package-lock.json declares no installed package names")
    return names


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
    require(GENERATOR in sources, "the locale-catalog generator entry module is missing")
    return sources


def dynamic_import_calls(text: str, name: str) -> list[str]:
    """Every `import(...)` call in a source, with its full balanced argument.

    A regex cannot stop at the right parenthesis, and stopping at the wrong one
    would compare a truncated call against the declaration -- so the argument is
    scanned with an explicit depth count and an unbalanced call is refused.
    """
    calls = []
    for match in DYNAMIC_IMPORT.finditer(text):
        start = match.end()
        index = start
        depth = 1
        while index < len(text) and depth > 0:
            character = text[index]
            if character == "(":
                depth += 1
            elif character == ")":
                depth -= 1
            index += 1
        require(depth == 0, f"{name} contains an unbalanced dynamic import call")
        calls.append(f"import({text[start : index - 1].strip()})")
    return calls


def package_name_of(specifier: str) -> str:
    if specifier.startswith("@"):
        return "/".join(specifier.split("/")[:2])
    return specifier.split("/", 1)[0]


def check_module_closure(sources: list[Path], packages: set[str]) -> None:
    """Every edge out of the generator must land inside the hashed closure."""
    generator_root = GENERATOR_DIR.resolve()
    for path in sources:
        if path.suffix != ".mjs":
            continue
        text = path.read_text(encoding="utf-8")
        for match in STATIC_SPECIFIER.finditer(text):
            specifier = match.group(1) or match.group(2)
            if specifier.startswith("node:"):
                continue
            if specifier.startswith("."):
                resolved = (path.parent / specifier).resolve()
                require(
                    resolved.parent == generator_root
                    and not resolved.is_symlink()
                    and resolved.is_file(),
                    f"{path.name} imports {specifier!r}, which leaves the hashed generator "
                    "closure",
                )
                continue
            require(
                package_name_of(specifier) in packages,
                f"{path.name} imports {specifier!r}, which frontend/package-lock.json does not "
                "pin",
            )
        dynamic = dynamic_import_calls(text, path.name)
        expected = list(ALLOWED_DYNAMIC_IMPORTS.get(path.name, ()))
        require(
            sorted(dynamic) == sorted(expected),
            f"{path.name} declares dynamic imports {sorted(dynamic)}, but the committed "
            f"generator closure allows exactly {sorted(expected)}",
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
    check_module_closure(sources, locked_packages())
    before = closure_identity(sources, node)

    # Node compares `process.argv[1]` against `fileURLToPath(import.meta.url)`
    # to decide whether it is the entry module, and it resolves the latter to a
    # real path. The sealed launcher runs governed targets from a checked
    # snapshot whose `frontend` is a symlink, so handing Node the unresolved
    # path made the generator load and then silently do nothing. Give Node the
    # resolved path -- the same bytes this closure just hashed -- so the
    # generation entry point actually generates.
    result = subprocess.run(
        [node, str(GENERATOR.resolve()), *sys.argv[1:]],
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
