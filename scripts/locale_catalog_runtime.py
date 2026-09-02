"""Sealed launcher for the locale-catalog Python command boundary.

The trusted host is responsible for the checked-out repository, the exact
mise-managed CPython binary, and the hash-locked `uv` artifact download. This
bootstrap enforces the repository side of that boundary: isolated interpreter
flags, the portable stdlib identity, no bytecode/source-tree shadowing,
declared executable AST capabilities, and the complete installed wheel RECORD
set before it installs either repository or third-party import root.
"""

from __future__ import annotations

import base64
import csv
import hashlib
import importlib.metadata
import json
from pathlib import Path
import runpy
import sys

ROOT = Path(__file__).resolve().parents[1]
SCRIPTS = ROOT / "scripts"
PROFILE = SCRIPTS / "locale_catalog_python_runtime.json"
LOCK = ROOT / "uv.lock"
VENV = ROOT / ".venv"


def refuse(message: str) -> None:
    raise SystemExit(f"locale-catalog python: {message}")


def verify_interpreter() -> None:
    flags = sys.flags
    if not (
        sys.implementation.name == "cpython"
        and sys.version_info[:3] == (3, 14, 7)
        and sys.implementation.cache_tag == "cpython-314"
        and flags.isolated
        and flags.no_site
        and flags.ignore_environment
        and flags.dont_write_bytecode
        and flags.safe_path
    ):
        refuse("requires CPython 3.14.7 / cpython-314 with -I -S -E -B")
    if sys.prefix != sys.base_prefix:
        refuse("must start from the exact base interpreter, not an inherited virtual environment")
    if PROFILE.is_symlink() or not PROFILE.is_file():
        refuse(f"missing regular runtime profile {PROFILE.relative_to(ROOT)}")
    profile = json.loads(PROFILE.read_text(encoding="utf-8"))
    expected = {
        "implementation": sys.implementation.name,
        "version": ".".join(map(str, sys.version_info[:3])),
        "cacheTag": sys.implementation.cache_tag,
        "stdlibIdentity": "cpython-3.14.7",
    }
    if profile != expected:
        refuse(
            "interpreter or portable stdlib identity differs from "
            f"{PROFILE.relative_to(ROOT)}; reinstall the declared mise runtime"
        )


def verify_source_tree() -> None:
    from locale_catalog_python_boundary import SourceBoundaryError, all_executable_sources

    for path in SCRIPTS.rglob("*"):
        if path.is_symlink():
            refuse(f"trusted scripts tree contains a symlink: {path.relative_to(ROOT)}")
        if path.suffix == ".pyc" or "__pycache__" in path.parts:
            refuse(f"trusted scripts tree contains bytecode: {path.relative_to(ROOT)}")
    try:
        all_executable_sources()
    except SourceBoundaryError as error:
        refuse(str(error))


def normalize_distribution_name(name: str) -> str:
    return name.lower().replace("_", "-").replace(".", "-")


def verify_record(site_packages: Path, distribution: importlib.metadata.Distribution) -> set[Path]:
    record = distribution.read_text("RECORD")
    if record is None:
        refuse(f"{distribution.metadata['Name']} has no wheel RECORD")
    recorded: set[Path] = set()
    for row in csv.reader(record.splitlines()):
        if len(row) != 3 or not row[0]:
            refuse(f"{distribution.metadata['Name']} has malformed RECORD")
        path = (site_packages / row[0]).resolve()
        try:
            path.relative_to(VENV.resolve())
        except ValueError:
            refuse(f"{distribution.metadata['Name']} RECORD escapes the locked virtual environment")
        if path.is_symlink() or not path.is_file():
            refuse(f"{distribution.metadata['Name']} RECORD names non-regular file {row[0]}")
        recorded.add(path)
        if row[1]:
            algorithm, encoded = row[1].split("=", 1)
            if algorithm != "sha256":
                refuse(f"{distribution.metadata['Name']} RECORD uses unsupported hash {algorithm}")
            actual = base64.urlsafe_b64encode(hashlib.sha256(path.read_bytes()).digest()).decode("ascii").rstrip("=")
            if actual != encoded:
                refuse(f"{distribution.metadata['Name']} RECORD hash mismatch for {row[0]}")
    return recorded


def verify_dependencies() -> Path:
    import tomllib

    if LOCK.is_symlink() or not LOCK.is_file():
        refuse("missing regular uv.lock")
    if VENV.is_symlink() or not VENV.is_dir():
        refuse("missing regular .venv created by the locked synchronizer")
    site_packages = VENV / "lib" / "python3.14" / "site-packages"
    if site_packages.is_symlink() or not site_packages.is_dir():
        refuse(f"missing exact dependency root {site_packages.relative_to(ROOT)}")
    lock = tomllib.loads(LOCK.read_text(encoding="utf-8"))
    expected = {
        normalize_distribution_name(package["name"]): package["version"]
        for package in lock.get("package", [])
        if package.get("source", {}).get("registry")
    }
    distributions = {
        normalize_distribution_name(distribution.metadata["Name"]): distribution
        for distribution in importlib.metadata.distributions(path=[str(site_packages)])
    }
    if set(distributions) != set(expected):
        refuse(
            "installed dependency set differs from uv.lock; "
            f"missing {sorted(set(expected) - set(distributions))}, "
            f"unexpected {sorted(set(distributions) - set(expected))}"
        )
    recorded: set[Path] = set()
    for name, distribution in sorted(distributions.items()):
        version = distribution.metadata["Version"]
        if version != expected[name]:
            refuse(f"installed {name} {version} differs from locked {expected[name]}")
        recorded.update(verify_record(site_packages, distribution))
    root = site_packages.resolve()
    actual_files = {
        path.resolve()
        for path in site_packages.rglob("*")
        if path.is_file() and not path.is_symlink()
    }
    virtualenv_bootstrap = {root / "_virtualenv.pth", root / "_virtualenv.py"}
    recorded_site_packages = {path for path in recorded if path.is_relative_to(root)}
    if actual_files != recorded_site_packages | virtualenv_bootstrap:
        extra = sorted(
            str(path.relative_to(root))
            for path in actual_files - recorded_site_packages - virtualenv_bootstrap
        )
        missing = sorted(str(path.relative_to(root)) for path in recorded_site_packages - actual_files)
        refuse(f"installed dependency RECORD set mismatch; extra {extra}, missing {missing}")
    if any((site_packages / name).exists() for name in ("sitecustomize.py", "usercustomize.py")):
        refuse("site-packages must not contain a startup customization module")
    for path in site_packages.rglob("*"):
        if path.is_symlink():
            refuse(f"site-packages contains a symlink: {path.relative_to(site_packages)}")
        if path.suffix == ".pyc" or "__pycache__" in path.parts:
            refuse(f"site-packages contains bytecode: {path.relative_to(site_packages)}")
    return site_packages


def main() -> None:
    if len(sys.argv) < 2:
        refuse("expected a declared Python entry point")
    target = sys.argv[1]
    arguments = sys.argv[2:]
    verify_interpreter()

    # This is the only sanctioned path mutation: both roots are verified above,
    # and runpy receives an entry point from the boundary's fixed allowlist.
    sys.path.insert(0, str(SCRIPTS))
    verify_source_tree()
    site_packages = verify_dependencies()
    sys.path.insert(1, str(site_packages))

    from locale_catalog_python_boundary import ENTRY_POINTS, SourceBoundaryError, scan_python_closure

    if target not in ENTRY_POINTS:
        refuse(f"{target!r} is not a declared Python entry point")
    try:
        scan_python_closure(target)
    except SourceBoundaryError as error:
        refuse(str(error))
    target_path = ROOT / target
    sys.argv = [str(target_path), *arguments]
    runpy.run_path(str(target_path), run_name="__main__")


if __name__ == "__main__":
    main()
