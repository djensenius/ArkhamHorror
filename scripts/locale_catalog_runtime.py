"""Sealed bootstrap for the locale-catalog Python command boundary.

`scripts/run-locale-catalog-python.sh` supplies the two authoritative inputs --
an explicit, canonical, non-symlink toolchain root and this invocation's own
workspace -- after discarding the caller's entire environment. This module
enforces the repository side of that boundary, in order:

1. **Environment attestation.** The interpreter must be the exact binary inside
   the sealed toolchain root, started with `-I -S -E -B`, running from its own
   base prefix, and its stdlib must match `locale_catalog_python_runtime.json`
   byte for byte. That lock is platform-independent on purpose: every stdlib
   source it pins was verified to be identical in the pinned CPython 3.14.7
   upstream tarball and in the `python-build-standalone` builds recorded in the
   lock's `distribution` block.
2. **Trusted git.** The one governed step that may consult git receives an
   absolute, non-symlink executable through `LOCALE_CATALOG_GIT`; nothing here
   ever resolves `git` through `PATH`.
3. **Source boundary.** Every declared repository Python source is parsed and
   capability-checked before any of it is imported.
4. **Import-closure attestation.** Every stdlib module the capability boundary
   lets a governed source reach, and everything those modules import in turn,
   must resolve to a byte-attested `.py` source -- never to a file-backed
   extension module. Anything that resolves to no file at all can only come
   from the pinned interpreter binary itself (a builtin or frozen module),
   whose distribution is pinned by checksum in the same lock. That is what
   makes "every imported stdlib byte is bound to committed metadata" a closure
   property rather than a claim about `.py` files alone.
5. **Dependency boundary.** The invocation-owned virtual environment must
   contain exactly the `uv.lock` distributions, with every wheel `RECORD` hash
   reproduced and no unrecorded, symlinked, or bytecode file.

Attestation is deliberately kept *out* of generated content: nothing measured
here is mixed into a catalog manifest, provenance digest, or contract revision,
so governed output stays byte-identical across hosts.
"""

from __future__ import annotations

import ast
import base64
import csv
import hashlib
import json
import os
from pathlib import Path
import sys

ROOT = Path(__file__).resolve().parents[1]
SCRIPTS = ROOT / "scripts"
PROFILE = SCRIPTS / "locale_catalog_python_runtime.json"
LOCK = ROOT / "uv.lock"
WORKSPACE_PREFIX = ".locale-catalog-python."
VENV_NAME = "venv"

# Suffixes CPython's path finder loads as a compiled extension module. An
# extension is bytes this repository can neither read as source nor pin
# portably, so no module the governed closure can reach is allowed to resolve
# to one.
EXTENSION_SUFFIXES = (".so", ".dylib", ".pyd")
# The sealed shell's fixed second-stage runner imports this module only after
# this bootstrap has validated the selected target.  Keep that runner's
# stdlib dependency in the closure proof as well; it is authority-bearing
# launcher source, not a governed Python target.
SEALED_RUNNER_STDLIB_IMPORTS = frozenset({"runpy"})


def refuse(message: str) -> None:
    raise SystemExit(f"locale-catalog python: {message}")


def require_sealed_executable(path: Path, what: str, *, sealed_root: Path | None = None) -> None:
    if path.is_symlink() or not path.is_file():
        refuse(f"{what} {path} is not a regular non-symlink file")
    if not path.stat().st_mode & 0o111:
        refuse(f"{what} {path} is not executable")
    if sealed_root is not None:
        try:
            path.relative_to(sealed_root)
        except ValueError:
            refuse(f"{what} {path} is outside the sealed toolchain root {sealed_root}")
        if path.parent.resolve() != path.parent:
            refuse(f"{what} {path} traverses a symlinked toolchain directory")


def runtime_platform() -> str:
    machine = os.uname().machine
    if sys.platform == "darwin" and machine == "arm64":
        return "darwin-arm64"
    if sys.platform == "linux" and machine == "x86_64":
        return "linux-x86_64"
    refuse(f"unsupported toolchain platform {sys.platform}/{machine}; no exact binary identity is declared")


def verify_binary_digest(profile_item: object, binary: Path, what: str) -> None:
    if not isinstance(profile_item, dict):
        refuse(f"{PROFILE.relative_to(ROOT)} has no complete identity for {what}")
    digests = profile_item.get("binarySha256")
    if not isinstance(digests, dict):
        refuse(f"{PROFILE.relative_to(ROOT)} has no platform digest table for {what}")
    expected = digests.get(runtime_platform())
    if not (
        isinstance(expected, list)
        and expected
        and all(
            isinstance(digest, str)
            and len(digest) == 64
            and all(character in "0123456789abcdef" for character in digest)
            for digest in expected
        )
    ):
        refuse(f"{PROFILE.relative_to(ROOT)} has no valid digest identity for {what} on this platform")
    actual = hashlib.sha256(binary.read_bytes()).hexdigest()
    if actual not in expected:
        refuse(f"{what} {binary} does not match a declared SHA-256 identity for this platform")


def read_sealed_root() -> Path:
    raw = os.environ.get("LOCALE_CATALOG_MISE_ROOT")
    if not raw:
        refuse("LOCALE_CATALOG_MISE_ROOT is required for authoritative commands")
    root = Path(raw)
    if not root.is_absolute():
        refuse(f"LOCALE_CATALOG_MISE_ROOT {raw!r} must be an absolute path")
    if root.is_symlink() or not root.is_dir():
        refuse(f"LOCALE_CATALOG_MISE_ROOT {raw!r} must be an existing non-symlink directory")
    if root.resolve() != root:
        refuse(f"LOCALE_CATALOG_MISE_ROOT {raw!r} must already be canonical")
    return root


def read_profile() -> dict:
    if PROFILE.is_symlink() or not PROFILE.is_file():
        refuse(f"missing regular toolchain lock {PROFILE.relative_to(ROOT)}")
    profile = json.loads(PROFILE.read_text(encoding="utf-8"))
    if not isinstance(profile, dict):
        refuse(f"{PROFILE.relative_to(ROOT)} is not a JSON object")
    return profile


def verify_interpreter(profile: dict, runtime_home: Path) -> None:
    if not (
        sys.implementation.name == profile["implementation"]
        and ".".join(map(str, sys.version_info[:3])) == profile["version"]
        and sys.implementation.cache_tag == profile["cacheTag"]
        and sys.flags.isolated
        and sys.flags.no_site
        and sys.flags.ignore_environment
        and sys.flags.dont_write_bytecode
        and sys.flags.safe_path
    ):
        refuse(
            f"requires {profile['implementation']} {profile['version']} / "
            f"{profile['cacheTag']} started with -I -S -E -B"
        )
    if sys.prefix != sys.base_prefix:
        refuse("must start from the exact base interpreter, not an inherited virtual environment")
    interpreter = profile["interpreter"]
    install = runtime_home
    binary = runtime_home / "bin" / "python3.14"
    if Path(sys.base_prefix) != install or Path(sys.base_exec_prefix) != install:
        refuse(
            f"interpreter base prefix {sys.base_prefix} is not the sealed toolchain "
            f"interpreter {install}"
        )
    if Path(sys.executable) != binary:
        refuse(f"running interpreter {sys.executable} is not the sealed binary {binary}")
    if Path(sys._base_executable) != binary:
        refuse(f"base interpreter {sys._base_executable} is not the sealed binary {binary}")
    require_sealed_executable(binary, "sealed interpreter", sealed_root=runtime_home)
    verify_binary_digest(interpreter, binary, "sealed CPython 3.14.7")


def verify_pycache_prefix() -> None:
    raw = os.environ.get("ARKHAM_LOCALE_CATALOG_PYCACHE_PREFIX")
    if not raw or sys.pycache_prefix != raw:
        refuse("Python must use this invocation's empty explicit pycache prefix")
    prefix = Path(raw)
    if (
        prefix.name != "pycache"
        or prefix.parent.parent != ROOT
        or not prefix.parent.name.startswith(WORKSPACE_PREFIX)
        or prefix.is_symlink()
        or not prefix.is_dir()
        or prefix.resolve() != prefix
    ):
        refuse("Python bytecode cache is not this invocation's own repository-owned workspace")
    if any(prefix.iterdir()):
        refuse("this invocation's Python bytecode cache is not empty")


def read_runtime_home() -> Path:
    raw = os.environ.get("ARKHAM_LOCALE_CATALOG_RUNTIME_HOME")
    if not raw:
        refuse("missing invocation-owned copied CPython runtime")
    runtime = Path(raw)
    if (
        runtime.name != "runtime"
        or runtime.parent.parent != ROOT
        or not runtime.parent.name.startswith(WORKSPACE_PREFIX)
        or runtime.is_symlink()
        or not runtime.is_dir()
        or runtime.resolve() != runtime
    ):
        refuse("copied CPython runtime is not this invocation's own repository-owned workspace")
    return runtime


def verify_stdlib(
    profile: dict, runtime_home: Path
) -> tuple[Path, set[str], set[str], dict[str, str]]:
    """Prove the interpreter's stdlib is exactly the pinned distribution's.

    This is a *closure* proof, not a sampling one: every `.py` file under the
    sealed stdlib root must either be a committed table entry whose bytes hash
    to the recorded digest, or one of the handful of exactly-named
    build-configuration modules the lock declares as platform variants -- and
    those are unreachable, because neither `sysconfig` nor any module that
    would pull them in is an importable module for a governed source. An
    injected or edited stdlib module therefore fails here before a single
    governed byte is read, whether or not anything imports it.

    Nothing can be introduced ahead of the sealed stdlib either: `sys.path`
    only ever gains the repository `scripts/` directory (whose contents must
    exactly equal the boundary's declared executable-source set) and the
    RECORD-verified dependency root.

    Returns the stdlib root, the set of resolvable source paths (relative,
    POSIX, including the declared platform variants), the variant subset, and
    an index of every file-backed extension module found under it, keyed by
    `"<directory>/<module name>"`, for `verify_stdlib_import_closure`.
    """
    interpreter = profile["interpreter"]
    stdlib_root = runtime_home / "lib" / "python3.14"
    if stdlib_root.is_symlink() or not stdlib_root.is_dir():
        refuse(f"sealed stdlib root {stdlib_root} is not a regular directory")
    modules = profile["stdlibModules"]
    variants = set(profile["platformVariantModules"])
    source_tree_digest = profile.get("stdlibSourceTreeSha256")
    if not isinstance(modules, dict) or len(modules) != profile["stdlibModuleCount"]:
        refuse("toolchain lock stdlibModules does not match its declared stdlibModuleCount")
    if source_tree_digest != "c618cf3f74e4625201ed9d508f280b256235370e949172500c02d2da662d53e5":
        refuse("toolchain lock does not carry the declared complete stdlib source-tree identity")

    extensions: dict[str, str] = {}
    for entry in stdlib_root.rglob("*"):
        if entry.is_symlink():
            refuse(
                f"sealed stdlib contains a symlink: {entry.relative_to(stdlib_root).as_posix()}"
            )
        if entry.name == "__pycache__" or entry.suffix == ".pyc":
            refuse(
                f"copied sealed stdlib contains bytecode: "
                f"{entry.relative_to(stdlib_root).as_posix()}"
            )
        if entry.is_file() and entry.name.endswith(EXTENSION_SUFFIXES):
            relative_path = entry.relative_to(stdlib_root).as_posix()
            directory, _, filename = relative_path.rpartition("/")
            key = f"{directory}/{filename.split('.')[0]}" if directory else filename.split(".")[0]
            extensions[key] = relative_path

    present = set()
    for source in stdlib_root.rglob("*.py"):
        relative_path = source.relative_to(stdlib_root).as_posix()
        parts = relative_path.split("/")
        if "__pycache__" in parts or parts[0] == "site-packages":
            continue
        if source.is_symlink() or not source.is_file():
            refuse(f"sealed stdlib source {relative_path} is not a regular file")
        if relative_path in variants:
            present.add(relative_path)
            continue
        expected = modules.get(relative_path)
        if expected is None:
            refuse(
                f"sealed stdlib contains unattested module {relative_path}; the committed "
                f"toolchain lock {PROFILE.relative_to(ROOT)} does not describe it"
            )
        if hashlib.sha256(source.read_bytes()).hexdigest() != expected:
            refuse(
                f"sealed stdlib source {relative_path} does not match the committed toolchain "
                f"lock {PROFILE.relative_to(ROOT)}"
            )
        present.add(relative_path)
    expected_paths = set(modules)
    actual_paths = present - variants
    if actual_paths != expected_paths:
        refuse(
            "sealed stdlib source inventory differs from the committed toolchain lock; "
            f"missing {sorted(expected_paths - actual_paths)}, "
            f"unexpected {sorted(actual_paths - expected_paths)}"
        )
    if not present:
        refuse(f"sealed stdlib root {stdlib_root} contains no attested module")
    return stdlib_root, present, variants, extensions


def _imported_module_names(tree: ast.AST, package: str) -> set[str]:
    """Every module name a source could import, over-approximated on purpose.

    Conditional, lazy and inside-function imports all count, every parent
    package of a dotted import counts, and a `from X import y` contributes both
    `X` and `X.y` because `y` may itself be a submodule. Over-approximating can
    only add candidates to the closure, so it can never let a reachable
    extension module escape the check below.
    """
    names: set[str] = set()
    for node in ast.walk(tree):
        if isinstance(node, ast.Import):
            for alias in node.names:
                segments = alias.name.split(".")
                for index in range(1, len(segments) + 1):
                    names.add(".".join(segments[:index]))
        elif isinstance(node, ast.ImportFrom):
            if node.level:
                owner = package.split(".") if package else []
                if node.level - 1 > len(owner):
                    continue
                owner = owner[: len(owner) - (node.level - 1)] if node.level > 1 else owner
                module = ".".join([*owner, node.module]) if node.module else ".".join(owner)
            else:
                module = node.module or ""
            if not module:
                continue
            segments = module.split(".")
            for index in range(1, len(segments) + 1):
                names.add(".".join(segments[:index]))
            for alias in node.names:
                if alias.name != "*":
                    names.add(f"{module}.{alias.name}")
    return names


def verify_stdlib_import_closure(
    profile: dict,
    stdlib_root: Path,
    attested: set[str],
    variants: set[str],
    extensions: dict[str, str],
) -> None:
    """Refuse if anything the governed closure can import is unbound bytes.

    The seeds are exactly the stdlib modules the capability boundary lets a
    governed source name -- the shared allowlist plus every per-source
    sensitive import -- and the walk follows the *attested* sources of those
    modules transitively. A name that resolves to an attested `.py` is bound by
    its committed digest. A name that resolves to nothing under the sealed
    stdlib can only be satisfied by the interpreter binary itself (a builtin or
    frozen module), which is the pinned, checksum-recorded distribution. Two
    resolutions are refused: a file-backed extension module, whose bytes are
    platform-specific and cannot be pinned by the portable lock, and a declared
    platform-variant module, which the lock exempts from hashing precisely
    because nothing is supposed to be able to reach it.
    """
    from locale_catalog_python_boundary import (
        ALLOWED_IMPORTS,
        LOCAL_MODULE_SOURCES,
        SOURCE_SENSITIVE_IMPORTS,
    )

    seeds = set(ALLOWED_IMPORTS)
    seeds |= SEALED_RUNNER_STDLIB_IMPORTS
    for extra in SOURCE_SENSITIVE_IMPORTS.values():
        seeds |= set(extra)
    # In-tree modules are scanned by the capability boundary, dependency
    # distributions by their wheel RECORDs, and `__future__` is a compiler
    # directive; only stdlib names are resolved here.
    seeds -= set(LOCAL_MODULE_SOURCES)
    seeds -= {"jsonschema", "jsonschema.validators", "referencing", "tree_sitter", "tree_sitter_haskell", "yaml"}

    def resolve(name: str) -> tuple[str, str] | None:
        """Resolve `name` the way CPython's path finder would.

        Order matters and is not the obvious one: a directory wins over a file,
        but *within* either case the extension loader is tried before the
        source loader. A planted `csv.so` therefore shadows the attested
        `csv.py`, which is exactly why this walk resolves extensions first
        instead of stopping at the first attested source it can find.
        """
        relative = name.replace(".", "/")
        package_source = f"{relative}/__init__.py"
        package_extension = f"{relative}/__init__"
        if package_source in attested or package_extension in extensions:
            if package_extension in extensions:
                return ("extension", extensions[package_extension])
            return ("variant" if package_source in variants else "source", package_source)
        leaf = relative.rpartition("/")[2]
        for key in (relative, f"lib-dynload/{leaf}"):
            if key in extensions:
                return ("extension", extensions[key])
        module_source = f"{relative}.py"
        if module_source in attested:
            return ("variant" if module_source in variants else "source", module_source)
        return None

    seen: set[str] = set()
    sources = 0
    pending = sorted(seeds)
    while pending:
        name = pending.pop()
        if name in seen:
            continue
        seen.add(name)
        resolved = resolve(name)
        if resolved is None:
            continue
        kind, relative_path = resolved
        if kind == "extension":
            refuse(
                f"the governed import closure reaches {name!r}, which the sealed stdlib "
                f"satisfies with the compiled extension {relative_path}; the committed "
                f"toolchain lock {PROFILE.relative_to(ROOT)} can only bind source bytes"
            )
        if kind == "variant":
            refuse(
                f"the governed import closure reaches {name!r}, which the committed toolchain "
                f"lock {PROFILE.relative_to(ROOT)} exempts from hashing as the platform variant "
                f"{relative_path}; a reachable module must be attested"
            )
        sources += 1
        source = stdlib_root / relative_path
        try:
            tree = ast.parse(source.read_bytes(), filename=relative_path)
        except SyntaxError as error:
            refuse(f"attested stdlib source {relative_path} does not parse: {error}")
        package = name if relative_path.endswith("/__init__.py") else name.rpartition(".")[0]
        pending.extend(sorted(_imported_module_names(tree, package) - seen))

    minimum = profile["stdlibImportClosureMinimumSources"]
    if not isinstance(minimum, int) or minimum <= 0:
        refuse(f"{PROFILE.relative_to(ROOT)} does not declare a positive import-closure floor")
    if sources < minimum:
        refuse(
            f"the governed stdlib import closure resolved only {sources} attested sources, "
            f"below the committed floor of {minimum}; the walk is not proving what it claims"
        )


def verify_trusted_git() -> None:
    raw = os.environ.get("LOCALE_CATALOG_GIT")
    if not raw:
        refuse("LOCALE_CATALOG_GIT is required; governed commands never resolve git through PATH")
    git = Path(raw)
    if not git.is_absolute():
        refuse(f"LOCALE_CATALOG_GIT {raw!r} must be an absolute path")
    require_sealed_executable(git, "trusted git")


def verify_trusted_node(profile: dict, sealed_root: Path) -> None:
    tools = profile.get("externalTools")
    if not isinstance(tools, dict) or set(tools) != {"node"}:
        refuse(f"{PROFILE.relative_to(ROOT)} must declare exactly the bound external tool node")
    node = tools["node"]
    if (
        not isinstance(node, dict)
        or set(node) != {"version", "binaryRelativePath", "binarySha256"}
        or node["version"] != "26.7.0"
        or not isinstance(node["binaryRelativePath"], str)
    ):
        refuse(f"{PROFILE.relative_to(ROOT)} has no complete Node 26.7.0 identity")
    expected = sealed_root / node["binaryRelativePath"]
    raw = os.environ.get("LOCALE_CATALOG_NODE")
    if raw != str(expected):
        refuse(
            "LOCALE_CATALOG_NODE is not the exact Node binary declared by the sealed "
            f"toolchain lock: expected {expected}, got {raw!r}"
        )
    require_sealed_executable(expected, "sealed Node 26.7.0", sealed_root=sealed_root)
    verify_binary_digest(node, expected, "sealed Node 26.7.0")


def verify_trusted_uv(profile: dict, sealed_root: Path) -> None:
    uv = profile.get("uv")
    if (
        not isinstance(uv, dict)
        or set(uv) != {"version", "installRelativePath", "binarySha256"}
        or uv["version"] != "0.12.6"
        or not isinstance(uv["installRelativePath"], str)
    ):
        refuse(f"{PROFILE.relative_to(ROOT)} has no complete uv 0.12.6 identity")
    candidates = sorted((sealed_root / uv["installRelativePath"]).glob("uv-*/uv"))
    if len(candidates) != 1:
        refuse(f"the sealed toolchain has {len(candidates)} uv 0.12.6 candidates, expected one")
    candidate = candidates[0]
    if os.environ.get("LOCALE_CATALOG_UV") != str(candidate):
        refuse("LOCALE_CATALOG_UV is not the exact uv binary declared by the sealed toolchain lock")
    require_sealed_executable(candidate, "sealed uv 0.12.6", sealed_root=sealed_root)
    verify_binary_digest(uv, candidate, "sealed uv 0.12.6")


def verify_explicit_stack() -> None:
    """The backend probe's only non-mise authority is explicit, never PATH-found."""
    raw = os.environ.get("LOCALE_CATALOG_STACK")
    if not raw:
        return
    stack = Path(raw)
    if not stack.is_absolute():
        refuse(f"LOCALE_CATALOG_STACK {raw!r} must be an absolute path")
    require_sealed_executable(stack, "explicitly bound stack")


def verify_source_tree() -> None:
    from locale_catalog_python_boundary import SourceBoundaryError, all_executable_sources

    for path in SCRIPTS.rglob("*"):
        if path.is_symlink():
            refuse(f"trusted scripts tree contains a symlink: {path.relative_to(ROOT)}")
        if path.is_dir():
            refuse(
                f"trusted scripts tree contains an undeclared nested directory: "
                f"{path.relative_to(ROOT)}"
            )
        if path.suffix == ".pyc" or "__pycache__" in path.parts:
            refuse(f"trusted scripts tree contains bytecode: {path.relative_to(ROOT)}")
    try:
        all_executable_sources()
    except SourceBoundaryError as error:
        refuse(str(error))


def normalize_distribution_name(name: str) -> str:
    return name.lower().replace("_", "-").replace(".", "-")


def distribution_metadata(metadata_dir: Path) -> tuple[str, str]:
    metadata = metadata_dir / "METADATA"
    if metadata.is_symlink() or not metadata.is_file():
        refuse(f"dependency metadata directory {metadata_dir.name} has no regular METADATA file")
    try:
        lines = metadata.read_text(encoding="utf-8").splitlines()
    except UnicodeDecodeError as error:
        refuse(f"dependency metadata directory {metadata_dir.name} is not strict UTF-8: {error}")
    values: dict[str, str] = {}
    for field in ("Name", "Version"):
        matches = [line[len(field) + 1 :] for line in lines if line.startswith(f"{field}:")]
        if len(matches) != 1 or not matches[0].strip():
            refuse(f"dependency metadata directory {metadata_dir.name} has no unique {field} field")
        values[field] = matches[0].strip()
    return values["Name"], values["Version"]


def verify_record(venv: Path, site_packages: Path, name: str, metadata_dir: Path) -> set[Path]:
    record_path = metadata_dir / "RECORD"
    if record_path.is_symlink() or not record_path.is_file():
        refuse(f"{name} has no regular wheel RECORD")
    try:
        record = record_path.read_text(encoding="utf-8")
    except UnicodeDecodeError as error:
        refuse(f"{name} has a non-UTF-8 wheel RECORD: {error}")
    recorded: set[Path] = set()
    for row in csv.reader(record.splitlines()):
        if len(row) != 3 or not row[0]:
            refuse(f"{name} has malformed RECORD")
        path = (site_packages / row[0]).resolve()
        try:
            path.relative_to(venv.resolve())
        except ValueError:
            refuse(f"{name} RECORD escapes the locked virtual environment")
        if path.is_symlink() or not path.is_file():
            refuse(f"{name} RECORD names non-regular file {row[0]}")
        recorded.add(path)
        if row[1]:
            algorithm, encoded = row[1].split("=", 1)
            if algorithm != "sha256":
                refuse(f"{name} RECORD uses unsupported hash {algorithm}")
            actual = base64.urlsafe_b64encode(hashlib.sha256(path.read_bytes()).digest()).decode("ascii").rstrip("=")
            if actual != encoded:
                refuse(f"{name} RECORD hash mismatch for {row[0]}")
    return recorded


def verify_dependencies() -> Path:
    import tomllib

    if LOCK.is_symlink() or not LOCK.is_file():
        refuse("missing regular uv.lock")
    raw_venv = os.environ.get("ARKHAM_LOCALE_CATALOG_PYTHON_VENV")
    if not raw_venv:
        refuse("missing invocation-owned virtual environment")
    venv = Path(raw_venv)
    if (
        venv.name != VENV_NAME
        or venv.parent.parent != ROOT
        or not venv.parent.name.startswith(WORKSPACE_PREFIX)
        or venv.is_symlink()
        or not venv.is_dir()
        or venv.resolve() != venv
    ):
        refuse("virtual environment is not this invocation's own repository-owned workspace")
    site_packages = venv / "lib" / "python3.14" / "site-packages"
    if site_packages.is_symlink() or not site_packages.is_dir():
        refuse(f"missing exact dependency root {site_packages.relative_to(ROOT)}")
    lock = tomllib.loads(LOCK.read_text(encoding="utf-8"))
    expected = {
        normalize_distribution_name(package["name"]): package["version"]
        for package in lock.get("package", [])
        if package.get("source", {}).get("registry")
    }
    distributions: dict[str, tuple[str, Path]] = {}
    for metadata_dir in site_packages.glob("*.dist-info"):
        if metadata_dir.is_symlink() or not metadata_dir.is_dir():
            refuse(f"dependency metadata entry {metadata_dir.name} is not a regular directory")
        name, version = distribution_metadata(metadata_dir)
        normalized = normalize_distribution_name(name)
        if normalized in distributions:
            refuse(f"dependency metadata has duplicate distribution {normalized}")
        distributions[normalized] = (version, metadata_dir)
    if set(distributions) != set(expected):
        refuse(
            "installed dependency set differs from uv.lock; "
            f"missing {sorted(set(expected) - set(distributions))}, "
            f"unexpected {sorted(set(distributions) - set(expected))}"
        )
    recorded: set[Path] = set()
    for name, (version, metadata_dir) in sorted(distributions.items()):
        if version != expected[name]:
            refuse(f"installed {name} {version} differs from locked {expected[name]}")
        recorded.update(verify_record(venv, site_packages, name, metadata_dir))
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
    sealed_root = read_sealed_root()
    runtime_home = read_runtime_home()
    profile = read_profile()
    verify_interpreter(profile, runtime_home)
    verify_pycache_prefix()
    stdlib_root, attested, variants, extensions = verify_stdlib(profile, runtime_home)
    verify_trusted_git()
    verify_trusted_node(profile, sealed_root)
    verify_trusted_uv(profile, sealed_root)
    verify_explicit_stack()

    # The only sanctioned path mutation: both roots are verified above.  The
    # sealed shell then executes this exact checked target in a second clean
    # interpreter, rather than giving any governed source a dynamic loader.
    sys.path.insert(0, str(SCRIPTS))
    verify_source_tree()
    verify_stdlib_import_closure(profile, stdlib_root, attested, variants, extensions)
    site_packages = verify_dependencies()
    sys.path.insert(1, str(site_packages))

    from locale_catalog_python_boundary import ENTRY_POINTS, SourceBoundaryError, scan_python_closure

    if target not in ENTRY_POINTS:
        refuse(f"{target!r} is not a declared Python entry point")
    try:
        scan_python_closure(target)
    except SourceBoundaryError as error:
        refuse(str(error))
    # The sealed shell executes the already-validated target in a second,
    # separately-clean environment. This bootstrap deliberately does not
    # import a target by path or execute dynamic code: `runpy`, importlib
    # loaders, `exec`, and `compile` are all outside the capability boundary.


if __name__ == "__main__":
    main()
