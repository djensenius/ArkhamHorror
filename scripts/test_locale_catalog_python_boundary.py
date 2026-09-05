#!/usr/bin/env python3
"""Adversarial, ownership-safe tests for the sealed locale-catalog Python boundary.

Every probe in this file -- source, schema, dependency, interpreter, alias and
tamper alike -- runs inside an *invocation-owned* temporary copy of the exact
governed tree. Nothing here ever creates, overwrites, truncates, restores or
deletes a path inside the canonical worktree, and `main()` proves that: it
snapshots the canonical paths earlier revisions of this test used to mutate
(including the `locale-catalog-python-dependency-marker` sentinel and the
`scripts/__pycache__` probe directory) and re-checks them byte for byte at the
end.

Ownership is explicit rather than implied. Each probe tree carries an
`.locale-catalog-boundary-owner` sentinel holding a freshly generated token;
cleanup removes a directory only after re-reading that sentinel and confirming
it still holds this invocation's token. The self-test-only mutating mode is
gated on presenting the same token on the command line, and there is no root
override in the production launcher or bootstrap at all -- a probe tree *is*
its own root -- so nothing here can weaken a production check.
"""

from __future__ import annotations

import argparse
import base64
import csv
import hashlib
import json
import os
from pathlib import Path
import shutil
import subprocess
import time
import uuid

import locale_catalog_python_boundary
import strict_json

ROOT = Path(__file__).resolve().parents[1]
LAUNCHER_RELATIVE_PATH = "scripts/run-locale-catalog-python.sh"
BOUNDARY_TEST_RELATIVE_PATH = "scripts/test_locale_catalog_python_boundary.py"
RUNTIME_PROFILE_RELATIVE_PATH = "scripts/locale_catalog_python_runtime.json"
FIXTURE_ENTRY = "scripts/build-locale-catalog-fixture.py"
HELPER_SOURCE = "scripts/json_schema_subset.py"
GRANTED_SOURCE = "scripts/strict_json.py"

# The exact repository subtree every governed Python command reads. A probe
# copy missing one of these fails loudly during setup rather than silently
# proving nothing.
GOVERNED_TREE_PATHS = (
    "scripts",
    "contracts",
    "frontend/schemas",
    "frontend/package.json",
    "mise.toml",
    "pyproject.toml",
    "uv.lock",
    "Dockerfile",
    ".github/workflows",
)

OWNER_SENTINEL = ".locale-catalog-boundary-owner"
PROBE_PREFIX = "locale-catalog-boundary-"
SCRATCH_OWNER_FILE = ".locale-catalog-boundary-scratch-owner"
WORKSPACE_PREFIX = ".locale-catalog-python."
WORKSPACE_OWNER_FILE = "owner"

# Canonical paths earlier revisions of this test created, overwrote, truncated,
# restored or removed by fixed name. They are witnessed here, never written.
CANONICAL_WITNESS_PATHS = (
    "locale-catalog-python-dependency-marker",
    "scripts/__pycache__",
    "scripts/build-locale-catalog-fixture.py",
    "scripts/strict_json.py",
    "scripts/json_schema_subset.py",
    "scripts/locale_catalog_python_boundary.py",
    "scripts/locale_catalog_runtime.py",
    "scripts/locale_catalog_python_runtime.json",
    "scripts/run-locale-catalog-python.sh",
    "frontend/schemas/locale-catalog/v1/manifest.schema.json",
    "contracts/manifest.json",
)

# Every reported bypass and variant the production visitor must reject, in the
# exact shape it was reported. Each is prepended to a real governed source.
BYPASS_SNIPPETS: dict[str, bytes] = {
    "runpy.run_path": b'import runpy\nrunpy.run_path("payload.py")\n',
    "runpy.run_module": b'import runpy\nrunpy.run_module("payload")\n',
    "aliased runpy": b'import runpy as r\nr.run_path("payload.py")\n',
    "subprocess.run": b'import subprocess\nsubprocess.run(["id"])\n',
    "subprocess.Popen": b'import subprocess\nsubprocess.Popen(["id"])\n',
    "subprocess.call": b'import subprocess\nsubprocess.call(["id"])\n',
    "subprocess.check_call": b'import subprocess\nsubprocess.check_call(["id"])\n',
    "subprocess.check_output": b'import subprocess\nsubprocess.check_output(["id"])\n',
    "os.system": b'import os\nos.system("id")\n',
    "os.spawnv": b'import os\nos.spawnv(os.P_WAIT, "/bin/sh", ["sh"])\n',
    "os.execv": b'import os\nos.execv("/bin/sh", ["sh"])\n',
    "os.popen": b'import os\nos.popen("id")\n',
    "importlib.import_module": b'import importlib\nimportlib.import_module("os")\n',
    "importlib loader spec": (
        b"import importlib\n"
        b'spec = importlib.util.spec_from_file_location("m", "m.py")\n'
    ),
    "importlib.metadata entry point load": (
        b"import importlib.metadata\n"
        b"importlib.metadata.entry_points()[0].load()\n"
    ),
    "importlib.metadata entry point alias": (
        b"import importlib.metadata as md\n"
        b"loaded = md.entry_points()\n"
        b"loaded[0].load()\n"
    ),
    "zipimport loader": b'import zipimport\nzipimport.zipimporter("x").load_module("os")\n',
    "ctypes": b'import ctypes\nctypes.CDLL("libc.so.6")\n',
    "pickle.loads": b"import pickle\npickle.loads(b'')\n",
    "marshal.loads": b"import marshal\nmarshal.loads(b'')\n",
    "shelve.open": b'import shelve\nshelve.open("db")\n',
    "builtins exec": b'exec("x = 1")\n',
    "builtins eval": b'eval("1")\n',
    "builtins compile": b'compile("x = 1", "<s>", "exec")\n',
    "builtins __import__": b'__import__("os")\n',
    "aliased builtins exec": b'import builtins as runtime_builtins\nruntime_builtins.exec("x = 1")\n',
    "imported builtins exec": b'from builtins import exec as runtime_exec\nruntime_exec("x = 1")\n',
    "aliased builtins import": b'from builtins import __import__ as runtime_import\nruntime_import("os")\n',
    "getattr sys.path": b'import sys\ngetattr(sys, "path")\n',
    "setattr sys.path": b'import sys\nsetattr(sys, "path", [])\n',
    "vars traversal": b"import sys\nvars(sys)\n",
    "globals traversal": b'globals()["payload"]\n',
    "locals traversal": b"locals()\n",
    "dynamic code object": b'code = compile("x = 1", "<s>", "exec")\nexec(code)\n',
    "aliased sys.path append": b'import sys as runtime_sys\nruntime_sys.path.append("outside")\n',
    "imported sys.modules": b"from sys import modules\nmodules.clear()\n",
    "imported sys.meta_path": b"from sys import meta_path\nmeta_path.clear()\n",
    "imported sys.path_hooks": b"from sys import path_hooks\npath_hooks.clear()\n",
    "pathlib import path mutation": (
        b"import sys\nfrom pathlib import Path\nsys.path.insert(0, str(Path('x')))\n"
    ),
    "sys in list literal": b'import sys\nholder = [sys]\nholder[0].path.append("outside")\n',
    "sys in tuple literal": b"import sys\nholder = (sys,)\nholder[0].modules.clear()\n",
    "sys in set literal": b"import sys\nholder = {sys}\n",
    "sys in dict literal": b'import sys\nholder = {"s": sys}\nholder["s"].path.append("x")\n',
    "sys in comprehension": b'import sys\nholder = [module for module in (sys,)]\nholder[0].path.append("x")\n',
    "sys in dict comprehension": b'import sys\nholder = {"s": module for module in (sys,)}\n',
    "sys through unpacking": b'import sys\nfirst, second = sys, sys\nfirst.path.append("x")\n',
    "sys through starred unpacking": b'import sys\nfirst, *rest = [sys]\nfirst.path.append("x")\n',
    "sys through subscript": b'import sys\nholder = [sys]\nchosen = holder[0]\nchosen.path.append("x")\n',
    "sys through slice": b"import sys\nholder = [sys][0:1]\n",
    "sys through function return": (
        b"import sys\n\n\ndef expose():\n    return sys\n\n\nexpose().path.append('x')\n"
    ),
    "sys through generator yield": (
        b"import sys\n\n\ndef expose():\n    yield sys\n"
    ),
    "sys through yield from": (
        b"import sys\n\n\ndef expose():\n    yield from [sys]\n"
    ),
    "sys through parameter default": (
        b"import sys\n\n\ndef expose(module=sys):\n    return module.path\n"
    ),
    "sys through keyword-only default": (
        b"import sys\n\n\ndef expose(*, module=sys):\n    return module.modules\n"
    ),
    "sys through lambda": b"import sys\nexpose = lambda: sys\n",
    "sys through closure": (
        b"import sys\n\n\ndef outer():\n    captured = sys\n\n    def inner():\n"
        b"        return captured.path\n\n    return inner\n"
    ),
    "sys through global": (
        b"import sys\n\nEXPOSED = None\n\n\ndef expose():\n    global EXPOSED\n"
        b"    EXPOSED = sys\n"
    ),
    "sys through nonlocal": (
        b"import sys\n\n\ndef outer():\n    captured = None\n\n    def inner():\n"
        b"        nonlocal captured\n        captured = sys\n\n    return inner\n"
    ),
    "sys through class field": (
        b"import sys\n\n\nclass Holder:\n    module = sys\n"
    ),
    "sys through instance field": (
        b"import sys\n\n\nclass Holder:\n    def __init__(self):\n        self.module = sys\n"
    ),
    "sys through attribute store": (
        b"import sys\nfrom pathlib import Path\n\nholder = Path('x')\nholder.module = sys\n"
    ),
    "sys through context manager": (
        b"import sys\n\nwith sys as module:\n    module.path.append('x')\n"
    ),
    "sys through for target": b"import sys\n\nfor module in [sys]:\n    module.path.append('x')\n",
    "sys through except alias": (
        b"import sys\n\ntry:\n    pass\nexcept Exception:\n    holder = sys\n"
    ),
    "sys through decorator": (
        b"import sys\n\n\n@sys.audit\ndef decorated():\n    return None\n"
    ),
    "sys through augmented assignment": b"import sys\n\nholder = []\nholder += [sys]\n",
    "sys through walrus": b"import sys\n\nif (holder := sys):\n    holder.path.append('x')\n",
    "sensitive module through another namespace": (
        b'import strict_json\nstrict_json.os.system("id")\n'
    ),
    "sys through another module namespace": (
        b"import strict_json\nstrict_json.sys.path.append('x')\n"
    ),
    "loader dunder": b'__loader__.load_module("os")\n',
    "spec dunder traversal": b"holder = __spec__.origin\n",
    "class dunder traversal": b"holder = ().__class__.__bases__[0]\n",
    "subclasses dunder traversal": b"holder = ().__class__.__subclasses__()\n",
    "builtins dunder traversal": b'holder = __builtins__["payload"]\n',
    "import path mutation via path_hooks": (
        b"import sys\nsys.path_hooks.insert(0, None)\n"
    ),
    "import path importer cache": b"import sys\nsys.path_importer_cache.clear()\n",
    "meta path mutation": b"import sys\nsys.meta_path.insert(0, None)\n",
}

# The subset re-proved end to end through the real authoritative command, so
# the in-process matrix is never the only thing between a bypass and a
# governed byte.
END_TO_END_BYPASSES = (
    "runpy.run_path",
    "subprocess.run",
    "os.system",
    "importlib.metadata entry point load",
    "sys in list literal",
    "getattr sys.path",
    "aliased sys.path append",
    "loader dunder",
)

# A source that already holds a narrow grant must not be able to widen it:
# these run against `strict_json.py`, which is granted `subprocess.run` as a
# call, `os.environ` as a value, and nothing else.
GRANT_OVERREACH_SNIPPETS: dict[str, bytes] = {
    "granted source widening to subprocess.Popen": b'import subprocess\nsubprocess.Popen(["id"])\n',
    "granted source widening to subprocess.check_output": (
        b'import subprocess\nsubprocess.check_output(["id"])\n'
    ),
    "granted source widening to os.system": b'import os\nos.system("id")\n',
    "granted source storing its granted callee": b"import subprocess\nhandler = subprocess.run\n",
    "granted source aliasing its granted callee": (
        b"import subprocess\nfrom pathlib import Path\n\nholder = [subprocess.run]\n"
    ),
    "granted source widening os.environ": b"import os\nos.environ.clear()\n",
    "granted source widening to sys.path": b'import sys\nsys.path.append("x")\n',
    "granted source widening to sys.modules": b"import sys\nsys.modules.clear()\n",
}

SCOPE_BYPASS_SNIPPETS: dict[str, bytes] = {
    "function parameter shadow": (
        b"import subprocess\n\ndef shadow(subprocess):\n    pass\n\nsubprocess.Popen(['id'])\n"
    ),
    "function assignment shadow": (
        b"import subprocess\n\ndef shadow():\n    subprocess = None\n\nsubprocess.Popen(['id'])\n"
    ),
    "conditional assignment shadow": (
        b"import subprocess\nif False:\n    subprocess = None\nsubprocess.Popen(['id'])\n"
    ),
    "class shadow": b"import subprocess\nclass subprocess:\n    pass\nsubprocess.Popen(['id'])\n",
    "exception alias shadow": (
        b"import subprocess\ntry:\n    pass\nexcept Exception as subprocess:\n    pass\nsubprocess.Popen(['id'])\n"
    ),
    "for target shadow": b"import subprocess\nfor subprocess in ():\n    pass\nsubprocess.Popen(['id'])\n",
    "with target shadow": (
        b"import subprocess\nwith object() as subprocess:\n    pass\nsubprocess.Popen(['id'])\n"
    ),
    "comprehension target shadow": (
        b"import subprocess\n[value for subprocess in ()]\nsubprocess.Popen(['id'])\n"
    ),
    "import alias shadow": b"import subprocess\nimport json as subprocess\nsubprocess.Popen(['id'])\n",
}

TAMPER_TARGETS = (
    "scripts/build-locale-catalog-fixture.py",
    "scripts/strict_json.py",
    "scripts/json_schema_subset.py",
    "scripts/locale_catalog_python_boundary.py",
    "scripts/locale_catalog_python_runtime.json",
    "frontend/schemas/locale-catalog/v1/manifest.schema.json",
)

HOSTILE_EXECUTABLES = (
    "python",
    "python3",
    "python3.14",
    "uv",
    "git",
    "env",
    "bash",
    "sh",
    "date",
    "mktemp",
)


class ProbeFailure(SystemExit):
    pass


def require(condition: object, message: str) -> None:
    if not condition:
        raise ProbeFailure(f"locale-catalog python boundary: {message}")


def find_single(root: Path, pattern: str, what: str) -> Path:
    """Exactly-one glob match, with a clear failure instead of StopIteration."""
    matches = sorted(root.glob(pattern))
    require(
        len(matches) == 1,
        f"expected exactly one {what} matching {pattern!r} under {root}, found {len(matches)}",
    )
    return matches[0]


def runtime_profile() -> dict:
    return json.loads((ROOT / RUNTIME_PROFILE_RELATIVE_PATH).read_text(encoding="utf-8"))


def sealed_root_from_environment() -> Path:
    raw = os.environ.get("LOCALE_CATALOG_MISE_ROOT")
    require(raw is not None, "this test must run through the sealed locale-catalog launcher")
    return Path(raw)


def sealed_uv_binary() -> Path:
    return find_single(
        sealed_root_from_environment() / "installs" / "uv", "0.12.6/uv-*/uv", "sealed uv binary"
    )


# ---------------------------------------------------------------------------
# Canonical-worktree witnesses
# ---------------------------------------------------------------------------


def witness(path: Path) -> str:
    if path.is_symlink():
        return f"symlink:{path.readlink()}"
    if not path.exists():
        return "absent"
    if path.is_dir():
        entries = sorted(child.name for child in path.iterdir())
        return "dir:" + hashlib.sha256("\n".join(entries).encode("utf-8")).hexdigest()
    return "file:" + hashlib.sha256(path.read_bytes()).hexdigest()


def capture_canonical_witnesses() -> dict[str, str]:
    return {
        relative_path: witness(ROOT / relative_path)
        for relative_path in CANONICAL_WITNESS_PATHS
    }


def require_canonical_unchanged(before: dict[str, str]) -> None:
    after = capture_canonical_witnesses()
    changed = sorted(key for key in before if before[key] != after[key])
    require(
        not changed,
        "the adversarial probes changed canonical worktree paths that must never be written: "
        f"{changed}",
    )
    stray = sorted(
        child.name
        for child in ROOT.iterdir()
        if child.name.startswith(PROBE_PREFIX)
        or child.name.startswith("locale-catalog-python-hook-")
        or child.name.startswith("locale-catalog-boundary-")
    )
    require(not stray, f"probe scratch leaked into the canonical worktree: {stray}")


# ---------------------------------------------------------------------------
# Invocation-owned probe trees
# ---------------------------------------------------------------------------


def git_environment() -> dict[str, str]:
    return {**os.environ, **strict_json.THROWAWAY_GIT_COMMIT_ENV_OVERRIDES}


def run_git(tree: Path, arguments: list[str]) -> None:
    result = subprocess.run(
        strict_json.git_argv(arguments),
        cwd=tree,
        capture_output=True,
        env=git_environment(),
        timeout=300,
    )
    require(
        result.returncode == 0,
        f"probe setup failure: {arguments!r} exited {result.returncode}: "
        f"{result.stderr.decode('utf-8', errors='replace')}",
    )


def create_probe_tree(
    scratch: Path,
    name: str,
    token: str,
    *,
    governed_paths: tuple[str, ...] = GOVERNED_TREE_PATHS,
    with_history: bool = True,
) -> Path:
    """Copy the exact governed tree into a fresh, uniquely named, token-owned
    directory. `mkdir()` without `exist_ok` makes the creation exclusive, so
    two concurrent invocations can never share one probe tree.
    """
    tree = scratch / f"{PROBE_PREFIX}{name}-{uuid.uuid4().hex}"
    tree.mkdir()
    (tree / OWNER_SENTINEL).write_text(token, encoding="utf-8")
    for relative_path in governed_paths:
        source = ROOT / relative_path
        require(
            source.exists(),
            f"probe setup failure: governed tree path {relative_path!r} is missing from the "
            "canonical worktree, so this probe would prove nothing",
        )
        destination = tree / relative_path
        destination.parent.mkdir(parents=True, exist_ok=True)
        if source.is_dir():
            shutil.copytree(source, destination, symlinks=True)
        else:
            shutil.copy2(source, destination)
    if with_history:
        run_git(tree, ["git", "init", "-q"])
        run_git(tree, ["git", "add", "-A"])
        run_git(tree, ["git", "commit", "-q", "-m", "probe baseline"])
    return tree


def release_probe_tree(tree: Path, token: str) -> None:
    """Remove a probe tree only after re-proving this invocation owns it."""
    sentinel = tree / OWNER_SENTINEL
    if tree.is_symlink() or not tree.is_dir():
        return
    if sentinel.is_symlink() or not sentinel.is_file():
        return
    if sentinel.read_text(encoding="utf-8") != token:
        return
    shutil.rmtree(tree)


def owned_scratch() -> tuple[Path, str]:
    """Create a token-owned probe parent inside the checked-out repository.

    Every destructive probe below stays below this new directory.  Keeping it
    in the repository (rather than an ambient platform temp path) makes its
    ownership and cleanup checks explicit and lets the canonical-worktree
    witness prove it did not survive.
    """
    token = uuid.uuid4().hex
    scratch = ROOT / f"{PROBE_PREFIX}root-{token}"
    scratch.mkdir(mode=0o700)
    (scratch / SCRATCH_OWNER_FILE).write_text(token, encoding="ascii")
    return scratch, token


def release_owned_scratch(scratch: Path, token: str) -> None:
    owner = scratch / SCRATCH_OWNER_FILE
    require(
        scratch.is_dir() and not scratch.is_symlink(),
        f"probe scratch {scratch} is no longer a regular directory",
    )
    require(
        owner.is_file() and not owner.is_symlink() and owner.read_text(encoding="ascii") == token,
        f"probe scratch {scratch} ownership record changed; refusing cleanup",
    )
    shutil.rmtree(scratch)


# ---------------------------------------------------------------------------
# Running the real authoritative command inside a probe tree
# ---------------------------------------------------------------------------


def probe_environment(
    overrides: dict[str, str] | None = None, *, drop: tuple[str, ...] = ()
) -> dict[str, str]:
    environment = {**os.environ}
    environment.pop("ARKHAM_LOCALE_CATALOG_PYTHON_VENV", None)
    for key in drop:
        environment.pop(key, None)
    if overrides is not None:
        environment.update(overrides)
    return environment


def run_authoritative(
    tree: Path,
    arguments: list[str],
    *,
    environment: dict[str, str] | None = None,
    timeout: int = 1800,
) -> subprocess.CompletedProcess:
    return subprocess.run(
        [str(tree / LAUNCHER_RELATIVE_PATH), *arguments],
        cwd=tree,
        env=probe_environment() if environment is None else environment,
        capture_output=True,
        text=True,
        check=False,
        timeout=timeout,
    )


def require_authoritative_failure(
    label: str,
    tree: Path,
    arguments: list[str],
    *,
    environment: dict[str, str] | None = None,
) -> None:
    result = run_authoritative(tree, arguments, environment=environment)
    require(
        result.returncode != 0,
        f"{label} was accepted by the real authoritative command\n"
        f"stdout:\n{result.stdout}\nstderr:\n{result.stderr}",
    )


def require_authoritative_success(
    label: str,
    tree: Path,
    arguments: list[str],
    *,
    environment: dict[str, str] | None = None,
) -> None:
    result = run_authoritative(tree, arguments, environment=environment)
    require(
        result.returncode == 0,
        f"{label} was rejected by the real authoritative command\n"
        f"stdout:\n{result.stdout}\nstderr:\n{result.stderr}",
    )


# ---------------------------------------------------------------------------
# Probe: the production capability visitor rejects every reported bypass
# ---------------------------------------------------------------------------


def probe_tree_reader(tree: Path):
    def reader(relative_path: str) -> bytes:
        path = tree / relative_path
        require(
            relative_path in locale_catalog_python_boundary.EXECUTABLE_SOURCES,
            f"probe setup failure: {relative_path!r} is not a declared executable source",
        )
        require(
            not path.is_symlink() and path.is_file(),
            f"probe setup failure: {relative_path!r} is missing from the probe tree",
        )
        return path.read_bytes()

    return reader


def scan_rejects(tree: Path, reader, target: str, label: str, snippet: bytes) -> None:
    path = tree / target
    original = path.read_bytes()
    try:
        path.write_bytes(snippet + original)
        rejected = False
        try:
            locale_catalog_python_boundary.scan_python_closure(FIXTURE_ENTRY, source_reader=reader)
        except locale_catalog_python_boundary.SourceBoundaryError:
            rejected = True
    finally:
        path.write_bytes(original)
    require(rejected, f"the production capability visitor accepted {label!r} in {target}")


def test_capability_bypass_matrix(scratch: Path, token: str) -> int:
    tree = create_probe_tree(scratch, "capability-matrix", token, with_history=False)
    try:
        reader = probe_tree_reader(tree)
        locale_catalog_python_boundary.scan_python_closure(FIXTURE_ENTRY, source_reader=reader)
        checked = 0
        matrices = (
            ((FIXTURE_ENTRY, HELPER_SOURCE), BYPASS_SNIPPETS),
            ((GRANTED_SOURCE,), {**GRANT_OVERREACH_SNIPPETS, **SCOPE_BYPASS_SNIPPETS}),
        )
        for targets, snippets in matrices:
            for target in targets:
                for label, snippet in sorted(snippets.items()):
                    scan_rejects(tree, reader, target, label, snippet)
                    checked += 1
        return checked
    finally:
        release_probe_tree(tree, token)


def test_capability_matrix_end_to_end(scratch: Path, token: str) -> int:
    checked = 0
    for label in END_TO_END_BYPASSES:
        snippet = BYPASS_SNIPPETS[label]
        for target in (FIXTURE_ENTRY, HELPER_SOURCE):
            tree = create_probe_tree(scratch, "bypass", token, with_history=False)
            try:
                path = tree / target
                path.write_bytes(snippet + path.read_bytes())
                require_authoritative_failure(
                    f"{label} in {target}", tree, [FIXTURE_ENTRY, "--check"]
                )
                checked += 1
            finally:
                release_probe_tree(tree, token)
    return checked


def test_validated_target_really_executes(scratch: Path, token: str) -> int:
    """A successful bootstrap is not sufficient: the selected checker must run."""
    tree = create_probe_tree(scratch, "target-execution", token, with_history=False)
    try:
        result = run_authoritative(tree, [FIXTURE_ENTRY, "--must-not-be-ignored"])
        require(
            result.returncode != 0 and "unrecognized arguments: --must-not-be-ignored" in result.stderr,
            "the sealed boundary accepted an argument that only the governed target can reject; "
            "it verified the target without executing it\n"
            f"stdout:\n{result.stdout}\nstderr:\n{result.stderr}",
        )
        return 1
    finally:
        release_probe_tree(tree, token)


# ---------------------------------------------------------------------------
# Probe: source, schema and bytecode tampering
# ---------------------------------------------------------------------------


def test_source_and_schema_tampering(scratch: Path, token: str) -> int:
    checked = 0
    for relative_path in TAMPER_TARGETS:
        tree = create_probe_tree(scratch, "tamper", token)
        try:
            path = tree / relative_path
            path.write_bytes(path.read_bytes() + b"\n")
            require_authoritative_failure(
                f"tampered {relative_path}", tree, [FIXTURE_ENTRY, "--check"]
            )
            checked += 1
        finally:
            release_probe_tree(tree, token)

    tree = create_probe_tree(scratch, "symlink-source", token, with_history=False)
    try:
        target = tree / "scripts" / "strict_json.py"
        moved = tree / "scripts" / "strict_json_original.py"
        target.rename(moved)
        target.symlink_to(moved)
        require_authoritative_failure("symlinked governed source", tree, [FIXTURE_ENTRY, "--check"])
        checked += 1
    finally:
        release_probe_tree(tree, token)

    tree = create_probe_tree(scratch, "bytecode", token, with_history=False)
    try:
        cache = tree / "scripts" / "__pycache__"
        cache.mkdir()
        (cache / "strict_json.cpython-314.pyc").write_bytes(b"poisoned")
        require_authoritative_failure(
            "poisoned repository bytecode", tree, [FIXTURE_ENTRY, "--check"]
        )
        checked += 1
    finally:
        release_probe_tree(tree, token)

    tree = create_probe_tree(scratch, "undeclared-source", token, with_history=False)
    try:
        (tree / "scripts" / "smuggled.py").write_bytes(b"VALUE = 1\n")
        require_authoritative_failure(
            "undeclared executable source", tree, [FIXTURE_ENTRY, "--check"]
        )
        checked += 1
    finally:
        release_probe_tree(tree, token)

    for relative_path in (
        "scripts/locale_catalog_python_boundary/__init__.py",
        "scripts/json/__init__.py",
    ):
        tree = create_probe_tree(scratch, "nested-import-shadow", token, with_history=False)
        try:
            path = tree / relative_path
            path.parent.mkdir()
            path.write_bytes(b"raise SystemExit('nested import shadow ran')\n")
            require_authoritative_failure(
                f"nested importable shadow {relative_path}", tree, [FIXTURE_ENTRY, "--check"]
            )
            checked += 1
        finally:
            release_probe_tree(tree, token)
    return checked


def test_workflow_base_authority_wiring() -> int:
    text = (ROOT / ".github" / "workflows" / "contracts.yml").read_text(encoding="utf-8")
    require(
        "BASE_SHA: ${{ github.event_name" in text
        and 'mise run contracts:revision-drift -- "$BASE_SHA"' in text
        and 'revision-drift -- "${{' not in text,
        "contracts workflow interpolates an Actions expression into shell source instead of "
        "passing the event base through the quoted BASE_SHA environment value",
    )
    return 1


def test_fixture_writer_ownership(scratch: Path, token: str) -> int:
    """A generated-looking but unowned sentinel must never become deletion input."""
    tree = create_probe_tree(scratch, "fixture-writer-ownership", token)
    try:
        sentinel = tree / "contracts" / "fixtures" / "locale-catalog-user-sentinel.json"
        bytes_before = b'{"this":"must survive"}\n'
        sentinel.write_bytes(bytes_before)
        result = run_authoritative(tree, [FIXTURE_ENTRY])
        require(
            result.returncode != 0
            and "refusing to delete pre-existing locale-catalog fixture paths" in result.stderr,
            "the governed fixture writer accepted an unowned generated-looking sentinel\n"
            f"stdout:\n{result.stdout}\nstderr:\n{result.stderr}",
        )
        require(
            sentinel.read_bytes() == bytes_before,
            "the governed fixture writer altered an unowned pre-existing sentinel",
        )
        return 1
    finally:
        release_probe_tree(tree, token)


# ---------------------------------------------------------------------------
# Probe: toolchain root and interpreter attestation
# ---------------------------------------------------------------------------


def mirror_toolchain(sealed_root: Path, destination: Path, tampered: dict[str, bytes]) -> None:
    """Hard-link a sealed toolchain into an owned directory.

    Files named in `tampered` are written fresh instead of linked, so the real
    toolchain's inodes are never opened for writing by this test.
    """
    profile = runtime_profile()
    subtrees = (
        profile["interpreter"]["installRelativePath"],
        str(Path(profile["externalTools"]["node"]["binaryRelativePath"]).parent),
        sealed_uv_binary().parent.relative_to(sealed_root).as_posix(),
    )
    for subtree in subtrees:
        source_root = sealed_root / subtree
        (destination / subtree).mkdir(parents=True, exist_ok=True)
        for source in sorted(source_root.rglob("*")):
            relative_path = source.relative_to(sealed_root).as_posix()
            target = destination / relative_path
            target.parent.mkdir(parents=True, exist_ok=True)
            if source.is_symlink():
                target.symlink_to(source.readlink())
            elif source.is_dir():
                target.mkdir(exist_ok=True)
            elif relative_path in tampered:
                target.write_bytes(tampered[relative_path])
                target.chmod(source.stat().st_mode & 0o7777)
            else:
                target.hardlink_to(source)
    for relative_path, content in tampered.items():
        target = destination / relative_path
        if not target.exists():
            target.parent.mkdir(parents=True, exist_ok=True)
            target.write_bytes(content)


def test_toolchain_roots(scratch: Path, token: str) -> int:
    tree = create_probe_tree(scratch, "toolchain", token)
    sealed_root = sealed_root_from_environment()
    profile = runtime_profile()
    stdlib_relative = profile["interpreter"]["stdlibRelativePath"]
    binary_relative = profile["interpreter"]["binaryRelativePath"]
    node_relative = profile["externalTools"]["node"]["binaryRelativePath"]
    uv_relative = sealed_uv_binary().relative_to(sealed_root).as_posix()
    checked = 0
    try:
        cases = {
            "absent toolchain root": {"LOCALE_CATALOG_MISE_ROOT": ""},
            "relative toolchain root": {"LOCALE_CATALOG_MISE_ROOT": "relative/mise"},
            "unnormalized toolchain root": {"LOCALE_CATALOG_MISE_ROOT": f"{sealed_root}/./"},
            "parent-traversing toolchain root": {
                "LOCALE_CATALOG_MISE_ROOT": f"{sealed_root}/../mise"
            },
            "missing toolchain root": {
                "LOCALE_CATALOG_MISE_ROOT": str(scratch / "definitely-absent")
            },
            "filesystem-root toolchain root": {"LOCALE_CATALOG_MISE_ROOT": "/"},
            "toolchain root pointed at the repository": {"LOCALE_CATALOG_MISE_ROOT": str(tree)},
            "relative explicit Stack authority": {"LOCALE_CATALOG_STACK": "stack"},
            "missing explicit Stack authority": {
                "LOCALE_CATALOG_STACK": str(scratch / "definitely-absent-stack")
            },
        }
        for label, overrides in sorted(cases.items()):
            require_authoritative_failure(
                label, tree, [FIXTURE_ENTRY, "--check"], environment=probe_environment(overrides)
            )
            checked += 1

        symlinked = scratch / f"symlinked-root-{uuid.uuid4().hex}"
        symlinked.symlink_to(sealed_root)
        require_authoritative_failure(
            "symlinked toolchain root",
            tree,
            [FIXTURE_ENTRY, "--check"],
            environment=probe_environment({"LOCALE_CATALOG_MISE_ROOT": str(symlinked)}),
        )
        checked += 1

        traversed = scratch / f"traversed-root-{uuid.uuid4().hex}"
        traversed.mkdir()
        (traversed / "link").symlink_to(sealed_root)
        require_authoritative_failure(
            "toolchain root reached through a symlinked component",
            tree,
            [FIXTURE_ENTRY, "--check"],
            environment=probe_environment({"LOCALE_CATALOG_MISE_ROOT": str(traversed / "link")}),
        )
        checked += 1

        # A faithful hard-linked mirror is accepted, proving the tampered
        # mirrors below fail for the tampering and not for being a copy.
        mirror = scratch / f"mirror-{uuid.uuid4().hex}"
        mirror.mkdir()
        mirror_toolchain(sealed_root, mirror, {})
        require_authoritative_success(
            "faithful toolchain mirror",
            tree,
            [FIXTURE_ENTRY, "--check"],
            environment=probe_environment({"LOCALE_CATALOG_MISE_ROOT": str(mirror)}),
        )
        checked += 1

        # CPython normally accepts a hash-based unchecked pyc ahead of its
        # source. The sealed launcher must redirect every interpreter to its
        # fresh empty pycache prefix before the bootstrap imports `ast`; this
        # deliberately valid malicious cache therefore cannot execute.
        pycache_root = scratch / f"pycache-root-{uuid.uuid4().hex}"
        pycache_root.mkdir()
        mirror_toolchain(sealed_root, pycache_root, {})
        marker = scratch / f"stdlib-pyc-marker-{uuid.uuid4().hex}"
        pyc = (
            pycache_root
            / stdlib_relative
            / "__pycache__"
            / "ast.cpython-314.pyc"
        )
        pyc.parent.mkdir(exist_ok=True)
        # The mirror uses hard links for speed. Break this one link before
        # writing the adversarial cache so the test can never mutate the
        # actual mise installation's pre-existing bytecode.
        pyc.unlink(missing_ok=True)
        writer = (
            "import importlib.util,marshal,struct,sys;"
            "from pathlib import Path;"
            "code=compile(f\"from pathlib import Path; Path({sys.argv[2]!r}).write_text('ran')\","
            "'<poisoned-ast>','exec');"
            "Path(sys.argv[1]).write_bytes(importlib.util.MAGIC_NUMBER+struct.pack('<I',1)"
            "+b'01234567'+marshal.dumps(code))"
        )
        compiled = subprocess.run(
            [
                str(sealed_root / binary_relative),
                "-I",
                "-S",
                "-E",
                "-B",
                "-c",
                writer,
                str(pyc),
                str(marker),
            ],
            capture_output=True,
            text=True,
            check=False,
            timeout=300,
        )
        require(compiled.returncode == 0, f"could not construct poisoned stdlib pyc: {compiled.stderr}")
        require(not marker.exists(), "constructing a poisoned pyc unexpectedly executed its payload")
        require_authoritative_success(
            "unchecked stdlib bytecode under the normal cache path",
            tree,
            [FIXTURE_ENTRY, "--check"],
            environment=probe_environment({"LOCALE_CATALOG_MISE_ROOT": str(pycache_root)}),
        )
        require(not marker.exists(), "unchecked stdlib bytecode executed before source attestation")
        checked += 1
        shutil.rmtree(pycache_root)

        missing_root = scratch / f"missing-stdlib-root-{uuid.uuid4().hex}"
        missing_root.mkdir()
        mirror_toolchain(sealed_root, missing_root, {})
        (missing_root / stdlib_relative / "json" / "__init__.py").unlink()
        require_authoritative_failure(
            "missing attested stdlib source",
            tree,
            [FIXTURE_ENTRY, "--check"],
            environment=probe_environment({"LOCALE_CATALOG_MISE_ROOT": str(missing_root)}),
        )
        checked += 1
        shutil.rmtree(missing_root)

        original_module = (sealed_root / stdlib_relative / "json" / "__init__.py").read_bytes()
        tampered_roots = {
            "edited stdlib module": {
                f"{stdlib_relative}/json/__init__.py": original_module + b"\nPAYLOAD = 1\n"
            },
            "injected stdlib module": {f"{stdlib_relative}/smuggled_module.py": b"PAYLOAD = 1\n"},
            "replaced interpreter binary": {binary_relative: b"#!/bin/sh\nexit 0\n"},
            "replaced Node binary": {node_relative: b"#!/bin/sh\nexit 0\n"},
            "replaced uv binary": {uv_relative: b"#!/bin/sh\nexit 0\n"},
            # A compiled extension planted where a module the governed closure
            # really imports would be found. Nothing hashes it, and CPython's
            # path finder prefers an extension to a source file, so the only
            # thing standing between it and execution is the runtime's
            # import-closure attestation.
            "extension shadowing a reachable lib-dynload module": {
                f"{stdlib_relative}/lib-dynload/_json.probe.so": b"\x7fELF probe\n"
            },
            "extension shadowing a reachable top-level module": {
                f"{stdlib_relative}/csv.probe.so": b"\x7fELF probe\n"
            },
        }
        for label, tampered in sorted(tampered_roots.items()):
            fake = scratch / f"fake-root-{uuid.uuid4().hex}"
            fake.mkdir()
            mirror_toolchain(sealed_root, fake, tampered)
            require_authoritative_failure(
                label,
                tree,
                [FIXTURE_ENTRY, "--check"],
                environment=probe_environment({"LOCALE_CATALOG_MISE_ROOT": str(fake)}),
            )
            checked += 1
            shutil.rmtree(fake)

        symlinked_binary = scratch / f"symlinked-binary-root-{uuid.uuid4().hex}"
        symlinked_binary.mkdir()
        mirror_toolchain(sealed_root, symlinked_binary, {})
        binary = symlinked_binary / binary_relative
        binary.unlink()
        binary.symlink_to(sealed_root / binary_relative)
        require_authoritative_failure(
            "symlinked interpreter binary",
            tree,
            [FIXTURE_ENTRY, "--check"],
            environment=probe_environment({"LOCALE_CATALOG_MISE_ROOT": str(symlinked_binary)}),
        )
        checked += 1
        shutil.rmtree(symlinked_binary)
        shutil.rmtree(mirror)

        system_python = Path("/usr/bin/python3")
        require(
            system_python.is_file(), "the test host has no system Python for wrong-runtime coverage"
        )
        unsealed = subprocess.run(
            [
                str(system_python),
                "-I",
                "-S",
                "-E",
                "-B",
                str(tree / "scripts" / "locale_catalog_runtime.py"),
                FIXTURE_ENTRY,
                "--check",
            ],
            cwd=tree,
            env=probe_environment(),
            capture_output=True,
            text=True,
            check=False,
            timeout=600,
        )
        require(
            unsealed.returncode != 0,
            f"an unsealed system interpreter ran the bootstrap\nstdout:\n{unsealed.stdout}\n"
            f"stderr:\n{unsealed.stderr}",
        )
        checked += 1

        unisolated = subprocess.run(
            [
                str(sealed_root / binary_relative),
                str(tree / "scripts" / "locale_catalog_runtime.py"),
                FIXTURE_ENTRY,
                "--check",
            ],
            cwd=tree,
            env=probe_environment(),
            capture_output=True,
            text=True,
            check=False,
            timeout=600,
        )
        require(
            unisolated.returncode != 0,
            "the sealed interpreter ran the bootstrap without -I -S -E -B\n"
            f"stdout:\n{unisolated.stdout}\nstderr:\n{unisolated.stderr}",
        )
        checked += 1
        return checked
    finally:
        release_probe_tree(tree, token)


# ---------------------------------------------------------------------------
# Probe: caller environment, startup hooks and PATH shadowing
# ---------------------------------------------------------------------------


def test_hostile_caller_environment(scratch: Path, token: str) -> int:
    tree = create_probe_tree(scratch, "hostile-env", token)
    try:
        hostile = scratch / f"hostile-{uuid.uuid4().hex}"
        markers = hostile / "markers"
        markers.mkdir(parents=True)
        hook_body = (
            "from pathlib import Path\n"
            "import uuid\n"
            f"Path({str(markers)!r}, uuid.uuid4().hex).write_text('executed')\n"
        )
        for name in ("sitecustomize.py", "usercustomize.py", "startup.py"):
            (hostile / name).write_text(hook_body, encoding="utf-8")
        bash_env = hostile / "bash_env.sh"
        bash_env.write_text(f'/usr/bin/touch "{markers}/bash-env-$$"\n', encoding="utf-8")
        fake_bin = hostile / "bin"
        fake_bin.mkdir()
        for name in HOSTILE_EXECUTABLES:
            executable = fake_bin / name
            executable.write_text(
                f'#!/bin/sh\n/usr/bin/touch "{markers}/exec-{name}-$$"\nexit 97\n',
                encoding="utf-8",
            )
            executable.chmod(0o755)
        hostile_home = hostile / "home"
        hostile_home.mkdir()
        for name in (".bashrc", ".bash_profile", ".profile"):
            (hostile_home / name).write_text(
                f'/usr/bin/touch "{markers}/{name}-$$"\n', encoding="utf-8"
            )

        environment = probe_environment(
            {
                "HOME": str(hostile_home),
                "PATH": f"{fake_bin}{os.pathsep}{os.environ['PATH']}",
                "BASH_ENV": str(bash_env),
                "ENV": str(bash_env),
                "CDPATH": str(hostile),
                "PYTHONHOME": str(hostile),
                "PYTHONPATH": str(hostile),
                "PYTHONSTARTUP": str(hostile / "startup.py"),
                "PYTHONUSERBASE": str(hostile / "user"),
                "PYTHONEXECUTABLE": str(fake_bin / "python3"),
                "GIT_DIR": str(hostile / "git-dir"),
                "GIT_INDEX_FILE": str(hostile / "git-index"),
                "GIT_CONFIG_GLOBAL": str(hostile / "gitconfig"),
                "UV_PROJECT_ENVIRONMENT": str(hostile / "uv-venv"),
                "UV_CACHE_DIR": str(hostile / "uv-cache"),
            }
        )
        require_authoritative_success(
            "authoritative command under a hostile caller environment",
            tree,
            [FIXTURE_ENTRY, "--check"],
            environment=environment,
        )
        planted = sorted(child.name for child in markers.iterdir())
        require(
            not planted,
            f"a caller-supplied startup hook or PATH-shadowed executable ran: {planted}",
        )

        # The launcher's *own* protocol variables are caller input too. A
        # caller that names the bound git, this invocation's dependency root,
        # or the sealing sentinel must not be able to choose any of them: the
        # sealed stage rebuilds all three from nothing. The fake `git` named
        # here is the same marker-emitting script, so if the runtime honoured
        # `LOCALE_CATALOG_GIT` the governed path self-tests would run it.
        require_authoritative_success(
            "authoritative command with caller-supplied launcher protocol variables",
            tree,
            [FIXTURE_ENTRY, "--check"],
            environment={
                **environment,
                "LOCALE_CATALOG_GIT": str(fake_bin / "git"),
                "LOCALE_CATALOG_NODE": str(fake_bin / "node"),
                "LOCALE_CATALOG_UV": str(fake_bin / "uv"),
                "LOCALE_CATALOG_SEALED_SHELL": "0",
                "ARKHAM_LOCALE_CATALOG_PYTHON_VENV": str(hostile / "uv-venv"),
            },
        )
        planted = sorted(child.name for child in markers.iterdir())
        require(
            not planted,
            f"a caller-supplied launcher protocol variable was honoured: {planted}",
        )
        return 2
    finally:
        release_probe_tree(tree, token)


# ---------------------------------------------------------------------------
# Probe: dependency tampering never survives a fresh authoritative run
# ---------------------------------------------------------------------------


def test_dependency_tampering(scratch: Path, token: str) -> int:
    tree = create_probe_tree(scratch, "dependency", token)
    try:
        marker = tree / "locale-catalog-python-dependency-marker"
        seeded = b"pre-existing probe sentinel\n"
        marker.write_bytes(seeded)

        priming = run_authoritative(tree, ["scripts/validate-contract-fixtures.py"])
        require(
            priming.returncode == 0,
            "the probe tree could not run the authoritative command before tampering\n"
            f"stdout:\n{priming.stdout}\nstderr:\n{priming.stderr}",
        )

        workspace = tree / f"{PROBE_PREFIX}venv-{uuid.uuid4().hex}"
        workspace.mkdir()
        try:
            build = subprocess.run(
                [
                    str(sealed_uv_binary()),
                    "sync",
                    "--locked",
                    "--no-cache",
                    "--link-mode",
                    "copy",
                    "--no-dev",
                    "--no-install-project",
                    "--quiet",
                ],
                cwd=tree,
                env=probe_environment({"UV_PROJECT_ENVIRONMENT": str(workspace / "venv")}),
                capture_output=True,
                text=True,
                check=False,
                timeout=1800,
            )
            require(
                build.returncode == 0,
                "probe setup failure: could not build a tamperable dependency root: "
                f"{build.stderr}",
            )
            site_packages = workspace / "venv" / "lib" / "python3.14" / "site-packages"
            dependency = find_single(site_packages, "jsonschema/__init__.py", "dependency module")
            record = find_single(site_packages, "jsonschema-*.dist-info/RECORD", "wheel RECORD")
            payload = tree / f"dependency-payload-{uuid.uuid4().hex}"
            dependency.write_bytes(
                dependency.read_bytes()
                + f"\nfrom pathlib import Path\nPath({str(payload)!r}).write_text('tampered')\n".encode(
                    "utf-8"
                )
            )
            rows = list(csv.reader(record.read_text(encoding="utf-8").splitlines()))
            for row in rows:
                if row and row[0] == "jsonschema/__init__.py":
                    row[1] = (
                        "sha256="
                        + base64.urlsafe_b64encode(hashlib.sha256(dependency.read_bytes()).digest())
                        .decode("ascii")
                        .rstrip("=")
                    )
            with record.open("w", encoding="utf-8", newline="") as handle:
                csv.writer(handle, lineterminator="\n").writerows(rows)

            result = run_authoritative(tree, ["scripts/validate-contract-fixtures.py"])
            require(
                result.returncode == 0 and not payload.exists(),
                "a tampered dependency with a rewritten RECORD survived lock-hashed "
                f"reinstallation; status {result.returncode}, payload {payload.exists()}\n"
                f"stdout:\n{result.stdout}\nstderr:\n{result.stderr}",
            )

            # The same tampered root, offered as this invocation's workspace,
            # must be rejected outright rather than trusted.
            rejected = subprocess.run(
                [
                    str(sealed_root_from_environment() / runtime_profile()["interpreter"]["binaryRelativePath"]),
                    "-I",
                    "-S",
                    "-E",
                    "-B",
                    str(tree / "scripts" / "locale_catalog_runtime.py"),
                    "scripts/validate-contract-fixtures.py",
                ],
                cwd=tree,
                env=probe_environment(
                    {"ARKHAM_LOCALE_CATALOG_PYTHON_VENV": str(workspace / "venv")}
                ),
                capture_output=True,
                text=True,
                check=False,
                timeout=1800,
            )
            require(
                rejected.returncode != 0,
                "a RECORD-rewritten dependency root was accepted as an invocation workspace\n"
                f"stdout:\n{rejected.stdout}\nstderr:\n{rejected.stderr}",
            )
        finally:
            shutil.rmtree(workspace, ignore_errors=True)

        require(
            marker.read_bytes() == seeded,
            "the dependency probe overwrote a pre-existing dependency-marker sentinel",
        )
        return 3
    finally:
        release_probe_tree(tree, token)


# ---------------------------------------------------------------------------
# Probe: workspace ownership and kill recovery
# ---------------------------------------------------------------------------


def dead_process_identifier() -> int:
    reaped = subprocess.Popen(["/bin/echo"], stdout=subprocess.PIPE, stderr=subprocess.PIPE)
    reaped.communicate(timeout=120)
    return reaped.pid


def test_workspace_ownership(scratch: Path, token: str) -> int:
    tree = create_probe_tree(scratch, "workspace", token)
    try:
        stale = tree / f"{WORKSPACE_PREFIX}stale"
        stale.mkdir()
        (stale / WORKSPACE_OWNER_FILE).write_text(f"{dead_process_identifier()} 0 foreign stale\n", encoding="utf-8")
        live = tree / f"{WORKSPACE_PREFIX}live"
        live.mkdir()
        (live / WORKSPACE_OWNER_FILE).write_text(f"{os.getpid()} 0 foreign live\n", encoding="utf-8")
        long_lived = tree / f"{WORKSPACE_PREFIX}long-lived"
        long_lived.mkdir()
        (long_lived / WORKSPACE_OWNER_FILE).write_text(f"{os.getpid()} 0 foreign old\n", encoding="utf-8")
        unowned = tree / f"{WORKSPACE_PREFIX}unowned"
        unowned.mkdir()
        garbled = tree / f"{WORKSPACE_PREFIX}garbled"
        garbled.mkdir()
        (garbled / WORKSPACE_OWNER_FILE).write_text("not-an-owner-record\n", encoding="utf-8")

        require_authoritative_success(
            "authoritative command alongside pre-existing workspaces", tree, [FIXTURE_ENTRY, "--check"]
        )
        require(stale.exists(), "a stale-looking pre-existing workspace was removed")
        require(live.exists(), "a live workspace was removed")
        require(long_lived.exists(), "a long-running invocation's workspace was removed")
        require(unowned.exists(), "a workspace without an ownership record was removed")
        require(garbled.exists(), "a workspace with an unparsable ownership record was removed")
        remaining = sorted(
            child.name for child in tree.iterdir() if child.name.startswith(WORKSPACE_PREFIX)
        )
        require(
            remaining
            == [
                f"{WORKSPACE_PREFIX}garbled",
                f"{WORKSPACE_PREFIX}live",
                f"{WORKSPACE_PREFIX}long-lived",
                f"{WORKSPACE_PREFIX}stale",
                f"{WORKSPACE_PREFIX}unowned",
            ],
            f"a successful invocation did not release its own workspace; found {remaining}",
        )
        return 5
    finally:
        release_probe_tree(tree, token)


def test_kill_recovery(scratch: Path, token: str) -> int:
    tree = create_probe_tree(scratch, "kill", token)
    try:
        process = subprocess.Popen(
            [str(tree / LAUNCHER_RELATIVE_PATH), FIXTURE_ENTRY, "--check"],
            cwd=tree,
            env=probe_environment(),
            stdout=subprocess.PIPE,
            stderr=subprocess.PIPE,
            text=True,
        )
        deadline = time.monotonic() + 300
        leaked: list[Path] = []
        while time.monotonic() < deadline:
            leaked = sorted(tree.glob(f"{WORKSPACE_PREFIX}*"))
            if leaked and (leaked[0] / WORKSPACE_OWNER_FILE).is_file():
                break
            if process.poll() is not None:
                break
            time.sleep(0.02)
        process.kill()
        try:
            process.communicate(timeout=300)
        except subprocess.TimeoutExpired:
            require(False, "a killed authoritative invocation did not terminate")
        require(leaked, "the authoritative command never created an owned workspace to reclaim")

        # A hard kill skips the launcher's cleanup trap. Future invocations
        # must never decide that this path is theirs merely from a
        # caller-creatable owner record, regardless of its age.
        survivor = leaked[0]
        owner = survivor / WORKSPACE_OWNER_FILE
        require(owner.is_file(), "an abandoned workspace carries no ownership record")
        recorded = owner.read_text(encoding="utf-8").split()
        require(
            len(recorded) == 4 and recorded[0].isdigit() and recorded[1].isdigit(),
            f"an abandoned workspace has an unparsable ownership record: {recorded!r}",
        )
        require_authoritative_success(
            "authoritative command after a killed invocation", tree, [FIXTURE_ENTRY, "--check"]
        )
        require(survivor.exists(), "a killed invocation's workspace was reclaimed by another run")
        return 1
    finally:
        release_probe_tree(tree, token)


def test_injected_setup_failure(scratch: Path, token: str) -> int:
    failed = False
    try:
        create_probe_tree(
            scratch,
            "injected-setup-failure",
            token,
            governed_paths=(*GOVERNED_TREE_PATHS, "definitely/absent/governed/path"),
        )
    except ProbeFailure as error:
        failed = True
        require(
            "probe setup failure" in str(error) and "definitely/absent/governed/path" in str(error),
            f"an injected setup failure did not name the missing input: {error}",
        )
    require(failed, "an injected setup failure was not reported as a probe failure")

    foreign = scratch / f"{PROBE_PREFIX}foreign-{uuid.uuid4().hex}"
    foreign.mkdir()
    try:
        (foreign / OWNER_SENTINEL).write_text("a-different-invocations-token", encoding="utf-8")
        release_probe_tree(foreign, token)
        require(
            foreign.is_dir() and (foreign / OWNER_SENTINEL).is_file(),
            "cleanup removed a directory this invocation does not own",
        )
    finally:
        shutil.rmtree(foreign, ignore_errors=True)

    unowned = scratch / f"{PROBE_PREFIX}unowned-{uuid.uuid4().hex}"
    unowned.mkdir()
    try:
        release_probe_tree(unowned, token)
        require(unowned.is_dir(), "cleanup removed a directory with no ownership sentinel")
    finally:
        shutil.rmtree(unowned, ignore_errors=True)
    return 3


def test_parallel_self_tests(scratch: Path, token: str) -> int:
    trees = [create_probe_tree(scratch, f"parallel-{index}", token, with_history=False) for index in range(2)]
    started = []
    try:
        for tree in trees:
            started.append(
                (
                    tree,
                    subprocess.Popen(
                        [
                            str(tree / LAUNCHER_RELATIVE_PATH),
                            BOUNDARY_TEST_RELATIVE_PATH,
                            "--probe",
                            "capability-matrix",
                            "--owner-token",
                            token,
                        ],
                        cwd=tree,
                        env=probe_environment(),
                        stdout=subprocess.PIPE,
                        stderr=subprocess.PIPE,
                        text=True,
                    ),
                )
            )
        for tree, process in started:
            stdout, stderr = process.communicate(timeout=3600)
            require(
                process.returncode == 0,
                f"a parallel self-test invocation in {tree.name} failed\n"
                f"stdout:\n{stdout}\nstderr:\n{stderr}",
            )
            require(
                (tree / OWNER_SENTINEL).read_text(encoding="utf-8") == token,
                f"a parallel self-test invocation disturbed {tree.name}'s ownership sentinel",
            )
        return len(trees)
    finally:
        for tree in trees:
            release_probe_tree(tree, token)


# ---------------------------------------------------------------------------
# Entry point
# ---------------------------------------------------------------------------


def require_owned_probe_mode(token: str | None) -> str:
    """Destructive self-test mode is gated on the owning token.

    There is no root override to grant: a probe tree *is* its own root, so this
    check can never relax a production check. It only refuses to let a stray
    invocation mutate a tree this process cannot demonstrably prove it owns.
    """
    require(token is not None, "--probe requires the owning --owner-token")
    sentinel = ROOT / OWNER_SENTINEL
    require(
        not sentinel.is_symlink() and sentinel.is_file(),
        f"--probe may only run inside an invocation-owned probe tree; {ROOT} carries no "
        f"{OWNER_SENTINEL} sentinel",
    )
    require(
        sentinel.read_text(encoding="utf-8") == token,
        "--probe was given a token that does not match this probe tree's ownership sentinel",
    )
    return token


def main() -> None:
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--probe", choices=("capability-matrix",))
    parser.add_argument("--owner-token")
    arguments = parser.parse_args()

    if arguments.probe is not None:
        token = require_owned_probe_mode(arguments.owner_token)
        scratch, scratch_token = owned_scratch()
        try:
            checked = test_capability_bypass_matrix(scratch, token)
        finally:
            release_owned_scratch(scratch, scratch_token)
        print(f"locale-catalog python boundary: {checked} capability-matrix rejections proved")
        return

    before = capture_canonical_witnesses()
    token = uuid.uuid4().hex
    scratch, scratch_token = owned_scratch()
    totals: dict[str, int] = {}
    try:
        totals["capability matrix"] = test_capability_bypass_matrix(scratch, token)
        totals["capability matrix end to end"] = test_capability_matrix_end_to_end(scratch, token)
        totals["target execution"] = test_validated_target_really_executes(scratch, token)
        totals["source and schema tampering"] = test_source_and_schema_tampering(scratch, token)
        totals["fixture writer ownership"] = test_fixture_writer_ownership(scratch, token)
        totals["workflow base authority"] = test_workflow_base_authority_wiring()
        totals["toolchain attestation"] = test_toolchain_roots(scratch, token)
        totals["hostile caller environment"] = test_hostile_caller_environment(scratch, token)
        totals["dependency tampering"] = test_dependency_tampering(scratch, token)
        totals["workspace ownership"] = test_workspace_ownership(scratch, token)
        totals["kill recovery"] = test_kill_recovery(scratch, token)
        totals["injected setup failure"] = test_injected_setup_failure(scratch, token)
        totals["parallel self-tests"] = test_parallel_self_tests(scratch, token)
    finally:
        release_owned_scratch(scratch, scratch_token)
    require_canonical_unchanged(before)
    summary = ", ".join(f"{name}: {count}" for name, count in sorted(totals.items()))
    print(
        "locale-catalog python boundary: "
        f"{sum(totals.values())} adversarial checks passed ({summary}); "
        "the canonical worktree was never written"
    )


if __name__ == "__main__":
    main()
