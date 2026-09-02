#!/usr/bin/env python3
"""Adversarial tests for the production locale-catalog Python launcher."""

from __future__ import annotations

import base64
import csv
import hashlib
import os
from pathlib import Path
import subprocess
import tempfile

ROOT = Path(__file__).resolve().parents[1]
SCRIPTS = ROOT / "scripts"
LAUNCHER = SCRIPTS / "run-locale-catalog-python.sh"
FIXTURE_TASK = [str(LAUNCHER), "scripts/build-locale-catalog-fixture.py", "--check"]


def require(condition: bool, message: str) -> None:
    if not condition:
        raise SystemExit(f"locale-catalog python boundary: {message}")


def run_command(command: list[str], *, environment: dict[str, str] | None = None) -> subprocess.CompletedProcess[str]:
    env = dict(os.environ)
    if environment is not None:
        env.update(environment)
    return subprocess.run(command, cwd=ROOT, env=env, capture_output=True, text=True, check=False)


def require_failure(label: str, command: list[str], *, environment: dict[str, str] | None = None) -> None:
    result = run_command(command, environment=environment)
    require(
        result.returncode != 0,
        f"{label} was accepted by the production command\nstdout:\n{result.stdout}\nstderr:\n{result.stderr}",
    )


def replace_temporarily(path: Path, injected: bytes, action) -> None:
    original = path.read_bytes()
    try:
        path.write_bytes(injected + original)
        action()
    finally:
        path.write_bytes(original)


def test_aliases_and_indirect_loaders() -> None:
    entry = SCRIPTS / "build-locale-catalog-fixture.py"
    helper = SCRIPTS / "strict_json.py"
    bypasses = {
        "aliased sys.path append": b'import sys as runtime_sys\nruntime_sys.path.append("outside")\n',
        "imported sys.modules": b"from sys import modules\nmodules.clear()\n",
        "aliased builtins exec": b'import builtins as runtime_builtins\nruntime_builtins.exec("x = 1")\n',
        "imported builtins exec": b'from builtins import exec as runtime_exec\nruntime_exec("x = 1")\n',
        "aliased builtins import": b'from builtins import __import__ as runtime_import\nruntime_import("os")\n',
        "loader dunder": b'__loader__.load_module("os")\n',
        "zipimport loader": b'import zipimport\nzipimport.zipimporter("x").load_module("os")\n',
        "getattr sys.path": b'import sys\ngetattr(sys, "path")\n',
    }
    for label, source in bypasses.items():
        replace_temporarily(entry, source, lambda: require_failure(label, FIXTURE_TASK))
        replace_temporarily(helper, source, lambda: require_failure(f"{label} in helper", FIXTURE_TASK))


def test_startup_hooks_and_path_shadowing() -> None:
    with tempfile.TemporaryDirectory(prefix="locale-catalog-python-hook-", dir=ROOT) as directory:
        hook_root = Path(directory)
        marker = hook_root / "executed"
        (hook_root / "sitecustomize.py").write_text(
            f"from pathlib import Path\nPath({str(marker)!r}).write_text('executed')\n",
            encoding="utf-8",
        )
        fake_bin = hook_root / "bin"
        fake_bin.mkdir()
        for name in ("python", "uv"):
            path = fake_bin / name
            path.write_text("#!/usr/bin/env bash\nexit 97\n", encoding="utf-8")
            path.chmod(0o755)
        result = run_command(
            FIXTURE_TASK,
            environment={
                "PYTHONHOME": str(hook_root),
                "PYTHONPATH": str(hook_root),
                "PYTHONSTARTUP": str(hook_root / "sitecustomize.py"),
                "PYTHONUSERBASE": str(hook_root / "user"),
                "PATH": f"{fake_bin}{os.pathsep}{os.environ['PATH']}",
            },
        )
        require(not marker.exists(), "startup hooks ran through the production task")


def test_bytecode_and_wrong_interpreter() -> None:
    cache = SCRIPTS / "__pycache__"
    require(not cache.exists(), f"test precondition failed: {cache} already exists")
    cache.mkdir()
    poisoned = cache / "strict_json.cpython-314.pyc"
    try:
        poisoned.write_bytes(b"poisoned")
        require_failure("poisoned repository bytecode", FIXTURE_TASK)
    finally:
        poisoned.unlink()
        cache.rmdir()

    with tempfile.TemporaryDirectory(prefix="locale-catalog-python-wrong-runtime-", dir=ROOT) as directory:
        require_failure(
            "wrong mise runtime root",
            [str(LAUNCHER), "scripts/build-locale-catalog-fixture.py", "--check"],
            environment={"HOME": directory},
        )
    system_python = Path("/usr/bin/python3")
    require(system_python.is_file(), "test host has no system Python for wrong-runtime coverage")
    require_failure(
        "wrong interpreter",
        [
            str(system_python),
            "-I",
            "-S",
            "-E",
            "-B",
            str(SCRIPTS / "locale_catalog_runtime.py"),
            "scripts/build-locale-catalog-fixture.py",
            "--check",
        ],
    )


def test_dependency_and_source_tampering() -> None:
    raw_venv = os.environ.get("ARKHAM_LOCALE_CATALOG_PYTHON_VENV")
    require(raw_venv is not None, "test must run through the production locale-catalog Python launcher")
    venv = Path(raw_venv)
    site_packages = venv / "lib" / "python3.14" / "site-packages"
    dependency = next(site_packages.glob("jsonschema/__init__.py"))
    record = next(site_packages.glob("jsonschema-*.dist-info/RECORD"))
    original_dependency = dependency.read_bytes()
    original_record = record.read_bytes()
    marker = ROOT / "locale-catalog-python-dependency-marker"
    try:
        dependency.write_bytes(
            original_dependency
            + f"\nfrom pathlib import Path\nPath({str(marker)!r}).write_text('tampered')\n".encode("utf-8")
        )
        rows = list(csv.reader(original_record.decode("utf-8").splitlines()))
        for row in rows:
            if row[0] == "jsonschema/__init__.py":
                row[1] = (
                    "sha256="
                    + base64.urlsafe_b64encode(hashlib.sha256(dependency.read_bytes()).digest())
                    .decode("ascii")
                    .rstrip("=")
                )
        with record.open("w", encoding="utf-8", newline="") as handle:
            csv.writer(handle, lineterminator="\n").writerows(rows)
        result = run_command([str(LAUNCHER), "scripts/validate-contract-fixtures.py"])
        require(
            result.returncode == 0 and not marker.exists(),
            "a tampered dependency with a rewritten RECORD survived lock-hashed reinstallation; "
            f"status {result.returncode}, marker {marker.exists()}\n"
            f"stdout:\n{result.stdout}\nstderr:\n{result.stderr}",
        )
    finally:
        dependency.write_bytes(original_dependency)
        record.write_bytes(original_record)
        marker.unlink(missing_ok=True)

    for relative_path in (
        "scripts/build-locale-catalog-fixture.py",
        "scripts/strict_json.py",
        "scripts/json_schema_subset.py",
        "frontend/schemas/locale-catalog/v1/manifest.schema.json",
    ):
        path = ROOT / relative_path
        original = path.read_bytes()
        try:
            path.write_bytes(original + b"\n")
            require_failure(f"tampered {relative_path}", FIXTURE_TASK)
        finally:
            path.write_bytes(original)


def main() -> None:
    test_aliases_and_indirect_loaders()
    test_startup_hooks_and_path_shadowing()
    test_bytecode_and_wrong_interpreter()
    test_dependency_and_source_tampering()
    print("locale-catalog python boundary: production adversarial checks passed")


if __name__ == "__main__":
    main()
