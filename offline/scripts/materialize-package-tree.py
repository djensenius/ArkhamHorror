#!/usr/bin/env python3
"""Turn a checked package tree into the regular-file representation we archive."""

from __future__ import annotations

import os
import shutil
import stat
import sys
from pathlib import Path


def fail(message: str) -> None:
    raise SystemExit(f"materialize-package-tree: {message}")


def inside(path: Path, root: Path) -> bool:
    try:
        path.relative_to(root)
    except ValueError:
        return False
    return True


def regular_tree(root: Path) -> list[Path]:
    paths: list[Path] = []
    for current, directories, files in os.walk(root, followlinks=False):
        current_path = Path(current)
        for name in directories + files:
            path = current_path / name
            mode = path.lstat().st_mode
            if stat.S_ISLNK(mode):
                if path.is_dir():
                    fail(f"directory symlinks are not a supported package representation: {path}")
                try:
                    target = path.resolve(strict=True)
                except (OSError, RuntimeError) as error:
                    fail(f"broken or cyclic package symlink {path}: {error}")
                if not inside(target, root) or not target.is_file() or target.is_symlink():
                    fail(f"package symlink does not resolve to an internal regular file: {path}")
                temporary = path.with_name(f".{path.name}.materialize-{os.getpid()}")
                try:
                    shutil.copy2(target, temporary, follow_symlinks=True)
                    os.replace(temporary, path)
                finally:
                    temporary.unlink(missing_ok=True)
                mode = path.lstat().st_mode
            if stat.S_ISREG(mode):
                paths.append(path)
            elif not stat.S_ISDIR(mode):
                fail(f"unsupported package file type: {path}")
    return paths


def main() -> None:
    if len(sys.argv) != 2:
        fail("usage: materialize-package-tree.py PACKAGE_DIR")
    root = Path(sys.argv[1])
    if root.is_symlink() or not root.is_dir():
        fail(f"package root is missing or unsafe: {root}")
    root = root.resolve(strict=True)
    files = regular_tree(root)
    for path in files:
        if path.stat().st_nlink > 1:
            temporary = path.with_name(f".{path.name}.dehardlink-{os.getpid()}")
            try:
                shutil.copy2(path, temporary, follow_symlinks=False)
                os.replace(temporary, path)
            finally:
                temporary.unlink(missing_ok=True)
    # Rewalk after replacements so the archive preflight's representation is
    # guaranteed before provenance and the final closure are calculated.
    for path in regular_tree(root):
        if path.stat().st_nlink != 1:
            fail(f"could not remove package hardlink: {path}")


if __name__ == "__main__":
    main()
