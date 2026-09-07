#!/usr/bin/env python3
"""Emit deterministic, fail-closed records for an authority tree or path set."""

from __future__ import annotations

import hashlib
import os
import stat
import sys
from pathlib import Path


def die(message: str) -> None:
    raise SystemExit(f"authority-tree: {message}")


def safe_relative(relative: str) -> None:
    if (
        not relative
        or relative.startswith("/")
        or relative.endswith("/")
        or "\t" in relative
        or "\n" in relative
        or "\r" in relative
        or any(part in {"", ".", ".."} for part in relative.split("/"))
    ):
        die(f"unsafe authority path: {relative!r}")
    try:
        relative.encode("utf-8")
    except UnicodeEncodeError:
        die(f"authority path cannot be represented as UTF-8: {relative!r}")


def relative_to_root(path: Path, root: Path) -> str:
    try:
        relative = path.relative_to(root).as_posix()
    except ValueError:
        die(f"authority symlink escapes its root: {path}")
    safe_relative(relative)
    return relative


def resolve_internal_link(path: Path, root: Path) -> Path:
    current = path
    for _ in range(64):
        try:
            mode = current.lstat().st_mode
        except OSError as error:
            die(f"could not inspect authority symlink {current}: {error}")
        if not stat.S_ISLNK(mode):
            if stat.S_ISREG(mode) or stat.S_ISDIR(mode):
                relative_to_root(current.resolve(strict=True), root)
                return current.resolve(strict=True)
            die(f"authority symlink has an unsupported final target: {current}")
        try:
            target = os.readlink(current)
        except OSError as error:
            die(f"could not read authority symlink {current}: {error}")
        if not target or target.startswith("/") or "\t" in target or "\n" in target or "\r" in target:
            die(f"unsafe authority symlink target: {path}")
        candidate = current.parent / target
        try:
            parent = candidate.parent.resolve(strict=True)
        except OSError as error:
            die(f"broken authority symlink target: {path}: {error}")
        current = parent / candidate.name
        try:
            relative_to_root(current.resolve(strict=False), root)
        except RuntimeError as error:
            die(f"authority symlink is cyclic: {path}: {error}")
    die(f"authority symlink chain is cyclic or too deep: {path}")


def hash_file(path: Path) -> str:
    digest = hashlib.sha256()
    with path.open("rb") as source:
        for block in iter(lambda: source.read(1024 * 1024), b""):
            digest.update(block)
    return digest.hexdigest()


def mode_text(path: Path) -> str:
    return format(stat.S_IMODE(path.lstat().st_mode), "o")


def tree_records(root: Path, *, allow_links: bool, selected: list[str]) -> list[str]:
    records: dict[str, str] = {}
    pending = list(selected)
    seen_selected: set[str] = set()

    def collect(path: Path, relative: str) -> None:
        safe_relative(relative)
        if relative in records:
            return
        try:
            mode = path.lstat().st_mode
        except OSError as error:
            die(f"could not inspect authority path {path}: {error}")
        if stat.S_ISLNK(mode):
            if not allow_links:
                die(f"authority tree contains a forbidden symlink: {relative}")
            try:
                target = os.readlink(path)
            except OSError as error:
                die(f"could not read authority symlink {path}: {error}")
            resolved = resolve_internal_link(path, root)
            records[relative] = f"link\t{relative}\t{target}"
            pending.append(relative_to_root(resolved, root))
            return
        if stat.S_ISREG(mode):
            records[relative] = f"file\t{relative}\t{mode_text(path)}\t{hash_file(path)}"
            return
        if stat.S_ISDIR(mode):
            records[relative] = f"dir\t{relative}\t{mode_text(path)}"
            try:
                children = sorted(os.scandir(path), key=lambda entry: os.fsencode(entry.name))
            except OSError as error:
                die(f"could not enumerate authority directory {path}: {error}")
            for child in children:
                child_relative = f"{relative}/{child.name}"
                collect(Path(child.path), child_relative)
            return
        die(f"authority tree contains an unsupported file type: {path}")

    while pending:
        relative = pending.pop(0)
        safe_relative(relative)
        if relative in seen_selected:
            continue
        seen_selected.add(relative)
        path = root / relative
        try:
            mode = path.lstat().st_mode
        except OSError as error:
            die(f"required authority path is missing: {path}: {error}")
        if stat.S_ISDIR(mode) and not stat.S_ISLNK(mode):
            collect(path, relative)
        elif stat.S_ISREG(mode) or stat.S_ISLNK(mode):
            collect(path, relative)
        else:
            die(f"required authority path has an unsupported file type: {path}")
    return [record for _, record in sorted(records.items(), key=lambda item: os.fsencode(item[0]))]


def main() -> None:
    if len(sys.argv) < 3:
        die("usage: authority-tree.py tree|paths ROOT [PATH ...]")
    mode = sys.argv[1]
    raw_root = Path(sys.argv[2])
    if raw_root.is_symlink():
        die(f"authority root is missing or unsafe: {raw_root}")
    try:
        root = raw_root.resolve(strict=True)
    except OSError as error:
        die(f"authority root is missing or unsafe: {sys.argv[2]}: {error}")
    if not root.is_dir() or root.is_symlink():
        die(f"authority root is missing or unsafe: {root}")
    if mode == "tree":
        if len(sys.argv) != 3:
            die("tree mode does not accept selected paths")
        selected = [entry.name for entry in sorted(os.scandir(root), key=lambda entry: os.fsencode(entry.name))]
        records = tree_records(root, allow_links=False, selected=selected)
    elif mode == "paths":
        if len(sys.argv) < 4:
            die("path mode requires one or more selected paths")
        records = tree_records(root, allow_links=True, selected=sys.argv[3:])
    else:
        die(f"unsupported mode: {mode}")
    if records:
        sys.stdout.write("\n".join(records))
        sys.stdout.write("\n")


if __name__ == "__main__":
    main()
