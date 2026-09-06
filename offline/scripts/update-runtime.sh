#!/usr/bin/env bash
# Trusted in-place updater copied into each offline package. It accepts only a
# same-platform archive whose published SHA-256 is supplied out of band, then
# preflights every archive member before changing the installed game tree.

set -euo pipefail

readonly MAX_ARCHIVE_MEMBERS=60000
readonly MAX_ARCHIVE_BYTES=$((2 * 1024 * 1024 * 1024))

die() { printf '[update] error: %s\n' "$*" >&2; exit 1; }
info() { printf '[update] %s\n' "$*"; }

sha256_file() {
    if command -v sha256sum >/dev/null 2>&1; then
        sha256sum "$1" | awk '{print $1}'
    elif command -v shasum >/dev/null 2>&1; then
        shasum -a 256 "$1" | awk '{print $1}'
    else
        die "No SHA-256 tool is available"
    fi
}

detect_platform() {
    local os arch
    case "$(uname -s)" in
        Darwin) os="macos" ;;
        Linux) os="linux" ;;
        *) die "Unsupported operating system: $(uname -s)" ;;
    esac
    case "$(uname -m)" in
        arm64|aarch64) arch="arm64" ;;
        x86_64|amd64) arch="x86_64" ;;
        *) die "Unsupported CPU architecture: $(uname -m)" ;;
    esac
    printf '%s-%s\n' "$os" "$arch"
}

random_hex() {
    local value
    value="$(LC_ALL=C od -An -N32 -tx1 /dev/urandom | tr -d ' \n')" \
        || die "Could not create an updater work token"
    case "$value" in
        *[!0-9a-f]*|"") die "Updater work token is invalid" ;;
    esac
    [ "${#value}" = 64 ] || die "Updater work token is invalid"
    printf '%s\n' "$value"
}

create_owned_workdir() {
    local base_dir="$1" parent token work
    parent="${base_dir}/.update-work"
    [ ! -L "$parent" ] || die "Updater work parent must not be a symlink"
    mkdir -p "$parent" || die "Could not create updater work parent"
    [ -d "$parent" ] && [ ! -L "$parent" ] || die "Updater work parent is unsafe"
    token="$(random_hex)"
    work="${parent}/update-${token}"
    [ ! -e "$work" ] && [ ! -L "$work" ] || die "Refusing to reuse updater work directory"
    (umask 077 && mkdir "$work") || die "Could not create updater work directory"
    printf '%s\n' "$token" > "${work}/owner"
    chmod 600 "${work}/owner"
    printf '%s\n' "$work"
}

owned_workdir_is_valid() {
    local work="$1" base_dir="$2" token basename
    case "$work" in "${base_dir}/.update-work/update-"*) ;; *) return 1 ;; esac
    [ -d "$work" ] && [ ! -L "$work" ] || return 1
    [ -f "${work}/owner" ] && [ ! -L "${work}/owner" ] || return 1
    token="$(cat "${work}/owner")"
    case "$token" in *[!0-9a-f]*|"") return 1 ;; esac
    [ "${#token}" = 64 ] || return 1
    basename="${work##*/}"
    [ "$basename" = "update-${token}" ] || return 1
}

verify_owned_workdir() {
    owned_workdir_is_valid "$@" || die "Updater work directory ownership marker is missing or invalid"
}

cleanup_owned_workdir() {
    local work="$1" base_dir="$2"
    if [ -n "$work" ] && [ -d "$work" ] && [ ! -L "$work" ]; then
        if owned_workdir_is_valid "$work" "$base_dir"; then
            rm -rf -- "$work"
        else
            printf '[update] warning: refusing unsafe updater work-directory cleanup\n' >&2
        fi
    fi
}

process_executable() {
    local pid="$1"
    case "$(uname -s)" in
        Linux) readlink "/proc/${pid}/exe" 2>/dev/null || true ;;
        Darwin)
            lsof -a -p "$pid" -d txt -Fn 2>/dev/null | sed -n 's/^n//p' | head -1
            ;;
    esac
}

stop_owned_process() {
    local pid_file="$1" expected="$2" pid executable attempts=0
    [ -f "$pid_file" ] && [ ! -L "$pid_file" ] || return 0
    pid="$(tr -d '[:space:]' < "$pid_file" 2>/dev/null || true)"
    case "$pid" in ''|*[!0-9]*) return 0 ;; esac
    kill -0 "$pid" 2>/dev/null || return 0
    executable="$(process_executable "$pid")"
    [ "$executable" = "$expected" ] || return 0
    kill -TERM "$pid" 2>/dev/null || true
    while kill -0 "$pid" 2>/dev/null && [ "$attempts" -lt 25 ]; do
        sleep 0.2
        attempts=$((attempts + 1))
    done
}

BASE_DIR="${1:-}"
if [ -z "$BASE_DIR" ]; then
    BASE_DIR="$(cd "$(dirname "$0")/.." && pwd -P)"
else
    [ -d "$BASE_DIR" ] && [ ! -L "$BASE_DIR" ] || die "Package base directory is missing or unsafe"
    BASE_DIR="$(cd "$BASE_DIR" && pwd -P)"
fi
GAME_DIR="${BASE_DIR}/game"
[ -d "$GAME_DIR" ] && [ ! -L "$GAME_DIR" ] || die "game/ directory is missing or unsafe"

EXPECTED_ARCHIVE_SHA256="${3:-}"
[ "$#" -le 3 ] || die "Usage: update.sh [package-dir] [owned-work-dir] <published-archive-sha256>"
case "$EXPECTED_ARCHIVE_SHA256" in
    *[!0-9a-f]*|"") die "A published 64-character lowercase SHA-256 is required" ;;
esac
[ "${#EXPECTED_ARCHIVE_SHA256}" = 64 ] \
    || die "A published 64-character lowercase SHA-256 is required"

PLATFORM="$(detect_platform)"
PLATFORM_FILE="${GAME_DIR}/config/release-platform"
[ -f "$PLATFORM_FILE" ] && [ ! -L "$PLATFORM_FILE" ] || die "Package platform marker is missing"
[ "$(cat "$PLATFORM_FILE")" = "$PLATFORM" ] \
    || die "Installed package platform does not match this host (${PLATFORM})"

WORK_DIR="${2:-}"
if [ -z "$WORK_DIR" ]; then
    WORK_DIR="$(create_owned_workdir "$BASE_DIR")"
fi
verify_owned_workdir "$WORK_DIR" "$BASE_DIR"
trap 'cleanup_owned_workdir "$WORK_DIR" "$BASE_DIR"' EXIT
EXTRACT_DIR="${WORK_DIR}/extract"
mkdir "$EXTRACT_DIR" || die "Could not create updater extraction directory"

CURRENT_VERSION="dev"
for marker in "${GAME_DIR}"/current_v[0-9]*; do
    [ -e "$marker" ] || continue
    CURRENT_VERSION="${marker##*/current_}"
    break
done

selection="$(
    python3 - "$BASE_DIR" "$PLATFORM" "$EXTRACT_DIR" "$MAX_ARCHIVE_MEMBERS" "$MAX_ARCHIVE_BYTES" "$EXPECTED_ARCHIVE_SHA256" <<'PY'
import hashlib
import os
import posixpath
import re
import sys
import tarfile
from pathlib import Path

base = Path(sys.argv[1])
platform = sys.argv[2]
destination = Path(sys.argv[3])
max_members = int(sys.argv[4])
max_bytes = int(sys.argv[5])
expected_digest = sys.argv[6]
pattern = re.compile(rf"^ArkhamHorror-{re.escape(platform)}-v([0-9]{{8}})\.([0-9]+)\.tar\.gz$")
candidates = []
versions = set()
for archive in base.iterdir():
    if not archive.is_file() or archive.is_symlink():
        continue
    match = pattern.fullmatch(archive.name)
    if match is not None:
        version = (match.group(1), int(match.group(2)))
        if version in versions:
            raise SystemExit(f"ambiguous same-platform release version: {version[0]}.{version[1]}")
        versions.add(version)
        candidates.append((version, archive))
if not candidates:
    raise SystemExit(f"no same-platform release archive for {platform}")
version, archive = max(candidates)
if not re.fullmatch(r"[0-9a-f]{64}", expected_digest):
    raise SystemExit("published archive SHA-256 is malformed")
digest = hashlib.sha256()
with archive.open("rb") as source:
    for block in iter(lambda: source.read(1024 * 1024), b""):
        digest.update(block)
if digest.hexdigest() != expected_digest:
    raise SystemExit(f"published checksum mismatch for {archive.name}")

entries = {}
members = []
total = 0
member_count = 0
with tarfile.open(archive, "r:gz") as tar:
    while (member := tar.next()) is not None:
        member_count += 1
        if member_count > max_members:
            raise SystemExit("archive member-count limit exceeded")
        raw = member.name
        if "\x00" in raw or "\\" in raw or raw.startswith("/"):
            raise SystemExit(f"unsafe archive member path: {raw!r}")
        if ".." in raw.split("/"):
            raise SystemExit(f"archive traversal path: {raw!r}")
        normalized = posixpath.normpath(raw)
        if normalized == ".":
            if not member.isdir():
                raise SystemExit("archive root is not a directory")
            continue
        if normalized == ".." or normalized.startswith("../") or normalized.startswith("/"):
            raise SystemExit(f"archive traversal path: {raw!r}")
        if member.issym() or member.islnk() or member.isfifo() or member.ischr() or member.isblk():
            raise SystemExit(f"unsupported archive member type: {raw!r}")
        if member.isdir():
            kind = "dir"
        elif member.isfile():
            kind = "file"
            total += member.size
            if total > max_bytes:
                raise SystemExit("archive uncompressed-size limit exceeded")
        else:
            raise SystemExit(f"unsupported archive member type: {raw!r}")
        if normalized in entries:
            raise SystemExit(f"duplicate archive member: {normalized}")
        entries[normalized] = kind
        members.append((member, normalized, kind))

for name, kind in entries.items():
    parts = name.split("/")
    for end in range(1, len(parts)):
        parent = "/".join(parts[:end])
        if entries.get(parent) == "file":
            raise SystemExit(f"file/directory archive collision: {parent}")
    if kind == "file" and any(other.startswith(f"{name}/") for other in entries):
        raise SystemExit(f"file/directory archive collision: {name}")

start = "game/start.sh"
if entries.get("game") != "dir" or entries.get(start) != "file":
    raise SystemExit("archive lacks a regular game/start.sh")
start_member = next(member for member, name, _ in members if name == start)
if start_member.mode & 0o111 == 0:
    raise SystemExit("archive game/start.sh is not executable")
platform_member = "game/config/release-platform"
if entries.get(platform_member) != "file":
    raise SystemExit("archive lacks game/config/release-platform")

game_members = [(member, name, kind) for member, name, kind in members if name == "game" or name.startswith("game/")]
for member, name, kind in sorted(game_members, key=lambda item: (item[1].count("/"), item[1])):
    if kind != "dir":
        continue
    target = destination / name
    target.mkdir(parents=True, exist_ok=True)
    os.chmod(target, member.mode & 0o777)
with tarfile.open(archive, "r:gz") as tar:
    for member, name, kind in game_members:
        if kind != "file":
            continue
        target = destination / name
        target.parent.mkdir(parents=True, exist_ok=True)
        current = tar.getmember(member.name)
        with tar.extractfile(current) as source:
            if source is None:
                raise SystemExit(f"could not read archive member: {name}")
            with target.open("xb") as output:
                for block in iter(lambda: source.read(1024 * 1024), b""):
                    output.write(block)
        os.chmod(target, member.mode & 0o777)

extracted_platform = (destination / platform_member).read_text(encoding="ascii").strip()
if extracted_platform != platform:
    raise SystemExit(f"archive platform marker is {extracted_platform!r}, expected {platform!r}")
new_version = f"v{version[0]}.{version[1]}"
print(f"{new_version}\t{archive.name}")
PY
)" || die "Release archive checksum or safety preflight failed"

IFS=$'\t' read -r NEW_VERSION ARCHIVE_NAME <<< "$selection"
[ -n "$NEW_VERSION" ] && [ -n "$ARCHIVE_NAME" ] || die "Archive preflight returned incomplete metadata"
info "Verified same-platform archive ${ARCHIVE_NAME}"

if [ "$NEW_VERSION" = "$CURRENT_VERSION" ]; then
    info "Already on ${CURRENT_VERSION}; no update is needed"
    exit 0
fi

release_version_is_newer() {
    local left="${1#v}" right="${2#v}"
    local left_date left_sequence right_date right_sequence
    [[ "$left" =~ ^[0-9]{8}\.[0-9]+$ ]] || die "Installed version marker is malformed: $1"
    [[ "$right" =~ ^[0-9]{8}\.[0-9]+$ ]] || die "Release archive version is malformed: $2"
    left_date="${left%%.*}"
    left_sequence="${left#*.}"
    right_date="${right%%.*}"
    right_sequence="${right#*.}"
    if ((10#$left_date != 10#$right_date)); then
        ((10#$left_date > 10#$right_date))
    else
        ((10#$left_sequence > 10#$right_sequence))
    fi
}

if [ "$CURRENT_VERSION" != "dev" ] && release_version_is_newer "$CURRENT_VERSION" "$NEW_VERSION"; then
    die "Refusing downgrade from ${CURRENT_VERSION} to ${NEW_VERSION}"
fi

# Never execute the old generated launcher. Stop only processes whose current
# executable resolves exactly inside the old game tree.
stop_owned_process "${GAME_DIR}/data/nginx.pid" "${GAME_DIR}/bin/nginx"
stop_owned_process "${GAME_DIR}/data/arkham-api.pid" "${GAME_DIR}/bin/arkham-api"

if [ "$CURRENT_VERSION" = "dev" ]; then
    new_release_date="${NEW_VERSION#v}"
    new_release_date="${new_release_date%.*}"
    BACKUP_NAME="game_v${new_release_date}.0"
else
    BACKUP_NAME="game_${CURRENT_VERSION}"
fi
if [ -e "${BASE_DIR}/${BACKUP_NAME}" ] || [ -L "${BASE_DIR}/${BACKUP_NAME}" ]; then
    BACKUP_NAME="${BACKUP_NAME}-$(random_hex | cut -c1-12)"
fi
while [ -e "${BASE_DIR}/${BACKUP_NAME}" ] || [ -L "${BASE_DIR}/${BACKUP_NAME}" ]; do
    BACKUP_NAME="${BACKUP_NAME}-$(random_hex | cut -c1-12)"
done

mv "$GAME_DIR" "${BASE_DIR}/${BACKUP_NAME}" \
    || die "Could not move the current game directory"
rollback() {
    rm -rf "${BASE_DIR}/game"
    mv "${BASE_DIR}/${BACKUP_NAME}" "$GAME_DIR"
}

if ! mv "${EXTRACT_DIR}/game" "$GAME_DIR"; then
    rollback
    die "Could not install the preflighted game directory"
fi
rm -f "${GAME_DIR}"/current_v* 2>/dev/null || true
if ! touch "${GAME_DIR}/current_${NEW_VERSION}"; then
    rollback
    die "Could not write the new version marker"
fi

info "Update complete: ${CURRENT_VERSION} -> ${NEW_VERSION}"
