#!/usr/bin/env bash
# Trusted in-place updater copied into each offline package. It accepts only a
# same-platform archive whose published SHA-256 is supplied out of band, then
# preflights every archive member before changing the installed game tree.

set -euo pipefail

readonly MAX_ARCHIVE_MEMBERS=60000
readonly MAX_ARCHIVE_BYTES=$((2 * 1024 * 1024 * 1024))

die() { printf '[update] error: %s\n' "$*" >&2; exit 1; }
info() { printf '[update] %s\n' "$*"; }

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

runtime_pgdata() {
    [ -n "${HOME:-}" ] || die "HOME is unavailable; cannot locate the PostgreSQL data directory"
    case "$(uname -s)" in
        Darwin) printf '%s\n' "${HOME}/Library/Application Support/ArkhamHorror/pgdata" ;;
        Linux) printf '%s\n' "${HOME}/.local/share/ArkhamHorror/pgdata" ;;
        *) die "Unsupported operating system: $(uname -s)" ;;
    esac
}

release_launcher_names() {
    case "$PLATFORM" in
        linux-*)
            printf '%s\n' \
                "Start-ArkhamHorror.bat" \
                "Update-ArkhamHorror.bat" \
                "Update-ArkhamHorror.sh"
            ;;
        macos-*)
            printf '%s\n' \
                "Start-ArkhamHorror.command" \
                "Update-ArkhamHorror.command" \
                "Update-ArkhamHorror.sh"
            ;;
        *) die "Unsupported package platform: $PLATFORM" ;;
    esac
}

stop_owned_process() {
    local label="$1" pid_file="$2" expected="$3" pid executable attempts=0
    [ -f "$pid_file" ] && [ ! -L "$pid_file" ] || return 0
    pid=""
    if ! IFS= read -r pid < "$pid_file"; then
        [ -n "$pid" ] || die "${label} PID file is unreadable: $pid_file"
    fi
    pid="${pid%$'\r'}"
    case "$pid" in
        ''|*[!0-9]*) die "${label} PID file is unsafe: $pid_file" ;;
    esac
    kill -0 "$pid" 2>/dev/null || return 0
    executable="$(process_executable "$pid")"
    [ -n "$executable" ] || die "Could not inspect live ${label} process ${pid}"
    [ "$executable" = "$expected" ] \
        || die "Refusing to replace the package while ${label} PID ${pid} is owned by ${executable}"
    info "Stopping ${label} process ${pid}"
    if ! kill -TERM "$pid" 2>/dev/null && kill -0 "$pid" 2>/dev/null; then
        die "Could not signal ${label} process ${pid}"
    fi
    while kill -0 "$pid" 2>/dev/null && [ "$attempts" -lt 25 ]; do
        sleep 0.2
        attempts=$((attempts + 1))
    done
    if kill -0 "$pid" 2>/dev/null; then
        executable="$(process_executable "$pid")"
        if [ -z "$executable" ]; then
            info "${label} process ${pid} exited after SIGTERM"
            return 0
        fi
        if [ "$executable" != "$expected" ]; then
            info "${label} process ${pid} exited before its PID was reused"
            return 0
        fi
        info "${label} process ${pid} did not stop after SIGTERM; sending SIGKILL"
        if ! kill -KILL "$pid" 2>/dev/null && kill -0 "$pid" 2>/dev/null; then
            die "Could not kill ${label} process ${pid}"
        fi
        attempts=0
        while kill -0 "$pid" 2>/dev/null && [ "$attempts" -lt 25 ]; do
            sleep 0.2
            attempts=$((attempts + 1))
        done
        if kill -0 "$pid" 2>/dev/null; then
            executable="$(process_executable "$pid")"
            [ -n "$executable" ] || return 0
            die "${label} process ${pid} survived SIGKILL; refusing package replacement"
        fi
    fi
    return 0
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
CURRENT_RELEASE_FILE="${GAME_DIR}/config/release-version"
if [ -e "$CURRENT_RELEASE_FILE" ] || [ -L "$CURRENT_RELEASE_FILE" ]; then
    [ -f "$CURRENT_RELEASE_FILE" ] && [ ! -L "$CURRENT_RELEASE_FILE" ] \
        || die "Installed package version marker is unsafe"
    CURRENT_VERSION="$(cat "$CURRENT_RELEASE_FILE")"
    if [ "$CURRENT_VERSION" != "dev" ] \
        && [[ ! "$CURRENT_VERSION" =~ ^v[0-9]{8}\.(0|[1-9][0-9]*)$ ]]; then
        die "Installed package version marker is malformed: $CURRENT_VERSION"
    fi
else
    marker_count=0
    for marker in "${GAME_DIR}"/current_v[0-9]*; do
        [ -e "$marker" ] || continue
        [ -f "$marker" ] && [ ! -L "$marker" ] \
            || die "Installed legacy version marker is unsafe: $marker"
        CURRENT_VERSION="${marker##*/current_}"
        marker_count=$((marker_count + 1))
    done
    [ "$marker_count" -le 1 ] || die "Installed package has ambiguous legacy version markers"
fi

UPDATE_NODE="${GAME_DIR}/tools/node"
UPDATE_ARCHIVE_HELPER="${GAME_DIR}/tools/update-archive.mjs"
[ -f "$UPDATE_NODE" ] && [ ! -L "$UPDATE_NODE" ] && [ -x "$UPDATE_NODE" ] \
    || die "Bundled updater Node runtime is missing or unsafe"
[ -f "$UPDATE_ARCHIVE_HELPER" ] && [ ! -L "$UPDATE_ARCHIVE_HELPER" ] \
    && [ -r "$UPDATE_ARCHIVE_HELPER" ] \
    || die "Bundled updater archive helper is missing or unsafe"
case "$(uname -s)" in
    Darwin)
        selection="$(
            /usr/bin/env -i \
                DYLD_LIBRARY_PATH="${GAME_DIR}/lib:${GAME_DIR}/pgsql/lib" \
                "$UPDATE_NODE" "$UPDATE_ARCHIVE_HELPER" \
                "$BASE_DIR" "$PLATFORM" "$EXTRACT_DIR" \
                "$MAX_ARCHIVE_MEMBERS" "$MAX_ARCHIVE_BYTES" "$EXPECTED_ARCHIVE_SHA256"
        )" || die "Release archive checksum or safety preflight failed"
        ;;
    Linux)
        selection="$(
            /usr/bin/env -i \
                LD_LIBRARY_PATH="${GAME_DIR}/lib:${GAME_DIR}/pgsql/lib" \
                "$UPDATE_NODE" "$UPDATE_ARCHIVE_HELPER" \
                "$BASE_DIR" "$PLATFORM" "$EXTRACT_DIR" \
                "$MAX_ARCHIVE_MEMBERS" "$MAX_ARCHIVE_BYTES" "$EXPECTED_ARCHIVE_SHA256"
        )" || die "Release archive checksum or safety preflight failed"
        ;;
esac

IFS=$'\t' read -r NEW_VERSION ARCHIVE_NAME <<< "$selection"
[ -n "$NEW_VERSION" ] && [ -n "$ARCHIVE_NAME" ] || die "Archive preflight returned incomplete metadata"
[[ "$NEW_VERSION" =~ ^v[0-9]{8}\.(0|[1-9][0-9]*)$ ]] \
    || die "Archive preflight returned a malformed release version"
[ "$ARCHIVE_NAME" = "ArkhamHorror-${PLATFORM}-${NEW_VERSION}.tar.gz" ] \
    || die "Archive preflight returned an inconsistent release filename"
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
    if [ "$left_date" != "$right_date" ]; then
        [[ "$left_date" > "$right_date" ]]
    elif [ "${#left_sequence}" -ne "${#right_sequence}" ]; then
        [ "${#left_sequence}" -gt "${#right_sequence}" ]
    else
        [[ "$left_sequence" > "$right_sequence" ]]
    fi
}

if [ "$CURRENT_VERSION" != "dev" ] && release_version_is_newer "$CURRENT_VERSION" "$NEW_VERSION"; then
    die "Refusing downgrade from ${CURRENT_VERSION} to ${NEW_VERSION}"
fi

# Never execute the old generated launcher. Stop only processes whose current
# executable resolves exactly inside the old game tree.
stop_owned_process "nginx" "${GAME_DIR}/data/nginx.pid" "${GAME_DIR}/bin/nginx"
stop_owned_process "arkham-api" "${GAME_DIR}/data/arkham-api.pid" "${GAME_DIR}/bin/arkham-api"
PGDATA_DIR="$(runtime_pgdata)"
stop_owned_process "PostgreSQL" "${PGDATA_DIR}/postmaster.pid" "${GAME_DIR}/pgsql/bin/postgres"

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

ROOT_BACKUP_DIR="${WORK_DIR}/root-launchers"
mkdir "$ROOT_BACKUP_DIR" || die "Could not create root-launcher rollback directory"
ROOT_LAUNCHERS=()
while IFS= read -r launcher; do
    ROOT_LAUNCHERS+=("$launcher")
done < <(release_launcher_names)

restore_root_launchers() {
    local index launcher destination backup
    for index in "${!ROOT_LAUNCHERS[@]}"; do
        launcher="${ROOT_LAUNCHERS[$index]}"
        destination="${BASE_DIR}/${launcher}"
        backup="${ROOT_BACKUP_DIR}/${launcher}"
        if [ -f "${ROOT_BACKUP_DIR}/present-${index}" ]; then
            if [ -e "$destination" ] || [ -L "$destination" ]; then
                rm -f -- "$destination" 2>/dev/null || return 1
            fi
            mv "$backup" "$destination" || return 1
        elif [ -f "${ROOT_BACKUP_DIR}/absent-${index}" ]; then
            if [ -e "$destination" ] || [ -L "$destination" ]; then
                rm -f -- "$destination" 2>/dev/null || return 1
            fi
        fi
    done
}

for index in "${!ROOT_LAUNCHERS[@]}"; do
    launcher="${ROOT_LAUNCHERS[$index]}"
    extracted="${EXTRACT_DIR}/${launcher}"
    [ -f "$extracted" ] && [ ! -L "$extracted" ] \
        || die "Preflighted release launcher is missing or unsafe: $launcher"
    destination="${BASE_DIR}/${launcher}"
    if [ -e "$destination" ] || [ -L "$destination" ]; then
        if ! mv "$destination" "${ROOT_BACKUP_DIR}/${launcher}"; then
            restore_root_launchers \
                || die "Could not restore root launchers after a backup failure"
            die "Could not preserve the current root launcher: $launcher"
        fi
        if ! : > "${ROOT_BACKUP_DIR}/present-${index}"; then
            mv "${ROOT_BACKUP_DIR}/${launcher}" "$destination" 2>/dev/null || true
            restore_root_launchers \
                || die "Could not restore root launchers after a rollback-marker failure"
            die "Could not record root-launcher rollback state"
        fi
    else
        if ! : > "${ROOT_BACKUP_DIR}/absent-${index}"; then
            restore_root_launchers \
                || die "Could not restore root launchers after a rollback-marker failure"
            die "Could not record root-launcher rollback state"
        fi
    fi
done

if ! mv "$GAME_DIR" "${BASE_DIR}/${BACKUP_NAME}"; then
    restore_root_launchers || die "Could not restore root launchers after the game backup failed"
    die "Could not move the current game directory"
fi
rollback() {
    rm -rf -- "${BASE_DIR}/game"
    mv "${BASE_DIR}/${BACKUP_NAME}" "$GAME_DIR" \
        || die "Could not restore the previous game directory"
    restore_root_launchers \
        || die "Could not restore the previous root launchers"
}

if ! mv "${EXTRACT_DIR}/game" "$GAME_DIR"; then
    rollback
    die "Could not install the preflighted game directory"
fi
for launcher in "${ROOT_LAUNCHERS[@]}"; do
    if ! mv "${EXTRACT_DIR}/${launcher}" "${BASE_DIR}/${launcher}"; then
        rollback
        die "Could not install the preflighted root launcher: $launcher"
    fi
done

info "Update complete: ${CURRENT_VERSION} -> ${NEW_VERSION}"
