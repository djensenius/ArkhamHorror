#!/usr/bin/env bash
# =============================================================================
# utils.sh - Shared helper library
# Provides logging, system detection, cached downloads, extraction, and toolchain PATH management
# Compatible with macOS and Linux (Ubuntu)
# =============================================================================

set -euo pipefail

# ── Colors ────────────────────────────────────────────────────────────────────
readonly CLR_RESET='\033[0m'
readonly CLR_GREEN='\033[0;32m'
readonly CLR_YELLOW='\033[0;33m'
readonly CLR_RED='\033[0;31m'
readonly CLR_BLUE='\033[0;34m'
readonly CLR_CYAN='\033[0;36m'

# ── Global paths (initialized by init_paths()) ────────────────────────────────
PROJECT_ROOT=""
OFFLINE_DIR=""
DEPS_DIR=""
SCRIPTS_DIR=""
_DIST_DIR=""
GHCUP_DIR=""    # GHC + Stack install directory: _deps/ghcup/
STACK_ROOT_DIR="" # Stack root directory: _deps/stack-root/
TMP_DIR=""      # Download cache + temporary extraction: offline/_tmp/
TOOLCHAIN_LOCK_FILE=""
TOOLCHAIN_RECEIPT_FILE=""
TOOLCHAIN_RECEIPT_TOKEN=""
TOOLCHAIN_RECEIPT_DIR=""
TOOLCHAIN_RECEIPT_OWNED=false
TOOLCHAIN_RECEIPT_ENV_FILE=""

# ── Logging ───────────────────────────────────────────────────────────────────

info()    { printf "${CLR_GREEN}[INFO]${CLR_RESET} %s\n" "$*"; }
step()    { printf "\n${CLR_CYAN}━━━ %s ━━━${CLR_RESET}\n" "$*"; }
warn()    { printf "${CLR_YELLOW}[WARN]${CLR_RESET} %s\n" "$*" >&2; }
die()     { printf "${CLR_RED}[ERROR]${CLR_RESET} %s\n" "$*" >&2; exit 1; }
substep() { printf "  ${CLR_BLUE}→${CLR_RESET} %s\n" "$*"; }

# ── System detection ──────────────────────────────────────────────────────────

detect_os() {
    case "$(uname -s)" in
        Darwin) echo "macos" ;;
        Linux)  echo "linux" ;;
        *)      die "Unsupported operating system: $(uname -s)" ;;
    esac
}

detect_arch() {
    case "$(uname -m)" in
        arm64|aarch64) echo "arm64" ;;
        x86_64|amd64)  echo "x86_64" ;;
        *)             die "Unsupported CPU architecture: $(uname -m)" ;;
    esac
}

detect_platform() {
    echo "$(detect_os)-$(detect_arch)"
}

has_cmd() {
    command -v "$1" >/dev/null 2>&1
}

# ── Safe directory creation for WSL/NTFS ─────────────────────────────────────
# When WSL accesses NTFS through DrvFS, deleting a directory may leave behind ghost handles,
# causing mkdir to report "File exists" even though the directory does not actually exist.
# This helper includes retry logic and works on WSL/NTFS as well as native Linux/macOS.

ensure_dir() {
    local dir="$1"
    local retries=5
    local i=0
    while [ $i -lt $retries ]; do
        if [ -d "$dir" ]; then
            return 0
        fi
        if mkdir -p "$dir" 2>/dev/null; then
            return 0
        fi
        # mkdir failed, but the directory may already exist (WSL/NTFS race condition)
        if [ -d "$dir" ]; then
            return 0
        fi
        i=$((i + 1))
        if [ $i -lt $retries ]; then
            warn "mkdir -p '$dir' failed (attempt ${i}); retrying in 1 second ..."
            sleep 1
        fi
    done
    # Final attempt; exit with an error if it still fails
    mkdir -p "$dir" || die "Unable to create directory: $dir (retried ${retries} times)"
}

# ── Path initialization ──────────────────────────────────────────────────────

consume_authorized_stage_capabilities() {
    local receipt_file receipt_token github_env
    [ "${ARKHAM_AUTHORIZED_STAGE_FD:-}" = "9" ] || return 0
    unset ARKHAM_AUTHORIZED_STAGE_FD
    if ! {
        IFS= read -r receipt_file <&9 \
            && IFS= read -r receipt_token <&9 \
            && IFS= read -r github_env <&9
    } 2>/dev/null; then
        exec 9<&-
        die "Authorized-stage capability frame is missing or incomplete"
    fi
    exec 9<&-
    TOOLCHAIN_RECEIPT_FILE="$receipt_file"
    TOOLCHAIN_RECEIPT_TOKEN="$receipt_token"
    TOOLCHAIN_RECEIPT_ENV_FILE="$github_env"
}

init_paths() {
    SCRIPTS_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
    OFFLINE_DIR="$(dirname "$SCRIPTS_DIR")"
    PROJECT_ROOT="$(dirname "$OFFLINE_DIR")"
    DEPS_DIR="${OFFLINE_DIR}/_deps"
    _DIST_DIR="${OFFLINE_DIR}/_dist"
    GHCUP_DIR="${DEPS_DIR}/ghcup"
    STACK_ROOT_DIR="${DEPS_DIR}/stack-root"
    TMP_DIR="${OFFLINE_DIR}/_tmp"
    TOOLCHAIN_LOCK_FILE="${ARKHAM_TOOLCHAIN_LOCK_FILE:-${OFFLINE_DIR}/toolchain.lock}"
    TOOLCHAIN_RECEIPT_FILE="${ARKHAM_TOOLCHAIN_RECEIPT_FILE:-}"
    TOOLCHAIN_RECEIPT_TOKEN="${ARKHAM_TOOLCHAIN_RECEIPT_TOKEN:-}"
    TOOLCHAIN_RECEIPT_DIR="${ARKHAM_TOOLCHAIN_RECEIPT_DIR:-${OFFLINE_DIR}/_session}"
    TOOLCHAIN_RECEIPT_OWNED=false
    TOOLCHAIN_RECEIPT_ENV_FILE="${GITHUB_ENV:-}"
    # Receipt capabilities are deliberately consumed into non-exported shell
    # variables before any tool/archive/package payload can run.
    unset ARKHAM_TOOLCHAIN_RECEIPT_FILE ARKHAM_TOOLCHAIN_RECEIPT_TOKEN
    # The command-file path is also a capability: retain it only in this
    # non-exported shell variable until the reviewed dependency stage publishes
    # a completed receipt, never while an untrusted tool can inherit it.
    unset GITHUB_ENV
    consume_authorized_stage_capabilities
}

# ── Toolchain authority ──────────────────────────────────────────────────────

sha256_file() {
    local file="$1" result digest
    [ -f "$file" ] && [ ! -L "$file" ] || return 1
    if has_cmd sha256sum; then
        result="$(sha256sum "$file")" || return 1
    elif has_cmd shasum; then
        result="$(shasum -a 256 "$file")" || return 1
    else
        die "Neither sha256sum nor shasum is available"
    fi
    digest="${result%%[[:space:]]*}"
    [[ "$digest" =~ ^[0-9a-f]{64}$ ]] || return 1
    printf '%s\n' "$digest"
}

sha256_text() {
    local result digest
    if has_cmd sha256sum; then
        result="$(sha256sum)" || return 1
    elif has_cmd shasum; then
        result="$(shasum -a 256)" || return 1
    else
        die "Neither sha256sum nor shasum is available"
    fi
    digest="${result%%[[:space:]]*}"
    [[ "$digest" =~ ^[0-9a-f]{64}$ ]] || return 1
    printf '%s\n' "$digest"
}

require_sha256() {
    [[ "${1:-}" =~ ^[0-9a-f]{64}$ ]] || die "Invalid SHA-256 authority: ${1:-<empty>}"
}

file_matches_sha256() {
    local file="$1" expected="$2" actual
    require_sha256 "$expected"
    actual="$(sha256_file "$file")" || return 1
    [ "$actual" = "$expected" ]
}

verify_file_sha256() {
    local file="$1" expected="$2" label="${3:-$1}"
    if ! file_matches_sha256 "$file" "$expected"; then
        local actual="<unreadable>"
        actual="$(sha256_file "$file" 2>/dev/null || printf '%s' "$actual")"
        die "SHA-256 authority check failed for ${label}: expected ${expected}, got ${actual}"
    fi
}

toolchain_lock_path() {
    local lock="${TOOLCHAIN_LOCK_FILE:-${ARKHAM_TOOLCHAIN_LOCK_FILE:-}}"
    [ -n "$lock" ] || die "Toolchain lock path is not initialized; call init_paths first"
    [ -f "$lock" ] && [ ! -L "$lock" ] || die "Toolchain authority file is missing or unsafe: $lock"
    printf '%s\n' "$lock"
}

# Looks up exactly one tab-separated authority record. The lock deliberately
# has exact platform rows, so a new platform cannot accidentally inherit an
# authority intended for a different executable or source archive.
toolchain_lock_record() {
    local record_type="$1" component="$2" platform="$3" artifact="$4"
    local lock record
    lock="$(toolchain_lock_path)"
    record="$(
        awk -F '\t' \
            -v wanted_type="$record_type" \
            -v wanted_component="$component" \
            -v wanted_platform="$platform" \
            -v wanted_artifact="$artifact" '
                /^[[:space:]]*#/ || NF == 0 { next }
                $1 == wanted_type && $2 == wanted_component && $3 == wanted_platform && $4 == wanted_artifact {
                    matches += 1
                    value = $0
                }
                END {
                    if (matches != 1) exit 1
                    print value
                }
            ' "$lock"
    )" || die "Missing or ambiguous ${record_type} authority for ${component}/${platform}/${artifact}"
    printf '%s\n' "$record"
}

toolchain_archive_sha256() {
    local component="$1" platform="$2" archive="$3"
    local record record_type record_component record_platform record_artifact kind digest version
    record="$(toolchain_lock_record archive "$component" "$platform" "$archive")"
    IFS=$'\t' read -r record_type record_component record_platform record_artifact kind digest version <<< "$record"
    [ "$kind" = "exact" ] || die "Archive authority for ${component}/${platform}/${archive} must be exact"
    require_sha256 "$digest"
    [ -n "$version" ] || die "Archive authority is incomplete for ${component}/${platform}/${archive}"
    printf '%s\n' "$digest"
}

# Prints authority-kind, digest, and version/recipe for an installed binary.
toolchain_binary_authority() {
    local component="$1" platform="$2" binary="$3"
    local record record_type record_component record_platform record_artifact kind digest version
    record="$(toolchain_lock_record binary "$component" "$platform" "$binary")"
    IFS=$'\t' read -r record_type record_component record_platform record_artifact kind digest version <<< "$record"
    case "$kind" in
        exact|derived) ;;
        *) die "Unsupported installed-binary authority kind ${kind} for ${component}/${platform}/${binary}" ;;
    esac
    require_sha256 "$digest"
    [ -n "$version" ] || die "Installed-binary authority is incomplete for ${component}/${platform}/${binary}"
    printf '%s\t%s\t%s\n' "$kind" "$digest" "$version"
}

toolchain_lock_digest() {
    local digest
    digest="$(sha256_file "$(toolchain_lock_path)")" || die "Could not hash the toolchain authority file"
    printf '%s\n' "$digest"
}

# A source-built executable is represented by a reproducible input identity,
# not by an impossible cross-compiler byte-for-byte promise. The complete
# declared execution closure is receipted outside the restored cache before
# any cache-resident executable can run.
toolchain_build_identity() {
    local component="$1" platform="$2" version="$3" source_sha256="$4" recipe="$5"
    require_sha256 "$source_sha256"
    printf '%s\n' "$component" "$platform" "$version" "$source_sha256" "$recipe" | sha256_text \
        || die "Could not compute build identity for ${component}/${platform}"
}

nginx_build_recipe() {
    printf '%s\n' "native-build-v2:--with-http_gzip_static_module,--without-http_gzip_module,--without-http_fastcgi_module,--without-http_uwsgi_module,--without-http_scgi_module,--without-http_grpc_module,--without-http_memcached_module,--without-http_empty_gif_module,--without-http_browser_module,--without-http_autoindex_module,--without-http_geo_module,--without-http_split_clients_module,--without-http_referer_module,--without-http_ssi_module,--without-http_userid_module,--without-http_mirror_module,--without-mail_pop3_module,--without-mail_imap_module,--without-mail_smtp_module,--without-stream_access_module"
}

random_hex() {
    local byte_count="${1:-32}" value
    [[ "$byte_count" =~ ^[1-9][0-9]*$ ]] || die "Invalid random byte count: $byte_count"
    [ -r /dev/urandom ] && has_cmd od || die "Secure random-byte source is unavailable"
    value="$(LC_ALL=C od -An -N"$byte_count" -tx1 /dev/urandom | tr -d ' \n')" \
        || die "Could not create an authority receipt token"
    [[ "$value" =~ ^[0-9a-f]+$ ]] || die "Secure random-byte source produced an invalid token"
    printf '%s\n' "$value"
}

require_receipt_token() {
    [[ "${1:-}" =~ ^[0-9a-f]{64}$ ]] || die "Invalid toolchain authority receipt token"
}

toolchain_receipt_path() {
    [ -n "${TOOLCHAIN_RECEIPT_FILE:-}" ] || die "No invocation-specific toolchain receipt is available"
    [ -f "$TOOLCHAIN_RECEIPT_FILE" ] && [ ! -L "$TOOLCHAIN_RECEIPT_FILE" ] \
        || die "Toolchain receipt is missing or unsafe: $TOOLCHAIN_RECEIPT_FILE"
    printf '%s\n' "$TOOLCHAIN_RECEIPT_FILE"
}

toolchain_receipt_header_equals() {
    local key="$1" expected="$2" receipt count actual
    receipt="$(toolchain_receipt_path)"
    count="$(awk -F '\t' -v key="$key" '$1 == key { matches += 1 } END { print matches + 0 }' "$receipt")"
    [ "$count" = "1" ] || die "Toolchain receipt has no unique ${key} field"
    actual="$(awk -F '\t' -v key="$key" '$1 == key { print $2 }' "$receipt")"
    [ "$actual" = "$expected" ] || die "Toolchain receipt has an unexpected ${key} field"
}

toolchain_receipt_token_digest() {
    require_receipt_token "$TOOLCHAIN_RECEIPT_TOKEN"
    printf '%s' "$TOOLCHAIN_RECEIPT_TOKEN" | sha256_text
}

# The token is never persisted with the receipt. Each mutable record carries a
# secret-suffixed SHA-256 authenticator instead, so a cache payload that finds
# the receipt path cannot rewrite an authority record it can later satisfy.
toolchain_receipt_record_authenticator() {
    local component="$1" lock_sha256="$2" identity="$3" closure_sha256="$4"
    safe_authority_component "$component"
    require_sha256 "$lock_sha256"
    require_sha256 "$identity"
    require_sha256 "$closure_sha256"
    require_receipt_token "$TOOLCHAIN_RECEIPT_TOKEN"
    printf 'record\t%s\t%s\t%s\t%s\t%s' \
        "$component" "$lock_sha256" "$identity" "$closure_sha256" "$TOOLCHAIN_RECEIPT_TOKEN" | sha256_text
}

# A receipt is created after cache restoration, in an invocation-unique
# directory, and its unguessable token is passed out-of-band through the
# current build environment. The token itself is not stored in the receipt:
# cached manifests and receipt bytes are observations only, never an authority
# root.
init_toolchain_authority_receipt() {
    local receipt_dir receipt_directory_token
    if [ -n "${TOOLCHAIN_RECEIPT_FILE:-}" ] || [ -n "${TOOLCHAIN_RECEIPT_TOKEN:-}" ]; then
        [ -n "${TOOLCHAIN_RECEIPT_FILE:-}" ] && [ -n "${TOOLCHAIN_RECEIPT_TOKEN:-}" ] \
            || die "Toolchain receipt path and token must be supplied together"
        require_receipt_token "$TOOLCHAIN_RECEIPT_TOKEN"
        toolchain_receipt_header_equals schema "2"
        toolchain_receipt_header_equals token_sha256 "$(toolchain_receipt_token_digest)"
        return 0
    fi

    [ ! -L "$TOOLCHAIN_RECEIPT_DIR" ] || die "Toolchain receipt directory must not be a symlink"
    ensure_dir "$TOOLCHAIN_RECEIPT_DIR"
    TOOLCHAIN_RECEIPT_TOKEN="$(random_hex)"
    # The receipt location is not the capability. Keep its random directory
    # token independent from the record-authentication secret so a payload
    # that can enumerate the shared parent cannot recover the secret merely
    # from the receipt path.
    receipt_directory_token="$(random_hex)"
    receipt_dir="${TOOLCHAIN_RECEIPT_DIR}/receipt-${receipt_directory_token}"
    [ ! -e "$receipt_dir" ] && [ ! -L "$receipt_dir" ] \
        || die "Refusing to reuse a pre-existing toolchain receipt directory: $receipt_dir"
    (umask 077 && mkdir "$receipt_dir") || die "Could not create toolchain receipt directory"
    TOOLCHAIN_RECEIPT_FILE="${receipt_dir}/receipt.tsv"
    printf 'schema\t2\ntoken_sha256\t%s\n' "$(toolchain_receipt_token_digest)" > "$TOOLCHAIN_RECEIPT_FILE"
    chmod 600 "$TOOLCHAIN_RECEIPT_FILE"
    TOOLCHAIN_RECEIPT_OWNED=true
}

# Publish a newly completed receipt only after the dependency stage has stopped
# launching untrusted archives and installed binaries. The GitHub command-file
# path was consumed by init_paths and is never inherited by those processes.
publish_toolchain_authority_receipt() {
    [ "$TOOLCHAIN_RECEIPT_OWNED" = true ] || return 0
    [ -n "$TOOLCHAIN_RECEIPT_ENV_FILE" ] || return 0
    [ -f "$TOOLCHAIN_RECEIPT_ENV_FILE" ] && [ ! -L "$TOOLCHAIN_RECEIPT_ENV_FILE" ] \
        && [ -w "$TOOLCHAIN_RECEIPT_ENV_FILE" ] \
        || die "GitHub environment file is missing or unsafe"
    require_toolchain_authority_receipt
    {
        printf 'ARKHAM_TOOLCHAIN_RECEIPT_FILE=%s\n' "$TOOLCHAIN_RECEIPT_FILE"
        printf 'ARKHAM_TOOLCHAIN_RECEIPT_TOKEN=%s\n' "$TOOLCHAIN_RECEIPT_TOKEN"
    } >> "$TOOLCHAIN_RECEIPT_ENV_FILE"
    TOOLCHAIN_RECEIPT_ENV_FILE=""
}

require_toolchain_authority_receipt() {
    [ -n "${TOOLCHAIN_RECEIPT_TOKEN:-}" ] || die "No invocation-specific toolchain authority receipt; run 01-check-project-deps.sh in this build"
    require_receipt_token "$TOOLCHAIN_RECEIPT_TOKEN"
    toolchain_receipt_header_equals schema "2"
    toolchain_receipt_header_equals token_sha256 "$(toolchain_receipt_token_digest)"
}

safe_authority_component() {
    [[ "$1" =~ ^[a-z0-9-]+$ ]] || die "Unsafe authority component name: $1"
}

file_mode() {
    local file="$1" mode
    mode="$(stat -c '%a' "$file" 2>/dev/null || stat -f '%Lp' "$file" 2>/dev/null)" \
        || return 1
    [[ "$mode" =~ ^[0-7]{3,4}$ ]] || return 1
    printf '%s\n' "$mode"
}

# Resolves a symlink without allowing its final target to leave `root`.
# Callers record the literal link separately and add the resolved target to
# the closure, so an in-root alias cannot omit the bytes it eventually loads.
resolve_internal_link() {
    local root="$1" path="$2" target next_dir hops=0
    root="$(cd -P "$root" && pwd)" \
        || die "Authority tree root is missing or unsafe: $root"
    [ -d "$root" ] && [ ! -L "$root" ] || die "Authority tree root is missing or unsafe: $root"
    while [ -L "$path" ]; do
        hops=$((hops + 1))
        [ "$hops" -le 64 ] || die "Authority symlink chain is cyclic or too deep: $path"
        target="$(readlink "$path")" || die "Could not read authority symlink: $path"
        case "$target" in
            ""|/*|*$'\t'*|*$'\n'*) die "Unsafe authority symlink target: $path" ;;
        esac
        next_dir="$(cd -P "$(dirname "$path")" && cd -P "$(dirname "$target")" && pwd)" \
            || die "Broken authority symlink target: $path"
        path="${next_dir}/$(basename "$target")"
        path="$(cd -P "$(dirname "$path")" && printf '%s/%s\n' "$(pwd)" "$(basename "$path")")" \
            || die "Broken authority symlink target: $path"
        case "$path" in
            "$root"/*) ;;
            *) die "Authority symlink escapes its root: $path" ;;
        esac
    done
    [ -f "$path" ] || [ -d "$path" ] || die "Authority symlink is dangling: $path"
    printf '%s\n' "$path"
}

validate_internal_link() {
    resolve_internal_link "$@" >/dev/null
}

verify_internal_symlink() {
    local root="$1" relative="$2" expected_target="$3" path actual
    safe_manifest_relative_path "$relative" || die "Unsafe symlink path: $relative"
    path="${root}/${relative}"
    [ -L "$path" ] || die "Expected authority symlink is missing: $path"
    actual="$(readlink "$path")" || die "Could not read authority symlink: $path"
    [ "$actual" = "$expected_target" ] \
        || die "Authority symlink target changed: $path"
    validate_internal_link "$root" "$path"
}

authority_path_record() {
    local root="$1" relative="$2" allow_links="$3"
    local path="${root}/${relative}" link_target digest mode
    safe_manifest_relative_path "$relative" || die "Unsafe authority tree path: $relative"
    case "$relative" in *$'\t'*|*$'\n'*) die "Unsupported authority tree path: $relative" ;; esac
    if [ -L "$path" ]; then
        [ "$allow_links" = "true" ] || die "Authority tree contains a forbidden symlink: $relative"
        link_target="$(readlink "$path")" || die "Could not read authority symlink: $path"
        validate_internal_link "$root" "$path"
        printf 'link\t%s\t%s\n' "$relative" "$link_target"
    elif [ -f "$path" ]; then
        digest="$(sha256_file "$path")" || die "Could not hash authority-tree file: $path"
        mode="$(file_mode "$path")" || die "Could not read authority-tree mode: $path"
        printf 'file\t%s\t%s\t%s\n' "$relative" "$mode" "$digest"
    elif [ -d "$path" ]; then
        mode="$(file_mode "$path")" || die "Could not read authority-tree directory mode: $path"
        printf 'dir\t%s\t%s\n' "$relative" "$mode"
    else
        die "Authority tree contains an unsupported file type: $path"
    fi
}

# Frontend output is shipped as ordinary files only. A link can change what a
# web server exposes after attestation, so it is rejected rather than followed.
authority_tree_records_shell() {
    local root="$1" entry relative
    [ -d "$root" ] && [ ! -L "$root" ] || die "Authority tree root is missing or unsafe: $root"
    while IFS= read -r entry; do
        relative="${entry#./}"
        authority_path_record "$root" "$relative" false
    done < <(
        cd "$root" \
            && find . -mindepth 1 -print | LC_ALL=C sort
    )
}

authority_records_python() {
    local mode="$1"
    shift
    local authority_python="/usr/bin/python3"
    local authority_script="${SCRIPTS_DIR}/authority-tree.py"
    [ -x "$authority_python" ] || return 1
    [ -f "$authority_script" ] && [ ! -L "$authority_script" ] \
        || die "Authority-tree verifier is missing or unsafe: $authority_script"
    "$authority_python" "$authority_script" "$mode" "$@"
}

authority_tree_records() {
    if [ -x /usr/bin/python3 ]; then
        authority_records_python tree "$@"
    else
        authority_tree_records_shell "$@"
    fi
}

authority_tree_digest() {
    local root="$1" records
    records="$(authority_tree_records "$root")" || return 1
    [ -n "$records" ] || die "Authority tree is empty: $root"
    printf '%s\n' "$records" | sha256_text
}

# Like authority_tree_digest, but only for explicitly named files/directories.
# Tool/package closures may contain archive-provided symlinks, but every link
# is canonicalized and required to resolve inside the selected root.
authority_paths_records_shell() {
    local root="$1"
    shift
    local selected entry relative path resolved resolved_relative
    local seen=$'\n'
    local cursor=0
    local -a pending=("$@")
    local -a entries=()
    [ $# -gt 0 ] || die "No authority paths were supplied"
    root="$(cd -P "$root" && pwd)" \
        || die "Authority tree root is missing or unsafe: $root"
    [ -d "$root" ] && [ ! -L "$root" ] || die "Authority tree root is missing or unsafe: $root"
    while [ "$cursor" -lt "${#pending[@]}" ]; do
        selected="${pending[$cursor]}"
        cursor=$((cursor + 1))
        safe_manifest_relative_path "$selected" || die "Unsafe authority path: $selected"
        case "$selected" in *$'\t'*|*$'\n'*) die "Unsafe authority path: $selected" ;; esac
        case "$seen" in *$'\n'"$selected"$'\n'*) continue ;; esac
        seen+="${selected}"$'\n'
        path="${root}/${selected}"
        if [ -d "$path" ] && [ ! -L "$path" ]; then
            while IFS= read -r entry; do
                relative="${entry#./}"
                entries+=("$relative")
                if [ -L "${root}/${relative}" ]; then
                    resolved="$(resolve_internal_link "$root" "${root}/${relative}")"
                    resolved_relative="${resolved#"$root"/}"
                    safe_manifest_relative_path "$resolved_relative" \
                        || die "Authority symlink has an unsafe resolved target: ${root}/${relative}"
                    pending+=("$resolved_relative")
                fi
            done < <(cd "$root" && find "./${selected}" -print)
        elif [ -f "$path" ] || [ -L "$path" ]; then
            entries+=("$selected")
            if [ -L "$path" ]; then
                resolved="$(resolve_internal_link "$root" "$path")"
                resolved_relative="${resolved#"$root"/}"
                safe_manifest_relative_path "$resolved_relative" \
                    || die "Authority symlink has an unsafe resolved target: $path"
                pending+=("$resolved_relative")
            fi
        else
            die "Required authority path is missing: $path"
        fi
    done
    printf '%s\n' "${entries[@]}" | LC_ALL=C sort -u | while IFS= read -r entry; do
        [ -n "$entry" ] || continue
        relative="$entry"
        authority_path_record "$root" "$relative" true
    done
}

authority_paths_records() {
    if [ -x /usr/bin/python3 ]; then
        authority_records_python paths "$@"
    else
        authority_paths_records_shell "$@"
    fi
}

authority_paths_digest() {
    local root="$1"
    shift
    local records
    records="$(authority_paths_records "$root" "$@")" || return 1
    [ -n "$records" ] || die "Authority path set is empty: $root"
    printf '%s\n' "$records" | sha256_text
}

record_authority_receipt() {
    local component="$1" identity="$2" closure_sha256="$3"
    local receipt existing existing_lock existing_identity existing_closure existing_authenticator authenticator extra
    safe_authority_component "$component"
    require_sha256 "$identity"
    require_sha256 "$closure_sha256"
    require_toolchain_authority_receipt
    receipt="$(toolchain_receipt_path)"
    existing="$(awk -F '\t' -v component="$component" '$1 == "record" && $2 == component { matches += 1; value = $0 } END { if (matches == 1) print value; else if (matches > 1) exit 1 }' "$receipt")" \
        || die "Toolchain receipt has duplicate records for ${component}"
    if [ -n "$existing" ]; then
        IFS=$'\t' read -r _ _ existing_lock existing_identity existing_closure existing_authenticator extra <<< "$existing"
        [ -z "$extra" ] || die "Toolchain receipt has a malformed record for ${component}"
        authenticator="$(toolchain_receipt_record_authenticator \
            "$component" "$existing_lock" "$existing_identity" "$existing_closure")"
        [ "$existing_authenticator" = "$authenticator" ] \
            || die "Toolchain receipt has an unauthenticated record for ${component}"
        [ "$existing_lock" = "$(toolchain_lock_digest)" ] \
            && [ "$existing_identity" = "$identity" ] \
            && [ "$existing_closure" = "$closure_sha256" ] \
            && return 0
        die "Toolchain receipt already has a different record for ${component}"
    fi
    authenticator="$(toolchain_receipt_record_authenticator \
        "$component" "$(toolchain_lock_digest)" "$identity" "$closure_sha256")"
    printf 'record\t%s\t%s\t%s\t%s\t%s\n' \
        "$component" "$(toolchain_lock_digest)" "$identity" "$closure_sha256" "$authenticator" >> "$receipt"
}

authority_receipt_closure() {
    local component="$1" identity="$2" receipt record record_type recorded_component lock_sha256 recorded_identity closure_sha256 authenticator extra
    safe_authority_component "$component"
    require_sha256 "$identity"
    require_toolchain_authority_receipt
    receipt="$(toolchain_receipt_path)"
    record="$(
        awk -F '\t' -v component="$component" '
            $1 == "record" && $2 == component {
                matches += 1
                value = $0
            }
            END {
                if (matches != 1) exit 1
                print value
            }
        ' "$receipt"
    )" || return 1
    IFS=$'\t' read -r record_type recorded_component lock_sha256 recorded_identity closure_sha256 authenticator extra <<< "$record"
    [ -z "$extra" ] || return 1
    require_sha256 "$lock_sha256"
    require_sha256 "$recorded_identity"
    require_sha256 "$closure_sha256"
    require_sha256 "$authenticator"
    [ "$lock_sha256" = "$(toolchain_lock_digest)" ] || return 1
    [ "$recorded_identity" = "$identity" ] || return 1
    [ "$authenticator" = "$(toolchain_receipt_record_authenticator \
        "$recorded_component" "$lock_sha256" "$recorded_identity" "$closure_sha256")" ] || return 1
    printf '%s\n' "$closure_sha256"
}

authority_receipt_identity() {
    local component="$1" receipt record record_type recorded_component lock_sha256 identity closure_sha256 authenticator extra
    safe_authority_component "$component"
    require_toolchain_authority_receipt
    receipt="$(toolchain_receipt_path)"
    record="$(
        awk -F '\t' -v component="$component" '
            $1 == "record" && $2 == component {
                matches += 1
                value = $0
            }
            END {
                if (matches != 1) exit 1
                print value
            }
        ' "$receipt"
    )" || return 1
    IFS=$'\t' read -r record_type recorded_component lock_sha256 identity closure_sha256 authenticator extra <<< "$record"
    [ -z "$extra" ] || return 1
    require_sha256 "$lock_sha256"
    require_sha256 "$identity"
    require_sha256 "$closure_sha256"
    require_sha256 "$authenticator"
    [ "$lock_sha256" = "$(toolchain_lock_digest)" ] || return 1
    [ "$authenticator" = "$(toolchain_receipt_record_authenticator \
        "$recorded_component" "$lock_sha256" "$identity" "$closure_sha256")" ] || return 1
    printf '%s\n' "$identity"
}

verify_authority_tree() {
    local component="$1" identity="$2" root="$3" expected actual
    expected="$(authority_receipt_closure "$component" "$identity")" || return 1
    actual="$(authority_tree_digest "$root")" || return 1
    [ "$actual" = "$expected" ]
}

verify_authority_tree_from_receipt() {
    local component="$1" root="$2" receipt record record_type recorded_component lock_sha256 identity closure_sha256 authenticator extra actual
    safe_authority_component "$component"
    require_toolchain_authority_receipt
    receipt="$(toolchain_receipt_path)"
    record="$(
        awk -F '\t' -v component="$component" '
            $1 == "record" && $2 == component {
                matches += 1
                value = $0
            }
            END {
                if (matches != 1) exit 1
                print value
            }
        ' "$receipt"
    )" || return 1
    IFS=$'\t' read -r record_type recorded_component lock_sha256 identity closure_sha256 authenticator extra <<< "$record"
    [ -z "$extra" ] || return 1
    require_sha256 "$lock_sha256"
    require_sha256 "$identity"
    require_sha256 "$closure_sha256"
    require_sha256 "$authenticator"
    [ "$lock_sha256" = "$(toolchain_lock_digest)" ] || return 1
    [ "$authenticator" = "$(toolchain_receipt_record_authenticator \
        "$recorded_component" "$lock_sha256" "$identity" "$closure_sha256")" ] || return 1
    actual="$(authority_tree_digest "$root")" || return 1
    [ "$actual" = "$closure_sha256" ]
}

verify_authority_paths() {
    local component="$1" identity="$2" root="$3"
    shift 3
    local expected actual
    expected="$(authority_receipt_closure "$component" "$identity")" || return 1
    actual="$(authority_paths_digest "$root" "$@")" || return 1
    [ "$actual" = "$expected" ]
}

manifest_path_for_component() {
    local component="$1"
    safe_authority_component "$component"
    printf '%s\n' "${DEPS_DIR}/.toolchain-authority/${component}-${PLATFORM}.manifest"
}

safe_manifest_relative_path() {
    local path="$1"
    case "$path" in
        ""|/*|*/|.|..|../*|*/../*|*/..|*//*|*/./*) return 1 ;;
    esac
    return 0
}

# Writes an execution-closure observation after construction from a verified
# archive or source build. The authoritative source archive digest covers each
# complete extracted vendor tree; the declared paths add the locally created
# launchers, resolved links, runtime directories, and interpreter subtree that
# later scripts execute. The observation is retained in the cache for
# diagnostics, but is never read as an authority root.
write_install_manifest() {
    local component="$1" root="$2" identity="$3" primary="$4"
    shift 4
    local authority kind expected_identity authority_version manifest manifest_dir partial closure_sha256

    [ $# -gt 0 ] || die "No execution-closure paths were supplied for ${component}"
    safe_manifest_relative_path "$primary" || die "Unsafe primary binary path: $primary"
    authority="$(toolchain_binary_authority "$component" "$PLATFORM" "$primary")"
    IFS=$'\t' read -r kind expected_identity authority_version <<< "$authority"
    [ "$identity" = "$expected_identity" ] || die "Build identity does not match the committed authority for ${component}/${PLATFORM}"
    [ -f "${root}/${primary}" ] && [ ! -L "${root}/${primary}" ] && [ -x "${root}/${primary}" ] \
        || die "Expected installed executable is missing or unsafe: ${root}/${primary}"
    closure_sha256="$(authority_paths_digest "$root" "$@")" \
        || die "Could not calculate ${component} execution closure"

    manifest="$(manifest_path_for_component "$component")"
    manifest_dir="$(dirname "$manifest")"
    ensure_dir "$manifest_dir"
    partial="${manifest}.partial.$$"
    rm -f "$partial"

    {
        printf 'schema\t2\n'
        printf 'component\t%s\n' "$component"
        printf 'platform\t%s\n' "$PLATFORM"
        printf 'lock_sha256\t%s\n' "$(toolchain_lock_digest)"
        printf 'authority_kind\t%s\n' "$kind"
        printf 'authority_sha256\t%s\n' "$expected_identity"
        printf 'authority_version\t%s\n' "$authority_version"
        printf 'primary\t%s\n' "$primary"
        printf 'closure_sha256\t%s\n' "$closure_sha256"
    } > "$partial"
    mv -f "$partial" "$manifest"
    printf '%s\n' "$closure_sha256"
}

# Validates the complete current execution tree against a receipt written
# outside the restored cache during this invocation. The co-located manifest
# above is intentionally not read here.
verify_install_manifest() {
    local component="$1" root="$2" identity="$3" primary="$4"
    shift 4
    local authority kind expected_identity authority_version

    [ $# -gt 0 ] || die "No execution-closure paths were supplied for ${component}"
    safe_manifest_relative_path "$primary" || die "Unsafe primary binary path: $primary"
    authority="$(toolchain_binary_authority "$component" "$PLATFORM" "$primary")"
    IFS=$'\t' read -r kind expected_identity authority_version <<< "$authority"
    [ "$identity" = "$expected_identity" ] || die "Build identity does not match the committed authority for ${component}/${PLATFORM}"

    [ -f "${root}/${primary}" ] && [ ! -L "${root}/${primary}" ] && [ -x "${root}/${primary}" ] \
        || die "Cached installed executable is missing or unsafe: ${root}/${primary}"
    verify_authority_paths "$component" "$identity" "$root" "$@" \
        || die "Cached ${component} execution closure does not match this invocation's authority receipt"

    if [ "$kind" = "exact" ]; then
        verify_file_sha256 "${root}/${primary}" "$expected_identity" "cached ${component} binary"
    fi
}

verify_binary_version_contains() {
    local component="$1" binary="$2" version_flag="$3" expected="$4"
    local output
    output="$("$binary" "$version_flag" 2>&1)" || die "${component} failed its post-identity version check"
    case "$output" in
        *"$expected"*) ;;
        *) die "${component} version check did not contain '${expected}': ${output}" ;;
    esac
}

# ── Cached downloads (every cache hit is verified against the lock) ──────────

# download_cached URL archive_name expected_sha256
#   Check whether the file already exists under _tmp/, validate it, then skip
#   the download. A corrupted cache is removed and redownloaded atomically.
#   Returns: full file path
# Note: all logs go to stderr (>&2), and only the file path is printed to stdout.
# This keeps logs visible when called inside $() command substitution without polluting the return value.
download_cached() {
    local url="$1"
    local filename="$2"
    local expected_sha256="${3:-}"
    local cache_path="${TMP_DIR}/${filename}"
    local partial_path="${cache_path}.partial.$$"

    require_sha256 "$expected_sha256"
    ensure_dir "$TMP_DIR"

    if [ -e "$cache_path" ] || [ -L "$cache_path" ]; then
        if ! file_matches_sha256 "$cache_path" "$expected_sha256"; then
            warn "Cached download failed SHA-256 verification; removing: _tmp/${filename}"
            rm -f "$cache_path" || die "Could not remove invalid cached download: $cache_path"
        fi
    fi

    if file_matches_sha256 "$cache_path" "$expected_sha256"; then
        printf "${CLR_GREEN}[INFO]${CLR_RESET} Using cache: _tmp/${filename}\n" >&2
        printf "  ${CLR_BLUE}→${CLR_RESET} (SHA-256 verified cache hit)\n" >&2
    else
        # Print the URL and destination before downloading
        printf "${CLR_GREEN}[INFO]${CLR_RESET} Downloading: ${filename}\n" >&2
        printf "  ${CLR_BLUE}→${CLR_RESET} Download URL: $url\n" >&2
        printf "  ${CLR_BLUE}→${CLR_RESET} Save path: offline/_tmp/${filename}\n" >&2
        rm -f "$partial_path"
        curl -fSL --connect-timeout 30 --max-time 600 --progress-bar \
            -o "$partial_path" "$url" || { rm -f "$partial_path"; die "Download failed: $url"; }
        if ! file_matches_sha256 "$partial_path" "$expected_sha256"; then
            rm -f "$partial_path"
            die "Downloaded bytes failed SHA-256 authority for ${filename}"
        fi
        mv -f "$partial_path" "$cache_path"
        printf "${CLR_GREEN}[INFO]${CLR_RESET} ✓ Download complete: ${filename}\n" >&2
    fi

    verify_file_sha256 "$cache_path" "$expected_sha256" "download cache ${filename}"
    echo "$cache_path"
}

# ── Extraction (with logging) ────────────────────────────────────────────────

extract_tgz() {
    local archive="$1"; local dest="$2"; local strip="${3:-0}"; local expected_sha256="${4:-}"
    require_sha256 "$expected_sha256"
    verify_file_sha256 "$archive" "$expected_sha256" "archive before tgz extraction"
    substep "Extract tgz: $(basename "$archive") → ${dest}"
    ensure_dir "$dest"
    tar -xzf "$archive" -C "$dest" ${strip:+--strip-components="$strip"}
}

extract_txz() {
    local archive="$1"; local dest="$2"; local strip="${3:-0}"; local expected_sha256="${4:-}"
    require_sha256 "$expected_sha256"
    verify_file_sha256 "$archive" "$expected_sha256" "archive before txz extraction"
    substep "Extract txz: $(basename "$archive") → ${dest}"
    ensure_dir "$dest"
    tar -xJf "$archive" -C "$dest" ${strip:+--strip-components="$strip"}
}

extract_tbz() {
    local archive="$1"; local dest="$2"; local strip="${3:-0}"; local expected_sha256="${4:-}"
    require_sha256 "$expected_sha256"
    verify_file_sha256 "$archive" "$expected_sha256" "archive before tbz extraction"
    substep "Extract tbz: $(basename "$archive") → ${dest}"
    ensure_dir "$dest"
    tar -xjf "$archive" -C "$dest" ${strip:+--strip-components="$strip"}
}

extract_zip() {
    local archive="$1"; local dest="$2"; local expected_sha256="${3:-}"
    require_sha256 "$expected_sha256"
    verify_file_sha256 "$archive" "$expected_sha256" "archive before zip extraction"
    substep "Extract zip: $(basename "$archive") → ${dest}"
    ensure_dir "$dest"
    unzip -qo "$archive" -d "$dest"
}

# Create a dedicated subdirectory for each extraction under _tmp/extract/
prepare_extract_dir() {
    local name="$1"
    local dir="${TMP_DIR}/extract/${name}"
    rm -rf "$dir"
    ensure_dir "$dir"
    echo "$dir"
}

# Clean up the extraction directory used by the current operation
cleanup_extract_dir() {
    local dir="$1"
    [ -d "$dir" ] && rm -rf "$dir" || true
}

# ── Installation logging ─────────────────────────────────────────────────────

# Print the installation target path in a consistent format
install_log() {
    local component="$1"
    local target="$2"
    info "  ✓ ${component} installed to: ${target}"
}

# ── Verification ─────────────────────────────────────────────────────────────

verify_cmd() {
    local name="$1" cmd="$2" flag="${3:---version}"
    substep "Verify $name ..."
    if "$cmd" "$flag" >/dev/null 2>&1; then
        info "  ✓ $name is available: $("$cmd" "$flag" 2>&1 | head -1)"
        return 0
    else
        die "  ✗ $name is not available; please check the installation"
    fi
}

# ── Activate toolchain PATH ──────────────────────────────────────────────────

source_ghcup_env() {
    if [ -f "${GHCUP_DIR}/env" ]; then
        source "${GHCUP_DIR}/env" 2>/dev/null || true
        return 0
    fi
    if [ -d "${GHCUP_DIR}/bin" ]; then
        export PATH="${GHCUP_DIR}/bin:${PATH}"
    fi
    return 0
}

activate_deps_path() {
    source_ghcup_env || true
    if [ -n "${STACK_ROOT_DIR:-}" ]; then
        ensure_dir "${STACK_ROOT_DIR}"
        export STACK_ROOT="${STACK_ROOT_DIR}"
    fi
    if [ -d "${DEPS_DIR}/node/bin" ]; then
        export PATH="${DEPS_DIR}/node/bin:${PATH}"
    fi
    if [ -d "${DEPS_DIR}/postgres/bin" ]; then
        export PATH="${DEPS_DIR}/postgres/bin:${PATH}"
    fi
    return 0
}

# ── Debug mode (triggered by build_all.sh --verbose) ─────────────────────────
# Child scripts inherit this automatically after sourcing utils.sh: if ARKHAM_TRACE=true, enable set -x

if [ "${ARKHAM_TRACE:-}" = "true" ]; then
    set -x
fi
