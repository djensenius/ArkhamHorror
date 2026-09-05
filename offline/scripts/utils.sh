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
# not by an impossible cross-compiler byte-for-byte promise. Its local raw
# executable SHA-256 is recorded atomically in an installation manifest and
# checked before cache-hit execution.
toolchain_build_identity() {
    local component="$1" platform="$2" version="$3" source_sha256="$4" recipe="$5"
    require_sha256 "$source_sha256"
    printf '%s\n' "$component" "$platform" "$version" "$source_sha256" "$recipe" | sha256_text \
        || die "Could not compute build identity for ${component}/${platform}"
}

nginx_build_recipe() {
    printf '%s\n' "native-build-v2:--with-http_gzip_static_module,--without-http_gzip_module,--without-http_fastcgi_module,--without-http_uwsgi_module,--without-http_scgi_module,--without-http_grpc_module,--without-http_memcached_module,--without-http_empty_gif_module,--without-http_browser_module,--without-http_autoindex_module,--without-http_geo_module,--without-http_split_clients_module,--without-http_referer_module,--without-http_ssi_module,--without-http_userid_module,--without-http_mirror_module,--without-mail_pop3_module,--without-mail_imap_module,--without-mail_smtp_module,--without-stream_access_module"
}

manifest_path_for_component() {
    local component="$1"
    [[ "$component" =~ ^[a-z0-9-]+$ ]] || die "Unsafe toolchain component name: $component"
    printf '%s\n' "${DEPS_DIR}/.toolchain-authority/${component}-${PLATFORM}.manifest"
}

safe_manifest_relative_path() {
    local path="$1"
    case "$path" in
        ""|/*|*/|.|..|../*|*/../*|*/..|*//*|*/./*) return 1 ;;
    esac
    return 0
}

manifest_header_equals() {
    local manifest="$1" key="$2" expected="$3" actual count
    count="$(awk -F '\t' -v key="$key" '$1 == key { matches += 1 } END { print matches + 0 }' "$manifest")"
    [ "$count" = "1" ] || die "Toolchain manifest ${manifest} has no unique ${key} field"
    actual="$(awk -F '\t' -v key="$key" '$1 == key { print $2 }' "$manifest")"
    [ "$actual" = "$expected" ] || die "Toolchain manifest ${manifest} has an unexpected ${key} field"
}

# Stores raw file/link identities only after an executable was installed from a
# verified authority. `primary` names the binary represented by the committed
# authority row; the rest are transitively required launchers/binaries.
write_install_manifest() {
    local component="$1" root="$2" identity="$3" primary="$4"
    shift 4
    local authority kind expected_identity authority_version manifest manifest_dir partial
    local relative path link_target

    [ $# -gt 0 ] || die "No installed files were supplied for ${component}"
    safe_manifest_relative_path "$primary" || die "Unsafe primary binary path: $primary"
    authority="$(toolchain_binary_authority "$component" "$PLATFORM" "$primary")"
    IFS=$'\t' read -r kind expected_identity authority_version <<< "$authority"
    [ "$identity" = "$expected_identity" ] || die "Build identity does not match the committed authority for ${component}/${PLATFORM}"

    manifest="$(manifest_path_for_component "$component")"
    manifest_dir="$(dirname "$manifest")"
    ensure_dir "$manifest_dir"
    partial="${manifest}.partial.$$"
    rm -f "$partial"

    {
        printf 'schema\t1\n'
        printf 'component\t%s\n' "$component"
        printf 'platform\t%s\n' "$PLATFORM"
        printf 'lock_sha256\t%s\n' "$(toolchain_lock_digest)"
        printf 'authority_kind\t%s\n' "$kind"
        printf 'authority_sha256\t%s\n' "$expected_identity"
        printf 'authority_version\t%s\n' "$authority_version"
        printf 'primary\t%s\n' "$primary"
        for relative in "$@"; do
            safe_manifest_relative_path "$relative" || die "Unsafe installed path in ${component} manifest: $relative"
            path="${root}/${relative}"
            if [ -L "$path" ]; then
                link_target="$(readlink "$path")" || die "Could not read installed symlink: $path"
                [ -n "$link_target" ] && [[ "$link_target" != /* ]] || die "Unsafe installed symlink: $path"
                printf 'link\t%s\t%s\n' "$relative" "$link_target"
            elif [ -f "$path" ]; then
                printf 'file\t%s\t%s\n' "$relative" "$(sha256_file "$path")"
            else
                die "Expected installed file is missing or unsafe: $path"
            fi
        done
    } > "$partial"
    mv -f "$partial" "$manifest"
}

# Validates a cached installation without executing a cached binary. This is
# the prerequisite for every version/configuration command below.
verify_install_manifest() {
    local component="$1" root="$2" identity="$3" primary="$4"
    shift 4
    local authority kind expected_identity authority_version manifest record_count
    local relative record_type recorded_path recorded_identity extra path actual

    [ $# -gt 0 ] || die "No installed files were supplied for ${component}"
    safe_manifest_relative_path "$primary" || die "Unsafe primary binary path: $primary"
    authority="$(toolchain_binary_authority "$component" "$PLATFORM" "$primary")"
    IFS=$'\t' read -r kind expected_identity authority_version <<< "$authority"
    [ "$identity" = "$expected_identity" ] || die "Build identity does not match the committed authority for ${component}/${PLATFORM}"

    manifest="$(manifest_path_for_component "$component")"
    [ -f "$manifest" ] && [ ! -L "$manifest" ] || die "Missing toolchain installation manifest: $manifest"
    manifest_header_equals "$manifest" schema "1"
    manifest_header_equals "$manifest" component "$component"
    manifest_header_equals "$manifest" platform "$PLATFORM"
    manifest_header_equals "$manifest" lock_sha256 "$(toolchain_lock_digest)"
    manifest_header_equals "$manifest" authority_kind "$kind"
    manifest_header_equals "$manifest" authority_sha256 "$expected_identity"
    manifest_header_equals "$manifest" authority_version "$authority_version"
    manifest_header_equals "$manifest" primary "$primary"

    record_count="$(awk -F '\t' '$1 == "file" || $1 == "link" { records += 1 } END { print records + 0 }' "$manifest")"
    [ "$record_count" = "$#" ] || die "Toolchain manifest ${manifest} has an unexpected installed-file set"

    for relative in "$@"; do
        safe_manifest_relative_path "$relative" || die "Unsafe installed path in ${component} manifest: $relative"
        record="$(
            awk -F '\t' -v wanted="$relative" '
                ($1 == "file" || $1 == "link") && $2 == wanted {
                    matches += 1
                    value = $0
                }
                END {
                    if (matches != 1) exit 1
                    print value
                }
            ' "$manifest"
        )" || die "Toolchain manifest ${manifest} has no unique identity for ${relative}"
        IFS=$'\t' read -r record_type recorded_path recorded_identity extra <<< "$record"
        [ -z "$extra" ] || die "Toolchain manifest ${manifest} has malformed identity for ${relative}"
        path="${root}/${relative}"
        case "$record_type" in
            file)
                require_sha256 "$recorded_identity"
                [ -f "$path" ] && [ ! -L "$path" ] || die "Cached installed file is missing or unsafe: $path"
                actual="$(sha256_file "$path")" || die "Could not hash cached installed file: $path"
                [ "$actual" = "$recorded_identity" ] || die "Cached installed file digest changed: $path"
                ;;
            link)
                [ -L "$path" ] || die "Cached installed symlink is missing: $path"
                actual="$(readlink "$path")" || die "Could not read cached installed symlink: $path"
                [ "$actual" = "$recorded_identity" ] || die "Cached installed symlink target changed: $path"
                ;;
            *) die "Toolchain manifest ${manifest} has unknown installed-file record type: $record_type" ;;
        esac
    done

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
