#!/usr/bin/env bash
# =============================================================================
# 05-package.sh - Package the offline distribution
# Architecture: Nginx (3000) → static frontend/dist/ files + /api proxy → backend (3002)
#       PostgreSQL (5433)
# Supports macOS / Linux / WSL
# =============================================================================

set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
source "${SCRIPT_DIR}/utils.sh"
# shellcheck disable=SC1091
source "${SCRIPT_DIR}/package-lifecycle.sh"

init_paths

OS="$(detect_os)"
ARCH="$(detect_arch)"
PLATFORM="$(detect_platform)"
source "${SCRIPT_DIR}/toolchain-authority.sh"
require_toolchain_authority_receipt

BACKEND_BIN="${DEPS_DIR}/arkham-api"
FRONTEND_SRC="${DEPS_DIR}/frontend"
SETUP_SQL="${PROJECT_ROOT}/setup.sql"
PG_BIN_DIR="${DEPS_DIR}/postgres/bin"
PG_LIB_DIR="${DEPS_DIR}/postgres/lib"
NGINX_BIN="${DEPS_DIR}/nginx/bin/nginx"
NGINX_VERSION="1.26.2"
NGINX_ARCHIVE="nginx-${NGINX_VERSION}.tar.gz"

PKG_NAME="ArkhamHorror-${PLATFORM}"
PKG_DIR="${_DIST_DIR}/${PKG_NAME}"

# ═════════════════════════════════════════════════════════════════════════════
# Nginx authority and package provenance
# ═════════════════════════════════════════════════════════════════════════════

nginx_build_identity_for_package() {
    local source_sha256
    source_sha256="$(toolchain_archive_sha256 nginx "$PLATFORM" "$NGINX_ARCHIVE")"
    toolchain_build_identity nginx "$PLATFORM" "$NGINX_VERSION" "$source_sha256" \
        "$(nginx_build_recipe)"
}

verify_nginx_dependency_for_packaging() {
    local identity nginx_version
    identity="$(nginx_build_identity_for_package)"
    verify_install_manifest nginx "${DEPS_DIR}/nginx" "$identity" "bin/nginx" "bin"
    nginx_version="$("$NGINX_BIN" -V 2>&1)" || die "Nginx failed its post-identity configuration check"
    case "$nginx_version" in
        *"nginx/${NGINX_VERSION}"*) ;;
        *) die "Nginx configuration check reported the wrong version: ${nginx_version}" ;;
    esac
    case "$nginx_version" in
        *"--with-http_gzip_static_module"*) ;;
        *) die "Nginx lacks --with-http_gzip_static_module required by the packaged config" ;;
    esac
}

package_runtime_nginx_closure_digest() {
    local root="${PKG_DIR}/game"
    local records selected entry relative path link_target digest mode
    records="$(
        {
            for selected in \
                bin/nginx \
                lib \
                pgsql/lib \
                start.sh \
                config/mime.types \
                config/toolchain.lock; do
                path="${root}/${selected}"
                if [ -d "$path" ] && [ ! -L "$path" ]; then
                    (cd "$root" && find "./${selected}" -print)
                elif [ -f "$path" ] || [ -L "$path" ]; then
                    printf './%s\n' "$selected"
                else
                    die "Packaged nginx runtime closure path is missing: $path"
                fi
            done
        } | LC_ALL=C sort -u | while IFS= read -r entry; do
            relative="${entry#./}"
            path="${root}/${relative}"
            if [ -L "$path" ]; then
                link_target="$(readlink "$path")" || die "Could not read packaged runtime symlink: $path"
                validate_internal_link "$root" "$path"
                printf 'link\t%s\t%s\n' "$relative" "$link_target"
            elif [ -f "$path" ]; then
                digest="$(sha256_file "$path")" || die "Could not hash packaged runtime closure file: $path"
                mode="$(file_mode "$path")" || die "Could not read packaged runtime closure mode: $path"
                printf 'file\t%s\t%s\t%s\n' "$relative" "$mode" "$digest"
            elif [ -d "$path" ]; then
                mode="$(file_mode "$path")" || die "Could not read packaged runtime closure mode: $path"
                printf 'dir\t%s\t%s\n' "$relative" "$mode"
            else
                die "Packaged nginx runtime closure has an unsupported file type: $path"
            fi
        done
    )" || die "Could not calculate packaged nginx runtime closure"
    [ -n "$records" ] || die "Packaged nginx runtime closure is empty"
    printf '%s\n' "$records" | sha256_text
}

write_nginx_provenance() {
    local provenance="${PKG_DIR}/game/config/toolchain-provenance.env"
    local partial="${provenance}.partial.$$"
    local packaged_lock="${PKG_DIR}/game/config/toolchain.lock"
    local source_sha256 build_identity binary_sha256 runtime_closure_sha256

    source_sha256="$(toolchain_archive_sha256 nginx "$PLATFORM" "$NGINX_ARCHIVE")"
    build_identity="$(nginx_build_identity_for_package)"
    binary_sha256="$(sha256_file "${PKG_DIR}/game/bin/nginx")" \
        || die "Could not hash the packaged nginx executable"
    cp "${OFFLINE_DIR}/toolchain.lock" "$packaged_lock"
    verify_file_sha256 "$packaged_lock" "$(toolchain_lock_digest)" "packaged toolchain authority"
    runtime_closure_sha256="$(package_runtime_nginx_closure_digest)"

    {
        printf 'schema=1\n'
        printf 'platform=%s\n' "$PLATFORM"
        printf 'toolchain_lock_sha256=%s\n' "$(toolchain_lock_digest)"
        printf 'nginx_source_archive=%s\n' "$NGINX_ARCHIVE"
        printf 'nginx_source_sha256=%s\n' "$source_sha256"
        printf 'nginx_build_identity=%s\n' "$build_identity"
        printf 'nginx_binary_sha256=%s\n' "$binary_sha256"
        printf 'nginx_runtime_closure_sha256=%s\n' "$runtime_closure_sha256"
        printf 'nginx_version=%s\n' "$NGINX_VERSION"
        printf 'nginx_required_configure_option=--with-http_gzip_static_module\n'
    } > "$partial"
    mv -f "$partial" "$provenance"
    verify_file_sha256 "${PKG_DIR}/game/bin/nginx" "$binary_sha256" "packaged nginx executable"
}

# ═════════════════════════════════════════════════════════════════════════════
# Generate start.sh (launcher script) — supports macOS / Linux / WSL
# ═════════════════════════════════════════════════════════════════════════════

generate_launch_script() {
    substep "Generating start.sh ..."

    cat > "${PKG_DIR}/game/start.sh" << 'LAUNCHSCRIPT'
#!/usr/bin/env bash
set -euo pipefail

# ── Root check: PostgreSQL must not run as root ───────────────────────────────
# Windows: Start-ArkhamHorror.bat already ensures a non-root user through -u arkham
# macOS/Linux: users normally run as a regular user; this is only a safety fallback
if [ "$(id -u)" = "0" ]; then
    echo "[ARKHAM] Error: running as root is not allowed." >&2
    echo "         PostgreSQL refuses to run initdb/pg_ctl as root." >&2
    if grep -qi microsoft /proc/version 2>/dev/null; then
        echo "         Please launch via Start-ArkhamHorror.bat (it runs automatically as user arkham)," >&2
        echo "         or specify the user manually: su arkham -c 'bash start.sh'" >&2
    else
        echo "         Please run as a regular user: bash start.sh" >&2
    fi
    exit 1
fi

SCRIPT_DIR="$(cd "$(dirname "$0")" && pwd)"
DATA_DIR="$SCRIPT_DIR/data"
NGINX_PID="$DATA_DIR/nginx.pid"
NGINX_LOG_DIR="$DATA_DIR"
API_PID_FILE="$DATA_DIR/arkham-api.pid"
PG_LOG="$DATA_DIR/pg.log"

NGINX_PORT="${ARKHAM_PORT:-3000}"
API_PORT="${ARKHAM_API_PORT:-3002}"
PG_PORT="${ARKHAM_PG_PORT:-5433}"
PG_USER="arkham_user"
PG_DB="arkham-horror-backend"

# Runtime pgdata directory: keep it under the OS user data directory to avoid WSL/NTFS permission issues
# macOS: ~/Library/Application Support/ArkhamHorror/pgdata/
# Linux/WSL: ~/.local/share/ArkhamHorror/pgdata/
case "$(uname -s)" in
    Darwin) PGDATA_OS="$HOME/Library/Application Support/ArkhamHorror/pgdata" ;;
    Linux)  PGDATA_OS="$HOME/.local/share/ArkhamHorror/pgdata" ;;
esac
PGDATA_OS_PARENT="$(dirname "$PGDATA_OS")"
PGDATA_LOCAL="$DATA_DIR/pgdata"        # Legacy physical backup location from older builds
BACKUP_DIR="$SCRIPT_DIR/../backup"
PG_DUMP_FILE="$BACKUP_DIR/latest.dump"
PG_DUMP_PREV="$BACKUP_DIR/previous.dump"
PG_DUMP_TMP="$BACKUP_DIR/latest.dump.tmp"
PG_DUMP_LOG="$DATA_DIR/pg_dump.log"
PG_RESTORE_LOG="$DATA_DIR/pg_restore.log"
VERSION_FILE="$PGDATA_OS_PARENT/pgdata_version"
VERSION_FILE_LOCAL="$DATA_DIR/pgdata_version"
PG_DATA="$PGDATA_OS"                    # Actual pgdata path used at runtime
START_LOCK_DIR="$PGDATA_OS_PARENT/start.lock"
START_LOCK_PID="$START_LOCK_DIR/pid"
START_LOCK_TS="$START_LOCK_DIR/timestamp"

# Unix domain socket directory:
# - macOS: distribution paths can be long; a Unix socket path longer than 103 bytes can make PostgreSQL fail to start
# - WSL/NTFS (DrvFS): Unix sockets are not supported, so the socket must live on the native Linux filesystem
# Detection rule: paths starting with /mnt/ are usually Windows drive mounts (DrvFS)
if [ "$(uname -s)" = "Darwin" ]; then
    PG_SOCKET_DIR="/tmp/arkham-pg-${PG_PORT}"
    mkdir -p "$PG_SOCKET_DIR" 2>/dev/null || PG_SOCKET_DIR="/tmp"
elif [[ "$DATA_DIR" == /mnt/* ]] && grep -qi microsoft /proc/version 2>/dev/null; then
    PG_SOCKET_DIR="/tmp/arkham-pg-${PG_PORT}"
    mkdir -p "$PG_SOCKET_DIR" 2>/dev/null || PG_SOCKET_DIR="/tmp"
else
    PG_SOCKET_DIR="$DATA_DIR"
fi

GREEN=$'\033[0;32m'
YELLOW=$'\033[0;33m'
RED=$'\033[0;31m'
CYAN=$'\033[0;36m'
MAGENTA=$'\033[0;35m'
BOLD=$'\033[1m'
RESET=$'\033[0m'

info()  { printf '%s[ARKHAM]%s %s\n' "$GREEN" "$RESET" "$*"; }
warn()  { printf '%s[ARKHAM]%s %s\n' "$YELLOW" "$RESET" "$*" >&2; }
die() {
    local code="$1" msg="$2" log="${3:-}"
    printf '%s[ARKHAM]%s [Error Code %s] %s\n' "$RED" "$RESET" "$code" "$msg" >&2
    if [ -n "$log" ] && [ -f "$log" ] && [ -s "$log" ]; then
        warn "Last 20 log lines:"
        tail -20 "$log" 2>/dev/null | while IFS= read -r line; do
            echo "  $line" >&2
        done
        echo "Full log: $log" >&2
    fi
    exit "$code"
}

# Open a URL in the default browser (cross-platform, silently ignore failures)
open_browser() {
    local url="$1"
    if grep -qi microsoft /proc/version 2>/dev/null; then
        # WSL: use /init to invoke cmd.exe. This bypasses binfmt_misc interop
        # which is often unavailable for non-default users (e.g. arkham).
        # /init is always present and can always call Windows executables.
        /init /mnt/c/Windows/System32/cmd.exe /c start "" "$url" >/dev/null 2>&1 || true
    elif [ "$(uname -s)" = "Darwin" ]; then
        open "$url" >/dev/null 2>&1 || true
    elif command -v xdg-open >/dev/null 2>&1; then
        xdg-open "$url" >/dev/null 2>&1 || true
    fi
}

get_primary_url() {
    echo "http://localhost:${NGINX_PORT}"
}

get_local_ipv4() {
    local candidate=""
    case "$(uname -s)" in
        Darwin)
            local iface=""
            iface="$(route -n get default 2>/dev/null | awk '/interface: / { print $2; exit }')"
            if [ -n "$iface" ] && command -v ipconfig >/dev/null 2>&1; then
                candidate="$(ipconfig getifaddr "$iface" 2>/dev/null || true)"
            fi
            if [ -z "$candidate" ] && command -v ifconfig >/dev/null 2>&1; then
                candidate="$(ifconfig 2>/dev/null | awk '
                    /^[a-z0-9]/ { iface=$1; sub(":", "", iface) }
                    /inet / && $2 != "127.0.0.1" {
                        if (iface !~ /^(lo|utun|awdl|llw|bridge)/) { print $2; exit }
                    }')"
            fi
            ;;
        Linux)
            if grep -qi microsoft /proc/version 2>/dev/null; then
                candidate="$(hostname -I 2>/dev/null | awk '{ for (i = 1; i <= NF; i++) if ($i !~ /^127\./) { print $i; exit } }')"
            elif command -v ip >/dev/null 2>&1; then
                candidate="$(ip -4 route get 1 2>/dev/null | awk '{ for (i = 1; i <= NF; i++) if ($i == "src") { print $(i + 1); exit } }')"
            fi
            if [ -z "$candidate" ]; then
                candidate="$(hostname -I 2>/dev/null | awk '{ for (i = 1; i <= NF; i++) if ($i !~ /^127\./) { print $i; exit } }')"
            fi
            ;;
    esac
    printf '%s\n' "$candidate"
}

get_fallback_url() {
    local ip=""
    ip="$(get_local_ipv4)"
    [ -n "$ip" ] || return 1
    printf 'http://%s:%s\n' "$ip" "$NGINX_PORT"
}

is_wsl() {
    grep -qi microsoft /proc/version 2>/dev/null
}

can_host_reach_localhost() {
    # Test whether the host browser can reach localhost:<NGINX_PORT>.
    # On WSL, use /init to call Windows-side curl.exe — this tests the actual
    # WSL2 localhost forwarding path from the Windows network stack.
    # /init bypasses binfmt_misc interop (which is often missing for non-default
    # users) and correctly propagates exit codes.
    # curl.exe is guaranteed present on Windows 10 1803+; WSL2 requires 1903+.
    if is_wsl; then
        /init /mnt/c/Windows/System32/curl.exe -sI --max-time 3 \
            "http://localhost:${NGINX_PORT}" >/dev/null 2>&1 && return 0 || return 1
    fi
    return 0
}

get_browser_url() {
    if can_host_reach_localhost; then
        get_primary_url
    else
        local fallback
        fallback="$(get_fallback_url || true)"
        if [ -n "$fallback" ]; then
            printf '%s\n' "$fallback"
        else
            get_primary_url
        fi
    fi
}

print_access_urls() {
    local primary fallback
    primary="$(get_primary_url)"
    printf '  %-14s %s%s%s\n' "URL" "$GREEN" "$primary" "$RESET"
    fallback="$(get_fallback_url || true)"
    if [ -n "$fallback" ] && [ "$fallback" != "$primary" ]; then
        printf '  %-14s %s%s%s\n' "Fallback URL" "$GREEN" "$fallback" "$RESET"
    fi
}

warn_localhost_fallback_if_needed() {
    can_host_reach_localhost && return 0
    local fallback
    fallback="$(get_fallback_url || true)"
    [ -n "$fallback" ] || return 0
    warn "localhost:${NGINX_PORT} is not reachable from this host; opening fallback URL instead: ${fallback}"
}

# If startup exits abnormally, automatically clean up all started services
_CLEANUP_ON_EXIT=0
_STARTUP_SUCCEEDED=0
_cleanup_on_exit() {
    release_start_lock
    if [ "$_CLEANUP_ON_EXIT" = "1" ]; then
        _CLEANUP_ON_EXIT=0          # Prevent re-triggering if do_stop itself fails
        warn "Startup did not finish; cleaning up any started services ..."
        do_stop 2>/dev/null || true
    fi
}
trap '_cleanup_on_exit' EXIT

pg_bin()  { echo "$SCRIPT_DIR/pgsql/bin"; }
pg_ctl()  { "$(pg_bin)/pg_ctl" "$@"; }
pg_ready(){ { "$(pg_bin)/pg_isready" -h 127.0.0.1 -p "$PG_PORT" -q; } 2>/dev/null; }
psql_cmd(){ "$(pg_bin)/psql" -w -h 127.0.0.1 -p "$PG_PORT" -U "$PG_USER" "$@"; }
pg_dump_cmd(){ "$(pg_bin)/pg_dump" -w -h 127.0.0.1 -p "$PG_PORT" -U "$PG_USER" "$@"; }
pg_restore_cmd(){ "$(pg_bin)/pg_restore" -w -h 127.0.0.1 -p "$PG_PORT" -U "$PG_USER" "$@"; }

# On WSL/NTFS (DrvFS), mkdir can fail because of timing issues, so retry
ensure_dir() {
    local target="$1" i=0
    while [ ! -d "$target" ] && [ $i -lt 5 ]; do
        mkdir -p "$target" 2>&1 || true
        [ -d "$target" ] && return 0
        i=$((i + 1))
        warn "Failed to create directory $target (retry $i/5) ..."
        sleep 1
    done
    [ -d "$target" ] || die 1008 "Unable to create directory: $target"
}

configure_runtime_env() {
    export PATH="$SCRIPT_DIR/bin:$(pg_bin):$PATH"
    case "$(uname -s)" in
        Linux)  export LD_LIBRARY_PATH="$SCRIPT_DIR/lib:$SCRIPT_DIR/pgsql/lib${LD_LIBRARY_PATH:+:$LD_LIBRARY_PATH}" ;;
        Darwin) export DYLD_LIBRARY_PATH="$SCRIPT_DIR/lib:$SCRIPT_DIR/pgsql/lib${DYLD_LIBRARY_PATH:+:$DYLD_LIBRARY_PATH}" ;;
    esac

    # Fix library symlinks: Windows extraction (zip/tar) often destroys symlinks,
    # turning them into copies or broken files. Recreate the expected versioned symlinks.
    _fix_lib_symlinks "$SCRIPT_DIR/pgsql/lib"
    _fix_lib_symlinks "$SCRIPT_DIR/lib"
}

nginx_provenance_value() {
    local key="$1" provenance="$SCRIPT_DIR/config/toolchain-provenance.env"
    local count value
    case "$key" in
        schema|platform|toolchain_lock_sha256|nginx_source_archive|nginx_source_sha256|nginx_build_identity|nginx_binary_sha256|nginx_runtime_closure_sha256|nginx_version|nginx_required_configure_option) ;;
        *) die 1015 "Invalid nginx provenance key: $key" ;;
    esac
    [ -f "$provenance" ] && [ ! -L "$provenance" ] \
        || die 1016 "Nginx provenance is missing or unsafe: $provenance"
    count="$(awk -F= -v key="$key" '$1 == key { matches += 1 } END { print matches + 0 }' "$provenance")"
    [ "$count" = "1" ] || die 1017 "Nginx provenance has no unique ${key} value"
    value="$(awk -F= -v key="$key" '$1 == key { print $2 }' "$provenance")"
    [ -n "$value" ] || die 1018 "Nginx provenance has an empty ${key} value"
    printf '%s\n' "$value"
}

nginx_lock_record() {
    local record_type="$1" platform="$2" artifact="$3"
    local lock="$SCRIPT_DIR/config/toolchain.lock"
    local record
    record="$(
        awk -F '\t' \
            -v record_type="$record_type" \
            -v platform="$platform" \
            -v artifact="$artifact" '
                $1 == record_type && $2 == "nginx" && $3 == platform && $4 == artifact {
                    matches += 1
                    value = $0
                }
                END {
                    if (matches != 1) exit 1
                    print value
                }
            ' "$lock"
    )" || die 1019 "Packaged toolchain authority has no unique nginx ${record_type} record"
    printf '%s\n' "$record"
}

runtime_sha256_file() {
    local file="$1" result digest
    [ -f "$file" ] && [ ! -L "$file" ] || return 1
    if command -v sha256sum >/dev/null 2>&1; then
        result="$(sha256sum "$file")" || return 1
    elif command -v shasum >/dev/null 2>&1; then
        result="$(shasum -a 256 "$file")" || return 1
    else
        return 1
    fi
    digest="${result%%[[:space:]]*}"
    printf '%s\n' "$digest"
}

runtime_sha256_text() {
    if command -v sha256sum >/dev/null 2>&1; then
        sha256sum | awk '{print $1}'
    elif command -v shasum >/dev/null 2>&1; then
        shasum -a 256 | awk '{print $1}'
    else
        die 1031 "No SHA-256 tool is available for packaged runtime verification"
    fi
}

runtime_file_mode() {
    local file="$1" mode
    mode="$(stat -c '%a' "$file" 2>/dev/null || stat -f '%Lp' "$file" 2>/dev/null)" \
        || die 1031 "Could not read packaged runtime mode: $file"
    case "$mode" in
        [0-7][0-7][0-7]|[0-7][0-7][0-7][0-7]) ;;
        *) die 1031 "Packaged runtime has an invalid mode: $file" ;;
    esac
    printf '%s\n' "$mode"
}

validate_runtime_internal_link() {
    local path="$1" root target next_dir hops=0
    root="$(cd -P "$SCRIPT_DIR" && pwd)"
    while [ -L "$path" ]; do
        hops=$((hops + 1))
        [ "$hops" -le 64 ] || die 1032 "Packaged runtime symlink chain is cyclic or too deep: $path"
        target="$(readlink "$path")" || die 1032 "Could not read packaged runtime symlink: $path"
        case "$target" in
            ""|/*|*$'\t'*|*$'\n'*|*$'\r'*) die 1032 "Unsafe packaged runtime symlink target: $path" ;;
        esac
        next_dir="$(cd -P "$(dirname "$path")" && cd -P "$(dirname "$target")" && pwd)" \
            || die 1032 "Broken packaged runtime symlink target: $path"
        path="${next_dir}/$(basename "$target")"
        path="$(cd -P "$(dirname "$path")" && printf '%s/%s\n' "$(pwd)" "$(basename "$path")")" \
            || die 1032 "Broken packaged runtime symlink target: $path"
        case "$path" in
            "$root"/*) ;;
            *) die 1032 "Packaged runtime symlink escapes the package: $path" ;;
        esac
    done
    [ -f "$path" ] || [ -d "$path" ] || die 1032 "Packaged runtime symlink is dangling: $path"
}

runtime_nginx_closure_digest() {
    local records selected entry relative path link_target digest mode
    records="$(
        {
            for selected in \
                bin/nginx \
                lib \
                pgsql/lib \
                start.sh \
                config/mime.types \
                config/toolchain.lock; do
                path="$SCRIPT_DIR/$selected"
                if [ -d "$path" ] && [ ! -L "$path" ]; then
                    (cd "$SCRIPT_DIR" && find "./${selected}" -print)
                elif [ -f "$path" ] || [ -L "$path" ]; then
                    printf './%s\n' "$selected"
                else
                    die 1033 "Packaged nginx runtime closure path is missing: $path"
                fi
            done
        } | LC_ALL=C sort -u | while IFS= read -r entry; do
            relative="${entry#./}"
            path="$SCRIPT_DIR/$relative"
            if [ -L "$path" ]; then
                link_target="$(readlink "$path")" || die 1034 "Could not read packaged runtime symlink: $path"
                validate_runtime_internal_link "$path"
                printf 'link\t%s\t%s\n' "$relative" "$link_target"
            elif [ -f "$path" ]; then
                digest="$(runtime_sha256_file "$path")" || die 1034 "Could not hash packaged runtime file: $path"
                mode="$(runtime_file_mode "$path")"
                printf 'file\t%s\t%s\t%s\n' "$relative" "$mode" "$digest"
            elif [ -d "$path" ]; then
                mode="$(runtime_file_mode "$path")"
                printf 'dir\t%s\t%s\n' "$relative" "$mode"
            else
                die 1034 "Packaged runtime closure has an unsupported file type: $path"
            fi
        done
    )" || die 1034 "Could not calculate packaged nginx runtime closure"
    [ -n "$records" ] || die 1034 "Packaged nginx runtime closure is empty"
    printf '%s\n' "$records" | runtime_sha256_text
}

verify_nginx_runtime_closure() {
    local expected actual
    expected="$(nginx_provenance_value nginx_runtime_closure_sha256)"
    case "$expected" in
        *[!0-9a-f]*|"") die 1035 "Nginx provenance has an invalid runtime closure SHA-256" ;;
    esac
    [ "${#expected}" = 64 ] || die 1035 "Nginx provenance has an invalid runtime closure SHA-256"
    actual="$(runtime_nginx_closure_digest)"
    [ "$actual" = "$expected" ] \
        || die 1035 "Packaged nginx executable/library closure changed before runtime loading"
}

# This is a package-local consistency check. The invocation-external release
# receipt above establishes authority before it; this metadata alone is never
# accepted as a root of trust.
verify_nginx_identity() {
    local expected_lock_sha256 actual_lock_sha256 expected_sha256 actual_sha256 expected_version expected_option nginx_version
    local platform source_archive source_sha256 build_identity archive_record binary_record
    local archive_type archive_component archive_platform archive_name archive_kind archive_sha archive_version
    local binary_type binary_component binary_platform binary_name binary_kind binary_sha binary_recipe
    [ "$(nginx_provenance_value schema)" = "1" ] || die 1020 "Unsupported nginx provenance schema"
    expected_lock_sha256="$(nginx_provenance_value toolchain_lock_sha256)"
    actual_lock_sha256="$(runtime_sha256_file "$SCRIPT_DIR/config/toolchain.lock")" \
        || die 1021 "Could not hash packaged toolchain authority"
    [ "$actual_lock_sha256" = "$expected_lock_sha256" ] \
        || die 1022 "Packaged toolchain authority digest does not match nginx provenance"

    platform="$(nginx_provenance_value platform)"
    source_archive="$(nginx_provenance_value nginx_source_archive)"
    source_sha256="$(nginx_provenance_value nginx_source_sha256)"
    build_identity="$(nginx_provenance_value nginx_build_identity)"
    archive_record="$(nginx_lock_record archive "$platform" "$source_archive")"
    binary_record="$(nginx_lock_record binary "$platform" "bin/nginx")"
    IFS=$'\t' read -r archive_type archive_component archive_platform archive_name archive_kind archive_sha archive_version <<< "$archive_record"
    IFS=$'\t' read -r binary_type binary_component binary_platform binary_name binary_kind binary_sha binary_recipe <<< "$binary_record"
    [ "$archive_kind" = "exact" ] && [ "$archive_sha" = "$source_sha256" ] \
        || die 1023 "Packaged nginx source authority does not match its lock"
    [ "$binary_kind" = "derived" ] && [ "$binary_sha" = "$build_identity" ] \
        || die 1024 "Packaged nginx build identity does not match its lock"

    expected_sha256="$(nginx_provenance_value nginx_binary_sha256)"
    case "$expected_sha256" in
        *[!0-9a-f]*|"") die 1025 "Nginx provenance has an invalid binary SHA-256" ;;
    esac
    [ "${#expected_sha256}" = 64 ] || die 1025 "Nginx provenance has an invalid binary SHA-256"
    actual_sha256="$(runtime_sha256_file "$SCRIPT_DIR/bin/nginx")" \
        || die 1026 "Could not hash packaged nginx executable"
    [ "$actual_sha256" = "$expected_sha256" ] \
        || die 1027 "Packaged nginx executable digest does not match its provenance"

    expected_version="$(nginx_provenance_value nginx_version)"
    expected_option="$(nginx_provenance_value nginx_required_configure_option)"
    nginx_version="$("$SCRIPT_DIR/bin/nginx" -V 2>&1)" \
        || die 1028 "Packaged nginx failed its post-identity configuration check"
    case "$nginx_version" in
        *"nginx/${expected_version}"*) ;;
        *) die 1029 "Packaged nginx reported the wrong version: ${nginx_version}" ;;
    esac
    case "$nginx_version" in
        *"$expected_option"*) ;;
        *) die 1030 "Packaged nginx lacks ${expected_option}" ;;
    esac
    info "nginx executable identity and gzip_static capability verified"
}

close_terminal_window_if_needed() {
    if [ "$(uname -s)" = "Darwin" ]; then
        ( sleep 0.5; osascript -e "tell application \"Terminal\" to close front window" 2>/dev/null ) &
        disown 2>/dev/null
    fi
}

_START_LOCK_HELD=0
_STARTED_BY_OTHER=0

release_start_lock() {
    if [ "$_START_LOCK_HELD" = "1" ]; then
        rm -rf "$START_LOCK_DIR" 2>/dev/null || true
        _START_LOCK_HELD=0
    fi
}

acquire_start_lock() {
    if mkdir "$START_LOCK_DIR" 2>/dev/null; then
        echo "$$" > "$START_LOCK_PID"
        date +%s > "$START_LOCK_TS"
        _START_LOCK_HELD=1
        return 0
    fi

    local lock_pid=""
    lock_pid="$(cat "$START_LOCK_PID" 2>/dev/null || echo "")"
    if [ -n "$lock_pid" ] && kill -0 "$lock_pid" 2>/dev/null; then
        return 1
    fi

    warn "Detected a stale startup lock; removing it ..."
    rm -rf "$START_LOCK_DIR" 2>/dev/null || true
    if mkdir "$START_LOCK_DIR" 2>/dev/null; then
        echo "$$" > "$START_LOCK_PID"
        date +%s > "$START_LOCK_TS"
        _START_LOCK_HELD=1
        return 0
    fi
    return 1
}

wait_for_existing_startup() {
    local waited=0
    warn "Another startup is already in progress; waiting for it to finish ..."
    while [ -d "$START_LOCK_DIR" ] && [ "$waited" -lt 90 ]; do
        if is_all_services_running; then
            _STARTED_BY_OTHER=1
            info "Another launcher finished startup successfully."
            return 0
        fi
        waited=$((waited + 1))
        sleep 1
    done

    if is_all_services_running; then
        _STARTED_BY_OTHER=1
        info "Services became healthy while waiting."
        return 0
    fi

    return 1
}

cluster_is_valid() {
    local cluster_dir="$1"
    [ -d "$cluster_dir" ] || return 1
    [ -f "$cluster_dir/PG_VERSION" ] || return 1
    [ -f "$cluster_dir/global/pg_control" ] || return 1
    [ -d "$cluster_dir/base" ] || return 1
    [ -d "$cluster_dir/pg_wal" ] || return 1
    [ -f "$cluster_dir/postgresql.conf" ] || return 1
    [ -f "$cluster_dir/pg_hba.conf" ] || return 1
}

dump_file_is_valid() {
    local dump_file="$1"
    [ -s "$dump_file" ] || return 1
    pg_restore_cmd -l "$dump_file" > /dev/null 2>&1
}

database_exists() {
    psql_cmd -d postgres -tAc "SELECT 1 FROM pg_database WHERE datname='$PG_DB'" 2>/dev/null | grep -q 1
}

cleanup_invalid_cluster() {
    local cluster_dir="$1"
    [ -e "$cluster_dir" ] || return 0
    warn "Removing invalid PostgreSQL cluster: $cluster_dir"
    rm -rf "$cluster_dir" 2>/dev/null || die 2008 "Failed to remove invalid PostgreSQL cluster"
}

backup_database_dump() {
    [ -d "$PG_DATA" ] || return 0
    pg_ready || return 0
    database_exists || return 0

    ensure_dir "$BACKUP_DIR"
    rm -f "$PG_DUMP_TMP" 2>/dev/null || true

    info "Exporting database backup ..."
    if ! pg_dump_cmd -d "$PG_DB" -Fc -f "$PG_DUMP_TMP" > "$PG_DUMP_LOG" 2>&1; then
        warn "[4006] Logical backup failed during pg_dump"
        return 1
    fi
    if ! dump_file_is_valid "$PG_DUMP_TMP"; then
        warn "[4006] Logical backup validation failed"
        rm -f "$PG_DUMP_TMP" 2>/dev/null || true
        return 1
    fi

    rm -f "$PG_DUMP_PREV" 2>/dev/null || true
    [ -f "$PG_DUMP_FILE" ] && mv "$PG_DUMP_FILE" "$PG_DUMP_PREV" 2>/dev/null || true
    mv "$PG_DUMP_TMP" "$PG_DUMP_FILE" || {
        warn "[4006] Failed to publish logical backup"
        rm -f "$PG_DUMP_TMP" 2>/dev/null || true
        return 1
    }

    date +%s > "$VERSION_FILE"
    date +%s > "$VERSION_FILE_LOCAL"
    info "Database backup written to $PG_DUMP_FILE"
}

start_postgres() {
    if pg_ready; then
        # Verify it is actually our instance (trust auth, no password required)
        if pg_port_is_foreign "$PG_PORT"; then
            die 2009 "Port $PG_PORT has a foreign PostgreSQL instance (requires password); cannot start"
        fi
        return 0
    fi

    info "Starting PostgreSQL ..."
    if ! pg_ctl -D "$PG_DATA" -l "$PG_LOG" -o "-k '$PG_SOCKET_DIR'" start; then
        die 2004 "PostgreSQL failed to start" "$PG_LOG"
    fi
    local tries=0
    while ! pg_ready; do
        tries=$((tries + 1)); [ "$tries" -gt 30 ] && die 2005 "PostgreSQL startup timed out" "$PG_LOG"
        sleep 1
    done
    info "PostgreSQL started (port $PG_PORT, PID $(get_pg_pid))"
}

restore_database_from_dump() {
    local dump_file="$1"

    info "Restoring database from logical backup: $(basename "$dump_file") ..."

    # Initialize a fresh cluster (inline; must not die so caller can fall back)
    ensure_dir "$DATA_DIR"
    rm -f "$PG_LOG" "$DATA_DIR/psql.log" "$DATA_DIR/initdb.log" "$PG_RESTORE_LOG" 2>/dev/null || true
    ensure_dir "$PG_DATA"
    if ! "$(pg_bin)/initdb" -D "$PG_DATA" -U "$PG_USER" --no-locale -E UTF8 \
        > "$DATA_DIR/initdb.log" 2>&1; then
        warn "initdb failed during restore (see $DATA_DIR/initdb.log)"
        return 1
    fi
    cat > "$PG_DATA/pg_hba.conf" << HBA_EOF
local   all   all                 trust
host    all   all   127.0.0.1/32  trust
host    all   all   ::1/128       trust
HBA_EOF

    # Configure socket directory
    cat >> "$PG_DATA/postgresql.conf" << PG_CONF_EOF

port = $PG_PORT
unix_socket_directories = '$PG_SOCKET_DIR'
listen_addresses = '127.0.0.1'
PG_CONF_EOF

    # Start PostgreSQL (inline; must not die)
    if ! pg_ctl -D "$PG_DATA" -l "$PG_LOG" -o "-k '$PG_SOCKET_DIR'" start; then
        warn "PostgreSQL failed to start during restore (see $PG_LOG)"
        return 1
    fi
    local tries=0
    while ! pg_ready; do
        tries=$((tries + 1))
        if [ "$tries" -gt 30 ]; then
            warn "PostgreSQL startup timed out during restore (see $PG_LOG)"
            return 1
        fi
        sleep 1
    done

    # Create database and restore dump
    if ! psql_cmd -d postgres -c "CREATE DATABASE \"$PG_DB\";" \
        > "$DATA_DIR/psql.log" 2>&1; then
        warn "Failed to create database during restore (see $DATA_DIR/psql.log)"
        return 1
    fi
    if ! pg_restore_cmd -d "$PG_DB" --clean --if-exists --no-owner "$dump_file" \
        > "$PG_RESTORE_LOG" 2>&1; then
        warn "pg_restore failed for $(basename "$dump_file") (see $PG_RESTORE_LOG)"
        return 1
    fi
    info "Logical backup restore complete."
    return 0
}

# Scan a directory for versioned .so files (lib*.so.X.Y) and recreate short symlinks:
#   lib*.so.X.Y  →  lib*.so   and  lib*.so.X
# On NTFS/DrvFS (WSL /mnt/ paths) symlinks are not supported; fall back to copying.
_fix_lib_symlinks() {
    local dir="$1"
    [ -d "$dir" ] || return 0

    # Short-circuit: if the first versioned .so already has valid soname and bare symlinks, skip
    local first_versioned
    first_versioned="$(find "$dir" -maxdepth 1 -name 'lib*.so.*' -print -quit 2>/dev/null)"
    if [ -n "$first_versioned" ]; then
        local bn="${first_versioned##*/}"
        local sn="${bn%.*}"
        local bare="${sn%.*}"
        # Verify soname link (e.g. libpq.so.5 -> libpq.so.5.14)
        local soname_ok=false
        if [ -L "$dir/$sn" ] && [ "$(readlink "$dir/$sn")" = "$bn" ]; then
            soname_ok=true
        fi
        # Verify bare link if applicable (e.g. libpq.so -> libpq.so.5.14)
        local bare_ok=false
        if [[ "$bare" == *.so ]]; then
            if [ -L "$dir/$bare" ]; then
                bare_ok=true
            fi
        else
            bare_ok=true  # no bare link expected for this file
        fi
        if $soname_ok && $bare_ok; then
            return 0
        fi
    fi

    local f basename linkname
    # Match versioned .so files: lib*.so.X, lib*.so.X.Y, lib*.so.X.Y.Z, etc.
    # Glob returns entries in lexicographic order, so libpq.so.5 comes before libpq.so.5.14;
    # later iterations naturally overwrite earlier symlinks with the correct longest target.
    for f in "$dir"/lib*.so.*; do
        [ -f "$f" ] || continue
        basename="$(basename "$f")"
        # e.g. libpq.so.5.14 → soname=libpq.so.5, bare=libpq.so
        #      libpq.so.5.14.3 → soname=libpq.so.5.14, bare=libpq.so.5 (not .so, skip bare link)
        local soname="${basename%.*}"      # strip last .N segment
        local bare="${soname%.*}"          # strip one more segment
        # Recreate the soname link (libpq.so.5 -> libpq.so.5.14)
        linkname="$dir/$soname"
        if [ ! -L "$linkname" ] || [ "$(readlink "$linkname")" != "$basename" ]; then
            # Try symlink first; fall back to copy if filesystem doesn't support symlinks
            # (e.g. NTFS via DrvFS without Developer Mode / SeCreateSymbolicLinkPrivilege)
            if ! ln -sf "$basename" "$linkname" 2>/dev/null; then
                cp -f "$f" "$linkname"
            fi
        fi
        # Recreate the bare link (libpq.so -> libpq.so.5.14) only if bare ends with .so
        if [[ "$bare" == *.so ]]; then
            linkname="$dir/$bare"
            if [ ! -L "$linkname" ] || [ "$(readlink "$linkname")" != "$basename" ]; then
                if ! ln -sf "$basename" "$linkname" 2>/dev/null; then
                    cp -f "$f" "$linkname"
                fi
            fi
        fi
    done
}

# ── macOS Gatekeeper: ad-hoc sign all binaries and clear quarantine ───────────
# Order: 1) make writable → 2) clear quarantine with xattr → 3) remove old signatures and re-sign ad-hoc → 4) clear xattr again
# Homebrew-bundled dylib source files may be read-only (0444), so chmod u+w is required first.
ensure_macos_signing() {
    if [ "$(uname -s)" != "Darwin" ]; then
        return 0
    fi
    if ! command -v codesign >/dev/null 2>&1; then
        warn "codesign not found; skipping signing (manual approval may be needed on macOS)"
        return 0
    fi

    # Skip if binaries haven't changed since last successful signing,
    # but verify the signature is still intact (user may have tampered with binaries)
    local marker="$DATA_DIR/signed.marker"
    local ref_bin="$SCRIPT_DIR/bin/arkham-api"
    if [ -f "$marker" ] && [ -f "$ref_bin" ] && [ "$marker" -nt "$ref_bin" ]; then
        if codesign --verify --strict "$ref_bin" >/dev/null 2>&1; then
            return 0
        fi
        # Signature broken; remove stale marker and re-sign
        rm -f "$marker"
    fi

    # 1) Ensure all files that need signing are writable (Homebrew dylibs may be read-only)
    for dir in bin pgsql/bin pgsql/lib lib; do
        local d="${SCRIPT_DIR}/${dir}"
        [ -d "$d" ] && chmod -R u+w "$d" 2>/dev/null || true
    done

    # 2) Clear all quarantine attributes first
    if ! xattr -rd com.apple.quarantine "$SCRIPT_DIR"; then
        warn "Failed to clear quarantine attributes with xattr (safe to ignore; continuing signing)"
    fi

    info "Applying ad-hoc signatures to all binaries and dynamic libraries ..."
    local cs_ok=0 cs_fail=0
    for dir in bin pgsql/bin pgsql/lib lib; do
        local d="${SCRIPT_DIR}/${dir}"
        [ ! -d "$d" ] && continue
        # lib/ contains Homebrew-bundled dylibs; remove signatures twice to ensure the original signature is fully stripped
        if [ "$dir" = "lib" ]; then
            find "$d" -name '*.dylib' -exec codesign --remove-signature {} \; 2>/dev/null || true
            find "$d" -name '*.dylib' -exec codesign --remove-signature {} \; 2>/dev/null || true
        fi
        while IFS= read -r -d '' f; do
            codesign --remove-signature "$f" 2>/dev/null || true
            if codesign --force --sign - "$f"; then cs_ok=$((cs_ok + 1))
            else cs_fail=$((cs_fail + 1)); warn "Signing failed: $f"
            fi
        done < <(find "$d" -type f \( -perm -a=x -o -name '*.so' -o -name '*.dylib' \) -print0 2>/dev/null)
    done
    info "  Signing complete: ${cs_ok} succeeded"$([ $cs_fail -gt 0 ] && echo ", ${cs_fail} failed")

    # 3) Clear quarantine attributes again after signing (codesign may re-trigger them)
    xattr -rd com.apple.quarantine "$SCRIPT_DIR" 2>/dev/null || true

    # Record successful signing so subsequent starts can skip this step
    touch "$marker"
}

is_pg_running()   { pg_ready; }
is_api_running()  {
    if [ -f "$API_PID_FILE" ]; then
        local p; p="$(cat "$API_PID_FILE" 2>/dev/null)"
        [ -n "$p" ] && kill -0 "$p" 2>/dev/null && return 0
    fi
    # Fallback: probe the port directly (PID files may be inaccessible across users)
    (echo >/dev/tcp/127.0.0.1/$API_PORT) 2>/dev/null
}
is_nginx_running() {
    if [ -f "$NGINX_PID" ]; then
        local p; p="$(cat "$NGINX_PID" 2>/dev/null)"
        [ -n "$p" ] && kill -0 "$p" 2>/dev/null && return 0
    fi
    (echo >/dev/tcp/127.0.0.1/$NGINX_PORT) 2>/dev/null
}

# Check whether all three services are healthy
is_all_services_running() { is_pg_running && is_api_running && is_nginx_running; }

# Get the PID for each running service (used for status output)
get_pg_pid() {
    # Prefer the PID file when the service was started by the same user
    if [ -f "$PG_DATA/postmaster.pid" ]; then
        local pid_val; pid_val="$(head -1 "$PG_DATA/postmaster.pid" 2>/dev/null)"
        [ -n "$pid_val" ] && echo "$pid_val" && return
    fi
    # Cross-user case: read the first line of postmaster.pid via SQL (PostgreSQL can read its own files internally)
    local pid_val; pid_val="$(psql_cmd -d postgres -tAc \
        "SELECT split_part(pg_read_file('postmaster.pid'), E'\n', 1);" 2>/dev/null | tr -d ' ')"
    if [ -n "$pid_val" ] && [ "$pid_val" != "" ]; then
        echo "$pid_val" && return
    fi
    # Final fallback: try lsof / ss
    local p=""
    p="$(lsof -ti ":$PG_PORT" -sTCP:LISTEN 2>/dev/null | head -1)"
    [ -n "$p" ] && echo "$p" && return
    p="$(ss -tlnp 2>/dev/null | grep ":${PG_PORT} " | grep -oP 'pid=\K[0-9]+' | head -1)"
    [ -n "$p" ] && echo "$p" && return
    echo "?"
}
get_api_pid() {
    if [ -f "$API_PID_FILE" ]; then
        cat "$API_PID_FILE" 2>/dev/null && return
    fi
    local p=""
    p="$(lsof -ti ":$API_PORT" -sTCP:LISTEN 2>/dev/null | head -1)"
    [ -n "$p" ] && echo "$p" && return
    p="$(ss -tlnp 2>/dev/null | grep ":${API_PORT} " | grep -oP 'pid=\K[0-9]+' | head -1)"
    [ -n "$p" ] && echo "$p" && return
    echo "?"
}
get_nginx_pid() {
    if [ -f "$NGINX_PID" ]; then
        cat "$NGINX_PID" 2>/dev/null && return
    fi
    local p=""
    p="$(lsof -ti ":$NGINX_PORT" -sTCP:LISTEN 2>/dev/null | head -1)"
    [ -n "$p" ] && echo "$p" && return
    p="$(ss -tlnp 2>/dev/null | grep ":${NGINX_PORT} " | grep -oP 'pid=\K[0-9]+' | head -1)"
    [ -n "$p" ] && echo "$p" && return
    echo "?"
}

# Find and terminate the process holding a port (fallback when PID files are missing)
# Send SIGTERM first → wait 5 seconds → escalate to SIGKILL if needed
kill_port_holder() {
    local port="$1" label="${2:-}"
    local pids=""
    if [ "$(uname -s)" = "Darwin" ]; then
        pids="$(lsof -ti ":$port" -sTCP:LISTEN 2>/dev/null || true)"
    else
        pids="$(ss -tlnp 2>/dev/null | grep ":${port} " | grep -o 'pid=[0-9]*' | sed 's/pid=//' | sort -u || true)"
    fi
    [ -z "$pids" ] && return 0
    local pid
    # First pass: SIGTERM (graceful shutdown)
    for pid in $pids; do
        [ "$pid" = "$$" ] && continue
        kill "$pid" 2>/dev/null && \
            warn "Sent SIGTERM via port ${port} (PID $pid)${label:+ [$label]}"
    done
    # Wait for the port to be released (up to 10 seconds)
    local i=0
    while is_port_in_use "$port" && [ $i -lt 10 ]; do
        i=$((i + 1)); sleep 1
    done
    sleep 2
    # Second pass: SIGKILL (force stop after SIGTERM timeout)
    if is_port_in_use "$port"; then
        if [ "$(uname -s)" = "Darwin" ]; then
            pids="$(lsof -ti ":$port" -sTCP:LISTEN 2>/dev/null || true)"
        else
            pids="$(ss -tlnp 2>/dev/null | grep ":${port} " | grep -o 'pid=[0-9]*' | sed 's/pid=//' | sort -u || true)"
        fi
        for pid in $pids; do
            [ "$pid" = "$$" ] && continue
            kill -9 "$pid" 2>/dev/null && \
                warn "SIGTERM timed out on port ${port}; sent SIGKILL (PID $pid)${label:+ [$label]}"
        done
        # Wait for SIGKILL to take effect (up to 3 seconds)
        i=0
        while is_port_in_use "$port" && [ $i -lt 6 ]; do
            i=$((i + 1)); sleep 1
        done
        sleep 2
    fi
}

# Wait for a PID to exit (up to max_wait seconds), then force-kill with SIGKILL on timeout
wait_pid_exit() {
    local pid="$1" label="${2:-}" max_wait="${3:-5}"
    local i=0
    while kill -0 "$pid" 2>/dev/null && [ $i -lt $((max_wait * 2)) ]; do
        i=$((i + 1)); sleep 0.5
    done
    if kill -0 "$pid" 2>/dev/null; then
        kill -9 "$pid" 2>/dev/null && \
            warn "${label} SIGTERM timed out; sent SIGKILL (PID $pid)"
        i=0
        while kill -0 "$pid" 2>/dev/null && [ $i -lt 6 ]; do
            i=$((i + 1)); sleep 0.5
        done
    fi
}

# Check whether a port is in use (without relying on PID files)
is_port_in_use() {
    if [ "$(uname -s)" = "Darwin" ]; then
        lsof -i ":$1" -sTCP:LISTEN >/dev/null 2>&1
    else
        ss -tln 2>/dev/null | grep -q ":$1 "
    fi
}

# Find an available port starting from the given base, incrementing up to max_attempts times
# Usage: find_available_port <base_port> <label> <max_attempts>
# Prints the available port to stdout; dies if all attempts fail
find_available_port() {
    local base="$1" label="$2" max_attempts="${3:-3}"
    local port="$base" attempt=0
    while [ $attempt -lt $max_attempts ]; do
        if ! is_port_in_use "$port"; then
            echo "$port"
            return 0
        fi
        warn "Port $port ($label) is already in use; trying $((port + 1)) ..."
        port=$((port + 1))
        attempt=$((attempt + 1))
    done
    die 1010 "All ports $base-$((base + max_attempts - 1)) ($label) are occupied; cannot start"
}

# Check if a PostgreSQL instance on the given port requires authentication.
# Our own instance uses trust auth (no password); if a password is required,
# it means the port is occupied by a foreign PostgreSQL instance.
pg_port_is_foreign() {
    local port="$1"
    # Not listening at all → not foreign
    is_port_in_use "$port" || return 1
    # Server not accepting connections → not a PG instance we can test
    "$(pg_bin)/pg_isready" -h 127.0.0.1 -p "$port" -q 2>/dev/null || return 1
    # Attempt a passwordless query using -w (--no-password) so psql exits immediately
    # instead of waiting for interactive password input when auth is required
    ! "$(pg_bin)/psql" -w -h 127.0.0.1 -p "$port" -U "$PG_USER" -d postgres -tAc "SELECT 1" >/dev/null 2>&1
}

# Find an available PostgreSQL port: skip ports that are occupied OR running a foreign instance
# Usage: find_available_pg_port <base_port> <max_attempts>
find_available_pg_port() {
    local base="$1" max_attempts="${2:-3}"
    local port="$base" attempt=0
    while [ $attempt -lt $max_attempts ]; do
        if is_port_in_use "$port"; then
            if pg_port_is_foreign "$port"; then
                warn "Port $port (PostgreSQL) belongs to a foreign instance (requires password); trying $((port + 1)) ..."
            else
                warn "Port $port (PostgreSQL) is already in use; trying $((port + 1)) ..."
            fi
        else
            echo "$port"
            return 0
        fi
        port=$((port + 1))
        attempt=$((attempt + 1))
    done
    die 1010 "All ports $base-$((base + max_attempts - 1)) (PostgreSQL) are occupied or foreign; cannot start"
}

# Detect system DNS resolvers for nginx.
# Prefers non-loopback entries from /etc/resolv.conf (Linux/WSL),
# falls back to scutil --dns (macOS), then to public DNS as last resort.
detect_resolvers() {
    local resolvers=""
    if [ -r /etc/resolv.conf ]; then
        resolvers=$(grep -E '^nameserver' /etc/resolv.conf \
            | awk '{print $2}' \
            | grep -vE '^(127\.|::1$)' \
            | head -3 \
            | sed -E '/:/ s/.*/[&]/' \
            | tr '\n' ' ')
    fi
    if [ -z "$resolvers" ] && [ "$(uname -s)" = "Darwin" ]; then
        resolvers=$(scutil --dns 2>/dev/null \
            | grep 'nameserver\[' \
            | awk '{print $3}' \
            | grep -vE '^(127\.|::1$)' \
            | sort -u \
            | head -3 \
            | sed -E '/:/ s/.*/[&]/' \
            | tr '\n' ' ')
    fi
    if [ -z "$resolvers" ]; then
        resolvers="223.5.5.5 8.8.8.8"
    fi
    echo "$resolvers"
}

generate_nginx_conf() {
    local conf="$SCRIPT_DIR/config/nginx.conf"
    local mime="$SCRIPT_DIR/config/mime.types"
    local frontend_root="$SCRIPT_DIR/frontend/dist"
    local pkg_root="$SCRIPT_DIR/.."
    local dns_resolvers
    dns_resolvers="$(detect_resolvers)"
    cat > "$conf" << NGINX_EOF
worker_processes auto;
pid "$NGINX_PID";
error_log "$NGINX_LOG_DIR/error.log" warn;
events { worker_connections 256; }
http {
  include "$mime";
  default_type application/octet-stream;
  access_log "$NGINX_LOG_DIR/access.log";
  sendfile on;
  keepalive_timeout 65;
  client_body_temp_path "$DATA_DIR/nginx_temp";
  proxy_temp_path       "$DATA_DIR/nginx_temp";
  map \$http_upgrade \$connection_upgrade { default upgrade; '' close; }
  # Locale-catalog cache policy: immutable only for the content-addressed and
  # revision-addressed paths, and only on a status that carries a payload.
  map \$uri \$catalog_path_cache {
    default                    "public, max-age=0, must-revalidate";
    "~^/locale-catalog/[cr]/"  "public, max-age=31536000, immutable";
  }
  # Brotli negotiation, identical to prod.nginxconf: a client that sends
  # \`br;q=0\` has refused brotli (RFC 9110 §12.5.3), so a positive token is
  # required and any zero-weighted \`br\` vetoes it. Unrecognised parameters
  # fall back to gzip or identity.
  map \$http_accept_encoding \$br_acceptable {
    default 0;
    "~*(^|,)[ \\t]*br[ \\t]*(;[ \\t]*q=(0\\.\\d*[1-9]\\d*|1(\\.0{1,3})?)[ \\t]*)?(,|\$)" 1;
  }
  map \$http_accept_encoding \$br_refused {
    default 0;
    "~*(^|,)[ \\t]*br[ \\t]*;[ \\t]*q=0(\\.0{1,3})?[ \\t]*(,|\$)" 1;
  }
  map "\$br_acceptable\$br_refused" \$accepts_br {
    default 0;
    "10"    1;
  }
  map \$status \$catalog_cache_control {
    default "no-store";
    200     \$catalog_path_cache;
    206     \$catalog_path_cache;
    304     \$catalog_path_cache;
  }
  server {
    listen $NGINX_PORT;
    server_name localhost;
    client_max_body_size 5M;
    location / {
      root "$frontend_root";
      try_files \$uri \$uri/ /index.html;
    }
    # The published locale catalog, with the same guarantees as the hosted
    # deployment: JSON MIME, nosniff, immutable caching for content-addressed
    # and revision paths, revalidation for the mutable manifest, and — because
    # \`^~\` beats the SPA catch-all — a real 404 for anything missing instead of
    # index.html. Error statuses are never storable (see the \$status map),
    # so a client that races a package upgrade cannot cache a 404.
    location ^~ /locale-catalog/ {
      root "$frontend_root";
      default_type application/json;
      gzip_static on;

      # Brotli delivery, mirroring prod.nginxconf: an \`if\` block is a nested
      # configuration level and add_header does not inherit into one that
      # declares its own, so the shared headers are repeated inside it. The
      # response still carries exactly one of each, which
      # scripts/validate-catalog-serving.py asserts against real nginx for this
      # config as well as for prod.
      set \$br "";
      if (\$accepts_br) { set \$br "y"; }
      if (-f "\$request_filename.br") { set \$br "\${br}f"; }
      if (\$br = "yf") {
        add_header Cache-Control \$catalog_cache_control always;
        add_header Vary "Accept-Encoding" always;
        add_header X-Content-Type-Options "nosniff" always;
        add_header Content-Encoding "br" always;
        rewrite ^(.*)\$ \$1.br break;
      }

      add_header Cache-Control \$catalog_cache_control always;
      add_header X-Content-Type-Options "nosniff" always;
      add_header Vary "Accept-Encoding" always;
      # Whole-file fetches only, as in prod.nginxconf.
      max_ranges 0;
    }
    # Card image routing:
    # 1. user cards/
    # 2. user cards_en/
    # 3. built-in frontend/dist/img/arkham/{lang}/cards/
    # 4. built-in frontend/dist/img/arkham/cards/
    # 5. CDN fallback
    location ~ ^/img/arkham/(?<request_lang>zh|fr|es|ko)/cards/(?<card_path>.+)$ {
      root "$pkg_root";
      try_files /cards/\$card_path
                /cards_en/\$card_path
                /game/frontend/dist/img/arkham/\$request_lang/cards/\$card_path
                /game/frontend/dist/img/arkham/cards/\$card_path
                @img_cdn;
    }
    location ~ ^/img/arkham/ita/cards/(?<card_path>.+)$ {
      root "$pkg_root";
      try_files /cards/\$card_path
                /cards_en/\$card_path
                /game/frontend/dist/img/arkham/ita/cards/\$card_path
                /game/frontend/dist/img/arkham/cards/\$card_path
                @img_cdn;
    }
    location ~ ^/img/arkham/cards/(?<card_path>.+)$ {
      root "$pkg_root";
      try_files /cards/\$card_path
                /cards_en/\$card_path
                /game/frontend/dist/img/arkham/cards/\$card_path
                @img_cdn;
    }
    # Localized non-card arkham images (e.g. /img/arkham/fr/tarot/tarot-0.jpg):
    # Priority: user cards/{path} > cards_en/{path} > dist/{lang}/{path} > dist/{path} > CDN
    location ~ ^/img/arkham/(?<request_lang>zh|fr|es|ko|ita)/(?<img_path>.+)$ {
      root "$pkg_root";
      try_files /cards/\$img_path
                /cards_en/\$img_path
                /game/frontend/dist/img/arkham/\$request_lang/\$img_path
                /game/frontend/dist/img/arkham/\$img_path
                @img_cdn;
    }
    # All other arkham images (portraits, boxes, sets, tarot, encounter-sets, root-level, etc.):
    # Priority: user cards/{path} > cards_en/{path} > dist/{path} > CDN
    location ~ ^/img/arkham/(?<img_path>.+)$ {
      root "$pkg_root";
      try_files /cards/\$img_path
                /cards_en/\$img_path
                /game/frontend/dist/img/arkham/\$img_path
                @img_cdn;
    }
    # Non-arkham images (e.g. /img/icons/favicon.ico)
    location /img/ {
      root "$frontend_root";
      try_files \$uri @img_cdn;
    }
    location @img_cdn {
      # Use a variable so nginx resolves the hostname at request time, not at startup.
      # This prevents nginx from failing to start when DNS is unavailable (e.g. offline WSL).
      set \$cdn_upstream "http://assets.arkhamhorror.app";
      resolver $dns_resolvers valid=300s ipv6=off;
      resolver_timeout 5s;
      expires 10m;
      add_header Server-Timing "cdn" always;
      proxy_pass \$cdn_upstream\$request_uri;
      proxy_set_header Host assets.arkhamhorror.app;
    }
    location /api {
      proxy_set_header X-Forwarded-For \$proxy_add_x_forwarded_for;
      proxy_set_header Host \$http_host;
      proxy_redirect off;
      proxy_pass http://127.0.0.1:$API_PORT;
      proxy_set_header X-Real-IP \$remote_addr;
      proxy_http_version 1.1;
      proxy_set_header Upgrade \$http_upgrade;
      proxy_set_header Connection \$connection_upgrade;
    }
    location /health {
      proxy_set_header X-Forwarded-For \$proxy_add_x_forwarded_for;
      proxy_set_header Host \$http_host;
      proxy_redirect off;
      proxy_pass http://127.0.0.1:$API_PORT;
      proxy_set_header X-Real-IP \$remote_addr;
      proxy_http_version 1.1;
      proxy_set_header Upgrade \$http_upgrade;
      proxy_set_header Connection \$connection_upgrade;
    }
  }
}
NGINX_EOF
}

validate_nginx_config() {
    ensure_dir "$DATA_DIR/nginx_temp"
    generate_nginx_conf
    touch "$NGINX_LOG_DIR/error.log" "$NGINX_LOG_DIR/access.log" 2>/dev/null || true
    verify_nginx_identity
    "$SCRIPT_DIR/bin/nginx" -t -p "$SCRIPT_DIR" -c "$SCRIPT_DIR/config/nginx.conf" \
        || die 1026 "Packaged nginx rejected its generated configuration" "$NGINX_LOG_DIR/error.log"
}

# This test-only action still invokes the actual shipped launcher, generated
# config, binary, and bundled-library environment. It intentionally starts no
# database or API because catalog files are static.
serve_nginx_for_validation() {
    validate_nginx_config
    exec "$SCRIPT_DIR/bin/nginx" -p "$SCRIPT_DIR" -e "$NGINX_LOG_DIR/error.log" \
        -c "$SCRIPT_DIR/config/nginx.conf" -g 'daemon off;'
}

# ── Stop ─────────────────────────────────────────────────────────────────────
do_stop() {
    info "Stopping services ..."

    # --- nginx ---
    if is_nginx_running; then
        local nginx_pid
        nginx_pid="$(cat "$NGINX_PID" 2>/dev/null || echo "")"
        verify_nginx_identity
        "$SCRIPT_DIR/bin/nginx" -e "$NGINX_LOG_DIR/error.log" \
            -c "$SCRIPT_DIR/config/nginx.conf" -s stop 2>/dev/null || true
        [ -n "$nginx_pid" ] && wait_pid_exit "$nginx_pid" "nginx" 5
        info "nginx stopped"
    fi
    # Port-based fallback: clean up leftover nginx when the PID file is missing
    if is_port_in_use "$NGINX_PORT"; then
        kill_port_holder "$NGINX_PORT" "nginx"
    fi
    rm -f "$NGINX_PID"

    # --- arkham-api ---
    if is_api_running; then
        local api_pid
        api_pid="$(cat "$API_PID_FILE" 2>/dev/null || echo "")"
        kill "$api_pid" 2>/dev/null || true
        [ -n "$api_pid" ] && wait_pid_exit "$api_pid" "arkham-api" 5
        info "arkham-api stopped"
    fi
    if is_port_in_use "$API_PORT"; then
        kill_port_holder "$API_PORT" "arkham-api"
    fi
    rm -f "$API_PID_FILE"

    # Export the logical backup while PostgreSQL is still running and the backend is already stopped.
    backup_database_dump || true

    # --- PostgreSQL ---
    if [ -f "$PG_DATA/postmaster.pid" ]; then
        # -m fast: roll back active transactions and disconnect clients; -w: wait for shutdown to complete
        pg_ctl -D "$PG_DATA" -m fast -w stop 2>/dev/null || true
        info "PostgreSQL stopped"
    fi
    if is_port_in_use "$PG_PORT"; then
        kill_port_holder "$PG_PORT" "postgresql"
    fi
    # Clean up leftover PostgreSQL PID and socket files
    rm -f "$PG_DATA/postmaster.pid" 2>/dev/null
    rm -f "$PG_SOCKET_DIR/.s.PGSQL.${PG_PORT}" 2>/dev/null
    rm -f "$PG_SOCKET_DIR/.s.PGSQL.${PG_PORT}.lock" 2>/dev/null

    # Clear runtime logs after a normal shutdown (keep them when startup fails for troubleshooting)
    if [ "$_STARTUP_SUCCEEDED" = "1" ]; then
        for f in "$PG_LOG" \
                 "$DATA_DIR/initdb.log" \
                 "$DATA_DIR/psql.log" \
                 "$PG_DUMP_LOG" \
                 "$PG_RESTORE_LOG" \
                 "$DATA_DIR/arkham-api.log" \
                 "$NGINX_LOG_DIR/error.log" \
                 "$NGINX_LOG_DIR/access.log"; do
            [ -f "$f" ] && :> "$f"
        done
    fi

    # --- Final verification: ensure all ports have been released ---
    local still_occupied=""
    is_port_in_use "$NGINX_PORT" && still_occupied="${still_occupied} ${NGINX_PORT}(nginx)"
    is_port_in_use "$API_PORT"   && still_occupied="${still_occupied} ${API_PORT}(api)"
    is_port_in_use "$PG_PORT"    && still_occupied="${still_occupied} ${PG_PORT}(pg)"
    if [ -n "$still_occupied" ]; then
        warn "[4004] These ports are still in use:${still_occupied}; attempting forced cleanup with SIGKILL ..."
        is_port_in_use "$NGINX_PORT" && kill_port_holder "$NGINX_PORT" "nginx"
        is_port_in_use "$API_PORT"   && kill_port_holder "$API_PORT" "arkham-api"
        is_port_in_use "$PG_PORT"    && kill_port_holder "$PG_PORT" "postgresql"
    fi

    # WSL/NTFS: file handles may linger briefly after process exit, so wait a bit
    if grep -qi microsoft /proc/version 2>/dev/null; then
        sleep 1
    fi

    info "All services stopped."
}

# ── Status ───────────────────────────────────────────────────────────────────
do_status() {
    echo ""
    printf '%sService Status%s\n\n' "$BOLD" "$RESET"
    if is_pg_running; then
        printf '  %s●%s PostgreSQL   %sRunning%s (Port %s, PID %s)\n' "$GREEN" "$RESET" "$GREEN" "$RESET" "$PG_PORT" "$(get_pg_pid)"
    else
        printf '  %s●%s PostgreSQL   %sStopped%s\n' "$RED" "$RESET" "$RED" "$RESET"
    fi
    if is_api_running; then
        printf '  %s●%s arkham-api   %sRunning%s (Port %s, PID %s)\n' "$GREEN" "$RESET" "$GREEN" "$RESET" "$API_PORT" "$(get_api_pid)"
    else
        printf '  %s●%s arkham-api   %sStopped%s\n' "$RED" "$RESET" "$RED" "$RESET"
    fi
    if is_nginx_running; then
        printf '  %s●%s nginx        %sRunning%s (Port %s, PID %s)\n' "$GREEN" "$RESET" "$GREEN" "$RESET" "$NGINX_PORT" "$(get_nginx_pid)"
    else
        printf '  %s●%s nginx        %sStopped%s\n' "$RED" "$RESET" "$RED" "$RESET"
    fi
    echo ""
}

# ── Initialize database ──────────────────────────────────────────────────────
init_database() {
    info "First run detected, initializing database ..."
    ensure_dir "$DATA_DIR"
    rm -f "$PG_LOG" "$DATA_DIR/psql.log" "$DATA_DIR/initdb.log" "$PG_RESTORE_LOG" 2>/dev/null || true
    ensure_dir "$PG_DATA"
    "$(pg_bin)/initdb" -D "$PG_DATA" -U "$PG_USER" --no-locale -E UTF8 \
        > "$DATA_DIR/initdb.log" 2>&1 \
        || die 2003 "initdb initialization failed" "$DATA_DIR/initdb.log"

    cat > "$PG_DATA/pg_hba.conf" << HBA_EOF
local   all   all                 trust
host    all   all   127.0.0.1/32  trust
host    all   all   ::1/128       trust
HBA_EOF

    cat >> "$PG_DATA/postgresql.conf" << PG_CONF_EOF

port = $PG_PORT
unix_socket_directories = '$PG_SOCKET_DIR'
listen_addresses = '127.0.0.1'
PG_CONF_EOF

    info "Database schema initialization complete."
}

# ── Start ────────────────────────────────────────────────────────────────────
do_start() {
    # Record start time. GNU date supports %N (nanoseconds); macOS date does not, falls back to integer seconds.
    local _start_epoch; _start_epoch="$(date +%s.%N 2>/dev/null)"; _start_epoch="${_start_epoch/N/0}"
    [ -x "$SCRIPT_DIR/bin/arkham-api" ] || die 1002 "bin/arkham-api not found"
    [ -x "$SCRIPT_DIR/bin/nginx" ]      || die 1003 "bin/nginx not found"
    [ -d "$SCRIPT_DIR/pgsql/bin" ]      || die 1004 "pgsql/bin/ not found"
    [ -d "$SCRIPT_DIR/frontend/dist" ]  || die 1005 "frontend/dist/ not found"

    if is_nginx_running || is_api_running || is_pg_running \
       || is_port_in_use "$NGINX_PORT" || is_port_in_use "$API_PORT" || is_port_in_use "$PG_PORT"; then
        warn "Detected occupied services or ports; cleaning up first ..."
        do_stop; sleep 1
    fi

    # Auto-increment ports if still occupied by external processes (up to 3 attempts each)
    # PostgreSQL uses a dedicated check that also detects foreign instances requiring authentication
    PG_PORT="$(find_available_pg_port "$PG_PORT" 3)"
    API_PORT="$(find_available_port "$API_PORT" "arkham-api" 3)"
    NGINX_PORT="$(find_available_port "$NGINX_PORT" "nginx" 3)"

    # Enable cleanup protection: from here until startup fully succeeds, any abnormal exit triggers do_stop automatically
    _CLEANUP_ON_EXIT=1

    # macOS: sign all binaries + clear quarantine (only performed at startup)
    ensure_macos_signing

    ensure_dir "$DATA_DIR"

    # 1. PostgreSQL
    # Store pgdata under the OS user data directory to avoid WSL/NTFS permission issues.
    # Restore from logical backup when the live cluster is missing or invalid.
    ensure_dir "$PGDATA_OS_PARENT" || die 2001 "Unable to create user data directory"
    ensure_dir "$BACKUP_DIR"

    if ! acquire_start_lock; then
        wait_for_existing_startup || die 2010 "Another startup is already in progress"
        _CLEANUP_ON_EXIT=0
        return 0
    fi

    # ── Handle force_init.dump: forced re-initialization from a user-supplied dump ──
    local FORCE_INIT_DUMP="$BACKUP_DIR/force_init.dump"
    local is_fresh=0
    if [ -f "$FORCE_INIT_DUMP" ]; then
        info "Detected force_init.dump; will re-initialize the database cluster ..."

        # If the current cluster is valid, export a backup as old.dump before destroying it
        if cluster_is_valid "$PG_DATA"; then
            info "Backing up current cluster before forced re-initialization ..."
            # Temporarily start PostgreSQL to perform the backup
            start_postgres
            if database_exists; then
                local OLD_DUMP="$BACKUP_DIR/old.dump"
                rm -f "$OLD_DUMP" 2>/dev/null || true
                if pg_dump_cmd -d "$PG_DB" -Fc -f "$OLD_DUMP" > "$PG_DUMP_LOG" 2>&1 \
                   && dump_file_is_valid "$OLD_DUMP"; then
                    info "Current database backed up to $OLD_DUMP"
                else
                    warn "Failed to back up current database (continuing with forced init anyway)"
                    rm -f "$OLD_DUMP" 2>/dev/null || true
                fi
            fi
            pg_ctl -D "$PG_DATA" -m fast -w stop 2>/dev/null || true
        fi

        # Destroy existing cluster
        cleanup_invalid_cluster "$PG_DATA"

        if dump_file_is_valid "$FORCE_INIT_DUMP"; then
            # Restore from force_init.dump
            if restore_database_from_dump "$FORCE_INIT_DUMP"; then
                info "Forced initialization from force_init.dump complete."
            else
                warn "Restore from force_init.dump failed; falling back to fresh initialization ..."
                pg_ctl -D "$PG_DATA" -m immediate stop 2>/dev/null || true
                cleanup_invalid_cluster "$PG_DATA"
                init_database
                is_fresh=1
            fi
        else
            warn "force_init.dump is invalid; starting fresh initialization ..."
            init_database
            is_fresh=1
        fi

        # Remove force_init.dump after processing (one-shot trigger)
        rm -f "$FORCE_INIT_DUMP" 2>/dev/null || true
        date +%s > "$VERSION_FILE"
        date +%s > "$VERSION_FILE_LOCAL"
    fi

    if [ "$is_fresh" != "1" ] && ! cluster_is_valid "$PG_DATA"; then
        [ -e "$PG_DATA" ] && cleanup_invalid_cluster "$PG_DATA"

        local restored=0
        # Try latest.dump first, then fall back to previous.dump
        for _dump_candidate in "$PG_DUMP_FILE" "$PG_DUMP_PREV"; do
            [ "$restored" = "1" ] && break
            if dump_file_is_valid "$_dump_candidate"; then
                if restore_database_from_dump "$_dump_candidate"; then
                    restored=1
                else
                    warn "Restore from $(basename "$_dump_candidate") failed; cleaning up ..."
                    # Stop PG if it was started during the failed restore, then wipe the broken cluster
                    pg_ctl -D "$PG_DATA" -m immediate stop 2>/dev/null || true
                    cleanup_invalid_cluster "$PG_DATA"
                fi
            fi
        done

        if [ "$restored" = "0" ]; then
            if cluster_is_valid "$PGDATA_LOCAL"; then
                info "Legacy physical backup detected; migrating it to the user data directory ..."
                cp -a "$PGDATA_LOCAL" "$PG_DATA" || die 2002 "Old data migration failed"
                date +%s > "$VERSION_FILE"
                date +%s > "$VERSION_FILE_LOCAL"
            else
                init_database
                is_fresh=1
                date +%s > "$VERSION_FILE"
                date +%s > "$VERSION_FILE_LOCAL"
            fi
        fi
    fi

    # Check and repair pgdata permissions (PostgreSQL requires 0700 or 0750)
    local pg_perms
    if [ "$(uname -s)" = "Darwin" ]; then
        pg_perms="$(stat -f '%Lp' "$PG_DATA" 2>/dev/null || echo "000")"
    else
        pg_perms="$(stat -c '%a' "$PG_DATA" 2>/dev/null || echo "000")"
    fi
    if [ "$pg_perms" != "700" ] && [ "$pg_perms" != "750" ]; then
        warn "pgdata permissions are invalid ($pg_perms); fixing to 0700 ..."
        chmod 700 "$PG_DATA" 2>/dev/null || true
        find "$PG_DATA" -type d -exec chmod 700 {} \; 2>/dev/null || true
        find "$PG_DATA" -type f -exec chmod 600 {} \; 2>/dev/null || true
    fi

    # Ensure unix_socket_directories points to the current PG_SOCKET_DIR
    if grep -q 'unix_socket_directories' "$PG_DATA/postgresql.conf" 2>/dev/null; then
        if [ "$(uname -s)" = "Darwin" ]; then
            sed -i '' "s|^unix_socket_directories = .*|unix_socket_directories = '$PG_SOCKET_DIR'|" "$PG_DATA/postgresql.conf"
        else
            sed -i "s|^unix_socket_directories = .*|unix_socket_directories = '$PG_SOCKET_DIR'|" "$PG_DATA/postgresql.conf"
        fi
    fi

    # Unified PostgreSQL startup (restore_database_from_dump starts PostgreSQL already)
    start_postgres

    # Create the database on the first run of a fresh cluster only
    if [ "$is_fresh" = "1" ]; then
        info "Creating database ..."
        psql_cmd -d postgres -c "CREATE DATABASE \"$PG_DB\";" \
            > "$DATA_DIR/psql.log" 2>&1 \
            || die 2006 "Failed to create database" "$DATA_DIR/psql.log"
        psql_cmd -d "$PG_DB" -f "$DATA_DIR/setup.sql" \
            >> "$DATA_DIR/psql.log" 2>&1 \
            || die 2007 "Schema import failed" "$DATA_DIR/psql.log"
        info "Database initialization complete."
    fi

    # Recovery check: ensure the database exists (prevents retries from skipping DB creation after a first-run failure)
    if ! database_exists; then
        info "Database $PG_DB does not exist; recreating it ..."
        psql_cmd -d postgres -c "CREATE DATABASE \"$PG_DB\";" \
            >> "$DATA_DIR/psql.log" 2>&1 \
            || die 2006 "Failed to create database" "$DATA_DIR/psql.log"
        psql_cmd -d "$PG_DB" -f "$DATA_DIR/setup.sql" \
            >> "$DATA_DIR/psql.log" 2>&1 \
            || die 2007 "Schema import failed" "$DATA_DIR/psql.log"
        info "Database recreation complete."
    fi

    # 2. arkham-api (port 3002)
    info "Starting backend API ..."
    export DATABASE_URL="postgres://${PG_USER}@127.0.0.1:${PG_PORT}/${PG_DB}"
    export PORT="$API_PORT"
    export PGHOST="127.0.0.1" PGPORT="$PG_PORT" PGSSLMODE="disable"
    # Auto-detect: use local relative paths when local images exist; fall back to the CDN otherwise (same behavior as web-entrypoint.sh)
    if [ -z "${ASSET_HOST+x}" ]; then
      if [ -n "$(ls -A "$SCRIPT_DIR/frontend/dist/img" 2>/dev/null)" ]; then
        export ASSET_HOST=""
      else
        export ASSET_HOST="https://assets.arkhamhorror.app"
      fi
    fi
    # nohup prevents SIGHUP; redirect stdout/stderr to log files so SIGPIPE cannot kill the process when the pipe closes
    ( cd "$SCRIPT_DIR"; nohup "$SCRIPT_DIR/bin/arkham-api" >> "$DATA_DIR/arkham-api.log" 2>&1 & echo $! > "$API_PID_FILE" )

    local tries=0
    while ! curl -fs "http://127.0.0.1:${API_PORT}/health" > /dev/null 2>&1; do
        tries=$((tries + 1)); [ "$tries" -gt 150 ] && die 3003 "Backend API startup timed out" "$DATA_DIR/arkham-api.log"; sleep 0.2
    done
    info "Backend API started (port $API_PORT, PID $(get_api_pid))"

    # 3. nginx (port 3000)
    info "Configuring and starting nginx ..."

    # Ensure user-facing card image directories exist (at pkg root, one level above game/)
    ensure_dir "$SCRIPT_DIR/../cards"
    ensure_dir "$SCRIPT_DIR/../cards_en"

    validate_nginx_config
    sync 2>/dev/null || true
    if ! "$SCRIPT_DIR/bin/nginx" -e "$NGINX_LOG_DIR/error.log" -c "$SCRIPT_DIR/config/nginx.conf" 2>&1; then
        die 3002 "nginx failed to start" "$NGINX_LOG_DIR/error.log"
    fi
    info "nginx started (port $NGINX_PORT, PID $(get_nginx_pid))"

    # ── Post-start health check: wait 2 seconds, then verify all services are still alive ─
    # Prevent a silent crash (for example, delayed termination by macOS Gatekeeper) from being treated as a successful startup
    sleep 0.5
    if ! is_pg_running;  then die 3004 "PostgreSQL crashed after startup" "$PG_LOG"; fi
    if ! is_api_running; then die 3005 "arkham-api crashed after startup" "$DATA_DIR/arkham-api.log"; fi
    if ! is_nginx_running; then die 3006 "nginx crashed after startup" "$NGINX_LOG_DIR/error.log"; fi

    # Startup succeeded completely; disable cleanup protection
    _CLEANUP_ON_EXIT=0
    _STARTUP_SUCCEEDED=1
    release_start_lock

    echo ""
    printf '%s============================================%s\n' "$CYAN" "$RESET"
    printf '%s  Arkham Horror LCG Started!%s\n' "$BOLD" "$RESET"
    printf '%s============================================%s\n' "$CYAN" "$RESET"
    echo ""
    print_access_urls
    echo ""
    printf '  %-14s PID %-8s Port %s\n' "PostgreSQL" "$(get_pg_pid)" "$PG_PORT"
    printf '  %-14s PID %-8s Port %s\n' "arkham-api" "$(get_api_pid)" "$API_PORT"
    printf '  %-14s PID %-8s Port %s\n' "nginx" "$(get_nginx_pid)" "$NGINX_PORT"
    echo ""
    local pkg_name; pkg_name="$(basename "$(dirname "$SCRIPT_DIR")")"
    local dir_name; dir_name="$(basename "$SCRIPT_DIR")"
    printf '  Status: %sbash %s/%s/start.sh --status%s\n' "$CYAN" "$pkg_name" "$dir_name" "$RESET"
    printf '  Stop:   %sbash %s/%s/start.sh --stop%s\n' "$CYAN" "$pkg_name" "$dir_name" "$RESET"

    # Display current version from marker file (absent before first update → show "dev")
    local _cur_ver="dev"
    for _marker in "$SCRIPT_DIR"/current_v[0-9]*; do
        [ -e "$_marker" ] || continue
        _cur_ver="$(basename "$_marker")"
        _cur_ver="${_cur_ver#current_}"
        break
    done
    printf '  Version: %s%s%s\n' "$GREEN" "$_cur_ver" "$RESET"
    local _end_epoch; _end_epoch="$(date +%s.%N 2>/dev/null)"; _end_epoch="${_end_epoch/N/0}"
    local _elapsed; _elapsed="$(awk "BEGIN{printf \"%.1f\", ${_end_epoch} - ${_start_epoch}}")"
    printf '  Started in %s%ss%s\n' "$MAGENTA" "$_elapsed" "$RESET"
    echo ""

    # Open the browser automatically (silent; failure does not affect services)
    warn_localhost_fallback_if_needed
    open_browser "$(get_browser_url)"
}

# ── Foreground keepalive: keep the terminal window open and stop all services automatically when the window closes ─
run_foreground() {
    # Graceful shutdown handler for HUP/INT/TERM signals.
    # When the terminal is already dead (e.g. Command+Q on macOS closes pty before
    # delivering SIGHUP), writing to stdout/stderr returns EIO. Under set -e this would
    # abort the handler before do_stop runs. Detect and redirect to avoid this.
    _on_exit_signal() {
        if ! : >/dev/tty 2>/dev/null; then
            # Terminal is gone; redirect all output to prevent EIO failures
            exec >/dev/null 2>&1
        fi
        info "Exit signal received, stopping all services ..."
        do_stop
        trap - EXIT
        close_terminal_window_if_needed
        exit 0
    }
    trap '_on_exit_signal' HUP INT TERM
    trap 'do_stop >/dev/null 2>&1 || true' EXIT

    echo ""
    printf '%s────────────────────────────────────────────%s\n' "$CYAN" "$RESET"
    printf '  Keep this terminal window open to keep the services running.\n'
    printf '  Press %sCtrl+C%s or %sclose this window%s to stop all services automatically.\n' "$BOLD" "$RESET" "$BOLD" "$RESET"
    printf '%s────────────────────────────────────────────%s\n' "$CYAN" "$RESET"
    echo ""

    # All platforms: poll /dev/tty every 5 seconds as a safety net.
    #
    # WSL:         Windows terminates wsl.exe without delivering any signal — polling
    #              is the ONLY cleanup mechanism.
    # macOS/Linux: SIGHUP is normally delivered on terminal close, but Command+Q or
    #              force-quit may bypass it. If SIGHUP works, the trap fires first and
    #              the poll never triggers; otherwise the poll catches it within 5s.
    #
    # Why not `read -t N </dev/tty`: bash's read -t uses alarm()/SIGALRM internally.
    # On WSL2 bash, when redirected from a pty device, SIGALRM can escape the internal
    # handler and kill the script (exit 142), breaking Start-ArkhamHorror.bat's retry logic.
    #
    # Cost: one fork(sleep) every 5s — negligible next to PG + API + nginx.
    while true; do
        sleep 5 || true
        if ! : < /dev/tty 2>/dev/null; then
            exec >/dev/null 2>&1
            info "Terminal disconnected (window closed), stopping all services ..."
            do_stop
            trap - EXIT
            exit 0
        fi
    done
}

# ── Argument parsing ──────────────────────────────────────────────────────────
ACTION="start"
while [ $# -gt 0 ]; do
    case "$1" in
        --stop)   ACTION="stop"; shift ;;
        --status) ACTION="status"; shift ;;
        --validate-nginx-config) ACTION="validate-nginx-config"; shift ;;
        --serve-nginx-for-validation) ACTION="serve-nginx-for-validation"; shift ;;
        --help|-h)
            echo "Usage: bash start.sh [--stop|--status|--validate-nginx-config|--help]"; exit 0 ;;
        *) die 1099 "Unknown argument: $1" ;;
    esac
done

verify_nginx_runtime_closure
configure_runtime_env

case "$ACTION" in
    start)
        # First check whether all services are already healthy and running
        if is_all_services_running; then
            info "All services are already running."
            echo ""
            printf '%s============================================%s\n' "$CYAN" "$RESET"
            printf '%s  Arkham Horror LCG (Already Running)%s\n' "$BOLD" "$RESET"
            printf '%s============================================%s\n' "$CYAN" "$RESET"
            echo ""
            print_access_urls
            echo ""
            printf '  %-14s PID %-8s Port %s\n' "PostgreSQL" "$(get_pg_pid)" "$PG_PORT"
            printf '  %-14s PID %-8s Port %s\n' "arkham-api" "$(get_api_pid)" "$API_PORT"
            printf '  %-14s PID %-8s Port %s\n' "nginx" "$(get_nginx_pid)" "$NGINX_PORT"
            echo ""
            printf '  This window will close automatically in 10 seconds.\n'
            echo ""
            warn_localhost_fallback_if_needed
            open_browser "$(get_browser_url)"
            sleep 10
            close_terminal_window_if_needed
            exit 10
        else
            do_start
            if [ "$_STARTED_BY_OTHER" = "1" ]; then
                echo ""
                printf '  Services are already running. This window will close automatically in 10 seconds.\n'
                echo ""
                warn_localhost_fallback_if_needed
                open_browser "$(get_browser_url)"
                sleep 10
                close_terminal_window_if_needed
                exit 10
            fi
            run_foreground
        fi
        ;;
    stop)   do_stop ;;
    status) do_status ;;
    validate-nginx-config) validate_nginx_config ;;
    serve-nginx-for-validation) serve_nginx_for_validation ;;
esac
LAUNCHSCRIPT

    chmod +x "${PKG_DIR}/game/start.sh"
    info "  ✓ start.sh generated"
}

# ── Generate Start-ArkhamHorror.bat (Windows launcher) ────────────────────────

generate_start_bat() {
    cat > "${PKG_DIR}/Start-ArkhamHorror.bat" << 'BATSCRIPT'
@echo off
chcp 65001 >nul 2>&1
setlocal EnableDelayedExpansion

title Arkham Horror LCG

echo.
echo   ============================================
echo     Arkham Horror LCG - Windows Offline Launcher
echo   ============================================
echo.

REM ---- 1. Check whether WSL is installed ----
where wsl >nul 2>&1
if %ERRORLEVEL% neq 0 goto :NO_WSL
goto :HAS_WSL

:NO_WSL
echo [^!] WSL is not installed. Starting installation...
echo     Administrator privileges are required, and a reboot will be needed after installation.
echo.
powershell -Command "Start-Process cmd -ArgumentList '/c wsl --install -d Ubuntu && pause' -Verb RunAs"
echo.
echo [^!] WSL installation has been launched in an elevated window.
echo     After installation finishes and the computer is restarted, double-click Start-ArkhamHorror.bat again.
echo.
pause
exit /b 0

:HAS_WSL
REM ---- 2. Probe Ubuntu distributions one by one ----
echo [*] Detecting Ubuntu distributions...
set "WSL_DISTRO="

wsl -d Ubuntu -- echo ok >nul 2>&1
if !ERRORLEVEL! equ 0 (set "WSL_DISTRO=Ubuntu"& goto :FOUND_DISTRO)

wsl -d Ubuntu-24.04 -- echo ok >nul 2>&1
if !ERRORLEVEL! equ 0 (set "WSL_DISTRO=Ubuntu-24.04"& goto :FOUND_DISTRO)

wsl -d Ubuntu-22.04 -- echo ok >nul 2>&1
if !ERRORLEVEL! equ 0 (set "WSL_DISTRO=Ubuntu-22.04"& goto :FOUND_DISTRO)

wsl -d Ubuntu-20.04 -- echo ok >nul 2>&1
if !ERRORLEVEL! equ 0 (set "WSL_DISTRO=Ubuntu-20.04"& goto :FOUND_DISTRO)

wsl -d Ubuntu-18.04 -- echo ok >nul 2>&1
if !ERRORLEVEL! equ 0 (set "WSL_DISTRO=Ubuntu-18.04"& goto :FOUND_DISTRO)

REM All probes failed; try installing Ubuntu
goto :NO_UBUNTU

:NO_UBUNTU
echo [^!] No Ubuntu distribution was found. Installing one now...
echo.
wsl --install -d Ubuntu
if !ERRORLEVEL! neq 0 goto :INSTALL_FAILED
echo.
echo [^!] Ubuntu installation completed. Please double-click Start-ArkhamHorror.bat again.
echo.
pause
exit /b 0

:INSTALL_FAILED
echo.
echo [^!] Ubuntu installation failed. Please install it manually:
echo     Open PowerShell and run: wsl --install -d Ubuntu
echo.
pause
exit /b 1

:FOUND_DISTRO
echo [*] Using distribution: !WSL_DISTRO!
echo.

REM ---- 2.5. Ensure the arkham user exists (PostgreSQL does not allow root) ----
wsl -d !WSL_DISTRO! -u root -- id arkham >nul 2>&1
if !ERRORLEVEL! equ 0 goto :HASUSER
echo [*] Creating arkham user...
wsl -d !WSL_DISTRO! -u root -- useradd -m -s /bin/bash arkham
wsl -d !WSL_DISTRO! -u root -- id arkham >nul 2>&1
if !ERRORLEVEL! neq 0 goto :USER_FAILED
echo [*] arkham user created
goto :HASUSER

:USER_FAILED
echo.
echo [^!] Failed to create the arkham user. Please run manually:
echo     wsl -d !WSL_DISTRO! -u root -- useradd -m -s /bin/bash arkham
echo.
pause
exit /b 1

:HASUSER
REM ---- 3. Get the WSL path for the current directory ----
set "WIN_DIR=%~dp0"
if "!WIN_DIR:~-1!"=="\" set "WIN_DIR=!WIN_DIR:~0,-1!"

for /f "tokens=*" %%i in ('wsl -d !WSL_DISTRO! wslpath -u "!WIN_DIR!" 2^>nul') do set "WSL_DIR=%%i"
if "!WSL_DIR!"=="" goto :PATH_FAILED
goto :PATH_OK

:PATH_FAILED
echo.
echo [^!] WSL path conversion failed. The path may contain special characters.
echo     Please move the folder to a path without special characters (for example, C:\ArkhamHorror\).
echo.
pause
exit /b 1

:PATH_OK
echo [*] Package path: !WIN_DIR!
echo [*] WSL path:    !WSL_DIR!
echo.

REM ---- 4. Start services ----
echo [*] Starting Arkham Horror LCG ...
echo.
wsl -d !WSL_DISTRO! -u arkham -- bash -c "cd '!WSL_DIR!/game' && bash start.sh"
set START_EXIT=!ERRORLEVEL!

if !START_EXIT! equ 10 goto :ALREADY_RUNNING
if !START_EXIT! equ 0 goto :START_OK
goto :START_RETRY

REM ---- 5. Startup failed: retry after restart ----
:START_RETRY
echo.
echo [^!] Startup failed (exit code: !START_EXIT!). Restarting !WSL_DISTRO! and retrying...
echo.
wsl -d !WSL_DISTRO! -u arkham -- bash -c "cd '!WSL_DIR!/game' && bash start.sh --stop" 2>nul
wsl --terminate !WSL_DISTRO! >nul 2>nul
timeout /t 2 /nobreak >nul
echo [*] Starting services again...
echo.
wsl -d !WSL_DISTRO! -u arkham -- bash -c "cd '!WSL_DIR!/game' && bash start.sh"
set START_EXIT=!ERRORLEVEL!
if !START_EXIT! equ 10 goto :ALREADY_RUNNING
if !START_EXIT! equ 0 goto :START_OK

echo.
echo [^!] It still failed after restart (exit code: !START_EXIT!). Please check the error messages above.
echo.
wsl -d !WSL_DISTRO! -u arkham -- bash -c "cd '!WSL_DIR!/game' && bash start.sh --stop" 2>nul
goto :END

:START_OK
echo.
goto :END

:ALREADY_RUNNING
echo.
echo [*] Services are already running. Closing this window now.
echo.
goto :END_NO_PAUSE

:END
pause

:END_NO_PAUSE
BATSCRIPT

    # The bat file must use CRLF line endings (required on Windows)
    if command -v unix2dos >/dev/null 2>&1; then
        unix2dos "${PKG_DIR}/Start-ArkhamHorror.bat" 2>/dev/null || true
    elif command -v sed >/dev/null 2>&1; then
        sed -i.bak 's/$/\r/' "${PKG_DIR}/Start-ArkhamHorror.bat" 2>/dev/null && rm -f "${PKG_DIR}/Start-ArkhamHorror.bat.bak" || true
    fi

    info "  ✓ Start-ArkhamHorror.bat generated"
}

# ── Generate mime.types ───────────────────────────────────────────────────────

generate_mime_types() {
    ensure_dir "${PKG_DIR}/game/config"
    cat > "${PKG_DIR}/game/config/mime.types" << 'MIME_EOF'
types {
  text/html                             html htm shtml;
  text/css                              css;
  text/xml                              xml;
  application/javascript                js mjs;
  application/json                      json;
  image/png                             png;
  image/jpeg                            jpeg jpg;
  image/gif                             gif;
  image/svg+xml                         svg svgz;
  image/webp                            webp;
  image/x-icon                          ico;
  application/font-woff                 woff;
  application/font-woff2                woff2;
  font/ttf                              ttf;
  font/otf                              otf;
  application/wasm                      wasm;
  application/octet-stream              bin;
  text/plain                            txt;
}
MIME_EOF
    substep "mime.types generated"
}

# ── Generate Start-ArkhamHorror.command ───────────────────────────────────────

generate_macos_command() {
    cat > "${PKG_DIR}/Start-ArkhamHorror.command" << 'MACCOMMAND'
#!/bin/bash
cd "$(dirname "$0")/game" && bash start.sh
MACCOMMAND
    chmod +x "${PKG_DIR}/Start-ArkhamHorror.command"
    info "  ✓ Start-ArkhamHorror.command generated"
}

# ═════════════════════════════════════════════════════════════════════════════
# Generate update.sh — authenticated platform-specific updater
# ═════════════════════════════════════════════════════════════════════════════

generate_update_script() {
    substep "Generating authenticated update.sh ..."
    [ -f "${SCRIPT_DIR}/update-runtime.sh" ] \
        || die "Trusted update-runtime.sh is missing: ${SCRIPT_DIR}/update-runtime.sh"
    cp "${SCRIPT_DIR}/update-runtime.sh" "${PKG_DIR}/game/update.sh"
    chmod +x "${PKG_DIR}/game/update.sh"
    info "  ✓ game/update.sh generated"
}

generate_update_launcher() {
    cat > "${PKG_DIR}/Update-ArkhamHorror.sh" << 'UPDATELAUNCHER'
#!/usr/bin/env bash
set -euo pipefail

BASE_DIR="$(cd "$(dirname "$0")" && pwd -P)"
UPDATE_SOURCE="${BASE_DIR}/game/update.sh"
[ -f "$UPDATE_SOURCE" ] && [ ! -L "$UPDATE_SOURCE" ] && [ -x "$UPDATE_SOURCE" ] \
    || { echo "Update launcher: missing regular executable game/update.sh" >&2; exit 1; }
if [ "$#" -gt 1 ]; then
  echo "Usage: Update-ArkhamHorror.sh [published-archive-sha256]" >&2
  exit 2
fi
if [ "$#" -eq 1 ]; then
  EXPECTED_ARCHIVE_SHA256="$1"
else
  printf 'Paste the published SHA-256 for this release archive: ' >&2
  IFS= read -r EXPECTED_ARCHIVE_SHA256 || exit 1
fi
case "$EXPECTED_ARCHIVE_SHA256" in
  *[!0-9a-f]*|"") echo "Update launcher: SHA-256 must be 64 lowercase hexadecimal characters" >&2; exit 1 ;;
esac
[ "${#EXPECTED_ARCHIVE_SHA256}" = 64 ] \
    || { echo "Update launcher: SHA-256 must be 64 lowercase hexadecimal characters" >&2; exit 1; }
WORK_PARENT="${BASE_DIR}/.update-work"
[ ! -L "$WORK_PARENT" ] || { echo "Update launcher: unsafe work parent" >&2; exit 1; }
mkdir -p "$WORK_PARENT"
[ -d "$WORK_PARENT" ] && [ ! -L "$WORK_PARENT" ] \
    || { echo "Update launcher: unsafe work parent" >&2; exit 1; }
TOKEN="$(LC_ALL=C od -An -N32 -tx1 /dev/urandom | tr -d ' \n')"
case "$TOKEN" in *[!0-9a-f]*|"") echo "Update launcher: invalid work token" >&2; exit 1 ;; esac
[ "${#TOKEN}" = 64 ] || { echo "Update launcher: invalid work token" >&2; exit 1; }
WORK_DIR="${WORK_PARENT}/update-${TOKEN}"
[ ! -e "$WORK_DIR" ] && [ ! -L "$WORK_DIR" ] \
    || { echo "Update launcher: refusing to reuse work directory" >&2; exit 1; }
(umask 077 && mkdir "$WORK_DIR") || { echo "Update launcher: could not create work directory" >&2; exit 1; }
printf '%s\n' "$TOKEN" > "${WORK_DIR}/owner"
chmod 600 "${WORK_DIR}/owner"
cleanup() {
  if [ -d "$WORK_DIR" ] && [ ! -L "$WORK_DIR" ] && [ -f "${WORK_DIR}/owner" ] \
      && [ ! -L "${WORK_DIR}/owner" ] && [ "$(cat "${WORK_DIR}/owner")" = "$TOKEN" ]; then
    rm -rf -- "$WORK_DIR"
  elif [ -e "$WORK_DIR" ] || [ -L "$WORK_DIR" ]; then
    echo "Update launcher: refusing unsafe work-directory cleanup" >&2
  fi
}
trap cleanup EXIT
cp "$UPDATE_SOURCE" "${WORK_DIR}/update.sh"
bash "${WORK_DIR}/update.sh" "$BASE_DIR" "$WORK_DIR" "$EXPECTED_ARCHIVE_SHA256"
UPDATELAUNCHER
    chmod +x "${PKG_DIR}/Update-ArkhamHorror.sh"
    info "  ✓ Update-ArkhamHorror.sh generated"
}

# ═════════════════════════════════════════════════════════════════════════════
# Generate Update-ArkhamHorror.bat (Windows/WSL launcher for update)
# ═════════════════════════════════════════════════════════════════════════════

generate_update_bat() {
    cat > "${PKG_DIR}/Update-ArkhamHorror.bat" << 'BATSCRIPT'
@echo off
chcp 65001 >nul 2>&1
setlocal EnableDelayedExpansion

title Arkham Horror LCG - Update

echo.
echo   ============================================
echo     Arkham Horror LCG - Update
echo   ============================================
echo.

REM ---- 1. Check WSL ----
where wsl >nul 2>&1
if %ERRORLEVEL% neq 0 (
    echo [^!] WSL is not installed. Please install WSL first by running Start-ArkhamHorror.bat.
    pause
    exit /b 1
)

REM ---- 2. Probe Ubuntu distributions ----
set "WSL_DISTRO="

wsl -d Ubuntu -- echo ok >nul 2>&1
if !ERRORLEVEL! equ 0 (set "WSL_DISTRO=Ubuntu"& goto :FOUND_DISTRO)

wsl -d Ubuntu-24.04 -- echo ok >nul 2>&1
if !ERRORLEVEL! equ 0 (set "WSL_DISTRO=Ubuntu-24.04"& goto :FOUND_DISTRO)

wsl -d Ubuntu-22.04 -- echo ok >nul 2>&1
if !ERRORLEVEL! equ 0 (set "WSL_DISTRO=Ubuntu-22.04"& goto :FOUND_DISTRO)

wsl -d Ubuntu-20.04 -- echo ok >nul 2>&1
if !ERRORLEVEL! equ 0 (set "WSL_DISTRO=Ubuntu-20.04"& goto :FOUND_DISTRO)

echo [^!] No Ubuntu distribution found. Please run Start-ArkhamHorror.bat first.
pause
exit /b 1

:FOUND_DISTRO
echo [*] Using distribution: !WSL_DISTRO!
echo.

REM ---- 3. Get WSL path ----
set "WIN_DIR=%~dp0"
if "!WIN_DIR:~-1!"=="\" set "WIN_DIR=!WIN_DIR:~0,-1!"

for /f "tokens=*" %%i in ('wsl -d !WSL_DISTRO! wslpath -u "!WIN_DIR!" 2^>nul') do set "WSL_DIR=%%i"
if "!WSL_DIR!"=="" (
    echo [^!] WSL path conversion failed.
    pause
    exit /b 1
)

echo [*] Package path: !WIN_DIR!
echo [*] WSL path:    !WSL_DIR!
echo.

REM ---- 4. Invoke the trusted top-level launcher (it creates an owned work dir) ----
echo [*] Starting update ...
echo.
wsl -d !WSL_DISTRO! -u arkham -- bash -c "cd '!WSL_DIR!' && bash Update-ArkhamHorror.sh"
set UPDATE_EXIT=!ERRORLEVEL!

if !UPDATE_EXIT! neq 0 (
    echo.
    echo [^!] Update failed (exit code: !UPDATE_EXIT!^). Please check the error messages above.
    echo.
)

pause
BATSCRIPT

    # CRLF line endings for Windows
    if command -v unix2dos >/dev/null 2>&1; then
        unix2dos "${PKG_DIR}/Update-ArkhamHorror.bat" 2>/dev/null || true
    elif command -v sed >/dev/null 2>&1; then
        sed -i.bak 's/$/\r/' "${PKG_DIR}/Update-ArkhamHorror.bat" 2>/dev/null && rm -f "${PKG_DIR}/Update-ArkhamHorror.bat.bak" || true
    fi

    info "  ✓ Update-ArkhamHorror.bat generated"
}

# ═════════════════════════════════════════════════════════════════════════════
# Generate Update-ArkhamHorror.command (macOS launcher for update)
# ═════════════════════════════════════════════════════════════════════════════

generate_update_command() {
    cat > "${PKG_DIR}/Update-ArkhamHorror.command" << 'MACCOMMAND'
#!/bin/bash
# The top-level update launcher creates an invocation-owned location before
# executing game/update.sh, which may rename game/.
BASE_DIR="$(cd "$(dirname "$0")" && pwd)"
bash "${BASE_DIR}/Update-ArkhamHorror.sh"
ret=$?
if [ $ret -ne 0 ]; then
    echo ""
    echo "Update failed. Please check the error messages above."
fi
echo ""
echo "Press Enter to close..."
read -r
exit $ret
MACCOMMAND
    chmod +x "${PKG_DIR}/Update-ArkhamHorror.command"
    info "  ✓ Update-ArkhamHorror.command generated"
}

# ═════════════════════════════════════════════════════════════════════════════

main() {
    step "Packaging distribution: ${PKG_NAME}"
    echo ""

    [ -f "$BACKEND_BIN" ]   || die "Backend binary does not exist: $BACKEND_BIN"
    [ -d "$FRONTEND_SRC" ]  || die "Frontend artifacts do not exist: $FRONTEND_SRC"
    [ -d "$PG_BIN_DIR" ]    || die "PostgreSQL does not exist: $PG_BIN_DIR"
    [ -f "$NGINX_BIN" ]     || die "Nginx does not exist: $NGINX_BIN"
    verify_authority_tree_from_receipt frontend "$FRONTEND_SRC" \
        || die "Frontend output does not match this invocation's complete authority receipt"
    verify_node_installation
    export PATH="${DEPS_DIR}/node/bin:${PATH}"
    verify_postgres_installation
    verify_nginx_dependency_for_packaging

    # Never execute a generated script from the previous output tree. A stale
    # package is untrusted input; only PID files whose live executable resolves
    # exactly to that prior package are eligible for termination.
    if [ -d "$PKG_DIR" ] && [ ! -L "$PKG_DIR" ]; then
        substep "Old distribution detected; stopping verified prior package processes ..."
        stop_previous_package_services "$PKG_DIR"
    fi

    # Remove old distribution directory
    # Problem: Start-ArkhamHorror.bat runs PostgreSQL initdb as the arkham user (UID 1001),
    # so the created pgdata directory is 0700 and owned by arkham; the current dev user may not be allowed to delete it.
    # Solution: try normal rm first, then fall back to sudo rm if needed.
    if [ -d "$PKG_DIR" ]; then
        if ! rm -rf "$PKG_DIR" 2>/dev/null; then
            warn "Normal deletion failed (data/pgdata may have been created by another user); sudo is required ..."
            sudo rm -rf "$PKG_DIR" \
                || die "Unable to delete old distribution directory: $PKG_DIR\n  Manual fix: sudo rm -rf $PKG_DIR"
        fi
    fi
    ensure_dir "${PKG_DIR}/game/bin"
    ensure_dir "${PKG_DIR}/game/pgsql/bin"
    ensure_dir "${PKG_DIR}/game/pgsql/lib"
    ensure_dir "${PKG_DIR}/game/frontend/dist"
    ensure_dir "${PKG_DIR}/game/config"
    ensure_dir "${PKG_DIR}/game/data"
    ensure_dir "${PKG_DIR}/backup"

    # Re-check every untrusted build input immediately before copying it. No
    # old package script has run above, and these receipt checks do not expose
    # their capability path/token to a child process.
    verify_authority_tree_from_receipt frontend "$FRONTEND_SRC" \
        || die "Frontend output changed after initial authority verification"
    verify_node_installation
    verify_postgres_installation
    verify_nginx_dependency_for_packaging
    local frontend_source_closure
    frontend_source_closure="$(authority_tree_digest "$FRONTEND_SRC")" \
        || die "Could not calculate the authenticated frontend source closure"

    # Copy backend
    substep "Copy: ${BACKEND_BIN} → ${PKG_DIR}/game/bin/arkham-api"
    cp "$BACKEND_BIN" "${PKG_DIR}/game/bin/arkham-api"
    chmod +x "${PKG_DIR}/game/bin/arkham-api"

    # Copy Nginx
    substep "Copy: ${NGINX_BIN} → ${PKG_DIR}/game/bin/nginx"
    cp "$NGINX_BIN" "${PKG_DIR}/game/bin/nginx"
    chmod +x "${PKG_DIR}/game/bin/nginx"

    # Copy frontend
    substep "Copy: ${FRONTEND_SRC}/ → ${PKG_DIR}/game/frontend/dist/"
    cp -r "${FRONTEND_SRC}/"* "${PKG_DIR}/game/frontend/dist/"

    # The copy above reads a tree this script does not own, and `--skip-frontend`
    # can hand it one nobody verified. The packaged catalog is therefore verified
    # *here*, in its final destination, and republished from the verified buffers
    # — so the bytes nginx serves out of the package are the bytes that passed.
    substep "Verifying the packaged locale catalog"
    if ! (cd "${PROJECT_ROOT}/frontend" \
            && node scripts/locale-catalog/verify-dist.mjs \
                 --dist "${PKG_DIR}/game/frontend/dist" --dist-only --publish); then
        die "  ✗ The packaged frontend does not contain a valid locale catalog"
    fi
    local frontend_package_closure
    frontend_package_closure="$(authority_tree_digest "${PKG_DIR}/game/frontend/dist")" \
        || die "Could not calculate the final packaged frontend closure"
    [ "$frontend_package_closure" = "$frontend_source_closure" ] \
        || die "The final packaged frontend tree differs from its authenticated source tree"

    # Copy PostgreSQL
    substep "Copy PostgreSQL binaries ..."
    for b in postgres initdb pg_ctl pg_isready psql pg_dump pg_restore; do
        [ -x "${PG_BIN_DIR}/${b}" ] && cp "${PG_BIN_DIR}/${b}" "${PKG_DIR}/game/pgsql/bin/"
    done
    [ -d "$PG_LIB_DIR" ] && cp -r "${PG_LIB_DIR}/"* "${PKG_DIR}/game/pgsql/lib/" 2>/dev/null || true
    local pg_share="${DEPS_DIR}/postgres/share"
    [ -d "$pg_share" ] && { ensure_dir "${PKG_DIR}/game/pgsql/share"; cp -r "${pg_share}/"* "${PKG_DIR}/game/pgsql/share/" 2>/dev/null || true; }

    # setup.sql is a full production database dump (tables + columns + constraints + indexes + triggers)
    # Docker initializes the database directly from this file instead of composing Sqitch migrations
    if [ -f "$SETUP_SQL" ]; then
        cp "$SETUP_SQL" "${PKG_DIR}/game/data/setup.sql"
        substep "setup.sql copied to game/data/ (full production database dump)"
    else
        warn "setup.sql not found"
    fi

    # Config files
    local config_src="${PROJECT_ROOT}/backend/arkham-api/config"
    for f in settings.yml client_session_key.aes favicon.ico robots.txt routes; do
        [ -f "${config_src}/$f" ] && cp "${config_src}/$f" "${PKG_DIR}/game/config/" 2>/dev/null || true
    done
    printf '%s\n' "$PLATFORM" > "${PKG_DIR}/game/config/release-platform"

    # Generate nginx/mime runtime files
    generate_mime_types
    generate_launch_script

    # Generate user-facing launch shortcuts (per target platform)
    if [ "$OS" = "linux" ]; then
        generate_start_bat
    elif [ "$OS" = "macos" ]; then
        generate_macos_command
    fi

    # Generate update scripts
    generate_update_script
    generate_update_launcher
    if [ "$OS" = "linux" ]; then
        generate_update_bat
    elif [ "$OS" = "macos" ]; then
        generate_update_command
    fi

    # No version marker written at build time.
    # The first update.sh run will create game/current_v<VERSION> automatically.

    # ── Collect dynamic library dependencies (self-contained distribution; target environment needs no dev packages) ─
    if [ "$OS" = "macos" ]; then
        substep "Bundling macOS dynamic library dependencies ..."
        ensure_dir "${PKG_DIR}/game/lib"

        # Use otool -L to scan non-system dylib dependencies of arkham-api and nginx
        # System libraries under /usr/lib/ and /System/Library/ are not bundled
        # Homebrew libraries (/opt/homebrew/ or /usr/local/) must be bundled
        local bundled=0
        for bin in "${PKG_DIR}/game/bin/arkham-api" "${PKG_DIR}/game/bin/nginx"; do
            while IFS= read -r line; do
                local lib_path
                lib_path="$(echo "$line" | sed 's/^[[:space:]]*//;s/ (compatibility.*//;s/ (.*//')"
                # Skip system libraries
                case "$lib_path" in
                    /usr/lib/*|/System/Library/*) continue ;;
                    @rpath/*|@executable_path/*)  continue ;;  # already using relative paths
                esac
                local lib_name="$(basename "$lib_path")"
                [ -f "${PKG_DIR}/game/lib/${lib_name}" ] && continue  # already bundled
                if [ -f "$lib_path" ]; then
                    cp "$lib_path" "${PKG_DIR}/game/lib/"
                    # Homebrew source dylibs may be read-only (0444); chmod is required before signing
                    chmod u+w "${PKG_DIR}/game/lib/${lib_name}"
                    # Strip the original Homebrew signature (it will be invalid on another machine and can block replacement with a fresh ad-hoc signature)
                    codesign --remove-signature "${PKG_DIR}/game/lib/${lib_name}" || warn "Failed to remove signature: ${lib_name}"
                    # A copied dylib can carry its Homebrew install ID even
                    # after nginx itself is rewritten to @rpath. Normalize
                    # that ID too, otherwise a recursive closure check sees a
                    # host-only dependency and dyld may resolve outside the
                    # package.
                    install_name_tool -id "@rpath/${lib_name}" "${PKG_DIR}/game/lib/${lib_name}" \
                        || die "install_name_tool could not rewrite bundled library ID: ${lib_name}"
                    # Rewrite absolute library references to @rpath (paired with -add_rpath)
                    install_name_tool -change "$lib_path" "@rpath/${lib_name}" "$bin" \
                        || die "install_name_tool could not rewrite ${lib_name} in $(basename "$bin")"
                    bundled=$((bundled + 1))
                    substep "  + ${lib_name} ← ${lib_path}"
                fi
            done < <(otool -L "$bin" 2>/dev/null | tail -n +2)
        done

        # Fix rpath so binaries prefer dylibs from the bundled lib/ directory
        for bin in "${PKG_DIR}/game/bin/arkham-api" "${PKG_DIR}/game/bin/nginx"; do
            install_name_tool -add_rpath "@executable_path/../lib" "$bin" 2>/dev/null || true
        done

        info "  ✓ Bundled ${bundled} macOS dynamic libraries → ${PKG_DIR}/game/lib/"
    elif [ "$OS" = "linux" ]; then
        substep "Collecting dynamic library dependencies ..."
        ensure_dir "${PKG_DIR}/game/lib"

        # Collect direct + transitive dependencies for all binaries and already bundled .so files
        # Exclude glibc core libraries (libc/libm/libdl/libpthread/librt/ld-linux/linux-vdso)
        #   because they are present on any Linux distribution and must match the target system kernel
        # Exclude libraries already present under pgsql/lib/ (for example libpq.so.5 from our own build)
        local bins_to_scan=(
            "${PKG_DIR}/game/bin/arkham-api"
            "${PKG_DIR}/game/bin/nginx"
        )
        # Also scan .so files under pgsql/lib (for example uuid-ossp.so depends on libuuid.so.1)
        while IFS= read -r extra_so; do
            bins_to_scan+=("$extra_so")
        done < <(find "${PKG_DIR}/game/pgsql/lib" -name '*.so' -type f 2>/dev/null)

        # First scan the NEEDED entries of binaries (direct dependencies)
        local needed_libs=()
        for bin_file in "${bins_to_scan[@]}"; do
            while IFS= read -r lib_name; do
                needed_libs+=("$lib_name")
            done < <(readelf -d "$bin_file" 2>/dev/null | grep NEEDED | sed 's/.*\[\(.*\)\]/\1/')
        done

        # For each NEEDED library, find the system path and copy it (excluding glibc core libs and already bundled ones)
        local copied_count=0
        for lib_name in "${needed_libs[@]}"; do
            # Skip glibc core libraries
            if echo "$lib_name" | grep -qE "^(libc\.so|libm\.so|libdl\.so|libpthread\.so|librt\.so)"; then
                continue
            fi
            # Skip libraries already under pgsql/lib (our self-built libpq must not be overridden by the system version)
            if [ -f "${PKG_DIR}/game/pgsql/lib/${lib_name}" ]; then
                continue
            fi
            # Skip libraries already copied
            if [ -f "${PKG_DIR}/game/lib/${lib_name}" ]; then
                continue
            fi
            # Find the system path
            local lib_path
            lib_path="$(ldconfig -p 2>/dev/null | grep -F "$lib_name" | head -1 | sed 's/.*=> //')"
            if [ -z "$lib_path" ] || [ ! -f "$lib_path" ]; then
                # Also try searching standard paths directly
                lib_path="$(find /lib /usr/lib -name "$lib_name" 2>/dev/null | head -1)"
            fi
            if [ -n "$lib_path" ] && [ -f "$lib_path" ]; then
                cp "$lib_path" "${PKG_DIR}/game/lib/"
                copied_count=$((copied_count + 1))
                info "    Bundled: ${lib_name} ← ${lib_path}"

                # Recurse into this library's own NEEDED entries (transitive dependencies)
                while IFS= read -r transitive_lib; do
                    if echo "$transitive_lib" | grep -qE "^(libc\.so|libm\.so|libdl\.so|libpthread\.so|librt\.so)"; then
                        continue
                    fi
                    if [ -f "${PKG_DIR}/game/lib/${transitive_lib}" ] || [ -f "${PKG_DIR}/game/pgsql/lib/${transitive_lib}" ]; then
                        continue
                    fi
                    local t_path
                    t_path="$(ldconfig -p 2>/dev/null | grep -F "$transitive_lib" | head -1 | sed 's/.*=> //')"
                    if [ -z "$t_path" ] || [ ! -f "$t_path" ]; then
                        t_path="$(find /lib /usr/lib -name "$transitive_lib" 2>/dev/null | head -1)"
                    fi
                    if [ -n "$t_path" ] && [ -f "$t_path" ]; then
                        cp "$t_path" "${PKG_DIR}/game/lib/"
                        copied_count=$((copied_count + 1))
                        info "    Bundled: ${transitive_lib} ← ${t_path} (transitive dependency)"
                    fi
                done < <(readelf -d "$lib_path" 2>/dev/null | grep NEEDED | sed 's/.*\[\(.*\)\]/\1/')
            else
                warn "    Not found: ${lib_name} (must be installed on the target system)"
            fi
        done

        info "  Bundled ${copied_count} dynamic libraries into game/lib/"
    fi

    # ── macOS Gatekeeper: ad-hoc signing after library collection ─────────────
    # Homebrew dylibs carry original signatures that must be stripped before
    # relocation. They must then be signed again: an unsigned relocated dylib
    # is rejected by dyld when the packaged nginx executes.
    if [ "$OS" = "macos" ]; then
        # Step 1: ensure dylibs under lib/ are writable first (Homebrew sources may be read-only)
        if [ -d "${PKG_DIR}/game/lib" ]; then
            chmod -R u+w "${PKG_DIR}/game/lib/" 2>/dev/null || true
            # Fully strip the original Homebrew signatures (run twice to be safe)
            find "${PKG_DIR}/game/lib" -name '*.dylib' -exec codesign --remove-signature {} \; >/dev/null 2>&1 || true
            find "${PKG_DIR}/game/lib" -name '*.dylib' -exec codesign --remove-signature {} \; >/dev/null 2>&1 || true
            substep "Original signatures stripped from bundled dylibs"
        fi

        # Step 2: ad-hoc sign all binaries (including lib/ dylibs, which will be stripped again in the next step)
        substep "Ad-hoc signing all binaries ..."
        local signed=0
        while IFS= read -r -d '' f; do
            if codesign --force --sign - "$f" >/dev/null 2>&1; then signed=$((signed + 1))
            else warn "Signing failed: $f"
            fi
        done < <(find "${PKG_DIR}" -type f \( -perm -a=x -o -name '*.so' -o -name '*.dylib' \) -print0 2>/dev/null)
        info "  ✓ Signed ${signed} files"

        # Step 3: leave every relocated dylib with a valid ad-hoc signature.
        # Re-signing is required after install_name_tool changed nginx to load
        # the bundled @rpath dependency.
        find "${PKG_DIR}/game/lib" -name '*.dylib' -exec codesign --force --sign - {} \; >/dev/null 2>&1 \
            || die "Could not sign a bundled macOS dynamic library"
        while IFS= read -r -d '' dylib; do
            codesign --verify --strict "$dylib" >/dev/null 2>&1 \
                || die "A bundled macOS dynamic library has an invalid signature: ${dylib}"
        done < <(find "${PKG_DIR}/game/lib" -name '*.dylib' -print0)
        touch "${PKG_DIR}/game/data/signed.marker"

        # Step 4: clear all quarantine attributes
        xattr -rd com.apple.quarantine "${PKG_DIR}" || true
    fi

    # Dynamic-library relocation and macOS signing can modify the nginx
    # executable, so its package SHA-256 provenance is written only after
    # every post-copy transformation has finished.
    write_nginx_provenance

    # Verification
    echo ""
    substep "Verifying distribution integrity ..."
    local required=("game/bin/arkham-api" "game/bin/nginx" "game/pgsql/bin/postgres" "game/pgsql/bin/initdb" "game/pgsql/bin/pg_ctl" "game/pgsql/bin/pg_dump" "game/pgsql/bin/pg_restore" "game/frontend/dist/index.html" "game/start.sh" "game/update.sh" "Update-ArkhamHorror.sh")
    if [ "$OS" = "linux" ]; then
        required+=("Start-ArkhamHorror.bat" "Update-ArkhamHorror.bat")
    elif [ "$OS" = "macos" ]; then
        required+=("Start-ArkhamHorror.command" "Update-ArkhamHorror.command")
    fi
    local ok=true
    for f in "${required[@]}"; do
        [ -e "${PKG_DIR}/${f}" ] || { warn "  Missing: $f"; ok=false; }
    done
    [ "$ok" = true ] && info "  ✓ All core files are present" || die "  ✗ Distribution is incomplete"

    # Create empty card image directories (users can drop custom card images here)
    ensure_dir "${PKG_DIR}/cards"
    ensure_dir "${PKG_DIR}/cards_en"

    echo ""
    local sz; sz="$(du -sh "$PKG_DIR" | cut -f1)"
    info "  ✓ Distribution ready: ${PKG_DIR} ($sz)"
    echo ""
    info "Usage: cd ${PKG_NAME}/game && bash start.sh"
}

main "$@"
