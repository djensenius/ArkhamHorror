#!/usr/bin/env bash
# shellcheck disable=SC1091,SC2016,SC2329
# Proves package replacement never executes a stale generated launcher and
# signals only a process whose executable belongs to that prior package.

set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
REPO_ROOT="$(cd "${SCRIPT_DIR}/../.." && pwd)"
WORK="${REPO_ROOT}/offline/_tmp/test-package-lifecycle-$$-${RANDOM}"
umask 077
mkdir -p \
    "$WORK/old/game/bin" \
    "$WORK/old/game/data/pgdata" \
    "$WORK/old/game/pgsql/bin" \
    "$WORK/home"
CROSS_UID_PID=""
CROSS_UID_WRAPPER_PID=""
cleanup() {
    if [ -n "$CROSS_UID_PID" ] && [ -x /usr/bin/sudo ]; then
        /usr/bin/sudo -n /bin/kill -KILL "$CROSS_UID_PID" 2>/dev/null || true
    fi
    if [ -n "$CROSS_UID_WRAPPER_PID" ]; then
        wait "$CROSS_UID_WRAPPER_PID" 2>/dev/null || true
    fi
    rm -rf "$WORK"
}
trap cleanup EXIT

source "${SCRIPT_DIR}/utils.sh"
source "${SCRIPT_DIR}/package-lifecycle.sh"

failures=0
fail() {
    printf 'package-lifecycle: %s\n' "$*" >&2
    failures=$((failures + 1))
}

if [ "$(detect_os)" = linux ] \
    && [ "$(id -u)" != "0" ] \
    && command -v cc >/dev/null 2>&1 \
    && [ -x /usr/bin/sudo ] \
    && /usr/bin/sudo -n true 2>/dev/null; then
    CROSS_UID_OLD="${WORK}/cross-uid-old"
    mkdir -p "${CROSS_UID_OLD}/game/bin" "${CROSS_UID_OLD}/game/data"
    cat > "${WORK}/cross-uid-process.c" <<'EOF'
#include <signal.h>
#include <stdio.h>
#include <unistd.h>

int main(int argc, char **argv) {
    if (argc != 2) return 2;
    FILE *output = fopen(argv[1], "w");
    if (output == NULL) return 3;
    fprintf(output, "%ld\n", (long)getpid());
    fclose(output);
    for (;;) pause();
}
EOF
    cc -O2 -o "${CROSS_UID_OLD}/game/bin/nginx" "${WORK}/cross-uid-process.c"
    CROSS_UID_PID_FILE="${CROSS_UID_OLD}/game/data/nginx.pid"
    /usr/bin/sudo -n "${CROSS_UID_OLD}/game/bin/nginx" "$CROSS_UID_PID_FILE" &
    CROSS_UID_WRAPPER_PID=$!
    for _ in $(seq 1 50); do
        [ -s "$CROSS_UID_PID_FILE" ] && break
        sleep 0.1
    done
    [ -s "$CROSS_UID_PID_FILE" ] \
        || { fail "cross-UID lifecycle fixture did not start"; exit 1; }
    CROSS_UID_PID="$(cat "$CROSS_UID_PID_FILE")"
    if kill -0 "$CROSS_UID_PID" 2>/dev/null; then
        fail "cross-UID fixture unexpectedly permits unprivileged signalling"
    fi
    HOME="${WORK}/home" stop_previous_package_services "$CROSS_UID_OLD"
    wait "$CROSS_UID_WRAPPER_PID" 2>/dev/null || true
    CROSS_UID_WRAPPER_PID=""
    if package_process_exists "$CROSS_UID_PID"; then
        fail "privileged prior-package process survived verified lifecycle cleanup"
    fi
    CROSS_UID_PID=""
fi

HOSTILE_MARKER="${WORK}/hostile-ran"
RECEIPT_INPUT="${WORK}/authority-receipt.tsv"
printf 'original receipt bytes\n' > "$RECEIPT_INPUT"
cat > "${WORK}/old/game/start.sh" <<EOF
#!/usr/bin/env bash
touch "${HOSTILE_MARKER}"
printf 'mutated receipt bytes\n' > "${RECEIPT_INPUT}"
EOF
chmod +x "${WORK}/old/game/start.sh"
printf '42\n' > "${WORK}/old/game/data/nginx.pid"
printf 'trusted binary placeholder\n' > "${WORK}/old/game/bin/nginx"
cat > "${WORK}/old/game/data/pgdata/postmaster.pid" <<'EOF'
84
/private/old-package/pgdata
1700000000
5433
/tmp
localhost
EOF
printf 'trusted postgres placeholder\n' > "${WORK}/old/game/pgsql/bin/postgres"
case "$(detect_os)" in
    macos) RUNTIME_PGDATA="${WORK}/home/Library/Application Support/ArkhamHorror/pgdata" ;;
    linux) RUNTIME_PGDATA="${WORK}/home/.local/share/ArkhamHorror/pgdata" ;;
esac
mkdir -p "$RUNTIME_PGDATA"
cat > "${RUNTIME_PGDATA}/postmaster.pid" <<EOF
126
${RUNTIME_PGDATA}
1700000000
5433
/tmp
localhost
EOF
SIMULATE_WSL=false
if [ "$(detect_os)" = linux ]; then
    SIMULATE_WSL=true
    ARKHAM_HOME="${WORK}/arkham-home"
    ARKHAM_PGDATA="${ARKHAM_HOME}/.local/share/ArkhamHorror/pgdata"
    mkdir -p "$ARKHAM_PGDATA"
    cat > "${ARKHAM_PGDATA}/postmaster.pid" <<EOF
168
${ARKHAM_PGDATA}
1700000000
5433
/tmp
localhost
EOF
fi

nginx_alive=1
legacy_postgres_alive=1
runtime_postgres_alive=1
wsl_postgres_alive=1
signals="${WORK}/signals"
if [ "$SIMULATE_WSL" = true ]; then
    package_is_wsl() { return 0; }
    package_arkham_home() { printf '%s\n' "$ARKHAM_HOME"; }
else
    package_is_wsl() { return 1; }
fi
package_process_executable() {
    case "$1" in
        42) printf '%s\n' "${WORK}/old/game/bin/nginx" ;;
        84) printf '%s\n' "${WORK}/old/game/pgsql/bin/postgres" ;;
        126) printf '%s\n' "${WORK}/old/game/pgsql/bin/postgres" ;;
        168) printf '%s\n' "${WORK}/old/game/pgsql/bin/postgres" ;;
        *) return 1 ;;
    esac
}
kill() {
    if [ "$1" = "-0" ]; then
        case "$2" in
            42) [ "$nginx_alive" = "1" ] ;;
            84) [ "$legacy_postgres_alive" = "1" ] ;;
            126) [ "$runtime_postgres_alive" = "1" ] ;;
            168) [ "$wsl_postgres_alive" = "1" ] ;;
            *) return 1 ;;
        esac
        return $?
    fi
    printf '%s\n' "$*" >> "$signals"
    if [ "$1" = "-TERM" ]; then
        case "$2" in
            42) nginx_alive=0 ;;
            84|126|168) : ;;
        esac
    elif [ "$1" = "-KILL" ]; then
        case "$2" in
            42) nginx_alive=0 ;;
            84) legacy_postgres_alive=0 ;;
            126) runtime_postgres_alive=0 ;;
            168) wsl_postgres_alive=0 ;;
        esac
    fi
    return 0
}
HOME="${WORK}/home" stop_previous_package_services "${WORK}/old"
[ ! -e "$HOSTILE_MARKER" ] || fail "hostile old start.sh was executed"
[ "$(cat "$RECEIPT_INPUT")" = "original receipt bytes" ] \
    || fail "hostile old start.sh received a chance to mutate build authority"
grep -Fx -- '-TERM 42' "$signals" >/dev/null || fail "verified prior nginx process was not terminated"
grep -Fx -- '-TERM 84' "$signals" >/dev/null \
    || fail "verified prior legacy PostgreSQL process was not terminated from its multi-line postmaster.pid"
grep -Fx -- '-KILL 84' "$signals" >/dev/null \
    || fail "stubborn prior legacy PostgreSQL process was not escalated before package replacement"
grep -Fx -- '-TERM 126' "$signals" >/dev/null \
    || fail "verified prior user-data PostgreSQL process was not terminated from its multi-line postmaster.pid"
grep -Fx -- '-KILL 126' "$signals" >/dev/null \
    || fail "stubborn prior user-data PostgreSQL process was not escalated before package replacement"
if [ "$SIMULATE_WSL" = true ]; then
    grep -Fx -- '-TERM 168' "$signals" >/dev/null \
        || fail "verified prior WSL arkham-user PostgreSQL process was not terminated"
    grep -Fx -- '-KILL 168' "$signals" >/dev/null \
        || fail "stubborn prior WSL arkham-user PostgreSQL process was not escalated"
fi

nginx_alive=1
legacy_postgres_alive=1
runtime_postgres_alive=1
wsl_postgres_alive=1
: > "$signals"
package_process_executable() {
    printf '%s\n' '/usr/bin/unrelated'
}
HOME="${WORK}/home" stop_previous_package_services "${WORK}/old"
[ ! -s "$signals" ] || fail "foreign process named by a stale PID file was signalled"

if grep -Fq 'bash "${PKG_DIR}/game/start.sh" --stop' "${REPO_ROOT}/offline/scripts/05-package.sh"; then
    fail "packager still executes an old generated start.sh"
fi
if grep -Fq 'generate_legacy_update_script_unused' "${REPO_ROOT}/offline/scripts/05-package.sh"; then
    fail "packager retains a callable legacy updater generator"
fi
grep -Fq 'stop_previous_package_services "$PKG_DIR"' "${REPO_ROOT}/offline/scripts/05-package.sh" \
    || fail "packager does not use trusted prior-package cleanup"

if [ "$failures" -ne 0 ]; then
    printf 'package-lifecycle: %s failure(s)\n' "$failures" >&2
    exit 1
fi
printf '%s\n' 'package-lifecycle: hostile old launchers are never executed and foreign PIDs are not signalled'
