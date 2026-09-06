#!/usr/bin/env bash
# shellcheck disable=SC1091,SC2016,SC2329
# Proves package replacement never executes a stale generated launcher and
# signals only a process whose executable belongs to that prior package.

set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
REPO_ROOT="$(cd "${SCRIPT_DIR}/../.." && pwd)"
WORK="${REPO_ROOT}/offline/_tmp/test-package-lifecycle-$$-${RANDOM}"
umask 077
mkdir -p "$WORK/old/game/bin" "$WORK/old/game/data"
trap 'rm -rf "$WORK"' EXIT

source "${SCRIPT_DIR}/utils.sh"
source "${SCRIPT_DIR}/package-lifecycle.sh"

failures=0
fail() {
    printf 'package-lifecycle: %s\n' "$*" >&2
    failures=$((failures + 1))
}

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

alive=1
signals="${WORK}/signals"
package_process_executable() {
    printf '%s\n' "${WORK}/old/game/bin/nginx"
}
kill() {
    if [ "$1" = "-0" ]; then
        [ "$alive" = "1" ]
        return
    fi
    printf '%s\n' "$*" >> "$signals"
    [ "$1" = "-TERM" ] && alive=0
    return 0
}
stop_previous_package_services "${WORK}/old"
[ ! -e "$HOSTILE_MARKER" ] || fail "hostile old start.sh was executed"
[ "$(cat "$RECEIPT_INPUT")" = "original receipt bytes" ] \
    || fail "hostile old start.sh received a chance to mutate build authority"
grep -Fx -- '-TERM 42' "$signals" >/dev/null || fail "verified prior nginx process was not terminated"

alive=1
: > "$signals"
package_process_executable() {
    printf '%s\n' '/usr/bin/unrelated'
}
stop_previous_package_services "${WORK}/old"
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
