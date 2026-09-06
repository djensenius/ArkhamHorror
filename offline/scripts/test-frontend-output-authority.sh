#!/usr/bin/env bash
# Verifies that the full rendered frontend, not only locale-catalog, is bound
# to an invocation-external authority receipt.

set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
REPO_ROOT="$(cd "${SCRIPT_DIR}/../.." && pwd)"
WORK="${REPO_ROOT}/offline/_tmp/test-frontend-output-authority-$$-${RANDOM}"
umask 077
mkdir -p "$WORK"
trap 'rm -rf "$WORK"' EXIT

source "${SCRIPT_DIR}/utils.sh"
init_paths
TMP_DIR="${WORK}/receipt-tmp"
DEPS_DIR="${WORK}/deps"
PLATFORM="macos-arm64"
TOOLCHAIN_RECEIPT_DIR="${WORK}/receipt-tmp"
export GITHUB_ENV=""
init_toolchain_authority_receipt

failures=0
fail() {
    printf 'frontend-output-authority: %s\n' "$*" >&2
    failures=$((failures + 1))
}

expect_reject() {
    local label="$1"
    shift
    if ("$@") >/dev/null 2>&1; then
        fail "${label} was accepted"
    fi
}

DIST="${WORK}/frontend"
mkdir -p "${DIST}/assets" "${DIST}/locale-catalog/c"
printf '<!doctype html><script src="/assets/app.js"></script>\n' > "${DIST}/index.html"
printf 'trusted frontend bundle\n' > "${DIST}/assets/app.js"
printf '{"trusted":"catalog"}\n' > "${DIST}/locale-catalog/c/trusted.json"

identity="$(printf '%s' 'frontend-inputs-v1' | sha256_text)"
closure="$(authority_tree_digest "$DIST")"
record_authority_receipt frontend "$identity" "$closure"
verify_authority_tree frontend "$identity" "$DIST" \
    || fail "a fresh complete frontend tree was rejected"
verify_authority_tree_from_receipt frontend "$DIST" \
    || fail "a fresh frontend tree was rejected by its external receipt"

# These simulate an attacker updating the old cache's self-issued source hash
# or catalog metadata along with a non-catalog asset. The external receipt is
# intentionally not touched, so both substitutions must fail.
printf '<!doctype html><script src="/assets/evil.js"></script>\n' > "${DIST}/index.html"
printf 'self-issued-cache-hash' > "${DIST}/source_hash"
expect_reject "substituted index.html plus self-issued metadata" \
    verify_authority_tree_from_receipt frontend "$DIST"
printf '<!doctype html><script src="/assets/app.js"></script>\n' > "${DIST}/index.html"
rm -f "${DIST}/source_hash"
verify_authority_tree_from_receipt frontend "$DIST" \
    || fail "restoring index.html did not restore the external receipt check"

printf 'substituted frontend bundle\n' > "${DIST}/assets/app.js"
printf '{"rewritten":"catalog-only metadata"}\n' > "${DIST}/locale-catalog/c/trusted.json"
expect_reject "substituted bundle plus catalog metadata" \
    verify_authority_tree_from_receipt frontend "$DIST"
printf 'trusted frontend bundle\n' > "${DIST}/assets/app.js"
printf '{"trusted":"catalog"}\n' > "${DIST}/locale-catalog/c/trusted.json"
verify_authority_tree_from_receipt frontend "$DIST" \
    || fail "restoring frontend bytes did not restore the external receipt check"

# Shipped frontend output is an ordinary-file document root. Every link is
# rejected, whether it would escape, point internally, or be dangling.
ln -s /etc/passwd "${DIST}/assets/escaping-link"
expect_reject "escaping frontend symlink" \
    verify_authority_tree_from_receipt frontend "$DIST"
rm -f "${DIST}/assets/escaping-link"

ln -s app.js "${DIST}/assets/internal-link"
expect_reject "internal frontend symlink" \
    verify_authority_tree_from_receipt frontend "$DIST"
rm -f "${DIST}/assets/internal-link"

ln -s absent.js "${DIST}/assets/broken-link"
expect_reject "broken frontend symlink" \
    verify_authority_tree_from_receipt frontend "$DIST"
rm -f "${DIST}/assets/broken-link"

mkfifo "${DIST}/assets/evil.fifo"
expect_reject "frontend FIFO" \
    verify_authority_tree_from_receipt frontend "$DIST"
rm -f "${DIST}/assets/evil.fifo"

if env -u PYTHONHOME -u PYTHONPATH /usr/bin/python3 - "${DIST}/assets" <<'PY'
import os
import socket
import sys

os.chdir(sys.argv[1])
sock = socket.socket(socket.AF_UNIX)
try:
    sock.bind("evil.socket")
finally:
    sock.close()
PY
then
    if [ -S "${DIST}/assets/evil.socket" ]; then
        expect_reject "frontend socket" \
            verify_authority_tree_from_receipt frontend "$DIST"
        rm -f "${DIST}/assets/evil.socket"
    fi
fi

if command -v mknod >/dev/null 2>&1 \
    && mknod "${DIST}/assets/evil.device" c 1 3 2>/dev/null; then
    expect_reject "frontend device" \
        verify_authority_tree_from_receipt frontend "$DIST"
    rm -f "${DIST}/assets/evil.device"
fi

PACKAGE_DIST="${WORK}/package/game/frontend/dist"
mkdir -p "$PACKAGE_DIST"
cp -R "${DIST}/." "$PACKAGE_DIST/"
verify_authority_tree_from_receipt frontend "$PACKAGE_DIST" \
    || fail "fresh final package frontend tree was rejected"
printf 'substituted final package bundle\n' > "${PACKAGE_DIST}/assets/app.js"
expect_reject "substituted final package frontend asset" \
    verify_authority_tree_from_receipt frontend "$PACKAGE_DIST"

if grep -Fq 'offline/_deps/frontend/' "${REPO_ROOT}/.github/workflows/build-offline.yml"; then
    fail "workflow still restores generated frontend output from cache"
fi
grep -Fq "'offline/toolchain.lock'" "${REPO_ROOT}/.github/workflows/build-offline.yml" \
    || fail "frontend workflow cache key does not bind the Node authority lock"
grep -Fq 'Discarding untrusted persisted frontend output before rebuilding' \
    "${REPO_ROOT}/offline/scripts/03-build-frontend.sh" \
    || fail "offline frontend build still accepts persisted rendered assets"

if [ "$failures" -ne 0 ]; then
    printf 'frontend-output-authority: %s failure(s)\n' "$failures" >&2
    exit 1
fi
printf '%s\n' 'frontend-output-authority: full frontend trees reject substitutions, links, and special files'
