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
printf '%s\n' 'frontend-output-authority: index and bundle substitutions are rejected by an external full-tree receipt'
