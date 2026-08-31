#!/usr/bin/env bash
# =============================================================================
# test-frontend-cache-hit.sh - the offline build's cache-hit branches behave on
# a fresh checkout.
#
# A CI cache restores `offline/_deps` and nothing else: `frontend/public/` is
# build output and is not in the cache. The cache-hit path must therefore be
# able to verify the restored build against its own manifest, must not delete a
# usable output, and must fall back to a rebuild — not `die` — when the restored
# output is unusable. This drives the real script's `verify_cached_locale_catalog`
# with `frontend/public/locale-catalog` absent.
# =============================================================================

set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
REPO_ROOT="$(cd "${SCRIPT_DIR}/../.." && pwd)"
FRONTEND_DIR="${REPO_ROOT}/frontend"

WORK="$(mktemp -d "${TMPDIR:-/tmp}/offline-cache-hit.XXXXXX")"
PUBLIC_CATALOG="${FRONTEND_DIR}/public/locale-catalog"
STASHED="${WORK}/public-locale-catalog"
restore() {
  if [ -d "$STASHED" ] && [ ! -d "$PUBLIC_CATALOG" ]; then
    mv "$STASHED" "$PUBLIC_CATALOG"
  fi
  rm -rf "$WORK"
}
trap restore EXIT

failures=0
fail() { echo "offline-cache-hit: $*" >&2; failures=$((failures + 1)); }

# A build output exactly as a cache would restore it.
DIST="${WORK}/frontend"
mkdir -p "$DIST"
if [ ! -d "${FRONTEND_DIR}/dist/locale-catalog" ]; then
  echo "offline-cache-hit: frontend/dist is missing; run 'npm run build' first" >&2
  exit 1
fi
cp -R "${FRONTEND_DIR}/dist/." "$DIST/"

# Simulate the fresh checkout: build output is cached, `public/` is not.
if [ -d "$PUBLIC_CATALOG" ]; then
  mv "$PUBLIC_CATALOG" "$STASHED"
fi

# Load the production verification helpers without running the build.
sed -n '1,/^# ── Build frontend/p' "${SCRIPT_DIR}/03-build-frontend.sh" \
  | grep -v -e '^source ' -e '^init_paths' -e '^activate_deps_path' \
  | sed -e 's/^FRONTEND_DIR=.*/:/' -e 's/^FRONTEND_OUTPUT=.*/:/' -e 's/^FRONTEND_BUILT_MARKER=.*/:/' \
  > "${WORK}/verify.sh"
FRONTEND_DIR="${FRONTEND_DIR}"
FRONTEND_BUILT_MARKER="${WORK}/stamp_frontend_built"
: > "$FRONTEND_BUILT_MARKER"
has_cmd() { command -v "$1" >/dev/null 2>&1; }
substep() { :; }
info() { :; }
step() { :; }
die() { echo "die: $*" >&2; return 1; }
# shellcheck disable=SC1090
source "${WORK}/verify.sh"

if ! verify_cached_locale_catalog "$DIST" >/dev/null 2>&1; then
  fail "a cached build output was rejected even though its manifest matches"
fi
if [ ! -f "${DIST}/locale-catalog/manifest.json" ]; then
  fail "verification deleted a usable cached build output"
fi

# An unusable cache must be reported as a miss, not accepted and not fatal.
CORRUPT="${WORK}/corrupt"
mkdir -p "$CORRUPT"
cp -R "${DIST}/." "$CORRUPT/"
printf '{"schemaVersion":"1.0.0"}' > "${CORRUPT}/locale-catalog/manifest.json"
if verify_cached_locale_catalog "$CORRUPT" >/dev/null 2>&1; then
  fail "a corrupt cached catalog was accepted"
fi

MISSING="${WORK}/missing"
mkdir -p "$MISSING"
cp -R "${DIST}/." "$MISSING/"
rm -rf "${MISSING}/locale-catalog"
if verify_cached_locale_catalog "$MISSING" >/dev/null 2>&1; then
  fail "a cached output with no catalog at all was accepted"
fi

# A cache can restore a compressed sibling that no longer matches the JSON next
# to it; nginx serves those bytes without ever reading the identity file.
mutate_and_expect_reject() {
  local label="$1"; shift
  local copy="${WORK}/$1"; shift
  rm -rf "$copy"
  mkdir -p "$copy"
  cp -R "${DIST}/." "$copy/"
  ( cd "${copy}/locale-catalog" && "$@" )
  if verify_cached_locale_catalog "$copy" >/dev/null 2>&1; then
    fail "${label} was accepted from a restored cache"
  fi
}

# No pipe here: `ls … | head -1` makes `ls` die of SIGPIPE, which `pipefail`
# then turns into a failing script (exit 141) on Linux.
pick_compressed() {
  local files=("${DIST}/locale-catalog"/c/*.json.gz)
  local first="${files[0]}"
  printf '%s' "${first#"${DIST}/locale-catalog/"}"
}
SAMPLE_GZ="$(pick_compressed)"
SAMPLE_JSON="${SAMPLE_GZ%.gz}"

mutate_and_expect_reject "a corrupt .gz sibling" corrupt-gz \
  bash -c "printf 'not gzip' > '${SAMPLE_GZ}'"
mutate_and_expect_reject "a stale .gz sibling" stale-gz \
  bash -c "printf 'x' | gzip -c > '${SAMPLE_GZ}'"
mutate_and_expect_reject "a missing .br sibling" missing-br \
  bash -c "rm -f '${SAMPLE_JSON}.br'"
mutate_and_expect_reject "an unlisted compressed artifact" extra-gz \
  bash -c "printf 'x' | gzip -c > 'c/not-in-the-manifest.json.gz'"
mutate_and_expect_reject "a decompression bomb" bomb-gz \
  bash -c "head -c 8000000 /dev/zero | gzip -c > '${SAMPLE_GZ}'"

# The strict check (used after a real build) must still demand `public/`, and
# it owns the destructive behaviour, so it gets its own throwaway copy.
STRICT="${WORK}/strict"
mkdir -p "$STRICT"
cp -R "${DIST}/." "$STRICT/"
if verify_locale_catalog "$STRICT" >/dev/null 2>&1; then
  fail "the post-build check passed without frontend/public/locale-catalog"
fi

if [ "$failures" -ne 0 ]; then
  echo "offline-cache-hit: ${failures} failure(s)" >&2
  exit 1
fi
echo "offline-cache-hit: cache-hit verification is self-contained and falls back to a rebuild"
