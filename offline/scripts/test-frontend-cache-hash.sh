#!/usr/bin/env bash
# =============================================================================
# test-frontend-cache-hash.sh - the offline build's cache key really covers the
# locale catalog's provenance inputs.
#
# The offline packager reuses `offline/_deps/frontend` whenever a stored hash
# matches, so a class of input missing from that hash means a stale catalog
# ships. This runs the production `compute_frontend_hash` from
# 03-build-frontend.sh against a throwaway copy of the repository and mutates
# one input class at a time; every mutation must move the hash, and an
# unreadable input must fail rather than quietly hash to nothing.
# =============================================================================

set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
REPO_ROOT="$(cd "${SCRIPT_DIR}/../.." && pwd)"

WORK="$(mktemp -d "${TMPDIR:-/tmp}/offline-cache-hash.XXXXXX")"
trap 'rm -rf "$WORK"' EXIT

failures=0
fail() { echo "offline-cache-hash: $*" >&2; failures=$((failures + 1)); }

# A minimal tree with exactly the paths the hash reads.
PROJECT_ROOT="${WORK}/repo"
FRONTEND_DIR="${PROJECT_ROOT}/frontend"
mkdir -p \
  "${FRONTEND_DIR}/src/locales/en" \
  "${FRONTEND_DIR}/homebrew/pack/locales/en" \
  "${FRONTEND_DIR}/scripts/locale-catalog" \
  "${FRONTEND_DIR}/schemas/locale-catalog/v1" \
  "${PROJECT_ROOT}/contracts/fixtures" \
  "${PROJECT_ROOT}/backend/arkham-api"

echo '{"alpha":"synthetic"}' > "${FRONTEND_DIR}/src/locales/en/base.json"
echo '{"engines":{"node":"1.2.3"}}' > "${FRONTEND_DIR}/package.json"
echo '{"lockfileVersion":3}' > "${FRONTEND_DIR}/package-lock.json"
echo '<!doctype html>' > "${FRONTEND_DIR}/index.html"
echo 'export default {}' > "${FRONTEND_DIR}/vite.config.js"
echo '{"beta":"synthetic"}' > "${FRONTEND_DIR}/homebrew/pack/locales/en/base.json"
echo '{"icon":"x"}' > "${FRONTEND_DIR}/homebrew/pack/icons.json"
echo '// generator' > "${FRONTEND_DIR}/scripts/locale-catalog/generate.mjs"
echo '{"$id":"chunk"}' > "${FRONTEND_DIR}/schemas/locale-catalog/v1/chunk.schema.json"
echo '{"tag":"fixture"}' > "${PROJECT_ROOT}/contracts/fixtures/question-read.json"
echo '{"revision":"0.1.22"}' > "${PROJECT_ROOT}/contracts/manifest.json"
echo '{"keys":[]}' > "${PROJECT_ROOT}/backend/arkham-api/i18n-emitted-keys.json"

# Pull in only the hashing half of the production script: everything above the
# first `verify_locale_catalog` definition, minus its `source utils.sh`
# bootstrap, which would need the whole offline environment.
sed -n '1,/^# Fails the build unless the locale catalog really is/p' \
  "${REPO_ROOT}/offline/scripts/03-build-frontend.sh" \
  | grep -v -e '^source ' -e '^init_paths' -e '^activate_deps_path' \
  | sed -e 's/^FRONTEND_DIR=.*/:/' -e 's/^FRONTEND_OUTPUT=.*/:/' -e 's/^FRONTEND_BUILT_MARKER=.*/:/' \
  > "${WORK}/hash.sh"

has_cmd() { command -v "$1" >/dev/null 2>&1; }
export -f has_cmd
# shellcheck disable=SC1090
source "${WORK}/hash.sh"

baseline="$(compute_frontend_hash)"
if [ -z "$baseline" ]; then
  fail "the baseline hash is empty; the hash function produced nothing"
fi

expect_change() {
  local label="$1" path="$2" content="$3" after
  cp "$path" "${path}.orig"
  printf '%s' "$content" > "$path"
  after="$(compute_frontend_hash)"
  mv "${path}.orig" "$path"
  if [ "$after" = "$baseline" ]; then
    fail "changing ${label} did not change the cache key"
  fi
}

expect_change "a locale source" "${FRONTEND_DIR}/src/locales/en/base.json" '{"alpha":"changed"}'
expect_change "a homebrew locale" "${FRONTEND_DIR}/homebrew/pack/locales/en/base.json" '{"beta":"changed"}'
expect_change "a homebrew icon map" "${FRONTEND_DIR}/homebrew/pack/icons.json" '{"icon":"y"}'
expect_change "the generator" "${FRONTEND_DIR}/scripts/locale-catalog/generate.mjs" '// changed'
expect_change "a catalog schema" "${FRONTEND_DIR}/schemas/locale-catalog/v1/chunk.schema.json" '{"$id":"other"}'
expect_change "a contract fixture" "${PROJECT_ROOT}/contracts/fixtures/question-read.json" '{"tag":"other"}'
expect_change "the contract manifest" "${PROJECT_ROOT}/contracts/manifest.json" '{"revision":"0.1.23"}'
expect_change "the backend key registry" "${PROJECT_ROOT}/backend/arkham-api/i18n-emitted-keys.json" '{"keys":["k"]}'
expect_change "the lockfile" "${FRONTEND_DIR}/package-lock.json" '{"lockfileVersion":4}'
expect_change "package.json" "${FRONTEND_DIR}/package.json" '{"engines":{"node":"1.2.4"}}'

# A new file in a hashed tree must count too.
echo '{"gamma":"added"}' > "${FRONTEND_DIR}/src/locales/en/extra.json"
if [ "$(compute_frontend_hash)" = "$baseline" ]; then
  fail "adding a locale file did not change the cache key"
fi
rm "${FRONTEND_DIR}/src/locales/en/extra.json"

# An unreadable input must fail loudly rather than hash to nothing.
mv "${FRONTEND_DIR}/index.html" "${FRONTEND_DIR}/index.html.away"
if compute_frontend_hash >/dev/null 2>&1; then
  fail "a missing hash input was silently ignored"
fi
mv "${FRONTEND_DIR}/index.html.away" "${FRONTEND_DIR}/index.html"

# An empty hashed directory is a broken checkout, not an empty hash.
mv "${FRONTEND_DIR}/schemas" "${FRONTEND_DIR}/schemas.away"
if compute_frontend_hash >/dev/null 2>&1; then
  fail "a missing hash input directory was silently ignored"
fi
mv "${FRONTEND_DIR}/schemas.away" "${FRONTEND_DIR}/schemas"

if [ "$(compute_frontend_hash)" != "$baseline" ]; then
  fail "the hash is not stable for unchanged inputs"
fi

if [ "$failures" -ne 0 ]; then
  echo "offline-cache-hash: ${failures} failure(s)" >&2
  exit 1
fi
echo "offline-cache-hash: cache key covers every locale-catalog provenance input"
