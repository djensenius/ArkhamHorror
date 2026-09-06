#!/usr/bin/env bash
# =============================================================================
# test-frontend-cache-hash.sh - the offline build's cache key really covers the
# locale catalog's provenance inputs.
#
# The offline build always recreates rendered output, but its source/cache
# identity still governs the verified Node/npm dependency path. A class of
# input missing from that hash could let a stale dependency cache influence the
# catalog. This runs the production `compute_frontend_hash` from
# 03-build-frontend.sh against a throwaway copy of the repository and mutates
# one input class at a time; every mutation must move the hash, and an
# unreadable input must fail rather than quietly hash to nothing.
# =============================================================================

set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
REPO_ROOT="$(cd "${SCRIPT_DIR}/../.." && pwd)"

WORK="${REPO_ROOT}/offline/_tmp/test-frontend-cache-hash-$$-${RANDOM}"
umask 077
mkdir -p "$WORK"
trap 'rm -rf "$WORK"' EXIT

failures=0
fail() { echo "offline-cache-hash: $*" >&2; failures=$((failures + 1)); }

# A minimal tree with exactly the paths the hash reads.
PROJECT_ROOT="${WORK}/repo"
FRONTEND_DIR="${PROJECT_ROOT}/frontend"
PLATFORM="macos-arm64"
NODE_NPM_CLI="lib/node_modules/npm/bin/npm-cli.js"
DEPS_DIR="${WORK}/deps"
mkdir -p \
  "${FRONTEND_DIR}/src/locales/en" \
  "${FRONTEND_DIR}/homebrew/pack/locales/en" \
  "${FRONTEND_DIR}/scripts/locale-catalog" \
  "${FRONTEND_DIR}/schemas/locale-catalog/v1" \
  "${PROJECT_ROOT}/contracts/fixtures" \
  "${PROJECT_ROOT}/backend/arkham-api" \
  "${DEPS_DIR}/node/bin"

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
cat > "${DEPS_DIR}/node/bin/node" <<'NODE'
#!/usr/bin/env bash
if [ "${1:-}" = "--version" ]; then
  printf '%s\n' 'v1.2.3'
fi
NODE
chmod +x "${DEPS_DIR}/node/bin/node"

# Pull in only the hashing half of the production script: everything above the
# first `verify_locale_catalog` definition, minus its `source utils.sh`
# bootstrap, which would need the whole offline environment.
sed -n '1,/^# Fails the build unless the locale catalog really is/p' \
  "${REPO_ROOT}/offline/scripts/03-build-frontend.sh" \
  | grep -v -e '^source ' -e '^init_paths' -e '^activate_deps_path' \
  | grep -v -e '^PLATFORM=' -e '^require_toolchain_authority_receipt$' -e '^verify_node_installation$' -e '^export PATH=' \
  | sed -e 's/^FRONTEND_DIR=.*/:/' -e 's/^FRONTEND_OUTPUT=.*/:/' -e 's/^FRONTEND_BUILT_MARKER=.*/:/' \
  > "${WORK}/hash.sh"

has_cmd() { command -v "$1" >/dev/null 2>&1; }
sha256_file() {
  if command -v sha256sum >/dev/null 2>&1; then
    sha256sum "$1" | awk '{print $1}'
  else
    shasum -a 256 "$1" | awk '{print $1}'
  fi
}
TOOLCHAIN_LOCK_TEST_DIGEST="aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa"
NODE_AUTHORITY_TEST="exact	bbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbb	v1.2.3"
toolchain_lock_digest() { printf '%s\n' "$TOOLCHAIN_LOCK_TEST_DIGEST"; }
toolchain_binary_authority() { printf '%s\n' "$NODE_AUTHORITY_TEST"; }
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

# A missing or symlinked lockfile must fail before any dependency tree can be
# admitted. `compute_frontend_hash` is the source/cache authority boundary.
mv "${FRONTEND_DIR}/package-lock.json" "${FRONTEND_DIR}/package-lock.json.away"
if compute_frontend_hash >/dev/null 2>&1; then
  fail "a missing package-lock.json was silently accepted"
fi
mv "${FRONTEND_DIR}/package-lock.json.away" "${FRONTEND_DIR}/package-lock.json"
mv "${FRONTEND_DIR}/package-lock.json" "${FRONTEND_DIR}/package-lock.json.real"
ln -s package-lock.json.real "${FRONTEND_DIR}/package-lock.json"
if compute_frontend_hash >/dev/null 2>&1; then
  fail "a symlinked package-lock.json was silently accepted"
fi
rm -f "${FRONTEND_DIR}/package-lock.json"
mv "${FRONTEND_DIR}/package-lock.json.real" "${FRONTEND_DIR}/package-lock.json"

# A replacement Node binary can retain the same --version output. The cache
# key must bind the authority digest itself, not merely the version string.
NODE_AUTHORITY_TEST="exact	cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc	v1.2.3"
if [ "$(compute_frontend_hash)" = "$baseline" ]; then
  fail "changing the same-version Node authority digest did not change the cache key"
fi
NODE_AUTHORITY_TEST="exact	bbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbb	v1.2.3"
TOOLCHAIN_LOCK_TEST_DIGEST="dddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddd"
if [ "$(compute_frontend_hash)" = "$baseline" ]; then
  fail "changing the toolchain lock digest did not change the cache key"
fi
TOOLCHAIN_LOCK_TEST_DIGEST="aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa"

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

# The offline build applies a deterministic helpers.ts transform before Vite
# and the catalog generator run. Its transformed bytes must be part of the
# source hash; otherwise a valid-looking cached catalog can describe a
# different semantic source than the cached frontend bundle.
transform_line="$(grep -n 'Patching helpers.ts:' "${REPO_ROOT}/offline/scripts/03-build-frontend.sh" | head -1 | cut -d: -f1)"
hash_line="$(grep -n 'current_hash="$(compute_frontend_hash)"' "${REPO_ROOT}/offline/scripts/03-build-frontend.sh" | head -1 | cut -d: -f1)"
if [ -z "$transform_line" ] || [ -z "$hash_line" ] || [ "$transform_line" -ge "$hash_line" ]; then
  fail "the deterministic helpers.ts transform is not applied before the frontend cache hash"
fi
grep -Fq 'OFFLINE_PUBLIC_CATALOG_STASH' "${REPO_ROOT}/offline/scripts/03-build-frontend.sh" \
  || fail "the offline build does not restore its temporary public catalog state"
grep -Fq 'run_offline_npm ci --ignore-scripts --prefer-offline' \
  "${REPO_ROOT}/offline/scripts/03-build-frontend.sh" \
  || fail "dependency installation does not invoke the verified npm CLI with scripts disabled"
if grep -Fq 'npm install' "${REPO_ROOT}/offline/scripts/03-build-frontend.sh"; then
  fail "the authoritative frontend build retains an unlocked npm install fallback"
fi

if [ "$failures" -ne 0 ]; then
  echo "offline-cache-hash: ${failures} failure(s)" >&2
  exit 1
fi
echo "offline-cache-hash: cache key covers every locale-catalog provenance input"
