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

# A restored manifest is untrusted input: it names every path that gets read and
# every digest that gets compared, so a cache must not be able to point the
# verifier — or nginx — at anything but a content-addressed chunk inside the
# catalog.
jq_manifest() {
  local copy="$1"; shift
  node -e '
    const fs = require("fs")
    const path = process.argv[1]
    const mutate = new Function("manifest", process.argv[2])
    const manifest = JSON.parse(fs.readFileSync(path, "utf8"))
    mutate(manifest)
    const bytes = JSON.stringify(manifest)
    fs.writeFileSync(path, bytes)
    const revision = path.replace(/manifest\.json$/, "") + "r/" + manifest.catalogRevision + "/manifest.json"
    if (fs.existsSync(revision)) fs.writeFileSync(revision, bytes)
  ' "${copy}/locale-catalog/manifest.json" "$1"
}

# Recompresses the manifests it rewrote, so a rejection proves the intended
# rule and not a stale `.gz`/`.br` sibling.
recompress_manifests() {
  local copy="$1" file
  while IFS= read -r file; do
    [ -f "${file}.gz" ] && gzip -9 -c "$file" > "${file}.gz"
    [ -f "${file}.br" ] && node -e '
      const {brotliCompressSync, constants} = require("node:zlib")
      const fs = require("fs")
      fs.writeFileSync(process.argv[2], brotliCompressSync(fs.readFileSync(process.argv[1]), {
        params: {[constants.BROTLI_PARAM_QUALITY]: 11},
      }))
    ' "$file" "${file}.br"
  done < <(find "${copy}/locale-catalog" -name manifest.json)
}

mutate_manifest_and_expect_reject() {
  local label="$1" name="$2" script="$3"
  local copy="${WORK}/${name}"
  rm -rf "$copy"
  mkdir -p "$copy"
  cp -R "${DIST}/." "$copy/"
  jq_manifest "$copy" "$script"
  recompress_manifests "$copy"
  if verify_cached_locale_catalog "$copy" >/dev/null 2>&1; then
    fail "${label} was accepted from a restored cache"
  fi
}

mutate_manifest_and_expect_reject "a traversing chunk path" traversal \
  'manifest.locales[0].chunks[0].path = manifest.basePath + "/../../../etc/passwd"'
mutate_manifest_and_expect_reject "an encoded traversing chunk path" traversal-encoded \
  'manifest.locales[0].chunks[0].path = manifest.basePath + "/c/%2e%2e/%2e%2e/secret.json"'
mutate_manifest_and_expect_reject "a doubled separator chunk path" traversal-double \
  'manifest.locales[0].chunks[0].path = manifest.basePath + "/c//" + manifest.locales[0].chunks[0].sha256 + ".json"'
mutate_manifest_and_expect_reject "a dot-segment chunk path" traversal-dot \
  'manifest.locales[0].chunks[0].path = manifest.basePath + "/c/./" + manifest.locales[0].chunks[0].sha256 + ".json"'
mutate_manifest_and_expect_reject "an absolute chunk path" absolute \
  'manifest.locales[0].chunks[0].path = "/etc/passwd"'
mutate_manifest_and_expect_reject "a platform chunk path" platform \
  'manifest.locales[0].chunks[0].path = manifest.basePath + "/c\\\\windows\\\\system32"'
mutate_manifest_and_expect_reject "a chunk whose name and digest disagree" digest-path \
  'manifest.locales[0].chunks[0].sha256 = "0".repeat(64)'
mutate_manifest_and_expect_reject "a non content-addressed chunk path" not-addressed \
  'manifest.locales[0].chunks[0].path = manifest.basePath + "/chunks/en-core.json"'
mutate_manifest_and_expect_reject "a schema-invalid manifest" schema-invalid \
  'manifest.digestAlgorithm = "md5"'
mutate_manifest_and_expect_reject "an unknown manifest field" schema-extra \
  'manifest.somethingElse = true'

# The manifest must describe the route it is actually served at.
mutate_manifest_and_expect_reject "a manifest published at another route" route-base \
  'manifest.basePath = "/outside-catalog"; for (const l of manifest.locales) for (const c of l.chunks) c.path = c.path.replace("/locale-catalog", "/outside-catalog"); manifest.manifestPath = "/outside-catalog/manifest.json"; manifest.chunkPathPrefix = "/outside-catalog/c/"; manifest.revisionManifestPath = manifest.revisionManifestPath.replace("/locale-catalog", "/outside-catalog")'
mutate_manifest_and_expect_reject "a manifest that names another manifest URL" route-manifest \
  'manifest.manifestPath = "/locale-catalog/other.json"'
mutate_manifest_and_expect_reject "a revision manifest path its revision does not derive" route-revision \
  'manifest.revisionManifestPath = "/locale-catalog/r/1." + "0".repeat(32) + "/manifest.json"'
mutate_manifest_and_expect_reject "a chunk prefix that is not the served one" route-prefix \
  'manifest.chunkPathPrefix = "/locale-catalog/chunks/"'

# Inherited names must not look like declared properties.
mutate_manifest_and_expect_reject "a constructor property" proto-constructor \
  'manifest.constructor = "x"'
mutate_manifest_and_expect_reject "a toString property" proto-tostring \
  'manifest.toString = "x"'
mutate_manifest_and_expect_reject "a nested prototype-like property" proto-nested \
  'manifest.locales[0].chunks[0].constructor = "x"'
mutate_manifest_and_expect_reject "a literal __proto__ key" proto-literal \
  'Object.defineProperty(manifest, "__proto__", {value: {}, enumerable: true, configurable: true, writable: true})'

# A symlink is a way to serve bytes from outside the route that the digest
# check would otherwise bless, whether it is a chunk, a compressed sibling or a
# whole directory.
symlink_and_expect_reject() {
  local label="$1" name="$2"; shift 2
  local copy="${WORK}/${name}"
  rm -rf "$copy"
  mkdir -p "$copy"
  cp -R "${DIST}/." "$copy/"
  ( cd "${copy}/locale-catalog" && "$@" )
  if verify_cached_locale_catalog "$copy" >/dev/null 2>&1; then
    fail "${label} was accepted from a restored cache"
  fi
}

OUTSIDE="${WORK}/outside"
mkdir -p "$OUTSIDE"
cp "${DIST}/locale-catalog/${SAMPLE_JSON}" "${OUTSIDE}/chunk.json"
cp "${DIST}/locale-catalog/${SAMPLE_GZ}" "${OUTSIDE}/chunk.json.gz"

# Even with byte-identical content, the artifact must live inside the catalog.
symlink_and_expect_reject "a symlinked chunk with matching bytes" symlink-chunk \
  bash -c "rm -f '${SAMPLE_JSON}' && ln -s '${OUTSIDE}/chunk.json' '${SAMPLE_JSON}'"
symlink_and_expect_reject "a symlinked compressed sibling" symlink-gz \
  bash -c "rm -f '${SAMPLE_GZ}' && ln -s '${OUTSIDE}/chunk.json.gz' '${SAMPLE_GZ}'"
symlink_and_expect_reject "a symlinked chunk directory" symlink-dir \
  bash -c "mv c c-real && ln -s c-real c"
symlink_and_expect_reject "a hard-linked chunk" hardlink-chunk \
  bash -c "rm -f '${SAMPLE_JSON}' && ln '${OUTSIDE}/chunk.json' '${SAMPLE_JSON}'"
symlink_and_expect_reject "a hard-linked compressed sibling" hardlink-gz \
  bash -c "rm -f '${SAMPLE_GZ}' && ln '${OUTSIDE}/chunk.json.gz' '${SAMPLE_GZ}'"
symlink_and_expect_reject "a hard-linked manifest" hardlink-manifest \
  bash -c "cp manifest.json '${OUTSIDE}/manifest.json' && rm -f manifest.json && ln '${OUTSIDE}/manifest.json' manifest.json"

# A directory that was validated must still be that directory: `c/` swapped for
# a symlink after validation, and an oversized or growing artifact, must all be
# refused rather than read.
symlink_and_expect_reject "an oversized chunk" oversized \
  bash -c "f=\$(ls c/*.json | head -c 200 | cut -d' ' -f1); : > /dev/null; python3 - <<'PY'
import glob, os
target = sorted(glob.glob('c/*.json'))[0]
with open(target, 'ab') as handle:
    handle.truncate(9 * 1024 * 1024)
PY"
symlink_and_expect_reject "a sparse multi-gigabyte chunk" sparse \
  bash -c "python3 - <<'PY'
import glob
target = sorted(glob.glob('c/*.json'))[0]
with open(target, 'r+b') as handle:
    handle.truncate(4 * 1024 * 1024 * 1024)
PY"
symlink_and_expect_reject "a truncated chunk" truncated \
  bash -c "python3 - <<'PY'
import glob
target = sorted(glob.glob('c/*.json'))[0]
with open(target, 'r+b') as handle:
    handle.truncate(4)
PY"

# The catalog root itself must be a real directory.
ROOTLINK="${WORK}/rootlink"
rm -rf "$ROOTLINK"
mkdir -p "$ROOTLINK"
cp -R "${DIST}/." "$ROOTLINK/"
mv "${ROOTLINK}/locale-catalog" "${ROOTLINK}/locale-catalog-real"
ln -s locale-catalog-real "${ROOTLINK}/locale-catalog"
if verify_cached_locale_catalog "$ROOTLINK" >/dev/null 2>&1; then
  fail "a symlinked catalog root was accepted from a restored cache"
fi

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
