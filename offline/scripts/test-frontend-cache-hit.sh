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
# Captured before anything sourced below can reassign SCRIPT_DIR.
PROD_SCRIPTS="$SCRIPT_DIR"
FRONTEND_DIR="${REPO_ROOT}/frontend"

WORK="$(mktemp -d "${TMPDIR:-/tmp}/offline-cache-hit.XXXXXX")"
PUBLIC_CATALOG="${FRONTEND_DIR}/public/locale-catalog"
STASHED="${WORK}/public-locale-catalog"
restore() {
  # A schema mutation is exercised against the real schema file, so it is put
  # back before anything else — including on a failure path.
  if [ -f "${WORK}/schema-backup/chunk.schema.json" ]; then
    cp "${WORK}/schema-backup/chunk.schema.json" \
      "${FRONTEND_DIR}/schemas/locale-catalog/v1/chunk.schema.json"
  fi
  if [ -f "${WORK}/schema-backup/manifest.schema.json" ]; then
    cp "${WORK}/schema-backup/manifest.schema.json" \
      "${FRONTEND_DIR}/schemas/locale-catalog/v1/manifest.schema.json"
  fi
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
sed -n '1,/^# ── Build frontend/p' "${PROD_SCRIPTS}/03-build-frontend.sh" \
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

# Rewrites one chunk and keeps every number that describes it consistent with
# the rewrite - digest, content path, byte count, key counts, per-locale and
# global totals, and both compressed siblings. A rejection therefore proves the
# rule under test rather than an arithmetic side effect, which the no-op
# control below confirms.
mutate_chunk() {
  local copy="$1" script="$2"
  node -e '
    const fs = require("node:fs")
    const path = require("node:path")
    const { createHash } = require("node:crypto")
    const zlib = require("node:zlib")
    const root = process.argv[1]
    const mutate = new Function("chunk", process.argv[2])
    const manifestPath = path.join(root, "manifest.json")
    const manifest = JSON.parse(fs.readFileSync(manifestPath, "utf8"))
    let target = null
    for (const locale of manifest.locales) {
      for (const chunk of locale.chunks) {
        const relative = chunk.path.slice(manifest.basePath.length + 1)
        const file = path.join(root, relative)
        if (!fs.existsSync(`${file}.gz`) || !fs.existsSync(`${file}.br`)) continue
        if (target === null || chunk.bytes < target.chunk.bytes) target = { locale, chunk, file }
      }
    }
    if (target === null) throw new Error("no compressed chunk to mutate")
    const parsed = JSON.parse(fs.readFileSync(target.file, "utf8"))
    mutate(parsed)
    const bytes = Buffer.from(JSON.stringify(parsed), "utf8")
    const sha256 = createHash("sha256").update(bytes).digest("hex")
    for (const suffix of ["", ".gz", ".br"]) fs.rmSync(`${target.file}${suffix}`, { force: true })
    const next = path.join(root, "c", `${sha256}.json`)
    fs.writeFileSync(next, bytes)
    fs.writeFileSync(`${next}.gz`, zlib.gzipSync(bytes, { level: 9 }))
    fs.writeFileSync(`${next}.br`, zlib.brotliCompressSync(bytes, {
      params: { [zlib.constants.BROTLI_PARAM_QUALITY]: 11 },
    }))
    const keys = Object.keys(parsed.entries)
    const unsupported = keys.filter((key) => parsed.entries[key].form === "unsupported").length
    const descriptor = target.chunk
    target.locale.keys += keys.length - descriptor.keys
    target.locale.bytes += bytes.length - descriptor.bytes
    manifest.totals.keys += keys.length - descriptor.keys
    manifest.totals.bytes += bytes.length - descriptor.bytes
    manifest.totals.unsupportedKeys += unsupported - descriptor.unsupportedKeys
    descriptor.path = `${manifest.basePath}/c/${sha256}.json`
    descriptor.sha256 = sha256
    descriptor.bytes = bytes.length
    descriptor.keys = keys.length
    descriptor.unsupportedKeys = unsupported
    const serialized = JSON.stringify(manifest)
    fs.writeFileSync(manifestPath, serialized)
    const revision = path.join(root, "r", manifest.catalogRevision, "manifest.json")
    if (fs.existsSync(revision)) fs.writeFileSync(revision, serialized)
  ' "${copy}/locale-catalog" "$script"
}

prepare_chunk_mutation() {
  local name="$1" script="$2"
  local copy="${WORK}/${name}"
  rm -rf "$copy"
  mkdir -p "$copy"
  cp -R "${DIST}/." "$copy/"
  mutate_chunk "$copy" "$script"
  recompress_manifests "$copy"
  printf '%s' "$copy"
}

mutate_chunk_and_expect_reject() {
  local label="$1" name="$2" script="$3" reason="${4:-}" copy
  copy="$(prepare_chunk_mutation "$name" "$script")"
  if verify_cached_locale_catalog "$copy" > "${WORK}/${name}.log" 2>&1; then
    fail "${label} was accepted from a restored cache"
    return
  fi
  if [ -n "$reason" ] && ! grep -q -- "$reason" "${WORK}/${name}.log"; then
    fail "${label} was rejected, but not for ${reason}: $(tr '\n' ' ' < "${WORK}/${name}.log")"
  fi
}

# Rewrites the manifest *and* the chunk bodies together, keeping every digest,
# content path and total consistent with the rewrite. A locale's fallback is
# recorded in both places, so a manifest-only mutation is caught by the
# descriptor/chunk agreement rule long before the resolution graph is examined;
# this is what makes a graph that is internally consistent but unresolvable
# reachable as a test.
rewrite_catalog_and_expect_reject() {
  local label="$1" name="$2" script="$3"
  local copy="${WORK}/${name}"
  rm -rf "$copy"
  mkdir -p "$copy"
  cp -R "${DIST}/." "$copy/"
  node -e '
    const fs = require("node:fs")
    const path = require("node:path")
    const { createHash } = require("node:crypto")
    const zlib = require("node:zlib")
    const root = process.argv[1]
    const manifestPath = path.join(root, "manifest.json")
    const manifest = JSON.parse(fs.readFileSync(manifestPath, "utf8"))
    const dirty = new Set()
    const bodies = new Map()
    const chunkOf = (descriptor) => {
      const relative = descriptor.path.slice(manifest.basePath.length + 1)
      if (!bodies.has(descriptor)) {
        bodies.set(descriptor, JSON.parse(fs.readFileSync(path.join(root, relative), "utf8")))
      }
      dirty.add(descriptor)
      return bodies.get(descriptor)
    }
    new Function("manifest", "chunkOf", process.argv[2])(manifest, chunkOf)
    for (const locale of manifest.locales) {
      for (const descriptor of locale.chunks) {
        if (!dirty.has(descriptor)) continue
        const previous = path.join(root, descriptor.path.slice(manifest.basePath.length + 1))
        const bytes = Buffer.from(JSON.stringify(bodies.get(descriptor)), "utf8")
        const sha256 = createHash("sha256").update(bytes).digest("hex")
        const hadSiblings = fs.existsSync(`${previous}.gz`)
        for (const suffix of ["", ".gz", ".br"]) fs.rmSync(`${previous}${suffix}`, { force: true })
        const next = path.join(root, "c", `${sha256}.json`)
        fs.writeFileSync(next, bytes)
        if (hadSiblings) {
          fs.writeFileSync(`${next}.gz`, zlib.gzipSync(bytes, { level: 9 }))
          fs.writeFileSync(`${next}.br`, zlib.brotliCompressSync(bytes, {
            params: { [zlib.constants.BROTLI_PARAM_QUALITY]: 11 },
          }))
        }
        const keys = Object.keys(bodies.get(descriptor).entries)
        descriptor.path = `${manifest.basePath}/c/${sha256}.json`
        descriptor.sha256 = sha256
        descriptor.bytes = bytes.length
        descriptor.keys = keys.length
        descriptor.unsupportedKeys = keys.filter(
          (key) => bodies.get(descriptor).entries[key].form === "unsupported",
        ).length
      }
    }
    // Totals are recomputed so the mutation cannot be rejected as arithmetic.
    const sum = (chunks, field) => chunks.reduce((total, chunk) => total + chunk[field], 0)
    manifest.totals = {
      locales: manifest.locales.length,
      chunks: manifest.locales.reduce((total, l) => total + l.chunks.length, 0),
      bytes: 0,
      keys: 0,
      unsupportedKeys: 0,
    }
    for (const locale of manifest.locales) {
      locale.keys = sum(locale.chunks, "keys")
      locale.bytes = sum(locale.chunks, "bytes")
      manifest.totals.keys += locale.keys
      manifest.totals.bytes += locale.bytes
      manifest.totals.unsupportedKeys += sum(locale.chunks, "unsupportedKeys")
    }
    const serialized = JSON.stringify(manifest)
    fs.writeFileSync(manifestPath, serialized)
    const revision = path.join(root, "r", manifest.catalogRevision, "manifest.json")
    if (fs.existsSync(revision)) fs.writeFileSync(revision, serialized)
  ' "${copy}/locale-catalog" "$script"
  recompress_manifests "$copy"
  if verify_cached_locale_catalog "$copy" > "${WORK}/${name}.log" 2>&1; then
    fail "${label} was accepted from a restored cache"
  fi
}

pick_compressed() {
  local files=("${DIST}/locale-catalog"/c/*.json.gz)
  local first="${files[0]}"
  printf '%s' "${first#"${DIST}/locale-catalog/"}"
}
SAMPLE_GZ="$(pick_compressed)"
SAMPLE_JSON="${SAMPLE_GZ%.gz}"

if ! verify_cached_locale_catalog "$DIST" >/dev/null 2>&1; then
  fail "a cached build output was rejected even though its manifest matches"
fi
if [ ! -f "${DIST}/locale-catalog/manifest.json" ]; then
  fail "verification deleted a usable cached build output"
fi
# What is served must be what was verified, so a second pass over the published
# snapshot must pass too.
if ! verify_cached_locale_catalog "$DIST" >/dev/null 2>&1; then
  fail "the published snapshot does not verify"
fi

# A leaf replaced, or an intermediate directory swapped, *after* it was verified
# must be refused rather than published.
swap_during_verification() {
  local label="$1" name="$2" hook="$3"
  local copy="${WORK}/${name}"
  rm -rf "$copy"
  mkdir -p "$copy"
  cp -R "${DIST}/." "$copy/"
  if LOCALE_CATALOG_VERIFY_HOOK="$hook" verify_cached_locale_catalog "$copy" >/dev/null 2>&1; then
    fail "${label} was accepted"
  fi
}

swap_during_verification "a leaf replaced after it was verified" swap-leaf \
  "printf tampered > \"\$DIST_CATALOG/${SAMPLE_JSON}\""
swap_during_verification "a directory swapped after it was verified" swap-dir \
  'cd "$DIST_CATALOG" && mv c c-real && ln -s c-real c'
swap_during_verification "a directory swapped and restored during verification" swap-restore \
  "cd \"\$DIST_CATALOG\" && mv c c-real && ln -s c-real c && rm c && mv c-real c && printf tampered > '${SAMPLE_JSON}'"

# Self-attested totals must be recomputed from the descriptors.
mutate_manifest_and_expect_reject "a repeated descriptor with unchanged totals" repeat-descriptor \
  'manifest.locales[0].chunks.push(Object.assign({}, manifest.locales[0].chunks[0], {pack: "repeat"}))'
mutate_manifest_and_expect_reject "a duplicated locale/pack" duplicate-pack \
  'manifest.locales[0].chunks.push(Object.assign({}, manifest.locales[0].chunks[0]))'
mutate_manifest_and_expect_reject "totals that contradict the descriptors" bad-totals \
  'manifest.totals.bytes = manifest.totals.bytes + 1'
mutate_manifest_and_expect_reject "a locale whose totals contradict its chunks" bad-locale-totals \
  'manifest.locales[0].keys = manifest.locales[0].keys + 1'
mutate_manifest_and_expect_reject "a self-attested oversized catalog" oversized-claim \
  'var one = manifest.locales[0].chunks[0]; manifest.locales[0].chunks = Array.from({length: 4094}, function (_, i) { return Object.assign({}, one, {pack: "p" + i, bytes: 8 * 1024 * 1024}) })'

# A locale record is a whole catalog slice: one record per locale, one default
# that ends the fallback chain, and every fallback or language-resolution target
# published here. Split a locale in two and each record looks complete while
# neither is.
SECOND_LOCALE='manifest.locales.filter(function (l) { return l.locale !== manifest.defaultLocale })[0]'
mutate_manifest_and_expect_reject "one locale split across two records" split-locale \
  'var sum = function (chunks, field) { return chunks.reduce(function (a, c) { return a + c[field] }, 0) };
   var l = manifest.locales.filter(function (x) { return x.chunks.length > 1 })[0];
   var moved = l.chunks.splice(1);
   l.keys = sum(l.chunks, "keys"); l.bytes = sum(l.chunks, "bytes");
   manifest.locales.push({locale: l.locale, fallback: l.fallback, chunks: moved,
     keys: sum(moved, "keys"), bytes: sum(moved, "bytes")});
   manifest.totals.locales = manifest.locales.length'
mutate_manifest_and_expect_reject "an unpublished default locale" default-missing \
  'manifest.defaultLocale = "zz"'
# The fallback a client walks is recorded in the record *and* in every chunk, so
# these rewrite both and stay internally consistent: what is left is a
# resolution graph that cannot be walked.
RETARGET='var set = function (l, to) { l.fallback = to; l.chunks.forEach(function (c) { chunkOf(c).fallback = to }) };
   var others = manifest.locales.filter(function (l) { return l.locale !== manifest.defaultLocale });
   var self = manifest.locales.filter(function (l) { return l.locale === manifest.defaultLocale })[0];'
rewrite_catalog_and_expect_reject "a default locale that falls back" default-falls-back \
  "$RETARGET"' set(self, others[0].locale)'
rewrite_catalog_and_expect_reject "a second locale that ends the chain" second-terminal \
  "$RETARGET"' set(others[0], null)'
rewrite_catalog_and_expect_reject "a fallback to an unpublished locale" fallback-missing \
  "$RETARGET"' set(others[0], "zz")'
rewrite_catalog_and_expect_reject "a locale that falls back to itself" fallback-self \
  "$RETARGET"' set(others[0], others[0].locale)'
rewrite_catalog_and_expect_reject "a fallback cycle that never reaches the default" fallback-cycle \
  "$RETARGET"' set(others[0], others[1].locale); set(others[1], others[0].locale)'
mutate_manifest_and_expect_reject "a language tag resolving to an unpublished locale" tag-missing \
  'manifest.languageResolution[0].locale = "zz"'
mutate_manifest_and_expect_reject "a language tag resolving two ways" tag-ambiguous \
  'manifest.languageResolution.push({tag: manifest.languageResolution[0].tag,
     locale: '"$SECOND_LOCALE"'.locale === manifest.languageResolution[0].locale
       ? manifest.defaultLocale : '"$SECOND_LOCALE"'.locale})'

# Entry keys are closed by the chunk schema's `propertyNames`, which an
# evaluator that skips the keyword would ignore. The no-op control proves the
# rewrite itself is accepted, so the rejections below are the key grammar.
CONTROL="$(prepare_chunk_mutation chunk-control '')"
if ! verify_cached_locale_catalog "$CONTROL" >/dev/null 2>&1; then
  fail "a consistently rewritten chunk was rejected"
fi
mutate_chunk_and_expect_reject "an entry key outside the published grammar" entry-key \
  'chunk.entries["bad/key"] = chunk.entries[Object.keys(chunk.entries)[0]]' 'property name'
mutate_chunk_and_expect_reject "an entry key with a control character" entry-key-control \
  'chunk.entries["bad\u0001key"] = chunk.entries[Object.keys(chunk.entries)[0]]' 'property name'
mutate_chunk_and_expect_reject "an entry key over the published length" entry-key-long \
  'chunk.entries["k".repeat(513)] = chunk.entries[Object.keys(chunk.entries)[0]]' 'property name'

# A schema assertion this verifier does not implement must stop it. Ignoring an
# unknown keyword validates against a weaker contract than the published one.
mkdir -p "${WORK}/schema-backup"
SCHEMA_V1="${FRONTEND_DIR}/schemas/locale-catalog/v1"
for schema in chunk manifest; do
  cp "${SCHEMA_V1}/${schema}.schema.json" "${WORK}/schema-backup/${schema}.schema.json"
  node -e '
    const fs = require("node:fs")
    const schema = JSON.parse(fs.readFileSync(process.argv[1], "utf8"))
    schema.properties[process.argv[2]].minProperties = 1
    fs.writeFileSync(process.argv[1], JSON.stringify(schema, null, 2) + "\n")
  ' "${SCHEMA_V1}/${schema}.schema.json" \
    "$([ "$schema" = chunk ] && echo entries || echo totals)"
  if verify_cached_locale_catalog "$DIST" > "${WORK}/schema-${schema}.log" 2>&1; then
    fail "an unimplemented keyword in the ${schema} schema was ignored"
  elif ! grep -q 'does not evaluate' "${WORK}/schema-${schema}.log"; then
    fail "the ${schema} schema was rejected, but not as an unimplemented keyword"
  fi
  cp "${WORK}/schema-backup/${schema}.schema.json" "${SCHEMA_V1}/${schema}.schema.json"
  rm -f "${WORK}/schema-backup/${schema}.schema.json"
done
if ! verify_cached_locale_catalog "$DIST" >/dev/null 2>&1; then
  fail "restoring the schemas did not restore verification"
fi

# An unlisted flood of sparse files must be refused by the bounded iterator,
# without reading or sorting them.
FLOOD="${WORK}/flood"
rm -rf "$FLOOD"
mkdir -p "$FLOOD"
cp -R "${DIST}/." "$FLOOD/"
python3 -c '
import os, sys
target = sys.argv[1]
for index in range(13000):
    with open(os.path.join(target, "flood-%d.bin" % index), "wb") as handle:
        handle.truncate(1024 * 1024 * 1024)
' "$FLOOD/locale-catalog/c"
if verify_cached_locale_catalog "$FLOOD" >/dev/null 2>&1; then
  fail "a catalog flooded with unlisted artifacts was accepted"
fi
rm -rf "$FLOOD"


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

# Every path that ends up serving bytes must publish the verified snapshot, not
# merely check an intermediate tree. These assert the wiring in the production
# seams; the behaviour itself is proven by the swap tests above.
assert_publishes() {
  local label="$1" file="$2" pattern="$3"
  if ! grep -qE -- "$pattern" "$file"; then
    fail "${label} does not publish the verified snapshot (${file})"
  fi
}

assert_publishes "the offline frontend build" \
  "${PROD_SCRIPTS}/03-build-frontend.sh" \
  'verify-dist\.mjs --dist "\$output" --publish'
assert_publishes "the offline cache-hit path" \
  "${PROD_SCRIPTS}/03-build-frontend.sh" \
  'verify-dist\.mjs --dist "\$output" --dist-only --publish'
assert_publishes "the offline packager" \
  "${PROD_SCRIPTS}/05-package.sh" \
  'verify-dist\.mjs \\$'
assert_publishes "the offline packager's final tree" \
  "${PROD_SCRIPTS}/05-package.sh" \
  'PKG_DIR.*game/frontend/dist" --dist-only --publish'
assert_publishes "the Docker frontend stage" \
  "${REPO_ROOT}/Dockerfile" \
  'verify-dist\.mjs --publish'

# `--skip-frontend` skips the build, so the packager — not the build — is what
# stands between an unverified `_deps/frontend` and the package.
if ! grep -q 'Verifying the packaged locale catalog' "${PROD_SCRIPTS}/05-package.sh"; then
  fail "the packager does not verify the tree it just copied, so --skip-frontend can package an unverified catalog"
fi

# The packaged destination is a plain directory copy, so publishing into it must
# work exactly as it does for a restored cache.
PACKAGED="${WORK}/packaged"
rm -rf "$PACKAGED"
mkdir -p "${PACKAGED}/game/frontend/dist"
cp -R "${DIST}/." "${PACKAGED}/game/frontend/dist/"
if ! (cd "${REPO_ROOT}/frontend" && node scripts/locale-catalog/verify-dist.mjs \
        --dist "${PACKAGED}/game/frontend/dist" --dist-only --publish >/dev/null 2>&1); then
  fail "the packaged tree could not be verified and republished"
fi
if (cd "${REPO_ROOT}/frontend" && LOCALE_CATALOG_VERIFY_HOOK="printf tampered > \"\$DIST_CATALOG/${SAMPLE_JSON}\"" \
      node scripts/locale-catalog/verify-dist.mjs \
        --dist "${PACKAGED}/game/frontend/dist" --dist-only --publish >/dev/null 2>&1); then
  fail "a leaf replaced during packaging verification was accepted"
fi

if [ "$failures" -ne 0 ]; then
  echo "offline-cache-hit: ${failures} failure(s)" >&2
  exit 1
fi
echo "offline-cache-hit: cache-hit verification is self-contained and falls back to a rebuild"
