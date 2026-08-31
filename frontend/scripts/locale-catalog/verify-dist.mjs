// Deploy-seam check: the catalog the generator produced must actually be in
// the static root the production container serves (`frontend/dist`, mounted at
// `/opt/arkham/src/frontend/dist` by prod.nginxconf), with the digests the
// manifest promises and the precompressed siblings nginx serves.
//
//   npm run build && node scripts/locale-catalog/verify-dist.mjs

import { existsSync, lstatSync, readFileSync, readdirSync, realpathSync } from 'node:fs'
import { brotliDecompressSync, gunzipSync } from 'node:zlib'
import { dirname, join, relative, resolve, sep } from 'node:path'
import { fileURLToPath } from 'node:url'

import { sha256Hex } from './canonical.mjs'

const FRONTEND_DIR = resolve(dirname(fileURLToPath(import.meta.url)), '..', '..')

// `--dist <dir>` lets the offline packager point at its own build output; the
// default is the directory prod.nginxconf serves.
const DIST_ONLY = process.argv.includes('--dist-only')
const distIndex = process.argv.indexOf('--dist')
if (distIndex !== -1 && (process.argv[distIndex + 1] ?? '').trim() === '') {
  console.error('locale-catalog: --dist requires a directory')
  process.exit(1)
}
const DIST_ROOT =
  distIndex === -1 ? join(FRONTEND_DIR, 'dist') : resolve(process.argv[distIndex + 1])
const DIST_CATALOG = join(DIST_ROOT, 'locale-catalog')

// scripts/precompress.cjs skips anything smaller than one MTU's worth of bytes.
const PRECOMPRESS_MIN_BYTES = 1024
// A compressed sibling should never exceed its payload by more than framing,
// and inflating it must not be allowed to outgrow the payload either.
const MAX_COMPRESSED_OVERHEAD = 4096
const MAX_INFLATE_SLACK = 4096
// The only shape a chunk path may take. A manifest is data, and data that
// names a filesystem path decides what nginx serves, so the grammar is closed:
// `<basePath>/c/<64 lowercase hex>.json`, and the digest in the name must be
// the digest the descriptor promises.
const CHUNK_PATH = /^c\/([0-9a-f]{64})\.json$/
const SCHEMA_DIR = resolve(FRONTEND_DIR, 'schemas/locale-catalog/v1')

const problems = []

function require(condition, message) {
  if (!condition) problems.push(message)
  return condition
}

/**
 * The subset of JSON Schema the v1 manifest uses, evaluated against the schema
 * file itself. A cached manifest is untrusted input: everything downstream —
 * which paths are read, which digests are compared — comes out of it, so it is
 * checked against the published contract before any of it is believed.
 */
function validateAgainstSchema(value, schema, root, path = '', errors = []) {
  if (schema.$ref) {
    const target = schema.$ref.replace(/^#\//, '').split('/')
    let resolved = root
    for (const part of target) resolved = resolved?.[part]
    if (resolved === undefined) throw new Error(`unresolvable $ref ${schema.$ref}`)
    return validateAgainstSchema(value, resolved, root, path, errors)
  }
  const at = path === '' ? '<root>' : path
  const type = schema.type
  const typeOf = Array.isArray(value) ? 'array' : value === null ? 'null' : typeof value
  if (type && type !== typeOf && !(type === 'integer' && Number.isInteger(value))) {
    errors.push(`${at} should be ${type}, is ${typeOf}`)
    return errors
  }
  if (schema.const !== undefined && value !== schema.const) {
    errors.push(`${at} should be ${JSON.stringify(schema.const)}`)
  }
  if (schema.enum && !schema.enum.includes(value)) errors.push(`${at} is not one of the enum`)
  if (typeof value === 'string') {
    if (schema.pattern && !new RegExp(schema.pattern, 'u').test(value)) {
      errors.push(`${at} does not match ${schema.pattern}`)
    }
    if (schema.maxLength !== undefined && value.length > schema.maxLength) {
      errors.push(`${at} is longer than ${schema.maxLength}`)
    }
    if (schema.minLength !== undefined && value.length < schema.minLength) {
      errors.push(`${at} is shorter than ${schema.minLength}`)
    }
  }
  if (typeof value === 'number') {
    if (schema.minimum !== undefined && value < schema.minimum) errors.push(`${at} < minimum`)
    if (schema.maximum !== undefined && value > schema.maximum) errors.push(`${at} > maximum`)
  }
  if (Array.isArray(value)) {
    if (schema.minItems !== undefined && value.length < schema.minItems) {
      errors.push(`${at} has fewer than ${schema.minItems} items`)
    }
    if (schema.maxItems !== undefined && value.length > schema.maxItems) {
      errors.push(`${at} has more than ${schema.maxItems} items`)
    }
    if (schema.uniqueItems) {
      const seen = new Set(value.map((item) => JSON.stringify(item)))
      if (seen.size !== value.length) errors.push(`${at} has duplicate items`)
    }
    if (schema.items) {
      value.forEach((item, index) =>
        validateAgainstSchema(item, schema.items, root, `${at}[${index}]`, errors),
      )
    }
  }
  if (value !== null && typeof value === 'object' && !Array.isArray(value)) {
    for (const name of schema.required ?? []) {
      if (!Object.hasOwn(value, name)) errors.push(`${at}.${name} is required`)
    }
    for (const [name, entry] of Object.entries(value)) {
      const property = schema.properties?.[name]
      if (property === undefined) {
        if (schema.additionalProperties === false) errors.push(`${at}.${name} is not allowed`)
        else if (typeof schema.additionalProperties === 'object') {
          validateAgainstSchema(entry, schema.additionalProperties, root, `${at}.${name}`, errors)
        }
        continue
      }
      validateAgainstSchema(entry, property, root, `${at}.${name}`, errors)
    }
  }
  if (schema.oneOf) {
    const matches = schema.oneOf.filter(
      (branch) => validateAgainstSchema(value, branch, root, at, []).length === 0,
    )
    if (matches.length !== 1) errors.push(`${at} matches ${matches.length} oneOf branches`)
  }
  return errors
}

/**
 * A repo-relative path inside the catalog, or null.
 *
 * Everything is decided here: no absolute path, no `..`, no encoded or
 * doubled separator, no dot segment, no backslash, and after canonicalization
 * the result must still sit beneath the catalog. Symlinks are rejected rather
 * than followed — a link is a way to serve bytes from outside the route that
 * the digest check would happily bless.
 */
function safeCatalogPath(relativePath) {
  if (typeof relativePath !== 'string' || relativePath === '') return null
  if (relativePath !== relativePath.normalize('NFC')) return null
  if (/[\\]/.test(relativePath)) return null
  if (/%2e|%2f|%5c/i.test(relativePath)) return null
  if (relativePath.startsWith('/') || /^[A-Za-z]:/.test(relativePath)) return null
  const segments = relativePath.split('/')
  if (segments.some((segment) => segment === '' || segment === '.' || segment === '..')) return null

  const absolute = resolve(DIST_CATALOG, relativePath)
  const within = relative(DIST_CATALOG, absolute)
  if (within === '' || within.startsWith('..') || within.split(sep).join('/') !== relativePath) {
    return null
  }

  // Every component, not just the leaf: a symlinked directory redirects the
  // whole subtree.
  let walked = DIST_CATALOG
  for (const segment of segments) {
    walked = join(walked, segment)
    let status
    try {
      status = lstatSync(walked)
    } catch {
      return null
    }
    if (status.isSymbolicLink()) return null
    if (walked !== absolute && !status.isDirectory()) return null
    if (walked === absolute && !status.isFile()) return null
  }
  try {
    if (realpathSync(absolute) !== realpathSync.native(absolute)) return null
    const real = realpathSync(absolute)
    const inside = relative(realpathSync(DIST_CATALOG), real)
    if (inside.startsWith('..') || inside === '') return null
  } catch {
    return null
  }
  return absolute
}

function listFiles(directory, prefix = '') {
  const out = []
  for (const entry of readdirSync(directory, { withFileTypes: true })) {
    const path = prefix === '' ? entry.name : `${prefix}/${entry.name}`
    if (entry.isDirectory()) out.push(...listFiles(join(directory, entry.name), path))
    else out.push(path)
  }
  return out.sort()
}

function listJson(directory, prefix = '') {
  const out = []
  for (const entry of readdirSync(directory, { withFileTypes: true })) {
    const path = prefix === '' ? entry.name : `${prefix}/${entry.name}`
    if (entry.isDirectory()) out.push(...listJson(join(directory, entry.name), path))
    else if (entry.isFile() && entry.name.endsWith('.json')) out.push(path)
  }
  return out
}

function verifyPublicMirror(expected) {
  // Vite copies public/ verbatim, so the served bytes must equal the bytes the
  // generator wrote during prebuild. `--dist-only` skips that comparison for a
  // build output restored from a cache, where `public/` belongs to a tree that
  // no longer exists; everything else still checks the output against its own
  // manifest, which is what makes a cached artifact usable.
  const publicCatalog = join(FRONTEND_DIR, 'public', 'locale-catalog')
  if (!require(existsSync(publicCatalog), 'public/locale-catalog is missing — prebuild did not run')) {
    return
  }
  const published = listJson(publicCatalog)
  require(
    published.length === expected.size,
    `public/locale-catalog has ${published.length} files, dist expects ${expected.size}`,
  )
  for (const relative of published) {
    const source = readFileSync(join(publicCatalog, relative))
    const served = existsSync(join(DIST_CATALOG, relative))
      ? readFileSync(join(DIST_CATALOG, relative))
      : null
    require(served !== null && served.equals(source), `dist/locale-catalog/${relative} is stale`)
  }
}

/**
 * nginx serves `.br` by an explicit `-f` test and `.gz` through gzip_static, so
 * those bytes are what a client gets — the identity file is never read. A cache
 * can restore a sibling that no longer matches it, so each one is decompressed
 * and compared under a size ceiling, and the identity/gz/br sets are compared
 * against the manifest in both directions.
 */
function verifyCompressedSiblings(expected) {
  const compressed = new Set()
  for (const relative of expected) {
    const file = safeCatalogPath(relative)
    if (file === null) continue
    const identity = readFileSync(file)
    if (identity.length < PRECOMPRESS_MIN_BYTES) continue
    for (const [suffix, inflate] of [
      ['.gz', gunzipSync],
      ['.br', brotliDecompressSync],
    ]) {
      const siblingRelative = `${relative}${suffix}`
      const sibling = safeCatalogPath(siblingRelative)
      if (
        !require(
          sibling !== null,
          `${relative} has no usable precompressed ${suffix} sibling (missing, a symlink, or outside the catalog)`,
        )
      ) {
        continue
      }
      compressed.add(siblingRelative)
      const packed = readFileSync(sibling)
      if (
        !require(
          packed.length <= identity.length + MAX_COMPRESSED_OVERHEAD,
          `${siblingRelative} is larger than the payload it encodes`,
        )
      ) {
        continue
      }
      let inflated = null
      try {
        inflated = inflate(packed, { maxOutputLength: identity.length + MAX_INFLATE_SLACK })
      } catch (error) {
        require(false, `${siblingRelative} does not decompress (${error.code ?? error.message})`)
        continue
      }
      require(
        inflated.equals(identity),
        `${siblingRelative} decompresses to different bytes than ${relative}`,
      )
    }
  }

  // Anything else under the catalog is unaccounted for: nginx would happily
  // serve it for a path the manifest never promised.
  for (const relative of listFiles(DIST_CATALOG)) {
    if (relative.endsWith('.gz') || relative.endsWith('.br')) {
      require(
        compressed.has(relative),
        `dist/locale-catalog/${relative} is a compressed artifact the manifest does not list`,
      )
      continue
    }
    require(
      expected.has(relative),
      `dist/locale-catalog/${relative} is an artifact the manifest does not list`,
    )
    require(
      safeCatalogPath(relative) !== null,
      `dist/locale-catalog/${relative} is a symlink or a non-regular file`,
    )
  }
}

function verifyCatalog() {
  if (!require(existsSync(DIST_CATALOG), `${DIST_CATALOG} is missing — the build did not publish the catalog`)) {
    return
  }
  const manifestSchema = JSON.parse(readFileSync(join(SCHEMA_DIR, 'manifest.schema.json'), 'utf8'))
  const manifestPath = safeCatalogPath('manifest.json')
  if (
    !require(
      manifestPath !== null,
      'dist/locale-catalog/manifest.json is missing, a symlink, or not a regular file',
    )
  ) {
    return
  }

  const manifestBytes = readFileSync(manifestPath)
  let manifest = null
  try {
    manifest = JSON.parse(manifestBytes.toString('utf8'))
  } catch (error) {
    require(false, `dist/locale-catalog/manifest.json is not JSON (${error.message})`)
    return
  }

  // The manifest decides every path read below, so it is checked against the
  // published v1 schema before any of it is believed.
  const manifestErrors = validateAgainstSchema(manifest, manifestSchema, manifestSchema)
  if (
    !require(
      manifestErrors.length === 0,
      `dist/locale-catalog/manifest.json does not satisfy the v1 manifest schema: ${manifestErrors
        .slice(0, 3)
        .join('; ')}`,
    )
  ) {
    return
  }

  const revisionRelative = `r/${manifest.catalogRevision}/manifest.json`
  const revisionManifest = safeCatalogPath(revisionRelative)
  if (
    require(
      revisionManifest !== null,
      'the immutable revision manifest is missing from dist, is a symlink, or is not a regular file',
    )
  ) {
    const revisionBytes = readFileSync(revisionManifest)
    require(revisionBytes.equals(manifestBytes), 'the stable and immutable manifests differ in dist')
    let revisionErrors = ['not JSON']
    try {
      revisionErrors = validateAgainstSchema(
        JSON.parse(revisionBytes.toString('utf8')),
        manifestSchema,
        manifestSchema,
      )
    } catch (error) {
      revisionErrors = [error.message]
    }
    require(
      revisionErrors.length === 0,
      `the immutable revision manifest does not satisfy the v1 manifest schema: ${revisionErrors
        .slice(0, 3)
        .join('; ')}`,
    )
  }

  const expected = new Set(['manifest.json', revisionRelative])
  for (const locale of manifest.locales) {
    for (const chunk of locale.chunks) {
      if (
        !require(
          chunk.path.startsWith(`${manifest.basePath}/`),
          `${chunk.path} is not published under ${manifest.basePath}`,
        )
      ) {
        continue
      }
      const relative = chunk.path.slice(`${manifest.basePath}/`.length)

      // Content addressing is the contract: the name *is* the digest, so a
      // descriptor cannot point at one file and promise another's bytes.
      const named = CHUNK_PATH.exec(relative)
      if (
        !require(named !== null, `${chunk.path} is not a content-addressed chunk path (c/<sha256>.json)`)
      ) {
        continue
      }
      if (!require(named[1] === chunk.sha256, `${chunk.path} names a digest it does not promise`)) {
        continue
      }

      expected.add(relative)
      const file = safeCatalogPath(relative)
      if (
        !require(
          file !== null,
          `${chunk.path} escapes the catalog, or is a symlink or a non-regular file`,
        )
      ) {
        continue
      }
      const bytes = readFileSync(file)
      require(bytes.length === chunk.bytes, `${chunk.path} size mismatch in dist`)
      require(sha256Hex(bytes) === chunk.sha256, `${chunk.path} digest mismatch in dist`)
    }
  }

  if (!DIST_ONLY) verifyPublicMirror(expected)
  verifyCompressedSiblings(expected)

  if (problems.length === 0) {
    console.log(
      `locale-catalog: dist publishes revision ${manifest.catalogRevision} ` +
        `(${manifest.totals.chunks} files, ${(manifest.totals.bytes / 1024 / 1024).toFixed(2)} MB)`,
    )
  }
}

verifyCatalog()

if (problems.length > 0) {
  console.error('locale-catalog: the build output is not servable')
  for (const problem of problems.slice(0, 20)) console.error(`  - ${problem}`)
  process.exitCode = 1
}
