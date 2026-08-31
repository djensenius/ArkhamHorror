// Deploy-seam check: the catalog the generator produced must actually be in
// the static root the production container serves (`frontend/dist`, mounted at
// `/opt/arkham/src/frontend/dist` by prod.nginxconf), with the digests the
// manifest promises and the precompressed siblings nginx serves.
//
//   npm run build && node scripts/locale-catalog/verify-dist.mjs

import {
  closeSync,
  constants as fsConstants,
  existsSync,
  fstatSync,
  lstatSync,
  openSync,
  readFileSync,
  readdirSync,
  readSync,
  realpathSync,
} from 'node:fs'
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
// The supplied root is canonicalized exactly once, so the tree that is
// verified is the tree those bytes really live in — an operator may reach it
// through a symlink (a CI workspace, a macOS `/var` -> `/private/var`), and
// what matters is that nothing *inside* it redirects, and that nothing moves
// after it has been checked.
const SUPPLIED_ROOT =
  distIndex === -1 ? join(FRONTEND_DIR, 'dist') : resolve(process.argv[distIndex + 1])
const DIST_ROOT = existsSync(SUPPLIED_ROOT) ? realpathSync(SUPPLIED_ROOT) : SUPPLIED_ROOT
const DIST_CATALOG = join(DIST_ROOT, 'locale-catalog')

// scripts/precompress.cjs skips anything smaller than one MTU's worth of bytes.
const PRECOMPRESS_MIN_BYTES = 1024
// A compressed sibling should never exceed its payload by more than framing,
// and inflating it must not be allowed to outgrow the payload either.
const MAX_COMPRESSED_OVERHEAD = 4096
const MAX_INFLATE_SLACK = 4096
// The generator's own ceilings, restated here because this side must refuse an
// oversized artifact before it allocates for it.
const MAX_CHUNK_BYTES = 8 * 1024 * 1024
const MAX_MANIFEST_BYTES = 8 * 1024 * 1024
const MAX_CATALOG_BYTES = 192 * 1024 * 1024
const MAX_CATALOG_FILES = 4096
// The only shape a chunk path may take. A manifest is data, and data that
// names a filesystem path decides what nginx serves, so the grammar is closed:
// `<basePath>/c/<64 lowercase hex>.json`, and the digest in the name must be
// the digest the descriptor promises.
const CHUNK_PATH = /^c\/([0-9a-f]{64})\.json$/
const SCHEMA_DIR = resolve(FRONTEND_DIR, 'schemas/locale-catalog/v1')
// The route this catalog is published at. `prod.nginxconf` serves exactly this
// prefix, so a manifest that describes a different one describes something
// nginx does not serve — and something this check would otherwise happily
// verify somewhere else on disk.
const ROUTE = '/locale-catalog'
const ROUTE_MANIFEST = `${ROUTE}/manifest.json`
const ROUTE_CHUNK_PREFIX = `${ROUTE}/c/`

const problems = []

/**
 * Parses JSON into null-prototype objects and refuses a literal `__proto__`
 * key. A manifest is untrusted input, and an inherited name that looks like a
 * declared property is exactly how `additionalProperties: false` gets bypassed.
 */
function parseUntrustedJson(text) {
  return JSON.parse(text, function reviver(key, value) {
    if (key === '__proto__') throw new SyntaxError('__proto__ is not an allowed key')
    if (value !== null && typeof value === 'object' && !Array.isArray(value)) {
      return Object.assign(Object.create(null), value)
    }
    return value
  })
}

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
  if (Object.hasOwn(schema, '$ref')) {
    const target = schema.$ref.replace(/^#\//, '').split('/')
    let resolved = root
    for (const part of target) {
      resolved = resolved !== null && typeof resolved === 'object' && Object.hasOwn(resolved, part)
        ? resolved[part]
        : undefined
    }
    if (resolved === undefined) throw new Error(`unresolvable $ref ${schema.$ref}`)
    return validateAgainstSchema(value, resolved, root, path, errors)
  }
  const at = path === '' ? '<root>' : path
  const type = Object.hasOwn(schema, 'type') ? schema.type : undefined
  const typeOf = Array.isArray(value) ? 'array' : value === null ? 'null' : typeof value
  if (type && type !== typeOf && !(type === 'integer' && Number.isInteger(value))) {
    errors.push(`${at} should be ${type}, is ${typeOf}`)
    return errors
  }
  if (Object.hasOwn(schema, 'const') && value !== schema.const) {
    errors.push(`${at} should be ${JSON.stringify(schema.const)}`)
  }
  if (Object.hasOwn(schema, 'enum') && !schema.enum.includes(value)) {
    errors.push(`${at} is not one of the enum`)
  }
  if (typeof value === 'string') {
    if (Object.hasOwn(schema, 'pattern') && !new RegExp(schema.pattern, 'u').test(value)) {
      errors.push(`${at} does not match ${schema.pattern}`)
    }
    if (Object.hasOwn(schema, 'maxLength') && value.length > schema.maxLength) {
      errors.push(`${at} is longer than ${schema.maxLength}`)
    }
    if (Object.hasOwn(schema, 'minLength') && value.length < schema.minLength) {
      errors.push(`${at} is shorter than ${schema.minLength}`)
    }
  }
  if (typeof value === 'number') {
    if (Object.hasOwn(schema, 'minimum') && value < schema.minimum) errors.push(`${at} < minimum`)
    if (Object.hasOwn(schema, 'maximum') && value > schema.maximum) errors.push(`${at} > maximum`)
  }
  if (Array.isArray(value)) {
    if (Object.hasOwn(schema, 'minItems') && value.length < schema.minItems) {
      errors.push(`${at} has fewer than ${schema.minItems} items`)
    }
    if (Object.hasOwn(schema, 'maxItems') && value.length > schema.maxItems) {
      errors.push(`${at} has more than ${schema.maxItems} items`)
    }
    if (Object.hasOwn(schema, 'uniqueItems') && schema.uniqueItems) {
      const seen = new Set(value.map((item) => JSON.stringify(item)))
      if (seen.size !== value.length) errors.push(`${at} has duplicate items`)
    }
    if (Object.hasOwn(schema, 'items')) {
      value.forEach((item, index) =>
        validateAgainstSchema(item, schema.items, root, `${at}[${index}]`, errors),
      )
    }
  }
  if (value !== null && typeof value === 'object' && !Array.isArray(value)) {
    for (const name of Object.hasOwn(schema, 'required') ? schema.required : []) {
      if (!Object.hasOwn(value, name)) errors.push(`${at}.${name} is required`)
    }
    // `Object.hasOwn` throughout, and never `schema.properties?.[name]`:
    // `__proto__`, `constructor` and `toString` are inherited names that would
    // otherwise look like declared properties and slip past
    // `additionalProperties: false`.
    const properties = Object.hasOwn(schema, 'properties') ? schema.properties : {}
    const additional = Object.hasOwn(schema, 'additionalProperties')
      ? schema.additionalProperties
      : undefined
    for (const name of Object.keys(value)) {
      const entry = value[name]
      if (!Object.hasOwn(properties, name)) {
        if (additional === false) errors.push(`${at}.${name} is not allowed`)
        else if (additional !== undefined && typeof additional === 'object') {
          validateAgainstSchema(entry, additional, root, `${at}.${name}`, errors)
        }
        continue
      }
      validateAgainstSchema(entry, properties[name], root, `${at}.${name}`, errors)
    }
  }
  if (Object.hasOwn(schema, 'not')) {
    if (validateAgainstSchema(value, schema.not, root, at, []).length === 0) {
      errors.push(`${at} matches a forbidden pattern`)
    }
  }
  if (Object.hasOwn(schema, 'oneOf')) {
    const matches = schema.oneOf.filter(
      (branch) => validateAgainstSchema(value, branch, root, at, []).length === 0,
    )
    if (matches.length !== 1) errors.push(`${at} matches ${matches.length} oneOf branches`)
  }
  return errors
}

/**
 * Directory identities along the catalog's own path, captured once and held.
 *
 * Node has no `openat`, so containment is enforced by *pinning* rather than by
 * hoping a path resolves the same way twice: every directory from the supplied
 * `--dist` root down to the catalog, and every directory walked below it, is
 * opened `O_NOFOLLOW|O_DIRECTORY` and kept open with its `(dev, ino)` recorded.
 * Anything that later swaps one of those directories — `c/` replaced by a
 * symlink after it was validated, an ancestor of `--dist` relinked — changes
 * the identity behind a descriptor still held, and `assertPinsIntact()` says so
 * before the run is allowed to succeed.
 */
const pinnedDirectories = []

function pinDirectory(absolute) {
  let descriptor = null
  try {
    descriptor = openSync(
      absolute,
      fsConstants.O_RDONLY | fsConstants.O_DIRECTORY | fsConstants.O_NOFOLLOW,
    )
  } catch (error) {
    return {
      ok: false,
      reason: `${absolute} is not a directory that opens without following links (${error.code ?? error.message})`,
    }
  }
  const held = fstatSync(descriptor)
  let link = null
  try {
    link = lstatSync(absolute)
  } catch (error) {
    closeSync(descriptor)
    return { ok: false, reason: `${absolute} disappeared (${error.code ?? error.message})` }
  }
  if (link.isSymbolicLink() || link.ino !== held.ino || link.dev !== held.dev) {
    closeSync(descriptor)
    return { ok: false, reason: `${absolute} is a symlink or was replaced while being opened` }
  }
  pinnedDirectories.push({ absolute, descriptor, dev: held.dev, ino: held.ino })
  return { ok: true, reason: null }
}

/** Every pinned directory must still be the directory it was pinned to. */
function assertPinsIntact() {
  for (const pin of pinnedDirectories) {
    let link = null
    try {
      link = lstatSync(pin.absolute)
    } catch (error) {
      return `${pin.absolute} disappeared (${error.code ?? error.message})`
    }
    const held = fstatSync(pin.descriptor)
    if (link.isSymbolicLink()) return `${pin.absolute} became a symlink`
    if (link.dev !== pin.dev || link.ino !== pin.ino) return `${pin.absolute} was replaced`
    if (held.dev !== pin.dev || held.ino !== pin.ino) return `${pin.absolute} changed identity`
  }
  return null
}

/**
 * Pins the supplied `--dist` root's whole ancestry and then the catalog: a
 * caller can point this anywhere, and a symlinked ancestor redirects
 * everything below it.
 */
function pinCatalogAncestry() {
  // From the canonical root down. Above it there is nothing left to resolve;
  // below it, every directory is pinned, so a component swapped for a symlink
  // after it was checked is caught by `assertPinsIntact()`.
  const relativeParts = relative(DIST_ROOT, DIST_CATALOG).split(sep).filter(Boolean)
  let walked = DIST_ROOT
  const pinnedRoot = pinDirectory(walked)
  if (!pinnedRoot.ok) return pinnedRoot.reason
  for (const part of relativeParts) {
    walked = join(walked, part)
    const pinned = pinDirectory(walked)
    if (!pinned.ok) return pinned.reason
  }
  return null
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
/** The catalog root itself must be a real directory, not a link to one. */
function rootIsContained() {
  let status = null
  try {
    status = lstatSync(DIST_CATALOG)
  } catch {
    return false
  }
  return status.isDirectory() && !status.isSymbolicLink()
}

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
  // whole subtree. Intermediate directories are *pinned*, so a later swap is
  // detected rather than silently followed.
  let walked = DIST_CATALOG
  for (const segment of segments) {
    walked = join(walked, segment)
    if (walked === absolute) break
    if (pinnedDirectories.some((pin) => pin.absolute === walked)) continue
    if (!pinDirectory(walked).ok) return null
  }
  let leaf
  try {
    leaf = lstatSync(absolute)
  } catch {
    return null
  }
  if (leaf.isSymbolicLink() || !leaf.isFile()) return null
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

/**
 * Reads a leaf without following anything, and proves what it read.
 *
 * The file is opened `O_NOFOLLOW`, then `fstat`ed *through that descriptor* and
 * read from it, so the bytes hashed are the bytes of the inode that was
 * checked — a path swapped between the check and the read cannot be smuggled
 * in. A hard link (`nlink > 1`) is refused for the same reason a symlink is:
 * the same bytes are reachable, and writable, from outside the catalog.
 */
function readContainedFile(absolute, maxBytes) {
  if (!Number.isInteger(maxBytes) || maxBytes <= 0) {
    throw new Error('readContainedFile needs an explicit byte ceiling')
  }
  let descriptor = null
  try {
    descriptor = openSync(absolute, fsConstants.O_RDONLY | fsConstants.O_NOFOLLOW)
    const before = fstatSync(descriptor)
    if (!before.isFile()) return { bytes: null, reason: 'is not a regular file' }
    if (before.nlink !== 1) return { bytes: null, reason: `has ${before.nlink} hard links` }

    // Size is decided from the descriptor *before* a byte is allocated: a
    // multi-gigabyte or sparse file must be refused, not read into memory.
    if (before.size > maxBytes) {
      return { bytes: null, reason: `is ${before.size} bytes, over the ${maxBytes}-byte ceiling` }
    }

    const bytes = Buffer.alloc(before.size)
    let read = 0
    while (read < bytes.length) {
      const chunk = readSync(descriptor, bytes, read, bytes.length - read, read)
      if (chunk === 0) break
      read += chunk
    }
    if (read !== bytes.length) return { bytes: null, reason: 'was truncated while being read' }

    // Exact EOF, and the same inode at the same size: a file that grew, shrank
    // or was replaced under the descriptor is refused rather than hashed.
    const tail = Buffer.alloc(1)
    if (readSync(descriptor, tail, 0, 1, bytes.length) !== 0) {
      return { bytes: null, reason: 'grew while being read' }
    }
    const after = fstatSync(descriptor)
    if (after.size !== before.size || after.ino !== before.ino || after.dev !== before.dev) {
      return { bytes: null, reason: 'changed while being read' }
    }
    return { bytes, reason: null }
  } catch (error) {
    return {
      bytes: null,
      reason: `cannot be opened without following links (${error.code ?? error.message})`,
    }
  } finally {
    if (descriptor !== null) closeSync(descriptor)
  }
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
    const identityRead = readContainedFile(file, MAX_CHUNK_BYTES)
    if (!require(identityRead.bytes !== null, `${relative} ${identityRead.reason}`)) continue
    const identity = identityRead.bytes
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
      // A compressed sibling is bounded by the payload it encodes, so an
      // oversized one is refused before it is allocated for.
      const siblingRead = readContainedFile(sibling, identity.length + MAX_COMPRESSED_OVERHEAD)
      if (!require(siblingRead.bytes !== null, `${siblingRelative} ${siblingRead.reason}`)) continue
      const packed = siblingRead.bytes
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
  if (!require(rootIsContained(), `${DIST_CATALOG} is not a real directory (a symlinked catalog root redirects everything below it)`)) {
    return
  }
  // The directories above the catalog decide what "inside the catalog" means,
  // and a caller can point `--dist` anywhere, so the whole ancestry is pinned
  // before anything below it is trusted.
  const ancestry = pinCatalogAncestry()
  if (!require(ancestry === null, `${DIST_CATALOG} has an unsafe ancestor: ${ancestry}`)) {
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

  const manifestRead = readContainedFile(manifestPath, MAX_MANIFEST_BYTES)
  if (!require(manifestRead.bytes !== null, `dist/locale-catalog/manifest.json ${manifestRead.reason}`)) {
    return
  }
  const manifestBytes = manifestRead.bytes
  let manifest = null
  try {
    manifest = parseUntrustedJson(manifestBytes.toString('utf8'))
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

  // The manifest must describe the route it is actually served at, and it must
  // describe it consistently: nothing here is inferred from the manifest's own
  // claims about where it lives.
  const revisionPath = `${ROUTE}/r/${manifest.catalogRevision}/manifest.json`
  require(manifest.basePath === ROUTE, `basePath is ${manifest.basePath}, not ${ROUTE}`)
  require(
    manifest.manifestPath === ROUTE_MANIFEST,
    `manifestPath is ${manifest.manifestPath}, not ${ROUTE_MANIFEST}`,
  )
  require(
    manifest.revisionManifestPath === revisionPath,
    `revisionManifestPath is ${manifest.revisionManifestPath}, not the path its own revision derives (${revisionPath})`,
  )
  require(
    manifest.chunkPathPrefix === ROUTE_CHUNK_PREFIX,
    `chunkPathPrefix is ${manifest.chunkPathPrefix}, not ${ROUTE_CHUNK_PREFIX}`,
  )

  require(
    manifest.totals.bytes <= MAX_CATALOG_BYTES,
    `the manifest claims ${manifest.totals.bytes} bytes, over the ${MAX_CATALOG_BYTES}-byte ceiling`,
  )
  const descriptors = manifest.locales.reduce((total, locale) => total + locale.chunks.length, 0)
  require(
    descriptors + 2 <= MAX_CATALOG_FILES,
    `the manifest lists ${descriptors} chunks, over the ${MAX_CATALOG_FILES}-file ceiling`,
  )

  const revisionRelative = `r/${manifest.catalogRevision}/manifest.json`
  const revisionManifest = safeCatalogPath(revisionRelative)
  if (
    require(
      revisionManifest !== null,
      'the immutable revision manifest is missing from dist, is a symlink, or is not a regular file',
    )
  ) {
    const revisionRead = readContainedFile(revisionManifest, MAX_MANIFEST_BYTES)
    if (!require(revisionRead.bytes !== null, `the immutable revision manifest ${revisionRead.reason}`)) {
      return
    }
    const revisionBytes = revisionRead.bytes
    require(revisionBytes.equals(manifestBytes), 'the stable and immutable manifests differ in dist')
    let revisionErrors = ['not JSON']
    try {
      revisionErrors = validateAgainstSchema(
        parseUntrustedJson(revisionBytes.toString('utf8')),
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
      if (!require(chunk.bytes <= MAX_CHUNK_BYTES, `${chunk.path} claims ${chunk.bytes} bytes`)) {
        continue
      }
      const read = readContainedFile(file, MAX_CHUNK_BYTES)
      if (!require(read.bytes !== null, `${chunk.path} ${read.reason}`)) continue
      require(read.bytes.length === chunk.bytes, `${chunk.path} size mismatch in dist`)
      require(sha256Hex(read.bytes) === chunk.sha256, `${chunk.path} digest mismatch in dist`)
    }
  }

  if (!DIST_ONLY) verifyPublicMirror(expected)
  verifyCompressedSiblings(expected)

  // Nothing that was validated may have been swapped since it was validated:
  // every directory this walked is still the directory it was pinned to.
  const drifted = assertPinsIntact()
  require(drifted === null, `the catalog changed while it was being verified: ${drifted}`)

  if (problems.length === 0) {
    console.log(
      `locale-catalog: dist publishes revision ${manifest.catalogRevision} ` +
        `(${manifest.totals.chunks} files, ${(manifest.totals.bytes / 1024 / 1024).toFixed(2)} MB)`,
    )
  }
}

try {
  verifyCatalog()
} finally {
  for (const pin of pinnedDirectories) closeSync(pin.descriptor)
}

if (problems.length > 0) {
  console.error('locale-catalog: the build output is not servable')
  for (const problem of problems.slice(0, 20)) console.error(`  - ${problem}`)
  process.exitCode = 1
}
