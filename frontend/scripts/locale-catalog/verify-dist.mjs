// Deploy-seam check: the catalog the generator produced must actually be in
// the static root the production container serves (`frontend/dist`, mounted at
// `/opt/arkham/src/frontend/dist` by prod.nginxconf), with the digests the
// manifest promises and the precompressed siblings nginx serves.
//
//   npm run build && node scripts/locale-catalog/verify-dist.mjs

import {
  chmodSync,
  closeSync,
  opendirSync,
  constants as fsConstants,
  existsSync,
  fstatSync,
  lstatSync,
  openSync,
  readFileSync,
  readdirSync,
  readSync,
  realpathSync,
  renameSync,
  rmSync,
  mkdirSync,
  mkdtempSync,
  writeFileSync,
} from 'node:fs'
import { brotliDecompressSync, gunzipSync } from 'node:zlib'
import { dirname, join, relative, resolve, sep } from 'node:path'
import { execFileSync } from 'node:child_process'
import { fileURLToPath } from 'node:url'

import { sha256Hex } from './canonical.mjs'

const FRONTEND_DIR = resolve(dirname(fileURLToPath(import.meta.url)), '..', '..')

// `--dist <dir>` lets the offline packager point at its own build output; the
// default is the directory prod.nginxconf serves.
const DIST_ONLY = process.argv.includes('--dist-only')
// `--publish` makes the check answer the only question that matters for a
// restored cache: are the bytes nginx will serve the bytes that were verified?
// Every artifact is written into a fresh private directory from the buffer
// that was hashed, and that directory — nothing else — is moved into place.
// Without it a verifier can only ever say "these bytes were correct when I read
// them".
const PUBLISH = process.argv.includes('--publish')
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
// Identity plus its `.gz` and `.br`: the most artifacts a valid catalog of
// MAX_CATALOG_FILES logical files can contain.
const MAX_CATALOG_ARTIFACTS = MAX_CATALOG_FILES * 3
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

// `fs.constants` silently yields `undefined` for a flag a platform does not
// have, and `undefined | 0` is `0` — which would turn every no-follow open into
// an ordinary one. Refuse to run rather than verify with the guarantees off.
for (const flag of ['O_RDONLY', 'O_NOFOLLOW', 'O_DIRECTORY']) {
  if (typeof fsConstants[flag] !== 'number') {
    console.error(`locale-catalog: this platform has no ${flag}; the verifier cannot guarantee containment`)
    process.exit(1)
  }
}

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

/**
 * Enumerates the catalog, stopping the moment it exceeds what a valid catalog
 * could contain. A directory full of files is not a reason to build an
 * unbounded array in memory before deciding it is invalid.
 */
/**
 * Streams the catalog's tree, entry by entry, against a closed topology.
 *
 * `readdirSync` materializes a whole directory before anyone can object to its
 * size, so this uses `opendirSync` and decides about each entry as it arrives.
 * Every entry counts against the budget — directories included — and the shape
 * is fixed in advance: the root holds the manifest and exactly `c/` and `r/`,
 * `c/` holds content-addressed leaves, `r/` holds one revision directory of
 * manifests. Anything broader or deeper is refused before it is walked, so a
 * directory with millions of entries costs one `readdir` step, not an array.
 */
const CATALOG_TOPOLOGY = {
  '': { directories: new Set(['c', 'r']), leaf: /^manifest\.json(\.gz|\.br)?$/ },
  c: { directories: new Set(), leaf: /^[0-9a-f]{64}\.json(\.gz|\.br)?$/ },
  r: { directories: 'revision', leaf: null },
}
const REVISION_DIRECTORY = /^1\.[0-9a-f]{32}$/
const MAX_CATALOG_DEPTH = 2

function listFiles(directory) {
  const paths = []
  const pending = [{ absolute: directory, prefix: '', depth: 0 }]
  let budget = MAX_CATALOG_ARTIFACTS

  while (pending.length > 0) {
    const current = pending.pop()
    if (current.depth > MAX_CATALOG_DEPTH) {
      return { paths, overflowed: true, reason: `${current.prefix} is deeper than a catalog can be` }
    }
    const shape =
      current.depth === 0
        ? CATALOG_TOPOLOGY['']
        : current.prefix === 'c'
          ? CATALOG_TOPOLOGY.c
          : current.prefix === 'r'
            ? CATALOG_TOPOLOGY.r
            : REVISION_DIRECTORY.test(current.prefix.slice(2))
              ? { directories: new Set(), leaf: /^manifest\.json(\.gz|\.br)?$/ }
              : null
    if (shape === null) {
      return { paths, overflowed: true, reason: `${current.prefix} is not a directory a catalog has` }
    }

    let handle = null
    try {
      handle = opendirSync(current.absolute)
      for (let entry = handle.readSync(); entry !== null; entry = handle.readSync()) {
        // Counted the moment it is seen, before anything is stored.
        budget -= 1
        if (budget < 0) {
          return { paths, overflowed: true, reason: `more than ${MAX_CATALOG_ARTIFACTS} entries` }
        }
        const path = current.prefix === '' ? entry.name : `${current.prefix}/${entry.name}`
        if (entry.isDirectory()) {
          const allowed =
            shape.directories === 'revision'
              ? REVISION_DIRECTORY.test(entry.name)
              : shape.directories.has(entry.name)
          if (!allowed) {
            return { paths, overflowed: true, reason: `${path} is not a directory a catalog has` }
          }
          pending.push({
            absolute: join(current.absolute, entry.name),
            prefix: path,
            depth: current.depth + 1,
          })
          continue
        }
        if (!entry.isFile()) {
          return { paths, overflowed: true, reason: `${path} is not a regular file` }
        }
        if (shape.leaf === null || !shape.leaf.test(entry.name)) {
          return { paths, overflowed: true, reason: `${path} is not an artifact a catalog has` }
        }
        paths.push(path)
      }
    } catch (error) {
      return { paths, overflowed: true, reason: `${current.prefix || '.'}: ${error.code ?? error.message}` }
    } finally {
      if (handle !== null) {
        try {
          handle.closeSync()
        } catch {
          // already closed by the iterator
        }
      }
    }
  }

  return { paths, overflowed: false, reason: null }
}

function listJson(directory) {
  const listed = listFiles(directory)
  return { paths: listed.paths.filter((path) => path.endsWith('.json')), overflowed: listed.overflowed }
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
  const listed = listJson(publicCatalog)
  if (!require(!listed.overflowed, `public/locale-catalog is not a catalog shape: ${listed.reason}`)) {
    return
  }
  const published = listed.paths
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
      if (
        require(
          inflated.equals(identity),
          `${siblingRelative} decompresses to different bytes than ${relative}`,
        )
      ) {
        recordVerified(siblingRelative, packed)
      }
    }
  }

  // Anything else under the catalog is unaccounted for: nginx would happily
  // serve it for a path the manifest never promised.
  const present = listFiles(DIST_CATALOG)
  if (!require(!present.overflowed, `dist/locale-catalog is not a catalog shape: ${present.reason}`)) {
    return
  }
  for (const relative of present.paths) {
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

/**
 * Checks the manifest against itself before anything on disk is touched.
 *
 * Totals are self-attested: a manifest can claim a small catalog and then list
 * the same 8 MiB chunk four thousand times, or list one `(locale, pack)` twice
 * with different digests. Everything here is recomputed from the descriptors —
 * per-locale and global key, chunk and byte counts — and each unique content
 * path is read exactly once.
 */
function preflightDescriptors(manifest) {
  const problems = []
  const paths = new Map()
  const packs = new Set()
  const digests = new Map()
  let totalKeys = 0
  let totalChunks = 0
  let totalBytes = 0
  let totalUnsupported = 0

  for (const locale of manifest.locales) {
    let localeKeys = 0
    let localeBytes = 0
    for (const chunk of locale.chunks) {
      const pack = `${locale.locale}/${chunk.pack}`
      if (packs.has(pack)) problems.push(`${pack} is listed more than once`)
      packs.add(pack)

      if (!chunk.path.startsWith(`${manifest.basePath}/`)) {
        problems.push(`${chunk.path} is not published under ${manifest.basePath}`)
        continue
      }
      const relative = chunk.path.slice(`${manifest.basePath}/`.length)

      // Content addressing is the contract: the name *is* the digest, so a
      // descriptor cannot point at one file and promise another's bytes.
      const named = CHUNK_PATH.exec(relative)
      if (named === null) {
        problems.push(`${chunk.path} is not a content-addressed chunk path (c/<sha256>.json)`)
        continue
      }
      if (named[1] !== chunk.sha256) {
        problems.push(`${chunk.path} names a digest it does not promise`)
        continue
      }
      if (chunk.bytes > MAX_CHUNK_BYTES) {
        problems.push(`${chunk.path} claims ${chunk.bytes} bytes`)
        continue
      }

      // A chunk carries its own locale, fallback and pack, so one content path
      // belongs to exactly one descriptor. Two descriptors sharing a path would
      // mean the same bytes claiming two identities.
      const seen = paths.get(relative)
      if (seen !== undefined) {
        problems.push(
          `${chunk.path} is claimed by ${seen.locale}/${seen.chunk.pack} and ${locale.locale}/${chunk.pack}`,
        )
      } else {
        paths.set(relative, { chunk, locale: locale.locale, fallback: locale.fallback ?? null })
        totalChunks += 1
        totalBytes += chunk.bytes
      }
      const digestPath = digests.get(chunk.sha256)
      if (digestPath !== undefined && digestPath !== relative) {
        problems.push(`digest ${chunk.sha256} is claimed by ${digestPath} and ${relative}`)
      }
      digests.set(chunk.sha256, relative)

      localeKeys += chunk.keys
      localeBytes += chunk.bytes
      totalUnsupported += chunk.unsupportedKeys
    }

    if (localeKeys !== locale.keys) {
      problems.push(`${locale.locale} claims ${locale.keys} keys, its chunks total ${localeKeys}`)
    }
    if (localeBytes !== locale.bytes) {
      problems.push(`${locale.locale} claims ${locale.bytes} bytes, its chunks total ${localeBytes}`)
    }
    totalKeys += localeKeys
  }

  if (paths.size + 2 > MAX_CATALOG_FILES) {
    problems.push(`the manifest lists ${paths.size} chunks, over the ${MAX_CATALOG_FILES}-file ceiling`)
  }
  if (totalBytes > MAX_CATALOG_BYTES) {
    problems.push(`the manifest's chunks total ${totalBytes} bytes, over the ${MAX_CATALOG_BYTES}-byte ceiling`)
  }
  if (manifest.totals.locales !== manifest.locales.length) {
    problems.push(
      `the manifest claims ${manifest.totals.locales} locales, it lists ${manifest.locales.length}`,
    )
  }
  if (manifest.totals.unsupportedKeys !== totalUnsupported) {
    problems.push(
      `the manifest claims ${manifest.totals.unsupportedKeys} unsupported keys, its descriptors total ${totalUnsupported}`,
    )
  }
  if (manifest.totals.keys !== totalKeys) {
    problems.push(`the manifest claims ${manifest.totals.keys} keys, its descriptors total ${totalKeys}`)
  }
  if (manifest.totals.chunks !== totalChunks) {
    problems.push(`the manifest claims ${manifest.totals.chunks} chunks, its descriptors total ${totalChunks}`)
  }
  if (manifest.totals.bytes !== totalBytes) {
    problems.push(`the manifest claims ${manifest.totals.bytes} bytes, its descriptors total ${totalBytes}`)
  }

  return { problems, paths }
}

/**
 * Collects the exact bytes each artifact was verified from, so `--publish` can
 * write those, and only those, into the tree that gets served.
 */
const verifiedArtifacts = new Map()
const verifiedBytes = verifiedArtifacts

function recordVerified(relative, bytes) {
  verifiedArtifacts.set(relative, bytes)
}

/**
 * Replaces the catalog with a private snapshot built from the verified bytes.
 *
 * The snapshot is created mode 0700 beside the target, written from the
 * buffers this run hashed, and moved into place with `rename`, which is atomic
 * within a filesystem. Anything that changed the source after it was read —
 * a leaf swapped back, an intermediate directory restored — cannot reach the
 * published tree, because the published tree was never read from disk again.
 */
function publishVerifiedSnapshot() {
  const parent = dirname(DIST_CATALOG)
  const staging = mkdtempSync(join(parent, '.locale-catalog-verified-'))
  try {
    chmodSync(staging, 0o700)
    for (const [relative, bytes] of verifiedBytes) {
      const target = join(staging, relative)
      mkdirSync(dirname(target), { recursive: true, mode: 0o700 })
      writeFileSync(target, bytes, { mode: 0o600, flag: 'wx' })
    }
    // Renaming the existing catalog aside keeps a rollback copy, which is the
    // better order — but on a layered filesystem (an image build, where the
    // catalog still lives on a lower layer) that rename is EXDEV. There the
    // old tree is removed first; the snapshot is already complete, and the
    // verified buffers are still in memory, so a failure after that point is
    // recoverable by writing them again.
    const retired = `${DIST_CATALOG}.replaced-${process.pid}`
    let rolledAside = false
    try {
      renameSync(DIST_CATALOG, retired)
      rolledAside = true
    } catch (error) {
      if (error.code !== 'EXDEV') throw error
      rmSync(DIST_CATALOG, { recursive: true, force: true })
    }
    try {
      renameSync(staging, DIST_CATALOG)
    } catch (error) {
      if (rolledAside) renameSync(retired, DIST_CATALOG)
      throw error
    }
    if (rolledAside) rmSync(retired, { recursive: true, force: true })
    chmodSync(DIST_CATALOG, 0o755)
  } catch (error) {
    rmSync(staging, { recursive: true, force: true })
    throw error
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
  recordVerified('manifest.json', manifestBytes)
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
    recordVerified(revisionRelative, revisionBytes)
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

  // Preflight: every descriptor is checked against every other descriptor
  // *before* a single byte is read. A manifest that repeats a chunk, or claims
  // totals its own descriptors contradict, is rejected here rather than after
  // 4,000 hashes have been computed from it.
  const preflight = preflightDescriptors(manifest)
  for (const problem of preflight.problems) require(false, problem)
  if (preflight.problems.length > 0) return

  const expected = new Set(['manifest.json', revisionRelative, ...preflight.paths.keys()])
  const chunkSchema = JSON.parse(readFileSync(join(SCHEMA_DIR, 'chunk.schema.json'), 'utf8'))
  for (const [relative, claim] of preflight.paths) {
    const chunk = claim.chunk
    const file = safeCatalogPath(relative)
    if (
      !require(
        file !== null,
        `${chunk.path} escapes the catalog, or is a symlink or a non-regular file`,
      )
    ) {
      continue
    }
    const read = readContainedFile(file, MAX_CHUNK_BYTES)
    if (!require(read.bytes !== null, `${chunk.path} ${read.reason}`)) continue
    if (!require(read.bytes.length === chunk.bytes, `${chunk.path} size mismatch in dist`)) continue
    if (!require(sha256Hex(read.bytes) === chunk.sha256, `${chunk.path} digest mismatch in dist`)) {
      continue
    }
    recordVerified(relative, read.bytes)

    // The descriptor and the chunk it points at must agree about what the
    // chunk *is*, not merely about its digest: a closed chunk carries one
    // locale, one fallback and one pack.
    let parsed = null
    try {
      parsed = parseUntrustedJson(read.bytes.toString('utf8'))
    } catch (error) {
      require(false, `${chunk.path} is not JSON (${error.message})`)
      continue
    }
    const chunkErrors = validateAgainstSchema(parsed, chunkSchema, chunkSchema)
    if (
      !require(
        chunkErrors.length === 0,
        `${chunk.path} does not satisfy the v1 chunk schema: ${chunkErrors.slice(0, 3).join('; ')}`,
      )
    ) {
      continue
    }
    const entries = Object.keys(parsed.entries)
    const unsupported = entries.filter((key) => parsed.entries[key].form === 'unsupported').length
    for (const [what, actual, promised] of [
      ['locale', parsed.locale, claim.locale],
      ['fallback', parsed.fallback ?? null, claim.fallback],
      ['pack', parsed.pack, chunk.pack],
      ['entry count', entries.length, chunk.keys],
      ['unsupported count', unsupported, chunk.unsupportedKeys],
      ['byte count', read.bytes.length, chunk.bytes],
    ]) {
      require(
        actual === promised,
        `${chunk.path} has ${what} ${JSON.stringify(actual)}, the manifest promises ${JSON.stringify(promised)}`,
      )
    }
  }

  for (const relative of listJson(DIST_CATALOG).paths) {
    require(expected.has(relative), `dist/locale-catalog/${relative} is not listed in the manifest`)
  }

  if (!DIST_ONLY) verifyPublicMirror(expected)
  verifyCompressedSiblings(expected)

  // A deterministic seam for the swap-restore test: a hook runs here, after
  // every read and before the final checks, so a test can replace a verified
  // leaf or restore an intermediate directory and prove the run still refuses
  // to bless it.
  const hook = process.env.LOCALE_CATALOG_VERIFY_HOOK
  if (hook !== undefined && hook !== '') {
    execFileSync('/bin/sh', ['-c', hook], { stdio: 'inherit', env: { ...process.env, DIST_CATALOG } })
  }

  // Nothing that was validated may have been swapped since it was validated:
  // every directory this walked is still the directory it was pinned to, and
  // every artifact still hashes to what it hashed to.
  const drifted = assertPinsIntact()
  require(drifted === null, `the catalog changed while it was being verified: ${drifted}`)

  for (const [relative, bytes] of verifiedArtifacts) {
    const file = safeCatalogPath(relative)
    if (!require(file !== null, `${relative} stopped being a contained regular file`)) continue
    const reread = readContainedFile(file, bytes.length)
    if (!require(reread.bytes !== null, `${relative} ${reread.reason} on re-read`)) continue
    require(reread.bytes.equals(bytes), `${relative} changed after it was verified`)
  }

  if (PUBLISH && problems.length === 0) publishVerifiedSnapshot()

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
