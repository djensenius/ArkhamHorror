// Deploy-seam check: the catalog the generator produced must actually be in
// the static root the production container serves (`frontend/dist`, mounted at
// `/opt/arkham/src/frontend/dist` by prod.nginxconf), with the digests the
// manifest promises and the precompressed siblings nginx serves.
//
//   npm run build && node scripts/locale-catalog/verify-dist.mjs

import { existsSync, readFileSync, readdirSync, statSync } from 'node:fs'
import { brotliDecompressSync, gunzipSync } from 'node:zlib'
import { dirname, join, resolve } from 'node:path'
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

const problems = []

function require(condition, message) {
  if (!condition) problems.push(message)
  return condition
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

if (require(existsSync(DIST_CATALOG), `${DIST_CATALOG} is missing — the build did not publish the catalog`)) {
  const manifestPath = join(DIST_CATALOG, 'manifest.json')
  if (require(existsSync(manifestPath), 'dist/locale-catalog/manifest.json is missing')) {
    const manifestBytes = readFileSync(manifestPath)
    const manifest = JSON.parse(manifestBytes.toString('utf8'))
    const revisionManifest = join(DIST_CATALOG, 'r', manifest.catalogRevision, 'manifest.json')

    require(existsSync(revisionManifest), 'the immutable revision manifest is missing from dist')
    require(
      existsSync(revisionManifest) && readFileSync(revisionManifest).equals(manifestBytes),
      'the stable and immutable manifests differ in dist',
    )

    const expected = new Set(['manifest.json', `r/${manifest.catalogRevision}/manifest.json`])
    for (const locale of manifest.locales) {
      for (const chunk of locale.chunks) {
        const relative = chunk.path.slice(`${manifest.basePath}/`.length)
        expected.add(relative)
        const file = join(DIST_CATALOG, relative)
        if (!require(existsSync(file), `${chunk.path} is missing from dist`)) continue
        const bytes = readFileSync(file)
        require(bytes.length === chunk.bytes, `${chunk.path} size mismatch in dist`)
        require(sha256Hex(bytes) === chunk.sha256, `${chunk.path} digest mismatch in dist`)
      }
    }

    for (const relative of listJson(DIST_CATALOG)) {
      require(expected.has(relative), `dist/locale-catalog/${relative} is not listed in the manifest`)
    }

    // Vite copies public/ verbatim, so the served bytes must equal the bytes
    // the generator wrote during prebuild. `--dist-only` skips that comparison
    // for a build output restored from a cache, where `public/` belongs to a
    // tree that no longer exists; everything above still checks the output
    // against its own manifest, which is what makes a cached artifact usable.
    const publicCatalog = join(FRONTEND_DIR, 'public', 'locale-catalog')
    if (
      !DIST_ONLY &&
      require(existsSync(publicCatalog), 'public/locale-catalog is missing — prebuild did not run')
    ) {
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

    // nginx serves .br by an explicit -f test and .gz through gzip_static;
    // both are produced by the postbuild precompress pass. A cache can restore
    // a compressed sibling that no longer matches the JSON beside it, and
    // nginx would serve those bytes without ever reading the identity file, so
    // each one is decompressed and compared — under a size ceiling, because a
    // corrupt sibling is exactly where a decompression bomb would sit.
    const compressed = new Set()
    for (const relative of expected) {
      const file = join(DIST_CATALOG, relative)
      if (!existsSync(file)) continue
      const identity = readFileSync(file)
      if (identity.length < PRECOMPRESS_MIN_BYTES) continue
      for (const [suffix, inflate] of [
        ['.gz', gunzipSync],
        ['.br', brotliDecompressSync],
      ]) {
        const sibling = `${file}${suffix}`
        if (!require(existsSync(sibling), `${relative} has no precompressed ${suffix} sibling`)) {
          continue
        }
        compressed.add(`${relative}${suffix}`)
        const packed = readFileSync(sibling)
        if (
          !require(
            packed.length <= identity.length + MAX_COMPRESSED_OVERHEAD,
            `${relative}${suffix} is larger than the payload it encodes`,
          )
        ) {
          continue
        }
        let inflated = null
        try {
          inflated = inflate(packed, { maxOutputLength: identity.length + MAX_INFLATE_SLACK })
        } catch (error) {
          require(false, `${relative}${suffix} does not decompress (${error.code ?? error.message})`)
          continue
        }
        require(
          inflated.equals(identity),
          `${relative}${suffix} decompresses to different bytes than ${relative}`,
        )
      }
    }

    // Anything else compressed under the catalog is unaccounted for: nginx
    // would happily serve it for a path the manifest never promised.
    for (const relative of listFiles(DIST_CATALOG)) {
      if (!relative.endsWith('.gz') && !relative.endsWith('.br')) continue
      require(
        compressed.has(relative),
        `dist/locale-catalog/${relative} is a compressed artifact the manifest does not list`,
      )
    }

    if (problems.length === 0) {
      console.log(
        `locale-catalog: dist publishes revision ${manifest.catalogRevision} ` +
          `(${manifest.totals.chunks} files, ${(manifest.totals.bytes / 1024 / 1024).toFixed(2)} MB)`,
      )
    }
  }
}

if (problems.length > 0) {
  console.error('locale-catalog: the build output is not servable')
  for (const problem of problems.slice(0, 20)) console.error(`  - ${problem}`)
  process.exitCode = 1
}
