// Deploy-seam check: the catalog the generator produced must actually be in
// the static root the production container serves (`frontend/dist`, mounted at
// `/opt/arkham/src/frontend/dist` by prod.nginxconf), with the digests the
// manifest promises and the precompressed siblings nginx serves.
//
//   npm run build && node scripts/locale-catalog/verify-dist.mjs

import { existsSync, readFileSync, readdirSync, statSync } from 'node:fs'
import { dirname, join, resolve } from 'node:path'
import { fileURLToPath } from 'node:url'

import { sha256Hex } from './canonical.mjs'

const FRONTEND_DIR = resolve(dirname(fileURLToPath(import.meta.url)), '..', '..')

// `--dist <dir>` lets the offline packager point at its own build output; the
// default is the directory prod.nginxconf serves.
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

const problems = []

function require(condition, message) {
  if (!condition) problems.push(message)
  return condition
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
    // the generator wrote during prebuild.
    const publicCatalog = join(FRONTEND_DIR, 'public', 'locale-catalog')
    if (require(existsSync(publicCatalog), 'public/locale-catalog is missing — prebuild did not run')) {
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
    // both are produced by the postbuild precompress pass.
    for (const relative of expected) {
      const file = join(DIST_CATALOG, relative)
      if (!existsSync(file) || statSync(file).size < PRECOMPRESS_MIN_BYTES) continue
      require(existsSync(`${file}.gz`), `${relative} has no precompressed .gz sibling`)
      require(existsSync(`${file}.br`), `${relative} has no precompressed .br sibling`)
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
