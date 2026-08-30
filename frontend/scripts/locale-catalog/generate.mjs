// Generates the public locale catalog: a deterministic, content-addressed set
// of chunk files plus a manifest that pins every chunk's size and SHA-256.
//
//   node scripts/locale-catalog/generate.mjs            # write public/locale-catalog
//   node scripts/locale-catalog/generate.mjs --check    # fail if that output is stale
//
// Everything published here is derived from the committed
// `frontend/src/locales/**` snapshot the Vue build bundles — no prose is
// copied into this toolchain, and no image bytes are ever embedded. The
// catalog revision is a digest over those sources plus this generator, the
// schemas, and the contract fixtures that define the required key set, so
// identical inputs always produce identical bytes and any input change is
// visible as a new revision.
//
// Failures are build failures. A missing or unsupported *required* key, a
// duplicate key, an unresolvable link, an undeclared variable, an unsafe path,
// or a size/count bound being exceeded aborts generation rather than shipping
// a success-shaped partial catalog.

import { existsSync, mkdirSync, readFileSync, readdirSync, rmSync, statSync, writeFileSync } from 'node:fs'
import { dirname, join, relative, resolve } from 'node:path'
import { fileURLToPath } from 'node:url'

import { canonicalJson, sha256Hex } from './canonical.mjs'
import {
  ASSET_PATH_VARIABLES,
  KEY_SEGMENT_PATTERN,
  UNSUPPORTED_REASONS,
  normalizeMessage,
  referencedVariables,
  staticLinkTargets,
} from './normalize.mjs'
import {
  collectSourceFiles,
  loadLocaleSources,
  makeVariableClassifier,
  toolchainVersions,
} from './sources.mjs'

const HERE = dirname(fileURLToPath(import.meta.url))
const FRONTEND_DIR = resolve(HERE, '..', '..')
const REPO_ROOT = resolve(FRONTEND_DIR, '..')

export const SCHEMA_VERSION = '1.0.0'
export const GENERATOR_NAME = 'arkham-locale-catalog'
export const GENERATOR_VERSION = '1.0.0'
export const BASE_PATH = '/locale-catalog'
export const DEFAULT_OUTPUT_DIR = join(FRONTEND_DIR, 'public', 'locale-catalog')

const SCHEMA_FILES = [
  'frontend/schemas/locale-catalog/v1/manifest.schema.json',
  'frontend/schemas/locale-catalog/v1/chunk.schema.json',
]

// The contract fixtures whose I18n keys this catalog revision must resolve.
// Required keys are read out of the fixtures themselves, never transcribed.
const REQUIRED_KEY_FIXTURES = [
  'contracts/fixtures/question-read.json',
  'contracts/fixtures/question-read-with-cards.json',
]
const CONTRACT_MANIFEST = 'contracts/manifest.json'

// Bounds, so a client's worst case stays predictable and a runaway source tree
// fails the build instead of the deployment.
const MAX_CHUNK_BYTES = 8 * 1024 * 1024
const MAX_CHUNKS_PER_LOCALE = 256
const MAX_CATALOG_BYTES = 192 * 1024 * 1024

const PACK_PATTERN = /^[A-Za-z0-9][A-Za-z0-9_-]{0,63}$/
const LOCALE_PATTERN = /^[a-z]{2,3}(?:-[A-Za-z0-9]{2,8})*$/

// Language tags probed through the production resolver so native clients get
// the web client's exact locale/fallback behavior instead of guessing at it.
const PROBE_LANGUAGE_TAGS = [
  'de', 'de-AT', 'en', 'en-GB', 'es', 'es-MX', 'fr', 'fr-CA', 'it', 'ko',
  'pt-BR', 'ru', 'zh', 'zh-CN', 'zh-Hans', 'zh-Hant', 'zh-TW',
]

class GenerationError extends Error {}

function fail(message) {
  throw new GenerationError(`locale-catalog: ${message}`)
}

function readRepoFile(relativePath) {
  const path = join(REPO_ROOT, relativePath)
  if (!statSync(path).isFile()) fail(`${relativePath} is not a regular file`)
  return readFileSync(path)
}

function digestRepoFiles(relativePaths) {
  return relativePaths
    .map((path) => ({ path, sha256: sha256Hex(readRepoFile(path)) }))
    .sort((a, b) => (a.path < b.path ? -1 : 1))
}

function generatorSourceDigests() {
  return readdirSync(HERE, { withFileTypes: true })
    .filter((entry) => entry.isFile() && entry.name.endsWith('.mjs'))
    .map((entry) => ({
      path: relative(REPO_ROOT, join(HERE, entry.name)).split('\\').join('/'),
      sha256: sha256Hex(readFileSync(join(HERE, entry.name))),
    }))
    .sort((a, b) => (a.path < b.path ? -1 : 1))
}

/**
 * Pulls the I18n keys the production contract fixtures actually reference:
 * `I18nEntry.key` values plus `$key` shorthands in titles and labels. This is
 * the machine-derived definition of "required", so the required set follows
 * the fixtures instead of a hand-maintained list.
 */
export function requiredKeysFromFixture(value, into = new Set()) {
  if (Array.isArray(value)) {
    for (const item of value) requiredKeysFromFixture(item, into)
    return into
  }
  if (value === null || typeof value !== 'object') return into

  if (value.tag === 'I18nEntry' && typeof value.key === 'string') into.add(value.key)
  for (const [field, child] of Object.entries(value)) {
    if (typeof child === 'string') {
      // `title`/`label`/`text` carry `$key` shorthands (see Arkham/I18n.hs and
      // handleI18n in src/arkham/i18n.ts); the key stops at the first space,
      // which is where typed variables begin.
      if (child.startsWith('$') && ['title', 'label', 'text'].includes(field)) {
        const key = child.slice(1).split(' ')[0]
        if (key !== '') into.add(key)
      }
      continue
    }
    requiredKeysFromFixture(child, into)
  }
  return into
}

function collectRequiredKeys() {
  const keys = new Set()
  for (const fixture of REQUIRED_KEY_FIXTURES) {
    requiredKeysFromFixture(JSON.parse(readRepoFile(fixture).toString('utf8')), keys)
  }
  if (keys.size === 0) fail('contract fixtures declared no required I18n keys')
  return [...keys].sort()
}

function flattenMessages(value, prefix, out) {
  if (typeof value === 'string') {
    if (out.has(prefix)) fail(`duplicate normalized key ${prefix}`)
    out.set(prefix, value)
    return
  }
  if (Array.isArray(value)) {
    value.forEach((item, index) => flattenMessages(item, `${prefix}.${index}`, out))
    return
  }
  if (value !== null && typeof value === 'object') {
    for (const key of Object.keys(value)) {
      if (!KEY_SEGMENT_PATTERN.test(key)) fail(`unsafe message key segment ${JSON.stringify(key)}`)
      flattenMessages(value[key], prefix === '' ? key : `${prefix}.${key}`, out)
    }
    return
  }
  fail(`non-string leaf at ${prefix} (${value === null ? 'null' : typeof value})`)
}

function packOf(key) {
  const pack = key.includes('.') ? key.slice(0, key.indexOf('.')) : 'core'
  if (!PACK_PATTERN.test(pack)) fail(`unsafe pack id derived from ${key}`)
  return pack
}

function normalizeLocale(messages, classifyVariable) {
  const flattened = new Map()
  flattenMessages(messages, '', flattened)

  const entries = new Map()
  for (const key of [...flattened.keys()].sort()) {
    const entry = normalizeMessage(flattened.get(key), { classifyVariable })
    if (entry.form !== 'unsupported') {
      // Every variable a node references must be declared on the entry, with
      // the same role, so a client can never encounter an undeclared or
      // contradictorily typed substitution slot.
      const declared = new Map(
        entry.variables.map((variable) => [`${variable.source}:${variable.name}`, variable]),
      )
      for (const [id, reference] of referencedVariables(entry)) {
        const declaration = declared.get(id)
        if (declaration === undefined) fail(`undeclared variable ${id} in ${key}`)
        if (declaration.role !== reference.role) {
          fail(`variable ${id} in ${key} is declared ${declaration.role} but referenced as ${reference.role}`)
        }
      }
    } else if (!UNSUPPORTED_REASONS.includes(entry.reason)) {
      fail(`unknown unsupported reason ${entry.reason} for ${key}`)
    }
    entries.set(key, entry)
  }
  return entries
}

/**
 * Links resolve against the entry's own locale first and the fallback locale
 * second, matching vue-i18n. A link that resolves to nothing is downgraded to
 * an explicitly unsupported entry rather than published as a dangling
 * reference.
 */
function resolveLinks(entries, fallbackEntries) {
  for (const [key, entry] of entries) {
    if (entry.form === 'unsupported') continue
    for (const target of staticLinkTargets(entry)) {
      if (entries.has(target) || fallbackEntries?.has(target)) continue
      entries.set(key, { form: 'unsupported', reason: 'unresolved-link', detail: target.slice(0, 120) })
      break
    }
  }
}

function chunkEntries(entries) {
  const packs = new Map()
  for (const [key, entry] of entries) {
    const pack = packOf(key)
    if (!packs.has(pack)) packs.set(pack, new Map())
    packs.get(pack).set(key, entry)
  }
  return packs
}

function localeResolutionTable(sources) {
  return PROBE_LANGUAGE_TAGS.map((tag) => ({
    tag,
    locale: sources.uiLocaleFor(sources.preferredLanguage(tag)),
  })).sort((a, b) => (a.tag < b.tag ? -1 : 1))
}

/** Builds the whole catalog in memory. Nothing is written by this function. */
export async function buildCatalog({ frontendDir = FRONTEND_DIR } = {}) {
  const sources = await loadLocaleSources(frontendDir)
  const classifyVariable = makeVariableClassifier(sources, ASSET_PATH_VARIABLES)

  const requiredKeys = collectRequiredKeys()
  const provenance = {
    schemaVersion: SCHEMA_VERSION,
    generator: { name: GENERATOR_NAME, version: GENERATOR_VERSION, sources: generatorSourceDigests() },
    schemas: digestRepoFiles(SCHEMA_FILES),
    toolchain: toolchainVersions(frontendDir),
    contract: {
      schemaRevision: JSON.parse(readRepoFile(CONTRACT_MANIFEST).toString('utf8')).schemaRevision,
      fixtures: digestRepoFiles(REQUIRED_KEY_FIXTURES),
      requiredKeys,
    },
    localeSources: collectSourceFiles(frontendDir),
  }
  const provenanceSha256 = sha256Hex(Buffer.from(canonicalJson(provenance), 'utf8'))
  const catalogRevision = `1.${provenanceSha256.slice(0, 32)}`
  const revisionPrefix = `${BASE_PATH}/r/${catalogRevision}`

  const defaultLocale = sources.uiLocaleFor('')
  if (!sources.locales.includes(defaultLocale)) fail(`default locale ${defaultLocale} is not supported`)

  const normalized = new Map()
  for (const locale of sources.locales) {
    if (!LOCALE_PATTERN.test(locale)) fail(`unsafe locale id ${JSON.stringify(locale)}`)
    normalized.set(locale, normalizeLocale(sources.messages[locale], classifyVariable))
  }
  for (const [locale, entries] of normalized) {
    resolveLinks(entries, locale === defaultLocale ? null : normalized.get(defaultLocale))
  }

  // Required keys must resolve in the default locale, and in every other
  // locale that ships its own copy of the key.
  for (const key of requiredKeys) {
    const fallbackEntry = normalized.get(defaultLocale).get(key)
    if (fallbackEntry === undefined) fail(`required key ${key} is missing from ${defaultLocale}`)
    if (fallbackEntry.form === 'unsupported') {
      fail(`required key ${key} is unsupported in ${defaultLocale} (${fallbackEntry.reason})`)
    }
    for (const [locale, entries] of normalized) {
      const entry = entries.get(key)
      if (entry !== undefined && entry.form === 'unsupported') {
        fail(`required key ${key} is unsupported in ${locale} (${entry.reason}: ${entry.detail})`)
      }
    }
  }

  const files = new Map()
  const locales = []
  let totalBytes = 0
  let totalKeys = 0
  let totalUnsupported = 0

  for (const locale of [...sources.locales].sort()) {
    const entries = normalized.get(locale)
    const fallback = locale === defaultLocale ? null : defaultLocale
    const packs = chunkEntries(entries)
    if (packs.size > MAX_CHUNKS_PER_LOCALE) fail(`${locale} produced ${packs.size} chunks`)

    const chunks = []
    let localeBytes = 0
    for (const pack of [...packs.keys()].sort()) {
      const packEntries = packs.get(pack)
      const chunk = {
        schemaVersion: SCHEMA_VERSION,
        catalogRevision,
        locale,
        fallback,
        pack,
        entries: Object.fromEntries([...packEntries.entries()].sort(([a], [b]) => (a < b ? -1 : 1))),
      }
      const bytes = Buffer.from(canonicalJson(chunk), 'utf8')
      const sha256 = sha256Hex(bytes)
      if (bytes.length > MAX_CHUNK_BYTES) fail(`${locale}/${pack} chunk is ${bytes.length} bytes`)

      const filePath = `r/${catalogRevision}/${locale}/${pack}.${sha256.slice(0, 16)}.json`
      if (files.has(filePath)) fail(`duplicate chunk path ${filePath}`)
      files.set(filePath, bytes)

      const unsupportedKeys = [...packEntries.values()].filter((entry) => entry.form === 'unsupported').length
      chunks.push({
        pack,
        path: `${revisionPrefix}/${locale}/${pack}.${sha256.slice(0, 16)}.json`,
        bytes: bytes.length,
        sha256,
        keys: packEntries.size,
        unsupportedKeys,
      })
      localeBytes += bytes.length
      totalKeys += packEntries.size
      totalUnsupported += unsupportedKeys
    }

    totalBytes += localeBytes
    locales.push({
      locale,
      fallback,
      chunks,
      keys: entries.size,
      bytes: localeBytes,
    })
  }

  if (totalBytes > MAX_CATALOG_BYTES) fail(`catalog is ${totalBytes} bytes`)

  const manifest = {
    schemaVersion: SCHEMA_VERSION,
    catalogRevision,
    basePath: BASE_PATH,
    manifestPath: `${BASE_PATH}/manifest.json`,
    revisionManifestPath: `${revisionPrefix}/manifest.json`,
    digestAlgorithm: 'sha256',
    defaultLocale,
    languageResolution: localeResolutionTable(sources),
    locales,
    totals: {
      locales: locales.length,
      chunks: files.size,
      bytes: totalBytes,
      keys: totalKeys,
      unsupportedKeys: totalUnsupported,
    },
    provenance: {
      sha256: provenanceSha256,
      generator: { name: GENERATOR_NAME, version: GENERATOR_VERSION },
      contractRevision: provenance.contract.schemaRevision,
      requiredKeys,
      localeSourceFiles: provenance.localeSources.length,
      localeSourcesSha256: sha256Hex(Buffer.from(canonicalJson(provenance.localeSources), 'utf8')),
      schemasSha256: sha256Hex(Buffer.from(canonicalJson(provenance.schemas), 'utf8')),
      generatorSha256: sha256Hex(Buffer.from(canonicalJson(provenance.generator), 'utf8')),
    },
  }

  const manifestBytes = Buffer.from(canonicalJson(manifest), 'utf8')
  files.set('manifest.json', manifestBytes)
  files.set(`r/${catalogRevision}/manifest.json`, manifestBytes)

  for (const path of files.keys()) {
    if (path.includes('..') || path.startsWith('/') || !/^[A-Za-z0-9][A-Za-z0-9._/-]*\.json$/.test(path)) {
      fail(`unsafe output path ${path}`)
    }
  }

  return { manifest, manifestSha256: sha256Hex(manifestBytes), files, catalogRevision, provenance }
}

export function writeCatalog(files, outputDir) {
  assertReplaceableOutputDir(outputDir)
  rmSync(outputDir, { recursive: true, force: true })
  for (const [path, bytes] of files) {
    const target = join(outputDir, path)
    if (!resolve(target).startsWith(`${resolve(outputDir)}/`)) fail(`refusing to write outside ${outputDir}`)
    mkdirSync(dirname(target), { recursive: true })
    writeFileSync(target, bytes)
  }
}

/**
 * `writeCatalog` replaces its output directory wholesale, so it must never be
 * pointed at a directory holding anything but a previously generated catalog
 * (e.g. a mistyped `--out`).
 */
export function assertReplaceableOutputDir(outputDir) {
  const target = resolve(outputDir)
  const repoRoot = resolve(REPO_ROOT)
  if (target === repoRoot || repoRoot.startsWith(`${target}/`)) {
    fail(`refusing to replace ${outputDir}: it contains the repository`)
  }
  if (!existsSync(target)) return
  if (!statSync(target).isDirectory()) fail(`${outputDir} is not a directory`)
  for (const entry of readdirSync(target)) {
    if (entry !== 'manifest.json' && entry !== 'r') {
      fail(`refusing to replace ${outputDir}: it holds ${entry}, which no catalog build wrote`)
    }
  }
}

function listFiles(directory, prefix = '') {
  const out = []
  let dirents
  try {
    dirents = readdirSync(directory, { withFileTypes: true })
  } catch (error) {
    if (error.code === 'ENOENT') return out
    throw error
  }
  for (const entry of dirents) {
    const path = prefix === '' ? entry.name : `${prefix}/${entry.name}`
    if (entry.isDirectory()) out.push(...listFiles(join(directory, entry.name), path))
    else if (entry.isFile()) out.push(path)
  }
  return out.sort()
}

/** Compares an already-written catalog with a freshly built one, byte for byte. */
export function diffCatalog(files, outputDir) {
  const problems = []
  const existing = new Set(listFiles(outputDir))
  for (const [path, bytes] of files) {
    if (!existing.delete(path)) {
      problems.push(`missing generated file ${path}`)
      continue
    }
    const actual = readFileSync(join(outputDir, path))
    if (!actual.equals(bytes)) problems.push(`stale generated file ${path}`)
  }
  for (const path of existing) problems.push(`unexpected file ${path}`)
  return problems
}

async function main(argv) {
  const check = argv.includes('--check')
  const outIndex = argv.indexOf('--out')
  if (outIndex !== -1 && (argv[outIndex + 1] ?? '').trim() === '') fail('--out requires a directory')
  const outputDir = outIndex === -1 ? DEFAULT_OUTPUT_DIR : resolve(argv[outIndex + 1])
  const provenanceIndex = argv.indexOf('--provenance')

  const { manifest, manifestSha256, files, catalogRevision, provenance } = await buildCatalog({})

  // Full provenance (every hashed input path and digest) for the validator,
  // which re-derives the manifest's provenance digests from it.
  if (provenanceIndex !== -1) {
    if ((argv[provenanceIndex + 1] ?? '').trim() === '') fail('--provenance requires a file path')
    const target = resolve(argv[provenanceIndex + 1])
    mkdirSync(dirname(target), { recursive: true })
    writeFileSync(target, canonicalJson(provenance))
  }

  if (check) {
    const problems = diffCatalog(files, outputDir)
    if (problems.length > 0) {
      console.error(`locale-catalog: ${outputDir} is not up to date`)
      for (const problem of problems.slice(0, 20)) console.error(`  - ${problem}`)
      process.exitCode = 1
      return
    }
    console.log(`locale-catalog: ${outputDir} matches revision ${catalogRevision}`)
    return
  }

  writeCatalog(files, outputDir)
  console.log(
    `locale-catalog: revision ${catalogRevision} (manifest sha256 ${manifestSha256})\n` +
      `  ${manifest.totals.locales} locales, ${manifest.totals.chunks} files, ` +
      `${manifest.totals.keys} keys (${manifest.totals.unsupportedKeys} unsupported), ` +
      `${(manifest.totals.bytes / 1024 / 1024).toFixed(2)} MB -> ${relative(REPO_ROOT, outputDir)}`,
  )
}

if (process.argv[1] === fileURLToPath(import.meta.url)) {
  main(process.argv.slice(2)).catch((error) => {
    console.error(error instanceof GenerationError ? error.message : error)
    process.exitCode = 1
  })
}
