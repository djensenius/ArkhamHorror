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
import { dirname, isAbsolute, join, relative, resolve } from 'node:path'
import { fileURLToPath } from 'node:url'

import { canonicalJson, sha256Hex } from './canonical.mjs'
import { analyzeComposition, findDuplicateKeys } from './inventory.mjs'
import {
  ASSET_PATH_VARIABLES,
  KEY_SEGMENT_PATTERN,
  MAX_MESSAGE_KEY_LENGTH,
  UNSUPPORTED_REASONS,
  normalizeMessage,
  referencedVariables,
  staticLinkTargets,
} from './normalize.mjs'
import {
  collectSourceFiles,
  loadLocaleSources,
  loadOwnershipTrees,
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
// `unsupported.detail`'s bound, read from the schema so the generator and the
// schema can never disagree: a long key in a conflict message used to produce
// a chunk its own schema rejected.
export const MAX_UNSUPPORTED_DETAIL = unsupportedDetailBound()
const truncateDetail = (detail) => detail.slice(0, MAX_UNSUPPORTED_DETAIL)

function unsupportedDetailBound() {
  const schema = JSON.parse(
    readFileSync(resolve(REPO_ROOT, 'frontend/schemas/locale-catalog/v1/chunk.schema.json'), 'utf8'),
  )
  const branch = schema.$defs.entry.oneOf.find((candidate) => candidate.properties?.detail)
  const bound = branch?.properties?.detail?.maxLength
  if (typeof bound !== 'number') {
    throw new Error('locale-catalog: the chunk schema does not bound unsupported.detail')
  }
  return bound
}
// A downgrade can expose another conflict; this bounds the settling loop.
const MAX_LINK_ROUNDS = 32
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

// The machine-derived statement of what the backend actually emits, produced
// by scripts/extract-backend-i18n-keys.py from the Haskell sources. Every
// emitted key the default locale translates has to render.
const BACKEND_KEYS = 'backend/arkham-api/i18n-emitted-keys.json'

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
  // `.json` is here for known-gaps.json: the set of holes the catalog carries
  // is part of what the generator does, so editing it must move the revision.
  return readdirSync(HERE, { withFileTypes: true })
    .filter((entry) => entry.isFile() && (entry.name.endsWith('.mjs') || entry.name.endsWith('.json')))
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

/**
 * Fails on any content the sources lose before the catalog ever sees it: a key
 * declared twice inside one JSON file (JSON.parse keeps the last silently), or
 * a contributor whose keys the `.ts` spread composition overrode.
 */
function assertSourceIntegrity(frontendDir, sourceFiles, sources, ownership) {
  const duplicates = []
  for (const { path } of sourceFiles) {
    if (!path.endsWith('.json')) continue
    const text = readFileSync(join(frontendDir, path), 'utf8')
    for (const duplicate of findDuplicateKeys(text, path)) duplicates.push(duplicate)
  }
  if (duplicates.length > 0) {
    const detail = duplicates
      .slice(0, 8)
      .map((d) => `${d.file}:${d.second.line}:${d.second.column} duplicates ${d.key} (first at ${d.first.line}:${d.first.column})`)
      .join('; ')
    fail(`${duplicates.length} duplicate key(s) in locale sources: ${detail}`)
  }

  const files = sources.fileTrees.map(({ path, tree }) => ({
    path: path.replace(/^\//, ''),
    tree,
  }))

  // Ownership is read off a second composition in which every JSON leaf is
  // tagged with the file it came from, so "who won this key" is an answer from
  // the module graph rather than a guess from matching values.
  const findings = []
  for (const locale of sources.locales) {
    const composed = ownership.trees[locale]
    const owned = new Set()
    for (const value of Object.values(composed)) {
      if (value instanceof String && value[ownership.ownerKey]?.startsWith('homebrew/')) owned.add(true)
    }
    const composesHomebrew = JSON.stringify(Object.keys(composed)).length > 0 && localeComposesHomebrew(composed, ownership.ownerKey)
    const contributors = files.filter((file) => {
      if (file.path.startsWith(`src/locales/${locale}/`)) return true
      return composesHomebrew && file.path.startsWith('homebrew/')
    })
    const result = analyzeComposition(composed, contributors, ownership.ownerKey, ownership.moduleKey)
    for (const finding of result.findings) findings.push({ locale, ...finding })
  }

  if (findings.length > 0) {
    const detail = findings
      .slice(0, 6)
      .map(
        (finding) =>
          `${finding.locale}:${finding.file} (${finding.kind}, ${finding.count}) ${finding.detail} — ${finding.examples.join(', ')}`,
      )
      .join('; ')
    fail(`locale composition dropped contributor content: ${detail}`)
  }
}

/** True when this locale's composed tree contains homebrew-owned content. */
function localeComposesHomebrew(composed, ownerKey) {
  const stack = [composed]
  while (stack.length > 0) {
    const node = stack.pop()
    if (node instanceof String) {
      if (node[ownerKey]?.startsWith('homebrew/')) return true
      continue
    }
    if (node === null || typeof node !== 'object') continue
    stack.push(...Object.values(node))
  }
  return false
}

/** The backend's machine-derived emitted-key registry. */
function loadBackendKeys() {
  const bytes = readRepoFile(BACKEND_KEYS)
  const artifact = JSON.parse(bytes.toString('utf8'))
  const keys = new Map()
  for (const entry of artifact.keys) {
    keys.set(entry.key, {
      site: entry.site,
      variables: new Map(entry.variables.map((variable) => [variable.name, variable.type])),
    })
  }
  return { artifact, keys, sha256: sha256Hex(bytes) }
}

function flattenMessages(value, prefix, out) {
  if (typeof value === 'string') {
    if (out.has(prefix)) fail(`duplicate normalized key ${prefix}`)
    // The published schemas bound a key at MAX_MESSAGE_KEY_LENGTH; refuse
    // here rather than emitting a chunk no client can validate.
    if (prefix.length > MAX_MESSAGE_KEY_LENGTH) {
      fail(`message key longer than ${MAX_MESSAGE_KEY_LENGTH} characters: ${prefix.slice(0, 80)}…`)
    }
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
 * Resolves the whole link graph before anything is classified as publishable.
 *
 * A `@:other.key` reference resolves against its own locale first and the
 * fallback locale second, exactly as vue-i18n does, so the graph spans both.
 * Existence alone is not enough: a link into an unsupported entry, a link into
 * nothing, and a link that eventually returns to its own starting point all
 * make the referring entry unrenderable. Each is propagated transitively —
 * with the offending path recorded — so an entry is only published when every
 * chain it starts is resolvable end to end.
 */
function resolveLinkGraph(normalized, defaultLocale) {
  const nodeId = (locale, key) => `${locale}\u0000${key}`
  const entryOf = (locale, key) => {
    const own = normalized.get(locale)?.get(key)
    if (own !== undefined) return { locale, entry: own }
    if (locale !== defaultLocale) {
      const fallback = normalized.get(defaultLocale)?.get(key)
      if (fallback !== undefined) return { locale: defaultLocale, entry: fallback }
    }
    return null
  }

  // 0 = unvisited, 1 = on the current path, 2 = settled.
  const state = new Map()
  const failure = new Map()

  const settle = (locale, key, problem) => {
    failure.set(nodeId(locale, key), problem)
    state.set(nodeId(locale, key), 2)
    return problem
  }

  const visit = (locale, key, path) => {
    const id = nodeId(locale, key)
    const known = state.get(id)
    if (known === 2) return failure.get(id) ?? null
    if (known === 1) {
      return { reason: 'link-cycle', detail: [...path, key].slice(-6).join(' -> ') }
    }

    const resolved = entryOf(locale, key)
    if (resolved === null) return settle(locale, key, { reason: 'unresolved-link', detail: key })
    if (resolved.entry.form === 'unsupported') {
      return settle(locale, key, {
        reason: 'unsupported-link-target',
        detail: `${key} (${resolved.entry.reason})`,
      })
    }

    state.set(id, 1)
    for (const target of staticLinkTargets(resolved.entry)) {
      const problem = visit(resolved.locale, target, [...path, key])
      if (problem !== null) {
        return settle(locale, key, {
          reason: problem.reason === 'link-cycle' ? 'link-cycle' : problem.reason,
          detail: problem.detail,
        })
      }
    }
    state.set(id, 2)
    return null
  }

  let downgraded = 0
  for (const [locale, entries] of normalized) {
    for (const [key, entry] of entries) {
      if (entry.form === 'unsupported') continue
      const problem = visit(locale, key, [])
      if (problem !== null) {
        entries.set(key, {
          form: 'unsupported',
          reason: problem.reason,
          detail: truncateDetail(String(problem.detail)),
        })
        downgraded += 1
      }
    }
  }
  return downgraded
}

/**
 * Publishes the *effective* variable contract of every entry: the variables its
 * own nodes need, plus the variables every entry it links to needs, resolved
 * transitively through the same locale-then-fallback path a client follows.
 *
 * A rendered link is rendered with the parent's variables, so an entry that
 * links to one needing `shelterValue` needs `shelterValue` too — otherwise the
 * client silently renders a hole. A name whose role disagrees across the chain
 * cannot be satisfied at all and makes the entry unsupported.
 */
export function resolveLinkedVariables(normalized, defaultLocale) {
  const entryOf = (locale, key) =>
    normalized.get(locale)?.get(key) ??
    (locale === defaultLocale ? undefined : normalized.get(defaultLocale)?.get(key))

  let conflicts = 0
  for (const [locale, entries] of normalized) {
    // A conflict found deep in the graph makes every ancestor that renders
    // through it unrenderable too, and downgrading an entry can expose a
    // conflict in its own parents. So this runs to a fixed point rather than
    // in one pass: keep resolving until a round changes nothing.
    for (let round = 0; ; round += 1) {
      if (round > MAX_LINK_ROUNDS) fail(`link resolution did not settle for ${locale}`)
      const cache = new Map()

      const collect = (key, seen) => {
        const cached = cache.get(key)
        if (cached !== undefined) return cached
        if (seen.has(key)) return { variables: new Map(), broken: null }
        seen.add(key)

        const entry = entryOf(locale, key)
        if (entry === undefined) return { variables: new Map(), broken: `${key} is missing` }
        if (entry.form === 'unsupported') {
          return { variables: new Map(), broken: `${key} is ${entry.reason}` }
        }

        const variables = new Map()
        for (const variable of entry.variables) {
          variables.set(`${variable.source}:${variable.name}`, variable)
        }
        let broken = null
        for (const target of staticLinkTargets(entry)) {
          const descendant = collect(target, seen)
          if (descendant.broken !== null) {
            broken ??= descendant.broken
            continue
          }
          for (const [id, variable] of descendant.variables) {
            const existing = variables.get(id)
            if (existing !== undefined && existing.role !== variable.role) {
              broken ??= `${id} is ${existing.role} in ${key} and ${variable.role} in ${target}`
              continue
            }
            if (!variables.has(id)) variables.set(id, variable)
          }
        }
        const answer = { variables, broken }
        cache.set(key, answer)
        return answer
      }

      let changed = false
      for (const [key, entry] of entries) {
        if (entry.form === 'unsupported') continue
        const targets = staticLinkTargets(entry)
        if (targets.length === 0) continue

        const own = new Map(
          entry.variables.map((variable) => [`${variable.source}:${variable.name}`, variable]),
        )
        const linked = new Map()
        let conflict = null
        for (const target of targets) {
          const descendant = collect(target, new Set([key]))
          if (descendant.broken !== null) {
            conflict ??= descendant.broken
            continue
          }
          for (const [id, variable] of descendant.variables) {
            const existing = own.get(id) ?? linked.get(id)
            if (existing !== undefined && existing.role !== variable.role) {
              conflict ??= `${id} is ${existing.role} here and ${variable.role} in ${target}`
              continue
            }
            if (!own.has(id)) linked.set(id, variable)
          }
        }

        if (conflict !== null) {
          entries.set(key, {
            form: 'unsupported',
            reason: 'conflicting-variable-role',
            detail: truncateDetail(conflict),
          })
          conflicts += 1
          changed = true
          continue
        }

        const resolved =
          linked.size > 0
            ? [...linked.values()].sort(
                (a, b) => a.source.localeCompare(b.source) || a.name.localeCompare(b.name),
              )
            : undefined
        if (canonicalJson(resolved ?? null) !== canonicalJson(entry.linkedVariables ?? null)) {
          if (resolved === undefined) delete entry.linkedVariables
          else entry.linkedVariables = resolved
          changed = true
        }
      }

      if (!changed) break
    }
  }
  return conflicts
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
export async function buildCatalog({ frontendDir = FRONTEND_DIR, ...options } = {}) {
  const sources = await loadLocaleSources(frontendDir)
  const classifyVariable = makeVariableClassifier(sources, ASSET_PATH_VARIABLES)

  const fixtureKeys = collectRequiredKeys()
  const backend = loadBackendKeys()
  const provenance = {
    schemaVersion: SCHEMA_VERSION,
    generator: { name: GENERATOR_NAME, version: GENERATOR_VERSION, sources: generatorSourceDigests() },
    schemas: digestRepoFiles(SCHEMA_FILES),
    toolchain: toolchainVersions(frontendDir),
    contract: {
      schemaRevision: JSON.parse(readRepoFile(CONTRACT_MANIFEST).toString('utf8')).schemaRevision,
      fixtures: digestRepoFiles(REQUIRED_KEY_FIXTURES),
      fixtureKeys,
    },
    backend: {
      path: BACKEND_KEYS,
      sha256: backend.sha256,
      artifactVersion: backend.artifact.artifactVersion,
      sourceSha256: backend.artifact.source.sha256,
      emittedKeys: backend.keys.size,
    },
    localeSources: collectSourceFiles(frontendDir),
    // The exact resolved dependency closure, integrity hashes included.
    lockfile: digestRepoFiles(['frontend/package-lock.json']),
  }
  const defaultLocale = sources.uiLocaleFor('')
  if (!sources.locales.includes(defaultLocale)) fail(`default locale ${defaultLocale} is not supported`)

  const ownership = await loadOwnershipTrees(frontendDir)
  assertSourceIntegrity(frontendDir, provenance.localeSources, sources, ownership)

  const normalized = new Map()
  for (const locale of sources.locales) {
    if (!LOCALE_PATTERN.test(locale)) fail(`unsafe locale id ${JSON.stringify(locale)}`)
    normalized.set(locale, normalizeLocale(sources.messages[locale], classifyVariable))
  }
  // A downgrade can invalidate an entry that linked to it, so the graph is
  // re-resolved until it stops changing.
  for (let pass = 0; pass < 8; pass += 1) {
    if (resolveLinkGraph(normalized, defaultLocale) === 0) break
    if (pass === 7) fail('link graph did not stabilize')
  }

  // A link is rendered with the parent's variables, so the parent's contract
  // includes everything its targets need.
  resolveLinkedVariables(normalized, defaultLocale)

  // Required = the keys the governed contract fixtures reference, plus every
  // key the backend actually emits that the default locale translates. The
  // fixtures must resolve outright; an emitted key with no translation at all
  // is a content gap in the locale sources (reported, and gated against
  // growth by the validator), but an emitted key that *is* translated must
  // render.
  const defaultEntries = normalized.get(defaultLocale)
  const untranslated = []
  const requiredKeys = new Set(fixtureKeys)
  for (const key of backend.keys.keys()) {
    if (defaultEntries.has(key)) requiredKeys.add(key)
    else untranslated.push(key)
  }
  untranslated.sort()

  for (const key of fixtureKeys) {
    if (!defaultEntries.has(key)) fail(`contract fixture key ${key} is missing from ${defaultLocale}`)
  }

  // Every offending key is collected before failing: fixing these one build at
  // a time, with 6,000 required keys, is not a workflow anyone should have.
  const unusable = []
  for (const key of [...requiredKeys].sort()) {
    const fallbackEntry = defaultEntries.get(key)
    if (fallbackEntry === undefined) {
      unusable.push(`${key} is missing from ${defaultLocale}`)
      continue
    }
    if (fallbackEntry.form === 'unsupported') {
      unusable.push(`${key} is unsupported in ${defaultLocale} (${fallbackEntry.reason}: ${fallbackEntry.detail})`)
    }
    for (const [locale, entries] of normalized) {
      const entry = entries.get(key)
      if (entry !== undefined && entry.form === 'unsupported') {
        unusable.push(`${key} is unsupported in ${locale} (${entry.reason}: ${entry.detail})`)
      }
    }
  }
  if (unusable.length > 0) {
    fail(
      `${unusable.length} backend-emitted key(s) cannot be published; they are emitted by the ` +
        `backend, so they cannot be optional:\n  ${unusable.join('\n  ')}`,
    )
  }

  // Variable coverage. Two questions, not one: does the backend send this
  // name at all, and does it send something the message can render?
  //
  // A name is not enough. The catalog declares a *role* for every variable and
  // the registry proves a *type* for most of them; where the registry could
  // not prove one, "unknown" is not a pass — a client cannot be told an entry
  // is supported when nobody knows what will arrive in its slot. Those entries
  // are published as `unsupported` and listed in the manifest, so a consumer
  // sees the hole in the schema rather than discovering it at the table.
  const ROLE_ACCEPTS = { text: new Set(['text', 'integer']) }
  const variableGaps = []
  const unknownVariableTypes = []
  for (const key of [...requiredKeys].sort()) {
    const record = backend.keys.get(key)
    if (record === undefined) continue
    const entry = defaultEntries.get(key)
    if (entry.form === 'unsupported') continue

    // Only a `text` slot is filled by the backend: an `icon` name is rendered
    // by the client from its own icon set, and a `presentation` name only
    // styles.
    const needed = [...entry.variables, ...(entry.linkedVariables ?? [])].filter(
      (variable) => variable.source === 'named' && variable.role === 'text',
    )
    const missing = needed
      .filter((variable) => !record.variables.has(variable.name))
      .map((variable) => variable.name)
    if (missing.length > 0) {
      variableGaps.push({
        key,
        missing,
        declared: [...record.variables.keys()].sort(),
        resolved: record.variables.size > 0,
      })
    }

    const unusable = needed
      .filter((variable) => record.variables.has(variable.name))
      .map((variable) => ({ variable, type: record.variables.get(variable.name) }))
      .filter(({ variable, type }) => !(ROLE_ACCEPTS[variable.role] ?? new Set()).has(type))
    if (unusable.length === 0) continue

    for (const { variable, type } of unusable) {
      unknownVariableTypes.push({ key, variable: variable.name, role: variable.role, type })
    }
    // Every locale, not just the default: the entry is unrenderable because of
    // what the backend sends, which does not vary by language.
    const detail = unusable
      .map(({ variable, type }) => `${variable.name} is ${type} for a ${variable.role} slot`)
      .join('; ')
    for (const entries of normalized.values()) {
      const localeEntry = entries.get(key)
      if (localeEntry === undefined || localeEntry.form === 'unsupported') continue
      entries.set(key, {
        form: 'unsupported',
        reason: 'unusable-variable-type',
        detail: truncateDetail(detail),
      })
    }
  }
  unknownVariableTypes.sort((a, b) => (a.key === b.key ? (a.variable < b.variable ? -1 : 1) : a.key < b.key ? -1 : 1))

  // Every entry the catalog cannot publish, taken after the variable pass so a
  // downgrade it made is visible here too.
  const unsupportedEntries = []
  for (const [locale, entries] of normalized) {
    for (const [key, entry] of entries) {
      if (entry.form === 'unsupported') unsupportedEntries.push({ locale, key, reason: entry.reason })
    }
  }
  unsupportedEntries.sort((a, b) =>
    a.locale === b.locale ? (a.key < b.key ? -1 : 1) : a.locale < b.locale ? -1 : 1,
  )

  const gaps = {
    sites: new Map(untranslated.map((key) => [key, backend.keys.get(key)?.site])),
    untranslatedKeys: untranslated,
    unsupportedEntries,
    variableGaps: variableGaps.map(({ key, missing }) => ({ key, missing })),
    unknownVariableTypes,
  }
  enforceKnownGaps(gaps, options.updateKnownGaps === true)

  // Phase 1: the catalog's content, with no revision and no URLs in it yet.
  const bodies = []
  for (const locale of [...sources.locales].sort()) {
    const entries = normalized.get(locale)
    const fallback = locale === defaultLocale ? null : defaultLocale
    const packs = chunkEntries(entries)
    if (packs.size > MAX_CHUNKS_PER_LOCALE) fail(`${locale} produced ${packs.size} chunks`)
    for (const pack of [...packs.keys()].sort()) {
      bodies.push({
        locale,
        fallback,
        pack,
        entries: Object.fromEntries(
          [...packs.get(pack).entries()].sort(([a], [b]) => (a < b ? -1 : 1)),
        ),
      })
    }
  }

  // Phase 2: bind the revision to *both* the inputs and the exact content they
  // produced, so a change in either — a locale byte, the generator, a
  // dependency, the Node major, or the rendered output itself — is a new
  // revision, and identical inputs always reproduce the same one.
  const outputSha256 = sha256Hex(Buffer.from(canonicalJson({ bodies }), 'utf8'))
  const revisionInput = { provenance, output: { sha256: outputSha256, chunks: bodies.length } }
  const provenanceSha256 = sha256Hex(Buffer.from(canonicalJson(revisionInput), 'utf8'))
  const catalogRevision = `1.${provenanceSha256.slice(0, 32)}`
  const revisionPrefix = `${BASE_PATH}/r/${catalogRevision}`

  // Phase 3: materialize the addressed files now that the revision is known.
  const files = new Map()
  const locales = []
  let totalBytes = 0
  let totalKeys = 0
  let totalUnsupported = 0

  for (const locale of [...sources.locales].sort()) {
    const localeBodies = bodies.filter((body) => body.locale === locale)
    const fallback = locale === defaultLocale ? null : defaultLocale
    const entries = normalized.get(locale)

    const chunks = []
    let localeBytes = 0
    for (const body of localeBodies) {
      const pack = body.pack
      const packEntries = new Map(Object.entries(body.entries))
      // A chunk deliberately does not name its revision: its bytes depend only
      // on its content, so a pack that did not change keeps the same URL from
      // one revision to the next. During a rolling deploy an old replica can
      // therefore still serve most of what a new manifest points at.
      const chunk = {
        schemaVersion: SCHEMA_VERSION,
        locale,
        fallback,
        pack,
        entries: body.entries,
      }
      const bytes = Buffer.from(canonicalJson(chunk), 'utf8')
      const sha256 = sha256Hex(bytes)
      if (bytes.length > MAX_CHUNK_BYTES) fail(`${locale}/${pack} chunk is ${bytes.length} bytes`)

      const filePath = `c/${sha256}.json`
      const existing = files.get(filePath)
      if (existing !== undefined && !existing.equals(bytes)) fail(`chunk digest collision at ${filePath}`)
      files.set(filePath, bytes)

      const unsupportedKeys = [...packEntries.values()].filter((entry) => entry.form === 'unsupported').length
      chunks.push({
        pack,
        path: `${BASE_PATH}/${filePath}`,
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
    chunkPathPrefix: `${BASE_PATH}/c/`,
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
    backend: {
      artifactPath: BACKEND_KEYS,
      artifactSha256: backend.sha256,
      sourceSha256: backend.artifact.source.sha256,
      emittedKeys: backend.keys.size,
      requiredKeys: requiredKeys.size,
      untranslatedKeys: untranslated,
      unknownVariableTypes,
      variableGaps: variableGaps.map((gap) => ({
        key: gap.key,
        missing: gap.missing,
        resolved: gap.resolved,
      })),
      dynamicSites: backend.artifact.dynamicSites.total,
    },
    provenance: {
      sha256: provenanceSha256,
      outputSha256,
      generator: { name: GENERATOR_NAME, version: GENERATOR_VERSION },
      contractRevision: provenance.contract.schemaRevision,
      fixtureKeys,
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

  return {
    manifest,
    manifestSha256: sha256Hex(manifestBytes),
    files,
    catalogRevision,
    provenance: revisionInput,
  }
}

export function writeCatalog(files, outputDir) {
  assertReplaceableOutputDir(outputDir)
  rmSync(outputDir, { recursive: true, force: true })
  for (const [path, bytes] of files) {
    const target = join(outputDir, path)
    const inside = relative(resolve(outputDir), resolve(target))
    if (inside === '' || inside.startsWith('..') || isAbsolute(inside)) {
      fail(`refusing to write outside ${outputDir}`)
    }
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
  const fromTarget = relative(target, repoRoot)
  if (!fromTarget.startsWith('..') && !isAbsolute(fromTarget)) {
    fail(`refusing to replace ${outputDir}: it contains the repository`)
  }
  if (!existsSync(target)) return
  if (!statSync(target).isDirectory()) fail(`${outputDir} is not a directory`)
  for (const entry of readdirSync(target)) {
    if (entry !== 'manifest.json' && entry !== 'r' && entry !== 'c') {
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

/**
 * Holes the catalog is knowingly carrying, pinned so they cannot grow.
 *
 * Three kinds exist, and none of them can be fixed by this generator: a key the
 * backend emits that the default locale never translated, an entry whose markup
 * this AST refuses (and that no required key needs), and a key whose text wants
 * a variable the backend was not seen to send. Inventing prose to close the
 * first is not an option, so instead the exact set is committed. A new hole
 * fails the build; a hole that has been fixed also fails, so the list can only
 * be shrunk deliberately.
 */
const JUSTIFICATIONS = Object.freeze({
  'missing-from-locale-sources':
    'the backend emits this key but no locale file defines it, so there is no text to publish; ' +
    'the Vue client renders the raw key today for the same reason. Writing the missing prose is ' +
    'a content change, not a build change.',
  'web-only-chrome':
    'the entry belongs to the web UI rather than gameplay (the About page), is never emitted by ' +
    'the backend, and carries markup — links, tables of contents — that the render AST does not ' +
    'model.',
  'unusable-variable-type':
    'the message needs a substitution the backend does not send in a form the slot can render, ' +
    'so the entry is published as `unsupported` and a consumer must treat it as unavailable. It is ' +
    'listed in the manifest under `backend.unknownVariableTypes`.',
  'source-syntax':
    'the source string is not valid vue-i18n message syntax, so neither the catalog nor the Vue ' +
    'client can render it; it is not backend-emitted.',
})

function enforceKnownGaps(actual, update) {
  const path = resolve(REPO_ROOT, 'frontend/scripts/locale-catalog/known-gaps.json')
  const previous = existsSync(path) ? JSON.parse(readFileSync(path, 'utf8')) : {}
  const rendered = {
    $comment:
      'Known, reviewed holes in the locale catalog. Every entry needs a justification from ' +
      'JUSTIFICATIONS. Regenerate deliberately with ' +
      '`node scripts/locale-catalog/generate.mjs --update-known-gaps`; every change must be reviewed.',
    justifications: JUSTIFICATIONS,
    unknownVariableTypes: actual.unknownVariableTypes,
    unsupportedEntries: actual.unsupportedEntries.map((entry) => ({
      ...entry,
      justification:
        previous.unsupportedEntries?.find(
          (candidate) => candidate.key === entry.key && candidate.locale === entry.locale,
        )?.justification ??
        (entry.reason === 'unusable-variable-type' ? 'unusable-variable-type' : undefined),
    })),
    untranslatedKeys: actual.untranslatedKeys.map((key) => ({
      key,
      // The emitter, so the list is an actionable blocker report rather than a
      // set of names: this is the call site whose text is missing.
      site: actual.sites?.get(key),
      justification:
        (Array.isArray(previous.untranslatedKeys)
          ? previous.untranslatedKeys.find((candidate) => candidate?.key === key)?.justification
          : undefined) ?? 'missing-from-locale-sources',
    })),
    variableGaps: actual.variableGaps,
  }
  if (update) {
    writeFileSync(path, canonicalJson(rendered))
    return
  }
  if (!existsSync(path)) fail(`${path} is missing; the catalog will not publish unreviewed gaps`)

  const expected = previous
  const unjustified = [
    ...(expected.unsupportedEntries ?? []),
    ...(expected.untranslatedKeys ?? []),
  ].filter((entry) => !Object.hasOwn(JUSTIFICATIONS, entry?.justification))
  if (unjustified.length > 0) {
    fail(
      `known-gaps.json carries ${unjustified.length} entry/entries without a justification from ` +
        `${Object.keys(JUSTIFICATIONS).join(', ')}: ${unjustified
          .map((entry) => entry.key)
          .slice(0, 10)
          .join(', ')}`,
    )
  }
  const differences = []
  const compare = (label, actualList, expectedList) => {
    const actualSet = new Set(actualList.map((entry) => canonicalJson(entry)))
    const expectedSet = new Set((expectedList ?? []).map((entry) => canonicalJson(entry)))
    for (const entry of actualSet) {
      if (!expectedSet.has(entry)) differences.push(`new ${label}: ${entry.trim()}`)
    }
    for (const entry of expectedSet) {
      if (!actualSet.has(entry)) differences.push(`fixed ${label} still listed: ${entry.trim()}`)
    }
  }
  compare('unsupported entry', rendered.unsupportedEntries, expected.unsupportedEntries)
  compare('untranslated key', rendered.untranslatedKeys, expected.untranslatedKeys)
  compare('variable gap', rendered.variableGaps, expected.variableGaps)
  compare('unusable variable type', rendered.unknownVariableTypes, expected.unknownVariableTypes)

  if (differences.length > 0) {
    fail(
      `known-gaps.json is out of date (${differences.length} difference(s)); review each one and ` +
        `re-run with --update-known-gaps:\n  ${differences.slice(0, 40).join('\n  ')}`,
    )
  }
}

async function main(argv) {
  const check = argv.includes('--check')
  const outIndex = argv.indexOf('--out')
  if (outIndex !== -1 && (argv[outIndex + 1] ?? '').trim() === '') fail('--out requires a directory')
  const outputDir = outIndex === -1 ? DEFAULT_OUTPUT_DIR : resolve(argv[outIndex + 1])
  const provenanceIndex = argv.indexOf('--provenance')

  const { manifest, manifestSha256, files, catalogRevision, provenance } = await buildCatalog({
    updateKnownGaps: argv.includes('--update-known-gaps'),
  })

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
      `${manifest.backend.requiredKeys} backend-required, ` +
      `${(manifest.totals.bytes / 1024 / 1024).toFixed(2)} MB -> ${relative(REPO_ROOT, outputDir)}`,
  )
}

if (process.argv[1] === fileURLToPath(import.meta.url)) {
  main(process.argv.slice(2)).catch((error) => {
    console.error(error instanceof GenerationError ? error.message : error)
    process.exitCode = 1
  })
}
