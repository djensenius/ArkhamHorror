// End-to-end tests for the production locale-catalog generator.
//
// These run the same `buildCatalog()` the frontend build runs, against the
// committed `src/locales/**` snapshot. No scenario prose is duplicated here:
// the required keys come from the governed contract fixtures, and the expected
// content is checked structurally or by digest.

import assert from 'node:assert/strict'
import { mkdirSync, readFileSync, rmSync, writeFileSync } from 'node:fs'
import { join, resolve } from 'node:path'
import test from 'node:test'

import {
  BASE_PATH,
  assertReplaceableOutputDir,
  buildCatalog,
  diffCatalog,
  MAX_UNSUPPORTED_DETAIL,
  requiredKeysFromFixture,
  resolveLinkedVariables,
  resolveVariablesAndLinks,
  writeCatalog,
} from '../scripts/locale-catalog/generate.mjs'
import { sha256Hex } from '../scripts/locale-catalog/canonical.mjs'

const REPO_ROOT = resolve('..')
const SCRATCH = resolve('node_modules', '.locale-catalog-test')

const NODE_TYPES = new Set([
  'text',
  'var',
  'linked',
  'table',
  'break',
  'rule',
  'paragraph',
  'heading',
  'group',
  'cardRef',
  'emphasis',
  'list',
  'image',
])

const built = await buildCatalog({})

function eachEntry(files, visit) {
  for (const [path, bytes] of files) {
    if (path.endsWith('manifest.json')) continue
    const chunk = JSON.parse(bytes.toString('utf8'))
    for (const [key, entry] of Object.entries(chunk.entries)) visit(chunk, key, entry)
  }
}

function walkNodes(nodes, visit) {
  for (const node of nodes) {
    visit(node)
    if (node.type === 'list') for (const item of node.items) walkNodes(item.children, visit)
    else if (node.type === 'table') {
      for (const row of [...node.head, ...node.body]) {
        for (const cell of row.cells) walkNodes(cell.children, visit)
      }
    } else if (Array.isArray(node.children)) walkNodes(node.children, visit)
  }
}

function entryNodes(entry) {
  if (entry.form === 'message') return [entry.nodes]
  if (entry.form === 'plural') return entry.cases
  return []
}

function chunkFor(files, manifest, locale, key) {
  const pack = key.includes('.') ? key.slice(0, key.indexOf('.')) : 'core'
  const descriptor = manifest.locales
    .find((entry) => entry.locale === locale)
    ?.chunks.find((chunk) => chunk.pack === pack)
  if (!descriptor) return undefined
  const bytes = files.get(descriptor.path.slice(`${BASE_PATH}/`.length))
  return JSON.parse(bytes.toString('utf8'))
}

const backendRegistry = JSON.parse(
  readFileSync(join(REPO_ROOT, 'backend/arkham-api/i18n-emitted-keys.json'), 'utf8'),
)

test('a repeated build is byte-for-byte identical', async () => {
  const second = await buildCatalog({})
  assert.equal(second.catalogRevision, built.catalogRevision)
  assert.equal(second.manifestSha256, built.manifestSha256)
  assert.deepEqual([...second.files.keys()].sort(), [...built.files.keys()].sort())
  for (const [path, bytes] of second.files) {
    assert.ok(built.files.get(path).equals(bytes), `${path} differs between builds`)
  }
})

test('every supported UI locale is published with an explicit fallback', () => {
  const language = readFileSync(resolve('src/locales/language.ts'), 'utf8')
  const declared = [...language.matchAll(/'([a-z-]+)'/g)]
    .map((match) => match[1])
    .filter((locale, index, all) => all.indexOf(locale) === index)

  const published = built.manifest.locales.map((entry) => entry.locale)
  for (const locale of published) assert.ok(declared.includes(locale), `${locale} is not a declared UI locale`)
  assert.ok(published.includes(built.manifest.defaultLocale))

  for (const entry of built.manifest.locales) {
    assert.equal(entry.fallback, entry.locale === built.manifest.defaultLocale ? null : 'en')
    assert.ok(entry.chunks.length > 0)
  }
  const resolution = new Map(built.manifest.languageResolution.map((row) => [row.tag, row.locale]))
  for (const tag of ['zh', 'zh-CN', 'zh-Hans', 'zh-Hant', 'zh-TW']) {
    assert.equal(resolution.get(tag), 'zh', `${tag} resolves to zh`)
  }
  assert.equal(resolution.get('fr-CA'), 'fr')
  assert.equal(resolution.get('ru'), 'en', 'an unsupported language falls back to the default locale')
})

test('the contract fixtures\u2019 required keys all resolve', () => {
  const required = new Set()
  for (const fixture of ['question-read.json', 'question-read-with-cards.json']) {
    requiredKeysFromFixture(
      JSON.parse(readFileSync(join(REPO_ROOT, 'contracts/fixtures', fixture), 'utf8')),
      required,
    )
  }
  assert.ok(required.size >= 6)
  assert.deepEqual([...required].sort(), [...built.manifest.provenance.fixtureKeys].sort())

  for (const key of required) {
    const entry = chunkFor(built.files, built.manifest, 'en', key)?.entries[key]
    assert.ok(entry, `${key} is missing from en`)
    assert.notEqual(entry.form, 'unsupported')
  }

  for (const locale of built.manifest.locales.map((entry) => entry.locale)) {
    for (const key of required) {
      const entry = chunkFor(built.files, built.manifest, locale, key)?.entries[key]
      if (entry) assert.notEqual(entry.form, 'unsupported', `${locale}/${key}`)
    }
  }
})

test('every backend-emitted key the default locale translates renders', () => {
  const emitted = backendRegistry.keys.map((entry) => entry.key)
  assert.ok(emitted.length > 1000, 'the backend registry looks empty')

  const entries = new Map()
  for (const locale of built.manifest.locales) {
    if (locale.locale !== built.manifest.defaultLocale) continue
    for (const chunk of locale.chunks) {
      const parsed = JSON.parse(built.files.get(chunk.path.slice(`${BASE_PATH}/`.length)).toString('utf8'))
      for (const [key, entry] of Object.entries(parsed.entries)) entries.set(key, entry)
    }
  }

  const translated = emitted.filter((key) => entries.has(key))
  // The one exception is deliberate and visible: an entry whose slot the
  // backend cannot fill is published as unsupported *and* listed in the
  // manifest, so a consumer sees the hole rather than a broken instruction.
  const unusable = new Set(built.manifest.backend.unknownVariableTypes.map((entry) => entry.key))
  const unsupported = translated.filter(
    (key) => entries.get(key).form === 'unsupported' && !unusable.has(key),
  )
  assert.deepEqual(unsupported, [], 'backend-emitted keys may never be unsupported')
  for (const key of unusable) {
    assert.equal(entries.get(key)?.reason, 'unusable-variable-type', `${key} reason`)
  }
  assert.equal(built.manifest.backend.emittedKeys, emitted.length)
  assert.deepEqual(
    built.manifest.backend.untranslatedKeys,
    emitted.filter((key) => !entries.has(key)).sort(),
  )

  // The two keys the independent review cited as silently optional: both are
  // emitted by production Haskell and both must now render.
  for (const key of [
    'childrenOfBlood.additionalRulesAndClarifications.bloodTokens',
    'edgeOfTheEarth.checkpoint2.theAttack3.body',
  ]) {
    assert.ok(emitted.includes(key), `${key} should be in the backend registry`)
    const entry = entries.get(key)
    assert.ok(entry, `${key} should be published`)
    assert.notEqual(entry.form, 'unsupported', `${key} must render`)
  }

  const gathering = entries.get('childrenOfBlood.additionalRulesAndClarifications.bloodTokens')
  const kinds = new Set()
  walkNodes(gathering.nodes, (node) => kinds.add(node.type))
  assert.ok(kinds.has('image'), 'the blood-token entry keeps its token image reference')
})

test('rich fixture content keeps its instructions, images, and emphasis', () => {
  const key = 'nightOfTheZealot.theGathering.setup.gatherSets'
  const entry = chunkFor(built.files, built.manifest, 'en', key).entries[key]
  const types = []
  const images = []
  walkNodes(entry.nodes, (node) => {
    types.push(node.type)
    if (node.type === 'image') images.push(node)
  })

  assert.ok(types.includes('emphasis'), 'encounter set names stay emphasized')
  assert.ok(types.includes('group'), 'the encounter-set section survives as a group')
  assert.equal(images.length, 6)
  for (const image of images) {
    assert.equal(image.role, 'encounterSet')
    assert.match(image.assetPath, /^encounter-sets\/[a-z0-9-]+\.png$/)
    assert.ok(!('src' in image) && !('url' in image), 'images are semantic references only')
  }
  // The catalog stores instructions, not markup: every text node is plain text.
  walkNodes(entry.nodes, (node) => {
    if (node.type === 'text') assert.ok(!/[<>]/.test(node.value), 'text nodes carry no markup')
  })
})

test('the published AST is closed, safe, and fully declared', () => {
  const reasons = new Map()
  let unsupported = 0
  let variables = 0

  eachEntry(built.files, (chunk, key, entry) => {
    if (entry.form === 'unsupported') {
      unsupported += 1
      reasons.set(entry.reason, (reasons.get(entry.reason) ?? 0) + 1)
      assert.ok(!('nodes' in entry) && !('cases' in entry), `${key} leaks partial content`)
      return
    }

    const declared = new Set(entry.variables.map((variable) => `${variable.source}:${variable.name}`))
    for (const nodes of entryNodes(entry)) {
      walkNodes(nodes, (node) => {
        assert.ok(NODE_TYPES.has(node.type), `${key} has node type ${node.type}`)
        if (node.type === 'var') {
          assert.ok(declared.has(`${node.source}:${node.name}`), `${key} uses undeclared ${node.name}`)
          variables += 1
        }
        if (node.type === 'linked' && node.target.kind === 'variable') {
          assert.ok(declared.has(`${node.target.source}:${node.target.name}`))
        }
        if (node.type === 'image') {
          assert.ok(!node.assetPath.includes('..'), `${key} image escapes the asset root`)
          assert.ok(!/^[a-z][a-z0-9+.-]*:/i.test(node.assetPath), `${key} image carries a scheme`)
        }
        if (node.type === 'text') {
          assert.ok(!node.value.includes('\uE000'), `${key} leaks a sentinel`)
        }
      })
    }
  })

  assert.ok(variables > 1000, 'typed variables are preserved across the catalog')
  assert.equal(unsupported, built.manifest.totals.unsupportedKeys)
  // Explicitly marked, and a vanishingly small share of the catalog.
  assert.ok(unsupported / built.manifest.totals.keys < 0.01, `${unsupported} unsupported entries`)
  for (const reason of reasons.keys()) {
    assert.ok(
      [
        'message-syntax-error',
        'unusable-variable-type',
        'unsupported-message-syntax',
        'html-parse-error',
        'unsupported-element',
        'unsupported-attribute',
        'placeholder-in-attribute',
        'asset-variable-outside-image',
        'unsupported-image-source',
        'image-path-escape',
        'invalid-style-token',
        'misplaced-list-item',
        'unresolved-link',
        'conflicting-variable-role',
        'invalid-style-declaration',
        'unsupported-link-target',
        'link-cycle',
      ].includes(reason),
      reason,
    )
  }
})

test('static links resolve inside the catalog', () => {
  const byLocale = new Map()
  eachEntry(built.files, (chunk, key) => {
    if (!byLocale.has(chunk.locale)) byLocale.set(chunk.locale, new Set())
    byLocale.get(chunk.locale).add(key)
  })

  eachEntry(built.files, (chunk, key, entry) => {
    for (const nodes of entryNodes(entry)) {
      walkNodes(nodes, (node) => {
        if (node.type !== 'linked' || node.target.kind !== 'static') return
        const own = byLocale.get(chunk.locale).has(node.target.key)
        const fallback = chunk.fallback !== null && byLocale.get(chunk.fallback).has(node.target.key)
        assert.ok(own || fallback, `${chunk.locale}/${key} links to missing ${node.target.key}`)
      })
    }
  })
})

test('the manifest pins every chunk by size and digest', () => {
  const seen = new Set()
  let chunks = 0

  for (const locale of built.manifest.locales) {
    for (const chunk of locale.chunks) {
      chunks += 1
      // Content-addressed, not revision-addressed: an unchanged pack keeps its
      // URL across revisions so a rolling deploy can still serve it.
      assert.equal(chunk.path, `${BASE_PATH}/c/${chunk.sha256}.json`)
      assert.ok(!seen.has(chunk.path), `duplicate chunk path ${chunk.path}`)
      seen.add(chunk.path)

      const bytes = built.files.get(chunk.path.slice(`${BASE_PATH}/`.length))
      assert.ok(bytes, `${chunk.path} was not generated`)
      assert.equal(bytes.length, chunk.bytes)
      assert.equal(sha256Hex(bytes), chunk.sha256)
      assert.ok(chunk.bytes <= 8 * 1024 * 1024)

      const parsed = JSON.parse(bytes.toString('utf8'))
      assert.ok(!('catalogRevision' in parsed), 'a chunk must not name its revision')
      assert.equal(parsed.locale, locale.locale)
      assert.equal(parsed.fallback, locale.fallback)
      assert.equal(Object.keys(parsed.entries).length, chunk.keys)
    }
    assert.ok(locale.chunks.length <= 256, 'chunk count per locale stays bounded')
  }

  assert.equal(chunks, built.manifest.totals.chunks)
  // The manifest is published twice, byte-identically: one stable URL a
  // capabilities response can point at, one immutable revision URL.
  const stable = built.files.get('manifest.json')
  const immutable = built.files.get(`r/${built.manifest.catalogRevision}/manifest.json`)
  assert.ok(stable.equals(immutable))
  assert.equal(sha256Hex(stable), built.manifestSha256)
  assert.equal(built.manifest.basePath, BASE_PATH)
  assert.equal(built.manifest.manifestPath, `${BASE_PATH}/manifest.json`)
  assert.equal(built.manifest.revisionManifestPath, `${BASE_PATH}/r/${built.manifest.catalogRevision}/manifest.json`)
  assert.equal(built.manifest.catalogRevision, `1.${built.manifest.provenance.sha256.slice(0, 32)}`)
})

test('the writer refuses any output directory it did not produce', () => {
  // Checked without calling writeCatalog(), which deletes its target: the
  // point of the guard is that a mistyped --out can never reach that rmSync.
  for (const directory of [REPO_ROOT, resolve('.'), resolve('src'), resolve('..', '..')]) {
    assert.throws(
      () => assertReplaceableOutputDir(directory),
      /refusing to replace|is not a directory/,
      `${directory} must be refused`,
    )
  }
  assert.doesNotThrow(() => assertReplaceableOutputDir(join(SCRATCH, 'does-not-exist')))
})

test('written output is verifiable and stale output is detected', () => {
  const outputDir = join(SCRATCH, 'catalog')
  rmSync(SCRATCH, { recursive: true, force: true })
  writeCatalog(built.files, outputDir)
  assert.deepEqual(diffCatalog(built.files, outputDir), [])

  const [firstChunk] = [...built.files.keys()].filter((path) => !path.endsWith('manifest.json'))
  const chunkPath = join(outputDir, firstChunk)
  const original = readFileSync(chunkPath)
  writeFileSync(chunkPath, Buffer.concat([original, Buffer.from(' ')]))
  assert.deepEqual(diffCatalog(built.files, outputDir), [`stale generated file ${firstChunk}`])

  rmSync(chunkPath)
  assert.deepEqual(diffCatalog(built.files, outputDir), [`missing generated file ${firstChunk}`])

  writeFileSync(chunkPath, original)
  mkdirSync(join(outputDir, 'r', built.manifest.catalogRevision, 'zz'), { recursive: true })
  writeFileSync(join(outputDir, 'r', built.manifest.catalogRevision, 'zz', 'extra.json'), '{}')
  assert.deepEqual(diffCatalog(built.files, outputDir), [
    `unexpected file r/${built.manifest.catalogRevision}/zz/extra.json`,
  ])

  rmSync(SCRATCH, { recursive: true, force: true })
})

// --- Linked-message variable contracts -------------------------------------
// A message that links another one renders the target's text too, so the
// variables the target needs are part of the parent's effective contract.

const linkEntry = (target, variables = []) => ({
  form: 'message',
  nodes: [{ type: 'linked', modifier: null, target: { kind: 'static', key: target } }],
  variables,
})

const textEntry = (variables) => ({
  form: 'message',
  nodes: variables.map((variable) => ({ type: 'var', ...variable })),
  variables,
})

test('a linked chain publishes every variable its descendants need', () => {
  const shelter = { name: 'shelterValue', source: 'named', role: 'text' }
  const xp = { name: 'xp', source: 'named', role: 'text' }
  const entries = new Map([
    ['parent', linkEntry('middle')],
    ['middle', linkEntry('leaf', [xp])],
    ['leaf', textEntry([shelter])],
  ])

  resolveLinkedVariables(new Map([['en', entries]]), 'en')

  assert.deepEqual(
    entries.get('parent').linkedVariables.map((variable) => variable.name),
    ['shelterValue', 'xp'],
  )
})

test('a link resolved through the fallback locale still contributes variables', () => {
  const count = { name: 'count', source: 'named', role: 'text' }
  const en = new Map([['leaf', textEntry([count])]])
  const fr = new Map([['parent', linkEntry('leaf')]])

  resolveLinkedVariables(
    new Map([
      ['en', en],
      ['fr', fr],
    ]),
    'en',
  )

  assert.deepEqual(
    fr.get('parent').linkedVariables.map((variable) => variable.name),
    ['count'],
  )
})

test('a cycle in the link graph terminates without inventing variables', () => {
  const entries = new Map([
    ['a', linkEntry('b')],
    ['b', linkEntry('a')],
    ['self', linkEntry('self')],
  ])

  resolveLinkedVariables(new Map([['en', entries]]), 'en')

  assert.equal(entries.get('a').linkedVariables, undefined)
  assert.equal(entries.get('self').linkedVariables, undefined)
})

test('a variable whose role disagrees across a link makes the parent unsupported', () => {
  const entries = new Map([
    ['parent', linkEntry('leaf', [{ name: 'name', source: 'named', role: 'text' }])],
    ['leaf', textEntry([{ name: 'name', source: 'named', role: 'assetPath' }])],
  ])

  const conflicts = resolveLinkedVariables(new Map([['en', entries]]), 'en')

  assert.equal(conflicts, 1)
  assert.equal(entries.get('parent').form, 'unsupported')
  assert.equal(entries.get('parent').reason, 'conflicting-variable-role')
})

test('the production catalog carries the variables a linked resolution needs', async () => {
  // edgeOfTheEarth part 3 links part 2, which asks for shelterValue.
  const built = await buildCatalog({})
  const key = 'edgeOfTheEarth.iceAndDeath.part3.investigatorSetup.body'
  const entry = chunkFor(built.files, built.manifest, built.manifest.defaultLocale, key)?.entries[key]

  assert.ok(entry, `${key} is not published in ${built.manifest.defaultLocale}`)
  const declared = [...(entry.variables ?? []), ...(entry.linkedVariables ?? [])].map(
    (variable) => variable.name,
  )
  assert.ok(declared.includes('shelterValue'), `expected shelterValue in ${declared.join(', ')}`)
})

test('a conflict detail is truncated to the bound the schema publishes', () => {
  const long = `${'k'.repeat(400)}`
  const entries = new Map([
    [long, linkEntry('leaf', [{ name: 'name', source: 'named', role: 'text' }])],
    ['leaf', textEntry([{ name: 'name', source: 'named', role: 'assetPath' }])],
  ])

  const conflicts = resolveLinkedVariables(new Map([['en', entries]]), 'en')

  assert.equal(conflicts, 1)
  const entry = entries.get(long)
  assert.equal(entry.form, 'unsupported')
  assert.ok(
    entry.detail.length <= MAX_UNSUPPORTED_DETAIL,
    `detail is ${entry.detail.length} characters, schema allows ${MAX_UNSUPPORTED_DETAIL}`,
  )

  // And the bound really is the schema's, not a second copy of the number.
  const schema = JSON.parse(
    readFileSync(join(REPO_ROOT, 'frontend/schemas/locale-catalog/v1/chunk.schema.json'), 'utf8'),
  )
  const branch = schema.$defs.entry.oneOf.find((candidate) => candidate.properties?.detail)
  assert.equal(branch.properties.detail.maxLength, MAX_UNSUPPORTED_DETAIL)
})

test('a variable the backend cannot type makes the entry unavailable, in every locale', async () => {
  const built = await buildCatalog({})
  const listed = built.manifest.backend.unknownVariableTypes
  assert.ok(Array.isArray(listed), 'the manifest must publish unknownVariableTypes')

  // Nothing may be published as renderable while its slot is unproven.
  for (const { key, variable, role, type } of listed) {
    assert.ok(['text', 'icon'].includes(role), `${key}.${variable} has role ${role}`)
    assert.notEqual(type, 'integer', `${key}.${variable} is integer and should have been accepted`)
    for (const locale of built.manifest.locales) {
      const entry = chunkFor(built.files, built.manifest, locale.locale, key)?.entries[key]
      if (entry === undefined) continue
      assert.equal(
        entry.form,
        'unsupported',
        `${key} is still published as renderable in ${locale.locale}`,
      )
      assert.equal(entry.reason, 'unusable-variable-type', `${key} reason in ${locale.locale}`)
    }
  }

  // …and the ones the source does prove stay supported, with their type.
  const proven = 'edgeOfTheEarth.iceAndDeath.part3.investigatorSetup.body'
  assert.equal(backendRegistry.keys.find((entry) => entry.key === proven).variables.find(
    (variable) => variable.name === 'shelterValue',
  ).type, 'integer')
  assert.ok(
    !listed.some((entry) => entry.key === proven),
    'a proven integer variable was reported as unusable',
  )
  assert.equal(
    chunkFor(built.files, built.manifest, built.manifest.defaultLocale, proven)?.entries[proven]
      ?.form,
    'message',
  )
})

test('variable coverage compares role and type, not just the name', async () => {
  const built = await buildCatalog({})
  const listed = built.manifest.backend.unknownVariableTypes
  // Every listed pair is a name the backend *does* send — the failure is what
  // it sends, which a name-only check would have missed entirely.
  for (const { key, variable } of listed.slice(0, 20)) {
    const record = backendRegistry.keys.find((entry) => entry.key === key)
    assert.ok(record, `${key} is not in the registry`)
    assert.ok(
      record.variables.some((declared) => declared.name === variable),
      `${key} does not declare ${variable}; this should have been a missing-variable gap`,
    )
  }
})

test('an entry that links an unusable one is unusable too, in every locale', async () => {
  const built = await buildCatalog({})
  const unusable = new Set(built.manifest.backend.unknownVariableTypes.map((entry) => entry.key))
  assert.ok(unusable.size > 0, 'nothing to check')

  // Nothing published may render *through* an entry the backend cannot fill.
  const linkTargets = (entry) => {
    const targets = []
    for (const nodes of entryNodes(entry)) {
      walkNodes(nodes, (node) => {
        if (node.type === 'linked' && node.target.kind === 'static') targets.push(node.target.key)
      })
    }
    return targets
  }
  eachEntry(built.files, (chunk, key, entry) => {
    if (entry.form === 'unsupported') return
    for (const target of linkTargets(entry)) {
      assert.ok(
        !unusable.has(target),
        `${key} is published as renderable in ${chunk.locale} but links ${target}, which is not`,
      )
    }
  })
})

test('a parent that links an unusable entry is downgraded, transitively and through the fallback', () => {
  // A synthetic three-level chain, resolved by the same production driver the
  // build uses.
  const unusableTarget = {
    form: 'message',
    nodes: [{ type: 'var', name: 'token', source: 'named', role: 'text' }],
    variables: [{ name: 'token', source: 'named', role: 'text' }],
  }
  const en = new Map([
    ['leaf', unusableTarget],
    ['middle', linkEntry('leaf')],
    ['parent', linkEntry('middle')],
  ])
  const fr = new Map([['frParent', linkEntry('leaf')]])
  const normalized = new Map([
    ['en', en],
    ['fr', fr],
  ])
  const backend = { keys: new Map([['leaf', { variables: new Map([['token', 'unknown']]) }]]) }

  const { unknownVariableTypes } = resolveVariablesAndLinks(
    normalized,
    'en',
    new Set(['leaf']),
    backend,
  )

  assert.deepEqual(unknownVariableTypes, [
    { key: 'leaf', variable: 'token', role: 'text', type: 'unknown' },
  ])
  for (const [locale, entries] of normalized) {
    for (const [key, entry] of entries) {
      assert.equal(entry.form, 'unsupported', `${locale}:${key} should be unsupported`)
    }
  }
  assert.equal(en.get('leaf').reason, 'unusable-variable-type')
  // …including the one that only reaches the leaf through the fallback locale.
  assert.equal(fr.get('frParent').reason, 'conflicting-variable-role')
})
