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
  requiredKeysFromFixture,
  writeCatalog,
} from '../scripts/locale-catalog/generate.mjs'
import { sha256Hex } from '../scripts/locale-catalog/canonical.mjs'

const REPO_ROOT = resolve('..')
const SCRATCH = resolve('node_modules', '.locale-catalog-test')

const NODE_TYPES = new Set([
  'text',
  'var',
  'linked',
  'break',
  'rule',
  'paragraph',
  'heading',
  'group',
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
    else if (Array.isArray(node.children)) walkNodes(node.children, visit)
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
  assert.deepEqual([...required].sort(), [...built.manifest.provenance.requiredKeys].sort())

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
      assert.ok(chunk.path.startsWith(`${BASE_PATH}/r/${built.manifest.catalogRevision}/${locale.locale}/`))
      assert.ok(!seen.has(chunk.path), `duplicate chunk path ${chunk.path}`)
      seen.add(chunk.path)

      const bytes = built.files.get(chunk.path.slice(`${BASE_PATH}/`.length))
      assert.ok(bytes, `${chunk.path} was not generated`)
      assert.equal(bytes.length, chunk.bytes)
      assert.equal(sha256Hex(bytes), chunk.sha256)
      assert.ok(chunk.path.includes(chunk.sha256.slice(0, 16)), 'chunk URLs are content-addressed')
      assert.ok(chunk.bytes <= 8 * 1024 * 1024)

      const parsed = JSON.parse(bytes.toString('utf8'))
      assert.equal(parsed.catalogRevision, built.manifest.catalogRevision)
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
