// Unit tests for the source-integrity checks that run before the catalog is
// built. All fixtures here are synthetic; no locale prose is duplicated.

import assert from 'node:assert/strict'
import { mkdirSync, mkdtempSync, readFileSync, rmSync, writeFileSync } from 'node:fs'
import { dirname, join, resolve } from 'node:path'
import { tmpdir } from 'node:os'
import test from 'node:test'
import { fileURLToPath } from 'node:url'

import { analyzeComposition, findDuplicateKeys } from '../scripts/locale-catalog/inventory.mjs'
import { nodeRuntime } from '../scripts/locale-catalog/sources.mjs'

const REPO_ROOT = resolve(dirname(fileURLToPath(import.meta.url)), '../..')

test('a duplicate key in one document is reported with both locations', () => {
  const text = ['{', '  "alpha": "first",', '  "beta": "kept",', '  "alpha": "second"', '}'].join('\n')
  const duplicates = findDuplicateKeys(text, 'synthetic.json')
  assert.equal(duplicates.length, 1)
  assert.equal(duplicates[0].key, 'alpha')
  assert.equal(duplicates[0].file, 'synthetic.json')
  assert.deepEqual(duplicates[0].first, { line: 2, column: 3 })
  assert.deepEqual(duplicates[0].second, { line: 4, column: 3 })
})

test('duplicates are found at every depth, in arrays, and for equal values', () => {
  const nested = JSON.stringify({ outer: { inner: 1 } }).replace('"inner":1', '"inner":1,"inner":2')
  assert.equal(findDuplicateKeys(nested, 'n.json')[0].key, 'outer.inner')

  const inArray = '{"list":[{"a":1,"a":2}]}'
  assert.equal(findDuplicateKeys(inArray, 'a.json').length, 1)

  // Identical values are still a duplicate: one of them is dead weight and the
  // reader cannot tell which.
  assert.equal(findDuplicateKeys('{"a":"same","a":"same"}', 's.json').length, 1)

  // A key that is only equal after unescaping counts too.
  assert.equal(findDuplicateKeys('{"a.b":1,"a\\u002Eb":2}', 'e.json').length, 1)

  assert.deepEqual(findDuplicateKeys('{"a":1,"b":{"a":2}}', 'ok.json'), [])
})

// The composed tree carries one owner per leaf, exactly as the ownership-tagging
// Vite plugin produces it, so these exercise the real attribution rules rather
// than a value-matching approximation.
const OWNER = '__localeCatalogOwner'
const MODULE = '__localeCatalogModule'
const mounted = (tree, file) => {
  Object.defineProperty(tree, MODULE, { value: file, enumerable: false })
  return tree
}
const owned = (text, file) => {
  const boxed = new String(text)
  boxed[OWNER] = file
  return boxed
}

test('a partially overridden contributor is reported with the file that won', () => {
  const first = 'src/locales/en/scenario/first.json'
  const second = 'src/locales/en/scenario/second.json'
  const composed = { scenario: { intro: owned('kept', first), outro: owned('other file wins', second) } }
  const files = [
    { path: first, tree: { intro: 'kept', outro: 'overridden', extra: 'never mounted' } },
    { path: second, tree: { outro: 'other file wins' } },
  ]

  const { findings } = analyzeComposition(composed, files, OWNER)
  assert.equal(findings.length, 1)
  assert.equal(findings[0].file, first)
  assert.equal(findings[0].kind, 'content-lost')
  assert.deepEqual(findings[0].examples, [`scenario.outro -> ${second}`, 'scenario.extra'])
  assert.equal(findings[0].count, 2)
})

test('short and identical values are still attributed to their real owner', () => {
  // Value matching cannot tell these apart; ownership tags can.
  const first = 'src/locales/en/a.json'
  const second = 'src/locales/en/b.json'
  const composed = { a: { ok: owned('NO', second) } }
  const files = [
    { path: first, tree: { ok: 'NO' } },
    { path: second, tree: { ok: 'NO' } },
  ]

  const { findings } = analyzeComposition(composed, files, OWNER)
  assert.equal(findings.length, 1)
  assert.equal(findings[0].file, first)
  assert.equal(findings[0].kind, 'no-surviving-content')
})

test('a contributor whose content never reaches the tree is reported', () => {
  const first = 'src/locales/en/scenario/first.json'
  const second = 'src/locales/en/scenario/second.json'
  const composed = { scenario: { intro: owned('from the second file', second) } }
  const files = [
    { path: first, tree: { intro: 'from the first file', extra: 'lost' } },
    { path: second, tree: { intro: 'from the second file' } },
  ]

  const { findings } = analyzeComposition(composed, files, OWNER)
  assert.equal(findings.length, 1)
  assert.equal(findings[0].file, first)
  assert.equal(findings[0].kind, 'no-surviving-content')
  assert.equal(findings[0].count, 2)
})

test('a file no module imports is reported as orphaned', () => {
  const used = 'src/locales/en/scenario/used.json'
  const composed = { scenario: { intro: owned('published', used) } }
  const files = [
    { path: used, tree: { intro: 'published' } },
    { path: 'src/locales/en/scenario/orphan.json', tree: { intro: 'translated but never imported' } },
  ]

  const { findings } = analyzeComposition(composed, files, OWNER)
  assert.equal(findings.length, 1)
  assert.equal(findings[0].file, 'src/locales/en/scenario/orphan.json')
  assert.equal(findings[0].kind, 'no-surviving-content')
})

test('a spread that fully overrides another file is still reported', () => {
  // `{ ...label, ...other }` at the same mount: `other` wins every leaf.
  const label = 'src/locales/en/label.json'
  const other = 'src/locales/en/other.json'
  const composed = { one: owned('other', other), two: owned('other two', other) }
  const files = [
    { path: label, tree: { one: 'label', two: 'label two' } },
    { path: other, tree: { one: 'other', two: 'other two' } },
  ]

  const { findings } = analyzeComposition(composed, files, OWNER)
  assert.equal(findings.length, 1)
  assert.equal(findings[0].file, label)
})

test('a file mounted under two aliases is attributed to both', () => {
  const shared = 'src/locales/en/shared.json'
  const composed = {
    first: { intro: owned('shared intro', shared) },
    second: { intro: owned('shared intro', shared) },
  }
  const files = [{ path: shared, tree: { intro: 'shared intro' } }]
  assert.deepEqual(analyzeComposition(composed, files, OWNER).findings, [])
})

test('a file mounted twice must survive at every mount', () => {
  // `returnToTheForgottenAge` mounts the same file under two names; content
  // overridden at one of them is lost there even though it survives elsewhere.
  const shared = 'src/locales/en/shared.json'
  const other = 'src/locales/en/other.json'
  const composed = {
    first: mounted({ intro: owned('shared intro', shared) }, shared),
    second: mounted({ intro: owned('other intro', other) }, shared),
  }
  const files = [
    { path: shared, tree: { intro: 'shared intro' } },
    { path: other, tree: { intro: 'other intro' } },
  ]

  const { findings } = analyzeComposition(composed, files, OWNER, MODULE)
  const lost = findings.find((finding) => finding.file === shared)
  assert.ok(lost, 'an overridden alias mount was not reported')
  assert.deepEqual(lost.examples, [`second.intro -> ${other}`])
})

test('an intact composition reports nothing', () => {
  const a = 'src/locales/en/a.json'
  const b = 'src/locales/en/b.json'
  const composed = { a: { one: owned('one', a) }, b: { two: owned('two', b) } }
  const files = [
    { path: a, tree: { one: 'one' } },
    { path: b, tree: { two: 'two' } },
  ]
  assert.deepEqual(analyzeComposition(composed, files, OWNER).findings, [])
})

// --- Runtime pin ------------------------------------------------------------
// The generator's output depends on the Node it runs on, so the version is
// pinned exactly and recorded in the provenance. A patch-level drift must be a
// hard failure, not a shrug.

test('the node engine pin is exact and must match the running runtime', (t) => {
  const directory = mkdtempSync(join(tmpdir(), 'locale-catalog-node-'))
  t.after(() => rmSync(directory, { recursive: true, force: true }))

  const frontend = join(directory, 'frontend')
  mkdirSync(frontend, { recursive: true })
  const write = (engine) =>
    writeFileSync(join(frontend, 'package.json'), JSON.stringify({ engines: { node: engine } }))

  write(process.versions.node)
  assert.deepEqual(nodeRuntime(frontend), { version: process.versions.node })

  const [major, minor, patch] = process.versions.node.split('.')
  write(`${major}.${minor}.${Number(patch) + 1}`)
  assert.throws(() => nodeRuntime(frontend), /pinned to Node/)

  write(`^${process.versions.node}`)
  assert.throws(() => nodeRuntime(frontend), /must pin an exact node version/)

  write(major)
  assert.throws(() => nodeRuntime(frontend), /must pin an exact node version/)
})

test('the repository pins the same exact node version everywhere', () => {
  const pinned = JSON.parse(readFileSync(join(REPO_ROOT, 'frontend/package.json'), 'utf8')).engines
    .node
  assert.match(pinned, /^\d+\.\d+\.\d+$/)

  const contains = (path, needle) =>
    assert.ok(
      readFileSync(join(REPO_ROOT, path), 'utf8').includes(needle),
      `${path} does not pin node ${pinned}`,
    )
  contains('mise.toml', pinned)
  contains('Dockerfile', pinned)
})
