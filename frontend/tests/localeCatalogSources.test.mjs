// Unit tests for the source-integrity checks that run before the catalog is
// built. All fixtures here are synthetic; no locale prose is duplicated.

import assert from 'node:assert/strict'
import test from 'node:test'

import { findCompositionLosses, findDuplicateKeys } from '../scripts/locale-catalog/inventory.mjs'

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

test('a partially overridden contributor is reported with its mount and owner', () => {
  const composed = {
    scenario: { intro: 'a long enough shared intro line', outro: 'replaced by the other file' },
  }
  const files = [
    {
      path: 'src/locales/en/scenario/first.json',
      tree: { intro: 'a long enough shared intro line', outro: 'this variant was overridden', extra: 'lost' },
    },
  ]
  const losses = findCompositionLosses(composed, files, new Set(files.map((file) => file.path)))
  assert.equal(losses.length, 1)
  assert.equal(losses[0].mount, 'scenario')
  assert.deepEqual(losses[0].overridden, ['outro'])
  assert.deepEqual(losses[0].missing, ['extra'])
})

test('a contributor whose content never reaches the tree is reported', () => {
  const composed = {
    scenario: { intro: 'from the second file', outro: 'only here' },
  }
  const files = [
    { path: 'src/locales/en/scenario/first.json', tree: { intro: 'from the first file', extra: 'lost' } },
    { path: 'src/locales/en/scenario/second.json', tree: { intro: 'from the second file', outro: 'only here' } },
  ]

  const losses = findCompositionLosses(composed, files, new Set(files.map((file) => file.path)))
  assert.equal(losses.length, 1)
  assert.equal(losses[0].file, 'src/locales/en/scenario/first.json')
  assert.equal(losses[0].mount, '<orphaned-or-overridden>')
  assert.equal(losses[0].missingCount, 2)
})

test('a file no module imports is reported as orphaned', () => {
  const composed = { scenario: { intro: 'published text for the scenario' } }
  const files = [
    { path: 'src/locales/en/scenario/used.json', tree: { intro: 'published text for the scenario' } },
    { path: 'src/locales/en/scenario/orphan.json', tree: { intro: 'translated but never imported' } },
  ]

  const losses = findCompositionLosses(composed, files, new Set(files.map((file) => file.path)))
  assert.equal(losses.length, 1)
  assert.equal(losses[0].file, 'src/locales/en/scenario/orphan.json')
  assert.equal(losses[0].mount, '<orphaned-or-overridden>')
})

test('a file that belongs to another locale is not a finding', () => {
  const composed = { scenario: { intro: 'english text for the scenario' } }
  const files = [
    { path: 'src/locales/en/scenario/used.json', tree: { intro: 'english text for the scenario' } },
    { path: 'homebrew/campaign/locales/en/base.json', tree: { intro: 'homebrew text nobody composed here' } },
  ]

  const losses = findCompositionLosses(composed, files, new Set(['src/locales/en/scenario/used.json']))
  assert.deepEqual(losses, [])
})

test('an intact composition reports nothing', () => {
  const composed = { a: { one: 'value one is long enough' }, b: { two: 'value two is long enough' } }
  const files = [
    { path: 'src/locales/en/a.json', tree: { one: 'value one is long enough' } },
    { path: 'src/locales/en/b.json', tree: { two: 'value two is long enough' } },
  ]
  assert.deepEqual(findCompositionLosses(composed, files, new Set(files.map((f) => f.path))), [])
})
