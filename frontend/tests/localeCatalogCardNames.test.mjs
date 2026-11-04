// Tests for the cardNames pack: the card-code lookup the web performs in
// `src/stores/dbCards.ts` (`buildCardIndex` + `getDbCard`), answered ahead of
// time per locale so native clients do one exact-key lookup per code.

import assert from 'node:assert/strict'
import { mkdirSync, mkdtempSync, readFileSync, rmSync, writeFileSync } from 'node:fs'
import { tmpdir } from 'node:os'
import { join, resolve } from 'node:path'
import test from 'node:test'

import {
  CARD_NAMES_PACK,
  cardNameEntries,
  cardNameKey,
  resolveCardNames,
} from '../scripts/locale-catalog/card-names.mjs'
import { addCardNames, assertNoCardNameClaims, buildCatalog } from '../scripts/locale-catalog/generate.mjs'

const card = (code, name) => ({ code, name })
const names = (cards) => Object.fromEntries(resolveCardNames(cards))

test('a plain code answers with its own record', () => {
  assert.deepEqual(names([card('01001', 'Roland Banks')]), {
    '01001': 'Roland Banks',
    '01001b': 'Roland Banks',
  })
})

test('a bare code answers with its "a" record only when nothing is filed under it', () => {
  assert.deepEqual(names([card('01121a', 'Predator or Prey?'), card('01121b', 'The Masked Hunter')]), {
    '01121': 'Predator or Prey?',
    '01121a': 'Predator or Prey?',
    '01121ab': 'Predator or Prey?',
    '01121b': 'The Masked Hunter',
    '01121bb': 'The Masked Hunter',
  })
  const filed = names([card('02000', 'Front'), card('02000a', 'Side A')])
  assert.equal(filed['02000'], 'Front')
  assert.equal(filed['02000a'], 'Side A')
})

test('the "b" alias never shadows a real "b" record, whatever the file order', () => {
  for (const cards of [
    [card('03000', 'Front'), card('03000b', 'Back')],
    [card('03000b', 'Back'), card('03000', 'Front')],
  ]) {
    const resolved = names(cards)
    assert.equal(resolved['03000'], 'Front')
    assert.equal(resolved['03000b'], 'Back')
    assert.equal(resolved['03000bb'], 'Back')
  }
  assert.equal(names([card('03001', 'Only Front')])['03001b'], 'Only Front')
})

test('only non-empty string names are published, with no fallthrough', () => {
  const resolved = names([
    card('04000', ''),
    card('04000a', 'Side A'),
    { code: '04001' },
    card('04002', 42),
    card('04003', 'Kept'),
  ])
  // `getDbCard('04000')` stops at the filed record, so 04000 is not answered
  // from 04000a; the unusable records publish nothing at all.
  assert.deepEqual(resolved, { '04000a': 'Side A', '04000ab': 'Side A', '04003': 'Kept', '04003b': 'Kept' })
})

test('unusable card data fails instead of being skipped', () => {
  assert.throws(() => resolveCardNames({}), /not an array/)
  assert.throws(() => resolveCardNames([null]), /not an object/)
  assert.throws(() => resolveCardNames([card('01.001', 'x')]), /unusable code/)
  assert.throws(() => resolveCardNames([{ name: 'x' }]), /unusable code/)
})

test('entries are code-ordered literal text messages under the pack prefix', () => {
  const entries = cardNameEntries([card('05001', '{not} @:a placeholder'), card('05000', 'B')], 'test')
  assert.deepEqual([...entries.keys()], ['cardNames.05000', 'cardNames.05000b', 'cardNames.05001', 'cardNames.05001b'])
  assert.deepEqual(entries.get('cardNames.05001'), {
    form: 'message',
    nodes: [{ type: 'text', value: '{not} @:a placeholder' }],
    variables: [],
  })
  assert.equal(cardNameKey('01001'), 'cardNames.01001')
})

const built = await buildCatalog({})
const { manifest } = built

function cardNamesChunk(locale) {
  const entry = manifest.locales.find((candidate) => candidate.locale === locale)
  assert.ok(entry, `catalog has no ${locale} locale`)
  const chunks = entry.chunks.filter((chunk) => chunk.pack === CARD_NAMES_PACK)
  assert.equal(chunks.length, 1, `${locale} has exactly one ${CARD_NAMES_PACK} chunk`)
  const relative = chunks[0].path.slice(manifest.basePath.length + 1)
  return JSON.parse(built.files.get(relative).toString('utf8'))
}

test('every catalog locale publishes the names resolved from its own card file', () => {
  const sourceLocales = ['de', 'en', 'es', 'fr', 'it', 'ko', 'zh', 'zh-cn']
  assert.deepEqual(manifest.locales.map((entry) => entry.locale.toLowerCase()).sort(), sourceLocales)
  for (const source of sourceLocales) {
    const locale = manifest.locales.find((entry) => entry.locale.toLowerCase() === source).locale
    const cards = JSON.parse(readFileSync(resolve('public', `cards_${source}.json`), 'utf8'))
    const expected = Object.fromEntries(cardNameEntries(cards, source))
    const chunk = cardNamesChunk(locale)
    assert.equal(chunk.locale, locale)
    assert.deepEqual(chunk.entries, expected, `${locale} cardNames differ from cards_${source}.json`)
  }
})

test('de and en differ where the card data does', () => {
  const de = cardNamesChunk('de').entries
  const en = cardNamesChunk('en').entries
  const text = (entries, code) => entries[cardNameKey(code)].nodes[0].value
  assert.equal(text(en, '01111'), 'Study')
  assert.equal(text(de, '01111'), 'Arbeitszimmer')
  assert.equal(text(en, '01121'), 'Predator or Prey?')
  assert.equal(text(de, '01121b'), 'Der Maskierte Jäger')
  assert.equal(text(de, '01001b'), 'Roland Banks')
  assert.deepEqual(Object.keys(de).sort(), Object.keys(en).sort())
})

test('card names take no part in the backend untranslated or variable-gap accounting', () => {
  const { backend } = manifest
  for (const key of backend.untranslatedKeys) assert.ok(!key.startsWith(`${CARD_NAMES_PACK}.`), key)
  for (const gap of [...backend.variableGaps, ...backend.unknownVariableTypes]) {
    assert.ok(!JSON.stringify(gap).includes(`"${CARD_NAMES_PACK}.`), JSON.stringify(gap))
  }
})

// The Night of the Zealot names the fixture must cover (AC3): the five
// investigators, the basic-weakness enemies a DrewPlayerEnemy label names, every
// location, enemy, story asset and treachery in the core-box encounter sets,
// and one example of each lookup rule (bare code from `a`, real `b`, lent `b`).
function nightOfTheZealotNameCodes() {
  const cards = JSON.parse(readFileSync(resolve('public', 'cards_en.json'), 'utf8'))
  const codes = new Set(['01001', '01002', '01003', '01004', '01005', '01101', '01102', '01103'])
  for (const code of ['01121', '01121b', '01001b']) codes.add(code)
  for (const card of cards) {
    if (card.pack_code !== 'core' || !card.encounter_code) continue
    if (['location', 'enemy', 'asset', 'treachery'].includes(card.type_code)) codes.add(card.code)
  }
  return [...codes].map(cardNameKey).sort()
}

// If an upstream sync renames a pinned card, this fails: update CARD_NAMES in
// scripts/build-locale-catalog-fixture.py, rewrite the fixtures and bump
// schemaRevision (see contracts/README.md, revision 0.1.58).
test('the contract fixture card names are what the real generator resolves', () => {
  const fixtures = resolve('..', 'contracts', 'fixtures')
  const expectedKeys = nightOfTheZealotNameCodes()
  for (const locale of ['en', 'de']) {
    const source = JSON.parse(readFileSync(resolve(fixtures, `locale-catalog-source-${locale}.json`), 'utf8'))
    const pinned = Object.entries(source).filter(([key]) => key.startsWith(`${CARD_NAMES_PACK}.`))
    assert.deepEqual(pinned.map(([key]) => key).sort(), expectedKeys, `${locale} fixture card-name coverage`)
    const real = cardNamesChunk(locale).entries
    for (const [key, name] of pinned) {
      assert.equal(real[key]?.nodes[0].value, name, `${locale} ${key}`)
    }
  }
})

test('a locale message inside the card-name pack is refused, not shadowed', () => {
  const scratch = mkdtempSync(join(tmpdir(), 'locale-catalog-card-names-'))
  try {
    mkdirSync(join(scratch, 'public'))
    writeFileSync(join(scratch, 'public', 'cards_en.json'), JSON.stringify([card('01001', 'Roland Banks')]))
    const text = (value) => ({ form: 'message', nodes: [{ type: 'text', value }], variables: [] })
    const clean = new Map([['en', new Map([['core.continue', text('Continue')]])]])
    addCardNames(clean, scratch)
    assert.equal(clean.get('en').get('cardNames.01001').nodes[0].value, 'Roland Banks')
    const shadowing = new Map([['en', new Map([['cardNames.01001', text('Not a card')]])]])
    assert.throws(() => addCardNames(shadowing, scratch), /en locale sources define cardNames\.01001, inside the card-name pack/)
  } finally {
    rmSync(scratch, { recursive: true, force: true })
  }
})

test('a backend or fixture key inside the card-name pack is refused', () => {
  assert.doesNotThrow(() => assertNoCardNameClaims(['core.continue', 'cardNamesExtra.x', 'label.cardNames']))
  assert.throws(
    () => assertNoCardNameClaims(['core.continue', 'cardNames.01001']),
    /backend or fixture key\(s\) fall in the cardNames pack: cardNames\.01001/,
  )
})
