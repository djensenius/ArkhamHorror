// Publishes the localized card names the web client substitutes into label
// variables, so a native client can do the same from the verified catalog.
//
// The backend sends a card or investigator name as `name`/`__name` (or
// `iname`/`__iname`) label variables: the English title plus the card code
// (see `cardNameVar`/`investigatorNameVar` in `Arkham/I18n.hs`). The web's
// `handleI18n` (`src/arkham/i18n.ts`) replaces the English title with
// `getDbCard(code).name` read from `public/cards_<locale>.json`. This module
// answers the same lookup ahead of time, per locale, and publishes each answer
// as `cardNames.<code>`, so a client needs one exact-key lookup per code and no
// knowledge of the suffix rules:
//
//   * `buildCardIndex` (`src/stores/dbCards.ts`) files every record under its
//     own code, then, in a second pass, lends each record its `<code>b` key when
//     no record is filed under that code -- so a real `b` record always wins.
//   * `getDbCard(code)` answers `index[code] ?? index[code + 'a']`.
//
// A code is published only when the record it resolves to has a non-empty
// string name. A record with no usable name publishes nothing, rather than
// falling through to another record the web would not have reached either.

import { readFileSync } from 'node:fs'
import { join } from 'node:path'

import { KEY_SEGMENT_PATTERN, MAX_MESSAGE_KEY_LENGTH } from './normalize.mjs'

export const CARD_NAMES_PACK = 'cardNames'

function fail(message) {
  throw new Error(`locale-catalog: ${message}`)
}

/**
 * Card-only catalog locales: languages the web shows card names in
 * (`preferredLanguage` in `src/locales/language.ts` picks the card file from
 * the device language) but has no UI translation for. Each is published as an
 * ordinary catalog locale whose only keys are `cardNames.*`, falling back to
 * the default locale for everything else. The value is the suffix of the
 * committed card file: Portuguese card data lives in `cards_po.json`.
 * Japanese is left out because `cards_ja.json` is empty.
 *
 * Keep in step with CARD_SOURCE_ALIASES in scripts/validate-locale-catalog.py,
 * which repeats the pt -> po mapping for its card-file coverage check.
 */
export const CARD_ONLY_LOCALES = Object.freeze({ pl: 'pl', pt: 'po', ru: 'ru', uk: 'uk' })

/** `public/cards_<file>.json`, the card file the catalog reads for a locale. */
export function cardSourcePath(locale) {
  const file = Object.hasOwn(CARD_ONLY_LOCALES, locale) ? CARD_ONLY_LOCALES[locale] : locale
  return `public/cards_${file}.json`
}

/** Mirrors `buildCardIndex` in `src/stores/dbCards.ts`, step for step. */
function buildCardIndex(cards) {
  const index = new Map()
  for (const card of cards) index.set(card.code, card)
  for (const card of cards) {
    const back = `${card.code}b`
    if (!index.has(back)) index.set(back, card)
  }
  return index
}

/**
 * Every code `getDbCard` resolves, mapped to the name it answers with.
 *
 * Returned in code order. `cards` is the parsed `cards_<locale>.json`; a record
 * without a usable code fails the build instead of being skipped, because the
 * web would index it under a key no client could ask for.
 */
export function resolveCardNames(cards, source = 'card data') {
  if (!Array.isArray(cards)) fail(`${source} is not an array of card records`)
  for (const card of cards) {
    if (card === null || typeof card !== 'object' || Array.isArray(card)) {
      fail(`${source} holds a card record that is not an object`)
    }
    if (typeof card.code !== 'string' || !KEY_SEGMENT_PATTERN.test(card.code)) {
      fail(`${source} holds a card with an unusable code ${JSON.stringify(card.code)}`)
    }
  }

  const index = buildCardIndex(cards)
  const resolved = new Map()
  const answer = (code, card) => {
    if (typeof card.name === 'string' && card.name !== '') resolved.set(code, card.name)
  }
  // `index[code]` first: every filed and lent key answers for itself.
  for (const [code, card] of index) answer(code, card)
  // Then `index[code + 'a']`, which only applies to a code nothing is filed
  // under. Lent keys all end in `b`, so an `a` key is always a real record.
  for (const [code, card] of index) {
    if (!code.endsWith('a')) continue
    const bare = code.slice(0, -1)
    if (bare !== '' && !index.has(bare)) answer(bare, card)
  }

  return new Map([...resolved].sort(([a], [b]) => (a < b ? -1 : a > b ? 1 : 0)))
}

/** The catalog key a client looks a code up under. */
export function cardNameKey(code) {
  const key = `${CARD_NAMES_PACK}.${code}`
  if (key.length > MAX_MESSAGE_KEY_LENGTH) fail(`card name key ${key.slice(0, 80)}… is too long`)
  return key
}

/**
 * The published entries for one locale: a literal text node per code. A card
 * name is a substitution value, not a vue-i18n message, so it is never parsed
 * for placeholders, links or markup.
 */
export function cardNameEntries(cards, source) {
  const entries = new Map()
  for (const [code, name] of resolveCardNames(cards, source)) {
    entries.set(cardNameKey(code), { form: 'message', nodes: [{ type: 'text', value: name }], variables: [] })
  }
  return entries
}

/** Reads and resolves the card file (`cardSourcePath`) of one catalog locale. */
export function loadCardNameEntries(frontendDir, locale) {
  const path = cardSourcePath(locale)
  let text
  try {
    text = readFileSync(join(frontendDir, path), 'utf8')
  } catch (error) {
    fail(`${path} is unreadable (${error.code ?? error.message}); every catalog locale needs card data`)
  }
  if (text.trim() === '') fail(`${path} is empty; every catalog locale needs card data`)
  let cards
  try {
    cards = JSON.parse(text)
  } catch (error) {
    fail(`${path} is not valid JSON: ${error.message}`)
  }
  return cardNameEntries(cards, path)
}
