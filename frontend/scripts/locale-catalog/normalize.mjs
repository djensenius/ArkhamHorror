// Turns one vue-i18n locale message into the locale catalog's typed render
// AST, or explicitly marks it unsupported.
//
// The pipeline mirrors what the Vue client actually does with the same string,
// in the same order, using the same libraries:
//
//   1. `@intlify/message-compiler` (the parser vue-i18n itself compiles
//      messages with) splits the message into text, literals, named/list
//      placeholders, and plural branches. Nothing here re-implements
//      vue-i18n's message grammar.
//   2. Every placeholder becomes an opaque sentinel. The catalog never
//      interpolates: variables stay declared and typed so a native client can
//      substitute the same values the backend sends in `I18nEntry.variables`.
//   3. `_x_`/`*x*` become bold/italic exactly as `formatContent()` in
//      `src/arkham/helpers.ts` does, in that order.
//   4. `parse5` (HTML5 tree construction, no `innerHTML` anywhere) parses the
//      result, and an allowlist maps elements to AST nodes.
//
// Anything the allowlist does not cover is never guessed at, silently
// stripped, or half-emitted: the whole entry is marked `unsupported` with a
// machine-readable reason, and the generator fails the build outright when
// such an entry is one the contract requires.

import { createParser } from '@intlify/message-compiler'
import { parseFragment } from 'parse5'

// @intlify/message-compiler's NodeTypes enum is not exported; these are its
// values, pinned by localeCatalogNormalize.test.mjs against the real parser.
const NODE_TEXT = 3
const NODE_NAMED = 4
const NODE_LIST = 5
const NODE_LINKED = 6
const NODE_LINKED_KEY = 7
const NODE_LITERAL = 9
const NODE_PLURAL = 1

// vue-i18n's built-in linked-message modifiers.
const LINKED_MODIFIERS = new Set(['upper', 'lower', 'capitalize'])

// Private-use sentinels. `assertSentinelFree` rejects source content that
// contains them, so a placeholder marker can never be forged by locale text.
const SENTINEL_OPEN = '\uE000'
const SENTINEL_CLOSE = '\uE001'
const SENTINEL_PATTERN = /[\uE000\uE001]/

// `FormattedEntry.vue` supplies exactly these two path variables to every
// I18nEntry translation:
//   imgPath    -> `${baseUrl}/img/arkham`
//   setImgPath -> `${baseUrl}/img/arkham/encounter-sets`
// They only ever appear inside an image source; the catalog resolves them to
// semantic asset paths instead of publishing any image bytes.
export const ASSET_PATH_VARIABLES = Object.freeze({
  imgPath: 'img/arkham',
  setImgPath: 'img/arkham/encounter-sets',
})

const ASSET_ROOT = 'img/arkham/'

const IMAGE_ROLES = new Map([
  ['encounter-sets', 'encounterSet'],
  ['cards', 'card'],
  ['tokens', 'token'],
  ['chaos-tokens', 'chaosToken'],
  ['campaigns', 'campaign'],
  ['homebrew', 'homebrew'],
  ['extra', 'extra'],
])

const IMAGE_PATH_PATTERN = /^[A-Za-z0-9][A-Za-z0-9._/-]{0,239}\.(?:png|jpe?g|avif|svg|webp)$/
const STYLE_TOKEN_PATTERN = /^[A-Za-z][A-Za-z0-9-]{0,63}$/
const VARIABLE_NAME_PATTERN = /^[A-Za-z0-9_]{1,64}$/

// A message key segment may hold anything the locale sources already use
// (apostrophes, brackets, CJK), but never a dot (the key separator), a path
// separator, a control character, or a sentinel.
export const KEY_SEGMENT_PATTERN = /^[^\u0000-\u001f\u007f./\\\ue000\ue001]+$/
// Matches the `maxLength` both v1 schemas put on a message key.
export const MAX_MESSAGE_KEY_LENGTH = 512
const MESSAGE_KEY_PATTERN = new RegExp(
  `^${KEY_SEGMENT_PATTERN.source.slice(1, -1)}(?:\\.${KEY_SEGMENT_PATTERN.source.slice(1, -1)})*$`,
)

const EMPHASIS_ELEMENTS = new Map([
  ['b', 'bold'],
  ['strong', 'bold'],
  ['i', 'italic'],
  ['em', 'italic'],
  ['u', 'underline'],
  ['s', 'strikethrough'],
  ['small', 'small'],
  ['minicaps', 'smallCaps'],
])

const GROUP_ELEMENTS = new Set(['div', 'section', 'header', 'span', 'blockquote'])
const HEADING_ELEMENTS = new Map([
  ['h1', 1],
  ['h2', 2],
  ['h3', 3],
  ['h4', 4],
  ['h5', 5],
  ['h6', 6],
])

// Attributes are allowlisted per element, not globally: an attribute an
// element's AST node cannot carry (a `src` on a paragraph, an `alt` on a list
// item) is refused rather than parsed and dropped.
const STYLE_ATTRIBUTES = new Set(['class', 'style'])
const GROUP_ATTRIBUTES = new Set(['class', 'style', 'data-image-id'])
const IMAGE_ATTRIBUTES = new Set(['class', 'style', 'src', 'alt', 'width', 'align'])
const NO_ATTRIBUTES = new Set()

function allowedAttributesFor(tagName) {
  if (tagName === 'img') return IMAGE_ATTRIBUTES
  if (tagName === 'br' || tagName === 'hr' || EMPHASIS_ELEMENTS.has(tagName)) return NO_ATTRIBUTES
  if (GROUP_ELEMENTS.has(tagName)) return GROUP_ATTRIBUTES
  if (
    tagName === 'p' ||
    tagName === 'ul' ||
    tagName === 'ol' ||
    tagName === 'li' ||
    HEADING_ELEMENTS.has(tagName)
  ) {
    return STYLE_ATTRIBUTES
  }
  return null
}

// Inline CSS is kept as structured, non-executable declarations rather than a
// raw string: a native client can map or ignore them, and nothing here can
// carry a URL, a script, or markup.
const STYLE_PROPERTY_PATTERN = /^(?:--)?[A-Za-z][A-Za-z0-9-]{0,31}$/
const STYLE_VALUE_PATTERN = /^[A-Za-z0-9 .,%#()/_-]{1,64}$/
const MAX_STYLE_DECLARATIONS = 8
const CARD_CODE_PATTERN = /^:?[A-Za-z0-9][A-Za-z0-9:_-]{1,31}$/
const IMAGE_ALIGNMENTS = new Set(['left', 'right', 'center', 'justify'])
const MAX_IMAGE_WIDTH = 4096

function styleDeclarations(attributes, tagName, placeholders) {
  const raw = attributes.get('style')
  if (raw === undefined) return []

  const declarations = []
  // Split on `;` outside parentheses so `clamp(1px, 2cqw, 3px)` stays intact.
  let depth = 0
  let chunk = ''
  const chunks = []
  for (const character of raw) {
    if (character === '(') depth += 1
    else if (character === ')') depth = Math.max(0, depth - 1)
    if (character === ';' && depth === 0) {
      chunks.push(chunk)
      chunk = ''
      continue
    }
    chunk += character
  }
  chunks.push(chunk)

  for (const entry of chunks) {
    const trimmed = entry.trim()
    if (trimmed === '') continue
    const separator = trimmed.indexOf(':')
    if (separator === -1) throw unsupported('invalid-style-declaration', `${tagName}: ${trimmed}`)
    const property = trimmed.slice(0, separator).trim()
    const value = trimmed.slice(separator + 1).trim()
    if (!STYLE_PROPERTY_PATTERN.test(property)) {
      throw unsupported('invalid-style-declaration', `${tagName}: ${trimmed}`)
    }

    // A `url(...)` is only ever kept as a resolved semantic asset reference —
    // never as a URL a client could fetch blindly, and never external.
    const urlMatch = value.match(/^url\(\s*([^)]*?)\s*\)$/i)
    if (urlMatch) {
      declarations.push({ property, asset: assetReference(urlMatch[1], placeholders, tagName) })
      continue
    }
    if (SENTINEL_PATTERN.test(value)) throw unsupported('placeholder-in-attribute', `${tagName}[style]`)
    if (!STYLE_VALUE_PATTERN.test(value) || /expression\s*\(|[a-z]+:\/\//i.test(value)) {
      throw unsupported('invalid-style-declaration', `${tagName}: ${trimmed}`)
    }
    declarations.push({ property, value })
  }

  if (declarations.length > MAX_STYLE_DECLARATIONS) {
    throw unsupported('invalid-style-declaration', `${tagName}: ${declarations.length} declarations`)
  }
  return declarations
}

/** Adds `style`/`styleVars` to a node only when the source carried them. */
function withPresentation(node, attributes, tagName, styleVariables, placeholders) {
  const declarations = styleDeclarations(attributes, tagName, placeholders)
  if (declarations.length > 0) node.style = declarations
  if (styleVariables !== undefined && styleVariables.length > 0) node.styleVars = styleVariables
  return node
}

export const UNSUPPORTED_REASONS = Object.freeze([
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
  'conflicting-variable-role',
  'invalid-style-declaration',
  'unsupported-link-target',
  'link-cycle',
])

class Unsupported extends Error {
  constructor(reason, detail) {
    super(`${reason}: ${detail}`)
    this.reason = reason
    this.detail = String(detail).slice(0, 120)
  }
}

function unsupported(reason, detail) {
  return new Unsupported(reason, detail)
}

/** Default classifier: only the two production asset-path variables are special. */
export function defaultClassifyVariable(name) {
  if (Object.hasOwn(ASSET_PATH_VARIABLES, name)) {
    return { role: 'assetPath', base: ASSET_PATH_VARIABLES[name] }
  }
  return { role: 'text' }
}

function assertSentinelFree(raw) {
  if (SENTINEL_PATTERN.test(raw)) {
    throw unsupported('message-syntax-error', 'message contains reserved private-use sentinel')
  }
}

// One parser for the whole run: it holds no state between parses, and the
// catalog compiles tens of thousands of messages.
let parseErrors = []
const messageParser = createParser({
  location: false,
  onError: (error) => parseErrors.push(error),
})

function parseMessage(raw) {
  parseErrors = []
  let ast
  try {
    ast = messageParser.parse(raw)
  } catch (error) {
    throw unsupported('message-syntax-error', error.message ?? 'parse failed')
  }
  if (parseErrors.length > 0) {
    throw unsupported('message-syntax-error', parseErrors[0].message ?? String(parseErrors[0].code))
  }
  return ast
}

/**
 * Replaces every placeholder in one vue-i18n message body with a sentinel and
 * records what it stood for, so HTML parsing can never see or split a
 * placeholder.
 */
function substitutePlaceholders(items, classifyVariable) {
  const placeholders = []
  let text = ''

  const push = (placeholder) => {
    placeholders.push(placeholder)
    text += `${SENTINEL_OPEN}${placeholders.length - 1}${SENTINEL_CLOSE}`
  }

  for (const item of items) {
    switch (item.type) {
      case NODE_TEXT:
        text += item.value ?? ''
        break
      // A `{'...'}` literal is inlined by vue-i18n before anything else sees
      // the string, so it is inlined here too.
      case NODE_LITERAL:
        text += item.value ?? ''
        break
      case NODE_NAMED: {
        const name = item.key
        const classification = classifyVariable(name)
        if (classification.role === 'literal') {
          // `{asterisk}`/`{underscore}`: `formatContent` turns these into
          // plain characters *after* emphasis pairing, so they are emitted as
          // text, never as emphasis delimiters.
          push({ kind: 'text', text: classification.text })
        } else if (classification.role === 'assetPath') {
          push({ kind: 'assetPath', name, base: classification.base })
        } else {
          if (!VARIABLE_NAME_PATTERN.test(name)) {
            throw unsupported('unsupported-message-syntax', `variable name ${name}`)
          }
          push({ kind: 'variable', name, source: 'named', role: classification.role })
        }
        break
      }
      case NODE_LIST:
        push({ kind: 'variable', name: String(item.index), source: 'list', role: 'text' })
        break
      // `@:other.key` / `@:{slot}`: a reference to another catalog key, kept
      // as a reference rather than inlined, so a client resolves it against
      // the same locale (then the fallback locale) exactly as vue-i18n does.
      case NODE_LINKED: {
        const modifier = item.modifier ? item.modifier.value : null
        if (modifier !== null && !LINKED_MODIFIERS.has(modifier)) {
          throw unsupported('unsupported-message-syntax', `linked modifier ${modifier}`)
        }
        const key = item.key
        let target
        if (key.type === NODE_LINKED_KEY || key.type === NODE_LITERAL) {
          if (!MESSAGE_KEY_PATTERN.test(key.value) || key.value.length > MAX_MESSAGE_KEY_LENGTH) {
            throw unsupported('unsupported-message-syntax', `linked key ${key.value}`)
          }
          target = { kind: 'static', key: key.value }
        } else if (key.type === NODE_NAMED) {
          if (!VARIABLE_NAME_PATTERN.test(key.key)) {
            throw unsupported('unsupported-message-syntax', `variable name ${key.key}`)
          }
          target = { kind: 'variable', name: key.key, source: 'named' }
        } else if (key.type === NODE_LIST) {
          target = { kind: 'variable', name: String(key.index), source: 'list' }
        } else {
          throw unsupported('unsupported-message-syntax', `linked key type ${key.type}`)
        }
        push({ kind: 'linked', target, modifier })
        break
      }
      default:
        throw unsupported('unsupported-message-syntax', `node type ${item.type}`)
    }
  }

  return { text, placeholders }
}

/** `formatContent()`'s emphasis pass, in its production order. */
function applyEmphasis(text) {
  return text.replace(/_([^_]*)_/g, '<strong>$1</strong>').replace(/\*([^*]*)\*/g, '<i>$1</i>')
}

function parseHtml(html) {
  const errors = []
  const fragment = parseFragment(html, {
    scriptingEnabled: false,
    onParseError: (error) => errors.push(error),
  })
  if (errors.length > 0) {
    throw unsupported('html-parse-error', errors[0].code ?? 'parse error')
  }
  return fragment
}

// `src` holds the asset-path variables, `class` may hold a style variable, and
// `style` may hold an asset URL; a placeholder anywhere else is data this AST
// cannot carry. Each of those three validates its own placeholders strictly.
const PLACEHOLDER_ATTRIBUTES = new Set(['src', 'class', 'style'])
const MAX_ATTRIBUTE_LENGTH = 240

function attributeMap(element, allowed) {
  const attributes = new Map()
  for (const attribute of element.attrs) {
    const name = attribute.name.toLowerCase()
    if (!allowed.has(name) || attribute.prefix !== undefined || attribute.namespace !== undefined) {
      throw unsupported('unsupported-attribute', `${element.tagName}[${attribute.prefix ? `${attribute.prefix}:` : ''}${name}]`)
    }
    if (!PLACEHOLDER_ATTRIBUTES.has(name) && SENTINEL_PATTERN.test(attribute.value)) {
      throw unsupported('placeholder-in-attribute', `${element.tagName}[${name}]`)
    }
    if (attribute.value.length > MAX_ATTRIBUTE_LENGTH) {
      throw unsupported('unsupported-attribute', `${element.tagName}[${name}] exceeds ${MAX_ATTRIBUTE_LENGTH} characters`)
    }
    attributes.set(name, attribute.value)
  }
  return attributes
}

/**
 * Class tokens become presentation hints. A class built from a placeholder
 * (`class='{restfulNight3Status}'`) becomes a declared style *variable* rather
 * than being dropped: the client substitutes the same value the backend sends.
 */
function styleTokens(attributes, tagName, placeholders) {
  const raw = attributes.get('class')
  if (raw === undefined) return { tokens: [], variables: [] }

  const tokens = []
  const variables = []
  for (const token of raw.split(/\s+/).filter((entry) => entry.length > 0)) {
    const placeholderMatch = token.match(
      new RegExp(`^${SENTINEL_OPEN}(\\d+)${SENTINEL_CLOSE}$`, 'u'),
    )
    if (placeholderMatch) {
      const placeholder = placeholders?.[Number(placeholderMatch[1])]
      if (placeholder?.kind !== 'variable') {
        throw unsupported('placeholder-in-attribute', `${tagName}[class]`)
      }
      variables.push({ name: placeholder.name, source: placeholder.source })
      continue
    }
    if (SENTINEL_PATTERN.test(token)) throw unsupported('placeholder-in-attribute', `${tagName}[class]`)
    if (!STYLE_TOKEN_PATTERN.test(token)) {
      throw unsupported('invalid-style-token', `${tagName}.${token}`)
    }
    tokens.push(token)
  }
  return { tokens: [...new Set(tokens)].sort(), variables }
}

/** Resolves `a/b/../c` without ever letting the result escape the asset root. */
function resolveAssetPath(base, relative) {
  const segments = []
  for (const segment of `${base}/${relative}`.split('/')) {
    if (segment === '' || segment === '.') continue
    if (segment === '..') {
      if (segments.length === 0) throw unsupported('image-path-escape', relative)
      segments.pop()
      continue
    }
    segments.push(segment)
  }
  const resolved = segments.join('/')
  if (!resolved.startsWith(ASSET_ROOT)) throw unsupported('image-path-escape', relative)
  const assetPath = resolved.slice(ASSET_ROOT.length)
  if (!IMAGE_PATH_PATTERN.test(assetPath)) {
    throw unsupported('unsupported-image-source', assetPath || relative)
  }
  return assetPath
}

/** Resolves an image source (or a CSS `url(...)`) to a semantic asset. */
function assetReference(source, placeholders, tagName) {
  const src = source.replace(/^['"]|['"]$/g, '')
  if (src === '') throw unsupported('unsupported-image-source', `${tagName} empty source`)

  const match = src.match(new RegExp(`^${SENTINEL_OPEN}(\\d+)${SENTINEL_CLOSE}(.*)$`, 'u'))
  let base
  let relative
  if (match) {
    const placeholder = placeholders[Number(match[1])]
    if (placeholder?.kind !== 'assetPath') {
      throw unsupported('placeholder-in-attribute', `src=${placeholder?.name ?? 'unknown'}`)
    }
    base = placeholder.base
    relative = match[2]
  } else if (src.startsWith(`/${ASSET_ROOT}`)) {
    base = ASSET_ROOT.replace(/\/$/, '')
    relative = src.slice(`/${ASSET_ROOT}`.length)
  } else {
    throw unsupported('unsupported-image-source', src)
  }

  if (SENTINEL_PATTERN.test(relative)) throw unsupported('placeholder-in-attribute', `${tagName} source`)

  const assetPath = resolveAssetPath(base, relative)
  return { role: IMAGE_ROLES.get(assetPath.split('/')[0]) ?? 'other', assetPath }
}

function imageNode(attributes, placeholders) {
  const { role, assetPath } = assetReference(attributes.get('src') ?? '', placeholders, 'img')
  const { tokens, variables } = styleTokens(attributes, 'img', placeholders)
  const node = { type: 'image', role, assetPath, styles: tokens }
  const alt = attributes.get('alt')
  if (alt !== undefined && alt !== '') node.alt = alt

  const width = attributes.get('width')
  if (width !== undefined) {
    if (!/^[0-9]{1,4}$/.test(width) || Number(width) < 1 || Number(width) > MAX_IMAGE_WIDTH) {
      throw unsupported('unsupported-attribute', `img[width=${width}]`)
    }
    node.width = Number(width)
  }

  const align = attributes.get('align')
  if (align !== undefined) {
    if (!IMAGE_ALIGNMENTS.has(align.toLowerCase())) {
      throw unsupported('unsupported-attribute', `img[align=${align}]`)
    }
    node.align = align.toLowerCase()
  }

  return withPresentation(node, attributes, 'img', variables, placeholders)
}

function textNodes(value, placeholders, out) {
  const parts = value.split(new RegExp(`${SENTINEL_OPEN}(\\d+)${SENTINEL_CLOSE}`, 'u'))
  for (let index = 0; index < parts.length; index += 1) {
    const part = parts[index]
    if (index % 2 === 0) {
      if (part !== '') out.push({ type: 'text', value: part })
      continue
    }
    const placeholder = placeholders[Number(part)]
    if (placeholder === undefined) throw unsupported('message-syntax-error', 'dangling placeholder')
    if (placeholder.kind === 'text') {
      out.push({ type: 'text', value: placeholder.text })
    } else if (placeholder.kind === 'linked') {
      out.push({ type: 'linked', target: placeholder.target, modifier: placeholder.modifier })
    } else if (placeholder.kind === 'assetPath') {
      throw unsupported('asset-variable-outside-image', placeholder.name)
    } else {
      out.push({
        type: 'var',
        name: placeholder.name,
        source: placeholder.source,
        role: placeholder.role,
      })
    }
  }
}

function convertChildren(parent, placeholders) {
  const out = []
  for (const child of parent.childNodes ?? []) {
    // A bare `<li>` outside any list still renders as a list item in every
    // browser; consecutive ones become one implicit list rather than an error.
    if (child.nodeName !== '#text' && child.tagName === 'li') {
      const attributes = attributeMap(child, STYLE_ATTRIBUTES)
      const item = styleTokens(attributes, 'li', placeholders)
      const entry = withPresentation(
        { styles: item.tokens, children: convertChildren(child, placeholders) },
        attributes,
        'li',
        item.variables,
        placeholders,
      )
      const last = out[out.length - 1]
      if (last !== undefined && last.type === 'list' && last.implicit === true) {
        last.items.push(entry)
      } else {
        out.push({ type: 'list', ordered: false, implicit: true, styles: [], items: [entry] })
      }
      continue
    }
    convertNode(child, placeholders, out)
  }
  return out
}

function listNode(element, placeholders, ordered, styles, styleVariables, attributes) {
  const items = []
  for (const child of element.childNodes ?? []) {
    if (child.nodeName === '#text') {
      if (child.value.trim() === '') continue
      const stray = []
      textNodes(child.value, placeholders, stray)
      if (items.length === 0) items.push({ styles: [], children: [] })
      items[items.length - 1].children.push(...stray)
      continue
    }
    if (child.nodeName === '#comment') continue

    // Content that is not an `<li>` — a nested list written directly under the
    // `<ul>`, an inline `<strong>`, or a first bullet whose `<li>` is missing —
    // is rendered by browsers alongside the preceding item. It is attached
    // there rather than refused, so no instruction is lost.
    if (child.tagName !== 'li') {
      const stray = []
      convertNode(child, placeholders, stray)
      if (items.length === 0) items.push({ styles: [], children: [] })
      items[items.length - 1].children.push(...stray)
      continue
    }
    const itemAttributes = attributeMap(child, STYLE_ATTRIBUTES)
    const item = styleTokens(itemAttributes, 'li', placeholders)
    items.push(
      withPresentation(
        { styles: item.tokens, children: convertChildren(child, placeholders) },
        itemAttributes,
        'li',
        item.variables,
      ),
    )
  }
  return withPresentation({ type: 'list', ordered, styles, items }, attributes, ordered ? 'ol' : 'ul', styleVariables)
}

function convertNode(node, placeholders, out) {
  if (node.nodeName === '#text') {
    textNodes(node.value, placeholders, out)
    return
  }
  if (node.nodeName === '#comment') return
  if (node.nodeName === '#documentType' || node.tagName === undefined) {
    throw unsupported('unsupported-element', node.nodeName)
  }

  const tagName = node.tagName
  const allowed = allowedAttributesFor(tagName)
  if (allowed === null) throw unsupported('unsupported-element', tagName)
  const attributes = attributeMap(node, allowed)

  if (tagName === 'br') {
    out.push({ type: 'break' })
    return
  }
  if (tagName === 'hr') {
    out.push({ type: 'rule' })
    return
  }
  if (tagName === 'img') {
    out.push(imageNode(attributes, placeholders))
    return
  }
  if (tagName === 'p') {
    const { tokens, variables } = styleTokens(attributes, tagName, placeholders)
    out.push(
      withPresentation(
        { type: 'paragraph', styles: tokens, children: convertChildren(node, placeholders) },
        attributes,
        tagName,
        variables,
      ),
    )
    return
  }
  if (HEADING_ELEMENTS.has(tagName)) {
    const { tokens, variables } = styleTokens(attributes, tagName, placeholders)
    out.push(
      withPresentation(
        {
          type: 'heading',
          level: HEADING_ELEMENTS.get(tagName),
          styles: tokens,
          children: convertChildren(node, placeholders),
        },
        attributes,
        tagName,
        variables,
      ),
    )
    return
  }
  if (tagName === 'ul' || tagName === 'ol') {
    const { tokens, variables } = styleTokens(attributes, tagName, placeholders)
    out.push(listNode(node, placeholders, tagName === 'ol', tokens, variables, attributes))
    return
  }
  if (EMPHASIS_ELEMENTS.has(tagName)) {
    out.push({
      type: 'emphasis',
      style: EMPHASIS_ELEMENTS.get(tagName),
      children: convertChildren(node, placeholders),
    })
    return
  }
  if (GROUP_ELEMENTS.has(tagName)) {
    const { tokens, variables } = styleTokens(attributes, tagName, placeholders)
    const children = convertChildren(node, placeholders)

    // `data-image-id` marks text that names a specific card; the web client
    // uses it to show that card's art on hover. The catalog keeps it as a
    // semantic card reference — a code, never an image.
    const cardCode = attributes.get('data-image-id')
    if (cardCode !== undefined) {
      if (!CARD_CODE_PATTERN.test(cardCode)) {
        throw unsupported('unsupported-attribute', `${tagName}[data-image-id=${cardCode}]`)
      }
      out.push(
        withPresentation(
          { type: 'cardRef', code: cardCode, styles: tokens, children },
          attributes,
          tagName,
          variables,
        ),
      )
      return
    }

    out.push(withPresentation({ type: 'group', styles: tokens, children }, attributes, tagName, variables))
    return
  }
  // Unreachable while allowedAttributesFor() and this dispatch agree; kept so
  // adding a tag to one without the other fails closed.
  throw unsupported('unsupported-element', tagName)
}

function sortedVariables(variables) {
  return [...variables.values()].sort(
    (a, b) => a.source.localeCompare(b.source) || a.name.localeCompare(b.name),
  )
}

/**
 * Declares one variable, refusing the entry outright if the same name is used
 * in two incompatible ways (say an icon placeholder that is also a linked
 * message key) rather than letting the last occurrence win and publishing a
 * declaration that contradicts the nodes.
 */
function declareVariable(into, declaration) {
  const id = `${declaration.source}:${declaration.name}`
  const existing = into.get(id)
  if (existing !== undefined && existing.role !== declaration.role) {
    throw unsupported('conflicting-variable-role', `${id} is both ${existing.role} and ${declaration.role}`)
  }
  into.set(id, declaration)
}

function collectVariables(nodes, into) {
  for (const node of nodes) {
    for (const variable of node.styleVars ?? []) {
      declareVariable(into, { name: variable.name, source: variable.source, role: 'text' })
    }
    if (node.type === 'var') {
      declareVariable(into, { name: node.name, source: node.source, role: node.role })
    } else if (node.type === 'linked') {
      if (node.target.kind === 'variable') {
        declareVariable(into, {
          name: node.target.name,
          source: node.target.source,
          role: 'text',
        })
      }
    } else if (node.type === 'list') {
      for (const item of node.items) {
        for (const variable of item.styleVars ?? []) {
          declareVariable(into, { name: variable.name, source: variable.source, role: 'text' })
        }
        collectVariables(item.children, into)
      }
    } else if (Array.isArray(node.children)) {
      collectVariables(node.children, into)
    }
  }
}

function normalizeBody(items, classifyVariable) {
  const { text, placeholders } = substitutePlaceholders(items, classifyVariable)
  const fragment = parseHtml(applyEmphasis(text))
  return convertChildren(fragment, placeholders)
}

/**
 * Normalizes one locale message.
 *
 * Returns `{ form: 'message' | 'plural', ... }` on success, or
 * `{ form: 'unsupported', reason, detail }` when the message uses markup or
 * message syntax this catalog revision deliberately refuses to reinterpret.
 */
export function normalizeMessage(raw, options = {}) {
  const classifyVariable = options.classifyVariable ?? defaultClassifyVariable
  try {
    if (typeof raw !== 'string') throw unsupported('message-syntax-error', `non-string value (${typeof raw})`)
    assertSentinelFree(raw)

    const ast = parseMessage(raw)
    const body = ast.body
    const variables = new Map()

    if (body.type === NODE_PLURAL) {
      const cases = body.cases.map((branch) => normalizeBody(branch.items, classifyVariable))
      for (const nodes of cases) collectVariables(nodes, variables)
      return { form: 'plural', cases, variables: sortedVariables(variables) }
    }

    const nodes = normalizeBody(body.items, classifyVariable)
    collectVariables(nodes, variables)
    return { form: 'message', nodes, variables: sortedVariables(variables) }
  } catch (error) {
    if (error instanceof Unsupported) {
      return { form: 'unsupported', reason: error.reason, detail: error.detail }
    }
    throw error
  }
}

function walkNodes(nodes, visit) {
  for (const node of nodes) {
    visit(node)
    if (node.type === 'list') {
      for (const item of node.items) walkNodes(item.children, visit)
    } else if (Array.isArray(node.children)) {
      walkNodes(node.children, visit)
    }
  }
}

/** Every variable a normalized node tree references, including style variables. */
export function walkAllNodes(nodes, visit) {
  walkNodes(nodes, visit)
}

/** Every statically-referenced linked key in a normalized entry. */
export function staticLinkTargets(entry) {
  const targets = new Set()
  const visit = (node) => {
    if (node.type === 'linked' && node.target.kind === 'static') targets.add(node.target.key)
  }
  if (entry.form === 'message') walkNodes(entry.nodes, visit)
  else if (entry.form === 'plural') for (const nodes of entry.cases) walkNodes(nodes, visit)
  return [...targets].sort()
}

/** Every variable referenced by a normalized entry's nodes, for declaration checks. */
export function referencedVariables(entry) {
  const referenced = new Map()
  const visit = (node) => {
    for (const variable of node.styleVars ?? []) {
      referenced.set(`${variable.source}:${variable.name}`, {
        name: variable.name,
        source: variable.source,
        role: 'text',
      })
    }
    for (const item of node.items ?? []) {
      for (const variable of item.styleVars ?? []) {
        referenced.set(`${variable.source}:${variable.name}`, {
          name: variable.name,
          source: variable.source,
          role: 'text',
        })
      }
    }
    if (node.type === 'var') {
      referenced.set(`${node.source}:${node.name}`, { name: node.name, source: node.source, role: node.role })
    } else if (node.type === 'linked' && node.target.kind === 'variable') {
      referenced.set(`${node.target.source}:${node.target.name}`, {
        name: node.target.name,
        source: node.target.source,
        role: 'text',
      })
    }
  }
  if (entry.form === 'message') walkNodes(entry.nodes, visit)
  else if (entry.form === 'plural') for (const nodes of entry.cases) walkNodes(nodes, visit)
  return referenced
}
