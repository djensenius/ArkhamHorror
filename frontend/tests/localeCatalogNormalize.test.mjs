// Unit tests for the locale-catalog render AST.
//
// Every message here is synthetic. Real scenario prose is never copied into
// this file; the tests that need real content (localeCatalogGenerate.test.mjs)
// read it from the committed locale sources instead.

import assert from 'node:assert/strict'
import { readFileSync } from 'node:fs'
import { resolve } from 'node:path'
import test from 'node:test'

import { createParser } from '@intlify/message-compiler'

import { makeVariableClassifier } from '../scripts/locale-catalog/sources.mjs'
import {
  ASSET_PATH_VARIABLES,
  UNSUPPORTED_REASONS,
  defaultClassifyVariable,
  normalizeMessage,
  referencedVariables,
  staticLinkTargets,
} from '../scripts/locale-catalog/normalize.mjs'

// Mirrors what makeVariableClassifier() derives from production
// `formatContent`/`replaceIcons`, without bundling the app for a unit test.
const classifyVariable = (name) => {
  if (name === 'skull') return { role: 'icon' }
  if (name === 'asterisk') return { role: 'literal', text: '*' }
  return defaultClassifyVariable(name)
}

const normalize = (raw) => normalizeMessage(raw, { classifyVariable })

test('the pinned @intlify node type numbers still match the real parser', () => {
  const parser = createParser({ location: false })
  const items = parser.parse("text {named} {0} {'{'} @:other.key").body.items
  assert.deepEqual(
    items.map((item) => item.type),
    [3, 4, 3, 5, 3, 9, 3, 6],
  )
  assert.equal(parser.parse('a | b').body.type, 1)
})

test('plain text, line breaks, and horizontal rules', () => {
  assert.deepEqual(normalize('Alpha<br />Beta<hr>Gamma'), {
    form: 'message',
    variables: [],
    nodes: [
      { type: 'text', value: 'Alpha' },
      { type: 'break' },
      { type: 'text', value: 'Beta' },
      { type: 'rule' },
      { type: 'text', value: 'Gamma' },
    ],
  })
})

test('paragraphs and headings keep their style hints', () => {
  const result = normalize("<p class='basic right'>Alpha</p><h3 class='term'>Beta</h3>")
  assert.deepEqual(result.nodes, [
    { type: 'paragraph', styles: ['basic', 'right'], children: [{ type: 'text', value: 'Alpha' }] },
    { type: 'heading', level: 3, styles: ['term'], children: [{ type: 'text', value: 'Beta' }] },
  ])
})

test('markdown-style and tag-based emphasis both normalize', () => {
  const result = normalize('*italic* _bold_ <b>b</b><em>e</em><u>u</u><s>s</s><minicaps>m</minicaps>')
  assert.deepEqual(
    result.nodes.filter((node) => node.type === 'emphasis').map((node) => node.style),
    ['italic', 'bold', 'bold', 'italic', 'underline', 'strikethrough', 'smallCaps'],
  )
})

test('lists keep item order, nesting, and style hints', () => {
  const result = normalize("<ul class='level-2'><li class='basic'>One</li><li>Two<ol><li>Two.a</li></ol></li></ul>")
  assert.deepEqual(result.nodes, [
    {
      type: 'list',
      ordered: false,
      styles: ['level-2'],
      items: [
        { styles: ['basic'], children: [{ type: 'text', value: 'One' }] },
        {
          styles: [],
          children: [
            { type: 'text', value: 'Two' },
            {
              type: 'list',
              ordered: true,
              styles: [],
              items: [{ styles: [], children: [{ type: 'text', value: 'Two.a' }] }],
            },
          ],
        },
      ],
    },
  ])
})

test('elements refuse attributes their node cannot carry', () => {
  for (const input of [
    "<b class='x'>bold</b>",
    "<br class='x'>",
    "<hr class='x'>",
    "<p src='{setImgPath}/rats.png'>x</p>",
    "<div alt='x'>y</div>",
    '<ul><li alt="x">y</li></ul>',
    '<span src="x">y</span>',
  ]) {
    const result = normalize(input)
    assert.equal(result.form, 'unsupported', input)
    assert.equal(result.reason, 'unsupported-attribute', input)
  }
})

test('variables are typed and declared, never interpolated', () => {
  const result = normalize('Move {count} to {name} and {0}')
  assert.deepEqual(result.variables, [
    { name: '0', source: 'list', role: 'text' },
    { name: 'count', source: 'named', role: 'text' },
    { name: 'name', source: 'named', role: 'text' },
  ])
  assert.deepEqual(
    result.nodes.filter((node) => node.type === 'var'),
    [
      { type: 'var', name: 'count', source: 'named', role: 'text' },
      { type: 'var', name: 'name', source: 'named', role: 'text' },
      { type: 'var', name: '0', source: 'list', role: 'text' },
    ],
  )
  assert.deepEqual([...referencedVariables(result).keys()].sort(), ['list:0', 'named:count', 'named:name'])
})

test('icon placeholders keep their production role', () => {
  const result = normalize('Reveal {skull}')
  assert.deepEqual(result.variables, [{ name: 'skull', source: 'named', role: 'icon' }])
})

test('literal escapes become text, never emphasis delimiters', () => {
  const result = normalize("{asterisk}not italic{asterisk} and {'{'}braced{'}'}")
  assert.deepEqual(result.nodes, [
    { type: 'text', value: '*' },
    { type: 'text', value: 'not italic' },
    { type: 'text', value: '*' },
    { type: 'text', value: ' and {braced}' },
  ])
  assert.deepEqual(result.variables, [])
})

test('linked messages stay references, with declared dynamic keys', () => {
  const stat = normalize('See @:other.key now')
  assert.deepEqual(stat.nodes[1], {
    type: 'linked',
    modifier: null,
    target: { kind: 'static', key: 'other.key' },
  })
  assert.deepEqual(staticLinkTargets(stat), ['other.key'])

  const dynamic = normalize('@.lower:{slot}')
  assert.deepEqual(dynamic.nodes, [
    { type: 'linked', modifier: 'lower', target: { kind: 'variable', name: 'slot', source: 'named' } },
  ])
  assert.deepEqual(dynamic.variables, [{ name: 'slot', source: 'named', role: 'text' }])
  assert.deepEqual(staticLinkTargets(dynamic), [])
})

test('plural branches are preserved as separate cases', () => {
  const result = normalize('one thing | {count} things')
  assert.equal(result.form, 'plural')
  assert.equal(result.cases.length, 2)
  assert.deepEqual(result.cases[0], [{ type: 'text', value: 'one thing' }])
  assert.deepEqual(result.variables, [{ name: 'count', source: 'named', role: 'text' }])
})

test('encounter-set images become semantic references, never bytes or URLs', () => {
  const result = normalize(
    "<section class='encounter-sets'><img src='{setImgPath}/rats.png' /><img src='{imgPath}/homebrew/x/sets/y.png' alt='Y' /></section>",
  )
  assert.deepEqual(result.nodes, [
    {
      type: 'group',
      styles: ['encounter-sets'],
      children: [
        { type: 'image', role: 'encounterSet', assetPath: 'encounter-sets/rats.png', styles: [] },
        { type: 'image', role: 'homebrew', assetPath: 'homebrew/x/sets/y.png', styles: [], alt: 'Y' },
      ],
    },
  ])
  assert.deepEqual(result.variables, [])
})

test('relative image paths are resolved inside the asset root', () => {
  const result = normalize("<img src='{setImgPath}/../cards/01001.avif' />")
  assert.deepEqual(result.nodes, [
    { type: 'image', role: 'card', assetPath: 'cards/01001.avif', styles: [] },
  ])
})

test('unsupported and unsafe input is refused, never partially rendered', () => {
  const cases = [
    ['<script>alert(1)</script>', 'unsupported-element'],
    ['<style>p{color:red}</style>', 'message-syntax-error'],
    ['<style>p:hover</style>', 'unsupported-element'],
    ['<a href="https://example.test">link</a>', 'unsupported-element'],
    ['<a href="javascript:alert(1)">link</a>', 'unsupported-element'],
    ['<p onclick="steal()">x</p>', 'unsupported-attribute'],
    ["<div style='position:fixed'>x</div>", 'unsupported-attribute'],
    ['<img src="https://evil.test/pixel.png" />', 'unsupported-image-source'],
    ['<img src="data:image/png;base64,AAAA" />', 'unsupported-image-source'],
    ["<img src='{setImgPath}/../../../../etc/passwd.png' />", 'image-path-escape'],
    ["<img src='{name}.png' />", 'placeholder-in-attribute'],
    ["<p class='{bonusClass}'>x</p>", 'placeholder-in-attribute'],
    ['{setImgPath}/loose.png', 'asset-variable-outside-image'],
    ['<table><tr><td>x</td></tr></table>', 'unsupported-element'],
    ['<ul>stray</ul>', 'misplaced-list-item'],
    ['<li>orphan</li>', 'misplaced-list-item'],
    ['<p a"b=c>x</p>', 'html-parse-error'],
    ['@.bogus:other.key', 'unsupported-message-syntax'],
    ['\uE0000\uE001', 'message-syntax-error'],
    // `skull` is an icon placeholder here and a linked message key there:
    // one declaration cannot describe both.
    ['{skull} @:{skull}', 'conflicting-variable-role'],
    // Longer than the maxLength both schemas put on a message key.
    [`@:${'a'.repeat(513)}`, 'unsupported-message-syntax'],
  ]

  for (const [input, reason] of cases) {
    const result = normalize(input)
    assert.equal(result.form, 'unsupported', `${input} should be unsupported`)
    assert.equal(result.reason, reason, `${input} reason`)
    assert.ok(UNSUPPORTED_REASONS.includes(result.reason))
    assert.ok(!('nodes' in result), 'an unsupported entry never carries partial content')
  }
})

test('non-string values are refused', () => {
  assert.equal(normalize(42).form, 'unsupported')
  assert.equal(normalize(null).form, 'unsupported')
})

test('placeholders may only appear in an image source, never another attribute', () => {
  for (const input of [
    "<img src='{setImgPath}/rats.png' alt='{name}' />",
    "<p class='{bonusClass}'>x</p>",
    "<li class='{status}'>x</li>",
  ]) {
    const result = normalize(input)
    assert.equal(result.form, 'unsupported', input)
    assert.equal(result.reason, 'placeholder-in-attribute', input)
  }
})

test('over-long attribute values are refused rather than published', () => {
  const result = normalize(`<img src='{setImgPath}/rats.png' alt='${'a'.repeat(241)}' />`)
  assert.equal(result.form, 'unsupported')
  assert.equal(result.reason, 'unsupported-attribute')
})

test('variable classification cannot be corrupted by the emphasis pass', () => {
  // Stand-ins with production's exact shape (see formatContent/replaceIcons in
  // src/arkham/helpers.ts): icons first, then emphasis, then literal escapes.
  const replaceIcons = (body) => body.replace(/\{skull\}/g, '<span class="skull-icon"></span>')
  const formatContent = (body) =>
    replaceIcons(body)
      .replace(/_([^_]*)_/g, '<strong>$1</strong>')
      .replace(/\*([^*]*)\*/g, '<i>$1</i>')
      .replace(/\{asterisk\}/g, '*')
      .replace(/\{underscore\}/g, '_')

  const classify = makeVariableClassifier({ formatContent, replaceIcons }, ASSET_PATH_VARIABLES)

  assert.deepEqual(classify('skull'), { role: 'icon' })
  assert.deepEqual(classify('asterisk'), { role: 'literal', text: '*' })
  assert.deepEqual(classify('underscore'), { role: 'literal', text: '_' })
  assert.deepEqual(classify('count'), { role: 'text' })
  assert.deepEqual(classify('setImgPath'), { role: 'assetPath', base: 'img/arkham/encounter-sets' })
  // Two underscores in a name would otherwise pair against each other in the
  // emphasis pass and be mistaken for a literal escape.
  assert.deepEqual(classify('resource_gain_count'), { role: 'text' })

  const entry = normalizeMessage('You gain {resource_gain_count} resources.', {
    classifyVariable: classify,
  })
  assert.deepEqual(entry.variables, [{ name: 'resource_gain_count', source: 'named', role: 'text' }])
  for (const node of entry.nodes) {
    if (node.type === 'text') assert.ok(!/[<>]/.test(node.value))
  }
})

test('the asset-path variables still match FormattedEntry.vue', () => {
  const source = readFileSync(resolve('src/arkham/components/FormattedEntry.vue'), 'utf8')
  assert.match(source, /imgPath: `\$\{baseUrl\}\/img\/arkham`/)
  assert.match(source, /setImgPath: `\$\{baseUrl\}\/img\/arkham\/encounter-sets`/)
  assert.deepEqual(ASSET_PATH_VARIABLES, {
    imgPath: 'img/arkham',
    setImgPath: 'img/arkham/encounter-sets',
  })
})

test('the emphasis pass still matches formatContent()', () => {
  const source = readFileSync(resolve('src/arkham/helpers.ts'), 'utf8')
  assert.match(source, /replace\(\/_\(\[\^_\]\*\)_\/g, '<strong>\$1<\/strong>'\)/)
  assert.match(source, /replace\(\/\\\*\(\[\^\\\*\]\*\)\\\*\/g, '<i>\$1<\/i>'\)/)
})
