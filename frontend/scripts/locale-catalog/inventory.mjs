// Source-integrity checks that must run *before* the catalog is built.
//
// Two silent-loss modes exist upstream of the generator and neither is visible
// in the composed message tree Vue evaluates:
//
//   1. A locale JSON file can declare the same key twice. `JSON.parse` (and
//      therefore Vite, and therefore the Vue build) keeps the last one and
//      discards the first without a word.
//   2. The per-locale `.ts` modules compose their sources with object spreads
//      (`{...base, ...event, ...}`) and the homebrew glob merges several files
//      into one scope. Two contributors claiming the same key silently
//      override each other.
//
// Both are detected here from the raw bytes and from the evaluated
// per-file trees, with the owning file (and, for duplicates, the exact
// offset) reported, so the catalog can fail closed instead of publishing a
// tree that quietly dropped a translation.

import { visit } from 'jsonc-parser'

/**
 * Reports every duplicate key in one raw locale JSON document, including keys
 * that are only equal after unescaping (`"a\u002Eb"` vs `"a.b"`), with the
 * full path and 1-based line/column of both occurrences.
 */
export function findDuplicateKeys(text, file) {
  const duplicates = []
  const path = []
  const seen = [new Map()]
  const lineStarts = [0]
  for (let index = 0; index < text.length; index += 1) {
    if (text[index] === '\n') lineStarts.push(index + 1)
  }
  const position = (offset) => {
    let low = 0
    let high = lineStarts.length - 1
    while (low < high) {
      const middle = Math.ceil((low + high) / 2)
      if (lineStarts[middle] <= offset) low = middle
      else high = middle - 1
    }
    return { line: low + 1, column: offset - lineStarts[low] + 1 }
  }

  visit(text, {
    onObjectBegin: () => {
      seen.push(new Map())
    },
    onObjectEnd: () => {
      seen.pop()
      path.pop()
    },
    onObjectProperty: (property, offset) => {
      const scope = seen[seen.length - 1]
      const previous = scope.get(property)
      const where = position(offset)
      if (previous !== undefined) {
        duplicates.push({
          file,
          key: [...path.slice(0, seen.length - 2), property].join('.'),
          first: previous,
          second: where,
        })
      } else {
        scope.set(property, where)
      }
      path[seen.length - 2] = property
      path.length = seen.length - 1
    },
    onArrayBegin: () => {
      seen.push(new Map())
    },
    onArrayEnd: () => {
      seen.pop()
      path.pop()
    },
    onError: () => {},
  })

  return duplicates
}

function leafEntries(value, prefix, out) {
  if (value === null || typeof value !== 'object') {
    out.set(prefix, value)
    return out
  }
  if (Array.isArray(value)) {
    value.forEach((item, index) => leafEntries(item, prefix === '' ? String(index) : `${prefix}.${index}`, out))
    return out
  }
  for (const [key, child] of Object.entries(value)) {
    leafEntries(child, prefix === '' ? key : `${prefix}.${key}`, out)
  }
  return out
}

function valueAt(tree, path) {
  let current = tree
  for (const segment of path.split('.')) {
    if (current === null || typeof current !== 'object') return undefined
    current = Array.isArray(current) ? current[Number(segment)] : current[segment]
    if (current === undefined) return undefined
  }
  return current
}

/**
 * Detects content a locale source file contributed that the composed tree does
 * not carry — the signature of one contributor overriding another through a
 * spread or a homebrew scope merge.
 *
 * A file's mount point is discovered, not assumed: sample leaves are looked up
 * by value in an index of the composed tree, and the mount is the prefix those
 * matches agree on. A file whose mount cannot be determined that way (nothing
 * it contributes survives anywhere) is reported as fully lost.
 */
export function findCompositionLosses(composed, files, ownedPaths) {
  const byValue = new Map()
  for (const [path, value] of leafEntries(composed, '', new Map())) {
    if (typeof value !== 'string' || value.length < 4) continue
    if (!byValue.has(value)) byValue.set(value, [])
    byValue.get(value).push(path)
  }

  const losses = []
  for (const { path, tree } of files) {
    if (tree === null || typeof tree !== 'object') continue
    const leaves = leafEntries(tree, '', new Map())
    if (leaves.size === 0) continue

    // Candidate mounts: for each sampled leaf, every composed path carrying
    // the same value, minus that leaf's own path.
    const votes = new Map()
    let sampled = 0
    for (const [leaf, value] of leaves) {
      if (typeof value !== 'string' || value.length < 4) continue
      const matches = byValue.get(value)
      if (matches === undefined) continue
      sampled += 1
      for (const match of matches) {
        if (match !== leaf && !match.endsWith(`.${leaf}`)) continue
        const prefix = match === leaf ? '' : match.slice(0, match.length - leaf.length - 1)
        votes.set(prefix, (votes.get(prefix) ?? 0) + 1)
      }
      if (sampled >= 12) break
    }

    // No votes means nothing this file contributes is reachable in the
    // composed tree. For a file that belongs to this locale that is an orphan
    // — a translated file no module imports, so the app never shows it. For a
    // file from elsewhere (a homebrew campaign only the default locale
    // composes) it is expected and not claimed as a finding.
    if (votes.size === 0) {
      const hasText = [...leaves.values()].some(
        (value) => typeof value === 'string' && value.length >= 4,
      )
      if (hasText && ownedPaths?.has(path)) {
        losses.push({
          file: path,
          mount: '<orphaned-or-overridden>',
          missing: [...leaves.keys()].slice(0, 8),
          missingCount: leaves.size,
          overridden: [],
          overriddenCount: 0,
        })
      }
      continue
    }

    const ranked = [...votes.entries()].sort((a, b) => b[1] - a[1] || a[0].length - b[0].length)
    const [mount, agreement] = ranked[0]
    // An ambiguous mount would produce noise, not findings: the winning mount
    // must be agreed on by at least two sampled leaves (or by the only one
    // available) and must hold a majority of the votes.
    if (agreement < Math.min(2, sampled) || agreement * 2 < sampled) continue
    const root = mount === '' ? composed : valueAt(composed, mount)
    if (root === null || typeof root !== 'object') continue

    const missing = []
    const overridden = []
    for (const [leaf, value] of leaves) {
      const found = valueAt(root, leaf)
      if (found === undefined) missing.push(leaf)
      else if (found !== value && (typeof found !== 'object' || found === null)) overridden.push(leaf)
    }

    if (missing.length > 0 || overridden.length > 0) {
      losses.push({
        file: path,
        mount: mount === '' ? '<root>' : mount,
        missing: missing.slice(0, 8),
        missingCount: missing.length,
        overridden: overridden.slice(0, 8),
        overriddenCount: overridden.length,
      })
    }
  }

  return losses
}
