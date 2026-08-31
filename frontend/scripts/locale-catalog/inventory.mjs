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
  if (value === null || typeof value !== 'object' || value instanceof String) {
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

/**
 * Attributes every published string to the source file that produced it, using
 * the ownership tags the module graph itself carries — not value matching.
 *
 * For each contributor this reports the leaves it lost to another file, the
 * leaves that reached no tree at all, and the files that won nothing anywhere.
 * A leaf has exactly one owner, so there is no ambiguity to shrug at: a file
 * whose declared content is not in the composed tree under its own name is a
 * finding.
 */
export function analyzeComposition(composed, files, ownerKey, moduleKey) {
  const ownerOf = new Map()
  const ownedByFile = new Map()
  // Mounts a file reaches by reference. A mount whose every leaf was
  // overridden owns nothing, so it cannot be inferred from the leaves.
  const mountedAt = new Map()
  if (moduleKey !== undefined) {
    const walkMounts = (node, path) => {
      if (node === null || typeof node !== 'object' || node instanceof String) return
      const owner = node[moduleKey]
      if (typeof owner === 'string') {
        if (!mountedAt.has(owner)) mountedAt.set(owner, new Set())
        mountedAt.get(owner).add(path)
      }
      for (const [key, value] of Object.entries(node)) {
        walkMounts(value, path === '' ? key : `${path}.${key}`)
      }
    }
    walkMounts(composed, '')
  }
  for (const [path, value] of leafEntries(composed, '', new Map())) {
    if (!(value instanceof String)) continue
    const owner = value[ownerKey]
    if (owner === undefined) continue
    ownerOf.set(path, owner)
    if (!ownedByFile.has(owner)) ownedByFile.set(owner, new Set())
    ownedByFile.get(owner).add(path)
  }

  const findings = []
  for (const { path, tree } of files) {
    if (tree === null || typeof tree !== 'object') continue
    const declared = [...leafEntries(tree, '', new Map()).keys()]
    if (declared.length === 0) continue

    const owned = ownedByFile.get(path) ?? new Set()
    if (owned.size === 0 && (mountedAt.get(path)?.size ?? 0) === 0) {
      findings.push({
        file: path,
        kind: 'no-surviving-content',
        detail: 'every key this file declares was overridden, or no module imports it',
        examples: declared.slice(0, 6),
        count: declared.length,
      })
      continue
    }

    // Mount points, derived from the paths this file actually owns. Each owned
    // path is explained by its *longest* matching declared key, so a file that
    // declares `cost.addToHand` is read as mounted at `label`, not at both
    // `label` and `label.cost`. A file may legitimately be mounted more than
    // once (an alias such as returnToTheForgottenAge), and every such mount is
    // kept.
    const declaredSet = new Set(declared)
    const byLength = [...declaredSet].sort((a, b) => b.length - a.length)
    const mounts = new Set(mountedAt.get(path) ?? [])
    for (const path_ of owned) {
      if (declaredSet.has(path_)) {
        mounts.add('')
        continue
      }
      const leaf = byLength.find((candidate) => path_.endsWith(`.${candidate}`))
      if (leaf !== undefined) mounts.add(path_.slice(0, path_.length - leaf.length - 1))
    }
    if (mounts.size === 0) {
      findings.push({
        file: path,
        kind: 'unattributable-mount',
        detail: 'the file owns published keys but none of them match its own key paths',
        examples: [...owned].slice(0, 6),
        count: owned.size,
      })
      continue
    }

    // Ownership is per (file, mount, leaf): a file mounted twice — an alias
    // such as returnToTheForgottenAge, or a shared pack included by two
    // campaigns — must survive at *every* mount. Accepting "it survived
    // somewhere" hides a mount where another file overrode it.
    const overridden = []
    const dropped = []
    for (const mount of [...mounts].sort()) {
      for (const leaf of declared) {
        const composedPath = mount === '' ? leaf : `${mount}.${leaf}`
        const owner = ownerOf.get(composedPath)
        if (owner === path) continue
        const where = mount === '' ? '<root>' : mount
        if (owner !== undefined) overridden.push(`${where}.${leaf} -> ${owner}`)
        else dropped.push(`${where}.${leaf}`)
      }
    }

    if (overridden.length > 0 || dropped.length > 0) {
      findings.push({
        file: path,
        kind: 'content-lost',
        detail: `mounted at ${[...mounts].map((mount) => mount || '<root>').join(', ')}`,
        examples: [...overridden.slice(0, 4), ...dropped.slice(0, 4)],
        count: overridden.length + dropped.length,
      })
    }
  }

  return { findings, ownedByFile }
}
