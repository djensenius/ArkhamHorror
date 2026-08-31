// Deterministic serialization helpers shared by the locale-catalog generator,
// its verifier, and its tests.
//
// Every published byte has to be reproducible from the committed locale
// sources alone: the catalog revision is a digest over those bytes, and the
// manifest pins each chunk's SHA-256. `canonicalJson` is therefore the only
// way this toolchain is allowed to turn a value into bytes.

import { createHash } from 'node:crypto'

function canonicalize(value, path) {
  if (value === null || typeof value === 'string' || typeof value === 'boolean') return value
  if (typeof value === 'number') {
    if (!Number.isInteger(value)) {
      throw new Error(`canonicalJson: non-integer number at ${path} (${value})`)
    }
    if (!Number.isSafeInteger(value)) {
      throw new Error(`canonicalJson: unsafe integer at ${path} (${value})`)
    }
    return value
  }
  if (Array.isArray(value)) return value.map((item, index) => canonicalize(item, `${path}[${index}]`))
  if (typeof value === 'object') {
    const out = {}
    // Sorted by UTF-16 code unit, which is what both JSON.stringify's key
    // iteration and a consumer's binary search agree on.
    for (const key of Object.keys(value).sort()) {
      if (value[key] === undefined) continue
      out[key] = canonicalize(value[key], `${path}.${key}`)
    }
    return out
  }
  throw new Error(`canonicalJson: unsupported value type ${typeof value} at ${path}`)
}

/** Canonical, byte-stable JSON: sorted keys, no insignificant whitespace, trailing newline. */
export function canonicalJson(value) {
  return `${JSON.stringify(canonicalize(value, '$'))}\n`
}

export function sha256Hex(input) {
  return createHash('sha256').update(input).digest('hex')
}

export function sha256OfJson(value) {
  return sha256Hex(Buffer.from(canonicalJson(value), 'utf8'))
}
