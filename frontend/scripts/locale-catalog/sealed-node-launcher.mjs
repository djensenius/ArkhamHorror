// Sealed launcher for every governed locale-catalog Node entry point.
//
// This file is part of the audited trusted computing base (TCB), together with
// the two shell launcher stages, `scripts/locale_catalog_runtime.py`,
// `scripts/locale_catalog_python_boundary.py` and
// `scripts/locale_catalog_python_runtime.json`. TCB changes are governed by
// ordinary human review and exact provenance; nothing here pretends to
// authenticate itself.
//
// What it does enforce, for everything *outside* the TCB (T1: hostile or
// mistaken committed generator source):
//
//   * The module graph is bound at run time, not by reading source text. Node's
//     synchronous `module.registerHooks` resolve/load hooks are installed
//     before the entry module is imported, so whitespace, comments, computed
//     specifiers and dynamic `import()` are all subject to the same rule --
//     there is no syntax to hide behind.
//   * A repository module may be imported only if it is in the committed
//     allowlist next to this file and its bytes still hash to the committed
//     digest. Loading serves those exact bytes.
//   * `node:` builtins are permitted. Everything under `frontend/node_modules`
//     is permitted as a class: it is bound by `package-lock.json`, which the
//     Python preflight hashes, and it is also where the generator's own
//     invocation-owned Vite output lives.
//   * Every other resolution -- a repository file outside the allowlist, an
//     absolute path, a `data:` URL, an unsupported scheme -- is refused.
//
// What it is not: a JavaScript capability sandbox. Allowed generator code runs
// with full Node privileges. The guarantee is that the graph which runs is the
// committed, hashed one, so provenance describes what executed.

import { createHash } from 'node:crypto'
import { readFileSync, realpathSync } from 'node:fs'
import { registerHooks } from 'node:module'
import { dirname, join, resolve, sep } from 'node:path'
import { fileURLToPath, pathToFileURL } from 'node:url'

function realOrSelf(path) {
  try {
    return realpathSync(path)
  } catch {
    return path
  }
}

const HERE = dirname(fileURLToPath(import.meta.url))
const FRONTEND = resolve(HERE, '..', '..')
// Node resolves symlinks before it hands a path to a loader hook, and the
// catalog validator runs the generator inside a clean clone whose
// `node_modules` is a link back to the installed tree. Both spellings of the
// locked dependency root are therefore accepted, and only those two.
const NODE_MODULES_ROOTS = [
  join(FRONTEND, 'node_modules') + sep,
  realOrSelf(join(FRONTEND, 'node_modules')) + sep,
]
const ALLOWLIST_FILE = join(HERE, 'sealed-node-allowlist.json')
const SELF = fileURLToPath(import.meta.url)

class SealedImportError extends Error {}

function fail(message) {
  throw new SealedImportError(`locale-catalog node launcher: ${message}`)
}

function sha256Hex(data) {
  return createHash('sha256').update(data).digest('hex')
}

function readAllowlist() {
  const declared = JSON.parse(readFileSync(ALLOWLIST_FILE, 'utf8'))
  if (typeof declared !== 'object' || declared === null) fail('the module allowlist is not an object')
  const entries = new Map()
  for (const [name, digest] of Object.entries(declared)) {
    if (name.includes('/') || name.includes('\\') || name === '..') {
      fail(`allowlist entry ${name} must be a plain file name beside the launcher`)
    }
    if (typeof digest !== 'string' || !/^[0-9a-f]{64}$/.test(digest)) {
      fail(`allowlist entry ${name} has no exact sha256 digest`)
    }
    entries.set(join(HERE, name), digest)
  }
  if (entries.size === 0) fail('the module allowlist is empty')
  return entries
}

/** Hash every allowed file and refuse on any mismatch. */
function verifyAllowlist(entries, when) {
  for (const [path, digest] of entries) {
    let actual
    try {
      actual = sha256Hex(readFileSync(path))
    } catch (error) {
      fail(`allowed module ${path} is unreadable ${when}: ${error.message}`)
    }
    if (actual !== digest) fail(`allowed module ${path} does not match its committed digest ${when}`)
  }
}

function decidePath(path, entries) {
  if (path === SELF || realOrSelf(path) === realOrSelf(SELF)) return 'launcher'
  for (const root of NODE_MODULES_ROOTS) {
    if (path.startsWith(root)) return 'locked'
  }
  if (entries.has(path)) return 'allowlisted'
  return null
}

function installHooks(entries) {
  registerHooks({
    resolve(specifier, context, nextResolve) {
      if (specifier.startsWith('node:')) return nextResolve(specifier, context)
      const resolved = nextResolve(specifier, context)
      const url = new URL(resolved.url)
      if (url.protocol === 'node:') return resolved
      if (url.protocol !== 'file:') {
        fail(`refusing to resolve ${specifier} to unsupported scheme ${url.protocol}`)
      }
      const path = fileURLToPath(url)
      if (decidePath(path, entries) === null) {
        fail(
          `refusing to import ${specifier} -> ${path}: it is neither a node: builtin, a ` +
            'locked package, nor a committed allowlisted generator module',
        )
      }
      return resolved
    },
    load(url, context, nextLoad) {
      const parsed = new URL(url)
      if (parsed.protocol === 'node:') return nextLoad(url, context)
      if (parsed.protocol !== 'file:') fail(`refusing to load unsupported scheme ${parsed.protocol}`)
      const path = fileURLToPath(parsed)
      const decision = decidePath(path, entries)
      if (decision === null) fail(`refusing to load ${path}`)
      if (decision === 'allowlisted') {
        const source = readFileSync(path)
        if (sha256Hex(source) !== entries.get(path)) {
          fail(`allowed module ${path} changed between verification and load`)
        }
        return { format: path.endsWith('.json') ? 'json' : 'module', shortCircuit: true, source }
      }
      return nextLoad(url, context)
    },
  })
}

async function main() {
  const [entryName, ...forwarded] = process.argv.slice(2)
  if (!entryName) fail('expected the name of a governed generator entry module')
  const entries = readAllowlist()
  const entryPath = join(HERE, entryName)
  if (!entries.has(entryPath)) fail(`${entryName} is not a committed generator entry module`)
  verifyAllowlist(entries, 'before the generator ran')
  installHooks(entries)

  // The entry module decides it is the entry by comparing `process.argv[1]`
  // with its own resolved URL, so hand it that exact path.
  process.argv = [process.argv[0], entryPath, ...forwarded]
  try {
    await import(pathToFileURL(entryPath).href)
  } finally {
    verifyAllowlist(entries, 'after the generator ran')
  }
}

main().catch((error) => {
  console.error(error instanceof SealedImportError ? error.message : error)
  process.exitCode = 1
})
