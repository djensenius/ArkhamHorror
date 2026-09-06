// The single production entry point for every locale-catalog Node command.
//
// This is NOT a JavaScript sandbox. The generator, the locale modules and every
// module this launcher permits run with full Node privileges, and all of that
// code is trusted: it is committed to this repository and reviewed through pull
// request. A malicious commit is out of scope.
//
// What it is for:
//
//   * Centralisation. Every production path -- the mise task, the catalog
//     validator, the serving gate, npm's `prebuild`, the container build, the
//     offline build and packaging -- starts here, so there is one place where
//     generation behaviour is defined instead of six.
//   * Drift detection. The module graph is bound at run time by Node's
//     synchronous `module.registerHooks`, so a module that is not in the
//     committed digest list -- or whose bytes no longer match it -- stops the
//     run and gets reported, whatever import syntax reached for it. Comments,
//     leading whitespace, computed specifiers and dynamic `import()` all end up
//     at the same resolver.
//   * Exact input identity. Allowed modules are served from the bytes that were
//     hashed, and those digests are folded into the catalog's provenance, so a
//     published catalog names the generator revision that produced it.
//
// Permitted resolutions are `node:` builtins, paths under
// `frontend/node_modules` (whose contents come from `package-lock.json`), the
// modules listed in `generator-module-digests.json`, and this invocation's own
// derived build output directory, which the generator creates privately and
// removes with an ownership check. That last root is *derived output*, not a
// locked artifact: nothing claims `package-lock.json` authenticates it.

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
const DIGEST_FILE = join(HERE, 'generator-module-digests.json')
const SELF = fileURLToPath(import.meta.url)

class ModuleGraphError extends Error {}

function fail(message) {
  throw new ModuleGraphError(`locale-catalog generator launcher: ${message}`)
}

function sha256Hex(data) {
  return createHash('sha256').update(data).digest('hex')
}

function readModuleDigests() {
  const declared = JSON.parse(readFileSync(DIGEST_FILE, 'utf8'))
  if (typeof declared !== 'object' || declared === null) fail('the generator module digest list is not an object')
  const entries = new Map()
  for (const [name, digest] of Object.entries(declared)) {
    if (name.includes('/') || name.includes('\\') || name === '..') {
      fail(`digest entry ${name} must be a plain file name beside the launcher`)
    }
    if (typeof digest !== 'string' || !/^[0-9a-f]{64}$/.test(digest)) {
      fail(`digest entry ${name} has no exact sha256 digest`)
    }
    entries.set(join(HERE, name), digest)
  }
  if (entries.size === 0) fail('the generator module digest list is empty')
  return entries
}

/** Hash every allowed file and refuse on any mismatch. */
function verifyModuleDigests(entries, when) {
  for (const [path, digest] of entries) {
    let actual
    try {
      actual = sha256Hex(readFileSync(path))
    } catch (error) {
      fail(`listed generator module ${path} is unreadable ${when}: ${error.message}`)
    }
    if (actual !== digest) fail(`listed generator module ${path} does not match its committed digest ${when}`)
  }
}

// This invocation's own derived build output. The generator creates it
// privately (mkdtemp, 0700) and removes it with an ownership check; it is
// registered here so the bundle Vite just wrote can be imported. It is derived
// output, not a locked dependency: package-lock.json says nothing about it.
const derivedOutputRoots = new Set()

export function registerDerivedOutputRoot(path) {
  derivedOutputRoots.add(realOrSelf(path) + sep)
}

function decidePath(path, entries) {
  if (path === SELF || realOrSelf(path) === realOrSelf(SELF)) return 'launcher'
  for (const root of NODE_MODULES_ROOTS) {
    if (path.startsWith(root)) return 'locked'
  }
  for (const root of derivedOutputRoots) {
    if (path.startsWith(root)) return 'derived'
  }
  if (entries.has(path)) return 'listed'
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
            'locked package, nor a generator module listed in generator-module-digests.json',
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
      if (decision === 'listed') {
        const source = readFileSync(path)
        if (sha256Hex(source) !== entries.get(path)) {
          fail(`listed generator module ${path} changed between verification and load`)
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
  const entries = readModuleDigests()
  const entryPath = join(HERE, entryName)
  if (!entries.has(entryPath)) fail(`${entryName} is not a listed generator entry module`)
  verifyModuleDigests(entries, 'before the generator ran')
  installHooks(entries)

  // The entry module decides it is the entry by comparing `process.argv[1]`
  // with its own resolved URL, so hand it that exact path.
  process.argv = [process.argv[0], entryPath, ...forwarded]
  try {
    await import(pathToFileURL(entryPath).href)
  } finally {
    verifyModuleDigests(entries, 'after the generator ran')
  }
}

// Only launch when this file *is* the entry module. The generator imports
// `registerDerivedOutputRoot` from here, and the frontend's own trusted tests
// import the generator modules directly; neither should start a run.
if (process.argv[1] === SELF || realOrSelf(process.argv[1] ?? '') === realOrSelf(SELF)) {
  main().catch((error) => {
    console.error(error instanceof ModuleGraphError ? error.message : error)
    process.exitCode = 1
  })
}
