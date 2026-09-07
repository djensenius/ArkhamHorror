// Invocation-owned scratch directory for the bundle Vite writes and the
// generator then imports.
//
// This is ordinary trusted repository code; the point of the ownership token is
// operational, not adversarial. Two properties matter:
//
//   * Cleanup happens on success and on every failure after creation, so a
//     failed generation does not leave a few hundred megabytes of copied
//     `public/` assets behind. (It did: `emptyOutDir` erased the marker that
//     used to live inside the output directory, so release refused to clean up
//     and 92 directories accumulated.)
//   * Cleanup removes only the directory this call created. The token is
//     re-read immediately before removal, so a path that is not ours -- one
//     that was replaced, or a caller passing something else -- is left alone.
//
// The layout is therefore a *parent* the generator owns, holding the marker,
// with a child `out/` handed to Vite. `emptyOutDir: true` wipes the child; it
// cannot reach the marker.

import { randomUUID } from 'node:crypto'
import { mkdirSync, mkdtempSync, readFileSync, realpathSync, rmSync, writeFileSync } from 'node:fs'
import { join, resolve } from 'node:path'

import { registerDerivedOutputRoot } from './generator-launcher.mjs'

const OWNER_FILE = '.locale-catalog-build-owner'
const OUTPUT_CHILD = 'out'

/**
 * Create an owned build root beside `node_modules`.
 *
 * It stays under `frontend/` because Node resolves the bundle's bare imports by
 * walking up to `frontend/node_modules`, and it stays *outside* `node_modules`
 * because derived executable output does not belong in the tree
 * `package-lock.json` describes.
 *
 * @param {string} frontendDir
 * @returns {{ root: string, out: string, token: string }}
 */
export function createOwnedBuildDirectory(frontendDir) {
  const root = realpathSync(mkdtempSync(join(resolve(frontendDir), '.locale-catalog-build-')))
  const token = randomUUID()
  try {
    writeFileSync(join(root, OWNER_FILE), token, { encoding: 'utf8' })
    mkdirSync(join(root, OUTPUT_CHILD))
  } catch (error) {
    // Never leave a half-built root behind: it would have no usable marker.
    rmSync(root, { recursive: true, force: true })
    throw error
  }
  registerDerivedOutputRoot(root)
  return { root, out: join(root, OUTPUT_CHILD), token }
}

/**
 * Remove an owned build root, only after re-proving this call owns it.
 *
 * @param {{ root: string, token: string }} owned
 * @returns {boolean} whether the directory was removed
 */
export function releaseOwnedBuildDirectory(owned) {
  if (!owned || typeof owned.root !== 'string' || typeof owned.token !== 'string') return false
  let recorded
  try {
    recorded = readFileSync(join(owned.root, OWNER_FILE), 'utf8')
  } catch {
    return false
  }
  if (recorded !== owned.token) return false
  rmSync(owned.root, { recursive: true, force: true })
  return true
}
