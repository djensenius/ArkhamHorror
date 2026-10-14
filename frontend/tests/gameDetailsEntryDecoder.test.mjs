import assert from 'node:assert/strict'
import { resolve } from 'node:path'
import test from 'node:test'
import { createServer } from 'vite'

test('game-list decoder accepts upstream broken rows with id, name, and error', async (t) => {
  const server = await createServer({
    root: resolve('.'),
    server: { middlewareMode: true },
    appType: 'custom',
    logLevel: 'silent',
  })
  t.after(() => server.close())

  const { gameDetailsEntryDecoder } = await server.ssrLoadModule('/src/arkham/types/Game.ts')
  const payload = {
    id: '00000000-0000-0000-0000-000000000007',
    name: 'Broken contract fixture',
    error: 'Contract fixture failed to load.',
  }

  const decoded = await gameDetailsEntryDecoder.decodePromise(payload)

  assert.deepEqual(decoded, { ...payload, tag: 'error' })
})
