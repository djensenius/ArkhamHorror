# Contract validators

`mise run contracts:openapi` and `mise run contracts:asyncapi` are required CI
checks. They used to run `npx --yes <cli>@<version>`, which pins only the
top-level CLI and resolves every transitive dependency from the live npm
registry on each run; a registry race once failed the AsyncAPI job with
`ETARGET`. Each CLI now lives in its own private package here with an exact
version and a committed `package-lock.json`, and the tasks install it with
`npm ci --ignore-scripts`, so every transitive version and integrity hash is
fixed.

- `openapi/`: `@redocly/cli`
- `asyncapi/`: `@asyncapi/cli`

Dependabot (`.github/dependabot.yml`) proposes weekly updates for both
directories.

## Bumping a CLI manually

1. Edit the exact version (no `^` or `~`) in `<dir>/package.json`.
2. Regenerate the lockfile with the Node pinned in `mise.toml` and its bundled
   npm (the npm that CI uses):

   ```sh
   mise exec -- npm --prefix tools/contract-validators/<dir> install --package-lock-only --ignore-scripts
   ```

   If a global mise config overrides `npm`, run the bundled one instead:
   `mise exec -- node "$(mise where node)/lib/node_modules/npm/bin/npm-cli.js" ...`.

3. Run the check: `mise run contracts:openapi` or `mise run contracts:asyncapi`.
4. Commit `package.json` and `package-lock.json` together. Never commit
   `node_modules/`.
