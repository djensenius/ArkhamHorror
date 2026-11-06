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
directories. Dependency-graph or vulnerability alerts for anything under
`tools/contract-validators/*` concern CI-only validator tooling, never the
shipped app, and should be triaged as such.

Both tasks set environment variables that the pinned CLIs honour to skip
usage telemetry and update checks: `REDOCLY_TELEMETRY=off` and
`REDOCLY_SUPPRESS_UPDATE_NOTICE=true` for Redocly, and `CI=true` for AsyncAPI,
whose 6.0.2 release has no dedicated metrics opt-out. Even with `CI=true`,
the AsyncAPI CLI may create `~/.asyncapi-analytics` (a random user id) on its
first run, but it sends nothing. Re-check these settings in the installed
sources after a bump.

## Bumping a CLI manually

1. Edit the exact version (no `^` or `~`) in `<dir>/package.json`.
2. Regenerate the lockfile with the Node pinned in `mise.toml` and its bundled
   npm (the npm that CI uses):

   ```sh
   mise exec -- npm --prefix tools/contract-validators/<dir> install --package-lock-only --ignore-scripts
   ```

   If a global mise config overrides `npm`, run the bundled one instead:
   `mise exec -- node "$(mise where node)/lib/node_modules/npm/bin/npm-cli.js" ...`.
   This fallback matters because npm 12 refuses to generate the AsyncAPI
   lockfile with `EALLOWREMOTE`, because `@oclif/plugin-plugins` pulls in
   `npm-11.21.2.tgz`. The lockfiles were generated with npm 11.19.0,
   the npm bundled with the pinned Node and used by CI.

3. Run the check: `mise run contracts:openapi` or `mise run contracts:asyncapi`.
4. Commit `package.json` and `package-lock.json` together. Never commit
   `node_modules/`.

## Refreshing transitive dependencies without bumping the CLI

Dependabot version updates only bump the direct dependency, so transitive
versions stay as locked until the lockfile is re-resolved:

1. Re-resolve every transitive dependency to the newest version within its
   declared range with the bundled npm. The exact CLI pin in `package.json`
   is kept:

   ```sh
   mise exec -- node "$(mise where node)/lib/node_modules/npm/bin/npm-cli.js" \
     --prefix tools/contract-validators/<dir> update --package-lock-only --ignore-scripts
   ```

   Don't delete the lockfile and run `install --package-lock-only` instead.
   Without a lockfile, npm builds from the installed `node_modules` (present
   after any `mise run contracts:openapi|asyncapi`) and writes back the same
   versions. npm may still print `up to date`, so inspect
   `git diff -- tools/contract-validators/<dir>/package-lock.json` to see
   what changed.

2. Run the check and commit the new `package-lock.json` as above.
