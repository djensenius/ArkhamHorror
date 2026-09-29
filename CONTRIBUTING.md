# Contributing

This fork focuses on stable, additive contracts for native Arkham Horror
clients. General game-engine and web-client changes should normally be proposed
to [the upstream project](https://github.com/halogenandtoast/ArkhamHorror).

## Before opening a pull request

- Search the [roadmap](https://github.com/djensenius/ArkhamHorror/issues/5) and
  existing issues.
- Discuss substantial protocol or runtime changes in an issue first.
- Report vulnerabilities through
  [private vulnerability reporting](https://github.com/djensenius/ArkhamHorror/security/advisories/new),
  not a public issue.
- Never commit authentication tokens, private game exports, or official card
  and gaming-mat artwork.

## Contract workflow

Install the pinned tools and run every contract check:

```sh
mise install
mise run contracts:validate
```

Runtime changes must also pass the relevant Haskell and container checks already
defined in `.github/workflows`.

Backend Haskell tools are opt-in so locale/contract-only CI jobs do not install
GHC or Stack. To install them locally, run:

```sh
MISE_ENV=haskell mise install
```

On macOS, backend builds also need Homebrew `pcre` and keg-only `libpq` visible
to Stack. A typical shell setup is:

```sh
export PATH="/opt/homebrew/opt/libpq/bin:$PATH"
export PKG_CONFIG_PATH="/opt/homebrew/opt/libpq/lib/pkgconfig:/opt/homebrew/lib/pkgconfig:$PKG_CONFIG_PATH"
export CPATH="/opt/homebrew/include:$CPATH"
export LIBRARY_PATH="/opt/homebrew/opt/libpq/lib:/opt/homebrew/lib:$LIBRARY_PATH"
export DYLD_LIBRARY_PATH="/opt/homebrew/opt/libpq/lib:$DYLD_LIBRARY_PATH"
```

## Pull requests

- Branch from the fork's current `main`.
- Keep changes focused and reviewable; aim for no more than 20 hand-written
  files or 2,000 hand-written lines.
- Preserve compatibility with existing web clients unless an issue documents a
  coordinated migration.
- Update OpenAPI, AsyncAPI, JSON Schemas, fixtures, and compatibility metadata
  together when the wire contract changes.
- Prefer independent commits that can be contributed upstream without
  native-client implementation details.
