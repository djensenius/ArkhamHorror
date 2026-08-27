# Native client contract

This directory is the versioned compatibility boundary between the Arkham
Horror backend and non-web clients.

The first revision intentionally covers only the walking-skeleton endpoints and
WebSocket envelopes. It does not claim full player-facing API coverage yet.
Expand the schemas alongside golden fixtures captured from the Haskell encoders;
do not infer wire shapes independently in each client.

## Contents

- `openapi.yaml`: REST operations and stable response models.
- `asyncapi.yaml`: game and Epic Multiplayer WebSocket channels.
- `route-inventory.json`: every generated backend route operation and its
  manually governed native-client scope.
- `schemas/`: reusable JSON Schemas for socket payloads.
- `fixtures/`: sanitized examples validated by every client.
- `manifest.json`: schema revision and compatibility metadata.

Committed fixtures must be backed by deterministic backend values, not
hand-authored independently of the server. The Haskell assertions live in
`backend/arkham-api/tests/Arkham/Api/JsonContractsSpec.hs` and compare decoded
fixture JSON with the real Aeson encoders.

## Route inventory

The route inventory expands every HTTP method in
`backend/arkham-api/config/routes` and identifies it by exact method, normalized
path, and Yesod resource. Validation fails on any addition, removal, rename,
method change, or reorder; it does not tolerate count drift.

Coverage is classified manually:

- `documented`: already represented in the current OpenAPI baseline.
- `required`: belongs in eventual native-client parity but is not fully
  documented yet.
- `excluded`: an administrator, operator-maintenance, or diagnostic surface
  that is outside native-client parity.

Epic Multiplayer organizer routes, spectating, replay, player game import,
standard/scenario export, and bug filing remain required player-facing
coverage. The administrator-only full export and repair/migration routes remain
excluded. Classification is product metadata and must not be inferred from
route naming alone.

## Validation

```sh
mise run contracts:validate
```

Contract changes must remain backward compatible with the Vue client. Runtime
changes belong in separate, small pull requests so they can be contributed
upstream independently of documentation and fixtures.
