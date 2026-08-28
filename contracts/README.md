# Native client contract

This directory is the versioned compatibility boundary between the Arkham
Horror backend and non-web clients.

Clients pin `manifest.json`'s `schemaRevision`, which versions the complete
contract bundle. OpenAPI and AsyncAPI `info.version` values version those
documents independently and advance only when their respective document
changes, so they do not need to equal the bundle revision.

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

Committed fixtures must be bound to the real backend codec, not maintained as
unchecked examples. The Haskell assertions live in
`backend/arkham-api/tests/Arkham/Api/JsonContractsSpec.hs`: outbound fixtures
are compared semantically with real Aeson encoder output, while inbound
fixtures must decode through the real `FromJSON` instance to the expected
constructor.

## Game discovery

- `GET /arkham/games` requires authentication, is ordered by most recently
  updated, and omits Epic Multiplayer group games because the event list owns
  those entries. A legacy row that cannot decode remains visible as an
  `{"error": "..."}` entry.
- `GET /arkham/games/{gameId}` requires both authentication and game
  membership. Its response includes the acting player, multiplayer mode,
  authoritative `PublicGame`, and optional Epic event ID.
- `GET /arkham/games/{gameId}/spectate` intentionally requires neither an
  account nor membership. Possession of the game UUID grants snapshot access,
  and the response identifies the current active player for rendering. Its
  WebSocket upgrade is server-enforced read-only: spectator frames are ignored
  without being decoded or applied to the game.
- The `PublicGame` schema governs all 58 top-level snapshot fields shared by
  REST game reads and WebSocket `GameUpdate`. Deep entity, question, target,
  source, and campaign unions remain intentionally broad until their dedicated
  schema slices land.

## WebSocket behavior

- Participant game sockets are bidirectional: servers emit `ServerMessage`
  envelopes and clients send answer payloads. Spectator game sockets and Epic
  Multiplayer event sockets are read-only; inbound frames are ignored.
- Participant and event sockets accept `Authorization: Token <token>` when
  upgrade headers are available. The `token` query parameter remains a
  compatibility fallback, and the header takes precedence when both are
  present. Spectator sockets require no authentication.
- The server sends an RFC 6455 ping every 15 seconds. It may negotiate
  permessage-deflate, but clients must also work without compression.
- Messages are not buffered for disconnected subscribers. After reconnecting,
  refetch the authoritative game or event state before applying new messages.
- `EventChanged` carries no payload and instructs clients to refetch event
  details. `SharedStateUpdate` is a complete versioned shared-state value.

The server-message schema and backend assertions cover every `ApiResponse`
constructor. The `GameUpdate` fixture comes from a deterministic pending
scenario built by the production `newScenario`, `PublicGame`, and `ApiResponse`
encoders. It pins generated identifiers and revision metadata without relying
on database state or private game data.

The client-answer schema covers every top-level `Answer` constructor.
Question responses include optional `playerId` and `questionVersion` fields.
Large nested unions such as raw engine messages, standalone/campaign settings,
deck lists, destiny values, and campaign steps are intentionally broad in this
revision; their representative fixtures are decoder-checked, and later slices
will tighten each nested schema without changing the top-level envelope.

## Route inventory

The route inventory expands every HTTP method in
`backend/arkham-api/config/routes` and identifies it by exact method, normalized
path, and Yesod resource. Validation fails on any addition, removal, rename,
method change, or reorder; it does not tolerate count drift.
The same check also requires the normalized method/path set marked
`documented` to match OpenAPI exactly, so coverage metadata cannot move ahead
of (or fall behind) the published REST contract.

Coverage is classified manually:

- `documented`: already represented in the current OpenAPI baseline.
- `required`: belongs in eventual native-client parity but is not fully
  documented yet.
- `excluded`: an administrator, operator-maintenance, or diagnostic surface
  that is outside native-client parity.

Epic Multiplayer organizer routes, replay, player game import,
standard/scenario export, and bug filing remain required player-facing coverage.
The administrator-only full export and repair/migration routes remain excluded.
Classification is product metadata and must not be inferred from route naming
alone.

## Validation

```sh
mise run contracts:validate
```

Contract changes must remain backward compatible with the Vue client. Runtime
changes belong in separate, small pull requests so they can be contributed
upstream independently of documentation and fixtures.
