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

## Capability negotiation

- `GET /capabilities` is public and identifies the running server's complete
  contract bundle, compatibility floor, API base path, baseline status, and
  additive capability identifiers.
- Clients compare the three numeric revision components, never the strings
  lexically. They ignore unknown capability identifiers and disable optional
  behavior when its identifier is absent.
- A `404` means the server predates negotiation. Clients may offer an explicitly
  labeled conservative compatibility mode using `/site-settings`; they must not
  infer capabilities by probing mutation routes.
- Backend tests bind the response to the production encoder, while contract
  validation also requires its revision, status, base path, and compatibility
  floor to equal `manifest.json`.

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

### CORE board snapshot (mode, phase, chaos bag, locations, investigators, act/agenda)

A dedicated fixture (`fixtures/get-game.json` / `fixtures/game-update.json`)
now carries a genuinely non-empty, deterministic board built from the real
production handlers: `TestImport.newGame` plus
`pushAndRunAll [StandaloneSetup, Setup, EndSetup]` and two natural
`chooseOnlyOption` answers, reaching an authentic mid-turn CORE snapshot
(Night of the Zealot, "The Gathering") with a placed investigator, a live
location, act/agenda stacks, an active player-window question, and a real
chaos bag. Nothing is hand-authored; every field comes from the same
`PublicGame`/`Api.GetGameJson`/`ApiResponse` encoders exercised by
`backend/arkham-api/tests/Arkham/Api/JsonContractsSpec.hs`, and both the REST
envelope and the WebSocket `GameUpdate` are asserted against the same
underlying value.

This slice tightens, with exact required keys and closed tags/enums where the
Haskell source is itself closed:

- `mode`: the `These Campaign Scenario` wrapper. The scenario (`That`) branch
  is fully specified (58 required keys), including `chaosBag`, `name`,
  `decksLayout`/`locationLayout` topology strings, and `difficulty`. The
  campaign-only (`This`) and campaign+scenario (`These`) branches remain
  broad: no fixture exercises them yet, so their shape is asserted only by
  Haskell's `These` wrapper, not by direct evidence.
- `phaseStep`: the 4-tag `PhaseStep` union, each tag's `contents` a closed
  sub-step enum taken directly from `Arkham.Phase`.
- `chaosBag` / chaos tokens: all 8 `ChaosBag` fields and all 5 `ChaosToken`
  fields. `chaosTokenFace` is deliberately typed as an open `string` (not a
  closed enum) because the backend's own `ToJSON`/`FromJSON ChaosTokenFace`
  instances accept arbitrary homebrew slugs.
- `locations`, `investigators`, `acts`, `agendas` (and their counterparts
  `otherInvestigators`/`killedInvestigators`): every currently emitted field
  is required and typed, including the location/investigator fields computed
  at snapshot time from full-game context (`connectedLocations`,
  `investigators`, `enemies`, `assets`, `events`, `skills`, `treacheries`,
  `scarletKeys`) rather than `LocationAttrs`'/`InvestigatorAttrs`'s own
  generic encoding. Every ID-shaped field is typed to its confirmed backend
  newtype rather than a plain string: `format: "uuid"` for `LocationId`/
  `AssetId`/`EnemyId`/`EventId`/`SkillId`/`TreacheryId`-backed fields (all
  wrap `UUID`, per `Arkham/Id.hs`), and the existing card-code `pattern` for
  `InvestigatorId`/`ActId`/`AgendaId`/`ScenarioId`/`ScarletKeyId`-backed
  fields (all wrap `CardCode`). These arrays are empty in this fixture (the
  opening board has no enemies/assets/treacheries placed yet), so only the
  *item type* is evidenced from source, not populated instances.

Known, deliberate scope limits carried by this fixture:

- Interactive `Question`/`Message`/`Target`/`Source` payloads, the full card
  union, `Placement` contents, matchers, and campaign-specific unions (tarot,
  scarlet key, standalone campaign log internals) stay broad. `Placement`'s
  28 tags are enumerated exactly, but each tag's `contents` shape varies and
  is left `true` (unconstrained).
- "The Gathering" only places one location (`Study`) during `Setup`; the rest
  of the investigators' starting locations are `setAside` cards, not placed
  entities. `connectedLocations`/`directions`/`connectsTo` are therefore
  correctly typed but empty in this fixture — not a fabricated topology.
- `contracts/manifest.json` has no hash/checksum mechanism to recompute;
  schema and fixture integrity is enforced entirely by
  `scripts/validate-contract-fixtures.py`'s registry-based JSON Schema
  validation, which this slice extends with `negativeFixtures` (missing
  required fields, wrong tags, malformed IDs, nullability violations, and
  structural drift) so a schema regression fails loudly instead of silently
  widening.

Discrepancies found against the Vue native client while grounding this slice
(cited here per the source-of-truth backend wire output, not fixed, since
this contract does not change runtime behavior):

- `Arkham.Phase`'s `Phase` enum has 6 constructors, including
  `ResolutionPhase`; the Vue `phaseDecoder`
  (`frontend/src/arkham/types/Phase.ts`) only lists 5, omitting
  `ResolutionPhase` entirely, so a server sending that phase name would fail
  to decode client-side. `PhaseStep`'s 4-tag union matches exactly between
  backend and Vue.
- Backend `ChaosToken` has exactly 5 fields
  (`chaosTokenId`, `chaosTokenFace`, `chaosTokenRevealedBy`,
  `chaosTokenCancelled`, `chaosTokenSealed`). Vue's `chaosTokenDecoder`
  (`frontend/src/arkham/types/ChaosToken.ts`) decodes only `chaosTokenId` and
  `chaosTokenFace`, silently drops `chaosTokenRevealedBy`/
  `chaosTokenCancelled`/`chaosTokenSealed`, and additionally declares optional
  `modifiers`/`modifiedFaces` fields that do not exist anywhere on the
  backend `ChaosToken` type.
- Backend `ChaosBag` has 8 fields; Vue's `chaosBagDecoder`
  (`frontend/src/arkham/types/ChaosBag.ts`) decodes only `chaosTokens` and
  `choice`, ignoring `setAsideChaosTokens`, `revealedChaosTokens`,
  `forceDraw`, `tokenPool`, `totalRevealedChaosTokens`, and
  `pendingRequests`.
- Backend `InvestigatorAttrs` has 81 fields in this fixture; Vue's
  `investigatorDecoder` (`frontend/src/arkham/types/Investigator.ts`) decodes
  59 of them and silently ignores the rest (e.g. `killed`, `traits`,
  `previousLocation`, `usedAbilities`, `drivenInsane`, `movement`). Notably,
  the wire `eliminated` field is ignored entirely in favor of a
  client-computed `defeated || resigned`.

## Game creation and multiplayer lobbies

- Creating a game requires authentication and at least one non-null
  `campaignId` or `scenarioId`. A campaign may also name its starting scenario.
  The legacy `deckIds` field remains required but is currently decoded without
  affecting initialization. Creation has no idempotency key, so clients must
  not automatically retry an ambiguous failure.
- `ultimatumsAndBoons` defaults to an empty set and
  `achievementsEnabled` defaults to true. Explicit `asIfRuling` takes
  precedence over the legacy `strictAsIfAt` boolean.
- Deletion is available to any participant, not a distinct owner. Missing and
  non-participant game IDs both return empty success, making deletion
  repeatable without disclosing existence.
- Authenticated accounts may preview a pending game without joining; its log is
  omitted. Joining twice is idempotent, and joining a game that already left
  the pending state returns its unchanged snapshot. An Epic participant cannot
  occupy groups in the same event twice.
- Open-seat responses use `c`-prefixed investigator codes. Seat claims accept
  prefixed or raw codes, only support `WithFriends`, and enforce one user per
  seat and one seat per user. There is no player-facing unclaim route.
- During initial deck selection and campaign upgrades, an embedded `deckList`
  takes precedence over `deckUrl`; omitting both means continue without an
  upgrade. Obsolete resubmissions succeed without changing state. The route
  uses the same shallow main-slot implementation check and error envelope as
  saved decks. It currently authenticates the caller without independently
  checking game membership.

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

## Accounts, settings, and notifications

- Password-reset request and update routes are public. The request route
  currently returns `404` for an unknown email, but clients must present the
  same neutral result as `200` and never use this distinction for account
  discovery. Reset tokens expire after one day; consuming an expired token
  deletes it and returns success without changing the password.
- Settings updates require authentication and currently accept only `beta`.
  Their three-field response omits the `admin` field present in `GET /whoami`,
  so clients must not decode both responses as the same shape.
- Account deletion requires authentication, removes outstanding reset tokens,
  and returns an empty success response.
- Notifications are unauthenticated global announcements, not user-specific
  records. They are returned newest first with `id`, `body`, and `createdAt`.

## Card and investigator catalogs

- Card catalogs, individual card lookup, and investigator artwork lookup are
  public. Catalog lists are unpaginated and use the compact, omit-default
  `CardDef` encoder.
- `GET /arkham/cards` defaults to player cards. `cardPool=campaign` selects
  campaign cards and `cardPool=both` combines both pools. The legacy
  `includeEncounter` parameter is presence-based and also selects the combined
  pool unless `cardPool` already names `campaign` or `both`.
- `CardDef.cardCode` uses the Aeson-prefixed form such as `c01020` or
  `c:dark-matter:151`. `GET /arkham/card/{cardCode}` instead expects the raw
  form without that leading `c`.
- `GET /arkham/investigators` returns raw artwork identifiers, not investigator
  objects. Alternate printings are returned as separate entries.
- The schema fixes all currently emitted `CardDef` top-level fields while
  intentionally leaving complex matcher, criterion, action, keyword, and
  customization payloads broad until their dedicated domain slices.

## Achievements

- Achievement operations require authentication. The account-wide list returns
  only the requesting user's earned and in-progress rows. A game-specific list
  requires game membership and returns rows for every user associated with
  that game, identified by `userId`.
- Lists are ordered by `earnedAt` ascending. An in-progress row has null
  `earnedAt` and may have a null `arkhamGameId`; `progress` is
  achievement-specific JSON.
- Clearing supports all earned achievements, a campaign identifier, or one
  achievement identifier. It never deletes in-progress rows.
- Achievement identifiers are flat constructor-name strings. The set is
  additive, so clients must preserve and gracefully display unknown values
  received from a newer server rather than decoding them as a closed enum.

## Decks

- Every deck operation requires authentication. Lists and detail reads expose
  only the requesting user's saved decks; list order is currently unspecified.
  Deleting an absent or unowned ID succeeds without disclosing its existence.
- Imported deck-list input requires only `slots` and `investigator_code`,
  accepts additional ArkhamDB fields, and defaults a missing `sideSlots` to an
  empty object. Any `sideSlots` value that does not decode as a card-quantity
  object currently normalizes to an empty object; clients should send an object
  even though legacy arrays are accepted. `investigator_name` should be
  supplied, and `meta` is a JSON-encoded string when present, not an embedded
  object.
- The normalized encoder always emits all nine deck-list fields. Card and
  investigator codes gain the Aeson `c` prefix, numeric external IDs become
  strings, and unknown input fields disappear.
- Saved deck responses additionally expose the database UUID, numeric `userId`,
  saved `url`, display `name`, and `investigatorName`. The legacy `deckId`
  supplied while creating a saved deck is required but ignored; the database
  UUID in the response is authoritative. Unknown create/fetch request fields
  are ignored.
- `/decks/validate` and deck creation only reject unimplemented card codes in
  `slots`. They do not validate deck legality, quantities, investigator
  support, or `sideSlots`. Validation is read-only and safely retryable.
- Fetch accepts a remote deck object or the first entry of a remote array.
  arkham.build share URLs use its public API and normalize the embedded `url`
  to null. Sync replaces a saved deck's embedded list without rerunning
  implemented-card validation.

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

`contracts:fixtures` also asserts a small set of `manifest.json`
`negativeFixtures` (`fixtures/invalid/*.json`) fail validation for their
declared reason: each entry's `expectedError` names the exact instance path,
JSON Schema keyword, and message substring that must appear somewhere in the
(recursively flattened) validation error tree, covering a missing required
field, a wrong closed-union tag, a malformed ID, a nullability violation, and
structural drift. This catches a schema regression that fails for the wrong
reason, not just "any failure", alongside the positive fixtures.

Contract changes must remain backward compatible with the Vue client. Runtime
changes belong in separate, small pull requests so they can be contributed
upstream independently of documentation and fixtures.
