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

- `mode`: `GameMode = These Campaign Scenario`, whose real Aeson wire shape
  (verified empirically; the `these` package ships no aeson dependency and
  this codebase declares no custom/orphan `ToJSON (These a b)` instance) is
  sibling `This`/`That` keys, never a wrapping `"These"` object. The schema
  is a disjoint 3-branch `oneOf`: `This`-only (a campaign with no active
  scenario), `That`-only (a standalone scenario, fully specified — 58
  required keys, including `chaosBag`, `name`, `decksLayout`/
  `locationLayout` topology strings, and `difficulty`), and both `This`+
  `That` present as siblings (a running campaign scenario). All three
  branches are backed by real production `toJSON` fixtures generated from
  the real `This`/`That`/`These` constructors
  (`fixtures/mode-campaign-only.json`, `fixtures/mode-campaign-scenario.json`,
  plus the existing standalone-scenario `get-game.json` fixture); a
  dedicated negative fixture proves the nonexistent `"These"` wrapper key is
  rejected across all three branches. The `This` campaign branch stays an
  intentionally broad object placeholder (`$defs/campaign`); no deep
  campaign schema is in scope for this slice. `turn`
  (`ScenarioAttrs.scenarioTurn`) has `minimum: 0` (not 1), since a scenario
  genuinely initializes at turn 0 before its first turn begins;
  `fixtures/mode-turn-zero.json` is a governed, hashed, real production
  `toJSON (gameMode fixtureBoardGameAtTurnZero)` fixture proving this exact
  value validates, alongside the existing negative fixture proving `-1` does
  not.
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
- `location.schema.json` is `oneOf` an ordinary location (all of the above)
  or the distinct enemy-location view `Arkham.Game.withEnemyLocationAsLocationData`
  emits (`enemyLocation: true`, `exhausted`, and several ordinary fields
  omitted entirely) for enemies that occupy their own board position rather
  than a placed `Location` entity; `fixtures/location-enemy-view.json` is a
  real production encoding of that second branch (`shapelessCellar`).
- `investigator.schema.json`'s `movement` is `Maybe Movement`
  (`InvestigatorAttrs.investigatorMovement`): `null` while no move is
  mid-resolution, or the full `Movement` object while one is.
  `fixtures/movement.json` is a real production `Movement` value (an
  in-progress `Direct` move to a location); `fixtures/get-game.json`'s
  investigator demonstrates the `null` branch.
- `act.schema.json`'s `advanceCost` is `Maybe Cost`, encoded as `null` when an
  act has none (most acts, including every First Night act in this scenario).
  `fixtures/act-no-advance-cost.json` is the real production encoding of
  `HotOnYourTail`, an act with no advance cost.
- Act/agenda `sequence` "side" fields are the closed, all-nullary Haskell
  unions `Arkham.Act.Sequence.ActSide` (`A`..`H`) and
  `Arkham.Agenda.Sequence.AgendaSide` (`A`..`D`); the schemas enumerate them
  exactly. `manifest.json`'s `enumBoundaryChecks` prove both boundary members
  (e.g. `A`/`H`) validate and an invented member (`Z`) does not, directly
  against the isolated enum sub-schema — this closed-set risk is fully
  retired by the Haskell ADT declaration itself, not by needing one JSON
  sample per letter.
- `public-game.schema.json` constrains `propertyNames` for every entity map
  it publishes, split by the two real key classes actually used: `$defs/
  uuidEntityMap` (`$defs/uuidMapKey`, `format: "uuid"`) for `locations`,
  `enemies`, `assets`, `treacheries`, `events`, `skills`, `concealed`,
  `question` (`PlayerId`), and `cards` (`CardId`, `Arkham/Card/Id.hs`) — all
  UUID-backed newtypes whose `ToJSONKey` is `deriving newtype` from the
  wrapped `UUID` (`Arkham/Id.hs`) — and `$defs/cardCodeEntityMap`
  (`$defs/cardCodeMapKey`) for `investigators`/`otherInvestigators`/
  `killedInvestigators`/`acts`/`agendas`/`stories`/`scarletKeys` and
  `roundHistory`/`phaseHistory`/`turnHistory` (`Map InvestigatorId History`,
  `Arkham/Game/Base.hs`) — all `CardCode`-backed newtypes (`InvestigatorId`/
  `ActId`/`AgendaId`/`StoryId`/`ScarletKeyId`). `CardCode`
  (`Arkham/Card/CardCode.hs`) is a bare `newtype CardCode = CardCode Text`
  with no character-class constraint at the type level; its `ToJSONKey`
  instance always prepends a literal `c` to the underlying text
  (`toJSONKeyText (T.cons 'c' . unCardCode)`). Empirically, every one of the
  815 real card codes in `data/cards.json` matches `^[0-9]{5}[a-z]?$`, but
  since `CardCode` places no such restriction on homebrew/unofficial
  content, `cardCodeMapKey`'s wire-key pattern is the broader
  `^c[0-9a-z:._-]+$` — the same grammar already used by the `cardCode`/`id`
  fields elsewhere in these schemas — rather than the narrower
  official-only shape. Every real `get-game.json`/`game-update.json`
  fixture keeps `enemies`/`assets`/`treacheries`/`events`/`concealed`/
  `skills`/`cards`/`stories`/`scarletKeys`/`*History` empty (no enemies are
  on the board at fixture setup), so two focused standalone fixtures —
  `fixtures/uuid-entity-map.json` (a real `createEnemy`-built Swarm of Rats
  entry) and `fixtures/card-code-entity-map.json` (a real `createStory`-built
  The Stakeout entry, keyed exactly as the real `Game/Runner.hs` call site
  does: `StoryId $ toCardCode card`) — validate each shared key class against
  a genuinely non-empty map rather than only ever an empty one. Two
  corresponding negative fixtures each add one additional, invalid-keyed
  entry (whose own embedded `id` field stays valid) alongside the real entry,
  isolating the `propertyNames` failure from any value-level check. Entity
  *value* schemas stay an intentionally broad object placeholder for every
  one of these maps — out of scope for this map-key slice.
- `investigator.schema.json`'s `unhealedHorrorThisRound` has no `minimum`
  constraint (plain `integer`): production's real arithmetic
  (`min 0 . subtract amount` in `Investigator/Runner/Damage.hs`) can and
  does drive this value negative on over-heal, so a `minimum: 0` schema
  constraint would reject genuinely valid wire output.
  `fixtures/investigator-unhealed-horror-negative.json` is a real production
  encoding (via the actual `HealHorrorDirectly` handler) with the value
  `-3`.
- Every governed schema/fixture/document is bound to a recomputed SHA-256 in
  `manifest.json`'s `artifactHashes`; see "Release immutability and
  `schemaRevision`" below for the enforcement rule.


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

`contracts:fixtures` (`scripts/validate-contract-fixtures.py`) validates
every schema and positive fixture, then asserts every `manifest.json`
`negativeFixtures` entry fails validation for exactly its declared reason.
Each entry is a *single deterministic mutation* (one JSON-Pointer
`remove`/`replace`/`add` operation) applied to a value looked up, also by
JSON Pointer, inside an already-validated positive fixture (`basePositiveFixture`
+ `basePointer`) — never a hand-copied duplicate payload. `expectedErrors` is
matched as an *exact* set against the flattened, normalized error list (every
declared expectation must be satisfied by exactly one real error, and no
unexplained extra errors may remain), which catches a regression that fails
for the wrong reason or fails "more than expected" — not just "any failure".
For a schema whose root is `oneOf` (e.g. `location.schema.json`,
`phase-step.schema.json`, `mode.schema.json`), `schemaBranch` (a `$defs` name
or a `oneOf` array index) scopes the exact-error check to the one production
branch being tested, while a supplementary check still confirms the full,
unscoped root schema also rejects the mutated instance overall (proving the
`oneOf`-wrapper diagnostic without enumerating unrelated sibling-branch
noise). A self-test (run automatically on every invocation) proves this
exact-match comparison is actually discriminating: adding one extra,
undeclared mutation on top of a real registered negative fixture must make
the comparison fail, even though the originally-expected error is still
present among the larger error set.

`manifest.json`'s `enumBoundaryChecks` separately prove a handful of closed,
all-nullary Haskell enums (e.g. act/agenda sequence sides) accept their
boundary members and reject an invented one, validated directly against the
isolated enum sub-schema located by JSON Pointer into the *schema* document
itself — these intentionally make no claim of being production-fixture JSON,
since the enum's members are already authoritatively fixed by the Haskell
ADT declaration cited in each schema's description.

### Release immutability and `schemaRevision`

Once a `schemaRevision` has been merged to `main`, every governed artifact at
that revision — every document in `manifest.json`'s `documents` array, every
fixture in `fixtures`, and the manifest's own descriptor content (including
`negativeFixtures`/`enumBoundaryChecks`) — is immutable. `manifest.json`'s
`artifactHashes` binds a SHA-256 of each governed artifact's real content
(the manifest hashes itself too, canonicalized with `artifactHashes` zeroed
out first, to avoid a circular self-hash); `contracts:revision-drift`
recomputes every hash and rejects a missing, extra (stale), or mismatched
entry.

`contracts:revision-drift` (`scripts/check-schema-revision-drift.py`)
enforces the actual bump rule: it recomputes the current governed-artifact
hash set, recomputes the same hash set as of a resolved base ref, and
requires `schemaRevision` to have strictly, numerically increased whenever
that diff is non-empty. Base-ref resolution is deterministic and fails
closed in CI: when both `GITHUB_ACTIONS` and `CI` are `"true"`, the gate
*requires* an explicit `CONTRACT_BASE_REF` (wired by the workflow from the
triggering event's own immutable base — `pull_request.base.sha`,
`push.before`, or a required `workflow_dispatch.inputs.base_sha` — never an
inferred branch/HEAD alias), validates it is a well-formed hex SHA, and
rejects an all-zero SHA except a narrowly-checked repository-initialization
escape hatch that cannot weaken `main`. Outside CI, an unset
`CONTRACT_BASE_REF` falls back, in order, to `fork/main`, `origin/main`,
`main`, and finally the commit this contract effort was originally branched
from, as a last-resort deterministic pin — all via local
`git show <ref>:path`, never a network call. A human updating only the hash
or only the version number is not sufficient; the gate cross-checks both.
Its comparison logic (`evaluate_drift`) is pure and is proven separately by
a set of small, fully in-memory self-tests with no git or filesystem
dependency, demonstrating a same-revision artifact change fails the gate
while a strictly higher revision passes it — deterministic in any
environment; `resolve_base_ref`'s own self-tests separately cover CI-mode
missing/invalid/all-zero/unresolvable-SHA rejection and successful
resolution both in and outside CI.

Every governed JSON read across this tooling — the manifest, schemas,
fixtures, negative-fixture descriptors, and the base-ref manifest read via
`git show <ref>:path` bytes — goes through a single shared strict loader
(`scripts/strict_json.py`, a plain sibling module imported by all three
contract scripts) rather than a bare `json.load`/`json.loads`: it rejects
duplicate object keys at any nesting depth (including a plain key and a
distinct `\uXXXX`-escaped form that decodes to the same text), the
non-JSON `NaN`/`Infinity`/`-Infinity` constants the stdlib accepts by
default, and non-strict UTF-8 decoding of raw bytes. Its own self-tests run
automatically whenever any of the three scripts is invoked, proving the
wiring end-to-end rather than only in isolation.

Contract changes must remain backward compatible with the Vue client. Runtime
changes belong in separate, small pull requests so they can be contributed
upstream independently of documentation and fixtures.
