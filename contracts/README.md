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
- The response carries exactly one optional field, `localeCatalog`, paired with
  `i18n.locale-catalog.v1` (see
  [Locale catalog discovery](#locale-catalog-discovery)). Its absence is a
  normal, current server that publishes no catalog — not an old one.
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
underlying value. Every governed fixture is bound to *both* of Aeson's
independently-implemented serialization paths: the ordinary `Aeson.toJSON`
assertion, and a second assertion (`viaWireEncoding`, round-tripping through
the real `Aeson.encode`, which always dispatches via `toEncoding` — the
actual REST/WebSocket wire path, per `Orphans.hs`'s
`ToContent a where toContent = toContent . toEncoding`) — because
`PublicGame`'s `ToJSON` instance hand-writes `toJSON` and `toEncoding`
separately rather than deriving one from the other, and this codebase has
already had a real historical bug where the two silently disagreed (see the
Haddock on `publicOtherInvestigators` in `Arkham/Game.hs`). A dedicated
`ToEncodingDriftProof` self-test type with a deliberately mismatched
`toJSON`/`toEncoding` pair proves this second assertion actually has teeth.

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
- `cost.schema.json`'s top-level `TaggedObject` envelope (`Arkham.Cost.Cost`,
  `deriveToJSON defaultOptions`) is closed (`additionalProperties: false`):
  only `tag` (required) and an optional `contents` are permitted, matching
  Aeson's exact wire shape for every constructor. `contents` itself stays
  intentionally unconstrained (any JSON value), since Cost is a broad tagged
  union of dozens of action/resource/clue/discard/matcher-driven
  constructors whose individual payload shapes remain out of scope for this
  contract slice.
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

### Native basic-choice questions

`schemas/basic-choice-question.schema.json` is the deliberately small
native-renderable slice for `ChooseOne`, `PlayerWindowChooseOne`,
`WindowChooseOne`, and `Read`. It is **not** referenced from
`PublicGame.question`: that UUID-keyed map remains broad so every other
engine question/message variant remains valid. A client may apply this
standalone schema to its own player's map value; any unknown question or
choice-label tag is unsupported, not a legal action to normalize or guess.
Recognized question, choice-label, and component envelopes are closed and
constructor-disjoint, so aliased, cross-variant, or otherwise impossible
fields also fail rather than being silently normalized. If any single choice
inside a real `choices`/`readChoices` array is an unsupported variant, the
*whole* prompt fails this schema (the array is validated element-by-element,
with no partial pass); `PublicGame.question` itself is untouched by this
schema and keeps every entry -- known or unknown -- exactly as encoded, so a
client falls back to an unsupported/update-required presentation for the
entire prompt rather than silently dropping or renumbering just the
unrecognized entry.

The production-generated
`question-player-window-choose-one.json` golden is the active CORE fixture
prompt, in its authoritative array order: resource `ComponentLabel`, deck
`ComponentLabel`, `EndTurnButton`, then `AbilityLabel`. The two compact sibling
goldens reuse its real `EndTurnButton` value under the production `ChooseOne`
and `WindowChooseOne` encoders. Backend tests bind every golden independently
to both `Aeson.toJSON` and the actual `Aeson.encode`/`toEncoding` wire path.

Native rendering uses the label/component tags, investigator/card identity,
token type, and ability display identity. Nested messages, criteria, windows,
sources, costs, and additive ability fields are losslessly preserved opaque
engine data: clients must not evaluate them, execute them, or derive new legal
actions from them. Message values are constrained only to their guaranteed
production constructor-object outer shape (`tag` plus opaque/additive
constructor data). Selecting a choice sends its zero-based array index.

For this slice, native clients send:

```json
{"tag":"Answer","contents":{"choice":2,"playerId":"00000000-0000-0000-0000-000000000001","questionVersion":3}}
```

`playerId` is the canonical UUID map key and `questionVersion` **must** equal
the authoritative `PublicGame.scenarioSteps` from the snapshot that supplied
the question. The native `Answer` schema requires both fields; the backend
decoder's optional legacy behavior is intentionally outside this native
branch. UUID spelling is exactly lowercase and hyphenated (`schemas/uuid.schema.json`,
shared by every governed UUID-string field, including a `LocationTarget`'s
`contents` below). `choice`, `questionVersion`, and `scenarioSteps` share the
non-negative signed-64 range `0...9223372036854775807` and use canonical raw
JSON integer tokens: no sign, decimal point, exponent, or leading zero. The
paired `answer-question.json` golden decodes through the real
`Entity.Answer` `FromJSON` instance, and backend tests prove choice `2`
selects the third (`EndTurnButton`) CORE option and version `3` equals that
snapshot's `scenarioSteps`.

#### The Gathering's opening `Read`/`ChooseOne(LocationTarget)` prompts

The same schema also covers the production-authentic opening prompt
sequence for Night of the Zealot's "The Gathering": a `Read` setup-intro
story beat, followed by the `startAt` starting-location `ChooseOne`.

- `question-read.json` is the real `setupTheGathering` opening `Read`
  (`Arkham.Helpers.FlavorText.setup` -> `flavor` -> `Arkham.Message.story`):
  `BasicReadChoices` with exactly one semantic continue choice
  (`{"tag":"Label","label":"$continue","messages":[]}`) and `readCards: null`.
  Only this exact governed continue shape is modeled; `BasicReadChoicesN`,
  `BasicReadChoicesUpToN`, and `LeadInvestigatorMustDecide` remain explicit
  unsupported values. `flavorText` (`Arkham.Text.FlavorText`) is itself a
  closed slice covering only the `BasicEntry`, `I18nEntry`, and `ListEntry`
  constructors these fixtures exercise; every other `FlavorTextEntry`
  constructor is likewise unsupported here (still opaque in `PublicGame`).
  `title`/`I18nEntry.key` are literal i18n lookup keys, never rendered
  narrative text -- the wire never carries actual scenario prose.
- `question-read-with-cards.json` is the sibling non-null `readCards`
  branch, built from the real (pure) `Arkham.Message.storyWithCards`, proving
  the `Maybe [CardCode]` missing/null/value distinction and the `BasicEntry`
  flavor-text constructor against the same schema.
- `question-choose-one-location.json` is the real `startAt` prompt
  (`Arkham.Scenario.Setup.startAt`) that follows: `ChooseOne` with a single
  `TargetLabel(LocationTarget)` choice for the real starting location
  ("Study", `d5a66e84-c729-4066-8475-d8a155609025`, matching `get-game.json`).
  Only the `LocationTarget` variant of the much broader `Arkham.Target` sum
  is modeled; every other `Target` constructor remains an explicit
  unsupported value here while staying opaque inside the broader
  `PublicGame.question` map (see the paired `forwardCompatibilityChecks`
  entry proving a `TargetLabel(EnemyTarget)` choice there).
- `question-choose-one-location-multiple.json` generalizes `startAt` to
  three real "The Gathering" locations (Attic, Hallway, Parlor) via the
  same shared `chooseTargetM`/`targeting`/`unsafeReveal`/`placeAllAt`
  production combinators, proving backend choice order and each choice's
  zero-based `Answer.choice` index are stable and never renumbered by an
  unsupported sibling entry.

No new capability string gates this slice; native clients gate it purely by
the negotiated `schemaRevision`. The existing versioned `Answer` frame (above)
remains sufficient to answer either prompt -- no new answer constructor was
required.

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

## Locale catalog (frontend-owned, v1)

Story content crosses the wire as opaque `I18nEntry` keys and typed variables,
so a native client also needs the server's message catalog. That catalog is
published as deployment-owned static JSON at `/locale-catalog/manifest.json`
plus immutable, content-addressed chunks, and is documented in
[`docs/locale-catalog.md`](../docs/locale-catalog.md).

Its schemas are deliberately **not** governed artifacts in `manifest.json`:
every document listed there is bound to the backend's real Aeson encoders in
`backend/arkham-api/tests/Arkham/Api/JsonContractsSpec.hs`, while the locale
catalog is generated from `frontend/src/locales/**` by the frontend build and
never touches the backend. It is therefore versioned independently by its own
`schemaVersion` (`frontend/schemas/locale-catalog/v1/`) and revisioned by a
digest of its sources.

The backend's own, governed side of this boundary is the optional
`localeCatalog` object in `capabilities.schema.json` (see
[Locale catalog discovery](#locale-catalog-discovery) below), which points at
the catalog and pins its digest without ever carrying catalog content.

The two boundaries are still bound to each other mechanically: the catalog's
required key set is extracted from `fixtures/question-read.json` and
`fixtures/question-read-with-cards.json`, and its manifest records those
fixtures' SHA-256 digests together with this manifest's `schemaRevision`, so a
catalog can always be traced to the contract revision it was generated for.

### Locale catalog discovery

`GET /capabilities` advertises a catalog with the `i18n.locale-catalog.v1`
identifier and the additive `localeCatalog` object. Both come from one `Maybe`
in `Base.Api.Types.Capabilities.serverCapabilities`, so a client can never see
one without the other, and `capabilities.schema.json` states that pairing in
both directions.

- **Absence is not legacy.** A server that omits the field and the identifier
  is a deployment that publishes no catalog. A client must treat story keys as
  unresolvable rather than probe a guessed catalog path, exactly as it must for
  any other absent capability.
- **`manifestUrl` is the only authority.** There is deliberately no separate
  origin or base-path field to disagree with it. A relative value (the hosted
  form, `/locale-catalog/manifest.json`) is resolved against the origin the
  capabilities response itself came from; an absolute value is always `https`
  and is what a split/static deployment configures explicitly. A client must
  not rewrite it onto another host, and must not accept a redirect to a
  different origin than the one the URL named.
- **The host means one thing to every client.** An absolute URL's host is
  either canonical dotted-decimal IPv4 (four octets `0-255`, no leading zeros)
  or a registered name whose final label is neither all-digits nor an `0x` hex
  literal. That is WHATWG's "ends in a number" rule: `127.1`, `2130706433`,
  `0x7f000001` and `01.02.03.04` are all `127.0.0.1` to a browser and something
  else to a stricter parser, and `999.999` or `1.2.3.4.5` are read differently
  again. `manifest.json`'s `manifestUrlChecks` is the single governed table for
  this: `contracts:fixtures` drives every row through the schema and the
  backend spec drives the same rows through the production validator, so the
  two can never diverge.
- **Trust is pinned, not assumed.** `manifestSha256` is the SHA-256 of the
  manifest bytes served at `manifestUrl`; a manifest that does not hash to it
  must be discarded rather than used. Every chunk digest then hangs off that
  verified manifest.
- **Caching.** `catalogRevision` is content-derived, so two servers reporting
  the same revision publish byte-identical catalogs and a client may key its
  cache on that value alone. `schemaVersion` is the catalog manifest's own
  version: a client that does not implement it must treat the catalog as
  unavailable rather than guess at the shape.
- **Compatibility.** The field is additive under this contract's existing
  unknown-field rule. The Vue client reads none of it and is unaffected, and an
  older native client that ignores unknown fields keeps working unchanged.

Advertising is deployment configuration, not a build-time constant: see the
`locale-catalog-*` settings in `backend/arkham-api/config/settings.yml` and
["Advertising the catalog"](../docs/locale-catalog.md#advertising-the-catalog).

#### The fixture is synthetic; the catalog it points at is not

`fixtures/capabilities-locale-catalog.json` must show a real, verifiable
pointer, but a governed fixture cannot carry a *production* revision or digest:
the catalog is regenerated from `frontend/src/locales/**` on every content
change, and pinning its output here would force a contract revision bump every
time a translator fixed a typo — while a stale copy would have the fixture
self-attest a catalog no artifact in this repository has.

So the fixture is derived, mechanically, from a committed **synthetic** v1
catalog manifest, `fixtures/locale-catalog-manifest.json`:

- it is a fixed test artifact, schema-valid against the published
  `frontend/schemas/locale-catalog/v1/manifest.schema.json`, narrative-free
  (a manifest is an index — paths, digests, counts and locale tags), and is
  never regenerated from locale sources;
- `contracts:fixtures` derives `schemaVersion`, `catalogRevision`,
  `defaultLocale`, `supportedLocales` and `manifestSha256` from **that file's
  exact bytes** and requires the advertised block to equal the result, so any
  drift in either file fails. It also re-derives the manifest's own provenance
  — `catalogRevision` from `provenance.sha256`, `revisionManifestPath` from
  `catalogRevision`, every chunk path from its digest, `totals` from the
  catalog's contents, `provenance.contractRevision` from this manifest's
  `schemaRevision` — and self-tests prove those checks reject a mutated copy;
- `Helpers.LocaleCatalog` feeds the **same bytes** through the production
  settings pipeline, so the Haskell fixture assertions and the Python
  derivation cannot disagree.

The opposite risk — a synthetic shape no real deployment can produce — is
closed by `locale-catalog:capability-settings`, which derives the settings from
the manifest the frontend build actually generated and requires the response it
produces to satisfy this schema, while asserting it stays *different* from the
governed fixture. Nothing generated is ever hashed into `artifactHashes`.

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
(`scripts/strict_json.py`, a plain sibling module imported by all four
contract scripts) rather than a bare `json.load`/`json.loads`: it rejects
duplicate object keys at any nesting depth (including a plain key and a
distinct `\uXXXX`-escaped form that decodes to the same text), the
non-JSON `NaN`/`Infinity`/`-Infinity` constants the stdlib accepts by
default, non-strict UTF-8 decoding of raw bytes, an ordinary
(syntactically valid) number literal whose exponent overflows finite
`float` range — e.g. `1e9999` — which the stdlib's bare `float()`
constructor would otherwise silently coerce to `inf` with no error at all
(`parse_constant` only intercepts the three named `NaN`/`Infinity`/
`-Infinity` tokens, not ordinary number syntax that merely evaluates too
large), and an integer literal with more digits than Python 3.11+'s
default `int()`-conversion safety limit (4300 digits), which the stdlib's
bare `int()` constructor would otherwise raise an uncontrolled `ValueError`
for. The overflow guard parses the raw literal via `decimal.Decimal`
(exact, never silently overflows) to determine precisely whether it fits
in a finite `float`, and fails closed rather than ever returning `inf` —
`jsonschema`'s `"number"` type check does not recognize `Decimal`, so the
value is still returned as a plain `float` once confirmed finite. Three
further numeric edge cases, all closed by review, are also guarded
explicitly rather than left as latent bugs: (1) the magnitude comparison
itself uses `Decimal.copy_abs()` (never rounds, never uses a context) — an
exponent extreme enough (e.g. `1e1000000`) makes the bare `abs()` builtin's
context-rounded comparison raise `decimal.Overflow` internally, which is
caught and re-raised as a controlled `StrictJSONError` rather than
escaping raw; (2) a genuinely nonzero literal whose magnitude underflows
below the smallest positive `float` (e.g. `1e-1000000`, or even an
ordinary `1e-400`) is rejected outright rather than silently converted to
signed zero — accepting that silently could make a real, nonzero value
wrongly appear to satisfy a JSON Schema `"minimum"`/`"exclusiveMinimum"`
constraint; (3) any other `decimal.DecimalException` is caught as a final
defensive fallback. A dedicated self-test in
`scripts/validate-contract-fixtures.py` proves the underflow danger
directly against the real `jsonschema` validator this tooling uses (not
merely in isolation), then proves the guard prevents it. Every guard's own
self-tests run automatically whenever any of the four scripts is invoked,
proving the wiring end-to-end rather than only in isolation.

Every governed path string a script reads bytes for — whether declared in
`manifest.json`'s `documents`/`fixtures`/`basePositiveFixture` entries, or
constructed internally — is first validated by
`strict_json.validate_governed_path`: rejected are non-string/empty
values, an absolute path, a backslash, an ASCII control character, any
`.`/`..`/empty path segment (which would make the string a non-canonical
alias of a different path), and anything outside this contract's small
fixed set of governed locations (the four exact top-level documents, or a
single flat file directly under `contracts/schemas/`/`contracts/fixtures/`
— never a nested subdirectory). `strict_json.read_governed_worktree_bytes`
then reads the current on-disk content via `lstat`, rejecting a symlink,
directory, or other non-regular file outright (unlike `Path.is_file()`/
`Path.read_bytes()`, which follow a symlink transparently) — this matters
because a governed path that is actually an on-disk symlink could resolve
to different bytes than the same pathname's content in a historical git
tree, silently disagreeing about what the "same governed artifact" even
is. It also rejects an *executable* regular file (any on-disk permission
execute bit set), and cross-checks the git *index* (staged) and *HEAD*
(last-committed) tree modes for the same path (when tracked) both equal
non-executable mode `100644` and agree with each other: `stat.S_ISREG`
alone cannot distinguish a mode-`100644` file from a mode-`100755` one, and
(for identical content) both hash identically — so, left unchecked, a bare
`chmod +x` (or a purely-staged `git update-index --chmod=+x` with the
on-disk bytes/permissions themselves untouched) on a governed file would
pass silently today (same hash, no revision-drift signal) and only
permanently break once that state is later committed and reused as an
immutable base ref (since `read_governed_git_ref_bytes`, described below,
strictly requires exact mode `100644` from history and could never read it
again). Rejecting the mode change the moment it is introduced, at the
worktree/head side, means the hashing tools fail loudly immediately
instead of silently accepting a landmine. `strict_json.read_governed_git_ref_bytes`
mirrors this for a
historical/base ref: it first confirms via `git ls-tree` that the path
resolves to exactly one regular, non-executable blob (mode `100644`) —
never a symlink (`120000`), an executable file (`100755`), or a
submodule/gitlink (`160000`) — before reading its content with
`git show`. Both readers additionally run the strict-JSON/overflow checks
above before returning bytes to a caller, so a caller that goes on to hash
or schema-validate those bytes never does so for content that was not
first confirmed to be both a genuine, non-executable regular file and (for
`.json` paths) well-formed strict JSON. `scripts/update-manifest-hashes.py`,
`scripts/check-schema-revision-drift.py` (both the worktree and base-ref
hash computers), and `scripts/validate-contract-fixtures.py` all read every
governed path exclusively through these two functions; end-to-end
self-tests (real on-disk symlinks-to-identical-bytes, real throwaway git
commits with symlink/executable/gitlink tree entries, a real executable
on-disk file, and a real purely-staged mode-only change made in an
isolated throwaway git index — never the real repository index, so it can
never race with a concurrent script's `index.lock`) prove the rejections
actually fire against real filesystem/git state, not merely a unit-level
string check.

The manifest's own *current* bytes are not exempt from any of this: earlier,
`update-manifest-hashes.py` and `check-schema-revision-drift.py` each read
`contracts/manifest.json` via a bare `path.read_bytes()`-equivalent call,
bypassing every one of the checks above for the one governed path both
tools trust unconditionally — a manifest that was currently a symlink, or
executable, or staged/committed at the wrong mode, would pass silently and
only permanently break a future base-ref read once committed. Both tools
now read the current manifest exclusively through
`strict_json.read_governed_worktree_bytes(ROOT, "contracts/manifest.json")`,
exactly like every other governed path. `update-manifest-hashes.py`'s
write-back is similarly hardened: `strict_json.write_governed_worktree_bytes`
re-validates the destination's current on-disk/index/HEAD mode immediately
before writing, then publishes the new content via a temporary file in the
same directory followed by an atomic `os.replace` — which, unlike
`Path.write_text`/`open(path, "w")`, never dereferences a symlink at the
destination, so even in the (already-rejected) case of a symlinked
manifest path, the external file it points at is providably never written
to. End-to-end self-tests prove both the writer in isolation (executable,
staged-mode-disagreement, and symlink-to-external-target rejections, each
confirmed to leave the original/external bytes completely untouched) and
the two real CLI scripts end-to-end: each is copied into a small
throwaway, freestanding scratch git repository and actually invoked as a
subprocess against a current manifest that is executable on disk, staged
executable while on-disk bits disagree, committed at a disagreeing HEAD
mode, or a symlink to an external sentinel file — proving both tools
reject every case (and leave a symlink's external target unchanged),
while a canonical mode-`100644` manifest still works end-to-end through
both tools.

Contract changes must remain backward compatible with the Vue client. Runtime
changes belong in separate, small pull requests so they can be contributed
upstream independently of documentation and fixtures.
