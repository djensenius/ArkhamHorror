# Deterministic backend replay checkpoints

`arkham-replay` is a fail-closed CLI/test seam. It uses the normal Haskell
`Game`, fixed seed, `handleAnswerPure`, queue runner, undo, and optional server
simulation. It adds no rules implementation or public replay/answer API; the
only HTTP addition is authenticated, game-bound import-attestation metadata.

Inspect an export (optionally after `--undo N`):

```sh
mise run replay:harness -- game-export.json --inspect-checkpoint > inspection.json
```

The inspection reports the exact input SHA-256/kind, game revision, governed
capabilities `schemaRevision`, executable build identity, retained checkpoint
provenance, and every open question's version/player/tag/full-prompt SHA-256.

## Plan

```json
{
  "schemaVersion": 1,
  "mode": "answers",
  "source": {
    "exportSha256": "64-lowercase-hex",
    "inputKind": "export",
    "gameGitRevision": "40-lowercase-hex",
    "replayBuild": {
      "gitRevision": "40-lowercase-hex",
      "gitTree": "40-lowercase-hex",
      "sourceSha256": "64-lowercase-hex",
      "sourceClean": true,
      "attestation": "git-clean"
    },
    "schemaRevision": "0.1.34"
  },
  "answers": [{
    "expect": {
      "type": "question",
      "name": "intro",
      "questionVersion": 7,
      "playerId": "00000000-0000-0000-0000-000000000001",
      "promptTag": "ChooseOne",
      "promptSha256": "64-lowercase-hex"
    },
    "answer": {
      "tag": "Answer",
      "contents": {
        "choice": 0,
        "playerId": "00000000-0000-0000-0000-000000000001",
        "questionVersion": 7
      }
    }
  }],
  "stopAt": {
    "type": "question",
    "name": "native-driver-start",
    "questionVersion": 8,
    "playerId": "00000000-0000-0000-0000-000000000001",
    "promptTag": "ChooseOne",
    "promptSha256": "64-lowercase-hex"
  }
}
```

```sh
mise run replay:harness -- game-export.json --replay-script replay-plan.json \
  --checkpoint-output native-driver.checkpoint.json --simulate-server
```

Each Answer is matched to its expected prompt before normal handling. Embedded
player/version fields must be exact. The export must retain its current
`ArkhamStep`, including step `0`; its residual queue stays parked, and `stopAt`
is checked before any drain. Deterministic history/`--replay-all` is rejected:
`choiceMessages` is a residual queue, not the Answer that produced the step.

## Build and checkpoint authority

The executable embeds its Git revision/tree, a SHA-256 over revision/tree plus
the complete backend diff and untracked backend bytes, source cleanliness, and
an attestation (`git-clean`, exact build-system `source-sha256`, or
`unattested`). Git metadata is a compile dependency and replay `Main` is
force-recompiled; runtime Git state is never trusted. Dirty development process
tests explicitly rebuild against the previously embedded exact source digest.
Unattested, false-clean, stale, or mismatched identities reject replay.
`arkham-replay --build-identity` prints this embedded identity.

The running `arkham-api` independently embeds the same build-identity shape.
`GET /api/v1/capabilities` returns it as JSON in
`X-Arkham-Backend-Build-Identity`. This is the compiled server authority, not
`Game.gameGitRevision` (the revision retained in game state), and no request
field, query parameter, or runtime Git checkout can replace it. A local native
workflow must reject an absent/malformed header, `unattested`, a false-clean
claim, or any identity unequal to both `arkham-replay --build-identity` and the
checkpoint provenance. `arkham-api --build-identity` prints the exact value
compiled into the server executable without starting network or database
services; it must equal the capabilities header once that executable is
running. Live checkpoint import is intentionally stricter than offline replay:
both the checkpoint/replay build and running server must report
`sourceClean: true` with `attestation: "git-clean"`. A source-SHA-attested dirty
development build may still exercise the offline harness, but it cannot create
a live replay game or produce game-bound authority. The current Docker build
omits Git metadata and therefore remains `unattested`; live replay stays
fail-closed until that build supplies independently verified source authority.

The output is an `ArkhamExport` with a mandatory `replayCheckpoint` envelope.
Its provenance binds the plan/source hashes and kinds, game revision, complete
build identity, contract revision, applied counts, exact checkpoint, and
Game/queue hashes. `envelopeSha256` covers the typed export plus every provenance
field except itself. Missing or changed metadata fails closed.

Normal game import remains the only import endpoint. For checkpoint bytes it
runs `decodeReplayInputEnvelope` and the complete backend checkpoint validator
before opening the game-creation transaction. That validator parses the typed
export and provenance, recomputes the canonical envelope digest over both,
compares it with the embedded `envelopeSha256`, and requires the complete JSON
bytes to equal the deterministic encoding the backend itself produces.
Changing export/provenance fields, adding ignored fields, duplicate-key or
whitespace rewrites, and retaining a plausible embedded digest are all rejected
before any game row exists.

The successful `POST /api/v1/arkham/games/import` body remains the production
handler's complete `PublicGame ArkhamGameId` JSON snapshot. Its top-level `id`
identifies the imported game, but the body is not an ID-only `{ "id": ... }`
envelope. Both ordinary exports and replay checkpoints return this same full
body together with `X-Arkham-Backend-Build-Identity`. A successful checkpoint
import additionally returns `X-Arkham-Replay-Import-Receipt`; ordinary exports
omit only that receipt header.

The checkpoint receipt's schema-versioned JSON binds the new game id, retained
`gameGitRevision`, import-time clean backend build, SHA-256 of the exact
decompressed checkpoint bytes, server-recomputed
`canonicalEnvelopeSha256`, full checkpoint provenance, the
checkpoint/imported/live player-ID mapping, whether game state was remapped,
and a digest over the receipt. The same receipt is persisted atomically with
the game, players, and retained steps. The server does not accept
caller-provided expected identities or digests. The Apple coordinator must
decode the full `PublicGame` body and both applicable authority headers from
this actual response rather than substituting an ID-only response contract.

After import, an authenticated administrator or member of that exact game can
read `GET /api/v1/arkham/games/{gameId}/replay-attestation`. It returns the
persisted receipt, exact checkpoint-byte digest, full checkpoint provenance,
canonical envelope digest, retained game revision, and the build identity compiled into the currently
running server. Ordinary games return 404. Malformed persisted authority, a
changed game revision, a dirty/unattested server, or any import-time/current
build mismatch fails closed rather than returning an attestation. The route is
metadata-only: it exposes no rules state, prompt, answer bridge, or mutation.

The Apple live driver must require all of the following:

1. capabilities `X-Arkham-Backend-Build-Identity`, import-response backend
   identity, attestation `runningServerBuild`, receipt `backendBuild`, checkpoint
   provenance `replayBuild`, and `arkham-replay --build-identity` are identical
   clean Git identities;
2. the import response's full `PublicGame.id`, attestation/receipt game id, and
   the requested game id are identical;
3. the retained game revision equals the attestation, receipt, and checkpoint
   provenance revision;
4. the import header receipt equals the durable `importReceipt`, including its
   player remapping and receipt digest;
5. the checkpoint byte SHA-256 and server-recomputed canonical envelope digest
   equal the locally retained checkpoint authorities.

Any missing, malformed, or unequal value keeps live replay disabled.

The checkpoint rebases to step `0`; one synthetic step carries the pending
queue. Prior undo patches, action diffs, and UI/log history are intentionally
absent, so historical undo/log reconstruction is unavailable. The authoritative
Game, seed, open question, and next continuation remain intact.

## Filesystem guarantees and limits

Export/script inputs use no-follow opens, descriptor `fstat`, descriptor reads,
and identity/content-digest revalidation before publication; inspection uses
the same path. Aliases, symlinks, hard links, directories, special files, and
changed parents/destinations are rejected. Output parents are opened with
`O_DIRECTORY|O_NOFOLLOW` and retained; stages are exclusively created, written,
and published relative to those descriptors. Each stage descriptor remains open
through publication or cleanup, preventing a removed inode from being reused as
false ownership. Cleanup and publication first atomically capture the visible
stage entry under a private sibling name, then verify it against the retained
descriptor before unlinking or renaming it. A foreign replacement is restored
without clobbering another concurrent entry. Secondaries publish first; the
checkpoint uses an atomic hard-link-to-absent operation, so concurrent writers
cannot clobber it. Acquisition and each ownership handoff stay masked, while
potentially blocking writes, fsyncs, hooks, and publication operations remain
interruptible with cleanup already armed.

Only schema `1`, exact answer scripts, and question checkpoints are supported.
Every scripted answer constructor must match the exact current prompt before its
messages can enter the queue. Malformed/stale prompts, incompatible constructors,
missing steps, provenance drift, unhandled/unused answers, and unreachable stops
exit non-zero without a checkpoint. Database-only answers, database/epic-event
side effects, and historical undo/log replay remain outside this harness.

`mise run replay:harness:test` also runs the committed process fixture through
the actual executable. It proves pre-drain queue preservation, answer messages
running before a non-empty retained queue across an answer boundary, a nonzero
`--undo` plus exact Answer reaching a later prompt, byte-identical simulated and
unsimulated checkpoints, server metric spans, checkpoint import/inspection, and
the fail-closed rejection matrix (including an actually unattested build).
