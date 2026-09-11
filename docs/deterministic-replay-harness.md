# Deterministic backend replay checkpoints

`arkham-replay` is a fail-closed CLI/test seam. It uses the normal Haskell
`Game`, fixed seed, `handleAnswerPure`, queue runner, undo, and optional server
simulation; it adds neither another rules implementation nor a production API.

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

The output is an `ArkhamExport` with a mandatory `replayCheckpoint` envelope.
Its provenance binds the plan/source hashes and kinds, game revision, complete
build identity, contract revision, applied counts, exact checkpoint, and
Game/queue hashes. `envelopeSha256` covers the typed export plus every provenance
field except itself. Missing or changed metadata fails closed.

Production import accepts the extra envelope but persists only typed
`ArkhamExport` fields. A later production export is therefore an ordinary
export with no original lineage. Retain/pin the original checkpoint and its
printed full-file SHA-256 for the native workflow.

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
