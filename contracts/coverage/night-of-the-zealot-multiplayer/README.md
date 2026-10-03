# Night of the Zealot multiplayer coverage recordings

This directory is the stable fixture target for task-2.20. The fork commits
only `summary.json`: a small, pretty-printed, sorted-key summary of the latest
2-, 3-, and 4-investigator WithFriends coverage runs. Full prompt recordings are
intentionally not committed because they contain raw questions and semantic
presentations for every answered prompt; they are generated as JSONL into the
gitignored `generated/` directory and uploaded by the non-required multiplayer
coverage workflow.

Generate locally from the repository root with:

```bash
mise run coverage:notz-multiplayer
```

Override the full-recording output directory if needed:

```bash
ARKHAM_NOTZ_MULTIPLAYER_COVERAGE_DIR=/tmp/notz-multiplayer-coverage mise run coverage:notz-multiplayer
```

The task writes `2p.jsonl`, `3p.jsonl`, `4p.jsonl`, and `summary.json`, then
fails if the generated summary differs from the committed
`contracts/coverage/night-of-the-zealot-multiplayer/summary.json`. The Haskell
spec also fails unless each run stops at `IsOver` with a real `01142` campaign
resolution recorded.

Each JSONL line is one answered prompt with sorted keys and includes:

- `scenario` and `scenarioKey`
- `investigator`
- `stepIndex`
- `questionVersion`
- `playerId` (the owner of the pending prompt being answered)
- `rawQuestion`
- `questionPresentation`
- `chosenAnswer`
- `choiceNote`
- `chosenChoiceKind`

The bot answers the active player first when that player has a pending question,
then the first coverage player with a pending question, always using that
player's own `game.question[playerId]` entry and submitting through the same
server answer path as the solo coverage generator. It prefers semantic choices
that resolve/start skill tests or skip optional triggers so multiplayer commit
windows do not loop forever, then rotates selectable choices when the same
per-player question shape repeats. Amount prompts use the minimum legal
allocation, token exchange uses zero, and campaign continuation uses the
server-provided next step.

`ChooseDeck`, `ChooseUpgradeDeck`, and `ChooseJoinDeck` prompts are recorded as
answers, but the pure `handleAnswerPure` seam deliberately rejects deck answers
because the production HTTP handler also updates deck/player database rows. The
coverage generator therefore applies the same server deck-loading message
constructor (`deckChosen`) directly, which keeps gameplay rules out of the test
while still driving the engine with the selected starter deck. Upgrade prompts
are recorded if they occur; the bot continues without upgrading.

The generated decklists are the same legal coverage decks used by the solo Night
of the Zealot fixture. The 2-, 3-, and 4-player runs use the first two, three,
and four core investigators respectively, with deterministic player ids and
run-level RNG seeds (`22000 + playerCount`) so the runs are reproducible. The
per-player `weaknessSeed` values in `summary.json` are fixture inputs used to
choose deterministic core basic weaknesses; they are not the multiplayer run RNG
seed.

## Hidden information finding

The server's REST and websocket game update projection is `PublicGame` in
`backend/arkham-api/library/Arkham/Game.hs`. The task-2.20 Haskell test is a
projection/wire-encoding check: it encodes `PublicGame` through `toEncoding`,
decodes those bytes, and verifies that the projection contains every
investigator entry, including other players' non-empty `hand` and `deck` fields
and the owner's own ordered `deck` field. That means hidden information is
present in the `PublicGame` bytes.

The per-participant conclusion comes from the server delivery code, not from a
per-user filter in the test: `Api/Handler/Arkham/Games.hs:148-158` builds the
same `PublicGame gameId g.name gameLog.entries g.currentData` for the REST game
payload while only the envelope `player` changes, and
`Api/Handler/Arkham/Games/Shared.hs:617-623` publishes a single
`GameUpdate (PublicGame gameId arkhamGameName publishLog arkhamGameCurrentData)`
to the whole room. The projection is therefore not filtered per participant.

The web client treats that payload as trusted game state and hides other hands in
the UI by default rather than relying on a server-side projection. In
`frontend/src/arkham/components/HandCard.vue`, hand cards render only for solo,
the owning investigator, revealed cards, or when the local
`showOtherPlayersHands` preference is enabled. `frontend/src/arkham/components/Player.vue`
applies the same preference/ownership gate to in-hand enemies and treacheries.
I found no matching UI gate for the full ordered `deck` field; that deck-order
leak affects the owner as well as other players. No gameplay or projection
change is made here; this coverage task reports the existing hidden-information
leak for follow-up client/server decisions.
