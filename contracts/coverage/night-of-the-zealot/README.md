# Night of the Zealot coverage recordings

This directory is the stable fixture target for task-1.2.13. The fork commits
only `summary.json`: a small, pretty-printed, sorted-key summary of the latest
coverage run. Full prompt recordings are intentionally not committed because
they contain raw questions and semantic presentations for every answered prompt;
they are generated as JSONL into the gitignored `generated/` directory and
uploaded by the non-required `Night of the Zealot coverage` workflow.

Generate locally from the repository root with:

```bash
mise run coverage:notz
```

Override the full-recording output directory if needed:

```bash
ARKHAM_NOTZ_COVERAGE_DIR=/tmp/notz-coverage mise run coverage:notz
```

The task writes one `<investigator>.jsonl` file per core investigator plus a
`summary.json` beside them, then fails if that generated summary differs from
the committed `contracts/coverage/night-of-the-zealot/summary.json`. The Haskell
spec also fails if any run does not reach the end of `01142`.

Each JSONL line is one answered prompt with sorted keys and includes:

- `scenario` and `scenarioKey`
- `investigator`
- `stepIndex`
- `questionVersion`
- `playerId`
- `rawQuestion`
- `questionPresentation`
- `chosenAnswer`
- `choiceNote`

The bot answers from the server's semantic presentation: it picks the first
`presentation.choices[i].selectable` choice, rotating to the next selectable
choice when the same question shape repeats. Amount prompts use the minimum
legal allocation, token exchange uses zero, and campaign continuation uses the
server-provided next step.

`ChooseDeck`, `ChooseUpgradeDeck`, and `ChooseJoinDeck` prompts are recorded as
answers, but the pure `handleAnswerPure` seam deliberately rejects deck answers
because the production HTTP handler also updates deck/player database rows. The
coverage generator therefore applies the same server deck-loading message
constructor (`deckChosen`) directly, which keeps gameplay rules out of the test
while still driving the engine with the selected starter deck. Upgrade prompts
are recorded if they occur; the bot continues without upgrading.

Starter decks include a deterministic core random basic weakness per
investigator, selected from a fixed seed so the run is reproducible.
