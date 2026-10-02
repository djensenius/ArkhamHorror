# Night of the Zealot coverage recordings

This directory is the stable fixture target for task-1.2.13. The checked-in
`generated/*.json` files are the current deterministic recordings for the five
core investigators. The non-required `Night of the Zealot coverage` workflow
regenerates the same files and uploads them as an artifact for review.

Generate locally from the repository root with:

```bash
cd backend
ARKHAM_NOTZ_COVERAGE_DIR="$PWD/../contracts/coverage/night-of-the-zealot/generated" \
  stack test arkham-api:spec --system-ghc --test-arguments '--match "Night of the Zealot coverage generator"'
```

Each investigator file is deterministic for a fixed backend revision. It has:

- `schemaVersion`: currently `1`.
- `campaign`, `difficulty`, `botPolicy`, `investigator`, and `deck` metadata.
- `records[]`, one entry per answered prompt:
  - `scenario`
  - `investigator`
  - `stepIndex`
  - `questionVersion`
  - `playerId`
  - `rawQuestion`
  - `questionPresentation`
  - `chosenAnswer`
  - `choiceNote`
- `stop`, with the final reason and `stepsByScenario` counts.

The workflow also writes `summary.json` beside the per-investigator files.
