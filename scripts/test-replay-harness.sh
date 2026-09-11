#!/bin/sh
set -eu
ROOT=$(CDPATH= cd -- "$(dirname "$0")/.." && pwd)
STACK="stack --stack-yaml backend/stack.yaml"
RUN_MODE=0
cd "$ROOT"
if [ "${1-}" = "--run" ]; then
  RUN_MODE=1
  shift
  $STACK build arkham-api:exe:arkham-replay --fast
else
  $STACK test arkham-api:exe:arkham-replay arkham-api:test:spec --fast \
    --test-arguments='--match Arkham.Replay'
fi
DIST=$($STACK path --dist-dir)
DIST_ABS="$ROOT/backend/arkham-api/$DIST"
OBJECT_DIR="$DIST_ABS/build/arkham-replay/arkham-replay-tmp"
EXE="$DIST_ABS/build/arkham-replay/arkham-replay"
GHC_VERSION=$($STACK exec -- ghc --numeric-version)
SETUP=$(find "$HOME/.stack/setup-exe-cache" -type f -name "Cabal-simple_*_ghc-$GHC_VERSION" | head -n 1)
test -n "$SETUP"
build_replay() {
  rm -f "$OBJECT_DIR/Main.o" "$OBJECT_DIR/Main.hi" \
    "$OBJECT_DIR/Main.dyn_o" "$OBJECT_DIR/Main.dyn_hi" "$EXE"
  if [ -n "${1-}" ]; then
    (cd backend/arkham-api && ARKHAM_REPLAY_ATTEST_SOURCE_SHA256="$1" \
      "$SETUP" --builddir="$DIST_ABS" build exe:arkham-replay --ghc-options "")
  else
    (cd backend/arkham-api && \
      "$SETUP" --builddir="$DIST_ABS" build exe:arkham-replay --ghc-options "")
  fi
}
build_replay
IDENTITY=$("$EXE" --build-identity)
ATTESTATION=$(printf '%s' "$IDENTITY" | python3 -c 'import json,sys; print(json.load(sys.stdin)["attestation"])')
if [ "$RUN_MODE" = "1" ]; then
  exec "$EXE" "$@"
fi
if [ "$ATTESTATION" = "unattested" ]; then
  python3 scripts/test-replay-harness-process.py "$EXE" --expect-unattested
  SOURCE_SHA=$(printf '%s' "$IDENTITY" | python3 -c 'import json,sys; print(json.load(sys.stdin)["sourceSha256"])')
  build_replay "$SOURCE_SHA"
else
  SOURCE_SHA=
fi
IDENTITY=$("$EXE" --build-identity)
printf '%s' "$IDENTITY" |
  python3 -c 'import json,sys; value=json.load(sys.stdin); assert value["attestation"] != "unattested", value'
python3 scripts/test-replay-harness-process.py "$EXE"
