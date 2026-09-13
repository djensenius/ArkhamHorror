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
run_replay_build() {
  if [ -n "${1-}" ]; then
    (cd backend/arkham-api && ARKHAM_REPLAY_ATTEST_SOURCE_SHA256="$1" \
      "$SETUP" --builddir="$DIST_ABS" build exe:arkham-replay --ghc-options "")
  else
    (cd backend/arkham-api && \
      "$SETUP" --builddir="$DIST_ABS" build exe:arkham-replay --ghc-options "")
  fi
}
run_replay_build_bounded() {
  python3 - "$ROOT/backend/arkham-api" "$SETUP" "$DIST_ABS" "${1-}" <<'PY'
import os
import signal
import subprocess
import sys

working_directory, setup, build_directory, expected = sys.argv[1:]
environment = os.environ.copy()
if expected:
    environment["ARKHAM_REPLAY_ATTEST_SOURCE_SHA256"] = expected
process = subprocess.Popen(
    [
        setup,
        f"--builddir={build_directory}",
        "build",
        "exe:arkham-replay",
        "--ghc-options",
        "",
    ],
    cwd=working_directory,
    env=environment,
    start_new_session=True,
)
try:
    return_code = process.wait(timeout=180)
except subprocess.TimeoutExpired:
    os.killpg(process.pid, signal.SIGKILL)
    process.wait()
    print("replay build exceeded the 180-second regression bound", file=sys.stderr)
    raise SystemExit(124)
raise SystemExit(return_code)
PY
}
build_replay() {
  rm -f "$OBJECT_DIR/Main.o" "$OBJECT_DIR/Main.hi" \
    "$OBJECT_DIR/Main.dyn_o" "$OBJECT_DIR/Main.dyn_hi" "$EXE"
  run_replay_build "${1-}"
}
identity_sha() {
  printf '%s' "$1" |
    python3 -c 'import json,sys; print(json.load(sys.stdin)["sourceSha256"])'
}
identity_clean() {
  printf '%s' "$1" |
    python3 -c 'import json,sys; print(str(json.load(sys.stdin)["sourceClean"]).lower())'
}
identity_attestation() {
  printf '%s' "$1" |
    python3 -c 'import json,sys; print(json.load(sys.stdin)["attestation"])'
}
test_build_identity_source_dependencies() {
  baseline=$1
  baseline_sha=$(identity_sha "$baseline")
  probe_dir=backend/arkham-api/app-replay/tmp
  ignored_probe="$probe_dir/ReplayBuildIdentityIgnoredProbe.hs"
  untracked_probe=backend/arkham-api/replay-build-identity-untracked-probe.txt
  untracked_fifo=backend/arkham-api/replay-build-identity-untracked-probe.fifo
  framing_probe_a=backend/arkham-api/replay-build-identity-frame-a.bin
  framing_probe_b=backend/arkham-api/replay-build-identity-frame-b.bin
  unicode_path_probe_a="$probe_dir/ReplayBuildIdentity$(printf '\320\220')Probe.hs"
  unicode_path_probe_b="$probe_dir/ReplayBuildIdentity$(printf '\324\220')Probe.hs"
  tracked_byte_probe=backend/arkham-api/tests/fixtures/replay/build-identity-byte-probe.txt
  tracked_byte_backup="$DIST_ABS/build-identity-byte-probe.backup"
  interface_dump="$DIST_ABS/build-identity-main.iface"
  test ! -e "$ignored_probe"
  test ! -e "$untracked_probe"
  test ! -e "$untracked_fifo"
  test ! -e "$framing_probe_a"
  test ! -e "$framing_probe_b"
  test ! -e "$unicode_path_probe_a"
  test ! -e "$unicode_path_probe_b"
  git ls-files --error-unmatch -- "$tracked_byte_probe" >/dev/null
  cp "$tracked_byte_probe" "$tracked_byte_backup"
  sleep 1
  mkdir -p "$probe_dir"
  trap 'cp "$tracked_byte_backup" "$tracked_byte_probe"; rm -f "$tracked_byte_backup" "$ignored_probe" "$untracked_probe" "$untracked_fifo" "$framing_probe_a" "$framing_probe_b" "$unicode_path_probe_a" "$unicode_path_probe_b" "$interface_dump"; rmdir "$probe_dir" 2>/dev/null || true' 0 HUP INT TERM

  cat >"$ignored_probe" <<'EOF'
module ReplayBuildIdentityIgnoredProbe where

probe :: Int
probe = 1
EOF
  printf '%s\n' "untracked build input 1" >"$untracked_probe"
  git check-ignore -q -- "$ignored_probe"
  if git check-ignore -q -- "$untracked_probe"; then
    printf '%s\n' "build identity regression probe unexpectedly ignored: $untracked_probe" >&2
    exit 1
  fi
  mkfifo "$untracked_fifo"
  if git check-ignore -q -- "$untracked_fifo"; then
    printf '%s\n' "build identity FIFO probe unexpectedly ignored: $untracked_fifo" >&2
    exit 1
  fi
  rm -f "$OBJECT_DIR/Main.o" "$OBJECT_DIR/Main.hi" \
    "$OBJECT_DIR/Main.dyn_o" "$OBJECT_DIR/Main.dyn_hi" "$EXE"
  if ! run_replay_build_bounded; then
    printf '%s\n' "build identity blocked or failed on an untracked FIFO" >&2
    exit 1
  fi
  special=$("$EXE" --build-identity)
  test "$(identity_clean "$special")" = false
  test "$(identity_attestation "$special")" = unattested
  $STACK exec -- ghc --show-iface "$OBJECT_DIR/Main.hi" >"$interface_dump"
  if grep -F "addDependentFile \"$ROOT/$untracked_fifo\"" "$interface_dump" >/dev/null; then
    printf '%s\n' "build identity registered an untracked FIFO as a source dependency" >&2
    exit 1
  fi
  rm -f "$interface_dump" "$untracked_fifo"

  run_replay_build
  ignored=$("$EXE" --build-identity)
  ignored_sha=$(identity_sha "$ignored")
  test "$(identity_clean "$ignored")" = false
  test "$ignored_sha" != "$baseline_sha"
  $STACK exec -- ghc --show-iface "$OBJECT_DIR/Main.hi" >"$interface_dump"
  for source in \
    backend/arkham-api/library/Arkham/Replay/BuildIdentity.hs \
    backend/arkham-api/app/main.hs \
    backend/arkham-api/app-replay/Main.hs \
    backend/arkham-api/app-capabilities-probe/Main.hs \
    backend/cards-discover/library/Cards/Discover.hs \
    backend/cards-discover/app/Main.hs \
    backend/devel-store-lock/library/DevelStoreLock.hs \
    backend/stack.yaml \
    "$ignored_probe" \
    "$untracked_probe"; do
    grep -F "addDependentFile \"$ROOT/$source\"" "$interface_dump" >/dev/null
  done
  rm -f "$interface_dump"

  sleep 1
  cat >"$ignored_probe" <<'EOF'
module ReplayBuildIdentityIgnoredProbe where

probe :: Int
probe = 2
EOF
  run_replay_build
  ignored_changed=$("$EXE" --build-identity)
  ignored_changed_sha=$(identity_sha "$ignored_changed")
  test "$ignored_changed_sha" != "$ignored_sha"

  sleep 1
  printf '%s\n' "untracked build input 2" >"$untracked_probe"
  run_replay_build
  untracked_changed=$("$EXE" --build-identity)
  untracked_changed_sha=$(identity_sha "$untracked_changed")
  test "$untracked_changed_sha" != "$ignored_changed_sha"

  sleep 1
  rm -f "$ignored_probe" "$untracked_probe"
  printf 'payload\000%s\000' "$framing_probe_b" >"$framing_probe_a"
  run_replay_build
  single_record=$("$EXE" --build-identity)
  single_record_sha=$(identity_sha "$single_record")

  sleep 1
  printf '%s' "payload" >"$framing_probe_a"
  : >"$framing_probe_b"
  run_replay_build
  two_records=$("$EXE" --build-identity)
  two_records_sha=$(identity_sha "$two_records")
  test "$two_records_sha" != "$single_record_sha"

  sleep 1
  rm -f "$framing_probe_a" "$framing_probe_b"

  printf '\304\200\n' >"$tracked_byte_probe"
  run_replay_build
  first_diff=$("$EXE" --build-identity)
  first_diff_sha=$(identity_sha "$first_diff")

  sleep 1
  printf '\310\200\n' >"$tracked_byte_probe"
  run_replay_build
  second_diff=$("$EXE" --build-identity)
  second_diff_sha=$(identity_sha "$second_diff")
  test "$second_diff_sha" != "$first_diff_sha"

  sleep 1
  cp "$tracked_byte_backup" "$tracked_byte_probe"
  cat >"$unicode_path_probe_a" <<'EOF'
module ReplayBuildIdentityUnicodeProbe where
EOF
  run_replay_build
  first_path=$("$EXE" --build-identity)
  first_path_sha=$(identity_sha "$first_path")

  sleep 1
  rm -f "$unicode_path_probe_a"
  cat >"$unicode_path_probe_b" <<'EOF'
module ReplayBuildIdentityUnicodeProbe where
EOF
  run_replay_build
  second_path=$("$EXE" --build-identity)
  second_path_sha=$(identity_sha "$second_path")
  test "$second_path_sha" != "$first_path_sha"

  sleep 1
  rm -f "$unicode_path_probe_b"
  rmdir "$probe_dir" 2>/dev/null || true
  run_replay_build
  restored=$("$EXE" --build-identity)
  test "$(identity_sha "$restored")" = "$baseline_sha"
  rm -f "$tracked_byte_backup"
  trap - 0 HUP INT TERM
  printf '%s\n' "build identity source appearance, raw bytes, framing, and dependency regression: ok"
}
build_replay
IDENTITY=$("$EXE" --build-identity)
ATTESTATION=$(printf '%s' "$IDENTITY" | python3 -c 'import json,sys; print(json.load(sys.stdin)["attestation"])')
if [ "$RUN_MODE" = "1" ]; then
  exec "$EXE" "$@"
fi
test_build_identity_source_dependencies "$IDENTITY"
if [ "$ATTESTATION" = "unattested" ]; then
  python3 backend/arkham-api/tests/Arkham/Replay/ProcessFixture.py "$EXE" --expect-unattested
  SOURCE_SHA=$(printf '%s' "$IDENTITY" | python3 -c 'import json,sys; print(json.load(sys.stdin)["sourceSha256"])')
  build_replay "$SOURCE_SHA"
else
  SOURCE_SHA=
fi
IDENTITY=$("$EXE" --build-identity)
printf '%s' "$IDENTITY" |
  python3 -c 'import json,sys; value=json.load(sys.stdin); assert value["attestation"] != "unattested", value'
python3 backend/arkham-api/tests/Arkham/Replay/ProcessFixture.py "$EXE"
