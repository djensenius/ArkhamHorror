#!/bin/sh
#
# Exercise drop-stale-generated.sh against a fake .stack-work layout.
#
# Usage: sh backend/scripts/test-drop-stale-generated.sh
set -eu

SCRIPT="$(cd "$(dirname "$0")" && pwd)/drop-stale-generated.sh"

TMP=$(mktemp -d)
trap 'rm -rf "$TMP"' EXIT INT TERM

API="$TMP/api"
DISCOVER="$TMP/discover"
DIST="$API/.stack-work/dist/aarch64-osx/ghc-9.14.1"
BUILD="$DIST/build"
CACHES="$DIST/stack-build-caches"
STAMP="$API/.stack-work/.cards-discover-hash"

fail() {
  echo "FAIL: $*" >&2
  exit 1
}

mkdir -p "$API/library/Arkham" "$DISCOVER/app" "$DISCOVER/library"
echo '{-# OPTIONS_GHC -F -pgmF cards-discover -optF Arkham.Asset.Cards #-}' \
  > "$API/library/Arkham/Generated.hs"
echo 'module Arkham.Plain where' > "$API/library/Arkham/Plain.hs"
echo 'main = pure ()' > "$DISCOVER/app/Main.hs"
echo 'module Discover where' > "$DISCOVER/library/Discover.hs"
echo 'module Spaced where' > "$DISCOVER/library/With Space.hs"

populate() {
  # Stack 3.7.1 keeps one plain file per component under the hash directory.
  mkdir -p "$BUILD/Arkham" "$CACHES/0123abcd"
  for ext in o hi dyn_o dyn_hi; do
    : > "$BUILD/Arkham/Generated.$ext"
  done
  : > "$BUILD/Arkham/Plain.o"
  : > "$CACHES/0123abcd/lib"
  : > "$CACHES/0123abcd/exe-arkham-api"
}

run() {
  sh "$SCRIPT" "$API" "$DISCOVER" > /dev/null
}

assert_dropped() {
  for ext in o hi dyn_o dyn_hi; do
    [ ! -e "$BUILD/Arkham/Generated.$ext" ] || fail "$1: Generated.$ext survived"
  done
  [ ! -e "$CACHES" ] || fail "$1: stack-build-caches survived"
  [ -f "$BUILD/Arkham/Plain.o" ] || fail "$1: unrelated Plain.o was removed"
  [ -s "$STAMP" ] || fail "$1: stamp not written"
}

assert_kept() {
  for ext in o hi dyn_o dyn_hi; do
    [ -f "$BUILD/Arkham/Generated.$ext" ] || fail "$1: Generated.$ext was removed"
  done
  [ -f "$CACHES/0123abcd/lib" ] || fail "$1: stack-build-caches lib was removed"
  [ -f "$CACHES/0123abcd/exe-arkham-api" ] \
    || fail "$1: stack-build-caches exe-arkham-api was removed"
  [ -f "$BUILD/Arkham/Plain.o" ] || fail "$1: unrelated Plain.o was removed"
}

# (d) DRY_RUN deletes nothing and writes no stamp. Not `DRY_RUN=1 run`: POSIX
# lets an assignment before a function call outlive it.
populate
DRY_RUN=1 sh "$SCRIPT" "$API" "$DISCOVER" > /dev/null
assert_kept "dry run"
[ ! -e "$STAMP" ] || fail "dry run: stamp was written"

# (a) No stamp yet: the generated artifacts and Stack's build caches go.
run
assert_dropped "first run"

# (b) Generator unchanged: a rebuilt cache is left alone.
populate
stamp=$(cat "$STAMP")
run
assert_kept "unchanged generator"
[ "$(cat "$STAMP")" = "$stamp" ] || fail "unchanged generator: stamp changed"

# (c) Generator edited: everything goes again.
echo '-- changed' >> "$DISCOVER/library/Discover.hs"
run
assert_dropped "changed generator"
[ "$(cat "$STAMP")" != "$stamp" ] || fail "changed generator: stamp not updated"

# (e) A file name with a space is hashed whole, not split.
populate
stamp=$(cat "$STAMP")
echo '-- changed' >> "$DISCOVER/library/With Space.hs"
run
assert_dropped "changed spaced file"
[ "$(cat "$STAMP")" != "$stamp" ] || fail "changed spaced file: stamp not updated"

# (f) An unreadable source stops the script before the stamp moves. Skipped
# where chmod cannot make it unreadable (e.g. running as root).
populate
stamp=$(cat "$STAMP")
echo '-- changed' >> "$DISCOVER/library/Discover.hs"
chmod 000 "$DISCOVER/library/Discover.hs"
if ! cat "$DISCOVER/library/Discover.hs" > /dev/null 2>&1; then
  if sh "$SCRIPT" "$API" "$DISCOVER" > /dev/null 2>&1; then
    fail "unreadable source: script succeeded"
  fi
  [ "$(cat "$STAMP")" = "$stamp" ] || fail "unreadable source: stamp changed"
  assert_kept "unreadable source"
fi
chmod 644 "$DISCOVER/library/Discover.hs"

echo "ok: drop-stale-generated.sh"
