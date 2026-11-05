#!/bin/sh
#
# Drop arkham-api build artifacts for the modules `cards-discover` generates,
# whenever the generator itself has changed.
#
# Why this exists:
#
# A module with
#
#   {-# OPTIONS_GHC -F -pgmF cards-discover ... #-}
#
# is a one-line pragma on disk; its real contents come out of cards-discover at
# compile time. GHC's recompilation check hashes the *un-preprocessed* source
# plus the import list it parses back out of the preprocessor's output -- the
# generator is not an input it knows about. So when cards-discover starts
# emitting something new (a new export, a new declaration, a different shape),
# every one of those modules looks unchanged and keeps the interface it was last
# compiled with. The first importer that mentions the new name then fails:
#
#   Module 'Arkham.Homebrew.UltimatumEntries' does not export 'DiscoveredModules'
#
# ...on a cache GHC is certain is up to date. Only dropping the stale artifacts
# fixes it; touching the sources does not, since Stack and GHC compare content
# hashes rather than mtimes.
#
# We therefore keep a stamp of the generator's own sources beside the cache. When
# it moves, the generated modules' artifacts go. Their dependents are left to
# GHC: the regenerated interfaces either hash the same (no cascade) or differ
# (normal cascade), which is exactly what the missing dependency edge would have
# given us.
#
# Dropping the objects alone is not enough, though. Stack keeps its own
# per-component record of the source hashes it last built with
# (.stack-work/dist/*/ghc-*/stack-build-caches), and it only hands a component to
# Cabal/GHC when that record disagrees with the tree. The preprocessed source
# never exists on disk, so after a generator-only change the record still
# matches, Stack skips arkham-api entirely, the missing objects go unnoticed and
# `stack install` copies the old executable. Removing the record too makes Stack
# run the build, and GHC then recompiles exactly the modules dropped here.
#
# Set DRY_RUN=1 to print what would be removed.
#
# Usage: drop-stale-generated.sh <arkham-api-dir> <cards-discover-dir>
#
# Docker runs this from scripts/docker-build-api.sh. A local build reuses its
# .stack-work the same way, so after pulling a cards-discover change, run from
# backend/ before building:
#
#   sh scripts/drop-stale-generated.sh arkham-api cards-discover
#
# `stack build --force-dirty` alone does not help: GHC still sees the unchanged
# one-line source and keeps the stale module. The blunt fallback is
# `stack clean arkham-api`, which recompiles all ~9,400 modules.
set -eu

API=${1:?arkham-api package directory}
DISCOVER=${2:?cards-discover package directory}

WORK="$API/.stack-work"
STAMP="$WORK/.cards-discover-hash"

# Ubuntu has sha256sum, macOS only shasum.
if command -v sha256sum >/dev/null 2>&1; then
  checksum="sha256sum"
else
  checksum="shasum -a 256"
fi

# Only the generator's hand-written sources: .stack-work below it holds build
# products whose contents are not stable between builds. The leading tag
# versions the stamp: bumping it forces one drop on every existing cache (v2:
# caches stamped before stack-build-caches was dropped may still be stale).
#
# No pipelines here: a pipeline reports only its last command's status, so an
# unreadable source would be hashed as partial input instead of stopping set -e
# before the stamp is written. Paths are read whole, so spaces are fine.
sources=$(find "$DISCOVER/app" "$DISCOVER/library" -type f -name '*.hs')
sources=$(printf '%s\n' "$sources" | LC_ALL=C sort)
input=$(mktemp)
trap 'rm -f "$input"' EXIT
{
  echo 'drop-stale-generated v2'
  while IFS= read -r source; do
    [ -n "$source" ] || continue
    cat "$source"
  done <<EOF
$sources
EOF
} > "$input"
hash=$($checksum < "$input")
hash=${hash%% *}

previous=$(cat "$STAMP" 2>/dev/null || true)

if [ "$hash" = "$previous" ]; then
  exit 0
fi

# An empty stamp also drops: either the cache predates this script, or it was
# written by a generator we cannot identify. Both are the stale case.
modules=$(
  find "$API/library" -type f -name '*.hs' -exec grep -lF 'pgmF cards-discover' {} + \
    | sed -e "s|^$API/library/||" -e 's|\.hs$||'
)

echo ">> cards-discover changed -- dropping stale interfaces for $(printf '%s\n' "$modules" | grep -c .) generated module(s)"

for build in "$WORK"/dist/*/ghc-*/build; do
  [ -d "$build" ] || continue
  for mod in $modules; do
    for ext in o hi dyn_o dyn_hi p_o p_hi; do
      artifact="$build/$mod.$ext"
      [ -f "$artifact" ] || continue
      if [ "${DRY_RUN:-}" = 1 ]; then
        echo "   would remove $artifact"
      else
        rm -f "$artifact"
      fi
    done
  done
done

# Stack's record of what it last built; see the header for why it must go too.
for caches in "$WORK"/dist/*/ghc-*/stack-build-caches; do
  [ -d "$caches" ] || continue
  if [ "${DRY_RUN:-}" = 1 ]; then
    echo "   would remove $caches"
  else
    rm -rf "$caches"
  fi
done

if [ "${DRY_RUN:-}" != 1 ]; then
  mkdir -p "$WORK"
  printf '%s\n' "$hash" > "$STAMP"
fi
