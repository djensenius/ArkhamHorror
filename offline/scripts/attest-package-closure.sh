#!/usr/bin/env bash
# CI-side release authority for a completed offline package. This runs after
# 05-package.sh so no package-local metadata can authorize its own bytes.

set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
source "${SCRIPT_DIR}/utils.sh"

init_paths
PLATFORM="$(detect_platform)"
source "${SCRIPT_DIR}/toolchain-authority.sh"
require_toolchain_authority_receipt

FINAL=false
if [ "${1:-}" = "--final" ]; then
    FINAL=true
    shift
fi
PACKAGE_DIR="${1:-}"
[ -n "$PACKAGE_DIR" ] && [ "$#" = 1 ] \
    || die "Usage: attest-package-closure.sh [--final] <ArkhamHorror-package-dir>"
[ -d "$PACKAGE_DIR" ] && [ ! -L "$PACKAGE_DIR" ] || die "Package directory is missing or unsafe: $PACKAGE_DIR"
PACKAGE_DIR="$(cd "$PACKAGE_DIR" && pwd)"
GAME_DIR="${PACKAGE_DIR}/game"
[ -d "$GAME_DIR" ] && [ ! -L "$GAME_DIR" ] || die "Package game directory is missing or unsafe: $GAME_DIR"

verify_file_sha256 "${GAME_DIR}/config/toolchain.lock" "$(toolchain_lock_digest)" \
    "packaged toolchain authority"

identity="$(nginx_build_identity)"
closure="$(authority_paths_digest "$GAME_DIR" \
    "bin/nginx" \
    "lib" \
    "pgsql/lib" \
    "start.sh" \
    "config/mime.types" \
    "config/toolchain.lock" \
    "config/toolchain-provenance.env")" \
    || die "Could not calculate the packaged nginx/library closure"
record_authority_receipt offline-nginx "$identity" "$closure"

frontend_identity="$(authority_receipt_identity frontend)" \
    || die "Frontend source receipt is missing before final package attestation"
verify_authority_tree_from_receipt frontend "${GAME_DIR}/frontend/dist" \
    || die "Final packaged frontend tree does not match its source authority receipt"
frontend_closure="$(authority_tree_digest "${GAME_DIR}/frontend/dist")" \
    || die "Could not calculate the final packaged frontend closure"
record_authority_receipt offline-frontend "$frontend_identity" "$frontend_closure"

if [ "$FINAL" = true ]; then
    # This is the archive input closure, not merely the executable subset.
    # authority_tree_digest rejects symlinks and special files; the packager's
    # materializer has already broken hardlink aliases into independent files.
    package_identity="$(printf 'offline-package-v1\t%s\t%s\n' \
        "$PLATFORM" "$(toolchain_lock_digest)" | sha256_text)"
    package_closure="$(authority_tree_digest "$PACKAGE_DIR")" \
        || die "Could not calculate the complete final package closure"
    record_authority_receipt offline-package "$package_identity" "$package_closure"
    info "Recorded external CI authority for the complete final package tree"
else
    info "Recorded external CI authority for packaged nginx and frontend closures"
fi
