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

PACKAGE_DIR="${1:-}"
[ -n "$PACKAGE_DIR" ] || die "Usage: attest-package-closure.sh <ArkhamHorror-package-dir>"
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

info "Recorded external CI authority for the complete packaged nginx closure"
