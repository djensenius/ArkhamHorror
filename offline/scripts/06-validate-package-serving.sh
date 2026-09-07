#!/usr/bin/env bash
# shellcheck disable=SC1091
# Runs the exact offline package serving gate without exposing the invocation
# receipt capability to its generated launcher or nginx subprocess.

set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
source "${SCRIPT_DIR}/utils.sh"

init_paths
require_toolchain_authority_receipt

PACKAGE_DIR="${1:-${_DIST_DIR}/ArkhamHorror-$(detect_platform)}"
[ -d "$PACKAGE_DIR" ] && [ ! -L "$PACKAGE_DIR" ] \
    || die "Offline package is missing or unsafe: $PACKAGE_DIR"

readonly SYSTEM_PYTHON="/usr/bin/python3"
[ -x "$SYSTEM_PYTHON" ] || die "Trusted system Python is unavailable: $SYSTEM_PYTHON"

exec 9<<EOF
${TOOLCHAIN_RECEIPT_FILE}
${TOOLCHAIN_RECEIPT_TOKEN}
EOF

exec "$SYSTEM_PYTHON" "${PROJECT_ROOT}/scripts/validate-catalog-serving.py" \
    --offline-package "$PACKAGE_DIR" \
    --offline-authority-fd 9
