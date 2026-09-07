#!/usr/bin/env bash
# Runs inside the capability-stripping handoff, creates one invocation-specific
# toolchain receipt, delegates the dependency stage through a second isolated
# handoff, then publishes the completed receipt to GitHub's per-job environment.

set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"

[ "$#" -ge 1 ] || {
    echo "run-initial-authorized-stage: missing stage script" >&2
    exit 2
}

source "${SCRIPT_DIR}/utils.sh"
init_paths
init_toolchain_authority_receipt

ARKHAM_TOOLCHAIN_RECEIPT_FILE="$TOOLCHAIN_RECEIPT_FILE" \
ARKHAM_TOOLCHAIN_RECEIPT_TOKEN="$TOOLCHAIN_RECEIPT_TOKEN" \
    /bin/sh "${SCRIPT_DIR}/run-authorized-stage.sh" "$@"

publish_toolchain_authority_receipt
