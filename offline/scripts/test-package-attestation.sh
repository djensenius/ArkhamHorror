#!/usr/bin/env bash
# Ensures the post-package attester authenticates the final copied frontend
# tree, rather than trusting only the pre-copy frontend source tree.

set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
REPO_ROOT="$(cd "${SCRIPT_DIR}/../.." && pwd)"
WORK="${REPO_ROOT}/offline/_tmp/test-package-attestation-$$-${RANDOM}"
PACKAGE="${WORK}/ArkhamHorror-test"
GAME="${PACKAGE}/game"
umask 077
mkdir -p "${GAME}/bin" "${GAME}/lib" "${GAME}/pgsql/lib" "${GAME}/config" "${GAME}/frontend/dist/assets"
trap 'rm -rf "$WORK"' EXIT

# shellcheck disable=SC1091,SC2034
source "${SCRIPT_DIR}/utils.sh"
init_paths
# shellcheck disable=SC2034
TOOLCHAIN_RECEIPT_DIR="${WORK}/receipts"
export GITHUB_ENV=""
init_toolchain_authority_receipt

printf 'test nginx\n' > "${GAME}/bin/nginx"
chmod +x "${GAME}/bin/nginx"
printf '#!/usr/bin/env bash\n' > "${GAME}/start.sh"
chmod +x "${GAME}/start.sh"
printf 'types { application/json json; }\n' > "${GAME}/config/mime.types"
printf 'self-issued provenance\n' > "${GAME}/config/toolchain-provenance.env"
cp "${REPO_ROOT}/offline/toolchain.lock" "${GAME}/config/toolchain.lock"
printf '<!doctype html>\n' > "${GAME}/frontend/dist/index.html"
printf 'trusted bundle\n' > "${GAME}/frontend/dist/assets/app.js"

frontend_identity="$(printf '%s' 'test-frontend-inputs' | sha256_text)"
frontend_closure="$(authority_tree_digest "${GAME}/frontend/dist")"
record_authority_receipt frontend "$frontend_identity" "$frontend_closure"

run_attester() {
    ARKHAM_TOOLCHAIN_RECEIPT_FILE="$TOOLCHAIN_RECEIPT_FILE" \
    ARKHAM_TOOLCHAIN_RECEIPT_TOKEN="$TOOLCHAIN_RECEIPT_TOKEN" \
        /bin/sh "${SCRIPT_DIR}/run-authorized-stage.sh" \
            "${SCRIPT_DIR}/attest-package-closure.sh" "$PACKAGE"
}

run_attester

printf 'substituted final package bundle\n' > "${GAME}/frontend/dist/assets/app.js"
if run_attester >/dev/null 2>&1; then
    printf '%s\n' 'package-attestation: final package frontend substitution was accepted' >&2
    exit 1
fi

printf '%s\n' 'package-attestation: final copied frontend substitution is rejected by external attestation'
