#!/usr/bin/env bash
# Ensures the package nginx executable and dynamic-library closure require an
# external invocation receipt rather than self-issued package metadata.

set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
REPO_ROOT="$(cd "${SCRIPT_DIR}/../.." && pwd)"
WORK="${REPO_ROOT}/offline/_tmp/test-package-authority-$$-${RANDOM}"
umask 077
mkdir -p "$WORK"
trap 'rm -rf "$WORK"' EXIT

source "${SCRIPT_DIR}/utils.sh"
init_paths
TMP_DIR="${WORK}/receipt-tmp"
DEPS_DIR="${WORK}/deps"
PLATFORM="macos-arm64"
TOOLCHAIN_RECEIPT_DIR="${WORK}/receipt-tmp"
export GITHUB_ENV=""
init_toolchain_authority_receipt

failures=0
fail() {
    printf 'package-authority: %s\n' "$*" >&2
    failures=$((failures + 1))
}

expect_reject() {
    local label="$1"
    shift
    if ("$@") >/dev/null 2>&1; then
        fail "${label} was accepted"
    fi
}

GAME="${WORK}/package/game"
mkdir -p "${GAME}/bin" "${GAME}/lib" "${GAME}/pgsql/lib" "${GAME}/config"
printf 'trusted nginx executable\n' > "${GAME}/bin/nginx"
chmod +x "${GAME}/bin/nginx"
printf 'trusted bundled library\n' > "${GAME}/lib/libpcre.dylib"
printf 'trusted postgres library\n' > "${GAME}/pgsql/lib/libpq.dylib"
printf '#!/usr/bin/env bash\n' > "${GAME}/start.sh"
chmod +x "${GAME}/start.sh"
printf 'types { application/json json; }\n' > "${GAME}/config/mime.types"
cp "${REPO_ROOT}/offline/toolchain.lock" "${GAME}/config/toolchain.lock"
printf 'nginx_binary_sha256=self-issued\n' > "${GAME}/config/toolchain-provenance.env"

identity="$(printf '%s' 'locked-nginx-build-identity' | sha256_text)"
closure="$(authority_paths_digest "$GAME" \
    bin/nginx lib pgsql/lib start.sh config/mime.types config/toolchain.lock config/toolchain-provenance.env)"
record_authority_receipt offline-nginx "$identity" "$closure"
verify_authority_paths offline-nginx "$identity" "$GAME" \
    bin/nginx lib pgsql/lib start.sh config/mime.types config/toolchain.lock config/toolchain-provenance.env \
    || fail "fresh package closure was rejected"

printf 'substituted nginx executable\n' > "${GAME}/bin/nginx"
chmod +x "${GAME}/bin/nginx"
printf 'nginx_binary_sha256=substituted-self-issued-value\n' > "${GAME}/config/toolchain-provenance.env"
expect_reject "substituted nginx plus rewritten provenance" \
    verify_authority_paths offline-nginx "$identity" "$GAME" \
      bin/nginx lib pgsql/lib start.sh config/mime.types config/toolchain.lock config/toolchain-provenance.env

printf 'trusted nginx executable\n' > "${GAME}/bin/nginx"
chmod +x "${GAME}/bin/nginx"
printf 'nginx_binary_sha256=self-issued\n' > "${GAME}/config/toolchain-provenance.env"
verify_authority_paths offline-nginx "$identity" "$GAME" \
    bin/nginx lib pgsql/lib start.sh config/mime.types config/toolchain.lock config/toolchain-provenance.env \
    || fail "restored package closure was rejected"

printf 'substituted bundled library\n' > "${GAME}/lib/libpcre.dylib"
expect_reject "substituted bundled library" \
    verify_authority_paths offline-nginx "$identity" "$GAME" \
      bin/nginx lib pgsql/lib start.sh config/mime.types config/toolchain.lock config/toolchain-provenance.env

external_line="$(grep -n '^verify_external_nginx_authority$' "${REPO_ROOT}/offline/scripts/05-package.sh" | tail -1 | cut -d: -f1)"
runtime_line="$(grep -n '^configure_runtime_env$' "${REPO_ROOT}/offline/scripts/05-package.sh" | tail -1 | cut -d: -f1)"
if [ -z "$external_line" ] || [ -z "$runtime_line" ] || [ "$external_line" -ge "$runtime_line" ]; then
    fail "package configures host library paths before checking external release authority"
fi
grep -Fq 'attest-package-closure.sh' "${REPO_ROOT}/.github/workflows/build-offline.yml" \
    || fail "workflow does not attest the completed package outside 05-package.sh"
grep -Fq '.tar.gz.sha256' "${REPO_ROOT}/.github/workflows/build-offline.yml" \
    || fail "workflow does not publish a detached release archive checksum"

if [ "$failures" -ne 0 ]; then
    printf 'package-authority: %s failure(s)\n' "$failures" >&2
    exit 1
fi
printf '%s\n' 'package-authority: binary/provenance and bundled-library substitutions are rejected by external authority'
