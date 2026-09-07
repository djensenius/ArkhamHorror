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

expect_build_option_reject() {
    local label="$1" expected="$2"
    shift 2
    local log="${WORK}/build-option-${label}.log"
    if bash "${REPO_ROOT}/offline/build_all.sh" "$@" >"$log" 2>&1; then
        fail "${label} build options were accepted"
    elif ! grep -Fq "$expected" "$log"; then
        fail "${label} build options failed without the authority explanation"
    fi
}

GAME="${WORK}/package/game"
mkdir -p "${GAME}/bin" "${GAME}/lib" "${GAME}/pgsql/lib" "${GAME}/config"
printf 'trusted nginx executable\n' > "${GAME}/bin/nginx"
chmod +x "${GAME}/bin/nginx"
printf 'trusted bundled library\n' > "${GAME}/lib/libpcre.dylib"
ln -s "libpcre.dylib" "${GAME}/lib/libpcre-alias.dylib"
printf 'trusted postgres library\n' > "${GAME}/pgsql/lib/libpq.dylib"
printf '#!/usr/bin/env bash\n' > "${GAME}/start.sh"
chmod +x "${GAME}/start.sh"
printf 'types { application/json json; }\n' > "${GAME}/config/mime.types"
cp "${REPO_ROOT}/offline/toolchain.lock" "${GAME}/config/toolchain.lock"
printf 'nginx_binary_sha256=self-issued\n' > "${GAME}/config/toolchain-provenance.env"

identity="$(printf '%s' 'locked-nginx-build-identity' | sha256_text)"
closure="$(authority_paths_digest "$GAME" \
    bin/nginx lib pgsql/lib start.sh config/mime.types config/toolchain.lock config/toolchain-provenance.env)"
PYTHONDONTWRITEBYTECODE=1 python3 - "$GAME" "$closure" "$REPO_ROOT" <<'PY'
import importlib.util
import sys
from pathlib import Path

game = Path(sys.argv[1])
expected = sys.argv[2]
repository = Path(sys.argv[3])
spec = importlib.util.spec_from_file_location(
    "catalog_serving", repository / "scripts" / "validate-catalog-serving.py"
)
module = importlib.util.module_from_spec(spec)
assert spec.loader is not None
spec.loader.exec_module(module)
actual = module.closure_digest(
    game,
    (
        "bin/nginx",
        "lib",
        "pgsql/lib",
        "start.sh",
        "config/mime.types",
        "config/toolchain.lock",
        "config/toolchain-provenance.env",
    ),
)
if actual != expected:
    raise SystemExit(f"shell/Python package closure mismatch: {actual} != {expected}")
PY
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
printf 'trusted bundled library\n' > "${GAME}/lib/libpcre.dylib"
rm -f "${GAME}/lib/libpcre-alias.dylib"
ln -s /etc/passwd "${GAME}/lib/libpcre-alias.dylib"
expect_reject "escaping bundled-library symlink" \
    verify_authority_paths offline-nginx "$identity" "$GAME" \
      bin/nginx lib pgsql/lib start.sh config/mime.types config/toolchain.lock config/toolchain-provenance.env

if grep -Eq 'ARKHAM_(RELEASE_AUTHORITY|REQUIRE_EXTERNAL_AUTHORITY)' "${REPO_ROOT}/offline/scripts/05-package.sh"; then
    fail "generated package launcher accepts CI authority capabilities"
fi
grep -Fq 'nginx_runtime_closure_sha256' "${REPO_ROOT}/offline/scripts/05-package.sh" \
    || fail "generated package launcher does not bind its nginx/library closure before loading"
grep -Fq 'verify_nginx_runtime_closure' "${REPO_ROOT}/offline/scripts/05-package.sh" \
    || fail "generated package launcher does not verify its nginx/library closure"
grep -Fq 'record_authority_receipt backend' "${REPO_ROOT}/offline/scripts/04-build-backend.sh" \
    || fail "backend build never authenticates its invocation output"
grep -Fq 'verify_authority_paths backend "$(backend_output_identity)"' "${REPO_ROOT}/offline/scripts/05-package.sh" \
    || fail "--skip-backend can package unauthenticated or stale backend output"
expect_build_option_reject skip-deps \
    "dependency authority must be established during the current invocation" \
    --skip-deps
expect_build_option_reject skip-frontend-package \
    "packaging accepts only outputs built and attested during the current invocation" \
    --skip-frontend
expect_build_option_reject skip-backend-package \
    "packaging accepts only outputs built and attested during the current invocation" \
    --skip-backend
if grep -Eq 'ARKHAM_(RELEASE_AUTHORITY|REQUIRE_EXTERNAL_AUTHORITY)' "${REPO_ROOT}/scripts/validate-catalog-serving.py"; then
    fail "serving validator exposes CI authority capabilities to package nginx"
fi
grep -Fq 'attest-package-closure.sh' "${REPO_ROOT}/.github/workflows/build-offline.yml" \
    || fail "workflow does not attest the completed package outside 05-package.sh"
grep -Fq '.tar.gz.sha256' "${REPO_ROOT}/.github/workflows/build-offline.yml" \
    || fail "workflow does not publish a detached release archive checksum"
grep -Fq 'attest-package-closure.sh --final' "${REPO_ROOT}/.github/workflows/build-offline.yml" \
    || fail "workflow does not attest the complete package immediately before archiving"
grep -Fq 'scripts/05-package.sh "${VERSION}"' "${REPO_ROOT}/.github/workflows/build-offline.yml" \
    || fail "workflow does not bind the release tag inside the packaged tree"
grep -Fq 'game/tools/node' "${REPO_ROOT}/offline/scripts/05-package.sh" \
    || fail "package does not carry its lock-attested updater Node runtime"
grep -Fq 'update-archive.mjs' "${REPO_ROOT}/offline/scripts/05-package.sh" \
    || fail "package does not carry its reviewed updater archive helper"
if grep -Eq '(^|[^[:alnum:]_])python3([[:space:]]|$)' "${REPO_ROOT}/offline/scripts/update-runtime.sh"; then
    fail "shipped updater still depends on an ambient Python interpreter"
fi
if ! grep -Fq 'persist-credentials: false' "${REPO_ROOT}/.github/workflows/build-offline.yml" \
    || awk '/^permissions:$/ { getline; if ($0 == "  contents: write") found=1 } END { exit !found }' \
        "${REPO_ROOT}/.github/workflows/build-offline.yml"; then
    fail "build/dependency workflow code can retain a write-capable repository credential"
fi

if [ "$failures" -ne 0 ]; then
    printf 'package-authority: %s failure(s)\n' "$failures" >&2
    exit 1
fi
printf '%s\n' 'package-authority: binary/provenance and bundled-library substitutions are rejected by external authority'
