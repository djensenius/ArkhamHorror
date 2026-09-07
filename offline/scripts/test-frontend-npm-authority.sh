#!/usr/bin/env bash
# Exercises the exact frontend dependency-install boundary without executing
# real package lifecycle code or modifying the checked-out frontend tree.

set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
REPO_ROOT="$(cd "${SCRIPT_DIR}/../.." && pwd)"
WORK="${REPO_ROOT}/offline/_tmp/test-frontend-npm-authority-$$-${RANDOM}"
PROJECT_ROOT="${WORK}/repo"
DEPS_DIR="${WORK}/deps"
TMP_DIR="${WORK}/tmp"
# shellcheck disable=SC2034
PLATFORM="macos-arm64"
# shellcheck disable=SC2034
NODE_NPM_CLI="lib/node_modules/npm/bin/npm-cli.js"
umask 077
mkdir -p "${PROJECT_ROOT}/frontend" "${DEPS_DIR}/node/bin" \
    "${DEPS_DIR}/node/lib/node_modules/npm/bin" \
    "${DEPS_DIR}/node/lib/node_modules/npm/lib" "$TMP_DIR"
trap 'rm -rf "$WORK"' EXIT

failures=0
fail() {
    printf 'frontend-npm-authority: %s\n' "$*" >&2
    failures=$((failures + 1))
}

die() {
    printf 'frontend-npm-authority: %s\n' "$*" >&2
    exit 1
}

ensure_dir() {
    mkdir -p "$1"
}

has_cmd() {
    command -v "$1" >/dev/null 2>&1
}

substep() { :; }
info() { :; }

sha256_file() {
    if command -v sha256sum >/dev/null 2>&1; then
        sha256sum "$1" | awk '{print $1}'
    else
        shasum -a 256 "$1" | awk '{print $1}'
    fi
}

FRONTEND_DIR="${PROJECT_ROOT}/frontend"
cat > "${FRONTEND_DIR}/package.json" <<'JSON'
{"name":"frontend-test","scripts":{"preinstall":"./lifecycle-payload.sh"}}
JSON
printf '{"lockfileVersion":3}\n' > "${FRONTEND_DIR}/package-lock.json"
printf '#!/usr/bin/env bash\ntouch "%s/lifecycle-ran"\n' "$WORK" > "${FRONTEND_DIR}/lifecycle-payload.sh"
chmod +x "${FRONTEND_DIR}/lifecycle-payload.sh"

cat > "${DEPS_DIR}/node/bin/node" <<NODE
#!/usr/bin/env bash
set -eu
cli="\${1:-}"
shift || true
if [ "\${1:-}" = "--version" ]; then
    printf '%s\\n' 'v26.7.0'
    exit 0
fi
if [ "\${1:-}" = "ci" ]; then
    printf '%s\\n' "\$@" > "${WORK}/npm-call"
    cache="\${npm_config_cache:?missing npm cache}"
    if [ -f "\${cache}/cache-marker" ]; then
        touch "${WORK}/npm-cache-hit"
    else
        touch "\${cache}/cache-marker"
    fi
    case " \$* " in
        *" --ignore-scripts "*) ;;
        *) "${FRONTEND_DIR}/lifecycle-payload.sh" ;;
    esac
    if grep -Fq '"lock-mismatch":true' "${FRONTEND_DIR}/package.json"; then
        mkdir -p "${FRONTEND_DIR}/node_modules/partial"
        printf 'partial cache' > "\${cache}/partial"
        exit 42
    fi
    mkdir -p "${FRONTEND_DIR}/node_modules/installed"
    printf 'exact dependency tree' > "${FRONTEND_DIR}/node_modules/installed/marker"
    exit 0
fi
printf '%s\\n' "unexpected verified-node invocation: \$cli \$*" >&2
exit 64
NODE
chmod +x "${DEPS_DIR}/node/bin/node"
printf '#!/usr/bin/env node\n' > "${DEPS_DIR}/node/lib/node_modules/npm/bin/npm-cli.js"
printf 'trusted npm import\n' > "${DEPS_DIR}/node/lib/node_modules/npm/lib/cli.js"

# Load the production helpers without bootstrapping its real toolchain or
# invoking main. The test supplies the verified Node authority below.
TESTABLE="${WORK}/03-build-frontend-testable.sh"
# shellcheck disable=SC2016
sed \
    -e '/^SCRIPT_DIR=/d' \
    -e '/^source "${SCRIPT_DIR}\/utils.sh"$/d' \
    -e '/^init_paths$/d' \
    -e '/^PLATFORM=/d' \
    -e '/^source "${SCRIPT_DIR}\/toolchain-authority.sh"$/d' \
    -e '/^require_toolchain_authority_receipt$/d' \
    -e '/^verify_node_installation$/d' \
    "${SCRIPT_DIR}/03-build-frontend.sh" > "$TESTABLE"
# shellcheck disable=SC1090
source "$TESTABLE"

EXPECTED_NODE="$(sha256_file "$OFFLINE_NODE")"
EXPECTED_NPM_CLI="$(sha256_file "$OFFLINE_NPM_CLI")"
EXPECTED_NPM_IMPORT="$(sha256_file "${DEPS_DIR}/node/lib/node_modules/npm/lib/cli.js")"
cp "$OFFLINE_NODE" "${WORK}/node.original"
cp "$OFFLINE_NPM_CLI" "${WORK}/npm-cli.original"
cp "${DEPS_DIR}/node/lib/node_modules/npm/lib/cli.js" "${WORK}/npm-import.original"
verify_node_installation() {
    [ "$(sha256_file "$OFFLINE_NODE")" = "$EXPECTED_NODE" ] \
        && [ "$(sha256_file "$OFFLINE_NPM_CLI")" = "$EXPECTED_NPM_CLI" ] \
        && [ "$(sha256_file "${DEPS_DIR}/node/lib/node_modules/npm/lib/cli.js")" = "$EXPECTED_NPM_IMPORT" ]
}

expect_reject() {
    local label="$1"
    shift
    if ("$@") >/dev/null 2>&1; then
        fail "${label} was accepted"
    fi
}

# Missing and linked lockfiles fail at the source/cache authority boundary.
mv "${FRONTEND_DIR}/package-lock.json" "${FRONTEND_DIR}/package-lock.json.real"
expect_reject "missing package-lock.json" require_frontend_lockfile
ln -s package-lock.json.real "${FRONTEND_DIR}/package-lock.json"
expect_reject "symlinked package-lock.json" require_frontend_lockfile
rm -f "${FRONTEND_DIR}/package-lock.json"
mv "${FRONTEND_DIR}/package-lock.json.real" "${FRONTEND_DIR}/package-lock.json"
require_frontend_lockfile

NM_REAL="${DEPS_DIR}/node_modules"
NM_LINK="${FRONTEND_DIR}/node_modules"
mkdir -p "$NM_REAL"
ln -s "$NM_REAL" "$NM_LINK"
prepare_frontend_npm_home
install_frontend_dependencies || fail "exact npm ci rejected a matching package-lock.json"
[ ! -e "${WORK}/lifecycle-ran" ] || fail "dependency lifecycle payload executed despite --ignore-scripts"
grep -Fx -- '--ignore-scripts' "${WORK}/npm-call" >/dev/null \
    || fail "verified npm CLI did not receive --ignore-scripts"
grep -Fx -- '--prefer-offline' "${WORK}/npm-call" >/dev/null \
    || fail "verified npm CLI did not receive --prefer-offline"
[ -f "${NM_REAL}/installed/marker" ] || fail "verified npm CLI did not create the exact dependency tree"
[ -L "$NM_LINK" ] && [ "$(readlink "$NM_LINK")" = "$NM_REAL" ] \
    || fail "successful npm ci tree was not moved to the controlled dependency location"
prepare_frontend_npm_home
install_frontend_dependencies || fail "cached exact npm ci rejected a matching package-lock.json"
[ -e "${WORK}/npm-cache-hit" ] \
    || fail "a second exact npm ci discarded the reusable verified package cache"

# A same-version Node binary or either npm execution-closure file cannot cross
# the post-install verification before catalog generation/build.
printf '\n# same version, substituted bytes\n' >> "$OFFLINE_NODE"
expect_reject "same-version Node substitution" verify_offline_node_runtime
cp "${WORK}/node.original" "$OFFLINE_NODE"
printf 'substituted npm CLI\n' > "$OFFLINE_NPM_CLI"
expect_reject "npm CLI substitution" verify_offline_node_runtime
cp "${WORK}/npm-cli.original" "$OFFLINE_NPM_CLI"
printf 'substituted npm import\n' > "${DEPS_DIR}/node/lib/node_modules/npm/lib/cli.js"
expect_reject "npm transitive import substitution" verify_offline_node_runtime
cp "${WORK}/npm-import.original" "${DEPS_DIR}/node/lib/node_modules/npm/lib/cli.js"
verify_offline_node_runtime || fail "restored Node/npm closure was rejected"

# This mirrors the mandatory verification immediately after npm ci and before
# catalog generation: a mutation made between those two operations fails.
printf '\n# mutate between ci and generation\n' >> "$OFFLINE_NODE"
expect_reject "Node mutation between npm ci and catalog generation" verify_offline_node_runtime
cp "${WORK}/node.original" "$OFFLINE_NODE"

# A manifest/lock mismatch causes ci to fail, and neither the partial modules
# nor the partially populated package cache survives as a valid build input.
printf '{"name":"frontend-test","lock-mismatch":true}\n' > "${FRONTEND_DIR}/package.json"
prepare_frontend_npm_home
expect_reject "manifest/lock mismatch npm ci" install_frontend_dependencies
[ ! -e "$NM_LINK" ] && [ ! -L "$NM_LINK" ] \
    || fail "failed npm ci left a source-tree node_modules entry"
[ ! -e "${NM_REAL}/partial" ] || fail "failed npm ci left a partial node_modules tree"
[ ! -e "$FRONTEND_NPM_CACHE" ] || fail "failed npm ci left a partial package cache"

SOURCE="${SCRIPT_DIR}/03-build-frontend.sh"
if grep -Fq 'npm install' "$SOURCE"; then
    fail "authoritative frontend script retains an unlocked npm install path"
fi
WORKFLOW="${REPO_ROOT}/.github/workflows/build-offline.yml"
grep -Fq 'offline/_deps/npm-cache/' "$WORKFLOW" \
    || fail "workflow does not cache verified npm package tarballs"
if grep -Fq 'offline/_deps/node_modules/' "$WORKFLOW"; then
    fail "workflow restores an untrusted node_modules tree"
fi
grep -Fq 'run_offline_npm ci --ignore-scripts --prefer-offline' "$SOURCE" \
    || fail "authoritative script does not invoke the verified npm CLI for dependency installation"
# shellcheck disable=SC2016
grep -Fq '"$OFFLINE_NODE" "$OFFLINE_NPM_CLI" "$@"' "$SOURCE" \
    || fail "authoritative script does not invoke npm through the verified Node/npm CLI pair"
install_line="$(grep -n 'install_frontend_dependencies$' "$SOURCE" | tail -1 | cut -d: -f1)"
lock_line="$(grep -n 'require_frontend_lockfile$' "$SOURCE" | tail -1 | cut -d: -f1)"
node_modules_line="$(grep -n '^    NM_LINK=' "$SOURCE" | head -1 | cut -d: -f1)"
lock_rechecks="$(grep -n 'verify_frontend_lockfile_identity$' "$SOURCE" | cut -d: -f1)"
generator_line="$(grep -n 'Generate the locale catalog with the pinned offline Node' "$SOURCE" | head -1 | cut -d: -f1)"
build_line="$(grep -n 'run_offline_npm run build -- --outDir' "$SOURCE" | head -1 | cut -d: -f1)"
node_checks="$(grep -n 'verify_offline_node_runtime$' "$SOURCE" | cut -d: -f1)"
[ -n "$install_line" ] && [ -n "$lock_line" ] && [ -n "$node_modules_line" ] \
    && [ -n "$generator_line" ] && [ -n "$build_line" ] \
    || fail "authoritative script lacks required install/generation/build stages"
if [ "$lock_line" -ge "$node_modules_line" ]; then
    fail "package-lock.json is not required before node_modules is touched"
fi
if ! printf '%s\n' "$lock_rechecks" | awk -v node_modules="$node_modules_line" '$1 < node_modules { found = 1 } END { exit !found }'; then
    fail "package-lock.json is not rechecked before node_modules is touched"
fi
if ! printf '%s\n' "$node_checks" | awk -v install="$install_line" -v generator="$generator_line" -v build="$build_line" '
    $1 > install && $1 < generator { after_install = 1 }
    $1 > generator && $1 < build { before_build = 1 }
    END { exit !(after_install && before_build) }
'; then
    fail "Node authority is not rechecked between ci, generation, and build"
fi

if [ "$failures" -ne 0 ]; then
    printf 'frontend-npm-authority: %s failure(s)\n' "$failures" >&2
    exit 1
fi
printf '%s\n' 'frontend-npm-authority: lock, exact npm ci, lifecycle, and Node/npm mutation protections passed'
