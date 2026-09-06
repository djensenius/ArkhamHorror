#!/usr/bin/env bash
# =============================================================================
# test-toolchain-authority.sh - adversarial checks for the offline toolchain
# authority boundary. Uses only an invocation-owned directory under offline/_tmp.
# =============================================================================

set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
REPO_ROOT="$(cd "${SCRIPT_DIR}/../.." && pwd)"

WORK="${REPO_ROOT}/offline/_tmp/test-toolchain-authority-$$-${RANDOM}"
umask 077
mkdir -p "$WORK/bin"
trap 'rm -rf "$WORK"' EXIT

# shellcheck disable=SC1091
source "${SCRIPT_DIR}/utils.sh"
init_paths
# shellcheck disable=SC1091
source "${SCRIPT_DIR}/toolchain-authority.sh"
TMP_DIR="${WORK}/toolchain-receipts"
TOOLCHAIN_RECEIPT_DIR="${WORK}/toolchain-receipts"
export GITHUB_ENV=""
init_toolchain_authority_receipt
[ ! -z "$TOOLCHAIN_RECEIPT_TOKEN" ] || {
    printf '%s\n' 'toolchain-authority: receipt token was not initialized' >&2
    exit 1
}
if grep -Fq "$TOOLCHAIN_RECEIPT_TOKEN" "$TOOLCHAIN_RECEIPT_FILE"; then
    printf '%s\n' 'toolchain-authority: receipt persists its secret capability' >&2
    exit 1
fi
case "$TOOLCHAIN_RECEIPT_FILE" in
    *"$TOOLCHAIN_RECEIPT_TOKEN"*)
        printf '%s\n' 'toolchain-authority: receipt path exposes its secret capability' >&2
        exit 1
        ;;
esac

failures=0
fail() {
    printf 'toolchain-authority: %s\n' "$*" >&2
    failures=$((failures + 1))
}

expect_reject() {
    local label="$1"
    shift
    if ("$@") >/dev/null 2>&1; then
        fail "${label} was accepted"
    fi
}

cat > "${WORK}/bin/curl" <<'CURL'
#!/usr/bin/env bash
set -euo pipefail
if [ -n "${ARKHAM_TOOLCHAIN_RECEIPT_FILE:-}" ] || [ -n "${ARKHAM_TOOLCHAIN_RECEIPT_TOKEN:-}" ]; then
    printf 'receipt capability leaked to archive downloader\n' >> "${CURL_STUB_LEAKS:?}"
    exit 97
fi
out=""
while [ "$#" -gt 0 ]; do
    case "$1" in
        -o) out="$2"; shift 2 ;;
        *) shift ;;
    esac
done
[ -n "$out" ] || exit 2
printf '%s' "${CURL_STUB_PAYLOAD:?}" > "$out"
printf 'download\n' >> "${CURL_STUB_CALLS:?}"
CURL
chmod +x "${WORK}/bin/curl"

ORIGINAL_PATH="$PATH"
export PATH="${WORK}/bin:${ORIGINAL_PATH}"
export CURL_STUB_CALLS="${WORK}/curl-calls"
export CURL_STUB_LEAKS="${WORK}/curl-leaks"
CACHE_DIR="${WORK}/cache"
TMP_DIR="$CACHE_DIR"
good_bytes='trusted archive bytes'
good_sha="$(printf '%s' "$good_bytes" | sha256_text)"
export CURL_STUB_PAYLOAD="$good_bytes"

first="$(download_cached 'https://authority.invalid/good.tar.gz' 'good.tar.gz' "$good_sha")"
[ "$(cat "$first")" = "$good_bytes" ] || fail "verified download did not preserve its bytes"
[ "$(wc -l < "$CURL_STUB_CALLS" | tr -d ' ')" = "1" ] || fail "first download did not call curl exactly once"
[ ! -e "$CURL_STUB_LEAKS" ] || fail "toolchain receipt capability leaked to the archive downloader"

second="$(download_cached 'https://authority.invalid/good.tar.gz' 'good.tar.gz' "$good_sha")"
[ "$second" = "$first" ] || fail "cache hit returned a different path"
[ "$(wc -l < "$CURL_STUB_CALLS" | tr -d ' ')" = "1" ] || fail "verified cache hit downloaded again"

printf 'corrupted cache' > "$first"
third="$(download_cached 'https://authority.invalid/good.tar.gz' 'good.tar.gz' "$good_sha")"
[ "$(cat "$third")" = "$good_bytes" ] || fail "corrupted cache was not replaced with verified bytes"
[ "$(wc -l < "$CURL_STUB_CALLS" | tr -d ' ')" = "2" ] || fail "corrupted cache did not force one redownload"

export CURL_STUB_PAYLOAD='substituted archive bytes'
expect_reject "wrong downloaded archive" \
    download_cached 'https://authority.invalid/wrong.tar.gz' 'wrong.tar.gz' "$good_sha"
[ ! -e "${CACHE_DIR}/wrong.tar.gz" ] || fail "wrong downloaded archive remained in cache"
[ ! -e "${CACHE_DIR}/wrong.tar.gz.partial.$$" ] || fail "wrong downloaded archive left a partial file"

printf 'not-a-tarball' > "${WORK}/wrong.tar.gz"
expect_reject "wrong archive before extraction" \
    extract_tgz "${WORK}/wrong.tar.gz" "${WORK}/must-not-extract" 0 "$good_sha"
[ ! -e "${WORK}/must-not-extract" ] || fail "wrong archive reached extraction"

lock="${REPO_ROOT}/offline/toolchain.lock"
if ! awk -F '\t' '
    /^[[:space:]]*#/ || NF == 0 { next }
    NF != 7 { exit 1 }
    $1 !~ /^(archive|binary|image)$/ { exit 1 }
    $5 !~ /^(exact|derived)$/ { exit 1 }
    $6 !~ /^[0-9a-f]{64}$/ { exit 1 }
    {
        key = $1 SUBSEP $2 SUBSEP $3 SUBSEP $4
        if (seen[key]++) exit 1
    }
    END { if (NR == 0) exit 1 }
' "$lock"; then
    fail "toolchain authority table has malformed or duplicate records"
fi
for platform in macos-arm64 macos-x86_64 linux-x86_64 linux-arm64; do
    for component in ghc stack node postgres nginx; do
        archive_count="$(awk -F '\t' -v component="$component" -v platform="$platform" '$1 == "archive" && $2 == component && $3 == platform { matches += 1 } END { print matches + 0 }' "$lock")"
        binary_count="$(awk -F '\t' -v component="$component" -v platform="$platform" '$1 == "binary" && $2 == component && $3 == platform { matches += 1 } END { print matches + 0 }' "$lock")"
        [ "$archive_count" = 1 ] || fail "${component}/${platform} has no unique archive authority"
        [ "$binary_count" = 1 ] || fail "${component}/${platform} has no unique binary authority"
    done
done

MISSING_ARCHIVE_LOCK="${WORK}/missing-archive.lock"
grep -v $'^archive\tnode\tmacos-arm64\tnode-v26.7.0-darwin-arm64.tar.gz\t' \
    "${REPO_ROOT}/offline/toolchain.lock" > "$MISSING_ARCHIVE_LOCK"
TOOLCHAIN_LOCK_FILE="$MISSING_ARCHIVE_LOCK"
expect_reject "missing platform archive authority" \
    toolchain_archive_sha256 node macos-arm64 node-v26.7.0-darwin-arm64.tar.gz

MISSING_BINARY_LOCK="${WORK}/missing-binary.lock"
grep -v $'^binary\tnode\tmacos-arm64\tbin/node\t' \
    "${REPO_ROOT}/offline/toolchain.lock" > "$MISSING_BINARY_LOCK"
TOOLCHAIN_LOCK_FILE="$MISSING_BINARY_LOCK"
expect_reject "missing platform binary authority" \
    toolchain_binary_authority node macos-arm64 bin/node
TOOLCHAIN_LOCK_FILE="${REPO_ROOT}/offline/toolchain.lock"

LOCK_COPY="${WORK}/cache-key.lock"
cp "${REPO_ROOT}/offline/toolchain.lock" "$LOCK_COPY"
TOOLCHAIN_LOCK_FILE="$LOCK_COPY"
key_before="$(toolchain_lock_digest):macos-arm64"
printf '\n# isolated cache-key mutation\n' >> "$LOCK_COPY"
key_after="$(toolchain_lock_digest):macos-arm64"
[ "$key_before" != "$key_after" ] || fail "toolchain cache identity did not change with the lock digest"
TOOLCHAIN_LOCK_FILE="${REPO_ROOT}/offline/toolchain.lock"

DEPS_DIR="${WORK}/deps"
PLATFORM="macos-arm64"
nginx_root="${DEPS_DIR}/nginx"
mkdir -p "${nginx_root}/bin"
printf 'trusted derived nginx binary' > "${nginx_root}/bin/nginx"
chmod +x "${nginx_root}/bin/nginx"
nginx_identity="$(toolchain_build_identity nginx "$PLATFORM" 1.26.2 \
    "$(toolchain_archive_sha256 nginx "$PLATFORM" nginx-1.26.2.tar.gz)" \
    "$(nginx_build_recipe)")"
nginx_closure="$(write_install_manifest nginx "$nginx_root" "$nginx_identity" bin/nginx bin)"
record_authority_receipt nginx "$nginx_identity" "$nginx_closure"
verify_install_manifest nginx "$nginx_root" "$nginx_identity" bin/nginx bin
expect_reject "narrowed nginx execution closure" \
    verify_install_manifest nginx "$nginx_root" "$nginx_identity" bin/nginx bin/nginx
printf 'substituted binary' > "${nginx_root}/bin/nginx"
chmod +x "${nginx_root}/bin/nginx"
# Rewriting the cached observation must not help: the expected closure comes
# from this invocation's out-of-cache receipt, not the adjacent manifest.
write_install_manifest nginx "$nginx_root" "$nginx_identity" bin/nginx bin >/dev/null
expect_reject "substituted installed nginx binary" \
    verify_install_manifest nginx "$nginx_root" "$nginx_identity" bin/nginx bin
python3 - "$TOOLCHAIN_RECEIPT_FILE" <<'PY'
import sys
from pathlib import Path

path = Path(sys.argv[1])
lines = []
for line in path.read_text(encoding="utf-8").splitlines():
    fields = line.split("\t")
    if fields[:2] == ["record", "nginx"]:
        fields[-1] = "0" * 64
        line = "\t".join(fields)
    lines.append(line)
path.write_text("\n".join(lines) + "\n", encoding="utf-8")
PY
expect_reject "rewritten receipt record without its secret authenticator" \
    verify_install_manifest nginx "$nginx_root" "$nginx_identity" bin/nginx bin

node_root="${DEPS_DIR}/node"
mkdir -p "${node_root}/bin" "${node_root}/lib/node_modules/npm/bin" "${node_root}/lib/node_modules/npm/lib" "${node_root}/outside"
printf 'trusted node binary' > "${node_root}/bin/node"
chmod +x "${node_root}/bin/node"
ln -s "../lib/node_modules/npm/bin/npm-cli.js" "${node_root}/bin/npm"
ln -s "../outside/npm-imported.js" "${node_root}/bin/npm-imported"
printf 'require("../lib/cli.js")\n' > "${node_root}/lib/node_modules/npm/bin/npm-cli.js"
printf 'trusted npm imported CLI\n' > "${node_root}/lib/node_modules/npm/lib/cli.js"
printf 'trusted symlinked npm import\n' > "${node_root}/outside/npm-imported.js"
NODE_TEST_LOCK="${WORK}/node-test.lock"
cp "${REPO_ROOT}/offline/toolchain.lock" "$NODE_TEST_LOCK"
node_identity="$(sha256_file "${node_root}/bin/node")"
sed -i.bak "s|^binary\\tnode\\tmacos-arm64\\tbin/node\\texact\\t[0-9a-f]*\\t|binary\\tnode\\tmacos-arm64\\tbin/node\\texact\\t${node_identity}\\t|" "$NODE_TEST_LOCK"
rm -f "${NODE_TEST_LOCK}.bak"
TOOLCHAIN_LOCK_FILE="$NODE_TEST_LOCK"
node_closure="$(write_install_manifest node "$node_root" "$node_identity" bin/node bin lib/node_modules/npm)"
record_authority_receipt node "$node_identity" "$node_closure"
verify_install_manifest node "$node_root" "$node_identity" bin/node bin lib/node_modules/npm
printf 'substituted node binary' > "${node_root}/bin/node"
chmod +x "${node_root}/bin/node"
write_install_manifest node "$node_root" "$node_identity" bin/node bin lib/node_modules/npm >/dev/null
expect_reject "substituted Node binary plus manifest" \
    verify_install_manifest node "$node_root" "$node_identity" bin/node bin lib/node_modules/npm
printf 'trusted node binary' > "${node_root}/bin/node"
chmod +x "${node_root}/bin/node"
write_install_manifest node "$node_root" "$node_identity" bin/node bin lib/node_modules/npm >/dev/null
verify_install_manifest node "$node_root" "$node_identity" bin/node bin lib/node_modules/npm
printf 'substituted npm imported CLI' > "${node_root}/lib/node_modules/npm/lib/cli.js"
write_install_manifest node "$node_root" "$node_identity" bin/node bin lib/node_modules/npm >/dev/null
expect_reject "substituted npm import dependency plus manifest" \
    verify_install_manifest node "$node_root" "$node_identity" bin/node bin lib/node_modules/npm
printf 'trusted npm imported CLI\n' > "${node_root}/lib/node_modules/npm/lib/cli.js"
write_install_manifest node "$node_root" "$node_identity" bin/node bin lib/node_modules/npm >/dev/null
verify_install_manifest node "$node_root" "$node_identity" bin/node bin lib/node_modules/npm
printf 'substituted symlinked npm import' > "${node_root}/outside/npm-imported.js"
write_install_manifest node "$node_root" "$node_identity" bin/node bin lib/node_modules/npm >/dev/null
expect_reject "substituted symlink target outside selected Node subtree" \
    verify_install_manifest node "$node_root" "$node_identity" bin/node bin lib/node_modules/npm
TOOLCHAIN_LOCK_FILE="${REPO_ROOT}/offline/toolchain.lock"

# The locked macOS bindist exposes `bin/ghc` as a symlink to the versioned
# regular executable. Exercise the archive extraction/copy layout the
# installer uses, rather than only an already-assembled synthetic install.
GHC_LAYOUT_SOURCE="${WORK}/ghc-bindist-layout/ghc-9.14.1-aarch64-apple-darwin"
mkdir -p "${GHC_LAYOUT_SOURCE}/bin" "${GHC_LAYOUT_SOURCE}/lib/ghc-9.14.1"
printf 'archive-layout resolved GHC binary' > "${GHC_LAYOUT_SOURCE}/bin/ghc-9.14.1"
chmod +x "${GHC_LAYOUT_SOURCE}/bin/ghc-9.14.1"
ln -s "ghc-9.14.1" "${GHC_LAYOUT_SOURCE}/bin/ghc"
printf 'archive-layout settings' > "${GHC_LAYOUT_SOURCE}/lib/ghc-9.14.1/settings"
GHC_LAYOUT_ARCHIVE="${WORK}/ghc-9.14.1-aarch64-apple-darwin.tar.xz"
tar -cJf "$GHC_LAYOUT_ARCHIVE" -C "${WORK}/ghc-bindist-layout" ghc-9.14.1-aarch64-apple-darwin
GHC_LAYOUT_EXTRACT="${WORK}/ghc-bindist-extract"
extract_txz "$GHC_LAYOUT_ARCHIVE" "$GHC_LAYOUT_EXTRACT" 0 "$(sha256_file "$GHC_LAYOUT_ARCHIVE")"
GHC_LAYOUT_INSTALLED="${WORK}/ghc-bindist-installed"
mkdir -p "${GHC_LAYOUT_INSTALLED}/ghc/9.14.1"
cp -R "${GHC_LAYOUT_EXTRACT}/ghc-9.14.1-aarch64-apple-darwin/"* "${GHC_LAYOUT_INSTALLED}/ghc/9.14.1/"
[ -L "${GHC_LAYOUT_INSTALLED}/ghc/9.14.1/bin/ghc" ] \
    && [ "$(readlink "${GHC_LAYOUT_INSTALLED}/ghc/9.14.1/bin/ghc")" = "ghc-9.14.1" ] \
    && [ -f "${GHC_LAYOUT_INSTALLED}/ghc/9.14.1/bin/ghc-9.14.1" ] \
    || fail "real GHC bindist symlink layout was not preserved after archive extraction"

ghc_root="${DEPS_DIR}/ghcup"
mkdir -p \
    "${ghc_root}/bin" \
    "${ghc_root}/ghc/9.14.1/bin" \
    "${ghc_root}/ghc/9.14.1/lib/ghc-9.14.1/package.conf.d"
printf 'trusted resolved GHC binary' > "${ghc_root}/ghc/9.14.1/bin/ghc-9.14.1"
chmod +x "${ghc_root}/ghc/9.14.1/bin/ghc-9.14.1"
ln -s "ghc-9.14.1" "${ghc_root}/ghc/9.14.1/bin/ghc"
ln -s "../ghc/9.14.1/bin/ghc" "${ghc_root}/bin/ghc"
printf 'trusted GHC settings' > "${ghc_root}/ghc/9.14.1/lib/ghc-9.14.1/settings"
printf 'trusted GHC package database' > "${ghc_root}/ghc/9.14.1/lib/ghc-9.14.1/package.conf.d/base.conf"
printf 'trusted GHC library' > "${ghc_root}/ghc/9.14.1/lib/libHSghc-9.14.1.dylib"
printf 'export PATH=trusted\n' > "${ghc_root}/env"
ghc_identity="$(ghc_build_identity ghc-9.14.1-aarch64-apple-darwin.tar.xz)"
ghc_closure="$(write_install_manifest ghc "$ghc_root" "$ghc_identity" ghc/9.14.1/bin/ghc-9.14.1 ghc/9.14.1 bin env)"
record_authority_receipt ghc "$ghc_identity" "$ghc_closure"
verify_install_manifest ghc "$ghc_root" "$ghc_identity" ghc/9.14.1/bin/ghc-9.14.1 ghc/9.14.1 bin env
verify_internal_symlink "$ghc_root" bin/ghc ../ghc/9.14.1/bin/ghc
verify_internal_symlink "$ghc_root" ghc/9.14.1/bin/ghc ghc-9.14.1
printf 'substituted resolved GHC binary' > "${ghc_root}/ghc/9.14.1/bin/ghc-9.14.1"
chmod +x "${ghc_root}/ghc/9.14.1/bin/ghc-9.14.1"
write_install_manifest ghc "$ghc_root" "$ghc_identity" ghc/9.14.1/bin/ghc-9.14.1 ghc/9.14.1 bin env >/dev/null
expect_reject "substituted GHC symlink target plus manifest" \
    verify_install_manifest ghc "$ghc_root" "$ghc_identity" ghc/9.14.1/bin/ghc-9.14.1 ghc/9.14.1 bin env
printf 'trusted resolved GHC binary' > "${ghc_root}/ghc/9.14.1/bin/ghc-9.14.1"
chmod +x "${ghc_root}/ghc/9.14.1/bin/ghc-9.14.1"
write_install_manifest ghc "$ghc_root" "$ghc_identity" ghc/9.14.1/bin/ghc-9.14.1 ghc/9.14.1 bin env >/dev/null
verify_install_manifest ghc "$ghc_root" "$ghc_identity" ghc/9.14.1/bin/ghc-9.14.1 ghc/9.14.1 bin env

printf 'alternate resolved GHC binary' > "${ghc_root}/ghc/9.14.1/bin/ghc-9.14.1-alt"
chmod +x "${ghc_root}/ghc/9.14.1/bin/ghc-9.14.1-alt"
rm -f "${ghc_root}/bin/ghc"
ln -s "../ghc/9.14.1/bin/ghc-9.14.1-alt" "${ghc_root}/bin/ghc"
write_install_manifest ghc "$ghc_root" "$ghc_identity" ghc/9.14.1/bin/ghc-9.14.1 ghc/9.14.1 bin env >/dev/null
expect_reject "redirected in-root GHC public symlink plus manifest" \
    verify_install_manifest ghc "$ghc_root" "$ghc_identity" ghc/9.14.1/bin/ghc-9.14.1 ghc/9.14.1 bin env
rm -f "${ghc_root}/bin/ghc" "${ghc_root}/ghc/9.14.1/bin/ghc-9.14.1-alt"
ln -s "../ghc/9.14.1/bin/ghc" "${ghc_root}/bin/ghc"

printf 'alternate internal GHC launcher target' > "${ghc_root}/ghc/9.14.1/bin/ghc-9.14.1-alt"
chmod +x "${ghc_root}/ghc/9.14.1/bin/ghc-9.14.1-alt"
rm -f "${ghc_root}/ghc/9.14.1/bin/ghc"
ln -s "ghc-9.14.1-alt" "${ghc_root}/ghc/9.14.1/bin/ghc"
write_install_manifest ghc "$ghc_root" "$ghc_identity" ghc/9.14.1/bin/ghc-9.14.1 ghc/9.14.1 bin env >/dev/null
expect_reject "redirected GHC internal symlink plus manifest" \
    verify_install_manifest ghc "$ghc_root" "$ghc_identity" ghc/9.14.1/bin/ghc-9.14.1 ghc/9.14.1 bin env
rm -f "${ghc_root}/ghc/9.14.1/bin/ghc" "${ghc_root}/ghc/9.14.1/bin/ghc-9.14.1-alt"
ln -s "ghc-9.14.1" "${ghc_root}/ghc/9.14.1/bin/ghc"

rm -f "${ghc_root}/bin/ghc"
ln -s "../../../../../outside" "${ghc_root}/bin/ghc"
(write_install_manifest ghc "$ghc_root" "$ghc_identity" ghc/9.14.1/bin/ghc-9.14.1 ghc/9.14.1 bin env) >/dev/null 2>&1 || true
expect_reject "escaping GHC public symlink" \
    verify_install_manifest ghc "$ghc_root" "$ghc_identity" ghc/9.14.1/bin/ghc-9.14.1 ghc/9.14.1 bin env
rm -f "${ghc_root}/bin/ghc"
ln -s "../ghc/9.14.1/bin/ghc" "${ghc_root}/bin/ghc"

printf 'substituted GHC settings' > "${ghc_root}/ghc/9.14.1/lib/ghc-9.14.1/settings"
write_install_manifest ghc "$ghc_root" "$ghc_identity" ghc/9.14.1/bin/ghc-9.14.1 ghc/9.14.1 bin env >/dev/null
expect_reject "substituted GHC settings plus manifest" \
    verify_install_manifest ghc "$ghc_root" "$ghc_identity" ghc/9.14.1/bin/ghc-9.14.1 ghc/9.14.1 bin env
printf 'trusted GHC settings' > "${ghc_root}/ghc/9.14.1/lib/ghc-9.14.1/settings"

printf 'substituted GHC package database' > "${ghc_root}/ghc/9.14.1/lib/ghc-9.14.1/package.conf.d/base.conf"
write_install_manifest ghc "$ghc_root" "$ghc_identity" ghc/9.14.1/bin/ghc-9.14.1 ghc/9.14.1 bin env >/dev/null
expect_reject "substituted GHC package database plus manifest" \
    verify_install_manifest ghc "$ghc_root" "$ghc_identity" ghc/9.14.1/bin/ghc-9.14.1 ghc/9.14.1 bin env
printf 'trusted GHC package database' > "${ghc_root}/ghc/9.14.1/lib/ghc-9.14.1/package.conf.d/base.conf"

printf 'substituted GHC library' > "${ghc_root}/ghc/9.14.1/lib/libHSghc-9.14.1.dylib"
write_install_manifest ghc "$ghc_root" "$ghc_identity" ghc/9.14.1/bin/ghc-9.14.1 ghc/9.14.1 bin env >/dev/null
expect_reject "substituted GHC library plus manifest" \
    verify_install_manifest ghc "$ghc_root" "$ghc_identity" ghc/9.14.1/bin/ghc-9.14.1 ghc/9.14.1 bin env

grep -Fq "hashFiles('offline/toolchain.lock'" "${REPO_ROOT}/.github/workflows/build-offline.yml" \
    || fail "build-offline workflow cache key does not bind offline/toolchain.lock"
grep -Fq "'offline/scripts/authority-tree.py'" "${REPO_ROOT}/.github/workflows/build-offline.yml" \
    || fail "build-offline workflow cache key does not bind the authority-tree verifier"
grep -Fq 'offline/_deps/.toolchain-authority/' "${REPO_ROOT}/.github/workflows/build-offline.yml" \
    || fail "build-offline workflow does not cache toolchain closure observations with installations"
# shellcheck disable=SC2016
grep -Fq 'verify_install_manifest nginx "${DEPS_DIR}/nginx" "$identity" "bin/nginx" "bin"' \
    "${REPO_ROOT}/offline/scripts/05-package.sh" \
    || fail "packager does not verify the complete nginx execution closure"
if grep -Fq 'offline/_session/' "${REPO_ROOT}/.github/workflows/build-offline.yml"; then
    fail "build-offline workflow caches invocation-specific authority receipts"
fi

runtime_index="$(
    awk -F '\t' '$1 == "image" && $2 == "nginx-runtime" && $3 == "multiarch" && $4 == "nginx:1.27.5" { print $6 }' \
        "${REPO_ROOT}/offline/toolchain.lock"
)"
[ "${#runtime_index}" = 64 ] || fail "nginx runtime index digest is missing from the authority table"
grep -Fq "FROM nginx:1.27.5@sha256:${runtime_index} AS app" "${REPO_ROOT}/Dockerfile" \
    || fail "production Dockerfile does not use the authority-locked nginx runtime index"
for platform in linux-x86_64 linux-arm64; do
    toolchain_lock_record image nginx-runtime "$platform" nginx:1.27.5 >/dev/null \
        || fail "nginx runtime has no ${platform} image-manifest authority"
done

if [ "$failures" -ne 0 ]; then
    printf 'toolchain-authority: %s failure(s)\n' "$failures" >&2
    exit 1
fi

printf '%s\n' 'toolchain-authority: cache corruption, archive substitution, missing authority, binary substitution, and cache-key binding rejected'
