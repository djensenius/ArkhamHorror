#!/usr/bin/env bash
# Exercises the same lock lookup and verified-download primitive Dockerfile uses.

set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
REPO_ROOT="$(cd "${SCRIPT_DIR}/../.." && pwd)"
WORK="${REPO_ROOT}/offline/_tmp/test-docker-toolchain-authority-$$-${RANDOM}"
umask 077
mkdir -p "$WORK/bin"
trap 'rm -rf "$WORK"' EXIT

# shellcheck disable=SC1091
. "${SCRIPT_DIR}/docker-toolchain.sh"

failures=0
fail() {
  printf 'docker-toolchain-authority: %s\n' "$*" >&2
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
#!/bin/sh
set -eu
out=
while [ "$#" -gt 0 ]; do
  case "$1" in
    -o) out=$2; shift 2 ;;
    *) shift ;;
  esac
done
[ -n "$out" ]
printf '%s' "${DOCKER_CURL_PAYLOAD:?}" > "$out"
printf 'fetch\n' >> "${DOCKER_CURL_CALLS:?}"
CURL
chmod +x "${WORK}/bin/curl"
export PATH="${WORK}/bin:${PATH}"
export DOCKER_CURL_CALLS="${WORK}/curl-calls"
export DOCKER_CURL_PAYLOAD='trusted Docker archive'

printf '%s' "$DOCKER_CURL_PAYLOAD" > "${WORK}/trusted.bin"
good_sha="$(docker_sha256_file "${WORK}/trusted.bin")"
LOCK="${WORK}/toolchain.lock"
printf 'archive\tdocker-test\tlinux-x86_64\ttest.tar\texact\t%s\t1\nbinary\tdocker-test\tlinux-x86_64\tbin/test\texact\t%s\t1\n' \
  "$good_sha" "$good_sha" > "$LOCK"

fetch_locked_archive "$LOCK" docker-test linux-x86_64 test.tar https://authority.invalid/test.tar "${WORK}/test.tar" \
  || fail "verified Docker archive download failed"
[ "$(cat "${WORK}/test.tar")" = "$DOCKER_CURL_PAYLOAD" ] || fail "verified Docker archive bytes changed"
verify_locked_binary "$LOCK" docker-test linux-x86_64 bin/test "${WORK}/test.tar" \
  || fail "exact Docker binary authority did not verify"

sed -i.bak "s/${good_sha}/$(printf '%064d' 0)/" "$LOCK"
rm -f "${LOCK}.bak"
expect_reject "Docker metadata digest drift" \
  fetch_locked_archive "$LOCK" docker-test linux-x86_64 test.tar https://authority.invalid/test.tar "${WORK}/drift.tar"
[ ! -e "${WORK}/drift.tar" ] || fail "metadata-drift archive was retained"

expect_reject "missing Docker Cabal authority" \
  docker_locked_archive_sha256 "${REPO_ROOT}/offline/toolchain.lock" cabal macos-arm64 missing.tar
for platform in linux-x86_64 linux-arm64; do
  for component in ghc stack cabal; do
    case "$component/$platform" in
      ghc/linux-x86_64) archive="ghc-9.14.1-x86_64-ubuntu20_04-linux.tar.xz" ;;
      ghc/linux-arm64) archive="ghc-9.14.1-aarch64-deb10-linux.tar.xz" ;;
      stack/linux-x86_64) archive="stack-3.7.1-linux-x86_64.tar.gz" ;;
      stack/linux-arm64) archive="stack-3.7.1-linux-aarch64.tar.gz" ;;
      cabal/linux-x86_64) archive="cabal-install-3.16.0.0-x86_64-linux-ubuntu22_04.tar.xz" ;;
      cabal/linux-arm64) archive="cabal-install-3.16.0.0-aarch64-linux-deb10.tar.xz" ;;
    esac
    docker_locked_archive_sha256 "${REPO_ROOT}/offline/toolchain.lock" "$component" "$platform" "$archive" >/dev/null \
      || fail "missing Docker ${component}/${platform} archive authority"
  done
  docker_locked_binary_sha256 "${REPO_ROOT}/offline/toolchain.lock" docker-ghc "$platform" bin/ghc >/dev/null \
    || fail "missing Docker GHC executable authority for ${platform}"
  docker_locked_binary_sha256 "${REPO_ROOT}/offline/toolchain.lock" docker-stack "$platform" bin/stack >/dev/null \
    || fail "missing Docker Stack executable authority for ${platform}"
  docker_locked_binary_sha256 "${REPO_ROOT}/offline/toolchain.lock" docker-cabal "$platform" bin/cabal >/dev/null \
    || fail "missing Docker Cabal executable authority for ${platform}"
done

if grep -Ev '^[[:space:]]*#' "${REPO_ROOT}/Dockerfile" | grep -Eq '(^|[;&[:space:]])ghcup([[:space:];]|$)'; then
  fail "Dockerfile still delegates GHC/Cabal/Stack installation to ghcup"
fi
for needle in 'fetch_locked_archive' ' ghc ' ' stack ' ' cabal ' 'verify_locked_binary'; do
  grep -Fq "$needle" "${REPO_ROOT}/Dockerfile" \
    || fail "Dockerfile does not use locked Docker toolchain primitive: ${needle}"
done

if [ "$failures" -ne 0 ]; then
  printf 'docker-toolchain-authority: %s failure(s)\n' "$failures" >&2
  exit 1
fi
printf '%s\n' 'docker-toolchain-authority: archive corruption, metadata drift, and missing Docker toolchain authority rejected'
