#!/bin/bash
# Sealed stage of the authoritative locale-catalog Python launcher.
#
# This file is deliberately *not* executable and carries no authority of its
# own: `scripts/run-locale-catalog-python.sh` -- a POSIX `sh` stage that no
# `BASH_ENV`/`ENV` startup hook can reach -- is the only sanctioned entry
# point, and it hands control here through `/usr/bin/env -i` with an
# environment it constructed itself. By the time bash reads this file, the
# caller's `HOME`, `PATH`, `PYTHON*`, `BASH_ENV`, `ENV`, `SHELLOPTS`, `CDPATH`,
# `IFS`, `GIT_*` and `UV_*` are already gone.
#
# Authority then comes from exactly two inputs, and nothing else:
#
#   1. `LOCALE_CATALOG_MISE_ROOT` -- an explicit, absolute, already-canonical,
#      non-symlink toolchain root supplied by the trusted host (the CI job's
#      sealed `defaults.run.shell`, or a maintainer who names it deliberately).
#      It is never defaulted, never derived from `$HOME`, and never guessed.
#   2. `scripts/locale_catalog_python_runtime.json` -- the committed toolchain
#      lock this stage attests the interpreter, its stdlib bytes, the external
#      tools *and the repository-side trusted computing base* against.
#
# Threat model (see docs/locale-catalog.md for the full statement):
#
#   T1 (enforced)   Hostile or mistaken *committed* repository source. Every
#                   identity below is checked against the committed lock before
#                   the governed code that depends on it runs.
#   T2 (not claimed) A concurrent same-UID process rewriting these files between
#                   the check and the use. Checking early narrows the window and
#                   catches stable-host tampering; it does not prevent a racing
#                   owner. CI runs each command in an isolated ephemeral job.
#   T3 (out of scope) ptrace/debuggers, the Docker daemon, the Git object
#                   database, and a compromised OS, kernel or runner.
set -euo pipefail

if [[ "${LOCALE_CATALOG_SEALED_SHELL:-}" != "1" ]]; then
  echo "locale-catalog python: this sealed stage runs only from scripts/run-locale-catalog-python.sh" >&2
  exit 1
fi

# This shell's own helper lookups are pinned too; nothing here is resolved
# through an inherited or defaulted PATH.
PATH="/usr/bin:/bin"
export PATH
readonly MKTEMP="/usr/bin/mktemp"
readonly RM="/bin/rm"
readonly DATE="/bin/date"
readonly MKDIR="/bin/mkdir"
readonly CP="/bin/cp"
readonly LN="/bin/ln"
readonly UNAME="/usr/bin/uname"
readonly FIND="/usr/bin/find"
readonly SORT="/usr/bin/sort"
readonly XARGS="/usr/bin/xargs"
readonly STAT="/usr/bin/stat"
readonly GREP="/usr/bin/grep"
readonly SED="/usr/bin/sed"
export LC_ALL=C
if [[ -x /usr/bin/sha256sum && ! -L /usr/bin/sha256sum ]]; then
  readonly SHA256="/usr/bin/sha256sum"
elif [[ -x /usr/bin/shasum && ! -L /usr/bin/shasum ]]; then
  readonly SHA256="/usr/bin/shasum"
else
  echo "locale-catalog python: no trusted system SHA-256 executable is available" >&2
  exit 1
fi

die() {
  echo "locale-catalog python: $1" >&2
  exit 1
}

ROOT="$(cd -- "${BASH_SOURCE[0]%/*}/.." && pwd -P)"
readonly ROOT
readonly PROFILE="${ROOT}/scripts/locale_catalog_python_runtime.json"

sha256_file() {
  local path="$1" output
  output="$("${SHA256}" -a 256 "${path}" 2>/dev/null)" ||
    output="$("${SHA256}" "${path}")" ||
    die "could not hash ${path} with the trusted system SHA-256 executable"
  printf '%s\n' "${output%% *}"
}

# One committed digest, read out of the toolchain lock by exact key. The lock
# is plain committed JSON with one digest per line, so this needs no parser --
# and it refuses anything but exactly one 64-hex match for the key.
lock_digest() {
  local key="$1" matches count
  matches="$("${GREP}" -oE "\"${key}\"[[:space:]]*:[[:space:]]*\"[0-9a-f]{64}\"" "${PROFILE}")" ||
    die "the committed toolchain lock declares no digest for '${key}'"
  count="$(printf '%s\n' "${matches}" | "${GREP}" -c .)"
  [[ "${count}" == "1" ]] ||
    die "the committed toolchain lock declares ${count} digests for '${key}', expected exactly one"
  printf '%s\n' "${matches}" | "${SED}" -E 's/.*"([0-9a-f]{64})"$/\1/'
}

# The repository-side trusted computing base: the capability analyzer, the
# bootstrap that runs it, and both launcher shell stages. The analyzer decides
# whether every other governed source may run, so it cannot be allowed to vouch
# for itself *after* executing -- its bytes are authenticated here, before this
# stage starts any interpreter. (T1. A same-UID process that rewrites one of
# these afterwards is T2 and is not claimed.)
verify_trusted_source() {
  local relative="$1" path="${ROOT}/$1" expected actual
  [[ ! -L "${path}" && -f "${path}" ]] ||
    die "trusted source '${relative}' is not a regular file"
  expected="$(lock_digest "${relative}")"
  actual="$(sha256_file "${path}")"
  [[ "${actual}" == "${expected}" ]] ||
    die "trusted source '${relative}' does not match the identity committed in ${PROFILE#"${ROOT}/"}"
}

[[ ! -L "${PROFILE}" && -f "${PROFILE}" ]] ||
  die "the committed toolchain lock '${PROFILE}' is not a regular file"
verify_trusted_source "scripts/run-locale-catalog-python.sh"
verify_trusted_source "scripts/locale-catalog-python-sealed.sh"
verify_trusted_source "scripts/locale_catalog_python_boundary.py"
verify_trusted_source "scripts/locale_catalog_runtime.py"
readonly SEALED_ROOT="${LOCALE_CATALOG_MISE_ROOT:?locale-catalog python: LOCALE_CATALOG_MISE_ROOT is required for authoritative commands; the *-local convenience tasks are not authoritative}"

# The sealed toolchain root must be named exactly, absolutely, and canonically.
# A relative path, an unnormalized path, a path that is (or traverses) a
# symlink, a missing path, and the filesystem root are all refused -- and so is
# any attempt to make the root follow the caller's home directory, which this
# shell no longer knows because `HOME` was discarded above.
[[ "${SEALED_ROOT}" == /* ]] || die "LOCALE_CATALOG_MISE_ROOT must be an absolute path, got '${SEALED_ROOT}'"
case "${SEALED_ROOT}" in
  */. | */.. | */./* | */../* | *//*)
    die "LOCALE_CATALOG_MISE_ROOT must be a normalized path, got '${SEALED_ROOT}'"
    ;;
esac
[[ ! -L "${SEALED_ROOT}" ]] || die "LOCALE_CATALOG_MISE_ROOT must not be a symlink"
[[ -d "${SEALED_ROOT}" ]] || die "LOCALE_CATALOG_MISE_ROOT '${SEALED_ROOT}' is not an existing directory"
[[ "${SEALED_ROOT}" != "/" ]] || die "LOCALE_CATALOG_MISE_ROOT must not be the filesystem root"
sealed_canonical="$(cd -- "${SEALED_ROOT}" && pwd -P)"
[[ "${sealed_canonical}" == "${SEALED_ROOT}" ]] ||
  die "LOCALE_CATALOG_MISE_ROOT must already be canonical; '${SEALED_ROOT}' resolves to '${sealed_canonical}'"

require_sealed_file() {
  local path="$1" what="$2"
  [[ ! -L "${path}" ]] || die "${what} '${path}' is a symlink; the sealed toolchain must be named directly"
  [[ -f "${path}" ]] || die "${what} '${path}' is not a regular file"
  [[ -x "${path}" ]] || die "${what} '${path}' is not executable"
  local canonical_parent
  canonical_parent="$(cd -- "${path%/*}" && pwd -P)" ||
    die "${what} '${path}' does not have a readable parent directory"
  [[ "${canonical_parent}/${path##*/}" == "${path}" ]] ||
    die "${what} '${path}' traverses a symlinked toolchain directory"
}

require_digest() {
  local path="$1" what="$2" actual expected
  shift 2
  actual="$(sha256_file "${path}")"
  for expected in "$@"; do
    [[ "${actual}" == "${expected}" ]] && return
  done
  die "${what} '${path}' does not match a declared SHA-256 identity for this platform"
}

hash_stream() {
  if [[ "${SHA256}" == */shasum ]]; then
    "${SHA256}" -a 256
  else
    "${SHA256}"
  fi
}

hash_files() {
  if [[ "${SHA256}" == */shasum ]]; then
    "${XARGS}" -0 "${SHA256}" -a 256
  else
    "${XARGS}" -0 "${SHA256}"
  fi
}

# The exact interpreter binary -- not the `bin/python` / `bin/python3` symlinks
# a local `pip install` can retarget, and never a PATH lookup.
readonly PYTHON="${SEALED_ROOT}/installs/python/3.14.7/bin/python3.14"
require_sealed_file "${PYTHON}" "sealed CPython 3.14.7"

readonly NODE="${SEALED_ROOT}/installs/node/26.7.0/bin/node"
require_sealed_file "${NODE}" "sealed Node 26.7.0"

shopt -s nullglob
uv_candidates=("${SEALED_ROOT}"/installs/uv/0.12.6/uv-*/uv)
shopt -u nullglob
[[ "${#uv_candidates[@]}" -eq 1 ]] ||
  die "expected exactly one sealed uv 0.12.6 binary under '${SEALED_ROOT}/installs/uv/0.12.6', found ${#uv_candidates[@]}"
readonly UV="${uv_candidates[0]}"
require_sealed_file "${UV}" "sealed uv 0.12.6"
readonly STDLIB="${SEALED_ROOT}/installs/python/3.14.7/lib/python3.14"
[[ ! -L "${STDLIB}" && -d "${STDLIB}" ]] ||
  die "sealed CPython stdlib '${STDLIB}' is not a regular directory"
stdlib_canonical="$(cd -- "${STDLIB}" && pwd -P)"
[[ "${stdlib_canonical}" == "${STDLIB}" ]] ||
  die "sealed CPython stdlib '${STDLIB}' traverses a symlinked toolchain directory"

# Revision drift is the only governed step that may consult git, and it
# consults exactly this absolute, non-symlink system binary -- never a
# PATH-selected `git`, and never a `git` shadowed inside the toolchain root.
readonly GIT="/usr/bin/git"
require_sealed_file "${GIT}" "trusted git"

# The production backend probe is the only target that needs a non-mise
# executable.  Its host supplies this one authority explicitly (the Haskell CI
# step binds the setup action's stack path); it is never found through PATH.
readonly STACK="${LOCALE_CATALOG_STACK:-}"
if [[ -n "${STACK}" ]]; then
  [[ "${STACK}" == /* ]] || die "LOCALE_CATALOG_STACK must be an absolute path"
  require_sealed_file "${STACK}" "explicitly bound stack"
fi

# A path under the explicit toolchain root is not enough: an attacker who can
# replace a binary there could retain the expected version/path.  Bind the
# executable bytes before the first one runs.  The two Linux CPython hashes
# correspond to the two pinned standalone builds in the committed profile;
# every other platform is rejected rather than approximated.
case "$("${UNAME}" -s):$("${UNAME}" -m)" in
  Darwin:arm64)
    readonly PYTHON_DIGESTS=("1ba16b38d45f006e449bb51a923dae83f3c384611bcd4ee428afd044b7ed4c95")
    require_digest "${PYTHON}" "sealed CPython 3.14.7" "${PYTHON_DIGESTS[@]}"
    require_digest "${NODE}" "sealed Node 26.7.0" \
      "a9bd0630891c2dcdee70de88270fee2cc0c4a9e76495039dd3b4f91c5e6b71df"
    require_digest "${UV}" "sealed uv 0.12.6" \
      "e8929237934c8679686428f5a7736c7ae7a5fe7a33b0504d1b03446cdbc43c94"
    readonly SYSCONFIG_SOURCE="_sysconfigdata__darwin_darwin.py"
    require_digest "${STDLIB}/${SYSCONFIG_SOURCE}" "active CPython sysconfig source" \
      "3f4f3d7287fe28096c5b80f9b92fe561b69b5f50a16e4bf075165c96d7892981"
    ;;
  Linux:x86_64)
    readonly PYTHON_DIGESTS=(
      "23cfacd2e3ce3d8745b9405641ca3d91e9803e49003faa7882f80a4da9414be7" \
      "ce7402fee6629ce791aeb871cd4d1a1e21ad2e90ca4b3236611484053a7e06ac"
    )
    require_digest "${PYTHON}" "sealed CPython 3.14.7" "${PYTHON_DIGESTS[@]}"
    require_digest "${NODE}" "sealed Node 26.7.0" \
      "ad19784f7e90ba789a099eccba77ede8dc90a778c424f1c10a70fed3ff903fdc"
    require_digest "${UV}" "sealed uv 0.12.6" \
      "d381f11517c66523211b0876552ff7dea5c1b4b0f13800571b35225761302fba"
    readonly SYSCONFIG_SOURCE="_sysconfigdata__linux_x86_64-linux-gnu.py"
    require_digest "${STDLIB}/${SYSCONFIG_SOURCE}" "active CPython sysconfig source" \
      "90ce56ecd6e00b572c035dafaab3a66a756e2c488cbd86b919dfee41fd364bf4"
    ;;
  *)
    die "unsupported toolchain platform $(${UNAME} -s):$(${UNAME} -m); no portable exact binary identity is declared"
    ;;
esac

verify_stdlib_before_python() {
  # `-B` stops writes but CPython can otherwise still *read* an attacker-made
  # cache.  Every Python process below instead uses a fresh owned
  # `pycache_prefix`, so the install's pre-existing cache is unreachable.  The
  # source tree itself is attested here, before this interpreter can import
  # even the bootstrap's first stdlib module.
  local unexpected source_digest
  unexpected="$("${FIND}" "${STDLIB}" -type l -print -quit)"
  [[ -z "${unexpected}" ]] ||
    die "sealed CPython stdlib contains a symlink: ${unexpected}"
  source_digest="$(
    cd -- "${STDLIB}"
    "${FIND}" . -type f -name '*.py' \
      ! -path './site-packages/*' \
      ! -path './_sysconfigdata__darwin_darwin.py' \
      ! -path './_sysconfigdata__linux_x86_64-linux-gnu.py' \
      ! -path './config-3.14-darwin/python-config.py' \
      ! -path './config-3.14-x86_64-linux-gnu/python-config.py' \
      -print0 | "${SORT}" -z | hash_files | hash_stream
  )"
  source_digest="${source_digest%% *}"
  [[ "${source_digest}" == "c618cf3f74e4625201ed9d508f280b256235370e949172500c02d2da662d53e5" ]] ||
    die "sealed CPython stdlib source set does not match the declared complete closure"
}

verify_stdlib_before_python

# `PATH` for the child exists only for the non-Python helpers the governed
# scripts shell out to (node/npm, nginx, stack). It deliberately excludes the
# interpreter's own `bin` directory, which a local `pip install` can fill with
# arbitrary executables, and the uv directory, which is named absolutely above.
readonly TRUSTED_PATH="${SEALED_ROOT}/installs/node/26.7.0/bin:/usr/local/.ghcup/bin:/opt/homebrew/bin:/usr/local/bin:/usr/bin:/bin"

cd "${ROOT}"

readonly WORKSPACE_PREFIX=".locale-catalog-python."
readonly OWNER_FILE="owner"
# `mktemp -d` creates the workspace exclusively (O_EXCL, mode 0700), so the
# ownership record below can never be planted by another invocation.
WORKSPACE="$("${MKTEMP}" -d "${ROOT}/${WORKSPACE_PREFIX}XXXXXX")"
readonly WORKSPACE
workspace_identity() {
  if [[ "$("${UNAME}" -s)" == "Darwin" ]]; then
    "${STAT}" -f '%d:%i' -- "${WORKSPACE}"
  else
    "${STAT}" -c '%d:%i' -- "${WORKSPACE}"
  fi
}
readonly WORKSPACE_ID="$(workspace_identity)"
readonly WORKSPACE_TOKEN="$$.${RANDOM}.${RANDOM}.$("${DATE}" +%s)"
printf '%s %s %s %s\n' "$$" "$("${DATE}" +%s)" "${WORKSPACE_TOKEN}" "${WORKSPACE_ID}" >"${WORKSPACE}/${OWNER_FILE}"

cleanup_workspace() {
  local owner_pid owner_epoch owner_token owner_identity extra
  [[ -d "${WORKSPACE}" && ! -L "${WORKSPACE}" ]] || return
  [[ -f "${WORKSPACE}/${OWNER_FILE}" && ! -L "${WORKSPACE}/${OWNER_FILE}" ]] || return
  owner_pid=""
  owner_epoch=""
  owner_token=""
  owner_identity=""
  extra=""
  read -r owner_pid owner_epoch owner_token owner_identity extra <"${WORKSPACE}/${OWNER_FILE}" || return
  [[ -z "${extra}" && "${owner_pid}" == "$$" && "${owner_token}" == "${WORKSPACE_TOKEN}" \
    && "${owner_identity}" == "${WORKSPACE_ID}" ]] || return
  [[ "$(workspace_identity)" == "${WORKSPACE_ID}" ]] || return
  "${RM}" -rf -- "${WORKSPACE}" || true
}
trap cleanup_workspace EXIT
readonly VENV="${WORKSPACE}/venv"
readonly SCRATCH_HOME="${WORKSPACE}/home"
readonly PYCACHE_PREFIX="${WORKSPACE}/pycache"
"${MKDIR}" -p "${SCRATCH_HOME}" "${PYCACHE_PREFIX}"
readonly SITE_PACKAGES="${VENV}/lib/python3.14/site-packages"
readonly RUNTIME_HOME="${WORKSPACE}/runtime"
readonly SOURCE_MANIFEST="${WORKSPACE}/source-manifest"
readonly LOCK_PROJECT="${WORKSPACE}/project"
readonly SOURCE_REPOSITORY="${WORKSPACE}/repository"

# Python still accepts valid unchecked .pyc files even with -B and
# pycache_prefix.  Build an invocation-owned reflink/copy of the already
# shell-attested installation, then remove every cache before its first
# interpreter start. The original managed install is read only; the runtime
# itself attests the copied sources again before importing a governed target.
case "$("${UNAME}" -s)" in
  Darwin) "${CP}" -cR "${SEALED_ROOT}/installs/python/3.14.7" "${RUNTIME_HOME}" ;;
  Linux) "${CP}" --reflink=auto -a "${SEALED_ROOT}/installs/python/3.14.7" "${RUNTIME_HOME}" ;;
  *) die "unsupported runtime-copy platform $(${UNAME} -s)" ;;
esac
# uv starts the copied interpreter with site enabled while it discovers the
# requested base. No copied base site-packages, .pth file, or sitecustomize may
# exist at that point; dependencies are installed only into VENV afterward.
"${RM}" -rf -- "${RUNTIME_HOME}/lib/python3.14/site-packages"
[[ ! -e "${RUNTIME_HOME}/lib/python3.14/site-packages" ]] ||
  die "copied CPython base still contains site-packages before uv starts it"
purge_runtime_bytecode() {
  "${FIND}" "${RUNTIME_HOME}" -type d -name __pycache__ -prune -exec "${RM}" -rf -- {} +
  local unexpected_cache
  unexpected_cache="$("${FIND}" "${RUNTIME_HOME}" \( -name __pycache__ -o -name '*.pyc' \) -print -quit)"
  [[ -z "${unexpected_cache}" ]] ||
    die "copied CPython runtime contains bytecode after cache removal: ${unexpected_cache}"
}
purge_runtime_bytecode
readonly RUNTIME_PYTHON="${RUNTIME_HOME}/bin/python3.14"
[[ ! -L "${RUNTIME_PYTHON}" && -f "${RUNTIME_PYTHON}" && -x "${RUNTIME_PYTHON}" ]] ||
  die "copied CPython runtime has no regular python3.14 executable"
require_digest "${RUNTIME_PYTHON}" "copied CPython 3.14.7" \
  "${PYTHON_DIGESTS[@]}"

# uv resolves, downloads, unpacks and *can build* distributions, and a PEP 517
# backend is arbitrary code that would run before anything else got a say. So
# the project and lock are attested first by a throwaway interpreter that
# imports only the standard library, executes no project code, creates no
# environment and builds nothing. It writes the exact bytes it validated into
# this invocation's own project directory, and uv is then pointed at *that*
# directory -- so what uv consumes is what was checked, not a second read of a
# file that could differ (T1). On a stable host those are the same bytes; a
# racing same-UID owner is T2 and is not claimed.
"${MKDIR}" "${LOCK_PROJECT}"
/usr/bin/env -i \
  HOME="${SCRATCH_HOME}" \
  PATH="${TRUSTED_PATH}" \
  "${RUNTIME_PYTHON}" -I -S -E -B -X "pycache_prefix=${PYCACHE_PREFIX}" \
  "${ROOT}/scripts/locale_catalog_runtime.py" --attest-dependency-sources "${LOCK_PROJECT}"

# `HOME` is this invocation's own empty directory: uv, git, and any helper that
# consults it can never read or write the caller's real home.  Building,
# redirected sources, local sources and project installation are all disabled,
# so even a lock that somehow passed the attestor above could not run code.
/usr/bin/env -i \
  HOME="${SCRATCH_HOME}" \
  PATH="${TRUSTED_PATH}" \
  UV_PROJECT_ENVIRONMENT="${VENV}" \
  UV_NO_CONFIG=1 \
  "${UV}" sync --locked --no-cache --link-mode copy --reinstall --no-dev \
  --no-build --no-sources --no-install-project --no-install-local \
  --project "${LOCK_PROJECT}" --python "${RUNTIME_PYTHON}" --quiet
purge_runtime_bytecode

# Not `exec`: the EXIT trap above must still reclaim this invocation's
# workspace once the governed command finishes.
status=0
/usr/bin/env -i \
  HOME="${SCRATCH_HOME}" \
  PATH="${TRUSTED_PATH}" \
  LOCALE_CATALOG_MISE_ROOT="${SEALED_ROOT}" \
  LOCALE_CATALOG_GIT="${GIT}" \
  LOCALE_CATALOG_NODE="${NODE}" \
  LOCALE_CATALOG_UV="${UV}" \
  LOCALE_CATALOG_STACK="${STACK}" \
  ARKHAM_LOCALE_CATALOG_REPOSITORY_ROOT="${ROOT}" \
  ARKHAM_LOCALE_CATALOG_PYTHON_VENV="${VENV}" \
  ARKHAM_LOCALE_CATALOG_PYCACHE_PREFIX="${PYCACHE_PREFIX}" \
  ARKHAM_LOCALE_CATALOG_RUNTIME_HOME="${RUNTIME_HOME}" \
  ARKHAM_LOCALE_CATALOG_LOCK_PROJECT="${LOCK_PROJECT}" \
  ARKHAM_LOCALE_CATALOG_SOURCE_MANIFEST="${SOURCE_MANIFEST}" \
  ARKHAM_LOCALE_CATALOG_SOURCE_MANIFEST_WRITE=1 \
  "${RUNTIME_PYTHON}" -I -S -E -B -X "pycache_prefix=${PYCACHE_PREFIX}" "${ROOT}/scripts/locale_catalog_runtime.py" "$@" || status=$?
if (( status != 0 )); then
  exit "${status}"
fi

# Snapshot every checked Python source into the private workspace. The first
# bootstrap wrote source-manifest after scanning it; compare the snapshot bytes
# before the second interpreter can execute even its bootstrap. Repository data
# stays read-only through named links, while no Python module is reopened from
# the mutable checkout.
"${MKDIR}" "${SOURCE_REPOSITORY}"
"${CP}" -R "${ROOT}/scripts" "${SOURCE_REPOSITORY}/scripts"
for input in .git .github Dockerfile backend contracts frontend mise.toml offline pyproject.toml uv.lock; do
  [[ -e "${ROOT}/${input}" || -L "${ROOT}/${input}" ]] || continue
  "${LN}" -s "${ROOT}/${input}" "${SOURCE_REPOSITORY}/${input}"
done
if [[ -f "${ROOT}/.locale-catalog-boundary-owner" && ! -L "${ROOT}/.locale-catalog-boundary-owner" ]]; then
  "${CP}" "${ROOT}/.locale-catalog-boundary-owner" "${SOURCE_REPOSITORY}/.locale-catalog-boundary-owner"
fi
snapshot_runtime_seen=0
while IFS=' ' read -r digest relative extra; do
  [[ -n "${digest}" && -n "${relative}" && -z "${extra}" && "${relative}" == scripts/*.py ]] ||
    die "source manifest is malformed"
  snapshot_path="${SOURCE_REPOSITORY}/${relative}"
  [[ -f "${snapshot_path}" && ! -L "${snapshot_path}" ]] ||
    die "source snapshot is missing ${relative}"
  [[ "$(sha256_file "${snapshot_path}")" == "${digest}" ]] ||
    die "source snapshot differs from the checked ${relative}"
  [[ "${relative}" == "scripts/locale_catalog_runtime.py" ]] && snapshot_runtime_seen=1
done <"${SOURCE_MANIFEST}"
[[ "${snapshot_runtime_seen}" == 1 ]] || die "source manifest omits the runtime bootstrap"

# Run only the checked private snapshot in a second clean interpreter. The
# short runner receives paths as argv rather than source text, and imports no
# ambient package.
cd "${SOURCE_REPOSITORY}"
/usr/bin/env -i \
  HOME="${SCRATCH_HOME}" \
  PATH="${TRUSTED_PATH}" \
  LOCALE_CATALOG_MISE_ROOT="${SEALED_ROOT}" \
  LOCALE_CATALOG_GIT="${GIT}" \
  LOCALE_CATALOG_NODE="${NODE}" \
  LOCALE_CATALOG_UV="${UV}" \
  LOCALE_CATALOG_STACK="${STACK}" \
  ARKHAM_LOCALE_CATALOG_REPOSITORY_ROOT="${ROOT}" \
  ARKHAM_LOCALE_CATALOG_PYTHON_VENV="${VENV}" \
  ARKHAM_LOCALE_CATALOG_PYCACHE_PREFIX="${PYCACHE_PREFIX}" \
  ARKHAM_LOCALE_CATALOG_RUNTIME_HOME="${RUNTIME_HOME}" \
  ARKHAM_LOCALE_CATALOG_LOCK_PROJECT="${LOCK_PROJECT}" \
  ARKHAM_LOCALE_CATALOG_SOURCE_MANIFEST="${SOURCE_MANIFEST}" \
  "${RUNTIME_PYTHON}" -I -S -E -B -X "pycache_prefix=${PYCACHE_PREFIX}" -c '
import runpy
import sys

scripts, site_packages, target, *arguments = sys.argv[1:]
sys.path[:0] = [scripts, site_packages]
sys.argv = [target, *arguments]
runpy.run_path(target, run_name="__main__")
' "${SOURCE_REPOSITORY}/scripts" "${SITE_PACKAGES}" "${SOURCE_REPOSITORY}/$1" "${@:2}" || status=$?
exit "${status}"
