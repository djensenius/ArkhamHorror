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
#      lock the runtime attests the interpreter and its stdlib bytes against.
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
readonly UNAME="/usr/bin/uname"
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

sha256_file() {
  local path="$1" output
  output="$("${SHA256}" -a 256 "${path}" 2>/dev/null)" ||
    output="$("${SHA256}" "${path}")" ||
    die "could not hash ${path} with the trusted system SHA-256 executable"
  printf '%s\n' "${output%% *}"
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
    require_digest "${PYTHON}" "sealed CPython 3.14.7" \
      "1ba16b38d45f006e449bb51a923dae83f3c384611bcd4ee428afd044b7ed4c95"
    require_digest "${NODE}" "sealed Node 26.7.0" \
      "a9bd0630891c2dcdee70de88270fee2cc0c4a9e76495039dd3b4f91c5e6b71df"
    require_digest "${UV}" "sealed uv 0.12.6" \
      "e8929237934c8679686428f5a7736c7ae7a5fe7a33b0504d1b03446cdbc43c94"
    ;;
  Linux:x86_64)
    require_digest "${PYTHON}" "sealed CPython 3.14.7" \
      "23cfacd2e3ce3d8745b9405641ca3d91e9803e49003faa7882f80a4da9414be7" \
      "ce7402fee6629ce791aeb871cd4d1a1e21ad2e90ca4b3236611484053a7e06ac"
    require_digest "${NODE}" "sealed Node 26.7.0" \
      "ad19784f7e90ba789a099eccba77ede8dc90a778c424f1c10a70fed3ff903fdc"
    require_digest "${UV}" "sealed uv 0.12.6" \
      "d381f11517c66523211b0876552ff7dea5c1b4b0f13800571b35225761302fba"
    ;;
  *)
    die "unsupported toolchain platform $(${UNAME} -s):$(${UNAME} -m); no portable exact binary identity is declared"
    ;;
esac

# `PATH` for the child exists only for the non-Python helpers the governed
# scripts shell out to (node/npm, nginx, stack). It deliberately excludes the
# interpreter's own `bin` directory, which a local `pip install` can fill with
# arbitrary executables, and the uv directory, which is named absolutely above.
readonly TRUSTED_PATH="${SEALED_ROOT}/installs/node/26.7.0/bin:/usr/local/.ghcup/bin:/opt/homebrew/bin:/usr/local/bin:/usr/bin:/bin"

cd "${ROOT}"

readonly WORKSPACE_PREFIX=".locale-catalog-python."
readonly OWNER_FILE="owner"
# Bounded staleness window for an abandoned workspace: long enough that a live
# invocation is never reclaimed, short enough that a killed one is not left
# behind forever.
readonly STALE_SECONDS=3600

# Ownership-safe reclamation: a workspace is removed only when its owner file
# is a readable regular file, records a bounded age older than the window, and
# names a process that is no longer alive. Anything unreadable, recent, still
# running, or not shaped like an owner record is left untouched.
reclaim_stale_workspaces() {
  local candidate owner_path owner_pid owner_epoch now
  now="$("${DATE}" +%s)"
  shopt -s nullglob
  for candidate in "${ROOT}/${WORKSPACE_PREFIX}"*; do
    [[ -d "${candidate}" && ! -L "${candidate}" ]] || continue
    owner_path="${candidate}/${OWNER_FILE}"
    [[ -f "${owner_path}" && ! -L "${owner_path}" ]] || continue
    owner_pid=""
    owner_epoch=""
    read -r owner_pid owner_epoch <"${owner_path}" || continue
    [[ "${owner_pid}" =~ ^[0-9]+$ && "${owner_epoch}" =~ ^[0-9]+$ ]] || continue
    (( now - owner_epoch > STALE_SECONDS )) || continue
    kill -0 "${owner_pid}" 2>/dev/null && continue
    # A workspace that cannot be removed (for example one another user owns)
    # is left where it is; reclamation is best-effort and must never abort the
    # governed command it is running on behalf of.
    "${RM}" -rf -- "${candidate}" || true
  done
  shopt -u nullglob
}

reclaim_stale_workspaces

# `mktemp -d` creates the workspace exclusively (O_EXCL, mode 0700), so the
# ownership record below can never be planted by another invocation.
WORKSPACE="$("${MKTEMP}" -d "${ROOT}/${WORKSPACE_PREFIX}XXXXXX")"
readonly WORKSPACE
trap '"${RM}" -rf -- "${WORKSPACE}"' EXIT
printf '%s %s\n' "$$" "$("${DATE}" +%s)" >"${WORKSPACE}/${OWNER_FILE}"
readonly VENV="${WORKSPACE}/venv"
readonly SCRATCH_HOME="${WORKSPACE}/home"
"${MKDIR}" -p "${SCRATCH_HOME}"
readonly SITE_PACKAGES="${VENV}/lib/python3.14/site-packages"

# `HOME` is this invocation's own empty directory: uv, git, and any helper that
# consults it can never read or write the caller's real home.
/usr/bin/env -i \
  HOME="${SCRATCH_HOME}" \
  PATH="${TRUSTED_PATH}" \
  UV_PROJECT_ENVIRONMENT="${VENV}" \
  UV_NO_CONFIG=1 \
  "${UV}" sync --locked --no-cache --link-mode copy --reinstall --no-dev --no-install-project --python "${PYTHON}" --quiet

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
  ARKHAM_LOCALE_CATALOG_PYTHON_VENV="${VENV}" \
  "${PYTHON}" -I -S -E -B "${ROOT}/scripts/locale_catalog_runtime.py" "$@" || status=$?
if (( status != 0 )); then
  exit "${status}"
fi

# Run only the just-validated fixed allowlisted target in a *second* clean
# interpreter.  The short runner is sealed in this script (which fixture
# provenance hashes), receives paths as argv rather than source text, and
# imports no ambient package.  Its one dynamic action is deliberately outside
# governed Python sources: those sources are fully capability-checked by the
# bootstrap above before this interpreter can read one.
/usr/bin/env -i \
  HOME="${SCRATCH_HOME}" \
  PATH="${TRUSTED_PATH}" \
  LOCALE_CATALOG_MISE_ROOT="${SEALED_ROOT}" \
  LOCALE_CATALOG_GIT="${GIT}" \
  LOCALE_CATALOG_NODE="${NODE}" \
  LOCALE_CATALOG_UV="${UV}" \
  LOCALE_CATALOG_STACK="${STACK}" \
  ARKHAM_LOCALE_CATALOG_PYTHON_VENV="${VENV}" \
  "${PYTHON}" -I -S -E -B -c '
import runpy
import sys

scripts, site_packages, target, *arguments = sys.argv[1:]
sys.path[:0] = [scripts, site_packages]
sys.argv = [target, *arguments]
runpy.run_path(target, run_name="__main__")
' "${ROOT}/scripts" "${SITE_PACKAGES}" "${ROOT}/$1" "${@:2}" || status=$?
exit "${status}"
