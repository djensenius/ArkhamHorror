#!/usr/bin/env bash
set -euo pipefail

readonly ROOT="$(cd -- "$(dirname -- "${BASH_SOURCE[0]}")/.." && pwd -P)"
readonly MISE_DATA_ROOT="${HOME}/.local/share/mise"
readonly PYTHON="${MISE_DATA_ROOT}/installs/python/3.14.7/bin/python"
readonly TRUSTED_PATH="${HOME}/.local/bin:${MISE_DATA_ROOT}/installs/node/26.7.0/bin:${MISE_DATA_ROOT}/installs/python/3.14.7/bin:${MISE_DATA_ROOT}/installs/uv/0.12.6/bin:/opt/homebrew/bin:/usr/local/bin:/usr/bin:/bin"
readonly LOCK_DIRECTORY="${ROOT}/.locale-catalog-python-sync.lock"

shopt -s nullglob
uv_candidates=("${MISE_DATA_ROOT}"/installs/uv/0.12.6/uv-*/uv)
if [[ ! -x "${PYTHON}" || "${#uv_candidates[@]}" -ne 1 ]]; then
  echo "locale-catalog python: exact mise Python 3.14.7 and uv 0.12.6 must be installed under ${MISE_DATA_ROOT}" >&2
  exit 1
fi
readonly UV="${uv_candidates[0]}"

if [[ "$("${PYTHON}" -c 'import sys; print(f"{sys.implementation.name} {sys.version_info.major}.{sys.version_info.minor}.{sys.version_info.micro} {sys.implementation.cache_tag}")')" != "cpython 3.14.7 cpython-314" ]]; then
  echo "locale-catalog python: ${PYTHON} is not the required CPython 3.14.7 / cpython-314 runtime" >&2
  exit 1
fi

cd "${ROOT}"
if [[ "${ARKHAM_LOCALE_CATALOG_PYTHON_LOCK_HELD:-}" != "${LOCK_DIRECTORY}" ]]; then
  while ! /bin/mkdir "${LOCK_DIRECTORY}" 2>/dev/null; do
    /bin/sleep 1
  done
  trap '/bin/rmdir "${LOCK_DIRECTORY}"' EXIT
fi
env -i HOME="${HOME}" PATH="${TRUSTED_PATH}" "${UV}" sync --locked --no-cache --link-mode copy --reinstall --no-dev --no-install-project --python "${PYTHON}" --quiet
env -i HOME="${HOME}" PATH="${TRUSTED_PATH}" ARKHAM_LOCALE_CATALOG_PYTHON_LOCK_HELD="${LOCK_DIRECTORY}" "${PYTHON}" -I -S -E -B "${ROOT}/scripts/locale_catalog_runtime.py" "$@"
