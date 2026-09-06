#!/bin/sh
# Starts a reviewed build stage in a minimal environment. A POSIX shell is
# used for this handoff so caller-controlled BASH_ENV/ENV code cannot observe
# receipt capabilities before the reviewed stage consumes and unexports them.

set -eu

[ "$#" -ge 1 ] || {
  echo "run-authorized-stage: missing stage script" >&2
  exit 2
}

safe_path='/opt/homebrew/bin:/usr/local/bin:/usr/bin:/bin:/usr/sbin:/sbin'
receipt_file=${ARKHAM_TOOLCHAIN_RECEIPT_FILE-}
receipt_token=${ARKHAM_TOOLCHAIN_RECEIPT_TOKEN-}

if [ -n "$receipt_file" ] || [ -n "$receipt_token" ]; then
  [ -n "$receipt_file" ] && [ -n "$receipt_token" ] || {
    echo "run-authorized-stage: receipt path and token must be supplied together" >&2
    exit 2
  }
fi

if [ -n "${GITHUB_ENV-}" ]; then
  exec /usr/bin/env -i \
    HOME="${HOME-}" \
    PATH="$safe_path" \
    GITHUB_ENV="$GITHUB_ENV" \
    ARKHAM_TOOLCHAIN_RECEIPT_FILE="$receipt_file" \
    ARKHAM_TOOLCHAIN_RECEIPT_TOKEN="$receipt_token" \
    /bin/bash --noprofile --norc "$@"
fi

exec /usr/bin/env -i \
  HOME="${HOME-}" \
  PATH="$safe_path" \
  ARKHAM_TOOLCHAIN_RECEIPT_FILE="$receipt_file" \
  ARKHAM_TOOLCHAIN_RECEIPT_TOKEN="$receipt_token" \
  /bin/bash --noprofile --norc "$@"
