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
github_env=${GITHUB_ENV-}

if [ -n "$receipt_file" ] || [ -n "$receipt_token" ]; then
  [ -n "$receipt_file" ] && [ -n "$receipt_token" ] || {
    echo "run-authorized-stage: receipt path and token must be supplied together" >&2
    exit 2
  }
fi

newline='
'
carriage_return="$(printf '\r')"
for value in "$receipt_file" "$receipt_token" "$github_env"; do
  case "$value" in
    *"$newline"*|*"$carriage_return"*)
      echo "run-authorized-stage: capability values must be single-line text" >&2
      exit 2
      ;;
  esac
done

# The reviewed stage consumes this frame immediately and closes fd 9 before it
# can execute any tool or payload. The subsequent exec replaces this process,
# so no ancestor retains the capabilities in its initial environment.
exec 9<<EOF
${receipt_file}
${receipt_token}
${github_env}
EOF

exec /usr/bin/env -i \
  HOME="${HOME-}" \
  PATH="$safe_path" \
  ARKHAM_AUTHORIZED_STAGE_FD=9 \
  /bin/bash --noprofile --norc "$@"
