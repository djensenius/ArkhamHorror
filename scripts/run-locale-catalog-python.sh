#!/bin/sh
# Authoritative entry point for every locale-catalog Python command.
#
# This stage is POSIX `sh` on purpose. A `#!/bin/bash` entry point would source
# a caller-supplied `BASH_ENV` script *before* the first line of the file ran,
# so no in-script guard could ever be early enough; a non-interactive POSIX
# shell reads neither `BASH_ENV` nor `ENV`. It therefore does exactly one
# thing: rebuild the environment from nothing with `/usr/bin/env -i` and hand
# the sealed bash stage the two inputs that carry authority -- the explicit
# toolchain root, and the arguments the caller asked for.
#
# `LOCALE_CATALOG_MISE_ROOT` is deliberately forwarded even when unset (as an
# empty value): the sealed stage refuses an empty root with a specific
# diagnostic rather than inventing one from `$HOME`.
set -eu

# Do not resolve even `dirname` through the caller's PATH before the
# environment is cleared.  Production routes name this script with a path;
# accepting a bare command name would itself delegate the first authority to
# PATH, so reject it instead.
case "$0" in
  */*) self_dir=${0%/*} ;;
  *)
    echo "locale-catalog python: invoke scripts/run-locale-catalog-python.sh with an explicit path, never through PATH" >&2
    exit 1
    ;;
esac
self_dir=$(CDPATH='' cd -- "$self_dir" && pwd -P)

exec /usr/bin/env -i \
  LOCALE_CATALOG_SEALED_SHELL=1 \
  LOCALE_CATALOG_MISE_ROOT="${LOCALE_CATALOG_MISE_ROOT-}" \
  LOCALE_CATALOG_PROBE="${LOCALE_CATALOG_PROBE-}" \
  /bin/bash --noprofile --norc -- "${self_dir}/locale-catalog-python-sealed.sh" "$@"
