#!/bin/sh
# Verify immutable Node/npm and nginx runtime closures inside Docker stages.
set -eu

die() {
  printf '%s\n' "docker-runtime-authority: $*" >&2
  exit 1
}

platform_for_arch() {
  case "$1" in
    amd64) printf '%s\n' linux-x86_64 ;;
    arm64) printf '%s\n' linux-arm64 ;;
    *) die "unsupported Docker target architecture: $1" ;;
  esac
}

lock_digest() {
  awk -F '\t' -v component="$2" -v platform="$3" -v artifact="$4" '
    $1 == "image" && $2 == component && $3 == platform && $4 == artifact {
      matches += 1
      value = $6
    }
    END {
      if (matches != 1 || length(value) != 64 || value ~ /[^0-9a-f]/) exit 1
      print value
    }
  ' "$1"
}

digest_records() {
  sha256sum | awk '{print $1}'
}

node_records() {
  find /usr/local/bin/node /usr/local/lib/node_modules/npm -xdev -print \
    | LC_ALL=C sort \
    | while IFS= read -r path; do
        if [ -L "$path" ]; then
          die "Node/npm closure contains a symlink: $path"
        elif [ -f "$path" ]; then
          sha256sum "$path" | awk -v path="$path" '{print "file\t" path "\t" $1}'
        elif [ -d "$path" ]; then
          printf 'dir\t%s\n' "$path"
        else
          die "Node/npm closure contains an unsupported path: $path"
        fi
      done
}

nginx_records() {
  queue=/usr/sbin/nginx
  seen=""
  while [ -n "$queue" ]; do
    # Loader paths cannot contain whitespace; disable pathname expansion before
    # deliberately splitting the trusted ldd path list.
    set -f
    # shellcheck disable=SC2086
    set -- $queue
    set +f
    file=$1
    shift
    queue="$*"
    case " $seen " in *" $file "*) continue ;; esac
    seen="${seen} ${file}"
    # Debian's dynamic-loader SONAME paths are symlinks. Hash the loader-visible
    # path (which follows the link) instead of rejecting the immutable base
    # image's conventional ABI aliases.
    [ -f "$file" ] || die "nginx closure has a missing path: $file"
    sha256sum "$file" | awk -v path="$file" '{print "file\t" path "\t" $1}'
    dependencies="$(ldd "$file" 2>/dev/null \
      | awk '$2 == "=>" && $3 ~ /^\// {print $3} $1 ~ /^\// {print $1}' \
      | tr '\n' ' ')" \
      || die "could not inspect nginx dependency closure: $file"
    queue="${queue}${queue:+ }${dependencies}"
  done
}

verify_closure() {
  lock="$1"
  component="$2"
  arch="$3"
  artifact="$4"
  platform="$(platform_for_arch "$arch")"
  expected="$(lock_digest "$lock" "$component" "$platform" "$artifact")" \
    || die "missing committed ${component}/${platform} closure authority"
  case "$component" in
    node-runtime) records="$(node_records)" ;;
    nginx-runtime-closure) records="$(nginx_records)" ;;
    *) die "unsupported closure component: $component" ;;
  esac
  actual="$(printf '%s\n' "$records" | LC_ALL=C sort -u | digest_records)" \
    || die "could not calculate ${component} closure"
  [ "$actual" = "$expected" ] || die "${component} closure differs from committed authority"
}

case "${1:-}" in
  node)
    [ "$#" = 3 ] || die "usage: $0 node LOCK TARGETARCH"
    [ "$(/usr/local/bin/node --version)" = v26.7.0 ] || die "Node version differs from the pinned runtime"
    verify_closure "$2" node-runtime "$3" node:26.7.0-alpine
    ;;
  nginx)
    [ "$#" = 3 ] || die "usage: $0 nginx LOCK TARGETARCH"
    verify_closure "$2" nginx-runtime-closure "$3" nginx:1.27.5
    ;;
  *)
    die "usage: $0 node|nginx LOCK TARGETARCH"
    ;;
esac
