#!/bin/sh
# Shared by Dockerfile and its shell regression test. Every Docker toolchain
# archive is looked up in the committed table before its bytes can be used.

set -eu

docker_sha256_file() {
  file=$1
  if command -v sha256sum >/dev/null 2>&1; then
    sha256sum "$file" | awk '{print $1}'
  elif command -v shasum >/dev/null 2>&1; then
    shasum -a 256 "$file" | awk '{print $1}'
  else
    echo "docker-toolchain: no SHA-256 utility is available" >&2
    return 1
  fi
}

docker_require_sha256() {
  value=${1:-}
  [ "${#value}" -eq 64 ] || return 1
  case "$value" in *[!0-9a-f]*) return 1 ;; esac
  return 0
}

docker_locked_archive_sha256() {
  lock=$1
  component=$2
  platform=$3
  archive=$4
  [ -f "$lock" ] && [ ! -L "$lock" ] || {
    echo "docker-toolchain: authority file is missing or unsafe: $lock" >&2
    return 1
  }
  record=$(
    awk -F '\t' \
      -v component="$component" \
      -v platform="$platform" \
      -v archive="$archive" '
        $1 == "archive" && $2 == component && $3 == platform && $4 == archive && $5 == "exact" {
          matches += 1
          digest = $6
        }
        END {
          if (matches != 1) exit 1
          print digest
        }
      ' "$lock"
  ) || {
    echo "docker-toolchain: no unique authority for ${component}/${platform}/${archive}" >&2
    return 1
  }
  docker_require_sha256 "$record" || {
    echo "docker-toolchain: invalid authority for ${component}/${platform}/${archive}" >&2
    return 1
  }
  printf '%s\n' "$record"
}

docker_locked_binary_sha256() {
  lock=$1
  component=$2
  platform=$3
  binary=$4
  [ -f "$lock" ] && [ ! -L "$lock" ] || {
    echo "docker-toolchain: authority file is missing or unsafe: $lock" >&2
    return 1
  }
  record=$(
    awk -F '\t' \
      -v component="$component" \
      -v platform="$platform" \
      -v binary="$binary" '
        $1 == "binary" && $2 == component && $3 == platform && $4 == binary && $5 == "exact" {
          matches += 1
          digest = $6
        }
        END {
          if (matches != 1) exit 1
          print digest
        }
      ' "$lock"
  ) || {
    echo "docker-toolchain: no unique exact binary authority for ${component}/${platform}/${binary}" >&2
    return 1
  }
  docker_require_sha256 "$record" || {
    echo "docker-toolchain: invalid binary authority for ${component}/${platform}/${binary}" >&2
    return 1
  }
  printf '%s\n' "$record"
}

verify_locked_binary() {
  lock=$1
  component=$2
  platform=$3
  binary=$4
  path=$5
  expected=$(docker_locked_binary_sha256 "$lock" "$component" "$platform" "$binary")
  actual=$(docker_sha256_file "$path")
  [ "$actual" = "$expected" ] || {
    echo "docker-toolchain: installed ${component} binary failed SHA-256 authority" >&2
    return 1
  }
}

# fetch_locked_archive LOCK COMPONENT PLATFORM ARCHIVE URL DESTINATION
fetch_locked_archive() {
  lock=$1
  component=$2
  platform=$3
  archive=$4
  url=$5
  destination=$6
  expected=$(docker_locked_archive_sha256 "$lock" "$component" "$platform" "$archive")
  destination_dir=$(dirname "$destination")
  partial="${destination}.partial.$$"
  mkdir -p "$destination_dir"
  rm -f "$partial"
  curl -fsSL --connect-timeout 30 --max-time 600 "$url" -o "$partial" || {
    rm -f "$partial"
    return 1
  }
  actual=$(docker_sha256_file "$partial") || {
    rm -f "$partial"
    return 1
  }
  [ "$actual" = "$expected" ] || {
    rm -f "$partial"
    echo "docker-toolchain: downloaded ${component} archive failed SHA-256 authority" >&2
    return 1
  }
  mv -f "$partial" "$destination"
}
