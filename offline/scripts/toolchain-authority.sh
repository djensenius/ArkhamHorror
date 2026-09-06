#!/usr/bin/env bash
# Shared offline toolchain identities. Source only after utils.sh and init_paths.

GHC_VERSION="9.14.1"
GHC_RESOLVED_BINARY="ghc/${GHC_VERSION}/bin/ghc-${GHC_VERSION}"
GHC_PUBLIC_BINARY="bin/ghc"
GHC_PUBLIC_TARGET="../ghc/${GHC_VERSION}/bin/ghc"
GHC_INTERNAL_TARGET="ghc-${GHC_VERSION}"
STACK_VERSION="3.7.1"
NODE_VERSION="26.7.0"
PG_VERSION="14.15"
NGINX_VERSION="1.26.2"
PG_SRC_ARCHIVE="postgresql-${PG_VERSION}.tar.bz2"
PG_SRC_URL="https://ftp.postgresql.org/pub/source/v${PG_VERSION}/${PG_SRC_ARCHIVE}"

ghc_build_identity() {
    local archive="$1" source_sha256
    source_sha256="$(toolchain_archive_sha256 ghc "$PLATFORM" "$archive")"
    toolchain_build_identity ghc "$PLATFORM" "$GHC_VERSION" "$source_sha256" \
        "archive-extract-v3-preserve-links:${GHC_RESOLVED_BINARY};${GHC_PUBLIC_BINARY}->${GHC_PUBLIC_TARGET};ghc/${GHC_VERSION}/bin/ghc->${GHC_INTERNAL_TARGET}"
}

stack_build_identity() {
    local archive="$1" source_sha256
    source_sha256="$(toolchain_archive_sha256 stack "$PLATFORM" "$archive")"
    toolchain_build_identity stack "$PLATFORM" "$STACK_VERSION" "$source_sha256" \
        "archive-extract-v1:bin/stack"
}

postgres_build_identity() {
    local source_sha256
    source_sha256="$(toolchain_archive_sha256 postgres "$PLATFORM" "$PG_SRC_ARCHIVE")"
    toolchain_build_identity postgres "$PLATFORM" "$PG_VERSION" "$source_sha256" \
        "native-build-v1:--with-uuid=e2fs,--without-readline,--without-zlib"
}

nginx_build_identity() {
    local archive="nginx-${NGINX_VERSION}.tar.gz" source_sha256
    source_sha256="$(toolchain_archive_sha256 nginx "$PLATFORM" "$archive")"
    toolchain_build_identity nginx "$PLATFORM" "$NGINX_VERSION" "$source_sha256" \
        "$(nginx_build_recipe)"
}

node_binary_identity() {
    local authority kind digest version
    authority="$(toolchain_binary_authority node "$PLATFORM" "bin/node")"
    IFS=$'\t' read -r kind digest version <<< "$authority"
    [ "$kind" = "exact" ] || die "Node binary authority must be exact"
    printf '%s\n' "$digest"
}

get_ghc_bindist_info() {
    local ver="${GHC_VERSION}"
    local archive=""
    case "$PLATFORM" in
        macos-arm64)   archive="ghc-${ver}-aarch64-apple-darwin.tar.xz" ;;
        macos-x86_64)  archive="ghc-${ver}-x86_64-apple-darwin.tar.xz" ;;
        linux-x86_64)  archive="ghc-${ver}-x86_64-ubuntu20_04-linux.tar.xz" ;;
        linux-arm64)   archive="ghc-${ver}-aarch64-deb10-linux.tar.xz" ;;
        *) die "Unsupported platform: $PLATFORM" ;;
    esac
    printf '%s|%s\n' "https://downloads.haskell.org/~ghc/${ver}/${archive}" "$archive"
}

get_stack_download_info() {
    local ver="${STACK_VERSION}"
    local archive=""
    case "$PLATFORM" in
        macos-arm64)   archive="stack-${ver}-osx-aarch64.tar.gz" ;;
        macos-x86_64)  archive="stack-${ver}-osx-x86_64.tar.gz" ;;
        linux-x86_64)  archive="stack-${ver}-linux-x86_64.tar.gz" ;;
        linux-arm64)   archive="stack-${ver}-linux-aarch64.tar.gz" ;;
        *) die "Unsupported platform: $PLATFORM" ;;
    esac
    printf '%s|%s\n' "https://github.com/commercialhaskell/stack/releases/download/v${ver}/${archive}" "$archive"
}

verify_ghc_and_stack_installation() {
    local ghc_archive="$1" stack_archive="$2"
    local ghc_identity stack_identity
    ghc_identity="$(ghc_build_identity "$ghc_archive")"
    stack_identity="$(stack_build_identity "$stack_archive")"
    verify_install_manifest ghc "$GHCUP_DIR" "$ghc_identity" "$GHC_RESOLVED_BINARY" \
        "ghc/${GHC_VERSION}" "bin" "env"
    verify_internal_symlink "$GHCUP_DIR" "$GHC_PUBLIC_BINARY" "$GHC_PUBLIC_TARGET"
    verify_internal_symlink "$GHCUP_DIR" "ghc/${GHC_VERSION}/bin/ghc" "$GHC_INTERNAL_TARGET"
    verify_install_manifest stack "$GHCUP_DIR" "$stack_identity" "bin/stack" "bin/stack"
    verify_binary_version_contains "GHC" "${GHCUP_DIR}/${GHC_RESOLVED_BINARY}" \
        "--numeric-version" "$GHC_VERSION"
    verify_binary_version_contains "Stack" "${GHCUP_DIR}/bin/stack" \
        "--numeric-version" "$STACK_VERSION"
}

verify_node_installation() {
    local identity authority kind digest expected_version
    identity="$(node_binary_identity)"
    authority="$(toolchain_binary_authority node "$PLATFORM" "bin/node")"
    IFS=$'\t' read -r kind digest expected_version <<< "$authority"
    verify_install_manifest node "${DEPS_DIR}/node" "$identity" "bin/node" \
        "bin" "lib/node_modules/npm"
    verify_binary_version_contains "Node.js" "${DEPS_DIR}/node/bin/node" "--version" "$expected_version"
    verify_cmd "npm" "${DEPS_DIR}/node/bin/npm" "--version"
}

verify_postgres_installation() {
    local identity
    identity="$(postgres_build_identity)"
    verify_install_manifest postgres "${DEPS_DIR}/postgres" "$identity" "bin/postgres" \
        "bin" "lib" "share"
    verify_binary_version_contains "PostgreSQL" "${DEPS_DIR}/postgres/bin/postgres" "--version" "$PG_VERSION"
}

verify_nginx_installation() {
    local identity nginx_version
    identity="$(nginx_build_identity)"
    verify_install_manifest nginx "${DEPS_DIR}/nginx" "$identity" "bin/nginx" "bin"
    nginx_version="$("${DEPS_DIR}/nginx/bin/nginx" -V 2>&1)" \
        || die "Nginx failed its post-identity configuration check"
    case "$nginx_version" in
        *"nginx/${NGINX_VERSION}"*) ;;
        *) die "Nginx configuration check reported the wrong version: ${nginx_version}" ;;
    esac
    case "$nginx_version" in
        *"--with-http_gzip_static_module"*) ;;
        *) die "Nginx lacks --with-http_gzip_static_module required by the shipped config" ;;
    esac
}

verify_all_offline_toolchain() {
    local ghc_info ghc_archive stack_info stack_archive
    ghc_info="$(get_ghc_bindist_info)"
    ghc_archive="${ghc_info##*|}"
    stack_info="$(get_stack_download_info)"
    stack_archive="${stack_info##*|}"
    verify_ghc_and_stack_installation "$ghc_archive" "$stack_archive"
    verify_node_installation
    verify_postgres_installation
    verify_nginx_installation
}
