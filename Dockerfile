FROM node:26.7.0-alpine@sha256:aadf416b2cdce311a8811ba3f0608a61b77dbf997500e2eafe781b51f6a0b019 AS frontend
ARG TARGETARCH

# Frontend

ENV LC_ALL=C.UTF-8

ARG ASSET_HOST=""

RUN mkdir -p /opt/arkham/src/frontend

WORKDIR /opt/arkham/src/frontend
COPY ./frontend/package.json ./frontend/tsconfig.json ./frontend/vite.config.js ./frontend/eslint.config.js ./frontend/package-lock.json /opt/arkham/src/frontend/
COPY ./offline/toolchain.lock ./offline/scripts/docker-runtime-authority.sh /opt/arkham/toolchain/
RUN --mount=type=cache,target=/root/.npm npm ci --ignore-scripts --prefer-offline
COPY ./frontend /opt/arkham/src/frontend
# The locale-catalog generator (run by npm's prebuild) derives its required-key
# set from the governed contract fixtures and from the backend's emitted-key
# registry, so both have to be in the image.
COPY ./contracts /opt/arkham/src/contracts
COPY ./backend/arkham-api/i18n-emitted-keys.json /opt/arkham/src/backend/arkham-api/i18n-emitted-keys.json
ENV VITE_ASSET_HOST=${ASSET_HOST}
# Reverify the exact Node executable and complete npm CLI import tree after ci
# and immediately before each untrusted package-script boundary.
RUN /opt/arkham/toolchain/docker-runtime-authority.sh node /opt/arkham/toolchain/toolchain.lock "$TARGETARCH" && \
    env -i HOME=/nonexistent PATH=/usr/local/bin:/usr/bin:/bin /usr/local/bin/node scripts/locale-catalog/generate.mjs
RUN /opt/arkham/toolchain/docker-runtime-authority.sh node /opt/arkham/toolchain/toolchain.lock "$TARGETARCH" && \
    /usr/local/bin/node /usr/local/lib/node_modules/npm/bin/npm-cli.js run build
# The image copies `dist` out of this stage, so the catalog is verified here and
# republished from the verified buffers: what the next stage copies — and what
# nginx serves — is exactly what passed, not an intermediate tree that happened
# to be correct when the build finished.
RUN node scripts/locale-catalog/verify-dist.mjs --publish

FROM ubuntu:22.04@sha256:2edbbc5dc405e9612ba3584ce95480277e3eb374407b5505fe26f17df77c7dbc AS base

ARG DEBIAN_FRONTEND=noninteractive
ENV LC_ALL=C.UTF-8
ENV TZ=UTC

# install dependencies
RUN \
    ln -snf /usr/share/zoneinfo/$TZ /etc/localtime && echo $TZ > /etc/timezone && \
    apt-get update -y && \
    apt-get install -y --no-install-recommends --fix-missing \
        libpcre3 \
        libpcre3-dev \
        libpq-dev \
        curl \
        libtinfo6 \
        libnuma-dev \
        zlib1g-dev \
        libgmp-dev \
        libgmp10 \
        libtinfo-dev \
        git \
        wget \
        lsb-release \
        software-properties-common \
        gnupg2 \
        apt-transport-https \
        gcc \
        autoconf \
        automake \
        build-essential && \
  rm -rf /var/lib/apt/lists/*

ARG TARGETARCH

ARG GHC=9.14.1
ARG CABAL=3.16.0.0
ARG STACK=3.7.1
ARG CACHE_ID="${TARGETARCH}-${GHC}-${CABAL}-${STACK}"
ENV CACHE_ID=${CACHE_ID}

# The builder has no mutable ghcup metadata path. It fetches the exact GHC,
# Cabal, and Stack archives named in the reviewed table, verifies each before
# extraction, and verifies Cabal's installed executable bytes before use.
COPY ./offline/toolchain.lock ./offline/scripts/docker-toolchain.sh /opt/arkham/toolchain/
RUN set -eu; \
    case "$TARGETARCH" in \
      arm64) \
        platform="linux-arm64"; \
        ghc_archive="ghc-${GHC}-aarch64-deb10-linux.tar.xz"; \
        stack_archive="stack-${STACK}-linux-aarch64.tar.gz"; \
        cabal_archive="cabal-install-${CABAL}-aarch64-linux-deb10.tar.xz" ;; \
      amd64) \
        platform="linux-x86_64"; \
        ghc_archive="ghc-${GHC}-x86_64-ubuntu20_04-linux.tar.xz"; \
        stack_archive="stack-${STACK}-linux-x86_64.tar.gz"; \
        cabal_archive="cabal-install-${CABAL}-x86_64-linux-ubuntu22_04.tar.xz" ;; \
      *) echo "Unsupported Docker target architecture: $TARGETARCH" >&2; exit 1 ;; \
    esac; \
    toolchain="/opt/arkham/toolchain"; \
    . "${toolchain}/docker-toolchain.sh"; \
    mkdir -p "${toolchain}/downloads" "${toolchain}/extract"; \
    fetch_locked_archive "${toolchain}/toolchain.lock" ghc "$platform" "$ghc_archive" \
      "https://downloads.haskell.org/~ghc/${GHC}/${ghc_archive}" "${toolchain}/downloads/${ghc_archive}"; \
    fetch_locked_archive "${toolchain}/toolchain.lock" stack "$platform" "$stack_archive" \
      "https://github.com/commercialhaskell/stack/releases/download/v${STACK}/${stack_archive}" "${toolchain}/downloads/${stack_archive}"; \
    fetch_locked_archive "${toolchain}/toolchain.lock" cabal "$platform" "$cabal_archive" \
      "https://downloads.haskell.org/~cabal/cabal-install-${CABAL}/${cabal_archive}" "${toolchain}/downloads/${cabal_archive}"; \
    tar -xJf "${toolchain}/downloads/${ghc_archive}" -C "${toolchain}/extract"; \
    ghc_dir="$(find "${toolchain}/extract" -maxdepth 1 -type d -name 'ghc-*' -print -quit)"; \
    test -n "$ghc_dir"; \
    cp -a "${ghc_dir}/." /usr/local/; \
    tar -xzf "${toolchain}/downloads/${stack_archive}" -C "${toolchain}/extract"; \
    stack_bin="$(find "${toolchain}/extract" -type f -name stack -perm -u+x -print -quit)"; \
    test -n "$stack_bin"; \
    install -m 0755 "$stack_bin" /usr/local/bin/stack; \
    mkdir -p "${toolchain}/extract/cabal"; \
    tar -xJf "${toolchain}/downloads/${cabal_archive}" -C "${toolchain}/extract/cabal"; \
    install -m 0755 "${toolchain}/extract/cabal/cabal" /usr/local/bin/cabal; \
    verify_locked_binary "${toolchain}/toolchain.lock" docker-ghc "$platform" bin/ghc /usr/local/bin/ghc; \
    verify_locked_binary "${toolchain}/toolchain.lock" docker-stack "$platform" bin/stack /usr/local/bin/stack; \
    verify_locked_binary "${toolchain}/toolchain.lock" docker-cabal "$platform" bin/cabal /usr/local/bin/cabal; \
    test "$(ghc --numeric-version)" = "$GHC"; \
    test "$(cabal --numeric-version)" = "$CABAL"; \
    test "$(stack --numeric-version)" = "$STACK"

FROM base AS dependencies

RUN mkdir -p \
  /opt/arkham/bin \
  /opt/arkham/src/backend/arkham-api/app \
  /opt/arkham/src/backend/arkham-api/library \
  /opt/arkham/src/backend/validate/app \
  /opt/arkham/src/backend/cards-discover/app \
  /opt/arkham/src/backend/cards-discover/library \
  /opt/arkham/src/backend/devel-store-lock/library

WORKDIR /opt/arkham/src/backend
COPY ./backend/stack.yaml ./backend/stack.yaml.lock /opt/arkham/src/backend/
COPY ./backend/arkham-api/package.yaml /opt/arkham/src/backend/arkham-api/package.yaml
COPY ./backend/validate/package.yaml /opt/arkham/src/backend/validate/package.yaml
COPY ./backend/cards-discover/package.yaml /opt/arkham/src/backend/cards-discover/package.yaml
COPY ./backend/devel-store-lock/package.yaml /opt/arkham/src/backend/devel-store-lock/package.yaml
RUN --mount=type=cache,id=stack-home-${CACHE_ID},target=/root/.stack \
    --mount=type=cache,id=stack-work-shared-${CACHE_ID},target=/opt/arkham/src/backend/.stack-work \
    stack build --system-ghc --dependencies-only --no-terminal --ghc-options '-fno-write-ide-info -j4 +RTS -A128m -n2m -RTS'

FROM dependencies AS api

RUN mkdir -p \
  /opt/arkham/src/backend \
  /opt/arkham/bin

COPY ./backend /opt/arkham/src/backend

WORKDIR /opt/arkham/src/backend/cards-discover
RUN --mount=type=cache,id=stack-home-${CACHE_ID},target=/root/.stack \
    --mount=type=cache,id=stack-work-shared-${CACHE_ID},target=/opt/arkham/src/backend/.stack-work \
    --mount=type=cache,id=stack-discover-${CACHE_ID},target=/opt/arkham/src/backend/cards-discover/.stack-work \
    stack build --system-ghc --no-terminal --ghc-options '-fno-write-ide-info -j4 +RTS -A128m -n2m -RTS' cards-discover

WORKDIR /opt/arkham/src/backend/arkham-api
# The build itself lives in scripts/docker-build-api.sh, which repairs a
# .stack-work cache left dirty by a cancelled build before compiling. Keep this
# RUN a bare script invocation: BuildKit keys a cache mount's *contents* by the
# text of the RUN that mounts it, so editing the command here throws away the
# cached .stack-work and forces a cold rebuild of all ~6800 modules. Editing the
# script does not.
RUN --mount=type=cache,id=stack-home-${CACHE_ID},target=/root/.stack \
    --mount=type=cache,id=stack-work-shared-${CACHE_ID},target=/opt/arkham/src/backend/.stack-work \
    --mount=type=cache,id=stack-api-${CACHE_ID},target=/opt/arkham/src/backend/arkham-api/.stack-work \
    --mount=type=cache,id=stack-discover-${CACHE_ID},target=/opt/arkham/src/backend/cards-discover/.stack-work \
    --mount=type=cache,id=stack-validate-${CACHE_ID},target=/opt/arkham/src/backend/validate/.stack-work \
    --mount=type=cache,id=stack-api-hie-${CACHE_ID},target=/opt/arkham/src/backend/arkham-api/.hie \
    --mount=type=cache,id=stack-validate-hie-${CACHE_ID},target=/opt/arkham/src/backend/validate/.hie \
    --mount=type=cache,id=stack-discover-hie-${CACHE_ID},target=/opt/arkham/src/backend/cards-discover/.hie \
  sh /opt/arkham/src/backend/scripts/docker-build-api.sh

# The final production image supplies the nginx bytes. Pin the official
# multi-platform manifest digest so the exact nginx runtime tested below is
# the one shipped, rather than a mutable Ubuntu apt package.
FROM nginx:1.27.5@sha256:6784fb0834aa7dbbe12e3d7471e69c290df3e6ba810dc38b34ae33d3c1c05f7d AS app
ARG TARGETARCH

# App

ENV LC_ALL=C.UTF-8
LABEL org.opencontainers.image.nginx-runtime-reference="nginx:1.27.5@sha256:6784fb0834aa7dbbe12e3d7471e69c290df3e6ba810dc38b34ae33d3c1c05f7d"

RUN mkdir -p \
  /opt/arkham/bin \
  /opt/arkham/src/backend/arkham-api \
  /opt/arkham/src/frontend \
  /var/log/nginx \
  /var/lib/nginx \
  /var/cache/nginx \
  /run

COPY --from=frontend /opt/arkham/src/frontend/dist /opt/arkham/src/frontend/dist
COPY --from=api /opt/arkham/bin/arkham-api /opt/arkham/bin/arkham-api
COPY ./backend/arkham-api/config /opt/arkham/src/backend/arkham-api/config
COPY ./prod.nginxconf /opt/arkham/src/backend/prod.nginxconf
COPY ./start.sh /opt/arkham/src/backend/arkham-api/start.sh
COPY ./web-entrypoint.sh /web-entrypoint.sh
COPY ./backend/arkham-api/digital-ocean.crt /opt/arkham/src/backend/arkham-api/digital-ocean.crt
COPY ./offline/toolchain.lock ./offline/scripts/docker-runtime-authority.sh /opt/arkham/toolchain/

RUN useradd -ms /bin/bash yesod && \
  chown -R yesod:yesod /opt/arkham /var/log/nginx /var/lib/nginx /var/cache/nginx /run && \
  chmod a+x /opt/arkham/src/backend/arkham-api/start.sh /web-entrypoint.sh && \
  /opt/arkham/toolchain/docker-runtime-authority.sh nginx /opt/arkham/toolchain/toolchain.lock "$TARGETARCH"
USER yesod
ENV PATH="$PATH:/opt/stack/bin:/opt/arkham/bin"

EXPOSE 3000

WORKDIR /opt/arkham/src/backend/arkham-api
ENTRYPOINT ["/web-entrypoint.sh"]
CMD ["./start.sh"]
