FROM node:26.7.0-alpine@sha256:aadf416b2cdce311a8811ba3f0608a61b77dbf997500e2eafe781b51f6a0b019 AS frontend

# Frontend

ENV LC_ALL=C.UTF-8

ARG ASSET_HOST=""
# ".arkhamhorror.app" in production, so the 3ed subdomain shares the sign-in
# cookie; empty for self-hosting, where the cookie stays on the host serving it
ARG AUTH_COOKIE_DOMAIN=""

RUN mkdir -p \
  /opt/arkham/src/backend/arkham-api \
  /opt/arkham/src/frontend

WORKDIR /opt/arkham/src/frontend
COPY ./frontend/package.json ./frontend/tsconfig.json ./frontend/vite.config.js ./frontend/eslint.config.js ./frontend/package-lock.json /opt/arkham/src/frontend/
RUN --mount=type=cache,target=/root/.npm npm ci --ignore-scripts --prefer-offline
COPY ./frontend /opt/arkham/src/frontend
# The locale-catalog generator (run by npm's prebuild) derives its required-key
# set from the governed contract fixtures and from the backend's emitted-key
# registry, so both have to be in the image.
COPY ./contracts /opt/arkham/src/contracts
COPY ./backend/arkham-api/i18n-emitted-keys.json /opt/arkham/src/backend/arkham-api/i18n-emitted-keys.json
ENV VITE_ASSET_HOST=${ASSET_HOST}
ENV VITE_AUTH_COOKIE_DOMAIN=${AUTH_COOKIE_DOMAIN}
RUN env -i HOME=/nonexistent PATH=/usr/local/bin:/usr/bin:/bin /usr/local/bin/node scripts/locale-catalog/generator-launcher.mjs generate.mjs
RUN /usr/local/bin/node /usr/local/lib/node_modules/npm/bin/npm-cli.js run build
# The image copies `dist` out of this stage, so the catalog is verified here and
# republished from the verified buffers: what the next stage copies — and what
# nginx serves — is exactly what passed, not an intermediate tree that happened
# to be correct when the build finished.
RUN env -i HOME=/nonexistent PATH=/usr/local/bin:/usr/bin:/bin /usr/local/bin/node scripts/locale-catalog/generator-launcher.mjs verify-dist.mjs --publish

# Third edition frontend, served from 3ed.arkhamhorror.app (see prod.nginxconf)
FROM node:26.7.0-alpine@sha256:aadf416b2cdce311a8811ba3f0608a61b77dbf997500e2eafe781b51f6a0b019 AS frontend-3ed

ENV LC_ALL=C.UTF-8

ARG ASSET_HOST=""
ARG AUTH_COOKIE_DOMAIN=""
ARG MAIN_SITE_URL="https://arkhamhorror.app"

WORKDIR /opt/arkham/src/frontend-3ed
COPY ./frontend-3ed/package.json ./frontend-3ed/package-lock.json /opt/arkham/src/frontend-3ed/
RUN --mount=type=cache,target=/root/.npm npm ci --ignore-scripts --prefer-offline
COPY ./frontend-3ed /opt/arkham/src/frontend-3ed
ENV VITE_ASSET_HOST=${ASSET_HOST}
ENV VITE_AUTH_COOKIE_DOMAIN=${AUTH_COOKIE_DOMAIN}
ENV VITE_MAIN_SITE_URL=${MAIN_SITE_URL}
RUN npm run build

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

# install ghcup
RUN \
    if [ "$TARGETARCH" = "arm64" ]; then \
    curl https://downloads.haskell.org/~ghcup/aarch64-linux-ghcup > /usr/bin/ghcup; \
    else \
    curl https://downloads.haskell.org/~ghcup/x86_64-linux-ghcup > /usr/bin/ghcup; \
    fi;
# Don't combine
RUN chmod +x /usr/bin/ghcup && \
    ghcup config set gpg-setting GPGNone
ENV BOOTSTRAP_HASKELL_NONINTERACTIVE=1

# install GHC, cabal, and Stack
RUN \
    ghcup -v install ghc --isolate /usr/local --force ${GHC} && \
    ghcup -v install cabal --isolate /usr/local/bin --force ${CABAL} && \
    ghcup -v install stack --isolate /usr/local/bin --force ${STACK}

FROM base AS dependencies

RUN mkdir -p \
  /opt/arkham/bin \
  /opt/arkham/src/backend/arkham-api/app \
  /opt/arkham/src/backend/arkham-api/library \
  /opt/arkham/src/backend/validate/app \
  /opt/arkham/src/backend/cards-discover/app \
  /opt/arkham/src/backend/cards-discover/library \
  /opt/arkham/src/backend/ah3e

WORKDIR /opt/arkham/src/backend
COPY ./backend/stack.yaml ./backend/stack.yaml.lock /opt/arkham/src/backend/
COPY ./backend/arkham-api/package.yaml /opt/arkham/src/backend/arkham-api/package.yaml
COPY ./backend/validate/package.yaml /opt/arkham/src/backend/validate/package.yaml
COPY ./backend/cards-discover/package.yaml /opt/arkham/src/backend/cards-discover/package.yaml
COPY ./backend/ah3e/package.yaml /opt/arkham/src/backend/ah3e/package.yaml
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

# The pinned nginx runtime image already supplies every transitive dependency
# of libpq and libpcre, but not those two SONAMEs themselves. Copy their exact
# bytes from the API build environment into a private directory so nginx keeps
# its separately governed loaded-library closure.
RUN set -eu; \
    runtime_dir=/opt/arkham/api-runtime-libs; \
    mkdir -p "$runtime_dir"; \
    for soname in libpq.so.5 libpcre.so.3; do \
      source_path="$(ldconfig -p | awk -v soname="$soname" '$1 == soname { print $NF; exit }')"; \
      test -n "$source_path" && test -f "$source_path"; \
      cp -L "$source_path" "${runtime_dir}/${soname}"; \
      chmod 0644 "${runtime_dir}/${soname}"; \
    done; \
    dependencies="$(LD_LIBRARY_PATH="$runtime_dir" ldd /opt/arkham/bin/arkham-api)"; \
    printf '%s\n' "$dependencies"; \
    ! printf '%s\n' "$dependencies" | grep -F 'not found'

# The custom-card MCP server's DSL reference, generated from the Haskell that runs
# it. Generated here rather than committed: the step and expression languages are
# `KeyMap.lookup` calls, not types, so nothing reifies them and a checked-in copy
# is the copy that goes stale.
FROM ubuntu:22.04@sha256:2edbbc5dc405e9612ba3584ce95480277e3eb374407b5505fe26f17df77c7dbc AS mcp
RUN apt-get update && \
  apt-get install -y --assume-yes --no-install-recommends python3 && \
  rm -rf /var/lib/apt/lists/*
COPY ./mcp /opt/arkham/mcp
# The whole tree, because the extraction needs more than the DSL modules: every
# hand-written `instance FromJSON` (to know which fields a decoder defaults) and
# every `<X>Attrs` record (the `$bindings` a card gets for free) is somewhere in
# here. Narrowing it would mean enumerating files that move.
COPY ./backend/arkham-api/library/Arkham /src/library/Arkham
RUN ARKHAM_SOURCE_DIR=/src/library/Arkham \
      python3 /opt/arkham/mcp/arkham-cards/extract_dsl.py && \
      test -s /opt/arkham/mcp/arkham-cards/dsl.json

# The final production image supplies the nginx bytes. Pin the official
# multi-platform manifest digest so the exact nginx runtime tested below is
# the one shipped, rather than a mutable Ubuntu apt package.
FROM nginx:1.27.5@sha256:6784fb0834aa7dbbe12e3d7471e69c290df3e6ba810dc38b34ae33d3c1c05f7d AS app

# App

ENV LC_ALL=C.UTF-8
LABEL org.opencontainers.image.nginx-runtime-reference="nginx:1.27.5@sha256:6784fb0834aa7dbbe12e3d7471e69c290df3e6ba810dc38b34ae33d3c1c05f7d"

RUN apt-get update && \
  apt-get install -y --assume-yes --no-install-recommends python3 && \
  rm -rf /var/lib/apt/lists/*

RUN mkdir -p \
  /opt/arkham/bin \
  /opt/arkham/src/backend/arkham-api \
  /opt/arkham/src/frontend \
  /opt/arkham/src/frontend-3ed \
  /var/log/nginx \
  /var/lib/nginx \
  /var/cache/nginx \
  /run

COPY --from=frontend /opt/arkham/src/frontend/dist /opt/arkham/src/frontend/dist
COPY --from=frontend-3ed /opt/arkham/src/frontend-3ed/dist /opt/arkham/src/frontend-3ed/dist
COPY --from=api /opt/arkham/bin/arkham-api /opt/arkham/bin/arkham-api
COPY --from=api /opt/arkham/api-runtime-libs /opt/arkham/api-runtime-libs
COPY ./backend/arkham-api/config /opt/arkham/src/backend/arkham-api/config
COPY ./prod.nginxconf /opt/arkham/src/backend/prod.nginxconf
COPY ./start.sh /opt/arkham/src/backend/arkham-api/start.sh
COPY ./web-entrypoint.sh /web-entrypoint.sh
COPY ./backend/arkham-api/digital-ocean.crt /opt/arkham/src/backend/arkham-api/digital-ocean.crt
# The MCP server, with dsl.json as the mcp stage generated it.
COPY --from=mcp /opt/arkham/mcp /opt/arkham/mcp

ENV LD_LIBRARY_PATH=/opt/arkham/api-runtime-libs
RUN useradd -ms /bin/bash yesod && \
  chown -R yesod:yesod /opt/arkham /var/log/nginx /var/lib/nginx /var/cache/nginx /run && \
  chmod a+x /opt/arkham/src/backend/arkham-api/start.sh /web-entrypoint.sh && \
  api_dependencies="$(ldd /opt/arkham/bin/arkham-api)" && \
  printf '%s\n' "$api_dependencies" && \
  ! printf '%s\n' "$api_dependencies" | grep -F 'not found'
USER yesod
ENV PATH="$PATH:/opt/stack/bin:/opt/arkham/bin"

# 3001 serves the 3ed frontend to hosts that can't route by name (docker-compose)
EXPOSE 3000 3001

WORKDIR /opt/arkham/src/backend/arkham-api
ENTRYPOINT ["/web-entrypoint.sh"]
CMD ["./start.sh"]
