#!/bin/sh
set -e

# Auto-detect ASSET_HOST if not explicitly set.
# If the mounted image directory has content, serve images locally.
# Otherwise fall back to the CloudFront CDN.
# Override by setting ASSET_HOST explicitly in docker-compose.yml:
#   ASSET_HOST=                                  # force local
#   ASSET_HOST=https://assets.arkhamhorror.app   # force CDN
if [ -z "${ASSET_HOST+x}" ]; then
  img_dir="/opt/arkham/src/frontend/dist/img"
  if [ -n "$(ls -A "$img_dir" 2>/dev/null)" ]; then
    export ASSET_HOST=""
  else
    export ASSET_HOST="https://assets.arkhamhorror.app"
  fi
fi

# Auto-configure the optional native-client locale catalog pointer from the
# catalog built into the production frontend. If an operator sets any of the
# ARKHAM_LOCALE_CATALOG_* variables, leave the whole group alone so the
# backend's existing all-or-nothing validation remains authoritative.
if [ -z "${ARKHAM_LOCALE_CATALOG_MANIFEST_URL+x}" ] && \
   [ -z "${ARKHAM_LOCALE_CATALOG_REVISION+x}" ] && \
   [ -z "${ARKHAM_LOCALE_CATALOG_SCHEMA_VERSION+x}" ] && \
   [ -z "${ARKHAM_LOCALE_CATALOG_DEFAULT_LOCALE+x}" ] && \
   [ -z "${ARKHAM_LOCALE_CATALOG_LOCALES+x}" ] && \
   [ -z "${ARKHAM_LOCALE_CATALOG_MANIFEST_SHA256+x}" ]; then
  catalog_manifest="/opt/arkham/src/frontend/dist/locale-catalog/manifest.json"
  if [ -f "$catalog_manifest" ]; then
    catalog_env="/tmp/arkham-locale-catalog.env"
    python3 - "$catalog_manifest" > "$catalog_env" <<'PY'
import hashlib
import json
import shlex
import sys

manifest_path = sys.argv[1]
with open(manifest_path, "rb") as handle:
    manifest_bytes = handle.read()
manifest = json.loads(manifest_bytes)
settings = {
    "ARKHAM_LOCALE_CATALOG_MANIFEST_URL": manifest["manifestPath"],
    "ARKHAM_LOCALE_CATALOG_REVISION": manifest["catalogRevision"],
    "ARKHAM_LOCALE_CATALOG_SCHEMA_VERSION": manifest["schemaVersion"],
    "ARKHAM_LOCALE_CATALOG_DEFAULT_LOCALE": manifest["defaultLocale"],
    "ARKHAM_LOCALE_CATALOG_LOCALES": ",".join(record["locale"] for record in manifest["locales"]),
    "ARKHAM_LOCALE_CATALOG_MANIFEST_SHA256": hashlib.sha256(manifest_bytes).hexdigest(),
}
for name, value in settings.items():
    print(f"export {name}={shlex.quote(value)}")
PY
    . "$catalog_env"
    rm -f "$catalog_env"
  fi
fi

# If the app already got a full DATABASE_URL, leave it alone.
if [ -z "${DATABASE_URL}" ]; then
  # Prefer env-injected password first (good for Kamal), then Docker secret file.
  if [ -n "${PGPASSWORD}" ]; then
    DB_PASSWORD="${PGPASSWORD}"
  elif [ -n "${POSTGRES_PASSWORD}" ]; then
    DB_PASSWORD="${POSTGRES_PASSWORD}"
  elif [ -f /run/secrets/postgres_password ]; then
    DB_PASSWORD=$(tr -d '\n\r' < /run/secrets/postgres_password)
  else
    echo "No database password found. Set DATABASE_URL, PGPASSWORD, POSTGRES_PASSWORD, or mount /run/secrets/postgres_password." >&2
    exit 1
  fi

  export PGUSER="${PGUSER:-arkham_pg_user}"
  export PGHOST="${PGHOST:-db}"
  export PGPORT="${PGPORT:-5432}"
  export PGDATABASE="${PGDATABASE:-arkham-horror-backend}"
  export PGPASSWORD="${DB_PASSWORD}"

  export DATABASE_URL="postgres://${PGUSER}:${PGPASSWORD}@${PGHOST}:${PGPORT}/${PGDATABASE}"

  unset DB_PASSWORD
fi

exec "$@"
