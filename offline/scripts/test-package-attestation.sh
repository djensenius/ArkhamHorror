#!/usr/bin/env bash
# Ensures the post-package attester authenticates the final copied frontend
# tree, rather than trusting only the pre-copy frontend source tree.

set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
REPO_ROOT="$(cd "${SCRIPT_DIR}/../.." && pwd)"
WORK="${REPO_ROOT}/offline/_tmp/test-package-attestation-$$-${RANDOM}"
PACKAGE="${WORK}/ArkhamHorror-test"
GAME="${PACKAGE}/game"
umask 077
mkdir -p "${GAME}/bin" "${GAME}/lib" "${GAME}/pgsql/bin" "${GAME}/pgsql/lib" \
    "${GAME}/pgsql/share" "${GAME}/config" "${GAME}/frontend/dist/assets"
trap 'rm -rf "$WORK"' EXIT

# shellcheck disable=SC1091,SC2034
source "${SCRIPT_DIR}/utils.sh"
init_paths
# shellcheck disable=SC2034
TOOLCHAIN_RECEIPT_DIR="${WORK}/receipts"
export GITHUB_ENV=""
init_toolchain_authority_receipt

printf 'test nginx\n' > "${GAME}/bin/nginx"
chmod +x "${GAME}/bin/nginx"
printf 'test backend\n' > "${GAME}/bin/arkham-api"
chmod +x "${GAME}/bin/arkham-api"
printf 'test postgres\n' > "${GAME}/pgsql/bin/postgres"
chmod +x "${GAME}/pgsql/bin/postgres"
printf 'timezone data\n' > "${GAME}/pgsql/share/timezonesets"
printf 'versioned postgres library\n' > "${GAME}/pgsql/lib/libpq.so.5.14"
ln -s libpq.so.5.14 "${GAME}/pgsql/lib/libpq.so.5"
ln -s libpq.so.5.14 "${GAME}/pgsql/lib/libpq.so"
printf '#!/usr/bin/env bash\n' > "${GAME}/start.sh"
chmod +x "${GAME}/start.sh"
printf '#!/usr/bin/env bash\n' > "${GAME}/update.sh"
chmod +x "${GAME}/update.sh"
printf '#!/usr/bin/env bash\n' > "${PACKAGE}/Update-ArkhamHorror.sh"
chmod +x "${PACKAGE}/Update-ArkhamHorror.sh"
printf 'types { application/json json; }\n' > "${GAME}/config/mime.types"
printf 'self-issued provenance\n' > "${GAME}/config/toolchain-provenance.env"
cp "${REPO_ROOT}/offline/toolchain.lock" "${GAME}/config/toolchain.lock"
printf '<!doctype html>\n' > "${GAME}/frontend/dist/index.html"
printf 'trusted bundle\n' > "${GAME}/frontend/dist/assets/app.js"

frontend_identity="$(printf '%s' 'test-frontend-inputs' | sha256_text)"
frontend_closure="$(authority_tree_digest "${GAME}/frontend/dist")"
record_authority_receipt frontend "$frontend_identity" "$frontend_closure"

run_attester() {
    ARKHAM_TOOLCHAIN_RECEIPT_FILE="$TOOLCHAIN_RECEIPT_FILE" \
    ARKHAM_TOOLCHAIN_RECEIPT_TOKEN="$TOOLCHAIN_RECEIPT_TOKEN" \
        /bin/sh "${SCRIPT_DIR}/run-authorized-stage.sh" \
            "${SCRIPT_DIR}/attest-package-closure.sh" "$@" "$PACKAGE"
}

# PostgreSQL's conventional versioned aliases are materialized before release
# attestation/tar, so updater preflight and archive representation agree.
/usr/bin/python3 "${SCRIPT_DIR}/materialize-package-tree.py" "$PACKAGE"
if find "$PACKAGE" -type l -print -quit | grep -q .; then
    printf '%s\n' 'package-attestation: package materializer retained a symlink' >&2
    exit 1
fi

# Exercise the generated launcher's runtime-library policy directly. Release
# validation must preserve materialized regular files, while ordinary startup
# keeps the extraction-repair behavior expected by installed packages.
RUNTIME_PROBE="${WORK}/runtime-probe"
mkdir -p "${RUNTIME_PROBE}/lib" "${RUNTIME_PROBE}/pgsql/lib"
for libdir in "${RUNTIME_PROBE}/lib" "${RUNTIME_PROBE}/pgsql/lib"; do
    printf 'versioned runtime library\n' > "${libdir}/libprobe.so.5.14"
    cp "${libdir}/libprobe.so.5.14" "${libdir}/libprobe.so.5"
    cp "${libdir}/libprobe.so.5.14" "${libdir}/libprobe.so"
done
LAUNCHER_FUNCTIONS="${WORK}/launcher-functions.sh"
sed -n '/^configure_runtime_env() {$/,/^}$/p' \
    "${SCRIPT_DIR}/05-package.sh" > "$LAUNCHER_FUNCTIONS"
sed -n '/^_fix_lib_symlinks() {$/,/^}$/p' \
    "${SCRIPT_DIR}/05-package.sh" >> "$LAUNCHER_FUNCTIONS"
(
    # shellcheck disable=SC1090,SC2317
    source "$LAUNCHER_FUNCTIONS"
    SCRIPT_DIR="$RUNTIME_PROBE"
    # Invoked by the sourced configure_runtime_env function.
    # shellcheck disable=SC2329
    pg_bin() { printf '%s\n' "${RUNTIME_PROBE}/pgsql/bin"; }
    ACTION="validate-nginx-config"
    configure_runtime_env
    if find "$RUNTIME_PROBE" -type l -print -quit | grep -q .; then
        printf '%s\n' 'package-attestation: validation mutated materialized libraries' >&2
        exit 1
    fi
    ACTION="serve-nginx-for-validation"
    configure_runtime_env
    if find "$RUNTIME_PROBE" -type l -print -quit | grep -q .; then
        printf '%s\n' 'package-attestation: serving validation mutated materialized libraries' >&2
        exit 1
    fi
    # Read by the sourced configure_runtime_env function.
    # shellcheck disable=SC2034
    ACTION="start"
    configure_runtime_env
    if [ ! -L "${RUNTIME_PROBE}/pgsql/lib/libprobe.so.5" ] \
        || [ ! -L "${RUNTIME_PROBE}/lib/libprobe.so" ]; then
        printf '%s\n' 'package-attestation: normal startup no longer repairs runtime library aliases' >&2
        exit 1
    fi
)

python3 - "$PACKAGE" <<'PY'
import io
import sys
import tarfile
from pathlib import Path

package = Path(sys.argv[1])
archive = io.BytesIO()
with tarfile.open(fileobj=archive, mode="w:gz") as output:
    output.add(package, arcname=".")
archive.seek(0)
with tarfile.open(fileobj=archive, mode="r:gz") as output:
    if any(member.issym() or member.islnk() for member in output):
        raise SystemExit("package tar representation retained a link member")
PY

run_attester
run_attester --final

for omitted in \
    "game/bin/arkham-api" \
    "game/pgsql/bin/postgres" \
    "game/pgsql/share/timezonesets" \
    "game/update.sh" \
    "Update-ArkhamHorror.sh" \
    "game/config/nginx.conf"; do
    printf 'post-attestation mutation\n' > "${PACKAGE}/${omitted}"
    if run_attester --final >/dev/null 2>&1; then
        printf '%s\n' "package-attestation: final closure accepted mutation of ${omitted}" >&2
        exit 1
    fi
    rm -f "${PACKAGE:?}/${omitted}"
done

printf 'substituted final package bundle\n' > "${GAME}/frontend/dist/assets/app.js"
if run_attester >/dev/null 2>&1; then
    printf '%s\n' 'package-attestation: final package frontend substitution was accepted' >&2
    exit 1
fi

printf '%s\n' 'package-attestation: final copied frontend substitution is rejected by external attestation'
