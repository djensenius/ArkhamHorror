#!/usr/bin/env bash
# shellcheck disable=SC1091,SC2034
# Proves receipt capabilities are consumed by a reviewed stage before that
# stage can launch an untrusted tool or honor BASH_ENV.

set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
REPO_ROOT="$(cd "${SCRIPT_DIR}/../.." && pwd)"
WORK="${REPO_ROOT}/offline/_tmp/test-receipt-capability-isolation-$$-${RANDOM}"
umask 077
mkdir -p "$WORK"
trap 'rm -rf "$WORK"' EXIT

source "${SCRIPT_DIR}/utils.sh"
GITHUB_ENV_FILE="${WORK}/github-env"
: > "$GITHUB_ENV_FILE"
export GITHUB_ENV="$GITHUB_ENV_FILE"
init_paths
TOOLCHAIN_RECEIPT_DIR="${WORK}/receipts"
init_toolchain_authority_receipt
RECEIPT_FILE="$TOOLCHAIN_RECEIPT_FILE"
RECEIPT_TOKEN="$TOOLCHAIN_RECEIPT_TOKEN"

PAYLOAD="${WORK}/untrusted-payload"
cat > "$PAYLOAD" <<'PAYLOAD'
#!/usr/bin/env bash
set -euo pipefail

case "$(env)" in
    *ARKHAM_TOOLCHAIN_RECEIPT_FILE=*|*ARKHAM_TOOLCHAIN_RECEIPT_TOKEN=*|*TOOLCHAIN_RECEIPT_FILE=*|*TOOLCHAIN_RECEIPT_TOKEN=*|*GITHUB_ENV=*)
        exit 91
        ;;
esac
for argument in "$@"; do
    case "$argument" in
        *receipt.tsv*|????????????????????????????????????????????????????????????????) exit 92 ;;
    esac
done
for descriptor in /dev/fd/*; do
    target="$(readlink "$descriptor" 2>/dev/null || true)"
    case "$target" in
        *receipt.tsv*) exit 93 ;;
    esac
done
if [ -r "/proc/${PPID}/environ" ]; then
    if tr '\0' '\n' < "/proc/${PPID}/environ" \
        | grep -Eq '^(ARKHAM_TOOLCHAIN_RECEIPT_FILE|ARKHAM_TOOLCHAIN_RECEIPT_TOKEN|GITHUB_ENV)='; then
        exit 94
    fi
fi
if [ -r "/proc/${PPID}/cmdline" ]; then
    while IFS= read -r argument; do
        case "$argument" in
            *receipt.tsv*|????????????????????????????????????????????????????????????????) exit 95 ;;
        esac
    done < <(tr '\0' '\n' < "/proc/${PPID}/cmdline")
fi
PAYLOAD
chmod +x "$PAYLOAD"

STAGE="${WORK}/reviewed-stage.sh"
cat > "$STAGE" <<'STAGE'
#!/usr/bin/env bash
set -euo pipefail
source "$1"
init_paths
"$2" "$3"
STAGE
chmod +x "$STAGE"

BASH_ENV_FILE="${WORK}/bash-env"
printf 'touch "%s/bash-env-ran"\n' "$WORK" > "$BASH_ENV_FILE"

BOOTSTRAP_ENV_FILE="${WORK}/bootstrap-github-env"
: > "$BOOTSTRAP_ENV_FILE"
BOOTSTRAP_SCRIPT_DIR="${WORK}/offline/scripts"
mkdir -p "$BOOTSTRAP_SCRIPT_DIR"
cp \
    "${SCRIPT_DIR}/utils.sh" \
    "${SCRIPT_DIR}/run-authorized-stage.sh" \
    "${SCRIPT_DIR}/run-initial-authorized-stage.sh" \
    "$BOOTSTRAP_SCRIPT_DIR/"
if ! GITHUB_ENV="$BOOTSTRAP_ENV_FILE" \
    BASH_ENV="$BASH_ENV_FILE" \
    /bin/sh "${BOOTSTRAP_SCRIPT_DIR}/run-authorized-stage.sh" \
        "${BOOTSTRAP_SCRIPT_DIR}/run-initial-authorized-stage.sh" \
        "$STAGE" "${BOOTSTRAP_SCRIPT_DIR}/utils.sh" "$PAYLOAD" "ordinary-argument"; then
    printf '%s\n' 'receipt-capability-isolation: initial authorized stage failed' >&2
    exit 1
fi
[ ! -e "${WORK}/bash-env-ran" ] || {
    printf '%s\n' 'receipt-capability-isolation: bootstrap honored BASH_ENV' >&2
    exit 1
}
bootstrap_receipt_file="$(
    awk -F= '$1 == "ARKHAM_TOOLCHAIN_RECEIPT_FILE" { print substr($0, index($0, "=") + 1) }' \
        "$BOOTSTRAP_ENV_FILE"
)"
bootstrap_receipt_token="$(
    awk -F= '$1 == "ARKHAM_TOOLCHAIN_RECEIPT_TOKEN" { print substr($0, index($0, "=") + 1) }' \
        "$BOOTSTRAP_ENV_FILE"
)"
case "$bootstrap_receipt_file" in
    "${WORK}"/offline/_session/receipt-*/receipt.tsv) ;;
    *) echo "receipt-capability-isolation: bootstrap receipt escaped its scratch root" >&2; exit 1 ;;
esac
[ -f "$bootstrap_receipt_file" ] \
    || { echo "receipt-capability-isolation: bootstrap receipt was not published" >&2; exit 1; }
require_receipt_token "$bootstrap_receipt_token"

if ! ARKHAM_TOOLCHAIN_RECEIPT_FILE="$RECEIPT_FILE" \
    ARKHAM_TOOLCHAIN_RECEIPT_TOKEN="$RECEIPT_TOKEN" \
    BASH_ENV="$BASH_ENV_FILE" \
    /bin/sh "${SCRIPT_DIR}/run-authorized-stage.sh" "$STAGE" "${SCRIPT_DIR}/utils.sh" "$PAYLOAD" "ordinary-argument"; then
    printf '%s\n' 'receipt-capability-isolation: untrusted payload received authority capability' >&2
    exit 1
fi

[ ! -e "${WORK}/bash-env-ran" ] || {
    printf '%s\n' 'receipt-capability-isolation: BASH_ENV was honored by authorized-stage handoff' >&2
    exit 1
}
exec 9<<EOF
${RECEIPT_FILE}
${RECEIPT_TOKEN}
EOF
if ! /usr/bin/python3 "${REPO_ROOT}/scripts/validate-catalog-serving.py" \
    --self-test --offline-authority-fd 9 >/dev/null; then
    printf '%s\n' 'receipt-capability-isolation: serving validator exposed its consumed authority frame' >&2
    exit 1
fi
publish_toolchain_authority_receipt
grep -Fx "ARKHAM_TOOLCHAIN_RECEIPT_FILE=${RECEIPT_FILE}" "$GITHUB_ENV_FILE" >/dev/null \
    || { echo "receipt-capability-isolation: completed receipt path was not published" >&2; exit 1; }
grep -Fx "ARKHAM_TOOLCHAIN_RECEIPT_TOKEN=${RECEIPT_TOKEN}" "$GITHUB_ENV_FILE" >/dev/null \
    || { echo "receipt-capability-isolation: completed receipt token was not published" >&2; exit 1; }

printf '%s\n' 'receipt-capability-isolation: receipt path/token stay out of untrusted env, args, descriptors, and ancestor procfs'
