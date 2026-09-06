#!/usr/bin/env bash
# Adversarial regression suite for the shipped authenticated updater.

set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
REPO_ROOT="$(cd "${SCRIPT_DIR}/../.." && pwd)"
UPDATER="${SCRIPT_DIR}/update-runtime.sh"
WORK="${REPO_ROOT}/offline/_tmp/test-updater-authority-$$-${RANDOM}"
umask 077
mkdir -p "$WORK"
trap 'rm -rf "$WORK"' EXIT

case "$(uname -s)-$(uname -m)" in
    Darwin-arm64|Darwin-aarch64) PLATFORM="macos-arm64" ;;
    Darwin-x86_64|Darwin-amd64) PLATFORM="macos-x86_64" ;;
    Linux-arm64|Linux-aarch64) PLATFORM="linux-arm64" ;;
    Linux-x86_64|Linux-amd64) PLATFORM="linux-x86_64" ;;
    *) echo "updater-authority: unsupported host" >&2; exit 1 ;;
esac

failures=0
fail() {
    printf 'updater-authority: %s\n' "$*" >&2
    failures=$((failures + 1))
}

sha256_file() {
    if command -v sha256sum >/dev/null 2>&1; then
        sha256sum "$1" | awk '{print $1}'
    else
        shasum -a 256 "$1" | awk '{print $1}'
    fi
}

make_base() {
    local base="$1" version="${2:-v20260901.1}"
    mkdir -p "${base}/game/config"
    printf '%s\n' "$PLATFORM" > "${base}/game/config/release-platform"
    printf '#!/usr/bin/env bash\nexit 0\n' > "${base}/game/start.sh"
    chmod +x "${base}/game/start.sh"
    printf 'old game\n' > "${base}/game/old.txt"
    touch "${base}/game/current_${version}"
}

make_archive() {
    local base="$1" version="$2" mode="$3"
    local archive="${base}/ArkhamHorror-${PLATFORM}-${version}.tar.gz"
    python3 - "$archive" "$PLATFORM" "$mode" <<'PY'
import io
import sys
import tarfile

archive, platform, mode = sys.argv[1:]
with tarfile.open(archive, "w:gz") as tar:
    def add_dir(name, permissions=0o755):
        entry = tarfile.TarInfo(name)
        entry.type = tarfile.DIRTYPE
        entry.mode = permissions
        tar.addfile(entry)

    def add_file(name, data, permissions=0o644):
        entry = tarfile.TarInfo(name)
        entry.type = tarfile.REGTYPE
        entry.mode = permissions
        encoded = data.encode("utf-8")
        entry.size = len(encoded)
        tar.addfile(entry, io.BytesIO(encoded))

    add_dir(".")
    add_dir("game")
    add_dir("game/config")
    add_file("game/config/release-platform", platform + "\n")
    add_file("game/start.sh", "#!/usr/bin/env bash\nexit 0\n", 0o644 if mode == "nonexecutable" else 0o755)
    add_file("game/new.txt", "new game\n")
    if mode == "traversal":
        add_file("../escape", "bad")
        add_file("game/../also-escape", "bad")
    elif mode == "symlink":
        entry = tarfile.TarInfo("game/evil-link")
        entry.type = tarfile.SYMTYPE
        entry.linkname = "../outside"
        tar.addfile(entry)
    elif mode == "hardlink":
        entry = tarfile.TarInfo("game/evil-hardlink")
        entry.type = tarfile.LNKTYPE
        entry.linkname = "game/start.sh"
        tar.addfile(entry)
    elif mode == "fifo":
        entry = tarfile.TarInfo("game/evil-fifo")
        entry.type = tarfile.FIFOTYPE
        tar.addfile(entry)
    elif mode == "device":
        entry = tarfile.TarInfo("game/evil-device")
        entry.type = tarfile.CHRTYPE
        entry.devmajor = 1
        entry.devminor = 3
        tar.addfile(entry)
    elif mode == "duplicate":
        add_file("game/start.sh", "#!/usr/bin/env bash\nexit 0\n", 0o755)
    elif mode == "collision":
        add_file("game/collision", "file")
        add_file("game/collision/child", "bad")
    elif mode == "member-limit":
        for index in range(60001):
            add_file(f"game/members/{index}", "")
PY
    printf '%s  %s\n' "$(sha256_file "$archive")" "$(basename "$archive")" > "${archive}.sha256"
}

release_archive() {
    find "$1" -maxdepth 1 -type f \
        -name "ArkhamHorror-${PLATFORM}-v*.tar.gz" -print \
        | LC_ALL=C sort \
        | head -1
}

published_digest_for() {
    local archive
    archive="$(release_archive "$1")"
    [ -n "$archive" ] || return 1
    sha256_file "$archive"
}

run_updater() {
    local base="$1" digest="$2"
    "$UPDATER" "$base" "" "$digest"
}

expect_reject() {
    local label="$1" base="$2" digest="$3"
    if run_updater "$base" "$digest" >/dev/null 2>&1; then
        fail "${label} was accepted"
    fi
    [ -f "${base}/game/old.txt" ] || fail "${label} changed the installed game before preflight completed"
}

VALID_BASE="${WORK}/valid"
make_base "$VALID_BASE"
make_archive "$VALID_BASE" v20260902.1 valid
run_updater "$VALID_BASE" "$(published_digest_for "$VALID_BASE")" >/dev/null
[ -f "${VALID_BASE}/game/new.txt" ] || fail "valid same-platform archive was not installed"
[ ! -e "${VALID_BASE}/game/old.txt" ] || fail "valid update left the old game in place"
[ -d "${VALID_BASE}/game_v20260901.1" ] || fail "valid update did not preserve the old game"
[ -f "${VALID_BASE}/game/current_v20260902.1" ] || fail "valid update did not write the new marker"

DEV_BASE="${WORK}/dev"
make_base "$DEV_BASE"
rm -f "${DEV_BASE}"/game/current_v*
make_archive "$DEV_BASE" v20260902.1 valid
run_updater "$DEV_BASE" "$(published_digest_for "$DEV_BASE")" >/dev/null
[ -d "${DEV_BASE}/game_v20260902.0" ] \
    || fail "first update did not preserve the dev game under the expected backup name"

WRONG_PLATFORM_BASE="${WORK}/wrong-platform"
make_base "$WRONG_PLATFORM_BASE"
python3 - "$WRONG_PLATFORM_BASE/ArkhamHorror-linux-x86_64-v20260902.1.tar.gz" <<'PY'
import io, sys, tarfile
with tarfile.open(sys.argv[1], "w:gz") as tar:
    entry = tarfile.TarInfo("game")
    entry.type = tarfile.DIRTYPE
    tar.addfile(entry)
PY
expect_reject "wrong-platform archive" "$WRONG_PLATFORM_BASE" "$(printf '%064d' 0)"

case "$PLATFORM" in
    macos-arm64) OTHER_PLATFORM="linux-x86_64" ;;
    macos-x86_64) OTHER_PLATFORM="linux-arm64" ;;
    linux-arm64) OTHER_PLATFORM="macos-x86_64" ;;
    linux-x86_64) OTHER_PLATFORM="macos-arm64" ;;
esac
MIXED_PLATFORM_BASE="${WORK}/mixed-platform"
make_base "$MIXED_PLATFORM_BASE"
make_archive "$MIXED_PLATFORM_BASE" v20260902.1 valid
cp "$(release_archive "$MIXED_PLATFORM_BASE")" \
    "${MIXED_PLATFORM_BASE}/ArkhamHorror-${OTHER_PLATFORM}-v20991231.1.tar.gz"
run_updater "$MIXED_PLATFORM_BASE" "$(published_digest_for "$MIXED_PLATFORM_BASE")" >/dev/null
[ -f "${MIXED_PLATFORM_BASE}/game/current_v20260902.1" ] \
    || fail "updater selected a newer archive from another platform"

CHECKSUM_BASE="${WORK}/checksum"
make_base "$CHECKSUM_BASE"
make_archive "$CHECKSUM_BASE" v20260902.1 valid
printf '%064d  ArkhamHorror-%s-v20260902.1.tar.gz\n' 0 "$PLATFORM" \
    > "${CHECKSUM_BASE}/ArkhamHorror-${PLATFORM}-v20260902.1.tar.gz.sha256"
expect_reject "published checksum mismatch" "$CHECKSUM_BASE" "$(printf '%064d' 0)"

for attack in traversal symlink hardlink fifo device duplicate collision member-limit nonexecutable; do
    base="${WORK}/${attack}"
    make_base "$base"
    make_archive "$base" v20260902.1 "$attack"
    expect_reject "archive ${attack}" "$base" "$(published_digest_for "$base")"
done

DOWNGRADE_BASE="${WORK}/downgrade"
make_base "$DOWNGRADE_BASE" v20260901.10
make_archive "$DOWNGRADE_BASE" v20260901.2 valid
expect_reject "numeric release downgrade" "$DOWNGRADE_BASE" "$(published_digest_for "$DOWNGRADE_BASE")"

DUPLICATE_BASE="${WORK}/duplicate-version"
make_base "$DUPLICATE_BASE"
make_archive "$DUPLICATE_BASE" v20260902.1 valid
cp "${DUPLICATE_BASE}/ArkhamHorror-${PLATFORM}-v20260902.1.tar.gz" \
    "${DUPLICATE_BASE}/ArkhamHorror-${PLATFORM}-v20260902.01.tar.gz"
cp "${DUPLICATE_BASE}/ArkhamHorror-${PLATFORM}-v20260902.1.tar.gz.sha256" \
    "${DUPLICATE_BASE}/ArkhamHorror-${PLATFORM}-v20260902.01.tar.gz.sha256"
expect_reject "ambiguous normalized release version" "$DUPLICATE_BASE" "$(published_digest_for "$DUPLICATE_BASE")"

BACKUP_SYMLINK_BASE="${WORK}/backup-symlink"
make_base "$BACKUP_SYMLINK_BASE"
make_archive "$BACKUP_SYMLINK_BASE" v20260902.1 valid
ln -s "${WORK}/outside" "${BACKUP_SYMLINK_BASE}/game_v20260901.1"
run_updater "$BACKUP_SYMLINK_BASE" "$(published_digest_for "$BACKUP_SYMLINK_BASE")" >/dev/null
[ -L "${BACKUP_SYMLINK_BASE}/game_v20260901.1" ] \
    || fail "updater replaced a pre-existing backup symlink"

# A predictable fixed work name must never be used or executed.
TEMP_BASE="${WORK}/temp-substitution"
make_base "$TEMP_BASE"
make_archive "$TEMP_BASE" v20260902.1 valid
mkdir -p "${TEMP_BASE}/.update-work/update-fixed"
printf '#!/usr/bin/env bash\ntouch "%s/hostile-ran"\n' "$TEMP_BASE" \
    > "${TEMP_BASE}/.update-work/update-fixed/update.sh"
run_updater "$TEMP_BASE" "$(published_digest_for "$TEMP_BASE")" >/dev/null
[ ! -e "${TEMP_BASE}/hostile-ran" ] || fail "predictable updater work path was executed"

MISSING_AUTHORITY_BASE="${WORK}/missing-published-checksum"
make_base "$MISSING_AUTHORITY_BASE"
make_archive "$MISSING_AUTHORITY_BASE" v20260902.1 valid
if "$UPDATER" "$MISSING_AUTHORITY_BASE" >/dev/null 2>&1; then
    fail "updater accepted an archive without a separately supplied published SHA-256"
fi

SIDECAR_SUBSTITUTION_BASE="${WORK}/sidecar-substitution"
make_base "$SIDECAR_SUBSTITUTION_BASE"
make_archive "$SIDECAR_SUBSTITUTION_BASE" v20260902.1 valid
SIDECAR_ARCHIVE="$(release_archive "$SIDECAR_SUBSTITUTION_BASE")"
PUBLISHED_DIGEST="$(sha256_file "$SIDECAR_ARCHIVE")"
printf 'substituted archive bytes' > "$SIDECAR_ARCHIVE"
printf '%s  %s\n' "$(sha256_file "$SIDECAR_ARCHIVE")" "$(basename "$SIDECAR_ARCHIVE")" \
    > "${SIDECAR_ARCHIVE}.sha256"
expect_reject "archive plus colocated checksum substitution" \
    "$SIDECAR_SUBSTITUTION_BASE" "$PUBLISHED_DIGEST"

# The top-level generated launcher prompts for an externally published digest
# and forwards it verbatim only after validating its exact format.
grep -Fq 'Paste the published SHA-256 for this release archive' "${SCRIPT_DIR}/05-package.sh" \
    || fail "generated updater launcher does not require an external checksum"
# shellcheck disable=SC2016
grep -Fq 'bash "${WORK_DIR}/update.sh" "$BASE_DIR" "$WORK_DIR" "$EXPECTED_ARCHIVE_SHA256"' \
    "${SCRIPT_DIR}/05-package.sh" \
    || fail "generated updater launcher does not forward the verified checksum"
if grep -Eq '/tmp/arkham-update|mktemp' "$UPDATER"; then
    fail "shipped updater still uses a predictable system temporary path"
fi

if [ "$failures" -ne 0 ]; then
    printf 'updater-authority: %s failure(s)\n' "$failures" >&2
    exit 1
fi
printf '%s\n' 'updater-authority: external checksums, platform selection, member types, collisions, and owned work protections passed'
