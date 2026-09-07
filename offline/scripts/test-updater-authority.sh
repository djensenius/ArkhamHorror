#!/usr/bin/env bash
# Adversarial regression suite for the shipped authenticated updater.

set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
REPO_ROOT="$(cd "${SCRIPT_DIR}/../.." && pwd)"
UPDATER="${SCRIPT_DIR}/update-runtime.sh"
WORK="${REPO_ROOT}/offline/_tmp/test-updater-authority-$$-${RANDOM}"
umask 077
mkdir -p "$WORK"
TEST_PIDS=()
cleanup() {
    local pid
    for pid in "${TEST_PIDS[@]}"; do
        kill -KILL "$pid" 2>/dev/null || true
        wait "$pid" 2>/dev/null || true
    done
    rm -rf "$WORK"
}
trap cleanup EXIT

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

release_launchers() {
    case "$PLATFORM" in
        linux-*)
            printf '%s\n' \
                "Start-ArkhamHorror.bat" \
                "Update-ArkhamHorror.bat" \
                "Update-ArkhamHorror.sh"
            ;;
        macos-*)
            printf '%s\n' \
                "Start-ArkhamHorror.command" \
                "Update-ArkhamHorror.command" \
                "Update-ArkhamHorror.sh"
            ;;
    esac
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
    local launcher node
    node="$(command -v node)"
    [ -n "$node" ] && [ -x "$node" ] \
        || { echo "updater-authority: Node.js is required for the updater fixture" >&2; exit 1; }
    mkdir -p \
        "${base}/game/config" \
        "${base}/game/tools" \
        "${base}/backup" \
        "${base}/cards" \
        "${base}/cards_en"
    printf '%s\n' "$PLATFORM" > "${base}/game/config/release-platform"
    printf '%s\n' "$version" > "${base}/game/config/release-version"
    printf '#!/usr/bin/env bash\nexit 0\n' > "${base}/game/start.sh"
    printf '#!/usr/bin/env bash\nexit 0\n' > "${base}/game/update.sh"
    chmod +x "${base}/game/start.sh"
    chmod +x "${base}/game/update.sh"
    cp "$node" "${base}/game/tools/node"
    chmod +x "${base}/game/tools/node"
    cp "${SCRIPT_DIR}/update-archive.mjs" "${base}/game/tools/update-archive.mjs"
    printf 'old game\n' > "${base}/game/old.txt"
    if [ "$version" != "dev" ]; then
        touch "${base}/game/current_${version}"
    fi
    printf 'preserve backup\n' > "${base}/backup/user.dump"
    printf 'preserve card\n' > "${base}/cards/user-card.png"
    printf 'preserve English card\n' > "${base}/cards_en/user-card.png"
    while IFS= read -r launcher; do
        printf 'old root launcher: %s\n' "$launcher" > "${base}/${launcher}"
    done < <(release_launchers)
}

make_archive() {
    local base="$1" version="$2" mode="$3"
    local archive="${base}/ArkhamHorror-${PLATFORM}-${version}.tar.gz"
    python3 - "$archive" "$PLATFORM" "$version" "$mode" <<'PY'
import io
import sys
import tarfile

archive, platform, version, mode = sys.argv[1:]
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
    add_dir("backup")
    add_dir("cards")
    add_dir("cards_en")
    add_dir("game")
    add_dir("game/config")
    add_dir("game/tools")
    add_file("game/config/release-platform", platform + "\n")
    embedded_version = "v20991231.1" if mode == "version-mismatch" else version
    if mode != "missing-version":
        add_file("game/config/release-version", embedded_version + "\n")
        add_file(f"game/current_{embedded_version}", "")
    add_file("game/start.sh", "#!/usr/bin/env bash\nexit 0\n", 0o644 if mode == "nonexecutable" else 0o755)
    add_file("game/update.sh", "#!/usr/bin/env bash\nexit 0\n", 0o755)
    add_file("game/tools/node", "#!/usr/bin/env bash\nexit 1\n", 0o755)
    add_file("game/tools/update-archive.mjs", "export default null;\n")
    add_file("game/new.txt", "new game\n")
    root_launchers = (
        ["Start-ArkhamHorror.bat", "Update-ArkhamHorror.bat", "Update-ArkhamHorror.sh"]
        if platform.startswith("linux-")
        else ["Start-ArkhamHorror.command", "Update-ArkhamHorror.command", "Update-ArkhamHorror.sh"]
    )
    for launcher in root_launchers:
        permissions = 0o755 if launcher.endswith((".sh", ".command")) else 0o644
        add_file(launcher, f"new root launcher: {launcher}\n", permissions)
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
    elif mode == "postgres-alias":
        add_dir("game/pgsql")
        add_dir("game/pgsql/lib")
        add_file("game/pgsql/lib/libpq.so.5.14", "versioned PostgreSQL library\n")
        add_file("game/pgsql/lib/libpq.so.5", "versioned PostgreSQL library\n")
        add_file("game/pgsql/lib/libpq.so", "versioned PostgreSQL library\n")
    elif mode == "unexpected-root":
        add_file("release-owned-but-unknown", "bad")
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
    HOME="${base}/home" "$UPDATER" "$base" "" "$digest"
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
[ "$(cat "${VALID_BASE}/game/config/release-version")" = "v20260902.1" ] \
    || fail "valid update did not install the authenticated release version"
while IFS= read -r launcher; do
    grep -Fq "new root launcher: ${launcher}" "${VALID_BASE}/${launcher}" \
        || fail "valid update left stale release-owned root launcher ${launcher}"
done < <(release_launchers)
[ "$(cat "${VALID_BASE}/backup/user.dump")" = "preserve backup" ] \
    || fail "valid update changed the user-owned backup directory"
[ "$(cat "${VALID_BASE}/cards/user-card.png")" = "preserve card" ] \
    || fail "valid update changed the user-owned card directory"
[ "$(cat "${VALID_BASE}/cards_en/user-card.png")" = "preserve English card" ] \
    || fail "valid update changed the user-owned English card directory"

# Model the PostgreSQL library layout produced on Linux: versioned objects with
# SONAME/development aliases. The archive uses the packager's canonical
# materialized regular-file representation.
POSTGRES_LINK_BASE="${WORK}/postgres-versioned-links"
make_base "$POSTGRES_LINK_BASE"
make_archive "$POSTGRES_LINK_BASE" v20260902.1 postgres-alias
run_updater "$POSTGRES_LINK_BASE" "$(published_digest_for "$POSTGRES_LINK_BASE")" >/dev/null \
    || fail "updater rejected a materialized PostgreSQL-style Linux package"
[ -f "${POSTGRES_LINK_BASE}/game/pgsql/lib/libpq.so" ] \
    || fail "updater did not install PostgreSQL-style library aliases"

PROCESS_BASE="${WORK}/running-processes"
make_base "$PROCESS_BASE"
mkdir -p \
    "${PROCESS_BASE}/game/bin" \
    "${PROCESS_BASE}/game/data" \
    "${PROCESS_BASE}/game/pgsql/bin"
case "$PLATFORM" in
    macos-*) PROCESS_PGDATA="${PROCESS_BASE}/home/Library/Application Support/ArkhamHorror/pgdata" ;;
    linux-*) PROCESS_PGDATA="${PROCESS_BASE}/home/.local/share/ArkhamHorror/pgdata" ;;
esac
mkdir -p "$PROCESS_PGDATA"
PROCESS_FIXTURE="${WORK}/owned-process"
cat > "${WORK}/owned-process.c" <<'EOF'
#include <signal.h>
#include <unistd.h>

int main(int argc, char **argv) {
    (void)argv;
    if (argc > 1) {
        signal(SIGTERM, SIG_IGN);
    }
    for (;;) {
        pause();
    }
}
EOF
cc -O2 -o "$PROCESS_FIXTURE" "${WORK}/owned-process.c"
cp "$PROCESS_FIXTURE" "${PROCESS_BASE}/game/bin/nginx"
cp "$PROCESS_FIXTURE" "${PROCESS_BASE}/game/bin/arkham-api"
cp "$PROCESS_FIXTURE" "${PROCESS_BASE}/game/pgsql/bin/postgres"
"${PROCESS_BASE}/game/bin/nginx" &
NGINX_TEST_PID=$!
TEST_PIDS+=("$NGINX_TEST_PID")
"${PROCESS_BASE}/game/bin/arkham-api" ignore-term &
API_TEST_PID=$!
TEST_PIDS+=("$API_TEST_PID")
"${PROCESS_BASE}/game/pgsql/bin/postgres" &
POSTGRES_TEST_PID=$!
TEST_PIDS+=("$POSTGRES_TEST_PID")
printf '%s\n' "$NGINX_TEST_PID" > "${PROCESS_BASE}/game/data/nginx.pid"
printf '%s\n' "$API_TEST_PID" > "${PROCESS_BASE}/game/data/arkham-api.pid"
cat > "${PROCESS_PGDATA}/postmaster.pid" <<EOF
${POSTGRES_TEST_PID}
${PROCESS_PGDATA}
1700000000
5433
/tmp
localhost
EOF
make_archive "$PROCESS_BASE" v20260902.1 valid
PROCESS_LOG="${WORK}/running-processes.log"
run_updater "$PROCESS_BASE" "$(published_digest_for "$PROCESS_BASE")" >"$PROCESS_LOG" 2>&1 \
    || fail "updater rejected an otherwise valid package with owned services running"
if ! grep -Fq "arkham-api process ${API_TEST_PID} did not stop after SIGTERM; sending SIGKILL" "$PROCESS_LOG"; then
    cat "$PROCESS_LOG" >&2
    fail "updater did not escalate a stubborn owned process before replacement"
fi
for pid in "$NGINX_TEST_PID" "$API_TEST_PID" "$POSTGRES_TEST_PID"; do
    state="$(ps -o stat= -p "$pid" 2>/dev/null | tr -d '[:space:]' || true)"
    case "$state" in
        ""|Z*) ;;
        *) fail "updater replaced the package while owned process ${pid} was still running" ;;
    esac
    wait "$pid" 2>/dev/null || true
done
[ -f "${PROCESS_BASE}/game/new.txt" ] \
    || fail "updater did not install the new package after stopping every owned service"

DEV_BASE="${WORK}/dev"
make_base "$DEV_BASE" dev
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

for attack in version-mismatch missing-version unexpected-root; do
    base="${WORK}/${attack}"
    make_base "$base"
    make_archive "$base" v20260902.1 "$attack"
    expect_reject "archive ${attack}" "$base" "$(published_digest_for "$base")"
done

RENAMED_RELEASE_BASE="${WORK}/renamed-release"
make_base "$RENAMED_RELEASE_BASE"
make_archive "$RENAMED_RELEASE_BASE" v20260901.2 valid
RENAMED_RELEASE_ARCHIVE="$(release_archive "$RENAMED_RELEASE_BASE")"
mv "$RENAMED_RELEASE_ARCHIVE" \
    "${RENAMED_RELEASE_BASE}/ArkhamHorror-${PLATFORM}-v20991231.1.tar.gz"
expect_reject "renamed authenticated older release" "$RENAMED_RELEASE_BASE" \
    "$(sha256_file "${RENAMED_RELEASE_BASE}/ArkhamHorror-${PLATFORM}-v20991231.1.tar.gz")"

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

POISONED_PYTHON_BASE="${WORK}/poisoned-python"
make_base "$POISONED_PYTHON_BASE"
make_archive "$POISONED_PYTHON_BASE" v20260902.1 valid
mkdir -p "${POISONED_PYTHON_BASE}/poison-bin"
cat > "${POISONED_PYTHON_BASE}/poison-bin/python3" <<EOF
#!/usr/bin/env bash
touch "${POISONED_PYTHON_BASE}/ambient-python-ran"
exit 99
EOF
chmod +x "${POISONED_PYTHON_BASE}/poison-bin/python3"
PATH="${POISONED_PYTHON_BASE}/poison-bin:${PATH}" \
    run_updater "$POISONED_PYTHON_BASE" "$(published_digest_for "$POISONED_PYTHON_BASE")" >/dev/null \
    || fail "updater failed when ambient python3 was poisoned"
[ ! -e "${POISONED_PYTHON_BASE}/ambient-python-ran" ] \
    || fail "updater executed ambient python3"

MISSING_NODE_BASE="${WORK}/missing-bundled-node"
make_base "$MISSING_NODE_BASE"
make_archive "$MISSING_NODE_BASE" v20260902.1 valid
rm -f "${MISSING_NODE_BASE}/game/tools/node"
expect_reject "missing bundled updater Node runtime" "$MISSING_NODE_BASE" \
    "$(published_digest_for "$MISSING_NODE_BASE")"

MISSING_HELPER_BASE="${WORK}/missing-bundled-helper"
make_base "$MISSING_HELPER_BASE"
make_archive "$MISSING_HELPER_BASE" v20260902.1 valid
rm -f "${MISSING_HELPER_BASE}/game/tools/update-archive.mjs"
expect_reject "missing bundled updater archive helper" "$MISSING_HELPER_BASE" \
    "$(published_digest_for "$MISSING_HELPER_BASE")"

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
printf '%s\n' 'updater-authority: authenticated versions, full release layout, self-contained extraction, and rollback protections passed'
