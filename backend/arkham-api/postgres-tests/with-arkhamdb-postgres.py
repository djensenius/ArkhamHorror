#!/usr/bin/env python3
r"""Run a bounded command against an owned, disposable PostgreSQL cluster.

Local/CI launcher checks (no Haskell build):
  export ARKHAM_TEST_LOG_DIR=/short/owned/log/directory
  # macOS: avoid the client-only libpq bin/ prepended by Haskell build flags.
  export ARKHAM_TEST_PG_BIN=/opt/homebrew/opt/postgresql@14/bin
  # CI: set ARKHAM_TEST_PG_BIN to the installed PostgreSQL server bin/ instead.
  python3 -B -m unittest discover -s backend/arkham-api/postgres-tests \
    -p 'test_arkhamdb_postgres.py' -v

Standalone Haskell connection/barrier checks, using cached Stack dependencies:
  cd backend
  MISE_ENV=haskell mise exec -- stack exec --system-ghc -- ghc -Wall -threaded -XGHC2021 \
    -XBlockArguments -XOverloadedRecordDot -XRecordWildCards \
    -iarkham-api/postgres-tests -main-is Postgres.HarnessSpec \
    -outputdir "$ARKHAM_TEST_LOG_DIR/ghc" -o "$ARKHAM_TEST_LOG_DIR/pg-harness-check" \
    arkham-api/postgres-tests/Postgres/HarnessSpec.hs
  cd ..
  python3 -B backend/arkham-api/postgres-tests/with-arkhamdb-postgres.py --log-dir "$ARKHAM_TEST_LOG_DIR" -- \
    "$ARKHAM_TEST_LOG_DIR/pg-harness-check"

Production-store integration component (registered in package.yaml):
  cd backend
  MISE_ENV=haskell mise exec -- stack build --system-ghc --test --no-run-tests
  cd ..
  python3 -B backend/arkham-api/postgres-tests/with-arkhamdb-postgres.py --log-dir "$ARKHAM_TEST_LOG_DIR" -- \
    mise exec -- stack test --system-ghc arkham-api:test:arkhamdb-postgres

Run all backend tests (also used by Haskell CI):
  python3 -B backend/arkham-api/postgres-tests/with-arkhamdb-postgres.py --log-dir "$ARKHAM_TEST_LOG_DIR" \
    --timeout 600 -- mise exec -- stack test --system-ghc

The child runs in backend/, with PG* and ARKHAMDB_TEST_* set by this launcher.
Set MISE_ENV=haskell and the lane's libpq/pcre environment before invoking it.
Build outside the launcher: its default command deadline is 180 seconds.
PostgreSQL server binaries must be on PATH or in ARKHAM_TEST_PG_BIN/--pg-bin.
Use one server installation, not the client-only Homebrew libpq bin/.
No Docker daemon, pre-existing database, PostgreSQL TCP listener, or live URL
is used. The public deck sync test binds its own loopback (127.0.0.1) HTTP
fixture on a random port; a populated http(s)_proxy/all_proxy variable (either
case) fails that test, which reports only the variable names.

Manifest edits require regeneration with native Hpack 0.39.6:
  cd backend
  hpack --version  # must report 0.39.6, not Stack's bundled 0.38.1
  hpack arkham-api
No production export changes are needed: persistentStoreWith is already
exported. The standalone harness checks do not replace the integration suite.
The integration transport is stubbed, never its persistence. Card-legality,
write-back, Games/Shared transaction hooks and hardening fixes remain separate.
"""

import argparse
from contextlib import contextmanager
import os
from pathlib import Path
import secrets
import shlex
import shutil
import signal
import subprocess
import sys
import tempfile


# Keep non-catalog process tooling outside scripts/: that flat directory is
# the sealed locale-catalog runner's exact, capability-linted import closure.
ROOT = Path(__file__).resolve().parents[3]
PORT = "25446"  # Unix socket only; never bind 5432, 5442, or any TCP port.
DATABASE = "arkhamdb_test"
USER = "arkhamdb_harness"
TARGET_VARIABLES = {"DATABASE_URL", "DB_HOST", "DB_PORT", "DB_NAME", "DB_USER", "DB_PASSWORD"}


def reject_target_environment(environment):
    names = sorted(
        name for name in environment
        if name.startswith(("PG", "ARKHAMDB_TEST_")) or name in TARGET_VARIABLES
    )
    if names:
        # Report names only: a connection URL/password must never reach logs.
        raise ValueError("refusing ambient database target variables: " + ", ".join(names))


def run(arguments, *, environment, timeout=30, **kwargs):
    return subprocess.run(
        arguments, env=environment, timeout=timeout, check=True, **kwargs
    )


def migration_files(root=ROOT):
    """Use the production plan's order and the actual deploy files, not SQL copies."""
    for line in (root / "migrations/sqitch.plan").read_text().splitlines():
        if not line or line.startswith(("%", "#", " ", "\t")):
            continue
        name = line.split()[0]
        path = root / "migrations/deploy" / (name + ".sql")
        if not path.is_file() or path.parent != root / "migrations/deploy":
            raise ValueError("invalid production migration: " + name)
        yield path


@contextmanager
def cluster(log_dir, pg_bin=None):
    reject_target_environment(os.environ)
    binaries = {}
    if pg_bin is None:
        pg_bin = os.environ.get("ARKHAM_TEST_PG_BIN")
    for name in ("initdb", "pg_ctl", "psql", "createdb", "postgres"):
        binary = shutil.which(name, path=str(pg_bin) if pg_bin is not None else None)
        if binary is None:
            raise ValueError("missing PostgreSQL dependency: " + name)
        binaries[name] = binary
    log_dir.mkdir(parents=True, exist_ok=True)
    owned = Path(tempfile.mkdtemp(prefix="pg-", dir=log_dir)).resolve()
    data = owned / "data"
    socket = owned / "s"
    socket.mkdir(mode=0o700)
    # sockaddr_un is only 104 bytes on macOS, including its terminating NUL.
    if len(os.fsencode(str(socket / (".s.PGSQL." + PORT)))) >= 104:
        shutil.rmtree(owned)
        raise ValueError("log directory path is too long for a PostgreSQL Unix socket")
    token = secrets.token_hex(24)
    (owned / "owner").write_text(token)
    environment = dict(os.environ)
    environment.update(
        PGHOST=str(socket), PGPORT=PORT, PGDATABASE=DATABASE, PGUSER=USER,
        PGCONNECT_TIMEOUT="5", ARKHAMDB_TEST_ROOT=str(owned), ARKHAMDB_TEST_TOKEN=token,
    )
    started = False
    try:
        run(
            [binaries["initdb"], "-D", str(data), "-U", USER, "--auth=trust",
             "--encoding=UTF8", "--no-locale"],
            environment=environment,
        )
        options = shlex.join([
            "-h", "", "-k", str(socket), "-p", PORT,
            "-c", "unix_socket_permissions=0700",
            "-c", "max_connections=12",
            "-c", "statement_timeout=8000",
            "-c", "lock_timeout=3000",
            "-c", "idle_in_transaction_session_timeout=10000",
        ])
        # Mark before starting: even a partially successful start needs teardown.
        started = True
        run(
            [binaries["pg_ctl"], "-D", str(data), "-l", str(owned / "postgres.log"),
             "-w", "-t", "20", "-o", options, "start"],
            environment=environment,
        )
        run([binaries["createdb"], DATABASE], environment=environment)
        for path in migration_files():
            run(
                [binaries["psql"], "-X", "--no-password", "-v", "ON_ERROR_STOP=1",
                 "-f", str(path)],
                environment=environment,
            )
        yield environment
    finally:
        if started:
            # Only pg_ctl for this exact private data directory may stop it.
            status = subprocess.run(
                [binaries["pg_ctl"], "-D", str(data), "status"],
                env=environment, timeout=10, check=False,
                stdout=subprocess.DEVNULL,
            )
            if status.returncode == 0:
                try:
                    run(
                        [binaries["pg_ctl"], "-D", str(data), "-w", "-t", "20",
                         "-m", "fast", "stop"],
                        environment=environment,
                    )
                except (subprocess.SubprocessError, OSError):
                    run(
                        [binaries["pg_ctl"], "-D", str(data), "-w", "-t", "20",
                         "-m", "immediate", "stop"],
                        environment=environment,
                    )
            elif status.returncode != 3:
                raise RuntimeError("cannot confirm owned cluster stopped; retained " + str(owned))
        # Keep server diagnostics; remove only our cluster/socket/ownership data.
        for path in (data, socket):
            if path.exists():
                shutil.rmtree(path)
        (owned / "owner").unlink()
        print("Owned PostgreSQL cluster removed; server log: " + str(owned / "postgres.log"), flush=True)


def run_child(command, environment, seconds):
    """Kill only this command's process group on timeout, before cluster teardown."""
    with subprocess.Popen(command, cwd=ROOT / "backend", env=environment, start_new_session=True) as child:
        try:
            return child.wait(timeout=seconds)
        except BaseException:
            # Stack may spawn a test executable; do not orphan it on cancellation.
            try:
                os.killpg(child.pid, signal.SIGTERM)
            except ProcessLookupError:
                pass
            try:
                try:
                    child.wait(timeout=5)
                except subprocess.TimeoutExpired:
                    pass
            finally:
                # The leader exiting does not mean its descendants exited.
                # Always escalate against this exact owned session's group.
                try:
                    os.killpg(child.pid, signal.SIGKILL)
                except ProcessLookupError:
                    pass
                child.wait(timeout=5)
            raise


def main():
    parser = argparse.ArgumentParser(description=__doc__, formatter_class=argparse.RawDescriptionHelpFormatter)
    parser.add_argument("--log-dir", type=Path, required=True)
    parser.add_argument("--pg-bin", type=Path, help="directory containing a complete PostgreSQL server installation")
    parser.add_argument("--timeout", type=int, default=180)
    parser.add_argument("command", nargs=argparse.REMAINDER)
    args = parser.parse_args()
    if args.command[:1] == ["--"]:
        args.command.pop(0)
    if not args.command:
        parser.error("a command after -- is required")
    if not 1 <= args.timeout <= 600:
        parser.error("--timeout must be between 1 and 600 seconds")
    # Ensure TERM takes the same finally/cleanup path as Ctrl-C.
    def interrupted(_signal, _frame):
        raise KeyboardInterrupt
    signal.signal(signal.SIGTERM, interrupted)
    try:
        with cluster(args.log_dir, args.pg_bin) as environment:
            return run_child(args.command, environment, args.timeout)
    except (ValueError, OSError, subprocess.SubprocessError, RuntimeError) as error:
        print("PostgreSQL harness failed: " + str(error), file=sys.stderr)
        return 1
    except KeyboardInterrupt:
        return 130


if __name__ == "__main__":
    sys.exit(main())
