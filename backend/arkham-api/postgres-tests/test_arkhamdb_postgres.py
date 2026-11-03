"""Launcher safety/lifecycle tests; no business SQL or production-store mocks.

Run from the repo root:
  python3 -B -m unittest discover -s backend/arkham-api/postgres-tests \
    -p 'test_arkhamdb_postgres.py' -v

Real-cluster cases require ARKHAM_TEST_LOG_DIR, e.g.
  /Users/david/.local/state/arkham-logs/copilot-20261028
CI should set it to a short, owned job log directory (not a database directory).
Missing PostgreSQL binaries or this directory is a failure, never a skipped test.
"""

import importlib.util
import os
from pathlib import Path
import signal
import subprocess
import sys
import time
import unittest
from unittest.mock import patch


SCRIPT = Path(__file__).with_name("with-arkhamdb-postgres.py")
SPEC = importlib.util.spec_from_file_location("arkhamdb_postgres", SCRIPT)
launcher = importlib.util.module_from_spec(SPEC)
SPEC.loader.exec_module(launcher)


class SafetyTests(unittest.TestCase):
    def test_rejects_live_targets_without_logging_values(self):
        for name in ("DATABASE_URL", "PGHOST", "PGSERVICE", "PGPASSFILE",
                     "PGOPTIONS", "DB_HOST", "ARKHAMDB_TEST_ROOT"):
            with self.subTest(name=name):
                with self.assertRaises(ValueError) as error:
                    launcher.reject_target_environment({name: "secret-live-target"})
                self.assertIn(name, str(error.exception))
                self.assertNotIn("secret-live-target", str(error.exception))

    def test_rejects_even_empty_target_overrides(self):
        with self.assertRaises(ValueError):
            launcher.reject_target_environment({"PGHOST": ""})

    def test_ordinary_environment_is_allowed(self):
        launcher.reject_target_environment({"PATH": "/bin", "CI": "true"})

    def test_plan_uses_real_deploy_files_in_production_order(self):
        paths = list(launcher.migration_files())
        self.assertEqual(paths[0].stem, "users")
        self.assertLess(
            paths.index(launcher.ROOT / "migrations/deploy/arkhamdb_account_linking.sql"),
            paths.index(launcher.ROOT / "migrations/deploy/arkhamdb_write_back.sql"),
        )
        self.assertTrue(all(path.is_file() for path in paths))

    def test_missing_dependency_fails_before_creating_cluster(self):
        with patch.dict(os.environ, {"PATH": "/missing"}, clear=True):
            with patch.object(launcher.shutil, "which", return_value=None):
                with self.assertRaisesRegex(ValueError, "missing PostgreSQL dependency: initdb"):
                    with launcher.cluster(Path("/must-not-create-this")):
                        self.fail("unreachable")


class RealClusterTests(unittest.TestCase):
    @classmethod
    def setUpClass(cls):
        # Fail explicitly instead of silently turning off integration coverage.
        cls.log_dir = Path(os.environ["ARKHAM_TEST_LOG_DIR"]).resolve()
        launcher.reject_target_environment(os.environ)

    def launch(self, command, timeout=30):
        return subprocess.run(
            [sys.executable, "-B", str(SCRIPT), "--log-dir", str(self.log_dir),
             "--timeout", str(timeout), "--", *command],
            cwd=launcher.ROOT, text=True, capture_output=True, timeout=90,
        )

    def assert_removed(self, result):
        prefix = "Owned PostgreSQL cluster removed; server log: "
        lines = [line for line in result.stdout.splitlines() if line.startswith(prefix)]
        self.assertEqual(len(lines), 1, result.stdout + result.stderr)
        owned = Path(lines[0][len(prefix):]).parent
        self.assertEqual(owned.parent, self.log_dir)
        self.assertFalse((owned / "data").exists())
        self.assertFalse((owned / "s").exists())
        self.assertFalse((owned / "owner").exists())
        self.assertTrue((owned / "postgres.log").is_file())
        return owned

    def test_all_production_migrations_and_socket_only_isolation(self):
        pg_bin = os.environ.get("ARKHAM_TEST_PG_BIN")
        psql = str(Path(pg_bin) / "psql") if pg_bin else "psql"
        result = self.launch([
            psql, "-X", "-v", "ON_ERROR_STOP=1", "-Atc",
            "SELECT current_database(), current_setting('listen_addresses'), "
            "to_regclass('arkhamdb_accounts'), to_regclass('arkhamdb_pushes'), "
            "to_regclass('idx_arkhamdb_pushes_game_outstanding');",
        ])
        self.assertEqual(result.returncode, 0, result.stdout + result.stderr)
        self.assertIn(
            "arkhamdb_test||arkhamdb_accounts|arkhamdb_pushes|idx_arkhamdb_pushes_game_outstanding",
            result.stdout,
        )
        self.assert_removed(result)

    def test_failed_command_preserves_exit_code_and_removes_cluster(self):
        result = self.launch([sys.executable, "-c", "raise SystemExit(23)"])
        self.assertEqual(result.returncode, 23, result.stdout + result.stderr)
        self.assert_removed(result)

    def test_expiry_index_migration_round_trip_and_foreign_name_collisions(self):
        change = "arkhamdb_link_states_expiry_index"
        index = "idx_arkhamdb_link_states_expiry"
        with launcher.cluster(self.log_dir) as environment:
            pg_bin = environment.get("ARKHAM_TEST_PG_BIN")
            psql = str(Path(pg_bin) / "psql") if pg_bin else "psql"

            def execute(*arguments, success=True):
                result = subprocess.run(
                    [psql, "-X", "--no-password", "-v", "ON_ERROR_STOP=1", "-At", *arguments],
                    env=environment, text=True, capture_output=True, timeout=30,
                )
                if success:
                    self.assertEqual(result.returncode, 0, result.stdout + result.stderr)
                else:
                    self.assertNotEqual(result.returncode, 0, result.stdout + result.stderr)
                return result.stdout.strip()

            def migration(kind, success=True):
                return execute("-f", str(launcher.ROOT / "migrations" / kind / (change + ".sql")),
                               success=success)

            execute("-c", "INSERT INTO users (username, email, password_digest) "
                    "VALUES ('expiry-migration-guard', 'guard@postgres-test.invalid', 'not-a-password')")
            execute("-c", "INSERT INTO arkhamdb_link_states "
                    "(nonce, user_id, expires_at, created_at, claimed_at, completion_hash, encrypted_pending_tokens) "
                    "SELECT 'migration-guard', id, now() + interval '10 minutes', now(), now(), "
                    "'guard-completion', 'encrypted-guard' FROM users WHERE username = 'expiry-migration-guard'")
            migration("verify")
            migration("revert")
            self.assertEqual(execute("-c", f"SELECT to_regclass('{index}') IS NULL"), "t")
            migration("verify", success=False)
            migration("deploy")
            migration("verify")
            migration("revert")
            # Neither a different table nor an unowned same-shaped index may
            # be adopted by deploy or removed by a failed migration's revert.
            for target in ("users (id)", "arkhamdb_link_states (expires_at, id)"):
                with self.subTest(target=target):
                    execute("-c", f"CREATE INDEX {index} ON {target}")
                    migration("deploy", success=False)
                    migration("verify", success=False)
                    migration("revert", success=False)
                    self.assertEqual(execute("-c", f"SELECT to_regclass('{index}') IS NOT NULL"), "t")
                    execute("-c", f"DROP INDEX {index}")
            migration("deploy")
            migration("verify")
            self.assertEqual(execute("-c", "SELECT count(*) FROM users "
                                    "WHERE username = 'expiry-migration-guard'"), "1")
            self.assertEqual(execute("-c", "SELECT completion_hash, encrypted_pending_tokens "
                                    "FROM arkhamdb_link_states WHERE nonce = 'migration-guard'"),
                             "guard-completion|encrypted-guard")

    def test_timed_out_command_is_reaped_before_cluster_removal(self):
        result = self.launch([sys.executable, "-c", "import time; time.sleep(60)"], timeout=1)
        self.assertEqual(result.returncode, 1, result.stdout + result.stderr)
        self.assertIn("timed out", result.stderr)
        self.assert_removed(result)

    def test_sigterm_terminates_child_and_removes_only_owned_cluster(self):
        result = self.launch([
            sys.executable, "-c",
            "import os, signal, time; os.kill(os.getppid(), signal.SIGTERM); time.sleep(60)",
        ])
        self.assertEqual(result.returncode, 130, result.stdout + result.stderr)
        self.assert_removed(result)

    def test_timeout_kills_ignoring_descendant_after_parent_exits_on_sigterm(self):
        command = """
import os, signal, time
reader, writer = os.pipe()
descendant = os.fork()
if descendant == 0:
    os.close(reader)
    signal.signal(signal.SIGTERM, signal.SIG_IGN)
    with open(os.devnull, "w") as sink:
        os.dup2(sink.fileno(), 1)
        os.dup2(sink.fileno(), 2)
    os.write(writer, b"ready")
    os.close(writer)
    time.sleep(60)
    os._exit(0)
os.close(writer)
assert os.read(reader, 5) == b"ready"
os.close(reader)
print("ignoring-descendant:" + str(descendant), flush=True)
time.sleep(60)
"""
        started = time.monotonic()
        result = self.launch([sys.executable, "-c", command], timeout=2)
        lines = [line for line in result.stdout.splitlines() if line.startswith("ignoring-descendant:")]
        self.assertEqual(len(lines), 1, result.stdout + result.stderr)
        descendant = int(lines[0].split(":")[1])

        def alive():
            state = subprocess.run(
                ["ps", "-p", str(descendant), "-o", "stat="],
                text=True, capture_output=True, check=False, timeout=5,
            ).stdout.strip()
            # A reparented grandchild cannot be waitpid-reaped by this test.
            return bool(state) and not state.startswith("Z")

        try:
            self.assertEqual(result.returncode, 1, result.stdout + result.stderr)
            self.assertIn("timed out", result.stderr)
            self.assertLess(time.monotonic() - started, 30)
            self.assert_removed(result)
            deadline = time.monotonic() + 5
            while alive() and time.monotonic() < deadline:
                time.sleep(0.05)
            self.assertFalse(alive(), "SIGTERM-ignoring descendant survived launcher teardown")
        finally:
            # Keep even a failing regression run from leaking its exact fixture.
            if alive():
                try:
                    os.kill(descendant, signal.SIGKILL)
                except ProcessLookupError:
                    pass


if __name__ == "__main__":
    unittest.main()
