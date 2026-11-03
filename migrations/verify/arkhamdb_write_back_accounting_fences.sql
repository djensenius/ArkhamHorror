-- Verify arkham-horror-backend:arkhamdb_write_back_accounting_fences on pg
BEGIN;
DO $$
BEGIN
  IF EXISTS (SELECT 1 FROM pg_constraint
             WHERE conrelid = 'arkhamdb_pushes'::regclass
               AND confrelid = 'arkhamdb_accounts'::regclass) THEN
    RAISE EXCEPTION 'Accounting must survive credential generation deletion';
  END IF;
  IF NOT EXISTS (SELECT 1 FROM pg_trigger
                 WHERE tgrelid = 'arkhamdb_accounts'::regclass
                   AND tgname = 'arkhamdb_account_accounting_fence'
                   AND tgenabled = 'O') THEN
    RAISE EXCEPTION 'Missing account accounting fence';
  END IF;
END;
$$;
ROLLBACK;
