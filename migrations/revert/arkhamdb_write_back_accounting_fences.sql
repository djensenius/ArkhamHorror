-- Revert arkham-horror-backend:arkhamdb_write_back_accounting_fences from pg
BEGIN;
-- Intentionally refuses rollback while orphaned accounting exists. Never
-- discard checkpoints merely to restore a credential-ownership constraint.
ALTER TABLE arkhamdb_pushes ADD CONSTRAINT arkhamdb_pushes_generation_fkey
  FOREIGN KEY (generation) REFERENCES arkhamdb_accounts (id) ON DELETE CASCADE;
DROP TRIGGER arkhamdb_account_accounting_fence ON arkhamdb_accounts;
DROP FUNCTION fence_arkhamdb_account_pushes();
COMMIT;
