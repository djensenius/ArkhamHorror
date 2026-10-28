-- Deploy arkham-horror-backend:arkhamdb_write_back_accounting_fences to pg
-- requires: arkhamdb_write_back
BEGIN;
-- Generation is an immutable admission token, not ownership of accounting.
-- Deleting credentials must not delete evidence of an uncertain remote write.
ALTER TABLE arkhamdb_pushes DROP CONSTRAINT arkhamdb_pushes_generation_fkey;
CREATE FUNCTION fence_arkhamdb_account_pushes() RETURNS trigger AS $$
BEGIN
  UPDATE arkhamdb_pushes
    SET status = CASE WHEN stage <> 'ready' OR target_id IS NOT NULL OR status = 'running'
                      THEN 'conflict' ELSE 'cancelled' END,
        error = CASE WHEN stage <> 'ready' OR target_id IS NOT NULL OR status = 'running'
                     THEN 'accountChanged' ELSE NULL END,
        updated_at = clock_timestamp()
    WHERE generation = OLD.id AND status NOT IN ('succeeded', 'cancelled');
  RETURN OLD;
END;
$$ LANGUAGE plpgsql;
-- Covers every DELETE path: unlink, callback generation replacement, revocation
-- and administrative deletion. No access/refresh/pending credentials survive.
CREATE TRIGGER arkhamdb_account_accounting_fence
  BEFORE DELETE ON arkhamdb_accounts
  FOR EACH ROW EXECUTE FUNCTION fence_arkhamdb_account_pushes();
COMMIT;
