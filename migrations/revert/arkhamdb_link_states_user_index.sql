-- Revert arkham-horror-backend:arkhamdb_link_states_user_index from pg
BEGIN;
-- Refuse to remove a same-named foreign object after a failed deploy.
DO $$
BEGIN
  IF NOT EXISTS (
    SELECT 1 FROM pg_index
    WHERE indexrelid = to_regclass('idx_arkhamdb_link_states_user')
      AND indrelid = 'arkhamdb_link_states'::regclass
      AND obj_description(indexrelid, 'pg_class') =
        'arkham-horror-backend:arkhamdb_link_states_user_index'
  ) THEN
    RAISE EXCEPTION 'ArkhamDB link-state user index is absent or not owned by this migration';
  END IF;
END
$$;
DROP INDEX idx_arkhamdb_link_states_user;
COMMIT;
