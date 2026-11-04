-- Deploy arkham-horror-backend:arkhamdb_link_states_user_index to pg
-- requires: arkhamdb_account_linking
BEGIN;
-- User deletion cascades to this table by user_id, and the store replaces and
-- reads a user's own pending states by user_id. Index it so neither scans.
-- Deliberately fail on a name collision rather than adopt an unrelated index.
CREATE INDEX idx_arkhamdb_link_states_user
  ON arkhamdb_link_states (user_id);
COMMENT ON INDEX idx_arkhamdb_link_states_user
  IS 'arkham-horror-backend:arkhamdb_link_states_user_index';
COMMIT;
