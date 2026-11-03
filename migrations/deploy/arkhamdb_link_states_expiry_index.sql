-- Deploy arkham-horror-backend:arkhamdb_link_states_expiry_index to pg
-- requires: arkhamdb_account_linking
BEGIN;
-- Nonces and staged credentials share this table. Match the sweep's range
-- predicate and ordering so active authorizations need not be scanned/sorted.
-- Deliberately fail on a name collision rather than adopt an unrelated index.
CREATE INDEX idx_arkhamdb_link_states_expiry
  ON arkhamdb_link_states (expires_at, id);
COMMENT ON INDEX idx_arkhamdb_link_states_expiry
  IS 'arkham-horror-backend:arkhamdb_link_states_expiry_index';
COMMIT;
