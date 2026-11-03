-- Revert arkham-horror-backend:arkhamdb_write_back_lookup_indexes from pg
BEGIN;
CREATE INDEX IF NOT EXISTS idx_arkhamdb_pushes_dispatch
  ON arkhamdb_pushes (id)
  WHERE status = 'pending';
DROP INDEX idx_arkhamdb_pushes_dispatch_active;
DROP INDEX idx_arkhamdb_pushes_campaign_outstanding;
DROP INDEX idx_arkhamdb_deck_links_campaign;
DROP INDEX idx_arkhamdb_campaign_decks_deck;
COMMIT;
