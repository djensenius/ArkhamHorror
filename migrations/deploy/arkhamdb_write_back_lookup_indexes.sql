-- Deploy arkham-horror-backend:arkhamdb_write_back_lookup_indexes to pg
-- requires: arkhamdb_write_back
BEGIN;
-- campaign_decks has UNIQUE(campaign_key), but no index starting with deck_id.
CREATE INDEX IF NOT EXISTS idx_arkhamdb_campaign_decks_deck
  ON arkhamdb_campaign_decks (deck_id);
-- deck_links has UNIQUE(deck_id), but no index starting with campaign_key.
CREATE INDEX IF NOT EXISTS idx_arkhamdb_deck_links_campaign
  ON arkhamdb_deck_links (campaign_key);
CREATE INDEX IF NOT EXISTS idx_arkhamdb_pushes_campaign_outstanding
  ON arkhamdb_pushes (campaign_key)
  WHERE status NOT IN ('succeeded', 'cancelled');
-- Cover pending work and running leases in one id-ordered dispatch scan.
CREATE INDEX IF NOT EXISTS idx_arkhamdb_pushes_dispatch_active
  ON arkhamdb_pushes (id)
  WHERE status IN ('pending', 'running');
DROP INDEX IF EXISTS idx_arkhamdb_pushes_dispatch;
COMMIT;
