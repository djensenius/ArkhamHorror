-- Deploy arkham-horror-backend:arkhamdb_write_back_lookup_indexes to pg
-- requires: arkhamdb_write_back
BEGIN;
-- campaign_decks has UNIQUE(campaign_key), but no index starting with deck_id.
CREATE INDEX IF NOT EXISTS idx_arkhamdb_campaign_decks_deck
  ON arkhamdb_campaign_decks (deck_id);
-- Scope user-deletion cascades to that user's retained campaign associations.
CREATE INDEX IF NOT EXISTS idx_arkhamdb_campaign_decks_user
  ON arkhamdb_campaign_decks (user_id);
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
-- Match campaignOwnsTarget's succeeded lineage lookup and newest-first order.
CREATE INDEX IF NOT EXISTS idx_arkhamdb_pushes_campaign_target
  ON arkhamdb_pushes (campaign_key, target_id, created_at DESC, id DESC)
  WHERE status = 'succeeded';
COMMIT;
