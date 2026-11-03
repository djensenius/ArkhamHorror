-- Deploy arkham-horror-backend:arkhamdb_write_back_lookup_indexes to pg
-- requires: arkhamdb_write_back
BEGIN;
-- campaign_decks has UNIQUE(campaign_key), but no index starting with deck_id.
CREATE INDEX IF NOT EXISTS idx_arkhamdb_campaign_decks_deck
  ON arkhamdb_campaign_decks (deck_id);
-- deck_links has UNIQUE(deck_id), but no index starting with campaign_key.
CREATE INDEX IF NOT EXISTS idx_arkhamdb_deck_links_campaign
  ON arkhamdb_deck_links (campaign_key);
COMMIT;
