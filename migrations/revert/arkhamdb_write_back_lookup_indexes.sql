-- Revert arkham-horror-backend:arkhamdb_write_back_lookup_indexes from pg
BEGIN;
DROP INDEX idx_arkhamdb_pushes_campaign_outstanding;
DROP INDEX idx_arkhamdb_deck_links_campaign;
DROP INDEX idx_arkhamdb_campaign_decks_deck;
COMMIT;
