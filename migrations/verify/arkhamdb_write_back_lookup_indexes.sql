-- Verify arkham-horror-backend:arkhamdb_write_back_lookup_indexes on pg
BEGIN;
SELECT 1 / count(*) FROM pg_indexes
  WHERE schemaname = current_schema() AND tablename = 'arkhamdb_campaign_decks'
    AND indexname = 'idx_arkhamdb_campaign_decks_deck';
SELECT 1 / count(*) FROM pg_indexes
  WHERE schemaname = current_schema() AND tablename = 'arkhamdb_deck_links'
    AND indexname = 'idx_arkhamdb_deck_links_campaign';
ROLLBACK;
