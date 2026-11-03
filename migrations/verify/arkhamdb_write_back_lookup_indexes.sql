-- Verify arkham-horror-backend:arkhamdb_write_back_lookup_indexes on pg
BEGIN;
SELECT 1 / count(*) FROM pg_indexes
  WHERE schemaname = current_schema() AND tablename = 'arkhamdb_campaign_decks'
    AND indexname = 'idx_arkhamdb_campaign_decks_deck';
SELECT 1 / count(*) FROM pg_indexes
  WHERE schemaname = current_schema() AND tablename = 'arkhamdb_deck_links'
    AND indexname = 'idx_arkhamdb_deck_links_campaign';
SELECT 1 / count(*) FROM pg_indexes
  WHERE schemaname = current_schema() AND tablename = 'arkhamdb_pushes'
    AND indexname = 'idx_arkhamdb_pushes_campaign_outstanding';
SELECT 1 / count(*) FROM pg_indexes
  WHERE schemaname = current_schema() AND tablename = 'arkhamdb_pushes'
    AND indexname = 'idx_arkhamdb_pushes_dispatch_active'
    AND indexdef LIKE '% USING btree (id) WHERE ((status)::text = ANY ((ARRAY[''pending''::character varying, ''running''::character varying])::text[]))';
SELECT 1 / (1 - count(*)) FROM pg_indexes
  WHERE schemaname = current_schema()
    AND indexname = 'idx_arkhamdb_pushes_dispatch';
ROLLBACK;
