-- Revert arkham-horror-backend:arkhamdb_write_back from pg
BEGIN;
DROP TABLE arkhamdb_pushes;
DROP TABLE arkhamdb_campaign_decks;
ALTER TABLE arkhamdb_deck_links DROP COLUMN campaign_key;
COMMIT;
