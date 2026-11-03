-- Revert arkham-horror-backend:arkhamdb_write_back from pg
BEGIN;
DROP TABLE arkhamdb_pushes;
DROP TABLE arkhamdb_campaign_decks;
ALTER TABLE arkhamdb_deck_links DROP COLUMN campaign_key;
ALTER TABLE arkhamdb_accounts DROP COLUMN last_write_back_attempt_at, DROP COLUMN write_back_failures;
COMMIT;
