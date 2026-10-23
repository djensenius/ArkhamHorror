-- Verify arkham-horror-backend:arkhamdb_write_back on pg
BEGIN;
SELECT campaign_key FROM arkhamdb_deck_links WHERE FALSE;
SELECT id, deck_id, user_id, generation, event_key, campaign_key, payload, total_xp,
       status, stage, source_id, target_id, expected_date_update, expected_version,
       remote_date_update, remote_version, error, mode, created_at, updated_at
  FROM arkhamdb_pushes WHERE FALSE;
SELECT id, campaign_key, deck_id, user_id, last_pushed_xp FROM arkhamdb_campaign_decks WHERE FALSE;
ROLLBACK;
