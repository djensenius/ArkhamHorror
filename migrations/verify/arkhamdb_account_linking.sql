-- Verify arkham-horror-backend:arkhamdb_account_linking on pg

BEGIN;

SELECT id, user_id, remote_user_id, remote_user_name, encrypted_access_token,
       encrypted_refresh_token, expires_at, created_at, updated_at
  FROM arkhamdb_accounts
 WHERE FALSE;

SELECT id, nonce, user_id, expires_at, created_at, claimed_at
  FROM arkhamdb_link_states
 WHERE FALSE;

SELECT id, deck_id, remote_deck_id, last_synced_date_update,
       last_synced_version, write_back, last_error, created_at, updated_at
  FROM arkhamdb_deck_links
 WHERE FALSE;

ROLLBACK;
