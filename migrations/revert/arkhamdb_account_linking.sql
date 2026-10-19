-- Revert arkham-horror-backend:arkhamdb_account_linking from pg

BEGIN;

DROP INDEX IF EXISTS idx_arkhamdb_deck_links_remote;
DROP TABLE IF EXISTS arkhamdb_deck_links;
DROP TABLE IF EXISTS arkhamdb_link_states;
DROP TABLE IF EXISTS arkhamdb_accounts;

COMMIT;
