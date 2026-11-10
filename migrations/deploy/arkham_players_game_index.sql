-- Deploy arkham-horror-backend:arkham_players_game_index to pg
-- requires: arkham_players
BEGIN;
-- Per-viewer redaction, pending-game join (under the game row lock) and other
-- paths look up a game's seats by arkham_game_id. Index it so none of them scan.
-- Deliberately fail on a name collision rather than adopt an unrelated index.
CREATE INDEX idx_arkham_players_game
  ON arkham_players (arkham_game_id);
COMMENT ON INDEX idx_arkham_players_game
  IS 'arkham-horror-backend:arkham_players_game_index';
COMMIT;
