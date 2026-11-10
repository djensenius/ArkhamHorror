-- Revert arkham-horror-backend:arkham_players_game_index from pg
BEGIN;
-- Refuse to remove a same-named foreign object after a failed deploy.
DO $$
BEGIN
  IF NOT EXISTS (
    SELECT 1 FROM pg_index
    WHERE indexrelid = to_regclass('idx_arkham_players_game')
      AND indrelid = 'arkham_players'::regclass
      AND obj_description(indexrelid, 'pg_class') =
        'arkham-horror-backend:arkham_players_game_index'
  ) THEN
    RAISE EXCEPTION 'Arkham players game index is absent or not owned by this migration';
  END IF;
END
$$;
DROP INDEX idx_arkham_players_game;
COMMIT;
