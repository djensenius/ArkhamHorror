-- Revert arkham-horror-backend:arkhamdb_write_back_undo_index from pg
BEGIN;
DROP INDEX idx_arkhamdb_pushes_game_outstanding;
COMMIT;
