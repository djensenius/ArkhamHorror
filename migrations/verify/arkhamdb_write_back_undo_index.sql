-- Verify arkham-horror-backend:arkhamdb_write_back_undo_index on pg
BEGIN;
SELECT 1 / count(*) FROM pg_indexes
  WHERE schemaname = current_schema() AND tablename = 'arkhamdb_pushes'
    AND indexname = 'idx_arkhamdb_pushes_game_outstanding';
ROLLBACK;
