-- Deploy arkham-horror-backend:arkhamdb_write_back_undo_index to pg
-- requires: arkhamdb_write_back
BEGIN;
-- Both undo admission (EXISTS) and cancellation use this exact expression and
-- predicate. Ordinary games never scan outstanding work belonging to others.
CREATE INDEX idx_arkhamdb_pushes_game_outstanding
  ON arkhamdb_pushes (split_part(campaign_key, '/', 1), user_id, id)
  WHERE status NOT IN ('succeeded', 'cancelled');
COMMIT;
