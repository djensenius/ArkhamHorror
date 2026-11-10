-- Verify arkham-horror-backend:arkham_players_game_index on pg
BEGIN;
SELECT 1 / count(*)
FROM pg_index AS i
JOIN pg_class AS c ON c.oid = i.indexrelid
JOIN pg_am AS am ON am.oid = c.relam
WHERE i.indexrelid = to_regclass('idx_arkham_players_game')
  AND i.indrelid = 'arkham_players'::regclass
  AND i.indisvalid AND i.indisready AND NOT i.indisunique
  AND i.indpred IS NULL AND i.indexprs IS NULL
  AND i.indnkeyatts = 1 AND i.indnatts = 1
  AND i.indkey[0] = (
    SELECT attnum FROM pg_attribute
    WHERE attrelid = i.indrelid AND attname = 'arkham_game_id' AND NOT attisdropped
  )
  AND am.amname = 'btree'
  AND obj_description(c.oid, 'pg_class') =
    'arkham-horror-backend:arkham_players_game_index';
ROLLBACK;
