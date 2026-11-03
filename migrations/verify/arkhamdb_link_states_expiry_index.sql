-- Verify arkham-horror-backend:arkhamdb_link_states_expiry_index on pg
BEGIN;
SELECT 1 / count(*)
FROM pg_index AS i
JOIN pg_class AS c ON c.oid = i.indexrelid
JOIN pg_am AS am ON am.oid = c.relam
WHERE i.indexrelid = to_regclass('idx_arkhamdb_link_states_expiry')
  AND i.indrelid = 'arkhamdb_link_states'::regclass
  AND i.indisvalid AND i.indisready AND NOT i.indisunique
  AND i.indpred IS NULL AND i.indexprs IS NULL
  AND i.indnkeyatts = 2 AND i.indnatts = 2
  AND i.indkey[0] = (
    SELECT attnum FROM pg_attribute
    WHERE attrelid = i.indrelid AND attname = 'expires_at' AND NOT attisdropped
  )
  AND i.indkey[1] = (
    SELECT attnum FROM pg_attribute
    WHERE attrelid = i.indrelid AND attname = 'id' AND NOT attisdropped
  )
  AND am.amname = 'btree'
  AND obj_description(c.oid, 'pg_class') =
    'arkham-horror-backend:arkhamdb_link_states_expiry_index';
ROLLBACK;
