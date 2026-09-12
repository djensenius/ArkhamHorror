-- Verify arkham-horror-backend:arkham_replay_attestations on pg

BEGIN;

SELECT id, receipt
  FROM arkham_replay_attestations
 WHERE FALSE;

DO $$
BEGIN
  IF NOT EXISTS (
    SELECT 1
      FROM information_schema.columns
     WHERE table_schema = 'public'
       AND table_name = 'arkham_replay_attestations'
       AND column_name = 'receipt'
       AND data_type = 'jsonb'
       AND is_nullable = 'NO'
  ) THEN
    RAISE EXCEPTION 'arkham_replay_attestations.receipt must be non-null jsonb';
  END IF;

  IF NOT EXISTS (
    SELECT 1
      FROM pg_constraint
     WHERE conrelid = 'public.arkham_replay_attestations'::regclass
       AND conname = 'arkham_replay_attestations_pkey'
       AND contype = 'p'
       AND conkey = ARRAY[
         (
           SELECT attnum
             FROM pg_attribute
            WHERE attrelid = 'public.arkham_replay_attestations'::regclass
              AND attname = 'id'
              AND NOT attisdropped
         )
       ]::smallint[]
       AND NOT condeferrable
       AND NOT condeferred
       AND convalidated
  ) THEN
    RAISE EXCEPTION 'arkham_replay_attestations must be keyed by game id';
  END IF;

  IF NOT EXISTS (
    SELECT 1
      FROM pg_constraint
     WHERE conname = 'arkham_replay_attestations_game_fk'
       AND conrelid = 'public.arkham_replay_attestations'::regclass
       AND confrelid = 'public.arkham_games'::regclass
       AND contype = 'f'
       AND confdeltype = 'c'
       AND confupdtype = 'a'
       AND confmatchtype = 's'
       AND conkey = ARRAY[
         (
           SELECT attnum
             FROM pg_attribute
            WHERE attrelid = 'public.arkham_replay_attestations'::regclass
              AND attname = 'id'
              AND NOT attisdropped
         )
       ]::smallint[]
       AND confkey = ARRAY[
         (
           SELECT attnum
             FROM pg_attribute
            WHERE attrelid = 'public.arkham_games'::regclass
              AND attname = 'id'
              AND NOT attisdropped
         )
       ]::smallint[]
       AND NOT condeferrable
       AND NOT condeferred
       AND convalidated
  ) THEN
    RAISE EXCEPTION 'arkham_replay_attestations must cascade with its game';
  END IF;

  IF NOT EXISTS (
    SELECT 1
      FROM pg_constraint
     WHERE conname = 'arkham_replay_attestations_receipt_object'
       AND conrelid = 'public.arkham_replay_attestations'::regclass
       AND contype = 'c'
       AND pg_get_constraintdef(oid, true)
         = 'CHECK (jsonb_typeof(receipt) = ''object''::text)'
       AND convalidated
  ) THEN
    RAISE EXCEPTION 'arkham_replay_attestations must require an object receipt';
  END IF;
END
$$;

ROLLBACK;
