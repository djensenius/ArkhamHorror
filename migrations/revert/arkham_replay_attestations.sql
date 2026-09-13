-- Revert arkham-horror-backend:arkham_replay_attestations from pg

BEGIN;

DROP TABLE IF EXISTS arkham_replay_attestations;

COMMIT;
