-- Deploy arkham-horror-backend:arkham_replay_attestations to pg
-- requires: arkham_games

BEGIN;

CREATE TABLE IF NOT EXISTS arkham_replay_attestations (
  id uuid PRIMARY KEY,
  receipt jsonb NOT NULL,
  CONSTRAINT arkham_replay_attestations_game_fk
    FOREIGN KEY (id) REFERENCES arkham_games (id) ON DELETE CASCADE,
  CONSTRAINT arkham_replay_attestations_receipt_object
    CHECK (jsonb_typeof(receipt) = 'object')
);

COMMIT;
