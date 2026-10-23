-- Deploy arkham-horror-backend:arkhamdb_write_back to pg
-- requires: arkhamdb_account_linking
BEGIN;
ALTER TABLE arkhamdb_accounts ADD COLUMN last_write_back_attempt_at timestamptz;
ALTER TABLE arkhamdb_accounts ADD COLUMN write_back_failures integer NOT NULL DEFAULT 0;
ALTER TABLE arkhamdb_deck_links ADD COLUMN campaign_key varchar;
CREATE TABLE arkhamdb_pushes (
  id uuid PRIMARY KEY DEFAULT uuid_generate_v4(),
  deck_id uuid REFERENCES arkham_decks (id) ON DELETE CASCADE NOT NULL,
  user_id bigint REFERENCES users (id) ON DELETE CASCADE NOT NULL,
  generation uuid REFERENCES arkhamdb_accounts (id) ON DELETE CASCADE NOT NULL,
  event_key varchar NOT NULL,
  campaign_key varchar NOT NULL,
  payload varchar NOT NULL,
  total_xp integer NOT NULL,
  status varchar NOT NULL,
  stage varchar NOT NULL,
  source_id varchar,
  target_id varchar,
  expected_date_update varchar,
  expected_version varchar,
  remote_date_update varchar,
  remote_version varchar,
  error varchar,
  mode varchar NOT NULL,
  created_at timestamptz NOT NULL,
  updated_at timestamptz NOT NULL,
  CONSTRAINT unique_arkham_d_b_push_event UNIQUE (deck_id, event_key)
);
CREATE INDEX idx_arkhamdb_pushes_pending ON arkhamdb_pushes (deck_id, created_at, id)
  WHERE status NOT IN ('succeeded', 'cancelled');
CREATE TABLE arkhamdb_campaign_decks (
  id uuid PRIMARY KEY DEFAULT uuid_generate_v4(),
  campaign_key varchar NOT NULL,
  deck_id uuid REFERENCES arkham_decks (id) ON DELETE CASCADE NOT NULL,
  user_id bigint REFERENCES users (id) ON DELETE CASCADE NOT NULL,
  last_pushed_xp integer NOT NULL DEFAULT 0,
  CONSTRAINT unique_arkham_d_b_campaign_deck UNIQUE (campaign_key)
);
COMMIT;
