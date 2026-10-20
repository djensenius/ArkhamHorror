-- Deploy arkham-horror-backend:arkhamdb_account_linking to pg
-- requires: users
-- requires: arkham_decks

BEGIN;

CREATE EXTENSION IF NOT EXISTS "uuid-ossp";

CREATE TABLE IF NOT EXISTS arkhamdb_accounts (
  id uuid PRIMARY KEY DEFAULT uuid_generate_v4(),
  user_id bigint REFERENCES users (id) ON DELETE CASCADE NOT NULL,
  remote_user_id varchar,
  remote_user_name varchar,
  encrypted_access_token varchar NOT NULL,
  encrypted_refresh_token varchar,
  expires_at timestamptz,
  created_at timestamptz NOT NULL,
  updated_at timestamptz NOT NULL,
  CONSTRAINT unique_arkham_d_b_account_user UNIQUE (user_id)
);

CREATE TABLE IF NOT EXISTS arkhamdb_link_states (
  id uuid PRIMARY KEY DEFAULT uuid_generate_v4(),
  nonce varchar NOT NULL,
  user_id bigint REFERENCES users (id) ON DELETE CASCADE NOT NULL,
  expires_at timestamptz NOT NULL,
  created_at timestamptz NOT NULL,
  claimed_at timestamptz,
  completion_hash varchar,
  encrypted_pending_tokens varchar,
  CONSTRAINT unique_arkham_d_b_link_state_nonce UNIQUE (nonce)
);

CREATE TABLE IF NOT EXISTS arkhamdb_deck_links (
  id uuid PRIMARY KEY DEFAULT uuid_generate_v4(),
  deck_id uuid REFERENCES arkham_decks (id) ON DELETE CASCADE NOT NULL,
  remote_deck_id varchar NOT NULL,
  last_synced_date_update varchar,
  last_synced_version varchar,
  write_back boolean NOT NULL DEFAULT false,
  last_error varchar,
  created_at timestamptz NOT NULL,
  updated_at timestamptz NOT NULL,
  CONSTRAINT unique_arkham_d_b_deck_link_deck UNIQUE (deck_id)
);

CREATE INDEX IF NOT EXISTS idx_arkhamdb_deck_links_remote
  ON arkhamdb_deck_links (remote_deck_id);

COMMIT;
