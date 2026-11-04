-- Deploy arkham-horror-backend:add_admin_to_users to pg
-- requires: users

BEGIN;

-- Entity.User reads this column, and setup.sql has always created it, but no
-- earlier change added it, so a database built from this plan alone lacked it.
-- Match setup.sql exactly (nullable, default false): on a database built from
-- setup.sql the column already exists and this change does nothing.
ALTER TABLE users ADD COLUMN IF NOT EXISTS admin BOOLEAN DEFAULT FALSE;

COMMIT;
