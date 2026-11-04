-- Revert arkham-horror-backend:add_admin_to_users from pg

BEGIN;

-- Deliberately a no-op: the column is never dropped. Databases created from
-- setup.sql had users.admin long before this change existed, and deploy left
-- it untouched there, so dropping it on revert would destroy real admin flags
-- that this change never created.

COMMIT;
