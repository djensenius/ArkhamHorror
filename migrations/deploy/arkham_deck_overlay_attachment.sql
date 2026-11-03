-- Deploy arkham-horror-backend:arkham_deck_overlay_attachment to pg
-- requires: arkham_deck_overlay
BEGIN;
ALTER TABLE arkham_decks ADD COLUMN overlay_attachment_id uuid;
-- Existing attachments are unknown. Only an actual new attachment write may
-- establish identity; migration/redeployment must not invent a new attachment.
COMMIT;
