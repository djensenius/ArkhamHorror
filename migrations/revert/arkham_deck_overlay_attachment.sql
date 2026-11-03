-- Revert arkham-horror-backend:arkham_deck_overlay_attachment from pg
BEGIN;
ALTER TABLE arkham_decks DROP COLUMN overlay_attachment_id;
COMMIT;
