-- Verify arkham-horror-backend:arkham_deck_overlay_attachment on pg
BEGIN;
SELECT overlay_attachment_id FROM arkham_decks WHERE FALSE;
ROLLBACK;
