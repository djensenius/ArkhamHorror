{- | Who is reading a game over REST, and the redacted response they get.

'Api.Arkham.Redact' decides what a 'Viewer' may see of an encoded game. This
module derives that viewer from the caller's current @arkham_players@ rows
(seats move through deck swaps and claim seat, so it is looked up on every
request) and wraps a response so its @game@ value is redacted on the way out.
-}
module Api.Arkham.Viewer (
  GameView (..),
  gameViewFor,
  viewerForUser,
  Redacted (..),
  redactGameField,
) where

import Api.Arkham.Redact (Viewer (..), redactFor)
import Arkham.Id (PlayerId (..))
import Data.Aeson qualified as Aeson
import Data.Aeson.KeyMap qualified as KeyMap
import Data.List (partition)
import Data.Set qualified as Set
import Database.Persist.Sql (SqlBackend)
import Import

{- | What a caller is shown of one game.

'FullView' is the unredacted game. It is only for a caller who holds every
seat row of the game: then nothing in it is hidden from them, and it also
covers Solo games whose stored seat ids no longer match the investigators'
@playerId@s (an imported Solo game keeps the exporter's player ids), where
'ViewerSeats' would wrongly hide the caller's own hands.
-}
data GameView = FullView | RedactedView Viewer
  deriving stock (Show, Eq)

{- | The decision 'viewerForUser' makes, on the game's seat rows as
@(owner, seat)@ pairs:

  * the caller owns every row (and there is at least one): 'FullView';
  * the caller owns some rows: 'ViewerSeats' with those seats;
  * the caller owns none (or the game has no rows): 'ViewerSpectator'.

Known edges of 'FullView' (tracked for TASK-2.22.5): it can include imported
seats nobody has claimed yet, and old replay steps from before a Main Street
swap, whose investigators carried other seats.
-}
gameViewFor :: UserId -> [(UserId, PlayerId)] -> GameView
gameViewFor userId rows
  | null rows = RedactedView ViewerSpectator
  | null others = FullView
  | null own = RedactedView ViewerSpectator
  | otherwise = RedactedView (ViewerSeats (Set.fromList (map snd own)))
 where
  (own, others) = partition ((== userId) . fst) rows

{- | The caller's 'GameView' of a game, from all of the game's
@arkham_players@ rows (one query by game id; a game has a handful of rows).
'selectList' rather than @getBy UniquePlayer@: that constraint is not in the
database, and Solo games store one row per seat for the same user.
-}
viewerForUser :: MonadIO m => UserId -> ArkhamGameId -> ReaderT SqlBackend m GameView
viewerForUser userId gameId = do
  players <- selectList [ArkhamPlayerArkhamGameId ==. gameId] []
  pure
    $ gameViewFor userId
      [ (arkhamPlayerUserId player, PlayerId (unArkhamPlayerKey key))
      | Entity key player <- players
      ]

{- | A response whose @game@ field is redacted for a 'GameView' when encoded.

The wrapped type, and so its field names and JSON shape, are unchanged; only
the value under @game@ is passed through 'redactFor'. 'FullView' encodes
exactly as the wrapped value does (including its streaming 'toEncoding').
-}
data Redacted a = Redacted GameView a
  deriving stock (Show)

instance ToJSON a => ToJSON (Redacted a) where
  toJSON (Redacted FullView a) = toJSON a
  toJSON (Redacted (RedactedView viewer) a) = redactGameField viewer (toJSON a)
  toEncoding (Redacted FullView a) = toEncoding a
  toEncoding redacted = toEncoding (toJSON redacted)

-- | Redact the value under a response object's @game@ key, if there is one.
redactGameField :: Viewer -> Aeson.Value -> Aeson.Value
redactGameField viewer = \case
  Aeson.Object o
    | Just game <- KeyMap.lookup "game" o ->
        Aeson.Object (KeyMap.insert "game" (redactFor viewer game) o)
  other -> other
