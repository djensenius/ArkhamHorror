{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE NoFieldSelectors #-}
{-# LANGUAGE OverloadedStrings #-}

-- The log reads outside the game payload are filtered per viewer
-- (TASK-2.22.5 P1): GET log/before pages and the log export, through the
-- production WAI application, so the caller's seats come from the real
-- arkham_players rows and the body is the real wire bytes.
module Postgres.LogAudienceSpec (spec) where

import Api.Arkham.Types.MultiplayerVariant (MultiplayerVariant (WithFriends))
import Arkham.Difficulty (Difficulty (Easy))
import Arkham.Game qualified as Game
import Arkham.Id (PlayerId (..))
import Arkham.Log.Entry
import Data.Aeson (Value (..), decode, object, toJSON, (.=))
import Data.Aeson.KeyMap qualified as KeyMap
import Data.Set qualified as Set
import Data.Text qualified as T
import Data.UUID (UUID)
import Data.UUID qualified as UUID
import Database.Persist qualified as P
import Database.Persist.Sql (SqlBackend)
import Entity.Arkham.Game
import Entity.Arkham.LogEntry
import Entity.Arkham.Player
import Entity.User (UserId)
import Foundation (App)
import Network.HTTP.Types (status200)
import Postgres.Harness
import Postgres.Production (authorizationFor, routeRequest)
import Relude
import Test.Hspec
import Yesod.Core (toPathPiece)

type SeedUser = SqlBackend -> Text -> IO UserId

-- | Who a seeded row is for.
data Audience
  = Public
  | NoPayload
  | NoAudience
  | OnlyA
  | OnlyB
  | -- | Seat B's audience on a payload that does not decode as a 'LogEntry',
    -- so a read that let it through would serve its flat body.
    UndecodableB
  | -- | An audience whose tag is neither Everyone nor OnlyPlayer.
    UnknownTag
  | -- | @"audience": null@.
    NullAudience
  | -- | A payload that is a JSON array.
    ArrayPayload
  | -- | A payload that is the jsonb @null@ (not SQL NULL).
    JsonNullPayload
  deriving stock (Eq, Show)

-- | The rows only a full view (or, for 'UndecodableB', seat B) may read.
failClosed :: [Audience]
failClosed = [UndecodableB, UnknownTag, NullAudience, ArrayPayload, JsonNullPayload]

{- | The seeded log, by seq (and step): rows 1-3 are public, a NULL payload and
a payload with no audience; rows 4-8 are the malformed audiences of
'failClosed', each with a sentinel body; 46 rows for seat B alone; 45 rows
alternating seat A alone and public; then 61 more rows for seat B alone. The
page size is 40, so the newest page and the one below the visible block hold
nothing for seat A: a filter after the LIMIT would answer seat A an empty
first page.
-}
seededLog :: [(Int, Audience)]
seededLog =
  [(1, Public), (2, NoPayload), (3, NoAudience)]
    <> zip [4 ..] failClosed
    <> [(n, OnlyB) | n <- [9 .. 54]]
    <> [(n, if odd n then OnlyA else Public) | n <- [55 .. 99]]
    <> [(n, OnlyB) | n <- [100 .. 160]]

label :: Int -> Text
label n = "r" <> T.justifyRight 3 '0' (show n)

-- | A seeded row's label: its flat body, which is all a legacy row shows.
bodyOf :: (Int, Audience) -> Text
bodyOf (n, audience) = case audience of
  UndecodableB -> "secret-b"
  UnknownTag -> "secret-unknown"
  NullAudience -> "secret-null-audience"
  ArrayPayload -> "secret-array"
  JsonNullPayload -> "secret-null-payload"
  _ -> label n

-- | The labels of the seeded rows with these audiences, oldest first.
labelsWith :: (Audience -> Bool) -> [Text]
labelsWith keep = [bodyOf row | row@(_, audience) <- seededLog, keep audience]

-- | The labels of the rows a holder of seat A alone may read, oldest first.
visibleToA :: [Text]
visibleToA = labelsWith (\audience -> audience /= OnlyB && audience `notElem` failClosed)

-- | Every seeded label, which a full view reads.
everyLabel :: [Text]
everyLabel = labelsWith (const True)

-- | The labels of the children of row 1 a holder of seat A alone may read.
childrenForA :: [Text]
childrenForA = ["c-a", "c-all"]

seatUuid :: Word32 -> Word32 -> UUID
seatUuid g = UUID.fromWords 0x2225 0 g

spec :: Harness -> App -> SeedUser -> Spec
spec harness app seedUser = describe "Log reads are filtered for the caller's seats (TASK-2.22.5)" do
  it "pages log/before for seat A without seat B's rows, and without ending early" $
    withConnection harness \connection -> do
      gameId <- seedGame connection 1 "log-audience-pages"
      alice <- seedUser connection "log-audience-pages-alice"
      bob <- seedUser connection "log-audience-pages-bob"
      seat connection 1 gameId alice 1
      seat connection 1 gameId bob 2
      pages <- pageBack alice gameId
      -- Every page but the terminating one has rows: two pages of 40 and 8.
      map length pages `shouldBe` [40, 8, 0]
      let rows = concat (reverse pages)
      map rowLabel rows `shouldBe` map Just visibleToA
      childrenOf "r001" rows `shouldBe` Just childrenForA

  it "gives seat B its own rows and not seat A's" $
    withConnection harness \connection -> do
      gameId <- seedGame connection 2 "log-audience-b"
      alice <- seedUser connection "log-audience-b-alice"
      bob <- seedUser connection "log-audience-b-bob"
      seat connection 2 gameId alice 1
      seat connection 2 gameId bob 2
      rows <- concat . reverse <$> pageBack bob gameId
      -- Seat B's own undecodable row reaches seat B as its flat body; the
      -- other malformed audiences reach nobody but a full view.
      map rowLabel rows
        `shouldBe` map Just
          (labelsWith \audience -> audience `notElem` [OnlyA, UnknownTag, NullAudience, ArrayPayload, JsonNullPayload])
      childrenOf "r001" rows `shouldBe` Just ["c-b", "c-all"]

  it "gives a caller holding every seat the whole log" $
    withConnection harness \connection -> do
      gameId <- seedGame connection 3 "log-audience-full"
      dave <- seedUser connection "log-audience-full-dave"
      seat connection 3 gameId dave 1
      seat connection 3 gameId dave 2
      pages <- pageBack dave gameId
      map length pages `shouldBe` [40, 40, 40, 40, 0]
      let rows = concat (reverse pages)
      map rowLabel rows `shouldBe` map Just everyLabel
      childrenOf "r001" rows `shouldBe` Just ["c-a", "c-b", "c-all"]

  it "filters the JSON and trace log exports the same way" $
    withConnection harness \connection -> do
      gameId <- seedGame connection 4 "log-audience-export"
      alice <- seedUser connection "log-audience-export-alice"
      bob <- seedUser connection "log-audience-export-bob"
      dave <- seedUser connection "log-audience-export-dave"
      seat connection 4 gameId alice 1
      seat connection 4 gameId bob 2
      (status, body) <- request alice (gamePath gameId "/log/export?format=json")
      status `shouldBe` status200
      let rows = fromMaybe [] do
            Object envelope <- decode body
            Array exported <- KeyMap.lookup "rows" envelope
            pure (toList exported)
      map rowLabel rows `shouldBe` map Just visibleToA
      childrenOf "r001" rows `shouldBe` Just childrenForA

      (traceStatus, traceBody) <- request alice (gamePath gameId "/log/export?format=trace")
      traceStatus `shouldBe` status200
      labelsIn (decodeUtf8 traceBody) `shouldBe` Set.fromList (visibleToA <> childrenForA)

      -- A step range is filtered the same way: steps 2-59 for seat A are the
      -- NULL and no-audience rows and 55-59, not the malformed rows 4-8.
      let ranged = "&fromStep=2&untilStep=60"
          inRange = [bodyOf row | row@(n, _) <- seededLog, n >= 2, n < 60]
          visibleInRange = filter (`elem` visibleToA) inRange
      exportedLabels alice gameId ("json" <> ranged) `shouldReturn` map Just visibleInRange
      tracedLabels alice gameId ("trace" <> ranged) `shouldReturn` Set.fromList visibleInRange

      -- A caller holding every seat still exports everything.
      fullGame <- seedGame connection 5 "log-audience-export-full"
      seat connection 5 fullGame dave 1
      seat connection 5 fullGame dave 2
      exportedLabels dave fullGame "json" `shouldReturn` map Just everyLabel
      tracedLabels dave fullGame "trace"
        `shouldReturn` Set.fromList (everyLabel <> ["c-a", "c-b", "c-all"])
      exportedLabels dave fullGame ("json" <> ranged) `shouldReturn` map Just inRange

  it "lists log sections from the banners the caller may read" $
    withConnection harness \connection -> do
      gameId <- seedBanners connection 6 "log-audience-sections"
      alice <- seedUser connection "log-audience-sections-alice"
      bob <- seedUser connection "log-audience-sections-bob"
      seat connection 6 gameId alice 1
      seat connection 6 gameId bob 2
      sectionCount alice gameId `shouldReturn` 1
      sectionCount bob gameId `shouldReturn` 2

      fullGame <- seedBanners connection 7 "log-audience-sections-full"
      dave <- seedUser connection "log-audience-sections-dave"
      seat connection 7 fullGame dave 1
      seat connection 7 fullGame dave 2
      sectionCount dave fullGame `shouldReturn` 2
 where
  request caller path = do
    authorization <- authorizationFor app caller
    routeRequest app "GET" path [authorization] ""
  exportRows caller gameId query = do
    (status, body) <- request caller (gamePath gameId ("/log/export?format=" <> query))
    status `shouldBe` status200
    pure $ fromMaybe [] do
      Object envelope <- decode body
      Array exported <- KeyMap.lookup "rows" envelope
      pure (toList exported)
  exportedLabels caller gameId query = map rowLabel <$> exportRows caller gameId query
  tracedLabels caller gameId query = do
    (status, body) <- request caller (gamePath gameId ("/log/export?format=" <> query))
    status `shouldBe` status200
    pure (labelsIn (decodeUtf8 body))
  sectionCount caller gameId = do
    (status, body) <- request caller (gamePath gameId "/log/sections")
    status `shouldBe` status200
    pure $ maybe (-1) length (decode body >>= arrayOf)
  -- Page back from the end the way the client does, until an empty page.
  -- Each page is newest first in the list of pages; rows in a page are
  -- oldest first.
  pageBack caller gameId = go (10000 :: Int) (10 :: Int)
   where
    go _ 0 = expectationFailure "log/before never ran out" >> pure []
    go cursor budget = do
      (status, body) <- request caller (gamePath gameId ("/log/before/" <> show cursor))
      status `shouldBe` status200
      let rows = fromMaybe [] (decode body >>= arrayOf)
      case mapMaybe rowSeq rows of
        [] -> pure [rows]
        s : rest -> (rows :) <$> go (foldl' min s rest) (budget - 1)
  arrayOf = \case
    Array rows -> Just (toList rows)
    _ -> Nothing

-- | The seq a row stands at: its own @seq@, or the one in a legacy body's label.
rowSeq :: Value -> Maybe Int
rowSeq row = case row of
  Object o
    | Just (Object entry) <- KeyMap.lookup "contents" o
    , Just (Number n) <- KeyMap.lookup "seq" entry ->
        Just (round n)
  _ -> rowLabel row >>= readMaybe . toString . T.drop 1

-- | The label a row was seeded with, structured or legacy.
rowLabel :: Value -> Maybe Text
rowLabel = \case
  Object o -> case (KeyMap.lookup "tag" o, KeyMap.lookup "contents" o) of
    (Just (String "LogRowStructured"), Just (Object entry)) -> entryLabel entry
    (Just (String "LogRowLegacy"), Just (Array parts)) | (String body : _) <- toList parts -> Just body
    _ -> Nothing
  _ -> Nothing

entryLabel :: KeyMap.KeyMap Value -> Maybe Text
entryLabel entry = do
  Array parts <- KeyMap.lookup "body" entry
  Object part : _ <- pure (toList parts)
  String text <- KeyMap.lookup "contents" part
  pure text

-- | The labels of the children of the row labelled @l@.
childrenOf :: Text -> [Value] -> Maybe [Text]
childrenOf l rows = listToMaybe do
  row@(Object o) <- rows
  guard (rowLabel row == Just l)
  Just (Object entry) <- [KeyMap.lookup "contents" o]
  Just (Array children) <- [KeyMap.lookup "children" entry]
  pure [text | Object child <- toList children, Just text <- [entryLabel child]]

-- | Every seeded row and child label in a trace export.
labelsIn :: Text -> Set Text
labelsIn traceText =
  Set.fromList
    [ word
    | word <- words traceText
    , isRowLabel word || T.isPrefixOf "secret-" word || word `elem` ["c-a", "c-b", "c-all"]
    ]
 where
  isRowLabel word = T.length word == 4 && T.isPrefixOf "r" word && T.all digit (T.drop 1 word)
  digit c = c >= '0' && c <= '9'

gamePath :: ArkhamGameId -> Text -> ByteString
gamePath gameId suffix = encodeUtf8 $ "/api/v1/arkham/games/" <> toPathPiece gameId <> suffix

-- | Take seat @n@ of game @g@; its arkham_players id is the seat's player id.
seat :: SqlBackend -> Word32 -> ArkhamGameId -> UserId -> Word32 -> IO ()
seat connection g gameId uid n =
  transaction connection
    $ P.insertKey (ArkhamPlayerKey $ seatUuid g n)
    $ ArkhamPlayer uid gameId (if n == 1 then "c01001" else "c01002")

-- | Game @g@ with 'seededLog' written to arkham_log_entries.
seedGame :: SqlBackend -> Word32 -> Text -> IO ArkhamGameId
seedGame connection g name = do
  now <- currentTimeMicros
  let game = Game.newCampaign "01" Nothing 1 2 Easy False
      seatA = PlayerId (seatUuid g 1)
      seatB = PlayerId (seatUuid g 2)
      entry n audience children =
        (mkLogEntry Narrative [LogText (label n)])
          { logEntrySeq = n
          , logEntryAudience = audience
          , logEntryChildren = children
          }
      child text audience = (mkLogEntry Narrative [LogText text]) {logEntryAudience = audience}
      payloadOf n = \case
        Public
          | n == 1 ->
              Just . toJSON
                $ entry n Everyone [child "c-a" (OnlyPlayer seatA), child "c-b" (OnlyPlayer seatB), child "c-all" Everyone]
          | otherwise -> Just (toJSON (entry n Everyone []))
        NoPayload -> Nothing
        NoAudience -> Just (object ["seq" .= n, "note" .= label n])
        OnlyA -> Just (toJSON (entry n (OnlyPlayer seatA) []))
        OnlyB -> Just (toJSON (entry n (OnlyPlayer seatB) []))
        -- A well-formed audience for seat B, but no body or kind: not a LogEntry.
        UndecodableB -> Just (object ["seq" .= n, "audience" .= OnlyPlayer seatB])
        UnknownTag -> Just (object ["seq" .= n, "audience" .= object ["tag" .= ("Somebody" :: Text)]])
        NullAudience -> Just (object ["seq" .= n, "audience" .= Null])
        ArrayPayload -> Just (toJSON [toJSON (entry n Everyone [])])
        JsonNullPayload -> Just Null
  transaction connection do
    gameId <- P.insert $ ArkhamGame name game 1 WithFriends now now False
    for_ seededLog \row@(n, audience) ->
      P.insert_
        ArkhamLogEntry
          { arkhamLogEntryBody = bodyOf row
          , arkhamLogEntryPayload = payloadOf n audience
          , arkhamLogEntrySeq = Just n
          , arkhamLogEntryGroupId = Nothing
          , arkhamLogEntryArkhamGameId = gameId
          , arkhamLogEntryStep = n
          , arkhamLogEntryCreatedAt = now
          }
    pure gameId

{- | Game @g@ with two scenario banners: one for everyone at step 1, and one
addressed to seat B alone at step 5.
-}
seedBanners :: SqlBackend -> Word32 -> Text -> IO ArkhamGameId
seedBanners connection g name = do
  now <- currentTimeMicros
  let game = Game.newCampaign "01" Nothing 1 2 Easy False
      banner n audience =
        (mkLogEntry Narrative [LogI18n "log.scenarioBegins" mempty])
          {logEntrySeq = n, logEntryAudience = audience}
  transaction connection do
    gameId <- P.insert $ ArkhamGame name game 1 WithFriends now now False
    for_ [(1, Everyone), (5, OnlyPlayer (PlayerId (seatUuid g 2)))] \(n, audience) -> do
      let entry = banner n audience
      P.insert_
        ArkhamLogEntry
          { arkhamLogEntryBody = logEntryToText entry
          , arkhamLogEntryPayload = Just (toJSON entry)
          , arkhamLogEntrySeq = Just n
          , arkhamLogEntryGroupId = Nothing
          , arkhamLogEntryArkhamGameId = gameId
          , arkhamLogEntryStep = n
          , arkhamLogEntryCreatedAt = now
          }
    pure gameId
