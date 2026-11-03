module Api.Handler.Arkham.Decks (
  getApiV1ArkhamDecksR,
  getApiV1ArkhamDeckR,
  postApiV1ArkhamDecksR,
  postApiV1ArkhamDecksFetchR,
  postApiV1ArkhamDecksValidateR,
  deleteApiV1ArkhamDeckR,
  deleteOwnedSavedDeck,
  deleteSavedDeckResponse,
  putApiV1ArkhamGameDecksR,
  postApiV1ArkhamSyncDeckR,
  requireGameDecksAccess,
  killedOrInsaneInvestigatorIds,
  killedOrInsaneInvestigatorIdsFromCampaignLog,
  mustReplaceInvestigatorDeck,
  replacementDeckRejection,
  toDeckErrors,
  putApiV1ArkhamDeckOverlayR,
  deleteApiV1ArkhamDeckOverlayR,
  savePublicSyncedDeck,
  saveDeckOverlay,
  validatePlayableDecklist,
  putApiV1ArkhamDeckUrlR,
  normalizeArkhamBuildDeckCodes,
) where

import Import hiding (delete, on, update, (=.), (==.))
import Import qualified as P

import Api.Arkham.ArkhamDBWriteBack (queueCampaignWriteBack, dispatchCampaignWriteBack)
import Api.Arkham.Deck (DeckError (..), toDeckErrors, validateDeckUpgrade)
import Api.Arkham.Helpers
import Api.Handler.Arkham.CustomCards (registerUserCustomCards)
import Api.Handler.Arkham.Games.Shared (publishToRoom, requireGameAccess)
import Arkham.Campaign.Types (campaignLog)
import Arkham.ArkhamDB.Campaign (withCampaignWriteBackBoundary, campaignWriteBackKey)
import Arkham.ArkhamDB.WriteBack (selectSavedDeckUpgradeTargets, savedDeckUpgradeUpdates)
import Arkham.ArkhamDB.Service (ArkhamDBError (..), adoptDecklist, arkhamDBErrorStatus)
import Entity.Arkham.ArkhamDBAccount
import Arkham.CampaignLog (CampaignLog, campaignLogRecordedSets)
import Arkham.CampaignLogKey (CampaignLogKey (DrivenInsaneInvestigators, KilledInvestigators), recordedCardCodes)
import Arkham.Card.CardCode
import Arkham.Card.CustomCard (arkhamBuildCustomCardCode, isArkhamBuildCardId)
import Arkham.Classes.Entity (attr)
import Arkham.Classes.HasQueue
import Arkham.Custom.Overlay (
  DeckOverlay (..),
  applyOverlay,
  decklistCustomCards,
  investigatorOverlay,
  needsOverlayInvestigator,
 )
import Arkham.Decklist
import Arkham.Game
import Arkham.Game.Diff
import Arkham.Game.State (isChooseDecks)
import Arkham.Game.Utils (gameInvestigators, modeCampaign)
import Arkham.Id
import Arkham.Investigator (decklistInvestigatorError, isKnownInvestigator)
import Arkham.Investigator.Cards (allInvestigatorCards)
import Arkham.Investigator.Types (investigatorDeckUrl, investigatorDrivenInsane, investigatorKilled, investigatorPlayerId)
import Arkham.Message
import Arkham.Queue
import Control.Exception (evaluate)
import Control.Lens (view)
import Control.Monad.Random (mkStdGen)
import Data.Aeson.Key qualified as Key
import Data.Aeson.KeyMap qualified as KeyMap
import Data.Aeson.Types (parseEither)
import Data.ByteString.Lazy qualified as BSL
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Data.Text qualified as T
import Data.Time.Clock
import Data.UUID.V4 qualified as UUID
import Data.Traversable (for)
import Database.Esqueleto.Experimental hiding (isNothing, (<&>))
import Database.Persist qualified as Persist
import Database.Persist.Sql (rawSql)
import Entity.Arkham.Step
import Json hiding (Success)
import Network.HTTP.Conduit (
  httpLbs,
  newManager,
  parseRequest,
  requestHeaders,
  responseBody,
  simpleHttp,
  tlsManagerSettings,
 )
import Network.HTTP.Types
import Network.HTTP.Types.Status qualified as Status
import UnliftIO.Exception (try)

getApiV1ArkhamDecksR :: Handler [Entity ArkhamDeck]
getApiV1ArkhamDecksR = do
  userId <- getRequestUserId
  -- A deck's play list is computed as it is served, and an overlay's custom
  -- investigator has to be resolvable for its signatures to come with it.
  registerUserCustomCards userId
  runDB $ select do
    decks <- from $ table @ArkhamDeck
    where_ $ decks.userId ==. val userId
    pure decks

-- | Delegates to 'Api.Handler.Arkham.Games.Shared.requireGameAccess'.
-- Preserved for callers and tests already targeting this name.
requireGameDecksAccess :: Monad m => Bool -> m Bool -> m () -> m ()
requireGameDecksAccess = requireGameAccess

-- | True when the decklist upgrades the SAME investigator (an alternate art of them counts)
-- rather than replacing them with a different one.
sameInvestigator :: InvestigatorId -> ArkhamDBDecklist -> Bool
sameInvestigator iid decklist =
  iid
    == decklist.investigator
    || maybe
      False
      ((toCardCode iid `elem`) . (.cardCodes))
      (Map.lookup (toCardCode decklist.investigator) allInvestigatorCards)

killedOrInsaneInvestigatorIdsFromCampaignLog :: CampaignLog -> Set InvestigatorId
killedOrInsaneInvestigatorIdsFromCampaignLog campaignLog' =
  let sets = campaignLogRecordedSets campaignLog'
      killed = recordedCardCodes $ Map.findWithDefault [] KilledInvestigators sets
      insane = recordedCardCodes $ Map.findWithDefault [] DrivenInsaneInvestigators sets
   in Set.fromList $ map InvestigatorId (killed <> insane)

mustReplaceInvestigatorDeck :: InvestigatorId -> Bool -> Set InvestigatorId -> Bool
mustReplaceInvestigatorDeck investigatorId killedOrDrivenInsane killedOrInsane =
  investigatorId `Set.member` killedOrInsane || killedOrDrivenInsane

killedOrInsaneInvestigatorIds :: Game -> Set InvestigatorId
killedOrInsaneInvestigatorIds game =
  let activeKilledOrInsane = Map.keysSet $ Map.filter killedOrDrivenInsane (gameInvestigators game)
      campaignKilledOrInsane = maybe mempty (killedOrInsaneInvestigatorIdsFromCampaignLog . attr campaignLog) $ modeCampaign game.gameMode
   in activeKilledOrInsane <> campaignKilledOrInsane
 where
  killedOrDrivenInsane investigator =
    attr investigatorKilled investigator || attr investigatorDrivenInsane investigator

replacementDeckRejection :: InvestigatorId -> Bool -> Set InvestigatorId -> Set InvestigatorId -> Maybe ArkhamDBDecklist -> Maybe Text
replacementDeckRejection investigatorId mustReplace killedOrInsane takenInvestigators mDecklist = case mDecklist of
  Nothing
    | mustReplace -> Just replacementRequiredMessage
    | otherwise -> Nothing
  Just decklist
    | sameInvestigator investigatorId decklist ->
        if mustReplace then Just replacementRequiredMessage else Nothing
    | any (`sameInvestigator` decklist) takenInvestigators -> Just "This investigator is already taken"
    | any (`sameInvestigator` decklist) killedOrInsane -> Just "That investigator was killed or driven insane"
    | otherwise -> Nothing
 where
  replacementRequiredMessage = "That investigator was killed or driven insane and must be replaced"

data CreateDeckPost = CreateDeckPost
  { deckId :: Text
  , deckName :: Text
  , deckUrl :: Maybe Text
  , deckList :: ArkhamDBDecklist
  , deckOverlay :: Maybe DeckOverlay
  }
  deriving stock (Show, Generic)
  deriving anyclass FromJSON

newtype ValidateDeckPost = ValidateDeckPost
  { validateDeckList :: ArkhamDBDecklist
  }
  deriving stock (Show, Generic)
  deriving anyclass FromJSON

newtype FetchDeckPost = FetchDeckPost
  { fetchDeckUrl :: Text
  }
  deriving stock (Show, Generic)

instance FromJSON FetchDeckPost where
  parseJSON = genericParseJSON $ aesonOptions $ Just "fetchDeck"

newtype UpdateDeckUrlPost = UpdateDeckUrlPost
  { updateDeckUrl :: Text
  }
  deriving stock (Show, Generic)

instance FromJSON UpdateDeckUrlPost where
  parseJSON = genericParseJSON $ aesonOptions $ Just "updateDeck"

data UpgradeDeckPost = UpgradeDeckPost
  { udpInvestigatorId :: InvestigatorId
  , udpDeckUrl :: Maybe Text
  , udpDeckList :: Maybe ArkhamDBDecklist
  }
  deriving stock (Show, Generic)

instance FromJSON UpgradeDeckPost where
  parseJSON = genericParseJSON $ aesonOptions $ Just "udp"

postApiV1ArkhamDecksR :: Handler (Entity ArkhamDeck)
postApiV1ArkhamDecksR = do
  userId <- getRequestUserId
  postData <- requireCheckJsonBody
  -- The overlay may name cards only this user has, so the library has to be
  -- resolvable before the list is checked or stored.
  registerUserCustomCards userId
  attachment <- liftIO UUID.nextRandom
  let base = fromPostData userId postData
      deck = maybe base (\overlay -> attachDeckOverlay attachment overlay base) $ deckOverlay postData
  case toDeckErrors (arkhamDeckPlayList deck) of
    [] -> runDB $ insertEntity deck
    err -> sendStatusJSON status400 err

postApiV1ArkhamDecksValidateR :: Handler ()
postApiV1ArkhamDecksValidateR = do
  userId <- getRequestUserId
  -- The deck may name cards only this user has -- their own, or a set they are
  -- subscribed to -- so the library has to be resolvable before it is checked,
  -- or every one of them reads as a card that is not implemented.
  registerUserCustomCards userId
  decklist <- requireCheckJsonBody
  case toDeckErrors decklist of
    [] -> sendStatusJSON status200 ()
    err -> sendStatusJSON status400 err

postApiV1ArkhamDecksFetchR :: Handler ArkhamDBDecklist
postApiV1ArkhamDecksFetchR = do
  userId <- getRequestUserId
  registerUserCustomCards userId
  FetchDeckPost {..} <- requireCheckJsonBody
  fetchDecklistFor fetchDeckUrl

{- | Load an upgraded (or replacement) deck into a running campaign.

Every failure here has to be inert: the deck is fetched and validated BEFORE the game is
touched, the engine run is wrapped so a throwing message can never be half-persisted, and
anything that goes wrong is reported as a real status code instead of a bare 500. The
reporter of #5256 hit the opposite of all three -- an upgrade that appeared to do nothing,
so they submitted it again, which re-ran the whole load on an already-upgraded deck.
-}
putApiV1ArkhamGameDecksR :: ArkhamGameId -> Handler ()
putApiV1ArkhamGameDecksR gameId = do
  Entity userId user <- getRequestUser
  requireGameDecksAccess
    user.admin
    (isJust <$> runDB (getBy $ UniquePlayer userId gameId))
    notFound
  postData <- requireCheckJsonBody
  {- The upgraded list -- or a retained overlay -- may name cards, or an
  investigator, only this user has, so the library has to be resolvable before it
  is validated and before the engine builds anything from it. Without this a deck
  naming a card built since the campaign started read as unimplemented, and a
  replacement custom investigator resolved to the missing-card stand-in (#5891). -}
  registerUserCustomCards userId
  now <- liftIO getCurrentTime

  -- Resolve the deck before opening the game transaction. Fetching inside it held the
  -- game's row lock across an ArkhamDB round trip, and every failure (site unreachable,
  -- malformed json, a card the engine does not implement) was a bare `error`: an opaque
  -- 500 with nothing for the player to act on.
  mDecklist <- resolveDecklist postData

  outcome <- runDB $ atomicallyWithGame gameId \gameEntity@ArkhamGame {..} -> do
    mLastStep <- getBy (UniqueStep gameId arkhamGameStep)
    let Game {..} = arkhamGameCurrentData
    let investigatorId = udpInvestigatorId postData
    let currentQueue = maybe [] (choiceMessages . arkhamStepChoice . entityVal) mLastStep

    -- A stale client (this campaign swaps which investigators exist between scenarios)
    -- would otherwise crash on getPlayer deep inside the engine.
    case Map.lookup investigatorId (gameInvestigators arkhamGameCurrentData) of
      Nothing -> pure $ Left (Status.status400, "That investigator is not in this game")
      Just investigatorEntity -> do
        let playerId = attr investigatorPlayerId investigatorEntity

        {- The deck this seat plays may have no investigator of its own: one built at
        arkham.build on a placeholder, with the real investigator laid over it. The
        overlay is saved on the deck row, so the upgraded list has to go through it as
        the original did, or the engine is handed the placeholder and throws from deep
        inside building it (#5891). Found by owner + the url the game is using, the same
        link the write-back below walks forward; among rows sharing a url, the one whose
        deck plays as this investigator. -}
        deckRows <- case attr investigatorDeckUrl investigatorEntity of
          Nothing -> pure []
          Just deckUrl -> select do
            decks <- from $ table @ArkhamDeck
            where_ $ decks.userId ==. val userId
            where_ $ decks.url ==. val (Just deckUrl)
            pure decks
        let savedOverlay = do
              row <-
                find ((== investigatorId) . (.investigator) . arkhamDeckPlayList . entityVal) deckRows
                  <|> listToMaybe deckRows
              arkhamDeckOverlay (entityVal row)

        {- A list that brings no investigator this app can build is never a replacement:
        an upgrade ask is about this seat, and the caller's whole library is registered
        above, so there is nothing else it could mean. The seat keeps its investigator --
        and gets its signatures back, which an arkham.build list cannot name. -}
        let overlayFor decklist
              | isJust (savedOverlay >>= overlayInvestigator) = savedOverlay
              | needsOverlayInvestigator decklist || not (isKnownInvestigator decklist.investigator) =
                  Just
                    $ maybe
                      (investigatorOverlay investigatorId)
                      (\o -> o {overlayInvestigator = Just investigatorId})
                      savedOverlay
              | otherwise = savedOverlay

        -- What the engine loads. The raw list is what the deck row keeps, so the overlay
        -- stays beside it rather than being folded in (see 'putApiV1ArkhamDeckOverlayR').
        let mPlayList = (\decklist -> maybe id applyOverlay (overlayFor decklist) decklist) <$> mDecklist

        -- A seat is owed a deck exactly while its own deck question is parked: answering
        -- drops it (and re-parks only the seats still waiting). Without this check a
        -- resubmit -- the client's upgrade buttons come back as soon as the request
        -- resolves, so a player who misses the websocket update clicks again -- re-ran the
        -- load against an already-upgraded deck, duplicating every card in it and (before
        -- the runMessages guard) destroying the campaign's parked question (#5256). Game
        -- state alone is too coarse: a multiplayer upgrade window stays in IsChooseDecks
        -- until the last seat answers. Report success either way so the redundant click
        -- still re-syncs the client from the publish below.
        if
          | not (isChooseDecks gameGameState) -> pure $ Right gameEntity
          | not (maybe False isDeckQuestion $ Map.lookup playerId gameQuestion) -> pure $ Right gameEntity
          -- Nothing is written yet, so saying no here leaves the upgrade to be retried.
          -- Without it the engine builds the stand-in for a def it does not have and the
          -- first read of it is a 500 with a call stack in it (#5891).
          | Just message <- mPlayList >>= decklistInvestigatorError ->
              pure $ Left (Status.status400, message)
          | otherwise -> do
              let killedOrInsane = killedOrInsaneInvestigatorIds arkhamGameCurrentData
                  mustReplace =
                    mustReplaceInvestigatorDeck
                      investigatorId
                      (attr investigatorKilled investigatorEntity || attr investigatorDrivenInsane investigatorEntity)
                      killedOrInsane
                  takenInvestigators = Set.delete investigatorId $ Map.keysSet $ gameInvestigators arkhamGameCurrentData
              -- Same-or-replacement is a question about the deck as played, so it is
              -- asked of the list with its overlay laid over it.
              case replacementDeckRejection investigatorId mustReplace killedOrInsane takenInvestigators mPlayList of
                Just reason -> pure $ Left (Status.status400, reason)
                Nothing -> do
                  -- The engine can throw (an unimplemented card, an unusable decklist, an
                  -- unexpected game state). Keep the whole run -- including the undo diff, which
                  -- forces the resulting game -- inside the catch, so a failure leaves the row
                  -- exactly as it was rather than persisting a partly-applied upgrade.
                  upgraded <- liftIO $ try @_ @SomeException do
                    gameRef <- newIORef arkhamGameCurrentData
                    queueRef <- newQueue currentQueue
                    genRef <- newIORef $ mkStdGen gameSeed
                    writeBackBoundary <- withCampaignWriteBackBoundary gameRef \observers ->
                      runGameApp (GameApp gameRef queueRef genRef (pure . const ()) Nothing) do
                        let question' = Map.delete playerId gameQuestion
                        unless (Map.null question') (push $ AskMap question')
                        -- No deck at all is the "continue without upgrading" path: push nothing
                        -- and let the parked continuation run.
                        for_ mPlayList \decklist ->
                          -- Registering first, so a card the upgrade brings in is recorded on
                          -- the game as a seated deck's are: the registry it resolved against
                          -- here is process-global and rebuilt from the game, so a card only in
                          -- it is a "Missing card" to every other client and after a restart.
                          pushAll
                            $ map DebugRegisterCustomCard (decklistCustomCards decklist)
                            <> [ if sameInvestigator investigatorId decklist
                                   then UpgradeDecklist investigatorId decklist
                                   else ReplaceInvestigator investigatorId decklist
                               ]
                        runMessages (gameIdToText gameId) observers
                    ge <- readIORef gameRef
                    updatedQueue <- readIORef (queueToRef queueRef)
                    diffDown <- evaluate $ diff ge arkhamGameCurrentData
                    pure (ge, updatedQueue, diffDown, writeBackBoundary)


                  -- Prepare all saved-list adoptions before writing either the
                  -- game or a deck. Copying a push marker first would hide an
                  -- unconsumed incorporated overlay from subsequent sync.
                  -- Whether this is the same investigator is asked of the deck as played;
                  -- what the row adopts is the RAW list, so the overlay stays beside it.
                  prepared <- case (mDecklist, mPlayList) of
                    (Just decklist, Just playList)
                      | sameInvestigator investigatorId playList -> do
                      -- Write-back history must never suppress ordinary saved
                      -- upgrades. The part-aware key cannot select a legacy
                      -- full-DE association; optional enqueue has its own fence.
                      targets <- selectSavedDeckUpgradeTargets
                        (\key -> fmap entityVal <$> getBy (UniqueArkhamDBCampaignDeck key))
                        Persist.get
                        (\url -> P.selectList [ArkhamDeckUserId P.==. userId, ArkhamDeckUrl P.==. Just url] [])
                        userId (campaignWriteBackKey gameId arkhamGameCurrentData investigatorId)
                        (attr investigatorDeckUrl investigatorEntity)
                      fmap sequence $ for (sort targets) \deckId -> do
                        -- Overlay editing writes this row too. Read under its
                        -- lock so consumption cannot erase a concurrent edit.
                        -- NO KEY UPDATE (non-key columns only) still excludes
                        -- overlay/sync and delete, but not another game's
                        -- enqueue key-share fence (which would SKIP its push).
                        locked <- rawSql "SELECT ?? FROM arkham_decks WHERE id = ? FOR NO KEY UPDATE"
                          [toPersistValue deckId] :: DB [Entity ArkhamDeck]
                        pure $ case viaNonEmpty head locked of
                          Nothing -> Left $ ArkhamDBError "notFound" "Saved deck no longer exists"
                          Just (Entity _ deck) -> (deckId,) <$> savedDeckUpgradeUpdates validateDeckUpgrade now deck decklist
                    _ -> pure $ Right []

                  case (prepared, upgraded) of
                    (Left err, _) -> pure $ Left (arkhamDBErrorStatus err, arkhamDBErrorMessage err)
                    -- First line only: engine `error`s carry a call stack the player cannot use.
                    (_, Left err) ->
                      pure $ Left (Status.status500, "Could not upgrade deck: " <> T.takeWhile (/= '\n') (tshow err))
                    (Right savedUpdates, Right (ge, updatedQueue, diffDown, writeBackBoundary)) -> do
                      let g' =
                            ArkhamGame
                              arkhamGameName
                              ge
                              (arkhamGameStep + 1)
                              arkhamGameMultiplayerVariant
                              arkhamGameCreatedAt
                              now
                              arkhamGameAllowSpectators

                      replace gameId g'
                      insert_
                        $ ArkhamStep
                          gameId
                          (Choice diffDown updatedQueue)
                          (arkhamGameStep + 1)
                          (ActionDiff $ view actionDiffL ge)

                      -- A campaign copy keeps the live investigator's old URL.
                      -- Prefer its owned association; URL matching is only for
                      -- saved decks without a write-back association (#5257).
                      -- Replacements are different decks, not upgrades of this one.
                      for_ savedUpdates \(deckId, updates) -> P.update deckId updates

                      for_ writeBackBoundary \(boundary, snapshot) ->
                        queueCampaignWriteBack gameId (arkhamGameStep + 1) now boundary snapshot
                      pure $ Right g'


  case outcome of
    Left (status, message) -> sendStatusJSON status (JSONError message)
    Right ArkhamGame {..} -> do
      dispatchCampaignWriteBack
      publishToRoom gameId
        $ GameUpdate
        $ PublicGame gameId arkhamGameName mempty arkhamGameCurrentData
 where
  resolveDecklist :: UpgradeDeckPost -> Handler (Maybe ArkhamDBDecklist)
  resolveDecklist postData = case udpDeckList postData of
    Just decklist -> Just <$> validateDecklist decklist
    -- Neither a decklist nor a url: "continue without upgrading".
    Nothing -> for (udpDeckUrl postData) (fetchDecklistFor >=> validateDecklist)

deckError :: Text -> Handler a
deckError = sendStatusJSON Status.status400 . JSONError

{- | Fetch a decklist from a url, saying why it could not be read.

'getDeckList' THROWS on a non-2xx response rather than returning Left, so every
caller has to catch one: a url that no longer resolves otherwise escapes as a bare
500 with nothing the player can act on. Deleting an upgrade on ArkhamDB deletes the
deck it made, and a campaign upgrade walks the saved row's url forward onto exactly
those decks (see 'putApiV1ArkhamGameDecksR'), so a dead url is the ordinary case
here rather than a corner one.
-}
fetchDecklistFor :: Text -> Handler ArkhamDBDecklist
fetchDecklistFor url =
  try @_ @SomeException (getDeckList url) >>= \case
    Right (Right decklist) -> pure decklist
    Right (Left err) -> deckError $ "Could not read the deck at " <> url <> ": " <> T.pack err
    Left _ ->
      deckError
        $ "Could not fetch the deck at "
        <> url
        <> ". If it has been deleted, point this deck at one that still exists."

validateDecklist :: ArkhamDBDecklist -> Handler ArkhamDBDecklist
validateDecklist decklist = case toDeckErrors decklist of
  [] -> pure decklist
  errs ->
    deckError
      $ "This deck contains cards that are not implemented yet: "
      <> T.intercalate ", " [tshow cCode | UnimplementedCard cCode <- errs]

fromPostData :: UserId -> CreateDeckPost -> ArkhamDeck
fromPostData userId CreateDeckPost {..} = do
  ArkhamDeck
    { arkhamDeckUserId = userId
    , arkhamDeckUrl = deckUrl
    , arkhamDeckInvestigatorName = tshow $ investigator_name deckList
    , arkhamDeckName = deckName
    , arkhamDeckList = deckList
    , arkhamDeckOverlay = Nothing
    , arkhamDeckOverlayAttachmentId = Nothing
    , arkhamDeckLastUsedAt = Nothing
    }

arkhamBuildDecklistUrl :: Text -> Maybe Text
arkhamBuildDecklistUrl url = do
  rest <-
    asum
      $ map
        (`T.stripPrefix` url)
        [ "https://arkham.build/decklist/view/"
        , "https://arkham.build/decklist/"
        , "http://arkham.build/decklist/view/"
        , "http://arkham.build/decklist/"
        ]
  let decklistId = T.takeWhile (/= '?') $ T.takeWhile (/= '/') rest
  guard $ not $ T.null decklistId
  pure $ "https://api.arkham.build/v1/public/arkhamdb/decklist/" <> decklistId

{- | Read a decklist off the wire, translating its arkham.build ids first.

The translation has to happen here, on the raw json, rather than on the decoded
decklist: 'FromJSON CardCode' strips every leading @c@ (the one 'ToJSON' puts on
so a code never serializes as a bare number), and an arkham.build id is a raw
uuid that was never given one. An id beginning with @c@ -- Jane Doe's
@c13c4114-...@ -- therefore arrives as @13c4114-...@, which is no longer an id
'isArkhamBuildCardId' recognizes, so it was never translated and the card read as
one that does not exist (#5891).
-}
decodeDeckList :: BSL.ByteString -> Either String ArkhamDBDecklist
decodeDeckList bytes = do
  value <- eitherDecode bytes
  -- A url can answer with the deck itself or with a list of them.
  deckValue <- case value of
    Json.Array decks -> maybe (Left "No decklist found") Right (listToMaybe $ toList decks)
    v -> Right v
  -- 'parseEither' rather than 'fromJSON': this module hides aeson's 'Success'.
  parseEither parseJSON (normalizeArkhamBuildDeckCodes deckValue)

{- | Rewrite the arkham.build card ids in a decklist to the codes an import of
that pack gave those cards.

arkham.build names a custom card by its own bare id; nothing here will try a
custom-card lookup unless the code carries the custom prefix, so a deck naming
those cards reads as a deck of cards that do not exist -- 'UnimplementedCard' --
even with the pack imported and sitting in the library.

On the json rather than on the decoded decklist, because that is the only place
an id is still intact -- see 'decodeDeckList' -- and because it is where the
client does the same rewrite (@normalizeArkhamBuildDeckCodes@ in
@arkhamBuildImport.ts@), which this has to agree with exactly. Applied wherever a
decklist is read off the wire rather than only in the client, since the client is
not the only thing that reads one: syncing a deck fetches it here and writes it
straight to the row. A decklist of printed cards comes through untouched, and a
translated one translates to itself.
-}
normalizeArkhamBuildDeckCodes :: Json.Value -> Json.Value
normalizeArkhamBuildDeckCodes = \case
  -- Qualified: esqueleto has a `Value` of its own, and this module imports both.
  Json.Object o ->
    Json.Object
      $ rewrite "slots" translateSlots
      $ rewrite "sideSlots" translateSlots
      $ rewrite "investigator_code" translateCode
      $ rewrite "meta" translateMeta o
  v -> v
 where
  rewrite key f o = maybe o (\v -> KeyMap.insert key (f v) o) (KeyMap.lookup key o)

  translate t
    | isArkhamBuildCardId t = unCardCode $ arkhamBuildCustomCardCode t
    | otherwise = t

  translateCode = \case
    Json.String t -> Json.String (translate t)
    v -> v

  -- The quantities are the values; the cards are the keys.
  translateSlots = \case
    Json.Object slots ->
      Json.Object
        $ KeyMap.fromList [(Key.fromText $ translate $ Key.toText k, v) | (k, v) <- KeyMap.toList slots]
    v -> v

  {- The alternate front an investigator was built with is named inside `meta`,
  which rides along as its own blob of json -- a string of it, from arkham.build.
  Left exactly as it arrived unless there is something in it to rewrite. -}
  translateMeta = \case
    Json.String raw -> Json.String $ fromMaybe raw do
      m <- decode @(Map Text Json.Value) (BSL.fromStrict $ encodeUtf8 raw)
      front <- Map.lookup "alternate_front" m
      let front' = translateCode front
      guard $ front' /= front
      pure $ decodeUtf8 $ BSL.toStrict $ encode $ Map.insert "alternate_front" front' m
    Json.Object m -> Json.Object $ rewrite "alternate_front" translateCode m
    v -> v

getDeckList :: MonadIO m => Text -> m (Either String ArkhamDBDecklist)
getDeckList url = liftIO case arkhamBuildDecklistUrl url of
  Just fetchUrl -> do
    request <- parseRequest $ T.unpack fetchUrl
    manager <- newManager tlsManagerSettings
    let request' = request {requestHeaders = ("X-Client-Id", "arkham-horror") : requestHeaders request}
    second (\d -> d {url = Nothing})
      . decodeDeckList
      . responseBody
      <$> httpLbs request' manager
  Nothing ->
    second (\d -> d {url = Just url})
      . decodeDeckList
      <$> simpleHttp (T.unpack url)

getApiV1ArkhamDeckR :: ArkhamDeckId -> Handler (Entity ArkhamDeck)
getApiV1ArkhamDeckR deckId = do
  userId <- getRequestUserId
  registerUserCustomCards userId
  mDeck <- runDB $ selectOne do
    decks <- from $ table @ArkhamDeck
    where_ $ decks.id ==. val deckId
    where_ $ decks.userId ==. val userId
    pure decks
  maybe notFound pure mDeck

deleteApiV1ArkhamDeckR :: ArkhamDeckId -> Handler ()
deleteApiV1ArkhamDeckR deckId = do
  userId <- getRequestUserId
  deleteSavedDeckResponse $ runDB $ deleteOwnedSavedDeck userId deckId

deleteOwnedSavedDeck :: UserId -> ArkhamDeckId -> DB Bool
deleteOwnedSavedDeck userId deckId = do
  -- Excludes enqueue's key-share fence until the usage check and DELETE
  -- commit together. A foreign/missing id remains a non-disclosing no-op.
  -- Must stay FOR UPDATE: a DELETE is a key change, unlike lockDeck's writers.
  locked <- rawSql "SELECT ?? FROM arkham_decks WHERE id = ? AND user_id = ? FOR UPDATE"
    [toPersistValue deckId, toPersistValue userId] :: DB [Entity ArkhamDeck]
  if null locked then pure True else do
    campaigns <- Persist.count [ArkhamDBCampaignDeckDeckId P.==. deckId]
    outstanding <- Persist.count [ArkhamDBPushDeckId P.==. deckId, ArkhamDBPushStatus P./<-. ["succeeded", "cancelled"]]
    if campaigns > 0 || outstanding > 0 then pure False else do
      Persist.delete deckId
      pure True

-- Shared actual HTTP response boundary; SQL ownership/usage checks above stay
-- in the locked transaction, not in a client-side or pre-I/O projection.
deleteSavedDeckResponse :: MonadHandler m => m Bool -> m ()
deleteSavedDeckResponse remove = do
  removed <- remove
  unless removed $ sendStatusJSON Status.status409 $
    JSONError "Saved deck has campaign accounting or unresolved write-back; deletion requires reconciliation"

newtype JSONError = JSONError {errorMsg :: Text}
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)

{- | Refetch a deck from the url it was imported from.

Every failure here used to come out the same way: nothing. 'getDeckList' throws
rather than returning Left, so a dead url was an unhandled 500, and both client
call sites were a bare @.then@ -- the player pressed Sync and the page did not
move. 'fetchDecklistFor' reports the reason instead.
-}
postApiV1ArkhamSyncDeckR :: ArkhamDeckId -> Handler (Entity ArkhamDeck)
postApiV1ArkhamSyncDeckR deckId = do
  userId <- getRequestUserId
  registerUserCustomCards userId
  deck <- ownedDeck deckId
  case arkhamDeckUrl deck of
    Nothing ->
      deckError
        "This deck was not imported from a url, so there is nothing to sync from. Point it at one first."
    Just deckUrl -> do
      decklist <- fetchDecklistFor deckUrl
      result <- runDB $ savePublicSyncedDeck userId deckId decklist
      either (\err -> sendStatusJSON (arkhamDBErrorStatus err) $ JSONError $ arkhamDBErrorMessage err) pure result

{- | Point a deck at a different url, and refetch it from there.

A campaign upgrade rewrites the saved row in place and walks its url forward onto
each new ArkhamDB deck, so a player who deletes those upgrades upstream is left
holding an upgraded list whose url no longer exists -- with no way back to the deck
they built. Repointing is that way back. The list is refetched in the same call so
the url and the list cannot disagree, and both the fetch and the validation run
before anything is written, so a bad url leaves the deck exactly as it was.

The new list is adopted under the row lock exactly as a sync's is, so a retained
overlay and its write-back provenance survive the repoint.
-}
putApiV1ArkhamDeckUrlR :: ArkhamDeckId -> Handler (Entity ArkhamDeck)
putApiV1ArkhamDeckUrlR deckId = do
  userId <- getRequestUserId
  deck <- ownedDeck deckId
  UpdateDeckUrlPost {..} <- requireCheckJsonBody
  let deckUrl = T.strip updateDeckUrl
  when (T.null deckUrl) $ deckError "Enter the url of the deck to point this at"
  -- The deck may name cards only this user has built.
  registerUserCustomCards (arkhamDeckUserId deck)
  decklist <- fetchDecklistFor deckUrl >>= validateDecklist
  -- An arkham.build decklist carries no url of its own (see 'getDeckList'), so the
  -- one the player gave us is the only one there is.
  let deckUrl' = fromMaybe deckUrl decklist.url
  result <- runDB $ saveAdoptedDeck [ArkhamDeckUrl P.=. Just deckUrl'] userId deckId decklist
  either (\err -> sendStatusJSON (arkhamDBErrorStatus err) $ JSONError $ arkhamDBErrorMessage err) pure result

ownedDeck :: ArkhamDeckId -> Handler ArkhamDeck
ownedDeck deckId = do
  userId <- getRequestUserId
  deck <- runDB $ get404 deckId
  unless (arkhamDeckUserId deck == userId)
    $ sendStatusJSON Status.status400 (JSONError "Deck does not belong to this user")
  pure deck

-- These writers share linked sync's deck-row lock. Validation must use the
-- locked current base/overlay, not a projection prepared before waiting.
lockedOwnedDeck :: UserId -> ArkhamDeckId -> ReaderT SqlBackend Handler ArkhamDeck
lockedOwnedDeck userId deckId = lockDeck deckId >>= \case
  Nothing -> lift notFound
  Just deck -> do
    requireDeckOwner userId deck
    pure deck

-- NO KEY UPDATE: these writers only UPDATE non-key columns (the primary key is
-- the table's only unique index). It still serializes them with each other,
-- linked sync and delete, but not with gameplay enqueue's FOR KEY SHARE SKIP
-- LOCKED fence, which would otherwise silently skip that deck's push.
lockDeck :: ArkhamDeckId -> DB (Maybe ArkhamDeck)
lockDeck deckId = do
  rows <- rawSql "SELECT ?? FROM arkham_decks WHERE id = ? FOR NO KEY UPDATE"
    [toPersistValue deckId] :: DB [Entity ArkhamDeck]
  pure $ entityVal <$> viaNonEmpty head rows

requireDeckOwner :: UserId -> ArkhamDeck -> ReaderT SqlBackend Handler ()
requireDeckOwner userId deck =
  unless (arkhamDeckUserId deck == userId)
    $ lift $ sendStatusJSON Status.status400 (JSONError "Deck does not belong to this user")

validatePlayableDecklist :: ArkhamDBDecklist -> Handler ()
validatePlayableDecklist decklist = case toDeckErrors decklist of
  [] -> pure ()
  errors -> sendStatusJSON Status.status400 errors

savePublicSyncedDeck :: UserId -> ArkhamDeckId -> ArkhamDBDecklist -> ReaderT SqlBackend Handler (Either ArkhamDBError (Entity ArkhamDeck))
savePublicSyncedDeck = saveAdoptedDeck []

-- | Adopt a fetched list into a saved deck under its row lock, with any extra
-- updates (a repointed url) written in the same statement.
saveAdoptedDeck :: [Update ArkhamDeck] -> UserId -> ArkhamDeckId -> ArkhamDBDecklist -> ReaderT SqlBackend Handler (Either ArkhamDBError (Entity ArkhamDeck))
saveAdoptedDeck extraUpdates userId deckId decklist = lockDeck deckId >>= \case
  Nothing -> pure $ Left $ ArkhamDBError "notFound" "Saved deck no longer exists"
  Just current -> do
    requireDeckOwner userId current
    case adoptDecklist current decklist of
      Left err -> pure $ Left err
      Right adopted -> do
        lift $ validatePlayableDecklist $ arkhamDeckPlayList adopted
        P.update deckId
          $ [ ArkhamDeckList P.=. arkhamDeckList adopted
            , ArkhamDeckOverlay P.=. arkhamDeckOverlay adopted
            , ArkhamDeckOverlayAttachmentId P.=. arkhamDeckOverlayAttachmentId adopted
            ]
          <> extraUpdates
        Right . Entity deckId <$> getJust deckId

saveDeckOverlay :: UserId -> ArkhamDeckId -> Maybe DeckOverlay -> ReaderT SqlBackend Handler (Entity ArkhamDeck)
saveDeckOverlay userId deckId overlay = do
  current <- lockedOwnedDeck userId deckId
  -- A PUT is a new attachment even when its JSON equals an earlier one;
  -- removing the overlay also removes its attachment identity.
  updated <- case overlay of
    Nothing -> pure current {arkhamDeckOverlay = Nothing, arkhamDeckOverlayAttachmentId = Nothing}
    Just value -> liftIO UUID.nextRandom <&> \attachment -> attachDeckOverlay attachment value current
  lift $ validatePlayableDecklist $ arkhamDeckPlayList updated
  P.update deckId
    [ ArkhamDeckOverlay P.=. arkhamDeckOverlay updated
    , ArkhamDeckOverlayAttachmentId P.=. arkhamDeckOverlayAttachmentId updated
    ]
  Entity deckId <$> getJust deckId

{- | Lay an overlay over a deck, or take it off again.

The deck's own list is untouched either way, so removing the overlay leaves the
deck exactly as it was and a later sync from ArkhamDB keeps the overlay.
-}
putApiV1ArkhamDeckOverlayR :: ArkhamDeckId -> Handler (Entity ArkhamDeck)
putApiV1ArkhamDeckOverlayR deckId = do
  deck <- ownedDeck deckId
  overlay <- requireCheckJsonBody
  registerUserCustomCards (arkhamDeckUserId deck)
  runDB $ saveDeckOverlay (arkhamDeckUserId deck) deckId $ Just overlay

deleteApiV1ArkhamDeckOverlayR :: ArkhamDeckId -> Handler (Entity ArkhamDeck)
deleteApiV1ArkhamDeckOverlayR deckId = do
  deck <- ownedDeck deckId
  registerUserCustomCards (arkhamDeckUserId deck)
  runDB $ saveDeckOverlay (arkhamDeckUserId deck) deckId Nothing
