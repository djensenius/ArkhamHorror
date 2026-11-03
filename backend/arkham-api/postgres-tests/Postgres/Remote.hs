{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE NoFieldSelectors #-}
{-# LANGUAGE OverloadedStrings #-}

-- Only ArkhamDB HTTP state is simulated. Never substitute local persistence.
module Postgres.Remote where

import Api.Arkham.ArkhamDBStore (persistentStore)
import Api.Arkham.ArkhamDBWriteBack (writeBackStore)
import Arkham.ArkhamDB
import Arkham.ArkhamDB.WriteBack (runWriteBack)
import Arkham.Decklist qualified as Deck
import Control.Concurrent.MVar qualified as MVar
import Data.Map.Strict qualified as Map
import Data.Time.Clock (addUTCTime)
import Database.Persist qualified as P
import Database.Persist.Sql (SqlBackend)
import Entity.Arkham.ArkhamDBAccount
import Foundation (App, unsafeHandler)
import Postgres.Harness
import Postgres.WriteBackSpec (Fixture (..))
import Relude
import Settings (ArkhamDBSettings)

data Remote = Remote
  { client :: ArkhamDBClient IO
  , decks :: MVar (Map Text ArkhamDBRemoteDeck)
  , calls :: MVar [Text]
  }

newRemote :: ArkhamDBClient IO -> ArkhamDBRemoteDeck -> IO Remote
newRemote unexpected source = do
  decks <- MVar.newMVar $ Map.singleton source.remoteDeckId source
  calls <- MVar.newMVar []
  let record name = MVar.modifyMVar_ calls $ pure . (<> [name])
      load key = Map.lookup key <$> MVar.readMVar decks
        >>= maybe (fail "Unexpected remote id") pure
      create original metadata = do
        -- A duplicate creation is a test failure, not another fake success.
        MVar.modifyMVar_ decks \allDecks -> do
          when (Map.member "124" allDecks) $ fail "Duplicate remote creation"
          let target = original
                { remoteDeckId = "124", dateUpdate = Just "created", deckVersion = Just "2.0"
                , nextDeckId = Nothing, deck = original.deck {Deck.meta = metadata}
                }
          pure $ Map.insert "124" target $ Map.adjust (\old -> old {nextDeckId = Just "124"}) source.remoteDeckId allDecks
        pure $ Right "124"
      client = unexpected
        { loadDeck = \_ key -> record ("load/" <> key) >> Right <$> load key
        , listDecks = \_ -> record "list" >> pure (Right [])
        , upgradeDeck = \_ key xp metadata -> do
            record $ "upgrade/" <> key <> "/" <> show xp
            original <- load key
            create original metadata
        , createDeck = \_ deck -> record "create" >> create source {deck} deck.meta
        , saveDeck = \_ key proposed -> do
            record $ "save/" <> key
            MVar.modifyMVar_ decks $ pure . Map.insert key proposed {dateUpdate = Just "saved"}
            pure $ Right key
        }
  pure Remote {..}

readRemote :: Remote -> Text -> IO ArkhamDBRemoteDeck
readRemote remote key = Map.lookup key <$> MVar.readMVar remote.decks >>= maybe (fail "Missing remote") pure

editRemote :: Remote -> Text -> (ArkhamDBRemoteDeck -> ArkhamDBRemoteDeck) -> IO ()
editRemote remote key edit = MVar.modifyMVar_ remote.decks $ pure . Map.adjust edit key

storedJob :: SqlBackend -> Fixture -> IO (P.Entity ArkhamDBPush)
storedJob connection f = transaction connection $
  P.selectFirst [ArkhamDBPushDeckId P.==. f.deckId] [P.Desc ArkhamDBPushCreatedAt, P.Desc ArkhamDBPushId]
    >>= maybe (liftIO $ fail "Expected persisted push") pure

push :: App -> ArkhamDBSettings -> ArkhamDBClient IO -> Fixture -> Bool -> Int -> IO ()
push app settings client f retry seconds = unsafeHandler app $
  runWriteBack (persistentStore $ const $ pure ()) writeBackStore
    (hoistArkhamDBClient liftIO client) settings (addUTCTime (fromIntegral seconds) f.now) retry f.uid f.deckId
