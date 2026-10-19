{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}

module Entity.Arkham.ArkhamDBAccount (
  module Entity.Arkham.ArkhamDBAccount,
) where

import Data.Time.Clock (UTCTime)
import Data.UUID
import Database.Persist.TH
import Entity.Arkham.Deck
import Entity.User
import Relude

share
  [mkPersist sqlSettings]
  [persistLowerCase|
ArkhamDBAccount sql=arkhamdb_accounts
  Id UUID default=uuid_generate_v4()
  userId UserId OnDeleteCascade
  remoteUserId Text Maybe
  remoteUserName Text Maybe
  encryptedAccessToken Text
  encryptedRefreshToken Text Maybe
  expiresAt UTCTime Maybe
  createdAt UTCTime
  updatedAt UTCTime
  UniqueArkhamDBAccountUser userId
  deriving Generic Show
ArkhamDBLinkState sql=arkhamdb_link_states
  Id UUID default=uuid_generate_v4()
  nonce Text
  userId UserId OnDeleteCascade
  expiresAt UTCTime
  createdAt UTCTime
  claimedAt UTCTime Maybe
  UniqueArkhamDBLinkStateNonce nonce
  deriving Generic Show
ArkhamDBDeckLink sql=arkhamdb_deck_links
  Id UUID default=uuid_generate_v4()
  deckId ArkhamDeckId OnDeleteCascade
  remoteDeckId Text
  lastSyncedDateUpdate Text Maybe
  lastSyncedVersion Text Maybe
  writeBack Bool default=False
  lastError Text Maybe
  createdAt UTCTime
  updatedAt UTCTime
  UniqueArkhamDBDeckLinkDeck deckId
  deriving Generic Show
|]
