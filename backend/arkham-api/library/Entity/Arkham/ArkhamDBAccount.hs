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
  lastWriteBackAttemptAt UTCTime Maybe
  writeBackFailures Int default=0
  UniqueArkhamDBAccountUser userId
  deriving Generic Show
ArkhamDBLinkState sql=arkhamdb_link_states
  Id UUID default=uuid_generate_v4()
  nonce Text
  userId UserId OnDeleteCascade
  expiresAt UTCTime
  createdAt UTCTime
  claimedAt UTCTime Maybe
  completionHash Text Maybe
  encryptedPendingTokens Text Maybe
  UniqueArkhamDBLinkStateNonce nonce
  deriving Generic Show
ArkhamDBDeckLink sql=arkhamdb_deck_links
  Id UUID default=uuid_generate_v4()
  deckId ArkhamDeckId OnDeleteCascade
  userId UserId OnDeleteCascade
  remoteDeckId Text
  lastSyncedDateUpdate Text Maybe
  lastSyncedVersion Text Maybe
  writeBack Bool default=False
  lastError Text Maybe
  campaignKey Text Maybe
  createdAt UTCTime
  updatedAt UTCTime
  UniqueArkhamDBDeckLinkDeck deckId
  UniqueArkhamDBDeckLinkUserRemote userId remoteDeckId
  deriving Generic Show
ArkhamDBPush sql=arkhamdb_pushes
  Id UUID default=uuid_generate_v4()
  deckId ArkhamDeckId OnDeleteCascade
  userId UserId OnDeleteCascade
  generation ArkhamDBAccountId OnDeleteCascade
  eventKey Text
  campaignKey Text
  payload Text
  totalXp Int
  status Text
  stage Text
  sourceId Text Maybe
  targetId Text Maybe
  expectedDateUpdate Text Maybe
  expectedVersion Text Maybe
  remoteDateUpdate Text Maybe
  remoteVersion Text Maybe
  error Text Maybe
  mode Text
  createdAt UTCTime
  updatedAt UTCTime
  boundary Text
  UniqueArkhamDBPushEvent deckId eventKey
  deriving Generic Show
ArkhamDBCampaignDeck sql=arkhamdb_campaign_decks
  Id UUID default=uuid_generate_v4()
  campaignKey Text
  deckId ArkhamDeckId OnDeleteCascade
  userId UserId OnDeleteCascade
  lastPushedXp Int default=0
  UniqueArkhamDBCampaignDeck campaignKey
  deriving Generic Show
|]
