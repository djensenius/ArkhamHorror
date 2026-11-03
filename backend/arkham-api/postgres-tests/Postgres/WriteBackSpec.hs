{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE NoFieldSelectors #-}
{-# LANGUAGE OverloadedStrings #-}

module Postgres.WriteBackSpec (seedCampaignGame) where

import Api.Arkham.Types.MultiplayerVariant (MultiplayerVariant (Solo))
import Arkham.Difficulty (Difficulty (Easy))
import Arkham.Game qualified as Game
import Data.Time.Clock (UTCTime, addUTCTime, getCurrentTime)
import Database.Persist qualified as P
import Database.Persist.Sql (Single (..), SqlBackend, rawSql, runSqlConn)
import Entity.Arkham.Game
import Postgres.Harness
import Relude hiding (first, second)

-- Write-back admits a legacy gameId/investigator key only for an existing
-- ordinary campaign game; a key that cannot prove its game is fenced.
seedCampaignGame :: SqlBackend -> Text -> UTCTime -> IO ArkhamGameId
seedCampaignGame connection label now = transaction connection $
  P.insert $ ArkhamGame label (Game.newCampaign "01" Nothing 1 1 Easy False) 2 Solo now now
