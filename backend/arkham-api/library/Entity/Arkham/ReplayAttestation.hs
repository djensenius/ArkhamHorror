{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}

module Entity.Arkham.ReplayAttestation (
  module Entity.Arkham.ReplayAttestation,
) where

import Relude

import Data.Aeson.Types (Value)
import Database.Persist.Postgresql.JSON ()
import Database.Persist.TH
import Entity
import Entity.Arkham.Game
import Orphans ()

mkEntity
  $(discoverEntities)
  [persistLowerCase|
ArkhamReplayAttestation sql=arkham_replay_attestations
  Id ArkhamGameId
  receipt Value
  deriving Generic Show
|]
