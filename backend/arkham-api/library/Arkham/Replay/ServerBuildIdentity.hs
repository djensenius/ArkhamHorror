{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fforce-recomp #-}

module Arkham.Replay.ServerBuildIdentity (
  serverBuildIdentity,
) where

import Arkham.Replay.BuildIdentity

-- Compiled into the running server; unlike a retained game's Git revision,
-- this value cannot be supplied or changed by an import request.
serverBuildIdentity :: ReplayBuildIdentity
serverBuildIdentity = $(embedReplayBuildIdentity)
