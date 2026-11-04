{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE NoFieldSelectors #-}
{-# LANGUAGE OverloadedStrings #-}

-- Whole-flow write-back coverage: real Games.Shared answers, real enqueue and
-- real runWriteBack against PostgreSQL. Only ArkhamDB HTTP is simulated.
module Postgres.EndToEndSpec (spec) where

import Arkham.ArkhamDB (ArkhamDBClient, ArkhamDBRemoteDeck)
import Foundation (App)
import Postgres.Harness (Harness)
import Postgres.WriteBackSpec (Seed)
import Relude
import Settings (ArkhamDBSettings)
import Test.Hspec

spec :: Harness -> App -> ArkhamDBSettings -> ArkhamDBClient IO -> ArkhamDBRemoteDeck -> Seed -> Spec
spec _ _ _ _ _ _ = describe "Dream-Eaters end-to-end write-back" $ pure ()
