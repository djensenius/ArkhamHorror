{-# LANGUAGE NoFieldSelectors #-}

module Api.Arkham.Types.GameStep (GameStepJson (..)) where

import Data.Aeson (ToJSON)
import Relude

newtype GameStepJson = GameStepJson
  { step :: Int
  }
  deriving stock (Show, Generic)
  deriving anyclass ToJSON
