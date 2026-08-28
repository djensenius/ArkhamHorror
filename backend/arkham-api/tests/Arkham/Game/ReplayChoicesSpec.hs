module Arkham.Game.ReplayChoicesSpec (spec) where

import Arkham.Classes.HasGame (getGame)
import Arkham.Game (replayChoices)
import Data.Aeson qualified as Aeson
import Data.Aeson.Patch (Operation (..), Patch (..))
import Data.Aeson.Pointer (Key (..), Pointer (..))
import TestImport.New

{- | Regression for #30: 'Api.Handler.Arkham.Replay.getApiV1ArkhamGameReplayR'
used to rebuild replay state through a version of 'Arkham.Game.replayChoices'
that called 'error' on a stored patch that failed to apply -- turning
corrupt or incompatible stored data into a 500. 'replayChoices' now returns
an explicit 'Either' with no partial variant; these tests exercise it
directly rather than duplicating its predicate.
-}
spec :: Spec
spec = describe "replayChoices" do
  it "returns the reconstructed game when stored patches apply cleanly" . gameTest $ \_ -> do
    g <- getGame
    Aeson.toJSON <$> replayChoices g [] `shouldBe` Right (Aeson.toJSON g)

  it "returns Left, rather than throwing, when a stored patch cannot apply" . gameTest $ \_ -> do
    g <- getGame
    -- Same corruption DiffSpec uses for 'unsafePatch': replacing a Bool
    -- field with a String applies at the raw JSON level but fails to parse
    -- back into a Game, so the underlying 'patch' call reports 'Error'.
    let corruptPatch = Patch [Rep (Pointer [OKey "gameInAction"]) (Aeson.String "not a bool")]
    case replayChoices g [corruptPatch] of
      Left _ -> pure ()
      Right _ -> expectationFailure "expected replayChoices to reject a corrupt stored patch"
