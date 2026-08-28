module Arkham.Game.ReplayChoicesSpec (spec) where

import Arkham.Classes.HasGame (getGame)
import Arkham.Game (replayChoicesEither)
import Data.Aeson qualified as Aeson
import Data.Aeson.Patch (Operation (..), Patch (..))
import Data.Aeson.Pointer (Key (..), Pointer (..))
import TestImport.New

{- | Regression for #30: 'Api.Handler.Arkham.Replay.getApiV1ArkhamGameReplayR'
used to rebuild replay state through 'Arkham.Game.replayChoices', which
called 'error' on a stored patch that failed to apply -- turning corrupt or
incompatible stored data into a 500. 'replayChoicesEither' is the typed
function the handler now calls; these tests exercise it directly rather
than duplicating its predicate.
-}
spec :: Spec
spec = describe "replayChoicesEither" do
  it "returns the reconstructed game when stored patches apply cleanly" . gameTest $ \_ -> do
    g <- getGame
    Aeson.toJSON <$> replayChoicesEither g [] `shouldBe` Right (Aeson.toJSON g)

  it "returns Left, rather than throwing, when a stored patch cannot apply" . gameTest $ \_ -> do
    g <- getGame
    -- Same corruption DiffSpec uses for 'unsafePatch': replacing a Bool
    -- field with a String applies at the raw JSON level but fails to parse
    -- back into a Game, so the underlying 'patch' call reports 'Error'.
    let corruptPatch = Patch [Rep (Pointer [OKey "gameInAction"]) (Aeson.String "not a bool")]
    case replayChoicesEither g [corruptPatch] of
      Left _ -> pure ()
      Right _ -> expectationFailure "expected replayChoicesEither to reject a corrupt stored patch"
