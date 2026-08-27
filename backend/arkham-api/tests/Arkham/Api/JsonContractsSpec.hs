module Arkham.Api.JsonContractsSpec (spec) where

import Api.Arkham.Helpers (ApiResponse (GameError))
import Api.Arkham.Types.GameStep (GameStepJson (..))
import Data.Aeson qualified as Aeson
import System.IO.Error qualified as IOError
import TestImport

loadFixture :: FilePath -> IO Aeson.Value
loadFixture fileName = findFixture fixturePaths
 where
  fixturePaths =
    [ "contracts/fixtures/" <> fileName
    , "../contracts/fixtures/" <> fileName
    , "../../contracts/fixtures/" <> fileName
    ]

  findFixture = \case
    [] ->
      fail
        $ "Could not find contract fixture "
        <> fileName
        <> "; searched: "
        <> show fixturePaths
    path : remainingPaths -> IOError.tryIOError (Aeson.eitherDecodeFileStrict' path) >>= \case
      Left err
        | IOError.isDoesNotExistError err -> findFixture remainingPaths
        | otherwise -> IOError.ioError err
      Right (Left err) ->
        fail $ "Could not decode contract fixture " <> fileName <> " at " <> path <> ": " <> err
      Right (Right fixture) -> pure fixture

spec :: Spec
spec = describe "Native client contract fixtures" do
  it "matches the real game-step encoder" do
    fixture <- loadFixture "game-step.json"

    Aeson.toJSON (GameStepJson 42) `shouldBe` fixture

  it "matches the real game-error encoder" do
    fixture <- loadFixture "game-error.json"

    Aeson.toJSON (GameError "The question changed before this answer arrived.") `shouldBe` fixture
