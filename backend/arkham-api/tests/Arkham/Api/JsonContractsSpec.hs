module Arkham.Api.JsonContractsSpec (spec) where

import Api.Arkham.Helpers (ApiResponse (GameError))
import Api.Handler.Arkham.Games (GameStepJson (..))
import Data.Aeson qualified as Aeson
import System.IO.Error qualified as IOError
import TestImport

loadFixture :: FilePath -> IO Aeson.Value
loadFixture fileName = findFixture fixturePaths >>= \case
    Left err -> fail $ "Could not decode contract fixture " <> fileName <> ": " <> err
    Right fixture -> pure fixture
 where
  fixturePaths =
    [ "contracts/fixtures/" <> fileName
    , "../contracts/fixtures/" <> fileName
    , "../../contracts/fixtures/" <> fileName
    ]

  findFixture = \case
    [] -> fail $ "Could not find contract fixture " <> fileName
    path : remainingPaths ->
      Aeson.eitherDecodeFileStrict' path `IOError.catchIOError` \err ->
        if IOError.isDoesNotExistError err
          then findFixture remainingPaths
          else IOError.ioError err

spec :: Spec
spec = describe "Native client contract fixtures" do
  it "matches the real game-step encoder" do
    fixture <- loadFixture "game-step.json"

    Aeson.toJSON (GameStepJson 42) `shouldBe` fixture

  it "matches the real game-error encoder" do
    fixture <- loadFixture "game-error.json"

    Aeson.toJSON (GameError "The question changed before this answer arrived.") `shouldBe` fixture
