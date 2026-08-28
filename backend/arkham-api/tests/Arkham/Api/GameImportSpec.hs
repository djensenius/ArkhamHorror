module Arkham.Api.GameImportSpec (spec) where

import Api.Handler.Arkham.Game.Debug (selectUploadedExportFile)
import Arkham.Prelude
import Test.Hspec

{- | Regression for #30: 'postApiV1ArkhamGamesImportR' used
@'Safe.fromJustNote' "No export file uploaded" . headMay@ to pick the
uploaded multipart file, which threw and returned a 500 whenever a request
carried no file. It now branches on 'selectUploadedExportFile' explicitly
and rejects a missing file with 'invalidArgs' (400) before ever reaching
decoding. These tests exercise that selector -- the exact function the
production handler branches on -- directly, covering both the
empty-upload ('Nothing') and file-present ('Just') control-flow branches.
-}
spec :: Spec
spec = describe "selectUploadedExportFile" do
  it "returns Nothing for an empty upload" do
    selectUploadedExportFile ([] :: [(Text, Int)]) `shouldBe` Nothing

  it "returns the first uploaded file's payload when present" do
    selectUploadedExportFile [("export", 42 :: Int), ("other", 7)] `shouldBe` Just 42
