module Arkham.Replay.MessageTimeoutSpec (spec) where

import Arkham.Replay.MessageTimeout
import Control.Concurrent (threadDelay)
import Control.Exception (finally)
import Data.IORef
import Test.Hspec
import Prelude

spec :: Spec
spec = describe "replay message timeout" do
  it "returns a completed drain result" do
    runReplayMessagesWithin 1000000 (pure ("completed" :: String))
      `shouldReturn` Just "completed"

  it "cancels and cleans up a stalled drain" do
    cleanedUp <- newIORef False
    result <-
      runReplayMessagesWithin 10000
        $ threadDelay 1000000 `finally` writeIORef cleanedUp True
    result `shouldBe` Nothing
    readIORef cleanedUp `shouldReturn` True

  it "never runs a drain when the supplied bound is non-positive" do
    started <- newIORef False
    result <-
      runReplayMessagesWithin 0
        $ writeIORef started True
    result `shouldBe` Nothing
    readIORef started `shouldReturn` False
