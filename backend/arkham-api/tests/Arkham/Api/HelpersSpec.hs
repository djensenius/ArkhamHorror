module Arkham.Api.HelpersSpec (spec) where

import Api.Arkham.Helpers (heartbeatLoop, tryRedis_)
import Arkham.Prelude
import Control.Exception qualified as Exception
import Test.Hspec

spec :: Spec
spec = describe "Redis helper exception boundaries" do
  it "swallows synchronous Redis-observability failures" do
    tryRedis_ (Exception.throwIO (userError "redis unavailable")) `shouldReturn` ()

  it "propagates asynchronous cancellation" do
    result <- Exception.try @Exception.AsyncException (tryRedis_ (Exception.throwIO Exception.ThreadKilled))
    case result of
      Left Exception.ThreadKilled -> pure ()
      other -> expectationFailure $ "expected ThreadKilled to propagate, got " <> show other

  it "keeps the room heartbeat going after a failed pass, until cancelled" do
    events <- newIORef ([] :: [String])
    let record event = modifyIORef' events (<> [event])
        times event = length . filter (== event) <$> readIORef events
        -- No delay. The guard only fires if ThreadKilled was swallowed, and
        -- turns what would be an endless loop into a failure.
        pause = do
          record "pause"
          pauses <- times "pause"
          when (pauses > 2) $ Exception.throwIO (Exception.ErrorCall "heartbeat ran past ThreadKilled")
        beat = do
          record "beat"
          beats <- times "beat"
          if beats == 1
            then Exception.throwIO (userError "redis unavailable")
            else Exception.throwIO Exception.ThreadKilled
        report e = record ("failure: " <> show e)
    result <- Exception.try @Exception.AsyncException (heartbeatLoop pause report beat)
    case result of
      Left Exception.ThreadKilled -> pure ()
      other -> expectationFailure $ "expected ThreadKilled to stop the loop, got " <> show other
    readIORef events
      `shouldReturn` ["pause", "beat", "failure: user error (redis unavailable)", "pause", "beat"]
