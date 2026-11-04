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
    (result, events) <- twoBeats \record e -> record ("failure: " <> show e)
    result `shouldBe` Left Exception.ThreadKilled
    events `shouldBe` ["pause", "beat", "failure: user error (redis unavailable)", "pause", "beat"]

  it "keeps the room heartbeat going when reporting a failed pass throws" do
    (result, events) <- twoBeats \record _ -> do
      record "report"
      Exception.throwIO (userError "stdout closed")
    result `shouldBe` Left Exception.ThreadKilled
    events `shouldBe` ["pause", "beat", "report", "pause", "beat"]

{- | Drive 'heartbeatLoop' for two beats: the first throws a 'userError', the
second is cancelled with 'ThreadKilled'. Returns how the loop ended and what
happened, in order; the reporter is handed the same recorder.

The pause has no delay. Its guard only fires if ThreadKilled was swallowed, and
turns what would be an endless loop into a failure.
-}
twoBeats
  :: ((String -> IO ()) -> SomeException -> IO ())
  -> IO (Either Exception.AsyncException (), [String])
twoBeats reporter = do
  events <- newIORef []
  let record event = modifyIORef' events (<> [event])
      times event = length . filter (== event) <$> readIORef events
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
  result <- Exception.try (heartbeatLoop pause (reporter record) beat)
  happened <- readIORef events
  pure (result, happened)
