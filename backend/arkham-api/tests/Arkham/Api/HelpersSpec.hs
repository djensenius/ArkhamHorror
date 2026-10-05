module Arkham.Api.HelpersSpec (spec) where

import Api.Arkham.Helpers (tryRedis_)
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
