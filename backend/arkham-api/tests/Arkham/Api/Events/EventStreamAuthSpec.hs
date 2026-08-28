module Arkham.Api.Events.EventStreamAuthSpec (spec) where

import Api.Handler.Arkham.Events (withEventMember)
import Arkham.Epic.Types (EpicRole (..))
import Arkham.Prelude
import Test.Hspec

spec :: Spec
spec = describe "withEventMember authorization gate" do
  it "never starts the protected action when authorization fails" do
    actionStarted <- newIORef False
    let failAuth = throwIO (userError "not a member") :: IO EpicRole
        stream _ = writeIORef actionStarted True :: IO ()
    (_ :: Either SomeException ()) <- try $ withEventMember failAuth stream
    readIORef actionStarted `shouldReturn` False

  it "runs the protected action when authorization succeeds" do
    actionStarted <- newIORef False
    let succeedAuth = pure Organizer :: IO EpicRole
        stream _ = writeIORef actionStarted True :: IO ()
    withEventMember succeedAuth stream
    readIORef actionStarted `shouldReturn` True

  it "passes the authorized role to the protected action" do
    received <- newIORef Nothing
    let succeedAuth = pure GroupPlayer :: IO EpicRole
        stream role = writeIORef received (Just role) :: IO ()
    withEventMember succeedAuth stream
    readIORef received `shouldReturn` Just GroupPlayer
