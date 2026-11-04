module Arkham.Api.Events.EventDeletionSpec (spec) where

import Api.Handler.Arkham.Events (EventDeletionOutcome (..), decideEventDeletion)
import Arkham.Prelude
import Test.Hspec

data Step = Probe | OrganizerCheck | LockedDelete
  deriving stock (Eq, Show)

-- Run the decision with scripted probe answers, recording each step taken.
run :: [Bool] -> Bool -> Maybe Int -> IO (EventDeletionOutcome Int, [Step])
run probes organizer deleted = do
  steps <- newIORef []
  answers <- newIORef probes
  let record s = modifyIORef' steps (<> [s])
      probe = do
        record Probe
        atomicModifyIORef' answers \case
          a : rest -> (rest, a)
          [] -> ([], False)
  outcome <-
    decideEventDeletion
      probe
      (record OrganizerCheck $> organizer)
      (record LockedDelete $> deleted)
  (outcome,) <$> readIORef steps

spec :: Spec
spec = describe "decideEventDeletion" do
  it "is Missing before any organizer check when the event does not exist" do
    run [False] True (Just 1) `shouldReturn` (Missing, [Probe])
    run [False] False Nothing `shouldReturn` (Missing, [Probe])

  it "deletes under the lock for the organizer of an existing event" do
    run [True] True (Just 7) `shouldReturn` (Deleted 7, [Probe, OrganizerCheck, LockedDelete])

  it "is Missing when a concurrent delete won before the organizer's lock" do
    run [True] True Nothing `shouldReturn` (Missing, [Probe, OrganizerCheck, LockedDelete])

  it "is Forbidden for a non-organizer of an event that still exists, without locking" do
    run [True, True] False (Just 1) `shouldReturn` (Forbidden, [Probe, OrganizerCheck, Probe])

  it "prefers Missing for a non-organizer when the event vanished, without locking" do
    run [True, False] False (Just 1) `shouldReturn` (Missing, [Probe, OrganizerCheck, Probe])
