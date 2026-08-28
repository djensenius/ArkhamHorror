module Arkham.Api.Events.EventListSpec (spec) where

import Api.Handler.Arkham.Events (deduplicateEventMemberships)
import Arkham.Epic.Types (EpicRole (..))
import Arkham.Prelude
import Test.Hspec

spec :: Spec
spec = describe "deduplicateEventMemberships" do
  it "preserves events with one membership" do
    let rows = [(1 :: Int, "Event", GroupPlayer)]
    deduplicateEventMemberships rows `shouldBe` rows

  it "prefers organizer when that membership appears first" do
    deduplicateEventMemberships
      [ (1 :: Int, "Event", Organizer)
      , (1, "Event", GroupPlayer)
      ]
      `shouldBe` [(1, "Event", Organizer)]

  it "prefers organizer when that membership appears second" do
    deduplicateEventMemberships
      [ (1 :: Int, "Event", GroupPlayer)
      , (1, "Event", Organizer)
      ]
      `shouldBe` [(1, "Event", Organizer)]

  it "keeps first-appearance ordering and canonical names for interleaved rows" do
    deduplicateEventMemberships
      [ (1 :: Int, "First", GroupPlayer)
      , (2, "Second", GroupPlayer)
      , (1, "Ignored duplicate name", Organizer)
      , (3, "Third", Organizer)
      , (2, "Second", GroupPlayer)
      ]
      `shouldBe`
        [ (1, "First", Organizer)
        , (2, "Second", GroupPlayer)
        , (3, "Third", Organizer)
        ]
