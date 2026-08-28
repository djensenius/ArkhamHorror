{- | Proves 'Api.Arkham.Epic.canonicalEpicGameLockOrder' directly: the single
pure ordering function BOTH 'Api.Handler.Arkham.Events.deleteEpicEventAggregate'
and 'Api.Handler.Arkham.Games.Shared.mainStreetSwapPlan' delegate to for their
own lock-acquisition plans, rather than each independently reimplementing a
sort. Testing it here, once, directly -- rather than only indirectly through
each caller's own tests -- is what makes "genuinely one shared function," not
"two functions that happen to agree today," a checked property.

These tests prove:

* the result is sorted ascending by @(group ordinal, game id)@, the primary
  key being the ordinal -- an input whose game ids are NOT in the same order
  as their ordinals still comes out ordinal-first, proving ordinal (not game
  id) is the primary sort key;
* two refs that share the same ordinal but have DISTINCT game ids (which
  cannot happen for two different games under
  'Entity.Arkham.Epic.ArkhamEpicGroup' \'s @UniqueEpicGroupOrdinal@
  constraint today, but which this function does not rely on that database
  invariant to order deterministically) are ordered by game id as a
  tie-break;
* duplicate refs (the same game id appearing more than once, whether at the
  same ordinal or different ones) collapse to a single occurrence of that
  game id, in its correct sorted position -- "lock each distinct linked game
  once, in canonical order" holds regardless of how many times, or in what
  order, a caller's input repeats a game;
* an already-sorted input, an empty input, and a single-element input are
  all handled without incident (the empty/singleton cases matter for
  'dedupeSorted' \'s own totality, exercised here as an end-to-end property of
  the exported function rather than only as an internal implementation
  detail).
-}
module Arkham.Api.EpicGameLockOrderSpec (spec) where

import Api.Arkham.Epic (EpicGameLockRef (..), canonicalEpicGameLockOrder)
import Arkham.Epic.Types (GroupOrdinal (..))
import Arkham.Prelude
import Data.UUID qualified as UUID
import Entity.Arkham.Game qualified as GameEntity
import Test.Hspec

-- | A game id distinguished only by its numeric tag -- deliberately
-- unrelated to any ordinal a test pairs it with, so a test that gives a
-- game id numeric order different from its ordinal order proves ordinal,
-- not game id, is the primary sort key.
fixtureGameId :: Int -> GameEntity.ArkhamGameId
fixtureGameId tag = GameEntity.ArkhamGameKey $ UUID.fromWords 0 0 0 (fromIntegral tag)

ref :: Int -> GameEntity.ArkhamGameId -> EpicGameLockRef
ref ordx = EpicGameLockRef (GroupOrdinal ordx)

spec :: Spec
spec = describe "canonicalEpicGameLockOrder (shared Epic game lock-order seam)" do
  it "sorts an already-ordinal-ordered input unchanged" do
    canonicalEpicGameLockOrder [ref 0 (fixtureGameId 0), ref 1 (fixtureGameId 1), ref 2 (fixtureGameId 2)]
      `shouldBe` [fixtureGameId 0, fixtureGameId 1, fixtureGameId 2]

  it "sorts an unsorted (descending-ordinal) input into ascending ordinal order" do
    canonicalEpicGameLockOrder [ref 2 (fixtureGameId 2), ref 0 (fixtureGameId 0), ref 1 (fixtureGameId 1)]
      `shouldBe` [fixtureGameId 0, fixtureGameId 1, fixtureGameId 2]

  it "orders by ordinal, not by game id, when a game id's own numeric order differs from its ordinal's" do
    -- Ordinal 0 is paired with the numerically LARGEST game id and ordinal 2
    -- with the SMALLEST -- if this function sorted by game id instead of
    -- ordinal, the result would come out reversed.
    canonicalEpicGameLockOrder [ref 0 (fixtureGameId 99), ref 1 (fixtureGameId 50), ref 2 (fixtureGameId 1)]
      `shouldBe` [fixtureGameId 99, fixtureGameId 50, fixtureGameId 1]

  it "breaks a tie between two refs sharing the same ordinal by ascending game id" do
    -- Not a case the database's own UniqueEpicGroupOrdinal constraint can
    -- produce for two DIFFERENT games in the same event today, but this
    -- function is deterministic and total on its own terms, independent of
    -- that (or any other) external invariant.
    canonicalEpicGameLockOrder [ref 0 (fixtureGameId 7), ref 0 (fixtureGameId 3)]
      `shouldBe` [fixtureGameId 3, fixtureGameId 7]

  it "collapses a game id repeated at the SAME ordinal to a single occurrence" do
    canonicalEpicGameLockOrder [ref 0 (fixtureGameId 1), ref 0 (fixtureGameId 1)]
      `shouldBe` [fixtureGameId 1]

  it "collapses a game id repeated at DIFFERENT ordinals to a single occurrence, in its sorted position" do
    canonicalEpicGameLockOrder
      [ref 2 (fixtureGameId 5), ref 0 (fixtureGameId 1), ref 1 (fixtureGameId 5)]
      `shouldBe` [fixtureGameId 1, fixtureGameId 5]

  it "handles an empty input" do
    canonicalEpicGameLockOrder [] `shouldBe` ([] :: [GameEntity.ArkhamGameId])

  it "handles a single-element input" do
    canonicalEpicGameLockOrder [ref 0 (fixtureGameId 1)] `shouldBe` [fixtureGameId 1]
