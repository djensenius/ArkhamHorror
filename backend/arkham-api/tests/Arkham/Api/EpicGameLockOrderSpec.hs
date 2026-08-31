{- | Proves 'Api.Arkham.Epic.canonicalEpicGameLockOrder' directly: the single
pure ordering function BOTH 'Api.Handler.Arkham.Events.deleteEpicEventAggregate'
and 'Api.Handler.Arkham.Games.Shared.mainStreetSwapPlan' delegate to for their
own lock-acquisition plans, rather than each independently reimplementing a
sort. Testing it here, once, directly -- rather than only indirectly through
each caller's own tests -- is what makes "genuinely one shared function," not
"two functions that happen to agree today," a checked property.

An earlier version of this function ordered by @(group ordinal, game id)@,
with game id only a tie-break. That is NOT subset-independent: a full
deletion sees every one of an event's linked games, while a Main Street swap
only ever sees the two it resolved, and an ordinal-based order can put the
same pair of games in OPPOSITE relative order depending on which other
ordinals happen to be in a caller's particular subset (see this module's
final test, and 'canonicalEpicGameLockOrder's own Haddock, for the
concrete scenario). Ordering by 'ArkhamGameId's own 'Ord' instance alone
has no such dependency: the relative order of any two ids that both appear
in ANY subset is fixed by the ids themselves.

These tests prove:

* the result is sorted ascending by 'ArkhamGameId' alone;
* duplicate ids (the same game appearing more than once in the input)
  collapse to a single occurrence, whether the duplicates are adjacent or
  separated by other distinct ids in the input;
* an already-sorted input, an empty input, and a single-element input are
  all handled without incident;
* the ordering is genuinely subset-independent: handed the FULL set of ids
  a deletion would see (including a repeated id, aliased across two
  ordinals) versus just the SUBSET of two ids a Main Street swap resolved,
  this function returns the SAME relative order for any pair of ids common
  to both -- exercised directly against 'mainStreetSwapPlan's actual
  'lockOrder' field, not merely re-derived, with fixture ids chosen so
  their OWN numeric order deliberately conflicts with any "the game
  mentioned first/at the lowest ordinal locks first" assumption, so this
  test cannot pass by coincidence.
-}
module Arkham.Api.EpicGameLockOrderSpec (spec) where

import Api.Arkham.Epic (canonicalEpicGameLockOrder)
import Api.Handler.Arkham.Games.Shared (MainStreetSwapPlan (..), mainStreetSwapPlan)
import Arkham.Prelude
import Data.UUID qualified as UUID
import Entity.Arkham.Game qualified as GameEntity
import Test.Hspec

-- | A game id distinguished only by its numeric tag.
fixtureGameId :: Int -> GameEntity.ArkhamGameId
fixtureGameId tag = GameEntity.ArkhamGameKey $ UUID.fromWords 0 0 0 (fromIntegral tag)

spec :: Spec
spec = describe "canonicalEpicGameLockOrder (shared Epic game lock-order seam)" do
  it "sorts an already-sorted input unchanged" do
    canonicalEpicGameLockOrder [fixtureGameId 0, fixtureGameId 1, fixtureGameId 2]
      `shouldBe` [fixtureGameId 0, fixtureGameId 1, fixtureGameId 2]

  it "sorts an unsorted input into ascending ArkhamGameId order" do
    canonicalEpicGameLockOrder [fixtureGameId 2, fixtureGameId 0, fixtureGameId 1]
      `shouldBe` [fixtureGameId 0, fixtureGameId 1, fixtureGameId 2]

  it "collapses a game id repeated adjacently (after sorting) to a single occurrence" do
    canonicalEpicGameLockOrder [fixtureGameId 1, fixtureGameId 1]
      `shouldBe` [fixtureGameId 1]

  it "collapses a game id repeated at NON-ADJACENT positions in the INPUT -- with a different id between them -- to a single occurrence" do
    -- The two occurrences of game 9 are separated by game 4 in the INPUT
    -- order ([9, 4, 9]) -- this is the shape that defeated the old
    -- adjacency-only 'dedupeSorted' helper, which sorted by
    -- @(ordinal, game id)@ and could leave two same-id refs at different
    -- ordinals non-adjacent even after sorting. Sorting purely by
    -- 'GameEntity.ArkhamGameId' (as this function now does) always groups
    -- equal ids together, so 'nubOrd' collapses them regardless of their
    -- input positions -- this test pins that regression directly, using
    -- the same input shape that previously broke the invariant.
    canonicalEpicGameLockOrder [fixtureGameId 9, fixtureGameId 4, fixtureGameId 9]
      `shouldBe` [fixtureGameId 4, fixtureGameId 9]

  it "handles an empty input" do
    canonicalEpicGameLockOrder [] `shouldBe` ([] :: [GameEntity.ArkhamGameId])

  it "handles a single-element input" do
    canonicalEpicGameLockOrder [fixtureGameId 1] `shouldBe` [fixtureGameId 1]

  it "is subset-independent: a full deletion-shaped input (with an aliased game repeated at two ordinals) and a swap-shaped two-element subset agree on the SAME pair's lock order, with fixture ids that deliberately conflict with ordinal/position order" do
    -- Models one event with three groups: ordinal 0 and ordinal 2 both
    -- linked to the SAME game (numeric tag 9, deliberately the LARGER of
    -- the two ids involved), and ordinal 1 linked to a DIFFERENT game
    -- (numeric tag 3, deliberately the SMALLER). A full deletion would see
    -- all three refs (in whatever order a query returns them; here,
    -- ordinal order: game 9, game 3, game 9). A Main Street swap naming
    -- ordinals 2 and 1 only ever resolves the SUBSET {game 9, game 3} --
    -- it never even sees ordinal 0. If lock order depended on ordinal (as
    -- an earlier, buggy version of this function did), deletion would put
    -- game 9 (lowest ordinal, 0) before game 3 (ordinal 1), while this
    -- swap would put game 3 (ordinal 1) before game 9 (ordinal 2) -- the
    -- OPPOSITE order for the same pair of games, a real cross-path
    -- deadlock. Both must actually agree: ascending by id, [game 3, game 9].
    let gameAtOrdinal0And2 = fixtureGameId 9
        gameAtOrdinal1 = fixtureGameId 3
        deletionFullRefs = [gameAtOrdinal0And2, gameAtOrdinal1, gameAtOrdinal0And2]
        deletionLockOrder = canonicalEpicGameLockOrder deletionFullRefs
        -- The swap request names ordinal 2 first, ordinal 1 second --
        -- exactly mirroring "a request that never sees ordinal 0 at all".
        swapPlan = mainStreetSwapPlan gameAtOrdinal0And2 gameAtOrdinal1
    deletionLockOrder `shouldBe` [fixtureGameId 3, fixtureGameId 9]
    swapPlan.lockOrder `shouldBe` [fixtureGameId 3, fixtureGameId 9]
    swapPlan.lockOrder `shouldBe` deletionLockOrder
