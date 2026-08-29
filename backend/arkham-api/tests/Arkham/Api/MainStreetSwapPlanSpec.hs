{- | Proves the pure lock-order/mapping split in
'Api.Handler.Arkham.Games.Shared.mainStreetSwapPlan' -- the production seam
'Api.Handler.Arkham.Games.Shared.swapMainStreetInvestigators' uses to decide
which 'ArkhamGameId' rows to lock, and in what order, before reading or
writing either game -- without a live database.

This is the fix for the cross-path deadlock risk between a Main Street
group swap and 'Api.Handler.Arkham.Events.deleteEpicEventAggregate': both
now lock every game they touch in ascending 'ArkhamGameId' order alone,
via the one shared 'Api.Arkham.Epic.canonicalEpicGameLockOrder' (see that
function's Haddock for why an earlier, ordinal-based version of this order
was not actually safe: it was not independent of which SUBSET of an
event's linked games a caller happened to have -- a full deletion sees
every linked game, while a swap only ever sees the two it resolved, and
ordering by ordinal could put the same pair of games in opposite relative
order for the two callers).

'mainStreetSwapPlan' therefore takes two plain 'ArkhamGameId's -- never an
ordinal -- so an ordinal cannot be smuggled into the lock-order decision by
construction; the caller's original ordinals only ever decide which group's
game becomes 'firstGameId' vs 'secondGameId' (see
'Api.Handler.Arkham.Games.Shared.planAndExecuteMainStreetSwap'), never the
lock acquisition order.

These tests prove:

* the lock order is ascending by 'ArkhamGameId' regardless of which game
  the caller names first -- reversing the two arguments computes the
  identical 'lockOrder', while 'firstGameId'\/'secondGameId' (which decide
  the actual swap semantics: whose investigator moves where, and the
  response) still reflect exactly what the caller asked, UNCHANGED by the
  sort;
* a degenerate request where both sides resolve to the same game id (which
  'Api.Handler.Arkham.Events.postApiV1ArkhamEventSwapMainStreetR' already
  rejects one level up, by ordinal) still produces a well-defined plan --
  a single-element lock order, since 'canonicalEpicGameLockOrder' locks
  each distinct linked game exactly once -- never a crash and never a
  redundant double-lock of the same row.

See "Arkham.Api.EpicGameLockOrderSpec" for the direct, cross-path proof
that 'mainStreetSwapPlan' and 'Api.Handler.Arkham.Events.deleteEpicEventAggregate'
compute the identical lock order for any pair of games common to both, using
fixture ids chosen so their numeric order conflicts with any ordinal-based
assumption.

As with 'Api.Handler.Arkham.Events.EventDeletionSpec', there is no
live-PostgreSQL integration test actually exercising two concurrent
transactions to observe an avoided deadlock; that residual risk rests on
this pure ordering matching deletion's, and on both writers acquiring their
locks explicitly, up front, before any read of mutable state or any update.
-}
module Arkham.Api.MainStreetSwapPlanSpec (spec) where

import Api.Handler.Arkham.Games.Shared (MainStreetSwapPlan (..), mainStreetSwapPlan)
import Arkham.Prelude
import Data.UUID qualified as UUID
import Entity.Arkham.Game qualified as GameEntity
import Test.Hspec

-- | Two fixture ids whose NUMERIC order is the REVERSE of the names given to
-- them here ('gameNamedFirst' has the numerically LARGER underlying id) --
-- deliberately, so a test that names the numerically-smaller game SECOND
-- still proves it locks FIRST: it is genuinely 'ArkhamGameId's own 'Ord'
-- instance controlling 'lockOrder', not argument position or any other
-- proxy for order.
gameNamedFirst :: GameEntity.ArkhamGameId
gameNamedFirst = GameEntity.ArkhamGameKey $ UUID.fromWords 0 0 0 9

gameNamedSecond :: GameEntity.ArkhamGameId
gameNamedSecond = GameEntity.ArkhamGameKey $ UUID.fromWords 0 0 0 3

spec :: Spec
spec = describe "mainStreetSwapPlan (Main Street swap lock-order seam)" do
  it "locks ascending by ArkhamGameId even when the caller names the numerically-larger game first" do
    let plan = mainStreetSwapPlan gameNamedFirst gameNamedSecond
    plan.lockOrder `shouldBe` [gameNamedSecond, gameNamedFirst]

  it "preserves the caller's first/second mapping for the actual swap semantics, unchanged by the lock sort" do
    let plan = mainStreetSwapPlan gameNamedFirst gameNamedSecond
    plan.firstGameId `shouldBe` gameNamedFirst
    plan.secondGameId `shouldBe` gameNamedSecond

  it "locks the identical order when the caller names the same two games in the opposite argument order" do
    let plan = mainStreetSwapPlan gameNamedSecond gameNamedFirst
    plan.lockOrder `shouldBe` [gameNamedSecond, gameNamedFirst]

  it "preserves ITS OWN first/second mapping too, unchanged by the lock sort" do
    let plan = mainStreetSwapPlan gameNamedSecond gameNamedFirst
    plan.firstGameId `shouldBe` gameNamedSecond
    plan.secondGameId `shouldBe` gameNamedFirst

  it "a degenerate request where both sides are the same game id produces a single-element lock order, never a crash and never a duplicate lock" do
    let plan = mainStreetSwapPlan gameNamedFirst gameNamedFirst
    plan.lockOrder `shouldBe` [gameNamedFirst]
    plan.firstGameId `shouldBe` gameNamedFirst
    plan.secondGameId `shouldBe` gameNamedFirst
