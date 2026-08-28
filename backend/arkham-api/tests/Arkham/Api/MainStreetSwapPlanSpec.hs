{- | Proves the pure lock-order/mapping split in
'Api.Handler.Arkham.Games.Shared.mainStreetSwapPlan' -- the production seam
'Api.Handler.Arkham.Games.Shared.swapMainStreetInvestigators' uses to decide
which 'ArkhamGameId' rows to lock, and in what order, before reading or
writing either game -- without a live database.

This is the fix for the cross-path deadlock risk between a Main Street
group swap and 'Api.Handler.Arkham.Events.deleteEpicEventAggregate':
deletion always locks every linked game in ascending @(group ordinal, game
id)@ order before locking the event. Before this fix, a swap request that
named its groups in descending ordinal order (e.g. ordinal 2 first, ordinal
1 second) would implicitly acquire its first 'Database.Persist.update' row
lock on the ordinal-2 game before the ordinal-1 game -- the exact reverse of
deletion's order. A concurrent deletion holding the ordinal-1 game's lock
and waiting on the ordinal-2 game, at the same moment such a swap held the
ordinal-2 game's lock and waited on the ordinal-1 game, would deadlock.

These tests prove:

* a swap requested in descending order (first ordinal 2, second ordinal 1)
  still computes a lock order of @[game for ordinal 1, game for ordinal
  2]@ -- ascending, matching deletion -- while 'firstGameId'/'secondGameId'
  (which decide the actual swap semantics: whose investigator moves where,
  and the response) remain exactly the ordinal-2, then ordinal-1 games the
  caller asked for, UNCHANGED by the sort;
* a swap requested in ascending order (first ordinal 1, second ordinal 2)
  computes the identical lock order, so the acquisition order never depends
  on which side the caller happened to call "first";
* the lock order is a pure function of @(ordinal, game id)@ pairs, so it is
  exactly reproducible without a live server, and matches the same
  ascending-ordinal convention
  'Api.Handler.Arkham.Events.deleteEpicEventAggregate' uses for its own
  per-game locks (see
  'Api.Handler.Arkham.Events.MonadEpicEventDeletion');
* a degenerate request where both sides resolve to the same game id (which
  'Api.Handler.Arkham.Events.postApiV1ArkhamEventSwapMainStreetR' already
  rejects one level up, by ordinal) still produces a well-defined plan --
  a single-element lock order, since 'canonicalEpicGameLockOrder' locks
  each distinct linked game exactly once -- never a crash and never a
  redundant double-lock of the same row.

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

-- | A game id distinguished only by its ordinal, exactly as
-- 'Api.Handler.Arkham.Events.EventDeletionSpec' fixtures its own game ids --
-- tests compare returned lists against these directly, so an ordering bug
-- shows up as an ordinary list-equality failure.
fixtureGameId :: Int -> GameEntity.ArkhamGameId
fixtureGameId ordx = GameEntity.ArkhamGameKey $ UUID.fromWords 0 0 0 (fromIntegral ordx)

spec :: Spec
spec = describe "mainStreetSwapPlan (Main Street swap lock-order seam)" do
  it "a descending-order request (first ordinal 2, second ordinal 1) still locks ascending: game 1, then game 2" do
    let plan = mainStreetSwapPlan (2, fixtureGameId 2) (1, fixtureGameId 1)
    plan.lockOrder `shouldBe` [fixtureGameId 1, fixtureGameId 2]

  it "a descending-order request preserves the caller's first/second mapping for the actual swap semantics, unchanged by the lock sort" do
    let plan = mainStreetSwapPlan (2, fixtureGameId 2) (1, fixtureGameId 1)
    plan.firstGameId `shouldBe` fixtureGameId 2
    plan.secondGameId `shouldBe` fixtureGameId 1

  it "an ascending-order request (first ordinal 1, second ordinal 2) locks the identical order as the descending request above" do
    let plan = mainStreetSwapPlan (1, fixtureGameId 1) (2, fixtureGameId 2)
    plan.lockOrder `shouldBe` [fixtureGameId 1, fixtureGameId 2]

  it "an ascending-order request preserves its own first/second mapping, unchanged by the lock sort" do
    let plan = mainStreetSwapPlan (1, fixtureGameId 1) (2, fixtureGameId 2)
    plan.firstGameId `shouldBe` fixtureGameId 1
    plan.secondGameId `shouldBe` fixtureGameId 2

  it "matches the ascending (ordinal, game id) order deleteEpicEventAggregate locks linked games in, for three ordinals taken pairwise" do
    -- deleteEpicEventAggregate (see Arkham.Api.Events.EventDeletionSpec)
    -- locks every linked game in ascending ordinal order; this checks the
    -- swap plan agrees for every ordering of any two of those same three
    -- ordinals, so the two paths can never disagree about which game locks
    -- first.
    let ordinals = [0, 1, 2] :: [Int]
        ordinalPairs = [(a, b) | a <- ordinals, b <- ordinals, a /= b]
        agrees (a, b) =
          (mainStreetSwapPlan (a, fixtureGameId a) (b, fixtureGameId b)).lockOrder
            == [fixtureGameId (min a b), fixtureGameId (max a b)]
    all agrees ordinalPairs `shouldBe` True

  it "a degenerate request where both sides resolve to the same game id produces a well-defined, single-element plan, never a crash and never a duplicate lock" do
    let plan = mainStreetSwapPlan (1, fixtureGameId 1) (1, fixtureGameId 1)
    plan.lockOrder `shouldBe` [fixtureGameId 1]
    plan.firstGameId `shouldBe` fixtureGameId 1
    plan.secondGameId `shouldBe` fixtureGameId 1
