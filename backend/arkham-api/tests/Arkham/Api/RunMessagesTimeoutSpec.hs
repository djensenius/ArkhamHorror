{- | Proves 'Api.Handler.Arkham.Games.Shared.runWithMessagesTimeout' -- the
production-used circuit breaker BOTH 'Api.Handler.Arkham.Games.Shared.updateGame'
(around its single 'Arkham.Game.Runner.runMessages' call, with the fixed
'Api.Handler.Arkham.Games.Shared.runMessagesTimeoutMicros' budget) and
'Api.Handler.Arkham.PendingGames.runPendingJoinSetup' (around its ENTIRE
engine-setup block, which can itself make up to two back-to-back
'runMessages' calls, wrapped ONCE with the SAME budget) delegate to for
their own real DB transactions -- exercised directly, against real
'UnliftIO.Timeout.timeout' (the exact function 'runWithMessagesTimeout'
imports and calls) and real 'threadDelay', rather than through a
live database or the full Arkham game engine (building a realistic
'Arkham.Game.Game' whose message queue provably never terminates is not a
tractable unit-test fixture; this module instead proves the GENERIC
circuit-breaker primitive both call sites share, with a real wall-clock
race against a genuinely blocking IO action).

This lets us assert:

* an action that completes well within the budget returns its result
  normally, with 'RunMessagesTimeout' never thrown;
* an action that runs LONGER than the budget is genuinely interrupted (not
  merely raced against and ignored): a shared 'IORef' proves the action's
  own "I finished" flag is never set -- checked BOTH immediately after
  'runWithMessagesTimeout' returns AND again a short while after the
  action's own internal delay would have elapsed, so a hypothetical future
  implementation that raced a detached worker thread against a timer
  (instead of relying on 'UnliftIO.Timeout.timeout''s own genuinely-
  interrupting cancellation) and merely returned early while the worker
  kept running unobserved would still be caught -- and 'RunMessagesTimeout'
  -- the SAME typed exception 'updateGame' throws today, carrying the
  exact game id and budget supplied -- propagates OUT of
  'runWithMessagesTimeout' rather than being swallowed into a fake
  success;
* an action that throws its OWN, DIFFERENT exception before the budget
  elapses propagates that exact exception unchanged (never reinterpreted
  as a timeout, and never swallowed);
* both timeout and this second, distinct-exception case escape
  'runWithMessagesTimeout' as an uncaught exception -- exactly what must
  happen for a caller's surrounding 'Database.Persist.Sql.runSqlPool'\/'runDB'
  transaction to roll back and release every lock it holds (a live
  Postgres property this pure/IO-only seam cannot itself demonstrate, and
  does not claim to -- see 'Api.Handler.Arkham.PendingGames' \/
  'Api.Handler.Arkham.Games.Shared'\'s own Haddocks for that rollback
  guarantee).
-}
module Arkham.Api.RunMessagesTimeoutSpec (spec) where

import Api.Handler.Arkham.Games.Shared (RunMessagesTimeout (..), runWithMessagesTimeout)
import Arkham.Prelude
import Control.Concurrent (threadDelay)
import Data.UUID qualified as UUID
import Entity.Arkham.Game qualified as GameEntity
import Test.Hspec

data DistinctFailure = DistinctFailure
  deriving stock (Eq, Show)
  deriving anyclass Exception

fixtureGameId :: GameEntity.ArkhamGameId
fixtureGameId = GameEntity.ArkhamGameKey $ UUID.fromWords 0 0 0 1

-- | A short budget so this module still runs quickly, while remaining
-- robust against CI scheduling\/GC pauses (deliberately NOT the tiny,
-- sub-100ms budgets that proved flaky under load) -- deliberately NOT the
-- real 30s production budget (see
-- 'Api.Handler.Arkham.Games.Shared.runMessagesTimeoutMicros'), which is
-- exactly why 'runWithMessagesTimeout' takes an explicit budget parameter
-- rather than hard-coding it: production supplies the real constant, this
-- module supplies a small one.
shortBudgetMicros :: Int
shortBudgetMicros = 200 * 1000 -- 200ms

spec :: Spec
spec = describe "runWithMessagesTimeout (production-used runMessages circuit breaker, exercised directly)" do
  it "an action completing well within the budget returns its result normally, with RunMessagesTimeout never thrown" do
    result <- runWithMessagesTimeout fixtureGameId shortBudgetMicros (pure (42 :: Int))
    result `shouldBe` 42

  it "an action that runs LONGER than the budget is genuinely interrupted -- its own 'finished' flag is never set, checked both immediately and again a short while after its internal delay would have elapsed -- and RunMessagesTimeout propagates with the exact game id and budget supplied" do
    finished <- newIORef False
    -- 3x the budget gives 'timeout' an ample, non-flaky window to fire
    -- well before this delay would otherwise elapse, without making the
    -- action's own internal delay unnecessarily long.
    let internalDelayMicros = shortBudgetMicros * 3
    outcome <-
      try
        $ runWithMessagesTimeout fixtureGameId shortBudgetMicros do
          threadDelay internalDelayMicros
          writeIORef finished True
    (outcome :: Either RunMessagesTimeout ()) `shouldBe` Left (RunMessagesTimeout fixtureGameId shortBudgetMicros)
    readIORef finished `shouldReturn` False
    -- Wait past 'internalDelayMicros' -- long enough for the action to
    -- have set 'finished' had it kept running in the background after
    -- 'runWithMessagesTimeout' returned -- and re-check. This is what
    -- actually distinguishes a GENUINE cancellation from a mere race where
    -- 'timeout' merely returns early while the action keeps running
    -- unobserved: the immediate check alone cannot tell the two apart, but
    -- this second, delayed check would catch a future implementation that
    -- raced a detached worker thread instead of relying on
    -- 'UnliftIO.Timeout.timeout''s own genuinely-interrupting cancellation.
    -- 'timeout' already fires at 'shortBudgetMicros', comfortably before
    -- 'internalDelayMicros' elapses (1/3 of it); waiting one more
    -- 'internalDelayMicros' after that is ample margin to outlive the
    -- would-be worker action without needlessly inflating this spec's
    -- wall time.
    threadDelay internalDelayMicros
    readIORef finished `shouldReturn` False

  it "an action that throws its OWN, DIFFERENT exception before the budget elapses propagates that exact exception unchanged -- never reinterpreted as a timeout, never swallowed" do
    distinctOutcome <-
      try
        $ runWithMessagesTimeout fixtureGameId shortBudgetMicros (throwIO DistinctFailure)
    (distinctOutcome :: Either DistinctFailure ()) `shouldBe` Left DistinctFailure
    -- The SAME call, viewed through 'RunMessagesTimeout''s own 'try', must
    -- NOT observe a timeout -- proving 'DistinctFailure' was never
    -- reinterpreted as one.
    timeoutOutcome <-
      try
        $ runWithMessagesTimeout fixtureGameId shortBudgetMicros (throwIO DistinctFailure)
          `catch` \DistinctFailure -> pure ()
    (timeoutOutcome :: Either RunMessagesTimeout ()) `shouldBe` Right ()

  it "a genuinely slow action's timeout is reported under the SAME budget value it was given, not a hard-coded production constant, proving the seam is parameterized rather than reusing a fixed 30s budget" do
    -- A distinct, larger budget than 'shortBudgetMicros' -- still small
    -- enough to keep this spec fast, but comfortably above the sub-20ms
    -- range that proved flaky under CI scheduling/GC pauses.
    let otherBudgetMicros = 100 * 1000 -- 100ms
    outcome <-
      try
        $ runWithMessagesTimeout fixtureGameId otherBudgetMicros (threadDelay (otherBudgetMicros * 5))
    (outcome :: Either RunMessagesTimeout ()) `shouldBe` Left (RunMessagesTimeout fixtureGameId otherBudgetMicros)
