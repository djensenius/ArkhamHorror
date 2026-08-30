module DevelStoreLockSpec (spec) where

import Arkham.Prelude
import Control.Concurrent (forkIO, threadDelay)
import Control.Concurrent.MVar (MVar)
import DevelStoreLock (getExistingStore, getOrCreateStore)
import Foreign.Store (Store (..))
import Test.Hspec

{- | Regression for the MEDIUM-severity finding that a prior version of
this lock lived as a plain top-level CAF directly inside the reloadable
home-package module @Api.Arkham.Lifecycle@: a live GHCi session that had
reloaded that module could end up with two distinct incarnations of the
lock, letting an old and a new incarnation of @DevelMain.update@\/
@DevelMain.shutdown@ each serialize only against their own copy while
racing the other -- and @DevelMain.shutdown@'s own former direct,
unsynchronized 'Foreign.Store.lookupStore'\/'Foreign.Store.readStore'
calls meant it was never even synchronized against @update@ to begin
with.

A live GHCi @:reload@ boundary itself cannot be reproduced inside a
single @hspec@ process (there is only ever one incarnation of any module
here), so these tests cannot directly exercise the reload-identity bug
itself. What they /can/, and do, prove directly against the real,
production 'getOrCreateStore'\/'getExistingStore' (not a stand-in): (1)
many genuinely concurrent callers targeting the same 'Foreign.Store.Store'
slot are actually serialized -- exactly one of them ever creates the
value, and every caller (including 'getExistingStore') observes that
exact same value, never a second, independently-fabricated one; and (2)
'getExistingStore' faithfully observes whatever 'getOrCreateStore' most
recently published, exactly as @DevelMain.shutdown@ depends on to share
@update@'s own lock. The reload-immunity itself is instead a structural
property of this module living in a genuinely separate Cabal component
(see this module's own Haddock and "backend/stack.yaml"'s @packages:@
list), not something a runtime test can observe from inside a single
process.

Each test uses a fresh, never-before-used 'Foreign.Store.Store' slot
number (arbitrarily large, spaced far apart) so that repeated test runs
within the same @hspec@ process -- and any other test module that might
itself use low-numbered slots -- can never collide with, or be corrupted
by, one another.
-}
spec :: Spec
spec = describe "DevelStoreLock" do
  it "getOrCreateStore returns the freshly published default the first time a slot is used" do
    let store = Store 9_100_001 :: Store Int
    value <- getOrCreateStore store (pure 42)
    value `shouldBe` 42

  it "getOrCreateStore retrieves the existing value on a later call, never re-running the default action" do
    let store = Store 9_100_002 :: Store Int
    defaultRunCount <- newIORef (0 :: Int)
    let mkDefault = atomicModifyIORef' defaultRunCount (\n -> (n + 1, ())) >> pure 7
    first <- getOrCreateStore store mkDefault
    second <- getOrCreateStore store mkDefault
    first `shouldBe` 7
    second `shouldBe` 7
    readIORef defaultRunCount `shouldReturn` 1

  it "getExistingStore returns Nothing before anything has ever been published to a slot" do
    let store = Store 9_100_003 :: Store Int
    getExistingStore store `shouldReturn` Nothing

  it "getExistingStore observes exactly what getOrCreateStore published, sharing the same lock" do
    let store = Store 9_100_004 :: Store Int
    published <- getOrCreateStore store (pure 99)
    observed <- getExistingStore store
    observed `shouldBe` Just published

  {- | The genuine concurrency\/serialization property that matters for
  @update@\/@shutdown@: many callers racing to be the first to populate a
  never-before-used slot must never fabricate and publish independent
  values -- exactly one default action wins, and every other caller
  (including ones that run before that default has even finished
  computing) instead retrieves that exact same published value. Mutation
  check: reverting 'getOrCreateStore' to compose
  'Foreign.Store.lookupStore'\/'Foreign.Store.writeStore' without going
  through @storeAccessLock@ (this module's own former, well-known
  unsynchronized composition) makes this fail intermittently, since two
  concurrent callers can then both observe an empty slot and both write,
  each believing itself the sole creator.
  -}
  it "many concurrent callers targeting the same never-before-used slot all observe the exact same, singly-created value" do
    let store = Store 9_100_005 :: Store Int
        callers = 50 :: Int
    defaultRunCount <- newIORef (0 :: Int)
    resultsVar <- newMVar []
    doneGates <- replicateM callers newEmptyMVar :: IO [MVar ()]
    let mkDefault = do
          -- A short delay widens the window in which a second,
          -- concurrent caller could -- absent proper serialization --
          -- itself also observe the slot as empty and race to publish
          -- its own independent value.
          threadDelay 2_000
          n <- atomicModifyIORef' defaultRunCount (\k -> (k + 1, k + 1))
          pure (100_000 + n)
    for_ doneGates $ \gate ->
      forkIO $ do
        value <- getOrCreateStore store mkDefault
        modifyMVar_ resultsVar (pure . (value :))
        putMVar gate ()
    traverse_ takeMVar doneGates
    results <- readMVar resultsVar
    length results `shouldBe` callers
    -- Every observed value is identical: nobody fabricated a second,
    -- independent default.
    length (nub results) `shouldBe` 1
    -- And only ONE caller's default action ever actually ran, even
    -- though every one of them raced to be first.
    readIORef defaultRunCount `shouldReturn` 1
