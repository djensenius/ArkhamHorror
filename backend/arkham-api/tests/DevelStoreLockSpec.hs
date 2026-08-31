{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}

module DevelStoreLockSpec (spec) where

import Arkham.Prelude
import Control.Concurrent (forkIO, threadDelay)
import Data.Proxy (Proxy (..))
import DevelStoreLock (
  StructuralHash (..),
  VersionedAccess (..),
  getExistingStore,
  getExistingVersionedStore,
  getOrCreateStore,
  getOrCreateVersionedStore,
 )
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
    firstAccess <- getOrCreateStore store mkDefault
    secondAccess <- getOrCreateStore store mkDefault
    firstAccess `shouldBe` 7
    secondAccess `shouldBe` 7
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

  {- | Root-cause regression for the MEDIUM \"Foreign.Store schema
  unsafely changed\" finding: a hand-maintained 'Foreign.Store' slot
  number was previously the ONLY thing standing between an old-shaped
  value still published at a slot and new code reading it as if it were
  the new shape -- and that discipline had already failed once (see
  "Api.Arkham.Lifecycle"\'s own Haddock on 'Api.Arkham.Lifecycle.RestartState').
  These tests exercise 'getOrCreateVersionedStore'\/'getExistingVersionedStore'
  and 'StructuralHash' directly against two local sample types
  ('SampleShapeV1' and 'SampleShapeV2') that share a name-independent
  \"is this the type whose shape changed\" role but have genuinely
  different 'GHC.Generics' shapes (different constructor arity), so this
  can prove automatic mismatch detection without needing an actual live
  GHCi @:reload@ boundary (which cannot be reproduced inside a single
  @hspec@ process -- see the module-level Haddock above).
  -}
  describe "schema-versioned access (getOrCreateVersionedStore / getExistingVersionedStore / StructuralHash)" do
    it "structuralHash is stable across repeated calls for the same type" do
      structuralHash (Proxy :: Proxy SampleShapeV1) `shouldBe` structuralHash (Proxy :: Proxy SampleShapeV1)

    it "structuralHash differs between two types with genuinely different shapes (constructor arity changed)" do
      structuralHash (Proxy :: Proxy SampleShapeV1)
        `shouldNotBe` structuralHash (Proxy :: Proxy SampleShapeV2)

    it "getOrCreateVersionedStore freshly publishes, tagged with the caller's own current hash, the first time a slot is used" do
      let store = Store 9_100_101 :: Store (Word64, SampleShapeV1)
          expected = structuralHash (Proxy :: Proxy SampleShapeV1)
      access <- getOrCreateVersionedStore expected store (pure (SampleShapeV1 42))
      case access of
        FreshlyPublished (SampleShapeV1 n) -> n `shouldBe` 42
        other -> expectationFailure ("expected FreshlyPublished, got " <> show other)

    it "getOrCreateVersionedStore reuses the existing value (never re-running the default) when the expected hash still matches" do
      let store = Store 9_100_102 :: Store (Word64, SampleShapeV1)
          expected = structuralHash (Proxy :: Proxy SampleShapeV1)
      defaultRunCount <- newIORef (0 :: Int)
      let mkDefault = atomicModifyIORef' defaultRunCount (\n -> (n + 1, ())) >> pure (SampleShapeV1 7)
      _ <- getOrCreateVersionedStore expected store mkDefault
      secondAccess <- getOrCreateVersionedStore expected store mkDefault
      case secondAccess of
        ReusedExisting (SampleShapeV1 n) -> n `shouldBe` 7
        other -> expectationFailure ("expected ReusedExisting, got " <> show other)
      readIORef defaultRunCount `shouldReturn` 1

    {- | The critical safety proof: a caller whose OWN current expected
    hash no longer matches what was actually published (production: code
    reloaded with a genuinely different shape for the same type name)
    must be refused a 'VersionMismatch' -- and, crucially, must never
    force\/read\/coerce the stale value to do so. This is proven here by
    publishing a value that would itself throw if ever forced ('error'
    inside a thunk, standing in for a heap object whose actual layout no
    longer matches what a naive coercion would assume), then reading it
    back under a deliberately different expected hash: if this test
    completes without that 'error' ever firing, the mismatch was
    detected purely from the hash tag, never from touching the value.

    Mutation check: reverting 'getOrCreateVersionedStore'\/'getExistingVersionedStore'
    to eagerly pattern-match \/ force the stored pair's second component
    (rather than only ever comparing 'fst' before ever deciding whether
    to look at 'snd') makes this test fail with the injected 'error'
    instead of cleanly reporting 'VersionMismatch'.
    -}
    it "getOrCreateVersionedStore reports VersionMismatch, without ever forcing the stale value, when the expected hash no longer matches" do
      let store = Store 9_100_103 :: Store (Word64, SampleShapeV1)
          staleHash = structuralHash (Proxy :: Proxy SampleShapeV1)
          currentExpected = structuralHash (Proxy :: Proxy SampleShapeV2)
          poisonedValue = error "this stale value must never actually be forced"
      -- Publish directly under the STALE hash (standing in for an older
      -- incarnation's own publish), pairing it with a thunk that blows
      -- up the instant anything ever demands it.
      _ <- getOrCreateVersionedStore staleHash store (pure poisonedValue)
      access <- getOrCreateVersionedStore currentExpected store (pure (error "mkDefault must not run either: the slot is not empty"))
      access `shouldBe` VersionMismatch staleHash currentExpected

    it "getExistingVersionedStore returns Nothing before anything has ever been published to a slot" do
      let store = Store 9_100_104 :: Store (Word64, SampleShapeV1)
          expected = structuralHash (Proxy :: Proxy SampleShapeV1)
      getExistingVersionedStore expected store `shouldReturn` Nothing

    it "getExistingVersionedStore observes ReusedExisting for exactly what getOrCreateVersionedStore published under a matching hash" do
      let store = Store 9_100_105 :: Store (Word64, SampleShapeV1)
          expected = structuralHash (Proxy :: Proxy SampleShapeV1)
      _ <- getOrCreateVersionedStore expected store (pure (SampleShapeV1 99))
      observed <- getExistingVersionedStore expected store
      case observed of
        Just (ReusedExisting (SampleShapeV1 n)) -> n `shouldBe` 99
        other -> expectationFailure ("expected Just (ReusedExisting ...), got " <> show other)

    it "getExistingVersionedStore also reports VersionMismatch (without forcing the value) for a mismatched hash" do
      let store = Store 9_100_106 :: Store (Word64, SampleShapeV1)
          staleHash = structuralHash (Proxy :: Proxy SampleShapeV1)
          currentExpected = structuralHash (Proxy :: Proxy SampleShapeV2)
      _ <- getOrCreateVersionedStore staleHash store (pure (error "this stale value must never actually be forced"))
      observed <- getExistingVersionedStore currentExpected store
      observed `shouldBe` Just (VersionMismatch staleHash currentExpected)

-- | A local sample type used only to exercise 'StructuralHash' directly
-- (see the @schema-versioned access@ tests above): deliberately a
-- single-constructor, single-field shape.
newtype SampleShapeV1 = SampleShapeV1 Int
  deriving stock (Eq, Show, Generic)
  deriving anyclass (StructuralHash)

-- | A second local sample type with a genuinely different
-- 'GHC.Generics' shape (two fields instead of one) -- standing in for
-- \"the same type name, reloaded with an incompatible new shape\",
-- without needing an actual live GHCi reload boundary.
data SampleShapeV2 = SampleShapeV2 Int Int
  deriving stock (Eq, Show, Generic)
  deriving anyclass (StructuralHash)
