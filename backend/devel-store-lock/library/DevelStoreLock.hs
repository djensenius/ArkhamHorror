{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}

{- | A genuinely reload-immune 'Foreign.Store' access guard.

@app\/DevelMain.hs@'s restart protocol (see
'Api.Arkham.Lifecycle.restartManagedGenerationUsing') publishes its
single authoritative 'Api.Arkham.Lifecycle.RestartState' into a
'Foreign.Store.Store', specifically so it survives GHCi\/@stack ghci@
reloads (that package's own documented purpose: \"Persists through GHCi
reloads.\"). But composing 'Foreign.Store.lookupStore' with a
conditional 'Foreign.Store.writeStore' -- exactly what
'getOrCreateStore' below must do -- is not itself safe without external
synchronization: 'Foreign.Store' is also documented \"Not thread-safe\",
and two concurrent callers (one via @update@, one via @shutdown@) could
otherwise both observe an empty slot and both write, silently
overwriting one generation's authoritative state with the other's.

An earlier version guarded exactly this composition with a plain
top-level @MVar ()@ CAF defined directly inside
@Api.Arkham.Lifecycle@ -- a module reachable, transitively, from
@app\/DevelMain.hs@'s own @stack ghci arkham-api:lib@ \/ @:l
app\/DevelMain.hs@ workflow (see that module's own Haddock), and
therefore part of the *same interpreted, reloadable module graph* as
@DevelMain.hs@ itself: if @Api.Arkham.Lifecycle@'s own source is ever
edited and @:reload@d during a live @ghci@ session (an entirely
realistic scenario while this very protocol is under active
development), GHC creates a genuinely *new* incarnation of that module,
with a freshly, independently initialized lock CAF -- disconnected from
whatever incarnation any other, still-live caller (or a caller that
has not yet itself been reloaded) is holding. Two different incarnations
of a lock cannot mutually exclude each other: the underlying
'Foreign.Store' slot's own *data* correctly survives the reload (that is
exactly what the package guarantees), but the *guard* around
composed access to it does not, reopening precisely the unsynchronized
concurrent-access race the lock was introduced to close in the first
place. @DevelMain.shutdown@ compounded this: it performed its own direct
'Foreign.Store.lookupStore'\/'Foreign.Store.readStore' calls entirely
outside that lock, so it was never even synchronized against @update@ to
begin with, regardless of the reload-identity problem.

This module is the fix: a genuinely separate Cabal package (a sibling of
@arkham-api@, exactly like this monorepo's own pre-existing
@backend\/validate@\/@backend\/cards-discover@ packages -- see
"backend/stack.yaml"'s own @packages:@ list), depended on by
@arkham-api@'s library component as an ordinary package dependency. When
@stack ghci arkham-api:lib@ (per @DevelMain.hs@'s own documented
workflow) is invoked and only @arkham-api:lib@ (transitively including
@Api.Arkham.Lifecycle@) is named as the interpreted target, this
module -- belonging to a *different* Cabal component entirely -- is,
per ordinary Cabal\/Stack\/GHC package-database semantics, always loaded
as pre-compiled object code, never re-interpreted\/re-@:reload@ed. This
is the exact same structural guarantee that already makes
'Foreign.Store'\'s own internal storage table (and any other external
package's own top-level CAF) reload-immune -- it lives in a genuinely
separate, externally-installed unit, outside whatever module graph a
@ghci@ session is currently interpreting. 'storeAccessLock' below
therefore has exactly one incarnation for the entire lifetime of the
OS process, regardless of how many times @Api.Arkham.Lifecycle@ (or any
other home module that merely *imports* this one) is itself edited and
reloaded: reloading a module never recreates the top-level bindings of
packages it depends on, only its own.

@Api.Arkham.Lifecycle.getOrCreateStore@ is now a thin re-export of
'getOrCreateStore' below; @DevelMain.shutdown@ now uses 'getExistingStore'
(added here specifically for its read-only path) instead of its own
former direct, unsynchronized 'Foreign.Store.lookupStore'\/
'Foreign.Store.readStore' calls -- both @update@ and @shutdown@ therefore
share the exact same, exactly-once-initialized lock.
-}
module DevelStoreLock (
  getOrCreateStore,
  getExistingStore,

  -- * Schema-versioned access
  -- $versioned
  StructuralHash (..),
  GStructuralHash (..),
  VersionedAccess (..),
  getOrCreateVersionedStore,
  getExistingVersionedStore,
) where

import Control.Concurrent.MVar (MVar, newMVar, withMVar)
import Data.Bits (xor)
import Data.Kind (Type)
import Data.Proxy (Proxy (..))
import Data.Typeable (Typeable, typeRep)
import Data.Word (Word64)
import Foreign.Store (Store (..), lookupStore, readStore, writeStore)
import GHC.Generics (
  Generic (Rep),
  K1 (..),
  M1 (..),
  U1 (..),
  V1,
  (:*:) (..),
  (:+:) (..),
 )
import System.IO.Unsafe (unsafePerformIO)
import Prelude

-- | The single, process-global, exactly-once-initialized guard for
-- every composed 'Foreign.Store' access this module performs -- see
-- this module's own Haddock for why a plain CAF is only safe here
-- because it is defined in a genuinely separate, never-reloaded Cabal
-- component.
{-# NOINLINE storeAccessLock #-}
storeAccessLock :: MVar ()
storeAccessLock = unsafePerformIO (newMVar ())

-- | Atomically retrieve the value already published at the given
-- 'Foreign.Store.Store', or -- if none has ever been published --
-- compute, publish, and return a fresh default. Serialized against every
-- other caller of this function /and/ 'getExistingStore' (including
-- concurrent ones, and ones separated by any number of intervening
-- GHCi\/@stack ghci@ reloads of any *other* module) via
-- 'storeAccessLock'.
getOrCreateStore :: Store a -> IO a -> IO a
getOrCreateStore store@(Store slot) mkDefault =
  withMVar storeAccessLock $ \() -> do
    mExisting <- lookupStore slot
    case mExisting of
      Just existing -> readStore existing
      Nothing -> do
        fresh <- mkDefault
        writeStore store fresh
        pure fresh

-- | Atomically read whatever is currently published at the given
-- 'Foreign.Store.Store', if anything, without ever publishing a
-- default -- for a caller (production: @DevelMain.shutdown@) that only
-- ever wants to observe an existing value, never create one. Serialized
-- against 'getOrCreateStore' (and every other caller of this function)
-- via the exact same 'storeAccessLock', so a concurrent @update@\/
-- @shutdown@ pair can never race each other the way two independent,
-- unsynchronized 'Foreign.Store.lookupStore' call sites could.
getExistingStore :: Store a -> IO (Maybe a)
getExistingStore (Store slot) =
  withMVar storeAccessLock $ \() -> do
    mExisting <- lookupStore slot
    traverse readStore mExisting

{- $versioned
'Foreign.Store' itself performs no runtime type check at all: reading a
slot whose /value/ was published under one Haskell-level type, then
reading it again after the type actually stored there has changed shape
(e.g. a sum type gaining a field on one of its constructors), silently
coerces the old, differently-laid-out heap object into the new type --
undefined behaviour, not merely a wrong answer. This previously relied
entirely on a human remembering to bump 'Api.Arkham.Lifecycle'\'s own
hand-picked 'Foreign.Store' slot number every time 'Api.Arkham.Lifecycle.RestartState'\'s
shape changed -- and, in fact, that very discipline was the root cause of
a MEDIUM-severity finding: 'Api.Arkham.Lifecycle.RestartState'\'s
'Api.Arkham.Lifecycle.RetireFailed' constructor gained a field without
the slot number being bumped to match, in a live-process-equivalent
scenario.

The functions below close this structurally instead of relying on
programmer discipline: every publish is tagged with a 'Word64'
/structural hash/ ('StructuralHash', computed automatically from a
type's 'GHC.Generics' representation -- every constructor's arity and
every field's own 'Data.Typeable.TypeRep', so it changes automatically
whenever the type's shape changes, without anyone needing to remember to
update a version number by hand) alongside the value itself. A later
read only ever actually forces (and therefore only ever risks
dereferencing the field layout of) the /value/ once its own tag has
first been confirmed, cheaply and without touching the value at all, to
match the CURRENT caller's own expected hash: the tag and the value are
stored together in one lazy pair, so comparing the tag never forces the
paired value, and a caller that sees a mismatched tag can therefore
safely refuse to ever touch it, rather than blindly 'readStore'-ing (and
thereby coercing) something that may have been published under an
incompatible, since-changed shape. On a mismatch, 'getOrCreateVersionedStore'
and 'getExistingVersionedStore' report exactly that ('VersionMismatch'),
and -- critically -- never themselves overwrite the stale slot: doing so
automatically could silently orphan whatever a still-live previous
generation (this module cannot tell, and must not guess, whether one
exists) actually left there. The caller (production:
'Api.Arkham.Lifecycle') decides the actual recovery policy, which for a
restart-protocol lock is necessarily a hard refusal -- see
'Api.Arkham.Lifecycle.DevelStoreSchemaStale'.
-}

-- | The result of a schema-checked 'Foreign.Store' access.
data VersionedAccess a
  = -- | Nothing had ever been published at this slot; @a@ was just
    -- freshly created and published, tagged with the caller's own
    -- current expected hash.
    FreshlyPublished a
  | -- | A value was already published, its own tag matches the
    -- caller's current expected hash, and it is therefore genuinely safe
    -- to reuse.
    ReusedExisting a
  | -- | A value was already published, but its own tag does /not/
    -- match the caller's current expected hash -- i.e. it was published
    -- by code with a different (structurally incompatible) shape for
    -- this type, most plausibly an earlier, since-reloaded incarnation
    -- of whatever module defines it. The stale value itself is never
    -- forced\/read\/coerced: only the two hashes are given back, for the
    -- caller to report and refuse to proceed on.
    VersionMismatch
      Word64
      -- ^ the hash actually stored
      Word64
      -- ^ this caller's own current expected hash
  deriving stock (Eq, Show)

{- | Atomically retrieve the schema-checked value already published at
the given slot, or create-and-publish a freshly tagged one if the slot
is empty -- see the @$versioned@ section above for exactly what problem
this closes over plain 'getOrCreateStore'. Serialized against every
other caller of this module's functions via the exact same
'storeAccessLock' as the untyped functions above.
-}
getOrCreateVersionedStore
  :: Word64
  -- ^ this caller's own current expected structural hash (production:
  -- @'structuralHash' (Proxy \@(RestartState ThreadId))@).
  -> Store (Word64, a)
  -> IO a
  -> IO (VersionedAccess a)
getOrCreateVersionedStore expected store@(Store slot) mkDefault =
  withMVar storeAccessLock $ \() -> do
    mExisting <- lookupStore slot
    case mExisting of
      Nothing -> do
        fresh <- mkDefault
        writeStore store (expected, fresh)
        pure (FreshlyPublished fresh)
      Just existing -> do
        -- Reading the pair itself never forces either field (GHC's pairs
        -- are lazy in both components): only 'fst' is ever demanded
        -- below, until (and unless) the hashes are first confirmed equal.
        (storedHash, storedValue) <- readStore existing
        pure $
          if storedHash == expected
            then ReusedExisting storedValue
            else VersionMismatch storedHash expected

-- | The read-only counterpart of 'getOrCreateVersionedStore', for a
-- caller that only ever wants to observe an existing published value,
-- never create one -- see 'getExistingStore'.
getExistingVersionedStore
  :: Word64
  -> Store (Word64, a)
  -> IO (Maybe (VersionedAccess a))
getExistingVersionedStore expected (Store slot) =
  withMVar storeAccessLock $ \() -> do
    mExisting <- lookupStore slot
    traverse
      ( \existing -> do
          (storedHash, storedValue) <- readStore existing
          pure $
            if storedHash == expected
              then ReusedExisting storedValue
              else VersionMismatch storedHash expected
      )
      mExisting

-- | Compute a 'Word64' hash of a type's own structural shape:
-- automatically, from its 'GHC.Generics' representation, rather than
-- from a manually-maintained version number -- see the @$versioned@
-- section above.
class StructuralHash a where
  structuralHash :: Proxy a -> Word64
  default structuralHash :: (GStructuralHash (Rep a)) => Proxy a -> Word64
  structuralHash _ = gStructuralHash (Proxy :: Proxy (Rep a))

-- | The 'GHC.Generics' worker class behind 'StructuralHash': folds every
-- constructor's own arity (via ':*:'\/'U1'), every choice between
-- constructors (via ':+:'\/'V1'), and every field's own
-- 'Data.Typeable.TypeRep' (via 'K1') into one hash, so /any/ structural
-- edit to a derived-'GHC.Generics.Generic' type (a constructor gaining,
-- losing, or reordering fields; a field's own type changing; a
-- constructor being added or removed) changes the resulting hash.
class GStructuralHash (f :: Type -> Type) where
  gStructuralHash :: Proxy f -> Word64

instance GStructuralHash V1 where
  gStructuralHash _ = fnvHash "V1"

instance GStructuralHash U1 where
  gStructuralHash _ = fnvHash "U1"

instance (GStructuralHash a, GStructuralHash b) => GStructuralHash (a :+: b) where
  gStructuralHash _ =
    combineHashes (fnvHash "+") [gStructuralHash (Proxy :: Proxy a), gStructuralHash (Proxy :: Proxy b)]

instance (GStructuralHash a, GStructuralHash b) => GStructuralHash (a :*: b) where
  gStructuralHash _ =
    combineHashes (fnvHash "*") [gStructuralHash (Proxy :: Proxy a), gStructuralHash (Proxy :: Proxy b)]

instance Typeable c => GStructuralHash (K1 i c) where
  gStructuralHash _ = fnvHash (show (typeRep (Proxy :: Proxy c)))

instance GStructuralHash f => GStructuralHash (M1 i c f) where
  gStructuralHash _ = gStructuralHash (Proxy :: Proxy f)

-- | A plain FNV-1a hash of a 'String': not cryptographic, only needed to
-- differ (with overwhelming probability) whenever its input does.
fnvHash :: String -> Word64
fnvHash = foldl' (\acc ch -> (acc `xor` fromIntegral (fromEnum ch)) * 0x100000001b3) 0xcbf29ce484222325

combineHashes :: Word64 -> [Word64] -> Word64
combineHashes salt = foldl' (\acc h -> ((acc `xor` h) * 0x100000001b3) `xor` salt) salt
