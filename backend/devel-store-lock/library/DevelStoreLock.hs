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

  -- * Legacy-slot-safe schema-versioned access
  -- $legacySlot
  LegacyDevelStoreSlotOccupied (..),
  getOrCreateVersionedStoreCheckingLegacySlot,
  getExistingVersionedStoreCheckingLegacySlot,

  -- * Test support
  withStoreAccessLock,
) where

import Control.Concurrent.MVar (MVar, newMVar, withMVar)
import Control.Exception (Exception, throwIO)
import Data.Bits (xor)
import Data.Kind (Type)
import Data.Proxy (Proxy (..))
import Data.Typeable (Typeable, typeRep)
import Data.Word (Word32, Word64)
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

{- | Perform an arbitrary 'IO' action serialized against this module's
own internal 'storeAccessLock', exactly as every other exported function
in this module already does -- exported purely for test code that needs
to perform a raw, direct 'Foreign.Store' operation (for example,
simulating an out-of-band publish at a specific slot to set up a
regression fixture) without corrupting, or being corrupted by, any
genuinely concurrent, properly-synchronized caller of this module's own
ordinary API: 'Foreign.Store' itself provides zero synchronization of
its own (every one of its operations, including even ones that touch
entirely different slot numbers, share one single, global, mutable
table), so any direct access from outside this module that is not
itself serialized against 'storeAccessLock' can race -- and, in
practice, does race, under this test suite's own @parallel@ execution --
against ordinary callers going through 'getOrCreateStore' \/
'getOrCreateVersionedStore' \/ etc.
-}
withStoreAccessLock :: IO a -> IO a
withStoreAccessLock action = withMVar storeAccessLock (\() -> action)

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
  withMVar storeAccessLock $ \() -> unguardedGetOrCreateVersionedStore expected store slot mkDefault

-- | The composed 'Foreign.Store.lookupStore'\/'Foreign.Store.writeStore'
-- body of 'getOrCreateVersionedStore', factored out so
-- 'getOrCreateVersionedStoreCheckingLegacySlot' can run it under the
-- exact same already-held 'storeAccessLock' span as its own legacy-slot
-- occupancy check, rather than releasing and immediately reacquiring the
-- lock in between (which would reopen a window for a concurrent caller
-- to act between the two).
unguardedGetOrCreateVersionedStore :: Word64 -> Store (Word64, a) -> Word32 -> IO a -> IO (VersionedAccess a)
unguardedGetOrCreateVersionedStore expected store slot mkDefault = do
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
  withMVar storeAccessLock $ \() -> unguardedGetExistingVersionedStore expected slot

-- | The composed body of 'getExistingVersionedStore', factored out for
-- the same reason as 'unguardedGetOrCreateVersionedStore'.
unguardedGetExistingVersionedStore :: Word64 -> Word32 -> IO (Maybe (VersionedAccess a))
unguardedGetExistingVersionedStore expected slot = do
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

{- $legacySlot
Even 'getOrCreateVersionedStore'\/'getExistingVersionedStore' are only as
safe as the /slot number/ a caller picks for them: they can only ever
detect a shape mismatch between two publications made at the /exact
same/ slot. If a slot number that some strictly /earlier/, differently
shaped (untagged, i.e. not even a @(Word64, a)@ pair at all) publication
scheme once used is later reused, unchanged, for a /new/, differently
laid-out @(Word64, a)@ scheme, then a live process that still holds an
old-shaped value published there (from before this exact code was ever
loaded) would have 'Foreign.Store.readStore' coerce it directly as the
new @(Word64, a)@ representation -- undefined behaviour (a crash, not
merely a wrong answer), and reached /before/ either scheme's own tag is
ever compared, because the two schemes don't even agree on the shape a
tag would be found at. This is exactly the root cause of a MEDIUM-severity
finding: @app\/DevelMain.hs@'s own restart-lock slot changed from a bare
@Store (MVar (RestartState ThreadId))@ to a tagged @Store (Word64, MVar
(RestartState ThreadId))@ across two commits, but kept the exact same
slot number across that change.

The functions below close this: given the caller's own current slot
/and/ the exact legacy slot number some strictly earlier, incompatible
scheme once used, they first check -- via 'Foreign.Store.lookupStore'
alone, which performs no decoding\/coercion of the underlying value at
all (only 'Foreign.Store.readStore' does) -- whether anything at all is
currently published at that /legacy/ slot. If so, this refuses outright
(throwing 'LegacyDevelStoreSlotOccupied', never touching, forcing, or
reading the legacy value itself) rather than proceeding to read\/write
the new slot at all: the only safe recovery from a live process actually
holding an old-shaped value is a genuinely fresh 'Foreign.Store' table,
i.e. a full process\/@ghci@ restart, exactly as 'DevelStoreSchemaStale'
already requires for a same-slot shape mismatch. This check, like every
other composed access this module performs, is itself serialized via
'storeAccessLock' against every other caller (including
'getOrCreateStore'\/'getExistingStore'\/'getOrCreateVersionedStore'\/
'getExistingVersionedStore', and every other call to these two
functions), so a concurrent legacy-slot check and a concurrent ordinary
access can never race each other either.
-}

{- | Thrown by 'getOrCreateVersionedStoreCheckingLegacySlot'\/'getExistingVersionedStoreCheckingLegacySlot'
instead of ever proceeding to touch their own (new) slot while a
strictly earlier, incompatible publication scheme's own slot is still
occupied -- see the @$legacySlot@ section above. Deliberately carries
only the legacy slot number itself: the value published there is never
read, forced, or otherwise touched by this module at all, so there is
nothing else safe to report about it.
-}
newtype LegacyDevelStoreSlotOccupied = LegacyDevelStoreSlotOccupied
  { legacyDevelStoreSlotOccupiedSlot :: Word32
  }
  deriving stock (Eq, Show)

instance Exception LegacyDevelStoreSlotOccupied

-- | Whether /anything/ is currently published at the given slot,
-- without ever decoding, coercing, forcing, or otherwise touching
-- whatever value (if any) is actually stored there -- 'Foreign.Store.lookupStore'
-- alone (never paired with 'Foreign.Store.readStore') is sufficient for
-- this and is documented to perform no such decoding itself; the
-- 'Store' this constructs is tagged with a throwaway phantom type
-- (@()@) purely so this function's own type is total, and that tag is
-- never actually used to decode anything -- only 'lookupStore''s own
-- boolean-shaped \"is the slot occupied at all\" answer is.
isStoreSlotOccupied :: Word32 -> IO Bool
isStoreSlotOccupied slot = maybe False (const True) <$> (lookupStore slot :: IO (Maybe (Store ())))

{- | Exactly 'getOrCreateVersionedStore', except it first refuses (via
'isStoreSlotOccupied', /never/ 'Foreign.Store.readStore') to proceed at
all if @legacySlot@ is currently occupied -- see the @$legacySlot@
section above. Serialized, via the same 'storeAccessLock', against every
other caller of any function in this module, so the legacy-slot check
and the subsequent versioned access it guards happen as one atomic,
uninterrupted composition -- no other caller can publish anything at
either slot in between.
-}
getOrCreateVersionedStoreCheckingLegacySlot
  :: Word32
  -- ^ legacySlot: the slot number a strictly earlier, incompatible
  -- publication scheme once used -- never itself read, only checked for
  -- occupancy.
  -> Word64
  -> Store (Word64, a)
  -> IO a
  -> IO (VersionedAccess a)
getOrCreateVersionedStoreCheckingLegacySlot legacySlot expected store@(Store slot) mkDefault =
  withMVar storeAccessLock $ \() -> do
    legacyOccupied <- isStoreSlotOccupied legacySlot
    if legacyOccupied
      then throwIO (LegacyDevelStoreSlotOccupied legacySlot)
      else unguardedGetOrCreateVersionedStore expected store slot mkDefault

-- | The read-only counterpart of 'getOrCreateVersionedStoreCheckingLegacySlot'.
getExistingVersionedStoreCheckingLegacySlot
  :: Word32
  -> Word64
  -> Store (Word64, a)
  -> IO (Maybe (VersionedAccess a))
getExistingVersionedStoreCheckingLegacySlot legacySlot expected (Store slot) =
  withMVar storeAccessLock $ \() -> do
    legacyOccupied <- isStoreSlotOccupied legacySlot
    if legacyOccupied
      then throwIO (LegacyDevelStoreSlotOccupied legacySlot)
      else unguardedGetExistingVersionedStore expected slot

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
