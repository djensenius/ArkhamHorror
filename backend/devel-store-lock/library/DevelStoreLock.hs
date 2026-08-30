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
) where

import Control.Concurrent.MVar (MVar, newMVar, withMVar)
import Foreign.Store (Store (..), lookupStore, readStore, writeStore)
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
