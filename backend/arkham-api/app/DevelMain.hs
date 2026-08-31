-- \$ stack ghci arkham-horror-backend:lib --no-load --work-dir .stack-work-devel
--
-- 2. Load this module
--
-- > :l app/DevelMain.hs
--
-- 3. Run @update@
--
-- > DevelMain.update
--
-- 4. Your app should now be running, you can connect at http://localhost:3000
--
-- 5. Make changes to your code
--
-- 6. After saving your changes, reload by running:
--
-- > :r
-- > DevelMain.update
--
-- You can also call @DevelMain.shutdown@ to stop the app
--
-- There is more information about this approach,
-- on the wiki: https://github.com/yesodweb/yesod/wiki/ghci
--
-- WARNING: GHCi does not notice changes made to your template files.
-- If you change a template, you'll need to either exit GHCi and reload,
-- or manually @touch@ another Haskell module.

{- | Running your app inside GHCi.

This option provides significantly faster code reload compared to
@yesod devel@. However, you do not get automatic code reload
(which may be a benefit, depending on your perspective). To use this:

1. Start up GHCi
-}
module DevelMain where

import Api.Arkham.Lifecycle (
  DevelStoreSchemaStale (..),
  RestartState (..),
  StopOutcome (..),
  drainOwnedCleanup,
  getExistingStore,
  getOrCreateStore,
  restartManagedGeneration,
  restartStateSchemaHash,
  shutdownThenDeliver,
  stopManagedGeneration,
 )
import Application (getApplicationRepl, shutdownApp)
import Prelude

import Control.Concurrent
import Control.Exception (catch)
import Foreign.Store
import GHC.Word
import Network.Wai.Handler.Warp

{- | Start or restart the server.
newStore is from foreign-store.
A Store holds onto some data across ghci reloads
-}
update :: IO ()
update =
  update' `catch` reportSchemaStale
 where
  update' = do
    lock <- getOrCreateStore restartLockStore restartStateSchemaHash (newMVar NotStarted)
    restartManagedGeneration
      lock
      killThread
      getApplicationRepl
      (\(_, site, _) -> shutdownApp site)
      (\(port, _site, app) -> runSettings (setPort port defaultSettings) app)
      -- 'shutdownThenDeliver' MUST finish (and deliver a result) before a
      -- /later/ 'update' call's own retirement of this same generation
      -- (inside 'restartManagedGeneration', which awaits this exact
      -- @newDone@ before ever attempting to start a replacement) can
      -- unblock -- but only on 'Right': signalling unconditionally, or
      -- before shutdown actually finished, would let a replacement
      -- foundation's supervisor start running concurrently with this one
      -- still being torn down, or (if shutdown threw) with this one never
      -- actually torn down at all. This ordering closes both an
      -- overlapping-generations window and a would-be deadlock if shutdown
      -- itself fails or is cancelled.
      (\(_, site, _) _result newDone -> shutdownThenDeliver (shutdownApp site) newDone)
    -- Opportunistically attempt to finish any cleanup capability a
    -- /previous/ generation's own asynchronously-interrupted shutdown
    -- durably transferred away (see 'drainOwnedCleanup''s own Haddock):
    -- never blocks or fails @update@ itself if there is nothing pending,
    -- or if what is pending still fails.
    _ <- drainOwnedCleanup
    pure ()

-- | kill the server
shutdown :: IO ()
shutdown =
  shutdown' `catch` reportSchemaStale
 where
  shutdown' = do
    mlock <- getExistingStore restartLockStore restartStateSchemaHash
    case mlock of
      -- no server running
      Nothing -> putStrLn "no Yesod app running"
      Just lock -> do
        outcome <- stopManagedGeneration lock killThread
        case outcome of
          NothingWasRunning -> putStrLn "no Yesod app running"
          StoppedCleanly -> putStrLn "Yesod app is shutdown"
          StopFailed err -> putStrLn ("Yesod app shutdown FAILED (resources may not be released): " <> show err)
    _ <- drainOwnedCleanup
    pure ()

-- | Report a stale-schema 'Foreign.Store' slot in a way that makes the
-- only safe recovery (a full process\/GHCi restart -- never merely
-- reloading again) unmistakable, rather than an opaque exception dump.
reportSchemaStale :: DevelStoreSchemaStale -> IO ()
reportSchemaStale (DevelStoreSchemaStale stored expected) =
  putStrLn $
    "DevelMain: restart-lock slot "
      <> show restartLockStoreNum
      <> " holds an incompatible, since-changed value (stored schema "
      <> show stored
      <> " /= current schema "
      <> show expected
      <> "). This process's Foreign.Store table is now stale: fully quit and\n"
      <> "restart GHCi (do NOT simply :r and retry) before running update/shutdown again."

{- | The single, authoritative 'Api.Arkham.Lifecycle.RestartState' cell:
see its own Haddock for why this replaces the scaffold's original
separate @tidStoreNum@\/@doneStore@ slots. Consolidating both into one
'Control.Concurrent.MVar.MVar' (rather than two independently-populated
'Foreign.Store' slots) closes two gaps a prior version of this module
had: the two slots were populated by separate, unmasked statements (an
async-exception gap between them, before 'restartManagedGeneration''s own
'Control.Exception.mask' ever began protecting anything), and -- more
fundamentally -- every generation shared the exact same @done@ 'MVar'
across restarts, so a first-generation acquisition\/spawn failure (which
durably filled that shared cell with 'Left', but never durably published
a 'Control.Concurrent.ThreadId') left a /later/, entirely successful
generation's own eventual shutdown result silently discarded, corrupting
every subsequent restart's view of what had actually happened.

'restartManagedGeneration'\/'stopManagedGeneration' now serialize every
caller through this exact 'MVar' (which doubles as the mutual-exclusion
lock, not just storage) and give each live generation its own, freshly
created completion cell (see 'Api.Arkham.Lifecycle.Running'), so there is
no shared cell left for any generation's outcome to ever collide with,
and two concurrent 'update' calls can no longer independently populate
two separate slots and spawn two untracked generations.

@update@ retrieves this cell via 'Api.Arkham.Lifecycle.getOrCreateStore',
never @Foreign.Store.storeAction@ (a prior version of this module's own
choice): @storeAction@ /always/ runs its supplied action and /always/
overwrites the store, even if a value was already published there -- so
a second, later @update@ call (e.g. after @:r@\/@DevelMain.update@,
GHCi's own normal restart-protocol usage) would silently fabricate a
brand-new, empty @newMVar NotStarted@ and publish it over the lock the
/first/ call already created and is actively serializing through,
orphaning whatever generation that first call started and defeating the
mutual exclusion 'restartManagedGeneration'\/'stopManagedGeneration'
otherwise provide entirely (including for genuinely concurrent callers,
who would each fabricate and publish their own independent lock).
'getOrCreateStore' retrieves the existing lock if one is already
published, and is itself safe to call concurrently (see its own
Haddock, and 'DevelStoreLock.getOrCreateStore''s: the actual
process-global serializer this delegates to lives in the @devel-store-lock@
package -- a genuinely separate Cabal component from @arkham-api@, not
merely another module within it, so it is pre-compiled object code and
is /never/ subject to GHCi's own @:r@\/@:l@ re-interpretation of this
module or 'Api.Arkham.Lifecycle' themselves. An earlier version of this
fix instead defined the serializing lock as a plain top-level CAF inside
'Api.Arkham.Lifecycle' -- a normal, reloadable home-package library
module -- so a live GHCi session that had reloaded that module (e.g.
because /its/ own source changed, or transitively via some other edit)
could end up with two distinct incarnations of that lock, letting an
old and a new incarnation of @update@\/@shutdown@ each serialize only
against their own copy while racing the other. Moving the lock itself
into @devel-store-lock@ removes that possibility structurally, rather
than relying on the two calls happening to run before any such
reload.

@shutdown@ now retrieves this exact same cell via
'Api.Arkham.Lifecycle.getExistingStore' (also delegating to
@devel-store-lock@) instead of its own prior direct, unsynchronized
'Foreign.Store.lookupStore'\/'Foreign.Store.readStore' calls, so
@update@ and @shutdown@ are guaranteed to observe and serialize through
the exact same lock, never two independently-raced ones.

@103@ is a fresh slot, never used by any prior version of this module
(which used @1@\/@0@ in the original Yesod scaffold, then @100@\/@101@
once 'shutdownThenDeliver' needed an @Either@, then @102@ once both
slots were consolidated into one 'Api.Arkham.Lifecycle.RestartState'):
'Foreign.Store' has no runtime type check at all, so reusing a slot whose
stored type has changed is memory-unsafe in an already-running GHCi
session that populated it under an old type -- a fresh slot avoids that
rather than papering over it; a full GHCi restart (already the normal
remedy whenever this module's own persisted types change) picks up the
new slot cleanly.

This slot's own published value is now additionally tagged with
'Api.Arkham.Lifecycle.RestartState''s own automatically-computed
structural shape signature (see 'Api.Arkham.Lifecycle.restartStateSchemaHash'),
rather than relying solely on hand-bumping this slot number: this
constructor previously gained a new field
('Api.Arkham.Lifecycle.RetireFailed') without this slot number itself
being bumped to match, a MEDIUM-severity finding under independent
review, precisely because a hand-maintained number is exactly the kind
of discipline that is easy to forget. 'getOrCreateStore'\/'getExistingStore'
now compare that tag against 'Api.Arkham.Lifecycle.restartStateSchemaHash'
on every access and refuse (throwing 'Api.Arkham.Lifecycle.DevelStoreSchemaStale'
-- see 'reportSchemaStale') to ever touch a value published under an
incompatible shape, rather than either coercing it unsafely or silently
orphaning it by overwriting it with a fresh default.
-}
restartLockStore :: Store (Word64, MVar (RestartState ThreadId))
restartLockStore = Store restartLockStoreNum

restartLockStoreNum :: Word32
restartLockStoreNum = 103
