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
  LegacyDevelStoreSlotOccupied (..),
  RestartState (..),
  StopOutcome (..),
  drainOwnedCleanup,
  getExistingStoreCheckingLegacySlot,
  getOrCreateStoreCheckingLegacySlot,
  releaseAllRecordingReceipt,
  restartManagedGeneration,
  restartStateSchemaHash,
  shutdownThenDeliverRecordingReceipt,
  stopManagedGeneration,
 )
import Application (getApplicationRepl, shutdownApp, shutdownAppActions)
import Prelude

import Control.Concurrent
import Control.Exception (catch)
import Data.IORef (newIORef)
import Foreign.Store
import GHC.Word
import Network.Wai.Handler.Warp

{- | Start or restart the server.
newStore is from foreign-store.
A Store holds onto some data across ghci reloads
-}
update :: IO ()
update =
  update' `catch` reportSchemaStale `catch` reportLegacySlotOccupied
 where
  update' = do
    lock <- getOrCreateStoreCheckingLegacySlot legacyRestartLockStoreNum restartLockStore restartStateSchemaHash (newMVar NotStarted)
    restartManagedGeneration
      lock
      killThread
      getApplicationRepl
      (\(_, site, _) -> shutdownApp site)
      (\(port, _site, app) -> runSettings (setPort port defaultSettings) app)
      -- 'shutdownThenDeliverRecordingReceipt' MUST finish (and deliver a
      -- result) before a /later/ 'update' call's own retirement of this
      -- same generation (inside 'restartManagedGeneration', which awaits
      -- this exact @newDone@ before ever attempting to start a
      -- replacement) can unblock -- but only on 'RetiredCleanly':
      -- signalling unconditionally, or before shutdown actually
      -- finished, would let a replacement foundation's supervisor start
      -- running concurrently with this one still being torn down, or (if
      -- shutdown threw) with this one never actually torn down at all.
      -- This ordering closes both an overlapping-generations window and
      -- a would-be deadlock if shutdown itself fails or is cancelled.
      --
      -- A fresh, single-write @receiptSink@ is created for /this exact
      -- attempt/ (never reused across restarts): if @releaseAllRecordingReceipt@
      -- (via 'shutdownApp') is asynchronously interrupted and durably
      -- transfers its own remainder away, the resulting receipt is
      -- carried into 'RestartState'\'s own 'RetireFailed' (see
      -- 'classifyRetirementFailure'), so a /later/ 'shutdown'\/'update'
      -- call can eventually observe that transferred work actually
      -- finishing and correctly clear this lock, rather than staying
      -- stuck forever -- see 'RetirementRetry''s own Haddock.
      (\(_, site, _) _result newDone -> do
        receiptSink <- newIORef Nothing
        -- Runs 'Application.shutdownAppActions' directly through exactly
        -- ONE managed release plan -- never wraps the already-managed
        -- 'Application.shutdownApp' in a second, outer plan (see
        -- 'Application.shutdownApp''s own Haddock for the
        -- MEDIUM-severity duplicate-release finding that composition
        -- caused).
        shutdownThenDeliverRecordingReceipt receiptSink (releaseAllRecordingReceipt receiptSink (shutdownAppActions site)) newDone)
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
  shutdown' `catch` reportSchemaStale `catch` reportLegacySlotOccupied
 where
  shutdown' = do
    mlock <- getExistingStoreCheckingLegacySlot legacyRestartLockStoreNum restartLockStore restartStateSchemaHash
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

-- | Report a still-occupied legacy 'Foreign.Store' slot (see
-- 'legacyRestartLockStoreNum''s own Haddock) in the same unmistakable
-- way: this is /never/ safe to retry or paper over by simply picking yet
-- another slot number at runtime -- only a genuinely fresh process\/GHCi
-- session (whose 'Foreign.Store' table starts empty) recovers from it.
reportLegacySlotOccupied :: LegacyDevelStoreSlotOccupied -> IO ()
reportLegacySlotOccupied (LegacyDevelStoreSlotOccupied legacySlot) =
  putStrLn $
    "DevelMain: legacy restart-lock slot "
      <> show legacySlot
      <> " is still occupied by an old, structurally incompatible publication\n"
      <> "from a strictly earlier version of this module. This process's Foreign.Store\n"
      <> "table cannot safely be used any further: fully quit and restart GHCi (do NOT\n"
      <> "simply :r and retry, and do NOT proceed by editing this module to pick yet\n"
      <> "another slot number at runtime) before running update/shutdown again."


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

@update@ retrieves this cell via 'Api.Arkham.Lifecycle.getOrCreateStoreCheckingLegacySlot',
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
'getOrCreateStoreCheckingLegacySlot' retrieves the existing lock if one
is already published, and is itself safe to call concurrently (see its
own Haddock, and 'DevelStoreLock.getOrCreateVersionedStoreCheckingLegacySlot''s:
the actual process-global serializer this delegates to lives in the
@devel-store-lock@ package -- a genuinely separate Cabal component from
@arkham-api@, not merely another module within it, so it is pre-compiled
object code and
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
'Api.Arkham.Lifecycle.getExistingStoreCheckingLegacySlot' (also
delegating to @devel-store-lock@) instead of its own prior direct,
unsynchronized 'Foreign.Store.lookupStore'\/'Foreign.Store.readStore'
calls, so @update@ and @shutdown@ are guaranteed to observe and
serialize through the exact same lock, never two independently-raced
ones.

@104@ was chosen as a genuinely /new/ slot after a git-history-confirmed
MEDIUM-severity finding proved @103@ itself unsafe to keep reusing:
@103@'s own /shape/ silently changed, in place, from an untagged
@Store (MVar (RestartState ThreadId))@ (the immediately prior version of
this module) to the current tagged @Store (Word64, MVar (RestartState
ThreadId))@ -- and 'Foreign.Store' has no runtime type check at all, so
a live GHCi session that still held an old-shaped value published at
@103@ would have a subsequent 'Foreign.Store.readStore' there coerce it
directly as the new, differently laid-out pair type: undefined behaviour
(a crash), reached /before/ either scheme's own schema tag could ever be
compared, because the two schemes don't even agree on the shape a tag
would be found at -- 'Api.Arkham.Lifecycle.DevelStoreSchemaStale'\'s own
same-slot hash comparison cannot detect or prevent this, since it can
only ever run /after/ successfully decoding the pair shape it itself
expects.

@104@ alone is therefore not sufficient by itself: an already-running
GHCi session's @103@ must also be accounted for, not merely abandoned
via a comment. 'legacyRestartLockStoreNum' names that exact prior slot;
@update@\/@shutdown@ both call
'Api.Arkham.Lifecycle.getOrCreateStoreCheckingLegacySlot'\/'Api.Arkham.Lifecycle.getExistingStoreCheckingLegacySlot'
(never the plain, non-legacy-aware versions) specifically so that, before
ever touching @104@ itself, they first check -- via
'Foreign.Store.lookupStore' alone, which performs no decoding\/coercion of
whatever is actually published there -- whether @103@ is still occupied,
and refuse outright (throwing 'LegacyDevelStoreSlotOccupied', see
'reportLegacySlotOccupied') rather than proceeding at all if so. The only
safe recovery, exactly as for 'DevelStoreSchemaStale', is a genuinely
fresh process\/GHCi restart (whose 'Foreign.Store' table starts
completely empty), never simply retrying or picking yet another slot
number at runtime while the same process\/session remains alive.

This slot's own published value is now additionally tagged with
'Api.Arkham.Lifecycle.RestartState''s own automatically-computed
structural shape signature (see 'Api.Arkham.Lifecycle.restartStateSchemaHash'),
rather than relying solely on hand-bumping this slot number: this
constructor previously gained a new field
('Api.Arkham.Lifecycle.RetireFailed') without this slot number itself
being bumped to match, a MEDIUM-severity finding under independent
review, precisely because a hand-maintained number is exactly the kind
of discipline that is easy to forget. 'getOrCreateStoreCheckingLegacySlot'\/'getExistingStoreCheckingLegacySlot'
now compare that tag against 'Api.Arkham.Lifecycle.restartStateSchemaHash'
on every access and refuse (throwing 'Api.Arkham.Lifecycle.DevelStoreSchemaStale'
-- see 'reportSchemaStale') to ever touch a value published under an
incompatible shape, rather than either coercing it unsafely or silently
orphaning it by overwriting it with a fresh default.
-}
restartLockStore :: Store (Word64, MVar (RestartState ThreadId))
restartLockStore = Store restartLockStoreNum

restartLockStoreNum :: Word32
restartLockStoreNum = 104

{- | The exact slot number a strictly earlier, incompatible
publication scheme (an untagged @Store (MVar (RestartState ThreadId))@)
once used for this exact restart-protocol lock, before 'restartLockStoreNum'
itself moved to @104@ -- see 'restartLockStore''s own Haddock above for
the full, git-history-confirmed segfault this exists to close. Never
read, forced, or otherwise touched by this module: only ever checked
for bare occupancy, by 'getOrCreateStoreCheckingLegacySlot'\/'getExistingStoreCheckingLegacySlot'
themselves (via 'Api.Arkham.Lifecycle.getOrCreateStoreCheckingLegacySlot'\/'Api.Arkham.Lifecycle.getExistingStoreCheckingLegacySlot',
which in turn only ever call 'Foreign.Store.lookupStore', never
'Foreign.Store.readStore', against it).
-}
legacyRestartLockStoreNum :: Word32
legacyRestartLockStoreNum = 103
