{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

{- | A single, process-global, durable owner of "leftover" cleanup work.

'Api.Arkham.Lifecycle.releaseAll' (via
'Api.Arkham.Lifecycle.ManagedReleasePlan') must never convert a genuine
/asynchronous/ cancellation into a synchronous, data-shaped failure the
way a synchronously-failed release is reported -- doing so was itself a
MEDIUM-severity finding: it let a caller (production:
@Application.handler@, which has no persistent generation lock the way
@app\/DevelMain.hs@\'s own restart-protocol lock does) receive what
/looked like/ an ordinary, already-classified 'Api.Arkham.Lifecycle.ReleaseAllFailed'
for a shutdown that was, in fact, still asynchronously in flight, with
nothing anywhere left durably responsible for finishing it.

The fix: before ever rethrowing the /original/, unwrapped asynchronous
exception, the caller registers its own exact, still-outstanding
capability ('transferPendingCleanup') into the single global owner
defined here. This module -- exactly like 'DevelStoreLock' -- is a
genuinely separate, never-reloaded Cabal component, so
'globalPendingCleanupOwner' has exactly one incarnation for the entire
lifetime of the OS process, regardless of how many times
@Api.Arkham.Lifecycle@ or @Application@ (both part of the ordinary,
reloadable @arkham-api:lib@ component) are themselves edited and
reloaded during a live @ghci@ session -- giving /both/ @DevelMain@\'s
restart protocol and @Application.handler@\/@Application.appMain@ the
same kind of durable, reload-immune place to retain leftover cleanup
work that only @DevelMain@\'s own 'Foreign.Store'-published restart lock
had before.

Deliberately kept generic (plain @IO (Either SomeException ())@ "attempt
to finish this one outstanding capability" actions, not anything typed
in terms of 'Api.Arkham.Lifecycle.ManagedReleasePlan' itself): this
package is a dependency /of/ @arkham-api@, so it cannot itself import
anything defined there without an (impossible) dependency cycle. Each
registered action is still only ever driven through this module's own
serialized 'drainPendingCleanup' -- never handed back out, aliased, or
independently callable by anything except whatever transferred it in the
first place -- so custody genuinely transfers exactly once, rather than
merely being copied.
-}
module PendingCleanupOwner (
  PendingCleanupOwner,
  globalPendingCleanupOwner,
  newPendingCleanupOwner,
  transferPendingCleanup,
  drainPendingCleanup,
  hasPendingCleanup,
) where

import Control.Concurrent.MVar (MVar, modifyMVar, modifyMVar_, newMVar, readMVar)
import Control.Exception (SomeException, try)
import System.IO.Unsafe (unsafePerformIO)
import Prelude

-- | An opaque handle to a durable queue of not-yet-finished cleanup
-- capabilities. Never exposes the underlying actions themselves --
-- only 'transferPendingCleanup' (hand one in, exactly once) and
-- 'drainPendingCleanup' (attempt every one currently owned, serialized
-- against every other caller of either function against the same
-- handle).
newtype PendingCleanupOwner = PendingCleanupOwner (MVar [IO (Either SomeException ())])

-- | Build a fresh, independently-owned 'PendingCleanupOwner' -- for
-- tests only in practice; production code always uses
-- 'globalPendingCleanupOwner' so every caller across the whole process
-- shares the exact same durable owner.
newPendingCleanupOwner :: IO PendingCleanupOwner
newPendingCleanupOwner = PendingCleanupOwner <$> newMVar []

-- | The single, process-global owner shared by both @DevelMain@'s
-- restart protocol and @Application.handler@\/@Application.appMain@ --
-- see this module's own Haddock for why a plain CAF is only safe here
-- because it is defined in a genuinely separate, never-reloaded Cabal
-- component.
{-# NOINLINE globalPendingCleanupOwner #-}
globalPendingCleanupOwner :: PendingCleanupOwner
globalPendingCleanupOwner = unsafePerformIO newPendingCleanupOwner

-- | Durably register one still-outstanding "attempt to finish this"
-- capability. Returns immediately (never itself runs @action@) --
-- ownership transfers here unconditionally; a later 'drainPendingCleanup'
-- call is the only thing that will ever actually run it.
transferPendingCleanup :: PendingCleanupOwner -> IO (Either SomeException ()) -> IO ()
transferPendingCleanup (PendingCleanupOwner pending) action =
  modifyMVar_ pending (\actions -> pure (actions ++ [action]))

{- | Attempt every capability currently owned, in the order they were
transferred in, keeping (in the same relative order) only those that do
not themselves report success -- so a later call never re-attempts, nor
therefore risks double-releasing, anything a previous call already
finished. Serialized against every other caller of this function /and/
'transferPendingCleanup' against the same owner via its own private
'MVar', so two concurrent callers can never both attempt (and
potentially double-run) the same still-outstanding capability.

Returns 'Right' @()@ once nothing remains owned (whether because nothing
ever was, or because every capability just attempted genuinely
succeeded), or 'Left' the complete list of exceptions raised by whatever
capabilities are still outstanding after this attempt.
-}
drainPendingCleanup :: PendingCleanupOwner -> IO (Either [SomeException] ())
drainPendingCleanup (PendingCleanupOwner pending) =
  modifyMVar pending $ \actions -> do
    attempted <- traverse attempt actions
    let stillOwned = [action | (action, Just _) <- attempted]
        failures = [err | (_, Just err) <- attempted]
    pure (stillOwned, if null failures then Right () else Left failures)
 where
  -- Flattens "the action itself threw" (sync, or an async exception it
  -- chose to rethrow rather than report as data -- see
  -- 'Api.Arkham.Lifecycle.ManagedReleasePlan') and "the action returned
  -- normally but reported some releases still outstanding" into the
  -- same "still owned" outcome: either way, nothing here is dropped
  -- except a genuine, confirmed 'Right' @()@.
  attempt action = do
    outcome <- try @SomeException action
    pure $ case outcome of
      Left threwErr -> (action, Just threwErr)
      Right (Left dataErr) -> (action, Just dataErr)
      Right (Right ()) -> (action, Nothing)

-- | Whether anything at all is currently owned -- for tests\/
-- introspection only.
hasPendingCleanup :: PendingCleanupOwner -> IO Bool
hasPendingCleanup (PendingCleanupOwner pending) = not . null <$> readMVar pending
