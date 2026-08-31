{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

{- | A single, process-global, durable owner of "leftover" cleanup work,
implemented as a nonblocking 'Control.Concurrent.STM.STM' state machine
-- never a lock held across the execution of an arbitrary capability's
own 'IO' action.

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
defined here, receiving back an opaque 'CleanupReceipt' identifying that
one hand-off. This module -- exactly like 'DevelStoreLock' -- is a
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

Root-cause fix for a MEDIUM-severity finding against this module's own
structural predecessor (a single, private 'Control.Concurrent.MVar.MVar'
holding the entire outstanding queue, drained by 'modifyMVar' -- which
held that /one/ lock for the entire duration of running /every/ still-
outstanding capability's own 'IO' action): if one of those actions
itself needed to durably transfer further outstanding work back into
this exact owner (e.g. a retried 'Api.Arkham.Lifecycle.ManagedReleasePlan'
asynchronously interrupted /again/ mid-retry), doing so would have
required taking the very same 'MVar' the outer drain already held --
deadlocking that thread against itself -- and, separately, an
asynchronous exception landing anywhere in that single long-held
'Control.Concurrent.MVar.modifyMVar' call unwound it back to its
/original/, pre-drain queue (via 'Control.Concurrent.MVar.modifyMVar''s
own bracket-like exception safety), silently discarding every
capability that /had/ already succeeded earlier in that same drain pass.

Every entry owned here now lives in one 'Control.Concurrent.STM.TVar',
keyed by its own opaque 'CleanupReceipt', with an explicit @Queued@\/
@Running@\/@Terminated@ state: claiming an entry (moving it from @Queued@
to @Running@) is one nonblocking, masked-safe 'Control.Concurrent.STM.atomically'
transaction; the claimed capability's own 'IO' action then runs entirely
/outside/ any 'Control.Concurrent.STM.STM' transaction (so it may itself
freely call 'transferPendingCleanup' again -- inserting into the very
same 'Control.Concurrent.STM.TVar' -- without any risk of deadlocking
against its own claim); and the outcome (success, a synchronous failure,
or an asynchronous interruption) is committed back via one more atomic,
nonblocking transaction immediately afterwards -- for an asynchronous
interruption, specifically /before/ the original exception is ever
rethrown, so nothing already recorded, and nothing not yet even
attempted, is ever lost or left permanently stuck 'Running' (which would
otherwise deadlock every later attempt to claim it).
-}
module PendingCleanupOwner (
  PendingCleanupOwner,
  globalPendingCleanupOwner,
  newPendingCleanupOwner,
  CleanupReceipt,
  ReceiptOutcome (..),
  transferPendingCleanup,
  drainPendingCleanup,
  attemptCleanupReceipt,
  hasPendingCleanup,
) where

import Control.Concurrent.STM (
  STM,
  TVar,
  atomically,
  modifyTVar',
  newTVarIO,
  readTVar,
  readTVarIO,
  stateTVar,
  writeTVar,
 )
import Control.Exception (SomeException, mask, throwIO, try)
import Data.List.NonEmpty (NonEmpty (..))
import Data.List.NonEmpty qualified as NE
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import System.IO.Unsafe (unsafePerformIO)
import Prelude

-- | An opaque, comparable identity for exactly one durably-transferred
-- cleanup capability -- see 'transferPendingCleanup'. Never itself
-- constructible outside this module: the only way to obtain one is to
-- transfer a capability in the first place, and the only things one can
-- be used for afterwards are 'attemptCleanupReceipt' (poll\/attempt
-- /this exact/ capability, never any other) and equality\/display.
newtype CleanupReceipt = CleanupReceipt Integer
  deriving stock (Eq, Ord, Show)

-- | One transferred capability's own current state.
data Entry
  = -- | Not currently claimed by anyone; safe for the next caller
    -- (whether 'drainPendingCleanup' or 'attemptCleanupReceipt') to
    -- atomically claim.
    Queued (IO (Either SomeException ()))
  | -- | Currently claimed, and its own 'IO' action is genuinely running
    -- right now (outside this 'Control.Concurrent.STM.TVar''s own
    -- transactions) -- present specifically so a second, concurrent
    -- caller can observe this and skip it, rather than racing to run the
    -- exact same capability twice.
    Running
  | -- | Confirmed complete: nothing further to ever attempt again. Kept
    -- (rather than removed) so a later, repeated 'attemptCleanupReceipt'
    -- against the exact same receipt (production:
    -- 'Api.Arkham.Lifecycle.stopManagedGeneration', retrying a
    -- 'Api.Arkham.Lifecycle.RetireFailed' lock any number of times) can
    -- always truthfully report success, rather than "receipt not
    -- found" ambiguously meaning either "never existed" or "already
    -- long finished".
    Terminated

-- | An opaque handle to a durable, keyed collection of not-yet-finished
-- cleanup capabilities. Never exposes the underlying actions themselves.
data PendingCleanupOwner = PendingCleanupOwner
  { pcoEntries :: TVar (Map CleanupReceipt Entry)
  , pcoNextId :: TVar Integer
  }

-- | Build a fresh, independently-owned 'PendingCleanupOwner' -- for
-- tests only in practice; production code always uses
-- 'globalPendingCleanupOwner' so every caller across the whole process
-- shares the exact same durable owner.
newPendingCleanupOwner :: IO PendingCleanupOwner
newPendingCleanupOwner = PendingCleanupOwner <$> newTVarIO Map.empty <*> newTVarIO 0

-- | The single, process-global owner shared by both @DevelMain@'s
-- restart protocol and @Application.handler@\/@Application.appMain@ --
-- see this module's own Haddock for why a plain CAF is only safe here
-- because it is defined in a genuinely separate, never-reloaded Cabal
-- component.
{-# NOINLINE globalPendingCleanupOwner #-}
globalPendingCleanupOwner :: PendingCleanupOwner
globalPendingCleanupOwner = unsafePerformIO newPendingCleanupOwner

{- | Durably register one still-outstanding "attempt to finish this"
capability, returning an opaque 'CleanupReceipt' that identifies /this
exact/ hand-off (never any other, past or future) for later polling via
'attemptCleanupReceipt'. Purely a nonblocking, masked-safe
'Control.Concurrent.STM.atomically' transaction -- never itself runs
@action@, and never blocks -- so it is always safe to call from inside an
already-'Control.Exception.mask'ed span immediately before rethrowing an
asynchronous exception (see 'Api.Arkham.Lifecycle.releaseAll''s own
Haddock).
-}
transferPendingCleanup :: PendingCleanupOwner -> IO (Either SomeException ()) -> IO CleanupReceipt
transferPendingCleanup owner action = atomically $ do
  n <- stateTVar (pcoNextId owner) (\next -> (next, next + 1))
  let receipt = CleanupReceipt n
  modifyTVar' (pcoEntries owner) (Map.insert receipt (Queued action))
  pure receipt

-- | Atomically claim the single, first still-'Queued' entry (in
-- ascending receipt order, i.e. oldest transfer first), if any,
-- returning it and marking it 'Running' -- or 'Nothing' if nothing is
-- currently 'Queued'. Never blocks.
claimOneQueued :: PendingCleanupOwner -> STM (Maybe (CleanupReceipt, IO (Either SomeException ())))
claimOneQueued owner = do
  entries <- readTVar (pcoEntries owner)
  case [(receipt, action) | (receipt, Queued action) <- Map.toAscList entries] of
    [] -> pure Nothing
    (claimed@(receipt, _) : _) -> do
      writeTVar (pcoEntries owner) (Map.insert receipt Running entries)
      pure (Just claimed)

{- | Run exactly one already-claimed capability's own 'IO' action
(entirely outside any 'Control.Concurrent.STM.STM' transaction, so it may
itself freely call 'transferPendingCleanup' -- e.g. a retried
'Api.Arkham.Lifecycle.ManagedReleasePlan' asynchronously interrupted
/again/ mid-retry -- without any risk of deadlocking against its own
claim), then atomically commit its outcome:

* success -> 'Terminated' (never attempted again, but a future poll of
  the same receipt still truthfully reports success);
* a synchronous, data-shaped failure -> back to 'Queued' (available for
  the very next attempt, exactly unchanged, so this receipt's own
  history of who-has-tried-so-far is entirely the caller's own concern
  -- see 'Api.Arkham.Lifecycle.classifyRetirementFailure' and
  'Api.Arkham.Lifecycle.RetireFailed');
* the action itself throwing (in production, only ever an asynchronous
  exception -- see 'Api.Arkham.Lifecycle.runManagedReleasePlan''s own
  Haddock for why a synchronous failure can never actually escape it as
  a thrown exception at all) -> back to 'Queued', /then/ the original
  exception (never wrapped) is rethrown -- committing the requeue before
  ever rethrowing is exactly what stops this receipt from being left
  permanently stuck 'Running' (which would deadlock every later attempt
  to claim it) the instant this call's own caller is itself cancelled
  while this capability happened to be running.

'action' itself runs fully interruptible (under 'restore', i.e. exactly
as asynchronously-cancellable as this call's own caller left it), but
every commit of the outcome back into 'pcoEntries' below is masked: once
'action' has itself returned or been caught, a /new/ asynchronous
exception arriving in the gap between that and the commit must never be
allowed to skip the commit and leave this receipt stuck 'Running'
forever (which would permanently wedge every later
'drainPendingCleanup'\/'attemptCleanupReceipt' call against it). Each
commit below is a single, non-retrying 'atomically' transaction that
runs to completion without blocking, so masking around it is genuinely
sufficient: GHC only delivers a pending asynchronous exception to a
masked thread at its own next interruptible operation, and an
'atomically' transaction that never calls 'retry' is not itself such a
point until after it has already committed.
-}
attemptOne :: PendingCleanupOwner -> CleanupReceipt -> IO (Either SomeException ()) -> IO (Either (NonEmpty SomeException) ())
attemptOne owner receipt action = mask $ \restore -> do
  outcome <- try @SomeException (restore action)
  case outcome of
    Right (Right ()) -> do
      atomically $ modifyTVar' (pcoEntries owner) (Map.insert receipt Terminated)
      pure (Right ())
    Right (Left err) -> do
      atomically $ modifyTVar' (pcoEntries owner) (Map.insert receipt (Queued action))
      pure (Left (err :| []))
    Left threwErr -> do
      atomically $ modifyTVar' (pcoEntries owner) (Map.insert receipt (Queued action))
      throwIO threwErr

{- | Attempt every capability currently owned, claiming (and running) one
entry at a time -- never a batch claimed all at once -- specifically so
that an asynchronous exception interrupting one entry's own attempt
(propagated straight out of this call: see 'attemptOne') leaves every
/other/ entry not yet reached still safely 'Queued', never stuck
'Running' forever. Returns 'Right' @()@ once nothing remains owned
(whether because nothing ever was, or because every capability
attempted this call genuinely succeeded), or 'Left' the complete list of
synchronous failures collected along the way (continuing to attempt
independent, later entries even after an earlier one fails
synchronously -- exactly like 'Api.Arkham.Lifecycle.ManagedReleasePlan'
own single pass).
-}
drainPendingCleanup :: PendingCleanupOwner -> IO (Either [SomeException] ())
drainPendingCleanup owner = go []
 where
  go failuresAcc = do
    claimed <- atomically (claimOneQueued owner)
    case claimed of
      Nothing -> pure (if null failuresAcc then Right () else Left (reverse failuresAcc))
      Just (receipt, action) -> do
        result <- attemptOne owner receipt action
        case result of
          Right () -> go failuresAcc
          Left errs -> go (reverse (NE.toList errs) ++ failuresAcc)

-- | The outcome of one 'attemptCleanupReceipt' call against a specific
-- receipt.
data ReceiptOutcome
  = -- | This receipt's own capability has now been confirmed complete
    -- (whether by this exact call, or by any earlier
    -- 'attemptCleanupReceipt'\/'drainPendingCleanup' call against it --
    -- see 'Entry''s own @Terminated@ Haddock).
    ReceiptSucceeded
  | -- | This attempt itself found the capability still outstanding and
    -- ran it, but it failed synchronously again; the exact failure(s)
    -- from /this/ attempt are given back (never accumulated history --
    -- see 'Api.Arkham.Lifecycle.RetireFailed', which is the one that
    -- accumulates across repeated calls).
    ReceiptFailed (NonEmpty SomeException)
  | -- | This receipt is currently claimed and being run by some /other/
    -- concurrent caller (another 'attemptCleanupReceipt' against the
    -- same receipt, or a concurrent 'drainPendingCleanup') -- this call
    -- made no attempt at all, to avoid ever running the same capability
    -- twice at once.
    ReceiptBusy
  deriving stock (Show)

{- | Poll, and if currently safe to do so, attempt exactly the one
capability identified by @receipt@ -- never any other entry this owner
may also be holding. Used by 'Api.Arkham.Lifecycle.stopManagedGeneration'
to make a 'Api.Arkham.Lifecycle.RetireFailed' lock's own retained
'CleanupReceipt' genuinely, repeatedly retriable (and, on success,
observably clearable back to 'Api.Arkham.Lifecycle.NotStarted'), whether
or not this exact call is the one that actually finishes the work (a
concurrent 'drainPendingCleanup' -- e.g. @DevelMain@'s own opportunistic
call, or 'Application.handler''s -- may finish it first; either way, the
very next poll against the exact same receipt observes 'ReceiptSucceeded').
-}
attemptCleanupReceipt :: PendingCleanupOwner -> CleanupReceipt -> IO ReceiptOutcome
attemptCleanupReceipt owner receipt = do
  claim <- atomically $ do
    entries <- readTVar (pcoEntries owner)
    case Map.lookup receipt entries of
      -- Never actually reachable given this module's own construction
      -- (a 'CleanupReceipt' is only ever obtainable from
      -- 'transferPendingCleanup', which always inserts an entry, and no
      -- entry is ever removed -- only transitioned to 'Terminated');
      -- treated as vacuously complete rather than looping forever on a
      -- receipt that could never legitimately still be outstanding.
      Nothing -> pure ClaimTerminated
      Just Terminated -> pure ClaimTerminated
      Just Running -> pure ClaimBusy
      Just (Queued action) -> do
        writeTVar (pcoEntries owner) (Map.insert receipt Running entries)
        pure (ClaimedForAttempt action)
  case claim of
    ClaimTerminated -> pure ReceiptSucceeded
    ClaimBusy -> pure ReceiptBusy
    ClaimedForAttempt action -> do
      result <- attemptOne owner receipt action
      pure $ case result of
        Right () -> ReceiptSucceeded
        Left errs -> ReceiptFailed errs

-- | The result of atomically deciding whether (and how)
-- 'attemptCleanupReceipt' may proceed against a specific receipt.
data ReceiptClaim
  = ClaimTerminated
  | ClaimBusy
  | ClaimedForAttempt (IO (Either SomeException ()))

-- | Whether anything at all is currently owned (queued or actively
-- running; excludes 'Terminated' entries) -- for tests\/introspection
-- only.
hasPendingCleanup :: PendingCleanupOwner -> IO Bool
hasPendingCleanup owner = any isOutstanding . Map.elems <$> readTVarIO (pcoEntries owner)
 where
  isOutstanding = \case
    Terminated -> False
    _ -> True
