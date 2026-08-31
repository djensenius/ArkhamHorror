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
to @Running@), running the claimed capability's own 'IO' action (entirely
/outside/ any 'Control.Concurrent.STM.STM' transaction, so it may itself
freely call 'transferPendingCleanup' again -- inserting into the very
same 'Control.Concurrent.STM.TVar' -- without any risk of deadlocking
against its own claim), and committing the outcome (success, a
synchronous failure, or an asynchronous interruption) back afterwards
all happen under one single, continuous 'Control.Exception.mask' (see
'attemptClaimed'): only the action itself runs under that mask's own
@restore@, i.e. exactly as asynchronously-cancellable as its caller left
it. For an asynchronous interruption, the requeue commit specifically
happens /before/ the original exception is ever rethrown, so nothing
already recorded, and nothing not yet even attempted (including the
claim itself), is ever lost or left permanently stuck 'Running' (which
would otherwise deadlock every later attempt to claim it).
-}
module PendingCleanupOwner (
  PendingCleanupOwner,
  globalPendingCleanupOwner,
  newPendingCleanupOwner,
  CleanupReceipt,
  CleanupAttempt (..),
  ReceiptOutcome (..),
  transferPendingCleanup,
  transferPendingCleanupEphemeralOnce,
  drainPendingCleanup,
  drainPendingCleanupBounded,
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
import Control.Monad (void, when)
import Data.Foldable (for_)
import Data.Functor ((<&>))
import Data.List.NonEmpty (NonEmpty (..))
import Data.List.NonEmpty qualified as NE
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Set (Set)
import Data.Set qualified as Set
import System.IO.Unsafe (unsafePerformIO)
import System.Timeout qualified as Timeout
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
data CleanupAttempt
  = CleanupComplete
  | CleanupFailed SomeException
  | CleanupDeferred
  deriving stock (Show)

data CleanupCapability = CleanupCapability
  { capabilityAction :: IO CleanupAttempt
  , capabilityOnSuccess :: CleanupReceipt -> STM ()
  }

data ClaimedOutcome
  = ClaimedSucceeded
  | ClaimedFailed (NonEmpty SomeException)
  | ClaimedDeferred

data Entry
  = -- | Not currently claimed by anyone; safe for the next caller
    -- (whether 'drainPendingCleanup' or 'attemptCleanupReceipt') to
    -- atomically claim.
    Queued CleanupCapability
  | -- | Currently claimed, and its own 'IO' action is genuinely running
    -- right now (outside this 'Control.Concurrent.STM.TVar''s own
    -- transactions) -- present specifically so a second, concurrent
    -- caller can observe this and skip it, rather than racing to run the
    -- exact same capability twice.
    Running
  | -- | Confirmed complete for a retained lifecycle receipt: nothing further
    -- to ever attempt again. Kept so a later, repeated
    -- 'attemptCleanupReceipt' against the exact same receipt (production:
    -- 'Api.Arkham.Lifecycle.stopManagedGeneration', retrying a
    -- 'Api.Arkham.Lifecycle.RetireFailed' lock any number of times) can
    -- always truthfully report success, rather than "receipt not
    -- found" ambiguously meaning either "never existed" or "already
    -- long finished". Fire-and-forget entries registered through
    -- 'transferPendingCleanupEphemeralOnce' are instead removed on success.
    Terminated

-- | An opaque handle to a durable, keyed collection of not-yet-finished
-- cleanup capabilities. Never exposes the underlying actions themselves.
data PendingCleanupOwner = PendingCleanupOwner
  { pcoEntries :: TVar (Map CleanupReceipt Entry)
  , pcoNextId :: TVar Integer
  , pcoDrainCursor :: TVar (Maybe CleanupReceipt)
  }

-- | Build a fresh, independently-owned 'PendingCleanupOwner' -- for
-- tests only in practice; production code always uses
-- 'globalPendingCleanupOwner' so every caller across the whole process
-- shares the exact same durable owner.
newPendingCleanupOwner :: IO PendingCleanupOwner
newPendingCleanupOwner =
  PendingCleanupOwner
    <$> newTVarIO Map.empty
    <*> newTVarIO 0
    <*> newTVarIO Nothing

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
      capability =
        CleanupCapability
          { capabilityAction =
              action <&> \case
                Right () -> CleanupComplete
                Left err -> CleanupFailed err
          , capabilityOnSuccess = \completedReceipt ->
              modifyTVar' (pcoEntries owner) (Map.insert completedReceipt Terminated)
          }
  modifyTVar' (pcoEntries owner) (Map.insert receipt (Queued capability))
  pure receipt

{- | Register fire-and-forget cleanup at most once for the identity represented
by @receiptSlot@. Concurrent and repeated callers atomically reuse the same
outstanding receipt. On success the owner removes the entry and clears the slot
in the same STM transaction, so room cleanup does not leave permanent
'Terminated' entries while lifecycle callers using 'transferPendingCleanup'
retain their pollable receipt semantics unchanged. 'CleanupDeferred' leaves the
same receipt queued and in the slot, allowing an eligibility change to resume
the same generation without a duplicate.
-}
transferPendingCleanupEphemeralOnce
  :: PendingCleanupOwner
  -> TVar (Maybe CleanupReceipt)
  -> IO CleanupAttempt
  -> IO CleanupReceipt
transferPendingCleanupEphemeralOnce owner receiptSlot action = atomically $ do
  entries <- readTVar (pcoEntries owner)
  current <- readTVar receiptSlot
  case current >>= \receipt -> fmap (\entry -> (receipt, entry)) (Map.lookup receipt entries) of
    Just (receipt, Queued _) -> pure receipt
    Just (receipt, Running) -> pure receipt
    _ -> do
      n <- stateTVar (pcoNextId owner) (\next -> (next, next + 1))
      let receipt = CleanupReceipt n
          capability =
            CleanupCapability
              { capabilityAction = action
              , capabilityOnSuccess = \completedReceipt -> do
                  modifyTVar' (pcoEntries owner) (Map.delete completedReceipt)
                  slotValue <- readTVar receiptSlot
                  when (slotValue == Just completedReceipt) $
                    writeTVar receiptSlot Nothing
              }
      writeTVar receiptSlot (Just receipt)
      writeTVar (pcoEntries owner) (Map.insert receipt (Queued capability) entries)
      pure receipt

{- | Atomically claim the single, first still-'Queued' entry (in
ascending receipt order, i.e. oldest transfer first) whose receipt is
/not/ in @excluded@, if any, returning it and marking it 'Running' -- or
'Left' @()@ if nothing eligible is currently 'Queued'. Never blocks.

@excluded@ exists purely so 'drainPendingCleanup' can attempt every
entry it currently owns /at most once per pass/ (see its own Haddock for
the starvation this closes): a persistently, synchronously failing entry
is requeued by 'attemptClaimed' exactly as before, but a single drain
pass must not re-claim it and loop on it forever while never reaching
any different, later-queued entry.
-}
claimNextExcluding :: PendingCleanupOwner -> Set CleanupReceipt -> STM (Either () (CleanupReceipt, CleanupCapability))
claimNextExcluding owner excluded = do
  entries <- readTVar (pcoEntries owner)
  case [(receipt, capability) | (receipt, Queued capability) <- Map.toAscList entries, receipt `Set.notMember` excluded] of
    [] -> pure (Left ())
    (claimed@(receipt, _) : _) -> do
      writeTVar (pcoEntries owner) (Map.insert receipt Running entries)
      pure (Right claimed)

{- | Atomically decide whether (and how) 'attemptCleanupReceipt' may
proceed against one specific, already-known receipt: already 'Terminated'
(or an ephemeral receipt already removed after success) reports
'ReceiptSucceeded' directly without ever attempting anything; already
'Running' (some other, concurrent caller has already claimed it) reports
'ReceiptBusy'; otherwise claims it exactly like 'claimNextExcluding'.
-}
claimSpecific :: PendingCleanupOwner -> CleanupReceipt -> STM (Either ReceiptOutcome (CleanupReceipt, CleanupCapability))
claimSpecific owner receipt = do
  entries <- readTVar (pcoEntries owner)
  case Map.lookup receipt entries of
    Nothing -> pure (Left ReceiptSucceeded)
    Just Terminated -> pure (Left ReceiptSucceeded)
    Just Running -> pure (Left ReceiptBusy)
    Just (Queued capability) -> do
      writeTVar (pcoEntries owner) (Map.insert receipt Running entries)
      pure (Right (receipt, capability))

{- | Atomically claim (via @claimSTM@) and, if anything was claimed, run
that one capability's own 'IO' action (entirely outside any
'Control.Concurrent.STM.STM' transaction, so it may itself freely call
'transferPendingCleanup' -- e.g. a retried
'Api.Arkham.Lifecycle.ManagedReleasePlan' asynchronously interrupted
/again/ mid-retry -- without any risk of deadlocking against its own
claim), then atomically commit its outcome:

* success -> retained as 'Terminated' or atomically removed, according to the
  explicit transfer API used (either way a future poll truthfully reports
  success);
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

The claim transaction, the action itself, and every commit of the
outcome back into 'pcoEntries' all run under ONE continuous, single
'Control.Exception.mask' spanning this entire function: only the action
itself runs under 'restore' (i.e. exactly as asynchronously-cancellable
as this call's own caller left it) -- everything else, INCLUDING the
initial claim (@Queued -> Running@), runs masked. This closes a
MEDIUM-severity finding against an earlier version of this function,
which performed the claim transaction /before/ ever entering its own
'Control.Exception.mask': an asynchronous exception landing in the gap
between that unmasked claim committing and this function's own 'mask'
call beginning left the claimed entry permanently stranded 'Running',
with nothing left to ever requeue or rethrow anything -- deadlocking
every later attempt to claim it. Every 'atomically' transaction here
(the claim, and each outcome commit) is a single, non-retrying
transaction that runs to completion without blocking, so masking around
all of them is genuinely sufficient: GHC only delivers a pending
asynchronous exception to a masked thread at its own next interruptible
operation, and an 'atomically' transaction that never calls 'retry' is
not itself such a point until after it has already committed.
-}
attemptClaimed
  :: PendingCleanupOwner
  -> STM (Either r (CleanupReceipt, CleanupCapability))
  -> IO (Either r (CleanupReceipt, ClaimedOutcome))
attemptClaimed owner claimSTM = mask $ \restore -> do
  claimed <- atomically claimSTM
  case claimed of
    Left notClaimed -> pure (Left notClaimed)
    Right (receipt, capability) -> do
      outcome <- try @SomeException (restore (capabilityAction capability))
      case outcome of
        Right CleanupComplete -> do
          atomically $ capabilityOnSuccess capability receipt
          pure (Right (receipt, ClaimedSucceeded))
        Right (CleanupFailed err) -> do
          atomically $ modifyTVar' (pcoEntries owner) (Map.insert receipt (Queued capability))
          pure (Right (receipt, ClaimedFailed (err :| [])))
        Right CleanupDeferred -> do
          atomically $ modifyTVar' (pcoEntries owner) (Map.insert receipt (Queued capability))
          pure (Right (receipt, ClaimedDeferred))
        Left threwErr -> do
          atomically $ modifyTVar' (pcoEntries owner) (Map.insert receipt (Queued capability))
          throwIO threwErr

{- | Attempt every capability currently owned, claiming (and running) one
entry at a time -- never a batch claimed all at once -- specifically so
that an asynchronous exception interrupting one entry's own attempt
(propagated straight out of this call: see 'attemptClaimed') leaves every
/other/ entry not yet reached still safely 'Queued', never stuck
'Running' forever.

Each entry currently owned is attempted AT MOST ONCE per call (tracked
via the growing @attempted@ set of receipts, threaded through 'go'):
root-causing a MEDIUM-severity starvation finding against this
function's own structural predecessor, which re-claimed the single
/oldest/ still-'Queued' entry on every iteration with no memory of what
it had already tried this pass -- so a persistently, synchronously
failing entry (requeued unchanged by 'attemptClaimed') was immediately
reclaimed again, forever, and no /other/, later-queued entry was ever
even reached at all. A newly 'transferPendingCleanup'd entry (always a
fresh, never-yet-attempted receipt) is never excluded by this pass's own
@attempted@ set, so independent, concurrently-arriving work is still
picked up within the very same call, exactly as before.

Returns 'Right' @()@ when this pass observed no synchronous failures, or
'Left' the complete list of synchronous failures collected along the
way (continuing to attempt independent, later entries after a failure).
A deliberately deferred or failing entry remains safely 'Queued' for a
later pass; this function never loops on either more than once per call.
-}
drainPendingCleanup :: PendingCleanupOwner -> IO (Either [SomeException] ())
drainPendingCleanup owner = go Set.empty []
 where
  go attempted failuresAcc = do
    claimed <- attemptClaimed owner (claimNextExcluding owner attempted)
    case claimed of
      Left () -> pure (if null failuresAcc then Right () else Left (reverse failuresAcc))
      Right (receipt, ClaimedSucceeded) -> go (Set.insert receipt attempted) failuresAcc
      Right (receipt, ClaimedDeferred) -> go (Set.insert receipt attempted) failuresAcc
      Right (receipt, ClaimedFailed errs) -> go (Set.insert receipt attempted) (reverse (NE.toList errs) ++ failuresAcc)

{- | Attempt a fixed, fair snapshot of at most @maxEntries@ queued
capabilities. Each individual attempt is capped at @attemptTimeoutMicros@;
timed-out actions are cancellation-safely requeued by 'attemptClaimed'. New
registrations are never pulled into the current pass, and a rotating cursor
prevents an old blocked or persistently failing entry from starving later
work across ticks.
-}
drainPendingCleanupBounded :: PendingCleanupOwner -> Int -> Int -> IO ()
drainPendingCleanupBounded owner maxEntries attemptTimeoutMicros = do
  receipts <- atomically snapshot
  for_ receipts $ \receipt ->
    void $
      Timeout.timeout
        (max 1 attemptTimeoutMicros)
        (attemptClaimed owner (claimSpecific owner receipt))
 where
  snapshot = do
    entries <- readTVar (pcoEntries owner)
    cursor <- readTVar (pcoDrainCursor owner)
    let queued = [receipt | (receipt, Queued _) <- Map.toAscList entries]
        ordered = case cursor of
          Nothing -> queued
          Just previous ->
            let (before, after) = span (<= previous) queued
             in after <> before
        selected = take (max 0 maxEntries) ordered
    for_ (lastMay selected) $ writeTVar (pcoDrainCursor owner) . Just
    pure selected

  lastMay = \case
    [] -> Nothing
    xs -> Just (last xs)

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
  | -- | The capability remains outstanding but deliberately performed no
    -- failing action this time (for example, an empty-only room cleanup
    -- whose room currently has a rejoined subscriber).
    ReceiptDeferred
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

The claim (@Queued -> Running@, or the already-resolved
'ReceiptSucceeded'\/'ReceiptBusy' cases) and the attempt-and-commit that
follows it both run through 'attemptClaimed', so they share the exact
same single, continuous 'Control.Exception.mask' -- see its own Haddock
for the MEDIUM-severity finding this closes (an asynchronous exception
landing between an unmasked claim and a not-yet-started attempt could
otherwise strand this exact receipt 'Running' forever).
-}
attemptCleanupReceipt :: PendingCleanupOwner -> CleanupReceipt -> IO ReceiptOutcome
attemptCleanupReceipt owner receipt = do
  result <- attemptClaimed owner (claimSpecific owner receipt)
  pure $ case result of
    Left alreadyResolved -> alreadyResolved
    Right (_, ClaimedSucceeded) -> ReceiptSucceeded
    Right (_, ClaimedFailed errs) -> ReceiptFailed errs
    Right (_, ClaimedDeferred) -> ReceiptDeferred

-- | Whether anything at all is currently owned (queued or actively
-- running; excludes 'Terminated' entries) -- for tests\/introspection
-- only.
hasPendingCleanup :: PendingCleanupOwner -> IO Bool
hasPendingCleanup owner = any isOutstanding . Map.elems <$> readTVarIO (pcoEntries owner)
 where
  isOutstanding = \case
    Terminated -> False
    _ -> True
