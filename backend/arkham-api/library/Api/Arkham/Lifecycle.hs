{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE RankNTypes #-}


{- | Small, generic, dependency-injectable resource-ownership helpers.

These exist so that 'Application.makeFoundation', 'Application.appMain',
'Application.handler', 'Application.getApplicationRepl', and
@app\/DevelMain.hs@ all share one concrete definition per ownership
pattern, and so that
'Arkham.Api.AwsEnvSupervisorLifecycleSpec' can import and execute those
/exact/ definitions against a fake resource instead of re-deriving the
same 'Control.Exception.bracket'\/'Control.Exception.bracketOnError'
shape locally: a regression at any production call site (reverting to
plain sequencing, swapping which combinator is used, or breaking the
restart-ordering/result-delivery contract) is then something these tests
can actually observe, rather than something a locally-duplicated
"mirror" of the pattern could never catch.

None of this is AWS\/'Api.Arkham.AwsEnvSupervisor'-specific; it is plain
resource-ownership plumbing.
-}
module Api.Arkham.Lifecycle (
  acquireTransferringOwnershipOnSuccess,
  AcquisitionCleanupFailed (..),
  acquireWithUnconditionalRelease,
  releaseAll,
  releaseAllRecordingReceipt,
  ReleaseAllFailed (..),
  shutdownThenDeliver,
  shutdownThenDeliverRecordingReceipt,
  RetirementOutcome (..),
  RetirementRetry (..),
  forkTransferringOwnership,
  forkTransferringOwnershipUsing,
  ManagedThread,
  managedThreadId,
  spawnManagedThread,
  waitManagedThread,
  cancelManagedThread,
  raceManaged_,
  ManagedCleanup,
  newManagedCleanup,
  runManagedCleanup,
  ManagedReleasePlan,
  newManagedReleasePlan,
  runManagedReleasePlan,
  RestartState (..),
  StopOutcome (..),
  restartManagedGeneration,
  restartManagedGenerationUsing,
  stopManagedGeneration,
  classifyRetirementFailure,
  getOrCreateStore,
  getExistingStore,
  getOrCreateStoreCheckingLegacySlot,
  getExistingStoreCheckingLegacySlot,
  DevelStoreLock.LegacyDevelStoreSlotOccupied (..),
  restartStateSchemaHash,
  DevelStoreSchemaStale (..),
  drainOwnedCleanup,
  PendingCleanupOwner.CleanupReceipt,
  PendingCleanupOwner.ReceiptOutcome (..),
) where

import Control.Concurrent (ThreadId, forkIOWithUnmask)
import Control.Concurrent.MVar (
  MVar,
  modifyMVar,
  newEmptyMVar,
  newMVar,
  putMVar,
  readMVar,
  takeMVar,
  tryPutMVar,
 )
import Control.Exception (
  AsyncException (ThreadKilled),
  Exception,
  SomeException,
  bracket,
  finally,
  fromException,
  mask,
  mask_,
  onException,
  throwIO,
  throwTo,
  toException,
  try,
 )
import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import Data.List.NonEmpty (NonEmpty (..))
import Data.List.NonEmpty qualified as NE
import Data.Proxy (Proxy (..))
import Data.Typeable (Typeable)
import Data.Word (Word32, Word64)
import DevelStoreLock (StructuralHash (..), VersionedAccess (..))
import DevelStoreLock qualified
import Foreign.Store (Store)
import GHC.Generics (Generic)
import PendingCleanupOwner (globalPendingCleanupOwner)
import PendingCleanupOwner qualified
import Prelude
import UnliftIO.Exception qualified as UE

{- | Thrown by 'acquireTransferringOwnershipOnSuccess' /instead of/
either individual failure whenever BOTH its own guarded body throws --
including this thread being asynchronously cancelled while running it --
AND the compensating release meant to tear down the already-acquired
resource in response ALSO then fails. Plain 'Control.Exception.bracketOnError'
cannot represent this: its own compensating cleanup is run for effect
only, via 'Control.Exception.onException', and if that cleanup itself
throws, THAT exception silently REPLACES the original one that
triggered it, with no way for any caller to learn the resource may
still be live, let alone retry tearing it down -- exactly the MEDIUM
finding this type fixes (nested Foundation acquisition, e.g. the AWS
supervisor \/ Redis connection \/ pub\/sub thread \/ database pool chain
built by 'Application.makeFoundation', whose own compensating release
can itself throw after a partial construction).

Preserves the /entire/ chronological history of cleanup failures (newest
first) across however many nested 'acquireTransferringOwnershipOnSuccess'
layers are involved -- if the body that failed was itself another nested
call whose own compensating cleanup had already failed (i.e. its own
exception was already an 'AcquisitionCleanupFailed'), this layer's own
history is appended, not wrapped, and its own retry capability is
composed /after/ the inner one's (retrying the innermost-acquired,
still-outstanding resource first, then this layer's own -- the exact
reverse of acquisition order, matching every other release ordering in
this module). The 'acquisitionCleanupFailedOriginal' field always names
the ORIGINAL failure that started this chain, not a wrapped
'AcquisitionCleanupFailed' from some deeper layer.

'acquisitionCleanupFailedRetry' is the /exact/, still-outstanding release
capability: nothing besides the attempt(s) already recorded in
'acquisitionCleanupFailedDuringCleanup' has actually run it, so it
remains genuinely safe to retry, any number of times, from anywhere that
holds this value -- see 'Api.Arkham.Lifecycle.StartCleanupFailed', which
retains exactly this capability so 'stopManagedGeneration' can actually
retry it, rather than only ever being able to report the same stale
failure forever.

Root-cause fix for a MEDIUM-severity finding: this capability used to be
built as plain @innerRetry >> release res@, stored unchanged across every
retry. If a retry ever made @innerRetry@ (the nested, more-deeply-
acquired resource's own release) genuinely succeed while @release res@
(this layer's own) kept failing, the very next retry re-ran the SAME,
already-succeeded @innerRetry@ all over again before even reaching
@release res@ -- a double-release risk for any non-idempotent resource,
and (via @>>@'s short-circuiting) one that could also starve @release
res@ of ever being attempted again for as long as @innerRetry@ kept
failing. 'acquisitionCleanupFailedRetry' is now an opaque
'ManagedCleanup' (built via 'composeManagedCleanup'\/'newManagedCleanup'),
which permanently forgets each step (here: first @innerRetry@'s own,
then @release res@) the instant -- and only the instant -- it is
durably committed as having completed without throwing, atomically with
that step's own execution (see 'ManagedCleanup''s own Haddock), so
calling 'runManagedCleanup' any number of times, even concurrently, can
never re-run a step already confirmed successful.
-}
data AcquisitionCleanupFailed = AcquisitionCleanupFailed
  { acquisitionCleanupFailedOriginal :: SomeException
  , acquisitionCleanupFailedDuringCleanup :: NonEmpty SomeException
  , acquisitionCleanupFailedRetry :: ManagedCleanup
  }

instance Show AcquisitionCleanupFailed where
  show (AcquisitionCleanupFailed original duringCleanup _) =
    "AcquisitionCleanupFailed { acquisitionCleanupFailedOriginal = "
      <> show original
      <> ", acquisitionCleanupFailedDuringCleanup = "
      <> show (NE.toList duringCleanup)
      <> " }"

instance Exception AcquisitionCleanupFailed

{- | Acquire a resource whose ownership transfers to the caller on
success, but which must never leak: if the body throws, or this thread
is asynchronously cancelled, after acquisition but before the body
returns, the resource is released before that exception propagates.
Used by 'Application.makeFoundation' (around
'Api.Arkham.AwsEnvSupervisor.newAwsEnvSupervisor', so a later
Redis\/database initialization failure cannot leak the supervisor) and
by 'Application.getApplicationRepl' (around 'Application.makeFoundation'
itself, so a later 'Network.Wai.Handler.Warp.getDevSettings'\/
'Application.makeApplication' failure cannot leak the whole 'App').

Deliberately hand-rolled rather than plain 'Control.Exception.bracketOnError'
-- see 'AcquisitionCleanupFailed' for exactly why: when the compensating
release this function runs in response to the body failing (or this
thread being cancelled) ITSELF then also fails, this throws
'AcquisitionCleanupFailed' (carrying both the original failure and the
exact, still-retriable release capability) instead of letting the
release's own exception silently discard the original and everything a
caller would need to retry it.
-}
acquireTransferringOwnershipOnSuccess :: IO res -> (res -> IO ()) -> (res -> IO a) -> IO a
acquireTransferringOwnershipOnSuccess acquire release use = mask $ \restore -> do
  res <- acquire
  outcome <- try @SomeException (restore (use res))
  case outcome of
    Right a -> pure a
    Left bodyErr -> do
      cleanupOutcome <- try @SomeException (release res)
      case cleanupOutcome of
        Right () -> throwIO bodyErr
        Left cleanupErr -> do
          (originalErr, priorCleanupErrs, retryAction) <- case fromException bodyErr of
            Just (AcquisitionCleanupFailed orig prior innerRetry) ->
              (,,) orig (NE.toList prior) <$> composeManagedCleanup innerRetry (release res)
            Nothing -> (,,) bodyErr [] <$> newManagedCleanup [release res]
          throwIO
            AcquisitionCleanupFailed
              { acquisitionCleanupFailedOriginal = originalErr
              , acquisitionCleanupFailedDuringCleanup = cleanupErr :| priorCleanupErrs
              , acquisitionCleanupFailedRetry = retryAction
              }

{- | An opaque, serialized handle to an ordered ("dependent") sequence of
cleanup steps -- unlike a plain 'IO ()' (a first-class value, freely
copyable and independently callable, any number of times, concurrently,
by anyone holding a reference), the only way to ever drive this is
through 'runManagedCleanup', which serializes every caller (concurrent
or sequential) against the exact same handle through a single, private
'Control.Concurrent.MVar.MVar', and masks each step's own execution
together with the durable commit of its own success as one atomic unit.

Root-cause fix for a MEDIUM-severity finding against this handle's
structural predecessor (a bare @IO ()@ built by composing steps with
plain, unmasked 'Data.IORef.IORef' bookkeeping): an asynchronous
exception landing in precisely the gap between a step returning
successfully and that success being durably recorded could make a later
retry re-run -- and potentially double-release -- a step that had, in
fact, already completed; and nothing about a plain @IO ()@ value itself
prevented two concurrent callers from racing to run the very same step
twice. Both are closed here structurally: 'runManagedCleanup' only ever
un-masks ('Control.Exception.mask'\'s own @restore@) around the literal
invocation of the current step, so there is no window, masked or
otherwise, between that step returning and its removal from this
handle's own remaining plan for an asynchronous exception to land in; and
the single 'Control.Concurrent.MVar.MVar'\'s own take\/put genuinely
serializes every call against the same handle, so two concurrent callers
can never observe, let alone act on, the same "still pending" step at
once.

Steps are attempted strictly in order, and a failure (synchronous, or
this thread being asynchronously interrupted mid-step) stops right
there, without attempting anything after it -- unlike 'ManagedReleasePlan',
these steps are NOT independent of one another: 'acquireTransferringOwnershipOnSuccess'
uses this specifically because a more-deeply-nested resource must be
released before the outer one that contains it.
-}
newtype ManagedCleanup = ManagedCleanup (MVar [IO ()])

-- | Build a fresh 'ManagedCleanup' owning exactly the given steps, in
-- order.
newManagedCleanup :: [IO ()] -> IO ManagedCleanup
newManagedCleanup steps = ManagedCleanup <$> newMVar steps

{- | Fold an already-outstanding nested 'ManagedCleanup' (production:
whatever an inner 'acquireTransferringOwnershipOnSuccess' layer's own
'AcquisitionCleanupFailed' retained) together with this layer's own,
single additional release step, into one freshly-owned 'ManagedCleanup'
that supersedes the inner one -- attempting the inner (more deeply
nested, and therefore correctly ordered first) steps, still exactly as
progress-aware as they were, before ever attempting this layer's own.

Takes exclusive ownership of @inner@'s own remaining steps via
'Control.Concurrent.MVar.takeMVar' (never merely peeking with
'Control.Concurrent.MVar.readMVar'): @inner@ is only ever reachable here
via an exception value this function's own caller just caught (nothing
else concurrently holds, or will ever again use, that exact handle), and
taking, rather than only reading, leaves the superseded handle
permanently unable to independently\/redundantly drive those same steps
a second time.
-}
composeManagedCleanup :: ManagedCleanup -> IO () -> IO ManagedCleanup
composeManagedCleanup (ManagedCleanup innerVar) outerStep = do
  innerSteps <- takeMVar innerVar
  newManagedCleanup (innerSteps ++ [outerStep])

{- | Attempt every step still outstanding in @cleanup@, strictly in
order, stopping at (and durably retaining, unchanged) the first step
that does not complete successfully -- synchronously, or via an
asynchronous interruption. Throws whatever that step raised (verbatim:
never wrapped) rather than returning it as a value, exactly matching
this handle's structural predecessor's own external contract, so
existing callers (production: 'stopManagedGeneration', retrying a
retained 'StartCleanupFailed') need not change shape.

Serialized against every other call to this function against the exact
same @cleanup@ (see 'ManagedCleanup''s own Haddock for exactly why, and
for the atomicity guarantee between a step succeeding and that success
being durably committed).
-}
runManagedCleanup :: ManagedCleanup -> IO ()
runManagedCleanup (ManagedCleanup stepsVar) = do
  outcome <- mask $ \restore -> modifyMVar stepsVar (go restore)
  either throwIO pure outcome
 where
  go restore steps = case steps of
    [] -> pure ([], Right ())
    (step : rest) -> do
      stepOutcome <- try @SomeException (restore step)
      case stepOutcome of
        Right () -> go restore rest
        Left err -> pure (step : rest, Left err)

{- | Acquire a resource and unconditionally release it once the body
returns, whether it succeeded, threw, or was cancelled -- there is no
"transfer ownership on success" case, because the body is the resource's
entire remaining lifetime. Exactly 'Control.Exception.bracket' -- used by
'Application.appMain' (around 'Application.makeFoundation'\/'Application.shutdownApp',
spanning the Warp server's entire run) and 'Application.handler' (around
each ad-hoc GHCi\/REPL 'Foundation').
-}
acquireWithUnconditionalRelease :: IO res -> (res -> IO ()) -> (res -> IO a) -> IO a
acquireWithUnconditionalRelease = bracket

{- | Run every release action in the list, even if an earlier one throws
a /synchronous/ exception: an earlier failure must never cause a later
release to be skipped, and a synchronously-failed action is never
dropped from the retry plan either -- see 'ManagedReleasePlan' for
exactly why (a MEDIUM-severity root-cause fix over this function's own
structural predecessor).

An /asynchronous/ exception delivered while a release action is running
(e.g. 'System.Timeout.timeout' or 'ThreadKilled' from a cancelled
shutdown) stops this call from attempting anything further immediately,
so cancellation can promptly abort cleanup instead of this becoming, in
effect, uninterruptible. Unlike a synchronous failure, this is never
converted into a synchronously-shaped 'ReleaseAllFailed' at all: doing so
was itself a MEDIUM-severity finding, letting a caller with no durable
place to retain the outstanding capability (production:
'Application.handler', which -- unlike @app\/DevelMain.hs@\'s own
'RestartState' lock -- has no persistent generation lock to retain a
leftover 'ReleaseAllFailed' in at all) receive what looked like an
ordinary, already-classified data-shaped failure for a shutdown that
was, in fact, still asynchronously in flight, with nothing anywhere left
responsible for finishing it. Instead: the exact, still-outstanding
capability is durably transferred to 'PendingCleanupOwner.globalPendingCleanupOwner'
/before/ the ORIGINAL asynchronous exception (never wrapped) is
rethrown -- see 'ManagedReleasePlan' and 'drainOwnedCleanup'.

Used by 'Application.shutdownApp' to release every foundation-owned
resource (the AWS supervisor, the room-heartbeat thread, the optional
pub\/sub-supervisor thread, the Redis connection, and the database
connection pool) unconditionally, rather than an ordinary sequence of
plain statements where a single throwing release would abandon every
release after it -- exactly the "unbounded across restarts" leak this
guards against.
-}
releaseAll :: [IO ()] -> IO ()
releaseAll actions = do
  discardedReceiptSink <- newIORef Nothing
  releaseAllRecordingReceipt discardedReceiptSink actions

{- | Exactly 'releaseAll', except that if this pass is asynchronously
interrupted and its own outstanding remainder is durably transferred to
'PendingCleanupOwner.globalPendingCleanupOwner' (see 'runManagedReleasePlan'\'s
own Haddock for exactly when that happens), the exact
'PendingCleanupOwner.CleanupReceipt' identifying that one hand-off is
additionally recorded into @receiptSink@ -- an already-empty,
single-write 'Data.IORef.IORef' -- /before/ the original exception is
ever rethrown, rather than merely discarding it the way plain
'releaseAll' does. @app\/DevelMain.hs@'s restart protocol (via
'shutdownThenDeliverRecordingReceipt') uses this, never plain
'releaseAll', specifically so that a resulting 'RetireFailed' can retain
the exact receipt needed to observe -- and, on success, clear -- this
teardown's own eventual completion later, however many
'stopManagedGeneration' calls that takes and regardless of whether this
process's own retry or some entirely different caller's
'drainOwnedCleanup' is the one that actually finishes it -- see
'RetireFailed''s own Haddock for the MEDIUM-severity finding this closes.
-}
releaseAllRecordingReceipt :: IORef (Maybe PendingCleanupOwner.CleanupReceipt) -> [IO ()] -> IO ()
releaseAllRecordingReceipt receiptSink actions = do
  plan <- newManagedReleasePlan actions
  outcome <- try @SomeException (runManagedReleasePlan plan)
  case outcome of
    Right (Right ()) -> pure ()
    Right (Left failures) -> throwIO (ReleaseAllFailed failures plan)
    Left asyncErr -> do
      -- 'runManagedReleasePlan' only ever throws (rather than returning
      -- a 'Left' failure /value/) for a genuine asynchronous
      -- interruption -- see its own Haddock -- so anything caught here
      -- is, by construction, never a synchronous failure needing
      -- 'ReleaseAllFailed'-style classification at all.
      mask_ $ do
        receipt <- PendingCleanupOwner.transferPendingCleanup globalPendingCleanupOwner (retryPlan plan)
        writeIORef receiptSink (Just receipt)
      throwIO asyncErr
 where
  retryPlan plan =
    either (Left . toException . NE.head) (const (Right ())) <$> runManagedReleasePlan plan

{- | An opaque, serialized handle to an /independent/ ("attempt every
action even if an earlier one fails") sequence of release actions --
see 'ManagedCleanup' for the "dependent, stop at first failure" variant,
and for exactly why an opaque, 'Control.Concurrent.MVar.MVar'-serialized
handle replaces a plain, copyable @IO ()@ retry value.
-}
newtype ManagedReleasePlan = ManagedReleasePlan (MVar [IO ()])

-- | Build a fresh 'ManagedReleasePlan' owning exactly the given
-- independent actions.
newManagedReleasePlan :: [IO ()] -> IO ManagedReleasePlan
newManagedReleasePlan actions = ManagedReleasePlan <$> newMVar actions

-- | The result of one attempted pass over a 'ManagedReleasePlan''s own
-- remaining actions.
data ReleasePassOutcome
  = -- | Every action still owned was attempted, and every one of them
    -- completed successfully.
    CleanPass
  | -- | Every action still owned was attempted, but at least one failed
    -- synchronously; the complete, newest-first history is carried here.
    SyncFailuresRemain (NonEmpty SomeException)
  | -- | An asynchronous exception interrupted this pass before every
    -- action had even been attempted.
    AsyncInterrupted SomeException

{- | Attempt every action still owned by @plan@, in order, continuing to
attempt independent later actions even after an earlier one fails
synchronously -- and, root-causing a MEDIUM-severity finding against this
handle's structural predecessor, retaining that synchronously-failed
action itself in the plan afterwards (never silently dropping it the way
an earlier, unconditional @writeIORef remainingRef rest@ did regardless
of outcome, which let a later retry report "clean" without ever actually
having retried a failed action).

Stops immediately -- without attempting anything further -- the instant
an asynchronous exception interrupts a release, exactly as before;
unlike a purely synchronous outcome, this is never turned into a
'ReleaseAllFailed'-shaped value at all (see 'releaseAll''s own Haddock):
the exact, still-outstanding remainder (the interrupted action,
conservatively treated as not yet confirmed successful, plus every
action after it in the plan that was never even attempted this pass, plus
every synchronous failure already collected earlier in the very same
pass) is durably handed to 'PendingCleanupOwner.globalPendingCleanupOwner'
/before/ the ORIGINAL exception (never wrapped in anything else) is
rethrown.

A whole pass runs as one atomic 'Control.Concurrent.MVar.MVar' transaction
(masked throughout, with 'Control.Exception.mask'\'s own @restore@
applied only around the literal invocation of whichever single action is
currently being attempted): two concurrent callers against the exact
same @plan@ are therefore always fully serialized, and there is no gap,
masked or otherwise, between an action completing and that being durably
reflected in @plan@\'s own remaining list for an asynchronous exception
to land in.

This function itself never touches 'PendingCleanupOwner.globalPendingCleanupOwner'
at all (a change from an earlier version, which called
'PendingCleanupOwner.transferPendingCleanup' directly from its own
'AsyncInterrupted' branch): doing the transfer here meant that /every/
caller of this function -- including 'PendingCleanupOwner.drainPendingCleanup'\/'PendingCleanupOwner.attemptCleanupReceipt'
themselves retrying an /already/-owned plan on a later pass -- would
re-transfer it again on a second interruption, growing the owner's own
queue with redundant entries for the exact same, already-owned plan on
every retried cancellation. Only 'releaseAllRecordingReceipt' (this
plan's own /first/-ever caller) performs that one-time registration; by
construction, that is therefore the /only/ place 'PendingCleanupOwner.CleanupReceipt's
are ever minted, so a receipt always identifies one, unambiguous
plan for the rest of its lifetime. This function is guaranteed to only
ever /throw/ for a genuine asynchronous interruption -- a synchronous
failure always comes back as 'Left', never as a thrown exception -- which
'releaseAllRecordingReceipt' and 'PendingCleanupOwner.attemptOne' both
rely on.
-}
runManagedReleasePlan :: ManagedReleasePlan -> IO (Either (NonEmpty SomeException) ())
runManagedReleasePlan (ManagedReleasePlan actionsVar) = do
  passOutcome <- mask $ \restore -> modifyMVar actionsVar (attemptPass restore)
  case passOutcome of
    CleanPass -> pure (Right ())
    SyncFailuresRemain failures -> pure (Left failures)
    AsyncInterrupted asyncErr -> throwIO asyncErr
 where
  attemptPass restore actions = go [] [] actions
   where
    go failuresAcc retainedRev toAttempt = case toAttempt of
      [] ->
        pure
          ( reverse retainedRev
          , maybe CleanPass SyncFailuresRemain (NE.nonEmpty (reverse failuresAcc))
          )
      (action : rest) -> do
        outcome <- try @SomeException (restore action)
        case outcome of
          Right () -> go failuresAcc retainedRev rest
          Left err
            | UE.isSyncException err -> go (err : failuresAcc) (action : retainedRev) rest
            | otherwise ->
                pure (reverse retainedRev ++ (action : rest), AsyncInterrupted err)

-- | A thin wrapper around 'runManagedReleasePlan' matching this handle's
-- structural predecessor's own external contract (throws
-- 'ReleaseAllFailed' on synchronously-remaining failures, so existing
-- retry call sites -- production: 'stopManagedGeneration', retrying a
-- retained 'RetireFailed' -- need not change shape).
runManagedReleasePlanOrThrow :: ManagedReleasePlan -> IO ()
runManagedReleasePlanOrThrow plan = do
  outcome <- runManagedReleasePlan plan
  case outcome of
    Right () -> pure ()
    Left failures -> throwIO (ReleaseAllFailed failures plan)

{- | Thrown by 'releaseAll' instead of a bare exception whenever every
attemptable action has been attempted and at least one has not been
confirmed to have completed successfully -- purely synchronous failures
only; see 'releaseAll''s own Haddock for why a genuine asynchronous
interruption is never represented this way at all. Retains both the
complete, newest-first history of every synchronous failure, and the
exact 'ManagedReleasePlan' to retry -- see
'Api.Arkham.Lifecycle.RetireFailed', which retains exactly this
capability so 'stopManagedGeneration' can actually retry it, rather than
abandoning it forever.
-}
data ReleaseAllFailed = ReleaseAllFailed
  { releaseAllFailedFailures :: NonEmpty SomeException
  , releaseAllFailedRetry :: ManagedReleasePlan
  }

instance Show ReleaseAllFailed where
  show (ReleaseAllFailed failures _) =
    "ReleaseAllFailed { releaseAllFailedFailures = " <> show (NE.toList failures) <> " }"

instance Exception ReleaseAllFailed

{- | Run a shutdown action, capturing /any/ exception it raises --
synchronous, or asynchronous delivered while it is blocked on its own
interruptible internal operations (e.g. awaiting a supervised thread) --
instead of letting it propagate and silently skip signalling entirely,
then atomically deliver the outcome to an observer (an empty 'MVar' a
restart protocol is waiting on).

Used by @app\/DevelMain.hs@'s restart protocol: without this, a
shutdown that throws mid-way never reaches the @putMVar@ that unblocks
the next restart's wait, deadlocking it forever. With this, the waiter
always receives a result -- 'RetiredCleanly' on a clean shutdown, or
'RetirementFailed' on a failed\/interrupted one -- and can choose not to
start a replacement generation on the latter, avoiding both the deadlock
and two live generations ever coexisting.

The delivery itself ('Control.Concurrent.MVar.tryPutMVar', not a
blocking 'Control.Concurrent.MVar.putMVar') runs under 'mask_' so that
once the shutdown outcome is known, nothing can prevent it from actually
reaching the waiter -- there is no gap between "outcome decided" and
"outcome delivered" for an asynchronous exception to land in.
'Control.Concurrent.MVar.tryPutMVar' specifically (rather than
'Control.Concurrent.MVar.putMVar') is what makes that delivery itself
unconditionally non-blocking: every caller of this function hands it a
freshly created, guaranteed-empty, single-writer @done@ (so in ordinary
use the two are behaviourally identical), but a blocking @putMVar@ into
an unexpectedly-already-full cell can still itself block -- even under
'mask_' -- deadlocking this finalizer forever instead of merely losing a
result nobody could have consumed anyway. @tryPutMVar@ can never block,
so this finalizer always completes.
-}
shutdownThenDeliver :: IO () -> MVar RetirementOutcome -> IO ()
shutdownThenDeliver shutdown done = do
  discardedReceiptSink <- newIORef Nothing
  shutdownThenDeliverRecordingReceipt discardedReceiptSink shutdown done

{- | The outcome of one generation's own teardown attempt, delivered by
'shutdownThenDeliver'\/'shutdownThenDeliverRecordingReceipt' into its own
dedicated 'Running' completion cell. Unlike a plain @Either
'SomeException' ()@, a failed teardown also carries the exact
'PendingCleanupOwner.CleanupReceipt' identifying any durably-transferred
outstanding cleanup work this teardown's own 'releaseAll' left behind --
'Nothing' if either nothing was transferred, or @shutdown@ was not built
from 'releaseAllRecordingReceipt' at all -- so that
'stopManagedGeneration' can genuinely observe that receipt's own eventual
completion later, root-causing the MEDIUM-severity finding that
'RetireFailed' previously had no way to ever clear itself once its own
teardown failure had already been durably transferred away: see
'RetireFailed''s own Haddock.
-}
data RetirementOutcome
  = -- | @shutdown@ completed without throwing.
    RetiredCleanly
  | -- | @shutdown@ threw (synchronously, or an asynchronous exception
    -- delivered while it was blocked on its own interruptible internal
    -- operations); the exact exception, and the exact receipt (if any)
    -- identifying whatever outstanding work it durably transferred away
    -- before this was thrown.
    RetirementFailed SomeException (Maybe PendingCleanupOwner.CleanupReceipt)
  deriving stock (Show)

{- | Exactly 'shutdownThenDeliver', except @receiptSink@ -- an
already-empty, single-write 'Data.IORef.IORef' that @shutdown@ itself is
expected to write into (production: @shutdown@ is
@releaseAllRecordingReceipt receiptSink actions@, i.e. @app\/DevelMain.hs@
builds its own per-attempt @receiptSink@ and threads it straight down
into 'releaseAllRecordingReceipt') -- is read immediately after
@shutdown@ throws, and its own value is carried into the resulting
'RetirementFailed' rather than discarded. @app\/DevelMain.hs@'s
restart protocol uses this (never plain 'shutdownThenDeliver') for
exactly this reason: only a generation with a persistent
'Api.Arkham.Lifecycle.RestartState' lock to retain that receipt in (via
'RetireFailed') has any use for it at all -- 'Application.handler''s own
per-call, lock-less teardown has nowhere to keep it, and continues to
use plain 'shutdownThenDeliver'\/'releaseAll'.
-}
shutdownThenDeliverRecordingReceipt
  :: IORef (Maybe PendingCleanupOwner.CleanupReceipt)
  -> IO ()
  -> MVar RetirementOutcome
  -> IO ()
shutdownThenDeliverRecordingReceipt receiptSink shutdown done = do
  result <- try shutdown
  outcome <- case result of
    Right () -> pure RetiredCleanly
    Left err -> RetirementFailed err <$> readIORef receiptSink
  mask_ (tryPutMVar done outcome >> pure ())

{- | Given an already-acquired resource whose remaining lifetime is meant
to be owned by a newly-forked child thread (which runs @body res@
unmasked, then runs @finalize res@ -- typically releasing @res@ --
exactly once, regardless of how @body@ exits, matching
'Control.Concurrent.forkFinally'), mask this thread from having @res@ in
hand until the child is definitely spawned and has therefore definitely
taken over that responsibility.

Used by @app\/DevelMain.hs@'s @start@, around the already-acquired 'App'
'Application.getApplicationRepl' returns (whose OWN acquisition already
has separate, established exception-safety -- see
'acquireTransferringOwnershipOnSuccess'): without this, an asynchronous
exception landing in the narrow window between that 'App' being returned
and 'Control.Concurrent.forkIOWithUnmask' actually spawning its child --
or 'forkIOWithUnmask' itself throwing synchronously, however rare -- would
leak the 'App' (and its AWS Env supervisor), since nothing would ever be
left to call @shutdownApp@ on it. If forking itself fails, or this thread
is asynchronously cancelled anywhere in this masked span, @release res@
runs here instead; if forking succeeds, the child is (from that instant)
unambiguously the sole owner of @res@'s eventual @finalize@.
-}
forkTransferringOwnership
  :: res
  -> (res -> IO ())
  -> (res -> IO a)
  -> (res -> Either SomeException a -> IO ())
  -> IO ThreadId
forkTransferringOwnership = forkTransferringOwnershipUsing forkIOWithUnmask

{- | Test-injectable generalization of 'forkTransferringOwnership':
@spawn@ stands in for 'Control.Concurrent.forkIOWithUnmask' itself
(taking the same \"run this with async exceptions unmasked\" body and
handing back a handle for whatever actually ran it), letting a
regression test substitute a fake that deterministically synchronizes
with a tester-driven cancellation at the exact moment that matters --
after @res@ is already in hand, strictly before the spawn primitive
itself has produced anything -- rather than relying on timing luck
against a real 'Control.Concurrent.forkIOWithUnmask' call (which, having
no interruptible operation of its own, essentially never actually loses
that race in practice, making that race non-deterministic and hard to
prove either way). 'onException' here is what actually matters: it
guarantees @release res@ runs before propagating /any/ exception raised
while acquiring the handle from @spawn@ -- including one delivered while
@spawn@ itself is blocked on a genuinely interruptible operation, which
even 'Control.Exception.mask' cannot defer (see
'Control.Concurrent.MVar.takeMVar' and STM's @retry@, which remain
interruptible by design even when masked) -- not merely one @spawn@
raises synchronously and immediately.

@spawn@ is itself called from within this function's own 'mask', but the
child body still runs genuinely unmasked via @spawn@'s /own/ supplied
callback (in production, 'Control.Concurrent.forkIOWithUnmask''s
@unmask@): unlike a plain 'mask'-produced @restore@ (which restores to
whatever masking state was in effect /at the point 'mask' was entered/,
so would only restore to /our/ masked state here, since we call @spawn@
from within our own 'mask'), 'forkIOWithUnmask''s @unmask@ is documented
and confirmed (by direct experimentation against GHC, including with an
already-masked caller before this function is ever invoked) to always
deliver a genuinely 'Control.Exception.Unmasked' state to its argument,
regardless of any enclosing masking context -- this is its specific,
documented purpose (\"used when the parent thread is masking
asynchronous exceptions and doesn't want its children to inherit that
masking state\"). So @spawn@'s own callback, not this function's own
@restore@, is deliberately used for the body here. The finalizer, which
must match 'Control.Concurrent.forkFinally' by running masked (so it
cannot itself be interrupted mid-cleanup/result-delivery), is wrapped in
an explicit 'mask_' rather than relying on it merely inheriting a masked
state from being forked within this function's own 'mask' (true today,
but not obviously so without tracing the masking-inheritance rules) --
making that invariant self-evident at the call site instead.
-}
forkTransferringOwnershipUsing
  :: (((forall a. IO a -> IO a) -> IO ()) -> IO handle)
  -> res
  -> (res -> IO ())
  -> (res -> IO b)
  -> (res -> Either SomeException b -> IO ())
  -> IO handle
forkTransferringOwnershipUsing spawn res release body finalize =
  mask $ \_restore ->
    spawn (\unmask -> try (unmask (body res)) >>= \outcome -> mask_ (finalize res outcome))
      `onException` release res

{- | A thread spawned with 'Control.Concurrent.forkIOWithUnmask' whose
completion is genuinely, synchronously awaitable via an 'MVar' its own
body fills exactly once (under 'mask_') on every exit path -- normal
return, a synchronous exception, or an asynchronous cancellation --
rather than only observable via the bounded, best-effort /poll/
@base@ actually offers for an arbitrary thread this code did not itself
spawn with its own completion signal ('GHC.Conc.Sync.threadStatus', which
has no blocking \"wait until this thread is dead\" counterpart at all).

Deliberately mirrors, but does not use, the \"async\" package's
'Control.Concurrent.Async.Async': @async@\'s own 'Control.Concurrent.Async.withAsync'
falls back to @Control.Concurrent.Async.uninterruptibleCancel@ internally
whenever its own wait is itself asynchronously interrupted while a child
is still running -- meaning a genuinely stuck child (e.g. blocked inside
a non-interruptible foreign call) can make *that* cancellation, and
therefore whatever enclosing cleanup depends on it completing (e.g.
Foundation shutdown), unconditionally unkillable until the child
eventually finishes, however long that takes. 'cancelManagedThread' below
uses only ordinary, interruptible 'Control.Exception.throwTo' and
'Control.Concurrent.MVar.takeMVar' -- so a caller of 'cancelManagedThread'
can always still itself be interrupted (e.g. by an enclosing bounded
wait), even against a managed thread that never responds; it never
becomes uninterruptible no matter how stuck the target is, and it never
lies about having stopped something it could not actually confirm was
terminal.
-}
data ManagedThread a = ManagedThread
  { managedThreadId :: ThreadId
  , managedThreadDone :: MVar (Either SomeException a)
  }

{- | Spawn a 'ManagedThread': masked from before the fork through the
completion 'MVar' existing, so there is no window in which the thread
could exist but nothing could ever learn of its completion. The body
itself still runs genuinely unmasked (via 'forkIOWithUnmask''s own
callback -- see 'forkTransferringOwnershipUsing''s Haddock for exactly
why that, and not this function's own 'mask'-provided @restore@, is what
genuinely unmasks it), so it remains fully interruptible throughout;
whatever it exits with -- a value, or any exception, synchronous or
asynchronous -- is captured by the enclosing 'Control.Exception.try' and
delivered, never lost, to 'waitManagedThread'\/'cancelManagedThread'.
-}
spawnManagedThread :: IO a -> IO (ManagedThread a)
spawnManagedThread action = mask_ $ do
  done <- newEmptyMVar
  tid <- forkIOWithUnmask $ \unmask -> do
    outcome <- try (unmask action)
    mask_ (tryPutMVar done outcome >> pure ())
  pure (ManagedThread tid done)

-- | Block until a 'ManagedThread' has completed, then either re-raise
-- whatever exception it exited with or return its result. An ordinary,
-- interruptible 'Control.Concurrent.MVar.readMVar' -- never a poll, and
-- never a destructive 'Control.Concurrent.MVar.takeMVar': the completion
-- cell is written at most once (see 'spawnManagedThread'), and a later
-- 'cancelManagedThread' call against the very same, already-finished
-- thread (e.g. from an enclosing @`Exception.onException`@ handler
-- triggered by this function's own re-thrown exception) must still be
-- able to observe that same completion rather than block forever on an
-- MVar this function has already drained.
waitManagedThread :: ManagedThread a -> IO a
waitManagedThread thread = readMVar (managedThreadDone thread) >>= either throwIO pure

{- | Ordinary, interruptible cancellation: deliver 'Control.Exception.ThreadKilled'
(a plain, interruptible 'Control.Exception.throwTo' -- never
@Control.Concurrent.Async.uninterruptibleCancel@) and then genuinely
/wait/ (an ordinary, blocking, non-destructive 'Control.Concurrent.MVar.readMVar'
on the completion cell itself -- never a bounded 'GHC.Conc.Sync.threadStatus'
poll, and never a timeout dressed up as success) for that thread to have
actually, synchronously finished before returning. Using 'Control.Concurrent.MVar.readMVar'
rather than 'Control.Concurrent.MVar.takeMVar' matters even beyond
ordinary interruptibility: it lets this function be safely called *again*
against a thread whose completion some other caller (e.g. 'waitManagedThread')
has already observed -- exactly the case when this is invoked from an
enclosing @`Exception.onException`@ handler triggered by that same
'waitManagedThread' re-throwing the target's own captured exception (see
'startSupervisedEnv''s @loopOnce@) -- without blocking forever on an MVar
a prior 'takeMVar' would have left permanently empty.

Deliberately does /not/ go through 'waitManagedThread' (which would
re-throw whatever the target itself exited with via its own
'Control.Exception.throwIO'): re-raising that from inside a @try
\@SomeException@ here would be indistinguishable from -- and so would
silently swallow -- an asynchronous exception delivered to /this/
(cancelling) thread while it is itself still blocked waiting, which would
incorrectly let this function return normally, claiming the target is
terminal, when in fact its own wait was merely interrupted before ever
observing that. Reading the completion 'MVar' directly avoids that
ambiguity entirely: whatever the target thread itself exited with
(including a genuine, unrelated failure that happened to race the
cancellation) is deliberately discarded once genuinely observed here --
callers that need to distinguish "cancelled cleanly" from "failed for an
unrelated reason" should use 'waitManagedThread' directly instead -- but
an exception delivered to /this/ thread before that value is ever
observed is never caught here, and propagates exactly as an ordinary
interruptible 'Control.Concurrent.MVar.readMVar' would: this can never
itself become unkillable, nor can it ever falsely report the target as
terminal, unlike @Async.withAsync@\/@uninterruptibleCancel@.
-}
cancelManagedThread :: ManagedThread a -> IO ()
cancelManagedThread thread = do
  throwTo (managedThreadId thread) ThreadKilled
  _ <- readMVar (managedThreadDone thread)
  pure ()

{- | A repository-owned, always-interruptible replacement for
@UnliftIO.Async.race_@\/@Control.Concurrent.Async.race_@: run @left@ and
@right@ concurrently (each on its own 'ManagedThread'), wait for whichever
finishes first (successfully or by throwing -- either way counts as
\"done\" for racing purposes, exactly like @race_@), then cancel and
genuinely await the other before returning.

@async@\/@unliftio@\'s own @race_@ is built on
@Control.Concurrent.Async.withAsync@ for both branches, whose own cleanup
(run once the racing @waitEither@ decides a winner, or if @race_@\'s
/caller/ is itself asynchronously cancelled while waiting) is
@Control.Concurrent.Async.uninterruptibleCancel@ -- so if the losing
branch does not respond to cancellation quickly (e.g. it is blocked
inside a synchronous, non-interruptible network read), that cleanup, and
therefore @race_@ itself, becomes unconditionally uninterruptible until
the loser eventually finishes, however long that takes. This can make an
enclosing shutdown (a 'cancelManagedThread' call from
'Application.shutdownApp' targeting whatever 'ManagedThread' this
function itself runs on) hang exactly as
'Api.Arkham.AwsEnvSupervisor''s own former @withAsync@ usage could.

This instead uses only ordinary, always-interruptible primitives
throughout -- 'spawnManagedThread', a plain 'Control.Concurrent.MVar.MVar'
signalled via 'Control.Concurrent.MVar.tryPutMVar' (so it can never
itself block), and 'cancelManagedThread' (ordinary @throwTo@ + a genuine,
interruptible, non-destructive wait) -- so a caller of this function can
always itself still be interrupted, even against a loser that never
responds; it never becomes uninterruptible no matter how stuck that loser
is, and it never returns having merely /started/ cancelling the loser
without having actually, synchronously confirmed it stopped.

Every child's cancellation is attempted even if the other fails or this
function's own wait is itself interrupted while racing: 'finally' ensures
'cancelBoth' always runs, and 'cancelBoth' itself always attempts to
cancel\/await *both* children regardless of whether the first one's
cancellation throws.

Returns whichever side finished first as an already-caught
@'Either' 'SomeException' ()@ (mirroring @Control.Exception.try
\@SomeException@ wrapped around an ordinary @race_@ call -- see
'Api.Arkham.Helpers.pubSubSupervisor', this function's only current
caller, for exactly why that outcome, not just @()@, is needed): only the
/synchronous/ result of whichever @action@ actually finished first is
ever captured this way; an asynchronous exception delivered to /this/
thread while it waits is never caught, and propagates exactly as an
ordinary interruptible 'Control.Concurrent.MVar.readMVar' would.
-}
raceManaged_ :: IO () -> IO () -> IO (Either SomeException ())
raceManaged_ leftAction rightAction = mask $ \restore -> do
  firstDone <- newEmptyMVar
  let announce action = do
        outcome <- try @SomeException action
        _ <- tryPutMVar firstDone outcome
        pure ()
  leftThread <- spawnManagedThread (announce leftAction)
  rightThread <-
    spawnManagedThread (announce rightAction)
      `onException` cancelManagedThread leftThread
  restore (readMVar firstDone)
    `finally` cancelBoth leftThread rightThread
 where
  cancelBoth :: ManagedThread () -> ManagedThread () -> IO ()
  cancelBoth leftThread rightThread = do
    leftOutcome <- try @SomeException (cancelManagedThread leftThread)
    rightOutcome <- try @SomeException (cancelManagedThread rightThread)
    case (leftOutcome, rightOutcome) of
      (Left e, _) -> throwIO e
      (Right (), Left e) -> throwIO e
      (Right (), Right ()) -> pure ()

{- | The single, authoritative state of @app\/DevelMain.hs@'s (or any
similar restart protocol's) current generation, held in exactly one
'Control.Concurrent.MVar.MVar' that itself serves as the serializing lock
for every restart\/stop attempt (see 'restartManagedGenerationUsing'\/'stopManagedGeneration'
below, the only ways this type is ever read or written).

This replaces an earlier design (@DevelMain.hs@'s two separate
'Foreign.Store' slots, @tidStoreNum@\/@doneStore@, plus this module's
former @restartGateForPreviousGeneration@\/@consumePreviousShutdownReplayable@\/
@acquireThenForkTransferringOwnershipGuarded@) that was found, under
independent review, to still be structurally unsound: those two slots
were populated by two separate, unmasked statements (an ordinary
async-exception gap between them), and -- more fundamentally -- every
generation shared the /same/ @done@ 'MVar' across restarts. An initial
acquisition\/spawn failure (which durably fills that shared @done@ with
'Left' but never durably publishes a @tidRef@) left a /later/, entirely
successful generation's own eventual shutdown result silently discarded
by 'Control.Concurrent.MVar.tryPutMVar' (which can never overwrite an
already-full cell) -- corrupting every /subsequent/ restart's view of
\"did the previous generation actually stop cleanly\" with a stale,
unrelated failure from a generation that was never even spawned.

'Running' below closes this at its root: each live generation owns its
*own*, freshly created @done@, never reused across restarts, so there is
no cell for any other generation's outcome to ever collide with. There is
also, by construction, exactly one 'MVar' anywhere describing \"what
generation (if any) is currently running\" -- not two independently
racing slots -- so every 'restartManagedGenerationUsing'\/'stopManagedGeneration'
call is fully serialized against every other one, including concurrent
callers, by ordinary 'Control.Concurrent.MVar.takeMVar'\/'Control.Concurrent.MVar.putMVar'
mutual exclusion.
-}
data RestartState handle
  = -- | No generation has ever been durably started -- either genuinely
    -- the very first attempt, or (see 'StartFailed') functionally
    -- equivalent to it: there is nothing live to cancel, and nothing
    -- pending to consult.
    NotStarted
  | -- | A generation is currently running, identified by @handle@
    -- (production: its 'Control.Concurrent.ThreadId'), with its own
    -- dedicated, single-use completion cell that only *this* generation's
    -- own eventual 'shutdownThenDeliver' call will ever fill.
    Running handle (MVar RetirementOutcome)
  | -- | The most recent attempt to start a replacement generation itself
    -- failed (acquisition or spawn threw) before any child was ever
    -- created. Kept distinct from 'NotStarted' purely for introspection
    -- (e.g. a caller reporting \"last start failed: ...\"); every
    -- production gate treats it exactly like 'NotStarted' -- there is
    -- nothing live to cancel, and no completion cell any other
    -- generation could ever collide with, so the next attempt proceeds
    -- immediately rather than replaying this stale failure forever.
    StartFailed SomeException
  | -- | The generation identified by @handle@ has already exited (its
    -- own 'Control.Concurrent.forkIOWithUnmask'-spawned body returned,
    -- threw, or was cancelled, and its own completion cell -- see
    -- 'Running' -- has therefore already been durably filled), but the
    -- attempt to tear it down (its @finalize@\/@shutdownApp@ call) itself
    -- failed, i.e. that completion cell holds 'Left', not 'Right'.
    --
    -- Deliberately kept distinct from both 'NotStarted' and 'StartFailed':
    -- unlike either of those, this generation's own resources
    -- (production: 'Application.shutdownApp''s AWS supervisor, Redis
    -- connection, database pool, room-heartbeat\/pub-sub threads --
    -- see 'releaseAll') may /not/ actually have been released. Every
    -- production gate therefore refuses to silently treat this the same
    -- as \"nothing to retire\": 'restartManagedGenerationUsing' re-raises
    -- the exact same recorded failure rather than proceeding to start a
    -- replacement generation on top of possibly-still-held resources.
    --
    -- The second field is the complete, newest-first history of every
    -- teardown attempt's own failure (at least one, by construction).
    -- The third field is a retry capability, when one is actually known
    -- to be safe -- see 'RetirementRetry' for the two ways this can be:
    -- either 'LocalRetry' (the teardown failure came from 'releaseAll'
    -- reporting purely synchronous failures, production: always, since
    -- 'Application.shutdownApp' uses it exclusively) or 'GlobalReceipt'
    -- (the teardown was asynchronously interrupted, and its own
    -- outstanding remainder was durably transferred to
    -- 'PendingCleanupOwner.globalPendingCleanupOwner' -- see
    -- 'RetirementOutcome') -- 'stopManagedGeneration' retries either kind
    -- (never merely replaying the same stale failure forever,
    -- root-causing a MEDIUM-severity finding that this constructor
    -- previously had nowhere at all to retain such a capability,
    -- silently abandoning every not-yet-released resource the instant
    -- an asynchronous cancellation interrupted 'releaseAll' mid-shutdown),
    -- and -- root-causing a further MEDIUM-severity finding -- a
    -- 'GlobalReceipt' lets this state genuinely, observably clear itself
    -- back to 'NotStarted' once that receipt's own eventual completion
    -- is confirmed, whether or not 'stopManagedGeneration''s own retry is
    -- what actually finished it (an entirely independent
    -- 'drainOwnedCleanup' call -- production: @DevelMain@'s own
    -- opportunistic one, or 'Application.handler''s -- may finish it
    -- first). It is 'Nothing' only for a teardown failure this module
    -- cannot itself prove safe to retry at all (some caller's own
    -- @finalize@ not built from 'releaseAllRecordingReceipt'\/'releaseAll'
    -- at all) -- exactly as unrecoverable-except-manually as this
    -- constructor was before this fix, never regressing it.
    RetireFailed handle (NonEmpty SomeException) (Maybe RetirementRetry)
  | -- | Acquiring (or spawning) the replacement generation's resource
    -- genuinely failed, /and/ so did every attempt made so far at the
    -- compensating cleanup meant to tear down whatever was actually
    -- created before this instead safely retire it. The first field is
    -- the original failure that triggered cleanup in the first place
    -- (whether @spawn@ failing at this exact layer, or @acquire@ itself
    -- throwing 'AcquisitionCleanupFailed' from a failed compensating
    -- release somewhere inside a /nested/
    -- 'acquireTransferringOwnershipOnSuccess' -- e.g. the AWS supervisor
    -- \/ Redis connection \/ pub\/sub thread \/ database pool chain built
    -- by 'Application.makeFoundation'). The second field is the
    -- complete, newest-first history of every cleanup attempt's own
    -- failure (at least one, by construction). The third field is the
    -- /exact/, still-outstanding release capability (built by
    -- 'composeManagedCleanup'\/'newManagedCleanup' -- an opaque
    -- 'ManagedCleanup', so it never re-runs an already-succeeded nested
    -- step, even under concurrent retries -- see 'acquireTransferringOwnershipOnSuccess'): only
    -- the attempt(s) recorded in the second field have actually run it,
    -- so it remains genuinely safe to retry -- see
    -- 'stopManagedGeneration', which is the only place that ever
    -- invokes it, always still serialized through the same @lock@ this
    -- constructor lives in, and always exactly once at a time.
    --
    -- Deliberately distinct from 'StartFailed': that constructor's own
    -- \"safe to treat exactly like 'NotStarted'\" guarantee depends
    -- entirely on either nothing ever having been created (@acquire@
    -- itself failed), or whatever @acquire@ /did/ create having since
    -- been genuinely, successfully released (@spawn@ failed, but
    -- @release@ then succeeded) -- both cases leave nothing live for a
    -- subsequent attempt to collide with. Here neither holds: cleanup
    -- itself is the thing that (so far) has failed, so the resource it
    -- was meant to tear down may still be fully or partially held. Every
    -- production /start/ gate therefore refuses to treat this as \"safe
    -- to retry immediately\" the way 'StartFailed' is -- exactly
    -- paralleling 'RetireFailed' (which this constructor otherwise
    -- mirrors, except there is no @handle@ here at all). Explicit
    -- recovery, however, unlike 'RetireFailed', IS possible here: the
    -- retained capability lets 'stopManagedGeneration' genuinely retry
    -- the exact same release, any number of times, clearing this state
    -- (to 'NotStarted') only once one such retry genuinely succeeds --
    -- see its own Haddock.
    StartCleanupFailed SomeException (NonEmpty SomeException) ManagedCleanup
  deriving stock (Generic)

{- | The two ways a 'RetireFailed' teardown failure can be genuinely,
safely retried -- see 'RetireFailed''s own Haddock, and
'classifyRetirementFailure', the only place either constructor is ever
built.
-}
data RetirementRetry
  = -- | The teardown failure was 'ReleaseAllFailed': a purely
    -- synchronous outcome (every action was attempted; at least one
    -- failed), never durably transferred anywhere else. The exact,
    -- still-outstanding 'ManagedReleasePlan' is retried directly,
    -- serialized through the same @lock@ 'RetireFailed' itself lives in
    -- -- see 'stopManagedGeneration'.
    LocalRetry ManagedReleasePlan
  | -- | The teardown was asynchronously interrupted before every action
    -- could even be attempted, and its own outstanding remainder was
    -- durably transferred to 'PendingCleanupOwner.globalPendingCleanupOwner'
    -- (see 'RetirementOutcome'\/'releaseAllRecordingReceipt'). The exact
    -- 'PendingCleanupOwner.CleanupReceipt' identifying that one hand-off
    -- is polled\/attempted (never re-transferred) via
    -- 'PendingCleanupOwner.attemptCleanupReceipt' -- see
    -- 'stopManagedGeneration'.
    GlobalReceipt PendingCleanupOwner.CleanupReceipt

{- | The truthful outcome of 'stopManagedGeneration', replacing a plain
'Bool' (which could only ever distinguish \"something was running\" from
\"nothing was running\", with no way to report \"something was running,
tried to stop, and that attempt itself failed\" other than lying about
one of the other two cases).
-}
data StopOutcome
  = -- | 'RestartState' was already 'NotStarted'\/'StartFailed': there was
    -- nothing live to stop.
    NothingWasRunning
  | -- | A live generation was found, cancelled, and its own teardown
    -- ('Right' from its completion cell) genuinely, successfully
    -- completed. 'RestartState' is now 'NotStarted'.
    StoppedCleanly
  | -- | A live generation was found and cancelled, but its own teardown
    -- itself failed ('Left' from its completion cell): its resources may
    -- not actually have been released. 'RestartState' is now
    -- 'RetireFailed', not 'NotStarted' -- see that constructor's own
    -- Haddock for why this must never be silently overwritten.
    StopFailed SomeException
  deriving stock (Show)

-- | 'RestartState'\'s own automatically-computed structural shape
-- signature (see "DevelStoreLock"\'s @$versioned@ section) -- this
-- changes automatically whenever 'RestartState'\'s own definition
-- changes shape (a constructor gaining, losing, or reordering fields; a
-- constructor being added or removed), without anyone needing to
-- remember to bump a hand-maintained slot number the way
-- @app\/DevelMain.hs@\'s own 'Foreign.Store' slot number previously had
-- to be -- root-causing the MEDIUM-severity finding that
-- 'RetireFailed' once gained a field without that number being bumped
-- to match.
instance Typeable handle => StructuralHash (RestartState handle)

-- | 'RestartState'\'s own current structural hash, specialized to
-- production's 'Control.Concurrent.ThreadId' -- the exact expected hash
-- @app\/DevelMain.hs@\'s restart-protocol lock is published\/read under.
restartStateSchemaHash :: Word64
restartStateSchemaHash = structuralHash (Proxy :: Proxy (RestartState ThreadId))

{- | Thrown by 'getOrCreateStore'\/'getExistingStore' instead of ever
touching a value published at a slot under an incompatible,
since-changed shape (production: a live @stack ghci@ session in which
@Api.Arkham.Lifecycle@\'s own source -- specifically 'RestartState'\'s
own definition -- was edited and @:reload@ed while a previous
incarnation had already published its own, differently-shaped value at
the exact same slot).

Deliberately fatal, never something this module attempts to recover
from automatically: silently proceeding to actually read the stale value
risks coercing an incompatible heap layout (undefined behaviour, not
merely a wrong answer -- see "DevelStoreLock"\'s own Haddock), and
silently overwriting it with a fresh default instead could orphan
whatever a still-live previous generation actually left published there,
which this process cannot itself prove does not exist. The only safe
recovery is a genuinely fresh 'Foreign.Store' table, i.e. a full
process\/@ghci@ restart -- never merely reloading this module again.
-}
data DevelStoreSchemaStale = DevelStoreSchemaStale
  { develStoreSchemaStaleStored :: Word64
  , develStoreSchemaStaleExpected :: Word64
  }
  deriving stock (Show)

instance Exception DevelStoreSchemaStale

{- | Atomically retrieve the value already published at the given
'Foreign.Store.Store' slot, or create-and-publish a fresh one (via
@mkDefault@) if the slot is currently empty -- unlike
@Foreign.Store.storeAction@ (the Yesod scaffold's own original helper),
which /always/ runs its action and /always/ overwrites the slot via
'Foreign.Store.writeStore', discarding whatever value (if any) was
already published there. That distinction matters enormously for a
restart-protocol lock\/state cell specifically: @storeAction slot
(newMVar NotStarted)@, called from @app\/DevelMain.hs@'s @update@ on
/every/ invocation (including a second, later @update@ after the first
generation is already running), would fabricate a brand new, empty
'MVar' 'NotStarted' and publish it over the existing one -- silently
losing all track of whatever generation the first call had already
started (and, for two genuinely concurrent initial callers, handing each
its own independent lock\/state, defeating the mutual exclusion
'restartManagedGenerationUsing'\/'stopManagedGeneration' otherwise
provide entirely).

A thin re-export of 'DevelStoreLock.getOrCreateVersionedStore' -- see
that module's own Haddock (and 'DevelStoreSchemaStale''s own) for
exactly why the composed 'Foreign.Store.lookupStore'\/'Foreign.Store.writeStore'
this performs is (a) guarded by a lock living in a genuinely separate,
never-reloaded Cabal package rather than a plain CAF defined directly in
/this/ module (an earlier version of this function did exactly that,
and was found, under independent review, to reopen the very race it
existed to close: this module itself is reachable, transitively, from
@app\/DevelMain.hs@'s own documented @stack ghci arkham-api:lib@ \/ @:l
app\/DevelMain.hs@ workflow, and is therefore just as reloadable as
@DevelMain.hs@ itself whenever its own source changes during a live
session), and (b) tagged with, and checked against, the caller's own
current expected structural hash (production: 'restartStateSchemaHash'),
throwing 'DevelStoreSchemaStale' rather than ever touching a
value published under an incompatible shape.
-}
getOrCreateStore :: Store (Word64, a) -> Word64 -> IO a -> IO a
getOrCreateStore store expected mkDefault = do
  access <- DevelStoreLock.getOrCreateVersionedStore expected store mkDefault
  case access of
    FreshlyPublished a -> pure a
    ReusedExisting a -> pure a
    VersionMismatch stored _ -> throwIO (DevelStoreSchemaStale stored expected)

-- | A thin re-export of 'DevelStoreLock.getExistingVersionedStore', for
-- a caller (production: @DevelMain.shutdown@) that only ever wants to
-- observe an existing published value, never create one -- sharing the
-- exact same lock as 'getOrCreateStore' (and therefore genuinely
-- serialized against it), unlike an earlier version of @shutdown@'s own
-- direct, unsynchronized 'Foreign.Store.lookupStore'\/'Foreign.Store.readStore'
-- calls, and the exact same schema check.
getExistingStore :: Store (Word64, a) -> Word64 -> IO (Maybe a)
getExistingStore store expected = do
  mAccess <- DevelStoreLock.getExistingVersionedStore expected store
  case mAccess of
    Nothing -> pure Nothing
    Just (FreshlyPublished a) -> pure (Just a)
    Just (ReusedExisting a) -> pure (Just a)
    Just (VersionMismatch stored _) -> throwIO (DevelStoreSchemaStale stored expected)

{- | Exactly 'getOrCreateStore', except it first refuses (via
'DevelStoreLock.LegacyDevelStoreSlotOccupied', /never/ reading or
forcing whatever value is actually published at @legacySlot@) to ever
touch @store@'s own slot at all while some strictly earlier, structurally
incompatible publication scheme's own slot is still occupied -- see
'DevelStoreLock''s own @$legacySlot@ section, and
@app\/DevelMain.hs@'s own restart-lock slot, the exact site this closes
a genuine, git-history-confirmed segfault for: an in-place slot-number
reuse across an untagged-to-tagged shape change, which
'getOrCreateVersionedStore'\/'getOrCreateStore' alone cannot detect
(they only ever compare two publications made at the *same* slot). The
only safe recovery from this exception is a full process\/GHCi restart
-- never retrying, and never simply picking a different slot number at
runtime -- exactly as 'DevelStoreSchemaStale' already requires for a
same-slot mismatch.
-}
getOrCreateStoreCheckingLegacySlot :: Word32 -> Store (Word64, a) -> Word64 -> IO a -> IO a
getOrCreateStoreCheckingLegacySlot legacySlot store expected mkDefault = do
  access <- DevelStoreLock.getOrCreateVersionedStoreCheckingLegacySlot legacySlot expected store mkDefault
  case access of
    FreshlyPublished a -> pure a
    ReusedExisting a -> pure a
    VersionMismatch stored _ -> throwIO (DevelStoreSchemaStale stored expected)

-- | The read-only counterpart of 'getOrCreateStoreCheckingLegacySlot',
-- exactly mirroring how 'getExistingStore' relates to 'getOrCreateStore'.
getExistingStoreCheckingLegacySlot :: Word32 -> Store (Word64, a) -> Word64 -> IO (Maybe a)
getExistingStoreCheckingLegacySlot legacySlot store expected = do
  mAccess <- DevelStoreLock.getExistingVersionedStoreCheckingLegacySlot legacySlot expected store
  case mAccess of
    Nothing -> pure Nothing
    Just (FreshlyPublished a) -> pure (Just a)
    Just (ReusedExisting a) -> pure (Just a)
    Just (VersionMismatch stored _) -> throwIO (DevelStoreSchemaStale stored expected)

-- | Attempt to finish every cleanup capability durably transferred to
-- 'PendingCleanupOwner.globalPendingCleanupOwner' so far (production:
-- whatever 'runManagedReleasePlan' handed off the instant an
-- asynchronous exception interrupted a foundation's shutdown) -- see
-- 'releaseAll''s own Haddock for exactly when that happens. Both
-- @app\/DevelMain.hs@\'s @update@\/@shutdown@ and 'Application.handler'
-- call this opportunistically, giving /both/ a genuine, demonstrable
-- place that eventually finishes leftover cleanup, not merely a place
-- that durably retains it forever unattempted.
drainOwnedCleanup :: IO (Either [SomeException] ())
drainOwnedCleanup = PendingCleanupOwner.drainPendingCleanup globalPendingCleanupOwner

{- | Start (or restart) a managed generation, fully serialized against
every other call (including concurrent ones) via @lock@: if a previous
generation is 'Running', cancel it and genuinely await its own dedicated
completion cell (never any other generation's); if 'NotStarted' or
'StartFailed', there is nothing live to retire, so this proceeds
immediately. Only then is @acquire@\/@spawn@\/publish attempted for the
/new/ generation, exactly as 'acquireThenForkTransferringOwnershipGuardedUsing'
(this function's structural predecessor) did -- but publishing now means
storing @'Running' handle newDone@ into @lock@ itself, rather than two
separately-scoped writes to two separate cells.

Precisely three things can go wrong, and each has an exact, narrow
recovery that restores @lock@ to a value describing reality, never a
stale one:

* Retiring the /previous/ generation (@cancel@, or awaiting its own
  @done@) itself throws or is cancelled -- i.e. /this thread's own/
  attempt to retire is interrupted, distinct from the previous
  generation's own teardown having genuinely completed and reported
  failure (see the next bullet): @lock@ is restored to the exact same
  'Running' value it held before this call -- nothing has actually
  changed yet (in production, @cancel@ has already sent
  'Control.Exception.ThreadKilled', so a subsequent retry's own @cancel@
  and @readMVar@ are simply repeated, both idempotent against an
  already-dying\/already-terminated target and an already-filled
  completion cell), so this is always safe to retry.
* The previous generation's own body has already exited and its
  completion cell was genuinely, successfully read, but that cell's own
  value is 'Left' -- its teardown (@finalize@\/@shutdownApp@) itself
  failed, so its resources may not actually have been released: @lock@
  is set to 'RetireFailed' (carrying the exact failure), and this call
  re-raises that same failure /without/ ever attempting to
  acquire\/spawn a replacement -- see 'RetireFailed''s own Haddock for
  why silently proceeding here would be unsound.
* The previous generation (if any) was already confirmed cleanly
  retired, but acquiring\/spawning the /replacement/ then fails or this
  thread is cancelled: @lock@ is set to 'NotStarted' -- never the stale
  previous 'Running' value, which no longer describes anything live --
  so a subsequent attempt starts fresh rather than trying to re-retire a
  generation that is already gone.

Only 'restore acquire' itself ever runs unmasked (matching every prior
version of this protocol): everything else -- consulting\/retiring the
previous generation, spawning the replacement, and publishing its handle
-- executes under one unbroken 'Control.Exception.mask', so there is no
gap, masked or otherwise, in which an asynchronous exception could leave
@lock@ describing something that is no longer true.
-}
restartManagedGenerationUsing
  :: MVar (RestartState handle)
  -- ^ lock: the single authoritative state cell. Also serves as the
  -- mutual-exclusion lock serializing every caller of this function and
  -- 'stopManagedGeneration' against each other.
  -> (((forall a. IO a -> IO a) -> IO ()) -> IO handle)
  -- ^ spawn: stands in for 'Control.Concurrent.forkIOWithUnmask' -- see
  -- 'forkTransferringOwnershipUsing' for why a test substitute matters.
  -> (handle -> IO ())
  -- ^ cancel: deliver a cancellation signal to a previous generation
  -- (production: 'Control.Concurrent.killThread'). Awaiting its actual
  -- termination is this function's own job (via that generation's own
  -- @done@, already held in @lock@) -- @cancel@ itself need only signal.
  -> IO res
  -- ^ acquire: the new generation's resource (production:
  -- 'Application.getApplicationRepl').
  -> (res -> IO ())
  -- ^ release: run if @acquire@ succeeds but @spawn@ then fails.
  -> (res -> IO b)
  -- ^ body: the new generation's own workload (production: running
  -- Warp).
  -> (res -> Either SomeException b -> MVar RetirementOutcome -> IO ())
  -- ^ finalize: run (masked) once @body@ exits, for any reason, with
  -- this generation's own freshly created completion cell -- typically
  -- @\\res outcome newDone -> shutdownThenDeliverRecordingReceipt sink (release' res) newDone@,
  -- for some per-attempt receipt @sink@ -- see 'shutdownThenDeliverRecordingReceipt'.
  -> IO ()
restartManagedGenerationUsing lock spawn cancel acquire release body finalize = mask $ \restore -> do
  priorState <- takeMVar lock
  retireOutcome <- try @SomeException (retirePrior priorState)
  case retireOutcome of
    Left cancelErr -> do
      -- This thread's own attempt to retire (cancel\/await) was itself
      -- interrupted, or @cancel@ threw synchronously: nothing has
      -- actually changed, so restore exactly what was there -- see this
      -- function's own Haddock for why repeating that retirement later
      -- is always safe.
      putMVar lock priorState
      throwIO cancelErr
    Right (Left (terminalState, terminalErr)) -> do
      -- The previous generation's own outcome (or a still-unresolved
      -- prior 'RetireFailed'\/'StartCleanupFailed') is already known to
      -- be unsafe to silently proceed past: never start a replacement
      -- on top of possibly-unreleased resources. 'retirePrior' pairs the
      -- exact 'RestartState' @lock@ should now hold with its own
      -- exception directly, so there is no separate lookup (and no
      -- partial function) needed to reconstruct it -- this remains total
      -- even for 'StartCleanupFailed', which (unlike 'RetireFailed') has
      -- no @handle@ at all to pair.
      putMVar lock terminalState
      throwIO terminalErr
    Right (Right ()) -> do
      startResult <- attemptAcquireAndSpawn restore
      case startResult of
        Started handle newDone -> putMVar lock (Running handle newDone)
        AcquireFailed err -> do
          -- Nothing was ever created: the previous generation (if any)
          -- is already confirmed cleanly retired, and this attempt never
          -- got as far as acquiring anything new either, so there is
          -- nothing live for @lock@ to describe.
          putMVar lock (StartFailed err)
          throwIO err
        SpawnFailedCleanly spawnErr -> do
          -- @acquire@ genuinely succeeded, but @spawn@ then failed --
          -- and the compensating @release@ run to clean up that
          -- already-acquired resource itself genuinely succeeded, so
          -- (exactly as if @acquire@ had failed outright) nothing is
          -- left live for @lock@ to describe.
          putMVar lock (StartFailed spawnErr)
          throwIO spawnErr
        FailedUncleanly originalErr cleanupErrs retryRelease -> do
          -- Either @spawn@ failed and the compensating @release@ run to
          -- clean up the resource @acquire@ already created ITSELF also
          -- failed, or @acquire@ itself threw 'AcquisitionCleanupFailed'
          -- from a nested compensating-cleanup failure further inside:
          -- unlike the case above, that resource's own teardown may not
          -- actually have completed. Never treat this as \"safe to retry
          -- immediately\" the way 'StartFailed' is -- see
          -- 'StartCleanupFailed''s own Haddock.
          putMVar lock (StartCleanupFailed originalErr cleanupErrs retryRelease)
          throwIO (NE.head cleanupErrs)
 where
  -- | Retire whatever @priorState@ describes, returning the previous
  -- generation's own teardown outcome (never discarding it -- see this
  -- function's own Haddock): 'Right' if there was nothing to retire, or
  -- retirement completed and its own completion cell held 'Right';
  -- 'Left', paired with the exact 'RestartState' @lock@ should now hold
  -- and the exact exception to re-raise, otherwise (pairing both
  -- directly here -- rather than reconstructing them separately
  -- afterwards from @priorState@ -- makes every case total by
  -- construction, including 'StartCleanupFailed', which has no @handle@
  -- for a 'RetireFailed'-shaped reconstruction to use). An exception
  -- escaping this action (caught by the outer 'try') means /this
  -- thread's own/ retirement attempt was itself interrupted, not that
  -- the previous generation's teardown reported failure.
  retirePrior = \case
    NotStarted -> pure (Right ())
    StartFailed _ -> pure (Right ())
    RetireFailed priorHandle failures maybeRetry -> pure (Left (RetireFailed priorHandle failures maybeRetry, NE.head failures))
    StartCleanupFailed originalErr cleanupErrs retryRelease ->
      pure (Left (StartCleanupFailed originalErr cleanupErrs retryRelease, NE.head cleanupErrs))
    Running priorHandle priorDone -> do
      cancel priorHandle
      outcome <- readMVar priorDone
      pure $ case outcome of
        RetiredCleanly -> Right ()
        RetirementFailed err maybeReceipt ->
          let (failures, maybeRetry) = classifyRetirementFailure err maybeReceipt
          in Left (RetireFailed priorHandle failures maybeRetry, err)

  -- | The exact outcome of attempting to acquire\/spawn a replacement
  -- generation, distinguishing every way it can fail precisely enough
  -- that the caller above never has to guess whether anything is still
  -- live (see 'StartCleanupFailed''s own Haddock for why 'StartFailed'
  -- alone cannot safely represent all of them).
  attemptAcquireAndSpawn restore = do
    acquireResult <- try @SomeException (restore acquire)
    case acquireResult of
      Left err -> pure $ case fromException err of
        -- @acquire@ was itself a (possibly nested)
        -- 'acquireTransferringOwnershipOnSuccess' whose own compensating
        -- cleanup already failed further inside: that failure, and its
        -- exact retriable capability, is preserved and carried through
        -- unchanged rather than being flattened into a plain,
        -- retryable 'AcquireFailed'.
        Just (AcquisitionCleanupFailed originalErr cleanupErrs retryRelease) ->
          FailedUncleanly originalErr cleanupErrs retryRelease
        Nothing -> AcquireFailed err
      Right res -> do
        newDone <- newEmptyMVar
        spawnResult <-
          try @SomeException
            (spawn (\unmask -> try (unmask (body res)) >>= \outcome -> mask_ (finalize res outcome newDone)))
        case spawnResult of
          Right handle -> pure (Started handle newDone)
          Left spawnErr -> do
            releaseResult <- try @SomeException (release res)
            case releaseResult of
              Right () -> pure (SpawnFailedCleanly spawnErr)
              Left cleanupErr ->
                FailedUncleanly spawnErr (cleanupErr :| []) <$> newManagedCleanup [release res]

-- | The exact outcome of 'attemptAcquireAndSpawn' (a purely local helper
-- of 'restartManagedGenerationUsing'): never conflates \"nothing was ever
-- created\" or \"created, but fully cleaned up\" (both safe to retry
-- immediately) with \"created, and the cleanup meant to tear it down
-- itself failed, at this layer or a nested one\" (never safe to retry
-- immediately -- see 'StartCleanupFailed').
data StartAttemptOutcome handle
  = Started handle (MVar RetirementOutcome)
  | AcquireFailed SomeException
  | SpawnFailedCleanly SomeException
  | -- | The original failure (@spawn@ failing at this exact layer, or
    -- @acquire@ itself throwing 'AcquisitionCleanupFailed' from a nested
    -- layer further inside), the complete newest-first history of every
    -- cleanup attempt's own failure so far, and the exact, still-
    -- outstanding release capability.
    FailedUncleanly SomeException (NonEmpty SomeException) ManagedCleanup

-- | 'restartManagedGenerationUsing', fixed to production's
-- 'Control.Concurrent.forkIOWithUnmask'.
restartManagedGeneration
  :: MVar (RestartState ThreadId)
  -> (ThreadId -> IO ())
  -> IO res
  -> (res -> IO ())
  -> (res -> IO b)
  -> (res -> Either SomeException b -> MVar RetirementOutcome -> IO ())
  -> IO ()
restartManagedGeneration lock = restartManagedGenerationUsing lock forkIOWithUnmask

{- | Cancel the currently running generation (if any), fully serialized
against 'restartManagedGenerationUsing' via the same @lock@, leaving
@lock@ as 'NotStarted' once genuinely, cleanly confirmed stopped. Returns
a 'StopOutcome' -- never a plain 'Bool', which could only ever
distinguish \"something was running\" from \"nothing was running\", with
no way to truthfully report \"something was running, was cancelled, and
its own teardown itself then failed\" other than lying about one of the
other two cases (previously: silently discarding that generation's own
completion-cell outcome and unconditionally reporting 'True').

Used by @app\/DevelMain.hs@'s standalone @shutdown@, which (before this
fix) read\/killed its 'Foreign.Store'-published 'ThreadId' entirely
outside of @update@\/@restartAppInNewThread@'s own protection -- a
concurrent @update@ call could race it. Serializing both through the
same @lock@ closes that gap too.

If cancelling\/awaiting the live generation is itself interrupted (this
thread receives an asynchronous exception while blocked on the genuinely
interruptible 'Control.Concurrent.MVar.readMVar' inside), @lock@ is
restored to the exact 'Running' value it held before -- never silently
marked 'NotStarted' while that generation might still, in fact, be alive
-- so this caller can always itself still be interrupted, and a failed
stop attempt never lets a live child's dependencies be released nor
lets a later caller believe nothing is running.

If the live generation is genuinely cancelled and its own completion
cell is genuinely read, but that cell holds 'Left' (its own teardown
failed), @lock@ is set to 'RetireFailed' -- never silently 'NotStarted'
-- and 'StopFailed' is returned, carrying the exact same failure; a
later 'restartManagedGenerationUsing'\/'stopManagedGeneration' call sees
that 'RetireFailed' and refuses to silently treat it as \"nothing
running\" either (see 'RetireFailed''s own Haddock). Unlike an earlier
version of this function, a 'RetireFailed' whose teardown failure
carries a retry capability (production: always, via 'releaseAll') is not
simply replayed forever: this retries it exactly the same way
'StartCleanupFailed' is retried below, root-causing the MEDIUM finding
that a generation whose shutdown was asynchronously cancelled mid-way
had no path anywhere back to ever finishing releasing what was left.

If @lock@ instead holds 'StartCleanupFailed', this is the ONE place that
actually retries its retained release capability -- never merely
replaying the same stale failure forever (see 'StartCleanupFailed''s own
Haddock for why the earlier version of this function, which did exactly
that, was itself a MEDIUM finding): the exact same capability is
attempted again, serialized against every other caller (concurrent or
sequential 'stopManagedGeneration'\/'restartManagedGenerationUsing'
call) via the same @lock@, so at most one retry ever runs at a time and
each sees the latest history. If this retry genuinely succeeds, @lock@
is finally cleared to 'NotStarted' and 'StoppedCleanly' is reported --
the only way out of 'StartCleanupFailed' there is. If it fails again
(synchronously -- see 'UnliftIO.Exception.tryAny'), the new failure is
prepended to the retained history and the exact same, still-untouched
capability is kept for a later retry; 'StopFailed' is returned. If this
retry attempt is itself asynchronously interrupted (this thread being
cancelled while blocked inside the capability, mid-retry), @lock@ is
restored to the exact 'StartCleanupFailed'\/'RetireFailed' it held
before this call, and the interrupting exception is rethrown rather than
being mistaken for a retry failure -- this loses no progress even then,
because the retained capability (an opaque 'ManagedCleanup'\/'ManagedReleasePlan',
built via 'composeManagedCleanup'\/'newManagedCleanup'\/'newManagedReleasePlan')
durably records, in its own private, 'Control.Concurrent.MVar.MVar'-serialized
state, exactly which of its
steps have already succeeded, independent of how any individual call to
it terminates; simply retaining the very same capability value
unchanged is already fully progress-aware.
-}
stopManagedGeneration :: MVar (RestartState handle) -> (handle -> IO ()) -> IO StopOutcome
stopManagedGeneration lock cancel = mask $ \restore -> do
  priorState <- takeMVar lock
  case priorState of
    NotStarted -> putMVar lock NotStarted >> pure NothingWasRunning
    StartFailed _ -> putMVar lock NotStarted >> pure NothingWasRunning
    RetireFailed priorHandle failuresSoFar maybeRetry -> case maybeRetry of
      Nothing -> putMVar lock priorState >> pure (StopFailed (NE.head failuresSoFar))
      Just (LocalRetry retryAction) -> do
        -- Exactly mirrors the 'StartCleanupFailed' case below -- see
        -- this function's own Haddock.
        retryOutcome <- try @SomeException (UE.tryAny (restore (runManagedReleasePlanOrThrow retryAction)))
        case retryOutcome of
          Left asyncErr -> do
            putMVar lock priorState
            throwIO asyncErr
          Right (Right ()) -> putMVar lock NotStarted >> pure StoppedCleanly
          Right (Left newErr) -> do
            let failuresSoFar' = NE.cons newErr failuresSoFar
            putMVar lock (RetireFailed priorHandle failuresSoFar' maybeRetry)
            pure (StopFailed newErr)
      Just (GlobalReceipt receipt) -> do
        -- Poll\/attempt (never re-transfer) this exact receipt --
        -- see 'PendingCleanupOwner.attemptCleanupReceipt''s own Haddock.
        -- Unlike 'runManagedReleasePlanOrThrow'\/'runManagedCleanup',
        -- this never itself throws for a synchronous outcome (success
        -- or failure are both returned as plain 'PendingCleanupOwner.ReceiptOutcome'
        -- data), so no inner 'UE.tryAny' is needed here: anything this
        -- 'Control.Exception.try' catches is, by construction, a
        -- genuine asynchronous interruption of this poll itself.
        pollOutcome <- try @SomeException (restore (PendingCleanupOwner.attemptCleanupReceipt globalPendingCleanupOwner receipt))
        case pollOutcome of
          Left asyncErr -> do
            putMVar lock priorState
            throwIO asyncErr
          Right PendingCleanupOwner.ReceiptSucceeded -> putMVar lock NotStarted >> pure StoppedCleanly
          Right (PendingCleanupOwner.ReceiptFailed newErrs) -> do
            let failuresSoFar' = NE.cons (NE.head newErrs) failuresSoFar
            putMVar lock (RetireFailed priorHandle failuresSoFar' maybeRetry)
            pure (StopFailed (NE.head newErrs))
          Right PendingCleanupOwner.ReceiptBusy ->
            -- Some other concurrent caller (another 'stopManagedGeneration'
            -- call against a different handle sharing this owner, a
            -- concurrent 'drainOwnedCleanup', or a doubled-up concurrent
            -- call against this exact @lock@ -- impossible while this
            -- 'Control.Exception.mask'\/@lock@ 'Control.Concurrent.MVar.takeMVar'
            -- serializes every caller, but kept total regardless) is
            -- already running this exact receipt right now: never race
            -- it, just report the same stale failure again this once.
            putMVar lock priorState >> pure (StopFailed (NE.head failuresSoFar))
    StartCleanupFailed originalErr cleanupErrs retryRelease -> do
      -- The outer 'try' only ever fires for a genuinely asynchronous
      -- interruption of THIS retry attempt: 'UE.tryAny' (inside)
      -- already catches every synchronous failure @retryRelease@ itself
      -- can raise into a plain 'Either', so it can never itself escape
      -- as a 'Left' here.
      retryOutcome <- try @SomeException (UE.tryAny (restore (runManagedCleanup retryRelease)))
      case retryOutcome of
        Left asyncErr -> do
          putMVar lock priorState
          throwIO asyncErr
        Right (Right ()) -> putMVar lock NotStarted >> pure StoppedCleanly
        Right (Left newCleanupErr) -> do
          let cleanupErrs' = NE.cons newCleanupErr cleanupErrs
          putMVar lock (StartCleanupFailed originalErr cleanupErrs' retryRelease)
          pure (StopFailed newCleanupErr)
    Running priorHandle priorDone -> do
      result <- try @SomeException (cancel priorHandle >> restore (readMVar priorDone))
      case result of
        Right RetiredCleanly -> putMVar lock NotStarted >> pure StoppedCleanly
        Right (RetirementFailed teardownErr maybeReceipt) -> do
          let (failures, maybeRetry) = classifyRetirementFailure teardownErr maybeReceipt
          putMVar lock (RetireFailed priorHandle failures maybeRetry) >> pure (StopFailed teardownErr)
        Left cancelErr -> putMVar lock priorState >> throwIO cancelErr

{- | Classify a generation's own teardown failure (production: whatever
@app\/DevelMain.hs@'s own @finalize@ -- always built from
'shutdownThenDeliverRecordingReceipt' -- raised, plus whatever receipt
(if any) that same call observed alongside it) into the complete failure
history and a safe retry capability, when one is known. A
'ReleaseAllFailed' names its own exact, narrowed, still-outstanding
retry action directly, so that becomes 'Just' a 'LocalRetry'; otherwise,
if a receipt was actually observed (the teardown was asynchronously
interrupted and its remainder durably transferred away), that becomes
'Just' a 'GlobalReceipt'; only a teardown failure that is neither (some
caller's own @finalize@ not built from 'shutdownThenDeliverRecordingReceipt'
at all) has no capability this module can itself prove safe to retry, so
'Nothing' -- never fabricating one -- exactly as
unrecoverable-except-manually as 'RetireFailed' was before either fix,
never regressing it.
-}
classifyRetirementFailure :: SomeException -> Maybe PendingCleanupOwner.CleanupReceipt -> (NonEmpty SomeException, Maybe RetirementRetry)
classifyRetirementFailure err maybeReceipt = case fromException err of
  Just (ReleaseAllFailed failures retryAction) -> (failures, Just (LocalRetry retryAction))
  Nothing -> (err :| [], GlobalReceipt <$> maybeReceipt)
