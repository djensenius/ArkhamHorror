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
  acquireWithUnconditionalRelease,
  releaseAll,
  shutdownThenDeliver,
  forkTransferringOwnership,
  forkTransferringOwnershipUsing,
  ManagedThread,
  managedThreadId,
  spawnManagedThread,
  waitManagedThread,
  cancelManagedThread,
  raceManaged_,
  RestartState (..),
  restartManagedGeneration,
  restartManagedGenerationUsing,
  stopManagedGeneration,
) where

import Control.Concurrent (ThreadId, forkIOWithUnmask)
import Control.Concurrent.MVar (MVar, newEmptyMVar, putMVar, readMVar, takeMVar, tryPutMVar)
import Control.Exception (
  AsyncException (ThreadKilled),
  SomeException,
  bracket,
  bracketOnError,
  finally,
  mask,
  mask_,
  onException,
  throwIO,
  throwTo,
  try,
 )
import Prelude

{- | Acquire a resource whose ownership transfers to the caller on
success, but which must never leak: if the body throws, or this thread
is asynchronously cancelled, after acquisition but before the body
returns, the resource is released before that exception propagates.
Exactly 'Control.Exception.bracketOnError' -- used by
'Application.makeFoundation' (around 'Api.Arkham.AwsEnvSupervisor.newAwsEnvSupervisor',
so a later Redis\/database initialization failure cannot leak the
supervisor) and by 'Application.getApplicationRepl' (around
'Application.makeFoundation' itself, so a later 'Network.Wai.Handler.Warp.getDevSettings'\/
'Application.makeApplication' failure cannot leak the whole 'App').
-}
acquireTransferringOwnershipOnSuccess :: IO res -> (res -> IO ()) -> (res -> IO a) -> IO a
acquireTransferringOwnershipOnSuccess = bracketOnError

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

{- | Run every release action in the list, even if an earlier one throws:
an earlier failure must never cause a later release to be skipped. If
one or more actions failed, re-raise the first such failure only once
every action in the list has actually been attempted; if all succeed,
this is silent.

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
  results <- traverse (try @SomeException) actions
  case [e | Left e <- results] of
    (e : _) -> throwIO e
    [] -> pure ()

{- | Run a shutdown action, capturing /any/ exception it raises --
synchronous, or asynchronous delivered while it is blocked on its own
interruptible internal operations (e.g. awaiting a supervised thread) --
instead of letting it propagate and silently skip signalling entirely,
then atomically deliver the outcome to an observer (an empty 'MVar' a
restart protocol is waiting on).

Used by @app\/DevelMain.hs@'s restart protocol: without this, a
shutdown that throws mid-way never reaches the @putMVar@ that unblocks
the next restart's wait, deadlocking it forever. With this, the waiter
always receives a result -- 'Right' on a clean shutdown, or 'Left' on a
failed\/interrupted one -- and can choose not to start a replacement
generation on 'Left', avoiding both the deadlock and two live
generations ever coexisting.

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
shutdownThenDeliver :: IO () -> MVar (Either SomeException ()) -> IO ()
shutdownThenDeliver shutdown done = do
  result <- try shutdown
  mask_ (tryPutMVar done result >> pure ())

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
    Running handle (MVar (Either SomeException ()))
  | -- | The most recent attempt to start a replacement generation itself
    -- failed (acquisition or spawn threw) before any child was ever
    -- created. Kept distinct from 'NotStarted' purely for introspection
    -- (e.g. a caller reporting \"last start failed: ...\"); every
    -- production gate treats it exactly like 'NotStarted' -- there is
    -- nothing live to cancel, and no completion cell any other
    -- generation could ever collide with, so the next attempt proceeds
    -- immediately rather than replaying this stale failure forever.
    StartFailed SomeException

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

Precisely two things can go wrong, and each has an exact, narrow recovery
that restores @lock@ to a value describing reality, never a stale one:

* Retiring the /previous/ generation (@cancel@, or awaiting its own
  @done@) itself throws or is cancelled: @lock@ is restored to the exact
  same 'Running' value it held before this call -- nothing has actually
  changed yet (in production, @cancel@ has already sent
  'Control.Exception.ThreadKilled', so a subsequent retry's own @cancel@
  and @readMVar@ are simply repeated, both idempotent against an
  already-dying\/already-terminated target and an already-filled
  completion cell), so this is always safe to retry.
* The previous generation (if any) was already confirmed retired, but
  acquiring\/spawning the /replacement/ then fails or this thread is
  cancelled: @lock@ is set to 'NotStarted' -- never the stale previous
  'Running' value, which no longer describes anything live -- so a
  subsequent attempt starts fresh rather than trying to re-retire a
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
  -> (res -> Either SomeException b -> MVar (Either SomeException ()) -> IO ())
  -- ^ finalize: run (masked) once @body@ exits, for any reason, with
  -- this generation's own freshly created completion cell -- typically
  -- @\\res outcome newDone -> shutdownThenDeliver (release' res) newDone@.
  -> IO ()
restartManagedGenerationUsing lock spawn cancel acquire release body finalize = mask $ \restore -> do
  priorState <- takeMVar lock
  retireResult <- try @SomeException (retirePrior priorState)
  case retireResult of
    Left err -> do
      -- Retiring the previous generation (if any) itself failed or was
      -- cancelled: nothing has actually changed, so restore exactly what
      -- was there -- see this function's own Haddock for why repeating
      -- that retirement later is always safe.
      putMVar lock priorState
      throwIO err
    Right () -> do
      startResult <- try @SomeException (acquireAndSpawn restore)
      case startResult of
        Right (handle, newDone) -> putMVar lock (Running handle newDone)
        Left err -> do
          -- The previous generation (if any) is already confirmed
          -- retired; only starting its replacement failed, so there is
          -- nothing live for @lock@ to describe.
          putMVar lock (StartFailed err)
          throwIO err
 where
  retirePrior = \case
    NotStarted -> pure ()
    StartFailed _ -> pure ()
    Running priorHandle priorDone -> do
      cancel priorHandle
      () <$ readMVar priorDone

  acquireAndSpawn restore = do
    res <- restore acquire
    newDone <- newEmptyMVar
    handle <-
      spawn (\unmask -> try (unmask (body res)) >>= \outcome -> mask_ (finalize res outcome newDone))
        `onException` release res
    pure (handle, newDone)

-- | 'restartManagedGenerationUsing', fixed to production's
-- 'Control.Concurrent.forkIOWithUnmask'.
restartManagedGeneration
  :: MVar (RestartState ThreadId)
  -> (ThreadId -> IO ())
  -> IO res
  -> (res -> IO ())
  -> (res -> IO b)
  -> (res -> Either SomeException b -> MVar (Either SomeException ()) -> IO ())
  -> IO ()
restartManagedGeneration lock = restartManagedGenerationUsing lock forkIOWithUnmask

{- | Cancel the currently running generation (if any), fully serialized
against 'restartManagedGenerationUsing' via the same @lock@, leaving
@lock@ as 'NotStarted' once genuinely confirmed stopped. Returns 'True'
if there was a live generation to stop, 'False' if @lock@ was already
'NotStarted'\/'StartFailed'.

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
-}
stopManagedGeneration :: MVar (RestartState handle) -> (handle -> IO ()) -> IO Bool
stopManagedGeneration lock cancel = mask $ \restore -> do
  priorState <- takeMVar lock
  case priorState of
    NotStarted -> putMVar lock NotStarted >> pure False
    StartFailed _ -> putMVar lock NotStarted >> pure False
    Running priorHandle priorDone -> do
      result <- try @SomeException (cancel priorHandle >> restore (() <$ readMVar priorDone))
      case result of
        Right () -> putMVar lock NotStarted >> pure True
        Left err -> putMVar lock priorState >> throwIO err
