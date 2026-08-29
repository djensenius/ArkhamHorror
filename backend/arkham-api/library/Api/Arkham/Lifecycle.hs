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
  proceedOnlyIfPreviousShutdownSucceededReplayable,
  proceedOnlyIfPreviousShutdownSucceededReplayableUsing,
  forkTransferringOwnership,
  forkTransferringOwnershipUsing,
  ManagedThread,
  managedThreadId,
  spawnManagedThread,
  waitManagedThread,
  cancelManagedThread,
  acquireThenForkTransferringOwnership,
  acquireThenForkTransferringOwnershipUsing,
) where

import Control.Concurrent (ThreadId, forkIOWithUnmask)
import Control.Concurrent.MVar (MVar, newEmptyMVar, putMVar, readMVar, takeMVar, tryPutMVar)
import Control.Exception (
  AsyncException (ThreadKilled),
  SomeException,
  bracket,
  bracketOnError,
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

{- | Used by @app\/DevelMain.hs@'s @restartAppInNewThread@: given the
'MVar' 'shutdownThenDeliver' will (eventually) fill with the /previous/
generation's shutdown outcome, only proceed to start a replacement
generation once that outcome is 'Right ()' -- and do so /repeatably/,
without ever consuming a 'Left' or leaving the cell permanently empty.

This replaces an earlier, one-shot @Either SomeException () -> IO a ->
IO a@ version that always 'takeMVar''d the outcome up front: on a
failed/interrupted previous shutdown, that version still consumed the
'Left' from the 'MVar' (leaving it empty) before re-throwing -- so this
same one-shot 'MVar' cell (reused, per @DevelMain.hs@'s design, across
every generation's own eventual shutdown) could never be filled again,
and the /next/ restart attempt's own wait on it would block forever
instead of observing the same failure immediately. This version instead:

* Peeks the outcome with 'readMVar' (non-consuming) rather than
  'takeMVar'. On 'Left', it is left untouched and re-thrown immediately
  -- every subsequent call sees the exact same failure immediately,
  without blocking, and without ever starting a replacement.
* Only on 'Right ()' does it 'takeMVar' (finally consuming that success,
  emptying the cell so the /next/ generation's own eventual shutdown can
  fill it again) immediately before running @onSuccess@.
* If @onSuccess@ itself then fails (e.g. starting the replacement
  generation throws, synchronously or asynchronously, having already
  consumed the previous 'Right'), the same failure is written back into
  the now-empty cell as a 'Left' before being re-thrown -- so the cell is
  never left permanently empty either, and every subsequent call
  immediately observes that same failure rather than blocking on a cell
  nothing will ever fill again.

The gap between 'takeMVar' returning (consuming the previous 'Right ()')
and @onSuccess@'s own failure handling being installed is closed with
'mask': only 'readMVar' (which does not consume anything, so being
interrupted there is harmless -- the cell is untouched for the next
caller) and @onSuccess@ itself (explicitly 'restore'd, so it still runs
interruptibly, and any exception it raises -- sync or async -- is caught
by 'try' and written back before propagating) run with asynchronous
exceptions enabled. 'takeMVar' remains genuinely interruptible by design
even under 'mask' (so a blocked take can still be cancelled), but nothing
is consumed unless it actually returns; once it returns, every step up to
@onSuccess@ beginning is masked, so there is no window in which the cell
has been emptied but nothing (yet) owns repopulating it.

The @onSuccess@-failure repopulation ('putMVar done (Left err)') is
additionally wrapped in an explicit 'mask_', even though it already
executes at this function's own ambient masked level (everything here
except the two 'restore'd calls above runs masked, so this call is never
actually unmasked to begin with): 'Control.Concurrent.MVar.putMVar' can
still be interrupted even under (non-uninterruptible) 'mask' if it has to
/block/ (i.e. the target cell is unexpectedly already full), and this
call's safety otherwise relies on the single-owner invariant that this
cell is always empty here (having just been emptied by the 'takeMVar'
above, with nothing else able to refill it concurrently by this module's
design) rather than on any local, self-evident guarantee. The explicit
'mask_' costs nothing, does not change today's observable masking state,
and documents the invariant this call actually depends on rather than
leaving it implicit.
-}
proceedOnlyIfPreviousShutdownSucceededReplayable
  :: MVar (Either SomeException ()) -> IO a -> IO a
proceedOnlyIfPreviousShutdownSucceededReplayable =
  proceedOnlyIfPreviousShutdownSucceededReplayableUsing (pure ())

{- | As 'proceedOnlyIfPreviousShutdownSucceededReplayable', parameterized
over an extra hook run (still under the enclosing 'mask', i.e. with
asynchronous exceptions deferred) immediately after 'takeMVar' consumes
the previous 'Right ()' and immediately before @onSuccess@ begins.
Production always passes @'pure' ()@ via
'proceedOnlyIfPreviousShutdownSucceededReplayable'; this seam exists
purely so a test can observe (e.g. via 'Control.Exception.getMaskingState')
that this exact window is genuinely masked, deterministically and without
racing any real exception delivery against it.
-}
proceedOnlyIfPreviousShutdownSucceededReplayableUsing
  :: IO () -> MVar (Either SomeException ()) -> IO a -> IO a
proceedOnlyIfPreviousShutdownSucceededReplayableUsing afterConsume done onSuccess = mask $ \restore -> do
  outcome <- restore (readMVar done)
  case outcome of
    Left err -> throwIO err
    Right () -> do
      _ <- takeMVar done
      afterConsume
      result <- try (restore onSuccess)
      case result of
        Left err -> do
          mask_ (putMVar done (Left err))
          throwIO err
        Right a -> pure a

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

{- | As 'forkTransferringOwnershipUsing', but additionally owns
*acquiring* @res@ itself, rather than accepting an already-acquired
value: @acquire@ runs via @restore@ (so it remains fully interruptible --
if it throws, or this thread is asynchronously cancelled while it is
still in flight, there is nothing yet to release), but the very instant
it returns, this thread is already back in a masked state. This closes
the exact gap an ordinary @res <- acquire@ bind followed by a *separate*
call to 'forkTransferringOwnership' leaves open: an asynchronous
exception landing in between those two statements -- after @acquire@ has
already committed to returning ownership on success, but before this
function's own protection has begun -- would otherwise leak @res@ with
nothing left to release it. @DevelMain.hs@'s @start@ calls this directly
around 'Application.getApplicationRepl' for exactly this reason: there is
no intervening bind between acquiring the whole @(Int, App, Application)@
tuple and this function taking over its ownership.

Also accepts @publish@, run masked immediately after @spawn@ succeeds and
strictly before this function returns to its own caller: used to durably
record the spawned child's handle (e.g. into a
'Foreign.Store.Store'\/'Data.IORef.IORef'-backed restart-state cell)
before any asynchronous exception delivered to *this* (the acquiring)
thread -- once it eventually returns to an unmasked context -- could ever
have a chance to lose it. If @publish@ itself throws, @cancel@ is used to
cancel the just-spawned child and wait for it to genuinely finish (which,
by 'forkTransferringOwnershipUsing''s own contract, always runs
@finalize@ -- typically releasing @res@ -- exactly once first) before
that failure propagates, so a failed publish can never leave an
untracked, still-running child (nor a leaked @res@) behind.
-}
acquireThenForkTransferringOwnershipUsing
  :: (((forall a. IO a -> IO a) -> IO ()) -> IO handle)
  -> IO res
  -> (res -> IO ())
  -> (res -> IO b)
  -> (res -> Either SomeException b -> IO ())
  -> (handle -> IO ())
  -- ^ cancel: cancel the spawned child and wait for it to have
  -- genuinely, synchronously finished -- used only if @publish@ itself
  -- fails.
  -> (handle -> IO ())
  -- ^ publish: durably record @handle@ before this function returns.
  -> IO handle
acquireThenForkTransferringOwnershipUsing spawn acquire release body finalize cancel publish =
  mask $ \restore -> do
    res <- restore acquire
    handle <-
      spawn (\unmask -> try (unmask (body res)) >>= \outcome -> mask_ (finalize res outcome))
        `onException` release res
    publish handle `onException` cancel handle
    pure handle

-- | 'acquireThenForkTransferringOwnershipUsing', fixed to production's
-- 'Control.Concurrent.forkIOWithUnmask'.
acquireThenForkTransferringOwnership
  :: IO res
  -> (res -> IO ())
  -> (res -> IO b)
  -> (res -> Either SomeException b -> IO ())
  -> (ThreadId -> IO ())
  -> (ThreadId -> IO ())
  -> IO ThreadId
acquireThenForkTransferringOwnership = acquireThenForkTransferringOwnershipUsing forkIOWithUnmask
