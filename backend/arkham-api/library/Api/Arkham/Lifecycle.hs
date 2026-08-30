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
  consumePreviousShutdownReplayable,
  consumePreviousShutdownReplayableUsing,
  restartGateForPreviousGeneration,
  forkTransferringOwnership,
  forkTransferringOwnershipUsing,
  ManagedThread,
  managedThreadId,
  spawnManagedThread,
  waitManagedThread,
  cancelManagedThread,
  raceManaged_,
  acquireThenForkTransferringOwnershipGuarded,
  acquireThenForkTransferringOwnershipGuardedUsing,
) where

import Control.Concurrent (ThreadId, forkIOWithUnmask, killThread)
import Control.Concurrent.MVar (MVar, newEmptyMVar, readMVar, takeMVar, tryPutMVar)
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
instead of observing the same failure immediately. This version instead
peeks the outcome with 'readMVar' (non-consuming) rather than 'takeMVar':
on 'Left', it is left untouched and re-thrown immediately -- every
subsequent call sees the exact same failure immediately, without
blocking, and without ever starting a replacement. Only on 'Right ()'
does it 'takeMVar' (finally consuming that success, emptying the cell so
the /next/ generation's own eventual shutdown can fill it again).

Unlike an earlier version of this function, this is deliberately *not*
itself a separate 'mask'\/'restore' boundary wrapping a caller-supplied
@onSuccess@: composing two independently-scoped 'mask' calls (this one,
restoring to call an entirely separate, later 'mask' inside what used to
be @acquireThenForkTransferringOwnership@) left ordinary, ambient-masking
Haskell code -- e.g. allocating a fresh ownership token -- running
genuinely /unmasked/ in the gap between them: an asynchronous exception
landing there, after this function's own 'takeMVar' had already consumed
the previous 'Right ()', would propagate with @done@ left permanently
empty, since no protection had even been installed yet to catch it. This
function is now a plain gate with no @onSuccess@ parameter at all,
designed to be called as the very first step /inside/
'acquireThenForkTransferringOwnershipGuarded''s own single 'mask' (see
its Haddock): 'readMVar'\/'takeMVar' remain genuinely interruptible even
under an enclosing 'mask' (so a blocked wait can still be cancelled), but
nothing is consumed unless 'readMVar' actually returns 'Right', and
nothing this function does can itself throw except by re-throwing an
already-recorded 'Left' -- so there is no window, masked or otherwise, in
which this gate's own execution could lose track of @done@.
-}
consumePreviousShutdownReplayable :: MVar (Either SomeException ()) -> IO ()
consumePreviousShutdownReplayable = consumePreviousShutdownReplayableUsing (pure ())

{- | As 'consumePreviousShutdownReplayable', parameterized over an extra
hook run immediately after 'takeMVar' consumes the previous 'Right ()'.
Production always passes @'pure' ()@ via
'consumePreviousShutdownReplayable'; this seam exists purely so a test
can observe (e.g. via 'Control.Exception.getMaskingState', with this
called from inside 'acquireThenForkTransferringOwnershipGuarded''s own
'mask') that this exact window is genuinely masked, deterministically and
without racing any real exception delivery against it.
-}
consumePreviousShutdownReplayableUsing :: IO () -> MVar (Either SomeException ()) -> IO ()
consumePreviousShutdownReplayableUsing afterConsume done = do
  outcome <- readMVar done
  case outcome of
    Left err -> throwIO err
    Right () -> do
      _ <- takeMVar done
      afterConsume

{- | Choose the correct restart gate for @app\/DevelMain.hs@'s @update@,
given the previously-published generation handle read from its own
durable 'Foreign.Store' -- or 'Nothing' if no generation has ever been
durably published there.

This is the fix for the finding that @DevelMain.hs@'s initial \"no server
running\" branch published its durable @tidRef@\/@done@ 'Foreign.Store'
slots (so a /later/ @update@ call would take the \"already running\"
branch below) as two ordinary, unmasked statements, strictly before ever
calling 'acquireThenForkTransferringOwnershipGuarded' (whose own single
'mask' is what actually protects everything from that point on -- see its
Haddock). An asynchronous exception landing in that narrow gap (after the
'Foreign.Store' slot exists, publishing @tidRef = 'Nothing'@, but before a
generation is ever actually spawned) used to leave that later @update@
call unconditionally treating @tidRef = 'Nothing'@ as \"a previous
generation exists and must be consulted\", calling
'consumePreviousShutdownReplayable' on a @done@ 'Control.Concurrent.MVar.MVar'
that no generation was ever going to fill -- deadlocking that
'Control.Concurrent.MVar.readMVar' (and therefore every subsequent
restart attempt) forever.

The fix is this exact distinction: @tidRef@'s own content is the single
source of truth for \"has a generation ever been durably published\",
independent of whether the 'Foreign.Store' slot itself already exists.
'Nothing' means exactly that -- whether because this is genuinely the
very first start, or because an earlier attempt was interrupted before
ever reaching a durable publish -- and in either case there is nothing
to kill and nothing pending in @done@ to consume, so the correct gate is
the same @'pure' ()@ used for a genuine first start. Only 'Just' means a
previous generation was actually spawned and durably published, in which
case it must be killed and its eventual result consumed exactly as
before.
-}
restartGateForPreviousGeneration :: Maybe ThreadId -> MVar (Either SomeException ()) -> IO ()
restartGateForPreviousGeneration mPreviousThreadId done = case mPreviousThreadId of
  Nothing -> pure ()
  Just previousThreadId -> do
    killThread previousThreadId
    consumePreviousShutdownReplayable done

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

{- | Combines 'consumePreviousShutdownReplayable' (or @'pure' ()@ for the
very first start, which has no previous generation to consult),
acquiring a resource whose ownership transfers to a newly-forked child,
and durably publishing a terminal failure into @done@ if no child was
ever actually spawned -- as a *single* masked transaction, with exactly
one 'Control.Exception.mask'\/@restore@ boundary (around @acquire@
alone). Used by @app\/DevelMain.hs@'s @start@ for both its initial \"no
server running\" branch (gate @= 'pure' ()@) and its restart branch
(gate @= 'consumePreviousShutdownReplayable' done@), so both get
identical protection.

This replaces an earlier design that composed three separately-scoped
combinators -- a gating @mask@ (this module's former
@proceedOnlyIfPreviousShutdownSucceededReplayable@, restoring to call an
entirely separate @onSuccess@), a wrapping @try@\/@transferred@-flag
combinator (@publishTerminalFailureUnlessTransferred@), and
@acquireThenForkTransferringOwnershipUsing@'s own, third, independent
@mask@ -- glued together by ordinary, *unmasked* Haskell code in between
(allocating a fresh ownership token, building the closure passed to the
next combinator). An asynchronous exception landing in either of those
unmasked gaps -- after the gate had already consumed the previous
generation's recorded success, but strictly before the next combinator's
own protection had even been installed -- would propagate with @done@
left permanently empty, since nothing had yet been set up to catch it:
every subsequent restart attempt would then block forever on
'consumePreviousShutdownReplayable''s own 'Control.Concurrent.MVar.readMVar'
against a cell nothing was ever going to fill again.

Folding @gate@, acquiring @res@, spawning the child, and publishing its
handle into *one* 'mask' closes every one of those gaps completely, not
merely narrows them: nothing between @gate@ running and @publish@
committing is ever restored except @acquire@ itself (so @acquire@ alone
remains genuinely interruptible, matching prior behavior exactly), and
everything else -- allocating a token, if any; spawning; publishing --
executes under an unbroken masked region an asynchronous exception simply
cannot enter. This also makes the historical \"transferred ownership
token\" this design used to need entirely unnecessary: because @publish@
now always runs strictly inside the *same* mask as everything else (never
crossing into a second, separately-scoped @mask@ an unmasked gap could
sit between), the only way this function's own @try@ below can ever
observe a 'Left' is if @publish@ itself never got the chance to durably
commit a handle in the first place -- and if @publish@ /did/ commit (so a
live child now exists), the only way to subsequently fail is via
@cancel@, which (matching @DevelMain.hs@'s own @cancel@:
@\\tid -> killThread tid >> (() \<$ readMVar done)@) itself already awaits
that same child's own terminal delivery into @done@ before ever
returning -- so by the time this function's own final
'Control.Concurrent.MVar.tryPutMVar' below could run, @done@ is either
still genuinely empty (no child ever existed) or has *already* been
filled by that same cancelled child's own finalizer, and
'Control.Concurrent.MVar.tryPutMVar' can never overwrite an already-full
cell. There is therefore no remaining code path, mutation or otherwise,
by which this function could durably publish a stale 'Left' over a live,
untracked child: doing so would require a live child that is neither
tracked by @done@'s own eventual filling nor ever cancelled\/awaited by
this same transaction, which this function's own control flow does not
admit.
-}
acquireThenForkTransferringOwnershipGuardedUsing
  :: IO ()
  -- ^ gate: run first, still under this function's own single 'mask'.
  -- Typically 'consumePreviousShutdownReplayable' @done@ (or
  -- 'consumePreviousShutdownReplayableUsing' for the test-only masking
  -- hook), or @'pure' ()@ when there is no previous generation to
  -- consult at all. A 'Left' recorded by a previous failed shutdown
  -- propagates immediately from here, untouched: no child has been
  -- created yet in this attempt, so there is nothing for this function
  -- to publish on its own behalf.
  -> (((forall a. IO a -> IO a) -> IO ()) -> IO handle)
  -- ^ spawn: stands in for 'Control.Concurrent.forkIOWithUnmask' -- see
  -- 'forkTransferringOwnershipUsing' for why a test substitute matters.
  -> MVar (Either SomeException ())
  -- ^ done: filled with 'Left' here only if no child was ever spawned;
  -- otherwise left for that child's own eventual 'shutdownThenDeliver'.
  -> IO res
  -> (res -> IO ())
  -> (res -> IO b)
  -> (res -> Either SomeException b -> IO ())
  -> (handle -> IO ())
  -- ^ cancel: cancel the spawned child and wait for it to have
  -- genuinely, synchronously finished (and, in production, for @done@
  -- to have been filled by its finalizer) -- used only if @publish@
  -- itself fails.
  -> (handle -> IO ())
  -- ^ publish: durably record @handle@ before this function returns.
  -> IO handle
acquireThenForkTransferringOwnershipGuardedUsing gate spawn done acquire release body finalize cancel publish =
  mask $ \restore -> do
    gate
    outcome <- try @SomeException do
      res <- restore acquire
      handle <-
        spawn (\unmask -> try (unmask (body res)) >>= \o -> mask_ (finalize res o))
          `onException` release res
      publish handle `onException` cancel handle
      pure handle
    case outcome of
      Right handle -> pure handle
      Left err -> do
        -- 'tryPutMVar' never overwrites an already-full cell (see this
        -- function's own Haddock): if @cancel@ above already delivered
        -- a cancelled child's own terminal result into @done@, this is
        -- a harmless no-op against an already-filled cell; if no child
        -- was ever spawned, this is the one and only place that will
        -- ever fill it. 'mask_' matches 'shutdownThenDeliver''s own
        -- identical guard: without it, a second asynchronous exception
        -- landing in the narrow window between catching @err@ and
        -- 'tryPutMVar' actually completing could interrupt this
        -- non-blocking write itself.
        _ <- mask_ (tryPutMVar done (Left err))
        throwIO err

-- | 'acquireThenForkTransferringOwnershipGuardedUsing', fixed to
-- production's 'Control.Concurrent.forkIOWithUnmask'.
acquireThenForkTransferringOwnershipGuarded
  :: IO ()
  -> MVar (Either SomeException ())
  -> IO res
  -> (res -> IO ())
  -> (res -> IO b)
  -> (res -> Either SomeException b -> IO ())
  -> (ThreadId -> IO ())
  -> (ThreadId -> IO ())
  -> IO ThreadId
acquireThenForkTransferringOwnershipGuarded gate = acquireThenForkTransferringOwnershipGuardedUsing gate forkIOWithUnmask
