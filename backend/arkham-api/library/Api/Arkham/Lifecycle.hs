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
  shutdownThenDeliver,
  proceedOnlyIfPreviousShutdownSucceededReplayable,
  proceedOnlyIfPreviousShutdownSucceededReplayableUsing,
  forkTransferringOwnership,
  forkTransferringOwnershipUsing,
) where

import Control.Concurrent (ThreadId, forkIOWithUnmask)
import Control.Concurrent.MVar (MVar, putMVar, readMVar, takeMVar)
import Control.Exception (
  SomeException,
  bracket,
  bracketOnError,
  mask,
  mask_,
  onException,
  throwIO,
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

The delivery itself ('putMVar') runs under 'mask_' so that once the
shutdown outcome is known, nothing can prevent it from actually reaching
the waiter -- there is no gap between "outcome decided" and "outcome
delivered" for an asynchronous exception to land in.
-}
shutdownThenDeliver :: IO () -> MVar (Either SomeException ()) -> IO ()
shutdownThenDeliver shutdown done = do
  result <- try shutdown
  mask_ (putMVar done result)

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
          putMVar done (Left err)
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
    spawn (\unmask -> try (unmask (body res)) >>= finalize res)
      `onException` release res
