{-# LANGUAGE NoImplicitPrelude #-}

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
  proceedOnlyIfPreviousShutdownSucceeded,
) where

import Control.Concurrent.MVar (MVar, putMVar)
import Control.Exception (
  SomeException,
  bracket,
  bracketOnError,
  mask_,
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
outcome 'shutdownThenDeliver' delivered for the /previous/ generation's
shutdown, only proceed to start a replacement generation on 'Right ()'.
On 'Left', the previous shutdown failed or was interrupted -- its
supervisor may not have actually stopped -- so no replacement is
started at all (which would otherwise risk two live generations); the
original exception is re-thrown instead of being swallowed, so the
failure is visible to whoever called @update@\/@restart@.
-}
proceedOnlyIfPreviousShutdownSucceeded :: Either SomeException () -> IO a -> IO a
proceedOnlyIfPreviousShutdownSucceeded (Left err) _onSuccess = throwIO err
proceedOnlyIfPreviousShutdownSucceeded (Right ()) onSuccess = onSuccess
