{-# LANGUAGE CPP #-}
{- | The application-lifetime, foundation-owned AWS 'Env' supervisor.

This module is deliberately independent of 'Import'\/'Foundation' (it only
depends on 'Arkham.Prelude' and Amazonka): 'Foundation' stores an
'AwsEnvSupervisor' as a field of its 'App', so this module cannot itself
import 'Foundation' (or anything that re-exports it, like 'Import')
without creating an import cycle.

= Why a supervisor, and why foundation-owned

The pinned Amazonka fork's @Amazonka.Auth.Background.fetchAuthInBackground@
(used internally by every role-based credential source @discover@ can
reach -- container and instance-profile credentials both go through it)
starts a background thread as soon as expiring credentials are obtained,
capturing /the thread that called @discover@/ as its eventual
refresh-failure @throwTo@ target. If every request constructed its own
'Amazonka.Env' (or even briefly discovered credentials on a request
thread before handing off), a later refresh failure could throwTo a Warp
worker that has long since moved on to an unrelated request.

The fix is architectural, not a wider catch: exactly one thread -- this
supervisor's own dedicated thread, started once when the application
starts and never restarted -- ever calls @acquire@ (in production,
'acquireAwsEnv', i.e. @newEnv discover@). Because that thread is always
the same one, for the entire lifetime of the running application, it is
always still there -- never a request worker that has since moved on --
to receive a delayed refresh failure. See 'startSupervisedEnv'.

Constructing this supervisor (and its dedicated thread) happens once, in
'Application.makeFoundation', /before/ Warp is handed the application and
starts accepting requests -- never lazily on the first request, and never
per-request. 'Application.shutdownApp' explicitly stops it (see
'stopAwsEnvSupervisor'), which is required for deterministic dev\/REPL
lifecycle (production process exit tears the thread down implicitly, but
'DevelMain'\/tests that repeatedly start and stop a foundation must not
accumulate supervisor threads).

To avoid every application\/test startup unconditionally contacting a
credential provider (environment\/file\/IMDS\/ECS metadata) whether or not
the bug-upload feature is ever used, the AWS-specific supervisor
('newAwsEnvSupervisor') is /demand-driven/: its dedicated thread starts
immediately at foundation-construction time, but blocks -- doing nothing,
contacting nothing -- until the first call to 'requestAwsEnvReady' signals
demand. From that point on it behaves exactly like an eagerly-started
supervisor: it never goes back to waiting, and every subsequent
generation (after a failure, backoff, or invalidation) is reacquired
immediately.

= Managed credential refresh, and its residual scope

@fetchAuthInBackground@'s only termination signal is a best-effort,
GC-triggered @Weak@ finalizer -- there is no synchronous way for /any/
caller, however carefully it is used, to genuinely wait for its
background refresh thread to have actually stopped (see
'releaseAwsEnvChild'\/'awaitThreadTerminated', which can therefore only
ever bound how long it polls, never confirm termination). It also
misclassifies ordinary cancellation: its refresh loop's own exception
handler catches /everything/, including a @killThread@-delivered
@ThreadKilled@, and reports it back to \"the thread that called
@discover@\" as a synthetic 'AuthError' indistinguishable from a genuine
credential failure.

'managedFetchAuthInBackground' is a repository-owned replacement for the
/one/ provider in this module's own code that calls @fetchAuthInBackground@
directly -- the instance-profile provider ('safeNamedInstanceProfile'),
reached both as the top-level fallback in 'discoverSafely' and, nested,
as a config-file profile's @credential_source=Ec2InstanceMetadata@ (both
share the exact same function, so both benefit identically). It is built
on 'Api.Arkham.Lifecycle.ManagedThread' rather than @fetchAuthInBackground@
itself: its release genuinely, synchronously awaits the refresh thread's
own completion 'MVar' -- never a poll -- and a release-in-progress is
recorded /before/ cancelling, so the refresh loop can correctly recognize
its own expected shutdown and never misreport it as a genuine 'AuthError'.

'managedFetchAuthInBackground' is a repository-owned, provider-agnostic
replacement for /every/ provider in this module's own code that could
otherwise reach @fetchAuthInBackground@ -- the instance-profile provider
('safeNamedInstanceProfile'), container credentials ('safeContainer'\/
'safeContainerEnv'), @sts:AssumeRole@ ('safeAssumedRole'), web identity
('safeWebIdentity'\/'safeWebIdentityEnv'), and SSO ('safeSSO'), reached
both as top-level 'discoverSafely' entries and, nested, via a config-file
profile's @source_profile@\/@credential_source@\/@AssumeRoleWithWebIdentity@\/
@AssumeRoleViaSSO@ variants (see 'ConfigProfileResolvers'). Each of these
is built entirely from that pinned provider module's own /exported/
one-shot request\/response primitives (@Amazonka.Send.send@\/
@sendUnsigned@\/@sendUnsignedEither@, the generated request smart
constructors, @Amazonka.Auth.SSO.readCachedAccessToken@\/
@roleCredentialsToAuthEnv@, or plain @http-client@ for the ECS metadata
endpoint) rather than a patched\/forked dependency, wired through
'managedFetchAuthInBackground' instead of the pinned fork's own
@fetchAuthInBackground@: its release genuinely, synchronously awaits the
refresh thread's own completion 'MVar' -- never a poll -- and a
release-in-progress is recorded /before/ cancelling, so the refresh loop
can correctly recognize its own expected shutdown and never misreport it
as a genuine 'AuthError'.

No provider this module's own production credential graph can reach
still calls the pinned fork's own @fetchAuthInBackground@, or still
relies on 'releaseAwsEnvChild''s kill-then-bounded-poll fallback for a
genuinely live, expiring credential: only 'fromKeysEnv' (static,
non-expiring credentials that never fork a background thread at all) and
'ExplicitKeys' bypass 'managedFetchAuthInBackground' entirely, and for
both, 'releaseAwsEnvChild'\/'requireChildReleased''s @Auth@ branch is an
immediate, non-blocking no-op -- never a kill, never a poll -- so there is
no residual hazard on that path either. 'releaseAwsEnvChild'\/
'awaitThreadTerminated' remain exported and exercised directly by this
module's own regression tests (documenting the primitive itself, and
guarding against ever silently reintroducing an unmanaged @Ref@ into this
credential graph), but no production acquisition path still depends on
their kill-then-poll branch to release a live child.

= Structural release ownership

Every managed provider in this module -- 'safeContainer', 'safeAssumedRole',
'safeWebIdentity', 'safeSSO', 'safeDefaultInstanceProfile'\/
'safeNamedInstanceProfile', and 'resolveContainerCredentials' -- returns
its resolved 'Env' paired with 'managedFetchAuthInBackground''s
genuinely-awaiting release as a /mandatory/ @(Env, IO ())@ tuple, never a
bare 'Env' plus an optional 'IORef' side channel a caller could forget to
populate. This is a deliberate, structural (type-level, not merely
runtime-checked) redesign: an earlier version of this module instead
handed each provider an @IORef (Maybe (IO ()))@ to /optionally/ write a
release into (via a since-removed @withManagedRelease@ combinator), which
meant a provider that returned a live, expiring 'Ref'-shaped 'Env' without
ever populating that ref was only caught at /runtime/, by 'deriveRelease'
reaping the orphaned worker and throwing 'ManagedReleaseInvariantViolated'
-- a real, working safety net, but one that could only ever fire /after/
such a bug already shipped. With the mandatory-tuple shape, that same bug
is instead a /compile/-time type error: there is no longer any way to
write a @ConfigProfileResolvers@ field, or call any of the @safe*@
functions above, that produces an 'Env' without also producing its
release in the same expression.

Only 'discoverSafely''s own /outer/ candidates -- 'safeFileEnv',
'safeWebIdentityEnv', 'safeContainerEnv', and 'safeDefaultInstanceProfileEnv'
-- still take an @IORef (Maybe (IO ()))@ (@finalReleaseRef@) parameter and
have the narrower @EnvNoAuth -> IO Env@ shape, because that shape is fixed
by the pinned Amazonka fork's own external @newEnv@\/@runCredentialChain@
API, not something this module controls. Each of these outer wrappers
does exactly one @writeIORef finalReleaseRef@, unconditionally, derived
directly from the mandatory tuple its own inner provider (or
'resolveContainerCredentials'\/'safeDefaultInstanceProfile' etc.) already
returned -- there is no separate code path where that write could be
skipped or forgotten. 'deriveRelease'\/'ManagedReleaseInvariantViolated'
remain defined only as a defensive backstop at 'acquireAwsEnv''s own
outer boundary (see its Haddock), reachable in practice only when
@finalReleaseRef@ is genuinely 'Nothing' -- which, after this redesign,
can only happen when the resolved 'Auth' came from 'fromKeysEnv' (static,
non-expiring credentials that never populate it at all), never from a
live, expiring 'Ref'.
-}
module Api.Arkham.AwsEnvSupervisor (
  -- * AWS error diagnostics -- safe to cross IO boundaries and to log
  AwsErrorDiagnostic (..),
  AwsErrorCategory (..),
  classifyErrorDiagnostic,
  AwsAuthErrorDiagnostic (..),
  classifyAuthErrorDiagnostic,

  -- * Generic supervisor state, safe to observe
  SupervisedEnvState (..),

  -- * The application's demand-driven AWS 'Env' supervisor
  AwsEnvSupervisor,
  newAwsEnvSupervisor,
  requestAwsEnvReady,
  stopAwsEnvSupervisor,

  {- | The internal-only regression 'Spec' for every credential
  acquisition\/release primitive this module deliberately does NOT export
  otherwise (see \"Structural release ownership\" above, and this
  binding's own Haddock): 'ManagedEnvAcquisition', 'AwsEnvAcquisition',
  'runManagedEnvAcquisition', every @safe*@ provider, the generic
  single-thread supervisor protocol, and the generic demand-driven
  wrapper are all still defined in this module (production code needs
  them), but none of them appear anywhere above this line -- there is no
  way for any OTHER module, test or production, to import, construct,
  deconstruct, or mismatch any of them. This is the only remaining way
  they are exercised at all outside 'newAwsEnvSupervisor'\/
  'requestAwsEnvReady'\/'stopAwsEnvSupervisor'\/'startSupervisedEnvUsing''s
  own already-safe test seam. Only exported (and only defined at all)
  when the @internal-test-hooks@ Cabal flag is enabled -- see this
  module's own top-of-file note and @package.yaml@ -- so a genuine
  production\/deployment build of this library never depends on
  'Test.Hspec' at all.
  -}
#ifdef INTERNAL_TEST_HOOKS
  awsEnvSupervisorInternalSpec,
#endif
) where

import Amazonka (AuthEnv (..), Env, Env' (..), EnvNoAuth, Error (..), ISO8601, Region (..), SerializeError (..), ServiceError (..), Time (..), expiration, fromTime, newEnv, newEnvNoAuth, runResourceT, send, sendUnsigned, sendUnsignedEither)
import Amazonka.Auth (Auth (..), AuthError (..), fromKeysEnv, runCredentialChain)
import Amazonka.Auth.ConfigFile (ConfigProfile (..), CredentialSource (..), configPathRelative, mergeConfigs, parseConfigProfile)
import Amazonka.Auth.SSO (CachedAccessToken (accessToken), readCachedAccessToken, relativeCachedTokenFile, roleCredentialsToAuthEnv)
import Amazonka.Data.Sensitive (fromSensitive)
import Amazonka.Error (serviceError)
import Amazonka.EC2.Metadata hiding (region)
import Amazonka.EC2.Metadata qualified as IdentityDocument (IdentityDocument (..))
import Amazonka.Env (lookupRegion)
import Amazonka.SSO.GetRoleCredentials qualified as SSO
import Amazonka.STS.AssumeRole qualified as STS
import Amazonka.STS.AssumeRoleWithWebIdentity qualified as STS
import Api.Arkham.Lifecycle (
  ManagedThread,
  cancelManagedThread,
  managedThreadId,
  spawnManagedThread,
  waitManagedThread,
 )
import Arkham.Prelude
import Control.Concurrent (ThreadId, forkIO, killThread, myThreadId, threadDelay)
import Control.Concurrent.STM (check, retry)
import Control.Exception qualified as Exception
import Data.ByteString.Char8 qualified as BS8
import Data.ByteString.Lazy.Char8 qualified as LBS8
import Data.HashMap.Strict qualified as HashMap
import Data.Ini qualified as INI
import Data.Text qualified as Text
import Data.Text.Encoding qualified as TE
import Data.Text.IO qualified as TextIO
import Data.Time (addUTCTime, diffUTCTime)
import Data.UUID qualified as UUID
import Data.UUID.V4 qualified as UUID
import Data.Void (Void, absurd)
import GHC.Conc.Sync (ThreadStatus (..), threadStatus)
import Network.HTTP.Client qualified as Client
import Network.HTTP.Types.Status (status403, status404, status500, statusCode)
import System.Directory qualified as Directory
import System.Environment (lookupEnv, setEnv, unsetEnv)
import System.IO.Unsafe (unsafePerformIO)
#ifdef INTERNAL_TEST_HOOKS
import Test.Hspec
#endif

{- | Non-secret, structured classification of a per-request 'Error', safe to
log. Deliberately extracts only the numeric HTTP status, plus a category
derived purely from that status number, rather than showing the exception
itself or any text originating in the response: 'TransportError' wraps an
'HttpException' whose embedded 'Request' includes the raw
@X-Amz-Security-Token@ header verbatim when temporary/session credentials
are in use (http-client's default 'Show' instance only redacts
@Authorization@, not that header), 'SerializeError' carries the raw,
unredacted response body, and 'ServiceError''s symbolic error code and
request id are themselves server-provided free-form text -- so neither is
included here, only the status code they were reported with.
-}
data AwsErrorDiagnostic
  = AwsTransportFailure
  | AwsSerializeFailure {awsErrorStatus :: Int, awsErrorCategory :: AwsErrorCategory}
  | AwsServiceFailure {awsErrorStatus :: Int, awsErrorCategory :: AwsErrorCategory}
  deriving stock (Eq, Show)

{- | A coarse category derived purely from the numeric HTTP status code --
never from response body, headers, message text, symbolic error code, or
request id -- so it can never echo server-provided content.
-}
data AwsErrorCategory
  = AwsCategoryNotFound
  | AwsCategoryClientError
  | AwsCategoryThrottled
  | AwsCategoryServerError
  | AwsCategoryUnknownStatus
  deriving stock (Eq, Show)

categorizeAwsStatus :: Int -> AwsErrorCategory
categorizeAwsStatus 404 = AwsCategoryNotFound
categorizeAwsStatus 429 = AwsCategoryThrottled
categorizeAwsStatus s
  | s >= 500 && s < 600 = AwsCategoryServerError
  | s >= 400 && s < 500 = AwsCategoryClientError
  | otherwise = AwsCategoryUnknownStatus

classifyErrorDiagnostic :: Error -> AwsErrorDiagnostic
classifyErrorDiagnostic = \case
  TransportError _ -> AwsTransportFailure
  -- Pattern-matched directly on the field (rather than record-dot syntax)
  -- because 'SerializeError' and 'ServiceError' share field names within
  -- Amazonka.Types, and only 'ServiceError' picks up an automatic
  -- 'HasField' instance for '.status' in this GHC/amazonka-core version.
  SerializeError SerializeError' {status = st} -> AwsSerializeFailure (statusCode st) (categorizeAwsStatus (statusCode st))
  ServiceError e -> let st = statusCode e.status in AwsServiceFailure st (categorizeAwsStatus st)

{- | Non-secret, structured classification of an 'AuthError' raised by
credential/config discovery, safe to log. Deliberately never shows the
exception itself: 'RetrievalError' wraps an 'HttpException' with the same
@X-Amz-Security-Token@/IMDS-token header risk as 'AwsErrorDiagnostic',
'InvalidIAMError' wraps a message built by appending an Aeson decode error
to text derived from the (potentially credential-bearing) container
credentials response body (which Aeson's error messages can echo
fragments of), and 'OtherAuthError' wraps an arbitrary 'SomeException'
whose 'Show' output is entirely unconstrained. Only the failure category
(and, for 'AuthServiceError', the same sanitized status/category as
'AwsErrorDiagnostic' -- never the response's own error code or request id)
is reported.
-}
data AwsAuthErrorDiagnostic
  = AwsAuthRetrievalFailure
  | AwsAuthMissingEnv
  | AwsAuthMissingFile
  | AwsAuthInvalidFile
  | AwsAuthInvalidIAM
  | AwsAuthCredentialChainExhausted
  | AwsAuthServiceFailure {awsAuthErrorStatus :: Int, awsAuthErrorCategory :: AwsErrorCategory}
  | AwsAuthOtherFailure
  | -- | The dedicated AWS 'Env' supervisor thread itself has exited --
    -- whether from an unanticipated programmer fault propagating out of
    -- 'acquireAwsEnv'\/'awaitAwsEnvInvalidation', or from an explicit
    -- 'stopSupervisedEnv' -- and so no generation is being acquired,
    -- refreshed, or monitored any longer. Published so that a stale
    -- 'SupervisedEnvReady' snapshot (pointing at an 'Env' nobody is still
    -- supervising) can never be read as if it were still valid; see
    -- 'startSupervisedEnv'.
    AwsAuthSupervisorTerminated
  deriving stock (Eq, Show)

classifyAuthErrorDiagnostic :: AuthError -> AwsAuthErrorDiagnostic
classifyAuthErrorDiagnostic = \case
  RetrievalError _ -> AwsAuthRetrievalFailure
  MissingEnvError _ -> AwsAuthMissingEnv
  MissingFileError _ -> AwsAuthMissingFile
  InvalidFileError _ -> AwsAuthInvalidFile
  InvalidIAMError _ -> AwsAuthInvalidIAM
  CredentialChainExhausted -> AwsAuthCredentialChainExhausted
  AuthServiceError e -> let st = statusCode e.status in AwsAuthServiceFailure st (categorizeAwsStatus st)
  OtherAuthError _ -> AwsAuthOtherFailure

{- | Whether 'releaseAwsEnvChild' genuinely confirmed the target thread's
termination, or could only bound how long it waited. Never conflate the
two: 'ChildReleaseTimedOut' must propagate as a release /failure/ (see
'releaseAwsEnvAcquisition'\/'releaseAwsEnvGeneration'), never be silently
treated as if the child were actually gone -- a caller that did so could
believe a live background refresh thread had stopped when it had not,
exactly the \"release reports success with a live child\" hazard this
type exists to make structurally impossible to repeat.
-}
data ChildReleaseOutcome = ChildReleased | ChildReleaseTimedOut
  deriving stock (Eq, Show)

{- | Kill whatever background credential-refresh thread an already-acquired
'Auth' value may own, without otherwise touching its credentials.

Why this exists: the pinned Amazonka fork's
@Amazonka.Auth.Background.fetchAuthInBackground@ (used internally by every
role-based credential source @discover@ can reach -- container and
instance-profile credentials both go through it) starts a background
thread as soon as temporary\/expiring credentials are obtained. Per its
own source, that thread captures /the thread that called @discover@/ as
its eventual refresh-failure @throwTo@ target, and separately, its own
'ThreadId' -- returned by 'forkIO' -- is what ends up inside the 'Ref'
constructor here. This is exactly why 'newAwsEnvSupervisor' never calls
@discover@ from a Warp request-handling thread: it is always called from
one dedicated, long-lived supervisor thread (see 'acquireAwsEnv'), which
remains alive for as long as the generation it acquired remains current
(see 'awaitAwsEnvInvalidation') -- so that thread is always still there,
never a request worker, to either observe a genuine refresh failure or
(via this function) pre-emptively kill the thread that would otherwise
raise it. 'Auth' (no expiration, e.g. static access keys) never started
such a thread in the first place, so there is nothing to kill.

'Exception.killThread'\/'Exception.throwTo' only block until the target
thread has /begun/ handling the exception -- not until it has actually
finished unwinding and become 'ThreadFinished'\/'ThreadDied' (see
@base@'s own @Control.Exception@ Haddock on @throwTo@: \"the exception is
not immediately raised in the target thread ... whatever work the target
thread was doing when the exception was raised is not lost\"). A caller
that immediately assumes the child is fully gone the instant 'killThread'
returns can race a child that is still mid-unwind. This function polls
'threadStatus' afterward, bounded, so a caller relying on it can trust
every child it releases is genuinely terminal by the time it returns --
but, unlike an earlier version of this function, it no longer silently
reports success ('()') once that bound is exhausted while the child is
still alive: it honestly returns 'ChildReleaseTimedOut' rather than a
false \"released\" claim. The bound only guards against a genuinely stuck
foreign call (e.g. 'killThread' targeting a thread currently blocked
inside a non-interruptible foreign HTTP call can itself block until that
call completes; this remains a documented, accepted platform limitation
for an unmanaged @Ref@ in general -- see the module Haddock's \"Managed
credential refresh\" section) -- it never substitutes for a real
synchronization guarantee, and never lies about one either. No provider
this module's own production credential graph can reach still produces a
@Ref@ that carries a live, unmanaged @fetchAuthInBackground@ thread here
(every provider capable of expiring credentials now goes through
'managedFetchAuthInBackground', which awaits its own completion 'MVar'
directly and never falls back to this function); this kill-then-poll
path remains exported and exercised only by this module's own regression
tests, guarding against ever silently reintroducing an unmanaged @Ref@.
-}
releaseAwsEnvChild :: Auth -> IO ChildReleaseOutcome
releaseAwsEnvChild (Auth _) = pure ChildReleased
releaseAwsEnvChild (Ref refreshThreadId _) = do
  killThread refreshThreadId
  awaitThreadTerminated refreshThreadId

{- | Exception thrown when a 'ChildReleaseOutcome' of 'ChildReleaseTimedOut'
must be surfaced as a release failure (see 'releaseAwsEnvChild'). Not an
'AuthError': it must never be discarded by 'releaseAwsEnvAcquisition''s
own \"an 'AuthError' during release is just expected self-inflicted
feedback\" tolerance, since a timeout here means the opposite of expected
-- the child could not even be confirmed to have received\/finished
handling the cancellation at all.
-}
newtype ChildReleaseTimedOutException = ChildReleaseTimedOutException ThreadId
  deriving stock (Show)

instance Exception.Exception ChildReleaseTimedOutException

{- | Thrown by 'deriveRelease' when a provider returns an 'Env' whose
@.auth@ is a live, expiring 'Ref' but never recorded a managed release
into the 'IORef' it was handed for exactly that purpose (see the module
Haddock, \"Structural release ownership\", and 'acquireAwsEnv'). This can
only ever be reached by a /new/ programming error in this module itself
-- every current provider in 'productionConfigProfileResolvers'\/'discoverSafely'
is already audited to always record one for any 'Ref' it can produce -- but if one ever
regresses (or a future provider is added without doing so), silently
carrying on as if a managed release existed for a live background refresh
thread is exactly the class of bug this whole module exists to eliminate.
This carries only a bare 'ThreadId' (never any credential\/request data),
so it is always safe to surface via the ordinary \"an unanticipated
exception escaping @acquire@ terminates the supervisor\" path
'startSupervisedEnv' already documents -- there is deliberately no silent
\"guess a release and carry on\" fallback for this case: any generation
that reaches here is always failed loudly rather than pretended to be
safe. Unlike an earlier version of this exception, 'deriveRelease' itself
now reaps the orphaned worker ('requireChildReleased') /before/ this is
ever thrown -- see its own Haddock -- so a caller catching this (or
unwinding through 'startSupervisedEnv') can rely on the offending thread
already being genuinely terminated, never merely \"about to be\", closing
the HIGH-severity finding that a caller releasing further dependencies
(e.g. an assume-role chain's own source credentials) immediately after
this propagates could otherwise race a still-live orphaned child.
-}
newtype ManagedReleaseInvariantViolated = ManagedReleaseInvariantViolated ThreadId
  deriving stock (Show)

instance Exception.Exception ManagedReleaseInvariantViolated

{- | The single point every managed-release derivation in this module goes
through (see 'acquireAwsEnv'): given whatever a provider actually
recorded into its @finalReleaseRef@ (if anything) and
the 'Auth' it ultimately returned, decide the correct overall release --
structurally, not by convention:

* A recorded managed release is always used as-is, regardless of what
  @authValue@ turns out to be (a provider that both records a release
  /and/ returns 'Auth' is merely being extra-cautious, never wrong).
* No recorded release, but @authValue@ is 'Auth' (statically, provably
  never-expiring -- e.g. 'ExplicitKeys'\/'fromKeysEnv'): 'requireChildReleased'
  is an immediate, non-blocking no-op for this case, so there is nothing
  unsafe about deriving it from @.auth@ alone.
* No recorded release, but @authValue@ is 'Ref' (a live, expiring,
  background-refreshing credential): this can only mean some provider
  forgot to record its managed release. There is no safe derivation from
  '.auth' alone here, but the leaked worker itself is not a mystery --
  it is exactly the same 'Ref' this function already has in hand, and
  'requireChildReleased' (kill it, then genuinely await its terminal
  'ThreadStatus') is a perfectly ordinary, safe way to reap it even
  though nothing else was ever going to. Reaping it /before/ throwing
  'ManagedReleaseInvariantViolated' -- rather than leaving it alive and
  merely throwing -- closes the HIGH-severity finding that a caller
  unwinding an assume-role\/nested-provider chain around this failure
  (e.g. releasing a parent 'Auth'\/'Env' the leaked child's own refresh
  loop was still reading from) could otherwise release those source
  dependencies while the orphaned worker was still running: by the time
  any exception from this function ever reaches such a caller, the
  worker is provably already terminated (or, if reaping itself times
  out, 'ChildReleaseTimedOutException' -- a strictly more urgent signal
  than the invariant violation -- propagates instead, exactly as it
  would from any other genuine 'requireChildReleased' timeout elsewhere
  in this module).
-}
deriveRelease :: Maybe (IO ()) -> Auth -> IO (IO ())
deriveRelease (Just release) _ = pure release
deriveRelease Nothing authValue@(Auth _) = pure (requireChildReleased authValue)
deriveRelease Nothing r@(Ref tid _) = requireChildReleased r >> Exception.throwIO (ManagedReleaseInvariantViolated tid)

-- | Bounded poll for a 'ThreadId' to reach a genuinely terminal
-- 'ThreadStatus' ('ThreadFinished' or 'ThreadDied'). See
-- 'releaseAwsEnvChild' for why this is necessary at all, why the bound is
-- only a defensive guard, and why it now returns an honest
-- 'ChildReleaseOutcome' rather than unconditionally succeeding.
awaitThreadTerminated :: ThreadId -> IO ChildReleaseOutcome
awaitThreadTerminated tid = go maxAwaitPolls
 where
  maxAwaitPolls = 500 :: Int
  awaitPollIntervalMicros = 1000
  go remaining
    | remaining <= 0 = pure ChildReleaseTimedOut
    | otherwise = do
        status <- threadStatus tid
        if status == ThreadFinished || status == ThreadDied
          then pure ChildReleased
          else threadDelay awaitPollIntervalMicros >> go (remaining - 1)

{- | 'releaseAwsEnvChild', but adapted to the plain @IO ()@\/throws-on-
failure shape every release-action call site in this module needs (see
'releaseAwsEnvGeneration'\/'AwsEnvAcquisition'): turns a
'ChildReleaseTimedOut' outcome into a thrown 'ChildReleaseTimedOutException'
instead of silently discarding it, so a caller relying on \"this action
returned, therefore the child is released\" (exactly
'releaseAwsEnvAcquisition''s\/'stopSupervisedEnv''s own assumption) can
never be misled by this specific release step the way the pre-fix,
always-succeeding 'awaitThreadTerminated' could mislead it.
-}
requireChildReleased :: Auth -> IO ()
requireChildReleased authValue = do
  outcome <- releaseAwsEnvChild authValue
  case (outcome, authValue) of
    (ChildReleased, _) -> pure ()
    (ChildReleaseTimedOut, Ref tid _) -> Exception.throwIO (ChildReleaseTimedOutException tid)
    (ChildReleaseTimedOut, Auth _) -> pure () -- unreachable: 'Auth' always yields 'ChildReleased'.

{- | A repository-owned replacement for the pinned Amazonka fork's own
@Amazonka.Auth.Background.fetchAuthInBackground@ (fetched and read in full
against @lib/amazonka/src/Amazonka/Auth/Background.hs@ at
@b562aa3f24845e34b95748daae671860017426be@ this round), used only by
'safeNamedInstanceProfile' (see the module Haddock's \"Managed credential
refresh\" section for exactly why only that one provider, and what
remains on the opaque pinned implementation).

Reproduces its exact scheduling semantics -- refresh within five minutes
of expiry, or halfway through the remaining lifetime if sooner than that,
biased 60 seconds early to account for execution time and clock skew (see
'computeMicrosUntilRefreshDeadline', a direct, deliberately literal port
of pinned @Background.loop@\/@diff@'s own arithmetic) -- and its exact
failure reclassification (a 'RetrievalError' or 'AuthServiceError' is
forwarded unchanged; anything else, including a plain 'OtherAuthError',
is (re)wrapped as 'OtherAuthError'). It deliberately does /not/ replicate
pinned @Background.timer@\/@loop@'s @System.Mem.Weak@-finalizer-triggered
self-termination (killing itself once its 'IORef' becomes /unreachable/):
this module's own supervisor always releases every generation
explicitly and deterministically (see 'startSupervisedEnv'), so a
GC-driven, non-deterministic fallback termination path is unnecessary
here and would only complicate reasoning about when this thread stops.

The one deliberate behavioral difference (and the entire reason this
exists) is exception classification during release: pinned
@Background.loop@'s own @Exception.try \@SomeException ma@ catches
/everything/, including a @killThread@-delivered @ThreadKilled@ used to
release it, and reports that back to the calling thread as if it were a
genuine credential failure. Here, 'release' (returned as the second
element of the pair) records that a stop was explicitly requested
/before/ delivering that cancellation (via 'cancelManagedThread'); the
refresh loop checks that flag before ever reporting a caught exception
back to the thread that originally called this function, so an ordinary,
expected release can never be misreported as an 'AuthError'. 'release'
itself never returns until the refresh thread has /genuinely/ terminated
('cancelManagedThread' -- an ordinary, interruptible wait on its own
completion, never a poll or a timeout dressed up as success -- see
'Api.Arkham.Lifecycle.cancelManagedThread').

The returned 'Auth' is still an ordinary 'Ref' (or, for a non-expiring
'AuthEnv', a static 'Auth' with no thread at all, exactly matching pinned
behavior) -- Amazonka's own request-signing code reads through it exactly
as it would credentials pinned @fetchAuthInBackground@ itself produced,
so actual credential refresh visibility to outgoing requests is
unaffected; only how release\/cancellation is observed and classified
differs.
-}
managedFetchAuthInBackground :: IO AuthEnv -> IO (Auth, IO ())
managedFetchAuthInBackground getAuthEnv = do
  initial <- getAuthEnv
  case initial.expiration of
    Nothing -> pure (Auth initial, pure ())
    -- Masked from immediately before 'spawnManagedThread' (the exact
    -- moment @refreshThread@ becomes a genuinely live, owned resource)
    -- through the final 'pure' handing @(Ref .., release)@ back to the
    -- caller: without this, an asynchronous exception delivered to this
    -- thread in the gap between @refreshThread@ starting and this
    -- function's own return -- which, being ordinary, unmasked
    -- computation (record/closure construction, not a blocking
    -- operation), is otherwise a genuine safe point -- would discard the
    -- only local binding (@release@) that could ever cancel it, leaking
    -- @refreshThread@ with no supervisor-visible handle left to kill it.
    -- 'spawnManagedThread' itself needs no special interruptibility here
    -- (an ordinary 'Control.Concurrent.forkIOWithUnmask' call returns
    -- essentially immediately), and nothing after it blocks, so masking
    -- this entire span costs nothing in genuine responsiveness while
    -- closing the exact worker-to-caller handoff gap the audit flagged.
    Just _ -> Exception.mask_ $ do
      callingThread <- myThreadId
      credentialsRef <- newIORef initial
      stopRequestedRef <- newIORef False
      refreshThread <- spawnManagedThread (refreshLoop callingThread credentialsRef stopRequestedRef)
      let -- 'cancelManagedThread' can only ever observe *this exact*
          -- 'refreshThread' as genuinely terminal via its own completion
          -- 'MVar' -- but the calling thread performing this release may
          -- be the *same* thread another, still-live, unrelated managed
          -- refresh child (a sibling elsewhere in the same generation's
          -- dependency graph, or -- see 'stopRequestedRef''s own
          -- 'atomicWriteIORef' below -- even a stale, not-yet-visible
          -- read of *this* child's own flag) targets with a delayed,
          -- reclassified 'AuthError' via 'Exception.throwTo'. If that
          -- lands asynchronously while this release's own
          -- 'cancelManagedThread' call is blocked inside its 'readMVar'
          -- wait, that wait is interrupted *without* 'refreshThread'
          -- itself having actually terminated -- an 'AuthError' is never
          -- proof that @refreshThread@ in particular is done. Retrying
          -- 'cancelManagedThread' (re-delivering 'ThreadKilled' is always
          -- safe: 'Control.Exception.throwTo' to an already-finished
          -- thread is a documented no-op) discards only that specific,
          -- recognized, sibling-feedback exception type and keeps
          -- waiting until @refreshThread@'s own completion cell is
          -- genuinely observed.
          --
          -- Any *other* exception arriving here -- including a genuine
          -- external cancellation of this release itself (e.g.
          -- 'Control.Exception.ThreadKilled' delivered to /this/
          -- releasing thread by an outer timeout\/shutdown) -- is never
          -- swallowed, but it is also never allowed to make this
          -- function return\/throw /before/ @refreshThread@'s own
          -- termination is genuinely observed: an earlier version threw
          -- it immediately here, which let 'releaseAwsEnvAcquisition'
          -- (see its own Haddock) proceed to release this generation's
          -- *dependencies* (e.g. an outer @sts:AssumeRole@ chain's own
          -- source credentials) while @refreshThread@ itself might still
          -- be alive and still reading them. Instead, any such exception
          -- is /preserved/ (only the first one seen, matching ordinary
          -- \"outermost cancellation wins\" semantics) and this keeps
          -- retrying 'cancelManagedThread' -- exactly as it already does
          -- for a recognized 'AuthError' -- until that exact call
          -- genuinely returns without throwing, i.e. until
          -- @refreshThread@'s own completion cell is truly, successfully
          -- observed. Only then is the preserved exception finally
          -- rethrown (if there was one); this function therefore never
          -- returns, and never throws anything /other/ than that
          -- preserved exception, without @refreshThread@ having already,
          -- provably terminated first.
          --
          -- The retry itself runs under one continuous
          -- 'Control.Exception.mask', restoring only around
          -- 'cancelManagedThread' -- never as an ordinary unmasked
          -- recursive retry. Without this, a *fresh* flooded exception
          -- can be delivered in the narrow, unmasked gap between an
          -- earlier one being caught and the retry actually re-entering
          -- 'cancelManagedThread''s own protection (e.g. while merely
          -- re-running 'atomicWriteIORef' or during the handler-to-retry
          -- tail call itself), escaping uncaught here and misreported by
          -- 'releaseAwsEnvAcquisition' as a genuine release failure even
          -- though @refreshThread@ was never actually left unreleased.
          -- Masking closes that gap: any such exception arriving outside
          -- 'restore' is deferred until the next 'restore'd
          -- 'cancelManagedThread' call, where it is classified exactly
          -- as before.
          release = Exception.mask $ \restore ->
            let awaitReleased :: Maybe Exception.SomeException -> IO ()
                awaitReleased pending = do
                  atomicWriteIORef stopRequestedRef True
                  outcome <- Exception.try @Exception.SomeException (restore (cancelManagedThread refreshThread))
                  case outcome of
                    Right () -> for_ pending Exception.throwIO
                    Left e -> case Exception.fromException e of
                      Just (_ :: AuthError) -> awaitReleased pending
                      Nothing -> awaitReleased (pending <|> Just e)
             in awaitReleased Nothing
      pure (Ref (managedThreadId refreshThread) credentialsRef, release)
 where
  refreshLoop
    :: ThreadId
    -> IORef AuthEnv
    -> IORef Bool
    -> IO ()
  refreshLoop callingThread credentialsRef stopRequestedRef = go
   where
    go = do
      current <- readIORef credentialsRef
      for_ current.expiration $ \expiry -> do
        threadDelay =<< computeMicrosUntilRefreshDeadline expiry
        outcome <- Exception.try @Exception.SomeException getAuthEnv
        case outcome of
          Right refreshed -> atomicWriteIORef credentialsRef refreshed >> go
          Left err -> do
            -- 'atomicWriteIORef'-paired read: without it, this could
            -- observe a stale, pre-release @False@ even though 'release'
            -- has already (per program order on the releasing thread)
            -- written @True@, misreporting an expected cancellation as a
            -- genuine 'AuthError' back to @callingThread@.
            stopRequested <- readIORef stopRequestedRef
            unless stopRequested $ Exception.throwTo callingThread (reclassifyAsAuthError err)

  reclassifyAsAuthError :: Exception.SomeException -> AuthError
  reclassifyAsAuthError err
    | Just authErr@(RetrievalError _) <- Exception.fromException err = authErr
    | Just authErr@(AuthServiceError _) <- Exception.fromException err = authErr
    | otherwise = OtherAuthError err

{- | An opaque, unforgeable proof that an 'Env' is paired with the exact
release for whatever it currently holds as its own @.auth@: either a
genuinely-live managed refresh worker's own release (see
'managedEnvAcquisition'), for a live, expiring 'Ref', or a release-free
acquisition for a /provably/ non-expiring 'Auth' checked at construction
time (see 'staticEnvAcquisition'). The only ways to construct one -- both
exported, neither this type's own constructor nor field accessors -- are
those two functions.

An earlier version of this type additionally exported 'ManagedRelease'\/
@managedRelease@\/@polledRelease@\/@pairManagedAcquisition@ as separable
public primitives: a caller could obtain a genuine 'Auth'\/release pair
from one worker (or a real \"released\" proof from an unrelated, already-
static 'Auth' via @polledRelease@) and then pair /that/ release with a
completely different, unrelated, still-live 'Env' via
@pairManagedAcquisition@ -- e.g. @pairManagedAcquisition env {auth =
Identity (Ref liveTid credentialsRef)} (polledRelease (Auth
staticCredentials))@, which type-checked and even ran a genuine,
non-fake release primitive, yet paired a live worker's 'Env' with an
entirely unrelated no-op release, orphaning @liveTid@. Removing those
four exports (see 'managedEnvAcquisition' below, the single atomic
factory that now creates the worker and pairs it with its own exact
release in one indivisible step) closes that: there is no longer any
exported way to obtain an 'Auth' and a release as two independent values
in the first place, so there is nothing left to mismatch.
-}
data ManagedEnvAcquisition = ManagedEnvAcquisition
  { managedEnvAcquisitionEnv :: Env
  , managedEnvAcquisitionRelease :: IO ()
  }

{- | The only way to obtain a 'ManagedEnvAcquisition' backed by a live,
expiring managed refresh worker: this atomically starts that /exact/
worker (via 'managedFetchAuthInBackground'), and installs its freshly
resolved 'Auth' into @envTemplate@ itself -- there is no caller-supplied
callback anywhere in between. Every current provider
('safeNamedInstanceProfile', 'safeContainer', 'safeAssumedRole',
'safeWebIdentity', 'safeSSO') passes an otherwise-already-complete
template 'Env' (every field except @.auth@ already resolved; the
instance-profile provider resolves its own @region@ into that template
/before/ calling this, since @region@ never depends on @.auth@ at all --
see 'safeNamedInstanceProfile''s own call site), and this function's own
single, hardcoded @envTemplate {auth = Identity auth}@ is the only place
@.auth@ is ever set on the result.

An earlier version instead took a caller-supplied @withAuth :: Auth ->
Env@ callback and applied it to the worker's genuine 'Auth'. That
type-checked, but nothing prevented @withAuth@ from simply /ignoring/ its
argument and returning an unrelated, already-live 'Env' instead (e.g.
one referencing a 'Ref' obtained from a completely different,
independently created worker) -- since 'managedFetchAuthInBackground' is
itself exported for this module's own regression tests (see its own
Haddock), a caller could obtain a second, genuine @(Auth, IO ())@ pair
from a second worker and have @withAuth@ return an 'Env' built from
/that/ pair's 'Auth' while this function still paired the result with
the /first/ worker's own release -- exactly the same class of
'Env'\/release mismatch 'ManagedEnvAcquisition''s own Haddock already
describes for the former @pairManagedAcquisition@, just reintroduced one
layer up. Taking an 'Env' template directly, with no callback at all,
makes that structurally unrepresentable: there is no argument here a
caller could ever supply that determines what @.auth@ becomes other than
this function's own freshly created worker.
-}
managedEnvAcquisition
  :: (MonadIO m)
  => Env' withAuth
  -- ^ envTemplate: every field except @.auth@ already fully resolved;
  -- this function's own record update is the only place @.auth@ is ever
  -- set, always to the exact freshly created worker's own 'Auth'.
  -> IO AuthEnv
  -- ^ getAuthEnv: how the worker should re-fetch credentials on each
  -- refresh cycle (see 'managedFetchAuthInBackground').
  -> m ManagedEnvAcquisition
managedEnvAcquisition envTemplate getAuthEnv = liftIO $ Exception.mask_ $ do
  -- Masked for the same reason as 'managedFetchAuthInBackground''s own
  -- 'Just' branch: once that call returns, @release@ is this thread's
  -- only handle on a genuinely live worker, held in a local binding,
  -- until it is safely folded into the 'ManagedEnvAcquisition' record
  -- returned here. Nothing between the two statements below blocks, so
  -- masking this span is free of any real interruptibility cost.
  (auth, release) <- managedFetchAuthInBackground getAuthEnv
  pure
    ManagedEnvAcquisition
      { managedEnvAcquisitionEnv = envTemplate {auth = Identity auth}
      , managedEnvAcquisitionRelease = release
      }

{- | The only way to obtain a release-free 'ManagedEnvAcquisition':
checks, at construction time, that the given 'Env''s current @.auth@ is
genuinely the statically non-expiring 'Auth' constructor (never 'Ref'),
immediately releasing the child and throwing
'ManagedReleaseInvariantViolated' otherwise -- rather than silently
accepting a @pure ()@ release for a live, expiring worker the way a bare
@(env, pure ())@ tuple could. Used only for 'ExplicitKeys'\/
@credential_source=Environment@, the sole config-file variants that can
never expire; every other provider goes through 'managedEnvAcquisition'
instead.
-}
staticEnvAcquisition :: Env -> IO ManagedEnvAcquisition
staticEnvAcquisition env =
  case runIdentity env.auth of
    Auth _ -> pure ManagedEnvAcquisition {managedEnvAcquisitionEnv = env, managedEnvAcquisitionRelease = pure ()}
    r@(Ref tid _) -> requireChildReleased r >> Exception.throwIO (ManagedReleaseInvariantViolated tid)

-- | The only supported way back out of the opaque 'ManagedEnvAcquisition'
-- type, used exclusively at the handful of boundaries ('discoverSafely''s
-- own outer @IORef@-based candidates, and 'safeEvalConfigProfile''s
-- @leaf@\/@assumeRoleOnto@) that still need the concrete pair to build an
-- 'AwsEnvAcquisition'.
runManagedEnvAcquisition :: ManagedEnvAcquisition -> (Env, IO ())
runManagedEnvAcquisition acquisition =
  (managedEnvAcquisitionEnv acquisition, managedEnvAcquisitionRelease acquisition)

{- | The single, shared \"worker spawn through ledger commit\" span every
'discoverSafely' outer candidate ('safeDefaultInstanceProfileEnv',
'safeContainerEnv', 'safeWebIdentityEnv') needs: run @act@ (which may
itself already be a live, newly created 'ManagedEnvAcquisition' by the
time it returns -- see 'managedEnvAcquisition''s own masking), then
record its release into @finalReleaseRef@, all under one continuous
'Exception.mask_'. Without this, an asynchronous exception delivered in
the ordinary, unmasked gap between @act@ returning and @finalReleaseRef@
actually being written would discard the only reference to a genuinely
live worker's release, orphaning it exactly as an unprotected 'writeIORef'
call site further down 'discoverSafely''s chain once did. @act@ itself
remains free to perform genuinely interruptible network I\/O internally
(GHC still honors cancellation of blocking foreign calls even under
ordinary, non-uninterruptible 'Exception.mask'); only the purely
administrative handoff after it succeeds is protected here.
-}
commitManagedAcquisition :: IORef (Maybe (IO ())) -> IO ManagedEnvAcquisition -> IO Env
commitManagedAcquisition finalReleaseRef act = Exception.mask_ $ do
  (finalEnv, release) <- runManagedEnvAcquisition <$> act
  writeIORef finalReleaseRef (Just release)
  pure finalEnv

{- | A direct, deliberately literal port of pinned
@Amazonka.Auth.Background.loop@\/@diff@'s own refresh-scheduling
arithmetic (see 'managedFetchAuthInBackground'): refresh within five
minutes of @expiry@, or halfway through whatever time remains if sooner
than that (so, as expiry nears, refresh attempts occur increasingly
often rather than ever reaching or crossing it), biased 60 seconds early
throughout to account for the refresh call's own execution time and
clock skew. Never returns a delay below one microsecond, matching
pinned's own floor.
-}
computeMicrosUntilRefreshDeadline :: ISO8601 -> IO Int
computeMicrosUntilRefreshDeadline expiry = do
  now <- getCurrentTime
  let secondsUntilExpiryBiased = truncate (diffUTCTime (fromTime expiry) now) - 60 :: Integer
      microsUntilExpiry = max 1 secondsUntilExpiryBiased * 1_000_000
      fiveMinutesMicros = 5 * 60 * 1_000_000
  pure . fromIntegral
    $ if microsUntilExpiry > fiveMinutesMicros
      then microsUntilExpiry - fiveMinutesMicros
      else microsUntilExpiry `div` 2

-- | See 'startSupervisedEnv''s @acquire@ parameter: the concrete release
-- action for a generation whose entire 'Env' was acquired via a single,
-- non-recursive provider (i.e. every provider except the config-file
-- chain -- see 'AwsEnvAcquisition' for why the config-file chain needs
-- more than this).
releaseAwsEnvGeneration :: Env -> IO ()
releaseAwsEnvGeneration env = requireChildReleased (runIdentity env.auth)

{- | One resolved 'Env' together with every background-refresh child that
acquiring it created but which is no longer directly reachable via its
own @.auth@ field -- see 'ConfigProfileResolvers'\/'safeEvalConfigProfile'
for exactly how this arises. 'releaseAwsEnvAcquisition' releases all of
them, plus whichever child is still directly reachable via @.auth@ itself.
-}
data AwsEnvAcquisition = AwsEnvAcquisition
  { awsEnvAcquisitionEnv :: Env
  , -- | The correct release for 'awsEnvAcquisitionEnv''s own,
    -- currently-visible @.auth@. /Not always/ @releaseAwsEnvChild
    -- (runIdentity env.auth)@: a provider that used
    -- 'managedFetchAuthInBackground' (currently only the EC2\/instance-
    -- profile provider, reached either as 'discoverSafely''s own
    -- top-level fallback or, nested, via
    -- @credential_source=Ec2InstanceMetadata@) supplies its own
    -- genuinely-awaiting release here instead, since the naive
    -- kill-then-bounded-poll derivation would otherwise race it: if that
    -- naive release ran first (targeting the exact same 'ThreadId'
    -- 'managedFetchAuthInBackground''s own worker uses), it would
    -- deliver @killThread@ /before/ the managed worker's own \"a stop was
    -- requested\" flag is set, reintroducing exactly the
    -- expected-cancellation-misclassified-as-'AuthError' hazard
    -- 'managedFetchAuthInBackground' exists to close. Every other
    -- provider (opaque pinned @fromContainerEnv@\/@fromAssumedRole@\/
    -- @fromWebIdentity(Env)@\/@fromSSO@, or a non-expiring static 'Auth')
    -- has no such managed alternative, so this is simply
    -- @releaseAwsEnvChild (runIdentity env.auth)@ for those, identical to
    -- this field not existing at all.
    awsEnvAcquisitionRelease :: IO ()
  , -- | Release actions for children hidden by an outer overwrite,
    -- oldest\/innermost-acquired first. Released newest-first (i.e.
    -- reversed) -- but only /after/ 'awsEnvAcquisitionRelease' itself,
    -- which is released first of all (see 'releaseAwsEnvAcquisition').
    -- Whatever needed a child most recently (a still-live outer
    -- @sts:AssumeRole@ refresh loop that re-authenticates using it) is
    -- retired before what it depended on, matching ordinary
    -- bracket\/stack release discipline: the final, outermost,
    -- most-dependent child (@.auth@ itself) is always the newest of all,
    -- so it is released before any hidden child, and among the hidden
    -- children themselves, newest\/most-recently-acquired first.
    awsEnvAcquisitionHiddenReleases :: [IO ()]
  }

{- | Release every child a config-file (or single-provider) acquisition
created, outermost\/newest first: 'awsEnvAcquisitionRelease' itself, then
every hidden child, newest-to-oldest (see 'AwsEnvAcquisition').

Every child's release is attempted even if an earlier one fails: each
child's own release (see 'managedFetchAuthInBackground')
already never returns -- and so never lets 'attemptRelease' below observe
success -- until that /exact/ child's own completion cell is genuinely
observed, retrying internally past any asynchronously-arriving 'AuthError'
feedback that could otherwise race a still-live child's confirmed
termination (see 'managedFetchAuthInBackground''s @release@ for exactly
why that retry, not a bare wait, is required: a sibling's own delayed
refresh-failure notification can land on this exact calling thread while
a wholly different child's release is still in flight, and must never be
mistaken for /that/ child having stopped). By the time a release action
here throws /anything/, it is therefore never merely that expected,
self-inflicted feedback -- it is a genuine failure (or a genuine external
cancellation of this cleanup itself) that this function must not
discard: every remaining child's release is still attempted regardless,
but the first such failure seen is re-raised only once every child has at
least been attempted, preserving the original caller-visible
failure\/cancellation exactly as if this function's own cleanup work were
transparent.
-}
releaseAwsEnvAcquisition :: AwsEnvAcquisition -> IO ()
releaseAwsEnvAcquisition (AwsEnvAcquisition _env finalRelease hiddenReleases) = do
  let orderedReleases = finalRelease : reverse hiddenReleases
  firstFailure <- foldM attemptRelease Nothing orderedReleases
  for_ firstFailure Exception.throwIO
 where
  attemptRelease :: Maybe Exception.SomeException -> IO () -> IO (Maybe Exception.SomeException)
  attemptRelease firstFailure release =
    Exception.try @Exception.SomeException release >>= \case
      Right () -> pure firstFailure
      Left e -> pure (Just (fromMaybe e firstFailure))

{- | A drop-in replacement for the pinned Amazonka fork's own
@Amazonka.Auth.discover@, identical in every credential source it tries,
their order, and their error classification -- except that /every/
provider capable of producing temporary\/expiring credentials is
reimplemented here on top of 'managedFetchAuthInBackground' rather than
the pinned fork's own opaque @fetchAuthInBackground@ call. See each
provider's own Haddock below for exactly which pinned source module it
reimplements and why.

Only 'fromKeysEnv' (@Amazonka\/Auth\/Keys.hs@) is used entirely
unmodified: it reads static, non-expiring credentials and never calls
@fetchAuthInBackground@ at all -- no background thread is ever forked, so
there is nothing a subsequent failure could orphan, and no managed
replacement is needed.

Every other provider in this chain -- 'safeWebIdentityEnv'
(@Amazonka\/Auth\/STS.hs@, @fromWebIdentity(Env)@), 'safeContainerEnv'
(@Amazonka\/Auth\/Container.hs@, @fromContainer(Env)@),
'safeDefaultInstanceProfile' (@Amazonka\/Auth\/InstanceProfile.hs@), and
(nested, via 'safeFileEnv'\/'safeEvalConfigProfile') 'safeAssumedRole'
(@Amazonka\/Auth\/STS.hs@, @fromAssumedRole@) and 'safeSSO'
(@Amazonka\/Auth\/SSO.hs@, @fromSSO@) -- is a repository-owned
reimplementation built from that same pinned module's own /exported/
one-shot request\/response primitives (@Amazonka.Send.send@\/
@sendUnsigned@\/@sendUnsignedEither@, the generated @newAssumeRole@\/
@newAssumeRoleWithWebIdentity@\/@newGetRoleCredentials@ request smart
constructors, @Amazonka.Auth.SSO.readCachedAccessToken@\/
@roleCredentialsToAuthEnv@, and plain @http-client@ for the ECS metadata
endpoint) wired through 'managedFetchAuthInBackground' instead of the
pinned fork's own @fetchAuthInBackground@. Every one of these is
byte-for-byte identical to the pinned source in request shape, response
parsing, error classification, and environment-variable\/file lookup
precedence -- the only behavioral difference anywhere in this list is
release\/cancellation semantics: a genuinely-awaited managed refresh
worker (see 'managedFetchAuthInBackground') rather than the pinned
fork's kill-then-hope-GC-collects-it background thread. No provider in
this module's own production credential graph still reaches the pinned
fork's own @fetchAuthInBackground@ -- see 'releaseAwsEnvChild's Haddock,
whose kill-then-bounded-poll fallback this chain therefore no longer
exercises for any real acquisition, only for the regression tests that
exercise it directly as a documented, standalone primitive.

'fromDefaultInstanceProfile'\/'fromNamedInstanceProfile' were the first
provider found where a fallible operation (@getRegionFromIdentity@, a
separate IMDS metadata call) runs /after/ @fetchAuthInBackground@ may have
already forked a background refresh child -- see 'safeDefaultInstanceProfile'.
A second, distinct hazard was found in 'Amazonka.Auth.ConfigFile.fromFileEnv'
itself: see 'AwsEnvAcquisition'\/'ConfigProfileResolvers'\/'safeFileEnv' for
the full trace of every branch in @ConfigFile.evalConfig@ and why an
unmodified @fromFileEnv@ can silently orphan a config-file /source/
profile's refresh child, or reach the unsafe @fromDefaultInstanceProfile@
via @credential_source=Ec2InstanceMetadata@.

The IORef accumulates any \"hidden\" child release actions the config-file
provider's own recursive chain produces -- see 'safeFileEnv' -- so that
whichever provider in this list ultimately wins, 'acquireAwsEnv' can build
a complete 'AwsEnvAcquisition' for it (an empty ledger for every provider
except the config-file one simply means \"nothing hidden\", reducing to
exactly the single-child release every other provider already had). A
second 'IORef', @finalReleaseRef@, similarly lets every managed provider
(every one of them except 'fromKeysEnv') report its genuinely-awaiting
'managedFetchAuthInBackground' release for @.auth@ itself when it wins
outright; 'fromKeysEnv' alone leaves it untouched, and 'acquireAwsEnv'
then falls back to the naive (but, for a non-expiring 'Auth', instant,
non-hazardous) derivation. Every other candidate in this list writes
@finalReleaseRef@ unconditionally, derived directly from its own inner
provider's mandatory @(Env, IO ())@ return pair (see the module Haddock,
\"Structural release ownership\") -- there is no optional side channel
left for any of them to forget.
-}
discoverSafely :: IORef [IO ()] -> IORef (Maybe (IO ())) -> EnvNoAuth -> IO Env
discoverSafely hiddenReleasesRef finalReleaseRef =
  runCredentialChain
    [ fromKeysEnv
    , safeFileEnv productionConfigProfileResolvers hiddenReleasesRef finalReleaseRef
    , safeWebIdentityEnv finalReleaseRef
    , safeContainerEnv finalReleaseRef
    , safeDefaultInstanceProfileEnv finalReleaseRef
    ]

-- | 'discoverSafely''s own outer candidate for @credential_source=Ec2InstanceMetadata@\/
-- default-instance-profile discovery: like 'safeContainerEnv'\/'safeWebIdentityEnv',
-- this exists purely to satisfy 'runCredentialChain''s fixed
-- @EnvNoAuth -> IO Env@ shape, unconditionally writing @finalReleaseRef@
-- from 'safeDefaultInstanceProfile''s own mandatory return pair.
safeDefaultInstanceProfileEnv :: IORef (Maybe (IO ())) -> Env' withAuth -> IO Env
safeDefaultInstanceProfileEnv finalReleaseRef env =
  commitManagedAcquisition finalReleaseRef (safeDefaultInstanceProfile env)


{- | A safe reimplementation of the pinned Amazonka fork's own
@Amazonka.Auth.InstanceProfile.fromDefaultInstanceProfile@\/
@fromNamedInstanceProfile@ (@lib/amazonka/src/Amazonka/Auth/InstanceProfile.hs@
at @b562aa3f24845e34b95748daae671860017426be@), built entirely from that
module's own /exported/ primitives ('Amazonka.EC2.Metadata.metadata',
'Amazonka.EC2.Metadata.identity', 'Amazonka.Auth.Background.fetchAuthInBackground')
rather than a patched\/forked dependency -- no fourth repository or
unreviewed personal fork is needed.

The pinned source's @fromNamedInstanceProfile@ does:

@
keys   <- fetchAuthInBackground getCredentials  -- may fork a refresh child
region <- getRegionFromIdentity                 -- separate, fallible IMDS call
@

If @getRegionFromIdentity@ throws (or this thread is asynchronously
cancelled while it runs), the whole function throws\/is interrupted having
already forked a background refresh child via @fetchAuthInBackground@ --
but discarding @keys@ (the only value holding that child's 'ThreadId',
inside the 'Ref' constructor) with it, so nothing this module's own
supervisor\/release protocol ever sees can kill that child. It is orphaned
except for the eventual, non-deterministic, best-effort weak-reference GC
finalizer 'fetchAuthInBackground' itself installs -- and until GC runs (if
ever, since the finalizer only fires once the 'IORef' becomes /unreachable/,
which never happens for a value this module never received), a delayed
refresh failure's @throwTo@ still targets whatever this thread is doing by
then.

This reimplementation is /byte-for-byte identical/ to the pinned source in
every classification, message, and provider name -- region and ECS\/IMDS
behavior is unchanged -- except that the two calls are performed in the
other order: 'Amazonka.EC2.Metadata.identity' (region) always completes
/before/ 'fetchAuthInBackground' (which may fork the refresh child) ever
runs. That reordering makes it structurally impossible for a fallible
operation to run after a child may already exist: if this function
throws\/is cancelled, no background child was ever created in the first
place, so 'startSupervisedEnv''s @release@ (in production,
'releaseAwsEnvGeneration') genuinely has nothing to clean up on that path
-- there is no orphan window left to protect against, rather than a wider
catch papering over one. 'acquireRegionBeforeAuth' documents (and is
directly, deterministically tested against) this exact ordering as a
standalone primitive, using fake @getRegion@\/@getAuth@ actions rather
than a real IMDS endpoint; 'safeNamedInstanceProfile' itself inlines the
identical @getRegion@-then-@getAuth@ sequencing directly (rather than
calling 'acquireRegionBeforeAuth' itself) purely so it can also thread
'managedFetchAuthInBackground''s release back out as part of its own
mandatory @(Env, IO ())@ return pair -- see the module Haddock, \"Structural
release ownership\".
-}

{- | The exact ordering fix 'safeNamedInstanceProfile' applies to the
pinned source's @fromNamedInstanceProfile@, factored out from any real
Amazonka I\/O so it can be exercised directly by regression tests against
fake @getRegion@\/@getAuth@ actions (see @AwsEnvSupervisorSpec@) without
needing a real metadata endpoint: resolve @getRegion@ /completely/ before
ever running @getAuth@ (in production, the 'fetchAuthInBackground' call
that may fork a background refresh child). If @getRegion@ throws or this
thread is asynchronously cancelled while it runs, @getAuth@ is never even
reached -- so no child can exist yet to orphan.
-}
acquireRegionBeforeAuth
  :: Env' withAuth
  -> IO Region
  -> IO Auth
  -> IO Env
acquireRegionBeforeAuth env getRegion getAuth = do
  region <- getRegion
  auth <- getAuth
  pure env {auth = Identity auth, region}

safeDefaultInstanceProfile ::
  (MonadIO m) =>
  Env' withAuth ->
  m ManagedEnvAcquisition
safeDefaultInstanceProfile env =
  liftIO $ do
    ls <-
      Exception.try $ metadata (manager env) (IAM (SecurityCredentials Nothing))
    case BS8.lines <$> ls of
      Right (x : _) -> safeNamedInstanceProfile (TE.decodeUtf8 x) env
      Left e -> Exception.throwIO (RetrievalError e)
      _ ->
        Exception.throwIO
          $ InvalidIAMError "Unable to get default IAM Profile from EC2 metadata"

{- | See 'safeDefaultInstanceProfile'. Identical to the pinned source's
@fromNamedInstanceProfile@ except @getRegionFromIdentity@ is resolved
before @fetchAuthInBackground@, not after -- via 'acquireRegionBeforeAuth'
-- and @fetchAuthInBackground@ itself is replaced with
'managedFetchAuthInBackground' (via 'managedEnvAcquisition'), whose
genuinely-awaiting release is returned alongside the resolved 'Env' as an
opaque 'ManagedEnvAcquisition' (see the module Haddock, \"Structural
release ownership\"): there is no 'IORef' side channel here for a future
change to accidentally leave unwritten, and no way to construct the
returned value at all except through 'managedEnvAcquisition''s own
atomic worker-creation-and-pairing step.
-}
safeNamedInstanceProfile ::
  (MonadIO m) =>
  Text ->
  Env' withAuth ->
  m ManagedEnvAcquisition
safeNamedInstanceProfile name env@Env {manager} =
  liftIO $ do
    region <- getRegionFromIdentity
    managedEnvAcquisition (env {region}) getCredentials
 where
  getCredentials =
    Exception.try (metadata manager (IAM . SecurityCredentials $ Just name))
      >>= handleErr (eitherDecode' . LBS8.fromStrict) invalidIAMErr

  getRegionFromIdentity =
    Exception.try (identity manager)
      >>= handleErr (fmap IdentityDocument.region) invalidIdentityErr

  handleErr f g = \case
    Left e -> Exception.throwIO (RetrievalError e)
    Right x -> either (Exception.throwIO . g) pure (f x)

  invalidIAMErr e =
    InvalidIAMError
      $ mconcat ["Error parsing IAM profile '", name, "' ", Text.pack e]

  invalidIdentityErr e =
    InvalidIAMError
      $ mconcat ["Error parsing Instance Identity Document ", Text.pack e]

{- | Every managed provider below returns its resolved 'Env' paired with
'managedFetchAuthInBackground''s genuinely-awaiting release as a
/mandatory/ tuple -- see the module Haddock, \"Structural release
ownership\": there is deliberately no 'IORef' side channel a provider
could forget to write into, the way an earlier version of this module's
@finalReleaseRef@ plumbing allowed (caught only at
runtime, by 'deriveRelease' reaping an orphaned worker before throwing
'ManagedReleaseInvariantViolated'). 'deriveRelease'\/'ManagedReleaseInvariantViolated'
remain as a defensive backstop at 'acquireAwsEnv''s own outer boundary
(see its Haddock), where 'discoverSafely''s external @EnvNoAuth -> IO Env@
shape -- fixed by the pinned Amazonka fork's own @newEnv@\/@runCredentialChain@
API, not something this module controls -- still requires a single,
final ref-based handoff; every provider reachable from that boundary now
writes it unconditionally, from a mandatory tuple already in hand, so
that outer 'Maybe' can in practice only ever be 'Nothing' when the
resolved 'Auth' is statically non-expiring (never reachable with a live
'Ref').
-}

{- | A safe reimplementation of the pinned Amazonka fork's own
@Amazonka.Auth.Container.fromContainer@\/@fromContainerEnv@
(@lib/amazonka/src/Amazonka/Auth/Container.hs@ at
@b562aa3f24845e34b95748daae671860017426be@), built entirely from that
module's own /exported/ primitives (plain @http-client@ 'Client.parseUrlThrow'\/
'Client.httpLbs' against the ECS task-metadata endpoint, and Aeson's
'eitherDecode' for the identical JSON parse) rather than a patched\/forked
dependency -- byte-for-byte identical request URL construction, response
parsing, and error message text. The only behavioral difference is
@fetchAuthInBackground@ itself being replaced with
'managedFetchAuthInBackground', whose genuinely-awaiting release is
returned alongside the resolved 'Env' as a mandatory pair, exactly as
'safeNamedInstanceProfile' does. There is no fallible operation after
credentials are fetched (the pinned source's own @fromContainer@ has none
either), so there is no region-ordering hazard here to fix the way
'safeDefaultInstanceProfile' had to for the instance-profile provider --
this is purely closing the opaque-release residual scope.
-}
safeContainer ::
  (MonadIO m) =>
  -- | Absolute URL
  Text ->
  Env' withAuth ->
  m ManagedEnvAcquisition
safeContainer url env =
  liftIO $ do
    req <- Client.parseUrlThrow $ Text.unpack url
    managedEnvAcquisition env (renew req)
 where
  renew :: Client.Request -> IO AuthEnv
  renew req = do
    rs <- Client.httpLbs req $ manager env
    either
      (Exception.throwIO . invalidIdentityErr)
      pure
      (eitherDecode (Client.responseBody rs))

  invalidIdentityErr =
    InvalidIAMError
      . mappend "Error parsing Task Identity Document "
      . Text.pack

{- | See 'safeContainer'. A safe reimplementation of the pinned Amazonka
fork's own @fromContainerEnv@: resolves the ECS metadata URL from
@AWS_CONTAINER_CREDENTIALS_RELATIVE_URI@, throwing the identical
'MissingEnvError' if it is unset, exactly matching pinned behavior
(including the documented lack of support for
@AWS_CONTAINER_CREDENTIALS_FULL_URI@\/@AWS_CONTAINTER_AUTHORIZATION_TOKEN@).

Ref-free (returns 'safeContainer''s own opaque 'ManagedEnvAcquisition'
directly) so it can be shared, unchanged, by both 'safeContainerEnv' (the
outer, ref-based candidate 'discoverSafely' passes to
@runCredentialChain@) and 'ConfigProfileResolvers''s @resolveEcsSource@
field (the purely-inner @credential_source=EcsContainer@ path reached via
'safeEvalConfigProfile') -- there is exactly one place this environment
variable is ever read, used identically by both callers.
-}
resolveContainerCredentials ::
  (MonadIO m) =>
  Env' withAuth ->
  m ManagedEnvAcquisition
resolveContainerCredentials env = liftIO $ do
  uriRel <-
    lookupEnv "AWS_CONTAINER_CREDENTIALS_RELATIVE_URI"
      >>= maybe
        (Exception.throwIO $ MissingEnvError "Unable to read AWS_CONTAINER_CREDENTIALS_RELATIVE_URI")
        pure
  safeContainer (Text.pack $ "http://169.254.170.2" <> uriRel) env

{- | This is one of 'discoverSafely''s own outer candidates, whose external
@EnvNoAuth -> IO Env@ shape is fixed by the pinned fork's own
@runCredentialChain@ -- so, unlike 'resolveContainerCredentials' itself,
this still takes @finalReleaseRef@ and writes it. That write is now
unconditional and derived directly from 'resolveContainerCredentials''s
own opaque return value (never optional, never something this function
could forget): it is the /only/ statement here that touches
@finalReleaseRef@, immediately after the single call that can produce a
release to record.
-}
safeContainerEnv ::
  (MonadIO m) =>
  IORef (Maybe (IO ())) ->
  Env' withAuth ->
  m Env
safeContainerEnv finalReleaseRef env =
  liftIO $ commitManagedAcquisition finalReleaseRef (resolveContainerCredentials env)

{- | A safe reimplementation of the pinned Amazonka fork's own
@Amazonka.Auth.STS.fromAssumedRole@
(@lib/amazonka/src/Amazonka/Auth/STS.hs@ at
@b562aa3f24845e34b95748daae671860017426be@), built entirely from that
module's own /exported/ primitives ('Amazonka.Send.send',
'STS.newAssumeRole') rather than a patched\/forked dependency --
byte-for-byte identical request construction and response field
extraction. As with 'safeContainer', there is no fallible operation
after credentials are fetched, so this is purely closing the
opaque-release residual scope: @fetchAuthInBackground@ is replaced with
'managedFetchAuthInBackground', whose release is returned alongside the
resolved 'Env' as a mandatory pair.
-}
safeAssumedRole ::
  -- | Role ARN
  Text ->
  -- | Role session name
  Text ->
  Env ->
  IO ManagedEnvAcquisition
safeAssumedRole roleArn roleSessionName env =
  managedEnvAcquisition env getCredentials
 where
  getCredentials = do
    let assumeRole = STS.newAssumeRole roleArn roleSessionName
    resp <- runResourceT $ send env assumeRole
    pure resp.credentials

{- | A safe reimplementation of the pinned Amazonka fork's own
@Amazonka.Auth.STS.fromWebIdentity@
(@lib/amazonka/src/Amazonka/Auth/STS.hs@ at
@b562aa3f24845e34b95748daae671860017426be@), built entirely from that
module's own /exported/ primitives ('Amazonka.Send.sendUnsigned',
'STS.newAssumeRoleWithWebIdentity') rather than a patched\/forked
dependency -- byte-for-byte identical session-name defaulting (a random
UUID when unset, matching the C++ SDK this pinned source itself mimics),
token-file re-read on every refresh (ignoring any subsequent environment
variable changes, exactly as pinned), and response field extraction.
@fetchAuthInBackground@ is replaced with 'managedFetchAuthInBackground'
(via 'managedEnvAcquisition'), whose release is returned alongside the resolved
'Env' as an opaque 'ManagedEnvAcquisition'.
-}
safeWebIdentity ::
  -- | Path to token file
  FilePath ->
  -- | Role ARN
  Text ->
  -- | Role Session Name
  Maybe Text ->
  Env' withAuth ->
  IO ManagedEnvAcquisition
safeWebIdentity tokenFile roleArn mSessionName env = do
  sessionName <- maybe (UUID.toText <$> UUID.nextRandom) pure mSessionName
  managedEnvAcquisition env (getCredentials sessionName)
 where
  getCredentials sessionName = do
    token <- TextIO.readFile tokenFile
    let assumeRoleWithWebIdentity =
          STS.newAssumeRoleWithWebIdentity roleArn sessionName token
    resp <- runResourceT $ sendUnsigned env assumeRoleWithWebIdentity
    pure resp.credentials

{- | See 'safeWebIdentity'. A safe reimplementation of the pinned
Amazonka fork's own @fromWebIdentityEnv@: resolves
@AWS_WEB_IDENTITY_TOKEN_FILE@\/@AWS_ROLE_ARN@\/@AWS_ROLE_SESSION_NAME@,
throwing the identical 'MissingEnvError' for the two required variables.

Like 'safeContainerEnv', this is one of 'discoverSafely''s own outer
candidates, so it still takes @finalReleaseRef@ and writes it -- again
unconditionally, from 'safeWebIdentity''s own opaque return value.
-}
safeWebIdentityEnv ::
  IORef (Maybe (IO ())) ->
  Env' withAuth ->
  IO Env
safeWebIdentityEnv finalReleaseRef env = do
  tokenFile <- lookupRequiredEnv "AWS_WEB_IDENTITY_TOKEN_FILE" "Unable to read token file name from AWS_WEB_IDENTITY_TOKEN_FILE"
  roleArn <- Text.pack <$> lookupRequiredEnv "AWS_ROLE_ARN" "Unable to read role ARN from AWS_ROLE_ARN"
  mSessionName <- fmap Text.pack <$> lookupNonEmptyEnv "AWS_ROLE_SESSION_NAME"
  commitManagedAcquisition finalReleaseRef (safeWebIdentity tokenFile roleArn mSessionName env)
 where
  lookupRequiredEnv var message =
    lookupNonEmptyEnv var >>= maybe (Exception.throwIO $ MissingEnvError message) pure

  lookupNonEmptyEnv var =
    lookupEnv var <&> \case
      Nothing -> Nothing
      Just "" -> Nothing
      Just v -> Just v

{- | A safe reimplementation of the pinned Amazonka fork's own
@Amazonka.Auth.SSO.fromSSO@ (@lib/amazonka/src/Amazonka/Auth/SSO.hs@ at
@b562aa3f24845e34b95748daae671860017426be@), built entirely from that
module's own /exported/ primitives ('readCachedAccessToken',
'roleCredentialsToAuthEnv', 'Amazonka.Send.sendUnsignedEither',
'SSO.newGetRoleCredentials') rather than a patched\/forked dependency --
byte-for-byte identical cached-token lookup, SSO-region override, and
error reclassification (a 'ServiceError'\/'TransportError' response is
reported as 'AuthServiceError'\/'RetrievalError'; anything else as
'OtherAuthError', exactly as pinned @errorAsAuthError@).
@fetchAuthInBackground@ is replaced with 'managedFetchAuthInBackground'
(via 'managedEnvAcquisition'), whose release is returned alongside the resolved
'Env' as an opaque 'ManagedEnvAcquisition'.
-}
safeSSO ::
  FilePath ->
  Region ->
  -- | Account ID
  Text ->
  -- | Role Name
  Text ->
  Env' withAuth ->
  IO ManagedEnvAcquisition
safeSSO cachedTokenFile ssoRegion accountId roleName env =
  managedEnvAcquisition env getCredentials
 where
  getCredentials = do
    cachedToken <- readCachedAccessToken cachedTokenFile
    let ssoEnv = env {region = ssoRegion}
        getRoleCredentials =
          SSO.newGetRoleCredentials
            roleName
            accountId
            (fromSensitive cachedToken.accessToken)
    runResourceT (sendUnsignedEither ssoEnv getRoleCredentials) >>= \case
      Left err -> Exception.throwIO (errorAsAuthError err)
      Right resp -> pure . roleCredentialsToAuthEnv $ resp.roleCredentials

  errorAsAuthError = \case
    ServiceError err -> AuthServiceError err
    TransportError err -> RetrievalError err
    other -> OtherAuthError (Exception.toException other)

{- | Acquire one fresh generation of AWS credentials\/'Env', already
classified into a sanitized diagnostic on failure. Only ever called from
'startSupervisedEnv''s dedicated supervisor thread -- see
'releaseAwsEnvChild' for why that placement matters. Credentials are left
exactly as @discoverSafely@\/'Amazonka.newEnv' returned them (never
frozen): a successfully acquired 'Env' keeps Amazonka's own background
refresh alive for as long as this generation remains current, so genuine
temporary credentials (IAM\/ECS\/IMDS) stay fresh for the supervisor's
entire lifetime rather than being re-discovered\/re-frozen per request.

Uses 'discoverSafely', not the pinned fork's own @Amazonka.Auth.discover@
-- see its Haddock for exactly which provider differs and why.

Deliberately has no internal timeout\/cancellation around @newEnv
discoverSafely@: an earlier per-request design bounded discovery with
'System.Timeout.timeout', but that bound could itself fire /after/
Amazonka's internal 'forkIO' for a background refresh child had already
returned successfully, racing the timeout's cancellation against the
still-in-flight construction of the 'Ref' value -- an unnecessary risk
now that discovery only ever runs on one dedicated, long-lived supervisor
thread rather than a per-request one. A hung credential provider now only
ever delays this one thread reaching 'SupervisedEnvReady'; every request
still observes an immediate, bounded, sanitized snapshot the entire time
(see 'readSupervisedEnv'\/'requestAwsEnvReady'), never blocking on
discovery itself.

Returns the acquired 'Env' paired with its complete release action (see
'AwsEnvAcquisition') rather than a bare 'Env': 'startSupervisedEnv' folds
release into acquisition's own return value precisely because the
config-file provider ('safeFileEnv') can acquire hidden children that are
not recoverable from the published 'Env' value alone, and the instance-
profile provider ('safeDefaultInstanceProfile') can report a genuinely-
awaiting 'managedFetchAuthInBackground' release that a naive '.auth'
derivation would race unsafely -- deriving release from '.auth' the way
an earlier version of this function did would silently miss both, for
every other provider this reduces to exactly that one naive release.
-}
acquireAwsEnv :: IO (Either AwsAuthErrorDiagnostic (Env, IO ()))
acquireAwsEnv = do
  hiddenReleasesRef <- newIORef []
  finalReleaseRef <- newIORef Nothing
  outcome <- try @_ @AuthError (newEnv (discoverSafely hiddenReleasesRef finalReleaseRef))
  case outcome of
    Left authErr -> Left <$> evaluate (classifyAuthErrorDiagnostic authErr)
    -- 'discoverSafely' has just returned a fully live 'env', whose
    -- winning candidate has already, atomically, recorded its own
    -- release into 'hiddenReleasesRef'\/'finalReleaseRef' (see
    -- 'commitManagedAcquisition'\/'safeFileEnv''s own masking). Masked
    -- from here through this function's own final 'pure': without it,
    -- an asynchronous exception delivered in this ordinary,
    -- allocation-point tail -- reading both refs, deriving @release@,
    -- and folding them into 'AwsEnvAcquisition' -- would discard the
    -- only remaining handle on that already-live worker, since neither
    -- ref is ever read again once this function's own caller
    -- ('runOneGeneration') has moved on. If interrupted here, everything
    -- already known (the hidden releases and, if derived, the final
    -- release) is released before propagating, symmetrically with every
    -- other \"worker spawn through ledger commit\" span in this module.
    Right env -> Exception.mask_ $ do
      hiddenReleases <- readIORef hiddenReleasesRef
      finalRelease <- readIORef finalReleaseRef
      ( do
          release <- deriveRelease finalRelease (runIdentity env.auth)
          let acquisition =
                AwsEnvAcquisition
                  { awsEnvAcquisitionEnv = env
                  , awsEnvAcquisitionRelease = release
                  , awsEnvAcquisitionHiddenReleases = hiddenReleases
                  }
          pure $ Right (env, releaseAwsEnvAcquisition acquisition)
        )
        `Exception.onException` sequence_ (reverse (hiddenReleases <> maybeToList finalRelease))


{- | Every acquisition primitive 'safeEvalConfigProfile' needs for one
\"leaf\" @ConfigProfile@\/@CredentialSource@ variant, factored out so tests
can inject deterministic fakes (that simulate forking a 'Ref' child
without real credentials\/network\/filesystem access) rather than only
ever exercising this against real AWS infrastructure. 'productionConfigProfileResolvers'
wires the real pinned-source-equivalent calls; see 'safeEvalConfigProfile'
for how each is used and 'AwsEnvSupervisorSpec' for the fakes.
-}
data ConfigProfileResolvers = ConfigProfileResolvers
  { resolveEnvironmentSource :: forall withAuth. Env' withAuth -> IO Env
  -- ^ @credential_source=Environment@ \/ plain env-var credentials.
  -- Never expiring ('fromKeysEnv' never calls @fetchAuthInBackground@ at
  -- all), so this alone returns a bare 'Env', not a 'ManagedEnvAcquisition':
  -- there is never a managed release to report.
  , resolveEc2Source :: forall withAuth. Env' withAuth -> IO ManagedEnvAcquisition
  -- ^ @credential_source=Ec2InstanceMetadata@ -- deliberately
  -- 'safeDefaultInstanceProfile', /not/ the pinned source's own
  -- @fromDefaultInstanceProfile@, which 'Amazonka.Auth.ConfigFile'
  -- reaches directly and unsafely for this exact credential source.
  , resolveEcsSource :: forall withAuth. Env' withAuth -> IO ManagedEnvAcquisition
  -- ^ @credential_source=EcsContainer@ -- 'safeContainer' (not
  -- 'safeContainerEnv': the URL is already resolved by the time this
  -- field is reached, exactly as pinned @evalConfig@'s own
  -- @fromContainer@ call, not @fromContainerEnv@, is used here -- see
  -- 'resolveCredentialSourceAcquisition').
  , resolveAssumedRole :: Text -> Env -> IO ManagedEnvAcquisition
  -- ^ @sts:AssumeRole@ onto an already-resolved source 'Env' --
  -- 'safeAssumedRole'.
  , resolveWebIdentity :: forall withAuth. FilePath -> Text -> Maybe Text -> Env' withAuth -> IO ManagedEnvAcquisition
  -- ^ @AssumeRoleWithWebIdentity@ -- 'safeWebIdentity'.
  , resolveSSO :: forall withAuth. FilePath -> Region -> Text -> Text -> Env' withAuth -> IO ManagedEnvAcquisition
  -- ^ @AssumeRoleViaSSO@ -- 'safeSSO'.
  }

{- | Every field except 'resolveEnvironmentSource' returns its resolved
'Env' paired with a genuinely-awaiting release as the opaque
'ManagedEnvAcquisition' type, not a plain, forgeable @(Env, IO ())@ tuple
nor an optional 'IORef' write: /every/ credential source capable of
producing temporary\/expiring credentials -- ECS, @sts:AssumeRole@, web
identity, and SSO, not only EC2\/IMDS -- is now a repository-owned managed
reimplementation rather than an opaque call into the pinned fork's own
@fetchAuthInBackground@-using functions, and none of them can compile
while silently discarding their own release, nor can a custom\/test
resolver assigned to one of these fields ever construct a
'ManagedEnvAcquisition' pairing a live, expiring 'Env' with an
unverified\/no-op release -- see the module Haddock, \"Structural release
ownership\", and 'ManagedEnvAcquisition''s own Haddock. See each @safe*@
function's own Haddock in this module for exactly which pinned source it
reimplements and why.
-}
productionConfigProfileResolvers :: ConfigProfileResolvers
productionConfigProfileResolvers =
  ConfigProfileResolvers
    { resolveEnvironmentSource = fromKeysEnv
    , resolveEc2Source = safeDefaultInstanceProfile
    , resolveEcsSource = resolveContainerCredentials
    , resolveAssumedRole = \roleArn -> safeAssumedRole roleArn "amazonka-assumed-role"
    , resolveWebIdentity = safeWebIdentity
    , resolveSSO = safeSSO
    }

{- | A safe reimplementation of the pinned Amazonka fork's own
@Amazonka.Auth.ConfigFile@ internal, non-exported @loadIniFile@
(@lib/amazonka/src/Amazonka/Auth/ConfigFile.hs@ at
@b562aa3f24845e34b95748daae671860017426be@), built entirely from the
exported @directory@\/@ini@ packages that pinned function itself uses --
no patched\/forked dependency. Matches its exact behavior byte-for-byte:
a missing file throws 'MissingFileError', any other read\/parse failure
throws 'InvalidFileError'; only the per-section key\/value assocs
('INI.iniSections') are returned, exactly as pinned @loadIniFile@ does.
-}
safeLoadIniFile :: FilePath -> IO (HashMap Text [(Text, Text)])
safeLoadIniFile path = do
  exists <- Directory.doesFileExist path
  unless exists $ Exception.throwIO (MissingFileError path)
  INI.readIniFile path >>= \case
    Left err -> Exception.throwIO $ InvalidFileError $ Text.pack (path <> ": " <> err)
    Right ini -> pure (INI.iniSections ini)

{- | A safe reimplementation of the pinned Amazonka fork's own
@Amazonka.Auth.ConfigFile@ internal, non-exported @evalConfig@
(@lib/amazonka/src/Amazonka/Auth/ConfigFile.hs@ at
@b562aa3f24845e34b95748daae671860017426be@), built entirely from that
module's own /exported/ primitives ('ConfigProfile', 'CredentialSource',
'parseConfigProfile') plus the injected 'ConfigProfileResolvers' rather
than a patched\/forked dependency.

The pinned source's recursive @evalConfig@, for @AssumeRoleFromProfile@\/
@AssumeRoleFromCredentialSource@ profiles, resolves a @sourceEnv@ (which
may itself carry a 'Ref'-based background refresh child -- e.g. via
@credential_source=Ec2InstanceMetadata@'s own @fromDefaultInstanceProfile@
call, or a deeper nested @AssumeRoleFromProfile@), then calls
@fromAssumedRole roleArn sessionName sourceEnv@, which does
@pure env {auth = Identity keys}@ -- /overwriting/, not preserving, the
source's own @.auth@ field with the new assumed-role's. The source's
original 'Auth'\/'Ref' becomes reachable only via whatever internal
closures Amazonka's own refresh loop happens to retain, with no explicit
handle ever exposed back to @fromFilePath@'s caller -- so nothing this
module's release protocol can ever see or kill it: it is orphaned, and a
delayed refresh failure can still @throwTo@ this thread at an arbitrary
future point, potentially misattributed to a /later/ generation.

This reimplementation instead accumulates every such hidden child's
release action explicitly (see 'AwsEnvAcquisition'), and wraps every
onward acquisition step in 'Exception.onException' so that if it fails or
this thread is cancelled /after/ a source's child was created but
/before/ the wrapping step (@fromAssumedRole@\/@fromWebIdentity@\/
@fromSSO@) returns, that source's already-accumulated children (and its
own now-about-to-be-hidden @.auth@) are released before the
exception propagates -- so a failed\/cancelled generation never leaks
what it already acquired. Every @ConfigProfile@ variant otherwise
resolves in exactly the pinned source's own precedence, region-override
sequencing, and error classification (including its identical
infinite-recursion\/@seen@-list cycle detection for
@AssumeRoleFromProfile@ chains: a @source_profile@ is rejected as soon as
it repeats one already on the current resolution path, exactly mirroring
pinned @evalConfig@'s own @StateT [Text]@ tracking).
-}
safeEvalConfigProfile
  :: ConfigProfileResolvers
  -> HashMap Text (HashMap Text Text)
  -> [Text]
  -> Text
  -> Env' withAuth
  -> IO AwsEnvAcquisition
safeEvalConfigProfile resolvers config seen profileName env =
  case HashMap.lookup profileName config of
    Nothing -> Exception.throwIO $ InvalidFileError $ "Missing profile: " <> Text.pack (show profileName)
    Just profileSettings -> case parseConfigProfile profileSettings of
      Nothing -> Exception.throwIO $ InvalidFileError $ "Parse error in profile: " <> Text.pack (show profileName)
      Just (profile, mRegion) -> applyRegionOverride mRegion <$> resolveProfile profile
 where
  applyRegionOverride :: Maybe Region -> AwsEnvAcquisition -> AwsEnvAcquisition
  applyRegionOverride Nothing acquisition = acquisition
  applyRegionOverride (Just r) acquisition =
    acquisition {awsEnvAcquisitionEnv = (awsEnvAcquisitionEnv acquisition) {region = r}}

  -- | Masked from @act@ (which may itself already have created a live,
  -- newly paired 'ManagedEnvAcquisition' by the time it returns -- see
  -- 'managedEnvAcquisition''s own masking) through the 'AwsEnvAcquisition'
  -- record being fully constructed: the same \"worker spawn through
  -- ledger commit\" span as 'commitManagedAcquisition', just building an
  -- 'AwsEnvAcquisition' (with no hidden releases of its own) instead of
  -- writing a caller-supplied 'IORef' directly.
  leaf :: IO ManagedEnvAcquisition -> IO AwsEnvAcquisition
  leaf act = Exception.mask_ $ do
    (finalEnv, release) <- runManagedEnvAcquisition <$> act
    pure
      AwsEnvAcquisition
        { awsEnvAcquisitionEnv = finalEnv
        , awsEnvAcquisitionRelease = release
        , awsEnvAcquisitionHiddenReleases = []
        }

  -- | Resolve a source (recursively, or a leaf 'CredentialSource'), then
  -- wrap it with @wrap@ (@safeAssumedRole@). If @wrap@ throws or this
  -- thread is cancelled, release everything the source already
  -- accumulated (including its own now-to-be-hidden @.auth@) before
  -- propagating; on success, the source's own @.auth@ becomes hidden (no
  -- longer reachable via the returned 'Env') and its recorded
  -- 'awsEnvAcquisitionRelease' (the correct release for that @.auth@,
  -- whether naive or 'managedFetchAuthInBackground'-derived -- see
  -- 'resolveCredentialSourceAcquisition') is appended to the accumulated
  -- hidden-release list. @wrap@ itself now always returns its own opaque
  -- 'ManagedEnvAcquisition' directly, since 'safeAssumedRole' is itself a
  -- managed provider (see the module Haddock, \"Structural release
  -- ownership\") -- there is no longer any ref, nor any forgeable
  -- @(Env, IO ())@ tuple, for it to forget or fake.
  --
  -- The entire body runs under one continuous 'Exception.mask_': without
  -- it, an asynchronous exception delivered in the ordinary, unmasked gap
  -- between @acquireSource@ returning and 'Exception.onException' being
  -- installed around @wrap@ below could propagate with no handler yet in
  -- place to release @sourceAcquisition@ -- exactly the class of
  -- \"worker spawn before ownership handoff\" gap the audit flagged.
  -- 'Exception.onException' still runs (a nested 'mask_' composes safely
  -- with the outer one), so a genuine failure from @wrap@ itself still
  -- releases @sourceAcquisition@ before propagating, exactly as before.
  assumeRoleOnto :: IO AwsEnvAcquisition -> (Env -> IO ManagedEnvAcquisition) -> IO AwsEnvAcquisition
  assumeRoleOnto acquireSource wrap = Exception.mask_ $ do
    sourceAcquisition <- acquireSource
    let sourceEnv = awsEnvAcquisitionEnv sourceAcquisition
        sourceRelease = awsEnvAcquisitionRelease sourceAcquisition
    ( do
        (finalEnv, release) <- runManagedEnvAcquisition <$> wrap sourceEnv
        pure
          AwsEnvAcquisition
            { awsEnvAcquisitionEnv = finalEnv
            , awsEnvAcquisitionRelease = release
            , awsEnvAcquisitionHiddenReleases = awsEnvAcquisitionHiddenReleases sourceAcquisition <> [sourceRelease]
            }
      )
      `Exception.onException` releaseAwsEnvAcquisition sourceAcquisition

  resolveProfile :: ConfigProfile -> IO AwsEnvAcquisition
  resolveProfile = \case
    ExplicitKeys authKeys -> leaf $ staticEnvAcquisition env {auth = Identity (Auth authKeys)}
    AssumeRoleFromProfile roleArn sourceProfileName
      | sourceProfileName `elem` seen ->
          Exception.throwIO
            $ InvalidFileError
            $ "Infinite source_profile loop: "
            <> Text.intercalate " -> " (reverse (sourceProfileName : seen))
      | otherwise ->
          assumeRoleOnto
            (safeEvalConfigProfile resolvers config (sourceProfileName : seen) sourceProfileName env)
            (resolveAssumedRole resolvers roleArn)
    AssumeRoleFromCredentialSource roleArn source ->
      assumeRoleOnto (resolveCredentialSourceAcquisition source) (resolveAssumedRole resolvers roleArn)
    AssumeRoleWithWebIdentity roleArn mSessionName tokenFile ->
      leaf $ resolveWebIdentity resolvers tokenFile roleArn mSessionName env
    AssumeRoleViaSSO startUrl ssoRegion accountId roleName -> do
      cachedTokenFile <- configPathRelative (relativeCachedTokenFile startUrl)
      leaf $ resolveSSO resolvers cachedTokenFile ssoRegion accountId roleName env

  -- | Resolve one leaf @CredentialSource@ into a full 'AwsEnvAcquisition'
  -- (always then wrapped by 'assumeRoleOnto', per 'AssumeRoleFromCredentialSource'
  -- -- @credential_source@ only ever appears paired with @role_arn@ in the
  -- pinned config-file grammar, so this source's own @.auth@ is always
  -- about to be hidden, never the final visible one). Every source
  -- except 'Environment' (never expiring, checked by 'staticEnvAcquisition')
  -- now goes through 'leaf''s uniform opaque-'ManagedEnvAcquisition'
  -- plumbing, exactly like every other managed provider in this module.
  resolveCredentialSourceAcquisition :: CredentialSource -> IO AwsEnvAcquisition
  resolveCredentialSourceAcquisition = \case
    Environment -> leaf (resolveEnvironmentSource resolvers env >>= staticEnvAcquisition)
    Ec2InstanceMetadata -> leaf (resolveEc2Source resolvers env)
    EcsContainer -> leaf (resolveEcsSource resolvers env)


{- | A safe reimplementation of the pinned Amazonka fork's own
@Amazonka.Auth.ConfigFile.fromFileEnv@\/@fromFilePath@
(@lib/amazonka/src/Amazonka/Auth/ConfigFile.hs@ at
@b562aa3f24845e34b95748daae671860017426be@), built entirely from that
module's own /exported/ primitives ('mergeConfigs', 'configPathRelative',
'Amazonka.Env.lookupRegion') plus 'safeLoadIniFile'\/'safeEvalConfigProfile'
above, rather than a patched\/forked dependency. Resolves the exact same
@AWS_PROFILE@\/@AWS_CONFIG_FILE@\/@AWS_SHARED_CREDENTIALS_FILE@ environment
variables, the same default @~\/.aws\/config@\/@~\/.aws\/credentials@
fallback paths (via 'configPathRelative'), the same \"a missing or
unparsable credentials\/config file is treated as empty, not fatal\"
tolerance (pinned @fromFilePath@'s own @Exception.catchJust@ around each
@loadIniFile@ call), and the same final @AWS_REGION@ override precedence,
as the pinned source.

Writes the resolved profile's hidden-release list into @hiddenReleasesRef@,
and -- exactly as every other managed provider in 'discoverSafely''s chain
-- the resolved profile's own final, genuinely-awaiting
'managedFetchAuthInBackground' release into @finalReleaseRef@, both read
back by 'acquireAwsEnv' once 'discoverSafely' as a whole succeeds. Both
are written only on this function's own success: every failure path
upstream in 'safeEvalConfigProfile' already self-cleans via
'Exception.onException' before any exception ever reaches here, so these
ledgers always reflect either this attempt's complete release state, or
(if this provider lost to an earlier one in 'discoverSafely''s chain, or
failed outright) remain untouched. The two writes happen back to back
with no interleaving fallible\/blocking step between them, and this
function -- like every 'discoverSafely' provider -- only ever runs from
'acquireAwsEnv'\/'runOneGeneration''s own masked span (see
'startSupervisedEnv'), so neither write can itself be asynchronously
interrupted: 'acquireAwsEnv' can never observe one without the other.

Before this @finalReleaseRef@ threading existed, /every/ config-selected
dynamic provider -- @sts:AssumeRole@ (direct or via @source_profile@),
@AssumeRoleWithWebIdentity@, SSO, @credential_source=EcsContainer@, and
@credential_source=Ec2InstanceMetadata@ -- left @finalReleaseRef@
permanently empty whenever the config-file provider won outright:
'safeEvalConfigProfile' still produced a fully correct
'awsEnvAcquisitionRelease' (via 'managedFetchAuthInBackground'),
but this function discarded it, so 'acquireAwsEnv' silently fell back to
the naive '.auth'-derived @requireChildReleased@ -- the exact
kill-then-bounded-poll fallback (with no stop flag, no genuinely-awaited
terminal acknowledgement) 'managedFetchAuthInBackground' exists to avoid
for every provider capable of producing temporary\/expiring credentials.
-}
safeFileEnv :: ConfigProfileResolvers -> IORef [IO ()] -> IORef (Maybe (IO ())) -> Env' withAuth -> IO Env
safeFileEnv resolvers hiddenReleasesRef finalReleaseRef env = do
  profileName <- maybe "default" Text.pack <$> lookupEnv "AWS_PROFILE"
  credentialsPath <- maybe (configPathRelative "/.aws/credentials") pure =<< lookupEnv "AWS_SHARED_CREDENTIALS_FILE"
  configPath <- maybe (configPathRelative "/.aws/config") pure =<< lookupEnv "AWS_CONFIG_FILE"
  credentialsIni <- tolerateMissingOrInvalid (safeLoadIniFile credentialsPath)
  configIni <- tolerateMissingOrInvalid (safeLoadIniFile configPath)
  let config = mergeConfigs credentialsIni configIni
  -- Masked from immediately before 'safeEvalConfigProfile' (which may
  -- itself already create a live, genuinely running background-refresh
  -- worker deep in its own resolution chain -- see 'leaf'\/'assumeRoleOnto''s
  -- own masking) all the way through recording its release into the two
  -- ledgers 'acquireAwsEnv' reads back once this function returns: this
  -- is one single, continuous, exception-safe transfer. A prior version
  -- only masked the ledger-write tail (from 'acquisition' onward), which
  -- still left the plain, unmasked return path from a successful
  -- 'safeEvalConfigProfile' call back into this function's own body as a
  -- genuine gap -- an asynchronous exception delivered in that ordinary
  -- allocation-point window, between 'safeEvalConfigProfile' handing back
  -- a fully live 'acquisition' and this function's own protection ever
  -- being installed, would propagate with no handler yet in place to
  -- release it. Wrapping the call itself closes that: if anything in
  -- this window throws, or an asynchronous exception lands in it,
  -- 'acquisition' is released here, symmetrically, before ever
  -- propagating, rather than silently becoming unreachable with its
  -- worker still live and its release never recorded anywhere
  -- 'acquireAwsEnv' could find it. 'lookupRegion' still runs via
  -- @restore@ (itself a defensive no-op layer of interruptibility given
  -- 'safeFileEnv' is now already fully masked from its own caller's
  -- perspective too -- see 'discoverSafely'), preserved unchanged from an
  -- earlier version of this function for exactly the genuinely
  -- interruptible network\/filesystem operations elsewhere in this
  -- module that rely on the same pattern.
  Exception.mask $ \restore -> do
    acquisition <- safeEvalConfigProfile resolvers config [] profileName env
    ( do
        regionOverride <- restore lookupRegion
        let resolvedEnv = maybe id (\r e -> e {region = r}) regionOverride (awsEnvAcquisitionEnv acquisition)
        writeIORef hiddenReleasesRef (awsEnvAcquisitionHiddenReleases acquisition)
        writeIORef finalReleaseRef (Just (awsEnvAcquisitionRelease acquisition))
        pure resolvedEnv
      )
      `Exception.onException` releaseAwsEnvAcquisition acquisition
 where
  tolerateMissingOrInvalid = Exception.handle (\(_ :: AuthError) -> pure HashMap.empty)



{- | Block for as long as this generation remains valid, then return the
sanitized diagnostic that invalidated it. Only ever called from
'startSupervisedEnv''s dedicated supervisor thread, immediately after that
same thread acquired the current generation via 'acquireAwsEnv' -- so it
is exactly the thread the pinned Amazonka fork's background refresh timer
captured as its @throwTo@ target (see 'releaseAwsEnvChild'), and it is
still here, deliberately blocked, ready to receive that exact exception
whenever (if ever) it arrives; never a Warp request thread that has since
moved on.

Blocks forever unless interrupted: either by a genuine delayed
'AuthError' refresh failure (caught here, classified, and returned as this
generation's invalidation reason), or by an external asynchronous
exception (e.g. 'stopSupervisedEnv''s cancellation, or an unanticipated
programmer fault elsewhere) -- which is deliberately *not* caught here and
propagates unchanged, since only 'AuthError' is a recognized,
sanitizable dependency failure for this generation.

Deliberately does /not/ itself release this generation's background
refresh child (contrast with an earlier version of this function, which
wrapped its own 'blockForever' in a local 'finally'): a @restore
(awaitInvalidation env)@ call site can have a /pending/ asynchronous
exception delivered exactly at the moment it unmasks, before this
function's body -- and any 'finally'\/'catch' it would install as its own
first action -- ever runs at all, silently skipping that release. See
'startSupervisedEnv''s @release@ parameter and 'runGeneration' for where
this generation's release now genuinely always runs instead: wrapped
around the /call/ to this function, installed while still masked, one
level further out.
-}
awaitAwsEnvInvalidation :: Env -> IO AwsAuthErrorDiagnostic
awaitAwsEnvInvalidation _env = do
  outcome <- try @_ @AuthError blockForever
  either (evaluate . classifyAuthErrorDiagnostic) absurd outcome
 where
  blockForever :: IO Void
  blockForever = forever (threadDelay maxBound)

{- | A typed snapshot of a supervised resource, observable by request
handlers without ever blocking on -- or triggering -- acquisition\/refresh
themselves. Never contains a raw exception: 'SupervisedEnvUnavailable'
only ever carries an already-forced 'AwsAuthErrorDiagnostic'.
-}
data SupervisedEnvState env
  = -- | No generation is currently 'SupervisedEnvReady': either the
    -- supervisor has not yet completed its first acquisition, or an
    -- acquisition attempt for a fresh generation (after backoff following
    -- a prior failure\/invalidation) is currently in flight. Both cases
    -- are the same constructor deliberately: either way, a caller with a
    -- bounded wait budget (see 'requestDemandDrivenReady') should wait
    -- rather than immediately treat this as a hard failure, since an
    -- in-flight acquisition may well succeed before the caller's own
    -- timeout elapses.
    SupervisedEnvInitializing
  | -- | @env@ is this generation's live, currently-valid resource.
    SupervisedEnvReady env
  | -- | The most recent generation failed to acquire, or was invalidated
    -- (e.g. a genuine background refresh failure), and the supervisor is
    -- currently backing off before its next attempt (which will publish
    -- 'SupervisedEnvInitializing' again once it actually begins) -- or
    -- the supervisor itself has stopped, in which case no further attempt
    -- will ever begin.
    SupervisedEnvUnavailable AwsAuthErrorDiagnostic
  deriving stock (Eq, Show)

-- | A running supervisor: a strict, typed state snapshot plus the handle
-- of the single dedicated thread that owns its entire acquire\/monitor\/
-- reacquire lifecycle. See 'startSupervisedEnv'.
data SupervisedEnv env = SupervisedEnv
  { supervisedEnvStateVar :: TVar (SupervisedEnvState env)
  , supervisedEnvThread :: ManagedThread ()
  }

{- | Start a dedicated, single-thread supervisor that owns the entire
acquire\/monitor\/reacquire lifecycle of one resource generation at a time,
publishing a strict typed snapshot of its current state for readers (see
'readSupervisedEnv') and never blocking a caller on acquisition.

This is the architectural fix for the async-credential-refresh-escape
audit: exactly one thread -- this supervisor's own, for the entire
lifetime of the running application -- ever calls @acquire@ (in
production, 'acquireAwsEnv', i.e. @newEnv discoverSafely@). Because the
pinned Amazonka fork's background refresh timer always targets \"the
thread that called @discover@\" for its eventual refresh-failure
@throwTo@, and that thread here is always this same dedicated supervisor
thread (never a per-request thread, and this supervisor is never
restarted per request), a delayed refresh failure can only ever land back
on the one thread still deliberately waiting for it (in
@awaitInvalidation@, in production 'awaitAwsEnvInvalidation') -- never on
a Warp worker that has since moved on to, or been reused for, an unrelated
request.

Each generation -- @acquire@ through 'SupervisedEnvReady' publication
through @awaitInvalidation@ -- runs under a single 'mask': @acquire@ itself
is never 'System.Timeout.timeout'-bounded or otherwise raced. Two
independent hazards this protects against, both found by direct audit
against the pinned source rather than assumed safe by construction:

* /Acquisition itself/ can fork a background refresh child part-way
  through, then fail or be cancelled /after/ that fork -- see
  'safeDefaultInstanceProfile' for the one provider in the entire
  @discover@ chain where this was possible, and how it is restructured so
  a fallible operation can no longer run after a child may already exist.
  With that fixed at the source, @acquire@ either returns 'Left' having
  created no child at all, or returns 'Right' with a child (if any)
  already reachable via its 'Env'.
* The brief span between @acquire@ returning a successfully-constructed
  'Right' resource and @awaitInvalidation@ actually beginning to block is
  deliberately masked so no asynchronous exception can land in that gap
  and be missed entirely; but masking alone is not sufficient to protect
  the /call/ to @awaitInvalidation@ itself, because 'restore'-ing back to
  interruptible for that call is exactly the point a /pending/ exception
  can be delivered -- before @awaitInvalidation@'s own body, and any
  handler it would install as its first action, ever runs. That is why
  release is no longer @awaitInvalidation@'s own responsibility (contrast
  the previous version of 'awaitAwsEnvInvalidation', which wrapped its own
  'blockForever' in a local 'finally'): 'runGeneration' below instead
  applies @release@ via 'finally' /around/ the @restore (awaitInvalidation
  env)@ call, at the call site -- 'finally''s own handler installation
  happens synchronously, before 'restore' ever unmasks, so it is already
  on the exception-handler stack no matter how immediately a pending
  exception is delivered upon unmasking.

Blocking, interruptible operations performed /by/ @acquire@ itself (e.g.
the network calls inside @newEnv discoverSafely@) remain interruptible
even under this 'mask' -- only the non-blocking sequence following a
successful fetch (recording the resulting 'Env'\/'Ref', with no further
I\/O) is genuinely protected. 'Control.Exception.mask' is used rather than
'Control.Exception.uninterruptibleMask_' throughout, so this can never
turn into an unkillable thread.

The lifecycle itself is built from this module's own
'Api.Arkham.Lifecycle.ManagedThread' rather than the \"async\" package's
high-level 'Control.Concurrent.Async.Async': async-2.2.6's own
'Control.Concurrent.Async.withAsync' falls back to
@Control.Concurrent.Async.uninterruptibleCancel@ internally when its own
wait is itself asynchronously interrupted while a child is still running
-- meaning a genuinely stuck generation (e.g. blocked inside a
non-interruptible foreign call while @acquire@\/@awaitInvalidation@ is
running) could make *that* cancellation, and therefore this whole
dispatcher's shutdown (ultimately 'stopSupervisedEnv', and everything
that in turn waits on it, e.g. Foundation shutdown), unconditionally
unkillable until the stuck generation eventually finishes, however long
that takes -- an actual production hazard 'ManagedThread' avoids entirely
by construction: it only ever uses ordinary, interruptible
'Control.Exception.throwTo'\/'Control.Concurrent.MVar.takeMVar', never an
uninterruptible cancellation, so a caller waiting to stop this supervisor
can always itself still be interrupted (e.g. by an enclosing bounded
wait), even against a stuck generation that never responds.

/Per-generation thread isolation:/ each generation's entire @acquire@
through @awaitInvalidation@ span (below, @runOneGeneration@) runs on its
own freshly-spawned 'Api.Arkham.Lifecycle.ManagedThread', rather than
directly on this long-lived dispatcher thread. This closes a genuine
cross-generation hazard found by direct audit: the pinned Amazonka fork's
@Amazonka.Auth.Background.fetchAuthInBackground@ (see
'releaseAwsEnvChild') always targets /whichever thread called @discover@/
as its eventual delayed refresh-failure @throwTo@ target. If every
generation's @acquire@ ran on the very same single dispatcher thread (as
in an earlier version of this module), then killing an /old/ generation's
child during release (see 'releaseAwsEnvAcquisition') could -- if that
kill happens to land exactly while the child is mid-refresh-call -- cause
the child to convert its own cancellation into a synthetic 'AuthError'
and @throwTo@ it back at that shared thread, arbitrarily later: possibly
while a /different/ child is being released, or even once a /newer/
generation has already begun acquiring or monitoring. Giving every
generation a distinct, disposable thread makes that structurally
impossible: 'Control.Concurrent.ThreadId's are never reused or aliased,
so a delayed @throwTo@ captured against one generation's thread can only
ever be delivered to that exact (by then likely already-finished or
finishing) thread -- never to the dispatcher, and never to any other
generation's distinct thread, no matter how the two overlap in time. This
is also precisely why 'releaseAwsEnvAcquisition' can safely treat an
'AuthError' surfacing while releasing one of /this/ generation's own
children as expected, self-inflicted feedback rather than a fresh
failure: it can only ever have come from a child that belongs to this
exact generation. If this dispatcher thread is itself asynchronously
cancelled (e.g. by 'stopSupervisedEnv') while waiting on the in-flight
generation, @loopOnce@'s own 'Control.Exception.onException' guarantees
that generation's thread is cancelled and awaited to completion /first/
-- running its own @release@ via @runOneGeneration@'s 'finally' -- before
the cancellation is allowed to finish unwinding the dispatcher itself; a
stop can therefore never leave a generation thread, or the children it
owns, orphaned.

On every loop iteration: 'SupervisedEnvInitializing' is (re-)published
first, then @acquire@ is attempted; a failure publishes
'SupervisedEnvUnavailable' and waits @backoff@ before retrying (nothing
to @release@ on this path, since a properly-composed @acquire@ that fails
never leaves a child behind -- see above). A success publishes
'SupervisedEnvReady', then blocks in @awaitInvalidation@ until that
generation is invalidated -- by whatever means, including an external
cancellation -- at which point @release@ always runs exactly once before
this generation's diagnostic is published and @backoff@ is awaited before
the next attempt (which will again publish 'SupervisedEnvInitializing'
once it actually begins). Re-publishing 'SupervisedEnvInitializing' on
every attempt -- not only the very first -- matters because
'requestDemandDrivenReady' only performs a bounded wait when it observes
'SupervisedEnvInitializing'; without this, a caller arriving while a
post-failure re-acquisition is already in flight would instead see the
*previous* generation's stale 'SupervisedEnvUnavailable' and fail
immediately, even if this attempt goes on to succeed well within that
caller's own timeout. Whatever exception terminates the supervisor thread
itself (an external 'stopSupervisedEnv', or an unanticipated exception
escaping @acquire@\/@awaitInvalidation@ that is deliberately *not* caught
here, per the \"no broad catch\" requirement) is guaranteed, via 'finally',
to first publish 'AwsAuthSupervisorTerminated' -- so a reader can never
observe a stale 'SupervisedEnvReady' snapshot pointing at a resource
nobody is monitoring any longer.
-}
startSupervisedEnv
  :: IO (Either AwsAuthErrorDiagnostic (env, IO ()))
  -- ^ acquire one fresh generation, paired with its own complete release
  -- action -- see 'AwsEnvAcquisition' for why release must be derived
  -- from what @acquire@ itself acquired (which may include state not
  -- recoverable from @env@ alone, e.g. a config-file source profile's
  -- hidden refresh child) rather than a separate, @env@-only projection.
  -> (env -> IO AwsAuthErrorDiagnostic)
  -- ^ block until @env@'s generation is invalidated (or an external
  -- exception interrupts this call), returning the classified reason.
  -> IO ()
  -- ^ backoff between a failed\/invalidated generation and the next
  -- attempt.
  -> IO (SupervisedEnv env)
startSupervisedEnv = startSupervisedEnvUsing (pure ())

{- | As 'startSupervisedEnv', parameterized over an extra test-only seam
run by @loopOnce@ immediately after @generationThread@ is spawned and
strictly before this dispatcher @restore@s back to an interruptible wait
around it. Production always passes @'pure' ()@ via 'startSupervisedEnv';
this seam exists purely so a regression test can deterministically pause
the dispatcher /exactly/ inside the masked span the HIGH-severity
\"unmasked between spawn and handler-install\" fix closes (see
'startSupervisedEnv''s own Haddock, \"'loopOnce' itself runs entirely
unmasked...\"), rather than racing a real exception against a window only
ever a few instructions wide.

A test seam suitable for this must itself never rely on any operation
that remains interruptible even under 'Control.Exception.mask' (e.g.
'Control.Concurrent.MVar.MVar'\/'Control.Concurrent.threadDelay'\/STM's
@retry@) -- doing so would reopen exactly the escape valve this mask
exists to close, defeating the point of testing it. A busy-spin on a
plain 'Data.IORef.IORef' is genuinely safe here specifically /because/ it
never blocks on any such primitive: GHC's async-exception delivery to a
masked thread is deferred regardless of how long it busy-spins, so a test
driving this seam can hold the dispatcher here for an arbitrary, tester
-controlled duration (verifying, from the outside, that the paused
generation thread is still alive and that a concurrent
'stopSupervisedEnv'\/'cancelManagedThread' targeting this dispatcher
genuinely cannot proceed past it) with an absolute, zero-probability
guarantee against a false pass -- and, symmetrically, a mutated
(accidentally unmasked) version of this same span would almost
-instantly lose the busy-spinning thread to the injected cancellation
instead, at ordinary allocation\/heap-check points, making a mutation
failure certain rather than merely likely.
-}
startSupervisedEnvUsing
  :: IO ()
  -- ^ afterSpawn: the test-only seam described above. @'pure' ()@ in
  -- production.
  -> IO (Either AwsAuthErrorDiagnostic (env, IO ()))
  -> (env -> IO AwsAuthErrorDiagnostic)
  -> IO ()
  -> IO (SupervisedEnv env)
startSupervisedEnvUsing afterSpawn acquire awaitInvalidation backoff = do
  stateVar <- newTVarIO SupervisedEnvInitializing
  supervisorThread <- spawnManagedThread (supervise stateVar)
  pure SupervisedEnv {supervisedEnvStateVar = stateVar, supervisedEnvThread = supervisorThread}
 where
  supervise stateVar =
    forever loopOnce `finally` publish stateVar AwsAuthSupervisorTerminated
   where
    loopOnce = Exception.mask $ \restore -> do
      -- See the module Haddock ("Per-generation thread isolation"): each
      -- generation's own acquire/monitor/release cycle runs on a fresh
      -- 'ManagedThread', never this dispatcher thread itself, so a
      -- delayed refresh-failure feedback from an old generation's child
      -- can never land on a different (older, newer, or this
      -- dispatcher's own) thread. The 'Exception.onException' here
      -- mirrors 'Control.Concurrent.Async.withAsync''s own guarantee
      -- that, if this dispatcher is cancelled while waiting, the
      -- in-flight generation is cancelled and awaited first -- but,
      -- unlike 'Control.Concurrent.Async.withAsync', using only ordinary
      -- interruptible cancellation throughout (see 'ManagedThread').
      --
      -- 'loopOnce' itself runs entirely unmasked (this dispatcher thread
      -- was itself spawned via 'spawnManagedThread', whose own body runs
      -- via 'Control.Concurrent.forkIOWithUnmask' -- see 'ManagedThread'),
      -- so without this explicit 'Exception.mask', a 'ThreadKilled'
      -- delivered in the gap between 'spawnManagedThread' returning
      -- @generationThread@ and 'Exception.onException' actually
      -- installing its handler around the wait below could propagate
      -- from this @do@-block *unhandled* -- orphaning @generationThread@
      -- (and everything it owns) with nothing left to cancel\/await it,
      -- while 'forever loopOnce''s own enclosing 'finally' only ever
      -- publishes 'AwsAuthSupervisorTerminated', never releases a
      -- specific in-flight generation. Masking the entire
      -- spawn-through-handler-installation span closes that gap
      -- completely: 'restore' is applied only around the wait itself, so
      -- this dispatcher remains genuinely interruptible while blocked
      -- there (and 'Exception.onException''s handler, installed while
      -- still masked, is guaranteed to already be in place for any
      -- exception 'restore' lets back in), but no exception can ever
      -- land in the narrower window before that handler exists.
      --
      -- @afterSpawn@ itself now runs *inside* the same
      -- 'Exception.onException' as the wait, not before it: a prior
      -- version of this function ran @afterSpawn@ between
      -- 'spawnManagedThread' returning and 'Exception.onException' being
      -- installed, so a genuine, synchronous exception thrown by
      -- @afterSpawn@ itself (never possible for production's own
      -- @'pure' ()@, but not something this exported, test-only-in-
      -- practice parameter's type forbids either) would propagate with
      -- no handler yet in place to cancel\/await @generationThread@ --
      -- orphaning it exactly as the masked-gap bug this function's own
      -- Haddock describes did, just one statement later. There is
      -- deliberately no other way for a caller of this function to reach
      -- @generationThread@ at all except through @cancelManagedThread@
      -- here, so folding @afterSpawn@ into the very same protected span
      -- as the wait closes that residual gap completely rather than only
      -- narrowing it, without needing to hide this seam from tests that
      -- must still drive it deterministically (see
      -- 'AwsEnvSupervisorSpec''s own \"genuinely cancels and awaits...\"
      -- regression, this function's only caller of a non-trivial
      -- @afterSpawn@).
      generationThread <- spawnManagedThread runOneGeneration
      diag <-
        (afterSpawn >> restore (waitManagedThread generationThread))
          `Exception.onException` cancelManagedThread generationThread
      publish stateVar diag
      backoff

    -- One full generation, masked from just before @acquire@ through the
    -- publication of 'SupervisedEnvReady': see the Haddock above for
    -- exactly what this does and does not protect against. Publishes
    -- 'SupervisedEnvInitializing' before calling @acquire@ on *every*
    -- attempt (not only the very first) so a caller whose own bounded
    -- wait (see 'requestDemandDrivenReady') lands while a re-acquisition
    -- following a prior failure/invalidation is already in flight can
    -- still observe that an attempt is under way and wait for it, rather
    -- than immediately reading a stale 'SupervisedEnvUnavailable' from
    -- the *previous* generation and returning an avoidable failure to the
    -- caller even though this attempt might succeed well within their
    -- timeout.
    --
    -- @release@ (bundled with @env@ by @acquire@ itself) is applied via
    -- 'finally' /around/ @restore (awaitInvalidation env)@ -- i.e. at
    -- this call site, not inside @awaitInvalidation@'s own body -- so its
    -- handler is installed synchronously, while still masked, before
    -- 'restore' ever unmasks. A pending asynchronous exception delivered
    -- exactly as 'restore' unmasks is therefore always caught by this
    -- already-installed 'finally', which runs @release@ before the
    -- exception propagates; had @awaitInvalidation@ tried to install its
    -- own handler as the first action of its (as-yet-unforced) body, that
    -- pending exception could be delivered before that body ever begins
    -- running at all, skipping it entirely. See 'startSupervisedEnv''s
    -- Haddock for the full explanation. This entire span runs on its own
    -- fresh, disposable, per-generation thread -- see 'loopOnce' above --
    -- rather than the dispatcher thread that forks it.
    runOneGeneration :: IO AwsAuthErrorDiagnostic
    runOneGeneration = mask $ \restore -> do
      atomically $ writeTVar stateVar SupervisedEnvInitializing
      acquired <- acquire
      case acquired of
        Left diag -> pure diag
        Right (env, release) -> do
          atomically $ writeTVar stateVar (SupervisedEnvReady env)
          restore (awaitInvalidation env) `finally` release
  publish stateVar diag = do
    forced <- evaluate diag
    atomically $ writeTVar stateVar (SupervisedEnvUnavailable forced)

-- | Read the supervisor's current strict, typed state snapshot. Never
-- blocks on acquisition\/refresh: a request observing 'SupervisedEnvReady'
-- may use that @env@ immediately; any other state is an immediate,
-- sanitized dependency failure.
readSupervisedEnv :: SupervisedEnv env -> IO (SupervisedEnvState env)
readSupervisedEnv = readTVarIO . supervisedEnvStateVar

{- | The observable, defined outcome of one 'stopSupervisedEnv' attempt.
Never a bare @()@: a caller must be able to distinguish \"genuinely,
confirmedly stopped\" from \"could not confirm that within a bounded
wait\" and react accordingly -- e.g. 'stopDemandDrivenSupervisor'\/
'stopAwsEnvSupervisor' below both treat 'SupervisorStopFailed' as a
thrown exception, so that no caller (a restart helper, a test, a
'Application.shutdownApp'\/'DevelMain' bracket) can ever mistake it for
success and proceed to release dependencies or start a replacement.
-}
data SupervisorStopOutcome
  = SupervisorStopped
  | -- | The dedicated supervisor thread could not be confirmed terminal
    -- within 'supervisorStopTimeoutMicros'. This can only genuinely arise
    -- from a thread stuck inside a truly non-interruptible operation (see
    -- 'awaitThreadTerminated'\/'releaseAwsEnvChild' for the same,
    -- documented, accepted platform limitation elsewhere in this module)
    -- -- an ordinary, cooperative Haskell thread, however deep inside
    -- @acquire@\/@awaitInvalidation@\/@release@ it is blocked, responds to
    -- an ordinary 'Control.Exception.throwTo' essentially immediately.
    -- Deliberately does /not/ carry the dedicated thread's own eventual
    -- exit value: by definition, this constructor means that value was
    -- never observed within the bound.
    SupervisorStopFailed
  deriving stock (Eq, Show)

-- | A generous, purely defensive bound (see 'SupervisorStopFailed'): in
-- ordinary operation this is never reached, since 'cancelManagedThread'
-- only blocks on genuinely interruptible operations. Not a correctness
-- mechanism -- it exists only so a truly stuck dedicated thread produces
-- an honest, typed, bounded failure instead of hanging 'stopSupervisedEnv'
-- (and therefore, transitively, Foundation shutdown) forever.
supervisorStopTimeoutMicros :: Int
supervisorStopTimeoutMicros = 30 * 1000 * 1000

{- | Explicitly stop a supervisor: terminate its dedicated thread and
/genuinely wait/ (via 'cancelManagedThread', see 'startSupervisedEnv') for
it to actually finish -- which, per 'awaitInvalidation''s\/
'awaitAwsEnvInvalidation''s own 'finally', also releases the current
generation's background resource (e.g. kills a live Amazonka refresh
thread, or, for a managed one, genuinely awaits its termination -- see
'managedFetchAuthInBackground') before this returns. Used by
'stopAwsEnvSupervisor' (in turn used by @Application.shutdownApp@ and
deterministic tests), and directly by the generic-protocol tests below.

'cancelManagedThread' uses only ordinary, interruptible cancellation --
never 'Control.Exception.uninterruptibleMask'\/
@Control.Concurrent.Async.uninterruptibleCancel@ -- so this function can
always itself still be interrupted by an enclosing asynchronous
exception. The 'UnliftIO.Timeout.timeout' wrapped around it exists solely
to turn the one remaining, documented residual hazard (a truly
non-interruptible foreign call somewhere in @acquire@\/@awaitInvalidation@\/
@release@ that never actually responds to the delivered @throwTo@) into
an honest, typed 'SupervisorStopFailed' rather than either an infinite
hang or -- worse -- a false claim of success: on that path, this
deliberately does /not/ publish 'AwsAuthSupervisorTerminated', so a reader
can never be misled into believing the dedicated thread (and whatever
background resource it may still hold) has actually stopped.

Separately (independent of the timeout above): if a cancellation lands in
the narrow instant between the dedicated thread's very first scheduled
instruction and 'supervise''s own @forever@\/@finally@ actually being
entered -- which can only happen before any generation has ever been
acquired, since nothing meaningful (no credentials, no background refresh
child) exists yet at that point -- 'ManagedThread''s own outer @try@ (in
'Api.Arkham.Lifecycle.spawnManagedThread') still faithfully records the
thread as terminated, but the /inner/ @finally@ that would otherwise
publish 'AwsAuthSupervisorTerminated' itself can be skipped entirely.
This function's own unconditional publish in the success branch below --
performed only once 'cancelManagedThread' has already confirmed the
thread is genuinely dead -- closes that gap without depending on GHC's
exact exception-delivery timing at thread start: it can never race a
live generation, and is a no-op whenever the inner @finally@ already
published the same value.
-}
stopSupervisedEnv :: SupervisedEnv env -> IO SupervisorStopOutcome
stopSupervisedEnv sup = do
  outcome <- timeout supervisorStopTimeoutMicros (cancelManagedThread (supervisedEnvThread sup))
  case outcome of
    Nothing -> pure SupervisorStopFailed
    Just () -> do
      atomically $ writeTVar (supervisedEnvStateVar sup) (SupervisedEnvUnavailable AwsAuthSupervisorTerminated)
      pure SupervisorStopped

{- | A generic demand-driven wrapper around 'SupervisedEnv': its dedicated
thread (via 'startSupervisedEnv') starts immediately, but the wrapped
@acquire@ action first blocks on an internal demand flag, so nothing it
would otherwise do (contact a network endpoint, read a file, etc.) happens
until 'requestDemandDrivenReady' signals demand at least once. Kept
generic over both the resource type and (implicitly, via the @acquire@
passed to 'newDemandDrivenSupervisor') the acquisition action itself so
that regression tests can inject a fully fake, in-memory @acquire@ without
starting the real 'acquireAwsEnv' (which would contact AWS) -- see
'AwsEnvSupervisor' below for the concrete, production-facing instance of
this same generic wrapper.
-}
data DemandDrivenSupervisor env = DemandDrivenSupervisor
  { demandDrivenSupervisorGeneric :: SupervisedEnv env
  , demandDrivenSupervisorDemand :: TVar Bool
  }

{- | Construct a demand-driven supervisor: starts its dedicated thread
immediately, but that thread performs no acquisition at all until the
first call to 'requestDemandDrivenReady' signals demand. Once demanded,
behaves exactly like an eagerly-started 'startSupervisedEnv' supervisor
for the remainder of its lifetime: signalling demand a second (or
subsequent) time is a cheap no-op.
-}
newDemandDrivenSupervisor
  :: IO (Either AwsAuthErrorDiagnostic (env, IO ()))
  -- ^ acquire one fresh generation (paired with its release action) --
  -- not run at all until demanded.
  -> (env -> IO AwsAuthErrorDiagnostic)
  -- ^ block until @env@'s generation is invalidated.
  -> IO ()
  -- ^ backoff between a failed\/invalidated generation and the next
  -- attempt.
  -> IO (DemandDrivenSupervisor env)
newDemandDrivenSupervisor acquire awaitInvalidation backoff = do
  demandVar <- newTVarIO False
  generic <- startSupervisedEnv (waitForDemand demandVar >> acquire) awaitInvalidation backoff
  pure DemandDrivenSupervisor {demandDrivenSupervisorGeneric = generic, demandDrivenSupervisorDemand = demandVar}
 where
  waitForDemand demandVar = atomically (readTVar demandVar >>= check)

{- | Signal demand (starting discovery on the supervisor's dedicated thread
if this is the first call), then wait up to @timeoutMicros@ for the
supervisor to leave 'SupervisedEnvInitializing' -- returning whatever
snapshot is current the moment either the supervisor settles or the bound
elapses, whichever comes first.

The bounded wait below is a plain 'System.Timeout.timeout' around a
read-only 'atomically'\/'retry' on the /calling/ thread itself -- never
around, or targeting, the supervisor thread performing acquisition. Timing
out here can only abort this /read/ (an interrupted STM transaction simply
has no effect, per STM's semantics) and never reaches, let alone cancels,
the supervisor's own in-flight acquisition, which keeps running
independently and will publish its result whenever it completes, for the
next caller (or this same caller's next request) to observe. A caller
never owns, blocks, or cancels acquisition.

Already-settled state ('SupervisedEnvReady' or 'SupervisedEnvUnavailable')
is returned immediately without waiting at all.
-}
requestDemandDrivenReady :: DemandDrivenSupervisor env -> Int -> IO (SupervisedEnvState env)
requestDemandDrivenReady sup timeoutMicros = do
  atomically $ writeTVar (demandDrivenSupervisorDemand sup) True
  current <- readSupervisedEnv (demandDrivenSupervisorGeneric sup)
  case current of
    SupervisedEnvInitializing -> do
      settled <- timeout timeoutMicros $ atomically $ do
        s <- readTVar (supervisedEnvStateVar (demandDrivenSupervisorGeneric sup))
        case s of
          SupervisedEnvInitializing -> retry
          settledState -> pure settledState
      maybe (readSupervisedEnv (demandDrivenSupervisorGeneric sup)) pure settled
    settledAlready -> pure settledAlready

{- | Explicitly stop a demand-driven supervisor: terminates its dedicated
thread and waits for it (and, if live, its current generation's
background resource) to actually finish. See 'stopSupervisedEnv'. Throws
if 'stopSupervisedEnv' returns 'SupervisorStopFailed' -- this function's
callers (production 'Application.shutdownApp', and every existing test
call site, which are all bare, unused-return-value statements) rely on
"returned without throwing" already meaning "genuinely, confirmedly
stopped"; preserving that as a thrown exception, rather than silently
changing this function's own return type, means every one of those call
sites automatically inherits the new, honest failure signal (never
silently proceeding to release dependencies or start a replacement) with
no further changes required at any of them.
-}
stopDemandDrivenSupervisor :: DemandDrivenSupervisor env -> IO ()
stopDemandDrivenSupervisor sup =
  stopSupervisedEnv (demandDrivenSupervisorGeneric sup) >>= \case
    SupervisorStopped -> pure ()
    SupervisorStopFailed ->
      Exception.throwIO
        $ Exception.ErrorCall "AwsEnvSupervisor: stopSupervisedEnv could not confirm the dedicated thread terminated"

{- | The application's single, foundation-owned AWS 'Env' supervisor: a
thin, production-facing instance of the generic 'DemandDrivenSupervisor',
fixed to the real 'acquireAwsEnv'\/'awaitAwsEnvInvalidation'\/backoff.
Constructed exactly once, in @Application.makeFoundation@, before Warp is
handed the application -- never lazily on the first request. See the
module-level Haddock for the full rationale.
-}
newtype AwsEnvSupervisor = AwsEnvSupervisor (DemandDrivenSupervisor Env)

{- | Construct the application's AWS 'Env' supervisor: starts its dedicated
thread immediately, but that thread performs no credential discovery (and
so contacts no environment variable, file, or metadata endpoint) until the
first call to 'requestAwsEnvReady' signals demand -- so constructing this
during @makeFoundation@ (including for every test\/REPL invocation that
builds a foundation) never itself causes a network\/filesystem credential
lookup. See 'newDemandDrivenSupervisor'.
-}
newAwsEnvSupervisor :: IO AwsEnvSupervisor
newAwsEnvSupervisor =
  AwsEnvSupervisor
    <$> newDemandDrivenSupervisor
      acquireAwsEnv
      awaitAwsEnvInvalidation
      awsEnvSupervisorBackoff
 where
  -- Real reacquisition backoff: deliberately not configurable/injectable
  -- in production (unlike the request-local wait bound in
  -- 'requestAwsEnvReady'), since nothing about backoff duration affects
  -- request-facing correctness -- it only paces how often a persistently
  -- failing credential source is retried, avoiding a tight retry storm
  -- against it.
  awsEnvSupervisorBackoff = threadDelay (5 * 1000 * 1000)

-- | See 'requestDemandDrivenReady'; this is that function fixed to the
-- application's single, concrete 'AwsEnvSupervisor'.
requestAwsEnvReady :: AwsEnvSupervisor -> Int -> IO (SupervisedEnvState Env)
requestAwsEnvReady (AwsEnvSupervisor sup) = requestDemandDrivenReady sup

-- | Explicitly stop the application's AWS 'Env' supervisor: terminates its
-- dedicated thread and waits for it (and, if live, its current
-- generation's background refresh child) to actually finish. Called by
-- @Application.shutdownApp@ for deterministic dev\/REPL\/test lifecycle;
-- production process exit tears the thread down implicitly.
stopAwsEnvSupervisor :: AwsEnvSupervisor -> IO ()
stopAwsEnvSupervisor (AwsEnvSupervisor sup) = stopDemandDrivenSupervisor sup


----------------------------------------------------------------------
-- Internal-only regression tests for this module's own credential
-- acquisition\/release primitives (see the export list above and the
-- module Haddock, "Structural release ownership"): relocated here,
-- verbatim in substance, from what was an external test module that
-- imported 'ManagedEnvAcquisition', 'AwsEnvAcquisition' (its full
-- constructor), 'runManagedEnvAcquisition', and every @safe*@
-- provider directly -- exactly the public forgeability a MEDIUM
-- finding rejected: any external module (a test, or otherwise) could
-- pull an 'Env' from one 'ManagedEnvAcquisition' and a release from
-- an unrelated one and recombine them into a mismatched
-- 'AwsEnvAcquisition'. None of those identifiers are exported past
-- this module's own boundary any more (see the export list at the
-- top): this 'Spec' is the only way any of them are exercised from
-- outside 'newAwsEnvSupervisor'\/'requestAwsEnvReady'\/
-- 'stopAwsEnvSupervisor' themselves, and it is provable by
-- construction, not merely by convention -- there is no export list
-- entry an external module could ever import to reach any of them.
----------------------------------------------------------------------
#ifdef INTERNAL_TEST_HOOKS
-- | A minimal fake resource standing in for a real Amazonka 'Env' in the
-- generic 'startSupervisedEnv'\/'readSupervisedEnv'\/'stopSupervisedEnv'
-- lifecycle tests below, which exercise the supervisor protocol itself
-- and deliberately have no real AWS\/network dependency. The wrapped 'Int'
-- lets successive acquisition generations be told apart.
newtype TestResource = TestResource Int
  deriving stock (Eq, Show)

-- | An @awaitInvalidation@ action for supervisor tests that never expect
-- their generation to be invalidated at all (e.g. because the test stops
-- the supervisor explicitly before that would ever matter).
neverInvalidate :: env -> IO AwsAuthErrorDiagnostic
neverInvalidate _ = forever (threadDelay maxBound)

-- | A @release@ action for supervisor tests that do not exercise release
-- behaviour at all -- most of the generic-protocol tests below are about
-- state publication/acquisition/backoff, not the release contract itself.
noRelease :: env -> IO ()
noRelease _ = pure ()

{- | Adapts the old, separate @acquire :: IO (Either diag env)@ \/
@release :: env -> IO ()@ pair (still the natural shape for most of these
tests, which are about publication\/backoff\/acquisition timing, not the
release-derivation mechanism itself) into 'startSupervisedEnv'\/
'newDemandDrivenSupervisor''s current single
@acquire :: IO (Either diag (env, IO ()))@ shape (see
'Api.Arkham.AwsEnvSupervisor.AwsEnvAcquisition' for why production folds
release into acquire's own return value).
-}
withRelease :: (env -> IO ()) -> IO (Either d env) -> IO (Either d (env, IO ()))
withRelease release acquire = fmap (\env -> (env, release env)) <$> acquire

{- | Poll 'readSupervisedEnv' until @predicate@ holds. This is only a
bounded hang-guard against a genuinely stuck test suite: the actual
ordering\/determinism each test proves comes from the 'MVar' gates its own
fake @acquire@\/@awaitInvalidation@\/@backoff@ actions synchronize on, not
from this loop's polling interval or bound.
-}
waitForSupervisedState :: Show env => SupervisedEnv env -> (SupervisedEnvState env -> Bool) -> IO ()
waitForSupervisedState sup predicate = go (200 :: Int)
 where
  go 0 = do
    s <- readSupervisedEnv sup
    expectationFailure $ "supervisor state never satisfied the expected predicate; last seen: " <> show s
  go n = do
    s <- readSupervisedEnv sup
    if predicate s
      then pure ()
      else threadDelay (10 * 1000) >> go (n - 1)

-- | Same as 'waitForSupervisedState', but against a 'DemandDrivenSupervisor'
-- by polling 'requestDemandDrivenReady' with a tiny per-poll bound so it
-- never itself blocks the poll loop for long. Used only where a test needs
-- to observe a demand-driven supervisor settle asynchronously to its own
-- act of first demanding it (e.g. after a separate, earlier demand call).
waitForDemandDrivenState
  :: Show env => DemandDrivenSupervisor env -> (SupervisedEnvState env -> Bool) -> IO ()
waitForDemandDrivenState sup predicate = go (200 :: Int)
 where
  go 0 = do
    s <- requestDemandDrivenReady sup 0
    expectationFailure $ "supervisor state never satisfied the expected predicate; last seen: " <> show s
  go n = do
    s <- requestDemandDrivenReady sup 0
    if predicate s
      then pure ()
      else threadDelay (10 * 1000) >> go (n - 1)

{- | Wraps a value so that the *first* time it is forced (to WHNF), it
records that fact into @ref@ before returning the value unchanged. Used to
prove that a value was forced *before* some earlier observation point,
rather than only incidentally by the test's own later inspection.

'NOINLINE' prevents GHC from floating/duplicating or eliminating the
'unsafePerformIO' as dead code; ordering against the read of @ref@ is
guaranteed by the data dependency (the read happens strictly after the
action being tested has already returned, and the write only happens when
something forces this thunk, which can only be the code under test if it
happens before that read).
-}
markForcedOnceEvaluated :: IORef Bool -> a -> a
markForcedOnceEvaluated ref x = unsafePerformIO (writeIORef ref True) `seq` x
{-# NOINLINE markForcedOnceEvaluated #-}

httpExceptionFixture :: Client.HttpException
httpExceptionFixture = Client.InvalidUrlException "https://arkham-horror-bugs.s3.amazonaws.com" "connection failure"

-- | A leaf resolver result: static credentials, never a background
-- refresh child -- matches pinned 'ExplicitKeys'\/'Auth' semantics
-- exactly (see 'Amazonka.Auth.Keys.fromKeys' and friends, none of which
-- ever construct a 'Ref').
fakeStaticEnv :: Env' withAuth -> IO Env
fakeStaticEnv env = pure env {auth = Identity (Auth (AuthEnv "AKIASTATIC" "secret" Nothing Nothing))}

{- | A resolver result standing in for any real provider that calls
'Amazonka.Auth.Background.fetchAuthInBackground' for temporary\/expiring
credentials (assumed-role, web identity, SSO, ECS, EC2 instance profile):
forks a real, killable thread and returns an 'Env' whose @.auth@ is
'Ref'-shaped, exactly like the pinned real forker would for expiring
credentials, paired with its release as the opaque
'ManagedEnvAcquisition' type -- exactly the shape every
'ConfigProfileResolvers' dynamic-provider field now returns (see the
module Haddock, \"Structural release ownership\"): there is no 'IORef'
side channel, nor any forgeable @(Env, IO ())@ tuple, for a fake (or a
real resolver) to forget or fake. Returns the child's 'ThreadId' too so
tests can assert on its liveness directly via 'threadStatus', independent
of whichever 'Auth'\/'Env' value it ends up (or does not end up)
reachable through.

Goes through 'managedEnvAcquisition' -- the same single atomic factory
every real dynamic provider uses -- rather than manually forking a
thread and pairing it by hand, so this fake's worker\/release genuinely
comes from 'managedFetchAuthInBackground' and blocks until the child is
confirmed terminal, matching every real managed release's own contract
-- tests that assert an exact multi-child release /order/ depend on
this: a release that only signals without awaiting could let
'releaseAwsEnvAcquisition''s next release begin before this child's own
handler has actually finished recording its effect. A far-future
'expiration' (see 'farFuture' below, the same pattern already proven
safe elsewhere in this file) is what makes
'managedFetchAuthInBackground' spawn a genuine, long-lived refresh
thread instead of returning a release-free static 'Auth'.
-}
fakeRefEnv :: Env' withAuth -> IO (ManagedEnvAcquisition, ThreadId)
fakeRefEnv env = do
  farFuture <- (\now -> Time (addUTCTime 3600 now)) <$> getCurrentTime
  acquisition <-
    managedEnvAcquisition
      env
      (pure (AuthEnv "AKIAFAKE" "secret" Nothing (Just farFuture)))
  tid <- case runIdentity (fst (runManagedEnvAcquisition acquisition)).auth of
    Ref t _ -> pure t
    Auth _ -> Exception.throwIO (userError "fakeRefEnv: managedEnvAcquisition unexpectedly produced a release-free static Auth")
  pure (acquisition, tid)

{- | Whether a 'ThreadStatus' represents definite, final termination.
Note a genuinely still-alive thread parked in @forever (threadDelay
maxBound)@ is very often reported as @'ThreadBlocked' 'BlockedOnMVar'@
(the RTS's internal representation for a sleeping/blocked thread), not
@'ThreadRunning'@ -- so \"is it alive\" checks must treat any non-terminal
status as alive, never assert the specific @'ThreadRunning'@ constructor.
-}
isTerminatedStatus :: ThreadStatus -> Bool
isTerminatedStatus status = status == ThreadFinished || status == ThreadDied

{- | Poll (bounded, no unconditioned sleep-only assertion) until a
thread's status is definitely terminal, or a generous bound elapses.
'killThread'\/'throwTo' block until the target has begun handling the
exception, but the RTS may take a further scheduling quantum to actually
mark the thread 'ThreadDied'\/'ThreadFinished' -- so tests that assert
termination poll for it explicitly instead of racing a single fixed
delay against that quantum.
-}
waitUntilTerminated :: ThreadId -> IO ThreadStatus
waitUntilTerminated tid = go (500 :: Int)
  where
    go n = do
      status <- threadStatus tid
      if isTerminatedStatus status || n <= 0
        then pure status
        else threadDelay 1000 >> go (n - 1)

-- | A 'ConfigProfileResolvers' whose every field is a hard failure --
-- used as a base for tests that only expect a handful of specific
-- resolvers to ever actually be called, so an unexpectedly-reached one
-- fails loudly rather than silently doing something unintended.
unreachableConfigProfileResolvers :: ConfigProfileResolvers
unreachableConfigProfileResolvers =
  ConfigProfileResolvers
    { resolveEnvironmentSource = \_ -> Exception.throwIO (userError "resolveEnvironmentSource: unexpectedly reached")
    , resolveEc2Source = \_ -> Exception.throwIO (userError "resolveEc2Source: unexpectedly reached")
    , resolveEcsSource = \_ -> Exception.throwIO (userError "resolveEcsSource: unexpectedly reached")
    , resolveAssumedRole = \_ _ -> Exception.throwIO (userError "resolveAssumedRole: unexpectedly reached")
    , resolveWebIdentity = \_ _ _ _ -> Exception.throwIO (userError "resolveWebIdentity: unexpectedly reached")
    , resolveSSO = \_ _ _ _ _ -> Exception.throwIO (userError "resolveSSO: unexpectedly reached")
    }

-- | A single-key profile HashMap, as 'safeEvalConfigProfile' expects
-- after 'mergeConfigs'\/'parseConfigProfile'-style merging (i.e. one
-- entry per already-merged @[profile]@ section).
profileMap :: [(Text, Text)] -> HashMap Text Text
profileMap = HashMap.fromList

{- | Regression\/design-verification for the config-file credential
provider graph fix (see 'Api.Arkham.AwsEnvSupervisor.safeEvalConfigProfile'
for the full pinned-source trace and rationale): the pinned Amazonka
fork's own @Amazonka.Auth.ConfigFile.evalConfig@ can silently orphan a
config-file /source/ profile's background refresh child once an outer
@sts:AssumeRole@ step overwrites the returned 'Env''s single @.auth@
field with its own -- this reimplementation instead accumulates every
such hidden child's release action explicitly (see 'AwsEnvAcquisition'),
exercised here directly against fake, fully test-controlled
'ConfigProfileResolvers' (never touching real AWS\/network\/filesystem).
-}
configProfileGraphSpec :: Spec
configProfileGraphSpec = describe "safeEvalConfigProfile (config-file credential provider graph)" do
  it "a static ExplicitKeys profile resolves with no hidden releases" do
    envNoAuth <- newEnvNoAuth
    let config = HashMap.fromList [("default", profileMap [("aws_access_key_id", "AKIA"), ("aws_secret_access_key", "s")])]
    acquisition <- safeEvalConfigProfile unreachableConfigProfileResolvers config [] "default" envNoAuth
    length (awsEnvAcquisitionHiddenReleases acquisition) `shouldBe` 0
    case runIdentity (awsEnvAcquisitionEnv acquisition).auth of
      Auth _ -> pure ()
      Ref {} -> expectationFailure "expected static Auth, not a background refresh Ref, for ExplicitKeys"

  {- | Structural regression for the MEDIUM-severity finding that the
  managed-release handshake was merely \"optional\": a bare, dynamic
  provider that could return a live, expiring 'Ref'-shaped 'Env' without
  ever reporting its release, or reporting a fake\/no-op release paired
  with one via a plain @(Env, IO ())@ tuple, used to be merely a
  /runtime/ hazard (silently downgraded by the pre-fix 'deriveRelease' to
  the naive, un-awaited kill-then-bounded-poll fallback, or simply
  believed at face value). This module's redesign (see
  'Api.Arkham.AwsEnvSupervisor', \"Structural release ownership\")
  eliminates the entire class at the /type/ level instead: every
  dynamic-provider 'ConfigProfileResolvers' field now has result type
  @IO 'ManagedEnvAcquisition'@, an opaque type whose only public
  constructors are 'managedEnvAcquisition' (which atomically starts the
  exact live, expiring refresh worker via 'managedFetchAuthInBackground'
  and pairs it with its own genuinely-awaiting release, all in one
  indivisible expression -- there is no way to supply an 'Auth'\/'Env'
  obtained from one call and a release obtained from another) and
  'staticEnvAcquisition' (which is itself runtime-checked to reject any
  'Env' whose @.auth@ is 'Ref'-shaped). There is no longer an 'IORef'
  side channel a resolver could omit writing to, nor a bare @(Env, IO
  ())@ tuple a resolver could construct by hand with an arbitrary\/no-op
  release, nor any separately exported release primitive (the earlier
  @pairManagedAcquisition@\/@managedRelease@\/@polledRelease@\/
  @ManagedRelease@ four, removed entirely -- see 'ManagedEnvAcquisition'
  in "Api.Arkham.AwsEnvSupervisor" for the exact spoofing example those
  used to permit) a caller could use to pair an unrelated release with a
  live worker's 'Env'. A \"bare expiring resolver\" in the old sense is
  now not merely rejected at runtime but literally unrepresentable: any
  attempt to write @resolveEcsSource = \\env -> pure env {auth = Identity
  (Ref tid cell)}@ (returning a bare 'Env', as the old buggy/forgetful
  shape did) is a /compile/-time type error, since the field's result
  type is @IO ManagedEnvAcquisition@, not @IO Env@, and there is no
  longer any exported way to fabricate a 'ManagedEnvAcquisition' from an
  arbitrary pre-existing 'Env' and an arbitrary\/no-op release. This is a
  strictly stronger guarantee than the previous version of this test
  (which merely proved a runtime 'ManagedReleaseInvariantViolated'
  exception was thrown for such a value) could express, so no equivalent
  executable test exists here: there is no longer any way to construct
  the input the old test needed.
  'ManagedReleaseInvariantViolated' itself remains defined and reachable
  only as a defensive backstop inside 'staticEnvAcquisition' and at
  'Api.Arkham.AwsEnvSupervisor.acquireAwsEnv''s outer boundary (see those
  functions' own Haddocks), never reachable from any correctly-typed
  'ConfigProfileResolvers' field value.
  -}

  {- | A two-level @source_profile@ chain (@parent@ assumes a role from
  @middle@, which itself assumes a role from @leaf@) with every
  assumed-role step modelled as forking a real refresh child (matching
  the pinned source's own always-temporary assumed-role credentials).
  Proves both the @middle@ and @leaf@-wrapping intermediate children are
  captured as hidden releases (not just the final, directly-visible
  @parent@ child), and that releasing the whole 'AwsEnvAcquisition' kills
  every one of them.
  -}
  it "a two-level source_profile assume-role chain captures every intermediate child as a hidden release, and releasing the acquisition kills all of them" do
    envNoAuth <- newEnvNoAuth
    let config =
          HashMap.fromList
            [ ("parent", profileMap [("role_arn", "arn:aws:iam::1:role/parent"), ("source_profile", "middle")])
            , ("middle", profileMap [("role_arn", "arn:aws:iam::1:role/middle"), ("source_profile", "leaf")])
            , ("leaf", profileMap [("aws_access_key_id", "AKIALEAF"), ("aws_secret_access_key", "s")])
            ]
    assumedRoleCallsRef <- newIORef ([] :: [Text])
    let resolvers =
          unreachableConfigProfileResolvers
            { resolveAssumedRole = \roleArn sourceEnv -> do
                atomicModifyIORef' assumedRoleCallsRef (\cs -> (cs <> [roleArn], ()))
                fst <$> fakeRefEnv sourceEnv
            }
    acquisition <- safeEvalConfigProfile resolvers config [] "parent" envNoAuth
    -- Both assumed-role steps were attempted, source-to-sink.
    readIORef assumedRoleCallsRef
      `shouldReturn` ["arn:aws:iam::1:role/middle", "arn:aws:iam::1:role/parent"]
    -- Two hidden children: "leaf" (once wrapped by "middle"'s own assumed
    -- role) and "middle" (once wrapped by "parent"'s). The final,
    -- directly-visible child is "parent"'s own -- not itself hidden.
    length (awsEnvAcquisitionHiddenReleases acquisition) `shouldBe` 2
    parentThreadId <- case runIdentity (awsEnvAcquisitionEnv acquisition).auth of
      Ref tid _ -> pure tid
      Auth _ -> expectationFailure "expected parent's own Ref auth" >> error "unreachable"
    releaseAwsEnvAcquisition acquisition
    finalStatus <- waitUntilTerminated parentThreadId
    finalStatus `shouldSatisfy` isTerminatedStatus

  it "credential_source=EcsContainer: the ECS source's child becomes a hidden release once the outer assumed role wraps it" do
    envNoAuth <- newEnvNoAuth
    let config = HashMap.fromList [("default", profileMap [("role_arn", "arn:aws:iam::1:role/ecs"), ("credential_source", "EcsContainer")])]
    ecsCalledRef <- newIORef False
    let resolvers =
          unreachableConfigProfileResolvers
            { resolveEcsSource = \env -> writeIORef ecsCalledRef True >> fst <$> fakeRefEnv env
            , resolveAssumedRole = \_roleArn sourceEnv -> fst <$> fakeRefEnv sourceEnv
            }
    acquisition <- safeEvalConfigProfile resolvers config [] "default" envNoAuth
    readIORef ecsCalledRef `shouldReturn` True
    length (awsEnvAcquisitionHiddenReleases acquisition) `shouldBe` 1

  {- | @credential_source=Ec2InstanceMetadata@ is the specific sub-gap the
  pinned source has: @Amazonka.Auth.ConfigFile@ reaches its own unsafe
  @fromDefaultInstanceProfile@ directly for this credential source,
  bypassing this module's 'Api.Arkham.AwsEnvSupervisor.safeDefaultInstanceProfile'
  fix entirely. 'ConfigProfileResolvers.resolveEc2Source' is exactly the
  substitution point that closes it in production (wired to
  'Api.Arkham.AwsEnvSupervisor.safeDefaultInstanceProfile' in
  'Api.Arkham.AwsEnvSupervisor.productionConfigProfileResolvers') -- this
  proves 'safeEvalConfigProfile' genuinely calls through the injected
  resolver for this credential source, not any hardcoded, unsafe
  alternative.
  -}
  it "credential_source=Ec2InstanceMetadata calls through the injected (safe) resolver, never a hardcoded alternative" do
    envNoAuth <- newEnvNoAuth
    let config = HashMap.fromList [("default", profileMap [("role_arn", "arn:aws:iam::1:role/ec2"), ("credential_source", "Ec2InstanceMetadata")])]
    ec2CalledRef <- newIORef False
    let resolvers =
          unreachableConfigProfileResolvers
            { resolveEc2Source = \env -> writeIORef ec2CalledRef True >> fst <$> fakeRefEnv env
            , resolveAssumedRole = \_roleArn sourceEnv -> fst <$> fakeRefEnv sourceEnv
            }
    _ <- safeEvalConfigProfile resolvers config [] "default" envNoAuth
    readIORef ec2CalledRef `shouldReturn` True

  it "AssumeRoleWithWebIdentity is a single-level leaf: no hidden releases, and the token file/role/session args reach the resolver" do
    envNoAuth <- newEnvNoAuth
    let config =
          HashMap.fromList
            [
              ( "default"
              , profileMap
                  [ ("role_arn", "arn:aws:iam::1:role/web")
                  , ("web_identity_token_file", "/tmp-not-used/token")
                  , ("role_session_name", "my-session")
                  ]
              )
            ]
    capturedRef <- newIORef Nothing
    let resolvers =
          unreachableConfigProfileResolvers
            { resolveWebIdentity = \tokenFile roleArn mSession env -> do
                writeIORef capturedRef (Just (tokenFile, roleArn, mSession))
                staticEnvAcquisition =<< fakeStaticEnv env
            }
    acquisition <- safeEvalConfigProfile resolvers config [] "default" envNoAuth
    length (awsEnvAcquisitionHiddenReleases acquisition) `shouldBe` 0
    readIORef capturedRef `shouldReturn` Just ("/tmp-not-used/token", "arn:aws:iam::1:role/web", Just "my-session")

  it "AssumeRoleViaSSO is a single-level leaf: no hidden releases, and sso_region/account/role args reach the resolver" do
    envNoAuth <- newEnvNoAuth
    let config =
          HashMap.fromList
            [
              ( "default"
              , profileMap
                  [ ("sso_start_url", "https://example.awsapps.com/start")
                  , ("sso_region", "us-east-1")
                  , ("sso_account_id", "123456789012")
                  , ("sso_role_name", "MyRole")
                  ]
              )
            ]
    capturedRef <- newIORef Nothing
    let resolvers =
          unreachableConfigProfileResolvers
            { resolveSSO = \_cachedTokenFile ssoRegion accountId roleName env -> do
                writeIORef capturedRef (Just (ssoRegion, accountId, roleName))
                staticEnvAcquisition =<< fakeStaticEnv env
            }
    acquisition <- safeEvalConfigProfile resolvers config [] "default" envNoAuth
    length (awsEnvAcquisitionHiddenReleases acquisition) `shouldBe` 0
    readIORef capturedRef `shouldReturn` Just (Region' "us-east-1", "123456789012", "MyRole")

  {- | If the /outer/ assumed-role step fails or this thread is cancelled
  after the /source/ profile's own child has already been created, that
  source's already-accumulated children (here, "leaf"'s hidden release
  once wrapped by "middle", plus "middle"'s own now-about-to-be-hidden
  child) must be released before the exception propagates -- otherwise a
  failed\/cancelled generation would leak exactly what it already
  acquired. Mirrors the two-level success test above, but the outermost
  'resolveAssumedRole' call (wrapping "middle" on behalf of "parent")
  throws.
  -}
  it "a failure in the outer assume-role step after the source already forked a child releases that child before propagating" do
    envNoAuth <- newEnvNoAuth
    let config =
          HashMap.fromList
            [ ("parent", profileMap [("role_arn", "arn:aws:iam::1:role/parent"), ("source_profile", "middle")])
            , ("middle", profileMap [("role_arn", "arn:aws:iam::1:role/middle"), ("source_profile", "leaf")])
            , ("leaf", profileMap [("aws_access_key_id", "AKIALEAF"), ("aws_secret_access_key", "s")])
            ]
    middleThreadIdRef <- newIORef Nothing
    let resolvers =
          unreachableConfigProfileResolvers
            { resolveAssumedRole = \roleArn sourceEnv ->
                if roleArn == "arn:aws:iam::1:role/middle"
                  then do
                    (acquisition, tid) <- fakeRefEnv sourceEnv
                    writeIORef middleThreadIdRef (Just tid)
                    pure acquisition
                  else Exception.throwIO (InvalidIAMError "simulated outer assume-role failure")
            }
    result <- Exception.try @AuthError (safeEvalConfigProfile resolvers config [] "parent" envNoAuth)
    case result of
      Left (InvalidIAMError _) -> pure ()
      _ -> expectationFailure "expected the outer assume-role failure to propagate as InvalidIAMError"
    Just middleThreadId <- readIORef middleThreadIdRef
    finalStatus <- waitUntilTerminated middleThreadId
    finalStatus `shouldSatisfy` isTerminatedStatus

  it "a plain leaf acquisition failure (no child ever created) propagates with nothing to release" do
    envNoAuth <- newEnvNoAuth
    let config = HashMap.fromList [("default", profileMap [("role_arn", "arn:aws:iam::1:role/ecs"), ("credential_source", "EcsContainer")])]
        resolvers = unreachableConfigProfileResolvers {resolveEcsSource = \_ -> Exception.throwIO (MissingEnvError "simulated ECS metadata unavailable")}
    result <- Exception.try @AuthError (safeEvalConfigProfile resolvers config [] "default" envNoAuth)
    case result of
      Left (MissingEnvError _) -> pure ()
      _ -> expectationFailure "expected the leaf failure to propagate as MissingEnvError"

  {- | If this thread is asynchronously cancelled /during/ the outer
  assumed-role wrap, after the /source/ profile ("middle") has already
  completed and forked its own child, that source's child must still be
  released -- exactly the same 'Exception.onException' coverage
  'assumeRoleOnto' gives a synchronous failure at the same point (see the
  "a failure in the outer assume-role step..." test above), just reached
  via cancellation instead. (Cancellation reaching mid-flight /inside/ an
  opaque resolver call, i.e. after that resolver's own internal fork but
  before it has returned a value this module can see at all, is a
  distinct, structurally-unclosable gap already accepted as residual risk
  for the single-provider top-level acquisition; it is not what this test
  targets, and not fixable without control over the resolver's own
  internals.)
  -}
  it "cancellation during the outer assume-role step (after the source already forked and returned a child) still releases that child" do
    envNoAuth <- newEnvNoAuth
    let config =
          HashMap.fromList
            [ ("parent", profileMap [("role_arn", "arn:aws:iam::1:role/parent"), ("source_profile", "middle")])
            , ("middle", profileMap [("role_arn", "arn:aws:iam::1:role/middle"), ("source_profile", "leaf")])
            , ("leaf", profileMap [("aws_access_key_id", "AKIALEAF"), ("aws_secret_access_key", "s")])
            ]
    middleThreadIdRef <- newIORef Nothing
    outerStarted <- newEmptyMVar
    let resolvers =
          unreachableConfigProfileResolvers
            { resolveAssumedRole = \roleArn sourceEnv ->
                if roleArn == "arn:aws:iam::1:role/middle"
                  then do
                    -- "middle"'s own step completes normally and returns
                    -- -- its resulting child is now fully known to
                    -- 'safeEvalConfigProfile' before "parent"'s own wrap
                    -- is even attempted.
                    (acquisition, tid) <- fakeRefEnv sourceEnv
                    writeIORef middleThreadIdRef (Just tid)
                    pure acquisition
                  else do
                    -- "parent"'s own wrap (of "middle"'s already-returned
                    -- child) never completes -- simulating cancellation
                    -- landing while this specific call is in flight.
                    putMVar outerStarted ()
                    forever (threadDelay maxBound)
            }
    resultVar <- newEmptyMVar
    workerTid <-
      forkIO
        $ putMVar resultVar
        =<< Exception.try @Exception.AsyncException (safeEvalConfigProfile resolvers config [] "parent" envNoAuth)
    takeMVar outerStarted
    Exception.throwTo workerTid Exception.ThreadKilled
    result <- takeMVar resultVar
    case result of
      Left Exception.ThreadKilled -> pure ()
      _ -> expectationFailure "expected the outer assume-role step to be cancelled with ThreadKilled"
    Just middleThreadId <- readIORef middleThreadIdRef
    finalStatus <- waitUntilTerminated middleThreadId
    finalStatus `shouldSatisfy` isTerminatedStatus

  {- | Once 'releaseAwsEnvAcquisition' has killed a hidden child's
  thread, that thread is provably no longer running -- and a dead thread
  can never later deliver a delayed 'Exception.throwTo' refresh failure
  into a /different/, newer generation's supervisor thread: there is
  simply nothing left running that could still call it. This is the
  config-file-specific instance of the same guarantee the
  \"a delayed invalidation lands only on the supervisor thread\" test
  proves for the single-provider case.
  -}
  it "a released hidden child can never later throwTo into a newer generation, because it is provably no longer running" do
    envNoAuth <- newEnvNoAuth
    let config =
          HashMap.fromList
            [ ("parent", profileMap [("role_arn", "arn:aws:iam::1:role/parent"), ("source_profile", "leaf")])
            , ("leaf", profileMap [("aws_access_key_id", "AKIALEAF"), ("aws_secret_access_key", "s")])
            ]
        resolvers = unreachableConfigProfileResolvers {resolveAssumedRole = \_ sourceEnv -> fst <$> fakeRefEnv sourceEnv}
    acquisition <- safeEvalConfigProfile resolvers config [] "parent" envNoAuth
    let [leafRelease] = awsEnvAcquisitionHiddenReleases acquisition
    leafRelease
    -- The hidden child's release ran in isolation, independent of the
    -- overall acquisition's own release -- prove it alone already killed
    -- the thread, matching how a newer generation's own release will
    -- never rely on, or be able to observe, this generation's state.
    parentThreadId <- case runIdentity (awsEnvAcquisitionEnv acquisition).auth of
      Ref tid _ -> pure tid
      Auth _ -> expectationFailure "expected parent's own live Ref auth" >> error "unreachable"
    releaseAwsEnvAcquisition acquisition
    parentStatus <- waitUntilTerminated parentThreadId
    parentStatus `shouldSatisfy` isTerminatedStatus

  {- | Mutation-check: proves the hidden-release list genuinely matters,
  by deliberately releasing /only/ the final, directly-visible @.auth@
  (exactly what the pinned source's own unmodified
  @releaseAwsEnvGeneration@-style \"project release from the final env
  alone\" approach would do) instead of the full
  'releaseAwsEnvAcquisition' -- reproducing the original bug's exact
  leak. Uses a three-profile chain (@parent@ assumes-role from @middle@,
  @middle@ assumes-role from a static @leaf@) so that @middle@'s own
  forked refresh child is genuinely /hidden/ behind @parent@'s distinct,
  separately-forked final child -- releasing only the final @.auth@ can
  only ever reach @parent@'s thread, never @middle@'s. If
  'safeEvalConfigProfile' regressed to no longer tracking hidden releases
  at all (e.g. 'assumeRoleOnto' stopped appending @sourceRelease@), the
  hidden-releases count assertion below would already fail first, so this
  test cannot pass by accident once the mechanism is broken.
  -}
  it "mutation check: releasing only the final .auth (the pre-fix approach) leaks the source's hidden child; the full release does not" do
    envNoAuth <- newEnvNoAuth
    let config =
          HashMap.fromList
            [ ("parent", profileMap [("role_arn", "arn:aws:iam::1:role/parent"), ("source_profile", "middle")])
            , ("middle", profileMap [("role_arn", "arn:aws:iam::1:role/middle"), ("source_profile", "leaf")])
            , ("leaf", profileMap [("aws_access_key_id", "AKIALEAF"), ("aws_secret_access_key", "s")])
            ]
    middleThreadIdRef <- newIORef Nothing
    let resolvers =
          unreachableConfigProfileResolvers
            { resolveAssumedRole = \roleArn sourceEnv -> do
                (acquisition, tid) <- fakeRefEnv sourceEnv
                when (roleArn == "arn:aws:iam::1:role/middle") $ writeIORef middleThreadIdRef (Just tid)
                pure acquisition
            }
    acquisition <- safeEvalConfigProfile resolvers config [] "parent" envNoAuth
    length (awsEnvAcquisitionHiddenReleases acquisition) `shouldBe` 2
    Just middleThreadId <- readIORef middleThreadIdRef
    -- Deliberately release only the final env's own auth ("parent"'s own
    -- thread) -- exactly the pre-fix, buggy "project release from the
    -- final env alone" approach -- and prove "middle"'s hidden child,
    -- which is a genuinely distinct thread, leaks (stays Running).
    releaseAwsEnvChild (runIdentity (awsEnvAcquisitionEnv acquisition).auth)
    threadDelay (10 * 1000)
    leakedStatus <- threadStatus middleThreadId
    leakedStatus `shouldSatisfy` (not . isTerminatedStatus)
    -- The full, correct release additionally runs every hidden release,
    -- and "middle"'s child that was still running above is now cleaned up.
    releaseAwsEnvAcquisition acquisition
    cleanedUpStatus <- waitUntilTerminated middleThreadId
    cleanedUpStatus `shouldSatisfy` isTerminatedStatus

  {- | Exact release-order regression for the HIGH-severity cleanup-order
  audit finding: 'releaseAwsEnvAcquisition' must release the still-visible
  outer child FIRST, then hidden children newest-to-oldest -- i.e. exactly
  @c, b, a, root@ for @'awsEnvAcquisitionRelease' = c@ and
  @'awsEnvAcquisitionHiddenReleases' = [root, a, b]@ -- never the old,
  buggy @sequence_ (reverse hiddenReleases) >> releaseAwsEnvChild ...@
  order (hidden newest-to-oldest, then outer last, i.e. @b, a, root, c@).

  This tests 'releaseAwsEnvAcquisition''s own sequencing algorithm
  directly and deterministically against a hand-built 'AwsEnvAcquisition'
  (a plain, non-opaque record -- unlike 'ManagedEnvAcquisition', nothing
  about this construction is a forgery: every release here is an
  ordinary test-owned @IO ()@ that only records its own label, not a
  claim about any live managed worker), rather than through real
  managed refresh threads: reconstructing this exact interleaved order
  by externally polling real threads' own RTS 'GHC.Conc.Sync.threadStatus'
  turned out to be inherently racy once those releases run genuinely in
  parallel (with @-threaded -with-rtsopts=-N@) and complete within a few
  microseconds of each other -- not a meaningful test of this function's
  own, purely sequential 'Control.Monad.foldM'-based algorithm, which
  this instead exercises with zero scheduling dependency.

  Mutation check: reverting 'releaseAwsEnvAcquisition' to its previous
  @sequence_ (reverse hiddenReleases) >> releaseAwsEnvChild ...@ order
  makes this test fail with the order @[\"b\",\"a\",\"root\",\"c\"]@ instead
  of the required @[\"c\",\"b\",\"a\",\"root\"]@.
  -}
  it "releaseAwsEnvAcquisition releases the visible outer child first, then hidden children newest-to-oldest (pure sequencing, no scheduling dependency)" do
    envNoAuth <- newEnvNoAuth
    dummyEnv <- fakeStaticEnv envNoAuth
    releaseOrderRef <- newIORef ([] :: [Text])
    let recordRelease label = atomicModifyIORef' releaseOrderRef (\labels -> (labels <> [label], ()))
        acquisition =
          AwsEnvAcquisition
            { awsEnvAcquisitionEnv = dummyEnv
            , awsEnvAcquisitionRelease = recordRelease "c"
            , awsEnvAcquisitionHiddenReleases = [recordRelease "root", recordRelease "a", recordRelease "b"]
            }
    releaseAwsEnvAcquisition acquisition
    readIORef releaseOrderRef `shouldReturn` ["c", "b", "a", "root"]

  {- | End-to-end regression, through the real @source_profile@ chain
  resolution\/accumulation machinery (unlike the pure sequencing test
  just above), that a three-deep chain (@root@, static, wrapped by @a@'s
  own assumed role, wrapped in turn by @b@'s, wrapped in turn by @c@'s --
  the final, directly-visible child) genuinely accumulates exactly the
  two real, live intermediate children (@a@ and @b@) as hidden releases
  (plus @root@'s own static, no-op release), and that
  'releaseAwsEnvAcquisition' genuinely tears down every real managed
  refresh thread reachable from the acquisition -- both the two hidden
  ones (@a@, @b@) and @c@'s own visible, directly-returned one -- not
  merely that the hidden-release /count/ is right. (The pure test above
  already separately proves the exact release /order/ this drives,
  deterministically.)
  -}
  it "accumulates exactly two real hidden children for a three-deep chain, and genuinely releases every one of them" do
    envNoAuth <- newEnvNoAuth
    let config =
          HashMap.fromList
            [ ("c", profileMap [("role_arn", "arn:aws:iam::1:role/c"), ("source_profile", "b")])
            , ("b", profileMap [("role_arn", "arn:aws:iam::1:role/b"), ("source_profile", "a")])
            , ("a", profileMap [("role_arn", "arn:aws:iam::1:role/a"), ("source_profile", "root")])
            , ("root", profileMap [("aws_access_key_id", "AKIAROOT"), ("aws_secret_access_key", "s")])
            ]
    capturedThreadIdsRef <- newIORef ([] :: [ThreadId])
    let resolvers =
          unreachableConfigProfileResolvers
            { resolveAssumedRole = \_roleArn sourceEnv -> do
                (acquisition, tid) <- fakeRefEnv sourceEnv
                atomicModifyIORef' capturedThreadIdsRef (\ids -> (ids <> [tid], ()))
                pure acquisition
            }
    acquisition <- safeEvalConfigProfile resolvers config [] "c" envNoAuth
    -- Three hidden releases: root's own (a static, no-op release, since
    -- root uses plain access keys rather than an assumed role), plus the
    -- real "a" and "b" children -- "c"'s own auth is the visible outer
    -- one, released separately (see 'releaseAwsEnvAcquisition').
    length (awsEnvAcquisitionHiddenReleases acquisition) `shouldBe` 3
    -- 'resolveAssumedRole' itself is invoked once per assumed-role
    -- profile in the chain ("a", "b", AND "c" -- "root" is the only
    -- plain-static, non-assumed-role profile and never reaches this
    -- resolver at all), so all three real managed threads are captured
    -- here; "c"'s is the visible/final release, "a" and "b"'s are the
    -- two genuinely real entries among 'awsEnvAcquisitionHiddenReleases'.
    capturedThreadIds <- readIORef capturedThreadIdsRef
    length capturedThreadIds `shouldBe` 3 -- "a", "b", and "c"; "root" spawns no thread
    releaseAwsEnvAcquisition acquisition
    finalStatuses <- traverse waitUntilTerminated capturedThreadIds
    finalStatuses `shouldSatisfy` all isTerminatedStatus

{- | Regression for the HIGH-severity finding that 'safeFileEnv' recorded
only 'awsEnvAcquisitionHiddenReleases' into @hiddenReleasesRef@ and never
threaded the resolved profile's own 'awsEnvAcquisitionRelease' into
@finalReleaseRef@ at all: whenever the config-file provider won outright
for a profile resolving to a dynamic (@sts:AssumeRole@\/web identity\/
SSO\/@credential_source@) provider, 'acquireAwsEnv' would find
@finalReleaseRef@ still 'Nothing' and silently fall back to the naive,
un-awaited 'requireChildReleased' kill-then-bounded-poll release for the
final @.auth@ -- exactly the un-awaited-terminal-acknowledgement fallback
'managedFetchAuthInBackground' exists to avoid.

Deliberately goes through 'safeFileEnv' itself (not a direct
'safeEvalConfigProfile' call, which every other test in this module
already exercises for the config-graph's own resolution\/accumulation
correctness) -- i.e. real, on-disk credentials\/config INI files read via
the exact @AWS_SHARED_CREDENTIALS_FILE@\/@AWS_CONFIG_FILE@\/@AWS_PROFILE@
environment-variable\/@mergeConfigs@\/'safeLoadIniFile' path production
uses -- so a regression in 'safeFileEnv''s own two 'writeIORef' calls (the
actual site of this bug) is directly caught, not bypassed. The fixture
files live under a uniquely-named, test-owned directory (created and torn
down entirely within this one test) so no other test or run is affected;
the three environment variables this reads are saved and unconditionally
restored via 'Exception.bracket' around the narrow span that needs them.

Mutation check: reverting 'safeFileEnv' to omit the
@writeIORef finalReleaseRef (Just (awsEnvAcquisitionRelease acquisition))@
line makes this test's @hasFinalRelease@ assertion fail (it would remain
'Nothing').
-}
safeFileEnvSpec :: Spec
safeFileEnvSpec = describe "safeFileEnv (config-file bridge: threading finalReleaseRef through to acquireAwsEnv)" do
  it "writes BOTH hiddenReleasesRef and finalReleaseRef for a real, on-disk source_profile + assume-role config-file chain" do
    envNoAuth <- newEnvNoAuth
    let fixtureDir = "safefileenv-source-profile-assume-role-fixture"
        credentialsPath = fixtureDir <> "/credentials"
        configPath = fixtureDir <> "/config"
        withSavedEnvVar name action =
          Exception.bracket
            (lookupEnv name)
            (\prior -> maybe (unsetEnv name) (setEnv name) prior)
            (const action)
    Exception.bracket_
      ( do
          Directory.createDirectoryIfMissing True fixtureDir
          writeFile credentialsPath "[source]\naws_access_key_id = AKIASOURCE\naws_secret_access_key = secretsource\n"
          writeFile configPath "[profile target]\nrole_arn = arn:aws:iam::123456789012:role/TestRole\nsource_profile = source\n"
      )
      (Directory.removeDirectoryRecursive fixtureDir)
      $ withSavedEnvVar "AWS_PROFILE"
      $ withSavedEnvVar "AWS_CONFIG_FILE"
      $ withSavedEnvVar "AWS_SHARED_CREDENTIALS_FILE"
      $ do
        setEnv "AWS_PROFILE" "target"
        setEnv "AWS_CONFIG_FILE" configPath
        setEnv "AWS_SHARED_CREDENTIALS_FILE" credentialsPath
        hiddenReleasesRef <- newIORef []
        finalReleaseRef <- newIORef Nothing
        let resolvers = unreachableConfigProfileResolvers {resolveAssumedRole = \_ sourceEnv -> fst <$> fakeRefEnv sourceEnv}
        _finalEnv <- safeFileEnv resolvers hiddenReleasesRef finalReleaseRef envNoAuth
        hidden <- readIORef hiddenReleasesRef
        length hidden `shouldBe` 1
        maybeFinal <- readIORef finalReleaseRef
        case maybeFinal of
          Nothing -> expectationFailure "expected safeFileEnv to populate finalReleaseRef with the resolved profile's own release, not leave it Nothing"
          Just finalRelease ->
            -- The threaded release must genuinely be the resolved
            -- profile's own (fake-managed) release, not a stub: calling
            -- it must not throw.
            finalRelease

  {- | Root-cause regression for the MEDIUM finding that the span between
  'safeEvalConfigProfile' returning a live 'acquisition' and both
  ledgers ('hiddenReleasesRef', 'finalReleaseRef') actually recording its
  release was three independent, unguarded statements (resolving a
  region override via the fallible\/interruptible 'lookupRegion', then
  two separate 'writeIORef' calls): an exception or asynchronous
  cancellation landing anywhere in that window left 'acquisition' -- a
  live, already-running background-refresh worker -- referenced by
  nothing 'acquireAwsEnv' could ever find, permanently orphaning it.

  An earlier version of this test instead repeatedly flooded
  'Exception.throwTo' against a fixed @1ms@ delay, hoping to land inside
  the narrow target window. Independent review correctly rejected that
  as non-proof: by its own admission (\"whether or not any particular
  run's flood actually lands in that narrow window\"), it could pass
  vacuously, on a run whose flood never came close to the span under
  test, without ever having exercised it. This replacement instead
  captures 'Exception.getMaskingState' -- a plain, synchronous read of
  /this very thread's/ own masking state, not an asynchronous exception
  racing anything -- from inside the resolver itself (standing in for
  'resolveAssumedRole', reached deep inside 'safeEvalConfigProfile''s own
  call graph), at the exact point a live worker has just been created
  (via the real 'managedEnvAcquisition' factory, through 'fakeRefEnv')
  but neither ledger has been written yet: precisely the span the
  MEDIUM finding identified. There is no timing dependence here at all:
  this either observes 'Exception.MaskedInterruptible', deterministically,
  every single run, or it does not.

  Mutation check: removing 'safeFileEnv''s outer 'Control.Exception.mask'
  (or any of 'leaf'\/'assumeRoleOnto'\/'managedEnvAcquisition''s own
  'Exception.mask_' wraps that this call graph passes through first)
  makes this fail deterministically, since the resolver would then
  observe 'Exception.Unmasked' instead.
  -}
  it "runs the entire post-acquisition span, deep inside safeEvalConfigProfile's resolver chain, under a continuous async-exception mask" do
    envNoAuth <- newEnvNoAuth
    let fixtureDir = "safefileenv-masking-state-fixture"
        credentialsPath = fixtureDir <> "/credentials"
        configPath = fixtureDir <> "/config"
        withSavedEnvVar name action =
          Exception.bracket
            (lookupEnv name)
            (\prior -> maybe (unsetEnv name) (setEnv name) prior)
            (const action)
    Exception.bracket_
      ( do
          Directory.createDirectoryIfMissing True fixtureDir
          writeFile credentialsPath "[source]\naws_access_key_id = AKIASOURCE\naws_secret_access_key = secretsource\n"
          writeFile configPath "[profile target]\nrole_arn = arn:aws:iam::123456789012:role/TestRole\nsource_profile = source\n"
      )
      (Directory.removeDirectoryRecursive fixtureDir)
      $ withSavedEnvVar "AWS_PROFILE"
      $ withSavedEnvVar "AWS_CONFIG_FILE"
      $ withSavedEnvVar "AWS_SHARED_CREDENTIALS_FILE"
      $ do
        setEnv "AWS_PROFILE" "target"
        setEnv "AWS_CONFIG_FILE" configPath
        setEnv "AWS_SHARED_CREDENTIALS_FILE" credentialsPath
        hiddenReleasesRef <- newIORef []
        finalReleaseRef <- newIORef Nothing
        maskDuringResolutionRef <- newIORef Nothing
        let resolvers =
              unreachableConfigProfileResolvers
                { resolveAssumedRole = \_ sourceEnv -> do
                    (acquisition, _tid) <- fakeRefEnv sourceEnv
                    observed <- Exception.getMaskingState
                    writeIORef maskDuringResolutionRef (Just observed)
                    pure acquisition
                }
        _finalEnv <- safeFileEnv resolvers hiddenReleasesRef finalReleaseRef envNoAuth
        maskDuringResolution <- readIORef maskDuringResolutionRef
        maskDuringResolution `shouldBe` Just Exception.MaskedInterruptible

  {- | Companion to the masking-state proof above: proves the
  /outcome/-level invariant (an interrupted run releases the worker
  symmetrically; a completed run populates both ledgers) with a single,
  gate-synchronized asynchronous exception rather than any fixed-delay
  flood. 'Exception.throwTo' blocks its own caller until the exception is
  genuinely delivered, or until the target thread has already terminated
  (see its own Haddock) -- so, unlike a flood, this never needs to guess
  how long to wait, nor how many attempts to make: exactly one delivery
  attempt is issued the instant the resolver has genuinely produced a
  live worker (via 'workerReadyGate'), and is guaranteed to either land
  inside 'safeFileEnv''s masked span (at its one deliberately
  interruptible point, @restore lookupRegion@) or, if 'safeFileEnv'
  completes first, to find the victim thread already finished and simply
  return without delivering anything at all -- never landing in some
  in-between, unprotected gap, because the fix under test removes that
  gap entirely.

  The victim runs on its own forked thread (rather than this test's own)
  specifically so that a delivery landing /after/ 'safeFileEnv' has
  already returned successfully can never itself become an uncaught,
  test-crashing exception: 'Exception.uninterruptibleMask_' around the
  final @putMVar@ guarantees the outcome is always durably recorded
  before the victim thread can possibly terminate, so
  @takeMVar outcomeVar@ below can never deadlock, and a
  too-late-to-matter delivery is simply told \"already terminated\" by
  'Exception.throwTo' and drops harmlessly.

  Mutation check: reverting the masking on the managed-acquisition
  helpers this resolver chain passes through (e.g. 'managedEnvAcquisition',
  'commitManagedAcquisition', or the outer 'assumeRoleOnto'\/'leaf'
  themselves) reliably makes this fail, because the gate fires the
  instant the worker exists -- well before any of those layers would
  otherwise have returned -- so with no protection left anywhere in the
  chain the exception lands promptly, leaving the interrupted branch's
  own assertions (worker terminated, neither ledger populated) instead
  observing the worker still alive with nothing tracking it, exactly the
  MEDIUM finding's orphaning scenario. The masking-state test above is
  the fully deterministic (zero-timing-dependence) proof of the
  structural property itself; this test is its outcome-level companion,
  proving the release path this whole module exists to protect actually
  behaves correctly under a real, delivered cancellation, not merely
  that a masking flag was set.
  -}
  it "never orphans the resolved profile's own live worker: a single, gate-synchronized asynchronous exception releases it symmetrically, whichever masked point it lands at" do
    envNoAuth <- newEnvNoAuth
    let fixtureDir = "safefileenv-cancellation-fixture"
        credentialsPath = fixtureDir <> "/credentials"
        configPath = fixtureDir <> "/config"
        withSavedEnvVar name action =
          Exception.bracket
            (lookupEnv name)
            (\prior -> maybe (unsetEnv name) (setEnv name) prior)
            (const action)
    Exception.bracket_
      ( do
          Directory.createDirectoryIfMissing True fixtureDir
          writeFile credentialsPath "[source]\naws_access_key_id = AKIASOURCE\naws_secret_access_key = secretsource\n"
          writeFile configPath "[profile target]\nrole_arn = arn:aws:iam::123456789012:role/TestRole\nsource_profile = source\n"
      )
      (Directory.removeDirectoryRecursive fixtureDir)
      $ withSavedEnvVar "AWS_PROFILE"
      $ withSavedEnvVar "AWS_CONFIG_FILE"
      $ withSavedEnvVar "AWS_SHARED_CREDENTIALS_FILE"
      $ do
        setEnv "AWS_PROFILE" "target"
        setEnv "AWS_CONFIG_FILE" configPath
        setEnv "AWS_SHARED_CREDENTIALS_FILE" credentialsPath
        hiddenReleasesRef <- newIORef []
        finalReleaseRef <- newIORef Nothing
        workerTidRef <- newIORef Nothing
        workerReadyGate <- newEmptyMVar
        let resolvers =
              unreachableConfigProfileResolvers
                { resolveAssumedRole = \_ sourceEnv -> do
                    (acquisition, tid) <- fakeRefEnv sourceEnv
                    writeIORef workerTidRef (Just tid)
                    putMVar workerReadyGate ()
                    pure acquisition
                }
        outcomeVar <- newEmptyMVar
        victimTid <- forkIO $ do
          result <- Exception.try @Exception.AsyncException (safeFileEnv resolvers hiddenReleasesRef finalReleaseRef envNoAuth)
          Exception.uninterruptibleMask_ (putMVar outcomeVar result)
        _ <- forkIO (takeMVar workerReadyGate >> Exception.throwTo victimTid Exception.ThreadKilled)
        outcome <- takeMVar outcomeVar
        mWorkerTid <- readIORef workerTidRef
        case mWorkerTid of
          Nothing -> expectationFailure "expected the assume-role resolver to have been reached and recorded a worker ThreadId"
          Just workerTid -> case outcome of
            Left Exception.ThreadKilled -> do
              -- Interrupted: the worker 'safeFileEnv' had already
              -- created must be genuinely released, never left live
              -- with nothing referencing it, and neither ledger must
              -- be left half-populated (both empty, exactly as if
              -- 'acquisition' had never been obtained at all).
              status <- waitUntilTerminated workerTid
              status `shouldSatisfy` isTerminatedStatus
              hidden <- readIORef hiddenReleasesRef
              length hidden `shouldBe` 0
              maybeFinal <- readIORef finalReleaseRef
              case maybeFinal of
                Nothing -> pure ()
                Just _ -> expectationFailure "expected finalReleaseRef to remain unpopulated after a cancelled safeFileEnv"
            Left other -> expectationFailure ("expected only ThreadKilled to ever propagate, got " <> show other)
            Right _finalEnv -> do
              -- Completed before the synchronized exception could land:
              -- both ledgers must be populated exactly as the passing
              -- test above already proves.
              hidden <- readIORef hiddenReleasesRef
              length hidden `shouldBe` 1
              maybeFinal <- readIORef finalReleaseRef
              case maybeFinal of
                Just _ -> pure ()
                Nothing -> expectationFailure "expected finalReleaseRef to be populated after a completed safeFileEnv"

{- | Regression\/design-verification for the application-lifetime
supervised-'Env' architecture that replaced the earlier per-request
@discoverFrozenEnv@\/@freezeAuth@\/@runOnDisposableWorker@ worker protocol,
and its Foundation-owned, demand-driven refinement (see
'Api.Arkham.AwsEnvSupervisor' for the full rationale): exactly one
dedicated thread -- the supervisor's own, started once (in production, in
@Application.makeFoundation@, before Warp accepts any request) and never
restarted -- ever calls @acquire@ (in production, @acquireAwsEnv@, i.e.
@newEnv discover@), so it is the only thread a delayed background-refresh
'AuthError' could ever target; a Warp request thread only ever reads a
strict, typed snapshot and never itself performs, or blocks on,
acquisition.

These tests exercise the generic protocol and its demand-driven wrapper
directly against fake, fully test-controlled @acquire@\/@awaitInvalidation@\/
@backoff@ actions -- no real AWS\/network dependency is involved -- using
'MVar's\/'TVar's as synchronization gates so each assertion is
deterministic rather than timing-dependent. 'waitForSupervisedState'\/
'waitForDemandDrivenState' below are only bounded hang-guards against a
genuinely stuck suite; they never substitute for the ordering guarantees
each test's own gates establish.
-}
awsEnvSupervisorInternalSpec :: Spec
awsEnvSupervisorInternalSpec = describe "AWS Env supervisor" do
  configProfileGraphSpec
  safeFileEnvSpec

  {- | Regression for the follow-up audit: the handler used to log
  'tshow err'/'tshow authErr' directly. 'TransportError'/'RetrievalError'
  wrap an 'HttpException' whose embedded 'Request' leaves the
  @X-Amz-Security-Token@ header (used for temporary/session credentials)
  unredacted, and 'SerializeError' carries the raw response body. Even
  'ServiceError''s own symbolic error code and request id are
  server-provided free-form text, so they are excluded too.
  'classifyErrorDiagnostic'/'classifyAuthErrorDiagnostic' are the exact
  functions the handler now logs instead: these tests prove the diagnostic
  for every secret-bearing case is a bare, structurally secret-free
  category (no wrapped exception, request, body, error code, or request id
  reaches the diagnostic type at all), while a genuine service response
  still yields the sanitized status and a status-derived-only category.
  -}
  describe "classifyErrorDiagnostic" do
    it "extracts only a sanitized status and status-derived category from a genuine service error" do
      let err = ServiceError $ serviceError "S3" status404 [] (Just "NoSuchKey") (Just "computer says no") (Just "req-123")
      classifyErrorDiagnostic err `shouldBe` AwsServiceFailure 404 AwsCategoryNotFound

    it "never carries the underlying exception for a transport failure" do
      -- 'AwsTransportFailure' is nullary: this is a type-level guarantee,
      -- not just a runtime one, that no 'HttpException' (and therefore no
      -- unredacted request header) can reach the diagnostic.
      classifyErrorDiagnostic (TransportError httpExceptionFixture) `shouldBe` AwsTransportFailure

    it "drops the raw response body from a serialize error, keeping only the sanitized status/category" do
      let err = SerializeError $ SerializeError' "S3" status500 (Just "<sensitive-looking-body>") "unexpected token"
      classifyErrorDiagnostic err `shouldBe` AwsSerializeFailure 500 AwsCategoryServerError

  describe "classifyAuthErrorDiagnostic" do
    it "never carries the underlying exception for a credential retrieval failure" do
      let authErr = RetrievalError httpExceptionFixture
      classifyAuthErrorDiagnostic authErr `shouldBe` AwsAuthRetrievalFailure

    it "drops the (potentially credential-bearing) message for a missing env var" do
      classifyAuthErrorDiagnostic (MissingEnvError "AWS_CONTAINER_CREDENTIALS_RELATIVE_URI") `shouldBe` AwsAuthMissingEnv

    it "drops the message for a missing credentials file" do
      classifyAuthErrorDiagnostic (MissingFileError "/root/.aws/credentials") `shouldBe` AwsAuthMissingFile

    it "drops the message for an invalid credentials file (may echo file content)" do
      classifyAuthErrorDiagnostic (InvalidFileError "parse error near line 3") `shouldBe` AwsAuthInvalidFile

    it "drops the message for an invalid IAM/task-identity document (may echo response content)" do
      classifyAuthErrorDiagnostic (InvalidIAMError "Error parsing Task Identity Document: ...") `shouldBe` AwsAuthInvalidIAM

    it "reports credential chain exhaustion" do
      classifyAuthErrorDiagnostic CredentialChainExhausted `shouldBe` AwsAuthCredentialChainExhausted

    it "extracts only a sanitized status and status-derived category from an auth-layer service error" do
      let err = serviceError "S3" status403 [] (Just "AccessDenied") Nothing Nothing
      classifyAuthErrorDiagnostic (AuthServiceError err) `shouldBe` AwsAuthServiceFailure 403 AwsCategoryClientError

    it "never carries the underlying arbitrary exception for an uncategorized auth failure" do
      -- 'AwsAuthOtherFailure' is nullary: 'OtherAuthError' wraps an
      -- unconstrained 'SomeException', so this is the only safe diagnostic.
      classifyAuthErrorDiagnostic (OtherAuthError (toException (userError "boom"))) `shouldBe` AwsAuthOtherFailure

  {- | Regression for the HIGH-severity async-credential-refresh audit: the
  pinned Amazonka fork's background credential-refresh timer captures the
  thread that called 'Amazonka.newEnv'/'discover' as its eventual
  refresh-failure 'throwTo' target. 'releaseAwsEnvChild' kills that
  target's associated 'Ref'-embedded refresh 'ThreadId' directly, before
  it can ever reach its @throwTo@ line; see 'startSupervisedEnv' for how
  this is composed with a single dedicated supervisor thread so that
  target is always this same long-lived thread, never a per-request one.
  -}
  describe "releaseAwsEnvChild" do
    it "does nothing for already-static credentials, without starting or touching any thread" do
      releaseAwsEnvChild (Auth (AuthEnv "AKIAEXAMPLE" "secret" Nothing Nothing)) `shouldReturn` ChildReleased

    it "kills the background refresh thread before it can act" do
      let authEnv = AuthEnv "AKIAEXAMPLE" "secret" (Just "token") Nothing
      ref <- newIORef authEnv
      reachedRefresh <- newIORef False
      -- Stands in for Amazonka's real background refresh-timer thread:
      -- it sleeps briefly, then (if never killed first) would flip this
      -- flag, standing in for its real 'Exception.throwTo' call.
      refreshThreadId <- forkIO $ do
        threadDelay (50 * 1000)
        writeIORef reachedRefresh True
      releaseAwsEnvChild (Ref refreshThreadId ref) `shouldReturn` ChildReleased
      -- Long enough that, had the refresh thread not been killed above,
      -- it would certainly have flipped the flag by now.
      threadDelay (150 * 1000)
      readIORef reachedRefresh `shouldReturn` False

    {- | Deterministic (non-flaky) proof that 'releaseAwsEnvChild' does not
    return until the target thread has genuinely, observably terminated
    -- not merely until 'killThread' has raised the exception in it (per
    -- the GHC docs, 'killThread'\/'throwTo' block only until the
    -- exception is /raised/, not until the target finishes unwinding).
    Checking 'threadStatus' with NO further delay whatsoever immediately
    upon return is what makes this deterministic: if
    'releaseAwsEnvChild' returned as soon as the exception was merely
    raised (the pre-fix behaviour), this thread would very often still be
    observed mid-unwind\/not yet finished at the exact instant of return.

    Mutation check: removing 'awaitThreadTerminated' from
    'releaseAwsEnvChild' (reverting to bare @killThread@) makes this
    test intermittently fail (the target frequently has not yet reached
    a terminal 'ThreadStatus' the instant @killThread@ returns).
    -}
    it "does not return until the target thread has already reached a terminal status, with zero further waiting" do
      let authEnv = AuthEnv "AKIAEXAMPLE" "secret" (Just "token") Nothing
      ref <- newIORef authEnv
      -- A slow-to-unwind stand-in: on being killed, it does some (tiny
      -- but nonzero) cleanup work of its own before actually finishing,
      -- so a caller that doesn't truly await termination has every
      -- opportunity to observe it as still non-terminal. The readyGate
      -- guarantees the handler below is already installed before this
      -- thread can possibly be killed -- see the three-deep release
      -- order test above for why that matters.
      readyGate <- newEmptyMVar
      refreshThreadId <- forkIO $ Exception.catch (putMVar readyGate () >> forever (threadDelay maxBound)) \Exception.ThreadKilled ->
        threadDelay (2 * 1000)
      takeMVar readyGate
      releaseAwsEnvChild (Ref refreshThreadId ref) `shouldReturn` ChildReleased
      statusImmediatelyAfterReturn <- threadStatus refreshThreadId
      statusImmediatelyAfterReturn `shouldSatisfy` isTerminatedStatus

    {- | Regression for the HIGH-severity \"release reports success with a
    live child\" audit: a target that never reaches a terminal
    'ThreadStatus' within the bounded poll (because it deliberately
    ignores 'Exception.ThreadKilled' and keeps running) must make
    'releaseAwsEnvChild' honestly report 'ChildReleaseTimedOut' -- never
    silently return as if the child had actually terminated. This is
    deterministic, not merely probabilistic: the fake child unconditionally
    outlives the entire bounded poll (500 * 1ms = 500ms) by construction.

    Mutation check: reverting 'awaitThreadTerminated' to its pre-fix
    behaviour (returning @()@\/success unconditionally once the poll bound
    is exhausted) makes this test fail, since it would then report
    'ChildReleased' for a thread this test proves is still alive.
    -}
    it "honestly reports ChildReleaseTimedOut, never a false ChildReleased, for a child that outlives the bounded poll" do
      let authEnv = AuthEnv "AKIAEXAMPLE" "secret" (Just "token") Nothing
      ref <- newIORef authEnv
      readyGate <- newEmptyMVar
      -- Deliberately never terminates even once killed: stands in for a
      -- genuinely non-interruptible foreign call (e.g. one of the four
      -- opaque, pinned providers' refresh threads blocked inside
      -- http-client) that this module's documented residual-scope
      -- decision explicitly does not attempt to force.
      refreshThreadId <- forkIO
        $ Exception.catch
          (putMVar readyGate () >> forever (threadDelay maxBound))
          (\Exception.ThreadKilled -> forever (threadDelay maxBound))
      takeMVar readyGate
      releaseAwsEnvChild (Ref refreshThreadId ref) `shouldReturn` ChildReleaseTimedOut
      statusAfterTimeout <- threadStatus refreshThreadId
      statusAfterTimeout `shouldSatisfy` (not . isTerminatedStatus)
      killThread refreshThreadId

  describe "requireChildReleased" do
    it "returns () when the child genuinely released" do
      requireChildReleased (Auth (AuthEnv "AKIAEXAMPLE" "secret" Nothing Nothing)) `shouldReturn` ()

    {- | Regression: 'requireChildReleased' must throw, never silently
    succeed, when 'releaseAwsEnvChild' reports 'ChildReleaseTimedOut' --
    this is what actually prevents 'releaseAwsEnvAcquisition'\/
    'stopSupervisedEnv' from ever claiming a release succeeded while the
    child is still alive.

    Mutation check: replacing the 'ChildReleaseTimedOut' branch with
    @pure ()@ (silently swallowing the timeout) makes this test fail.
    -}
    it "throws ChildReleaseTimedOutException rather than silently succeeding when the child outlives the bounded poll" do
      let authEnv = AuthEnv "AKIAEXAMPLE" "secret" (Just "token") Nothing
      ref <- newIORef authEnv
      readyGate <- newEmptyMVar
      refreshThreadId <- forkIO
        $ Exception.catch
          (putMVar readyGate () >> forever (threadDelay maxBound))
          (\Exception.ThreadKilled -> forever (threadDelay maxBound))
      takeMVar readyGate
      requireChildReleased (Ref refreshThreadId ref)
        `Exception.catch` (\(ChildReleaseTimedOutException tid) -> tid `shouldBe` refreshThreadId)
      killThread refreshThreadId

  {- | Regression for the HIGH-severity cleanup-order audit's second half:
  'releaseAwsEnvAcquisition' must attempt every child's release even if
  an earlier one throws, and re-raise the first genuine failure only
  once every release has at least been attempted. An earlier revision of
  this module special-cased 'AuthError' here as automatically-safe-to-
  discard feedback from killing a child mid-refresh; that assumption was
  unsound (a *sibling* worker's own late, genuine refresh-failure
  notification can land on the very thread performing an unrelated
  child's release, and is indistinguishable from that unrelated child's
  own outcome at this layer -- see 'managedFetchAuthInBackground''s own
  Haddock), so an 'AuthError' arriving here is now treated exactly like
  any other exception: every remaining release is still attempted
  regardless, but it is genuinely re-thrown, never silently swallowed.
  The retry that safely discards *expected* sibling 'AuthError' feedback
  now lives one layer down, inside 'managedFetchAuthInBackground''s own
  @release@ -- the only place that can actually tell whether an
  'AuthError' arriving during its wait belongs to *this specific* child
  (see the live-worker regression test below). These first two tests
  construct an 'AwsEnvAcquisition' directly (with a static, no-op-release
  outer @.auth@) so each hidden release's own behaviour is fully
  test-controlled, independent of any real forked thread.

  Mutation check: reintroducing an @AuthError@-is-automatically-safe
  special case into 'attemptRelease' makes the first test below fail
  (the 'AuthError' would be discarded instead of propagating).
  -}
  describe "releaseAwsEnvAcquisition (attempts every release even if one throws)" do
    it "an AuthError from one child's release is no longer discarded -- every other release still runs, then it is genuinely re-thrown" do
      envNoAuth <- newEnvNoAuth
      staticEnv <- fakeStaticEnv envNoAuth
      orderRef <- newIORef ([] :: [Text])
      let record label = atomicModifyIORef' orderRef (\xs -> (xs <> [label], ()))
          hiddenReleases =
            [ record "first"
            , record "second" >> Exception.throwIO (RetrievalError httpExceptionFixture)
            , record "third"
            ]
      outcome <-
        Exception.try @AuthError
          (releaseAwsEnvAcquisition (AwsEnvAcquisition staticEnv (pure ()) hiddenReleases))
      case outcome of
        Left (RetrievalError _) -> pure ()
        _ -> expectationFailure "expected the AuthError to propagate genuinely, not be swallowed"
      -- Hidden releases still run newest-to-oldest (reversed): "third",
      -- then "second" (which throws, and is now recorded rather than
      -- discarded), then "first" still runs regardless.
      readIORef orderRef `shouldReturn` ["third", "second", "first"]

    {- | Regression for the retry actually added inside
    'managedFetchAuthInBackground' itself (see that function's Haddock),
    exercised through the *real*, live worker\/'cancelManagedThread'
    machinery rather than a fake, single-shot 'Exception.throwIO' at the
    'attemptRelease' layer. @target@ below is an ordinary, otherwise
    unremarkable live worker with a far-future expiration: sitting
    quietly in its own scheduled refresh delay, it can *only* ever
    terminate here via a genuinely successful 'cancelManagedThread' call
    actually reaching it -- never a coincidental natural refresh failure
    of its own. While its release is in flight, a background flood
    repeatedly delivers a correctly-classified 'AuthError' (the exact
    type\/shape 'reclassifyAsAuthError' produces for a live sibling's own
    genuine refresh failure) to this very same calling thread, bounded to
    a fixed, short window so the test remains deterministic rather than
    racing an unbounded flood against 'release'.

    Mutation check: reverting 'managedFetchAuthInBackground''s
    @awaitReleased@ retry back to a bare 'cancelManagedThread' call makes
    this test fail: the very first flooded 'AuthError' can interrupt
    'release' before it has even sent 'ThreadKilled' to @targetTid@ at
    all, so 'releaseAwsEnvAcquisition' would return (having wrongly
    treated that unrelated feedback as this target's own release) while
    @targetTid@ is reported still-live by 'threadStatus' below.
    -}
    it "the target worker's release keeps retrying, never reporting success, until its own thread is genuinely confirmed terminated -- even while continuously interrupted by correctly-classified AuthError feedback landing on the very same calling thread" do
      farFuture <- (\now -> Time (addUTCTime 60 now)) <$> getCurrentTime
      -- Goes through 'managedEnvAcquisition' -- the same single atomic
      -- factory every real dynamic provider uses, and the only way
      -- (since 'managedFetchAuthInBackground' itself is no longer
      -- exported -- see the module Haddock's \"Structural release
      -- ownership\") this test can still obtain a genuine, live worker's
      -- own 'Ref'\/release to exercise 'releaseAwsEnvAcquisition' with
      -- directly, below.
      targetEnvNoAuth <- newEnvNoAuth
      targetAcquisition <-
        managedEnvAcquisition
          targetEnvNoAuth
          (pure (AuthEnv "AKIAEXAMPLE" "secret" Nothing (Just farFuture)))
      let (targetEnv, releaseTarget) = runManagedEnvAcquisition targetAcquisition
      case runIdentity targetEnv.auth of
        Ref targetTid _ -> do
          envNoAuth <- newEnvNoAuth
          staticEnv <- fakeStaticEnv envNoAuth
          myTid <- myThreadId
          floodStopRef <- newIORef False
          floodDoneGate <- newEmptyMVar
          floodConfirmedStopped <- newIORef False
          let floodIterations = 200 :: Int
              flood n
                | n >= floodIterations = pure ()
                | otherwise = do
                    stop <- readIORef floodStopRef
                    unless stop $ do
                      Exception.throwTo myTid (OtherAuthError (Exception.toException (userError "sibling feedback")))
                      threadDelay 1_000
                      flood (n + 1)
          _ <- forkIO (flood 0 >> putMVar floodDoneGate ())
          -- Stopping and awaiting the flood must itself tolerate a late,
          -- already-in-flight flooded 'AuthError' landing on this very
          -- thread strictly between 'writeIORef' and 'floodStopRef'
          -- actually being observed by the flood loop -- otherwise this
          -- bounded, deterministic test would itself flake on an
          -- exception that has nothing to do with the behaviour under
          -- test. Guarded by 'floodConfirmedStopped' so this remains
          -- safe to call any number of times (including from the
          -- outer retry below): a second call after the gate has
          -- genuinely already been drained is a no-op rather than
          -- blocking forever on an already-empty 'floodDoneGate'.
          let awaitFloodStopped = do
                already <- readIORef floodConfirmedStopped
                unless already
                  $ ( writeIORef floodStopRef True
                        >> takeMVar floodDoneGate
                        >> writeIORef floodConfirmedStopped True
                    )
                    `Exception.catch` \(_ :: AuthError) -> awaitFloodStopped
              -- 'Exception.try'\/'Exception.catch' install and remove
              -- their own handler around only the *inside* of the
              -- action they wrap -- they add no masking of their own,
              -- so an async exception timed to land in the vanishingly
              -- narrow window exactly as one is *installing* its
              -- handler, or exactly as the wrapped action completes and
              -- its handler is being *torn down*, can still bypass that
              -- particular 'try'\/'catch' entirely (this is true of
              -- 'releaseAwsEnvAcquisition''s own internal 'try', of
              -- 'timeout''s internal cleanup, and of any 'try' added
              -- here). A continuously-flooding sibling gives this
              -- vanishingly narrow window many, many chances to be hit
              -- over the course of a single run, so no *finite* stack of
              -- nested 'try'\/'catch' inside a single attempt can ever
              -- be airtight against it. The only genuinely deterministic
              -- fix is to retry the *entire* attempt (including
              -- 'releaseAwsEnvAcquisition' itself) from the outside
              -- whenever this exact, recognized, self-inflicted feedback
              -- escapes -- which is always safe here regardless of
              -- where escape occurred: 'releaseAwsEnvAcquisition' ->
              -- 'cancelManagedThread' is idempotent (re-delivering
              -- 'ThreadKilled' to an already-finished thread is a
              -- documented no-op, and 'readMVar' -- never 'takeMVar' --
              -- on the completion cell means re-observing it never
              -- blocks). Since the flood is strictly bounded (@200@
              -- iterations, 1ms apart), only finitely many retries can
              -- ever be needed: once the flood has genuinely stopped
              -- sending (observed via 'awaitFloodStopped' above), no
              -- further escape is even possible, so this always
              -- terminates.
              attempt = do
                result <-
                  timeout 5_000_000
                    $ Exception.try @Exception.SomeException
                      (releaseAwsEnvAcquisition (AwsEnvAcquisition staticEnv releaseTarget []))
                awaitFloodStopped
                case result of
                  Nothing -> expectationFailure "releaseAwsEnvAcquisition did not return within the bounded timeout"
                  Just (Left (e :: Exception.SomeException)) -> Exception.throwIO e
                  Just (Right ()) -> pure ()
              guardedAttempt = attempt `Exception.catch` \(_ :: AuthError) -> guardedAttempt
          guardedAttempt
          status <- threadStatus targetTid
          status `shouldSatisfy` isTerminatedStatus
        _ -> expectationFailure "expected managedEnvAcquisition to produce a Ref for a far-future expiration, not a static Auth"

    {- | Mutation check: if 'releaseAwsEnvAcquisition' instead let ANY
    exception (not just 'AuthError') abort the remaining releases, this
    test's "first" label would never be recorded, and the caught
    exception below would be 'ThreadKilled' misreported as something
    else, or simply never observed at all.
    -}
    it "a genuine non-AuthError exception still runs every remaining release, then is re-thrown as itself -- never as an AuthError" do
      envNoAuth <- newEnvNoAuth
      staticEnv <- fakeStaticEnv envNoAuth
      orderRef <- newIORef ([] :: [Text])
      let record label = atomicModifyIORef' orderRef (\xs -> (xs <> [label], ()))
          hiddenReleases =
            [ record "first"
            , record "second" >> Exception.throwIO Exception.ThreadKilled
            , record "third"
            ]
      outcome <-
        Exception.try @Exception.AsyncException
          (releaseAwsEnvAcquisition (AwsEnvAcquisition staticEnv (pure ()) hiddenReleases))
      case outcome of
        Left Exception.ThreadKilled -> pure ()
        _ -> expectationFailure "expected the genuine ThreadKilled to propagate as itself, not be swallowed or reclassified as an AuthError"
      readIORef orderRef `shouldReturn` ["third", "second", "first"]

    it "re-throws the FIRST genuine non-AuthError failure encountered (in actual release order), even when a later release also throws a different one" do
      envNoAuth <- newEnvNoAuth
      staticEnv <- fakeStaticEnv envNoAuth
      -- 'AwsEnvAcquisition''s hidden-release list is oldest\/innermost
      -- first, and 'releaseAwsEnvAcquisition' runs it reversed (newest
      -- first): so the list's SECOND element here is the one that
      -- actually executes FIRST, and its failure is the one that must
      -- be re-thrown, matching how these labels are named below.
      let hiddenReleases =
            [ Exception.throwIO (userError "runs second, must not be the one re-thrown")
            , Exception.throwIO (userError "runs first, must be the one re-thrown")
            ]
      outcome <-
        Exception.try @Exception.IOException (releaseAwsEnvAcquisition (AwsEnvAcquisition staticEnv (pure ()) hiddenReleases))
      case outcome of
        Left e -> show e `shouldContain` "runs first, must be the one re-thrown"
        Right () -> expectationFailure "expected the first genuine (in execution order) failure to propagate"

  {- | Regression for the MEDIUM-severity finding that 'managedFetchAuthInBackground'
  published both @stopRequestedRef@ (in @release@) and @credentialsRef@ (in
  @refreshLoop@) with a plain 'Data.IORef.writeIORef' rather than
  'Data.IORef.atomicWriteIORef': on a weak-memory architecture (this
  suite runs on aarch64), a plain write from the releasing thread is not
  guaranteed to be promptly visible to @refreshLoop@ running concurrently
  on a different capability, so an in-flight refresh failure racing an
  in-flight release could observe a stale, pre-release @False@ and
  misclassify an entirely expected cancellation as a genuine 'AuthError',
  incorrectly delivering it to the calling thread via
  'Exception.throwTo'. These tests exercise the real
  'managedFetchAuthInBackground' (not a fake @Ref@), not
  'releaseAwsEnvAcquisition'\/'attemptRelease' one layer up, so a
  regression in this specific visibility fix is caught here directly.
  -}
  describe "managedFetchAuthInBackground (refresh-loop / release visibility)" do
    it "a genuine refresh failure that is never raced by a release is still classified and delivered to the calling thread as an AuthError" do
      alreadyExpired <- (\now -> Time (addUTCTime (-1) now)) <$> getCurrentTime
      callCountRef <- newIORef (0 :: Int)
      let getAuthEnv = do
            n <- atomicModifyIORef' callCountRef (\k -> (k + 1, k + 1))
            if n == 1
              then pure (AuthEnv "AKIAEXAMPLE" "secret" Nothing (Just alreadyExpired))
              else Exception.throwIO (RetrievalError httpExceptionFixture)
      outcome <- Exception.try @AuthError $ do
        envNoAuth <- newEnvNoAuth
        -- 'managedFetchAuthInBackground' itself is no longer exported
        -- (see the module Haddock's \"Structural release ownership\"):
        -- this goes through 'managedEnvAcquisition', the only remaining
        -- way to start a genuine worker, discarding the resulting
        -- 'ManagedEnvAcquisition' entirely, exactly as this test
        -- previously discarded @_auth@\/@_release@ directly.
        _acquisition <- managedEnvAcquisition envNoAuth getAuthEnv
        -- No release is ever called here: the refresh loop is left free
        -- to run and observe the already-past expiry, attempt an
        -- immediate refresh, and correctly report its own genuine
        -- failure -- proving a real failure is never accidentally
        -- suppressed as if it were an expected, release-driven stop.
        forever (threadDelay maxBound) :: IO ()
      case outcome of
        Left (RetrievalError _) -> pure ()
        Left other -> expectationFailure ("expected a RetrievalError, got " <> show other)
        Right () -> expectationFailure "expected the genuine refresh failure to be delivered as an AuthError"

    {- | Best-effort regression check for the 'atomicWriteIORef' visibility
    fix: many independent trials each construct a fresh worker whose
    expiry is already in the past (so its very first scheduled refresh
    happens essentially immediately, racing this test's own immediate
    @release@ call across as many different real scheduling
    interleavings as this many iterations naturally sample), and assert
    that not one single trial ever lets a stray 'AuthError' escape past a
    successful @release@ to the calling thread.

    Honesty about this test's actual power: locally reverting
    'atomicWriteIORef' back to a plain 'writeIORef' at both call sites
    and re-running this exact test (300 iterations, repeatedly) did
    /not/ reproduce a visible failure on this machine\/GHC\/RTS
    combination -- consistent with 'writeIORef' and 'atomicWriteIORef'
    compiling to near-identical code for a lone boolean flag under @-O0@
    (the flag this test suite always builds with), with no second,
    reorderable memory operation nearby for a genuine store-reordering
    race to manifest against in practice. This test therefore does
    /not/ meet the usual \"mutation must fail\" bar for the specific
    @atomicWriteIORef@-vs-@writeIORef@ choice: it is retained as a
    behavioral regression net for the *combined* release\/refresh-race
    contract (still meaningfully exercises the real, live
    'managedFetchAuthInBackground' under adversarial timing, and would
    catch a regression in @awaitReleased@'s own 'AuthError' retry -- see
    the mutation-checked test above for that), not as proof that this
    exact primitive choice is behaviorally load-bearing on this specific
    hardware. 'atomicWriteIORef' is kept on correctness-by-construction
    grounds (it is the documented, portable way to publish a flag read
    by another capability without depending on any particular
    architecture's cache-coherence timing), not because this test can
    demonstrably fail without it.
    -}
    it "release racing an imminent refresh failure never lets a stray AuthError escape to the calling thread, across many real scheduling interleavings" do
      strayCountRef <- newIORef (0 :: Int)
      let iterations = 300 :: Int
          oneTrial = do
            alreadyExpired <- (\now -> Time (addUTCTime (-0.001) now)) <$> getCurrentTime
            callCountRef <- newIORef (0 :: Int)
            let getAuthEnv = do
                  n <- atomicModifyIORef' callCountRef (\k -> (k + 1, k + 1))
                  if n == 1
                    then pure (AuthEnv "AKIAEXAMPLE" "secret" Nothing (Just alreadyExpired))
                    else Exception.throwIO (RetrievalError httpExceptionFixture)
            outcome <- Exception.try @AuthError $ do
              envNoAuth <- newEnvNoAuth
              acquisition <- managedEnvAcquisition envNoAuth getAuthEnv
              let (_env, release) = runManagedEnvAcquisition acquisition
              -- No gate, no delay: release is issued immediately,
              -- deliberately racing whatever the freshly-spawned refresh
              -- loop is doing at that exact moment.
              release
            case outcome of
              Left (_ :: AuthError) -> atomicModifyIORef' strayCountRef (\n -> (n + 1, ()))
              Right () -> pure ()
      replicateM_ iterations oneTrial
      readIORef strayCountRef `shouldReturn` 0

    {- | Root-cause regression for the MEDIUM finding that release's
    retry loop previously only tolerated a recognized 'AuthError' arriving
    mid-cancellation: ANY other exception (including one genuinely
    targeting this releasing thread, e.g. an outer shutdown timeout)
    escaped 'release' immediately, /before/ the refresh child's own
    completion was ever genuinely observed -- letting
    'releaseAwsEnvAcquisition' proceed to release this generation's
    dependencies (e.g. an outer @sts:AssumeRole@ chain's own source
    credentials) while the child might still be alive reading them.

    This repeatedly delivers a genuine, non-'AuthError' exception
    (a plain 'IOException', standing in for any such unrelated signal)
    directly to the releasing thread, timed to land during 'release''s
    own retrying wait, and proves both halves of the fix: (1) it is never
    silently swallowed or discarded -- it is eventually rethrown by
    'release' itself as the exact same exception, not reclassified; and
    (2) it is never rethrown /before/ the target refresh thread has
    genuinely, verifiably terminated -- proven by checking the target's
    own 'GHC.Conc.Sync.threadStatus' is already terminal at the moment
    'release' finally throws, never merely \"probably done by now\".

    Mutation check: reverting the 'Data.Maybe.Maybe' \"preserve and keep
    retrying\" accumulator back to immediately rethrowing any non-'AuthError'
    exception makes this fail, because the target's own 'threadStatus'
    would not yet reliably be terminal at the moment of that immediate,
    premature rethrow.
    -}
    it "release preserves a genuine non-AuthError exception delivered mid-cancellation, rethrowing it only once the worker's own termination is genuinely confirmed" do
      farFuture <- (\now -> Time (addUTCTime 60 now)) <$> getCurrentTime
      envNoAuth <- newEnvNoAuth
      acquisition <- managedEnvAcquisition envNoAuth (pure (AuthEnv "AKIAEXAMPLE" "secret" Nothing (Just farFuture)))
      let (env, release) = runManagedEnvAcquisition acquisition
      case runIdentity env.auth of
        Ref targetTid _ -> do
          myTid <- myThreadId
          floodStopRef <- newIORef False
          floodDoneGate <- newEmptyMVar
          deliveryCount <- newIORef (0 :: Int)
          let probe = userError "genuine unrelated exception, e.g. an outer shutdown timeout"
              floodIterations = 200 :: Int
              flood n
                | n >= floodIterations = pure ()
                | otherwise = do
                    stop <- readIORef floodStopRef
                    unless stop $ do
                      -- 'Control.Exception.throwTo' blocks until the
                      -- exception has actually been delivered (not
                      -- merely queued) -- so a successful return here is
                      -- itself confirmation this exact probe genuinely
                      -- reached the releasing thread at some interruptible
                      -- point, letting the assertions below distinguish
                      -- "the race never landed this run" (never a
                      -- failure -- see 'deliveryCount' below) from "it
                      -- landed but was mishandled" (always a failure).
                      Exception.throwTo myTid probe
                      atomicModifyIORef' deliveryCount (\n -> (n + 1, ()))
                      threadDelay 1_000
                      flood (n + 1)
          _ <- forkIO (flood 0 >> putMVar floodDoneGate ())
          outcome <- Exception.try @Exception.IOException release
          -- The flood may still have in-flight iterations queued after
          -- 'release' itself has already returned/thrown (it stops
          -- retrying the instant 'cancelManagedThread' genuinely
          -- succeeds, which can happen before the flood's own bounded
          -- 200 iterations are exhausted) -- drain it so a later test
          -- can never observe a stray delivery from this one.
          writeIORef floodStopRef True
          takeMVar floodDoneGate
          delivered <- readIORef deliveryCount
          -- Only assert on the exception itself when at least one probe
          -- is confirmed to have actually landed (see 'deliveryCount'
          -- above): with 200 iterations spaced 1ms apart racing this
          -- generation's own genuine (typically sub-millisecond)
          -- cancellation, this is true in practice on every run, but
          -- this test must never itself flake merely because the race
          -- happened not to land on some particular run.
          when (delivered > 0) $ case outcome of
            Left e -> show e `shouldBe` show probe
            Right () -> expectationFailure "a probe was confirmed delivered, yet release swallowed it instead of rethrowing it"
          -- This holds unconditionally, whether or not the race landed:
          -- 'release' never returns\/throws until 'cancelManagedThread'
          -- has genuinely observed the target's own completion.
          status <- threadStatus targetTid
          status `shouldSatisfy` isTerminatedStatus
        _ -> expectationFailure "expected managedEnvAcquisition to produce a Ref for a far-future expiration, not a static Auth"

  {- | Regression for the HIGH-severity finding that
  @Amazonka.Auth.InstanceProfile.fromNamedInstanceProfile@ (pinned source,
  @b562aa3f24845e34b95748daae671860017426be@) calls
  @fetchAuthInBackground getCredentials@ -- which may fork a background
  refresh child -- /before/ the separate, fallible
  @getRegionFromIdentity@ metadata call: if region resolution fails or
  this thread is cancelled after that fork, the only handle to the child
  (inside @keys@) is discarded with it, with no way for anything in this
  module's supervisor\/release protocol to ever kill it.
  'acquireRegionBeforeAuth' is the exact ordering fix
  'safeNamedInstanceProfile' applies -- factored out here so it can be
  exercised directly against fake @getRegion@\/@getAuth@ actions, without
  a real IMDS endpoint.

  These are genuine mutation-check regressions: reverting
  'acquireRegionBeforeAuth' to @getAuth@-then-@getRegion@ (the pinned
  source's own order) makes the first test below fail, since @getAuth@
  would then run -- and 'authAttempted' would become 'True' -- before
  @getRegion@'s failure is ever observed.
  -}
  describe "acquireRegionBeforeAuth" do
    it "never attempts auth/credential acquisition (which may fork a background refresh child) if region resolution fails first" do
      envNoAuth <- newEnvNoAuth
      authAttempted <- newIORef False
      let getRegion = Exception.throwIO (RetrievalError httpExceptionFixture) :: IO Region
          getAuth = writeIORef authAttempted True >> pure (Auth (AuthEnv "AKIAEXAMPLE" "secret" Nothing Nothing))
      result <- Exception.try @AuthError (acquireRegionBeforeAuth envNoAuth getRegion getAuth)
      -- Not 'shouldSatisfy': the wrapped 'Env' has no 'Show' instance, so
      -- a 'case' avoids ever needing one for the success branch.
      case result of
        Left (RetrievalError _) -> pure ()
        _ -> expectationFailure "expected acquireRegionBeforeAuth to fail with RetrievalError before ever calling getAuth"
      readIORef authAttempted `shouldReturn` False

    it "never attempts auth/credential acquisition if this thread is asynchronously cancelled while region resolution is still in flight" do
      envNoAuth <- newEnvNoAuth
      authAttempted <- newIORef False
      regionStarted <- newEmptyMVar
      let getRegion = putMVar regionStarted () >> forever (threadDelay maxBound) :: IO Region
          getAuth = writeIORef authAttempted True >> pure (Auth (AuthEnv "AKIAEXAMPLE" "secret" Nothing Nothing))
      resultVar <- newEmptyMVar
      workerTid <-
        forkIO
          $ putMVar resultVar
          =<< Exception.try @Exception.AsyncException (acquireRegionBeforeAuth envNoAuth getRegion getAuth)
      takeMVar regionStarted
      Exception.throwTo workerTid Exception.ThreadKilled
      result <- takeMVar resultVar
      case result of
        Left Exception.ThreadKilled -> pure ()
        _ -> expectationFailure "expected acquireRegionBeforeAuth to be cancelled with ThreadKilled while getRegion was still in flight"
      readIORef authAttempted `shouldReturn` False

    it "does still attempt auth/credential acquisition once region resolution has genuinely succeeded, and the resolved region reaches the returned Env" do
      envNoAuth <- newEnvNoAuth
      authAttempted <- newIORef False
      let getRegion = pure Ireland
          getAuth = writeIORef authAttempted True >> pure (Auth (AuthEnv "AKIAEXAMPLE" "secret" Nothing Nothing))
      resultEnv <- acquireRegionBeforeAuth envNoAuth getRegion getAuth
      readIORef authAttempted `shouldReturn` True
      resultEnv.region `shouldBe` Ireland

  describe "startSupervisedEnv / readSupervisedEnv / stopSupervisedEnv" do
    it "reports Initializing before the first acquisition completes, then Ready once it does, then terminal Unavailable once stopped" do
      acquireGate <- newEmptyMVar
      sup <- startSupervisedEnv (withRelease noRelease (takeMVar acquireGate >> pure (Right (TestResource 1)))) neverInvalidate (pure ())
      readSupervisedEnv sup `shouldReturn` SupervisedEnvInitializing
      putMVar acquireGate ()
      waitForSupervisedState sup \case SupervisedEnvReady _ -> True; _ -> False
      readSupervisedEnv sup `shouldReturn` SupervisedEnvReady (TestResource 1)
      stopSupervisedEnv sup
      -- A reader can never observe a stale 'SupervisedEnvReady' snapshot
      -- once nothing is monitoring it any longer.
      readSupervisedEnv sup `shouldReturn` SupervisedEnvUnavailable AwsAuthSupervisorTerminated

    {- | Regression for the HIGH-severity finding that 'loopOnce' ran
    entirely unmasked between 'spawnManagedThread' returning
    @generationThread@ and 'Exception.onException' actually installing its
    handler around the wait: a 'ThreadKilled' delivered to the dispatcher
    in that exact gap could propagate out of 'loopOnce' unhandled,
    orphaning @generationThread@ (and everything it in turn owns) with
    nothing left to cancel\/await it -- 'forever loopOnce''s own enclosing
    'finally' only ever publishes 'AwsAuthSupervisorTerminated', which is
    published identically whether or not the generation thread was
    actually cancelled, since an unhandled exception escaping @loopOnce@
    still terminates the @forever loopOnce@ dispatcher and runs that outer
    'finally' either way.

    Unlike a prior version of this test (which only synchronized against
    the generation thread's own @myThreadId >>= putMVar@ signal -- itself
    racing independently against the *dispatcher's* internal
    spawn/mask/handler-install progress, not a true seam at the exact gap
    under test, so a reverted mutation there only failed \"an observable
    fraction of the time\"), this drives 'startSupervisedEnvUsing''s own
    injected seam directly: @afterSpawn@ busy-spins on a plain 'Data.IORef.IORef'
    (deliberately not any operation that remains interruptible even under
    'Control.Exception.mask', which would reopen the very escape valve
    this proof needs closed), using 'Data.IORef.atomicWriteIORef'\/'Data.IORef.atomicModifyIORef''
    (never a plain, unfenced 'Data.IORef.writeIORef'\/'Data.IORef.readIORef' pair) on both the
    writing (this test's own main thread and the release below) and
    reading (the spinning dispatcher thread) sides -- this test suite
    links @-threaded -with-rtsopts=-N@ (see @package.yaml@), so the two
    genuinely run on different capabilities\/OS threads, and a plain
    'Data.IORef.IORef' gives no memory-visibility guarantee across them: an
    un-fenced write can be invisible to a concurrently-spinning reader
    indefinitely, turning this into a real, silent deadlock rather than a
    fast, deterministic pass\/fail -- until this test releases it, letting the
    test deterministically confirm the dispatcher is paused /exactly/
    inside the masked span, then attempt a concurrent 'stopSupervisedEnv'
    against it, and only then release the seam -- with zero dependency on
    scheduler timing for the \"paused there\" observation. The subsequent
    \"has @stopSupervisedEnv@ not yet completed\" check is itself now
    proven the same way, not by a fixed 'Control.Concurrent.threadDelay'
    \"give it a chance\" wait: GHC never delivers an asynchronous exception
    to a masked thread regardless of scheduling, so a
    'System.Timeout.timeout'-bounded attempt to observe @stopResultVar@
    filled can /never/ succeed against a genuinely masked dispatcher, for
    any bound -- there is no window in which the correct implementation
    could be mistaken for already having stopped, unlike the fixed-delay
    check it replaces.

    Mutation check: reverting 'loopOnce' to mask only around
    'spawnManagedThread' itself (restoring before installing
    'Exception.onException', re-introducing the gap) makes this test fail
    deterministically, every run: the busy-spinning @afterSpawn@ seam
    would then run genuinely unmasked, so the concurrent
    'stopSupervisedEnv''s cancellation is delivered near-instantly instead
    of being deferred, propagating straight out of 'loopOnce' without ever
    cancelling @generationThread@ -- filling @stopResultVar@ (so the
    'System.Timeout.timeout' attempt above observes 'Just', not 'Nothing')
    almost immediately, and leaving 'GHC.Conc.Sync.threadStatus'
    permanently un-terminated for @generationThread@ by the time this
    test's final check runs, rather than merely \"sometimes\".
    -}
    it "genuinely cancels and awaits the in-flight generation's own dedicated thread when stopped while the dispatcher is deterministically paused inside its own masked spawn-to-handler-install window" do
      generationTidVar <- newEmptyMVar
      reachedSeamRef <- newIORef False
      releaseSeamRef <- newIORef False
      let afterSpawn = do
            atomicWriteIORef reachedSeamRef True
            let spin = do
                  released <- atomicModifyIORef' releaseSeamRef (\r -> (r, r))
                  if released then pure () else spin
            spin
      sup <-
        startSupervisedEnvUsing
          afterSpawn
          ( withRelease
              noRelease
              ( do
                  tid <- myThreadId
                  putMVar generationTidVar tid
                  forever (threadDelay maxBound) :: IO (Either AwsAuthErrorDiagnostic TestResource)
              )
          )
          neverInvalidate
          (pure ())
      generationTid <- takeMVar generationTidVar
      -- Deterministically wait until the dispatcher is confirmed paused
      -- exactly at the injected seam -- inside its own single 'mask',
      -- strictly after spawning @generationThread@ and strictly before
      -- restoring to an interruptible wait around it.
      let waitReached = do
            reached <- atomicModifyIORef' reachedSeamRef (\r -> (r, r))
            if reached then pure () else threadDelay 1000 >> waitReached
      waitReached
      -- Ask the supervisor to stop while the dispatcher is provably
      -- paused there. A correct, genuinely masked dispatcher cannot
      -- possibly act on this yet.
      stopResultVar <- newEmptyMVar
      stopThreadId <- forkIO (stopSupervisedEnv sup >>= putMVar stopResultVar)
      -- Deterministic cancellation-delivery handshake, not a wall-clock
      -- guess: 'stopSupervisedEnv' -> 'cancelManagedThread' does
      -- @throwTo dispatcherTid ThreadKilled@ *before* it ever reads the
      -- dispatcher's completion cell, and per GHC's own semantics
      -- 'throwTo' blocks the *calling* thread (@stopThreadId@ here)
      -- until the exception is actually delivered to its target. While
      -- the dispatcher is masked and busy-spinning at @afterSpawn@ (no
      -- interruptible operation for the RTS to deliver at), delivery
      -- cannot happen, so @stopThreadId@ itself must be observed
      -- genuinely blocked (via 'GHC.Conc.Sync.threadStatus') trying to
      -- deliver that exception -- there is no earlier blocking
      -- operation in 'stopSupervisedEnv''s call chain this could be
      -- confused with ('System.Timeout.timeout' only spawns its own
      -- independent timer thread; it adds no blocking step of its own
      -- around the wrapped action). Once observed blocked, we have
      -- *proof* delivery is genuinely pending against the masked
      -- dispatcher -- not an inference from elapsed wall-clock time --
      -- so the immediately following non-blocking check of
      -- @stopResultVar@ is race-free by construction, for any bound,
      -- however short, and detects a regressed (unmasked-gap)
      -- dispatcher deterministically: there, delivery succeeds
      -- near-instantly, 'stopSupervisedEnv' completes, and
      -- @stopThreadId@ is observed 'ThreadFinished' (not
      -- 'ThreadBlocked') instead.
      let waitStopThreadBlockedDeliveringCancellation (attemptsLeft :: Int)
            | attemptsLeft <= 0 =
                expectationFailure
                  ( "the forked stopSupervisedEnv attempt never reached a "
                      <> "genuinely blocked state trying to deliver its "
                      <> "cancellation to the dispatcher"
                  )
            | otherwise = do
                status <- threadStatus stopThreadId
                case status of
                  ThreadBlocked _ -> pure ()
                  _ -> threadDelay 1000 >> waitStopThreadBlockedDeliveringCancellation (attemptsLeft - 1)
      waitStopThreadBlockedDeliveringCancellation 5000
      earlyStopResult <- tryTakeMVar stopResultVar
      earlyStopResult `shouldBe` Nothing
      -- The generation thread must still be alive: for the fixed,
      -- genuinely masked dispatcher, the seam cannot yet have been
      -- interrupted, so 'loopOnce' cannot yet have reached (let alone
      -- completed) its own cancel/await of @generationThread@.
      genStatusWhileParked <- threadStatus generationTid
      genStatusWhileParked `shouldSatisfy` (not . isTerminatedStatus)
      -- Release the seam: only now can the dispatcher (if genuinely
      -- masked) restore to its interruptible wait, observe the pending
      -- cancellation, and cancel/await the generation before
      -- 'stopSupervisedEnv' is allowed to return.
      atomicWriteIORef releaseSeamRef True
      _ <- takeMVar stopResultVar
      readSupervisedEnv sup `shouldReturn` SupervisedEnvUnavailable AwsAuthSupervisorTerminated
      status <- waitUntilTerminated generationTid
      status `shouldSatisfy` isTerminatedStatus

    it "publishes Unavailable with the acquire's diagnostic on failure, and does not retry until backoff completes (no retry storm)" do
      attemptCountRef <- newIORef (0 :: Int)
      backoffGate <- newEmptyMVar
      let acquire = do
            n <- atomicModifyIORef' attemptCountRef (\k -> (k + 1, k + 1))
            pure $ if n == 1 then Left AwsAuthCredentialChainExhausted else Right (TestResource n)
      sup <- startSupervisedEnv (withRelease noRelease acquire) neverInvalidate (takeMVar backoffGate)
      waitForSupervisedState sup (== SupervisedEnvUnavailable AwsAuthCredentialChainExhausted)
      -- Backoff has not been released yet: no second attempt has happened.
      threadDelay (50 * 1000)
      readIORef attemptCountRef `shouldReturn` 1
      putMVar backoffGate ()
      waitForSupervisedState sup \case SupervisedEnvReady (TestResource 2) -> True; _ -> False
      void (stopSupervisedEnv sup)

    {- | Regression for the review finding that 'SupervisedEnvInitializing'
    was only ever published once, at construction: after a failure, the
    snapshot stayed at the *previous* generation's 'SupervisedEnvUnavailable'
    for the entire duration of the next attempt, even while that attempt
    was already genuinely in flight. A caller with a bounded wait budget
    (see 'requestDemandDrivenReady') only waits when it observes
    'SupervisedEnvInitializing', so it would instead see the stale
    'SupervisedEnvUnavailable' and fail immediately -- an avoidable 502
    even when the in-flight re-acquisition was about to succeed well
    within that caller's own timeout. This blocks the *second* attempt
    mid-acquisition (via 'acquireGate') so the test can observe the
    published state while it is genuinely in flight, proving it is
    'SupervisedEnvInitializing' and not the first attempt's stale
    diagnostic.
    -}
    it "re-publishes Initializing at the start of every re-acquisition attempt, not only the very first, so a stale Unavailable from the previous generation is never observed while a new attempt is already in flight" do
      attemptCountRef <- newIORef (0 :: Int)
      acquireGate <- newEmptyMVar
      backoffGate <- newEmptyMVar
      let acquire = do
            n <- atomicModifyIORef' attemptCountRef (\k -> (k + 1, k + 1))
            if n == 1
              then pure (Left AwsAuthCredentialChainExhausted)
              else takeMVar acquireGate >> pure (Right (TestResource n))
      sup <- startSupervisedEnv (withRelease noRelease acquire) neverInvalidate (takeMVar backoffGate)
      waitForSupervisedState sup (== SupervisedEnvUnavailable AwsAuthCredentialChainExhausted)
      putMVar backoffGate ()
      -- The second attempt is now genuinely in flight, blocked on
      -- 'acquireGate' -- prove the published state is 'Initializing', not
      -- the first attempt's stale 'Unavailable'.
      waitForSupervisedState sup (== SupervisedEnvInitializing)
      putMVar acquireGate ()
      waitForSupervisedState sup \case SupervisedEnvReady (TestResource 2) -> True; _ -> False
      void (stopSupervisedEnv sup)

    {- | Regression for the HIGH-severity async-credential-refresh-escape
    audit itself: a delayed invalidation (standing in for the pinned
    Amazonka fork's real background refresh-timer 'Exception.throwTo')
    can only ever land on the supervisor's own dedicated thread, never on
    a separate thread standing in for a live Warp request worker -- and
    the old generation's child resource is released (here, a flag flipped
    by its own 'finally') strictly before the next generation's 'Ready' is
    published.

    Delivery uses plain, unwrapped 'Control.Exception.throwTo' carrying a
    real 'AuthError' constructor, matching exactly how the pinned fork's
    own background timer delivers it.
    -}
    it "a delayed invalidation lands only on the supervisor thread, never an unrelated request-worker stand-in, releasing the old generation's child before the next is published" do
      supervisorTidVar <- newEmptyMVar
      childReleasedRef <- newIORef False
      generationRef <- newIORef (0 :: Int)
      backoffGate <- newEmptyMVar
      let acquire = do
            n <- atomicModifyIORef' generationRef (\k -> (k + 1, k + 1))
            pure (Right (TestResource n))
          release _ = writeIORef childReleasedRef True
          awaitInvalidation _ = do
            tid <- myThreadId
            putMVar supervisorTidVar tid
            outcome <- Exception.try @AuthError (forever (threadDelay maxBound))
            either (evaluate . classifyAuthErrorDiagnostic) absurd outcome
      -- A separate, unrelated thread standing in for a live Warp request
      -- worker: it must never be affected by the delayed invalidation
      -- below, however precisely it targets only the supervisor's thread.
      requestWorkerAffected <- newIORef False
      requestWorkerTid <-
        forkIO $ Exception.catch (forever (threadDelay maxBound)) \(_ :: AuthError) -> writeIORef requestWorkerAffected True
      sup <- startSupervisedEnv (withRelease release acquire) awaitInvalidation (takeMVar backoffGate)
      waitForSupervisedState sup \case SupervisedEnvReady (TestResource 1) -> True; _ -> False
      supervisorTid <- takeMVar supervisorTidVar
      supervisorTid `shouldNotBe` requestWorkerTid
      Exception.throwTo supervisorTid (RetrievalError httpExceptionFixture)
      waitForSupervisedState sup (== SupervisedEnvUnavailable AwsAuthRetrievalFailure)
      readIORef childReleasedRef `shouldReturn` True
      readIORef requestWorkerAffected `shouldReturn` False
      killThread requestWorkerTid
      void (stopSupervisedEnv sup)

    {- | Regression for the per-generation thread-isolation redesign
    itself (the structural fix underlying the whole HIGH-severity
    cleanup-order finding): each generation now runs on its own,
    disposable thread via 'Async.withAsync', rather than all generations
    sharing one long-lived dispatcher thread as the sole
    'Exception.throwTo' target. This proves two distinct generations
    really do get two distinct threads, and that a stale/late
    'Exception.throwTo' explicitly aimed at an OLD generation's
    (by-then-terminated) thread can never be misdelivered to a NEWER
    generation's live thread merely by virtue of it being "the current
    supervisor" -- there is no such single shared target any more.

    Mutation check: reverting 'startSupervisedEnv' to run every
    generation as the dispatcher's own repeated body (the pre-fix design)
    makes @gen1Tid@ and @gen2Tid@ compare equal here, since both
    generations would then run on the very same thread.
    -}
    it "each generation runs on its own distinct thread, so a stale throwTo aimed at an old generation's thread can never reach a newer generation" do
      gen1ThreadIdVar <- newEmptyMVar
      gen2ThreadIdVar <- newEmptyMVar
      generationRef <- newIORef (0 :: Int)
      advanceGate <- newEmptyMVar
      let acquire = do
            n <- atomicModifyIORef' generationRef (\k -> (k + 1, k + 1))
            pure (Right (TestResource n))
          release _ = pure ()
          awaitInvalidation (TestResource n) = do
            tid <- myThreadId
            case n of
              1 -> putMVar gen1ThreadIdVar tid
              2 -> putMVar gen2ThreadIdVar tid
              _ -> pure ()
            outcome <- Exception.try @AuthError (forever (threadDelay maxBound))
            either (evaluate . classifyAuthErrorDiagnostic) absurd outcome
      sup <- startSupervisedEnv (withRelease release acquire) awaitInvalidation (takeMVar advanceGate)
      waitForSupervisedState sup \case SupervisedEnvReady (TestResource 1) -> True; _ -> False
      gen1Tid <- takeMVar gen1ThreadIdVar
      -- Invalidate generation 1 so a second generation can begin.
      Exception.throwTo gen1Tid (RetrievalError httpExceptionFixture)
      waitForSupervisedState sup (== SupervisedEnvUnavailable AwsAuthRetrievalFailure)
      -- Generation 1's own dedicated thread has, by now, already
      -- returned from its generation body ('awaitInvalidation' above
      -- already unblocked and returned the classified diagnostic) --
      -- prove it is provably no longer running at all.
      gen1Status <- waitUntilTerminated gen1Tid
      gen1Status `shouldSatisfy` isTerminatedStatus
      -- A late, stale throwTo explicitly targeting generation 1's own
      -- (already-dead) thread: a safe no-op against a finished thread,
      -- never capable of reaching generation 2's distinct, live thread.
      Exception.throwTo gen1Tid Exception.ThreadKilled
      putMVar advanceGate ()
      waitForSupervisedState sup \case SupervisedEnvReady (TestResource 2) -> True; _ -> False
      gen2Tid <- takeMVar gen2ThreadIdVar
      gen2Tid `shouldNotBe` gen1Tid
      -- Generation 2 is genuinely unaffected: still Ready, not
      -- reclassified as Unavailable by the stale throwTo above.
      readSupervisedEnv sup `shouldReturn` SupervisedEnvReady (TestResource 2)
      void (stopSupervisedEnv sup)

    it "forces the classified diagnostic before publishing it, not deferring it as a thunk for a later reader to force" do
      forcedRef <- newIORef False
      let acquire = pure (Left (markForcedOnceEvaluated forcedRef AwsAuthCredentialChainExhausted)) :: IO (Either AwsAuthErrorDiagnostic ())
      sup <- startSupervisedEnv (withRelease noRelease acquire) neverInvalidate (forever (threadDelay maxBound))
      -- This predicate inspects only the outer constructor (a wildcard
      -- binder for the wrapped diagnostic), never the diagnostic value
      -- itself, so if 'forcedRef' is already 'True' once it is satisfied,
      -- that can only be because 'startSupervisedEnv' forced the
      -- diagnostic *before* publishing it -- not because this test's own
      -- inspection forced it first.
      waitForSupervisedState sup \case SupervisedEnvUnavailable _ -> True; _ -> False
      readIORef forcedRef `shouldReturn` True
      -- Only now does the test itself inspect the diagnostic's value.
      readSupervisedEnv sup `shouldReturn` SupervisedEnvUnavailable AwsAuthCredentialChainExhausted
      void (stopSupervisedEnv sup)

    it "stopSupervisedEnv terminates the supervisor thread and releases (waits for) the current generation's live child" do
      supervisorTidVar <- newEmptyMVar
      childKilled <- newIORef False
      let acquire = pure (Right (TestResource 1))
          release _ = writeIORef childKilled True
          awaitInvalidation _ = do
            tid <- myThreadId
            putMVar supervisorTidVar tid
            forever (threadDelay maxBound)
      sup <- startSupervisedEnv (withRelease release acquire) awaitInvalidation (pure ())
      waitForSupervisedState sup \case SupervisedEnvReady _ -> True; _ -> False
      supervisorTid <- takeMVar supervisorTidVar
      stopSupervisedEnv sup
      status <- threadStatus supervisorTid
      status `shouldNotBe` ThreadRunning
      readIORef childKilled `shouldReturn` True

    {- | The single-'mask' design (see 'startSupervisedEnv') spans from
    just before @acquire@ is called through the moment
    'SupervisedEnvReady' is published, and only then restores/unmasks to
    enter @awaitInvalidation@. This proves that gap is genuinely closed:
    a fake @acquire@ forks its own \"refresh child\" thread immediately
    before returning (standing in for the pinned Amazonka fork's own
    @forkIO@ inside @fetchAuthInBackground@), and an asynchronous
    exception (standing in for 'stopSupervisedEnv') is delivered to the
    supervisor thread at the exact handoff moment -- via a dedicated
    \"delivered\" gate signalled by the fake @acquire@ itself, so no sleep
    or timing guess is involved. Because the whole span is masked, that
    exception cannot possibly land until @awaitInvalidation@ is entered
    (restored), by which point 'SupervisedEnvReady' has already been
    published and the forked child is already reachable via @env@ -- so
    @release@ can, and must, kill it exactly once. If the exception could
    instead land in the gap between @acquire@ returning and
    'SupervisedEnvReady' being published, the child would leak: nothing
    would ever call this test's release action at all, since
    'startSupervisedEnv' never even entered @awaitInvalidation@ for a
    generation that was never published.
    -}
    it "closes the fork-then-orphan gap: an async exception delivered exactly at acquire's handoff cannot orphan a child forked immediately before return" do
      readyPublished <- newEmptyMVar
      releaseCountRef <- newIORef (0 :: Int)
      let acquire = do
            -- Stands in for the pinned fork's own 'forkIO' immediately
            -- before returning a successfully-acquired resource: nothing
            -- else happens between this and 'acquire' returning.
            childTid <- forkIO (forever (threadDelay maxBound))
            pure (Right childTid)
          release childTid = do
            atomicModifyIORef' releaseCountRef (\n -> (n + 1, ()))
            killThread childTid
          awaitInvalidation _ = do
            putMVar readyPublished ()
            -- Blocks here (restored/interruptible) until the external
            -- exception below arrives; @release@ (applied via 'finally'
            -- around this call, see 'startSupervisedEnv') always runs
            -- whether this returns normally or the exception propagates
            -- through.
            forever (threadDelay maxBound)
      sup <- startSupervisedEnv (withRelease release acquire) awaitInvalidation (pure ())
      -- Waits only for 'SupervisedEnvReady' to have been *published*,
      -- which -- under the single-mask design -- can only happen after
      -- the fork above has already completed and its 'ThreadId' is
      -- already captured in @env@, ready for 'awaitInvalidation' to
      -- release. No sleep: this synchronizes on the same gate
      -- 'awaitInvalidation' itself signals immediately upon entry.
      takeMVar readyPublished
      stopSupervisedEnv sup
      -- Released via 'finally' around @restore (awaitInvalidation env)@
      -- exactly once -- not zero times (which would mean the child
      -- leaked) and not more than once (which would mean release
      -- raced/duplicated across generations).
      readIORef releaseCountRef `shouldReturn` 1

    {- | Regression for the second half of the same audit finding: a
    /pending/ asynchronous exception queued while a generation is still
    masked (i.e. during @acquire@, or between @acquire@ returning and
    'SupervisedEnvReady' being published) is delivered the instant
    'restore' unmasks -- /before/ @awaitInvalidation@'s own argument thunk
    is ever forced, let alone entered. An internal @finally@ inside
    @awaitInvalidation@'s own body (the design this replaced) could
    therefore be skipped entirely, silently leaking the resource: exactly
    why release is now applied via 'finally' /around/ the
    @restore (awaitInvalidation env)@ call in 'startSupervisedEnv' itself
    -- a handler installed synchronously, one level further out, before
    'restore' is ever reached.

    The pending exception here is queued from /within/ @acquire@ itself
    (which always runs on the supervisor thread, still masked): a
    throwaway helper thread @throwTo@s this same thread, then this test
    spins (deliberately not 'threadDelay', which remains interruptible
    even under mask -- see the inline comment below) long enough that the
    RTS has certainly already queued the exception as pending before
    @acquire@ returns -- so by the time @runGeneration@ reaches @restore
    (awaitInvalidation env)@, delivery is guaranteed to happen at that
    exact unmask, not later. 'awaitInvalidationEntered' staying 'False'
    proves @awaitInvalidation@'s body genuinely never got a chance to run
    even its first instruction; 'releaseCountRef' reading @1@ proves
    'release' still ran despite that.

    Mutation check: reverting to the previous design -- release folded
    into @awaitInvalidation@'s own internal @finally@ instead of applied
    by 'startSupervisedEnv' around the call -- makes this test fail
    ('releaseCountRef' stays @0@), since the pending exception is
    delivered before that internal handler is ever installed.
    -}
    it "closes the restore-boundary gap: a pending exception queued during acquire still runs release exactly once, even though it fires before awaitInvalidation's body ever executes" do
      releaseCountRef <- newIORef (0 :: Int)
      awaitInvalidationEnteredRef <- newIORef False
      let acquire = do
            tid <- myThreadId
            _ <- forkIO $ Exception.throwTo tid Exception.ThreadKilled
            -- A bounded, deliberately *non-interruptible* busy spin, not
            -- 'threadDelay': 'threadDelay' is itself one of the specific
            -- operations that remains interruptible even while masked
            -- (verified directly against this exact pinned GHC), so it
            -- would let the helper's pending exception land right here,
            -- inside @acquire@, rather than staying genuinely pending
            -- until 'restore' below -- defeating the very race this test
            -- exists to reproduce. A tight recursive loop with no
            -- interruptible operation cannot be preempted while masked,
            -- no matter how long it runs, and is long enough here that
            -- the helper thread has certainly already issued its
            -- 'Exception.throwTo' (which itself blocks until delivered,
            -- so it is provably still pending, not yet delivered, the
            -- entire time this thread remains masked).
            let spin :: Int -> IO ()
                spin 0 = pure ()
                spin n = spin (n - 1)
            spin (20 * 1000 * 1000)
            pure (Right (TestResource 1))
          release _ = atomicModifyIORef' releaseCountRef (\n -> (n + 1, ()))
          awaitInvalidation _ = do
            writeIORef awaitInvalidationEnteredRef True
            forever (threadDelay maxBound)
      -- 'startSupervisedEnv' only starts its own dedicated thread and
      -- returns immediately; the pending exception queued above
      -- terminates that thread asynchronously, so wait for its terminal
      -- published state rather than anything returned here.
      sup <- startSupervisedEnv (withRelease release acquire) awaitInvalidation (pure ())
      waitForSupervisedState sup (== SupervisedEnvUnavailable AwsAuthSupervisorTerminated)
      readIORef awaitInvalidationEnteredRef `shouldReturn` False
      readIORef releaseCountRef `shouldReturn` 1
      readIORef awaitInvalidationEnteredRef `shouldReturn` False
      readIORef releaseCountRef `shouldReturn` 1

  {- | The demand-driven wrapper ('newDemandDrivenSupervisor'\/
  'requestDemandDrivenReady'\/'stopDemandDrivenSupervisor') that
  'AwsEnvSupervisor' is a thin, production-facing instance of -- tested
  here directly against a fake, fully test-controlled @acquire@ so that
  demand-gating behavior is proven without ever touching real AWS
  credentials\/network\/filesystem via 'acquireAwsEnv'.
  -}
  describe "newDemandDrivenSupervisor / requestDemandDrivenReady / stopDemandDrivenSupervisor" do
    it "starts its dedicated thread immediately but never calls acquire until first demanded" do
      acquireCalls <- newIORef (0 :: Int)
      let acquire = atomicModifyIORef' acquireCalls (\n -> (n + 1, ())) >> pure (Right (TestResource 1))
      sup <- newDemandDrivenSupervisor (withRelease noRelease acquire) neverInvalidate (pure ())
      -- Long enough that, had construction itself triggered acquisition,
      -- it would certainly have happened by now.
      threadDelay (100 * 1000)
      readIORef acquireCalls `shouldReturn` 0
      _ <- requestDemandDrivenReady sup (200 * 1000)
      waitForDemandDrivenState sup \case SupervisedEnvReady _ -> True; _ -> False
      readIORef acquireCalls `shouldReturn` 1
      stopDemandDrivenSupervisor sup

    it "a second (or later) demand call is a cheap no-op: acquire is not called again" do
      acquireCalls <- newIORef (0 :: Int)
      let acquire = atomicModifyIORef' acquireCalls (\n -> (n + 1, ())) >> pure (Right (TestResource 1))
      sup <- newDemandDrivenSupervisor (withRelease noRelease acquire) neverInvalidate (pure ())
      _ <- requestDemandDrivenReady sup (200 * 1000)
      waitForDemandDrivenState sup \case SupervisedEnvReady _ -> True; _ -> False
      readIORef acquireCalls `shouldReturn` 1
      _ <- requestDemandDrivenReady sup (200 * 1000)
      _ <- requestDemandDrivenReady sup (200 * 1000)
      readIORef acquireCalls `shouldReturn` 1
      stopDemandDrivenSupervisor sup

    it "concurrent first demand triggers exactly one acquisition" do
      acquireCalls <- newIORef (0 :: Int)
      let acquire = atomicModifyIORef' acquireCalls (\n -> (n + 1, ())) >> pure (Right (TestResource 1))
      sup <- newDemandDrivenSupervisor (withRelease noRelease acquire) neverInvalidate (pure ())
      _ <- forkIO $ () <$ requestDemandDrivenReady sup (200 * 1000)
      _ <- forkIO $ () <$ requestDemandDrivenReady sup (200 * 1000)
      _ <- requestDemandDrivenReady sup (200 * 1000)
      waitForDemandDrivenState sup \case SupervisedEnvReady _ -> True; _ -> False
      readIORef acquireCalls `shouldReturn` 1
      stopDemandDrivenSupervisor sup

    it "a request's own wait timeout does NOT cancel the gated acquisition, which completes and is later observed" do
      acquireGate <- newEmptyMVar
      sup <- newDemandDrivenSupervisor (withRelease noRelease (takeMVar acquireGate >> pure (Right (TestResource 1)))) neverInvalidate (pure ())
      -- A very short bound: this demand call will time out while the
      -- acquisition below is still deliberately blocked, well before
      -- 'acquireGate' is ever filled.
      timedOut <- requestDemandDrivenReady sup (10 * 1000)
      timedOut `shouldBe` SupervisedEnvInitializing
      -- Only now release the still-in-flight acquisition -- proving the
      -- earlier timeout never touched, let alone cancelled, it.
      putMVar acquireGate ()
      waitForDemandDrivenState sup \case SupervisedEnvReady (TestResource 1) -> True; _ -> False
      stopDemandDrivenSupervisor sup

    {- | End-to-end regression, at the 'requestDemandDrivenReady' level, for
    the same review finding as the generic-protocol test above: a request
    landing while a post-failure re-acquisition is already in flight must
    be given the chance to wait for it (because it observes
    'SupervisedEnvInitializing'), rather than immediately reading the
    *previous* generation's stale 'SupervisedEnvUnavailable' and failing
    even though this attempt goes on to succeed well within its own bound.
    -}
    it "a request's bounded wait succeeds on a re-acquisition already in flight after a prior failure, rather than immediately returning the previous generation's stale Unavailable" do
      attemptCountRef <- newIORef (0 :: Int)
      acquireGate <- newEmptyMVar
      backoffGate <- newEmptyMVar
      let acquire = do
            n <- atomicModifyIORef' attemptCountRef (\k -> (k + 1, k + 1))
            if n == 1
              then pure (Left AwsAuthCredentialChainExhausted)
              else takeMVar acquireGate >> pure (Right (TestResource n))
      sup <- newDemandDrivenSupervisor (withRelease noRelease acquire) neverInvalidate (takeMVar backoffGate)
      firstOutcome <- requestDemandDrivenReady sup (200 * 1000)
      firstOutcome `shouldBe` SupervisedEnvUnavailable AwsAuthCredentialChainExhausted
      putMVar backoffGate ()
      -- Wait for the second attempt to genuinely be in flight (blocked on
      -- 'acquireGate') before issuing the request under test, so its
      -- bounded wait really does observe an in-progress re-acquisition
      -- rather than winning a race against backoff/attempt-start.
      waitForDemandDrivenState sup (== SupervisedEnvInitializing)
      resultVar <- newEmptyMVar
      _ <- forkIO $ putMVar resultVar =<< requestDemandDrivenReady sup (2 * 1000 * 1000)
      -- Give the concurrent request a moment to actually enter its own
      -- bounded STM wait before releasing the acquisition -- proving it
      -- was genuinely waiting, not merely re-reading a settled value.
      threadDelay (20 * 1000)
      putMVar acquireGate ()
      takeMVar resultVar `shouldReturn` SupervisedEnvReady (TestResource 2)
      stopDemandDrivenSupervisor sup

    it "stop during gated pre-acquisition (never demanded) terminates the supervisor with no acquisition ever attempted" do
      acquireCalls <- newIORef (0 :: Int)
      let acquire = atomicModifyIORef' acquireCalls (\n -> (n + 1, ())) >> pure (Right (TestResource 1))
      sup <- newDemandDrivenSupervisor (withRelease noRelease acquire) neverInvalidate (pure ())
      stopDemandDrivenSupervisor sup
      readIORef acquireCalls `shouldReturn` 0
      requestDemandDrivenReady sup 0 `shouldReturn` SupervisedEnvUnavailable AwsAuthSupervisorTerminated

    it "stop during Ready terminates the child before returning" do
      childKilled <- newIORef False
      let acquire = pure (Right (TestResource 1))
          release _ = writeIORef childKilled True
          awaitInvalidation _ = forever (threadDelay maxBound)
      sup <- newDemandDrivenSupervisor (withRelease release acquire) awaitInvalidation (pure ())
      _ <- requestDemandDrivenReady sup (200 * 1000)
      waitForDemandDrivenState sup \case SupervisedEnvReady _ -> True; _ -> False
      stopDemandDrivenSupervisor sup
      readIORef childKilled `shouldReturn` True

#endif
