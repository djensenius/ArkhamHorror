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

Every other role-based provider @discover@\/'discoverSafely' can reach --
container credentials ('fromContainerEnv'), @sts:AssumeRole@
('fromAssumedRole'), web identity ('fromWebIdentityEnv'\/'fromWebIdentity'),
and SSO ('fromSSO') -- remains an opaque call into the pinned Amazonka
fork, each internally still using @fetchAuthInBackground@ with no
interception point short of a full reimplementation from lower-level
service-client primitives (audited this round: @Amazonka.Auth.STS@
exposes no lower-level one-shot fetch primitive at all, only the
high-level @fromAssumedRole@\/@fromWebIdentity@; @Amazonka.Auth.SSO@ does
expose some -- @readCachedAccessToken@, @roleCredentialsToAuthEnv@ -- but a
full reimplementation was not undertaken this round). For exactly these
four providers, release still falls back to 'releaseAwsEnvChild''s
kill-then-bounded-poll, with the exact same non-genuine-termination and
misclassification residual risk described above. This is a deliberate,
documented, honest scope decision -- not a silent regression -- consistent
with the constraint that no fourth repository or unreviewed personal
dependency fork may be introduced to close it.
-}
module Api.Arkham.AwsEnvSupervisor (
  -- * AWS error diagnostics -- safe to cross IO boundaries and to log
  AwsErrorDiagnostic (..),
  AwsErrorCategory (..),
  classifyErrorDiagnostic,
  AwsAuthErrorDiagnostic (..),
  classifyAuthErrorDiagnostic,

  -- * Credential acquisition\/release primitives -- exposed for regression tests
  ChildReleaseOutcome (..),
  ChildReleaseTimedOutException (..),
  releaseAwsEnvChild,
  requireChildReleased,
  releaseAwsEnvGeneration,
  acquireAwsEnv,
  acquireRegionBeforeAuth,
  awaitAwsEnvInvalidation,
  managedFetchAuthInBackground,

  -- * Config-file credential provider graph -- exposed for regression tests
  AwsEnvAcquisition (..),
  releaseAwsEnvAcquisition,
  ConfigProfileResolvers (..),
  productionConfigProfileResolvers,
  safeLoadIniFile,
  safeEvalConfigProfile,
  safeFileEnv,
  discoverSafely,

  -- * Generic single-thread supervisor protocol -- exposed for regression tests
  SupervisedEnvState (..),
  SupervisedEnv,
  startSupervisedEnv,
  readSupervisedEnv,
  SupervisorStopOutcome (..),
  stopSupervisedEnv,

  -- * Generic demand-driven wrapper -- exposed for regression tests
  DemandDrivenSupervisor,
  newDemandDrivenSupervisor,
  requestDemandDrivenReady,
  stopDemandDrivenSupervisor,

  -- * The application's demand-driven AWS 'Env' supervisor
  AwsEnvSupervisor,
  newAwsEnvSupervisor,
  requestAwsEnvReady,
  stopAwsEnvSupervisor,
) where

import Amazonka (AuthEnv, Env, Env' (..), EnvNoAuth, Error (..), ISO8601, Region, SerializeError (..), ServiceError (..), expiration, fromTime, newEnv)
import Amazonka.Auth (Auth (..), AuthError (..), fromContainerEnv, fromKeysEnv, fromWebIdentityEnv, runCredentialChain)
import Amazonka.Auth.ConfigFile (ConfigProfile (..), CredentialSource (..), configPathRelative, mergeConfigs, parseConfigProfile)
import Amazonka.Auth.SSO (fromSSO, relativeCachedTokenFile)
import Amazonka.Auth.STS (fromAssumedRole, fromWebIdentity)
import Amazonka.EC2.Metadata hiding (region)
import Amazonka.EC2.Metadata qualified as IdentityDocument (IdentityDocument (..))
import Amazonka.Env (lookupRegion)
import Api.Arkham.Lifecycle (
  ManagedThread,
  cancelManagedThread,
  managedThreadId,
  spawnManagedThread,
  waitManagedThread,
 )
import Arkham.Prelude
import Control.Concurrent (ThreadId, killThread, myThreadId, threadDelay)
import Control.Concurrent.STM (check, retry)
import Control.Exception qualified as Exception
import Data.ByteString.Char8 qualified as BS8
import Data.ByteString.Lazy.Char8 qualified as LBS8
import Data.HashMap.Strict qualified as HashMap
import Data.Ini qualified as INI
import Data.Text qualified as Text
import Data.Text.Encoding qualified as TE
import Data.Time (diffUTCTime)
import Data.Void (Void, absurd)
import GHC.Conc.Sync (ThreadStatus (..), threadStatus)
import Network.HTTP.Types.Status (statusCode)
import System.Directory qualified as Directory
import System.Environment (lookupEnv)

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
'threadStatus' afterward, bounded, so 'releaseAwsEnvAcquisition' can rely
on every child it releases being genuinely terminal by the time it
returns -- but, unlike an earlier version of this function, it no longer
silently reports success ('()') once that bound is exhausted while the
child is still alive: it honestly returns 'ChildReleaseTimedOut', which
'releaseAwsEnvAcquisition'\/'releaseAwsEnvGeneration' then turn into a
thrown failure rather than a false \"released\" claim. The bound only
guards against a genuinely stuck foreign call (e.g. 'killThread' targeting
a thread currently blocked inside a non-interruptible foreign HTTP call
can itself block until that call completes; this is a documented,
accepted platform limitation for these four opaque, pinned providers --
see the module Haddock's \"Managed credential refresh\" section -- not
something this function can or should silently paper over) -- it never
substitutes for a real synchronization guarantee, and never lies about
one either.
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
    Just _ -> do
      callingThread <- myThreadId
      credentialsRef <- newIORef initial
      stopRequestedRef <- newIORef False
      refreshThread <- spawnManagedThread (refreshLoop callingThread credentialsRef stopRequestedRef)
      let release = do
            -- Recorded *before* delivering cancellation (see
            -- 'cancelManagedThread'), so a caught exception the refresh
            -- loop observes as a *result* of this release can always be
            -- correctly recognized as expected, never misreported.
            writeIORef stopRequestedRef True
            cancelManagedThread refreshThread
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
          Right refreshed -> writeIORef credentialsRef refreshed >> go
          Left err -> do
            stopRequested <- readIORef stopRequestedRef
            unless stopRequested $ Exception.throwTo callingThread (reclassifyAsAuthError err)

  reclassifyAsAuthError :: Exception.SomeException -> AuthError
  reclassifyAsAuthError err
    | Just authErr@(RetrievalError _) <- Exception.fromException err = authErr
    | Just authErr@(AuthServiceError _) <- Exception.fromException err = authErr
    | otherwise = OtherAuthError err

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

Every child's release is attempted even if an earlier one fails: this
generation's own children can only ever target /this generation's own/
dedicated thread with a delayed refresh-failure 'AuthError' (see
'startSupervisedEnv''s per-generation thread isolation for why that
target can never be a different generation's thread, nor this module's
long-lived dispatcher) -- so an 'AuthError' surfacing while releasing one
child here is provably just that same self-inflicted, expected feedback
from killing it mid-refresh, not a fresh failure, and is discarded rather
than aborting the remaining releases or being misreported. Any other
(non-'AuthError') exception -- e.g. a genuine external cancellation
landing while a specific child's release was in flight -- is never
swallowed: every remaining child's release is still attempted regardless,
but the first such genuine exception seen is re-raised only once every
child has at least been attempted, preserving the original
caller-visible failure\/cancellation exactly as if this function's own
cleanup work were transparent.
-}
releaseAwsEnvAcquisition :: AwsEnvAcquisition -> IO ()
releaseAwsEnvAcquisition (AwsEnvAcquisition _env finalRelease hiddenReleases) = do
  let orderedReleases = finalRelease : reverse hiddenReleases
  firstGenuineFailure <- foldM attemptRelease Nothing orderedReleases
  for_ firstGenuineFailure Exception.throwIO
 where
  attemptRelease :: Maybe Exception.SomeException -> IO () -> IO (Maybe Exception.SomeException)
  attemptRelease firstGenuineFailure release =
    Exception.try @Exception.SomeException release >>= \case
      Right () -> pure firstGenuineFailure
      Left e
        | Just (_ :: AuthError) <- Exception.fromException e -> pure firstGenuineFailure
        | otherwise -> pure (Just (fromMaybe e firstGenuineFailure))

{- | A drop-in replacement for the pinned Amazonka fork's own
@Amazonka.Auth.discover@, identical in every credential source it tries,
their order, and their error classification -- except for the instance
profile\/IMDS provider (replaced with 'safeDefaultInstanceProfile') and the
config-file provider (replaced with 'safeFileEnv'). See those functions'
Haddocks for exactly why, and for the pinned source
(@lib/amazonka/src/Amazonka/Auth/InstanceProfile.hs@\/@ConfigFile.hs@ at
@b562aa3f24845e34b95748daae671860017426be@) each reimplements.

Every other provider @discover@ tries was audited directly against that
same pinned commit and found /already/ safe to use unmodified:

* 'fromKeysEnv' (@Amazonka\/Auth\/Keys.hs@) reads static, non-expiring
  credentials and never calls 'fetchAuthInBackground' at all -- no
  background thread is ever forked, so there is nothing a subsequent
  failure could orphan.
* 'fromWebIdentityEnv' (@Amazonka\/Auth\/STS.hs@, @fromWebIdentity@) and
  'fromContainerEnv' (@Amazonka\/Auth\/Container.hs@, @fromContainer@) do
  call 'fetchAuthInBackground', but as the /last/ fallible action before
  returning -- nothing that can throw follows it, so a background child it
  creates is always safely attached to the 'Env' these functions return.

'fromDefaultInstanceProfile'\/'fromNamedInstanceProfile' were the first
provider found where a fallible operation (@getRegionFromIdentity@, a
separate IMDS metadata call) runs /after/ 'fetchAuthInBackground' may have
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
second 'IORef', @finalReleaseRef@, similarly lets 'safeDefaultInstanceProfile'
report a smarter, genuinely-awaiting release for @.auth@ itself when it
wins outright (see 'AwsEnvAcquisition'); every other provider leaves it
untouched, and 'acquireAwsEnv' then falls back to the naive derivation.
-}
discoverSafely :: IORef [IO ()] -> IORef (Maybe (IO ())) -> EnvNoAuth -> IO Env
discoverSafely hiddenReleasesRef finalReleaseRef =
  runCredentialChain
    [ fromKeysEnv
    , safeFileEnv productionConfigProfileResolvers hiddenReleasesRef
    , fromWebIdentityEnv
    , fromContainerEnv
    , safeDefaultInstanceProfile finalReleaseRef
    ]


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
catch papering over one. See 'acquireRegionBeforeAuth' for the exact
ordering, factored out so it is directly, deterministically testable
without a real IMDS endpoint.
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
  IORef (Maybe (IO ())) ->
  Env' withAuth ->
  m Env
safeDefaultInstanceProfile finalReleaseRef env =
  liftIO $ do
    ls <-
      Exception.try $ metadata (manager env) (IAM (SecurityCredentials Nothing))
    case BS8.lines <$> ls of
      Right (x : _) -> safeNamedInstanceProfile finalReleaseRef (TE.decodeUtf8 x) env
      Left e -> Exception.throwIO (RetrievalError e)
      _ ->
        Exception.throwIO
          $ InvalidIAMError "Unable to get default IAM Profile from EC2 metadata"

{- | See 'safeDefaultInstanceProfile'. Identical to the pinned source's
@fromNamedInstanceProfile@ except @getRegionFromIdentity@ is resolved
before @fetchAuthInBackground@, not after -- via 'acquireRegionBeforeAuth'
-- and @fetchAuthInBackground@ itself is replaced with
'managedFetchAuthInBackground', whose genuinely-awaiting release is
recorded into @finalReleaseRef@ for 'acquireAwsEnv'\/'safeEvalConfigProfile'
to pick up (see 'AwsEnvAcquisition'). Written only on success -- by
construction, 'acquireRegionBeforeAuth' never runs @getAuth@ (and so never
runs this write) unless @getRegion@ already succeeded, and this function
as a whole only returns once both have.
-}
safeNamedInstanceProfile ::
  (MonadIO m) =>
  IORef (Maybe (IO ())) ->
  Text ->
  Env' withAuth ->
  m Env
safeNamedInstanceProfile finalReleaseRef name env@Env {manager} =
  liftIO $ acquireRegionBeforeAuth env getRegionFromIdentity getAuth
 where
  getAuth = do
    (auth, release) <- managedFetchAuthInBackground getCredentials
    writeIORef finalReleaseRef (Just release)
    pure auth

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
    Right env -> do
      hiddenReleases <- readIORef hiddenReleasesRef
      finalRelease <- readIORef finalReleaseRef
      let release = fromMaybe (requireChildReleased (runIdentity env.auth)) finalRelease
          acquisition =
            AwsEnvAcquisition
              { awsEnvAcquisitionEnv = env
              , awsEnvAcquisitionRelease = release
              , awsEnvAcquisitionHiddenReleases = hiddenReleases
              }
      pure $ Right (env, releaseAwsEnvAcquisition acquisition)


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
  , resolveEc2Source :: forall withAuth. IORef (Maybe (IO ())) -> Env' withAuth -> IO Env
  -- ^ @credential_source=Ec2InstanceMetadata@ -- deliberately
  -- 'safeDefaultInstanceProfile', /not/ the pinned source's own
  -- @fromDefaultInstanceProfile@, which 'Amazonka.Auth.ConfigFile'
  -- reaches directly and unsafely for this exact credential source. The
  -- 'IORef' lets it report a genuinely-awaiting
  -- 'managedFetchAuthInBackground' release, exactly as the top-level
  -- 'discoverSafely' entry does -- see 'resolveCredentialSourceAcquisition'.
  , resolveEcsSource :: forall withAuth. Env' withAuth -> IO Env
  -- ^ @credential_source=EcsContainer@.
  , resolveAssumedRole :: Text -> Env -> IO Env
  -- ^ @sts:AssumeRole@ onto an already-resolved source 'Env'.
  , resolveWebIdentity :: forall withAuth. FilePath -> Text -> Maybe Text -> Env' withAuth -> IO Env
  -- ^ @AssumeRoleWithWebIdentity@.
  , resolveSSO :: forall withAuth. FilePath -> Region -> Text -> Text -> Env' withAuth -> IO Env
  -- ^ @AssumeRoleViaSSO@.
  }

productionConfigProfileResolvers :: ConfigProfileResolvers
productionConfigProfileResolvers =
  ConfigProfileResolvers
    { resolveEnvironmentSource = fromKeysEnv
    , resolveEc2Source = safeDefaultInstanceProfile
    , resolveEcsSource = fromContainerEnv
    , resolveAssumedRole = \roleArn -> fromAssumedRole roleArn "amazonka-assumed-role"
    , resolveWebIdentity = fromWebIdentity
    , resolveSSO = fromSSO
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

  leaf :: IO Env -> IO AwsEnvAcquisition
  leaf act = do
    finalEnv <- act
    pure
      AwsEnvAcquisition
        { awsEnvAcquisitionEnv = finalEnv
        , awsEnvAcquisitionRelease = requireChildReleased (runIdentity finalEnv.auth)
        , awsEnvAcquisitionHiddenReleases = []
        }

  -- | Resolve a source (recursively, or a leaf 'CredentialSource'), then
  -- wrap it with @wrap@ (@fromAssumedRole@). If @wrap@ throws or this
  -- thread is cancelled, release everything the source already
  -- accumulated (including its own now-to-be-hidden @.auth@) before
  -- propagating; on success, the source's own @.auth@ becomes hidden (no
  -- longer reachable via the returned 'Env') and its recorded
  -- 'awsEnvAcquisitionRelease' (the correct release for that @.auth@,
  -- whether naive or 'managedFetchAuthInBackground'-derived -- see
  -- 'resolveCredentialSourceAcquisition') is appended to the accumulated
  -- hidden-release list.
  assumeRoleOnto :: IO AwsEnvAcquisition -> (Env -> IO Env) -> IO AwsEnvAcquisition
  assumeRoleOnto acquireSource wrap = do
    sourceAcquisition <- acquireSource
    let sourceEnv = awsEnvAcquisitionEnv sourceAcquisition
        sourceRelease = awsEnvAcquisitionRelease sourceAcquisition
    ( do
        finalEnv <- wrap sourceEnv
        pure
          AwsEnvAcquisition
            { awsEnvAcquisitionEnv = finalEnv
            , awsEnvAcquisitionRelease = requireChildReleased (runIdentity finalEnv.auth)
            , awsEnvAcquisitionHiddenReleases = awsEnvAcquisitionHiddenReleases sourceAcquisition <> [sourceRelease]
            }
      )
      `Exception.onException` releaseAwsEnvAcquisition sourceAcquisition

  resolveProfile :: ConfigProfile -> IO AwsEnvAcquisition
  resolveProfile = \case
    ExplicitKeys authKeys -> leaf $ pure env {auth = Identity (Auth authKeys)}
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
  -- about to be hidden, never the final visible one). @Ec2InstanceMetadata@
  -- gets its own fresh, per-call release ref so 'resolveEc2Source'
  -- (production: 'safeDefaultInstanceProfile') can report a genuinely-
  -- awaiting 'managedFetchAuthInBackground' release exactly as the
  -- top-level 'discoverSafely' entry does, rather than the naive '.auth'
  -- derivation 'leaf' uses for every other, non-managed source.
  resolveCredentialSourceAcquisition :: CredentialSource -> IO AwsEnvAcquisition
  resolveCredentialSourceAcquisition = \case
    Environment -> leaf (resolveEnvironmentSource resolvers env)
    Ec2InstanceMetadata -> do
      sourceFinalReleaseRef <- newIORef Nothing
      finalEnv <- resolveEc2Source resolvers sourceFinalReleaseRef env
      sourceFinalRelease <- readIORef sourceFinalReleaseRef
      pure
        AwsEnvAcquisition
          { awsEnvAcquisitionEnv = finalEnv
          , awsEnvAcquisitionRelease =
              fromMaybe (requireChildReleased (runIdentity finalEnv.auth)) sourceFinalRelease
          , awsEnvAcquisitionHiddenReleases = []
          }
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

Writes the resolved profile's hidden-release list into @hiddenReleasesRef@
-- read back by 'acquireAwsEnv' once 'discoverSafely' as a whole succeeds
-- but only on this function's own success: every failure path upstream in
'safeEvalConfigProfile' already self-cleans via 'Exception.onException'
before any exception ever reaches here, so this ledger always reflects
either this attempt's complete hidden-release list, or (if this provider
lost to an earlier one in 'discoverSafely''s chain, or failed outright)
remains untouched.
-}
safeFileEnv :: ConfigProfileResolvers -> IORef [IO ()] -> Env' withAuth -> IO Env
safeFileEnv resolvers hiddenReleasesRef env = do
  profileName <- maybe "default" Text.pack <$> lookupEnv "AWS_PROFILE"
  credentialsPath <- maybe (configPathRelative "/.aws/credentials") pure =<< lookupEnv "AWS_SHARED_CREDENTIALS_FILE"
  configPath <- maybe (configPathRelative "/.aws/config") pure =<< lookupEnv "AWS_CONFIG_FILE"
  credentialsIni <- tolerateMissingOrInvalid (safeLoadIniFile credentialsPath)
  configIni <- tolerateMissingOrInvalid (safeLoadIniFile configPath)
  let config = mergeConfigs credentialsIni configIni
  acquisition <- safeEvalConfigProfile resolvers config [] profileName env
  regionOverride <- lookupRegion
  let resolvedEnv = maybe id (\r e -> e {region = r}) regionOverride (awsEnvAcquisitionEnv acquisition)
  writeIORef hiddenReleasesRef (awsEnvAcquisitionHiddenReleases acquisition)
  pure resolvedEnv
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
startSupervisedEnv acquire awaitInvalidation backoff = do
  stateVar <- newTVarIO SupervisedEnvInitializing
  supervisorThread <- spawnManagedThread (supervise stateVar)
  pure SupervisedEnv {supervisedEnvStateVar = stateVar, supervisedEnvThread = supervisorThread}
 where
  supervise stateVar =
    forever loopOnce `finally` publish stateVar AwsAuthSupervisorTerminated
   where
    loopOnce = do
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
      generationThread <- spawnManagedThread runOneGeneration
      diag <- waitManagedThread generationThread `Exception.onException` cancelManagedThread generationThread
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

