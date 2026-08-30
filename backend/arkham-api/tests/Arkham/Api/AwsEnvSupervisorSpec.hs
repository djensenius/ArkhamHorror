module Arkham.Api.AwsEnvSupervisorSpec (spec) where

import Amazonka (AuthEnv (..), Env, Env' (..), Error (..), Region (..), SerializeError (..), Time (..), newEnvNoAuth)
import Amazonka.Auth (Auth (..), AuthError (..))
import Amazonka.Error (serviceError)
import Api.Arkham.AwsEnvSupervisor (
  AwsAuthErrorDiagnostic (..),
  AwsErrorCategory (..),
  AwsErrorDiagnostic (..),
  AwsEnvAcquisition (..),
  ChildReleaseOutcome (..),
  ChildReleaseTimedOutException (..),
  ConfigProfileResolvers (..),
  DemandDrivenSupervisor,
  ManagedEnvAcquisition,
  SupervisedEnv,
  SupervisedEnvState (..),
  acquireRegionBeforeAuth,
  classifyAuthErrorDiagnostic,
  classifyErrorDiagnostic,
  managedFetchAuthInBackground,
  newDemandDrivenSupervisor,
  pairManagedAcquisition,
  polledRelease,
  readSupervisedEnv,
  releaseAwsEnvAcquisition,
  releaseAwsEnvChild,
  requestDemandDrivenReady,
  requireChildReleased,
  safeEvalConfigProfile,
  safeFileEnv,
  staticEnvAcquisition,
  startSupervisedEnv,
  startSupervisedEnvUsing,
  stopDemandDrivenSupervisor,
  stopSupervisedEnv,
 )
import Arkham.Prelude
import Control.Concurrent (ThreadId, forkIO, killThread, myThreadId, threadDelay)
import Control.Exception qualified as Exception
import Data.HashMap.Strict qualified as HashMap
import Data.Time (addUTCTime)
import Data.Void (absurd)
import GHC.Conc.Sync (ThreadStatus (..), threadStatus)
import Network.HTTP.Client qualified as Client
import Network.HTTP.Types.Status (status403, status404, status500)
import System.Directory qualified as Directory
import System.Environment (lookupEnv, setEnv, unsetEnv)
import System.IO.Unsafe (unsafePerformIO)
import Test.Hspec

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

Uses 'polledRelease' (built on 'requireChildReleased', not a bare
'killThread') so this fake's release genuinely blocks until the child is
confirmed terminal, matching every real managed release's own contract
-- tests that assert an exact multi-child release /order/ depend on
this: a release that only signals without awaiting could let
'releaseAwsEnvAcquisition''s next release begin before this child's own
handler has actually finished recording its effect.
-}
fakeRefEnv :: Env' withAuth -> IO (ManagedEnvAcquisition, ThreadId)
fakeRefEnv env = do
  tid <- forkIO (forever (threadDelay maxBound))
  cell <- newIORef (AuthEnv "AKIAFAKE" "secret" Nothing Nothing)
  let auth = Ref tid cell
  pure (pairManagedAcquisition env {auth = Identity auth} (polledRelease auth), tid)

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
  constructors are 'pairManagedAcquisition' (which requires an actual
  'ManagedRelease' value, itself producible only by 'managedRelease' or
  'polledRelease' wrapping a genuine, awaited managed-refresh handle) and
  'staticEnvAcquisition' (which is itself runtime-checked to reject any
  'Env' whose @.auth@ is 'Ref'-shaped). There is no longer an 'IORef'
  side channel a resolver could omit writing to, nor a bare @(Env, IO
  ())@ tuple a resolver could construct by hand with an arbitrary\/no-op
  release. A \"bare expiring resolver\" in the old sense is now not
  merely rejected at runtime but literally unrepresentable: any attempt
  to write @resolveEcsSource = \\env -> pure env {auth = Identity (Ref tid
  cell)}@ (returning a bare 'Env', as the old buggy/forgetful shape did),
  or even @resolveEcsSource = \\env -> pairManagedAcquisition env {auth =
  Identity (Ref tid cell)} (ManagedRelease (pure ()))@ (fabricating a
  fake, non-awaiting release), is a /compile/-time type error or an
  inaccessible-constructor error, not something this module could even
  attempt to run and catch. This is a strictly stronger guarantee than
  the previous version of this test (which merely proved a runtime
  'ManagedReleaseInvariantViolated' exception was thrown for such a
  value) could express, so no equivalent executable test exists here:
  there is no longer any way to construct the input the old test needed.
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
  audit finding: for a chain of three real refresh children (@root@,
  static, wrapped by @a@'s own assumed role, wrapped in turn by @b@'s,
  wrapped in turn by @c@'s -- the final, directly-visible child),
  'releaseAwsEnvAcquisition' must release the still-visible outer child
  (@c@) FIRST, then the hidden children newest-to-oldest (@b@, then
  @a@) -- i.e. exactly @c, b, a@ -- never the old, buggy @b, a, c@ order
  (hidden-then-outer). Each fake child records its own label into a
  shared, order-preserving list the instant it is actually killed (from
  inside its own exception handler, not merely once
  'releaseAwsEnvChild''s\/'awaitThreadTerminated''s call returns), so this
  proves the true release sequence, not just that all three eventually
  terminate.

  Mutation check: reverting 'releaseAwsEnvAcquisition' to its previous
  @sequence_ (reverse hiddenReleases) >> releaseAwsEnvChild ...@ order
  (hidden newest-to-oldest, then outer last) makes this test fail with
  the order @[\"b\",\"a\",\"c\"]@ instead of the required @[\"c\",\"b\",\"a\"]@.
  -}
  it "releases the still-visible outer child first, then hidden children newest-to-oldest: exact order c, b, a for a three-deep chain" do
    envNoAuth <- newEnvNoAuth
    let config =
          HashMap.fromList
            [ ("c", profileMap [("role_arn", "arn:aws:iam::1:role/c"), ("source_profile", "b")])
            , ("b", profileMap [("role_arn", "arn:aws:iam::1:role/b"), ("source_profile", "a")])
            , ("a", profileMap [("role_arn", "arn:aws:iam::1:role/a"), ("source_profile", "root")])
            , ("root", profileMap [("aws_access_key_id", "AKIAROOT"), ("aws_secret_access_key", "s")])
            ]
    releaseOrderRef <- newIORef ([] :: [Text])
    let labelFor roleArn
          | roleArn == "arn:aws:iam::1:role/a" = "a"
          | roleArn == "arn:aws:iam::1:role/b" = "b"
          | roleArn == "arn:aws:iam::1:role/c" = "c"
          | otherwise = error "unexpected role_arn in three-deep order test"
        resolvers =
          unreachableConfigProfileResolvers
            { resolveAssumedRole = \roleArn sourceEnv -> do
                let label = labelFor roleArn
                -- A readiness gate, signalled from strictly inside the
                -- already-installed 'Exception.catch' handler region,
                -- before this function returns: without it, a
                -- freshly-forked child could still be killed before it
                -- has ever installed its handler (its very first
                -- scheduler slice not yet run), silently dying via an
                -- unmasked, uncaught 'ThreadKilled' with nothing
                -- recorded -- exactly the same forkIO/catch-installation
                -- race documented on 'forkTransferringOwnership''s own
                -- cancellation test below.
                readyGate <- newEmptyMVar
                tid <-
                  forkIO
                    $ Exception.catch
                      (putMVar readyGate () >> forever (threadDelay maxBound))
                    $ \Exception.ThreadKilled ->
                      atomicModifyIORef' releaseOrderRef (\labels -> (labels <> [label], ()))
                takeMVar readyGate
                cell <- newIORef (AuthEnv "AKIAFAKE" "secret" Nothing Nothing)
                let auth = Ref tid cell
                -- 'polledRelease' (built on 'requireChildReleased', not a
                -- bare 'killThread'): the exact release-order assertion
                -- below depends on each release genuinely awaiting its
                -- own child's confirmed termination before returning,
                -- not merely signalling it.
                pure (pairManagedAcquisition sourceEnv {auth = Identity auth} (polledRelease auth))
            }
    acquisition <- safeEvalConfigProfile resolvers config [] "c" envNoAuth
    -- Three hidden releases: root's own (a static, no-op release, since
    -- root uses plain access keys rather than an assumed role), plus the
    -- real "a" and "b" children -- "c"'s own auth is the visible outer
    -- one, released separately (see 'releaseAwsEnvAcquisition').
    length (awsEnvAcquisitionHiddenReleases acquisition) `shouldBe` 3
    releaseAwsEnvAcquisition acquisition
    -- Bounded poll: each fake child's own handler above appends its
    -- label synchronously as part of being killed, so once all three
    -- labels are present the true release order is already fixed and
    -- will never change further.
    let waitForAllLabels (n :: Int)
          | n <= 0 = expectationFailure "not all three children were ever released"
          | otherwise = do
              labels <- readIORef releaseOrderRef
              if length labels >= 3 then pure () else threadDelay 1000 >> waitForAllLabels (n - 1)
    waitForAllLabels 2000
    readIORef releaseOrderRef `shouldReturn` ["c", "b", "a"]

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
spec :: Spec
spec = describe "AWS Env supervisor" do
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
      (targetAuth, releaseTarget) <-
        managedFetchAuthInBackground (pure (AuthEnv "AKIAEXAMPLE" "secret" Nothing (Just farFuture)))
      case targetAuth of
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
        _ -> expectationFailure "expected managedFetchAuthInBackground to return a Ref for a far-future expiration, not a static Auth"

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
        (_auth, _release) <- managedFetchAuthInBackground getAuthEnv
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
              (_auth, release) <- managedFetchAuthInBackground getAuthEnv
              -- No gate, no delay: release is issued immediately,
              -- deliberately racing whatever the freshly-spawned refresh
              -- loop is doing at that exact moment.
              release
            case outcome of
              Left (_ :: AuthError) -> atomicModifyIORef' strayCountRef (\n -> (n + 1, ()))
              Right () -> pure ()
      replicateM_ iterations oneTrial
      readIORef strayCountRef `shouldReturn` 0

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
                  "the forked stopSupervisedEnv attempt never reached a \
                  \genuinely blocked state trying to deliver its \
                  \cancellation to the dispatcher"
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
