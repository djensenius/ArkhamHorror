module Arkham.Api.GameBugUploadSpec (spec) where

import Amazonka (Error (..))
import Amazonka.Error (serviceError)
import Api.Arkham.AwsEnvSupervisor (AwsErrorCategory (..), AwsErrorDiagnostic (..))
import Api.Handler.Arkham.Game.Bug (
  BugUploadFailure (..),
  BugUploadOutcome (..),
  HeadObjectOutcome (..),
  classifyHeadObjectError,
  runBugUploadPolicy,
  runHeadObjectAction,
  runPutObjectAction,
 )
import Arkham.Prelude
import Data.Text qualified as T
import Network.HTTP.Client qualified as Client
import Network.HTTP.Types.Status (Status, status403, status404, status429, status500)
import System.IO.Unsafe (unsafePerformIO)
import Test.Hspec

{- | Wraps a value so that the *first* time it is forced (to WHNF), it
records that fact into @ref@ before returning the value unchanged. Used to
prove that 'runHeadObjectAction'\/'runPutObjectAction' force their
sanitized outcome *inside* the action, rather than returning an unforced
thunk that only happens to be forced later by the test's own
'shouldBe'\/'evaluate' call -- by checking @ref@'s flag *before* the test
itself ever inspects the returned value.

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

-- | A bare-bones S3 'ServiceError' carrying only the HTTP status that
-- 'classifyHeadObjectError' inspects. HeadObject responses have no body, so
-- production code never has more than this to go on either.
serviceErrorWithStatus :: Status -> Error
serviceErrorWithStatus status = ServiceError $ serviceError "S3" status [] Nothing Nothing Nothing

transportFailure :: Error
transportFailure = TransportError httpExceptionFixture

-- | A bare HTTP transport exception, used directly as 'transportFailure''s
-- payload, without ever needing to pattern-match back out of an existing
-- 'Error' value (which would require a partial fallback for constructors
-- that cannot occur).
httpExceptionFixture :: Client.HttpException
httpExceptionFixture = Client.InvalidUrlException "https://arkham-horror-bugs.s3.amazonaws.com" "connection failure"

{- | Regression for #30: 'postApiV1ArkhamGameBugR' caught only 'Amazonka.Error'
around HeadObject and treated *every* service error -- including
permission-denied, throttling, and 5xx responses, plus any transport
failure -- as "object missing", unconditionally proceeding to PUT and
returning the public success URL regardless of what actually happened.

'classifyHeadObjectError' and 'runBugUploadPolicy' are the exact functions
the production handler now uses to classify HeadObject failures and to
sequence the protected PUT. These tests exercise them directly. See
'Arkham.Api.AwsEnvSupervisorSpec' for the AWS credential-supervisor
lifecycle (acquisition, refresh-failure isolation, demand-gating) that
the production handler now consults before ever reaching these actions.
-}
spec :: Spec
spec = describe "bug report upload" do
  describe "classifyHeadObjectError" do
    it "classifies a genuine 404 as absent" do
      classifyHeadObjectError (serviceErrorWithStatus status404) `shouldBe` ObjectAbsent

    it "classifies a 403 permission-denied response as a failure, not absent" do
      classifyHeadObjectError (serviceErrorWithStatus status403) `shouldBe` HeadObjectFailed (AwsServiceFailure 403 AwsCategoryClientError)

    it "classifies a 429 throttling response as a failure, not absent" do
      classifyHeadObjectError (serviceErrorWithStatus status429) `shouldBe` HeadObjectFailed (AwsServiceFailure 429 AwsCategoryThrottled)

    it "classifies a 500 server error as a failure, not absent" do
      classifyHeadObjectError (serviceErrorWithStatus status500) `shouldBe` HeadObjectFailed (AwsServiceFailure 500 AwsCategoryServerError)

    it "classifies a transport-level failure (DNS/TLS/timeout/etc) as a failure, not absent" do
      classifyHeadObjectError transportFailure `shouldBe` HeadObjectFailed AwsTransportFailure

  describe "runBugUploadPolicy" do
    it "skips PUT and succeeds when the object already exists" do
      putCalls <- newIORef (0 :: Int)
      outcome <-
        runBugUploadPolicy
          (pure ObjectPresent)
          (modifyIORef' putCalls (+ 1) >> pure (Right ()))
      readIORef putCalls `shouldReturn` 0
      outcome `shouldBe` BugUploadSucceeded

    it "performs exactly one PUT and succeeds when the object is genuinely absent" do
      putCalls <- newIORef (0 :: Int)
      outcome <-
        runBugUploadPolicy
          (pure ObjectAbsent)
          (modifyIORef' putCalls (+ 1) >> pure (Right ()))
      readIORef putCalls `shouldReturn` 1
      outcome `shouldBe` BugUploadSucceeded

    it "fails without ever calling PUT when HeadObject fails for a non-404 reason" do
      putCalls <- newIORef (0 :: Int)
      outcome <-
        runBugUploadPolicy
          (pure $ classifyHeadObjectError (serviceErrorWithStatus status403))
          (modifyIORef' putCalls (+ 1) >> pure (Right ()))
      readIORef putCalls `shouldReturn` 0
      outcome `shouldBe` BugUploadFailed (HeadCheckFailed (AwsServiceFailure 403 AwsCategoryClientError))

    it "fails without ever calling PUT on a transport-level HeadObject failure" do
      putCalls <- newIORef (0 :: Int)
      outcome <-
        runBugUploadPolicy
          (pure $ classifyHeadObjectError transportFailure)
          (modifyIORef' putCalls (+ 1) >> pure (Right ()))
      readIORef putCalls `shouldReturn` 0
      outcome `shouldBe` BugUploadFailed (HeadCheckFailed AwsTransportFailure)

    it "reports failure (never a false success) when PUT itself fails" do
      outcome <- runBugUploadPolicy (pure ObjectAbsent) (pure $ Left (AwsServiceFailure 500 AwsCategoryServerError))
      outcome `shouldBe` BugUploadFailed (PutObjectFailed (AwsServiceFailure 500 AwsCategoryServerError))

    {- | Regression for the async-refresh/lazy-thunk audit: the diagnostic
    used to cross the 'runResourceT IO' -> 'Handler' boundary through a
    non-strict 'IORef' that could retain a thunk closing over the raw
    'Error' until something happened to force it later. It is now threaded
    straight through 'runBugUploadPolicy''s ordinary return value as a
    field of 'BugUploadFailure', built via 'classifyErrorDiagnostic' at the
    exact point of failure. 'evaluate' only forces its argument to weak
    head normal form, but every type reachable from @outcome@ here is
    declared with this package's default 'StrictData' extension (down to a
    plain 'Int'), so that single outer force cascades through every nested
    constructor -- the practical effect is the whole outcome ending up
    fully evaluated, not just its outer constructor. This is possible --
    without ever touching the original exception value again -- only
    because nothing about that exception survives past classification: a
    distinctive "secret" string embedded in the raw error's fields never
    appears anywhere in the forced outcome's textual representation.
    -}
    it "never retains any part of the raw exception once forced into a BugUploadFailure, even under full strict evaluation" do
      let secret = "s3-secret-req-id-must-never-leak-6f2a1"
      let err = ServiceError $ serviceError "S3" status500 [] (Just secret) (Just secret) (Just secret)
      outcome <- runBugUploadPolicy (pure $ classifyHeadObjectError err) (pure $ Right ())
      forced <- evaluate outcome
      forced `shouldBe` BugUploadFailed (HeadCheckFailed (AwsServiceFailure 500 AwsCategoryServerError))
      T.isInfixOf secret (tshow forced) `shouldBe` False

  {- | Regression for the PUT-result-laziness audit: a bare @pure $ either
  ... / case ... of ...@ only builds a thunk, so a failing PUT's
  classified diagnostic (and, transitively, whatever raw 'Error' it was
  built from) could still be unforced when 'runResourceT' returns --
  forced only incidentally, later, by whatever eventually pattern-matches
  the outcome. 'runHeadObjectAction'\/'runPutObjectAction' instead
  'evaluate' the classified outcome themselves, from inside the action,
  before returning it. 'markForcedOnceEvaluated' proves this happens
  *inside* the action under test: its "forced" flag is asserted 'True'
  immediately after the action returns, strictly before the test itself
  ever forces the returned value (e.g. via 'shouldBe').
  -}
  describe "runHeadObjectAction" do
    it "reports the object present without inspecting/forcing anything from a successful response" do
      runHeadObjectAction (pure (Right ("response" :: String))) `shouldReturn` ObjectPresent

    it "forces the classified diagnostic to normal form before returning, not deferring it as a thunk" do
      forcedRef <- newIORef False
      let poisonedErr = markForcedOnceEvaluated forcedRef (serviceErrorWithStatus status403)
      outcome <- runHeadObjectAction (pure (Left poisonedErr) :: IO (Either Error ()))
      -- Asserted before 'shouldBe' below ever forces 'outcome' itself, so
      -- this can only be 'True' already if 'runHeadObjectAction' forced
      -- the diagnostic internally.
      readIORef forcedRef `shouldReturn` True
      outcome `shouldBe` HeadObjectFailed (AwsServiceFailure 403 AwsCategoryClientError)

  describe "runPutObjectAction" do
    it "succeeds without inspecting/forcing anything from a successful response" do
      runPutObjectAction (pure (Right ("response" :: String))) `shouldReturn` Right ()

    it "forces the classified diagnostic to normal form before returning, not deferring it as a thunk" do
      forcedRef <- newIORef False
      let poisonedErr = markForcedOnceEvaluated forcedRef (serviceErrorWithStatus status500)
      outcome <- runPutObjectAction (pure (Left poisonedErr) :: IO (Either Error ()))
      -- As above: proves the forcing happened inside the action, not
      -- merely as a side effect of this test's own inspection of
      -- 'outcome'.
      readIORef forcedRef `shouldReturn` True
      outcome `shouldBe` Left (AwsServiceFailure 500 AwsCategoryServerError)
