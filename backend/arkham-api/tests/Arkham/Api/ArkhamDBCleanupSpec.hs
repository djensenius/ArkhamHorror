module Arkham.Api.ArkhamDBCleanupSpec where

import Api.Arkham.ArkhamDBCleanup (withCleanupWorker)
import Control.Concurrent (newEmptyMVar, newMVar, putMVar, readMVar, takeMVar, tryTakeMVar, modifyMVar)
import Control.Exception (finally, throwIO)
import Relude hiding (newEmptyMVar, newMVar, putMVar, readMVar, takeMVar, tryTakeMVar)
import System.IO.Error (ioeGetErrorString, userError)
import System.Timeout (timeout)
import Test.Hspec

spec :: Spec
spec = describe "ArkhamDB cleanup worker lifecycle" do
  it "retries after a synchronous failure without forwarding exception contents" do
    attempts <- newMVar (0 :: Int)
    retried <- newEmptyMVar
    warned <- newEmptyMVar
    let sweep = do
          attempt <- modifyMVar attempts $ \n -> pure (n + 1, n)
          if attempt == 0 then throwIO $ userError "private-pending-credential"
            else putMVar retried ()
    bounded $ withCleanupWorker 1000 1000000 sweep (putMVar warned ()) do
      takeMVar retried
      takeMVar warned
    readMVar attempts >>= (`shouldSatisfy` (>= 2))

  it "cancels and joins an in-flight sweep on normal server return" do
    started <- newEmptyMVar
    blocked <- newEmptyMVar
    stopped <- newEmptyMVar
    let sweep = (putMVar started () >> takeMVar blocked)
          `finally` putMVar stopped ()
    bounded $ withCleanupWorker 1000 1000000 sweep (pure ()) $
      takeMVar started
    tryTakeMVar stopped `shouldReturn` Just ()

  it "cancels and joins an in-flight sweep on a server exception" do
    started <- newEmptyMVar
    blocked <- newEmptyMVar
    stopped <- newEmptyMVar
    let sweep = (putMVar started () >> takeMVar blocked)
          `finally` putMVar stopped ()
        server = takeMVar started >> throwIO (userError "server-failed")
    bounded (withCleanupWorker 1000 1000000 sweep (pure ()) server)
      `shouldThrow` (\err -> ioeGetErrorString err == "server-failed")
    tryTakeMVar stopped `shouldReturn` Just ()

  it "times out a stalled sweep, warns, and schedules another attempt" do
    attempts <- newMVar (0 :: Int)
    blocked <- newEmptyMVar
    retried <- newEmptyMVar
    warned <- newEmptyMVar
    let sweep = do
          attempt <- modifyMVar attempts $ \n -> pure (n + 1, n)
          if attempt == 0 then takeMVar blocked else putMVar retried ()
    bounded $ withCleanupWorker 1000 10000 sweep (putMVar warned ()) do
      takeMVar retried
      takeMVar warned
    readMVar attempts >>= (`shouldSatisfy` (>= 2))

  it "continues scheduling if the warning sink also fails" do
    attempts <- newMVar (0 :: Int)
    retried <- newEmptyMVar
    let sweep = do
          attempt <- modifyMVar attempts $ \n -> pure (n + 1, n)
          when (attempt > 0) $ putMVar retried ()
          throwIO $ userError "private-driver-error"
        warn = throwIO $ userError "logger-failed"
    bounded $ withCleanupWorker 1000 1000000 sweep warn $ takeMVar retried
    readMVar attempts >>= (`shouldSatisfy` (>= 2))

bounded :: IO a -> IO a
bounded action = timeout 2000000 action >>= maybe (fail "cleanup lifecycle test timed out") pure
