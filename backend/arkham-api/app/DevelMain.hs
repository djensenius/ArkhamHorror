-- \$ stack ghci arkham-horror-backend:lib --no-load --work-dir .stack-work-devel
--
-- 2. Load this module
--
-- > :l app/DevelMain.hs
--
-- 3. Run @update@
--
-- > DevelMain.update
--
-- 4. Your app should now be running, you can connect at http://localhost:3000
--
-- 5. Make changes to your code
--
-- 6. After saving your changes, reload by running:
--
-- > :r
-- > DevelMain.update
--
-- You can also call @DevelMain.shutdown@ to stop the app
--
-- There is more information about this approach,
-- on the wiki: https://github.com/yesodweb/yesod/wiki/ghci
--
-- WARNING: GHCi does not notice changes made to your template files.
-- If you change a template, you'll need to either exit GHCi and reload,
-- or manually @touch@ another Haskell module.

{- | Running your app inside GHCi.

This option provides significantly faster code reload compared to
@yesod devel@. However, you do not get automatic code reload
(which may be a benefit, depending on your perspective). To use this:

1. Start up GHCi
-}
module DevelMain where

import Api.Arkham.Lifecycle (proceedOnlyIfPreviousShutdownSucceeded, shutdownThenDeliver)
import Application (getApplicationRepl, shutdownApp)
import Prelude

import Control.Concurrent
import Control.Exception (SomeException)
import Control.Monad ((>=>))
import Data.IORef
import Foreign.Store
import GHC.Word
import Network.Wai.Handler.Warp

{- | Start or restart the server.
newStore is from foreign-store.
A Store holds onto some data across ghci reloads
-}
update :: IO ()
update = do
  mtidStore <- lookupStore tidStoreNum
  case mtidStore of
    -- no server running
    Nothing -> do
      done <- storeAction doneStore newEmptyMVar
      tid <- start done
      _ <- storeAction (Store tidStoreNum) (newIORef tid)
      pure ()
    -- server is already running
    Just tidStore -> restartAppInNewThread tidStore
 where
  doneStore :: Store (MVar (Either SomeException ()))
  doneStore = Store 0

  -- Shut the server down with killThread, wait for the previous
  -- generation's shutdown result, and only start a replacement if that
  -- shutdown actually succeeded ('Right'). A failed/interrupted shutdown
  -- ('Left') is reported and re-thrown rather than silently starting a
  -- new generation on top of a supervisor that may not have actually
  -- stopped -- this is exactly what 'shutdownThenDeliver' (used in
  -- 'start' below) exists to make observable instead of deadlocking this
  -- 'takeMVar' forever.
  restartAppInNewThread :: Store (IORef ThreadId) -> IO ()
  restartAppInNewThread tidStore = modifyStoredIORef tidStore $ \tid -> do
    killThread tid
    result <- withStore doneStore takeMVar
    case result of
      Left err ->
        putStrLn
          $ "DevelMain: the previous generation's shutdown failed; NOT starting a replacement: "
          <> show err
      Right () -> pure ()
    proceedOnlyIfPreviousShutdownSucceeded result (readStore doneStore >>= start)

  -- \| Start the server in a separate thread.
  start
    :: MVar (Either SomeException ())
    -- \^ Written to (with the shutdown outcome) when the thread is killed.
    -> IO ThreadId
  start done = do
    (port, site, app) <- getApplicationRepl
    forkFinally
      (runSettings (setPort port defaultSettings) app)
      -- 'shutdownThenDeliver' MUST finish (and deliver a result) before
      -- 'restartAppInNewThread''s 'takeMVar' can unblock, which then
      -- immediately proceeds to 'start' a brand-new foundation (and a
      -- brand-new AWS 'Env' supervisor) -- but only on 'Right': signalling
      -- unconditionally, or before shutdown actually finished, would let
      -- that new foundation's supervisor start running concurrently with
      -- the old one still being torn down, or (if shutdown threw) with
      -- the old one never actually torn down at all. This ordering closes
      -- both an overlapping-generations window and a would-be deadlock if
      -- shutdown itself fails or is cancelled.
      (\_ -> shutdownThenDeliver (shutdownApp site) done)

-- | kill the server
shutdown :: IO ()
shutdown = do
  mtidStore <- lookupStore tidStoreNum
  case mtidStore of
    -- no server running
    Nothing -> putStrLn "no Yesod app running"
    Just tidStore -> do
      withStore tidStore $ readIORef >=> killThread
      putStrLn "Yesod app is shutdown"

tidStoreNum :: Word32
tidStoreNum = 1

modifyStoredIORef :: Store (IORef a) -> (a -> IO a) -> IO ()
modifyStoredIORef store f = withStore store $ \ref -> do
  v <- readIORef ref
  f v >>= writeIORef ref
