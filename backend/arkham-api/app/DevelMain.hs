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

import Api.Arkham.Lifecycle (
  acquireThenForkTransferringOwnership,
  proceedOnlyIfPreviousShutdownSucceededReplayable,
  shutdownThenDeliver,
 )
import Application (getApplicationRepl, shutdownApp)
import Prelude

import Control.Concurrent
import Control.Exception (SomeException)
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
      tidRef <- storeAction (Store tidStoreNum) (newIORef Nothing)
      start tidRef done
    -- server is already running
    Just tidStore -> withStore tidStore (`restartAppInNewThread` doneStore)
 where
  doneStore :: Store (MVar (Either SomeException ()))
  doneStore = Store 0

  -- Kill the previous generation's Warp thread, wait for its shutdown
  -- result, and only start a replacement if that shutdown actually
  -- succeeded ('Right'). A failed/interrupted shutdown ('Left') is
  -- reported and re-thrown rather than silently starting a new
  -- generation on top of a supervisor that may not have actually
  -- stopped; unlike an earlier version of this protocol, that 'Left' is
  -- never consumed here, so every subsequent restart attempt observes
  -- the exact same failure immediately rather than blocking forever on
  -- an already-emptied one-shot 'MVar' -- see
  -- 'proceedOnlyIfPreviousShutdownSucceededReplayable'. @tidRef@ itself
  -- is never cleared to 'Nothing' here: 'start' (via
  -- 'acquireThenForkTransferringOwnership''s @publish@ callback) always
  -- overwrites it with the new generation's 'ThreadId' before this
  -- function's own caller (GHCi) could ever observe a stale one, and
  -- 'proceedOnlyIfPreviousShutdownSucceededReplayable' guarantees that
  -- overwrite is only even attempted once the previous shutdown
  -- genuinely succeeded.
  restartAppInNewThread :: IORef (Maybe ThreadId) -> Store (MVar (Either SomeException ())) -> IO ()
  restartAppInNewThread tidRef doneStoreRef = do
    done <- readStore doneStoreRef
    mtid <- readIORef tidRef
    maybe (pure ()) killThread mtid
    proceedOnlyIfPreviousShutdownSucceededReplayable done (start tidRef done)

  {- | Start the server in a separate thread, and durably publish its
  'ThreadId' into @tidRef@ before returning.

  Uses 'acquireThenForkTransferringOwnership' rather than a plain
  @(port, site, app) <- getApplicationRepl@ bind followed by a
  /separate/ ownership-transfer call: that would leave a genuine gap
  between 'Application.getApplicationRepl' returning an already-owned
  @App@ and this thread's own protection actually beginning -- an
  asynchronous exception landing in exactly that window (or
  'Control.Concurrent.forkIOWithUnmask' itself throwing, however rare)
  would leak the returned @App@ (and its AWS Env supervisor), since
  nothing would yet own its eventual 'Application.shutdownApp'.
  'acquireThenForkTransferringOwnership' instead masks from *before*
  'Application.getApplicationRepl' is even called through the child
  being definitely spawned, so there is no such gap: this thread is
  already back in a masked state the very instant acquisition returns.

  The @publish@ callback (@writeIORef tidRef . Just@) runs -- still
  masked -- immediately after the child is spawned and strictly before
  this function returns to 'update'\/'restartAppInNewThread': without
  this, an asynchronous exception landing between 'start' returning and
  a *separate*, subsequent @writeIORef@ elsewhere could leave the freshly
  spawned child (already running Warp) permanently untracked -- no
  future 'update'\/'shutdown' call could ever find its 'ThreadId' to kill
  it. If @publish@ itself somehow throws (only possible via a genuine
  asynchronous exception landing in that exact masked window), @cancel@
  kills the just-spawned child and waits for its finalizer
  ('shutdownThenDeliver') to have genuinely run and recorded a result in
  @done@, so a failed publish can never leave an untracked, still-running
  child behind either.
  -}
  start
    :: IORef (Maybe ThreadId)
    -> MVar (Either SomeException ())
    -- \^ Written to (with the shutdown outcome) when the thread is killed.
    -> IO ()
  start tidRef done =
    () <$ acquireThenForkTransferringOwnership
      getApplicationRepl
      (\(_, site, _) -> shutdownApp site)
      (\(port, _site, app) -> runSettings (setPort port defaultSettings) app)
      -- 'shutdownThenDeliver' MUST finish (and deliver a result) before
      -- 'restartAppInNewThread''s wait can unblock, which then
      -- immediately proceeds to 'start' a brand-new foundation (and a
      -- brand-new AWS 'Env' supervisor) -- but only on 'Right': signalling
      -- unconditionally, or before shutdown actually finished, would let
      -- that new foundation's supervisor start running concurrently with
      -- the old one still being torn down, or (if shutdown threw) with
      -- the old one never actually torn down at all. This ordering closes
      -- both an overlapping-generations window and a would-be deadlock if
      -- shutdown itself fails or is cancelled.
      (\(_, site, _) _result -> shutdownThenDeliver (shutdownApp site) done)
      (\tid -> killThread tid >> (() <$ readMVar done))
      (writeIORef tidRef . Just)

-- | kill the server
shutdown :: IO ()
shutdown = do
  mtidStore <- lookupStore tidStoreNum
  case mtidStore of
    -- no server running
    Nothing -> putStrLn "no Yesod app running"
    Just tidStore -> do
      mtid <- withStore tidStore readIORef
      case mtid of
        Nothing -> putStrLn "no Yesod app running"
        Just tid -> do
          killThread tid
          putStrLn "Yesod app is shutdown"

tidStoreNum :: Word32
tidStoreNum = 1
