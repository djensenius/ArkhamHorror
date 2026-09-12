module Arkham.Replay.MessageTimeout
  ( replayMessageTimeoutMicros
  , runReplayMessagesWithTimeout
  , runReplayMessagesWithin
  )
where

import System.Timeout (timeout)
import Prelude

replayMessageTimeoutMicros :: Int
replayMessageTimeoutMicros = 30 * 1000000

runReplayMessagesWithTimeout :: IO a -> IO (Maybe a)
runReplayMessagesWithTimeout = runReplayMessagesWithin replayMessageTimeoutMicros

runReplayMessagesWithin :: Int -> IO a -> IO (Maybe a)
runReplayMessagesWithin timeoutMicros action
  | timeoutMicros <= 0 = pure Nothing
  | otherwise = timeout timeoutMicros action
