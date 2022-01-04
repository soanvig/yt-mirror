module Actor (
  ActorRef,
  Behavior (..),
  spawn,
  send
) where

import Control.Concurrent ( forkIO )
import Control.Concurrent.STM

runBehavior :: Show msg => TQueue msg -> Behavior msg -> IO ()
runBehavior queue (Behavior b) = do
  msg <- atomically (readTQueue queue)
  b msg >>= runBehavior queue

-- public

newtype ActorRef msg = ActorRef (TQueue msg)

newtype Behavior msg = Behavior (msg -> IO (Behavior msg))

spawn :: Show msg => Behavior msg -> IO (ActorRef msg)
spawn behavior = do
  queue <- newTQueueIO
  forkIO (runBehavior queue behavior) 
  return (ActorRef queue)

send :: Show msg => ActorRef msg -> msg -> IO ()
send (ActorRef queue) msg = atomically (writeTQueue queue msg)
  
