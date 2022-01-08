module Actor (
  ActorRef,
  ActorId (..),
  Behavior (..),
  spawn,
  spawnWithBox,
  send
) where

import Control.Concurrent ( forkIO )
import Control.Concurrent.STM

runBehavior :: Show msg => TQueue msg -> Behavior msg -> IO ()
runBehavior queue (Behavior b) = do
  msg <- atomically (readTQueue queue)
  b msg >>= runBehavior queue

-- public

data ActorId = ActorId Int String deriving (Show)
newtype Behavior msg = Behavior (msg -> IO (Behavior msg))
newtype ActorRef msg = ActorRef (TQueue msg)

spawn :: Show msg => Behavior msg -> IO (ActorRef msg)
spawn behavior = do
  queue <- newTQueueIO
  spawnWithBox queue behavior

spawnWithBox :: Show msg => TQueue msg -> Behavior msg -> IO (ActorRef msg)
spawnWithBox queue behavior = do
  forkIO (runBehavior queue behavior) 
  return (ActorRef queue)

send :: Show msg => ActorRef msg -> msg -> IO ()
send (ActorRef queue) msg = atomically (writeTQueue queue msg)
  
