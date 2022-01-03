module Actor  (ActorRef (), Behaviour (..), spawn, send) where

import Control.Monad
import Control.Concurrent
import Control.Concurrent.STM
import Data.IORef
import System.IO

data ActorRef msg = ActorRef {
  refMbox :: TQueue msg
}

newtype Behaviour msg = Behaviour (msg -> IO (Behaviour msg))

go :: Show msg => TQueue msg -> Behaviour msg -> IO ()
go mbox (Behaviour b) = do
  msg <- atomically (readTQueue mbox)
  b msg >>= go mbox

spawn :: Show msg => Behaviour msg -> IO (ActorRef msg)
spawn behaviour = do
  mvar <- newEmptyMVar 
  mbox <- newTQueueIO
  forkIO (go mbox behaviour) 
  return (ActorRef mbox)

send :: Show msg => ActorRef msg -> msg -> IO ()
send recipient msg = atomically (writeTQueue (refMbox recipient) msg)
  
