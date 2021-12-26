{-# LANGUAGE LambdaCase #-}

module Actor  ( ActorRef (refId)
  , Behaviour (..)
  , spawn
  , send
  ) where


import Control.Monad
import Control.Concurrent
import Control.Concurrent.STM
import Data.IORef
import System.IO

data ActorRef msg
  = ActorRef
  { refId   :: ThreadId
  , refMbox :: TQueue msg
  }

newtype Behaviour msg = Behaviour (msg -> IO (Behaviour msg))

go :: TQueue msg -> Behaviour msg -> IO ()
go mbox (Behaviour b) = do
  msg <- atomically (readTQueue mbox)
  b msg >>= go mbox

spawn :: Behaviour msg -> IO (ActorRef msg)
spawn behaviour = do
  mbox <- newTQueueIO
  pid  <- forkIO (go mbox behaviour)
  return (ActorRef pid mbox)

send :: ActorRef msg -> msg -> IO ()
send recipient msg = atomically (writeTQueue (refMbox recipient) msg)

data FileReaderMsg a
  = OpenFile FilePath
  | SendLine (ActorRef a) (String -> a)
  | GotLine String
  | CloseFile

fileReader :: Behaviour (FileReaderMsg t)
fileReader = whenClosed
  where
    whenClosed = Behaviour $ \case
      OpenFile fp -> do
        h <- openFile fp ReadMode
        pure (whenOpened h)
      _ -> error "inappropriate state"
    whenOpened h = Behaviour $ \case
      SendLine replyTo mapping -> do
        line <- hGetLine h
        replyTo `send` mapping line
        pure (whenOpened h)
      CloseFile -> do
        hClose h
        pure whenClosed
      _ -> error "inappropriate state"

newtype PrinterMsg = PrinterLine String

printer :: Behaviour PrinterMsg
printer = Behaviour $ \case
  PrinterLine line -> do
    print line
    pure printer

test :: IO ()
test = do
  fileActor <- spawn fileReader
  printerActor <- spawn printer
  send fileActor (OpenFile "./docs.md")
  send fileActor (SendLine printerActor PrinterLine)
  send fileActor (SendLine printerActor PrinterLine)
  send fileActor (SendLine printerActor PrinterLine)
  send fileActor CloseFile

  return ()