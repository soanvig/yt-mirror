module Lib where

import Definitions
import qualified FirefoxRepository as FR
import qualified ProcessRepository as PR
import Data.Maybe (mapMaybe)
import Data.List (nubBy)
import Control.Monad.Reader
import Database.SQLite.Simple (Connection)
import qualified Actor as A
import qualified Logger
import qualified Data.List as FR
import qualified Downloader as D
import Control.Concurrent.STM
import Control.Concurrent


isProcessExisting :: [Process] -> Process -> Bool
isProcessExisting existingProcesses process = any (isSameProcess process) existingProcesses

notF :: (a -> Bool) -> a -> Bool
notF f x = not $ f x

saveNewProcesses :: [Process] -> ReaderT Connection IO ()
saveNewProcesses processes = do
    let allProcesses = nubBy isSameProcess processes
    existingProcesses <- PR.getProcesses
    let newProcesses = filter ((notF . isProcessExisting) existingProcesses) allProcesses

    lift $ Logger.log (Logger.SaveProcessesLog newProcesses existingProcesses)

    PR.saveProcesses newProcesses

saveBookmarksToProcesses :: IO ()
saveBookmarksToProcesses = do
  bookmarks <- FR.openRepository "./places.sqlite" FR.loadBookmarks
  let processes = mapMaybe newProcess bookmarks
  PR.openRepository (saveNewProcesses processes)

waitForProcessedCounter :: Int -> TVar Int -> IO ()
waitForProcessedCounter toProcess processedCounter = do
  threadDelay $ 5 * 1000000 -- 5 seconds
  processedCount <- readTVarIO processedCounter

  if processedCount == toProcess then
     return ()
  else
     waitForProcessedCounter toProcess processedCounter

-- Round robins b over a
roundRobin :: [a] -> [b] -> [(a, b)]
roundRobin [] [] = []
roundRobin [] _ = []
roundRobin _ [] = []
roundRobin a b = zip a b ++ roundRobin a (drop (length a) b)

test :: IO ()
test = do
  processedCounter <- newTVarIO 0
  downloadSaverActor <- A.spawn D.downloadSaver
  downloaderActors <- mapM (\_ -> A.spawn $ D.downloader processedCounter downloadSaverActor) [1..5]

  saveBookmarksToProcesses

  pendingProcesses <- PR.openRepository PR.getPendingProcesses

  -- let pendingProcesses = [
        -- Process "5FjWe31S_0g" ProcessPending
        -- , Process "1Y1rWCbj7gE" ProcessPending 
        -- , Process "if7jHQk0YKc" ProcessPending 
        -- , Process "MjTSw5htw4s" ProcessPending
        -- ]

  Logger.log $ Logger.StartingProcessingLog pendingProcesses

  let actorsWithProcesses = roundRobin downloaderActors pendingProcesses

  mapM_ (\(actor, process) -> A.send actor $ D.DownloaderStart process) actorsWithProcesses

  waitForProcessedCounter (length pendingProcesses) processedCounter

  Logger.log Logger.FinishedLog

