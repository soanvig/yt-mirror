module Lib where

import Definitions
import qualified FirefoxRepository as FR
import qualified ProcessRepository as PR
import Data.Maybe (mapMaybe)
import Data.List (nub)
import Control.Monad.Reader
import Database.SQLite.Simple (Connection)
import qualified Actor as A
import qualified Logger
import qualified Data.List as FR
import qualified Downloader as D
import Control.Concurrent.STM
import Control.Concurrent
import Helpers

saveBookmarksToProcesses :: IO ()
saveBookmarksToProcesses = do
  bookmarks <- FR.openRepository "./places.sqlite" FR.loadBookmarks
  let processes = mapMaybe newProcess bookmarks
  PR.openRepository (PR.saveProcesses processes)

test :: IO ()
test = do
  processedCounter <- newTVarIO 0
  downloadSaverActor <- A.spawn D.downloadSaver
  downloaderActors <- mapM (\_ -> A.spawn $ D.downloader processedCounter downloadSaverActor) [1..5]

  Logger.log Logger.SavingProcesses

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

  waitFor (length pendingProcesses) (==) processedCounter

  Logger.log Logger.FinishedLog

