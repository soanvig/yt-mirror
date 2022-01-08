module Lib (
  run
) where

import Definitions
import qualified FirefoxRepository as FR
import qualified ProcessRepository as PR
import Data.Maybe (mapMaybe)
import Data.List (nub)
import Database.SQLite.Simple (Connection)
import qualified Actor as A
import qualified Logger as L
import qualified Data.List as FR
import qualified Downloader as D
import Control.Concurrent.STM
import Helpers

saveBookmarksToProcesses :: String -> IO ()
saveBookmarksToProcesses placesLocation = do
  bookmarks <- FR.openRepository placesLocation FR.loadBookmarks
  let processes = mapMaybe bookmarkToProcess bookmarks
  PR.openRepository (PR.saveProcesses processes)

-- public

run :: IO ()
run = do
  processedCounter <- newTVarIO 0
  downloadSaverActor <- A.spawn $ D.downloadSaver processedCounter
  downloaderActors <- mapM (\_ -> A.spawn $ D.downloader downloadSaverActor) [1..10]

  L.log L.ProcessSavingStarted

  saveBookmarksToProcesses "./places.sqlite"

  pendingProcesses <- PR.openRepository PR.getPendingProcesses

  -- let pendingProcesses = [
        -- Process "-gtZQ2xgcBE" ProcessPending
        -- , Process "1Y1rWCbj7gE" ProcessPending 
        -- , Process "if7jHQk0YKc" ProcessPending 
        -- , Process "MjTSw5htw4s" ProcessPending
        -- ]

  L.log (L.ProcessingStarted pendingProcesses)

  let actorsWithProcesses = roundRobin downloaderActors pendingProcesses

  mapM_ (\(actor, process) -> A.send actor $ D.DownloaderStart process) actorsWithProcesses

  waitFor (length pendingProcesses) (==) processedCounter

  L.log L.Finished

