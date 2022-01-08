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

testProcesses :: [Process]
testProcesses = [
        Process "-gtZQ2xgcBE" ProcessPending Nothing -- works
        , Process "vtC8sSWuPY0" ProcessPending Nothing -- error
        , Process "MBW3Jo9yoxo" ProcessPending Nothing -- works
        , Process "hiPXP7nIVtI" ProcessPending Nothing -- error
        ]

getPendingProcesses :: IO [Process]
getPendingProcesses = do
  -- PR.openRepository PR.getPendingProcesses
  return testProcesses
  
-- public

run :: IO ()
run = do
  let actorCount = 2
  let actorIds = fmap (\x -> A.ActorId x (getRandomString 5 x)) [1..actorCount]
  processedCounter <- newTVarIO 0
  downloaderBox <- newTQueueIO
  downloadSaverActor <- A.spawn (D.downloadSaver processedCounter)
  downloaderActors <- mapM
    (A.spawnWithBox downloaderBox  . D.downloader downloadSaverActor)
    actorIds

  L.log L.ProcessSavingStarted

  saveBookmarksToProcesses "./places.sqlite"

  pendingProcesses <- getPendingProcesses
  
  L.log (L.ProcessingStarted actorCount pendingProcesses)

  let actorsWithProcesses = roundRobin downloaderActors pendingProcesses

  mapM_ (\(actor, process) -> A.send actor $ D.DownloaderStart process) actorsWithProcesses

  waitFor (length pendingProcesses) (==) processedCounter

  L.log L.Finished

