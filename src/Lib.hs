module Lib (
  prepare
  , synchronize
  , SynchronizeOptions(..)
  , PrepareOptions(..)
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

testProcesses :: [Process]
testProcesses = [
  Process "-gtZQ2xgcBE" ProcessPending Nothing -- works
  , Process "vtC8sSWuPY0" ProcessPending Nothing -- error
  , Process "MBW3Jo9yoxo" ProcessPending Nothing -- works
  , Process "hiPXP7nIVtI" ProcessPending Nothing -- error
  ]

getPendingProcesses :: FilePath -> IO [Process]
getPendingProcesses processPath = do
  PR.openRepository processPath PR.getPendingProcesses
  -- return testProcesses
  
-- public

data SynchronizeOptions = SynchronizeOptions {
  synchronizeProcesses :: String
  , synchronizeTmpDir :: String
  , synchronizeTargetDir :: String
}
data PrepareOptions = PrepareOptions { prepareProcesses :: String, prepareBookmarks :: String }

prepare :: PrepareOptions -> IO ()
prepare (PrepareOptions bookmarksPath processPath) = do
  bookmarks <- FR.openRepository bookmarksPath FR.loadBookmarks

  L.log $ L.PreparingStarted (length bookmarks)

  let processes = mapMaybe bookmarkToProcess bookmarks
  PR.openRepository processPath (PR.saveProcesses processes)

  L.log L.PreparingFinished

synchronize :: SynchronizeOptions -> IO ()
synchronize (SynchronizeOptions processPath tmpDir targetDir) = do
  let actorCount = 2
  let actorIds = fmap (\x -> A.ActorId x (getRandomString 5 x)) [1..actorCount]

  processedCounter <- newTVarIO 0
  downloaderBox <- newTQueueIO
  downloadSaverActor <- A.spawn (D.downloadSaver processPath processedCounter)
  downloaderActors <- mapM
    (A.spawnWithBox downloaderBox  . D.downloader downloadSaverActor)
    actorIds

  pendingProcesses <- getPendingProcesses processPath
  
  L.log (L.AllSynchronizationStarted actorCount pendingProcesses)

  let actorsWithProcesses = roundRobin downloaderActors pendingProcesses

  mapM_ (\(actor, process) -> A.send actor $ D.DownloaderStart process) actorsWithProcesses

  waitFor (length pendingProcesses) (==) processedCounter

  L.log L.Finished

