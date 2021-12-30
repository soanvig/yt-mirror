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

test :: IO ()
test = do
  downloaderActor <- A.spawn D.downloader
  bookmarks <- FR.openRepository "./places.sqlite" FR.loadBookmarks
  let processes = mapMaybe newProcess bookmarks
  PR.openRepository (saveNewProcesses processes)
  pendingProcesses <- PR.openRepository PR.getPendingProcesses

  mapM_ (A.send downloaderActor . D.DownloaderStart) pendingProcesses
  
