module Lib where

import Definitions
import FirefoxRepository
import qualified ProcessRepository as PR
import Data.Maybe (mapMaybe)
import Data.List (nubBy)
import Control.Monad.Reader
import Database.SQLite.Simple (Connection)

getProcesses :: IO [Process]
getProcesses = do
  bookmarks <- loadBookmarks "./places.sqlite"
  return $ mapMaybe newProcess bookmarks

isProcessExisting :: [Process] -> Process -> Bool
isProcessExisting existingProcesses process = any (isSameProcess process) existingProcesses

notF :: (a -> Bool) -> a -> Bool
notF f x = not $ f x

operation :: ReaderT Connection IO ()
operation = do
    conn <- ask
    processes <- lift getProcesses
    let allProcesses = nubBy isSameProcess processes
    existingProcesses <- PR.getProcesses
    let newProcesses = filter ((notF . isProcessExisting) existingProcesses) allProcesses
    lift $ print $ "All processes: " ++ (show . length) allProcesses
    lift $ print $ "Existing processes: " ++ (show . length) existingProcesses
    lift $ print $ "New processes: " ++ (show . length) newProcesses
    PR.saveProcesses newProcesses

test = PR.openRepository operation
 

