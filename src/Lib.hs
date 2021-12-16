module Lib where

import Definitions
import FirefoxRepository
import qualified ProcessRepository as PR
import Data.Maybe (mapMaybe)
import Data.List (nubBy)

getProcesses :: IO [Process]
getProcesses = do
  bookmarks <- loadBookmarks "./places.sqlite"
  return $ mapMaybe newProcess bookmarks

isProcessExisting :: [Process] -> Process -> Bool
isProcessExisting existingProcesses process = any (isSameProcess process) existingProcesses

notF :: (a -> Bool) -> a -> Bool
notF f x = not $ f x

test :: IO ()
test = PR.openRepository (
  \conn -> do
    processes <- getProcesses
    let allProcesses = nubBy isSameProcess processes
    existingProcesses <- PR.getProcesses conn
    let newProcesses = filter ((notF . isProcessExisting) existingProcesses) allProcesses
    print $ "All processes: " ++ (show . length) allProcesses
    print $ "Existing processes: " ++ (show . length) existingProcesses
    print $ "New processes: " ++ (show . length) newProcesses
    PR.saveProcesses conn newProcesses
  )

