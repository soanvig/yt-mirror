module Lib where

import Definitions
import FirefoxRepository
import Data.Maybe (mapMaybe)

getProcesses :: IO [Process]
getProcesses = do
  bookmarks <- loadBookmarks "./places.sqlite"
  return $ mapMaybe newProcess bookmarks

