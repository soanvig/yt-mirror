module Logger where

import Definitions

data Log
  = Finished
  | ProcessingStarted [Process]
  | DownloadStarted String
  | DownloadFinished String
  | DownloadError String
  | ProcessSavingStarted
  deriving (Show)

log :: Log -> IO ()
log (ProcessingStarted processes) = do
  print $ "Starting processing of pending processes: " ++ (show . length) processes
log Finished = print "Finished processing all processes"
log (DownloadStarted youtubeId) = print $ "Downloading: " ++ youtubeId
log (DownloadFinished youtubeId) = print $ "Downloading finished: " ++ youtubeId
log (DownloadError youtubeId) = print $ "Downloading error: " ++ youtubeId
log ProcessSavingStarted = print "Preparing bookmarks for processing. This may take a while..."
