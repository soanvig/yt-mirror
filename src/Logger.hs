module Logger where

  import Definitions

  data Log = SaveProcessesLog [Process] [Process]
            | FinishedLog {}
            | StartingProcessingLog [Process]
            | DownloadStartedLog String
            | DownloadFinishedLog String
            | DownloadErrorLog String
            deriving (Show)

  log :: Log -> IO ()
  log (SaveProcessesLog newProcesses existingProcesses) = do
    let newProcessesLength = length newProcesses
    let existingProcessesLength = length existingProcesses
    let allProcessesLength = newProcessesLength + existingProcessesLength

    print $ "All processes: " ++ show allProcessesLength
    print $ "Existing processes: " ++ show existingProcessesLength
    print $ "New processes: " ++ show newProcessesLength
  log (StartingProcessingLog processes) = do
    print $ "Starting processing of pending processes: " ++ (show . length) processes
  log FinishedLog = print "Finished processing all processes"
  log (DownloadStartedLog youtubeId) = print $ "Downloading: " ++ youtubeId
  log (DownloadFinishedLog youtubeId) = print $ "Downloading finished: " ++ youtubeId
  log (DownloadErrorLog youtubeId) = print $ "Downloading error: " ++ youtubeId
