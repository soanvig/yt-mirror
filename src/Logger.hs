module Logger where

  import Definitions

  data Log = SaveProcessesLog { logNewProcesses :: [Process], logExistingProcesses :: [Process] }
            | FinishedLog {}
            | StartingProcessingLog { logProcesses :: [Process] }
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
