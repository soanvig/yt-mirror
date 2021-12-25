module Logger where

  import Definitions

  data SaveProcessesLog = SaveProcessesLog {
    logNewProcesses :: [Process],
    logExistingProcesses :: [Process]
  } deriving (Show)

  type Log = SaveProcessesLog

  log :: Log -> IO ()
  log (SaveProcessesLog newProcesses existingProcesses)= do
    let newProcessesLength = length existingProcesses
    let existingProcessesLength = length existingProcesses
    let allProcessesLength = newProcessesLength + existingProcessesLength

    print $ "All processes: " ++ show allProcessesLength
    print $ "Existing processes: " ++ show existingProcessesLength
    print $ "New processes: " ++ show newProcessesLength