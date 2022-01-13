module Downloader (
  downloader,
  downloadSaver,
  DownloaderMsg (..)
) where

import qualified Actor as A
import Definitions
import Youtube
import qualified ProcessRepository as PR
import System.Process
import Control.Concurrent.STM ( atomically, modifyTVar, TVar )
import System.IO ( stderr, stdin, hGetContents )
import System.Exit ( ExitCode(ExitSuccess, ExitFailure) )
import qualified Logger as L
import Data.String.Interpolate
import Helpers

getDownloadParams :: String -> FilePath -> FilePath -> [String]
getDownloadParams youtubeId tmpDir targetDir = [
  "-x"
  , "-o"
  , [iii|#{tmpDir}/%(title)s.%(ext)s|]
  , "-q"
  , "--no-warnings"
  , "--exec"
  , [iii|mv {} #{targetDir}/"|]
  , "--"
  , youtubeId
  ]

data DownloadSaverMsg = SynchronizationFinished String
  | DownloadFailed String String
  deriving (Show)

-- public

newtype DownloaderMsg = DownloaderStart Process deriving (Show)

downloader :: A.ActorRef DownloadSaverMsg -> FilePath -> FilePath -> A.ActorId -> A.Behavior DownloaderMsg
downloader saverActor tmpDir targetDir currentActorId = A.Behavior $ \case
  DownloaderStart process -> do
    let youtubeId = processYoutubeId process
                    
    L.log (L.SynchronizationStarted youtubeId currentActorId)

    let shellProcess = (proc "youtube-dl" (getDownloadParams youtubeId tmpDir targetDir)) {
      std_in  = UseHandle stdin,
      std_out = CreatePipe,
      std_err = CreatePipe
    }

    processHandler@(_, _, Just hErr, p) <- createProcess_ "" shellProcess

    exitCode <- waitForProcess p
    errorMessage <- hGetContents hErr
    
    -- What is the consequence of not closing the handles?
    -- If they are closed with `withCreateProcess` or with `cleanupProcess`
    -- then I'm unable to get contents, because `hGetContents` is lazy.
    -- According to this: https://stackoverflow.com/questions/32337336/do-i-need-to-call-closehandle
    -- quitting process should automatically close handles by the system
    -- cleanupProcess processHandler
    
    case exitCode of
      ExitFailure _ -> do
        A.send saverActor (DownloadFailed youtubeId errorMessage)
        L.log (L.SynchronizationError youtubeId currentActorId)
      ExitSuccess -> do
        A.send saverActor (SynchronizationFinished youtubeId)
        L.log (L.SynchronizationFinished youtubeId currentActorId)
    
    return (downloader saverActor tmpDir targetDir currentActorId)

downloadSaver :: FilePath -> TVar Int -> A.Behavior DownloadSaverMsg
downloadSaver processPath processedCounter = A.Behavior $ \case
  SynchronizationFinished youtubeId -> do
    PR.openRepository processPath (PR.finishProcess youtubeId)
    atomically $ modifyTVar processedCounter (+ 1)
    return (downloadSaver processPath processedCounter)
  DownloadFailed youtubeId errorMessage -> do
    let pureErrorMessage = trim . replace '\n' ' ' $ errorMessage
    PR.openRepository processPath (PR.errorProcess youtubeId pureErrorMessage)
    atomically $ modifyTVar processedCounter (+ 1)
    return (downloadSaver processPath processedCounter)

