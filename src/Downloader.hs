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
import System.IO ( stderr, stdin )
import System.Exit ( ExitCode(ExitSuccess, ExitFailure) )
import qualified Logger as L
import Data.String.Interpolate

getDownloadParams :: String -> [String]
getDownloadParams youtubeId = [
  "-x",
  "-o",
  "/tmp/%(title)s.%(ext)s",
  "-q",
  "--no-warnings",
  "--exec",
  "mv {} ~/music/synchronized/",
  "--",
  youtubeId
  ]

data DownloadSaverMsg = DownloadFinished String | DownloadFailed String deriving (Show)

-- public

newtype DownloaderMsg = DownloaderStart Process deriving (Show)

downloader :: A.ActorRef DownloadSaverMsg -> A.Behavior DownloaderMsg
downloader saverActor = A.Behavior $ \case
  DownloaderStart process -> do
    let youtubeId = processYoutubeId process

    L.log (L.DownloadStarted youtubeId)

    let shellProcess = (proc "youtube-dl" (getDownloadParams youtubeId)) {
        std_in  = UseHandle stdin,
        std_out = CreatePipe,
        std_err = UseHandle stderr
      }

    exitCode <- withCreateProcess shellProcess $ \_ _ _ p -> waitForProcess p
    
    case exitCode of
      ExitFailure _ -> A.send saverActor (DownloadFailed youtubeId)
      ExitSuccess -> A.send saverActor (DownloadFinished youtubeId)
    
    return (downloader saverActor)

downloadSaver :: TVar Int -> A.Behavior DownloadSaverMsg
downloadSaver processedCounter = A.Behavior $ \case
  DownloadFinished youtubeId -> do
    PR.openRepository (PR.finishProcess youtubeId)
    atomically $ modifyTVar processedCounter (+ 1)
    L.log (L.DownloadFinished youtubeId)
    return (downloadSaver processedCounter)
  DownloadFailed youtubeId -> do
    PR.openRepository (PR.errorProcess youtubeId)
    atomically $ modifyTVar processedCounter (+ 1)
    L.log (L.DownloadError youtubeId)
    return (downloadSaver processedCounter)

