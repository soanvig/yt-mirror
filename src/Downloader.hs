module Downloader (downloader, downloadSaver, DownloaderMsg (..)) where

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

getDownloadCommand :: String -> String
getDownloadCommand youtubeId = [iii|
youtube-dl
-x -o "/tmp/%(title)s.%(ext)s"
-q --no-warnings
--exec "mv {} ~/music/synchronized/"
#{youtubeId}
|]

-- public

newtype DownloaderMsg = DownloaderStart Process deriving (Show)
data DownloadSaverMsg = DownloadFinished String | DownloadFailed String deriving (Show)

downloader :: TVar Int -> A.ActorRef DownloadSaverMsg -> A.Behaviour DownloaderMsg
downloader processedCounter saverActor = A.Behaviour $ \case
  DownloaderStart process -> do
    let youtubeId = processYoutubeId process

    L.log $ L.DownloadStartedLog youtubeId

    let shellProcess = (shell $ getDownloadCommand youtubeId) {
        std_in  = UseHandle stdin
      , std_out = CreatePipe
      , std_err = UseHandle stderr
      }

    exitCode <- withCreateProcess shellProcess $ \_ _ _ p -> waitForProcess p
    
    case exitCode of
        ExitFailure _ -> do
          A.send saverActor (DownloadFailed youtubeId)
          L.log $ L.DownloadErrorLog youtubeId
        ExitSuccess -> do
          A.send saverActor (DownloadFinished youtubeId)
          L.log $ L.DownloadFinishedLog youtubeId
        
    atomically $ modifyTVar processedCounter (+ 1)
    return (downloader processedCounter saverActor)

downloadSaver :: A.Behaviour DownloadSaverMsg
downloadSaver = A.Behaviour $ \case
  DownloadFinished youtubeId -> do
    PR.openRepository (PR.finishProcess youtubeId)
    return downloadSaver
  DownloadFailed youtubeId -> do
    PR.openRepository (PR.errorProcess youtubeId)
    return downloadSaver

