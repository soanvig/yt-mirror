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

getDownloadParams :: String -> [String]
getDownloadParams youtubeId = [
  "-x"
  , "-o"
  , "/tmp/%(title)s.%(ext)s"
  , "-q"
  , "--no-warnings"
  , "--exec"
  , "mv {} ~/music/synchronized/"
  , "--"
  , youtubeId
  ]

data DownloadSaverMsg = DownloadFinished String
  | DownloadFailed String String
  deriving (Show)

-- public

newtype DownloaderMsg = DownloaderStart Process deriving (Show)

downloader :: A.ActorRef DownloadSaverMsg -> A.ActorId -> A.Behavior DownloaderMsg
downloader saverActor currentActorId = A.Behavior $ \case
  DownloaderStart process -> do
    let youtubeId = processYoutubeId process
                    
    L.log (L.DownloadStarted youtubeId currentActorId)

    let shellProcess = (proc "youtube-dl" (getDownloadParams youtubeId)) {
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
        L.log (L.DownloadError youtubeId currentActorId)
      ExitSuccess -> do
        A.send saverActor (DownloadFinished youtubeId)
        L.log (L.DownloadFinished youtubeId currentActorId)
    
    return (downloader saverActor currentActorId)

downloadSaver :: TVar Int -> A.Behavior DownloadSaverMsg
downloadSaver processedCounter = A.Behavior $ \case
  DownloadFinished youtubeId -> do
    PR.openRepository (PR.finishProcess youtubeId)
    atomically $ modifyTVar processedCounter (+ 1)
    return (downloadSaver processedCounter)
  DownloadFailed youtubeId errorMessage -> do
    PR.openRepository (PR.errorProcess youtubeId (trim errorMessage))
    atomically $ modifyTVar processedCounter (+ 1)
    return (downloadSaver processedCounter)

