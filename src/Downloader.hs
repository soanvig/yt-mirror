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
import Debug.Trace (trace, traceId, traceIO)
import Data.List (isInfixOf)

getDownloadParams :: String -> FilePath -> FilePath -> String -> [String]
getDownloadParams youtubeId tmpDir targetDir matchFilter = [
  "-x"
  , "-o"
  , [iii|#{tmpDir}/%(title)s.%(ext)s|]
  , "--no-warnings"
  , "--exec"
  , [iii|mv {} #{targetDir}/|]
  , "--match-filter"
  , if matchFilter == "" then "comment_count >? 0" else matchFilter
  , "--"
  , youtubeId
  ]

data DownloadSaverMsg = SynchronizationFinished String
  | DownloadFailed String String
  | DownloadSkipped String
  deriving (Show)

-- public

newtype DownloaderMsg = DownloaderStart Process deriving (Show)

downloader :: A.ActorRef DownloadSaverMsg -> FilePath -> FilePath -> String -> A.ActorId -> A.Behavior DownloaderMsg
downloader saverActor tmpDir targetDir matchFilter currentActorId = A.Behavior $ \case
  DownloaderStart process -> do
    let youtubeId = processYoutubeId process

    L.log (L.SynchronizationStarted youtubeId currentActorId)

    let shellProcess = (proc "yt-dlp" (getDownloadParams youtubeId tmpDir targetDir matchFilter)) {
      std_in  = UseHandle stdin,
      std_out = CreatePipe,
      std_err = CreatePipe
    }

    processHandler@(_, Just hOut, Just hErr, p) <- createProcess_ "" shellProcess

    exitCode <- waitForProcess p
    errorMessage <- hGetContents hErr
    outputMessage <- hGetContents hOut

    -- What is the consequence of not closing the handles?
    -- If they are closed with `withCreateProcess` or with `cleanupProcess`
    -- then I'm unable to get contents, because `hGetContents` is lazy.
    -- According to this: https://stackoverflow.com/questions/32337336/do-i-need-to-call-closehandle
    -- quitting process should automatically close handles by the system
    -- cleanupProcess processHandler

    let isSkipped = "skipping .." `isInfixOf` outputMessage

    let (downloaderMsg, logMsg) = if isSkipped
        then (
          DownloadSkipped youtubeId
          , L.SynchronizationSkipped youtubeId currentActorId
          )
        else
          case exitCode of
            ExitFailure _ -> (
              DownloadFailed youtubeId errorMessage
              , L.SynchronizationError youtubeId currentActorId
              )
            ExitSuccess -> (
              SynchronizationFinished youtubeId
              , L.SynchronizationFinished youtubeId currentActorId
              )

    A.send saverActor downloaderMsg
    L.log logMsg

    return (downloader saverActor tmpDir targetDir matchFilter currentActorId)

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
  DownloadSkipped youtubeId -> do
    PR.openRepository processPath (PR.skipProcess youtubeId)
    atomically $ modifyTVar processedCounter (+ 1)
    return (downloadSaver processPath processedCounter)

