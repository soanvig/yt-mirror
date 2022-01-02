{-# LANGUAGE LambdaCase #-}

module Downloader (downloader, DownloaderMsg (..)) where

import Actor
import Definitions
import Youtube
import qualified ProcessRepository as PR
import System.Process
import Control.Concurrent.STM ( atomically, modifyTVar, TVar )
import System.IO ( stderr, stdin )
import System.Exit ( ExitCode(ExitSuccess, ExitFailure) )
import qualified Logger as L

-- public

newtype DownloaderMsg = DownloaderStart Process deriving (Show)

downloader :: TVar Int -> Behaviour DownloaderMsg
downloader processedCounter = Behaviour $ \case
  DownloaderStart process -> do
    let youtubeId = processYoutubeId process

    L.log $ L.DownloadStartedLog youtubeId

    let command = "youtube-dl -x -o \"/tmp/%(title)s.%(ext)s\" -q --no-warnings --exec \"mv {} ~/music/synchronized/\" " ++ youtubeId
    let shellProcess = (shell command) {
        std_in  = UseHandle stdin
      , std_out = CreatePipe
      , std_err = UseHandle stderr
      }

    exitCode <- withCreateProcess shellProcess $ \_ _ _ p -> waitForProcess p
    
    case exitCode of
        ExitFailure _ -> do
          PR.openRepository (PR.errorProcess youtubeId)
          L.log $ L.DownloadErrorLog youtubeId
        ExitSuccess -> do
          PR.openRepository (PR.finishProcess youtubeId)
          L.log $ L.DownloadFinishedLog youtubeId
        
    atomically $ modifyTVar processedCounter (+ 1)
    return (downloader processedCounter)

