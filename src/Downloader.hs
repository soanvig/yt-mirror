{-# LANGUAGE LambdaCase #-}

module Downloader (downloader, DownloaderMsg (..)) where

import Actor
import Definitions
import Youtube
import qualified ProcessRepository as PR
import System.Process (callCommand)
import Control.Concurrent.STM

-- public

newtype DownloaderMsg = DownloaderStart Process deriving (Show)

downloader :: TVar Int -> Behaviour DownloaderMsg
downloader processedCounter = Behaviour $ \case
  DownloaderStart process -> do
    let youtubeId = processYoutubeId process
    callCommand $ "youtube-dl -x -o \"/tmp/%(title)s.%(ext)s\" --exec \"mv {} ~/music/synchronized/\" " ++ youtubeId
    PR.openRepository (PR.updateProcessWithState youtubeId Processed)
    atomically $ modifyTVar processedCounter (+ 1)
    return (downloader processedCounter)

