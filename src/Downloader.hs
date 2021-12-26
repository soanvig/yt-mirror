module Downloader (download) where

import Definitions
import System.Process (callCommand)

-- public

download :: String -> IO ()
download id = do
  callCommand $ "youtube-dl -x -o \"/tmp/%(title)s.%(ext)s\" --exec \"mv {} ~/music/synchronized/\" " ++ id

