module Main where

import Lib
import System.IO
import Program

main :: IO ()
main = do
  hSetBuffering stdout LineBuffering

  command <- getCommand 

  case command of
    Prepare (PrepareOptions processPath bookmarksPath) -> prepare bookmarksPath processPath 
    Synchronize (SynchronizeOptions processPath) -> synchronize processPath 
