module Main where

import Lib
import System.IO
import Program

main :: IO ()
main = do
  hSetBuffering stdout LineBuffering

  command <- getCommand 

  case command of
    Prepare prepareOptions -> prepare prepareOptions
    Synchronize synchronizeOptions -> synchronize synchronizeOptions 
