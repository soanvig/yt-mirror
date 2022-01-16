module Main where

import Lib
import System.IO
import Program
import Dependencies
import System.Exit (die)
import Control.Monad (void)
import GHC.OldList (intercalate)

main :: IO ()
main = do
  hSetBuffering stdout LineBuffering

  dependencyResult <- verifyDependencies

  command <- getCommand

  void $ case dependencyResult of
    Right _ -> return ()
    Left executables -> die $ "Missing dependencies in PATH: " ++ intercalate "," executables

  case command of
    Prepare prepareOptions -> prepare prepareOptions
    Synchronize synchronizeOptions -> synchronize synchronizeOptions 
    Failed failedOptions -> failed failedOptions 
