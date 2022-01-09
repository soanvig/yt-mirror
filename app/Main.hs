module Main where

import Lib
import System.IO
import Options.Applicative
import Data.Semigroup ((<>))

data ProgramOptions = ProgramOptions {
  add :: AddOptions,
  commit :: CommitOptions
} deriving (Show)

data AddOptions = AddOptions { file :: String } deriving (Show)
data CommitOptions = CommitOptions { message :: String } deriving (Show)
data Command = Add AddOptions | Commit CommitOptions

addOptions :: Parser Command
addOptions = (Add . AddOptions)  <$> strOption
          ( long "file"
         <> metavar "FILE"
         <> help "File to add" ) 

commitOptions :: Parser Command
commitOptions = (Commit . CommitOptions) <$> strOption
          ( long "message"
         <> metavar "MESSAGE"
         <> help "Message for commit" ) 


parser :: Parser Command
parser = subparser
  ( command "add" (info addOptions ( progDesc "Add a file to the repository" ))
 <> command "commit" (info commitOptions ( progDesc "Record changes to the repository" ))
  )

execute :: Command -> IO ()
execute (Add (AddOptions file)) = putStrLn $ "Adding: " ++ file
execute (Commit (CommitOptions message)) = putStrLn $ "Commiting with message: " ++ message

runArguments :: IO ()
runArguments = execute =<< execParser opts
  where
    opts = info (parser <**> helper)
      ( fullDesc
     <> progDesc "Print a greeting for TARGET"
     <> header "hello - a test for optparse-applicative" )

main :: IO ()
main = do
  hSetBuffering stdout LineBuffering
  runArguments
  run
