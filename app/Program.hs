module Program (
  Command (..)
  , getCommand
) where

import Options.Applicative
import Lib
import Data.Semigroup ((<>))


data Command = Prepare PrepareOptions | Synchronize SynchronizeOptions

prepareOptionsParser :: Parser PrepareOptions
prepareOptionsParser = PrepareOptions <$>
  strOption (
    long "processes"
    <> short 'p'
    <> metavar "FILE"
    <> help "Location for processes database (created automatically if doesn't exist)"
  )
  <*> strOption (
    long "bookmarks"
    <> short 'b'
    <> metavar "FILE"
    <> help "Location of browser database (Firefox: `places.sqlite`)"
  )

synchronizeOptionsParser :: Parser SynchronizeOptions
synchronizeOptionsParser = SynchronizeOptions <$>
  strOption (
    long "processes"
    <> short 'p'
    <> metavar "FILE"
    <> help "Location for processes database (created automatically if doesn't exist)"
  )
  <*> strOption (
    long "target"
    <> short 't'
    <> metavar "DIRECTORY"
    <> help "Path to a directory into which music files will be downloaded"
  )
  <*> strOption (
    long "tmp"
    <> metavar "DIRECTORY"
    <> value "/tmp"
    <> help "Path to a directory in which temporary files will be stored (default: /tmp)"
  )

prepareOptions :: Parser Command
prepareOptions = Prepare <$> prepareOptionsParser

synchronizeOptions :: Parser Command
synchronizeOptions = Synchronize <$> synchronizeOptionsParser

optionsParser :: Parser Command
optionsParser = hsubparser (
  command "prepare" (
    info
    prepareOptions
    (progDesc "Take bookmarks, and prepare them to synchronization, by saving in process database")
  )
  <> command "synchronize" (
    info
    synchronizeOptions
    (progDesc "Synchronize all pending bookmarks")
  )
  )

getCommand :: IO Command
getCommand = execParser opts
  where
    opts = info (helper <*> optionsParser)
      ( fullDesc
      <> header "yt-mirror - automated YouTube downloader (https://github.com/soanvig/yt-mirror)" )