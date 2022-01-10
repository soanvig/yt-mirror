module Program (
  PrepareOptions(..)
  , SynchronizeOptions(..)
  , Command (..)
  , getCommand
) where

import Options.Applicative
import Data.Semigroup ((<>))

newtype SynchronizeOptions = SynchronizeOptions { synchronizeProcesses :: String }
data PrepareOptions = PrepareOptions { prepareProcesses :: String, prepareBookmarks :: String }
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