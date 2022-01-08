module ProcessRepository (
  openRepository,
  finishProcess,
  errorProcess,
  getProcesses,
  getPendingProcesses,
  saveProcesses
) where

import Database.SQLite.Simple
import Database.SQLite.Simple.FromField
import Database.SQLite.Simple.ToField
import Definitions
import Database.SQLite.Simple.Ok
import Data.Text (unpack, pack)
import Control.Monad.Reader
import Data.String.Interpolate

instance FromField ProcessState where
  fromField f = (parseFieldData . fieldData) f
    where
      parseFieldData (SQLText txt) = Ok (read $ unpack txt :: ProcessState)
      parseFieldData d = returnError ConversionFailed f "expecting SQLText column type"

instance ToField ProcessState where
  toField state = SQLText . pack . show $ state

instance FromRow Process where
  fromRow = Process <$> field <*> field <*> field

instance ToRow Process where
  toRow (Process youtubeId state errorMessage) = toRow (youtubeId, state, errorMessage)

saveSingleProcess :: Process -> ReaderT Connection IO ()
saveSingleProcess process = do
  conn <- ask
  lift $ execute conn "INSERT OR IGNORE INTO process (youtubeId, state, errorMessage) VALUES (?, ?, ?)" process

-- Public

openRepository :: ReaderT Connection IO a -> IO a
openRepository operation = do
  conn <- open "./process.sqlite"
  execute_ conn [iii|
CREATE TABLE IF NOT EXISTS process (
  youtubeId TEXT NOT NULL PRIMARY KEY,
  state TEXT NOT NULL,
  errorMessage TEXT
)
|]

  result <- runReaderT operation conn

  close conn

  return result

getProcesses :: ReaderT Connection IO [Process]
getProcesses = do
  conn <- ask
  lift $ query_ conn "SELECT youtubeId, state FROM process"

getPendingProcesses :: ReaderT Connection IO [Process]
getPendingProcesses = do
  conn <- ask
  lift $ query conn "SELECT youtubeId, state FROM process WHERE state = (?)" [ProcessPending]

finishProcess :: String -> ReaderT Connection IO ()
finishProcess youtubeId = do
  conn <- ask
  lift $ execute conn "UPDATE process SET state = (?) WHERE youtubeId = (?)" (ProcessFinished, youtubeId)

errorProcess :: String -> String -> ReaderT Connection IO ()
errorProcess youtubeId errorMessage = do
  conn <- ask
  lift $ execute conn "UPDATE process SET state = (?), errorMessage = (?) WHERE youtubeId = (?)" (ProcessFailed, errorMessage, youtubeId)

saveProcesses :: [Process] -> ReaderT Connection IO ()
saveProcesses [] = return ()
saveProcesses (x : xs) = do
  saveSingleProcess x
  saveProcesses xs
  
