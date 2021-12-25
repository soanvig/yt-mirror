{-# LANGUAGE OverloadedStrings #-}

module ProcessRepository (openRepository, getProcesses, saveProcesses) where

import Database.SQLite.Simple
import Database.SQLite.Simple.FromField
import Database.SQLite.Simple.ToField
import Definitions
import Database.SQLite.Simple.Ok
import Data.Text (unpack, pack)
import Control.Monad.Reader

instance FromField ProcessState where
  fromField f = (parseFieldData . fieldData) f
    where
      parseFieldData (SQLText txt) = Ok (read $ unpack txt :: ProcessState)
      parseFieldData d = returnError ConversionFailed f "expecting SQLText column type"

instance ToField ProcessState where
  toField state = SQLText . pack . show $ state

instance FromRow Process where
  fromRow = Process <$> field <*> field

instance ToRow Process where
  toRow (Process youtubeId state) = toRow (youtubeId, state)

saveSingleProcess :: Process -> ReaderT Connection IO ()
saveSingleProcess process = do
  conn <- ask
  lift $ execute conn "INSERT INTO process (youtubeId, state) VALUES (?, ?)" process

-- Public

openRepository :: ReaderT Connection IO a -> IO a
openRepository operation = do
  conn <- open "./process.sqlite"
  execute_ conn "CREATE TABLE IF NOT EXISTS process (\
    \youtubeId TEXT NOT NULL PRIMARY KEY,\
    \state TEXT\
    \)"

  result <- runReaderT operation conn

  close conn

  return result

getProcesses :: ReaderT Connection IO [Process]
getProcesses = do
  conn <- ask
  lift $ query_ conn "SELECT * FROM process"

saveProcesses :: [Process] -> ReaderT Connection IO ()
saveProcesses [] = return ()
saveProcesses (x : rest) = do
  conn <- ask
  saveSingleProcess x
  saveProcesses rest
  
