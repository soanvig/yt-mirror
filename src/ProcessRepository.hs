{-# LANGUAGE OverloadedStrings #-}

module ProcessRepository where

import Database.SQLite.Simple
import Database.SQLite.Simple.FromField
import Database.SQLite.Simple.ToField
import Definitions
import Database.SQLite.Simple.Ok
import Data.Text (unpack, pack)

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

openRepository :: IO Connection
openRepository = do
  conn <- open "./process.sqlite"
  execute_ conn "CREATE TABLE IF NOT EXISTS process (\
    \youtubeId TEXT NOT NULL PRIMARY KEY,\
    \state TEXT\
    \)"
  return conn

closeRepository :: Connection -> IO ()
closeRepository = close

saveSingleProcess :: Connection -> Process -> IO ()
saveSingleProcess conn = execute conn "INSERT INTO process (youtubeId, state) VALUES (?, ?)"

getProcesses :: Connection -> IO [Process]
getProcesses conn = query_ conn "SELECT * FROM process"

saveProcesses :: Connection -> [Process] -> IO ()
saveProcesses conn processes = do
  mapM_ (saveSingleProcess conn) processes
  
  
