module Definitions where

  import Youtube

  data Bookmark = Bookmark {
    bookmarkTitle :: String,
    bookmarkUrl :: String
  } deriving (Show)

  data ProcessState = ProcessPending
    | ProcessHasError
    | ProcessFinished
    deriving (Show, Read)

  data Process = Process {
    processYoutubeId :: String,
    processState :: ProcessState 
  } deriving (Show)

  newProcess :: Bookmark -> Maybe Process
  newProcess bookmark = do
    youtubeUrl <- (toYoutubeUrl . bookmarkUrl) bookmark
    youtubeId <- getYoutubeId youtubeUrl

    return Process {
      processYoutubeId = youtubeId,
      processState = ProcessPending
    }

  isSameProcess :: Process -> Process -> Bool
  isSameProcess p1 p2 = processYoutubeId p1 == processYoutubeId p2
  
