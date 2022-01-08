module Definitions where

import Youtube

data Bookmark = Bookmark
  { bookmarkTitle :: String,
    bookmarkUrl :: String
  }
  deriving (Show)

data ProcessState
  = ProcessPending
  | ProcessFailed
  | ProcessFinished
  deriving (Show, Read)

data Process = Process
  { processYoutubeId :: String,
    processState :: ProcessState,
    processError :: Maybe String
  }
  deriving (Show)

instance Eq Process where
  (==) p1 p2 = processYoutubeId p1 == processYoutubeId p2

bookmarkToProcess :: Bookmark -> Maybe Process
bookmarkToProcess bookmark = do
  youtubeId <- (getYoutubeId . bookmarkUrl) bookmark

  return (Process youtubeId ProcessPending Nothing)

