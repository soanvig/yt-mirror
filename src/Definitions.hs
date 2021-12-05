module Definitions where

  import Youtube

  data Bookmark = Bookmark {
    bookmarkTitle :: String,
    bookmarkUrl :: String
  } deriving (Show)

  data ProcessState = Pending
    | InProgress
    | Processed
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
      processState = Pending
    }
  