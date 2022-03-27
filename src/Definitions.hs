module Definitions where

import Youtube
import Control.Monad.Reader (ReaderT)
import qualified Data.ByteString.Lazy (ByteString)

data Bookmark = Bookmark
  { bookmarkTitle :: String,
    bookmarkUrl :: String
  }
  deriving (Show)

data ProcessState
  = ProcessPending
  | ProcessFailed
  | ProcessFinished
  | ProcessSkipped
  deriving (Show, Read)

data Process = Process
  { processYoutubeId :: String,
    processState :: ProcessState,
    processError :: Maybe String
  }
  deriving (Show)

class Repository a where
  loadBookmarks :: a -> IO [Bookmark]

instance Eq Process where
  (==) p1 p2 = processYoutubeId p1 == processYoutubeId p2

bookmarkToProcess :: Bookmark -> Maybe Process
bookmarkToProcess bookmark = do
  youtubeId <- (getYoutubeId . bookmarkUrl) bookmark

  return (Process youtubeId ProcessPending Nothing)

