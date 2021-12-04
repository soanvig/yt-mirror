module Lib where

import Definitions
import FirefoxRepository
import Data.Maybe (fromJust)
import Network.URL (host, importURL, exportHost, URLType(..), host, url_type)

isYoutubeHost :: URLType -> Bool
isYoutubeHost (Absolute h) = host h == "youtube.com" || host h == "www.youtube.com"
isYoutubeHost _ = False

isYoutube :: Bookmark -> Bool
isYoutube (Bookmark _ url) = (isYoutubeHost . url_type) (fromJust (importURL url))

getYoutubeBookmarks :: IO [Bookmark]
getYoutubeBookmarks = do
  bookmarks <- loadBookmarks "./places.sqlite"
  return $ filter isYoutube bookmarks

