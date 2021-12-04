{-# LANGUAGE OverloadedStrings #-}

module FirefoxRepository where

import Database.SQLite.Simple 
import Definitions

loadBookmarks :: String -> IO [Bookmark]
loadBookmarks placesLocation = do
  conn <- open placesLocation

  bookmarksRows <- query_  conn "SELECT moz_bookmarks.title, moz_places.url FROM moz_bookmarks INNER JOIN moz_places ON moz_places.id = moz_bookmarks.fk" :: IO[Bookmark]
  
  close conn

  return bookmarksRows
