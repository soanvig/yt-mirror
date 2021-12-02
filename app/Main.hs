{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Applicative
import Database.SQLite.Simple 
import Definitions
import Data.Maybe (isJust, fromJust)
import Data.Text (intercalate, pack, Text, unpack)
import Data.List (isPrefixOf)
import Network.URL (host, importURL, exportHost, URLType(..), host, url_type)
import Debug.Trace (trace)

isYoutubeHost :: URLType -> Bool
isYoutubeHost (Absolute h) = host h == "youtube.com" || host h == "www.youtube.com"
isYoutubeHost _ = False

isYoutube :: Bookmark -> Bool
isYoutube (Bookmark _ url) = (isYoutubeHost . url_type) (fromJust (importURL url))

main :: IO ()
main = do
  conn <- open "./places.sqlite"

  bookmarksRows <- query_  conn "SELECT moz_bookmarks.title, moz_places.url FROM moz_bookmarks INNER JOIN moz_places ON moz_places.id = moz_bookmarks.fk" :: IO[Bookmark]

  print $ filter isYoutube bookmarksRows
  
  close conn
