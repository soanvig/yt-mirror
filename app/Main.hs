module Main where

import Lib

main :: IO ()
main = do
  bookmarks <- getYoutubeBookmarks
  print bookmarks
