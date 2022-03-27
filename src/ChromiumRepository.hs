{-# LANGUAGE DeriveGeneric #-}

module ChromiumRepository (
  ChromeRepository(..)
) where

import Definitions
import GHC.Generics
import Data.Text (Text)
import Data.Aeson
import qualified Data.ByteString.Lazy as B

newtype BookmarkCore = BookmarkCore {
  roots :: BookmarkRoots
} deriving (Generic, Show)

data BookmarkRoots = BookmarkRoots {
  bookmark_bar :: BookmarkEntry
  , other :: BookmarkEntry
  , synced :: BookmarkEntry
} deriving (Generic, Show)

-- @TODO We could differentiate between different bookmark types (folder and url)
data BookmarkEntry = BookmarkEntry {
  children :: Maybe [BookmarkEntry]
  , url :: Maybe String
  , name :: String
} deriving (Generic, Show)

instance FromJSON BookmarkCore
instance FromJSON BookmarkRoots
instance FromJSON BookmarkEntry

parseBookmarks :: BookmarkEntry -> [Bookmark]
parseBookmarks (BookmarkEntry (Just children) Nothing _) = concatMap parseBookmarks children
parseBookmarks (BookmarkEntry Nothing (Just url) name) = [Bookmark name url]
parseBookmarks entry = error $ "Could not parse bookmark entry: " ++ show entry

collectBookmarks :: BookmarkCore -> [Bookmark]
collectBookmarks core = concatMap parseBookmarks $ sequence [bookmark_bar . roots, other . roots] core

-- Public

newtype ChromeRepository = ChromeRepository String

instance Repository ChromeRepository where
  loadBookmarks (ChromeRepository bookmarksLocation) = do
    bookmarksContent <- B.readFile bookmarksLocation

    let parsedContent = decode bookmarksContent :: Maybe BookmarkCore

    case parsedContent of
      Just core -> return $ collectBookmarks core
      Nothing -> error "Cannot parse bookmarks file"
