module Definitions where

  import Database.SQLite.Simple.FromRow
  import Data.Text (Text)

  data Bookmark = Bookmark {
    bookmarkTitle :: String,
    bookmarkUrl :: String
    } deriving (Show)

  instance FromRow Bookmark where
    fromRow = Bookmark
      <$> field
      <*> field

  -- data BookmarkRow = BookmarkRow {
    -- bookmarkId :: Int,
    -- bookmarkType :: Int,
    -- bookmarkFk :: Maybe Int,
    -- bookmarkParent :: Int,
    -- bookmarkPosition :: Int,
    -- bookmarkTitle :: Maybe Text
    -- bookmarkKeywordId :: Maybe Int,
    -- bookmarkFolderType :: Maybe Text,
    -- bookmarkDateAdded :: Int,
    -- bookmarkLastModified :: Int,
    -- bookmarkGuid :: Text,
    -- bookmarkSyncStatus :: Int,
    -- bookmarkSyncChangeCounter :: Int
    -- } deriving (Show)

  -- instance FromRow BookmarkRow where
  --   fromRow = BookmarkRow
  --     <$> field
  --     <*> field

  -- data PlaceRow = PlaceRow {
  --   placeId :: Int,
  --   placeUrl :: Text
    -- placeTitle :: Text,
    -- placeRevHost :: Text,
    -- placeVisitCount :: Int,
    -- placeHidden :: Int,
    -- placeTyped :: Int,
    -- placeFrecency :: Int,
    -- placeLastVisitDate :: Int,
    -- placeGuid :: Int,
    -- placeForeignCount :: Int,
    -- placeUrlHash :: Int,
    -- placeDescription :: Text,
    -- placePreviewImageUrl :: Text,
    -- placeOriginId :: Int
  -- } deriving (Show)

  -- instance FromRow PlaceRow where
  --   fromRow = PlaceRow
  --     <$> field
  --     <*> field


