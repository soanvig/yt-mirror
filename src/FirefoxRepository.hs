module FirefoxRepository (
  FirefoxRepository(..)
) where

import Database.SQLite.Simple 
import Definitions
import Data.String.Interpolate

instance FromRow Bookmark where
    fromRow = Bookmark
      <$> field
      <*> field

selectBookmarksQuery :: Query
selectBookmarksQuery = [iii|
SELECT moz_bookmarks.title, moz_places.url
FROM moz_bookmarks
INNER JOIN moz_places
ON moz_places.id = moz_bookmarks.fk
|]
      
-- Public

newtype FirefoxRepository = FirefoxRepository String

instance Repository FirefoxRepository where
  loadBookmarks (FirefoxRepository placesLocation) = do
    conn <- open placesLocation
    result <-  query_  conn selectBookmarksQuery
    close conn

    return result
