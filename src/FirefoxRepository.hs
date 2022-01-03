module FirefoxRepository (openRepository, loadBookmarks) where

import Database.SQLite.Simple 
import Definitions
import Control.Monad.Reader

instance FromRow Bookmark where
    fromRow = Bookmark
      <$> field
      <*> field
      
-- Public

openRepository :: String -> ReaderT Connection IO a -> IO a
openRepository placesLocation operation = do
  conn <- open placesLocation

  result <- runReaderT operation conn

  close conn

  return result

loadBookmarks :: ReaderT Connection IO [Bookmark]
loadBookmarks = do
  conn <- ask
  lift $ query_  conn "SELECT moz_bookmarks.title, moz_places.url FROM moz_bookmarks INNER JOIN moz_places ON moz_places.id = moz_bookmarks.fk"
