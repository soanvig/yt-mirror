module Youtube (getYoutubeId, toYoutubeUrl) where

import Network.URL
import Data.Maybe ()

fromCondition :: (a -> Bool) -> a -> Maybe a
fromCondition cond x = if cond x then Just x else Nothing;

isYoutubeHost :: URLType -> Bool
isYoutubeHost (Absolute h) = host h == "youtube.com" || host h == "www.youtube.com"
isYoutubeHost _ = False

-- Public

toYoutubeUrl :: String -> Maybe String
toYoutubeUrl url = importURL url >>= fromCondition (isYoutubeHost . url_type) >> Just url

getYoutubeId :: String -> Maybe String
getYoutubeId url = importURL url >>= (findYoutubeId . url_params)
  where
    findYoutubeId :: [(String, String)] -> Maybe String
    findYoutubeId [] = Nothing
    findYoutubeId (("v", youtubeId) : rest) = Just youtubeId
    findYoutubeId ((x, y) : rest) = findYoutubeId rest