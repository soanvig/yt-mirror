module Youtube (
  getYoutubeId
) where

import Network.URI
import Helpers
import Data.List.Split ( splitOn )

isYoutubeHost :: URIAuth -> Bool
isYoutubeHost auth = uriRegName auth == "youtube.com" || uriRegName auth == "www.youtube.com"

parseQuery :: String -> [(String, String)]
parseQuery ('?' : xs) = parseQuery xs
parseQuery "" = []
parseQuery query =  (fmap parseParam . getParams) query
  where
    getParams = splitOn "&"
    parseParam = toTuple . splitOn "="
    toTuple [name, value] = (name, value)
    toTuple _ = undefined -- unreachable

toYoutubeUrl :: String -> Maybe URI
toYoutubeUrl url = parseURI url >>= (
  \parsedUrl -> uriAuthority parsedUrl
    >>= maybeCondition isYoutubeHost
    >> Just parsedUrl
  )

findYoutubeId :: [(String, String)] -> Maybe String
findYoutubeId [] = Nothing
findYoutubeId (("v", youtubeId) : rest) = Just youtubeId
findYoutubeId ((x, y) : rest) = findYoutubeId rest

-- Public

getYoutubeId :: String -> Maybe String
getYoutubeId url = toYoutubeUrl url >>= (findYoutubeId . parseQuery . uriQuery)
    