module Dependencies (verifyDependencies) where

import System.Directory (findExecutable)
import Data.Either (lefts)

verifyDependency :: String -> IO (Either String String)
verifyDependency executable = do
  result <- findExecutable executable
  case result of
    Nothing -> return $ Left executable
    Just _ -> return $ Right executable

dependencyList :: [String]
dependencyList = [
  "yt-dlp"
  , "sqlite3"
  , "ffmpeg"
  ]

-- public

verifyDependencies :: IO (Either [String] ())
verifyDependencies = do
  results <- mapM verifyDependency dependencyList
  let errors = lefts results

  case errors of
    [] -> return $ Right ()
    e -> return $ Left e
  
