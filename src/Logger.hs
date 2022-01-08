module Logger where

import Definitions
import Actor (ActorId (..))
import System.Console.ANSI
import Data.String.Interpolate
import Text.Read (Lexeme(String))
import Data.Char (ord)
import Data.Time.Format (formatTime, defaultTimeLocale)
import Data.Time.LocalTime (getZonedTime)

data Log
  = Finished
  | ProcessingStarted Int [Process]
  | DownloadStarted String ActorId
  | DownloadFinished String ActorId
  | DownloadError String ActorId
  | ProcessSavingStarted
  deriving (Show)

actorColors = do
  intensity <- [Vivid, Dull]
  color <- [Green, Yellow, Blue, Magenta, Cyan]
  [(intensity, color)]

printActor :: ActorId -> String -> IO ()
printActor (ActorId n id) text = do
  now <- getZonedTime
  let color = actorColors !! (n `mod` length actorColors)
  let time = formatTime defaultTimeLocale "%Y-%m-%d %H:%M:%S" now

  setSGR [uncurry (SetColor Foreground) color]
  putStrLn ([iii|[#{id}] #{time} #{text}|] :: String)
  setSGR [Reset]

log :: Log -> IO ()

log (ProcessingStarted actorCount processes) = do
  putStrLn ([iii|Starting processing of #{length processes} pending processes using #{actorCount} actors|] :: String)
  
log Finished = do
  putStrLn "Finished processing all processes"

log (DownloadStarted youtubeId actorId) = do
  printActor actorId [iii|Downloading: #{youtubeId}|]

log (DownloadFinished youtubeId actorId) = do
  printActor actorId [iii|Downloading finished: #{youtubeId}|]

log (DownloadError youtubeId actorId) = do
  printActor actorId [iii|Downloading error: #{youtubeId}|]

log ProcessSavingStarted = do
  setSGR [SetColor Foreground Vivid Red]
  putStrLn "Preparing bookmarks for processing. This may take a while..."
  setSGR [Reset]
