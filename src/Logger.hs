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
  | AllSynchronizationStarted Int [Process]
  | SynchronizationStarted String ActorId
  | SynchronizationFinished String ActorId
  | SynchronizationError String ActorId
  | PreparingStarted Int
  | PreparingFinished
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

log (AllSynchronizationStarted actorCount processes) = do
  putStrLn ([iii|Starting synchronizing of #{length processes} pending bookmarks using #{actorCount} actors.|] :: String)
  
log Finished = do
  setSGR [SetColor Foreground Vivid Green]
  putStrLn "Finished synchronizing all bookmarks."
  setSGR [Reset]

log (SynchronizationStarted youtubeId actorId) = do
  printActor actorId [iii|Downloading: #{youtubeId}.|]

log (SynchronizationFinished youtubeId actorId) = do
  printActor actorId [iii|Downloading finished: #{youtubeId}.|]

log (SynchronizationError youtubeId actorId) = do
  printActor actorId [iii|Downloading error: #{youtubeId}.|]

log (PreparingStarted bookmarksCount) = do
  putStrLn [iii|Preparing bookmarks (overall Youtube bookmarks: #{bookmarksCount}) for synchronization.|]

log PreparingFinished = do
  setSGR [SetColor Foreground Vivid Green]
  putStrLn "Bookmarks prepared!"
  setSGR [Reset]
