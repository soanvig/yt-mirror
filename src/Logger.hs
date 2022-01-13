module Logger where

import Definitions
import Actor (ActorId (..))
import System.Console.ANSI
import Data.String.Interpolate
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
  | FailedLogRequested Bool [Process]
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
  let sgr = [uncurry (SetColor Foreground) color];

  putStrLn ([iii|#{setSGRCode sgr}[#{id}] #{time} #{text}#{setSGRCode [Reset]}|] :: String)

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

log (FailedLogRequested isShort processes) = do
  mapM_ (logProcess isShort) processes

  where
    logProcess :: Bool -> Process -> IO ()
    logProcess _ (Process youtubeId _ Nothing) = do
      return ()
    logProcess True (Process youtubeId _ (Just error)) = do
      putStrLn youtubeId
    logProcess False (Process youtubeId _ (Just error)) = do
      setSGR [SetColor Foreground Vivid Green]
      putStr youtubeId
      setSGR [Reset]
      putStr " | "
      putStrLn error
            
      

