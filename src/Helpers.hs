module Helpers where

import Control.Concurrent.STM (TVar, readTVarIO)
import Control.Concurrent (threadDelay)
import System.Random
import Data.Char (isSpace)

-- Round robins b over a
roundRobin :: [a] -> [b] -> [(a, b)]
roundRobin [] [] = []
roundRobin [] _ = []
roundRobin _ [] = []
roundRobin a b = zip a b ++ roundRobin a (drop (length a) b)

waitFor :: Eq a => a -> (a -> a -> Bool) -> TVar a -> IO ()
waitFor expectedValue eq var = do
   threadDelay $ 5 * 1000000 -- 5 seconds
   currentValue <- readTVarIO var

   if eq currentValue expectedValue then
      return ()
   else
      waitFor expectedValue eq var

maybeCondition :: (a -> Bool) -> a -> Maybe a
maybeCondition cond x = if cond x then Just x else Nothing;

getRandomString :: Int -> Int -> String
getRandomString length seed = take length $ randomRs ('a', 'z') (mkStdGen seed)

trim :: String -> String
trim = f . f
   where f = reverse . dropWhile isSpace

replace :: Eq a => a -> a -> [a] -> [a]
replace x y [] = []
replace x y (z:zs) | z == x = y : replace x y zs
replace x y (z:zs) = z : replace x y zs
