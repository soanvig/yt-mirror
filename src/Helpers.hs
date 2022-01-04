module Helpers where

import Control.Concurrent.STM (TVar, readTVarIO)
import Control.Concurrent (threadDelay)

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