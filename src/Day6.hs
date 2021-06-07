module Day6
    ( day6
    ) where

import qualified Data.Text as T
import Data.List (nub, intersect)

day6 :: IO ()
day6 = do
  inputs <- readFile "input/day6"
  let groups =  map (lines . T.unpack) (T.splitOn "\n\n" (T.pack inputs))
  putStrLn $ "Part 1: " ++ show (sum (map (length . nub . concat) groups))
  putStrLn $ "Part 2: " ++ show (sum (map (length . foldl1 intersect) groups))
