module Day7
    ( day7
    ) where

import qualified Data.Map as M
import Data.List.Split (chunksOf)
import Data.Maybe (fromJust)

type BagGraph = M.Map String [(Int, String)]

day7 :: IO ()
day7 = do
  input <- readFile "input/day7"
  let graph = genGraph input

  -- subtract 1, since a golden bag doesnt contain itself!
  putStrLn $ "Part 1: " ++ show (M.size (M.filterWithKey (\x _ -> countGolden graph x > 0) graph) - 1)
  putStrLn $ "Part 2: " ++ show (countInBag graph "shiny gold" - 1)

genGraph :: String -> BagGraph
genGraph input = graph
  where rules = lines input
        graph = foldl (\acc x -> uncurry M.insert (parseRule x) acc) M.empty rules

parseRule :: String -> (String, [(Int, String)])
parseRule input = rule
  where parts = words input
        name = unwords (take 2 parts)
        contains = if length parts > 7 then
                     foldl (\acc x -> (read (head x), unwords (take 2 (drop 1 x))):acc) [] (chunksOf 4 (drop 4 parts))
                   else
                     []
        rule = (name, contains)

countGolden :: BagGraph -> String -> Int
countGolden _ "shiny gold" = 1
countGolden graph key = foldl (\acc x -> acc + fst x * countGolden graph (snd x)) 0 (fromJust (M.lookup key graph))

countInBag :: BagGraph -> String -> Int
countInBag graph key = res
  where contents = fromJust (M.lookup key graph)
        res = if null contents then
          1
        else
          foldl (\acc x -> acc + fst x * countInBag graph (snd x)) 1 contents