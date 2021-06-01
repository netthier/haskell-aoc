module Day3
    ( day3
    ) where

day3 :: IO ()
day3 = do
  inputs <- fmap lines (readFile "input/day3")
  let a = countTrees 1 1 inputs
  let b = countTrees 3 1 inputs
  let c = countTrees 5 1 inputs
  let d = countTrees 7 1 inputs
  let e = countTrees 1 2 inputs
  putStrLn ("Part 1: " ++ show b)
  putStrLn ("Part 2: " ++ show (a * b * c * d * e))

countTrees :: Int -> Int -> [[Char]] -> Int
countTrees x_off y_off grid =
  length
  [x |
    (i, x) <- zip [0..] grid,
    i `mod` y_off == 0,
    (cycle x) !! (i `div` y_off * x_off) == '#']