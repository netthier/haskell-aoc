module Day1
    ( day1
    ) where

day1 :: IO ()
day1 = do
  inputs <- fmap lines (readFile "input/day1")
  let nums = map read inputs :: [Int]
  putStrLn $ "Part 1: " ++ show (head [ x * y | x <- nums, y <- nums, x + y == 2020])
  putStrLn $ "Part 2: " ++ show (head [ x * y * z | x <- nums, y <- nums, z <- nums, x + y + z == 2020])
