module Main where

-- TODO: Find a better way to do this :)

import System.Environment
import Day1
import Day2
import Day3
import Day4
import Day5
import Day6
import Day7

main :: IO ()
main = do
  args <- getArgs
  let first = head args
  case first of
    "1" -> day1
    "2" -> day2
    "3" -> day3
    "4" -> day4
    "5" -> day5
    "6" -> day6
    "7" -> day7
    _ -> print (first ++ " is not a valid day")
