module Main where

import System.Environment
import Day1
import Day2
import Day3

main :: IO ()
main = do
  args <- getArgs
  let first = head args
  case first of
    "1" -> day1
    "2" -> day2
    "3" -> day3
    _ -> print (first ++ " is not a valid day")
