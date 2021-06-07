module Day2
    ( day2
    ) where

data Entry = Entry {
  _entryNum1 :: Int,
  _entryNum2 :: Int,
  _entryChar :: Char,
  _entryPass :: String
}

day2 :: IO ()
day2 = do
  inputs <- fmap lines (readFile "input/day2")
  let parsed = map parseEntry inputs
  putStrLn $ "Part 1: " ++ show (length $ filter validateEntry1 parsed)
  putStrLn $ "Part 2: " ++ show (length $ filter validateEntry2 parsed)

validateEntry1 :: Entry -> Bool
validateEntry1 (Entry num1 num2 char pass) = num1 <= count && num2 >= count
  where count = length $ filter (== char) pass

validateEntry2 :: Entry -> Bool
validateEntry2 (Entry num1 num2 char pass) = (a || b) && not (a && b)
  where a = pass !! (num1 - 1) == char
        b = pass !! (num2 - 1) == char

parseEntry :: String -> Entry
parseEntry str = Entry num1 num2 char pass
  where parts = words str
        (num1', num2') = break (=='-') (head parts)
        num1 = read num1'
        num2 = read $ tail num2'
        char = head $ parts !! 1
        pass = parts !! 2