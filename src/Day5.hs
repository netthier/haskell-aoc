module Day5
    ( day5
    ) where

type Seat = (Int, Int)

day5 :: IO ()
day5 = do
  inputs <- fmap lines (readFile "input/day5")
  let seat_ids = map (seatId . calcSeat) inputs
  putStrLn $ "Part 1: " ++ show (maximum seat_ids)
  putStrLn $ "Part 2: " ++ show (head [x | x <- [0..1023], (x - 1) `elem` seat_ids, (x + 1) `elem` seat_ids, not (x `elem` seat_ids)])

calcSeat :: String -> Seat
calcSeat str = res
  where row = head (foldl bsp [0..127] (take 7 str))
        col = head (foldl bsp [0..7] (drop 7 str))
        res = (row, col)

bsp :: [Int] -> Char -> [Int]
bsp acc c
  | c == 'F' || c == 'L' = take (length acc `div` 2) acc
  | otherwise = drop (length acc `div` 2) acc

seatId :: Seat -> Int
seatId seat = fst seat * 8 + snd seat