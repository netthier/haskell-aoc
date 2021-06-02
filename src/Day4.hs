module Day4
    ( day4
    ) where

import qualified Data.Text as T
import qualified Data.Map.Strict as Map
import qualified Data.Bifunctor as B
import Data.Maybe (mapMaybe)
import Data.Char (isNumber, isAlpha)
import Data.Ix (inRange)

data Passport = Passport {
  birthYear :: Int,
  issueYear :: Int,
  expYear :: Int,
  height :: (Int, String),
  hairColor :: String,
  eyeColor :: String,
  passId :: String
}

day4 :: IO ()
day4 = do
  input <- readFile "input/day4"
  let raw_passports = T.splitOn "\n\n" (T.pack input)
  let passports = mapMaybe (parsePassport . T.unpack) raw_passports
  putStrLn $ "Part 1: " ++ show (length passports)
  putStrLn $ "Part 1: " ++ show (length $ filter validatePassport passports)

validatePassport :: Passport -> Bool
validatePassport passport = res
  where (Passport byr iyr eyr hgt hcl ecl pid) = passport
        res = inRange (1920, 2002) byr
              && inRange (2010, 2020) iyr
              && inRange (2020, 2030) eyr
              && ((snd hgt == "cm" && inRange (150, 193) (fst hgt))
                 || (snd hgt == "in" && inRange (59, 76) (fst hgt)))
              && head hcl == '#' && all (\c -> isNumber c || elem c ['a'..'f']) (tail hcl)
              && ecl `elem` ["amb", "blu", "brn", "gry", "grn", "hzl", "oth"]
              && length pid == 9 && all isNumber pid

parsePassport :: String -> Maybe Passport
parsePassport raw = do
        let lines =  T.splitOn "\n" (T.pack raw)
        let fields = concatMap (words . T.unpack) lines
        let split_fields = map (B.second tail . break (== ':')) fields
        let pass_map = Map.fromList split_fields
        byr <- fmap read (Map.lookup "byr" pass_map)
        iyr <- fmap read (Map.lookup "iyr" pass_map)
        eyr <- fmap read (Map.lookup "eyr" pass_map)
        hgt <- fmap (B.first read . break isAlpha) (Map.lookup "hgt" pass_map)
        hcl <- Map.lookup "hcl" pass_map
        ecl <- Map.lookup "ecl" pass_map
        pid <- Map.lookup "pid" pass_map
        Just (Passport byr iyr eyr hgt hcl ecl pid)
