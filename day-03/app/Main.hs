module Main where

import Data.Char (isUpper, ord)
import Data.List (intersect)

itemInCommon :: (String, String, String) -> String
itemInCommon (l, m, r) = [x | x <- l, y <- m, z <- r, (x == y) && (x == z) && (y == z)]

-- | [ord 'a' ... ord 'z'] -> [97, 122] - 95 -> [1, 26]
-- | [ord 'A' ... ord 'Z'] -> [65, 90] - 38 ->  [27, 52]
charToScore :: Char -> Int
charToScore c
  | isUpper c = ord c - 38
  | otherwise = ord c - 96

stringToGroupOfRucksacks :: [String] -> [(String, String, String)]
stringToGroupOfRucksacks [] = []
stringToGroupOfRucksacks (r1:r2:r3:rs) = (r1, r2, r3): stringToGroupOfRucksacks rs

main :: IO ()
main = do
  content <- lines <$> readFile "input.txt"
  let groupOfRucksacks = stringToGroupOfRucksacks content
  let prioritiesSum = sum $ map ((charToScore . head) . itemInCommon) groupOfRucksacks
  print prioritiesSum
