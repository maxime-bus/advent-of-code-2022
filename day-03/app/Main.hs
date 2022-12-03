module Main where

import Data.List (intersect)
import Data.Char (ord, isUpper)

stringToRuckSack :: String -> (String, String)
stringToRuckSack s = splitAt half s
  where half = length s `div` 2

itemInCommon :: (String, String) -> String
itemInCommon (left, right) = left `intersect` right

-- | [ord 'a' ... ord 'z'] -> [97, 122] - 95 -> [1, 26]
-- | [ord 'A' ... ord 'Z'] -> [65, 90] - 38 ->  [27, 52]
charToScore :: Char -> Int
charToScore c
  | isUpper c = ord c - 38
  | otherwise = ord c - 96

main :: IO ()
main = do
  content <- lines <$> readFile "input.txt"
  let ruckSacks = map stringToRuckSack content
  let prioritiesSum = sum $ map ((charToScore . head) . itemInCommon) ruckSacks
  print prioritiesSum
