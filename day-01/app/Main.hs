module Main where

import System.IO

sort :: (Ord a) => [a] -> [a]
sort [] = []
sort (x : xs) =
  let s = sort [a | a <- xs, a <= x]
      b = sort [a | a <- xs, a > x]
   in s ++ [x] ++ b

toCalories :: [String] -> [Integer]
toCalories [] = []
toCalories ("" : xs) = toCalories xs
toCalories xs = sum (map read (takeWhile (/= "") xs)) : toCalories (dropWhile (/= "") xs)

main :: IO ()
main = do
  calories <- fmap (toCalories . lines) $ openFile "input.txt" ReadMode >>= hGetContents 
  putStrLn ("Part 1 : " ++ show (maximum calories))
  putStrLn ("Part 2 : " ++ show (sum $ take 3 $ reverse $ sort calories))
