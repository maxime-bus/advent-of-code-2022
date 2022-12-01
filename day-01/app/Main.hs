module Main where

import System.IO
import Data.List (sort)

toCalories :: [String] -> [Integer]
toCalories [] = []
toCalories ("" : xs) = toCalories xs
toCalories xs = sum (map read (takeWhile (/= "") xs)) : toCalories (dropWhile (/= "") xs)

main :: IO ()
main = do
  calories <- fmap (toCalories . lines) $ openFile "input.txt" ReadMode >>= hGetContents 
  putStrLn ("Part 1 : " ++ show (maximum calories))
  putStrLn ("Part 2 : " ++ show (sum $ take 3 $ reverse $ sort calories))
