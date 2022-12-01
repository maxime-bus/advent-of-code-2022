module Main where

import System.IO

sort :: (Ord a) => [a] -> [a]
sort [] = []
sort (x:xs) = 
  let s = sort [a | a <- xs, a <= x]
      b = sort [a | a <- xs, a > x]
  in s ++ [x] ++ b

c :: [String] -> [Integer]
c [] = []
c ("":xs) = c xs
c xs = sum (map read (takeWhile (/= "") xs)) : c (dropWhile (/= "") xs)

main :: IO ()
main = part2

part1 :: IO ()
part1 = do
  handle <- openFile "input.txt" ReadMode
  content <- hGetContents handle
  print $ foldr max 0 (c $ lines content)

part2 :: IO ()
part2 = do
  handle <- openFile "input.txt" ReadMode
  content <- hGetContents handle
  print $ sum $ take 3 $ reverse $ sort (c $ lines content)
