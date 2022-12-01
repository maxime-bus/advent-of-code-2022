module Main where

import System.IO

c :: [String] -> [Integer]
c [] = []
c ("":xs) = c xs
c xs = sum (map read (takeWhile (/= "") xs)) : c (dropWhile (/= "") xs)

main :: IO ()
main = part1

part1 :: IO ()
part1 = do
  handle <- openFile "input.txt" ReadMode
  content <- hGetContents handle
  print $ foldr max 0 (c $ lines content)
