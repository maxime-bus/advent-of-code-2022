module Main where

import System.IO
import Data.List (isSubsequenceOf)
import Data.List.Split (splitOn)

type SectionID = Int

fullyContains :: (Eq a) => ([a], [a]) -> Bool
fullyContains (a, b) = isSubsequenceOf a b || isSubsequenceOf b a

parseSection :: String -> [SectionID]
parseSection s = 
  let [s1, s2] = splitOn "-" s
  in [read s1 .. read s2] --unsafe read

parseLine :: String -> ([SectionID], [SectionID])
parseLine l = 
  let [p1, p2] = splitOn "," l
  in (parseSection p1, parseSection p2)

part1 :: (Eq a) => [([a], [a])] -> Int
part1 x = length $ filter fullyContains x

main :: IO ()
main = do
  content <- lines <$> readFile "input.txt"
  let sections = mapM parseLine content
  print (part1 $ map parseLine content) 

