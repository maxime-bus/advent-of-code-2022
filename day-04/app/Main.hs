module Main
  ( main,
  )
where

import Data.List (intersect)
import Data.List.Split (splitOn)
import System.IO

type SectionID = Int

overlaps :: (Eq a) => ([a], [a]) -> Bool
overlaps (a, b) = (not . null) $ intersect a b

parseSection :: String -> [SectionID]
parseSection s =
  let [s1, s2] = splitOn "-" s
   in [read s1 .. read s2] -- unsafe read

parseLine :: String -> ([SectionID], [SectionID])
parseLine l =
  let [p1, p2] = splitOn "," l
   in (parseSection p1, parseSection p2)

part2 :: (Eq a) => [([a], [a])] -> Int
part2 x = length $ filter overlaps x

main :: IO ()
main = do
  content <- lines <$> readFile "input.txt"
  let sections = mapM parseLine content
  print (part2 $ map parseLine content)
