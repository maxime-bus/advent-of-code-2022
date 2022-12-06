module Main
  ( main,
  )
where

import Control.Lens
import Data.Char (isAlpha, isDigit, isSpace)
import Data.List (transpose)
import Data.Maybe (fromMaybe)

moveCrates :: Int -> String -> String -> (String, String)
moveCrates _ [] t = ([], t)
moveCrates 0 source target = (source, target)
moveCrates n (s:source) target = moveCrates (n-1) source (s:target)

part1 :: [[Int]] -> [String] -> String
part1 [] stacks = map head stacks
part1 (action : actions) stacks =
  let [m, f, t] = action
      source = stacks ^. element (f - 1)
      target = stacks ^. element (t - 1)
      (source', target') = moveCrates m source target
      stacks' = (element (t - 1) .~ target') $ (element (f - 1) .~ source') stacks
   in part1 actions stacks'

stacks c = map (dropWhile isSpace) $ filter (any isAlpha) $ transpose $ init $ takeWhile (not . null) c

craneActions :: [String] -> [[Int]]
craneActions c = map ((map read . filter (all isDigit)) . words) (tail $ dropWhile (not . null) c)

content = lines <$> readFile "input.txt"

main :: IO ()
main = do
  c <- content
  print $ part1 (craneActions c) (stacks c)
