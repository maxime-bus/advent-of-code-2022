module Main
  ( main,
  )
where

import Data.Char (isDigit)
import Data.Either (isLeft)
import Data.Stack
import Text.Parsec

type SourceStack = Stack

type TargetStack = Stack

stackFromList :: [a] -> Stack a
stackFromList l = foldl stackPush stackNew (reverse l)

moveCrates :: Int -> SourceStack a -> TargetStack a -> (SourceStack a, TargetStack a)
moveCrates 0 sourceStack targetStack = (sourceStack, targetStack)
moveCrates n sourceStack targetStack =
  case stackPop sourceStack of
    Just (sourceStack', movedCrate) -> moveCrates (n - 1) sourceStack' (stackPush targetStack movedCrate)
    Nothing -> (sourceStack, targetStack)

noCrate :: Parsec String () Char
noCrate = char ' ' *> char ' ' <* char ' '

crate :: Parsec String () Char
crate = (char '[' *> letter <* char ']') <|> noCrate

crates :: Parsec String () String
crates = sepBy crate space

-- invert :: [[a]] -> [[a]]
-- invert l = foldr (zipWith (:)) (replicate (length (head l)) []) l

main :: IO ()
main = do
  content <- lines <$> readFile "input.txt"
  let cratesConfig = takeWhile (not . isDigit . (!! 1)) content
  let crates = parse crates "" (head cratesConfig)
  print "toto"
