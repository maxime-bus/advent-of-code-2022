module Main
  ( main,
  )
where

type PlayerShape = Shape

type OpponentShape = Shape

type PlayerScore = Integer

data RoundOutcome = Win | Loss | Draw deriving (Eq, Show)

data Shape = Rock | Paper | Scissors deriving (Eq, Show)

pointsFromOutcome :: RoundOutcome -> Integer
pointsFromOutcome Win = 6
pointsFromOutcome Draw = 3
pointsFromOutcome Loss = 0

roundOutcome :: PlayerShape -> OpponentShape -> RoundOutcome
roundOutcome Rock Scissors = Win
roundOutcome Paper Rock = Win
roundOutcome Scissors Paper = Win
roundOutcome ps os
  | ps == os = Draw
  | otherwise = Loss

pointsForShape :: Shape -> Integer
pointsForShape Rock = 1
pointsForShape Paper = 2
pointsForShape Scissors = 3

charToShape :: Char -> Maybe Shape
charToShape s
  | s == 'A' || s == 'X' = Just Rock
  | s == 'B' || s == 'Y' = Just Paper
  | s == 'C' || s == 'Z' = Just Scissors
  | otherwise = Nothing

parseLine :: String -> Maybe (PlayerShape, OpponentShape)
parseLine [os, ' ', ps] = (,) <$> charToShape ps <*> charToShape os
parseLine _ = Nothing

parseFile :: [String] -> Maybe [(PlayerShape, OpponentShape)]
parseFile = mapM parseLine 

playRound :: (PlayerShape, OpponentShape) -> PlayerScore
playRound (ps, os) = pointsFromOutcome (roundOutcome ps os) + pointsForShape ps

main :: IO ()
main = do
  content <- readFile "input.txt"
  let rounds = parseFile $ lines content
  let finalScore = fmap (foldr (\ r a -> playRound r + a) 0) rounds 
  print finalScore
