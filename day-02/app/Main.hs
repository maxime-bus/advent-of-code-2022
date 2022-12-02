module Main
  ( main,
  )
where

type OpponentShape = Shape

type ExpectedOutcome = RoundOutcome

type PlayerScore = Integer

data RoundOutcome = Win | Loss | Draw deriving (Eq, Show)

data Shape = Rock | Paper | Scissors deriving (Eq, Show)

pointsFromOutcome :: RoundOutcome -> Integer
pointsFromOutcome Win = 6
pointsFromOutcome Draw = 3
pointsFromOutcome Loss = 0

pointsForShape :: Shape -> Integer
pointsForShape Rock = 1
pointsForShape Paper = 2
pointsForShape Scissors = 3

charToShape :: Char -> Maybe Shape
charToShape 'A' = Just Rock
charToShape 'B' = Just Paper
charToShape 'C' = Just Scissors
charToShape _ = Nothing

charToOutcome :: Char -> Maybe RoundOutcome
charToOutcome 'X' = Just Loss
charToOutcome 'Y' = Just Draw
charToOutcome 'Z' = Just Win
charToOutcome _ = Nothing

expectedShape :: OpponentShape -> ExpectedOutcome -> Shape
expectedShape Rock Win = Paper
expectedShape Paper Win = Scissors
expectedShape Scissors Win = Rock
expectedShape Rock Loss = Scissors
expectedShape Paper Loss = Rock
expectedShape Scissors Loss = Paper
expectedShape shape Draw = shape

parseLine :: String -> Maybe (OpponentShape, ExpectedOutcome)
parseLine [os, ' ', eo] = (,) <$> charToShape os <*> charToOutcome eo
parseLine _ = Nothing

parseFile :: [String] -> Maybe [(OpponentShape, ExpectedOutcome)]
parseFile = mapM parseLine

playRound :: (OpponentShape, ExpectedOutcome) -> PlayerScore
playRound (os, eo) = pointsFromOutcome eo + pointsForShape (expectedShape os eo)

main :: IO ()
main = do
  content <- readFile "input.txt"
  let rounds = parseFile $ lines content
  let finalScore = fmap (foldr (\r a -> playRound r + a) 0) rounds
  print finalScore
