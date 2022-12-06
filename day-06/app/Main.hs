module Main where

slidingWindow :: Int -> [a] -> [[a]]
slidingWindow _ [] = []
slidingWindow size xs
  | size >= length xs = [xs]
  | otherwise = take size xs : slidingWindow size (tail xs)

hasDuplicates :: (Eq a) => [a] -> Bool
hasDuplicates [] = False
hasDuplicates (x:xs) 
  | x `elem` xs = True
  | otherwise = hasDuplicates xs

part1 :: String -> Int
part1 s = (+4) $ length $ takeWhile (== True) $ map hasDuplicates $ slidingWindow 4 s
 
part2 :: String -> Int
part2 s = (+14) $ length $ takeWhile (== True) $ map hasDuplicates $ slidingWindow 14 s

main :: IO ()
main = do
  content <- readFile "input.txt"
  print $ part1 content
  print $ part2 content
