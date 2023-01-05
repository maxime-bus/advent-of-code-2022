module Main where

import Test.Hspec

data FileSystem = File String Int | Directory String [FileSystem] deriving (Show, Eq)

tests = hspec $ do
  describe "Filesystem manipulation" $ do
    it "can add file to folder" $ do
      File "foo" 1 `addTo` Directory "dir" [] `shouldBe` Directory "dir" [File "foo" 1]

    it "can add directory to another directory" $ do
      Directory "dir1" [] `addTo` Directory "dir2" [] `shouldBe` Directory "dir2" [Directory "dir1" []]

addTo :: FileSystem -> FileSystem -> FileSystem
addTo f@(File _ _) d@(Directory dn fs) = Directory dn (f:fs)
addTo d1@(Directory d1n d1fs) d2@(Directory d2n d2fs) = Directory d2n (d1:d2fs)



fsSize :: FileSystem -> Int
fsSize (File _ s) = s
fsSize (Directory _ fs) = sum (map fsSize fs)

content :: IO [String]
content = lines <$> readFile "input.txt"

main :: IO ()
main = do
  putStrLn "foo"
