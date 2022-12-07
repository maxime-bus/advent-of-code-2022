module Main where

import Data.Monoid
import Data.Foldable
import Control.Applicative

data FileSystem a = File String a | Folder String [FileSystem a] deriving (Show)

isFolder :: FileSystem a -> Bool
isFolder (Folder _ _ ) = True
isFolder _ = False

instance Functor FileSystem where
  fmap f (File n s) = File n (f s)
  fmap f (Folder n fs) = Folder n (map (fmap f) fs)

instance Foldable FileSystem where
  foldMap f (File _ s) = f s
  foldMap f (Folder _ fs) = fold (fmap (foldMap f) fs)

example :: FileSystem Int
example =
  Folder
    "/"
    [ Folder
        "a"
        [ Folder
            "e"
            [ File "i" 584
            ],
          File "f" 29116,
          File "g" 2557,
          File "h.lst" 62596
        ],
      File "b.txt" 14848514,
      File "c.dat" 8504156,
      Folder
        "d"
        [ File "j" 4060174,
          File "d.log" 8033020,
          File "d.ext" 5626152,
          File "k" 7214296
        ]
    ]

content :: IO [String]
content = lines <$> readFile "input.txt"

main :: IO ()
main = do
  print $ "Example : " ++ show (getSum $ foldMap Sum example)
