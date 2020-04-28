module Main where

import Lib
import Util

main :: IO ()
main = do
  putStrLn $ show $ solve testBoard
  putStrLn $ show $ solve hardBoard

testBoard :: WorkingBoard
testBoard = 
  convert 
  [ [5, 3, 0, 0, 7, 0, 0, 0, 0]
  , [6, 0, 0, 1, 9, 5, 0, 0, 0]
  , [0, 9, 8, 0, 0, 0, 0, 6, 0]
  , [8, 0, 0, 0, 6, 0, 0, 0, 3]
  , [4, 0, 0, 8, 0, 3, 0, 0, 1]
  , [7, 0, 0, 0, 2, 0, 0, 0, 6]
  , [0, 6, 0, 0, 0, 0, 2, 8, 0]
  , [0, 0, 0, 4, 1, 9, 0, 0, 5]
  , [0, 0, 0, 0, 8, 0, 0, 7, 9]
  ]

hardBoard :: WorkingBoard
hardBoard = 
  convert 
  [ [0, 0, 0, 0, 0, 0, 6, 8, 0]
  , [0, 0, 0, 0, 7, 3, 0, 0, 9]
  , [3, 0, 9, 0, 0, 0, 0, 4, 5]
  , [4, 9, 0, 0, 0, 0, 0, 0, 0]
  , [8, 0, 3, 0, 5, 0, 9, 0, 2]
  , [0, 0, 0, 0, 0, 0, 0, 3, 6]
  , [9, 6, 0, 0, 0, 0, 3, 0, 8]
  , [7, 0, 0, 6, 8, 0, 0, 0, 0]
  , [0, 2, 8, 0, 0, 0, 0, 0, 0]
  ]

convert :: [[Int]] -> WorkingBoard
convert board = parseBoard $ map2D convertTile board
  where
    convertTile n
      | n >= 1 && n <= 9 = NonEmpty n
      | otherwise = Empty
