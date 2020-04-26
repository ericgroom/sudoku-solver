module Lib where

import Data.Set (Set, fromList, singleton)

someFunc :: IO ()
someFunc = putStrLn "foo"


data InputTile = Empty | NonEmpty Int
    deriving Show
type InputBoard = [[InputTile]]
data Tile = Resolved Int | Unresolved (Set Int)
    deriving Show
type WorkingBoard = [[Tile]]

allPossibleValues :: Set Int
allPossibleValues = fromList [1, 2, 3, 4, 5, 6, 7, 8, 9]

possibleValues :: InputTile -> Tile
possibleValues Empty = Unresolved allPossibleValues
possibleValues (NonEmpty x) = Resolved x

parseBoard :: InputBoard -> WorkingBoard
parseBoard input = [[possibleValues tile | tile <- row] | row <- input]