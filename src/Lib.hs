module Lib where

import Data.Set (Set, fromList, singleton)
import Util

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

parseBoard :: InputBoard -> WorkingBoard
parseBoard = map2D possibleValues
  where possibleValues Empty = Unresolved allPossibleValues
        possibleValues (NonEmpty x) = Resolved x


data Coord = Coord { x :: Int
                   , y :: Int
                   } deriving (Show)

allCoords :: WorkingBoard -> [Coord]
allCoords board =
    [Coord {x=x, y=y} | (y, row) <- enumerate board, (x, _) <- enumerate row]
    where enumerate = zip [0..]

getCoord :: WorkingBoard -> Coord -> Tile
getCoord board coord =
    board !! (y coord) !! (x coord)

maybeCoord :: WorkingBoard -> Coord -> Maybe Tile
maybeCoord board coord = do
  let yLen = length board
  Just safeY <- if yVal >= 0 && yVal < yLen then return $ Just yVal else return Nothing
  let xLen = length (board !! safeY)
  Just safeX <- if xVal >= 0 && xVal < xLen then return $ Just xVal else return Nothing
  return $ getCoord board Coord{x=safeX, y=safeY}
  where xVal = x coord
        yVal = y coord
        max = (length board) - 1
