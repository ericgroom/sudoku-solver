module Lib where

import Data.Set (Set, fromList, singleton)
import Util
import Data.Maybe (isJust, fromJust)

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

up :: Coord -> Coord
up c =
  c { y = (y c) - 1 }
down :: Coord -> Coord
down c =
  c { y = (y c) + 1 }
left :: Coord -> Coord
left c =
  c { x = (x c) - 1 }
right :: Coord -> Coord
right c =
  c { x = (x c) + 1 }
upRight :: Coord -> Coord
upRight c =
  Coord { x = (x c) + 1, y = (y c) - 1 }
upLeft :: Coord -> Coord
upLeft c =
  Coord { x = (x c) - 1, y = (y c) - 1 }
downRight :: Coord -> Coord
downRight c =
  Coord { x = (x c) + 1, y = (y c) + 1 }
downLeft :: Coord -> Coord
downLeft c =
  Coord { x = (x c) - 1, y = (y c) + 1 }
allDirections :: [Coord -> Coord]
allDirections = [up, down, left, right, upRight, upLeft, downRight, downLeft]

traverse' :: (Coord -> Coord) -> Coord -> [Coord]
traverse' direction start =
  let next = direction start in
      next : traverse' direction next

getTraversal :: WorkingBoard -> (Coord -> Coord) -> Coord -> [Tile]
getTraversal board direction start =
  map fromJust $ takeWhile isJust $ map (maybeCoord board) $ traverse' direction start

-- $> getTraversal testBoard up Coord{x=0, y=8}
