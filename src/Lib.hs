module Lib where

import Data.Set (Set, fromList, singleton, delete, size, elemAt)
import Util
import Data.Maybe (isJust, fromJust)
import Debug.Trace

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

type Direction = (Coord -> Coord)
up :: Direction
up c =
  c { y = (y c) - 1 }
down :: Direction
down c =
  c { y = (y c) + 1 }
left :: Direction
left c =
  c { x = (x c) - 1 }
right :: Direction
right c =
  c { x = (x c) + 1 }
upRight :: Direction
upRight c =
  Coord { x = (x c) + 1, y = (y c) - 1 }
upLeft :: Direction
upLeft c =
  Coord { x = (x c) - 1, y = (y c) - 1 }
downRight :: Direction
downRight c =
  Coord { x = (x c) + 1, y = (y c) + 1 }
downLeft :: Direction
downLeft c =
  Coord { x = (x c) - 1, y = (y c) + 1 }
allDirections :: [Direction]
allDirections = [up, down, left, right, upRight, upLeft, downRight, downLeft]

traverse' :: (Direction) -> Coord -> [Coord]
traverse' direction start =
  let next = direction start in
      next : traverse' direction next

getTraversal :: WorkingBoard -> Direction -> Coord -> [Tile]
getTraversal board direction start =
  map fromJust $ takeWhile isJust $ map (maybeCoord board) $ traverse' direction start

getSurroundingDirections :: WorkingBoard -> [Direction] -> Coord -> [Tile]
getSurroundingDirections board directions start =
  foldl (++) [] $ map go directions
    where go = \d -> getTraversal board d start

getSubgrid :: WorkingBoard -> Int -> Int -> Coord -> [Tile]
getSubgrid board width height coord =
  let 
    coords = 
      [ Coord{x=ix, y=iy}
      | iy <- [startY..startY+height-1]
      , ix <- [startX..startX+width-1]
      , ix /= (x coord) || iy /= (y coord)
      ]
      in
      map (getCoord board) coords 
  where relativeX = (x coord) `mod` width
        relativeY = (y coord) `mod` height
        startX = (x coord) - relativeX
        startY = (y coord) - relativeY

resolve :: WorkingBoard -> Coord -> Tile
resolve board coord =
  case (getCoord board coord) of
    Resolved x -> Resolved x
    Unresolved s -> foldl reduce (Unresolved s) allPatterns
  where surroundingRow = getSurroundingDirections board [left, right] coord
        surroundingCol = getSurroundingDirections board [up, down] coord
        surroundingGrid = getSubgrid              board 3 3 coord
        allPatterns = surroundingRow ++ surroundingCol ++ surroundingGrid
        reduce (Resolved x) _ = Resolved x
        reduce (Unresolved s) (Resolved x) = let reduced = delete x s in if size reduced <= 1 then Resolved (elemAt 0 reduced) else Unresolved reduced
        reduce (Unresolved s) (Unresolved _) = Unresolved s

-- 1 2 or 4 expected at 2,0
-- $> resolve testBoard Coord{x=2, y=0}
