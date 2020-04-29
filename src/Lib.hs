module Lib where

import Util
import Coord
import Data.Set (Set, fromList, singleton, delete, size, elemAt, difference)
import Data.Maybe (isJust, fromJust)
import Debug.Trace

data InputTile = Empty | NonEmpty Int
    deriving Show 
type InputBoard = [[InputTile]]
data Tile = Resolved Int | Unresolved (Set Int)
    deriving (Show, Eq)
type WorkingBoard = [[Tile]]

allPossibleValues :: Set Int
allPossibleValues = fromList [1, 2, 3, 4, 5, 6, 7, 8, 9]

parseBoard :: InputBoard -> WorkingBoard
parseBoard = map2D possibleValues
  where possibleValues Empty = Unresolved allPossibleValues
        possibleValues (NonEmpty x) = Resolved x

allCoordPairs :: WorkingBoard -> [[(Tile, Coord)]]
allCoordPairs board = 
    [[ (tile, Coord{x=x, y=y}) | (x, tile) <- enumerate row ] | (y, row) <- enumerate board]
    where enumerate = zip [0..]

allCoords :: WorkingBoard -> [Coord]
allCoords board =
  concat $ map2D (\(_, c) -> c) $ allCoordPairs board

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

resolveTile :: WorkingBoard -> Coord -> Tile
resolveTile board coord =
  case (getCoord board coord) of
    Resolved x -> Resolved x
    Unresolved s -> fromSet $ refine s 
  where surroundingRow = getSurroundingDirections board [left, right] coord
        surroundingCol = getSurroundingDirections board [up, down] coord
        surroundingGrid = getSubgrid              board 3 3 coord
        refine = foldl (.) id [noDuplicatesRule surroundingRow, noDuplicatesRule surroundingCol, noDuplicatesRule surroundingGrid]

noDuplicatesRule :: [Tile] -> Set Int -> Set Int
noDuplicatesRule surroundings possibilities =
  possibilities `difference` resolvedSurroundings
    where resolvedSurroundings = fromList $ map fromResolved $ filter isResolved surroundings

fromSet :: Set Int -> Tile
fromSet s
    | len == 1  = Resolved (elemAt 0 s)
    | len > 1   = Unresolved s
    | otherwise = error "impossible"
  where len = size s

resolve :: WorkingBoard -> WorkingBoard
resolve board = map2D ((resolveTile board) . onlyCoord) $ allCoordPairs board
  where onlyCoord (_, coord) = coord

isResolved :: Tile -> Bool
isResolved (Resolved _) = True
isResolved (Unresolved _) = False

fromResolved :: Tile -> Int
fromResolved (Resolved x) = x
fromResolved (Unresolved _) = error "called fromResolved on an Unresolved"

resolveCompletely :: WorkingBoard -> WorkingBoard
resolveCompletely board = 
  let step = resolve board in
      if step == board then
        error ("no progress" ++ show step)
        else
      if all isResolved (concat step) then step else resolveCompletely step 

solve :: WorkingBoard -> [[Int]]
solve = map2D fromResolved . resolveCompletely
