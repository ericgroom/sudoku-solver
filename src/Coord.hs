module Coord where

data Coord = Coord { x :: Int
                   , y :: Int
                   } deriving (Show)

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
