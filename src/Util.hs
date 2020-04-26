module Util where

map2D :: (a -> b) -> [[a]] -> [[b]]
map2D f ys = [[f x | x <- y] | y <- ys]

