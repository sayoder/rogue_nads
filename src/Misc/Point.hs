module Misc.Point where

type Point = (Int, Int)

getPtX :: Point -> Int
getPtX (x, _) = x

getPtY :: Point -> Int
getPtY (_, y) = y
