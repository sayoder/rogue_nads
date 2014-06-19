module CP137Display where

data Point = Point Int Int deriving (Show, Eq)
data Rect = Rect Point Point deriving (Show, Eq)

getX :: Point -> Int
getX (Point x _) = x

getY :: Point -> Int
getY (Point _ y) = y

--Given coordinates (x, y) of a tile on data/default.bmp. Rows and columns
--start at 0, so the first smiley face is (0,1).
--Returns a Rect defining the pixel location of the desired snippet.
coordsToRect :: Point -> Rect
coordsToRect (Point x y) = Rect (Point tlx tly) (Point brx bry)
    where tlx = x * 8
          tly = y * 12
          brx = (x + 1) * 8
          bry = (y + 1) * 12
