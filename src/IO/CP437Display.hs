module IO.CP437Display where

--Our graphics set is based on Code Page 437:
--http://en.wikipedia.org/wiki/Code_page_437

import Misc.Point
import qualified Graphics.UI.SDL as SDL

--Given coordinates (x, y) of a tile on data/default.bmp. Rows and columns
--start at 0, so the first smiley face is (0,1).
--Returns a Rect defining the pixel location of the desired snippet.
coordsToRect :: Point -> Maybe SDL.Rect
coordsToRect (x,y) = Just $ SDL.Rect tlx tly 8 12
    where tlx = x * 8
          tly = y * 12
          brx = (x + 1) * 8
          bry = (y + 1) * 12

smiley = coordsToRect (1,0)
