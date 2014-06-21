module IO.Render where

import qualified Graphics.UI.SDL as SDL
import qualified IO.Color as Color
import qualified IO.CP437Display as CP437
import Misc.Point

import qualified Paths_roguenads as Paths

tileSize :: Point
tileSize = (8,12)

imgPath :: IO FilePath
imgPath = do let str = "data/default.bmp"
             Paths.getDataFileName "data/default.bmp"

--The only image we should ever need to load.
image :: IO SDL.Surface
image = imgPath >>= SDL.loadBMP

--Given a point representing a location in tile coordinates, will return an
--SDL.Rect representing the area to blit onto. We have a set tile size (8 x 12
--pixels, for now), so all we need is tile coordinates to figure this out.
destRect :: Point -> SDL.Rect
destRect p = SDL.Rect (fst p * fst tileSize) (snd p * snd tileSize) (fst tileSize) (snd tileSize)

--Given a Surface and tile coordinates, will display the surface at the given
--tile coordinates. Eventually, this will just be given the relevant portion of
--the game state dictionary, and it will blit all of the tiles in an area.
display :: IO SDL.Surface -> Point ->  IO ()
display image coords = do
    screen <- SDL.getVideoSurface
    let format = SDL.surfaceGetPixelFormat screen
    px <- Color.toPixel Color.black (return format)
    SDL.fillRect screen Nothing px
    --The image slice that we want to display.
    let slice = CP437.atSymbol
    let dest = destRect coords
    img <- image
    SDL.blitSurface img slice screen $ Just dest
    SDL.flip screen
