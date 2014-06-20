module IO.Render where

import qualified Graphics.UI.SDL as SDL
import qualified IO.Color as Color
import qualified IO.CP437Display as CP437

import qualified Paths_roguenads as Paths

imgPath :: IO FilePath
imgPath = do let str = "data/default.bmp"
             Paths.getDataFileName "data/default.bmp"

--The only image we should ever need to load.
image :: IO SDL.Surface
image = imgPath >>= SDL.loadBMP


display :: IO SDL.Surface -> IO ()
display image = do screen <- SDL.getVideoSurface
                   let format = SDL.surfaceGetPixelFormat screen
                   px <- Color.toPixel Color.black (return format)
                   SDL.fillRect screen Nothing px
                   --The image slice that we want to display.
                   let slice = CP437.atSymbol
                   img <- image
                   SDL.blitSurface img slice screen $ Just $ SDL.Rect 0 0 0 0
                   SDL.flip screen
