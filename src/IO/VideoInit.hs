module IO.VideoInit where

--Standard libraries
import qualified Control.Monad as Monad
import System.Exit (exitSuccess)

--External libraries
import qualified Graphics.UI.SDL as SDL

--Local modules
import qualified IO.Color as Color

import qualified Paths_roguenads as Paths

width = 800
height = 600

init = SDL.withInit [SDL.InitVideo] videoSetup

imgPath :: IO FilePath
imgPath = do let str = "data/default.bmp"
             Paths.getDataFileName "data/default.bmp"

videoSetup = do screen <- SDL.setVideoMode width height 24 [SDL.SWSurface]
                SDL.setCaption "Test" ""
                SDL.enableUnicode True
                path <- imgPath
                image <- SDL.loadBMP path
                display image
                loop (display image)

display :: SDL.Surface -> IO ()
display image = do screen <- SDL.getVideoSurface
                   let format = SDL.surfaceGetPixelFormat screen
                   px <- Color.toPixel Color.black (return format)
                   SDL.fillRect screen Nothing px
                   SDL.blitSurface image Nothing screen (Just (SDL.Rect 0 0 0 0))
                   SDL.flip screen

loop :: IO () -> IO ()
loop display = do event <- SDL.waitEvent
                  case event of
                       SDL.Quit -> exitSuccess
                       SDL.KeyDown (SDL.Keysym _ _ 'q') -> exitSuccess
                       SDL.KeyDown (SDL.Keysym _ _ ' ') -> display
                       _  -> return ()
                  loop display
