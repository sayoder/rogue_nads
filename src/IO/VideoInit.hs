module IO.VideoInit where

--External libraries
import qualified Graphics.UI.SDL as SDL

--Local modules
import qualified IO.Color as Color
import qualified IO.CP437Display as CP437
import qualified IO.KeyInput as KeyInput
import qualified IO.Render as Render

--Eventually these should probably not be hard-coded.
width = 800
height = 600

--SDL initialization boilerplate
init = SDL.withInit [SDL.InitVideo] videoSetup

--SDL initialization boilerplate
videoSetup = do screen <- SDL.setVideoMode width height 24 [SDL.SWSurface]
                SDL.setCaption "Test" ""
                SDL.enableUnicode True
                Render.display Render.image (0,0)
                KeyInput.waitForKeyPress KeyInput.initialState

