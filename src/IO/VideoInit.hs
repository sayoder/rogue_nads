module IO.VideoInit where

--External libraries
import qualified Graphics.UI.SDL as SDL

--Local modules
import qualified IO.Color as Color
import qualified IO.CP437Display as CP437
import qualified IO.KeyInput as KeyInput
import qualified IO.Render as Render

width = 800
height = 600

init = SDL.withInit [SDL.InitVideo] videoSetup

videoSetup = do screen <- SDL.setVideoMode width height 24 [SDL.SWSurface]
                SDL.setCaption "Test" ""
                SDL.enableUnicode True
                Render.display Render.image
                KeyInput.waitForKeyPress KeyInput.initialState

