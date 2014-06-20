module IO.VideoInit where

--Standard libraries
import Prelude hiding (Left,Right)
import qualified Control.Monad as Monad
import System.Exit (exitSuccess)

--External libraries
import qualified Graphics.UI.SDL as SDL

--Local modules
import qualified IO.Color as Color
import qualified IO.CP437Display as CP437
import qualified Paths_roguenads as Paths
import qualified RogueState.RogueState as RS
import qualified Character.Player as P
import qualified Character.Actor as A
import Misc.Point

data Direction = Up | Left | Down | Right

initialState :: RS.State
initialState = RS.State $ P.Player (40,40)

width = 800
height = 600

init = SDL.withInit [SDL.InitVideo] videoSetup

imgPath :: IO FilePath
imgPath = do let str = "data/default.bmp"
             Paths.getDataFileName "data/default.bmp"

--The only image we should ever need to load.
image :: IO SDL.Surface
image = imgPath >>= SDL.loadBMP

videoSetup = do screen <- SDL.setVideoMode width height 24 [SDL.SWSurface]
                SDL.setCaption "Test" ""
                SDL.enableUnicode True
                display
                loop initialState

display :: IO ()
display = do screen <- SDL.getVideoSurface
             let format = SDL.surfaceGetPixelFormat screen
             px <- Color.toPixel Color.black (return format)
             SDL.fillRect screen Nothing px
             --The image slice that we want to display.
             let slice = CP437.smiley
             img <- image
             SDL.blitSurface img slice screen $ Just $ SDL.Rect 0 0 0 0
             SDL.flip screen

loop :: RS.State -> IO ()
loop = waitForKeyPress

waitForKeyPress :: RS.State -> IO ()
waitForKeyPress st = do event <- SDL.waitEvent
                        case event of
                             SDL.Quit -> exitSuccess
                             SDL.KeyDown (SDL.Keysym SDL.SDLK_q     _       _ ) -> exitSuccess
                             SDL.KeyDown (SDL.Keysym SDL.SDLK_DOWN  _       _ ) -> movePlayer Down st
                             SDL.KeyDown (SDL.Keysym SDL.SDLK_UP    _       _ ) -> movePlayer Up st
                             SDL.KeyDown (SDL.Keysym SDL.SDLK_LEFT  _       _ ) -> movePlayer Left st
                             SDL.KeyDown (SDL.Keysym SDL.SDLK_RIGHT _       _ ) -> movePlayer Right st
                             SDL.KeyDown (SDL.Keysym _              _      ' ') -> display
                             _  -> return ()
                        waitForKeyPress st


movePlayer :: Direction -> RS.State -> IO ()
movePlayer Down st = movePlayer' st (0,1)
movePlayer Left st = movePlayer' st (-1,0)
movePlayer Right st = movePlayer' st (1,0)
movePlayer Up st = movePlayer' st (0,-1)

movePlayer' :: RS.State -> Point -> IO ()
movePlayer' st tuple = do let newst = mvmtStateChange st tuple
                          putStrLn $ show (A.getX $ RS.player st, A.getY $ RS.player st)
                          waitForKeyPress newst

mvmtStateChange :: RS.State -> Point -> RS.State
mvmtStateChange st dir = st { RS.player = mpHelper dir (RS.player st) }
        where mpHelper (x,y) p = p { P.loc = ((A.getX p) + x, (A.getY p) + y) }
