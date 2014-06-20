module IO.KeyInput where

--Standard libraries
import System.Exit (exitSuccess)
--Avoid conflicts with the Direction data type.
import Prelude hiding (Left,Right)

import qualified Graphics.UI.SDL as SDL

import qualified Character.Player as P
import qualified Character.Actor as A
import qualified GameState.GameState as GS
import qualified IO.Render as Render
import Misc.Point

data Direction = Up | Left | Down | Right

initialState :: GS.State
initialState = GS.State $ P.Player (40,40)

waitForKeyPress :: GS.State -> IO ()
waitForKeyPress st = do
       event <- SDL.waitEvent
       case event of
            SDL.Quit -> exitSuccess
            SDL.KeyDown (SDL.Keysym SDL.SDLK_q     _  _ ) -> exitSuccess
            SDL.KeyDown (SDL.Keysym SDL.SDLK_DOWN  _  _ ) -> movePlayer Down st
            SDL.KeyDown (SDL.Keysym SDL.SDLK_UP    _  _ ) -> movePlayer Up st
            SDL.KeyDown (SDL.Keysym SDL.SDLK_LEFT  _  _ ) -> movePlayer Left st
            SDL.KeyDown (SDL.Keysym SDL.SDLK_RIGHT _  _ ) -> movePlayer Right st
            SDL.KeyDown (SDL.Keysym _              _ ' ') -> Render.display Render.image
            _  -> return ()
       waitForKeyPress st


movePlayer :: Direction -> GS.State -> IO ()
movePlayer Down st = movePlayer' st (0,1)
movePlayer Left st = movePlayer' st (-1,0)
movePlayer Right st = movePlayer' st (1,0)
movePlayer Up st = movePlayer' st (0,-1)

movePlayer' :: GS.State -> Point -> IO ()
movePlayer' st tuple = do
                let newst = mvmtStateChange st tuple
                print (A.getX $ GS.player st, A.getY $ GS.player st)
                waitForKeyPress newst

mvmtStateChange :: GS.State -> Point -> GS.State
mvmtStateChange st dir = st { GS.player = mpHelper dir (GS.player st) }
        where mpHelper (x,y) p = p { P.loc = (A.getX p + x, A.getY p + y) }
