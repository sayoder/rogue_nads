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

--Gives the initial state of the program. Eventually, this should go somewhere
--else.
initialState :: GS.State
initialState = GS.State $ P.Player (40,40)

--Refresh the screen, then wait for an SDL.KeyDown event, passing off to
--movePlayer in the event that a direction key is pressed. If no key press
--occurs, the function just calls itself again, making this the main game loop.
waitForKeyPress :: GS.State -> IO ()
waitForKeyPress st = do
       refresh st
       event <- SDL.waitEvent
       case event of
            SDL.Quit -> exitSuccess
            SDL.KeyDown (SDL.Keysym SDL.SDLK_q     _  _ ) -> exitSuccess
            SDL.KeyDown (SDL.Keysym SDL.SDLK_DOWN  _  _ ) -> movePlayer Down st
            SDL.KeyDown (SDL.Keysym SDL.SDLK_UP    _  _ ) -> movePlayer Up st
            SDL.KeyDown (SDL.Keysym SDL.SDLK_LEFT  _  _ ) -> movePlayer Left st
            SDL.KeyDown (SDL.Keysym SDL.SDLK_RIGHT _  _ ) -> movePlayer Right st
            _  -> return ()
       waitForKeyPress st

--Refresh the screen, given the game state.
refresh :: GS.State -> IO ()
refresh st = Render.display Render.image $ playerCoords st

--Given the state dictionary, returns player coordinates.
--TODO maybe put elsewhere
playerCoords :: GS.State -> Point
playerCoords st = A.getLocation $ GS.player st

-- The main entry point for player movement.
movePlayer :: Direction -> GS.State -> IO ()
movePlayer Down st = movePlayer' st (0,1)
movePlayer Left st = movePlayer' st (-1,0)
movePlayer Right st = movePlayer' st (1,0)
movePlayer Up st = movePlayer' st (0,-1)

--A helper function that obtains an updated state and passes it to
--waitForKeyPress.
movePlayer' :: GS.State -> Point -> IO ()
movePlayer' st tuple = do
                let newst = setPlayerCoords st tuple
                waitForKeyPress newst

--Updates the player's coordinate data in the global game state dictionary. The
--syntax used here is a little weird. It is record update syntax, a way to
--update only one field of data type. For example, if
--State = { playerState :: Player, foo :: SomethingElse}, this would translate to:
--
--data State = State Player SomethingElse
--
--playerState :: State -> Player
--playerState (State p _ ) = p
--
--Similarly, the function:
--      setPlayerCoords st (x,y) = st { GS.player = someNewPlayerState (GS.player st) (x,y) }
--translates to:
--      setPlayerCoords (State player foo ) (x,y) = State (someNewPlayerState player (x,y)) foo
--
--
--The benefits are not very visible when the state dictionary is this small, but
--as the state dictionary becomes large, it will be extremely helpful to
--avoid having pattern matching like: setPlayerCoords (State a b c d e f g h i j) (x,y)
setPlayerCoords :: GS.State -> Point -> GS.State
setPlayerCoords st (x,y) = st { GS.player = mpHelper (GS.player st) }
        where mpHelper p = p { P.loc = (A.getX p + x, A.getY p + y) }
