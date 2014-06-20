module GameState.GameState where
import Character.Player

data State = State {
                     player :: Player
                   }
