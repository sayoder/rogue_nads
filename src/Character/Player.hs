module Character.Player where
import Character.Actor
import Misc.Point

data Player = Player {
                       loc :: Point
                     }

instance Actor Player where
    getLocation = loc
    getX = fst . loc
    getY = snd . loc
