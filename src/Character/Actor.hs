module Character.Actor where
import Misc.Point

class Actor a where
    getLocation :: a -> Point
    getX :: a -> Int
    getY :: a -> Int
