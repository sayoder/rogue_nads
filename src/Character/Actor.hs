module Character.Actor where
import Misc.Point

--Not sure if this typeclass is totally necessary yet.
class Actor a where
    getLocation :: a -> Point
    getX :: a -> Int
    getY :: a -> Int
