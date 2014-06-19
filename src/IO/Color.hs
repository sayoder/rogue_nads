module IO.Color where
import Data.Word (Word8)
import qualified Graphics.UI.SDL as SDL

type Red = Word8
type Green = Word8
type Blue = Word8
data Color = Color Red Green Blue deriving(Show, Eq)

white = Color 0xFF 0xFF 0xFF
red   = Color 0xFF 0    0
green = Color 0    0xFF 0
blue  = Color 0    0    0xFF
black = Color 0    0    0

toPixel :: Color -> IO SDL.PixelFormat -> IO SDL.Pixel
toPixel (Color r g b) format = format >>= (\f -> SDL.mapRGB f r g b)
