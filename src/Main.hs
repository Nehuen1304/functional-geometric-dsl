module Main where
import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Display
import Graphics.UI.GLUT.Begin
import Dibujo
import Interp
import qualified Graphics.Gloss.Data.Point.Arithmetic as V
import Basica.Escher as B
import Data.Bool (Bool(True))
import Graphics.Gloss (display, Display (InWindow))
import qualified Basica.Comun as B

-- Defino Display de gloss
win :: Display
win = InWindow "Escher" (800,800) (100,100)

-- Defino Color de fondo
colorini :: Color
colorini = white

-- Defino la Imagen a mostrar
picture :: Picture
picture = interp B.interpBas (B.escher 3 B.FishHDPLUS) (0,0) (800,0) (0,800)

-- defino main para mostrar la figura, la defino como display que toma Display, un background color y un Picture
main :: IO ()
main = display win colorini picture


