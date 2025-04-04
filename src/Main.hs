module Main where
import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Display
import Graphics.UI.GLUT.Begin
import Dibujo
import Interp
import qualified Basica.Ejemplo as E
import qualified Graphics.Gloss.Data.Point.Arithmetic as V
import Basica.Escher
import Basica.BasicaDoble as B

-- Defino Display de gloss
win :: Display
win = InWindow "Doble" (800,800) (100,100)

-- Defino Color de fondo
colorini :: Color
colorini = aquamarine

-- Defino la Imagen a mostrar
picture :: Picture
picture = interp B.interpBas B.ejemplo (0,0) (800,0) (0,800)

-- defino main para mostrar la figura, la defino como display que toma Display, un background color y un Picture
main :: IO ()
main = display win colorini picture

