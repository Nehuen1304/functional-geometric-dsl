module Main where

import Basica.Escher as B
import Graphics.Gloss
import Interp

-- Defino Display de gloss
win :: Display
win = InWindow "Escher" (800, 800) (100, 100)

-- Defino Color de fondo
colorini :: Color
colorini = white

-- Defino la Imagen a mostrar
picture :: Picture
picture = interp B.interpBas (B.escher 3 B.Triangulo) (0, 0) (800, 0) (0, 800)

-- defino main para mostrar la figura, la defino como display que toma Display, un background color y un Picture
main :: IO ()
main = display win colorini picture
