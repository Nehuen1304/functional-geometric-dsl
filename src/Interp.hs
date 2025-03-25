module Interp where

import Dibujo
import Graphics.Gloss
import qualified Graphics.Gloss.Data.Point.Arithmetic as V
import Graphics.Gloss.Data.Vector ()

-- Gloss provee el tipo Vector y Picture.
type ImagenFlotante = Vector -> Vector -> Vector -> Picture

type Interpretacion a = a -> ImagenFlotante

mitad :: Vector -> Vector
mitad = (0.5 V.*)

invertir :: Vector -> Vector
invertir = ((-1) V.*)

-- Interpretaciones de los constructores de Dibujo

-- interpreta el operador de rotacion
interp_rotar :: ImagenFlotante -> ImagenFlotante
interp_rotar imagen origen ancho alto = imagen (origen V.+ ancho) alto (invertir ancho)

-- interpreta el operador de espejar
interp_espejar :: ImagenFlotante -> ImagenFlotante
interp_espejar imagen origen ancho alto = imagen (origen V.+ ancho) (invertir ancho) alto

-- interpreta el operador de rotacion 45
interp_rotar45 :: ImagenFlotante -> ImagenFlotante
interp_rotar45 imagen origen ancho alto = imagen nOrig nAncho nAlto
  where
    nOrig = origen V.+ mitad (ancho V.+ alto)
    nAncho = mitad (ancho V.+ alto)
    nAlto = mitad (alto V.- alto)

-- interpreta el operador de apilar
interp_apilar :: Int -> Int -> ImagenFlotante -> ImagenFlotante -> ImagenFlotante
interp_apilar = undefined

-- interpreta el operador de juntar
interp_juntar :: Int -> Int -> ImagenFlotante -> ImagenFlotante -> ImagenFlotante
interp_juntar = undefined

-- interpreta el operador de encimar
interp_encimar :: ImagenFlotante -> ImagenFlotante -> ImagenFlotante
interp_encimar = undefined

-- interpreta cualquier expresion del tipo Dibujo a
-- utilizar foldDib
interp :: Interpretacion a -> Dibujo a -> ImagenFlotante
interp = undefined
