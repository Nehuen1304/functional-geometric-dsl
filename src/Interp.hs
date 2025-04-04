{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use camelCase" #-}
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

opuesto :: Vector -> Vector
opuesto = ((-1) V.*)

-- Interpretaciones de los constructores de Dibujo

-- interpreta el operador de rotacion
interp_rotar :: ImagenFlotante -> ImagenFlotante
interp_rotar imagen origen ancho alto = imagen (origen V.+ ancho) alto (opuesto ancho)

-- interpreta el operador de espejar
interp_espejar :: ImagenFlotante -> ImagenFlotante
interp_espejar imagen origen ancho = imagen (origen V.+ ancho) (opuesto ancho)

-- interpreta el operador de rotacion 45
interp_rotar45 :: ImagenFlotante -> ImagenFlotante
interp_rotar45 imagen origen ancho alto = imagen nOrig nAncho nAlto
  where
    nOrig = origen V.+ mitad (ancho V.+ alto)
    nAncho = mitad (ancho V.+ alto)
    nAlto = mitad (alto V.- alto)

-- interpreta el operador de apilar
interp_apilar :: Float -> Float -> ImagenFlotante -> ImagenFlotante -> ImagenFlotante
interp_apilar m n imagen1 imagen2 origen ancho alto = Pictures [imagen1 nOrigen1 ancho nAlto1, imagen2 origen ancho nAlto2]
  where
    ratio1 = m / (n + m)
    ratio2 = n / (n + m)
    nOrigen1 = origen V.+ nAlto2
    nAlto1 = ratio1 V.* alto
    nAlto2 = ratio2 V.* alto

-- interpreta el operador de juntar
interp_juntar :: Float -> Float -> ImagenFlotante -> ImagenFlotante -> ImagenFlotante
interp_juntar m n imagen1 imagen2 origen ancho alto = Pictures [imagen1 origen nAncho1 alto, imagen2 nOrigen2 nAncho2 alto]
  where
    ratio1 = m / (n + m)
    ratio2 = n / (n + m)
    nAncho1 = ratio1 V.* ancho
    nOrigen2 = origen V.+ nAncho1
    nAncho2 = ratio2 V.* ancho

-- interpreta el operador de encimar
interp_encimar :: ImagenFlotante -> ImagenFlotante -> ImagenFlotante
interp_encimar imagen1 imagen2 origen ancho alto = Pictures [imagen1 origen ancho alto, imagen2 origen ancho alto]

-- interpreta cualquier expresion del tipo Dibujo a
-- utilizar foldDib
interp :: Interpretacion a -> Dibujo a -> ImagenFlotante
interp f =
  foldDib f interp_rotar interp_espejar interp_rotar45 interp_apilar interp_juntar interp_encimar
