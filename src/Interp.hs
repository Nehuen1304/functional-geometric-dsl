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

maximo_vector :: Vector -> Vector -> Vector
maximo_vector (x,y) (a,b) = (max x a, max y b)

minimo_vector :: Vector -> Vector -> Vector
minimo_vector (x,y) (a,b) = (min x a, min y b)
-- Interpretaciones de los constructores de Dibujo

-- interpreta el operador de rotacion
interp_rotar :: ImagenFlotante -> ImagenFlotante
interp_rotar imagen origen ancho alto = imagen (origen V.+ ancho) alto (opuesto ancho)

-- interpreta el operador de espejar
interp_espejar :: ImagenFlotante -> ImagenFlotante
interp_espejar imagen origen ancho alto = imagen (origen V.+ ancho) (opuesto ancho) alto

-- interpreta el operador de rotacion 45
interp_rotar45 :: ImagenFlotante -> ImagenFlotante
interp_rotar45 imagen origen ancho alto = imagen nOrig nAncho nAlto
  where
    nOrig = origen V.+ mitad (ancho V.+ alto)
    nAncho = mitad (ancho V.+ alto)
    nAlto = mitad (alto V.- ancho)

interp_apilar :: Float -> Float -> ImagenFlotante -> ImagenFlotante -> ImagenFlotante
interp_apilar m n img1 img2 origen ancho alto = 
    Pictures [img1 (origen V.+ h') ancho (r V.* alto),
              img2 origen ancho h']
    where
        r' = n / (m + n)
        r = m / (m + n)
        h' = r' V.* alto


-- interpreta el operador de juntar
interp_juntar :: Float -> Float -> ImagenFlotante -> ImagenFlotante -> ImagenFlotante
interp_juntar m n img1 img2 origen ancho alto =
    Pictures [img1 origen w' alto,
              img2 (origen V.+ w') (r' V.* ancho) alto ]
    where
        r' = n / (m + n)
        r = m / (m + n)
        w' = r V.* ancho  



-- interpreta el operador de encimar
interp_encimar :: ImagenFlotante -> ImagenFlotante -> ImagenFlotante
interp_encimar img1 img2 origen ancho alto =
  Pictures [img1 origen ancho alto, 
            img2 origen ancho alto]


-- interpreta cualquier expresion del tipo Dibujo a
-- utilizar foldDib
interp :: Interpretacion a -> Dibujo a -> ImagenFlotante
interp inter  = foldDib inter interp_rotar interp_espejar interp_rotar45 interp_apilar interp_juntar interp_encimar 
