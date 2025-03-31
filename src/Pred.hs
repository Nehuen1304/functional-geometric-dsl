module Pred where

import Dibujo

type Pred a = a -> Bool




-- Para la definiciones de la funciones de este modulo, no pueden utilizar
-- pattern-matching, sino alto orden a traves de la funcion foldDib, mapDib

-- Dado un predicado sobre básicas, cambiar todas las que satisfacen
-- el predicado por el resultado de llamar a la función indicada por el
-- segundo argumento con dicha figura.
-- Por ejemplo, `cambiar (== Triangulo) (\x -> Rotar (Basica x))` rota
-- todos los triángulos.

cambiar :: Pred a -> (a -> Dibujo a) -> Dibujo a -> Dibujo a
cambiar p f = mapDib (\x -> if p x then f x else Basica x)

-- Alguna básica satisface el predicado.
anyDib :: Pred a -> Dibujo a -> Bool
anyDib p = foldDib p 
                id 
                id 
                id 
                (\_ _ x y -> x || y) 
                (\_ _ x y -> x || y) 
                (||)

-- Todas las básicas satisfacen el predicado.
allDib :: Pred a -> Dibujo a -> Bool
allDib p = foldDib p 
                   id 
                   id 
                   id 
                   (\_ _ x y -> x && y) 
                   (\_ _ x y -> x && y) 
                   (&&)

-- Hay 4 rotaciones seguidas.
esRot360 :: Dibujo a -> Bool
esRot360 = foldDib (const False) 
                 (\r -> r) 
                 (const False) 
                 (const False) 
                 (\_ _ _ _ -> False) 
                 (\_ _ _ _ -> False) 
                 (\_ _ -> False)

-- Hay 2 espejados seguidos.
esFlip2 :: Dibujo a -> Bool
esFlip2 = foldDib (const False) 
                (const False) 
                (\e -> e) 
                (const False) 
                (\_ _ _ _ -> False) 
                (\_ _ _ _ -> False) 
                (\_ _ -> False)

data Superfluo = RotacionSuperflua | FlipSuperfluo
  deriving (Show, Eq)


-- Chequea si el dibujo tiene una rotacion superflua
errorRotacion :: Dibujo a -> [Superfluo]
errorRotacion dib = if esRot360 dib then [RotacionSuperflua] else []

-- Chequea si el dibujo tiene un flip superfluo
errorFlip :: Dibujo a -> [Superfluo]
errorFlip dib = if esFlip2 dib then [FlipSuperfluo] else []

-- Aplica todos los chequeos y acumula todos los errores, y
-- sólo devuelve la figura si no hubo ningún error.
checkSuperfluo :: Dibujo a -> Either [Superfluo] (Dibujo a)
checkSuperfluo dib = 
  let errores = errorRotacion dib ++ errorFlip dib
  in if null errores 
     then Right dib
     else Left errores
