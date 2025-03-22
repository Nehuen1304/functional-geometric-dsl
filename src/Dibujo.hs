module Dibujo where

-- Definir el lenguaje via constructores de tipo
data Dibujo a = Basica a 
              | Rotar (Dibujo a)
              | Espejar (Dibujo a) 
              | Rot45 (Dibujo a)
              | Apilar Float Float (Dibujo a) (Dibujo a)
              | Juntar Float Float (Dibujo a) (Dibujo a)
              | Encimar (Dibujo a) (Dibujo a) 
              deriving(Eq, Show)


-- Composición n-veces de una función con sí misma.
comp :: (a -> a) -> Int -> a -> a
comp f 0 a = a 
comp f n a = comp f (n-1) (f a)
-- Rotaciones de múltiplos de 90.
r180 :: Dibujo a -> Dibujo a
r180 a  = Rotar ( Rotar a ) 

r270 :: Dibujo a -> Dibujo a
r270 a = Rotar (Rotar (Rotar a)) 

-- Pone una figura sobre la otra, ambas ocupan el mismo espacio.
(.-.) :: Dibujo a -> Dibujo a -> Dibujo a
(.-.) = Apilar 1 1

-- Pone una figura al lado de la otra, ambas ocupan el mismo espacio.
(///) :: Dibujo a -> Dibujo a -> Dibujo a
(///) = Juntar 1 1 

-- Superpone una figura con otra.
(^^^) :: Dibujo a -> Dibujo a -> Dibujo a
(^^^) = Encimar 

-- Dadas cuatro dibujos las ubica en los cuatro cuadrantes.
cuarteto :: Dibujo a -> Dibujo a -> Dibujo a -> Dibujo a -> Dibujo a
cuarteto a b c d = (.-.) ((///) a b) ((///) c d)   

-- Una dibujo repetido con las cuatro rotaciones, superpuestas.
encimar4 :: Dibujo a -> Dibujo a
encimar4 a = (^^^) ((^^^) ((^^^) a (Rotar a )) (Rotar (Rotar a))) (Rotar (Rotar (Rotar a)))  

-- Cuadrado con la misma figura rotada i * 90, para i ∈ {0, ..., 3}.
-- No confundir con encimar4!
ciclar :: Dibujo a -> Dibujo a
ciclar a = cuarteto a (Rotar a) (Rotar(Rotar a)) (Rotar(Rotar(Rotar a)))

-- Transfomar un valor de tipo a como una Basica.
pureDib :: a -> Dibujo a
pureDib   = Basica  

-- map para nuestro lenguaje.
mapDib :: (a -> b) -> Dibujo a -> Dibujo b
mapDib f (Basica a) = pureDib (f a)
mapDib f (Rotar a) = Rotar (mapDib f a)
mapDib f (Espejar a) = Espejar (mapDib f a)
mapDib f (Rot45 a) = Rot45 (mapDib f a)
mapDib f (Apilar i j a b) = Apilar i j (mapDib f a) (mapDib f b)
mapDib f (Juntar i j a b) = Juntar i j (mapDib f a) (mapDib f b)
mapDib f (Encimar a b) = Encimar (mapDib f a) (mapDib f b)


-- Funcion de fold para Dibujos a
foldDib :: (a -> b) -> (b -> b) -> (b -> b) -> (b -> b) ->
       (Float -> Float -> b -> b -> b) ->
       (Float -> Float -> b -> b -> b) ->
       (b -> b -> b) ->
       Dibujo a -> b 
foldDib fBasica _ _ _ _ _ _ (Basica a) = fBasica a
foldDib fBasica fRotar fEspejar fRot45 fApilar fJuntar fEncimar (Rotar a) = fRotar (foldDib fBasica fRotar fEspejar fRot45 fApilar fJuntar fEncimar a)
foldDib fBasica fRotar fEspejar fRot45 fApilar fJuntar fEncimar (Espejar a) = fEspejar (foldDib fBasica fRotar fEspejar fRot45 fApilar fJuntar fEncimar a)
foldDib fBasica fRotar fEspejar fRot45 fApilar fJuntar fEncimar (Rot45 a) = fRot45 (foldDib fBasica fRotar fEspejar fRot45 fApilar fJuntar fEncimar a)
foldDib fBasica fRotar fEspejar fRot45 fApilar fJuntar fEncimar (Apilar i j a b) = fApilar i j (foldDib fBasica fRotar fEspejar fRot45 fApilar fJuntar fEncimar a) (foldDib fBasica fRotar fEspejar fRot45 fApilar fJuntar fEncimar b)
foldDib fBasica fRotar fEspejar fRot45 fApilar fJuntar fEncimar (Juntar i j a b) = fJuntar i j (foldDib fBasica fRotar fEspejar fRot45 fApilar fJuntar fEncimar a) (foldDib fBasica fRotar fEspejar fRot45 fApilar fJuntar fEncimar b)
foldDib fBasica fRotar fEspejar fRot45 fApilar fJuntar fEncimar (Encimar a b) = fEncimar (foldDib fBasica fRotar fEspejar fRot45 fApilar fJuntar fEncimar a) (foldDib fBasica fRotar fEspejar fRot45 fApilar fJuntar fEncimar b)
