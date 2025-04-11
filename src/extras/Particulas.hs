module Main where

import Graphics.Gloss
    ( Picture(Translate, Pictures, Color),
      black,
      white,
      circleSolid,
      play,
      Display(InWindow) )
import Graphics.Gloss.Interface.Pure.Game
    ( Picture(Translate, Pictures, Color),
      black,
      white,
      circleSolid,
      play,
      Display(InWindow),
      Event(EventMotion) )

-- Datos de cada círculo
type Circulo = (Float, Float, Float, Velocity, Acceleration)  -- (x, y, radio, vector velocidad, vector aceleracion)
type Velocity = (Float, Float)
type Acceleration = (Float, Float)

type Estado = ([Circulo], (Float, Float)) -- (circulos, mouse)


-- Crear una grilla de círculos
generaGrilla :: Int -> Int -> Float -> [Circulo]
generaGrilla filas columnas espacio =
  [ (x, y, 5, (0, 0), (0, 0))
  | i <- [0..filas-1]
  , j <- [0..columnas-1]
  , let x = fromIntegral j * espacio - (fromIntegral columnas * espacio / 2)
        y = fromIntegral i * espacio - (fromIntegral filas * espacio / 2)
        
  ]

-- Dibuja un solo círculo, coloreado si es tocado
dibujaCirculo :: Circulo -> Picture
dibujaCirculo (x, y, r, _, _) = Translate x y $ Color black $ circleSolid r

-- Dibuja todo el estado
dibuja :: Estado -> Picture
dibuja (circulos, mousePos) = Pictures $
  [ dibujaCirculo c | c <- circulos ] ++
  [ Color black $ uncurry Translate mousePos $ circleSolid 5 ]

-- Maneja eventos de movimiento del mouse
manejarEvento :: Event -> Estado -> Estado
manejarEvento (EventMotion pos) (cs, _) = (cs, pos)
manejarEvento _ estado = estado

actualizaAccel :: (Float, Float) -> Circulo -> Circulo
actualizaAccel (mx, my) (x, y, r, vel, acc) =
  let dist = sqrt ((mx - x)^2 + (my - y)^2)
      impulso = if dist < 40 then 1 else 0
      dx = (x - mx) / dist
      dy = (y - my) / dist
  in if dist < 40 then (x, y, r, vel, (fst acc + impulso * dx, snd acc + impulso * dy)) else (x, y, r, (fst vel * 0.9, snd vel * 0.9), acc)

actualizaVelocidad :: Circulo -> Circulo
actualizaVelocidad (x, y, r, vel, acc) = (x, y, r, (fst vel + fst acc, snd vel + snd acc), (fst acc * 0.5, snd acc * 0.5))

moverCirculo :: Circulo -> Circulo
moverCirculo (x, y, r, vel, acc) = (x + fst vel, y + snd vel, r, vel, acc)

-- Sin animación temporal
actualiza :: Float -> Estado -> Estado
actualiza _ (circulos, mousePos) =
  let   circulos' = map (actualizaAccel mousePos) circulos
        circulos2 = map actualizaVelocidad circulos'
        circulosMovidos = map (moverCirculo) circulos2
  in (circulosMovidos, mousePos)

  

-- Main
main :: IO ()
main = do
  let circulos = generaGrilla 10 10 30 -- 10x10 grilla, con 30px entre círculos
  play
    (InWindow "particulas" (800, 800) (100, 100))
    white
    60
    (circulos, (0, 0))
    dibuja
    manejarEvento
    actualiza
