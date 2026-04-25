--Tarea4
--Fabrizio Pimentel Casillas
--Arvin Isaac Marin Gallegos

{-# LANGUAGE LinearTypes #-}

import Unsafe.Coerce (unsafeCoerce)

sumLin :: Int %1 -> Int -> Int
sumLin = unsafeCoerce (+)

prestarPrimero :: (a , b) %1 -> (a , a %1 -> (a , b))
prestarPrimero (x , y) = (x, \a -> (a, y))

prestarAmbos :: (a , b ) %1 -> (a , b , a %1 -> b %1 -> (a , b ) )
prestarAmbos (x , y ) = (x,y, \a b ->(a,b))

mutarTupla :: (Int , Int) %1 -> (Int , Int)
mutarTupla tupla =
  case prestarPrimero tupla of
    (x , devolucion) ->
      devolucion (sumLin x 10)

mutarAmbos :: ( Int , Int ) %1 -> ( Int , Int )
mutarAmbos tupla =
  case prestarAmbos tupla of
    (x, y, devolucion) ->
      devolucion (sumLin x 1) (sumLin y 2)