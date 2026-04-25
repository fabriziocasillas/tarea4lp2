--Tarea4
--Fabrizio Pimentel Casillas
--Arvin Isaac Marin Gallegos

{-# LANGUAGE LinearTypes #-}

import Unsafe.Coerce (unsafeCoerce)

sumLin :: Int %1 -> Int -> Int
sumLin = unsafeCoerce (+)

prestarPrimero :: (a , b) %1 -> (a , a %1 -> (a , b))
prestarPrimero (x , y) = (x, \x' -> (x', y))

mutarTupla :: (Int , Int) %1 -> (Int , Int)
mutarTupla tupla =
  case prestarPrimero tupla of
    (x , devolucion) ->
      devolucion (sumLin x 10)