module RARisimo
    ( frecuencias,
      ganadores,
      hoffman,
      rarisimo
    ) where

import Hoffman
import FrecuenciaV2
import qualified Data.Map as Map
import Data.List (nub, sortOn)

frecuencias :: String -> [Frecuencia Hoffman]
frecuencias str = map (\c -> Frecuencia (nuevoHoffman c) (count c str)) (nub str)
  where
    -- Cuenta cuántas veces aparece un carácter en una lista
    count x = length . filter (== x)


ganadores :: [Frecuencia a] -> Maybe (Frecuencia a, Frecuencia a, [Frecuencia a])
ganadores [] = Nothing
ganadores [_] = Nothing
ganadores xs = Just (x, y, resto)
  where
    sorted = sortOn frecuencia xs
    x = head sorted
    y = sorted !! 1
    resto = drop 2 sorted


hoffman :: String -> Maybe Hoffman
hoffman "" = Nothing
hoffman str = Just $ construirHoffman (frecuencias str)
  where
    construirHoffman :: [Frecuencia Hoffman] -> Hoffman
    construirHoffman [x] = valor x
    construirHoffman xs = construirHoffman (fusionarLosMenores xs)

    fusionarLosMenores :: [Frecuencia Hoffman] -> [Frecuencia Hoffman]
    fusionarLosMenores (x:y:xs) = (Frecuencia (fusionHoffman (valor x) (valor y)) (frecuencia x + frecuencia y)) : xs
    fusionarLosMenores _ = error "La lista no tiene suficientes elementos para fusionar."


rarisimo :: String -> Map.Map Char String
rarisimo str = codificacion (fromJust (hoffman str))
  where
    fromJust (Just x) = x
    fromJust Nothing = error "No se pudo construir el árbol de Hoffman"