module RARisimo
    ( frecuencias,
      ganadores,
      hoffman,
      rarisimo
    ) where

import Hoffman
import FrecuenciaV2
import qualified Data.Map as Map
import Data.List (nub, sortOn, insertBy)
import Data.Ord (comparing)

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


-- Función que construye el árbol de Huffman
hoffman :: String -> Maybe Hoffman
hoffman "" = Nothing
hoffman str = Just $ construirHoffman (sortByFrecuencia (frecuencias str))
  where
    -- Construcción del árbol de Huffman
    construirHoffman :: [Frecuencia Hoffman] -> Hoffman
    construirHoffman [x] = valor x  -- Solo queda un árbol
    construirHoffman xs = construirHoffman (fusionarLosMenores xs)

    -- Ordena la lista de frecuencias por la frecuencia de menor a mayor
    sortByFrecuencia :: [Frecuencia Hoffman] -> [Frecuencia Hoffman]
    sortByFrecuencia = sortOn frecuencia

    -- Fusión de los árboles con menor frecuencia
    fusionarLosMenores :: [Frecuencia Hoffman] -> [Frecuencia Hoffman]
    fusionarLosMenores (x:y:xs) = 
        let nuevoArbol = fusionHoffman (valor y) (valor x)  -- Fusionamos los árboles
            nuevaFrecuencia = Frecuencia nuevoArbol (frecuencia x + frecuencia y)
        in insertBy (comparing frecuencia) nuevaFrecuencia xs  -- Insertamos en el lugar adecuado
    fusionarLosMenores _ = error "La lista no tiene suficientes elementos para fusionar."


rarisimo :: String -> Map.Map Char String
rarisimo str = codificacion (fromJust (hoffman str))
  where
    fromJust (Just x) = x
    fromJust Nothing = error "No se pudo construir el árbol de Hoffman"