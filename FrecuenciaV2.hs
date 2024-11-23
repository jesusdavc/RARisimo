module FrecuenciaPlus 
  ( Frecuencia,
    iniciarFrecuencia,
    contar,
    valor,
    frecuencia
  ) where

-- Definición del tipo de datos Frecuencia
-- Este tipo contiene un valor (de cualquier tipo comparable por igualdad)
-- y un contador que almacena la cantidad de veces que aparece
data Frecuencia a = Frecuencia a Int
  deriving (Eq) -- La clase Eq permite verificar igualdad entre dos Frecuencias

-- Constructores
-- Función para iniciar una Frecuencia
-- Crea una Frecuencia con un contador inicial de 1
iniciarFrecuencia :: Eq a => a -> Frecuencia a
iniciarFrecuencia v = Frecuencia v 1

-- Función para contar las ocurrencias de un valor en una lista
contar :: Eq a => a -> [a] -> Frecuencia a
contar v xs = Frecuencia v (length (filter (== v) xs))

-- Función para obtener el valor de una Frecuencia
valor :: Frecuencia a -> a
valor (Frecuencia v _) = v

-- Función para obtener el contador de una Frecuencia
frecuencia :: Frecuencia a -> Int
frecuencia (Frecuencia _ f) = f



-- Instancia de la clase Show
-- Permite mostrar una Frecuencia como una cadena
instance Show a => Show (Frecuencia a) where
  show (Frecuencia v f) = "Frecuencia {valor = " ++ show v ++ ", contador = " ++ show f ++ "}"

-- Instancia de la clase Ord
-- Permite ordenar las Frecuencias según el contador (f)
instance Eq a => Ord (Frecuencia a) where
  (Frecuencia _ f1) <= (Frecuencia _ f2) = f1 <= f2