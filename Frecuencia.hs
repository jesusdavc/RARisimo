-- Se especifican los elementos del modulo a exportar
module Frecuencia (Frecuencia, iniciarFrecuencia, contar) where

-- Al definir los constructores de esta forma, las funciones de acceso, valor y frecuencia,
-- quedan definidas implicitamente
data Frecuencia a   = NuevaFrecuencia { valor :: a, frecuencia :: Int }
                    | Conteo { valor :: a, frecuencia :: Int }
                    deriving Eq
--Constructores
iniciarFrecuencia :: Eq a => a -> Frecuencia a
iniciarFrecuencia x = NuevaFrecuencia x 1

contar :: Eq a => a -> [a] -> Frecuencia a
contar x [] = Conteo x 0
contar x y = Conteo x (contarAux 0 x y)
-- Funcion auxiliar para hacer recursion de cola
contarAux :: Eq a => Int -> a -> [a] -> Int
contarAux acc _ [] = acc
contarAux acc x (y:ys) = contarAux (if x == y then acc + 1 else acc) x ys

-- Instancia de la clase Show
instance Show a => Show (Frecuencia a) where
    show (NuevaFrecuencia v f) = "Valor: " ++ show v ++ ", Frecuencia: " ++ show f
    show (Conteo v f) = "Valor: " ++ show v ++ ", Frecuencia: " ++ show f

-- Instancia de la clase Ord basada en el contador
instance Eq a => Ord (Frecuencia a) where
    compare (NuevaFrecuencia _ f1) (NuevaFrecuencia _ f2) = compare f1 f2
    compare (NuevaFrecuencia _ f1) (Conteo _ f2) = compare f1 f2
    compare (Conteo _ f1) (NuevaFrecuencia _ f2) = compare f1 f2
    compare (Conteo _ f1) (Conteo _ f2) = compare f1 f2