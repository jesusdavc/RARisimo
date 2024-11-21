-- Definición del tipo de datos Hoffman
data Hoffman = Hoja Char
             | Nodo Hoffman Hoffman
             deriving (Show, Eq)

-- Función para crear un árbol Hoffman con una sola hoja
nuevoHoffman :: Char -> Hoffman
nuevoHoffman = Hoja

-- Función para combinar dos árboles Hoffman
fusionHoffman :: Hoffman -> Hoffman -> Hoffman
fusionHoffman = Nodo

