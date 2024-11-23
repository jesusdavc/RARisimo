--Funciones de construcción
-- Definición del tipo de datos Hoffman
data Hoffman = Hoja Char
            | Rama Hoffman Hoffman
            deriving (Show, Eq)

-- Función para crear un árbol Hoffman con una sola hoja
nuevoHoffman :: Char -> Hoffman
nuevoHoffman = Hoja

-- Función para combinar dos árboles Hoffman
fusionHoffman :: Hoffman -> Hoffman -> Hoffman
fusionHoffman = Rama

--Funciones de acceso
-- Devuelve el carácter de una hoja
obtenerCaracter :: Hoffman -> Char
obtenerCaracter (Hoja c) = c
obtenerCaracter _ = error "El árbol no es una hoja"

-- Devuelve el subárbol izquierdo
arbolIzquierdo :: Hoffman -> Hoffman
arbolIzquierdo (Rama izq _) = izq
arbolIzquierdo _ = error "El árbol no es una rama"

-- Devuelve el subárbol derecho
arbolDerecho :: Hoffman -> Hoffman
arbolDerecho (Rama _ der) = der
arbolDerecho _ = error "El árbol no es una rama"
