module Hoffman
    ( Hoffman
    , nuevoHoffman
    , fusionHoffman
    , obtenerCaracter
    , arbolIzquierdo
    , arbolDerecho
    , codificacion
    ) where

import qualified Data.Map as Map

-- | Tipo de datos para representar un árbol de Hoffman.
-- Un árbol de Hoffman puede ser una hoja o una rama.
data Hoffman = Hoja Char       -- ^ Una hoja contiene un carácter.
             | Rama Hoffman Hoffman  -- ^ Una rama contiene dos subárboles (hijos izquierdo y derecho).
             deriving (Eq)

-- | Crea un nuevo árbol de Hoffman con un solo nodo hoja.
-- Este nodo hoja contiene un carácter.
nuevoHoffman :: Char -> Hoffman
nuevoHoffman c = Hoja c

-- | Fusiona dos árboles de Hoffman.
-- Crea un nuevo nodo rama cuyo hijo izquierdo es el primer árbol
-- y el hijo derecho es el segundo árbol.
fusionHoffman :: Hoffman -> Hoffman -> Hoffman
fusionHoffman izq der = Rama izq der

-- | Devuelve el carácter contenido en una hoja de un árbol de Hoffman.
-- Si el árbol no es una hoja, arroja un error.
obtenerCaracter :: Hoffman -> Char
obtenerCaracter (Hoja c) = c
obtenerCaracter _        = error "Intento de obtener carácter de una rama, no es una hoja."

-- | Devuelve el subárbol izquierdo de un nodo rama.
-- Si el árbol no es una rama, arroja un error.
arbolIzquierdo :: Hoffman -> Hoffman
arbolIzquierdo (Rama izq _) = izq
arbolIzquierdo _            = error "Intento de obtener subárbol izquierdo de una hoja."

-- | Devuelve el subárbol derecho de un nodo rama.
-- Si el árbol no es una rama, arroja un error.
arbolDerecho :: Hoffman -> Hoffman
arbolDerecho (Rama _ der) = der
arbolDerecho _            = error "Intento de obtener subárbol derecho de una hoja."

-- | Genera la codificación de Hoffman para todos los caracteres en un árbol.
-- Devuelve un mapa que asocia a cada carácter su cadena binaria correspondiente.
codificacion :: Hoffman -> Map.Map Char String
codificacion = codificacionAux ""
  where
    -- Función auxiliar que recorre el árbol y genera la codificación
    codificacionAux :: String -> Hoffman -> Map.Map Char String
    codificacionAux pref (Hoja c) = Map.singleton c pref
    codificacionAux pref (Rama izq der) =
        Map.union (codificacionAux (pref ++ "0") izq) (codificacionAux (pref ++ "1") der)
--Instancias Show y Read para Hoffman

-- Instancia de Show para Hoffman
instance Show Hoffman where
    show (Hoja c)         = "Hoja " ++ show c
    show (Rama izq der)   = "Rama (" ++ show izq ++ ") (" ++ show der ++ ")"

-- Instancia de Read para Hoffman
instance Read Hoffman where
    readsPrec _ input = 
        case stripPrefix "Hoja " input of
            Just rest -> [(Hoja (read rest), "")]
            Nothing -> case stripPrefix "Rama (" input of
                Just rest -> 
                    let (izq, rest1) = head (reads rest)
                        (der, rest2) = head (reads (tail rest1)) -- tail para saltar el espacio y paréntesis de cierre
                    in [(Rama izq der, rest2)]
                Nothing -> []

stripPrefix :: String -> String -> Maybe String
stripPrefix = stripPrefix