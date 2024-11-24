import Hoffman
import qualified Data.Map as Map

-- Función para crear un árbol de Hoffman más grande
crearArbol :: Hoffman
crearArbol =  
    fusionHoffman (nuevoHoffman 'a') (fusionHoffman (nuevoHoffman 'b') (nuevoHoffman 'c'))

main :: IO ()
main = do
  
  -- Crear un árbol grande
  let tree = crearArbol

  -- Mostrar la codificación de Hoffman para todos los caracteres en el árbol
  let cod = codificacion tree
  putStrLn "Codificación de Hoffman:"
  print cod

  -- Ejemplo de obtener el carácter de una hoja
  putStrLn "\nCaracter de la hoja 'a':"
  print $ obtenerCaracter (nuevoHoffman 'a')

  -- Ejemplo de los subárboles
  putStrLn "\nSubárbol izquierdo de la rama principal:"
  print $ arbolIzquierdo tree
  putStrLn "Subárbol derecho de la rama principal:"
  print $ arbolDerecho tree