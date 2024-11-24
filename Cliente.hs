import RARisimo
import Frecuencia
import Hoffman
import qualified Data.Map as Map
import System.IO
import System.Directory


-- | Función principal que maneja la interacción con el usuario.
-- El programa comienza mostrando un menú y permite al usuario seleccionar opciones.
main :: IO ()
main = do
    putStrLn "RARísimo" -- Título del programa
    menu                -- Muestra el menú principal


-- | Función que muestra el menú de opciones y maneja la interacción con el usuario.
menu :: IO ()
menu = do
    putStrLn "\nSeleccione una opción:"
    putStrLn "1. Codificar"
    putStrLn "2. Decodificar"
    putStrLn "3. Analizar"
    putStrLn "4. Salir"
    opcion <- getLine -- Leer la opción seleccionada por el usuario
    case opcion of
        "1" -> codificar    -- Llama a la función para codificar un archivo
        "2" -> decodificar  -- Llama a la función para decodificar un archivo
        "3" -> analizar     -- LLama a la función para analizar un archivo al codificar
        "4" -> putStrLn "Saliendo... Programa terminado."
        _   -> do
            putStrLn "Opción no válida. Intente nuevamente."
            menu -- Si la opción no es válida, se vuelve a mostrar el menú


-- | Función para codificar un archivo.
-- Lee un archivo, genera su representación en el árbol de Hoffman,
-- codifica el contenido y guarda un archivo `.raro` con el árbol y el contenido codificado.
codificar :: IO ()
codificar = do
    putStrLn "Ingrese el path del archivo a codificar:"
    inputPath <- getLine
    fileExists <- doesFileExist inputPath -- Verifica si el archivo existe
    if fileExists
        then do
            -- Leer contenido del archivo
            content <- readFile inputPath
            let cleanedContent = if last content == '\n' then init content else content  -- Eliminar salto de línea al final

            -- Construir el árbol de Hoffman
            let tree = hoffman cleanedContent
            case tree of
                Nothing -> putStrLn "No se pudo construir el árbol de Hoffman."
                Just t -> do
                    -- Generar el mapa de codificación
                    let codMap = generarCodi t "" Map.empty

                    -- Codificar el contenido
                    let encodedContent = concatMap (\c -> Map.findWithDefault "" c codMap) content

                    -- Serializar el árbol de Hoffman con las codificaciones
                    let serializedTree = serializarArbolCodi t codMap

                    -- Guardar el archivo codificado
                    let outputPath = inputPath ++ ".raro"
                    writeFile outputPath (serializedTree ++ "\n" ++ encodedContent ++ "\n")
                    putStrLn $ "Archivo codificado guardado en: " ++ outputPath
        else do
            putStrLn "El archivo no existe. Intente nuevamente."
            codificar -- Reintentar si el archivo no existe


generarCodi :: Hoffman -> String -> Map.Map Char String -> Map.Map Char String
generarCodi (Hoja c) code acc = Map.insert c code acc -- Agrega el carácter con su codificación actual
generarCodi (Rama izq der) code acc =
    let accLeft = generarCodi izq (code ++ "0") acc  -- Codificación "0" para el subárbol izquierdo
        accRight = generarCodi der (code ++ "1") accLeft -- Codificación "1" para el subárbol derecho
    in accRight


-- | Función que serializa el árbol de Hoffman con las codificaciones.
-- Devuelve una representación en texto del árbol, incluyendo las codificaciones binarias.
serializarArbolCodi :: Hoffman -> Map.Map Char String -> String
serializarArbolCodi (Hoja c) codMap = "H(" ++ [c] ++ ", " ++ Map.findWithDefault "" c codMap ++ ")"
serializarArbolCodi (Rama izq der) codMap =
    "R(" ++ serializarArbolCodi izq codMap ++ "," ++ serializarArbolCodi der codMap ++ ")"


-- | Función para decodificar un archivo `.raro`.
-- Lee el archivo codificado, reconstruye el árbol de Hoffman, decodifica el contenido
-- y guarda el archivo decodificado en un nuevo archivo sin la extensión `.raro`.
decodificar :: IO ()
decodificar = do
    putStrLn "Ingrese el path del archivo a decodificar:"
    inputPath <- getLine
    fileExists <- doesFileExist inputPath -- Verifica si el archivo existe
    if fileExists
        then do
            -- Leer contenido del archivo
            content <- readFile inputPath

            -- Separar la representación del árbol y la cadena binaria
            let (treeStr, encodedContent) = parseContent content

            -- Crear un mapa inverso de codificación -> carácter
            let decodeMap = construirMapaDecodificacion treeStr

            -- Decodificar la cadena binaria
            let decodedContent = decodificarBinario decodeMap encodedContent

            -- Guardar el archivo decodificado
            let outputPath = takeWhile (/= '.') inputPath  -- Quitar la extensión `.raro`
            writeFile outputPath (decodedContent ++ "\n") -- Añadir una línea vacía al final
            putStrLn $ "Archivo decodificado guardado en: " ++ outputPath
        else do
            putStrLn "El archivo no existe. Intente nuevamente."
            decodificar -- Reintentar si el archivo no existe


-- | Función que divide el contenido del archivo en dos partes:
-- La representación del árbol y la cadena codificada.
parseContent :: String -> (String, String)
parseContent content =
    let [treeStr, encodedContent] = lines content
    in (treeStr, encodedContent)


-- | Función que reconstruye el mapa de decodificación desde la representación del árbol.
-- Toma el árbol serializado y produce un mapa con las codificaciones binarias como claves.
construirMapaDecodificacion :: String -> Map.Map String Char
construirMapaDecodificacion treeStr = Map.fromList $ parseHojas treeStr
  where
    -- Función auxiliar que extrae las hojas del árbol serializado
    parseHojas :: String -> [(String, Char)]
    parseHojas [] = []
    parseHojas ('H':'(':c:',':' ':rest) =
        let (code, restTail) = span (/= ')') rest -- Extrae el código binario
        in (code, c) : parseHojas (drop 1 restTail) -- Continúa con el resto del árbol
    parseHojas (_:rest) = parseHojas rest -- Ignorar otros caracteres


-- | Función que decodifica una cadena binaria utilizando el mapa de decodificación.
-- Devuelve la cadena original.
-- Recursion de cola.
decodificarBinario :: Map.Map String Char -> String -> String
decodificarBinario decodeMap = go ""
  where
    go acc [] = [] -- Si la cadena binaria está vacía, terminamos
    go acc (x:xs) =
        let newAcc = acc ++ [x] -- Añadimos el siguiente bit a `acc`
        in case Map.lookup newAcc decodeMap of
               Just c  -> c : go "" xs  -- Si encontramos el carácter, lo añadimos al resultado
               Nothing -> go newAcc xs  -- Seguimos acumulando bits si no encontramos el carácter


-- Función para analizar un código. 
analizar :: IO ()
analizar = do
    putStrLn "Ingrese el path del archivo a analizar:"
    inputPath <- getLine
    fileExists <- doesFileExist inputPath
    if fileExists
        then do
            -- Obtener tamaño del archivo en bytes
            fileSize <- getFileSize inputPath
            
            -- Leer contenido del archivo
            content <- readFile inputPath
            
            -- Construir el árbol de Hoffman
            let tree = hoffman content
            case tree of
                Nothing -> putStrLn "No se pudo construir el árbol de Hoffman."
                Just t -> do
                    -- Calcular el tamaño codificado
                    let codMap = generarCodi t "" Map.empty
                    let encodedContent = concatMap (\c -> Map.findWithDefault "" c codMap) content
                    let encodedSize = length encodedContent `div` 8 -- Tamaño aproximado en bytes
                    
                    -- Mostrar resultados
                    putStrLn $ "Tamaño original: " ++ show fileSize ++ " bytes"
                    putStrLn $ "Tamaño codificado: " ++ show encodedSize ++ " bytes"
                    putStrLn $ "Ganancia: " ++ show (calcularGanancia fileSize encodedSize) ++ "%"
        else do
            putStrLn "El archivo no existe. Intente nuevamente."
            analizar


-- Función para calcular el porcentaje de ganancia
calcularGanancia :: Integer -> Int -> Double
calcularGanancia original codificado =
    let ganancia = fromIntegral (original - toInteger codificado) / fromIntegral original * 100
    in ganancia
-- | Función que genera las codificaciones durante la construcción del árbol.
-- Toma un árbol de Hoffman y produce un mapa de caracteres con sus codificaciones binarias.
-- Recursion de cola.