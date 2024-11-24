import RARisimo
import Frecuencia
import Hoffman
import qualified Data.Map as Map
import System.IO
import System.Directory

-- Función principal que maneja la interacción con el usuario
main :: IO ()
main = do
    putStrLn "RARísimo"
    menu

-- Función que muestra el menú de opciones
menu :: IO ()
menu = do
    putStrLn "\nSeleccione una opción:"
    putStrLn "1. Codificar"
    putStrLn "3. Analizar"

    opcion <- getLine
    case opcion of
        "1" -> codificar
        "3" -> analizar 
        _   -> do
            putStrLn "Opción no válida. Intente nuevamente."
            menu

-- Función para codificar un archivo
codificar :: IO ()
codificar = do
    putStrLn "Ingrese el path del archivo a codificar:"
    inputPath <- getLine
    fileExists <- doesFileExist inputPath
    if fileExists
        then do
            -- Leer contenido del archivo
            content <- readFile inputPath

            -- Construir el árbol de Hoffman
            let tree = hoffman content
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
            codificar
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
-- Función para generar las codificaciones mientras se construye el árbol
generarCodi :: Hoffman -> String -> Map.Map Char String -> Map.Map Char String
generarCodi (Hoja c) code acc = Map.insert c code acc
generarCodi (Rama izq der) code acc =
    let accLeft = generarCodi izq (code ++ "0") acc
        accRight = generarCodi der (code ++ "1") accLeft
    in accRight

-- Función para serializar el árbol de Hoffman con las codificaciones
serializarArbolCodi :: Hoffman -> Map.Map Char String -> String
serializarArbolCodi (Hoja c) codMap = "H(" ++ [c] ++ ", " ++ Map.findWithDefault "" c codMap ++ ")"
serializarArbolCodi (Rama izq der) codMap =
    "R(" ++ serializarArbolCodi izq codMap ++ "," ++ serializarArbolCodi der codMap ++ ")"


