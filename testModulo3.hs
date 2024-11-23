import RARisimo
import Frecuencia
import Hoffman
import qualified Data.Map as Map


main :: IO ()
main = do
    let str = "abacab"
    
    -- Frecuencias de los caracteres
    let freqs = frecuencias str
    print freqs

    -- Los dos ganadores con menor frecuencia
    let g = ganadores freqs
    print g

    -- Construcción del árbol de Hoffman
    let tree = hoffman str
    print tree

    -- Codificación de Hoffman
    let cod = rarisimo str
    print cod
