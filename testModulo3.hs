import RARisimo
import FrecuenciaV2
import Hoffman
import qualified Data.Map as Map


main :: IO ()
main = do
    let str = "abracadabra"
    
    -- Frecuencias de los caracteres
    let freqs = frecuencias str
    print freqs

    -- Los dos ganadores con menor frecuencia
    let g = ganadores freqs
    print g

    -- Construcción del árbol de Huffman
    let tree = hoffman str
    print tree

    -- Codificación de Huffman
    let cod = rarisimo str
    print cod
