import RARisimo
import qualified Data.Map as Map

main :: IO ()
main = do
    let str = "abracadabra"
    
    -- Frecuencias de los caracteres
    let freqs = frecuencias str
    print freqs
