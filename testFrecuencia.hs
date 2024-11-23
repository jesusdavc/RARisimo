import FrecuenciaV2

main :: IO ()
main = do
  -- Crear una frecuencia inicial
  let f1 = iniciarFrecuencia 'x'
  print f1 -- Resultado: Frecuencia {valor = 'x', contador = 1}

  -- Contar ocurrencias de un carácter en una lista
  let lista = "haskell es geniala"
  let f2 = contar 'e' lista

  print f2 -- Resultado: Frecuencia {valor = 'e', contador = 2}

  -- Obtener el valor y el contador
  print $ valor f2       -- Resultado: 'e'
  print $ frecuencia f2  -- Resultado: 2

  -- Comparar frecuencias
  let f3 = contar 'o' lista
  print f3
  print $ valor f3
  print $ frecuencia f3
  print (f2 > f3)        -- Resultado: True
  print (f2 <= f3)       -- Resultado: False
