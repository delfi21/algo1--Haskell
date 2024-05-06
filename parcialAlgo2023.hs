-- Ejercicio 1 "Votos En Blanco"
sumarElementos :: (Num t) => [t] -> t 
sumarElementos[] = 0 
sumarElementos[a] = a
sumarElementos(x:xs)= sumarElementos[x] + sumarElementos(xs)


votosEnBlanco :: [(String,String)] -> [Integer] -> Integer -> Integer 
votosEnBlanco [] [] n = n 
votosEnBlanco (x:xs) (y:ys) n = n - sumarElementos(y:ys)

-- Ejercicio 2 " Fórmulas Válidas "
tuplasValidas :: [(String,String)] -> Bool
tuplasValidas [] = False 
tuplasValidas[(a,b)] | a == b = False 
                     | otherwise = True 
tuplasValidas (x:xs) | tuplasValidas [x] == False = False 
                     | otherwise = tuplasValidas (xs)

elementosValidos :: [(String, String)] -> Bool -- compara el primer elemento 
elementosValidos [] = False 
elementosValidos [(a,b)] = True 
elementosValidos ((a,b):(c,d):xs) | a == c || a == d || b == c || b == d = False 
                                  | otherwise = elementosValidos((a,b):xs)

elementosValidosRecursiva :: [(String,String)] -> Bool 
elementosValidosRecursiva[] = False 
elementosValidosRecursiva[a] = True 
elementosValidosRecursiva(x:xs) | elementosValidos (x:xs) == False = False 
                                | otherwise = elementosValidos (xs)

formulasValidas :: [(String,String)] -> Bool 
formulasValidas [] = False 
formulasValidas (x:xs) | elementosValidosRecursiva(x:xs) && tuplasValidas (x:xs) == True = True 
                       | otherwise = False 

-- Ejercicio 3 " Porcentaje de Votos"           
division :: Int -> Int -> Float
division a b = (fromIntegral a) / (fromIntegral b) 

encontrarMiTupla :: String -> [(String,String)]-> [Int] -> Int 
encontrarMiTupla a ((b,c):xs) (voto:votos) | a == b = voto
                                           | otherwise = encontrarMiTupla a (xs) (votos)

porcentajeDeVotos :: String -> [(String, String)] -> [Int] -> Float
porcentajeDeVotos a (x:xs) (y:ys) = division ((encontrarMiTupla a (x:xs) (y:ys))*100)  (sumarElementos (y:ys))

-- ejercicio 4  "Proximo Presidente"
mayor :: [Int] -> Int 
mayor[a] = a
mayor(x:y:ys) | x > y = mayor(x:ys)
              | otherwise = mayor(y:ys)

proximoPresidente :: [(String,String)] -> [Int] -> String
proximoPresidente [(a,b)] [c] = a
proximoPresidente ((a,b):xs) (voto:votos) | mayor (voto:votos) == voto = a 
                                          | otherwise = proximoPresidente (xs) (votos)





