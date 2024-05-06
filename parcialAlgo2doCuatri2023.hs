-- ejercicio 1 "Atajaron Suplentes"
sumaElementos :: [Int] -> Int 
sumaElementos[] = 0 
sumaElementos [a] = a 
sumaelementos(x:xs) = sumaElementos[x] + sumaElementos (xs)


atajaronSuplentes :: [(String,String)] -> [Int] -> Int -> Int
atajaronSuplentes [] [] a = a 
atajaronSuplentes (equipo:equipos) (gol:goles) totalGoles = totalGoles - sumaElementos(gol: goles)

-- Ejercicio 2 "Equipos Validos"
tuplasValidas :: [(String,String)] -> Bool 
tuplasValidas [(a,b)] | a == b = False 
                      | otherwise = True 
tuplasValidas (x:xs)  | tuplasValidas[x] == False = False 
                      | otherwise= tuplasValidas (xs)

elementosValidos :: [(String,String)] -> Bool 
elementosValidos[a] = True
elementosValidos[(a,b), (c,d)] | a == c || a == d || b == c || b == c = False 
                               | otherwise = True 
elementosValidos ((a,b):(c,d):xs)| elementosValidos[(a,b), (c,d)]== False = False 
                                 | otherwise = elementosValidos ((a,b):xs)

elementosValidosRecursiva :: [(String,String)] -> Bool 
elementosValidosRecursiva[]= True 
elementosValidosRecursiva [a] = True 
elementosValidosRecursiva(x:xs) | elementosValidos (x:xs) == False = False 
                                | otherwise= True

equiposValidos:: [(String,String)] -> Bool
equiposValidos [] = True 
equiposValidos(x:xs) | elementosValidosRecursiva(x:xs) == True && tuplasValidas (x:xs) == True = True 
                     | otherwise = False 

-- ejercicio 3 "Porcentaje de Goles"
division :: Int -> Int -> Float
division a b = (fromIntegral a) / (fromIntegral b)

miTupla :: String -> [(String,String)] -> [Int] -> Int 
miTupla a ((b,c):xs) (gol:goles) | a == c = gol 
                                 | otherwise = miTupla a (xs) (goles)

porcentajeDeGoles :: String -> [(String,String)] -> [Int] -> Float 
porcentajeDeGoles  a (x:xs) (gol:goles) = division ((miTupla a (x:xs) (gol:goles))*100) (sumaElementos(gol:goles))

-- Ejercicio 4 "Valla menos Vencida" 
menor :: [Int] -> Int
menor[a] = a
menor(x:y:ys) | x > y = menor(y:ys)
              | otherwise = menor(x:ys)

vallaMenosVencida :: [(String,String)] -> [Int] -> String
vallaMenosVencida [(a,b)] _ = b  
vallaMenosVencida (x:xs) (gol:goles) | menor (gol:goles) == gol = snd(x)
                                     | otherwise = vallaMenosVencida (xs) (goles)




