-- Programo la funcion relaciones validas 
-- defino primero una funcion que me devuelva falso <-> los elementos de una tupla son iguales 
elementost_iguales :: [(String, String)]-> Bool
elementost_iguales[] = True 
elementost_iguales[(x,y)]  | x == y = False
                           | otherwise= True 
elementost_iguales(x:y:ys) | fst(x) == snd(x) = False
                           | otherwise = elementost_iguales(y:ys)
-- defino una funcion que compara tuplas como elementos de una lista y una que hace la recursion 
-- pregunta: ¿tengo q redefinir los casos bases si la funcion que llamo ya los contempla?
-- pregunta: Es necesario el caso base de 1 elemento no?
-- pregunta: ¿Como podria hacer esto mas corto? jajaja ¿estoy haciendo cosas de mas?
elementosl_iguales :: [(String, String)] -> Bool
elementosl_iguales[] = True 
elementosl_iguales[(x,y)] = True
elementosl_iguales(x:y:ys)| x == y || fst(x) == snd(y) && snd(x) == fst (y) = False 
                          | otherwise= elementosl_iguales(x:ys)
elementosl_recursivo :: [(String, String)] -> Bool
elementosl_recursivo[]= True 
elementosl_recursivo[(x,y)] = True
elementosl_recursivo(x:y:ys) | elementosl_iguales(x:y:ys) == False = False
                             | otherwise= elementosl_iguales(y:ys)

-- ahora si, defino elementosValidos
elementosValidos :: [(String, String)] -> Bool
elementosValidos[] = True 
elementosValidos [(x,y)]= True
elementosValidos(x:y:ys) | elementost_iguales(x:y:ys) == False || elementosl_recursivo(x:y:ys) == False = False 
                         | otherwise = True

-- Problema Personas 
-- Entiendo, por los requiere, que elementosValidos me devuelve True -> ya se que mi lista no tiene ni tuplas con los elementos iguales,
--ni elementos de la lista iguales
-- defino una funcion que compare el primer elemento de la primer tupla con el primer elemento de la segunda 
-- defino una funcion longitud por las dudas :: spoiler, no la use 
longitud :: (Num t)=>[(t)]->t 
longitud[]=0
longitud[(a)]=1
longitud(x:xs)=longitud[(x)]+longitud(xs)
-- creo una funcion que me crea una lista con cada elementos de las tuplas
f1_recursiva :: [(String,String)] -> [(String)]
f1_recursiva[]=[]
f1_recursiva[(a,b)]=[(a),(b)]
f1_recursiva(x:y:ys)=f1_recursiva[(x)]++f1_recursiva(y:ys)

-- ahora creo una funcion quitar, uso la funcion pertenece como auxiliar

pertenece :: (Eq t) => t -> [t] -> Bool
pertenece _ [] = False
pertenece n [a] | n == a = True 
                | otherwise = False 
pertenece n (x:y:ys) | n == x = True 
                     | otherwise = pertenece n (y:ys)

quitar1 :: (Eq t) => t -> [t] -> [t]
quitar1 _ [] = []
quitar1 n [a] | n == a = []
              | otherwise = [a]
quitar1 n (x:y:ys) | pertenece n [x] == True && pertenece n (y:ys) == False = (y:ys)
                   | pertenece n [x] == False  && pertenece n (y:ys) == True = [x] ++ quitar1 n (y:ys)
                   | pertenece n [x] == True && pertenece n (y:ys) == True = quitar1 n (y:ys)
                   | pertenece n [x] == False && pertenece n (y:ys) == False = (x:y:ys)


--quitar ::  (Eq t) => t -> [t] -> [t]
--quitar _ [] = []
--quitar n (x:y:ys) | n == x = (y:ys)
                  -- | otherwise = [x] ++ quitar n (y:ys) -- esto esta andando mal, y por eso no funciona personas entiendo, porque anda mal?

quitar_distintos1 :: [String] -> [String]
quitar_distintos1 [] = []
quitar_distintos1 [a] = [a]
quitar_distintos1 (x:y:ys) = [x] ++ quitar1 x (y:ys) 

-- esta funcion me saca los repetidos al primer elemento 

quitar_distintos_recursivo :: [String] -> [String]
quitar_distintos_recursivo [] = []
quitar_distintos_recursivo [a] = [a]
quitar_distintos_recursivo (x:y:ys) = [x] ++ quitar_distintos_recursivo(quitar_distintos1(tail(quitar_distintos1(x:y:ys))))
-- no entiendo muy bien porque esto funciona 

-- defino la funcion personas 
personas :: [(String, String)] -> [String]
personas [] = []
personas(x:y:ys) = quitar_distintos_recursivo(f1_recursiva(x:y:ys))

-- problema tres // Amigos de 
amigosDe :: String -> [(String, String)] -> [String]
amigosDe _ [] = []
amigosDe n [(x,y)] | x == n = [y]
                   | y == n = [x]
                   | otherwise = []
amigosDe n (x:y:ys) = amigosDe n [x] ++ amigosDe n (y:ys)
-- pregunta, porque no puedo usar mayusculas en los nombre de las funciones?
-- problema 4  personaConMasAmigos 
-- creo una funcion que hace una lista con la cantidad de elementos de amigosDe que tiene cada elemento de la tupla 
 
-- mas amigos -> comparo las longitudes de amigos de n para cada elemento de las tuplas 
-- defino una funcion que me diga cuantas veces se repite el primer elemento
repetidos :: [String] -> Int
repetidos[] = 0
repetidos[a] = 0
repetidos[a,b] | a == b = 1
               | otherwise = 0
repetidos (x:y:ys) = repetidos[x,y] + repetidos(x:ys)

repetidos_recursiva :: [String] -> [Int]
repetidos_recursiva[] = 0
repetidos_recursiva[a] = 0
repetidos_recursiva(x:y:ys) = [repetidos(x:y:ys)] ++ [repetidos(y:ys)]

creo_tuplas :: [String] -> [Int] -> [(String,Int)]
creo_tuplas[a] [b] = [(a,b)]
creo_tuplas (x:y:ys) (a:b:bs) = creo_tuplas[x] [a] ++ creo_tuplas(y:ys) (b:bs)

creo_lista :: [(String, Int)] -> [Int]
creo_lista[(x,y)] = [y]
creo_lista(x:y:ys) = creo_lista[x] ++ creo_lista(y:ys)

mistuplas :: [(String,String)] -> [(String,Int)]
mistuplas (x:y:ys) = (creo_tuplas f1_recursiva(x:y:ys) repetidos_recursiva(x:y:ys)) 

--personaConMasAmigos :: [(String,String)] -> String
--personaConMasAmigos[(x,y)] = x 
--personaConMasAmigos(x:y:ys) | mayor == snd(head(mistuplas(x:y:ys))) = fst(mistuplas(x:y:ys))
                            -- | otherwise = personaConMasAmigos(y:ys)
                            --where mayor = mayor(creo_lista(creo_tuplas(x:y:ys)))

cant_de_amigos :: String -> [(String,String)] -> Int 
cant_de_amigos x relaciones = longitud (amigosDe x relaciones)

buscarPersonaConMasAmigos :: [String] -> [String,String] -> String
buscarPersonaConMasAmigos [x]_ = x
buscarPersonaConMasAmigos(x:xs) relaciones | cant_de_amigos x relaciones > cant_de_amigos llamadoRecursivo relaciones = x
                                           | otherwise = llamadoRecursivo 
                                           where llamadoRecursivo = buscarPersonaConMasAmigos xs relaciones  -- lo tengo que pensar 

personaConMasAmigos :: [(String,String)] -> String
personaConMasAmigos relaciones = buscarPersonaConMasAmigos (personas relaciones) relaciones 



