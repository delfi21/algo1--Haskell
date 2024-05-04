-- eejercicio 1.1 
longitud :: [t] -> Int
longitud [] = 0 
longitud [a] = 1
longitud(x:y:ys) = longitud[x] + longitud(y:ys)
-- 1.2 
ultimo :: (Ord t) => [t] -> t
ultimo [a] = a
ultimo(x:y:ys) | ys == [] = y 
               | otherwise = ultimo(ultimo [x,y]:ys)
-- 1.3 
principio :: [t] -> [t]
principio[_]= []
principio(x:xs) = [x] ++ principio(xs)

-- 1.4
mandar_ultimo:: [t] -> [t] -- una funcion que manda el primer elemento al ultimo lugar
mandar_ultimo[]=[]
mandar_ultimo[a]=[a]
mandar_ultimo(x:y:ys) = (y:ys) ++ [x]

reverso :: [t] -> [t]
reverso[] = []
reverso[a] =[a]
reverso(x:y:ys) = reverso(principio(mandar_ultimo(x:y:ys))) ++ [x]

--Ejercicio 2
--2.1
--podria hacerlo para distintos tipos de datos??? 
pertenece :: (Eq t) => t -> [t] -> Bool
pertenece _ [] = False 
pertenece n [a] | n == a = True 
                | otherwise = False 
pertenece n (x:y:ys) | pertenece n [x] == True = True
                     | otherwise = pertenece n (y:ys) 

--2.2
todosIguales :: (Eq t) => [t] -> Bool
todosIguales [] = True 
todosIguales[a] = True 
todosIguales [a,b] | a == b = True 
                   | otherwise = False 
todosIguales(x:y:ys) | x /= y = False 
                     | otherwise = todosIguales(y:ys)

-- 2.3
todosDistintos :: (Eq t) => [t] -> Bool 
todosDistintos[] = True 
todosDistintos[a]=True 
todosDistintos[a,b] | a == b = False 
                    | otherwise = True 
todosDistintos(x:y:ys) | x == y = False 
                       | otherwise = todosDistintos(x:ys) && todosDistintos (y:ys)
--2.4
hayRepetidos :: (Ord t) => t -> [t] -> Bool 
hayRepetidos _[] = False 
hayRepetidos _ [a] = False 
hayRepetidos n (x:y:ys) | pertenece n [x] == True && pertenece n (y:ys) == True = True
                        | pertenece n [x] == False && pertenece n (y:ys) == True = hayRepetidos n (y:ys)
                        | pertenece n [x] == True && pertenece n (y:ys)== False = False 
                        | pertenece n [x]== False && pertenece n (y:ys) == False = False 

--2.5
-- quitar saca el primer elemento repetido al n
-- tengo que lponer el caso baso 1 eleemno por que use pattern matchin co (x:y:ys)
quitar :: (Eq t) => t -> [t] -> [t]
quitar _ [] = []
quitar n [a] | n==a = []
             | otherwise = [a]
quitar n (x:y:ys) | n == x = (y:ys)
                  | otherwise = (x:quitar n (y:ys))


--quitar todos: saca todos los elementos iguales a n
--2.6
quitar_todos :: (Eq t) => t -> [t] -> [t]
quitar_todos _ [] = []
quitar_todos n [a] | n == a = [] 
                   | otherwise = [a]
quitar_todos n (x:y:ys) | pertenece n (x:y:ys) == True = quitar_todos (n) (quitar n (x:y:ys))
                        | otherwise= (x:y:ys)
--2.7
eliminar_repetidos :: (Ord t) => t -> [t] -> [t] -- me deja los elementos una sola vez 
eliminar_repetidos _ [] =[]
eliminar_repetidos _ [a] = [a]
eliminar_repetidos n (x:y:ys) | quitar_todos n (x:y:ys) == (x:y:ys) = (x:y:ys)
                              | otherwise = quitar_todos n (x:y:ys) ++ [n]

-- creo una funcion que me da listas sin elementos repetidos 
-- f1 es una auxiliar que saca los repetidos al primer elemento 
f1 :: (Ord t) => [t] -> [t]
f1 [] = []
f1 [a] = [a]
f1(x:xs) | pertenece x (xs) == False  = (x:xs)
         | otherwise = (x:quitar_todos x (xs))

listasSinRepetidos :: (Ord t) => [t] -> [t]
listasSinRepetidos[] = []
listasSinRepetidos[a] = [a]
listasSinRepetidos[a,b] | a ==b = [a]
                        | otherwise= [a,b]
listasSinRepetidos (x:y:ys) = listasSinRepetidos(f1 (tail(f1(x:y:ys)))) ++ [x]
 -- 2.8 mismos elementos (le puse elementos iguales)
elementosIguales :: (Ord t) => [t] -> [t] -> Bool
elementosIguales [] [] = True 
elementosIguales [] (x:xs) = False 
elementosIguales (x:xs) [] = False
elementosIguales [a] [b] | a ==b = True 
                         | otherwise = False 
elementosIguales (x:xs) (y:ys) | longitud (listasSinRepetidos(x:xs)) /= longitud(listasSinRepetidos(y:ys)) =False 
                               | otherwise = estanIncluidos (listasSinRepetidos(x:xs)) (listasSinRepetidos(y:ys))

estanIncluidos :: (Ord t) => [t] -> [t] -> Bool 
estanIncluidos _ [] = False 
estanIncluidos [a] (x:xs) | pertenece a (x:xs) == False = False
                          | otherwise = True 
estanIncluidos (x:xs) (y:ys) | pertenece x (y:ys) == False = False 
                             | otherwise = estanIncluidos (xs) (y:ys) -- esta funcion esta muy pensada para ser usada en la anterior, es decir, asumo listas de igual tamaño y q se yo q mas

--2.9 
capicua :: (Eq t) => [t] -> Bool
capicua [] = True 
capicua[a] = True 
capicua(x:xs) | reverso(x:xs) == (x:xs) = True 
              | otherwise= False 

-- ejercicio 3 
--3.1
sumatoria :: [Integer] -> Integer 
sumatoria [] = 0
sumatoria[a] = a
sumatoria(x:xs) = sumatoria[x] + sumatoria(xs)
--3.2
productoria :: [Integer] -> Integer
productoria [] = 0
productoria[a] = a
productoria(x:y:ys)= productoria[x]*productoria(y:ys)
--3.3
mayor :: [Integer] -> Integer -- no acepta listas vacias // le puse mayor pero era maximo
mayor[a] = a
mayor[x,y] | x >= y = x
           | otherwise = y 
mayor(x:y:ys) = mayor([mayor[x,y]]++(ys))
--3.4
sumarN :: Integer -> [Integer] -> [Integer]
sumarN n [] = [n]
sumarN n [a] = [a+n]
sumarN  n (x:y:ys) = sumarN n [x] ++ sumarN n (y:ys)
--3.5
sumarElPrimero :: [Integer] -> [Integer] --no acepta listas vacias 
sumarElPrimero(x:xs) = sumarN (x) (x:xs)
--3.6
sumarElUltimo :: [Integer] -> [Integer]
sumarElUltimo(x:xs) = sumarN (ultimo(x:xs)) (x:xs)
--3.7
resto :: Integer -> Integer -> Integer --hago esta funcion para ahorrarme los problemas de mod con los negativos // igual, para pares, solo mandas positivps 
resto a b| b >= 0 = mod b a
         | otherwise = mod (-b) a

pares :: [Integer] -> [Integer]
pares[] =[]
pares[a] | a<=0 = []
         |resto 2 a == 0 = [a]
         | otherwise = []
pares(x:xs) = pares[x] ++ pares(xs)
--3.8
multiplosDeN :: Integer -> [Integer] -> [Integer] -- considero multiplo a los 
multiplosDeN _ [] = []
multiplosDeN n [a] | mod a n == 0 = [a]
                   | otherwise = []
multiplosDeN n (x:xs) = multiplosDeN n [x] ++ multiplosDeN n (xs)
-- 3.9
mayor_adelante :: [Integer] -> [Integer] -- manda el mayor a el primer lugar 
mayor_adelante[] = []
mayor_adelante[a] = [a]
mayor_adelante(x:xs) | mayor(x:xs) == x = (x:xs)
                     | otherwise = mayor_adelante(xs) ++[x]

ordenar :: [Integer] -> [Integer]
ordenar [] = []
ordenar[a] = [a]
ordenar (x:y:ys) = reverso([mayor(x:y:ys)] ++ mayor_adelante(tail(mayor_adelante(x:y:ys))))

-- ejercicio 4
--4.1
sacarBlancosRepetidos :: [Char] -> [Char]
sacarBlancosRepetidos[] = []
sacarBlancosRepetidos[x]=[x]
sacarBlancosRepetidos(x:y:ys) | x == ' '   && y == ' ' = sacarBlancosRepetidos(y:ys)
                              | otherwise = x:sacarBlancosRepetidos(y:ys) -- revisar esta logica 


-- 4.2
-- entiendo que me pide contar la cantidad de espacios o algo asi no la cantidad de combinaciones 
-- observacion:  la cantidad de palabras es la cantidad de blancos +1, 

contarBlancos :: [Char] -> Integer
contarBlancos[]= 0
contarBlancos[x] = 1
contarBlancos(x:y:ys) | y == ' ' = 1 + contarBlancos(ys) -- no entiendo porque ahi no va (y:ys) pero gueno 
                      | otherwise = contarBlancos(y:ys)

contarPalabras :: [Char] -> Integer 
contarPalabras[] = 0
contarPalabras[x] = 1
contarPalabras(x:y:ys) = (contarBlancos (sacarBlancosRepetidos(x:y:ys)) + 1)

-- 4.3
quitar_primeros_vacios :: [Char] -> [Char]
quitar_primeros_vacios[] =[]
quitar_primeros_vacios[x] | x == ' ' = []
                          | otherwise= [x]
quitar_primeros_vacios(x:y:ys) | x == ' ' = (y:ys)
                               | otherwise = (x:y:ys)

--palabras 
sacarPrimerosVacios :: [Char] -> [Char] -- la escribo ya pensando q las listas van a pasar quitar blancos primero 
sacarPrimerosVacios(x:xs) | x == ' ' = (xs)
                          | otherwise = (x:xs)
                        
aux1 :: [Char] -> [Char]
aux1[] =[]
aux1(x:xs) | x == ' ' = []
             | otherwise = [x] ++ aux1(xs) -- esta funcion me devuelve la primer palabra como lista de char sin blancos 

aux2 :: [Char] -> [Char] -> [[Char]]
aux2 [] l = [aux1 l]
aux2 (x:xs) l | x == ' ' = aux1 l : aux2 xs xs
              | otherwise = aux2 xs l  -- esto lo saque de cuba wiki y no lo entiendo bien 

palabras :: [Char] -> [[Char]]
palabras l = aux2 l l

-- 4.5
comparoPalabras :: [[Char]] -> [Char]
comparoPalabras [] = []
comparoPalabras [a] = a
comparoPalabras (x:y:ys) | longitud x < longitud y = comparoPalabras(y:ys)
                         | otherwise = comparoPalabras(x:ys)

palabraMasLarga :: [Char] -> [Char]
palabraMasLarga [] = []
palabraMasLarga[a] = [a]
palabraMasLarga(x:y:ys) = comparoPalabras(palabras(x:y:ys))

-- 4.6 
aplanar :: [[Char]] -> [Char]
aplanar[] = []
aplanar[a] = a 
aplanar(x:y:ys) = aplanar[x] ++ aplanar(y:ys)

--4.7 
aplanarConBlancos :: [[Char]] -> [Char]
aplanarConBlancos[] = []
aplanarConBlancos(x:xs) = aplanar[x] ++ [' '] ++ aplanarConBlancos(xs)

--4.8
crear_listaN :: Char -> Int -> [Char]
crear_listaN _ 0 = []
crear_listaN s 1 = [s]
crear_listaN s n = crear_listaN s 1 ++ crear_listaN s (n-1)
                 
aplanarConNblancos :: [[Char]]-> Int -> [Char]
aplanarConNblancos [[]] n = []
aplanarConNblancos [a] n = a
aplanarConNblancos (x:xs) n = aplanar[x] ++ crear_listaN ' ' n  ++ aplanarConNblancos (xs) n

--ejercicio 5 
-- ejercicio 5.1
sumatoria_plus :: (Num t) => [t] -> t
sumatoria_plus[] = 0
sumatoria_plus[a] = a
sumatoria_plus (x:xs) = sumatoria_plus[x] + sumatoria_plus(xs)

longitud_plus :: (Num t) => [t] -> t
longitud_plus [] = 0 
longitud_plus [a] = 1
longitud_plus(x:y:ys) = longitud_plus[x] + longitud_plus(y:ys) 

principio_plus :: (Num t) => [t] -> [t]
principio_plus[_]= []
principio_plus(x:xs) = [x] ++ principio_plus(xs)  --- las redefini para q no me tire error por usar t 

sumarHastaN :: (Ord t ) => (Num t) => [t] -> t -> t -- no lo use al final
sumarHastaN _ 0 = 0 
sumarHastaN [a] n = a 
sumarHastaN (x:y:ys) n | longitud_plus (x:y:ys) <= n = sumatoria_plus(x:y:ys)
                        | otherwise = sumarHastaN (principio_plus (x:y:ys)) n

sumaAcumulada_aux ::  (Num t) => [t] -> [t]
sumaAcumulada_aux[a] = [a]
sumaAcumulada_aux(x:xs) =  [sumatoria_plus(x:xs)] ++ sumaAcumulada_aux(principio_plus(x:xs))

sumaAcumulada :: (Num t) => [t] -> [t]
sumaAcumulada[a] = [a]
sumaAcumulada(x:xs) = reverso(sumaAcumulada_aux(x:xs))

-- 5.2
-- es el de los primos, y la verdad esta bueno para pensar pero ahora no tengo tiempo :) 

